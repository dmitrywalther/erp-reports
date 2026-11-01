# Run the real generator entirely in memory: database operations are replaced
# in its evaluation environment; no connection or persistent writes are made.
# Rscript --vanilla -e "source('demo/test_seed_lifecycle.R', encoding='UTF-8')"
local({
  seed <- new.env(parent = globalenv())
  seed$dbConnect <- function(...) structure(list(), class = "seed_test_connection")
  seed$dbExistsTable <- function(...) TRUE
  seed$dbGetQuery <- function(...) data.frame(n = 0)
  seed$dbWriteTable <- function(...) TRUE
  seed$dbExecute <- function(...) 0L
  seed$dbDisconnect <- function(...) TRUE
  seed$Sys.time <- function() as.POSIXct("2026-11-01 10:00:00", tz = "UTC")
  suppressMessages(source("demo/seed_demo.R", local = seed, keep.source = FALSE,
                          encoding = "UTF-8"))

  assert <- function(condition, message) {
    if (!isTRUE(condition)) stop(message, call. = FALSE)
  }
  not_future <- function(value, label) {
    n_future <- sum(value > seed$now_utc, na.rm = TRUE)
    assert(n_future == 0, sprintf("%s contains %d completed events in the future", label, n_future))
  }
  ordered <- function(before, after, label) {
    assert(all(is.na(after) | (!is.na(before) & before <= after)), paste("Invalid stage chronology:", label))
  }

  # These are actual, completed stages; planned loading/payment may be future.
  not_future(seed$docs_received_at, "docsReceived")
  not_future(seed$docs_approved_at, "docsApproved")
  not_future(seed$paid_at, "paid")
  not_future(seed$problem_at, "docsProblem")
  ordered(seed$unloading_plan, seed$docs_received_at, "unload -> docsReceived")
  ordered(seed$docs_received_at, seed$docs_approved_at, "docsReceived -> docsApproved")
  ordered(seed$docs_approved_at, seed$paid_at, "docsApproved -> paid")
  ordered(seed$docs_rec, seed$problem_at, "docsReceived -> docsProblem")
  assert(all(is.na(seed$docs_approved_at[seed$has_docs_problem])), "Unresolved document problems cannot already be approved")
  assert(!any(seed$billable %in% which(seed$has_docs_problem)), "Unresolved document problems cannot have client bills")
  for (column in c("charterer_certificate_info_created_at", "charterer_certificate_info_ready_at")) {
    not_future(seed$requests[[column]], paste("client certificate", column))
  }
  for (column in c("carrier_certificate_info_created_at", "carrier_certificate_info_ready_at", "carrier_bill_info_paid_at")) {
    not_future(seed$transportations[[column]], paste("carrier", column))
  }
  ordered(seed$transportations$carrier_certificate_info_created_at,
          seed$transportations$carrier_certificate_info_ready_at, "carrier certificate created -> ready")

  for (column in c("created_at_original", "created_at", "payment_date")) {
    not_future(seed$bills_df[[column]], paste("bill", column))
  }
  ordered(seed$bills_df$created_at_original, seed$bills_df$created_at, "bill original -> updated")
  ordered(seed$bills_df$created_at, seed$bills_df$payment_date, "bill created -> paid")
  for (bill in seq_along(seed$bills)) {
    request_rows <- match(seed$bills[[bill]]$request_ids, seed$requests$id)
    assert(all(seed$docs_approved_at[request_rows] <= seed$bills_df$created_at[bill]),
           "A bill cannot precede document approval for its requests")
  }
  for (column in c("created_at", "send_date", "delivery_date", "verification_date", "returned_date")) {
    not_future(seed$packets[[column]], paste("packet", column))
  }
  ordered(seed$packets$created_at, seed$packets$send_date, "formed -> sent")
  ordered(seed$packets$send_date, seed$packets$delivery_date, "sent -> delivered")
  ordered(seed$packets$delivery_date, seed$packets$verification_date, "delivered -> verified")
  ordered(seed$packets$verification_date, seed$packets$returned_date, "verified -> returned")
  ordered(seed$packets$verification_date, seed$bills_df$payment_date, "verified -> paid")
  assert(any(seed$loading_plan > seed$now_utc), "Future planned trips must remain in the demo")
  cat("Seed lifecycle: PASS (real generator in memory; completed document/bill stages are ordered and not future)\n")
})
