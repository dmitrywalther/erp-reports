# No external network/database: validate route selection and a loopback timeout.
# Rscript --vanilla -e "source('demo/test_doublets.R', encoding='UTF-8')"
.libPaths(c(file.path(Sys.getenv("LOCALAPPDATA"), "R", "erp-reports-lib"), .libPaths()))
suppressPackageStartupMessages({ library(shiny); library(dplyr); library(sf) })
find_assignment <- function(expr, name) {
  if (missing(expr) || (!is.call(expr) && !is.expression(expr))) return(NULL)
  if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
      identical(expr[[2]], as.name(name))) return(expr)
  for (part in as.list(expr)) {
    found <- find_assignment(part, name)
    if (!is.null(found)) return(found)
  }
  NULL
}
server_source <- file("server.R", open = "rt", encoding = "UTF-8")
server_code <- parse(server_source)
close(server_source)
test_env <- new.env(parent = globalenv())
test_env$input <- reactiveValues(doublets = NULL)
fixture <- data.frame(
  Кругорейс = c("A", "A", "B", "B"),
  loading_longitude = c(37.6, 30.3, 60, 70), loading_latitude = c(55.7, 59.9, 50, 55),
  unloading_longitude = c(30.3, 37.6, 70, 60), unloading_latitude = c(59.9, 55.7, 55, 50),
  stringsAsFactors = FALSE
)
test_env$doubletsTable <- reactiveValues(data = fixture)
test_env$safe_osrm_route <- function(pts) {
  stopifnot(nrow(pts) == 2L, all(is.finite(as.matrix(pts))))
  pts
}
reactives <- c("selected_direct_route", "selected_intermediate_route", "selected_back_route",
               "selected_start_point", "selected_finish_point", "selected_start_point1", "selected_finish_point1",
               "selected_direct_route_a", "selected_finish_route_a")
guard <- find_assignment(server_code, "selected_doublet_rows")
if (!is.null(guard)) eval(guard, test_env)
for (name in reactives) eval(find_assignment(server_code, name), test_env)

for (selection in list(NULL, "", "stale", character(0), c("A", "B"))) {
  test_env$input$doublets <- selection
  for (name in reactives) {
    err <- tryCatch(isolate(test_env[[name]]()), error = identity)
    stopifnot(inherits(err, "shiny.silent.error"))
  }
}
test_env$input$doublets <- "A"
for (incomplete in list(NULL, fixture[0, ], fixture[1, ], fixture[c(1, 2, 1), ])) {
  test_env$doubletsTable$data <- incomplete
  for (name in reactives) {
    err <- tryCatch(isolate(test_env[[name]]()), error = identity)
    stopifnot(inherits(err, "shiny.silent.error"))
  }
}
test_env$doubletsTable$data <- fixture
stopifnot(identical(as.numeric(isolate(test_env$selected_start_point())), c(37.6, 55.7)),
          identical(as.numeric(isolate(test_env$selected_finish_point())), c(30.3, 59.9)),
          identical(as.numeric(isolate(test_env$selected_start_point1())), c(30.3, 59.9)),
          identical(as.numeric(isolate(test_env$selected_finish_point1())), c(37.6, 55.7)),
          nrow(isolate(test_env$selected_direct_route())) == 2L,
          nrow(isolate(test_env$selected_intermediate_route())) == 2L,
          nrow(isolate(test_env$selected_back_route())) == 2L,
          isolate(test_env$selected_direct_route_a())$loading_longitude == 37.6,
          isolate(test_env$selected_finish_route_a())$loading_longitude == 30.3)
cat("Circular route regression checks passed: pending, empty, stale, incomplete, ambiguous selections pause; valid pair preserves coordinates.\n")

local({
  route_env <- new.env(parent = environment())
  route_env$ONLINE_DEMO <- TRUE
  bounded <- find_assignment(server_code, "online_osrm_route")
  stopifnot(!is.null(bounded))
  eval(bounded, route_env)
  eval(find_assignment(server_code, "safe_osrm_route"), route_env)
  pts <- data.frame(x = c(37.6, 30.3), y = c(55.7, 59.9))
  previous <- options(osrm.server = "https://routing.openstreetmap.de/", osrm.profile = "car")
  on.exit(options(previous), add = TRUE)
  handles <- list()
  make_handle <- function(...) {
    handles[[length(handles) + 1L]] <<- list(...)
    curl::new_handle(...)
  }
  fetch <- function(url, handle) {
    stopifnot(inherits(handle, "curl_handle"),
              grepl("/routed-car/route/v1/driving/", url, fixed = TRUE),
              grepl("geometries=geojson", url, fixed = TRUE))
    list(status_code = 200L, content = charToRaw(
      '{"code":"Ok","routes":[{"duration":120,"distance":3000,"geometry":{"type":"LineString","coordinates":[[37.6,55.7],[30.3,59.9]]}}]}'))
  }
  route <- route_env$online_osrm_route(pts, fetch = fetch, make_handle = make_handle)
  stopifnot(inherits(route, "sf"), route$duration == 2, route$distance == 3,
            handles[[1]]$timeout == 5, handles[[1]]$connecttimeout == 5,
            identical(unname(sf::st_coordinates(route)[, 1:2]), unname(as.matrix(pts))))

  # A listening socket that never replies exercises libcurl's real timeout.
  port <- httpuv::randomPort()
  listener <- serverSocket(port)
  on.exit(close(listener), add = TRUE)
  options(osrm.server = paste0("http://127.0.0.1:", port, "/"), osrm.profile = "driving")
  started <- proc.time()[["elapsed"]]
  fallback <- suppressMessages(route_env$safe_osrm_route(pts))
  elapsed <- proc.time()[["elapsed"]] - started
  stopifnot(elapsed >= 4, elapsed < 10, inherits(fallback, "sf"),
            is.finite(fallback$distance), fallback$distance > 0,
            identical(unname(sf::st_coordinates(fallback)[, 1:2]), unname(as.matrix(pts))))

  route_env$ONLINE_DEMO <- FALSE
  route_env$online_osrm_route <- function(...) stop("Online client used in local mode")
  route_env$osrmRoute <- function(loc, returnclass, overview) {
    stopifnot(identical(loc, pts), returnclass == "sf", overview == "full")
    "local-osrm-route"
  }
  stopifnot(identical(route_env$safe_osrm_route(pts), "local-osrm-route"))
  route_env$osrmRoute <- function(...) stop("Route unavailable")
  stopifnot(inherits(suppressMessages(route_env$safe_osrm_route(pts)), "sf"))
  cat("Online OSRM timeout PASS: real stalled loopback request stops after ",
      round(elapsed, 1), " seconds; sf response, fallback and local path preserved.\n", sep = "")
})
