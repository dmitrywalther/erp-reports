# Undefined ratios are missing values, not zero performance. Convert PostgreSQL
# bigint counters before division: bit64 warns on 0/0 in an empty report.
report_ratio <- function(numerator, denominator) {
  result <- as.numeric(numerator) / as.numeric(denominator)
  result[!is.finite(result)] <- NA_real_
  result
}
