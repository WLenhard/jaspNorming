# Shared helpers for the jaspNorming tests (sourced automatically by testthat).
# The tests are property-based: they check structure, ranges and relations
# instead of hard-coded numbers, so they do not depend on exact cNORM output.

# Deterministic synthetic data: n must be a multiple of 4.
normingTestData <- function(n = 600, seed = 2026) {
  set.seed(seed)
  age   <- round(runif(n, 6, 12), 2)
  raw   <- pmax(0, round(rnorm(n, mean = 10 + 4 * (age - 6), sd = 4)))
  score <- rbinom(n, 20, plogis(-1 + 0.4 * (age - 6)))      # count data, max 20
  data.frame(
    raw       = raw,
    age       = age,
    score     = score,
    weight    = 1,
    sex       = factor(rep(1:2, length.out = n)),             # 50/50
    migration = factor(rep(c(0, 0, 1, 1), length.out = n))    # 50/50, crossed with sex
  )
}

# Run an analysis with default QML options, overridden by the arguments in `...`.
runNorming <- function(analysis, data, ...) {
  options <- jaspTools::analysisOptions(analysis)
  options <- modifyList(options, list(...))
  jaspTools::runAnalysis(analysis, data, options)
}

# TRUE if the analysis finished without error.
analysisOk <- function(results) identical(results[["status"]], "complete")

# Table data (list of row-lists) of a result element, or NULL if it does not exist.
getTable <- function(results, name) results[["results"]][[name]][["data"]]

# Numeric vector of one column of a table (NA where a cell is missing/empty).
tableCol <- function(tbl, field) {
  vapply(tbl, function(row) {
    v <- row[[field]]
    if (is.null(v)) NA_real_ else suppressWarnings(as.numeric(v))
  }, numeric(1))
}
