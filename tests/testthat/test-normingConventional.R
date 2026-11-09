dat <- normingTestData()

test_that("Instructions are shown when no raw score variable is selected", {
  results <- runNorming("normingConventional", dat)
  expect_true(analysisOk(results))
  expect_false(is.null(results[["results"]][["instructions"]]))
  expect_null(getTable(results, "normTable"))
})

test_that("Rankit norming without smoothing gives a consistent norm table", {
  results <- runNorming("normingConventional", dat, raw = "raw", k = FALSE, scale = "T")
  expect_true(analysisOk(results))

  tab <- getTable(results, "normTable")
  expect_false(is.null(tab))
  expect_equal(length(tab), length(unique(dat$raw)))   # one row per observed raw score

  raw  <- tableCol(tab, "raw")
  norm <- tableCol(tab, "norm")
  perc <- tableCol(tab, "perc")
  expect_true(all(diff(raw) > 0))                      # sorted, unique raw scores
  expect_true(all(diff(norm) >= 0))                    # higher raw -> higher norm score
  expect_true(all(perc >= 0 & perc <= 100))
  expect_true(all(diff(perc) >= 0))

  # no regression requested -> no model tables
  expect_null(getTable(results, "modelFit"))
  expect_null(getTable(results, "modelCoef"))
})

test_that("Inverting the ranking order reverses the norm score order", {
  results <- runNorming("normingConventional", dat, raw = "raw", k = FALSE, descend = TRUE)
  expect_true(analysisOk(results))
  norm <- tableCol(getTable(results, "normTable"), "norm")
  expect_true(all(diff(norm) <= 0))
})

test_that("The norm scale is applied", {
  medianNorm <- function(scale) {
    res <- runNorming("normingConventional", dat, raw = "raw", k = FALSE, scale = scale)
    expect_true(analysisOk(res))
    median(tableCol(getTable(res, "normTable"), "norm"))
  }
  expect_true(abs(medianNorm("T")  - 50)  < 10)
  expect_true(abs(medianNorm("IQ") - 100) < 15)
  expect_true(abs(medianNorm("z")  - 0)   < 1)
})

test_that("Polynomial smoothing produces model fit, coefficients and a norm table", {
  results <- runNorming("normingConventional", dat, raw = "raw", k = TRUE, kPower = 4,
                        selectionType = "automaticSelection", model = TRUE, scale = "T")
  expect_true(analysisOk(results))

  fit <- getTable(results, "modelFit")
  expect_equal(length(fit), 1L)
  expect_equal(fit[[1]][["nobs"]], nrow(dat))
  expect_true(fit[[1]][["terms"]] >= 1)
  expect_true(fit[[1]][["r2adj"]] > 0 && fit[[1]][["r2adj"]] <= 1)

  coefs <- getTable(results, "modelCoef")
  expect_true(length(coefs) >= 1)
  expect_true(all(is.finite(tableCol(coefs, "est"))))

  tab <- getTable(results, "normTable")
  expect_true(length(tab) > 0)
  expect_true(all(is.finite(tableCol(tab, "norm"))))
})

test_that("Manual term selection fits the requested number of terms", {
  results <- runNorming("normingConventional", dat, raw = "raw", k = TRUE, kPower = 4,
                        selectionType = "manualSelection", terms = 3, model = TRUE)
  expect_true(analysisOk(results))
  expect_equal(getTable(results, "modelFit")[[1]][["terms"]], 3)
})

test_that("Confidence intervals are only added on request and bracket sensible values", {
  without <- runNorming("normingConventional", dat, raw = "raw", k = FALSE, includeCI = FALSE)
  expect_true(analysisOk(without))
  expect_false("lCI" %in% names(getTable(without, "normTable")[[1]]))

  with <- runNorming("normingConventional", dat, raw = "raw", k = FALSE,
                     includeCI = TRUE, ciLevel = 0.90, reliability = 0.90)
  expect_true(analysisOk(with))
  tab <- getTable(with, "normTable")
  expect_true(all(c("lCI", "uCI", "lCI_PR", "uCI_PR") %in% names(tab[[1]])))
  expect_true(all(tableCol(tab, "lCI") < tableCol(tab, "uCI")))
  expect_true(all(tableCol(tab, "lCI_PR") < tableCol(tab, "uCI_PR")))
})

test_that("Missing values are removed and weights are accepted", {
  d <- dat
  d$raw[1:10] <- NA
  results <- runNorming("normingConventional", d, raw = "raw", weights = "weight", k = FALSE)
  expect_true(analysisOk(results))
  expect_true(length(getTable(results, "normTable")) > 0)
})

test_that("Too few observations or no variance is reported as an error", {
  tiny <- data.frame(raw = c(1, 2, 3))
  res <- runNorming("normingConventional", tiny, raw = "raw", k = FALSE)
  expect_true(res[["status"]] %in% c("error", "validationError"))

  constant <- data.frame(raw = rep(5, 50))
  res <- runNorming("normingConventional", constant, raw = "raw", k = FALSE)
  expect_true(res[["status"]] %in% c("error", "validationError"))
})
