dat <- normingTestData()

# the model-based methods need functions that older cNORM versions do not have
skipIfMissing <- function(fun) {
  testthat::skip_if_not(exists(fun, envir = asNamespace("cNORM"), inherits = FALSE),
                        paste0("cNORM::", fun, " is not available"))
}

test_that("Instructions are shown until raw score and grouping variable are selected", {
  res <- runNorming("normingContinuous", dat)
  expect_true(analysisOk(res))
  expect_false(is.null(res[["results"]][["instructions"]]))

  res <- runNorming("normingContinuous", dat, raw = "raw")      # group missing
  expect_true(analysisOk(res))
  expect_false(is.null(res[["results"]][["instructions"]]))
  expect_null(getTable(res, "modelFit"))
})

# ---- Taylor polynomials ------------------------------------------------------

taylorOpts <- list(raw = "raw", group = "age", method = "taylor",
                   k = 3, t = 2, normAge = "9", scale = "T")

test_that("Taylor: model fit, coefficients and norm table are produced", {
  res <- do.call(runNorming, c(list("normingContinuous", dat), taylorOpts))
  expect_true(analysisOk(res))

  fit <- getTable(res, "modelFit")
  expect_equal(length(fit), 1L)
  expect_equal(fit[[1]][["nobs"]], nrow(dat))
  expect_true(fit[[1]][["r2"]] > 0 && fit[[1]][["r2"]] <= 1)    # adjusted R2 of the model
  expect_true(fit[[1]][["rmse"]] > 0)

  coefs <- getTable(res, "modelCoef")
  expect_true(length(coefs) >= 1)
  expect_true(all(is.finite(tableCol(coefs, "est"))))

  tab <- getTable(res, "normTable")
  expect_true(length(tab) > 0)
  norm <- tableCol(tab, "norm")
  perc <- tableCol(tab, "perc")
  expect_true(all(diff(norm) >= 0))                              # monotone in raw score
  expect_true(all(perc >= 0 & perc <= 100))
})

test_that("Taylor: the norm table follows the chosen scale", {
  normRange <- function(scale) {
    opts <- modifyList(taylorOpts, list(scale = scale, range = 3))
    res <- do.call(runNorming, c(list("normingContinuous", dat), opts))
    expect_true(analysisOk(res))
    range(tableCol(getTable(res, "normTable"), "norm"))
  }
  t  <- normRange("T")
  iq <- normRange("IQ")
  expect_true(t[1]  >= 20 - 1e-6 && t[2]  <= 80  + 1e-6)         # 50 +/- 3 * 10
  expect_true(iq[1] >= 55 - 1e-6 && iq[2] <= 145 + 1e-6)         # 100 +/- 3 * 15
  expect_true(max(iq) > max(t))
})

test_that("Taylor: manual term selection fits the requested number of terms", {
  opts <- modifyList(taylorOpts, list(selectionType = "manualSelection", terms = 4))
  res <- do.call(runNorming, c(list("normingContinuous", dat), opts))
  expect_true(analysisOk(res))
  expect_equal(getTable(res, "modelFit")[[1]][["npar"]], 4)
})

test_that("Taylor: the percentile plot is created without error", {
  res <- do.call(runNorming, c(list("normingContinuous", dat), taylorOpts, list(plot = TRUE)))
  expect_true(analysisOk(res))
  expect_false(is.null(res[["results"]][["percentilePlot"]]))
  expect_null(res[["results"]][["percentilePlot"]][["error"]])
})

test_that("Taylor: no norm table without a norm age; model tables can be switched off", {
  opts <- modifyList(taylorOpts, list(normAge = "", model = FALSE))
  res <- do.call(runNorming, c(list("normingContinuous", dat), opts))
  expect_true(analysisOk(res))
  expect_null(getTable(res, "normTable"))
  expect_null(getTable(res, "modelFit"))
  expect_null(getTable(res, "modelCoef"))
})

test_that("Taylor: confidence intervals appear in the norm table on request", {
  opts <- modifyList(taylorOpts, list(includeCI = TRUE, ciLevel = 0.90, reliability = 0.90))
  res <- do.call(runNorming, c(list("normingContinuous", dat), opts))
  expect_true(analysisOk(res))
  tab <- getTable(res, "normTable")
  expect_true(all(c("lCI", "uCI") %in% names(tab[[1]])))
  expect_true(all(tableCol(tab, "lCI") < tableCol(tab, "uCI"), na.rm = TRUE))
})

test_that("Taylor: a grouping variable with a single value is reported as an error", {
  d <- dat
  d$age <- 8
  res <- do.call(runNorming, c(list("normingContinuous", d), taylorOpts))
  expect_true(res[["status"]] %in% c("error", "validationError"))
})

test_that("Too few complete observations are reported as an error", {
  res <- do.call(runNorming, c(list("normingContinuous", dat[1:6, ]), taylorOpts))
  expect_true(res[["status"]] %in% c("error", "validationError"))
})

# ---- Beta-binomial -----------------------------------------------------------

test_that("Beta-binomial: model, parameter table and norm table are produced", {
  skipIfMissing("cnorm.betabinomial")
  res <- runNorming("normingContinuous", dat, raw = "score", group = "age",
                    method = "betabinomial", alpha = 2, beta = 2, itemnumber = 20,
                    normAge = "9", scale = "T")
  expect_true(analysisOk(res))

  fit <- getTable(res, "modelFit")
  expect_equal(length(fit), 1L)
  expect_true(is.finite(fit[[1]][["aic"]]) && is.finite(fit[[1]][["bic"]]))

  coefs <- getTable(res, "modelCoef")
  expect_true(all(c("est", "se", "z", "p") %in% names(coefs[[1]])))
  expect_true(length(getTable(res, "normTable")) > 0)
})

# ---- SHASH -------------------------------------------------------------------

test_that("SHASH: model, parameter table and norm table are produced", {
  skipIfMissing("cnorm.shash")
  res <- runNorming("normingContinuous", dat, raw = "raw", group = "age", method = "shash",
                    muDegree = 2, sigmaDegree = 1, epsilonDegree = 1,
                    fixDelta = TRUE, deltaValue = 1, normAge = "9", scale = "IQ")
  expect_true(analysisOk(res))
  expect_equal(length(getTable(res, "modelFit")), 1L)
  expect_true(all(c("est", "se", "z", "p") %in% names(getTable(res, "modelCoef")[[1]])))
  expect_true(length(getTable(res, "normTable")) > 0)
})

# ---- Conway-Maxwell-Poisson --------------------------------------------------

test_that("CMP: model, parameter table and norm table are produced", {
  skipIfMissing("cnorm.cmp")
  res <- runNorming("normingContinuous", dat, raw = "score", group = "age", method = "cmp",
                    cmpMuDegree = 2, cmpFixNu = TRUE, cmpNuValue = 1,
                    cmpMaxScore = 20, normAge = "9", scale = "T")
  expect_true(analysisOk(res))
  expect_equal(length(getTable(res, "modelFit")), 1L)
  expect_true(length(getTable(res, "normTable")) > 0)
})
