dat <- normingTestData()

test_that("Instructions are shown when neither age nor stratification variables are selected", {
  res <- runNorming("normingPreparation", dat)
  expect_true(analysisOk(res))
  expect_false(is.null(res[["results"]][["instructions"]]))
  expect_null(getTable(res, "groupTab"))
  expect_null(getTable(res, "marginsTab"))
})

# ---- Norm groups -------------------------------------------------------------

test_that("Norm groups: the requested number of groups is formed and covers the sample", {
  res <- runNorming("normingPreparation", dat, age = "age", nGroups = 4)
  expect_true(analysisOk(res))

  tab <- getTable(res, "groupTab")
  expect_equal(length(tab), 4L)
  expect_equal(sum(tableCol(tab, "n")), nrow(dat))
  expect_equal(sum(tableCol(tab, "prop")), 100, tolerance = 1e-6)
  expect_true(all(diff(tableCol(tab, "midpoint")) > 0))
  expect_true(all(tableCol(tab, "minVal") <= tableCol(tab, "maxVal")))
  expect_equal(min(tableCol(tab, "minVal")), min(dat$age))
  expect_equal(max(tableCol(tab, "maxVal")), max(dat$age))

  expect_false(is.null(res[["results"]][["groupPlot"]]))
  expect_null(res[["results"]][["groupPlot"]][["error"]])
})

test_that("Norm groups: automatic group number (0) works", {
  res <- runNorming("normingPreparation", dat, age = "age", nGroups = 0)
  expect_true(analysisOk(res))
  tab <- getTable(res, "groupTab")
  expect_true(length(tab) >= 2)
  expect_equal(sum(tableCol(tab, "n")), nrow(dat))
})

test_that("Norm groups: equidistant intervals work and missing ages are ignored", {
  d <- dat
  d$age[1:20] <- NA
  res <- runNorming("normingPreparation", d, age = "age", nGroups = 3, equidistant = TRUE)
  expect_true(analysisOk(res))
  tab <- getTable(res, "groupTab")
  expect_equal(length(tab), 3L)
  expect_equal(sum(tableCol(tab, "n")), sum(!is.na(d$age)))
})

test_that("Norm groups: too few observations are reported as an error", {
  res <- runNorming("normingPreparation", data.frame(age = c(6, 7, 8)), age = "age")
  expect_true(res[["status"]] %in% c("error", "validationError"))
})

# ---- Raking ------------------------------------------------------------------

test_that("Raking: marginals table lists every level with correct sample shares", {
  res <- runNorming("normingPreparation", dat, stratVars = list("sex", "migration"),
                    margins = "sex: 1=0.5, 2=0.5; migration: 0=0.5, 1=0.5")
  expect_true(analysisOk(res))

  tab <- getTable(res, "marginsTab")
  expect_equal(length(tab), 4L)                                  # 2 levels x 2 variables
  expect_equal(sum(tableCol(tab, "n")), 2 * nrow(dat))
  expect_equal(tableCol(tab, "sPr"), rep(0.5, 4))
  expect_equal(tableCol(tab, "tPr"), rep(0.5, 4))
})

test_that("Raking: targets equal to the sample give (near) unit weights and no design effect", {
  res <- runNorming("normingPreparation", dat, stratVars = list("sex", "migration"),
                    margins = "sex: 1=0.5, 2=0.5; migration: 0=0.5, 1=0.5")
  expect_true(analysisOk(res))
  w <- getTable(res, "weightSummaryTab")
  expect_equal(length(w), 1L)
  expect_equal(w[[1]][["nValid"]], nrow(dat))
  expect_equal(w[[1]][["maxW"]], w[[1]][["minW"]], tolerance = 1e-6)
  expect_equal(w[[1]][["deff"]], 1, tolerance = 1e-6)
  expect_equal(w[[1]][["nEff"]], nrow(dat), tolerance = 1e-6)
})

test_that("Raking: unbalanced targets produce varying weights and a design effect above 1", {
  res <- runNorming("normingPreparation", dat, stratVars = list("sex"),
                    margins = "sex: 1=0.7, 2=0.3")
  expect_true(analysisOk(res))
  w <- getTable(res, "weightSummaryTab")[[1]]
  expect_true(w[["maxW"]] > w[["minW"]])
  expect_true(w[["deff"]] > 1)
  expect_true(w[["nEff"]] < nrow(dat))
})

test_that("Raking: stratification variables without target proportions still give a table", {
  res <- runNorming("normingPreparation", dat, stratVars = list("sex"), margins = "")
  expect_true(analysisOk(res))
  expect_equal(length(getTable(res, "marginsTab")), 2L)
})

test_that("Age groups and raking can be combined", {
  res <- runNorming("normingPreparation", dat, age = "age", nGroups = 3,
                    stratVars = list("sex"), margins = "sex: 1=0.5, 2=0.5")
  expect_true(analysisOk(res))
  expect_false(is.null(getTable(res, "groupTab")))
  expect_false(is.null(getTable(res, "marginsTab")))
})

test_that("Raking: proportions that do not sum to 1 are reported as an error", {
  res <- runNorming("normingPreparation", dat, stratVars = list("sex"),
                    margins = "sex: 1=0.7, 2=0.7")
  expect_true(res[["status"]] %in% c("error", "validationError"))
  
  res <- runNorming("normingPreparation", dat, stratVars = list("sex"),
                    margins = "sex: 1=0.52, 2=0.48")            # sums to 1: fine
  expect_true(analysisOk(res))
})

test_that("Raking: unreadable or unknown variables in the proportions are reported as an error", {
  res <- runNorming("normingPreparation", dat, stratVars = list("sex"), margins = "sex 1=0.5")
  expect_true(res[["status"]] %in% c("error", "validationError"))
  
  res <- runNorming("normingPreparation", dat, stratVars = list("sex"),
                    margins = "migration: 0=0.5, 1=0.5")
  expect_true(res[["status"]] %in% c("error", "validationError"))
})