# R/common.R

.cnormInternal <- function(name) utils::getFromNamespace(name, "cNORM")

.normingPlotTheme <- function(legendPos = "right") {
  jaspGraphs::themeJaspRaw(fontsize = 12) +
    ggplot2::theme(
      legend.position = legendPos
    )
}

.normingAddCitations <- function(jaspResults, type = "continuous", options = list()) {
  cites <- c(
    "Lenhard, A., Lenhard, W., Suggate, S., & Segerer, R. (2018). A continuous solution to the norming problem. Assessment, 25(1), 112-125. https://doi.org/10.1177/1073191116656437"
  )
  
  if (type %in% c("continuous", "conventional")) {
    cites <- c(
      cites,
      "Lenhard, W., & Lenhard, A. (2021). Improvement of norm score quality via regression-based continuous norming. Educational and Psychological Measurement, 81(2), 229-261. https://doi.org/10.1177/0013164420928457",
      "Gary, S., Lenhard, W., & Lenhard, A. (2021). Modelling norm scores with the cNORM package in R. Psych, 3(3), 501-521. https://doi.org/10.3390/psych3030033"
    )
  }
  
  if (type == "continuous" && !is.null(options$method)) {
    if (options$method %in% c("betabinomial", "shash")) {
      cites <- c(
        cites,
        "Lenhard, A., Lenhard, W., & Gary, S. (2019). Continuous norming of psychometric tests: A simulation study of parametric and semi-parametric approaches. PLoS ONE, 14(9), e0222279. https://doi.org/10.1371/journal.pone.0222279"
      )
    }
    if (options$method == "shash") {
      cites <- c(
        cites,
        "Jones, M. C., & Pewsey, A. (2009). Sinh-arcsinh distributions. Biometrika, 96(4), 761-780. https://doi.org/10.1093/biomet/asp053",
        "Laurenz, R., & Lenhard, W. (2025). Comparison of cNORM and LMS methods for estimating reference percentile curves from biometric data. Scientific Reports, 15, 29580. https://doi.org/10.1038/s41598-025-29580-4"
      )
    } else if (options$method == "cmp") {
      cites <- c(
        cites,
        "Conway, R. W., & Maxwell, W. L. (1962). A queuing model with state dependent service rates. Journal of Industrial Engineering, 13(2), 132-136.",
        "Huang, A. (2017). Mean-parametrized Conway-Maxwell-Poisson regression models for dispersed counts. Statistical Modelling, 17(6), 359-380. https://doi.org/10.1177/1471082X17697749"
      )
    }
  }
  
  usesWeights <- (!is.null(options$weights) && nzchar(options$weights)) ||
    (type == "preparation" && length(options$stratVars) > 0L)
  if (usesWeights) {
    cites <- c(
      cites,
      "Gary, S., Lenhard, W., Lenhard, A., & Herzberg, D. S. (2023). A tutorial on automatic post-stratification and weighting in conventional and regression-based norming of psychometric tests. Behavior Research Methods, 56, 4632-4642. https://doi.org/10.3758/s13428-023-02207-0",
      "Gary, S., Lenhard, A., Lenhard, W., & Herzberg, D. S. (2023). Reducing the bias of norm scores in non-representative samples: Weighting as an adjunct to continuous norming methods. Assessment, 30(8), 2491-2509. https://doi.org/10.1177/10731911231153832"
    )
  }
  
  jaspResults$addCitation(unique(cites))
}

.saveScaleColumn <- function(jaspResults, key, colName, values, deps) {
  if (is.null(colName)) return()
  colName <- trimws(colName)
  if (!nzchar(colName)) return()
  if (!jaspBase::jaspResultsCalledFromJasp()) return()
  if (!is.null(jaspResults[[key]])) return()
  
  tryCatch({
    if (!isTRUE(tryCatch(jaspBase:::.getColumnExists(colName), error = function(e) FALSE))) {
      jaspBase::createColumns(colName)
    }
    colObj <- jaspBase::createJaspColumn(columnName = colName)
    colObj$dependOn(deps)
    jaspResults[[key]] <- colObj
    colObj$setScale(as.numeric(values))
  }, error = function(e) {
    warnKey <- paste0(key, "_warn")
    if (is.null(jaspResults[[warnKey]])) {
      msg <- jaspBase::createJaspHtml(title = gettext("Column Save Notice"))
      msg$dependOn(deps)
      msg$text <- gettextf(
        "<p>Could not save column <b>%s</b> to dataset: %s</p>",
        colName, jaspBase::decodeColNames(conditionMessage(e))
      )
      jaspResults[[warnKey]] <- msg
    }
  })
}

.resolveScale <- function(scale, sdRange = 3.0) {
  if (scale == "Wechsler") {
    m1 <- 10; sd1 <- 3
  } else if (scale == "PISA") {
    m1 <- 500; sd1 <- 100
  } else if (scale == "T") {
    m1 <- 50; sd1 <- 10
  } else if (scale == "IQ") {
    m1 <- 100; sd1 <- 15
  } else {
    m1 <- 0; sd1 <- 1
  }
  list(
    m1         = m1,
    sd1        = sd1,
    scaleParam = c(m1, sd1),
    minNorm    = m1 - sdRange * sd1,
    maxNorm    = m1 + sdRange * sd1
  )
}

.normingAsNumeric <- function(x, varName) {
  out <- if (is.factor(x)) {
    suppressWarnings(as.numeric(as.character(x)))
  } else {
    suppressWarnings(as.numeric(x))
  }
  if (all(is.na(out)) && !all(is.na(x))) {
    jaspBase::.quitAnalysis(gettextf(
      "The variable '%s' must contain numeric values.",
      jaspBase::decodeColNames(varName)
    ))
  }
  out
}

.appendCI <- function(df, m1, sd1, ciVal, relVal) {
  if (!is.null(ciVal) && !is.null(relVal)) {
    se    <- stats::qnorm(1 - ((1 - ciVal) / 2)) * sqrt(relVal * (1 - relVal))
    z     <- (df$norm - m1) / sd1
    zPred <- relVal * z
    df$lowerCI    <- (zPred - se) * sd1 + m1
    df$upperCI    <- (zPred + se) * sd1 + m1
    df$lowerCI_PR <- stats::pnorm(zPred - se) * 100
    df$upperCI_PR <- stats::pnorm(zPred + se) * 100
  } else {
    df$lowerCI    <- NA_real_
    df$upperCI    <- NA_real_
    df$lowerCI_PR <- NA_real_
    df$upperCI_PR <- NA_real_
  }
  df
}

.parseMarginsString <- function(specStr) {
  if (is.null(specStr) || !nzchar(trimws(specStr))) return(NULL)
  blocks <- unlist(strsplit(specStr, ";", fixed = TRUE))
  rows <- list()
  for (b in blocks) {
    b <- trimws(b)
    if (!nzchar(b)) next
    parts <- unlist(strsplit(b, ":", fixed = TRUE))
    if (length(parts) != 2L) return(NULL)
    varName <- trimws(parts[1])
    if (!nzchar(varName)) return(NULL)
    pairs <- unlist(strsplit(parts[2], ",", fixed = TRUE))
    for (p in pairs) {
      p <- trimws(p)
      if (!nzchar(p)) next
      kv <- unlist(strsplit(p, "=", fixed = TRUE))
      if (length(kv) != 2L) return(NULL)
      lvlName  <- trimws(kv[1])
      shareVal <- suppressWarnings(as.numeric(trimws(kv[2])))
      if (!nzchar(lvlName) || is.na(shareVal)) return(NULL)
      rows[[length(rows) + 1L]] <- data.frame(
        variables = varName,
        levels    = lvlName,
        share     = shareVal,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0L) return(NULL)
  do.call(rbind, rows)
}

.normingInstructions <- function(jaspResults, type = "continuous") {
  if (!is.null(jaspResults[["instructions"]])) return()
  html <- jaspBase::createJaspHtml(title = gettext("Instructions"))
  html$position <- 0
  
  msg <- if (type == "continuous") {
    gettext("<p>Please select a <b>Raw score variable</b> and an <b>Explanatory / grouping variable</b> (e.g., age or grade) to begin continuous norm modeling.</p>")
  } else if (type == "conventional") {
    gettext("<p>Please select a <b>Raw score variable</b> to compute single-group norm scores.</p>")
  } else {
    gettext("<p>Please select a <b>Continuous Explanatory Variable</b> to form norm groups, or <b>Stratification Variables</b> to compute raking weights.</p>")
  }
  
  html$text <- msg
  jaspResults[["instructions"]] <- html
}