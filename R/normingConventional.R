# R/normingConventional.R

#' @export
normingConventional <- function(jaspResults, dataset, options) {
  ready <- (options$raw != "")
  if (!ready) {
    .normingInstructions(jaspResults, "conventional")
    return()
  }
  
  dataComplete <- .normingReadSingleData(dataset, options)
  scaleInfo    <- .resolveScale(options$scale, as.numeric(options$range))
  
  # Rank & optionally fit polynomial smoothing (cached via jaspState)
  convState  <- .getConventionalState(jaspResults, dataComplete, scaleInfo, options)
  dataRanked <- convState$dataRanked
  model      <- convState$model
  
  # 1. Conversion Plot
  .createConventionalPlot(jaspResults, dataRanked, model, scaleInfo, options)
  
  # 2. Model Fit & Regression Coefficients Tables
  if (isTRUE(options$k) && isTRUE(options$model) && !is.null(model)) {
    .createConventionalModelTables(jaspResults, model, nrow(dataRanked), options)
  }
  
  # 3. Norm Score Table
  .createConventionalTable(jaspResults, dataRanked, model, scaleInfo, options)
}

.conventionalModelDeps <- function() {
  c("raw", "weights", "scale", "descend", "k", "kPower", "selectionType", "terms")
}

# --- Read Data ---
.normingReadSingleData <- function(dataset, options) {
  vars <- options$raw
  if (nzchar(options$weights)) vars <- c(vars, options$weights)
  
  if (is.null(dataset)) {
    df <- jaspBase::.readDataSetToEnd(columns.as.numeric = vars)
  } else {
    df <- dataset[, vars, drop = FALSE]
  }
  
  x <- .normingAsNumeric(df[[options$raw]], options$raw)
  w <- if (nzchar(options$weights)) .normingAsNumeric(df[[options$weights]], options$weights) else NULL
  
  cleanDF <- if (!is.null(w)) data.frame(raw = x, weights = w) else data.frame(raw = x)
  cleanDF <- stats::na.omit(cleanDF)
  
  if (nrow(cleanDF) < 5L)
    jaspBase::.quitAnalysis(gettext("At least 5 valid observations are required."))
  if (length(unique(cleanDF$raw)) < 2L)
    jaspBase::.quitAnalysis(gettext("Raw score must contain at least 2 distinct values."))
  if (!is.null(w) && any(!is.finite(cleanDF$weights) | cleanDF$weights <= 0))
    jaspBase::.quitAnalysis(gettext("All case weights must be positive finite numbers."))
  
  return(cleanDF)
}

# --- State Caching for Ranking & Polynomial Smoothing ---
.getConventionalState <- function(jaspResults, dataComplete, scaleInfo, options) {
  if (!is.null(jaspResults[["convState"]])) {
    return(jaspResults[["convState"]]$object)
  }
  
  hasWeights <- "weights" %in% names(dataComplete)
  dataRanked <- if (hasWeights) {
    cNORM::rankByGroup(dataComplete, raw = dataComplete$raw, group = FALSE,
                       scale = scaleInfo$scaleParam, descend = isTRUE(options$descend),
                       weights = dataComplete$weights)
  } else {
    cNORM::rankByGroup(dataComplete, raw = dataComplete$raw, group = FALSE,
                       scale = scaleInfo$scaleParam, descend = isTRUE(options$descend))
  }
  
  model <- NULL
  if (isTRUE(options$k)) {
    kPower     <- max(1L, as.integer(options$kPower))
    dataRanked <- cNORM::computePowers(dataRanked, k = kPower, norm = "normValue")
    useManual  <- identical(options$selectionType, "manualSelection")
    terms      <- if (useManual) min(max(1L, as.integer(options$terms)), kPower) else NULL
    wVec       <- if (hasWeights && "weights" %in% names(dataRanked)) dataRanked$weights else NULL
    
    model <- tryCatch({
      if (useManual) {
        cNORM::bestModel(dataRanked, raw = "raw", k = kPower, terms = terms, weights = wVec, plot = FALSE)
      } else {
        cNORM::bestModel(dataRanked, raw = "raw", k = kPower, weights = wVec, plot = FALSE)
      }
    }, error = function(e) {
      jaspBase::.quitAnalysis(gettextf(
        "Polynomial smoothing failed: %s",
        jaspBase::decodeColNames(conditionMessage(e))
      ))
    })
  }
  
  res <- list(dataRanked = dataRanked, model = model)
  jaspResults[["convState"]] <- jaspBase::createJaspState(res, dependencies = .conventionalModelDeps())
  return(res)
}

# --- Plot ---
.createConventionalPlot <- function(jaspResults, dataRanked, model, scaleInfo, options) {
  if (!is.null(jaspResults[["plot"]])) return()
  
  plot <- jaspBase::createJaspPlot(title = gettext("Norm Score Conversion"), width = 600, height = 450)
  plot$dependOn(c(.conventionalModelDeps(), "plotting", "range"))
  plot$position <- 1
  
  plotNormScore <- identical(options$plotting, "Norm Score")
  yLabel   <- if (plotNormScore) gettext("Norm Score") else gettext("Percentile")
  obsLabel <- gettext("Observed")
  modLabel <- gettext("Model")
  
  tab1 <- unique(data.frame(
    raw    = as.numeric(dataRanked$raw),
    y      = if (plotNormScore) as.numeric(dataRanked$normValue) else as.numeric(dataRanked$percentile) * 100,
    series = factor(obsLabel, levels = c(obsLabel, modLabel))
  ))
  tab1 <- tab1[order(tab1$raw), ]
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = tab1,
      ggplot2::aes(x = raw, y = y, color = series),
      size = 2
    )
  
  if (!is.null(model)) {
    L_seq    <- seq(max(scaleInfo$minNorm, model$minL1), min(scaleInfo$maxNorm, model$maxL1), length.out = 200)
    raw_seq  <- vapply(L_seq, function(l) as.numeric(cNORM::predictRaw(l, 0, model$coefficients)), numeric(1))
    perc_seq <- stats::pnorm((L_seq - scaleInfo$m1) / scaleInfo$sd1) * 100
    regData  <- data.frame(
      raw    = raw_seq,
      y      = if (plotNormScore) L_seq else perc_seq,
      series = factor(modLabel, levels = c(obsLabel, modLabel))
    )
    
    colorVals <- stats::setNames(c("black", "#d95f02"), c(obsLabel, modLabel))
    p <- p +
      ggplot2::geom_line(
        data = regData,
        ggplot2::aes(x = raw, y = y, color = series),
        linewidth = 1
      ) +
      ggplot2::scale_color_manual(name = "", values = colorVals)
  } else {
    tab1$series <- factor(obsLabel, levels = obsLabel)
    colorVals   <- stats::setNames("black", obsLabel)
    p <- ggplot2::ggplot() +
      ggplot2::geom_point(
        data = tab1,
        ggplot2::aes(x = raw, y = y, color = series),
        size = 2
      ) +
      ggplot2::scale_color_manual(name = "", values = colorVals)
  }
  
  p <- p +
    ggplot2::labs(x = jaspBase::decodeColNames(options$raw), y = yLabel) +
    .normingPlotTheme(legendPos = "top")
  
  plot$plotObject <- p
  jaspResults[["plot"]] <- plot
}

# --- Model Fit Tables ---
.createConventionalModelTables <- function(jaspResults, model, nObs, options) {
  deps <- c(.conventionalModelDeps(), "model")
  
  if (is.null(jaspResults[["modelFit"]])) {
    fitTab <- jaspBase::createJaspTable(title = gettext("Model Fit"))
    fitTab$dependOn(deps)
    fitTab$position <- 2
    
    fitTab$addColumnInfo(name = "nobs",  title = gettext("N"),           type = "integer")
    fitTab$addColumnInfo(name = "terms", title = gettext("Terms"),       type = "integer")
    fitTab$addColumnInfo(name = "rmse",  title = gettext("RMSE"),        type = "number")
    fitTab$addColumnInfo(name = "r2adj", title = gettext("Adj. R\u00B2"), type = "number")
    fitTab$addColumnInfo(name = "bic",   title = gettext("BIC"),         type = "number")
    
    fitTab$addRows(list(list(
      nobs  = as.integer(nObs),
      terms = as.integer(model$ideal.model),
      rmse  = model$rmse,
      r2adj = model$subset$adjr2[model$ideal.model],
      bic   = model$subset$bic[model$ideal.model]
    )))
    jaspResults[["modelFit"]] <- fitTab
  }
  
  if (is.null(jaspResults[["modelCoef"]])) {
    coefTab <- jaspBase::createJaspTable(title = gettext("Regression Coefficients"))
    coefTab$dependOn(deps)
    coefTab$position <- 3
    
    coefTab$addColumnInfo(name = "term", title = gettext("Term"),     type = "string")
    coefTab$addColumnInfo(name = "est",  title = gettext("Estimate"), type = "number")
    
    coefs <- model$coefficients
    tNames <- if (!is.null(names(coefs))) names(coefs) else paste0("Term_", seq_along(coefs))
    outDF <- data.frame(
      term = tNames,
      est  = as.numeric(coefs),
      stringsAsFactors = FALSE
    )
    coefTab$setData(outDF)
    jaspResults[["modelCoef"]] <- coefTab
  }
}

# --- Norm Score Table ---
.createConventionalTable <- function(jaspResults, dataRanked, model, scaleInfo, options) {
  if (!is.null(jaspResults[["normTable"]])) return()
  
  table <- jaspBase::createJaspTable(title = gettext("Norm Score Table"))
  table$dependOn(c(.conventionalModelDeps(), "range", "minRaw", "maxRaw",
                   "stepping", "includeCI", "ciLevel", "reliability"))
  table$position <- 4
  
  table$addColumnInfo(name = "raw",  title = gettext("Raw Score"),  type = "number")
  table$addColumnInfo(name = "norm", title = gettext("Norm Score"), type = "number")
  table$addColumnInfo(name = "perc", title = gettext("Percentile"), type = "number")
  
  hasCI <- isTRUE(options$includeCI)
  if (hasCI) {
    table$addColumnInfo(name = "lCI",    title = gettext("Lower CI (Norm)"), type = "number")
    table$addColumnInfo(name = "uCI",    title = gettext("Upper CI (Norm)"), type = "number")
    table$addColumnInfo(name = "lCI_PR", title = gettext("Lower CI (PR)"),   type = "number")
    table$addColumnInfo(name = "uCI_PR", title = gettext("Upper CI (PR)"),   type = "number")
  }
  
  ciVal  <- if (hasCI) as.numeric(options$ciLevel) else NULL
  relVal <- if (hasCI) as.numeric(options$reliability) else NULL
  
  tabErr <- NULL
  tab <- tryCatch({
    if (!is.null(model)) {
      minR    <- if (options$maxRaw > options$minRaw) as.numeric(options$minRaw) else NULL
      maxR    <- if (options$maxRaw > options$minRaw) as.numeric(options$maxRaw) else NULL
      stepVal <- max(0.01, as.numeric(options$stepping))
      cNORM::rawTable(0, model, minNorm = scaleInfo$minNorm, maxNorm = scaleInfo$maxNorm,
                      minRaw = minR, maxRaw = maxR, step = stepVal,
                      CI = ciVal, reliability = relVal)
    } else {
      t0 <- unique(data.frame(
        raw        = dataRanked$raw,
        norm       = dataRanked$normValue,
        percentile = dataRanked$percentile * 100
      ))
      t0 <- t0[order(t0$raw), ]
      .appendCI(t0, scaleInfo$m1, scaleInfo$sd1, ciVal, relVal)
    }
  }, error = function(e) {
    tabErr <<- conditionMessage(e)
    NULL
  })
  
  if (!is.null(tabErr)) {
    table$addFootnote(
      gettextf("Could not compute norm table: %s", jaspBase::decodeColNames(tabErr)),
      symbol = gettext("<em>Error:</em>")
    )
  } else if (!is.null(tab) && nrow(tab) > 0L) {
    rawC  <- if ("raw" %in% names(tab)) "raw" else "x"
    normC <- if ("norm" %in% names(tab)) "norm" else "normValue"
    percC <- if ("percentile" %in% names(tab)) "percentile" else "Percentile"
    
    outDF <- data.frame(
      raw  = as.numeric(tab[[rawC]]),
      norm = as.numeric(tab[[normC]]),
      perc = as.numeric(tab[[percC]])
    )
    if (hasCI) {
      hasCols <- "lowerCI" %in% names(tab)
      outDF$lCI    <- if (hasCols) as.numeric(tab$lowerCI)    else NA_real_
      outDF$uCI    <- if (hasCols) as.numeric(tab$upperCI)    else NA_real_
      outDF$lCI_PR <- if (hasCols) as.numeric(tab$lowerCI_PR) else NA_real_
      outDF$uCI_PR <- if (hasCols) as.numeric(tab$upperCI_PR) else NA_real_
    }
    table$setData(outDF)
  }
  
  jaspResults[["normTable"]] <- table
}