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
  
  # Rank single group (Rankit)
  hasWeights <- "weights" %in% names(dataComplete)
  dataRanked <- if (hasWeights) {
    cNORM::rankByGroup(dataComplete, raw = dataComplete$raw, group = FALSE,
                       scale = scaleInfo$scaleParam, descend = isTRUE(options$descend),
                       weights = dataComplete$weights)
  } else {
    cNORM::rankByGroup(dataComplete, raw = dataComplete$raw, group = FALSE,
                       scale = scaleInfo$scaleParam, descend = isTRUE(options$descend))
  }
  
  # Polynomial Regression (if selected)
  regression <- isTRUE(options$k)
  model <- NULL
  if (regression) {
    kPower <- as.integer(options$kPower)
    dataRanked <- cNORM::computePowers(dataRanked, k = kPower, norm = "normValue")
    useManual <- identical(options$selectionType, "manualSelection")
    terms <- if (useManual) as.integer(options$terms) else NULL
    wVec <- if (hasWeights) dataRanked$weights else NULL
    
    model <- if (useManual) {
      cNORM::bestModel(dataRanked, raw = "raw", k = kPower, terms = terms, weights = wVec, plot = FALSE)
    } else {
      cNORM::bestModel(dataRanked, raw = "raw", k = kPower, weights = wVec, plot = FALSE)
    }
  }
  
  # 1. Conversion Plot
  .createConventionalPlot(jaspResults, dataRanked, model, scaleInfo, options)
  
  # 2. Model Fit & Regression Coefficients Tables
  if (regression && isTRUE(options$model)) {
    .createConventionalModelTables(jaspResults, model, nrow(dataRanked), options)
  }
  
  # 3. Norm Score Table
  .createConventionalTable(jaspResults, dataRanked, model, scaleInfo, options)
}

# --- Read Data ---
.normingReadSingleData <- function(dataset, options) {
  vars <- options$raw
  if (nzchar(options$weights)) vars <- c(vars, options$weights)
  
  if (is.null(dataset)) {
    df <- jaspBase::readDataSetToEnd(columns.as.numeric = vars)
  } else {
    df <- dataset[, vars, drop = FALSE]
  }
  
  x <- as.numeric(df[[options$raw]])
  w <- if (nzchar(options$weights)) as.numeric(df[[options$weights]]) else NULL
  
  cleanDF <- if (!is.null(w)) data.frame(raw = x, weights = w) else data.frame(raw = x)
  cleanDF <- na.omit(cleanDF)
  
  if (nrow(cleanDF) < 5) jaspBase::reject(gettext("At least 5 valid observations are required."))
  if (length(unique(cleanDF$raw)) < 2) jaspBase::reject(gettext("Raw score must contain at least 2 distinct values."))
  return(cleanDF)
}

# --- Plot ---
.createConventionalPlot <- function(jaspResults, dataRanked, model, scaleInfo, options) {
  if (!is.null(jaspResults[["plot"]])) return()
  
  plot <- jaspBase::createJaspPlot(title = gettext("Norm Score Conversion"), width = 600, height = 450)
  plot$dependOn(c("raw", "weights", "scale", "descend", "k", "kPower", "selectionType", "terms",
                  "plotting", "range", "minRaw", "maxRaw", "stepping", "includeCI", "ciLevel", "reliability"))
  plot$position <- 1
  
  plotNormScore <- identical(options$plotting, "Norm Score")
  yCol   <- if (plotNormScore) "normValue" else "percentile"
  yLabel <- if (plotNormScore) gettext("Norm Score") else gettext("Percentile")
  
  tab1 <- unique(data.frame(raw = dataRanked$raw, normValue = dataRanked$normValue, percentile = dataRanked$percentile * 100))
  tab1 <- tab1[order(tab1$raw), ]
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_point(data = tab1, ggplot2::aes(x = raw, y = .data[[yCol]], color = "Observed"), size = 2)
  
  if (!is.null(model)) {
    L_seq <- seq(max(scaleInfo$minNorm, model$minL1), min(scaleInfo$maxNorm, model$maxL1), length.out = 200)
    raw_seq <- vapply(L_seq, function(l) as.numeric(cNORM::predictRaw(l, 0, model$coefficients)), numeric(1))
    perc_seq <- stats::pnorm((L_seq - scaleInfo$m1) / scaleInfo$sd1) * 100
    regData <- data.frame(raw = raw_seq, normValue = L_seq, percentile = perc_seq)
    
    p <- p + ggplot2::geom_line(data = regData, ggplot2::aes(x = raw, y = .data[[yCol]], color = "Model"), linewidth = 1) +
      ggplot2::scale_color_manual(name = "", values = c("Observed" = "black", "Model" = "#d95f02"))
  } else {
    p <- p + ggplot2::scale_color_manual(name = "", values = c("Observed" = "black"))
  }
  
  p <- p + ggplot2::labs(x = gettext("Raw Score"), y = yLabel) +
    jaspGraphs::themeJaspRaw() +
    ggplot2::theme(legend.position = "top")
  
  plot$plotObject <- p
  jaspResults[["plot"]] <- plot
}

# --- Model Fit Tables ---
.createConventionalModelTables <- function(jaspResults, model, nObs, options) {
  if (!is.null(jaspResults[["modelFit"]])) return()
  
  fitTab <- jaspBase::createJaspTable(title = gettext("Model Fit"))
  fitTab$dependOn(c("raw", "weights", "scale", "descend", "k", "kPower", "selectionType", "terms", "model"))
  fitTab$position <- 2
  
  fitTab$addColumnInfo(name = "nobs",  title = gettext("N"),          type = "integer")
  fitTab$addColumnInfo(name = "terms", title = gettext("Terms"),      type = "integer")
  fitTab$addColumnInfo(name = "rmse",  title = gettext("RMSE"),       type = "number")
  fitTab$addColumnInfo(name = "r2adj", title = gettext("Adj. R\u00B2"), type = "number")
  fitTab$addColumnInfo(name = "bic",   title = gettext("BIC"),        type = "number")
  
  fitTab$addRows(list(list(
    nobs  = nObs,
    terms = as.integer(model$ideal.model),
    rmse  = model$rmse,
    r2adj = model$subset$adjr2[model$ideal.model],
    bic   = model$subset$bic[model$ideal.model]
  )))
  jaspResults[["modelFit"]] <- fitTab
  
  coefTab <- jaspBase::createJaspTable(title = gettext("Regression Coefficients"))
  coefTab$dependOn(c("raw", "weights", "scale", "descend", "k", "kPower", "selectionType", "terms", "model"))
  coefTab$position <- 3
  
  coefTab$addColumnInfo(name = "term", title = gettext("Term"),     type = "string")
  coefTab$addColumnInfo(name = "est",  title = gettext("Estimate"), type = "number")
  
  for (i in seq_along(model$coefficients)) {
    coefTab$addRows(list(list(term = names(model$coefficients)[i], est = as.numeric(model$coefficients[[i]]))))
  }
  jaspResults[["modelCoef"]] <- coefTab
}

# --- Norm Score Table ---
.createConventionalTable <- function(jaspResults, dataRanked, model, scaleInfo, options) {
  if (!is.null(jaspResults[["normTable"]])) return()
  
  table <- jaspBase::createJaspTable(title = gettext("Norm Score Table"))
  table$dependOn(c("raw", "weights", "scale", "descend", "k", "kPower", "selectionType", "terms",
                   "range", "minRaw", "maxRaw", "stepping", "includeCI", "ciLevel", "reliability"))
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
  
  if (!is.null(model)) {
    minR <- if (options$maxRaw > options$minRaw) options$minRaw else NULL
    maxR <- if (options$maxRaw > options$minRaw) options$maxRaw else NULL
    tab <- cNORM::rawTable(0, model, minNorm = scaleInfo$minNorm, maxNorm = scaleInfo$maxNorm,
                           minRaw = minR, maxRaw = maxR, step = as.numeric(options$stepping),
                           CI = ciVal, reliability = relVal)
  } else {
    tab <- unique(data.frame(raw = dataRanked$raw, norm = dataRanked$normValue, percentile = dataRanked$percentile * 100))
    tab <- tab[order(tab$raw), ]
    tab <- .appendCI(tab, scaleInfo$m1, scaleInfo$sd1, ciVal, relVal)
  }
  
  rawC  <- if ("raw" %in% names(tab)) "raw" else "x"
  normC <- if ("norm" %in% names(tab)) "norm" else "normValue"
  percC <- if ("percentile" %in% names(tab)) "percentile" else "Percentile"
  
  for (i in seq_len(nrow(tab))) {
    row <- list(raw = tab[[rawC]][i], norm = tab[[normC]][i], perc = tab[[percC]][i])
    if (hasCI && "lowerCI" %in% names(tab)) {
      row$lCI    <- tab$lowerCI[i]
      row$uCI    <- tab$upperCI[i]
      row$lCI_PR <- tab$lowerCI_PR[i]
      row$uCI_PR <- tab$upperCI_PR[i]
    }
    table$addRows(list(row))
  }
  
  jaspResults[["normTable"]] <- table
}