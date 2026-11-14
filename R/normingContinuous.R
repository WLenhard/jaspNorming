# R/normingContinuous.R

#' @export
normingContinuous <- function(jaspResults, dataset, options) {
  .normingAddCitations(jaspResults, "continuous", options)
  
  # 1. Check requirements
  ready <- (options$raw != "" && options$group != "")
  if (!ready) {
    .normingInstructions(jaspResults, "continuous")
    return()
  }
  
  # 2. Read and clean dataset
  dataComplete <- .normingReadContinuousData(dataset, options)
  scaleInfo    <- .resolveScale(options$scale, as.numeric(options$range))
  
  # 3. Fit and cache model using jaspState
  modelState <- .getContinuousModel(jaspResults, dataComplete, scaleInfo, options)
  if (is.null(modelState)) return()
  
  # 4. Diagnostic Percentile Plot
  if (isTRUE(options$plot)) {
    .createContinuousPlot(jaspResults, modelState, scaleInfo, options)
  }
  
  # 5. Model Fit & Coefficient Tables
  if (isTRUE(options$model)) {
    .createContinuousModelFitTable(jaspResults, modelState, options)
    .createContinuousCoefTable(jaspResults, modelState, options)
  }
  
  # 6. Norm Score Lookup Table
  if (!is.null(options$normAge) && nzchar(trimws(options$normAge))) {
    .createContinuousNormTable(jaspResults, modelState, scaleInfo, options)
  }
  
  # 7. Save Predicted Norm Scores / Percentiles to Dataset
  if (isTRUE(options$saveNorm) || isTRUE(options$savePerc)) {
    .saveContinuousPredictions(jaspResults, dataComplete, modelState, scaleInfo, options)
  }
}

# --- Data Reader ---
.normingReadContinuousData <- function(dataset, options) {
  vars <- c(options$raw, options$group)
  if (nzchar(options$weights)) vars <- c(vars, options$weights)
  
  if (is.null(dataset)) {
    df <- jaspBase::.readDataSetToEnd(columns.as.numeric = vars)
  } else {
    df <- dataset[, vars, drop = FALSE]
  }
  
  x <- .normingAsNumeric(df[[options$raw]],   options$raw)
  g <- .normingAsNumeric(df[[options$group]], options$group)
  w <- if (nzchar(options$weights)) .normingAsNumeric(df[[options$weights]], options$weights) else NULL
  
  fullDF   <- if (!is.null(w)) data.frame(raw = x, group = g, weights = w) else data.frame(raw = x, group = g)
  validIdx <- which(stats::complete.cases(fullDF))
  cleanDF  <- fullDF[validIdx, , drop = FALSE]
  
  if (nrow(cleanDF) < 10L)
    jaspBase::.quitAnalysis(gettext("At least 10 valid, complete observations are required."))
  if (length(unique(cleanDF$raw)) < 2L)
    jaspBase::.quitAnalysis(gettext("The raw score variable has zero variance."))
  if (length(unique(cleanDF$group)) < 2L)
    jaspBase::.quitAnalysis(gettext("The grouping variable has only 1 distinct value. Use Single Group Norming instead."))
  if (!is.null(w) && any(!is.finite(cleanDF$weights) | cleanDF$weights <= 0))
    jaspBase::.quitAnalysis(gettext("All case weights must be positive finite numbers."))
  
  attr(cleanDF, "validIdx")  <- validIdx
  attr(cleanDF, "nRowsFull") <- nrow(fullDF)
  return(cleanDF)
}

.continuousModelDeps <- function() {
  c("raw", "group", "weights", "method", "scale", "descend", "nGroups",
    "equidistant", "k", "t", "selectionType", "terms", "alpha", "beta",
    "itemnumber", "muDegree", "sigmaDegree", "epsilonDegree", "deltaDegree",
    "fixDelta", "deltaValue", "cmpMuDegree", "cmpNuDegree", "cmpFixNu",
    "cmpNuValue", "cmpMaxScore")
}

# --- Model Fitting with JASP State Caching ---
.getContinuousModel <- function(jaspResults, dataComplete, scaleInfo, options) {
  if (!is.null(jaspResults[["modelState"]])) {
    return(jaspResults[["modelState"]]$object)
  }
  
  method     <- options$method
  res        <- list(method = method, data = dataComplete)
  hasWeights <- "weights" %in% names(dataComplete)
  wVec       <- if (hasWeights) dataComplete$weights else NULL
  
  if (method == "betabinomial") {
    score <- as.integer(round(dataComplete$raw))
    if (any(score < 0L))
      jaspBase::.quitAnalysis(gettext("Beta-Binomial norming requires non-negative integer raw scores."))
    nItem <- as.integer(options$itemnumber)
    nMax  <- if (is.null(nItem) || is.na(nItem) || nItem <= 0L) max(score, na.rm = TRUE) else nItem
    if (nMax < max(score, na.rm = TRUE))
      jaspBase::.quitAnalysis(gettextf(
        "The specified item number (%d) is smaller than the maximum observed raw score (%d).",
        nMax, max(score, na.rm = TRUE)))
  } else if (method == "cmp") {
    score <- as.integer(round(dataComplete$raw))
    if (any(score < 0L))
      jaspBase::.quitAnalysis(gettext("Conway-Maxwell-Poisson (CMP) norming requires non-negative count raw scores."))
    msOpt <- as.integer(options$cmpMaxScore)
    maxSc <- if (is.null(msOpt) || is.na(msOpt) || msOpt <= 0L) NULL else msOpt
    if (!is.null(maxSc) && maxSc < max(score, na.rm = TRUE))
      jaspBase::.quitAnalysis(gettextf(
        "The specified maximum score (%d) is smaller than the maximum observed raw score (%d).",
        maxSc, max(score, na.rm = TRUE)))
  }
  
  res <- tryCatch({
    if (method == "taylor") {
      k          <- as.integer(options$k)
      t          <- as.integer(options$t)
      descend    <- isTRUE(options$descend)
      nGroupsOpt <- as.integer(options$nGroups)
      equidist   <- isTRUE(options$equidistant)
      
      dataComplete$origAge <- dataComplete$group
      nUniqueAge <- length(unique(dataComplete$group))
      
      if (!is.na(nGroupsOpt) && nGroupsOpt > 0L) {
        dataComplete$group <- cNORM::getGroups(dataComplete$origAge, n = nGroupsOpt, equidistant = equidist)
      } else if (nUniqueAge > 20L) {
        targetN <- max(2L, min(20L, floor(nrow(dataComplete) / 100)))
        dataComplete$group <- cNORM::getGroups(dataComplete$origAge, n = targetN, equidistant = equidist)
      }
      
      dataRanked <- if (hasWeights) {
        cNORM::rankByGroup(dataComplete, raw = dataComplete$raw, group = dataComplete$group,
                           weights = dataComplete$weights, scale = scaleInfo$scaleParam, descend = descend)
      } else {
        cNORM::rankByGroup(dataComplete, raw = dataComplete$raw, group = dataComplete$group,
                           scale = scaleInfo$scaleParam, descend = descend)
      }
      
      nDistinctGroups <- length(unique(dataRanked$group))
      if (t >= nDistinctGroups) t <- max(1L, nDistinctGroups - 1L)
      
      dataPrepared <- cNORM::computePowers(dataRanked, k = k, t = t, norm = "normValue")
      wPrepared    <- if (hasWeights && "weights" %in% names(dataPrepared)) dataPrepared$weights else NULL
      
      useManual <- identical(options$selectionType, "manualSelection")
      maxTerms  <- k * t + k + t
      terms     <- if (useManual) min(max(1L, as.integer(options$terms)), maxTerms) else NULL
      
      model <- if (useManual) {
        cNORM::bestModel(dataPrepared, raw = "raw", terms = terms, k = k, t = t, weights = wPrepared, plot = FALSE)
      } else {
        cNORM::bestModel(dataPrepared, raw = "raw", k = k, t = t, weights = wPrepared, plot = FALSE)
      }
      
      res$model        <- model
      res$dataPrepared <- dataPrepared
      
    } else if (method == "betabinomial") {
      model <- cNORM::cnorm.betabinomial(
        age = dataComplete$group, score = score, n = nMax, weights = wVec,
        mode = 2, alpha = as.integer(options$alpha), beta = as.integer(options$beta),
        scale = scaleInfo$scaleParam, plot = FALSE
      )
      diag <- cNORM::diagnostics.betabinomial(model, age = dataComplete$group, score = score, weights = wVec)
      
      res$model <- model
      res$diag  <- diag
      res$nMax  <- nMax
      
    } else if (method == "shash") {
      fixDelta <- isTRUE(options$fixDelta)
      deltaDeg <- if (fixDelta) NULL else as.integer(options$deltaDegree)
      deltaVal <- if (fixDelta) as.numeric(options$deltaValue) else 1.0
      
      model <- cNORM::cnorm.shash(
        age = dataComplete$group, score = dataComplete$raw, weights = wVec,
        mu_degree = as.integer(options$muDegree), sigma_degree = as.integer(options$sigmaDegree),
        epsilon_degree = as.integer(options$epsilonDegree), delta_degree = deltaDeg,
        delta = deltaVal, scale = scaleInfo$scaleParam, plot = FALSE
      )
      diag <- cNORM::diagnostics.shash(model, age = dataComplete$group, score = dataComplete$raw, weights = wVec)
      
      res$model <- model
      res$diag  <- diag
      
    } else if (method == "cmp") {
      fixNu <- isTRUE(options$cmpFixNu)
      nuDeg <- if (fixNu) NULL else as.integer(options$cmpNuDegree)
      nuVal <- if (fixNu) as.numeric(options$cmpNuValue) else 1.0
      
      model <- cNORM::cnorm.cmp(
        age = dataComplete$group, score = score, weights = wVec,
        mu_degree = as.integer(options$cmpMuDegree), nu_degree = nuDeg,
        nu = nuVal, scale = scaleInfo$scaleParam, max_score = maxSc, plot = FALSE
      )
      diag <- cNORM::diagnostics.cmp(model, age = dataComplete$group, score = score, weights = wVec)
      
      res$model <- model
      res$diag  <- diag
    }
    res
  }, error = function(e) {
    jaspBase::.quitAnalysis(gettextf(
      "Model estimation failed: %s",
      jaspBase::decodeColNames(conditionMessage(e))
    ))
  })
  
  state <- jaspBase::createJaspState(res, dependencies = .continuousModelDeps())
  jaspResults[["modelState"]] <- state
  return(res)
}

# --- Save Individual Predictions to Dataset ---
.saveContinuousPredictions <- function(jaspResults, dataComplete, modelState, scaleInfo, options) {
  wantNorm <- isTRUE(options$saveNorm) && !is.null(options$normColName) && nzchar(trimws(options$normColName))
  wantPerc <- isTRUE(options$savePerc) && !is.null(options$percColName) && nzchar(trimws(options$percColName))
  if (!wantNorm && !wantPerc) return()
  
  needNormCompute <- wantNorm && is.null(jaspResults[["savedContNormCol"]])
  needPercCompute <- wantPerc && is.null(jaspResults[["savedContPercCol"]])
  if (!needNormCompute && !needPercCompute) return()
  
  validIdx  <- attr(dataComplete, "validIdx")
  nRowsFull <- attr(dataComplete, "nRowsFull")
  rawValid  <- dataComplete$raw
  ageValid  <- dataComplete$group
  sdRange   <- as.numeric(options$range)
  
  predNorm <- tryCatch({
    if (modelState$method == "taylor") {
      suppressMessages(cNORM::predictNorm(
        raw     = rawValid,
        A       = ageValid,
        model   = modelState$model,
        minNorm = scaleInfo$minNorm,
        maxNorm = scaleInfo$maxNorm,
        silent  = TRUE
      ))
    } else if (modelState$method %in% c("betabinomial", "cmp")) {
      suppressWarnings(stats::predict(
        modelState$model,
        age   = ageValid,
        score = as.integer(round(rawValid)),
        range = sdRange
      ))
    } else if (modelState$method == "shash") {
      suppressWarnings(stats::predict(
        modelState$model,
        age   = ageValid,
        score = rawValid,
        range = sdRange
      ))
    }
  }, error = function(e) rep(NA_real_, length(rawValid)))
  
  if (wantNorm) {
    fullNorm <- rep(NA_real_, nRowsFull)
    fullNorm[validIdx] <- as.numeric(predNorm)
    .saveScaleColumn(
      jaspResults = jaspResults,
      key         = "savedContNormCol",
      colName     = options$normColName,
      values      = fullNorm,
      deps        = c(.continuousModelDeps(), "range", "saveNorm", "normColName")
    )
  }
  
  if (wantPerc) {
    predPerc <- stats::pnorm((as.numeric(predNorm) - scaleInfo$m1) / scaleInfo$sd1) * 100
    fullPerc <- rep(NA_real_, nRowsFull)
    fullPerc[validIdx] <- predPerc
    .saveScaleColumn(
      jaspResults = jaspResults,
      key         = "savedContPercCol",
      colName     = options$percColName,
      values      = fullPerc,
      deps        = c(.continuousModelDeps(), "range", "savePerc", "percColName")
    )
  }
}

# --- Percentile Curves Plot ---
.createContinuousPlot <- function(jaspResults, modelState, scaleInfo, options) {
  if (!is.null(jaspResults[["percentilePlot"]])) return()
  
  plot <- jaspBase::createJaspPlot(title = gettext("Percentile Curves"), width = 650, height = 450)
  plot$dependOn(c(.continuousModelDeps(), "plot"))
  plot$position <- 1
  
  percentiles <- c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)
  labels      <- paste0(percentiles * 100, "%")
  levels      <- paste0("PR", percentiles * 100)
  colors      <- grDevices::rainbow(length(levels), end = 0.8)
  
  p <- ggplot2::ggplot()
  method <- modelState$method
  
  if (method == "taylor") {
    dp <- modelState$dataPrepared
    m  <- modelState$model
    predAgeVec <- if ("origAge" %in% names(dp)) dp$origAge else dp$group
    
    p <- p + ggplot2::geom_point(
      data = data.frame(age = predAgeVec, raw = dp$raw),
      ggplot2::aes(x = age, y = raw), color = "black", alpha = 0.2, size = 0.7
    )
    
    T_vals    <- stats::qnorm(percentiles, mean = scaleInfo$m1, sd = scaleInfo$sd1)
    AGEP_pred <- seq(min(predAgeVec), max(predAgeVec), length.out = 100)
    predList  <- lapply(seq_along(AGEP_pred), function(i) {
      yPred <- cNORM::predictRaw(T_vals, AGEP_pred[i], m$coefficients, minRaw = min(dp$raw), maxRaw = max(dp$raw))
      data.frame(age = AGEP_pred[i], value = as.numeric(yPred), percentile = factor(levels, levels = levels))
    })
    predDF <- do.call(rbind, predList)
    
    p <- p +
      ggplot2::geom_line(data = predDF, ggplot2::aes(x = age, y = value, color = percentile), linewidth = 0.8) +
      ggplot2::scale_color_manual(values = stats::setNames(colors, levels), labels = labels, name = gettext("Percentile"))
    
  } else {
    d <- modelState$data
    p <- p + ggplot2::geom_point(
      data = data.frame(age = d$group, raw = d$raw),
      ggplot2::aes(x = age, y = raw), color = "black", alpha = 0.2, size = 0.7
    )
    pred_ages <- seq(min(d$group), max(d$group), length.out = 100)
    
    if (method == "betabinomial") {
      predictCoefficients2 <- .cnormInternal("predictCoefficients2")
      bb_distribution      <- .cnormInternal("bb_distribution")
      preds <- predictCoefficients2(modelState$model, pred_ages, modelState$nMax)
      pMat <- vapply(seq_along(pred_ages), function(j) {
        dist <- bb_distribution(preds$a[j], preds$b[j], modelState$nMax)
        vapply(percentiles, function(pr) as.numeric(dist$x[which.max(dist$cum >= pr)]), numeric(1))
      }, numeric(length(percentiles)))
      pMat <- t(pMat)
    } else if (method == "shash") {
      preds <- .cnormInternal("predictCoefficients_shash")(modelState$model, pred_ages)
      pLines <- lapply(percentiles, function(pr) {
        cNORM::qshash(pr, mu = preds$mu, sigma = preds$sigma, epsilon = preds$epsilon, delta = preds$delta)
      })
      pMat <- do.call(cbind, pLines)
    } else if (method == "cmp") {
      preds            <- .cnormInternal("predictCoefficients_cmp")(modelState$model, pred_ages)
      o                <- .cnormInternal("cmp_opts")(modelState$model)
      cmp_quantile_mid <- .cnormInternal("cmp_quantile_mid")
      pLines <- lapply(percentiles, function(pr) {
        cmp_quantile_mid(rep(pr, length(pred_ages)), mu = preds$mu, nu = preds$nu,
                         tol = o$tol, max_terms = o$max_terms, max_score = o$max_score)
      })
      pMat <- do.call(cbind, pLines)
    }
    
    predList <- lapply(seq_along(percentiles), function(i) {
      data.frame(age = pred_ages, value = as.numeric(pMat[, i]), percentile = factor(levels[i], levels = levels))
    })
    predDF <- do.call(rbind, predList)
    
    if (method == "betabinomial") {
      p <- p + ggplot2::geom_step(data = predDF, ggplot2::aes(x = age, y = value, color = percentile), direction = "mid", linewidth = 0.8)
    } else {
      p <- p + ggplot2::geom_line(data = predDF, ggplot2::aes(x = age, y = value, color = percentile), linewidth = 0.8)
    }
    p <- p + ggplot2::scale_color_manual(values = stats::setNames(colors, levels), labels = labels, name = gettext("Percentile"))
  }
  
  p <- p +
    ggplot2::labs(
      x = jaspBase::decodeColNames(options$group),
      y = jaspBase::decodeColNames(options$raw)
    ) +
    .normingPlotTheme(legendPos = "right")
  
  plot$plotObject <- p
  jaspResults[["percentilePlot"]] <- plot
}

# --- Model Fit Table ---
.createContinuousModelFitTable <- function(jaspResults, modelState, options) {
  if (!is.null(jaspResults[["modelFit"]])) return()
  
  table <- jaspBase::createJaspTable(title = gettext("Model Fit"))
  table$dependOn(c(.continuousModelDeps(), "model"))
  table$position <- 2
  
  table$addColumnInfo(name = "method", title = gettext("Method"),             type = "string")
  table$addColumnInfo(name = "nobs",   title = gettext("N"),                  type = "integer")
  table$addColumnInfo(name = "npar",   title = gettext("Terms / Parameters"), type = "integer")
  table$addColumnInfo(name = "aic",    title = gettext("AIC"),                type = "number")
  table$addColumnInfo(name = "bic",    title = gettext("BIC"),                type = "number")
  table$addColumnInfo(name = "r2",     title = gettext("R\u00B2"),            type = "number")
  table$addColumnInfo(name = "rmse",   title = gettext("RMSE"),               type = "number")
  
  methodLabels <- c(
    taylor       = gettext("Taylor Polynomials"),
    betabinomial = gettext("Beta-Binomial"),
    shash        = gettext("Sinh-Arcsinh (SHASH)"),
    cmp          = gettext("Conway-Maxwell-Poisson (CMP)")
  )
  mLabel <- if (modelState$method %in% names(methodLabels)) methodLabels[[modelState$method]] else modelState$method
  
  if (modelState$method == "taylor") {
    m   <- modelState$model
    idx <- m$ideal.model
    row <- list(
      method = mLabel,
      nobs   = nrow(modelState$dataPrepared),
      npar   = as.integer(idx),
      aic    = NA_real_,
      bic    = m$subset$bic[idx],
      r2     = m$subset$adjr2[idx],
      rmse   = m$rmse
    )
  } else {
    d <- modelState$diag
    row <- list(
      method = mLabel,
      nobs   = as.integer(d$n_obs),
      npar   = as.integer(d$n_params),
      aic    = d$AIC,
      bic    = d$BIC,
      r2     = d$R2,
      rmse   = d$rmse
    )
  }
  
  table$addRows(list(row))
  jaspResults[["modelFit"]] <- table
}

# --- Parameter Estimates Table ---
.createContinuousCoefTable <- function(jaspResults, modelState, options) {
  if (!is.null(jaspResults[["modelCoef"]])) return()
  
  table <- jaspBase::createJaspTable(title = gettext("Parameter Estimates"))
  table$dependOn(c(.continuousModelDeps(), "model"))
  table$position <- 3
  
  table$addColumnInfo(name = "term", title = gettext("Term"),     type = "string")
  table$addColumnInfo(name = "est",  title = gettext("Estimate"), type = "number")
  
  if (modelState$method == "taylor") {
    coefs  <- modelState$model$coefficients
    tNames <- if (!is.null(names(coefs)) && all(nzchar(names(coefs)))) names(coefs) else paste0("Term_", seq_along(coefs))
    outDF  <- data.frame(
      term = tNames,
      est  = as.numeric(coefs),
      stringsAsFactors = FALSE
    )
    table$setData(outDF)
  } else {
    table$addColumnInfo(name = "se", title = gettext("SE"), type = "number")
    table$addColumnInfo(name = "z",  title = gettext("z"),  type = "number")
    table$addColumnInfo(name = "p",  title = gettext("p"),  type = "pvalue")
    
    d   <- modelState$diag
    est <- if (!is.null(d$param_estimates)) {
      d$param_estimates
    } else {
      c(d$mu_estimates, d$sigma_estimates, d$epsilon_estimates, d$delta_estimates, d$nu_estimates)
    }
    se <- if (!is.null(d$param_se)) {
      d$param_se
    } else {
      c(d$mu_se, d$sigma_se, d$epsilon_se, d$delta_se, d$nu_se)
    }
    z <- if (!is.null(d$z_values)) {
      d$z_values
    } else {
      c(d$mu_z_values, d$sigma_z_values, d$epsilon_z_values, d$delta_z_values, d$nu_z_values)
    }
    p <- if (!is.null(d$p_values)) {
      d$p_values
    } else {
      c(d$mu_p_values, d$sigma_p_values, d$epsilon_p_values, d$delta_p_values, d$nu_p_values)
    }
    
    nPar <- length(est)
    makeCompNames <- function(vec, prefix) {
      if (is.null(vec) || length(vec) == 0L) return(character(0))
      if (!is.null(names(vec)) && all(nzchar(names(vec)))) return(names(vec))
      paste0(prefix, "_", seq_along(vec) - 1L)
    }
    
    parNamesFromModel <- if (!is.null(modelState$model$result$par)) names(modelState$model$result$par) else NULL
    termNames <- if (!is.null(names(est)) && length(names(est)) == nPar && all(nzchar(names(est)))) {
      names(est)
    } else if (!is.null(d$param_names) && length(d$param_names) == nPar) {
      as.character(d$param_names)
    } else if (!is.null(parNamesFromModel) && length(parNamesFromModel) == nPar && all(nzchar(parNamesFromModel))) {
      parNamesFromModel
    } else {
      compNames <- c(
        makeCompNames(d$mu_estimates,      "mu"),
        makeCompNames(d$sigma_estimates,   "sigma"),
        makeCompNames(d$epsilon_estimates, "epsilon"),
        makeCompNames(d$delta_estimates,   "delta"),
        makeCompNames(d$nu_estimates,      "nu")
      )
      if (length(compNames) == nPar) compNames else paste0("param_", seq_len(nPar) - 1L)
    }
    
    padVec <- function(v, n) {
      if (is.null(v) || length(v) == 0L) return(rep(NA_real_, n))
      v <- as.numeric(unname(v))
      if (length(v) < n) c(v, rep(NA_real_, n - length(v))) else v[seq_len(n)]
    }
    
    if (nPar > 0L) {
      outDF <- data.frame(
        term = termNames,
        est  = as.numeric(unname(est)),
        se   = padVec(se, nPar),
        z    = padVec(z,  nPar),
        p    = padVec(p,  nPar),
        stringsAsFactors = FALSE
      )
      table$setData(outDF)
    }
  }
  
  jaspResults[["modelCoef"]] <- table
}

# --- Norm Score Table ---
.createContinuousNormTable <- function(jaspResults, modelState, scaleInfo, options) {
  if (!is.null(jaspResults[["normTable"]])) return()
  
  normAge <- suppressWarnings(as.numeric(trimws(options$normAge)))
  if (is.na(normAge)) {
    table <- jaspBase::createJaspTable(title = gettext("Norm Score Table"))
    table$dependOn(c(.continuousModelDeps(), "normAge", "range", "minRaw", "maxRaw", "stepping", "includeCI", "ciLevel", "reliability"))
    table$position <- 4
    table$setError(gettext("Please enter a single valid numeric value for Age / Group to compute a norm table."))
    jaspResults[["normTable"]] <- table
    return()
  }
  
  table <- jaspBase::createJaspTable(title = paste0(gettext("Norm Score Table (Age / Value = "), normAge, ")"))
  table$dependOn(c(.continuousModelDeps(), "normAge", "range", "minRaw", "maxRaw", "stepping", "includeCI", "ciLevel", "reliability"))
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
  
  obsAges <- modelState$data$group
  if (normAge < min(obsAges, na.rm = TRUE) || normAge > max(obsAges, na.rm = TRUE)) {
    table$addFootnote(
      gettextf("The requested value (%.2f) lies outside the observed group/age range [%.2f, %.2f]. Extrapolated norms should be interpreted with caution.",
               normAge, min(obsAges, na.rm = TRUE), max(obsAges, na.rm = TRUE)),
      symbol = gettext("<em>Warning:</em>")
    )
  }
  
  ciVal   <- if (hasCI) as.numeric(options$ciLevel) else NULL
  relVal  <- if (hasCI) as.numeric(options$reliability) else NULL
  stepVal <- max(0.01, as.numeric(options$stepping))
  minR    <- if (options$maxRaw > options$minRaw) as.numeric(options$minRaw) else NULL
  maxR    <- if (options$maxRaw > options$minRaw) as.numeric(options$maxRaw) else NULL
  
  tabErr <- NULL
  tab <- tryCatch({
    if (modelState$method == "taylor") {
      cNORM::rawTable(normAge, modelState$model, minNorm = scaleInfo$minNorm, maxNorm = scaleInfo$maxNorm,
                      minRaw = minR, maxRaw = maxR, step = stepVal, CI = ciVal, reliability = relVal)
    } else if (modelState$method == "betabinomial") {
      cNORM::normTable.betabinomial(modelState$model, ages = normAge, n = modelState$nMax,
                                    range = as.numeric(options$range), CI = ciVal, reliability = relVal)[[1]]
    } else if (modelState$method == "shash") {
      df <- cNORM::normTable.shash(modelState$model, ages = normAge, start = minR, end = maxR,
                                   step = stepVal, CI = ciVal, reliability = relVal)[[1]]
      if (!("norm" %in% names(df))) {
        df$norm <- pmin(pmax(scaleInfo$m1 + scaleInfo$sd1 * df$z, scaleInfo$minNorm), scaleInfo$maxNorm)
      }
      df
    } else if (modelState$method == "cmp") {
      cNORM::normTable.cmp(modelState$model, ages = normAge, start = minR, end = maxR,
                           step = max(1L, as.integer(round(stepVal))),
                           range = as.numeric(options$range), CI = ciVal, reliability = relVal)[[1]]
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