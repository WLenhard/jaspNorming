# R/normingPreparation.R

#' @export
normingPreparation <- function(jaspResults, dataset, options) {
  hasAge   <- options$age != ""
  hasStrat <- length(options$stratVars) > 0L
  
  if (!hasAge && !hasStrat) {
    .normingInstructions(jaspResults, "preparation")
    return()
  }
  
  # 1. Norm grouping
  if (hasAge)
    .runGrouping(jaspResults, dataset, options)
  
  # 2. Raking / post-stratification
  if (hasStrat)
    .runRaking(jaspResults, dataset, options)
}

# ---- Norm groups -------------------------------------------------------------

.runGrouping <- function(jaspResults, dataset, options) {
  if (!is.null(jaspResults[["groupTab"]]) && !is.null(jaspResults[["groupPlot"]]))
    return()
  
  df <- if (is.null(dataset)) {
    jaspBase::.readDataSetToEnd(columns.as.numeric = options$age)
  } else {
    dataset[, options$age, drop = FALSE]
  }
  ageVals <- stats::na.omit(.normingAsNumeric(df[[options$age]], options$age))
  
  if (length(ageVals) < 6L)
    jaspBase::.quitAnalysis(gettext("At least 6 valid observations are required."))
  
  nGroupsOpt   <- as.integer(options$nGroups)
  nArg         <- if (is.na(nGroupsOpt) || nGroupsOpt <= 0L) NULL else nGroupsOpt
  grpMidpoints <- cNORM::getGroups(ageVals, n = nArg, equidistant = isTRUE(options$equidistant))
  uMids        <- sort(unique(grpMidpoints))
  
  summ <- do.call(rbind, lapply(seq_along(uMids), function(i) {
    vals <- ageVals[grpMidpoints == uMids[i]]
    data.frame(
      grpNo    = i,
      midpoint = uMids[i],
      minVal   = min(vals),
      maxVal   = max(vals),
      n        = length(vals),
      prop     = length(vals) / length(ageVals) * 100
    )
  }))
  summ$label <- sprintf("%.2f\n[%.1f-%.1f]", summ$midpoint, summ$minVal, summ$maxVal)
  
  deps <- c("age", "nGroups", "equidistant")
  
  if (is.null(jaspResults[["groupTab"]])) {
    tab <- jaspBase::createJaspTable(title = gettext("Norm Group Summary"))
    tab$dependOn(deps)
    tab$position <- 1
    tab$addColumnInfo(name = "grpNo",    title = gettext("Group"),                 type = "integer")
    tab$addColumnInfo(name = "midpoint", title = gettext("Group Mean (Midpoint)"), type = "number")
    tab$addColumnInfo(name = "minVal",   title = gettext("Min"),                   type = "number")
    tab$addColumnInfo(name = "maxVal",   title = gettext("Max"),                   type = "number")
    tab$addColumnInfo(name = "n",        title = gettext("N"),                     type = "integer")
    tab$addColumnInfo(name = "prop",     title = gettext("% of Sample"),           type = "number")
    tab$setData(summ[, c("grpNo", "midpoint", "minVal", "maxVal", "n", "prop")])
    jaspResults[["groupTab"]] <- tab
  }
  
  if (is.null(jaspResults[["groupPlot"]])) {
    plot <- jaspBase::createJaspPlot(title = gettext("Norm Group Sample Sizes"), width = 550, height = 360)
    plot$dependOn(deps)
    plot$position <- 2
    
    pDF <- summ
    pDF$labelFactor <- factor(pDF$label, levels = pDF$label)
    angled <- nrow(pDF) > 8L
    
    p <- ggplot2::ggplot(pDF, ggplot2::aes(x = labelFactor, y = n)) +
      ggplot2::geom_col(fill = "#3E6DA9", alpha = 0.85, width = 0.65) +
      ggplot2::geom_hline(yintercept = 100, linetype = "dashed", color = "#2ca25f", linewidth = 0.7) +
      ggplot2::geom_hline(yintercept = 50,  linetype = "dotted", color = "#d95f02", linewidth = 0.7) +
      ggplot2::labs(x = gettext("Group Midpoint [Range]"), y = gettext("Sample Size (N)")) +
      .normingPlotTheme(legendPos = "none") +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(
          size  = 9.5,
          angle = if (angled) 35 else 0,
          hjust = if (angled) 1 else 0.5
        )
      )
    
    plot$plotObject <- p
    jaspResults[["groupPlot"]] <- plot
  }
}

# ---- Raking ------------------------------------------------------------------

.validateAndEncodeMargins <- function(marginsDF, stratVars) {
  stratDecoded <- jaspBase::decodeColNames(stratVars)
  
  unknown <- setdiff(unique(marginsDF$variables), stratDecoded)
  if (length(unknown) > 0L)
    jaspBase::.quitAnalysis(gettextf(
      "The population proportions refer to variable(s) that are not among the stratification variables: %s.",
      paste(unknown, collapse = ", ")
    ))
  
  if (anyNA(marginsDF$share) || any(!is.finite(marginsDF$share) | marginsDF$share < 0))
    jaspBase::.quitAnalysis(gettext("The population proportions must be non-negative finite numbers."))
  
  sums <- tapply(marginsDF$share, marginsDF$variables, sum)
  bad  <- names(sums)[abs(sums - 1) > 0.05]
  if (length(bad) > 0L)
    jaspBase::.quitAnalysis(gettextf(
      "The proportions of each variable must add up to approximately 1 (between 0.95 and 1.05). This is not the case for: %s.",
      paste0(bad, " (", format(round(sums[bad], 3), nsmall = 2), ")", collapse = ", ")
    ))
  
  nameMap <- stats::setNames(stratVars, stratDecoded)
  marginsDF$variables <- unname(nameMap[marginsDF$variables])
  marginsDF
}

.runRaking <- function(jaspResults, dataset, options) {
  if (!is.null(jaspResults[["marginsTab"]]) && !is.null(jaspResults[["weightSummaryTab"]]))
    return()
  
  stratVars <- unlist(options$stratVars)
  df <- if (is.null(dataset)) {
    jaspBase::.readDataSetToEnd(columns = stratVars)
  } else {
    dataset[, stratVars, drop = FALSE]
  }
  df <- stats::na.omit(df)
  nTotal <- nrow(df)
  
  if (nTotal < 2L)
    jaspBase::.quitAnalysis(gettext("At least 2 complete observations across the stratification variables are required."))
  
  for (v in stratVars) df[[v]] <- as.character(df[[v]])
  
  marginsText <- options$margins
  marginsDF   <- .parseMarginsString(marginsText)
  
  if (is.null(marginsDF) && !is.null(marginsText) && nzchar(trimws(marginsText)))
    jaspBase::.quitAnalysis(gettext(
      "The population proportions could not be read. Use the format: variable: level=share, level=share; variable: level=share, ..."
    ))
  
  if (!is.null(marginsDF))
    marginsDF <- .validateAndEncodeMargins(marginsDF, stratVars)
  
  hasTarget <- !is.null(marginsDF)
  
  rakeError    <- NULL
  rakedWeights <- NULL
  if (hasTarget) {
    rakedWeights <- tryCatch(
      suppressMessages(cNORM::computeWeights(
        data               = df,
        population.margins = marginsDF,
        standardized       = isTRUE(options$standardized)
      )),
      error = function(e) { rakeError <<- conditionMessage(e); NULL }
    )
  }
  rakeFootnote <- function(tbl) {
    if (!is.null(rakeError))
      tbl$addFootnote(
        gettextf("Raking failed: %s", jaspBase::decodeColNames(rakeError)),
        symbol = gettext("<em>Error:</em>")
      )
  }
  
  deps <- c("stratVars", "margins", "standardized")
  
  if (is.null(jaspResults[["marginsTab"]])) {
    tab <- jaspBase::createJaspTable(title = gettext("Stratification Marginals & Raking Weights"))
    tab$dependOn(deps)
    tab$position <- 3
    
    tab$addColumnInfo(name = "var",  title = gettext("Variable"),          type = "string")
    tab$addColumnInfo(name = "lvl",  title = gettext("Level"),             type = "string")
    tab$addColumnInfo(name = "n",    title = gettext("Sample N"),          type = "integer")
    tab$addColumnInfo(name = "sPr",  title = gettext("Sample Prop."),      type = "number")
    tab$addColumnInfo(name = "tPr",  title = gettext("Target Prop."),      type = "number")
    tab$addColumnInfo(name = "cWgt", title = gettext("Mean Raked Weight"), type = "number")
    
    rows <- list()
    for (v in stratVars) {
      tb   <- table(df[[v]])
      vLab <- jaspBase::decodeColNames(v)
      for (lvl in names(tb)) {
        n_lvl    <- as.integer(tb[[lvl]])
        t_prop   <- NA_real_
        c_weight <- NA_real_
        
        if (hasTarget) {
          mIdx <- which(marginsDF$variables == v & as.character(marginsDF$levels) == as.character(lvl))
          if (length(mIdx) > 0L) t_prop <- marginsDF$share[mIdx[1]]
        }
        if (!is.null(rakedWeights))
          c_weight <- mean(rakedWeights[df[[v]] == lvl], na.rm = TRUE)
        
        rows[[length(rows) + 1L]] <- data.frame(
          var  = vLab,
          lvl  = as.character(lvl),
          n    = n_lvl,
          sPr  = n_lvl / nTotal,
          tPr  = t_prop,
          cWgt = c_weight,
          stringsAsFactors = FALSE
        )
      }
    }
    if (length(rows) > 0L)
      tab$setData(do.call(rbind, rows))
    
    rakeFootnote(tab)
    jaspResults[["marginsTab"]] <- tab
  }
  
  if (is.null(jaspResults[["weightSummaryTab"]])) {
    wTab <- jaspBase::createJaspTable(title = gettext("Raking Weight Diagnostics"))
    wTab$dependOn(deps)
    wTab$position <- 4
    
    wTab$addColumnInfo(name = "nValid", title = gettext("Complete Cases"), type = "integer")
    wTab$addColumnInfo(name = "minW",   title = gettext("Min Weight"),     type = "number")
    wTab$addColumnInfo(name = "maxW",   title = gettext("Max Weight"),     type = "number")
    wTab$addColumnInfo(name = "meanW",  title = gettext("Mean Weight"),    type = "number")
    wTab$addColumnInfo(name = "deff",   title = gettext("Design Effect"),  type = "number")
    wTab$addColumnInfo(name = "nEff",   title = gettext("Effective N"),    type = "number")
    
    if (!is.null(rakedWeights)) {
      sumW  <- sum(rakedWeights)
      sumW2 <- sum(rakedWeights^2)
      nEff  <- (sumW^2) / sumW2
      row <- list(nValid = nTotal, minW = min(rakedWeights), maxW = max(rakedWeights),
                  meanW = mean(rakedWeights), deff = nTotal / nEff, nEff = nEff)
    } else {
      row <- list(nValid = nTotal, minW = NA_real_, maxW = NA_real_,
                  meanW = NA_real_, deff = NA_real_, nEff = NA_real_)
    }
    wTab$addRows(list(row))
    rakeFootnote(wTab)
    jaspResults[["weightSummaryTab"]] <- wTab
  }
}