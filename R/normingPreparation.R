# R/normingPreparation.R

#' @export
normingPreparation <- function(jaspResults, dataset, options) {
  hasAge   <- options$age != ""
  hasStrat <- length(options$stratVars) > 0L
  
  if (!hasAge && !hasStrat) {
    .normingInstructions(jaspResults, "preparation")
    return()
  }
  
  # 1. Norm Grouping
  if (hasAge) {
    .runGrouping(jaspResults, dataset, options)
  }
  
  # 2. Raking / Post-Stratification
  if (hasStrat) {
    .runRaking(jaspResults, dataset, options)
  }
}

.runGrouping <- function(jaspResults, dataset, options) {
  df <- if (is.null(dataset)) jaspBase::readDataSetToEnd(columns.as.numeric = options$age) else dataset[, options$age, drop = FALSE]
  ageVals <- na.omit(as.numeric(df[[options$age]]))
  
  if (length(ageVals) < 6L) jaspBase::reject(gettext("At least 6 valid observations are required."))
  
  nGroupsOpt <- as.integer(options$nGroups)
  nArg <- if (is.na(nGroupsOpt) || nGroupsOpt <= 0L) NULL else nGroupsOpt
  equidist <- isTRUE(options$equidistant)
  
  grpMidpoints <- cNORM::getGroups(ageVals, n = nArg, equidistant = equidist)
  uMids <- sort(unique(grpMidpoints))
  
  # Summary Table
  if (is.null(jaspResults[["groupTab"]])) {
    tab <- jaspBase::createJaspTable(title = gettext("Norm Group Summary"))
    tab$dependOn(c("age", "nGroups", "equidistant"))
    tab$position <- 1
    
    tab$addColumnInfo(name = "grpNo",    title = gettext("Group"),                 type = "integer")
    tab$addColumnInfo(name = "midpoint", title = gettext("Group Mean (Midpoint)"), type = "number")
    tab$addColumnInfo(name = "minVal",   title = gettext("Min"),                   type = "number")
    tab$addColumnInfo(name = "maxVal",   title = gettext("Max"),                   type = "number")
    tab$addColumnInfo(name = "n",        title = gettext("N"),                     type = "integer")
    tab$addColumnInfo(name = "prop",     title = gettext("% of Sample"),           type = "number")
    
    plotDF <- data.frame(midpoint = uMids, label = character(length(uMids)), n = integer(length(uMids)))
    
    for (i in seq_along(uMids)) {
      idx <- which(grpMidpoints == uMids[i])
      vals <- ageVals[idx]
      n_i <- length(idx)
      min_i <- min(vals)
      max_i <- max(vals)
      
      tab$addRows(list(list(
        grpNo = i, midpoint = uMids[i], minVal = min_i, maxVal = max_i,
        n = n_i, prop = (n_i / length(ageVals)) * 100
      )))
      plotDF$label[i] <- sprintf("%.2f\n[%.1f-%.1f]", uMids[i], min_i, max_i)
      plotDF$n[i] <- n_i
    }
    jaspResults[["groupTab"]] <- tab
  }
  
  # Barplot
  if (is.null(jaspResults[["groupPlot"]])) {
    plot <- jaspBase::createJaspPlot(title = gettext("Norm Group Sample Sizes"), width = 550, height = 360)
    plot$dependOn(c("age", "nGroups", "equidistant"))
    plot$position <- 2
    
    pDF <- plotDF
    pDF$labelFactor <- factor(pDF$label, levels = pDF$label)
    
    p <- ggplot2::ggplot(pDF, ggplot2::aes(x = labelFactor, y = n)) +
      ggplot2::geom_col(fill = "#3E6DA9", alpha = 0.85, width = 0.65) +
      ggplot2::geom_hline(yintercept = 100, linetype = "dashed", color = "#2ca25f", linewidth = 0.7) +
      ggplot2::geom_hline(yintercept = 50, linetype = "dotted", color = "#d95f02", linewidth = 0.7) +
      ggplot2::labs(x = gettext("Group Midpoint [Range]"), y = gettext("Sample Size (N)")) +
      jaspGraphs::themeJaspRaw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = if (nrow(pDF) > 8) 35 else 0, hjust = 1))
    
    plot$plotObject <- p
    jaspResults[["groupPlot"]] <- plot
  }
}

.runRaking <- function(jaspResults, dataset, options) {
  stratVars <- options$stratVars
  df <- if (is.null(dataset)) jaspBase::readDataSetToEnd(columns = stratVars) else dataset[, stratVars, drop = FALSE]
  df <- na.omit(df)
  for (v in stratVars) df[[v]] <- as.character(df[[v]])
  
  nTotal <- nrow(df)
  marginsDF <- .parseMarginsString(options$margins)
  hasTarget <- !is.null(marginsDF)
  
  rakedWeights <- if (hasTarget) {
    tryCatch(cNORM::computeWeights(data = df, population.margins = marginsDF,
                                   standardized = isTRUE(options$standardized)), error = function(e) NULL)
  } else NULL
  
  # Marginals Table
  if (is.null(jaspResults[["marginsTab"]])) {
    tab <- jaspBase::createJaspTable(title = gettext("Stratification Marginals & Raking Weights"))
    tab$dependOn(c("stratVars", "margins", "standardized"))
    tab$position <- 3
    
    tab$addColumnInfo(name = "var",  title = gettext("Variable"),          type = "string")
    tab$addColumnInfo(name = "lvl",  title = gettext("Level"),             type = "string")
    tab$addColumnInfo(name = "n",    title = gettext("Sample N"),          type = "integer")
    tab$addColumnInfo(name = "sPr",  title = gettext("Sample Prop."),      type = "number")
    tab$addColumnInfo(name = "tPr",  title = gettext("Target Prop."),      type = "number")
    tab$addColumnInfo(name = "cWgt", title = gettext("Mean Raked Weight"), type = "number")
    
    for (v in stratVars) {
      tb <- table(df[[v]])
      for (lvl in names(tb)) {
        n_lvl <- as.integer(tb[[lvl]])
        t_prop <- NA_real_
        c_weight <- NA_real_
        
        if (hasTarget) {
          mIdx <- which(marginsDF$variables == v & as.character(marginsDF$levels) == as.character(lvl))
          if (length(mIdx) > 0L) t_prop <- marginsDF$share[mIdx[1]]
          if (!is.null(rakedWeights)) c_weight <- mean(rakedWeights[df[[v]] == lvl], na.rm = TRUE)
        }
        
        tab$addRows(list(list(
          var = v, lvl = as.character(lvl), n = n_lvl, sPr = n_lvl / nTotal, tPr = t_prop, cWgt = c_weight
        )))
      }
    }
    jaspResults[["marginsTab"]] <- tab
  }
  
  # Weight Diagnostics Table
  if (is.null(jaspResults[["weightSummaryTab"]])) {
    wTab <- jaspBase::createJaspTable(title = gettext("Raking Weight Diagnostics"))
    wTab$dependOn(c("stratVars", "margins", "standardized"))
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
      wTab$addRows(list(list(
        nValid = nTotal, minW = min(rakedWeights), maxW = max(rakedWeights),
        meanW = mean(rakedWeights), deff = nTotal / nEff, nEff = nEff
      )))
    } else {
      wTab$addRows(list(list(nValid = nTotal, minW = NA_real_, maxW = NA_real_, meanW = NA_real_, deff = NA_real_, nEff = NA_real_)))
    }
    jaspResults[["weightSummaryTab"]] <- wTab
  }
}