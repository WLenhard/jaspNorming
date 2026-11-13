# R/common.R

.cnormInternal <- function(name) utils::getFromNamespace(name, "cNORM")

.normingPlotTheme <- function(legendPos = "right") {
  jaspGraphs::themeJaspRaw(fontsize = 12) +
    ggplot2::theme(
      axis.title.x    = ggplot2::element_text(size = 12, face = "plain"),
      axis.title.y    = ggplot2::element_text(size = 12, face = "plain"),
      axis.text.x     = ggplot2::element_text(size = 10, colour = "black"),
      axis.text.y     = ggplot2::element_text(size = 10, colour = "black"),
      legend.title    = ggplot2::element_text(size = 11),
      legend.text     = ggplot2::element_text(size = 9.5),
      legend.position = legendPos
    )
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