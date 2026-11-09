# R/common.R

.cnormInternal <- function(name) utils::getFromNamespace(name, "cNORM")

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
    m1 = m1,
    sd1 = sd1,
    scaleParam = c(m1, sd1),
    minNorm = m1 - sdRange * sd1,
    maxNorm = m1 + sdRange * sd1
  )
}

.normingAsNumeric <- function(x, varName) {
  out <- if (is.factor(x)) suppressWarnings(as.numeric(as.character(x))) else as.numeric(x)
  if (all(is.na(out)) && !all(is.na(x)))
    jaspBase::.quitAnalysis(gettextf("The variable '%s' must have numeric values.", varName))
  out
}

.appendCI <- function(df, m1, sd1, ciVal, relVal) {
  if (!is.null(ciVal) && !is.null(relVal)) {
    se <- stats::qnorm(1 - ((1 - ciVal) / 2)) * sqrt(relVal * (1 - relVal))
    z  <- (df$norm - m1) / sd1
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
  blocks <- unlist(strsplit(specStr, ";"))
  rows <- list()
  for (b in blocks) {
    b <- trimws(b)
    if (!nzchar(b)) next
    parts <- unlist(strsplit(b, ":", fixed = TRUE))
    if (length(parts) != 2L) return(NULL)
    varName <- trimws(parts[1])
    pairs <- unlist(strsplit(parts[2], ","))
    for (p in pairs) {
      p <- trimws(p)
      if (!nzchar(p)) next
      kv <- unlist(strsplit(p, "=", fixed = TRUE))
      if (length(kv) != 2L) return(NULL)
      rows[[length(rows) + 1L]] <- data.frame(
        variables = varName,
        levels = trimws(kv[1]),
        share = as.numeric(trimws(kv[2])),
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
    gettext("<p>Please select a <b>Raw Score Variable</b> and an <b>Explanatory Variable</b> (e.g., age or grade) to begin continuous norm modeling.</p>")
  } else if (type == "conventional") {
    gettext("<p>Please select a <b>Raw Score Variable</b> to compute single group norm scores.</p>")
  } else {
    gettext("<p>Please select an <b>Explanatory Variable</b> to form norm groups, or <b>Stratification Variables</b> to compute raking weights.</p>")
  }
  
  html$text <- msg
  jaspResults[["instructions"]] <- html
}