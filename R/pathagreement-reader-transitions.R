# Category direction is defined only for two determinate ordinal readings.
# Nondiagnostic is an adequacy state, never the lowest severity category.
.paReaderTransitions <- function(d) {
  empty <- list(readerTransitionsTable = data.frame(), readerTransitionMatrix = data.frame())
  if (!d$options$readerTransitions) return(empty)
  if (d$options$readerWorkflow != "ordinal_cutoff") {
    stop("Category transitions require the ordinal cutoff workflow.")
  }
  if (is.null(d$long)) {
    labels <- c(d$options$transitionBaseline, d$options$transitionComparison)
    idx <- match(labels, d$vars)
    if (anyNA(idx) || idx[1] == idx[2]) {
      stop("Select distinct baseline and comparison reader column names for transitions.")
    }
    pairs <- list(idx); names(pairs) <- paste(labels, collapse = " to ")
  } else {
    pairs <- lapply(d$long$readers, function(reader) {
      vapply(d$long$modalities, function(m)
        which(d$long$map$reader == reader & d$long$map$modality == m), integer(1))
    })
    names(pairs) <- d$long$readers
  }
  summaries <- matrices <- list()
  for (p in seq_along(pairs)) {
    pair <- pairs[[p]]; i <- pair[1]; j <- pair[2]
    a <- as.integer(d$raw[[i]]); b <- as.integer(d$raw[[j]])
    sa <- d$status[, i]; sb <- d$status[, j]
    det <- sa == "determinate" & sb == "determinate"
    obs <- sa %in% c("determinate", "nondiagnostic") &
      sb %in% c("determinate", "nondiagnostic")
    counts <- c(sum(a[det] == b[det]), sum(b[det] > a[det]), sum(b[det] < a[det]),
      sum(!d$calls[det, i] & d$calls[det, j]), sum(d$calls[det, i] & !d$calls[det, j]),
      sum(sa == "nondiagnostic" & sb == "determinate"),
      sum(sa == "determinate" & sb == "nondiagnostic"),
      sum(sa == "nondiagnostic" & sb == "nondiagnostic"), sum(!obs))
    denominator <- c(rep(sum(det), 5), rep(sum(obs), 3), length(obs))
    summaries[[p]] <- data.frame(pair = names(pairs)[p],
      transition = c("Unchanged category", "Higher category", "Lower category",
        "Negative to positive", "Positive to negative", "Became determinate",
        "Became nondiagnostic", "Nondiagnostic in both", "Missing/unmapped pair"),
      population = c(rep("Both determinate", 5), rep("Both observed", 3), "All case pairs"),
      numerator = counts, denominator = denominator,
      estimate = ifelse(denominator > 0, counts / denominator, NA_real_))
    categories <- c(d$scale, if (d$options$readerNondiagnostic) d$options$readerNondiagnosticLevel)
    ca <- factor(as.character(d$original_raw[[i]][obs]), levels = categories)
    cb <- factor(as.character(d$original_raw[[j]][obs]), levels = categories)
    tab <- as.data.frame(table(baseline = ca, comparison = cb), stringsAsFactors = FALSE)
    names(tab)[3] <- "n"
    tab$pair <- names(pairs)[p]; tab$denominator <- sum(obs)
    tab$baseline_status <- ifelse(tab$baseline == d$options$readerNondiagnosticLevel,
                                   "nondiagnostic", "determinate")
    tab$comparison_status <- ifelse(tab$comparison == d$options$readerNondiagnosticLevel,
                                     "nondiagnostic", "determinate")
    matrices[[p]] <- tab
  }
  list(readerTransitionsTable = do.call(rbind, summaries),
       readerTransitionMatrix = do.call(rbind, matrices))
}
