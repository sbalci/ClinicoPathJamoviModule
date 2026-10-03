# Input preparation for the opt-in reader diagnostic workflow. No row is dropped
# for another reader's missing value; estimators choose their own populations.
.paReaderDefaults <- function() {
  list(readerWorkflow = "off", vars = NULL, readerScores = NULL,
       referenceStandard = NULL, caseID = NULL, useMetadataRows = FALSE,
       readerCluster = NULL, readerPositive = "", readerNegative = "",
       referencePositive = "", referenceNegative = "", referenceAccuracy = TRUE,
       readerPopulation = "available", clinicalCutoff = "", positiveRule = "ge",
       nearCutoffWidth = "", ordinalCutoff = "", nearCutoffLevels = "",
       readerPairs = "all", readerPair1 = "", readerPair2 = "", readerCaseLimit = 50L,
       readerIntervals = TRUE, readerBootstrap = FALSE, readerConfidence = 0.95,
       bootstrapSamples = 1000L, seed = 42L, wght = "unweighted",
       showKappaBalanced = FALSE, compositeReader = "", readerCode = FALSE)
}

.paReaderPrepare <- function(data, options) {
  o <- utils::modifyList(.paReaderDefaults(), options)
  pending <- function(message) list(ready = FALSE, message = message)
  numeric_mode <- identical(o$readerWorkflow, "numeric_cutoff")
  vars <- if (numeric_mode) o$readerScores else o$vars
  if (length(vars) < 2L) return(pending("Select at least two independent reader columns."))
  if (anyDuplicated(vars)) stop("Select distinct reader columns.")
  selected <- c(vars, o$referenceStandard, o$caseID, o$readerCluster)
  if (!all(selected %in% names(data))) stop("A selected column is not in the data.")
  if (!o$readerWorkflow %in% c("binary", "numeric_cutoff", "ordinal_cutoff")) {
    stop("Choose a reader workflow.")
  }
  rows <- seq_len(nrow(data))
  meta <- rep(FALSE, length(rows))
  if (o$useMetadataRows && length(o$caseID)) {
    meta <- grepl("^META_", as.character(data[[o$caseID]]), ignore.case = TRUE)
  }
  rows <- rows[!meta]
  raw <- data[rows, vars, drop = FALSE]
  # Binary metadata labels can be removed, but an ordinal scale is declared by
  # the user: an unobserved boundary category must retain its position.
  only_meta <- setdiff(unlist(lapply(data[meta, vars, drop = FALSE], as.character)),
                       unlist(lapply(raw, as.character)))
  for (v in vars) {
    if (is.factor(raw[[v]]) && any(meta) && o$readerWorkflow != "ordinal_cutoff") {
      raw[[v]] <- factor(as.character(raw[[v]]),
                        levels = setdiff(levels(raw[[v]]), only_meta),
                        ordered = is.ordered(raw[[v]]))
    }
  }
  if (!length(rows)) return(pending("No case rows remain after removing metadata."))
  levels_for <- function(x) if (is.factor(x)) levels(x) else unique(as.character(x[!is.na(x)]))
  labels <- function(positive, negative, available, what) {
    if (identical(positive, negative)) stop(paste(what, "positive and negative labels must differ."))
    if (!all(c(positive, negative) %in% available)) {
      stop(paste0(what, " labels must match available levels: ", paste(available, collapse = ", ")))
    }
  }
  number <- function(x, what) {
    value <- suppressWarnings(as.numeric(x))
    if (length(value) != 1L || !is.finite(value)) stop(paste(what, "must be one finite number."))
    value
  }
  available <- unique(unlist(lapply(raw, levels_for), use.names = FALSE))
  calls <- near <- matrix(NA, nrow(raw), length(vars), dimnames = list(NULL, vars))
  near[,] <- FALSE
  has_band <- FALSE
  scale <- NULL
  if (o$readerWorkflow == "binary") {
    if (!nzchar(o$readerPositive) || !nzchar(o$readerNegative)) {
      return(pending(paste0("Specify positive and negative reader labels. Available: ",
                            paste(available, collapse = ", "))))
    }
    labels(o$readerPositive, o$readerNegative, available, "Reader")
    for (j in seq_along(vars)) {
      x <- as.character(raw[[j]])
      calls[, j] <- ifelse(x %in% c(o$readerPositive, o$readerNegative),
                          x == o$readerPositive, NA)
    }
    rule <- paste("Positive:", o$readerPositive, "Negative:", o$readerNegative)
  } else {
    if (numeric_mode) {
      if (!all(vapply(raw, is.numeric, logical(1)))) stop("Numeric cutoff mode requires numeric scores.")
      if (any(vapply(raw, function(x) any(!is.finite(x) & !is.na(x)), logical(1)))) {
        stop("Reader scores contain infinite values. Correct them or mark them missing.")
      }
      if (!nzchar(o$clinicalCutoff)) return(pending("Enter the prespecified clinical cutoff."))
      boundary <- number(o$clinicalCutoff, "Clinical cutoff")
      scores <- as.matrix(raw)
      if (nzchar(o$nearCutoffWidth)) {
        width <- number(o$nearCutoffWidth, "Near-cutoff width")
        if (width < 0) stop("Near-cutoff width must be nonnegative.")
        near <- abs(scores - boundary) <= width
        has_band <- TRUE
      }
    } else {
      if (!all(vapply(raw, is.ordered, logical(1)))) {
        stop("Ordinal cutoff mode requires ordered factors with a common declared level order.")
      }
      scale <- levels(raw[[1]])
      if (length(scale) < 2L || !all(vapply(raw, function(x) identical(levels(x), scale), logical(1)))) {
        stop("All ordinal readers must use the same declared level order.")
      }
      if (!nzchar(o$ordinalCutoff)) return(pending("Enter the ordinal boundary category."))
      boundary <- match(o$ordinalCutoff, scale)
      if (is.na(boundary)) stop(paste("Unknown ordinal boundary. Available:", paste(scale, collapse = ", ")))
      scores <- matrix(vapply(raw, as.integer, integer(nrow(raw))), nrow = nrow(raw))
      if (nzchar(o$nearCutoffLevels)) {
        # The jamovi TextBox is single-line; R callers may also supply newlines.
        band <- if (o$nearCutoffLevels %in% scale) o$nearCutoffLevels else
          if (grepl("\n", o$nearCutoffLevels, fixed = TRUE))
            strsplit(o$nearCutoffLevels, "\n", fixed = TRUE)[[1]] else
              trimws(strsplit(o$nearCutoffLevels, ";", fixed = TRUE)[[1]])
        if (!all(band %in% scale)) stop("Every near-cutoff category must match a declared ordinal level.")
        near <- matrix(scores %in% match(band, scale), nrow(raw), length(vars))
        has_band <- TRUE
      }
    }
    calls <- switch(o$positiveRule, ge = scores >= boundary, gt = scores > boundary,
                    le = scores <= boundary, lt = scores < boundary,
                    stop("Unknown cutoff inequality."))
    colnames(calls) <- vars
    signs <- c(ge = ">=", gt = ">", le = "<=", lt = "<")
    rule <- paste("Positive when score", signs[[o$positiveRule]],
                  if (numeric_mode) o$clinicalCutoff else o$ordinalCutoff)
  }
  near[is.na(near)] <- FALSE
  ref <- rep(NA, nrow(raw))
  ref_raw <- rep(NA_character_, nrow(raw))
  ref_ready <- FALSE
  ref_message <- "Reference accuracy is off or no reference was selected."
  if (o$referenceAccuracy && length(o$referenceStandard)) {
    ref_raw <- as.character(data[[o$referenceStandard]][rows])
    ref_levels <- levels_for(data[[o$referenceStandard]][rows])
    if (!nzchar(o$referencePositive) || !nzchar(o$referenceNegative)) {
      ref_message <- paste("Specify both reference labels. Available:", paste(ref_levels, collapse = ", "))
    } else {
      labels(o$referencePositive, o$referenceNegative, ref_levels, "Reference")
      ref <- ifelse(ref_raw %in% c(o$referencePositive, o$referenceNegative),
                    ref_raw == o$referencePositive, NA)
      ref_ready <- TRUE
      ref_message <- paste("Column:", o$referenceStandard, "positive:", o$referencePositive,
                           "negative:", o$referenceNegative)
    }
  }
  ids <- as.character(rows)
  if (length(o$caseID)) {
    supplied <- as.character(data[[o$caseID]][rows])
    ok <- !is.na(supplied) & nzchar(supplied)
    ids[ok] <- supplied[ok]
  }
  clusters <- NULL
  if (length(o$readerCluster)) {
    clusters <- as.character(data[[o$readerCluster]][rows])
    if (anyNA(clusters) || any(!nzchar(clusters))) stop("Every case row must have a patient/cluster ID.")
  }
  pairs <- utils::combn(seq_along(vars), 2, simplify = FALSE)
  if (o$readerPairs == "selected") {
    if (!nzchar(o$readerPair1) || !nzchar(o$readerPair2)) return(pending("Enter both selected reader column names."))
    idx <- match(c(o$readerPair1, o$readerPair2), vars)
    if (anyNA(idx) || idx[1] == idx[2]) stop("The selected pair must name two distinct selected reader columns.")
    pairs <- list(idx)
  }
  list(ready = TRUE, options = o, raw = raw, calls = calls, near = near,
       has_band = has_band, reference = ref, reference_raw = ref_raw,
       reference_ready = ref_ready, reference_message = ref_message,
       rows = rows, ids = ids, clusters = clusters, pairs = pairs, rule = rule,
       scale = scale, metadata_n = sum(meta), vars = vars,
       shared = stats::complete.cases(calls) & !is.na(ref))
}
