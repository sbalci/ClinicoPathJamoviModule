# A long reader study is converted to case rows only once, before resampling.
# Internal names are generated; identifiers containing separators cannot collide.
.paReaderPrepareLong <- function(data, o) {
  required <- c(o$caseID, o$readerID, o$modality, o$rating)
  if (length(required) != 4L) return(list(ready = FALSE,
    message = "Select case ID, reader ID, modality and rating for long input."))
  if (!all(c(required, o$referenceStandard, o$readerCluster) %in% names(data))) {
    stop("A selected long-input column is not in the data.")
  }
  ids <- lapply(required[1:3], function(v) as.character(data[[v]]))
  if (any(vapply(ids, function(x) anyNA(x) || any(!nzchar(x)), logical(1)))) {
    stop("Case, reader and modality IDs must be nonempty on every row.")
  }
  case <- ids[[1]]; reader <- ids[[2]]; modality <- ids[[3]]
  if (anyDuplicated(data.frame(case, reader, modality))) {
    stop("Duplicate case x reader x modality records; resolve them before analysis.")
  }
  mods <- c(o$baselineModality, o$comparisonModality)
  if (any(!nzchar(mods))) return(list(ready = FALSE,
    message = paste("Enter baseline and comparison modalities. Available:",
                     paste(sort(unique(modality)), collapse = ", "))))
  if (mods[1] == mods[2] || !all(mods %in% modality)) {
    stop("Select two distinct observed modality labels.")
  }
  if (o$useMetadataRows) stop("Long input uses explicit IDs, not metadata rows.")
  cases <- sort(unique(case)); readers <- sort(unique(reader[modality %in% mods]))
  first <- match(cases, case)
  consistent <- function(v, what) {
    x <- as.character(data[[v]])
    if (any(vapply(split(x, case), function(z) length(unique(z)) != 1L, logical(1)))) {
      stop(paste(what, "must be consistent within each case, including missingness."))
    }
    data[[v]][first]
  }
  wide <- data.frame(.case = cases)
  if (length(o$referenceStandard)) wide$.reference <- consistent(o$referenceStandard, "Reference")
  if (length(o$readerCluster)) wide$.cluster <- consistent(o$readerCluster, "Patient/cluster ID")
  map <- expand.grid(reader = readers, modality = mods, stringsAsFactors = FALSE)
  map$column <- paste0("reading_", seq_len(nrow(map)))
  recorded <- matrix(FALSE, length(cases), nrow(map))
  for (j in seq_len(nrow(map))) {
    take <- which(reader == map$reader[j] & modality == map$modality[j])
    at <- match(cases, case[take])
    wide[[map$column[j]]] <- data[[o$rating]][take[at]]
    recorded[, j] <- !is.na(at)
  }
  nested <- o
  nested$readerFormat <- "wide"; nested$caseID <- ".case"
  nested$vars <- map$column; nested$readerScores <- map$column
  nested$referenceStandard <- if (length(o$referenceStandard)) ".reference" else NULL
  nested$readerCluster <- if (length(o$readerCluster)) ".cluster" else NULL
  nested$readerPairs <- "all"
  result <- .paReaderPrepare(wide, nested)
  if (!isTRUE(result$ready)) return(result)
  if (any(result$status == "unmapped")) {
    stop("Unmapped long-input ratings. Map positive/negative labels or explicitly declare nondiagnostic results.")
  }
  result$options <- o
  result$vars <- paste(encodeString(map$reader, quote = "'"),
                      encodeString(map$modality, quote = "'"), sep = " / ")
  result$reference_message <- if (result$reference_ready)
    paste("Column:", o$referenceStandard, "positive:", o$referencePositive,
          "negative:", o$referenceNegative) else result$reference_message
  result$pairs <- list() # Between-reader pair tables belong to the wide workflow.
  result$long <- list(map = map, readers = readers, modalities = mods,
                     recorded = recorded, cases = cases,
                     original_rows = lapply(cases, function(x) which(case == x)))
  result
}

.paReaderModalityPopulation <- function(d) {
  observed <- apply(matrix(d$status %in% c("determinate", "nondiagnostic"),
                           nrow = nrow(d$status)), 1, all)
  determinate <- stats::complete.cases(d$calls)
  mask <- if (d$options$modalityPopulation == "all_observed") observed else determinate
  mask & !is.na(d$reference)
}

.paReaderModalityStatistic <- function(d, rows) {
  # Equal reader weights on a complete matched grid equal pooled-read rates.
  nreaders <- length(d$long$readers)
  result <- list()
  for (m in d$long$modalities) {
    cols <- which(d$long$map$modality == m)
    calls <- as.vector(d$calls[rows, cols, drop = FALSE])
    ref <- rep(d$reference[rows], length(cols))
    state <- as.vector(d$status[rows, cols, drop = FALSE])
    if (d$options$modalityPopulation == "all_observed") {
      tab <- .paReaderObserved(calls, ref, state)
    } else {
      tab <- .paReaderObserved(calls, ref, rep("determinate", length(calls)))[1:3, ]
    }
    result[[m]] <- tab
  }
  a <- result[[1]]; b <- result[[2]]
  estimates <- c(a$estimate, b$estimate, b$estimate - a$estimate)
  names(estimates) <- c(paste0("baseline_", seq_len(nrow(a))),
    paste0("comparison_", seq_len(nrow(b))), paste0("difference_", seq_len(nrow(a))))
  list(tables = result, statistic = estimates, nreaders = nreaders)
}

.paReaderModalityReport <- function(d, checkpoint = NULL) {
  empty <- list(readerModalitySummary = data.frame(), readerModalityContrast = data.frame())
  if (is.null(d$long) || !d$options$readerModalityComparison || !d$reference_ready) return(empty)
  mask <- .paReaderModalityPopulation(d)
  rows <- which(mask)
  out <- .paReaderModalityStatistic(d, rows)
  val <- out$statistic
  ci <- data.frame(lower = rep(NA_real_, length(val)), upper = NA_real_,
    status = if (length(rows)) "Bootstrap not requested" else "No complete eligible cases",
    attempted = 0L, undefined = 0L)
  if (d$options$readerIntervals && d$options$readerBootstrap && length(rows)) {
    ci <- .readerResample(data.frame(row = rows),
      function(z) .paReaderModalityStatistic(d, z$row)$statistic,
      clusters = if (is.null(d$clusters)) NULL else d$clusters[rows],
      samples = d$options$bootstrapSamples, confidence = d$options$readerConfidence,
      seed = d$options$seed, checkpoint = checkpoint)
  }
  k <- nrow(out$tables[[1]])
  summary <- do.call(rbind, lapply(seq_len(2), function(j) {
    tab <- out$tables[[j]]
    tab$modality <- d$long$modalities[j]
    tab$cases <- length(rows); tab$readers <- out$nreaders
    tab$readings <- length(rows) * out$nreaders
    tab$excluded_cases <- nrow(d$calls) - length(rows)
    tab$population <- d$options$modalityPopulation
    cbind(tab, ci[(j - 1) * k + seq_len(k), c("lower", "upper", "status", "attempted", "undefined")])
  }))
  contrast <- data.frame(baseline = d$long$modalities[1], comparison = d$long$modalities[2],
    metric = out$tables[[1]]$metric, population = d$options$modalityPopulation,
    cases = length(rows), readers = out$nreaders, readings = length(rows) * out$nreaders,
    excluded_cases = nrow(d$calls) - length(rows),
    units = if (is.null(d$clusters)) length(rows) else length(unique(d$clusters[rows])),
    difference = 100 * val[2 * k + seq_len(k)],
    lower = 100 * ci$lower[2 * k + seq_len(k)], upper = 100 * ci$upper[2 * k + seq_len(k)],
    status = ci$status[2 * k + seq_len(k)], attempted = ci$attempted[2 * k + seq_len(k)],
    undefined = ci$undefined[2 * k + seq_len(k)], row.names = NULL)
  list(readerModalitySummary = summary, readerModalityContrast = contrast)
}
