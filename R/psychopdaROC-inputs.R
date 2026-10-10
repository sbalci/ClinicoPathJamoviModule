# Parse free-text probabilities once, before computation or rendering.
.psychopdaProbabilityList <- function(text, endpoints = TRUE) {
  tokens <- trimws(strsplit(paste0(text, ","), ",", fixed = TRUE)[[1]])
  values <- suppressWarnings(as.numeric(tokens))
  valid <- is.finite(values) & if (endpoints) values >= 0 & values <= 1 else
    values > 0 & values < 1
  list(values = sort(unique(values[valid])), given = length(tokens),
    kept = sum(valid), duplicates = anyDuplicated(values[valid]) > 0L)
}

# Bound the number of points BEFORE allocating a user-specified threshold grid.
.psychopdaThresholdGrid <- function(text, max_points = 100L) {
  parts <- suppressWarnings(as.numeric(strsplit(text, ",", fixed = TRUE)[[1]]))
  valid <- length(parts) == 3L && all(is.finite(parts)) &&
    parts[1] > 0 && parts[2] < 1 && parts[1] <= parts[2] && parts[3] > 0
  if (!valid)
    return(list(values = seq(0.05, 0.5, by = 0.05), fallback = TRUE, capped = FALSE))
  count <- floor((parts[2] - parts[1]) / parts[3]) + 1
  capped <- !is.finite(count) || count > max_points
  values <- if (capped) seq(parts[1], parts[2], length.out = max_points) else
    seq(parts[1], parts[2], by = parts[3])
  list(values = unique(values), fallback = FALSE, capped = capped)
}

# Reserve the selected optimum and endpoints within the per-marker row limit.
.psychopdaThresholdIndices <- function(n, optimal, limit) {
  if (n <= limit) return(seq_len(n))
  selected <- unique(c(optimal, 1L, n))
  selected <- utils::head(selected, limit)
  remaining <- setdiff(seq_len(n), selected)
  slots <- limit - length(selected)
  if (slots > 0L)
    selected <- c(selected, remaining[round(seq(1, length(remaining), length.out = slots))])
  sort(unique(selected))
}

# Resolve both observed classes explicitly; extra levels never become controls implicitly.
.psychopdaClassCoding <- function(outcome, positive = NULL, negative = NULL) {
  values <- as.character(outcome)
  values[values %in% ""] <- NA_character_
  observed <- droplevels(factor(outcome, exclude = c(NA, "")))
  levels <- levels(observed)
  choice <- function(x) {
    if (!length(x) || is.na(x[1]) || !nzchar(x[1])) NULL else as.character(x[1])
  }
  positive <- choice(positive)
  negative <- choice(negative)
  status <- if (length(levels) < 2L) "single" else if
    (is.null(positive) || is.null(negative)) "unset" else if
    (identical(positive, negative)) "same" else if
    (!positive %in% levels) "invalid_positive" else if
    (!negative %in% levels) "invalid_negative" else "selected"
  included <- !is.na(values) & values %in% c(negative, positive)
  list(levels = levels, counts = as.integer(table(observed)), values = values,
    positive = positive, negative = negative, status = status, included = included,
    n_missing = sum(is.na(values)), n_other = sum(!is.na(values) & !included))
}

# IDs depend on pair positions, never on separators that can appear in marker names.
.psychopdaPairKey <- function(first, second) paste0("pair_", first, "_", second)

# Quoting and escaping each full name keeps a separator within a name unambiguous.
.psychopdaPairLabel <- function(first, second) {
  paste(encodeString(first, quote = '"'), "vs.", encodeString(second, quote = '"'))
}
.psychopdaAUCInference <- function(auc, se, method = "logit") {
  unavailable <- list(ci = c(NA_real_, NA_real_), p = NA_real_)
  if (!is.finite(auc) || !is.finite(se) || se <= 0 || auc <= 0 || auc >= 1)
    return(unavailable)
  if (identical(method, "delong")) {
    return(list(ci = pmax(0, pmin(1, auc + c(-1, 1) * stats::qnorm(.975) * se)),
      p = 2 * stats::pnorm(-abs((auc - .5) / se))))
  }
  # Delta-method logit interval and the matching test of logit(AUC) = 0.
  logit_se <- se / (auc * (1 - auc))
  list(ci = stats::plogis(stats::qlogis(auc) + c(-1, 1) * stats::qnorm(.975) * logit_se),
    p = 2 * stats::pnorm(-abs(stats::qlogis(auc) / logit_se)))
}

.psychopdaPatientData <- function(data, included, id, markers, outcome,
                                 subgroup = NULL, method = "rows") {
  selected <- data[included, , drop = FALSE]
  if (is.null(id) || !nzchar(id)) {
    if (!identical(method, "rows"))
      stop("patient_id_required", call. = FALSE)
    return(list(data = selected, rows = nrow(selected), patients = NA_integer_))
  }
  if (id %in% c(markers, outcome, subgroup))
    stop("patient_id_role",
      call. = FALSE)
  # Keep the original values: formatting numeric IDs as text can merge distinct
  # large identifiers through rounding before grouping.
  ids <- data[[id]]
  if (anyNA(ids[included]) || any(!nzchar(trimws(as.character(ids[included])))))
    stop("patient_id_missing",
      call. = FALSE)
  selected_ids <- unique(ids[included])
  if (identical(method, "rows")) {
    if (anyDuplicated(ids[included]))
      stop("patient_repeated",
        call. = FALSE)
    return(list(data = selected, rows = nrow(selected), patients = length(selected_ids)))
  }
  groups <- split(which(included), match(ids[included], selected_ids))
  # Check all observed outcomes for included patients, including otherwise excluded levels.
  all_groups <- split(seq_len(nrow(data)), match(ids, selected_ids))
  for (idx in all_groups) {
    values <- unique(as.character(data[[outcome]][idx]))
    values[values == ""] <- NA_character_
    if (length(values[!is.na(values)]) > 1L)
      stop("patient_outcome",
        call. = FALSE)
  }
  if (!is.null(subgroup) && nzchar(subgroup)) {
    if (any(vapply(groups, function(idx) length(unique(data[[subgroup]][idx])) > 1L,
      logical(1))))
      stop("patient_subgroup",
        call. = FALSE)
  }
  result <- data[vapply(groups, `[`, integer(1), 1L), , drop = FALSE]
  summarize <- if (identical(method, "patientMedian")) stats::median else mean
  for (marker in unique(markers)) {
    result[[marker]] <- vapply(groups, function(idx) {
      values <- jmvcore::toNumeric(data[[marker]][idx])
      if (all(is.na(values))) NA_real_ else summarize(values, na.rm = TRUE)
    }, numeric(1))
  }
  rownames(result) <- NULL
  list(data = result, rows = nrow(selected), patients = nrow(result))
}
