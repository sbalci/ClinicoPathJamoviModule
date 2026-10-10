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
