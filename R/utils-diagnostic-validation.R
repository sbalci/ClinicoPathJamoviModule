# Shared fixed-model diagnostic validation primitives. No model fitting or cutoff selection.
.dvProvenance <- function(table, options, text = NULL) {
  .deleteTableRows(table)
  if (!isTRUE(options$showValidationContext)) return(invisible(NULL))
  # Self-contained (no .diag* calls): this file also ships to modules without the comparator
  # helper. File-level code cannot call .(), so a caller wanting translated output passes
  # `text` (any subset of the names below). Show the reference label, not the option key,
  # and "Not specified" for blanks; break "<" before a letter because cells are parsed as HTML.
  t <- c(cohortRole = "Cohort role", samplingDesign = "Sampling design",
         thresholdSource = "Threshold source", modelVersion = "Model version",
         analysisUnit = "Unit of observation", referenceType = "Reference interpretation",
         referenceDescription = "Comparator description", not_specified = "Not specified",
         legacy = "Not specified", disease = "Disease reference",
         consensus = "Expert consensus", method = "Comparative method",
         note = "User-declared context does not verify independence or external validation. Mapping and exclusion ledgers define the analyzed pairs; unknown levels must not silently become negative.")
  if (length(text)) t[names(text)] <- unlist(text)
  fields <- c("cohortRole", "samplingDesign", "thresholdSource", "modelVersion",
              "analysisUnit", "referenceType", "referenceDescription")
  for (name in fields) {
    value <- tryCatch(as.character(options[[name]]), error = function(e) character())
    if (name == "referenceType" && length(value) == 1L &&
        value %in% c("legacy", "disease", "consensus", "method"))
      value <- t[[value]]
    value <- if (length(value) != 1L || is.na(value) || !nzchar(trimws(value))) t[["not_specified"]]
             else gsub("<(?=[A-Za-z/!?])", "< ", value, perl = TRUE)
    table$addRow(rowKey = name, values = list(setting = t[[name]], value = value))
  }
  table$setNote("provenance", t[["note"]])
}

.dvPR <- function(scores, labels, positive, higher = TRUE) {
  keep <- is.finite(scores) & !is.na(labels)
  y <- as.integer(labels[keep] == positive)
  x <- scores[keep]
  if (!length(y) || length(unique(y)) < 2L) return(NULL)
  if (!higher) x <- -x
  ord <- order(x, decreasing = TRUE)
  x <- x[ord]
  y <- y[ord]
  ends <- c(which(diff(x) != 0), length(x))
  tp <- cumsum(y)[ends]
  recall <- c(0, tp / sum(y))
  precision <- c(1, tp / ends)
  threshold <- c(Inf, x[ends])
  if (!higher) threshold <- -threshold
  f1 <- ifelse(precision + recall > 0, 2 * precision * recall / (precision + recall), 0)
  best <- which.max(f1)
  list(auprc = sum(diff(recall) * (utils::head(precision, -1) + utils::tail(precision, -1)) / 2),
       avg_precision = sum(diff(recall) * utils::tail(precision, -1)),
       max_f1 = f1[best], precision = precision[best], recall = recall[best],
       curve = data.frame(threshold = threshold, recall = recall, precision = precision),
       prevalence = mean(y), n_positive = sum(y), n_negative = sum(1 - y))
}

.dvNumbers <- function(text, lower = -Inf, upper = Inf, fallback = numeric()) {
  if (is.null(text) || !nzchar(trimws(text))) return(fallback)
  parts <- trimws(strsplit(text, ",", fixed = TRUE)[[1]])
  x <- suppressWarnings(as.numeric(parts))
  if (!length(x) || any(!is.finite(x)) || any(x < lower | x > upper) ||
      grepl(",$", trimws(text))) stop("Enter comma-separated numbers within the allowed range.")
  if (length(x) > 100) stop("At most 100 scenario values or cutoffs are allowed.")
  unique(x)
}

.dvWorkload <- function(sensitivity, specificity, prevalence, population, utility = .1) {
  if (any(!is.finite(c(prevalence, population, utility))) ||
      any(prevalence < 0 | prevalence > 1) || population <= 0 ||
      utility <= 0 || utility >= 1) stop("Invalid prevalence, population size or utility threshold.")
  if (any(is.finite(c(sensitivity, specificity)) &
          (c(sensitivity, specificity) < 0 | c(sensitivity, specificity) > 1))) {
    stop("Sensitivity and specificity must be proportions.")
  }
  tp <- population * prevalence * sensitivity
  fp <- population * (1 - prevalence) * (1 - specificity)
  # An absent target stratum contributes zero even if its conditional rate is undefined.
  tp[prevalence == 0] <- 0
  fp[prevalence == 1] <- 0
  fn <- population * prevalence - tp
  tn <- population * (1 - prevalence) - fp
  flags <- tp + fp
  ratio <- function(a, b) ifelse(is.finite(b) & b > 0, a / b, NA_real_)
  data.frame(prevalence = prevalence, population = population, tp = tp, fp = fp,
             fn = fn, tn = tn, flags = flags, ppv = ratio(tp, flags),
             npv = ratio(tn, tn + fn), flags_per_1000 = 1000 * flags / population,
             flags_per_detected = ratio(flags, tp), tests_per_detected = ratio(population, tp),
             net_benefit = (tp - fp * utility / (1 - utility)) / population)
}

.dvCounts <- function(y, scores, cutoff, higher = TRUE) {
  flag <- if (higher) scores >= cutoff else scores <= cutoff
  c(tp = sum(flag & y == 1), fp = sum(flag & y == 0),
    fn = sum(!flag & y == 1), tn = sum(!flag & y == 0))
}

# Area under the precision-recall curve interpolated linearly in (TP, FP) counts between
# neighbouring thresholds, which traces achievable operating points (Davis & Goadrich 2006),
# integrated exactly (Keilwagen, Grosse & Grau 2014; equal to PRROC's auc.integral). Per step a->b:
# precision(t) = (A + B t) / (C + D t) and recall gains B / P, so the area is
# B/P * [B/D + (A D - B C) / D^2 * log((C + D) / C)], or B/P * B/D at C = 0.
# .dvPR()'s $auprc is the PR-space trapezoid from a (recall 0, precision 1) anchor, which overstates
# the area when scores tie across classes; psychopdaROC and precisionrecall still report that one.
.dvAuprc <- function(scores, labels, positive, higher = TRUE) {
  keep <- is.finite(scores) & !is.na(labels)
  y <- as.integer(labels[keep] == positive)
  if (!length(y) || length(unique(y)) < 2L) return(NA_real_)
  x <- if (higher) scores[keep] else -scores[keep]
  ord <- order(x, decreasing = TRUE)
  ends <- c(which(diff(x[ord]) != 0), length(x))
  tp <- c(0, cumsum(y[ord])[ends])
  n <- c(0, ends)
  A <- utils::head(tp, -1); B <- diff(tp); C <- utils::head(n, -1); D <- diff(n)
  step <- ifelse(C == 0, B / D, B / D + (A * D - B * C) / D^2 * log((C + D) / C))
  sum(B / sum(y) * step)
}

.dvScoreMetrics <- function(y, scores, cutoff, higher = TRUE, probability = FALSE) {
  z <- .dvCounts(y, scores, cutoff, higher)
  div <- function(a, b) if (b > 0) unname(a / b) else NA_real_
  pr <- .dvPR(scores, y, 1, higher)
  n1 <- sum(y == 1)
  n0 <- sum(y == 0)
  ranks <- rank(if (higher) scores else -scores)
  auc <- if (n1 * n0 > 0) (sum(ranks[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0) else NA_real_
  ans <- c(auc = auc, ap = if (is.null(pr)) NA_real_ else pr$avg_precision,
           pr_auc = if (is.null(pr)) NA_real_ else .dvAuprc(scores, y, 1, higher),   # interpolated, as the main path
           sensitivity = div(z['tp'], n1), specificity = div(z['tn'], n0),
           ppv = div(z['tp'], z['tp'] + z['fp']),
           npv = div(z['tn'], z['tn'] + z['fn']),
           f1 = div(2 * z['tp'], 2 * z['tp'] + z['fp'] + z['fn']),
           flags_per_1000 = 1000 * (z['tp'] + z['fp']) / length(y),
           brier = NA_real_, calibration_intercept = NA_real_,
           calibration_slope = NA_real_, calibration_in_large = NA_real_)
  names(ans)[names(ans) == 'flags_per_1000.tp'] <- 'flags_per_1000'
  if (n1 == 0 || n0 == 0) ans[c("ppv", "npv")] <- NA_real_
  if (probability) {
    if (any(scores < 0 | scores > 1)) stop("Probability inputs must lie between 0 and 1.")
    if (!higher) stop("Probability inputs must denote the selected positive class (higher is positive).")
    ans['brier'] <- mean((scores - y)^2)
    # Calibration logits at exact 0/1 are undefined; never silently clip or refit predictions.
    if (n0 > 0 && n1 > 0 && all(scores > 0 & scores < 1)) {
      logits <- stats::qlogis(scores)
      fit <- suppressWarnings(tryCatch(stats::glm(y ~ logits, family = stats::binomial()),
                                      error = function(e) NULL))
      citl <- suppressWarnings(tryCatch(stats::glm(y ~ 1, offset = logits,
                                   family = stats::binomial()), error = function(e) NULL))
      valid <- function(f) !is.null(f) && isTRUE(f$converged) &&
        all(is.finite(stats::coef(f))) && !any(f$fitted.values < 1e-8 | f$fitted.values > 1-1e-8)
      if (valid(fit)) ans[c('calibration_intercept', 'calibration_slope')] <- stats::coef(fit)
      if (valid(citl)) ans['calibration_in_large'] <- stats::coef(citl)[1]
    }
  }
  ans
}

.dvBootstrap <- function(y, scores, cluster, cutoff, higher = TRUE, probability = FALSE,
                         draws = 999L, seed = 1L, confidence = .95, site = NULL) {
  if (anyNA(cluster) || any(!nzchar(as.character(cluster)))) stop("Cluster IDs must be complete.")
  if (draws < 100 || confidence <= 0 || confidence >= 1) stop("Invalid bootstrap settings.")
  scores <- as.matrix(scores)
  if (length(cluster) != length(y) || nrow(scores) != length(y)) stop("Mismatched input lengths.")
  if (is.null(site)) site <- rep("All", length(y))
  groups <- split(seq_along(y), as.character(cluster))
  if (any(vapply(groups, function(i) length(unique(y[i])) != 1L, logical(1)))) {
    stop("Reference status must be consistent within each patient cluster.")
  }
  if (any(vapply(groups, function(i) length(unique(site[i])) != 1L, logical(1)))) {
    stop("Each patient cluster must belong to one site.")
  }
  strata <- split(names(groups), vapply(groups, function(i) as.character(site[i[1]]), character(1)))
  if (any(lengths(strata) < 2L)) stop("Each fixed site needs at least two independent clusters.")
  point <- vapply(seq_len(ncol(scores)), function(j)
    .dvScoreMetrics(y, scores[, j], cutoff, higher, probability), numeric(13))
  # Preserve both the caller's RNG state and its absence.
  had_seed <- exists('.Random.seed', envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get('.Random.seed', envir = .GlobalEnv)
  on.exit(if (had_seed) assign('.Random.seed', old_seed, envir = .GlobalEnv)
          else if (exists('.Random.seed', envir = .GlobalEnv, inherits = FALSE))
            rm('.Random.seed', envir = .GlobalEnv), add = TRUE)
  set.seed(seed)
  boot <- array(NA_real_, c(nrow(point), ncol(point), draws))
  for (b in seq_len(draws)) {
    picked <- unlist(lapply(strata, function(ids) ids[sample.int(length(ids), length(ids), TRUE)]),
                     use.names = FALSE)
    idx <- unlist(groups[picked], use.names = FALSE)
    for (j in seq_len(ncol(scores))) {
      boot[, j, b] <- .dvScoreMetrics(y[idx], scores[idx, j], cutoff, higher, probability)
    }
  }
  summarize <- function(values, estimate) {
    good <- is.finite(values)
    valid_n <- sum(good)
    limits <- c(NA_real_, NA_real_)
    status <- if (!is.finite(estimate)) "Metric undefined" else if (valid_n < max(100, .8 * draws))
      "Insufficient valid replicates" else if (diff(range(values[good])) < 1e-12)
        "Degenerate bootstrap; interval unavailable" else "Percentile; fixed model and sites"
    if (identical(status, "Percentile; fixed model and sites")) {
      limits <- unname(stats::quantile(values[good], c((1-confidence)/2, (1+confidence)/2)))
    }
    list(estimate = estimate, lower = limits[1], upper = limits[2],
         valid = valid_n, failed = draws-valid_n, status = status)
  }
  rows <- list()
  for (j in seq_len(ncol(scores))) for (m in seq_len(nrow(point))) {
    rows[[length(rows)+1L]] <- c(list(model = colnames(scores)[j], metric = rownames(point)[m]),
                                summarize(boot[m, j, ], point[m, j]))
  }
  if (ncol(scores) > 1L) for (j in 2:ncol(scores)) for (m in seq_len(nrow(point))) {
    rows[[length(rows)+1L]] <- c(list(model = paste(colnames(scores)[j], "minus", colnames(scores)[1]),
      metric = rownames(point)[m]), summarize(boot[m, j, ] - boot[m, 1, ], point[m, j]-point[m, 1]))
  }
  do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE))
}
