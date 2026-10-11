.dualROCOracle <- local({
# Independent ROC numerical oracles, created 2026-10-11.
# This file was derived without reading either ClinicoPath analysis, its helpers,
# or existing tests. Source this file to use the independent functions without
# running any pROC or scikit-learn calls. No ClinicoPath package is loaded.
#
# Run: LANG=en_US.UTF-8 Rscript --vanilla /private/tmp/roc-independent-oracles.R
#
# Definitions: positive = Case; higher means score >= cutoff is positive,
# lower means score <= cutoff is positive. A missing outcome or marker is omitted
# for one-marker inference; paired inference omits any row missing either marker.
#
# Sources checked 2026-10-11:
# DeLong ER, DeLong DM, Clarke-Pearson DL (1988), Biometrics 44:837-845.
# PMID 3203132; DOI 10.2307/2531595.
# https://pubmed.ncbi.nlm.nih.gov/3203132/
# https://cran.r-project.org/web/packages/pROC/refman/pROC.html
# https://scikit-learn.org/stable/modules/generated/sklearn.metrics.average_precision_score.html
# https://scikit-learn.org/stable/modules/generated/sklearn.metrics.precision_recall_curve.html
# https://cran.r-project.org/web/packages/PRROC/refman/PRROC.html
# The original paper's abstract was verified; the formula below is written out
# from the U-statistic estimand and independently adjudicated against pROC.

oracle_clean <- function(outcome, score, positive = "Case") {
  stopifnot(length(outcome) == length(score), is.numeric(score))
  keep <- !is.na(outcome) & !is.na(score)
  outcome <- as.character(outcome[keep])
  score <- score[keep]
  if (any(!is.finite(score))) stop("Oracle requires finite nonmissing scores")
  if (length(unique(outcome)) != 2L || !positive %in% outcome) {
    stop("Oracle requires two observed outcome classes, including positive")
  }
  list(y = outcome == positive, score = score, keep = which(keep))
}

oracle_influence <- function(outcome, score, direction = "higher", positive = "Case") {
  stopifnot(direction %in% c("higher", "lower"))
  d <- oracle_clean(outcome, score, positive)
  oriented <- if (direction == "higher") d$score else -d$score
  x <- oriented[d$y]
  z <- oriented[!d$y]
  if (length(x) < 2L || length(z) < 2L) stop("Need at least two per class for variance")
  # phi(x,z) = I(x > z) + 0.5 I(x = z). Cases are rows, controls columns.
  phi <- outer(x, z, ">") + 0.5 * outer(x, z, "==")
  # U = mean(phi); case influence is mean over controls, and conversely.
  case_influence <- rowMeans(phi)
  control_influence <- colMeans(phi)
  list(auc = mean(phi), case = case_influence, control = control_influence,
       n_case = length(x), n_control = length(z), keep = d$keep,
       pair_count = length(phi), wins = sum(phi == 1), ties = sum(phi == 0.5))
}

oracle_auc <- function(outcome, score, direction = "higher", positive = "Case",
                       conf.level = 0.95) {
  stopifnot(length(conf.level) == 1L, conf.level > 0, conf.level < 1)
  v <- oracle_influence(outcome, score, direction, positive)
  # Independent observations within each class: S = var(V_case)/m + var(V_control)/n.
  variance <- stats::var(v$case) / v$n_case + stats::var(v$control) / v$n_control
  se <- sqrt(variance)
  ci <- pmax(0, pmin(1, v$auc + c(-1, 1) * stats::qnorm((1 + conf.level) / 2) * se))
  c(n = v$n_case + v$n_control, n_case = v$n_case, n_control = v$n_control,
    auc = v$auc, variance = variance, se = se, ci_lower = ci[1], ci_upper = ci[2],
    pair_wins = v$wins, pair_ties = v$ties, pair_count = v$pair_count)
}

oracle_auc_rank <- function(outcome, score, direction = "higher", positive = "Case") {
  d <- oracle_clean(outcome, score, positive)
  x <- if (direction == "higher") d$score else -d$score
  m <- sum(d$y)
  n <- sum(!d$y)
  # Separate Mann-Whitney rank-sum route; ties receive their mean rank.
  (sum(rank(x, ties.method = "average")[d$y]) - m * (m + 1) / 2) / (m * n)
}

oracle_paired <- function(outcome, score1, score2, direction1 = "higher",
                          direction2 = direction1, positive = "Case", conf.level = 0.95) {
  stopifnot(length(outcome) == length(score1), length(outcome) == length(score2))
  keep <- !is.na(outcome) & !is.na(score1) & !is.na(score2)
  v1 <- oracle_influence(outcome[keep], score1[keep], direction1, positive)
  v2 <- oracle_influence(outcome[keep], score2[keep], direction2, positive)
  # Structural components remain aligned by patient in each outcome stratum.
  s <- stats::cov(cbind(v1$case, v2$case)) / v1$n_case +
       stats::cov(cbind(v1$control, v2$control)) / v1$n_control
  contrast <- c(1, -1)
  difference <- v1$auc - v2$auc
  variance <- drop(t(contrast) %*% s %*% contrast)
  se <- sqrt(variance)
  z <- if (se > 0) difference / se else NA_real_
  p <- if (is.finite(z)) 2 * stats::pnorm(-abs(z)) else NA_real_
  ci <- difference + c(-1, 1) * stats::qnorm((1 + conf.level) / 2) * se
  list(summary = c(n = sum(keep), n_case = v1$n_case, n_control = v1$n_control,
                   auc1 = v1$auc, auc2 = v2$auc, variance1 = s[1, 1],
                   variance2 = s[2, 2], covariance = s[1, 2],
                   difference = difference, difference_variance = variance,
                   se = se, z = z, p = p, ci_lower = ci[1], ci_upper = ci[2]),
       covariance_matrix = s, keep = which(keep))
}

oracle_divide <- function(numerator, denominator) {
  value <- numerator / denominator
  value[is.nan(value)] <- NA_real_
  value
}

oracle_thresholds <- function(outcome, score, cutpoints, direction = "higher",
                              positive = "Case") {
  stopifnot(direction %in% c("higher", "lower"), is.numeric(cutpoints))
  d <- oracle_clean(outcome, score, positive)
  m <- sum(d$y)
  n <- sum(!d$y)
  rows <- lapply(cutpoints, function(cutpoint) {
    classified <- if (direction == "higher") d$score >= cutpoint else d$score <= cutpoint
    tp <- sum(classified & d$y)
    fn <- sum(!classified & d$y)
    fp <- sum(classified & !d$y)
    tn <- sum(!classified & !d$y)
    sensitivity <- tp / m
    specificity <- tn / n
    # Integer numerators keep mathematically identical optima together.
    youden_numerator <- tp * n + tn * m - m * n
    distance_numerator <- fn^2 * n^2 + fp^2 * m^2
    data.frame(threshold = cutpoint, tp, fn, fp, tn, sensitivity, specificity,
               ppv = oracle_divide(tp, tp + fp), npv = oracle_divide(tn, tn + fn),
               lr_pos = oracle_divide(sensitivity, 1 - specificity),
               lr_neg = oracle_divide(1 - sensitivity, specificity),
               youden = youden_numerator / (m * n),
               distance_squared = distance_numerator / (m^2 * n^2),
               youden_numerator, distance_numerator)
  })
  do.call(rbind, rows)
}

oracle_candidates <- function(outcome, score, direction = "higher", positive = "Case",
                              grid = c("midpoint", "observed")) {
  grid <- match.arg(grid)
  d <- oracle_clean(outcome, score, positive)
  values <- sort(unique(d$score))
  cuts <- if (grid == "midpoint") {
    c(-Inf, head(values, -1L) / 2 + tail(values, -1L) / 2, Inf)
  } else {
    c(-Inf, values, Inf)
  }
  tab <- oracle_thresholds(outcome, score, cuts, direction, positive)
  tab$best_youden <- tab$youden_numerator == max(tab$youden_numerator)
  tab$best_closest <- tab$distance_numerator == min(tab$distance_numerator)
  tab
}

oracle_pr <- function(outcome, score, direction = "higher", positive = "Case") {
  d <- oracle_clean(outcome, score, positive)
  stopifnot(direction %in% c("higher", "lower"))
  # Add whole equal-score groups. Never decide an arbitrary order inside a tie.
  thresholds <- sort(unique(d$score), decreasing = direction == "higher")
  tab <- oracle_thresholds(outcome, score, thresholds, direction, positive)
  recall <- c(0, tab$sensitivity)
  precision <- c(1, tab$ppv)
  delta_recall <- diff(recall)
  trapezoidal <- sum(delta_recall * (head(precision, -1L) + tail(precision, -1L)) / 2)
  average_precision <- sum(delta_recall * tail(precision, -1L))
  tp_previous <- c(0, head(tab$tp, -1L))
  fp_previous <- c(0, head(tab$fp, -1L))
  delta_tp <- tab$tp - tp_previous
  delta_fp <- tab$fp - fp_previous
  # Interpolate counts within a whole score-tie group, not precision itself:
  # P(t)=(TP0+t*dTP)/(TP0+FP0+t*(dTP+dFP)), dRecall=dTP/nCase * dt.
  # If no observations have yet been selected, the right-hand limit is dTP/(dTP+dFP).
  # A pure-control group has no recall width and contributes zero area.
  piece_integral <- piece_closed_form <- numeric(nrow(tab))
  for (i in seq_len(nrow(tab))) {
    if (delta_tp[i] == 0) next
    a <- tp_previous[i]
    b <- delta_tp[i]
    c0 <- tp_previous[i] + fp_previous[i]
    d0 <- delta_tp[i] + delta_fp[i]
    integrand <- function(t) {
      denominator <- c0 + t * d0
      ifelse(denominator == 0, b / d0, (a + t * b) / denominator)
    }
    piece_integral[i] <- stats::integrate(integrand, lower = 0, upper = 1,
      rel.tol = 1e-12, abs.tol = 1e-13, subdivisions = 1000L)$value * delta_recall[i]
    piece_closed_form[i] <- delta_recall[i] * if (c0 == 0) b / d0 else
      b / d0 + (a * d0 - b * c0) / d0^2 * log1p(d0 / c0)
  }
  list(trapezoidal = trapezoidal, average_precision = average_precision,
       interpolated = sum(piece_integral), interpolated_closed_form = sum(piece_closed_form),
       points = data.frame(threshold = c(NA_real_, thresholds), recall, precision),
       interpolation_pieces = data.frame(thresholds, tp_previous, fp_previous, delta_tp, delta_fp,
                                        area_numeric = piece_integral, area_closed_form = piece_closed_form))
}

make_roc_oracle_fixtures <- function() {
  fixtures <- list()
  control_counts <- c(7, 5, 5, 4, 4, 3, 2, 2)
  case_counts <- c(2, 2, 3, 4, 4, 5, 5, 7)
  specifications <- list(
    balanced = list(control = control_counts, case = case_counts),
    imbalanced = list(control = 2 * control_counts, case = c(0, 1, 1, 2, 2, 3, 3, 4))
  )
  for (population in names(specifications)) {
    spec <- specifications[[population]]
    control <- rep(0:7, spec$control)
    case <- rep(0:7, spec$case)
    score1 <- as.numeric(c(control, case))
    disease <- factor(c(rep("Control", length(control)), rep("Case", length(case))),
                      levels = c("Case", "Control"))
    # Pairing is fixed by row. The second marker is noisier and attenuated in cases.
    score2 <- pmax(-2, pmin(9, score1 +
      rep(c(1, -1, 0, 2, -2, 0, 1, -1), length.out = length(score1)) -
      as.integer(disease == "Case")))
    original <- data.frame(subject_id = seq_along(score1), disease, score1, score2)
    for (missing in c(FALSE, TRUE)) {
      d <- original
      if (missing) {
        d$disease[c(5, length(control) + 9)] <- NA
        d$score1[c(11, length(control) + 12)] <- NA
        d$score2[c(19, length(control) + 15)] <- NA
      }
      for (direction in c("higher", "lower")) {
        dd <- d
        if (direction == "lower") dd[c("score1", "score2")] <- -dd[c("score1", "score2")]
        name <- paste(population, if (missing) "missing" else "complete", direction, sep = "_")
        fixtures[[name]] <- list(data = dd, direction = direction, positive = "Case",
                                 description = paste(population,
                                   if (missing) "distinct outcome/marker NA rows" else "complete",
                                   direction, "scores indicate Case"))
      }
    }
  }
  # Endpoint fixtures: a constant marker has interpolated PR area equal to prevalence;
  # a mixed highest tie distinguishes precision=1 at recall=0 from the right-hand limit.
  original <- fixtures$balanced_complete_higher$data
  for (kind in c("constant", "mixed_highest")) {
    d <- original
    if (kind == "constant") {
      d$score1 <- d$score2 <- rep(2, nrow(d))
    } else {
      d$score1 <- c(rep(0, 24), rep(2, 8), rep(1, 24), rep(2, 8))
    }
    for (direction in c("higher", "lower")) {
      dd <- d
      if (direction == "lower") dd[c("score1", "score2")] <- -dd[c("score1", "score2")]
      fixtures[[paste(kind, direction, sep = "_")]] <- list(data = dd, direction = direction,
        positive = "Case", description = paste(kind, "endpoint fixture", direction))
    }
  }
  fixtures
}

  list(fixtures = make_roc_oracle_fixtures, auc = oracle_auc, paired = oracle_paired,
       thresholds = oracle_thresholds, candidates = oracle_candidates, pr = oracle_pr)
})
