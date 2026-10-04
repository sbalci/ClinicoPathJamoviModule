# Interpretable changes for independent cases with repeated measurements.
# Callers supply the same complete-case rows used by the existing paired plots.
.jjWithinPairedEffects <- function(data, estimator = "mean", ci_method = "bootstrap",
                                  confidence = 0.95, samples = 2000L, seed = 20250101L,
                                  ratios = FALSE, checkpoint = function() NULL) {
  stopifnot(is.data.frame(data), ncol(data) >= 2L, ncol(data) <= 4L,
            nrow(data) >= 3L, all(vapply(data, is.numeric, logical(1))),
            all(is.finite(as.matrix(data))),
            estimator %in% c("mean", "median", "hodges_lehmann"),
            ci_method %in% c("bootstrap", "standard"),
            is.finite(confidence), confidence > 0, confidence < 1,
            is.finite(samples), samples >= 200L, samples <= 10000L)

  # DescTools uses randomized selection internally for its Walsh median.
  # Restore the caller's stream for both analytic and bootstrap paths.
  withr::local_seed(seed)
  data <- data[do.call(order, unname(data)), , drop = FALSE]

  indices <- utils::combn(seq_len(ncol(data)), 2L)
  n <- nrow(data)
  estimate <- switch(estimator, mean = base::mean, median = stats::median,
                     hodges_lehmann = function(x) DescTools::HodgesLehmann(x))
  label <- switch(estimator, mean = "Mean paired change", median = "Median paired change",
                  hodges_lehmann = "Hodges-Lehmann shift")
  alpha <- (1 - confidence) / 2
  changes <- lapply(seq_len(ncol(indices)), function(i) {
    data[[indices[2L, i]]] - data[[indices[1L, i]]]
  })
  if (any(!is.finite(unlist(changes, use.names = FALSE))))
    stop("Paired differences overflowed; rescale the measurements before analysis.")
  boot_changes <- ci_method == "bootstrap" || estimator == "median"
  log_ratios <- if (ratios) lapply(seq_len(ncol(indices)), function(i) {
    a <- data[[indices[1L, i]]]
    b <- data[[indices[2L, i]]]
    if (any(a <= 0 | b <= 0)) return(NULL)
    log(b) - log(a)
  }) else vector("list", ncol(indices))

  # The same draw is used for every contrast; no independent resampling of modalities.
  boot <- if (boot_changes || (ratios && ci_method == "bootstrap")) {
    withr::with_seed(seed, {
      out <- matrix(NA_real_, nrow = samples, ncol = 2L * length(changes))
      for (b in seq_len(samples)) {
        if (b %% 50L == 0L) checkpoint()
        draw <- sample.int(n, n, replace = TRUE)
        for (i in seq_along(changes)) {
          if (boot_changes) out[b, i] <- estimate(changes[[i]][draw])
          if (ratios && ci_method == "bootstrap" && !is.null(log_ratios[[i]]))
            out[b, length(changes) + i] <- mean(log_ratios[[i]][draw])
        }
      }
      out
    })
  } else NULL

  percentile <- function(x) {
    good <- is.finite(x)
    if (sum(good) < ceiling(0.9 * samples))
      return(list(ci = c(NA_real_, NA_real_), note =
        "Fewer than 90% of bootstrap estimates were finite; interval unavailable."))
    if (length(unique(x[good])) < 2L)
      return(list(ci = c(NA_real_, NA_real_), note =
        "All bootstrap estimates were identical; interval unavailable."))
    list(ci = unname(stats::quantile(x[good], c(alpha, 1 - alpha))), note = "")
  }
  t_interval <- function(x) {
    if (stats::sd(x) == 0)
      return(list(ci = c(NA_real_, NA_real_), note =
        "All paired changes were identical; t interval unavailable."))
    list(ci = unname(stats::t.test(x, conf.level = confidence)$conf.int), note = "")
  }

  rows <- list()
  for (i in seq_along(changes)) {
    d <- changes[[i]]
    method <- "Paired bootstrap percentile"
    if (boot_changes) {
      interval <- percentile(boot[, i])
    } else if (estimator == "mean") {
      method <- "Paired t interval"
      interval <- t_interval(d)
    } else {
      method <- "Signed-rank interval (normal approximation)"
      warning_text <- character()
      wt <- tryCatch(withCallingHandlers(
        stats::wilcox.test(d, conf.int = TRUE, conf.level = confidence,
                          exact = FALSE, correct = TRUE),
        warning = function(w) {
          warning_text <<- c(warning_text, conditionMessage(w))
          invokeRestart("muffleWarning")
        }), error = function(e) e)
      valid <- !inherits(wt, "error") && length(wt$conf.int) == 2L &&
        all(is.finite(wt$conf.int)) && length(warning_text) == 0L
      interval <- list(ci = if (valid) unname(wt$conf.int) else c(NA_real_, NA_real_),
                       note = if (valid) "" else
                         "The signed-rank interval could not be estimated reliably; use bootstrap.")
    }
    base <- list(first = names(data)[indices[1L, i]],
                 second = names(data)[indices[2L, i]], n = n)
    rows[[length(rows) + 1L]] <- c(base, list(
      effect = label, ratio = FALSE, estimate = unname(estimate(d)),
      lower = interval$ci[1L], upper = interval$ci[2L], method = method,
      note = interval$note))
    if (!ratios) next
    lr <- log_ratios[[i]]
    ratio_method <- if (ci_method == "bootstrap") "Paired bootstrap percentile (log scale)"
      else "Paired t interval (log scale)"
    if (is.null(lr)) {
      ri <- list(ci = c(NA_real_, NA_real_), note =
        "Time ratios require strictly positive values in both measurements; no pairs were dropped.")
      ratio_estimate <- NA_real_
    } else {
      ri <- if (ci_method == "bootstrap") percentile(boot[, length(changes) + i])
        else t_interval(lr)
      ratio_estimate <- exp(mean(lr))
      ri$ci <- exp(ri$ci)
      if (!is.finite(ratio_estimate) || ratio_estimate <= 0 ||
          any(!is.na(ri$ci) & (!is.finite(ri$ci) | ri$ci <= 0))) {
        ratio_estimate <- NA_real_
        ri$ci[] <- NA_real_
        ri$note <- "The time ratio exceeded numerical range; estimate and interval unavailable."
      }
    }
    rows[[length(rows) + 1L]] <- c(base, list(
      effect = "Geometric mean paired time ratio", ratio = TRUE, estimate = ratio_estimate,
      lower = ri$ci[1L], upper = ri$ci[2L], method = ratio_method, note = ri$note))
  }
  do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE))
}

# Reuse the plot's test family. With 3+ nonparametric measurements its post-hoc
# test is Durbin-Conover, not a series of signed-rank tests.
.jjWithinPairedPValues <- function(data, type, pairwise = FALSE, adjustment = "holm") {
  if (!type %in% c("parametric", "nonparametric")) return(NULL)
  if (ncol(data) > 2L && !pairwise) return(NULL)
  long <- data.frame(
    subject = rep(seq_len(nrow(data)), ncol(data)),
    measurement = factor(rep(names(data), each = nrow(data)), levels = names(data)),
    value = unlist(data, use.names = FALSE))
  warnings <- character()
  out <- tryCatch(withCallingHandlers({
    if (ncol(data) == 2L) {
      result <- statsExpressions::two_sample_test(
        long, x = "measurement", y = "value", subject.id = "subject", paired = TRUE,
        type = type, exact = FALSE)
      data.frame(first = names(data)[1L], second = names(data)[2L],
                 p = result$p.value, test = result$method)
    } else {
      result <- statsExpressions::pairwise_comparisons(
        long, x = "measurement", y = "value", subject.id = "subject", paired = TRUE,
        type = type, p.adjust.method = adjustment, exact = FALSE)
      data.frame(first = result$group1, second = result$group2,
                 p = result$p.value, test = result$test)
    }
  }, warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  }), error = function(e) NULL)
  if (!is.null(out)) attr(out, "warnings") <- unique(warnings)
  out
}
