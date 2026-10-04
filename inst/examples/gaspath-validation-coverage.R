# Reproducible design-specific checks; not universal validation of percentile intervals.
# Run from the repository root. All observations below are synthetic.
source("R/utils-diagnostic-validation.R")
set.seed(20261004)
simulation_count <- 100L
designs <- expand.grid(prevalence = c(.05, .2), unequal_size = c(FALSE, TRUE))
results <- list()
for (design in seq_len(nrow(designs))) {
  intervals <- list()
  for (simulation in seq_len(simulation_count)) {
    n <- 300L
    y <- stats::rbinom(n, 1, designs$prevalence[design])
    flags <- stats::rbinom(n, 1, ifelse(y == 1, .8, .1))
    scores <- ifelse(flags == 1, .9, .1)
    size <- if (designs$unequal_size[design]) sample.int(5, n, TRUE) else rep(3, n)
    idx <- rep(seq_len(n), size)
    estimate <- .dvBootstrap(y[idx], cbind(A = scores[idx]), idx, .5,
                             draws = 199, seed = simulation)
    estimate <- estimate[estimate$metric %in% c("sensitivity", "specificity", "auc"), ]
    estimate$truth <- c(auc = .85, sensitivity = .8, specificity = .9)[estimate$metric]
    intervals[[simulation]] <- estimate
  }
  all <- do.call(rbind, intervals)
  for (metric in unique(all$metric)) {
    z <- all[all$metric == metric, ]
    valid <- is.finite(z$lower) & is.finite(z$upper)
    covered <- valid & z$lower <= z$truth & z$upper >= z$truth
    coverage <- if (sum(valid)) mean(covered[valid]) else NA_real_
    results[[length(results)+1L]] <- data.frame(
      prevalence = designs$prevalence[design], unequal_size = designs$unequal_size[design],
      metric = metric, simulations = simulation_count, available = sum(valid),
      conditional_coverage = coverage, coverage_count_over_all = sum(covered)/simulation_count,
      mcse = sqrt(coverage*(1-coverage)/sum(valid)))
  }
}
write.csv(do.call(rbind, results), "literature/yang-2026-gaspath-bootstrap-coverage.csv", row.names = FALSE)

set.seed(982)
y <- stats::rbinom(50000, 1, .01)
scores <- matrix(stats::runif(50000*50), 50000, 50)
timing <- system.time(for (j in seq_len(ncol(scores))) .dvPR(scores[, j], y, 1))
benchmark <- data.frame(rows = 50000, predictors = 50,
  operation = "Tie-aware PR summaries; 50 score columns", elapsed_seconds = unname(timing['elapsed']),
  score_matrix_bytes = as.numeric(object.size(scores)))
write.csv(benchmark, "literature/yang-2026-gaspath-performance.csv", row.names = FALSE)
print(do.call(rbind, results))
print(benchmark)
