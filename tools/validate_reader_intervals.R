# Reproducible coverage and timing audit, run from the repository root.
# These simulations measure finite-sample behavior, not universal validity.
for (f in c("pathagreement-reader-data.R", "pathagreement-reader-metrics.R",
            "utils-reader-resampling.R", "pathagreement-reader-report.R")) source(file.path("R", f))
outdir <- commandArgs(trailingOnly = TRUE)
if (!length(outdir)) outdir <- "literature/reader-reproducibility-validation"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
scenarios <- list(
  unequal_margins = list(p = c(.35, .20, .05, .40), n = 240, copies = 1),
  rare_categories = list(p = c(.008, .006, .004, .982), n = 120, copies = 1),
  clusters_60 = list(p = c(.35, .20, .05, .40), n = 60, copies = 3),
  clusters_8 = list(p = c(.35, .20, .05, .40), n = 8, copies = 3))
stat <- function(z) c(.paReaderAgreement(z$x, z$y)[c("kappa", "positive_agreement", "negative_agreement")],
                      .paReaderAccuracy(z$x, z$y)["balanced_accuracy"])
repetitions <- 300L
draws <- 399L
audit <- list()
for (s in seq_along(scenarios)) {
  cfg <- scenarios[[s]]
  p <- cfg$p
  px <- p[1] + p[2]; py <- p[1] + p[3]
  pe <- px * py + (1 - px) * (1 - py)
  truth <- c(kappa = (p[1] + p[4] - pe)/(1 - pe),
    positive_agreement = 2*p[1]/(2*p[1] + p[2] + p[3]),
    negative_agreement = 2*p[4]/(2*p[4] + p[2] + p[3]),
    balanced_accuracy = (p[1]/py + p[4]/(1-py))/2)
  available <- covered <- matrix(FALSE, repetitions, length(truth))
  for (r in seq_len(repetitions)) {
    withr::with_seed(20261003L + s*1000L + r, {
      cell <- rep(sample.int(4, cfg$n, replace = TRUE, prob = p), each = cfg$copies)
      d <- data.frame(x = cell %in% c(1, 2), y = cell %in% c(1, 3))
      clusters <- if (cfg$copies > 1) rep(seq_len(cfg$n), each = cfg$copies) else NULL
      ci <- .readerResample(d, stat, clusters, samples = draws, seed = 90000L + r)
      available[r, ] <- is.finite(ci$lower) & is.finite(ci$upper)
      covered[r, ] <- available[r, ] & ci$lower <= truth & truth <= ci$upper
    })
  }
  for (j in seq_along(truth)) {
    n_available <- sum(available[, j]); n_covered <- sum(covered[, j], na.rm = TRUE)
    mc <- if (n_available) as.numeric(binom.test(n_covered, n_available)$conf.int) else c(NA, NA)
    audit[[length(audit) + 1L]] <- data.frame(scenario = names(scenarios)[s],
      metric = names(truth)[j], truth = truth[j], repetitions = repetitions,
      draws = draws, sampling_units = cfg$n, specimens = cfg$n*cfg$copies,
      intervals_available = n_available, availability = n_available/repetitions,
      conditional_coverage = if (n_available) n_covered/n_available else NA_real_,
      mc_lower = mc[1], mc_upper = mc[2])
  }
  message("Completed ", names(scenarios)[s])
}
utils::write.csv(do.call(rbind, audit), file.path(outdir, "coverage.csv"), row.names = FALSE)
withr::with_seed(20261003, {
  d <- as.data.frame(matrix(rnorm(50000L*50L), 50000L, 50L))
  names(d) <- paste0("Reader", seq_len(50))
  options <- list(readerWorkflow = "numeric_cutoff", readerScores = names(d),
    clinicalCutoff = "0", referenceAccuracy = FALSE, readerIntervals = FALSE)
  elapsed <- system.time(result <- .paReaderReport(.paReaderPrepare(d, options)))[["elapsed"]]
  options$readerPairs <- "selected"; options$readerPair1 <- "Reader1"; options$readerPair2 <- "Reader2"
  options$readerIntervals <- TRUE; options$readerBootstrap <- TRUE; options$bootstrapSamples <- 1000L
  selected <- system.time(pair <- .paReaderReport(.paReaderPrepare(d, options)))[["elapsed"]]
  timing <- data.frame(task = c("50000 specimens x 50 readers: all-pair points", "50000 specimens: selected pair, 1000 draws"),
    elapsed_seconds = c(elapsed, selected), report_bytes = c(as.numeric(object.size(result)), as.numeric(object.size(pair))))
  utils::write.csv(timing, file.path(outdir, "timing.csv"), row.names = FALSE)
})
writeLines(c(R.version.string, paste("Platform:", R.version$platform),
             "Seed: 20261003 + scenario*1000 + repetition; bootstrap seed: 90000 + repetition",
             "Confidence: 0.95; 300 repetitions; 399 draws; percentile type 7 quantiles",
             "MC intervals: exact binomial 95%, conditional on interval availability"),
           file.path(outdir, "environment.txt"))
