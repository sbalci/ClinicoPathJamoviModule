# Reproducible design-specific coverage check; not a universal validation claim.
# Run from the repository root. Readers are fixed, with perfect within-case
# correlation; paired patient members share latent reading difficulty.
# True modality changes are +.15 PPA, +.12 NPA and +.135 overall agreement.
source('R/utils-reader-resampling.R')
for (file in Sys.glob('R/pathagreement-reader-*.R')) source(file)
nsim <- 200L
bootstrap_draws <- 399L
truth <- c(.15, .12, .135)
limits <- array(NA_real_, c(nsim, 3, 2))
set.seed(70149)
for (simulation in seq_len(nsim)) {
  patients <- 60L
  d <- expand.grid(patient = seq_len(patients), disease = c(FALSE, TRUE),
                   reader = c('R1', 'R2', 'R3'), modality = c('A', 'B'))
  # Shared latent variable makes paired changes and reader dependence explicit.
  latent <- stats::runif(patients)
  threshold <- ifelse(d$modality == 'A', ifelse(d$disease, .65, .70),
                      ifelse(d$disease, .80, .82))
  correct <- latent[d$patient] < threshold
  d$case <- paste(d$patient, d$disease)
  d$reference <- ifelse(d$disease, 'P', 'N')
  d$rating <- ifelse(correct, d$reference, ifelse(d$disease, 'N', 'P'))
  p <- .paReaderPrepare(d, list(readerWorkflow = 'binary', readerFormat = 'long',
    readerID = 'reader', modality = 'modality', rating = 'rating', caseID = 'case',
    readerCluster = 'patient', baselineModality = 'A', comparisonModality = 'B',
    readerPositive = 'P', readerNegative = 'N', referenceStandard = 'reference',
    referencePositive = 'P', referenceNegative = 'N', readerModalityComparison = TRUE,
    readerBootstrap = TRUE, bootstrapSamples = bootstrap_draws, seed = simulation))
  r <- .paReaderModalityReport(p)$readerModalityContrast
  limits[simulation, , 1] <- r$lower / 100
  limits[simulation, , 2] <- r$upper / 100
}
summary <- do.call(rbind, lapply(seq_len(3), function(j) {
  available <- is.finite(limits[, j, 1]) & is.finite(limits[, j, 2])
  covered <- limits[available, j, 1] <= truth[j] & limits[available, j, 2] >= truth[j]
  coverage <- mean(covered)
  data.frame(metric = c('PPA difference', 'NPA difference', 'Overall difference')[j],
    simulations = nsim, bootstrap_draws = bootstrap_draws, available = sum(available),
    coverage = coverage, monte_carlo_se = sqrt(coverage * (1 - coverage) / sum(available)),
    mean_width = mean(limits[available, j, 2] - limits[available, j, 1]))
}))
print(summary)
write.csv(summary, 'literature/satturwar-2026-reader-coverage-check.csv', row.names = FALSE)
