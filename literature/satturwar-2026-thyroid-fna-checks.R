# Arithmetic audit of published aggregate counts; NOT reconstructed patient data.
# Source: Satturwar et al., doi:10.1002/cncy.70149, Tables 2-6.
# Run from the repository root:
# Rscript --vanilla literature/satturwar-2026-thyroid-fna-checks.R
# No case IDs, reader IDs, paired predictions, or times are available here.
# Consequently, these data cannot estimate clustered uncertainty or paired tests.

categories <- c("I", "II", "III", "IV", "V", "VI")
make_matrix <- function(x) {
  matrix(x, nrow = 6L, byrow = TRUE,
         dimnames = list(reviewer = categories, consensus = categories))
}
counts <- list(
  microscopy = make_matrix(c(
    14, 25, 2, 6, 0, 1,
    0, 67, 28, 15, 0, 20,
    0, 9, 13, 16, 0, 18,
    0, 4, 0, 8, 0, 6,
    1, 0, 0, 0, 0, 14,
    0, 0, 2, 0, 0, 31
  )),
  single_layer = make_matrix(c(
    11, 15, 0, 4, 0, 0,
    4, 61, 18, 8, 0, 9,
    0, 22, 23, 18, 0, 16,
    0, 6, 2, 13, 0, 4,
    0, 1, 2, 2, 0, 14,
    0, 0, 0, 0, 0, 47
  )),
  seven_layer = make_matrix(c(
    10, 13, 1, 4, 0, 0,
    4, 60, 14, 12, 0, 6,
    1, 23, 23, 12, 0, 19,
    0, 5, 3, 16, 0, 4,
    0, 4, 4, 0, 0, 17,
    0, 0, 0, 1, 0, 44
  ))
)
expected_col_totals <- c(15, 105, 45, 45, 0, 90)
stopifnot(all(vapply(counts, function(x) {
  identical(as.numeric(colSums(x)), expected_col_totals) && sum(x) == 300
}, logical(1))))

# Table 6 confidence limits, transcribed in metric order below.
published_ci <- list(
  microscopy = c(52.4, 67.2, 53.9, 73.0, 82.3, 94.2, 42.6, 60.4, 55.5, 67.1),
  single_layer = c(71.6, 84.1, 48.1, 67.7, 76.4, 88.3, 53.1, 73.1, 65.2, 76.1),
  seven_layer = c(72.8, 85.1, 47.1, 66.8, 75.2, 87.1, 54.6, 74.9, 65.6, 76.4)
)
reported_counts <- list(
  microscopy = c(108, 180, 67, 105, 108, 121, 67, 130, 175, 285),
  single_layer = c(141, 180, 61, 105, 141, 170, 61, 96, 202, 285),
  seven_layer = c(143, 180, 60, 105, 143, 175, 60, 92, 203, 285)
)

audit_one <- function(modality) {
  x <- counts[[modality]]
  positive <- c("III", "IV", "V", "VI")
  a <- sum(x[positive, positive])
  b <- sum(x[positive, "II"])
  c <- sum(x["II", positive])
  d <- x["II", "II"]
  nd_pos <- sum(x["I", positive])
  nd_neg <- x["I", "II"]
  fractions <- matrix(c(
    a, a + c + nd_pos,
    d, b + d + nd_neg,
    a, a + b,
    d, c + d,
    a + d, a + b + c + d + nd_pos + nd_neg
  ), ncol = 2, byrow = TRUE)
  stopifnot(identical(as.numeric(t(fractions)), reported_counts[[modality]]))
  ci <- t(apply(fractions, 1L, function(z) {
    100 * stats::binom.test(z[1], z[2])$conf.int
  }))
  reported <- matrix(published_ci[[modality]], ncol = 2, byrow = TRUE)
  metrics <- data.frame(
    modality = modality,
    metric = c("PPA_all_reference_positive", "NPA_all_reference_negative",
               "reference_positive_given_positive_call",
               "reference_negative_given_negative_call",
               "overall_binary_agreement_all_eligible"),
    numerator = fractions[, 1], denominator = fractions[, 2],
    percent = 100 * fractions[, 1] / fractions[, 2],
    independent_binomial_lower = ci[, 1], independent_binomial_upper = ci[, 2],
    published_lower = reported[, 1], published_upper = reported[, 2],
    ci_matches_to_1_decimal = round(ci[, 1], 1) == reported[, 1] &
      round(ci[, 2], 1) == reported[, 2]
  )
  denominators <- data.frame(
    modality = modality, eligible_reads = sum(x[, -1]),
    positive_positive = a, positive_negative = b,
    negative_positive = c, negative_negative = d,
    nondiagnostic_positive = nd_pos, nondiagnostic_negative = nd_neg,
    determinate_reads = a + b + c + d,
    nondiagnostic_all_reads = sum(x["I", ]),
    determinate_coverage_percent = 100 * (a + b + c + d) / sum(x[, -1]),
    ppa_determinate_only = 100 * a / (a + c),
    npa_determinate_only = 100 * d / (b + d),
    exact_multicategory_agreement = 100 * sum(diag(x)) / sum(x)
  )
  list(metrics = metrics, denominators = denominators)
}
checks <- lapply(names(counts), audit_one)
metrics <- do.call(rbind, lapply(checks, `[[`, "metrics"))
denominators <- do.call(rbind, lapply(checks, `[[`, "denominators"))
out_dir <- "literature"
utils::write.csv(metrics, file.path(out_dir, "satturwar-2026-thyroid-fna-metrics.csv"),
                 row.names = FALSE)
utils::write.csv(denominators,
                 file.path(out_dir, "satturwar-2026-thyroid-fna-denominators.csv"),
                 row.names = FALSE)
print(denominators, row.names = FALSE)
cat("\nAll 15 published metric fractions reconstructed from Tables 2-4.\n")
cat(sum(metrics$ci_matches_to_1_decimal),
    "of 15 CIs match independent-read Clopper-Pearson limits after rounding.\n")
cat("These are reproduction checks, not endorsements of independent-read inference.\n")
cat("Table 1/3 single-layer malignant agreement: 47/90 =",
    round(100 * 47 / 90, 1), "% (published: 52.5%).\n")

# Supplement Table S1: independent histology comparator, same repeated reads.
# Unlike the primary III+ triage endpoint, S1 malignant concordance uses V/VI.
# These derived rates are descriptive only; no clustered CIs can be recovered.
histology <- list(
  microscopy = cbind(benign = c(47, 98, 30, 9, 1, 1),
                     malignant = c(1, 32, 26, 9, 14, 32)),
  single_layer = cbind(benign = c(30, 87, 51, 15, 3, 0),
                      malignant = c(0, 13, 28, 10, 16, 47)),
  seven_layer = cbind(benign = c(28, 82, 53, 16, 7, 0),
                     malignant = c(0, 14, 25, 12, 18, 45))
)
histology_checks <- do.call(rbind, lapply(names(histology), function(modality) {
  x <- histology[[modality]]
  stopifnot(identical(as.numeric(colSums(x)), c(186, 114)),
            identical(as.numeric(rowSums(x)), as.numeric(rowSums(counts[[modality]]))))
  data.frame(
    modality = modality,
    benign_reads = 186, malignant_reads = 114,
    benign_concordance_II = 100 * x[2, 1] / 186,
    malignant_concordance_V_VI = 100 * sum(x[5:6, 2]) / 114,
    derived_triage_sensitivity_III_VI = 100 * sum(x[3:6, 2]) / 114,
    nondiagnostic_benign = x[1, 1], nondiagnostic_malignant = x[1, 2]
  )
}))
utils::write.csv(histology_checks,
                 file.path(out_dir, "satturwar-2026-thyroid-fna-histology.csv"),
                 row.names = FALSE)
print(histology_checks, row.names = FALSE)

# Independently anchored smoke check of current pure ClinicoPath helpers using
# FDA guidance Table 4, not the study's unobserved patient-level records.
helpers <- new.env()
sys.source("R/pathagreement-reader-metrics.R", envir = helpers)
test_call <- rep(c(TRUE, TRUE, FALSE, FALSE), c(40, 5, 4, 171))
reference_call <- rep(c(TRUE, FALSE, TRUE, FALSE), c(40, 5, 4, 171))
accuracy <- helpers$.paReaderAccuracy(test_call, reference_call)
agreement <- helpers$.paReaderAgreement(test_call, reference_call)
stopifnot(
  isTRUE(all.equal(unname(accuracy["sensitivity"]), 40 / 44)),
  isTRUE(all.equal(unname(accuracy["specificity"]), 171 / 176)),
  isTRUE(all.equal(unname(agreement["positive_agreement"]), 80 / 89)),
  isTRUE(all.equal(unname(agreement["negative_agreement"]), 342 / 351))
)
cat("FDA example: current sensitivity/specificity helpers give directional",
    "PPA/NPA; positive/negative agreement helpers give symmetric PSA/NSA.\n")
