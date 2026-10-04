# Independent arithmetic from rounded published summaries, not recovered patient data.
# Source: https://doi.org/10.1002/2056-4538.70113 (Stage III, open-category cohort)
n_slides <- 7056L
positive_slides <- 45L
negative_slides <- n_slides - positive_slides
specificity <- c(0.905, 0.956)
inferred_fp <- negative_slides * (1 - specificity)
inferred_flags <- positive_slides + inferred_fp
result <- data.frame(
  strategy = c("default_argmax", "gas_probability_at_least_0.95"),
  total_slides = n_slides,
  positive_slides = positive_slides,
  observed_prevalence = positive_slides / n_slides,
  reported_sensitivity = 1,
  rounded_specificity = specificity,
  inferred_false_positive_flags = inferred_fp,
  inferred_total_flags = inferred_flags,
  inferred_ppv = positive_slides / inferred_flags,
  inferred_flags_per_1000 = inferred_flags / n_slides * 1000,
  inferred_flags_per_detected_positive = inferred_flags / positive_slides
)
# The intervals below describe rounding compatibility, NOT sampling uncertainty.
compatible_fp <- lapply(specificity, function(sp) {
  candidates <- 0:negative_slides
  range(candidates[round(1 - candidates / negative_slides, 3) == sp])
})
result$rounding_compatible_fp_min <- vapply(compatible_fp, min, numeric(1))
result$rounding_compatible_fp_max <- vapply(compatible_fp, max, numeric(1))
print(result, row.names = FALSE)
cat("\nSensitivity 95% Clopper-Pearson CI, assuming 45 independent positives:\n")
print(stats::binom.test(positive_slides, positive_slides)$conf.int)

stopifnot(
  negative_slides == 7011,
  abs(result$inferred_false_positive_flags[1] - 666.045) < 1e-8,
  abs(result$inferred_ppv[1] - 0.06328713) < 1e-8,
  abs(result$inferred_false_positive_flags[2] - 308.484) < 1e-8,
  all(result$inferred_ppv >= 0 & result$inferred_ppv <= 1)
)
# Execute from the repository root. Outputs are intentionally labelled inferred.
write.csv(result, "literature/yang-2026-gaspath-arithmetic.csv", row.names = FALSE)
