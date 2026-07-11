# Example estimates for the E-value analysis (the E-value is computed from entered
# estimate + CI, so no data frame is strictly required; this is illustrative).
evalue_test_data <- data.frame(
    study       = c("Cohort A", "Cohort B", "Case-control C"),
    effect_type = c("RR", "HR", "OR"),
    estimate    = c(3.9, 1.6, 2.5),
    ci_lower    = c(1.8, 1.2, 1.3),
    ci_upper    = c(8.7, 2.1, 4.8))
usethis::use_data(evalue_test_data, overwrite = TRUE)
write.csv(evalue_test_data, "data/evalue_test_data.csv", row.names = FALSE)
