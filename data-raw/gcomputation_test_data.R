# Confounded point-treatment cohort for g-computation: stage (and age) confound the
# treatment-death association, so the crude risk difference is biased upward.
set.seed(2026); n <- 800
age <- round(rnorm(n, 60, 10))
stage <- factor(sample(c("I", "II", "III"), n, TRUE, c(.4, .35, .25)))
sev <- as.integer(stage)
treated <- rbinom(n, 1, plogis(-1 + 0.5 * sev + 0.02 * (age - 60)))
death <- rbinom(n, 1, plogis(-1.5 + 0.7 * treated + 0.6 * sev + 0.03 * (age - 60)))
gcomputation_test_data <- data.frame(
    death   = factor(ifelse(death == 1, "Dead", "Alive"), levels = c("Alive", "Dead")),
    treated = factor(ifelse(treated == 1, "Treated", "Control"), levels = c("Control", "Treated")),
    age     = age,
    stage   = stage)
usethis::use_data(gcomputation_test_data, overwrite = TRUE)
write.csv(gcomputation_test_data, "data/gcomputation_test_data.csv", row.names = FALSE)
