# Paired ctDNA/MRD cohort: baseline + follow-up VAF per patient, with a survival outcome
# that depends on MRD status (persistent disease -> worse survival). ~55% clear.
set.seed(2026)
n <- 120
baseline_vaf <- round(runif(n, 0.5, 25), 2)
cleared <- rbinom(n, 1, 0.55)
followup_vaf <- ifelse(cleared == 1,
    round(pmax(0, rnorm(n, 0, 0.02)), 3),
    round(baseline_vaf * runif(n, 0.3, 1.3), 3))
days_between <- round(runif(n, 21, 42))
mrd_pos <- as.integer(followup_vaf > 0.05)
os_time  <- round(rexp(n, rate = 0.02 * exp(1.1 * mrd_pos)), 1)
os_event <- rbinom(n, 1, 0.7)
arm <- factor(sample(c("Chemo", "Chemo+IO"), n, TRUE))

ctdnadynamics_test_data <- data.frame(
    patient_id = sprintf("P%03d", seq_len(n)),
    baseline_vaf = baseline_vaf,
    followup_vaf = followup_vaf,
    days_between = days_between,
    treatment_arm = arm,
    os_months = os_time,
    os_event = os_event)

usethis::use_data(ctdnadynamics_test_data, overwrite = TRUE)
write.csv(ctdnadynamics_test_data, "data/ctdnadynamics_test_data.csv", row.names = FALSE)
