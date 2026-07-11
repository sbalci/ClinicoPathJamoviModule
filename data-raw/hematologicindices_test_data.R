# CBC + biochemistry cohort (250 patients) for the hematologicindices analysis;
# survival worsens with higher NLR. Albumin in g/dL, CRP in mg/L.
set.seed(2026); n <- 250
lymph <- round(pmax(0.2, rnorm(n, 1.8, 0.6)), 2)
neut <- round(pmax(0.5, rnorm(n, 5, 2)), 2)
plt <- round(pmax(50, rnorm(n, 270, 90)))
mono <- round(pmax(0.1, rnorm(n, 0.6, 0.2)), 2)
alb <- round(pmax(2, rnorm(n, 3.9, 0.6)), 1)
crp <- round(pmax(0.5, rexp(n, 0.08)), 1)
time <- round(rexp(n, 0.05 * (0.5 + (neut/lymph)/5)), 1); status <- rbinom(n, 1, 0.7)
hematologicindices_test_data <- data.frame(
    patient_id = sprintf("P%03d", 1:n),
    neutrophils = neut, lymphocytes = lymph, platelets = plt, monocytes = mono,
    albumin_gdl = alb, crp_mgL = crp,
    followup_months = time,
    dead = factor(ifelse(status == 1, "Dead", "Alive"), levels = c("Alive", "Dead")))
usethis::use_data(hematologicindices_test_data, overwrite = TRUE)
write.csv(hematologicindices_test_data, "data/hematologicindices_test_data.csv", row.names = FALSE)

