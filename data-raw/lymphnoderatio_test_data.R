# Nodal cohort: 300 resections; positive-node count scales with latent risk, survival
# worsens with LNR. For the lymphnoderatio analysis.
set.seed(7); n <- 300
examined <- pmax(1, rpois(n, 12)); truerisk <- runif(n)
positive <- pmin(examined, rbinom(n, examined, truerisk^2))
lnr <- positive / examined
time <- round(rexp(n, 0.05 * (0.4 + lnr)), 1); status <- rbinom(n, 1, 0.7)
lymphnoderatio_test_data <- data.frame(
    patient_id = sprintf("P%03d", 1:n),
    n_positive = positive, n_examined = examined,
    followup_months = time,
    dead = factor(ifelse(status == 1, "Dead", "Alive"), levels = c("Alive", "Dead")))
usethis::use_data(lymphnoderatio_test_data, overwrite = TRUE)
write.csv(lymphnoderatio_test_data, "data/lymphnoderatio_test_data.csv", row.names = FALSE)

