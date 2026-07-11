# Test data for interruptedtimeseries: monthly outcome with an intervention at month 25.
# Simulates a lab turnaround-time QI intervention: modest baseline trend, a clear level
# drop at the intervention, and a steeper decline afterwards, with mild AR(1) noise.
set.seed(2026)
n <- 48; itime <- 25
time <- 1:n
post <- as.integer(time >= itime)
time_after <- pmax(0, time - itime + 1) * post
e <- numeric(n); e[1] <- rnorm(1, 0, 1.5)
for (t in 2:n) e[t] <- 0.3 * e[t-1] + rnorm(1, 0, 1.5)
outcome <- 72 - 0.15 * time + (-8) * post + (-0.5) * time_after + e
interruptedtimeseries_test_data <- data.frame(
    month = time,
    turnaround_hours = round(outcome, 1))
usethis::use_data(interruptedtimeseries_test_data, overwrite = TRUE)
write.csv(interruptedtimeseries_test_data,
          "data/interruptedtimeseries_test_data.csv", row.names = FALSE)
