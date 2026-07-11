# Test data for joinpoint: 21-year incidence-rate series with a trend change around 2010
# (declining then rising), on the log scale with small multiplicative noise.
set.seed(2026); yr <- 2000:2020
rate <- exp(ifelse(yr <= 2010,
                   log(45) - 0.03 * (yr - 2000),
                   log(45) - 0.3 + 0.04 * (yr - 2010)) + rnorm(21, 0, 0.03))
joinpoint_test_data <- data.frame(year = yr, incidence_rate = round(rate, 2))
usethis::use_data(joinpoint_test_data, overwrite = TRUE)
write.csv(joinpoint_test_data, "data/joinpoint_test_data.csv", row.names = FALSE)
