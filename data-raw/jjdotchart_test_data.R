# Data generation script for jjdotchart (Cleveland Dot Chart)
# Creates realistic laboratory turnaround time / biomarker quality data

set.seed(42)

# Generate lab turnaround data across multiple hospital centres
centres <- paste0("Centre_", LETTERS[1:12])
n_per_centre <- sample(15:30, length(centres), replace = TRUE)

lab_data <- data.frame(
  id = seq_len(sum(n_per_centre)),
  centre = factor(rep(centres, times = n_per_centre)),
  region = factor(rep(rep(c("North", "South", "East", "West"), each = 3), times = n_per_centre)),
  turnaround_hours = unlist(lapply(1:12, function(i) {
    base_mean <- 24 + rnorm(1, mean = 0, sd = 4)
    pmax(4, rnorm(n_per_centre[i], mean = base_mean, sd = 6))
  })),
  biomarker_score = unlist(lapply(1:12, function(i) {
    rnorm(n_per_centre[i], mean = 50 + i * 2, sd = 10)
  }))
)

# Save RDA format
jjdotchart_test_data <- lab_data
save(jjdotchart_test_data, file = "data/jjdotchart_test_data.rda", compress = "xz")

# Save CSV format in inst/extdata if needed
write.csv(lab_data, file = "inst/extdata/jjdotchart_test_data.csv", row.names = FALSE)

message("jjdotchart test data generated successfully.")
