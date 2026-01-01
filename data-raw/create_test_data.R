# Create wide format test data for time-dependent covariates
# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

set.seed(123)
n <- 200

# Wide format survival data with time-dependent treatment changes
test_wide_time_dependent <- data.frame(
  id = 1:n,
  age = round(rnorm(n, 65, 12)),
  sex = factor(sample(c("Male", "Female"), n, replace = TRUE)),
  stage = factor(sample(c("I", "II", "III", "IV"), n, replace = TRUE, prob = c(0.3, 0.3, 0.25, 0.15))),

  # Baseline treatment (will be used for intervals 0-6 months)
  treatment_baseline = factor(sample(c("Standard", "Experimental"), n, replace = TRUE)),

  # Treatment at 6 months
  treatment_t6 = factor(sample(c("Standard", "Experimental", "Switched"), n, replace = TRUE)),

  # Treatment at 12 months
  treatment_t12 = factor(sample(c("Standard", "Experimental", "Switched", "Stopped"), n, replace = TRUE)),

  # Treatment at 18 months
  treatment_t18 = factor(sample(c("Standard", "Experimental", "Switched", "Stopped"), n, replace = TRUE)),

  # Performance status changes
  ps_baseline = sample(0:2, n, replace = TRUE, prob = c(0.6, 0.3, 0.1)),
  ps_t6 = sample(0:3, n, replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1)),
  ps_t12 = sample(0:4, n, replace = TRUE, prob = c(0.3, 0.3, 0.2, 0.15, 0.05)),
  ps_t18 = sample(0:4, n, replace = TRUE, prob = c(0.25, 0.25, 0.25, 0.15, 0.1)),

  # Survival time and outcome
  time = round(pmax(rexp(n, rate = 0.01), 1)), # Minimum 1 month follow-up
  status = rbinom(n, 1, 0.4) # Event rate ~40%
)

# Ensure some follow-up times are longer than change points
test_wide_time_dependent$time[test_wide_time_dependent$time > 24] <-
  test_wide_time_dependent$time[test_wide_time_dependent$time > 24] + sample(12:36, sum(test_wide_time_dependent$time > 24), replace = TRUE)

# Save the dataset
save(test_wide_time_dependent, file = "data/test_wide_time_dependent.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(test_wide_time_dependent, "data/test_wide_time_dependent.omv")
  message("✓ Created test_wide_time_dependent.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(test_wide_time_dependent, "data/test_wide_time_dependent.omv")
  message("✓ Created test_wide_time_dependent.omv")
}

# Create long format test data for time-dependent covariates
set.seed(456)
n <- 100 # Fewer subjects but multiple rows per subject

# Create base subject data
subjects <- data.frame(
  id = 1:n,
  age = round(rnorm(n, 65, 12)),
  sex = factor(sample(c("Male", "Female"), n, replace = TRUE)),
  stage = factor(sample(c("I", "II", "III", "IV"), n, replace = TRUE, prob = c(0.3, 0.3, 0.25, 0.15))),
  final_time = round(pmax(rexp(n, rate = 0.01), 6)), # Minimum 6 months follow-up
  final_status = rbinom(n, 1, 0.4)
)

# Create long format data with multiple intervals per subject
test_long_time_dependent <- data.frame()

for (i in 1:n) {
  subject <- subjects[i, ]

  # Determine number of intervals based on follow-up time
  max_time <- subject$final_time
  intervals <- c(0, 6, 12, 18, max_time)
  intervals <- intervals[intervals <= max_time]
  intervals <- unique(sort(intervals))

  if (length(intervals) < 2) {
    intervals <- c(0, max_time)
  }

  # Create rows for each interval
  for (j in 1:(length(intervals)-1)) {
    tstart <- intervals[j]
    tstop <- intervals[j+1]

    # Status is 1 only in the last interval if subject has event
    status <- ifelse(j == (length(intervals)-1), subject$final_status, 0)

    # Time-dependent covariates that change at each interval
    treatment <- sample(c("Standard", "Experimental", "Switched"), 1)
    ps <- sample(0:3, 1, prob = c(0.4, 0.3, 0.2, 0.1))

    # Add some variability based on time
    if (tstart >= 12) {
      treatment <- sample(c("Standard", "Experimental", "Switched", "Stopped"), 1)
      ps <- sample(0:4, 1, prob = c(0.3, 0.3, 0.2, 0.15, 0.05))
    }

    row <- data.frame(
      id = subject$id,
      age = subject$age,
      sex = subject$sex,
      stage = subject$stage,
      tstart = tstart,
      tstop = tstop,
      status = status,
      treatment = factor(treatment),
      performance_status = ps
    )

    test_long_time_dependent <- rbind(test_long_time_dependent, row)
  }
}

# Save the long format dataset
save(test_long_time_dependent, file = "data/test_long_time_dependent.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(test_long_time_dependent, "data/test_long_time_dependent.omv")
  message("✓ Created test_long_time_dependent.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(test_long_time_dependent, "data/test_long_time_dependent.omv")
  message("✓ Created test_long_time_dependent.omv")
}

cat("Test datasets created successfully:\n")
cat("- test_wide_time_dependent.rda:", nrow(test_wide_time_dependent), "subjects\n")
cat("- test_long_time_dependent.rda:", length(unique(test_long_time_dependent$id)), "subjects,", nrow(test_long_time_dependent), "intervals\n")
