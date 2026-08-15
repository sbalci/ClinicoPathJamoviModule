library(jmvcore)
library(R6)
library(ggplot2)

# Source the files
source("R/studydiagram.h.R")
source("R/studydiagram.b.R")

# Mock data for participant_step
set.seed(123)
n <- 100
step_levels <- c("Screening", "Randomization", "Follow-up")
data_part <- data.frame(
  id = 1:n,
  step_excl = factor(sample(c(NA, step_levels), n, replace = TRUE, prob = c(0.4, 0.2, 0.2, 0.2)), levels = step_levels, ordered = TRUE),
  reason = sample(c("Ineligible", "Withdrew", "Lost to follow-up", "Adverse event", "Protocol violation that is extremely long and might get truncated if we are not careful about wrapping the text properly in the diagram output"), n, replace = TRUE)
)

# Ensure some NAs have no reason (completed)
data_part$reason[is.na(data_part$step_excl)] <- NA

# Test 1: Participant Step
print("Testing Participant Step...")
options_part <- studydiagramOptions$new(
  data_format = "participant_step",
  participant_id = "id",
  step_excluded = "step_excl",
  exclusion_reason_participant = "reason",
  show_percentages = TRUE,
  show_exclusion_boxes = TRUE,
  show_interpretation = TRUE
)

studydiagram_part <- studydiagramClass$new(options = options_part, data = data_part)
studydiagram_part$run()

print("Participant Step Summary:")
print(studydiagram_part$results$summary$asDF)

# Test plot generation (internal function)
# We can't easily view the plot in CLI, but we can check if it runs without error
# and inspect the state
print("Plot State:")
print(studydiagram_part$results$plot$state)


# Mock data for step_summary
data_summary <- data.frame(
  step = c("Enrolled", "Allocated", "Followed-up", "Analyzed"),
  count = c(100, 80, 75, 70),
  reasons = c(NA, "Not eligible (n=20)", "Withdrew (n=5)", "Missing data (n=5)")
)

# Test 2: Step Summary
print("Testing Step Summary...")
options_sum <- studydiagramOptions$new(
  data_format = "step_summary",
  step_name = "step",
  participant_count = "count",
  exclusion_reason_summary = "reasons",
  show_exclusion_boxes = TRUE
)

studydiagram_sum <- studydiagramClass$new(options = options_sum, data = data_summary)
studydiagram_sum$run()

print("Step Summary Table:")
print(studydiagram_sum$results$summary$asDF)


# Mock data for exclusion_mapping
data_map <- data.frame(
  id = 1:50,
  excl_reason = sample(c("Inclusion Criteria", "Exclusion Criteria", "Declined", "Other"), 50, replace = TRUE)
)
# Some participants have no exclusion (NA or empty) - wait, this format implies we map reasons to steps.
# If a participant is NOT in the exclusion column, are they survivors?
# The code says: "Count participants excluded at this step... remaining <- remaining - excluded_at_step"
# So yes, anyone NOT matching the reasons is retained.

# Create dummy levels for mapping
data_map$excl_reason <- factor(data_map$excl_reason)

print("Testing Exclusion Mapping...")
options_map <- studydiagramOptions$new(
  data_format = "exclusion_mapping",
  participant_id_mapping = "id",
  exclusion_reason_mapping = "excl_reason",
  step1_exclusions = "Inclusion Criteria", # Mapping specific levels
  step2_exclusions = "Exclusion Criteria",
  step3_exclusions = "Declined",
  step1_label = "Screening",
  step2_label = "Eligibility",
  step3_label = "Consent"
)

studydiagram_map <- studydiagramClass$new(options = options_map, data = data_map)
studydiagram_map$run()

print("Exclusion Mapping Summary:")
print(studydiagram_map$results$summary$asDF)