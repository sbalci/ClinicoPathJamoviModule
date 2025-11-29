
# Verification script for studydiagram
library(jmvcore)
library(R6)
library(ggplot2)

# Source the function files
source("R/studydiagram.h.R")
source("R/studydiagram.b.R")

# Mock data for participant_step format
set.seed(123)
n <- 100
data_step <- data.frame(
    id = 1:n,
    step_excl = sample(c(NA, 1, 2, 3), n, replace = TRUE, prob = c(0.7, 0.1, 0.1, 0.1)),
    reason = sample(c("Reason A", "Reason B"), n, replace = TRUE)
)

# Test participant_step format
print("Testing participant_step format...")
options <- studydiagramOptions$new(
    data_format = "participant_step",
    participant_id = "id",
    step_excluded = "step_excl",
    exclusion_reason_participant = "reason",
    diagram_type = "consort_standard",
    show_percentages = TRUE,
    show_exclusion_boxes = TRUE,
    direction = "TB",
    color_scheme = "gray",
    show_interpretation = TRUE
)

# Create analysis object
analysis <- studydiagramClass$new(
    options = options,
    data = data_step
)

# Run analysis
analysis$run()

# Check results
print("Checking summary table...")
summary_table <- analysis$results$summary
print(summary_table$asDF)

# Mock data for step_summary format
data_summary <- data.frame(
    step = c("Screening", "Enrollment", "Analysis"),
    count = c(100, 80, 75),
    reasons = c("Ineligible (20)", "Withdrew (5)", "")
)

# Test step_summary format
print("Testing step_summary format...")
options_summary <- studydiagramOptions$new(
    data_format = "step_summary",
    step_name = "step",
    participant_count = "count",
    exclusion_reason_summary = "reasons",
    diagram_type = "flowchart_standard"
)

analysis_summary <- studydiagramClass$new(
    options = options_summary,
    data = data_summary
)

analysis_summary$run()

print("Checking summary table for step_summary...")
print(analysis_summary$results$summary$asDF)

# Mock data for exclusion_mapping format
data_mapping <- data.frame(
    id = 1:50,
    reason = c(rep("Ineligible", 10), rep("Withdrew", 5), rep(NA, 35))
)

# Test exclusion_mapping format
print("Testing exclusion_mapping format...")
options_mapping <- studydiagramOptions$new(
    data_format = "exclusion_mapping",
    participant_id_mapping = "id",
    exclusion_reason_mapping = "reason",
    step1_exclusions = "Ineligible",
    step2_exclusions = "Withdrew",
    step1_label = "Screening",
    step2_label = "Enrollment"
)

analysis_mapping <- studydiagramClass$new(
    options = options_mapping,
    data = data_mapping
)

analysis_mapping$run()

print("Checking summary table for exclusion_mapping...")
print(analysis_mapping$results$summary$asDF)

print("Verification complete!")
