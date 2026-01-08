# ═══════════════════════════════════════════════════════════
# Example Usage: swimmerplot
# ═══════════════════════════════════════════════════════════
#
# Comprehensive examples for clinical timeline visualization
# using the swimmerplot jamovi function

library(ClinicoPath)

# ───────────────────────────────────────────────────────────
# Example 1: Basic Swimmer Plot
# ───────────────────────────────────────────────────────────

# Load test data
data(swimmerplot_test)

# Basic timeline showing patient follow-up duration
basic_plot <- swimmerplot(
  data = swimmerplot_test,
  patientID = "PatientID",
  startTime = "StartTime",
  endTime = "EndTime"
)

# ───────────────────────────────────────────────────────────
# Example 2: Timeline with Response Assessment
# ───────────────────────────────────────────────────────────

# Add response variable to show treatment outcomes
response_plot <- swimmerplot(
  data = swimmerplot_test,
  patientID = "PatientID",
  startTime = "StartTime",
  endTime = "EndTime",
  responseVar = "Response"
)

# ───────────────────────────────────────────────────────────
# Example 3: Timeline with Milestone Events
# ───────────────────────────────────────────────────────────

# Add key clinical milestones
milestone_plot <- swimmerplot(
  data = swimmerplot_test,
  patientID = "PatientID",
  startTime = "StartTime",
  endTime = "EndTime",
  responseVar = "Response",
  milestone1 = "TreatmentStart",
  milestone1_name = "Treatment",
  milestone2 = "FirstAssessment",
  milestone2_name = "First Assessment",
  milestone3 = "BestResponse",
  milestone3_name = "Best Response",
  milestone4 = "Progression",
  milestone4_name = "Progression"
)

# ───────────────────────────────────────────────────────────
# Example 4: Immunotherapy Trial Timeline
# ───────────────────────────────────────────────────────────

# Load immunotherapy data
data(swimmerplot_immuno)

# Comprehensive immunotherapy visualization
immuno_plot <- swimmerplot(
  data = swimmerplot_immuno,
  patientID = "PatientID",
  startTime = "StartTime",
  endTime = "EndTime",
  responseVar = "Response",
  milestone1 = "ImmunotherapyStart",
  milestone1_name = "IO Initiation",
  milestone2 = "FirstResponse",
  milestone2_name = "First Response (8-12 wk)",
  milestone3 = "ConfirmedResponse",
  milestone3_name = "Confirmed Response (12-16 wk)",
  eventVar = "irAE",
  eventTimeVar = "irAE_Time",
  groupVar = "PDL1_Status",
  censorVar = "Censored",
  sortBy = "duration",
  colorPalette = "Set2",
  showReferenceLine = TRUE,
  referenceLineValue = 180,
  plotTitle = "Immunotherapy Trial: Patient Follow-up",
  xAxisTitle = "Time (Days)",
  timeUnit = "days"
)

# ───────────────────────────────────────────────────────────
# Example 5: Surgical Outcomes Timeline
# ───────────────────────────────────────────────────────────

# Load surgery data
data(swimmerplot_surgery)

# Perioperative timeline with complications
surgery_plot <- swimmerplot(
  data = swimmerplot_surgery,
  patientID = "PatientID",
  startTime = "StartTime",
  endTime = "EndTime",
  responseVar = "Outcome",
  milestone1 = "SurgeryDate",
  milestone1_name = "Surgery",
  milestone2 = "Discharge",
  milestone2_name = "Hospital Discharge",
  milestone3 = "FirstVisit",
  milestone3_name = "First Follow-up Visit",
  milestone4 = "ComplicationDate",
  milestone4_name = "Complication Onset",
  eventVar = "ComplicationType",
  eventTimeVar = "ComplicationDate",
  groupVar = "SurgeryType",
  sortBy = "duration",
  colorPalette = "Dark2",
  plotTitle = "Surgical Outcomes: Postoperative Timeline",
  xAxisTitle = "Days Post-Surgery"
)

# ───────────────────────────────────────────────────────────
# Example 6: Treatment Arms Comparison
# ───────────────────────────────────────────────────────────

# Load grouped comparison data
data(swimmerplot_grouped)

# Compare experimental vs control arms
comparison_plot <- swimmerplot(
  data = swimmerplot_grouped,
  patientID = "PatientID",
  startTime = "StartTime",
  endTime = "EndTime",
  responseVar = "Response",
  milestone1 = "TreatmentStart",
  milestone1_name = "Treatment Initiation",
  milestone2 = "FirstAssessment",
  milestone2_name = "8-Week Assessment",
  milestone3 = "BestResponse",
  milestone3_name = "Best Response",
  milestone4 = "Progression",
  milestone4_name = "Disease Progression",
  eventVar = "AdverseEvent",
  eventTimeVar = "EventTime",
  groupVar = "Group",
  censorVar = "Censored",
  sortBy = "duration",
  colorPalette = "Set1",
  showReferenceLine = TRUE,
  referenceLineValue = 180,
  plotTitle = "Phase III Trial: Experimental vs Control",
  xAxisTitle = "Time from Enrollment (Days)",
  showLegend = TRUE
)

# ───────────────────────────────────────────────────────────
# Example 7: Five Milestones Timeline
# ───────────────────────────────────────────────────────────

# Load data with all five milestones
data(swimmerplot_milestones)

# Complete disease course timeline
complete_timeline <- swimmerplot(
  data = swimmerplot_milestones,
  patientID = "PatientID",
  startTime = "StartTime",
  endTime = "EndTime",
  responseVar = "Response",
  milestone1 = "Diagnosis",
  milestone1_name = "Diagnosis",
  milestone2 = "Surgery",
  milestone2_name = "Surgical Resection",
  milestone3 = "ChemoStart",
  milestone3_name = "Adjuvant Chemo",
  milestone4 = "Recurrence",
  milestone4_name = "Disease Recurrence",
  milestone5 = "Death",
  milestone5_name = "Death",
  eventVar = "EventType",
  eventTimeVar = "EventTime",
  groupVar = "Stage",
  censorVar = "Censored",
  sortBy = "response",
  colorPalette = "viridis",
  plotTitle = "Complete Disease Course Timeline",
  xAxisTitle = "Time (Days)",
  showLegend = TRUE
)

# ───────────────────────────────────────────────────────────
# Example 8: Timeline with Multiple Events
# ───────────────────────────────────────────────────────────

# Load events data
data(swimmerplot_events)

# Focus on adverse events and treatment modifications
events_plot <- swimmerplot(
  data = swimmerplot_events,
  patientID = "PatientID",
  startTime = "StartTime",
  endTime = "EndTime",
  responseVar = "Response",
  milestone1 = "TreatmentStart",
  milestone1_name = "Treatment Start",
  eventVar = "Event1_Type",
  eventTimeVar = "Event1_Time",
  groupVar = "TreatmentLine",
  sortBy = "none",
  colorPalette = "Accent",
  plotTitle = "Treatment-Related Events Timeline",
  xAxisTitle = "Days from Treatment Start"
)

# ───────────────────────────────────────────────────────────
# Example 9: Timeline with Time Unit Conversion
# ───────────────────────────────────────────────────────────

# Display timeline in weeks
weeks_plot <- swimmerplot(
  data = swimmerplot_test,
  patientID = "PatientID",
  startTime = "StartTime",
  endTime = "EndTime",
  responseVar = "Response",
  milestone1 = "TreatmentStart",
  milestone1_name = "Treatment",
  milestone2 = "FirstAssessment",
  milestone2_name = "Assessment",
  timeUnit = "weeks",
  plotTitle = "Clinical Trial Timeline",
  xAxisTitle = "Time (Weeks)"
)

# Display timeline in months
months_plot <- swimmerplot(
  data = swimmerplot_test,
  patientID = "PatientID",
  startTime = "StartTime",
  endTime = "EndTime",
  responseVar = "Response",
  timeUnit = "months",
  plotTitle = "Long-term Follow-up",
  xAxisTitle = "Time (Months)"
)

# ───────────────────────────────────────────────────────────
# Example 10: Publication-Ready Timeline
# ───────────────────────────────────────────────────────────

# High-quality visualization for publication
publication_plot <- swimmerplot(
  data = swimmerplot_test,
  patientID = "PatientID",
  startTime = "StartTime",
  endTime = "EndTime",
  responseVar = "Response",
  milestone1 = "TreatmentStart",
  milestone1_name = "Treatment Initiation",
  milestone2 = "FirstAssessment",
  milestone2_name = "First Response Assessment",
  milestone3 = "BestResponse",
  milestone3_name = "Best Overall Response",
  milestone4 = "Progression",
  milestone4_name = "Disease Progression",
  eventVar = "AdverseEvent",
  eventTimeVar = "EventTime",
  censorVar = "Censored",
  groupVar = "TreatmentArm",
  sortBy = "duration",
  colorPalette = "Set2",
  plotTitle = "Phase II Clinical Trial: Patient Follow-up Timeline",
  xAxisTitle = "Time Since Enrollment (Days)",
  yAxisTitle = "Patient Identifier",
  showLegend = TRUE,
  showReferenceLine = TRUE,
  referenceLineValue = 180,
  timeUnit = "days"
)

# ───────────────────────────────────────────────────────────
# Example 11: Date/Time Format Handling
# ───────────────────────────────────────────────────────────

# Load data with actual dates
data(swimmerplot_dates)

# Timeline using date formats
dates_plot <- swimmerplot(
  data = swimmerplot_dates,
  patientID = "PatientID",
  startTime = "StartTime",
  endTime = "EndTime",
  responseVar = "Response",
  milestone1 = "Milestone1_Days",
  milestone1_name = "Treatment Start",
  milestone2 = "Milestone2_Days",
  milestone2_name = "First Response",
  milestone3 = "Milestone3_Days",
  milestone3_name = "Progression",
  groupVar = "Cohort",
  censorVar = "Censored",
  timeType = "raw",
  dateFormat = "YYYY-MM-DD",
  sortBy = "duration",
  plotTitle = "Cohort Study Timeline"
)

# ───────────────────────────────────────────────────────────
# Example 12: Sorted Timelines
# ───────────────────────────────────────────────────────────

# Sort by duration (longest to shortest)
sorted_duration <- swimmerplot(
  data = swimmerplot_test,
  patientID = "PatientID",
  startTime = "StartTime",
  endTime = "EndTime",
  responseVar = "Response",
  sortBy = "duration",
  plotTitle = "Sorted by Duration"
)

# Sort by response category
sorted_response <- swimmerplot(
  data = swimmerplot_test,
  patientID = "PatientID",
  startTime = "StartTime",
  endTime = "EndTime",
  responseVar = "Response",
  sortBy = "response",
  plotTitle = "Sorted by Response (CR/PR/SD/PD)"
)

# No sorting (original order)
no_sort <- swimmerplot(
  data = swimmerplot_test,
  patientID = "PatientID",
  startTime = "StartTime",
  endTime = "EndTime",
  responseVar = "Response",
  sortBy = "none",
  plotTitle = "Original Patient Order"
)

# ───────────────────────────────────────────────────────────
# Clinical Interpretation Guide
# ───────────────────────────────────────────────────────────

# Swimmer Plot Applications:
# 1. Clinical Trial Reporting (RECIST response, PFS/OS)
# 2. Case Series Visualization (treatment timelines)
# 3. Immunotherapy Trials (delayed/durable responses)
# 4. Surgical Outcomes (perioperative events)
# 5. Adverse Event Timelines (safety monitoring)
# 6. Treatment Modification Tracking (dose delays/reductions)
# 7. Multi-line Therapy Sequencing
# 8. Real-world Evidence Studies (variable follow-up)

# Response Categories (RECIST 1.1):
# - CR (Complete Response): Disappearance of all target lesions
# - PR (Partial Response): ≥30% decrease in sum of target lesions
# - SD (Stable Disease): Neither PR nor PD criteria met
# - PD (Progressive Disease): ≥20% increase or new lesions

# Timeline Components:
# - Start Time: Treatment initiation or study enrollment
# - End Time: Death, progression, last follow-up, or data cutoff
# - Milestones: Key clinical events (response assessment, surgery, etc.)
# - Events: Adverse events, dose modifications, hospitalizations
# - Censoring: Ongoing response at last follow-up (open circle/triangle)

# ───────────────────────────────────────────────────────────
# Tips for Effective Swimmer Plots
# ───────────────────────────────────────────────────────────

# 1. Color Coding:
#    - Use intuitive colors (green for CR/PR, red for PD)
#    - Consistent color palette across related plots
#    - Consider colorblind-friendly palettes (viridis, Set2)

# 2. Milestone Selection:
#    - Limit to 3-5 most clinically relevant events
#    - Use clear, concise milestone names
#    - Ensure milestone times are within patient follow-up

# 3. Event Markers:
#    - Focus on clinically significant events
#    - Avoid overcrowding with too many markers
#    - Use shapes/symbols that print clearly

# 4. Sorting Strategy:
#    - Duration: Shows range of follow-up times
#    - Response: Groups patients by outcome
#    - None: Preserves enrollment order or patient ID sequence

# 5. Reference Lines:
#    - Mark clinically significant timepoints (e.g., 6 months)
#    - Show protocol-defined assessment times
#    - Indicate median PFS/OS from prior studies

# 6. Grouping:
#    - Stratify by treatment arm, biomarker status, stage
#    - Facilitates visual comparison between subgroups
#    - Consider separate plots for >3 groups

# ───────────────────────────────────────────────────────────
# Real-World Use Cases
# ───────────────────────────────────────────────────────────

# Use Case 1: Immunotherapy Durable Responders
# ---------------------------------------------
# Goal: Visualize long-term responders to checkpoint inhibitors

# Identify patients with >12 months response
long_responders <- swimmerplot_immuno[swimmerplot_immuno$EndTime > 365 &
                                      swimmerplot_immuno$Response %in% c("CR", "PR"), ]

if (nrow(long_responders) > 0) {
  durable_response_plot <- swimmerplot(
    data = long_responders,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    responseVar = "Response",
    milestone1 = "ImmunotherapyStart",
    milestone1_name = "IO Start",
    milestone2 = "FirstResponse",
    milestone2_name = "First Response",
    milestone3 = "ConfirmedResponse",
    milestone3_name = "Confirmed",
    eventVar = "irAE",
    eventTimeVar = "irAE_Time",
    groupVar = "PDL1_Status",
    timeUnit = "months",
    plotTitle = "Durable Responders (>12 Months)",
    xAxisTitle = "Time (Months)"
  )
}

# Use Case 2: Perioperative Timeline
# -----------------------------------
# Goal: Track surgical complications and recovery

surgery_timeline <- swimmerplot(
  data = swimmerplot_surgery,
  patientID = "PatientID",
  startTime = "StartTime",
  endTime = "EndTime",
  responseVar = "Outcome",
  milestone1 = "SurgeryDate",
  milestone1_name = "Surgery",
  milestone2 = "Discharge",
  milestone2_name = "Discharge",
  milestone3 = "FirstVisit",
  milestone3_name = "Follow-up",
  eventVar = "ComplicationType",
  eventTimeVar = "ComplicationDate",
  groupVar = "ASA_Score",
  sortBy = "duration",
  showReferenceLine = TRUE,
  referenceLineValue = 30,
  plotTitle = "30-Day Postoperative Outcomes",
  xAxisTitle = "Days Post-Surgery"
)

# Use Case 3: Treatment Sequencing
# ---------------------------------
# Goal: Visualize multi-line therapy patterns

# Filter to patients who received multiple treatment lines
multi_line <- swimmerplot_events[swimmerplot_events$TreatmentLine %in%
                                c("Second Line", "Third Line+"), ]

if (nrow(multi_line) > 0) {
  sequencing_plot <- swimmerplot(
    data = multi_line,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    responseVar = "Response",
    eventVar = "Event1_Type",
    eventTimeVar = "Event1_Time",
    groupVar = "TreatmentLine",
    sortBy = "duration",
    plotTitle = "Later-Line Therapy Outcomes"
  )
}

# Use Case 4: Adverse Event Timeline
# -----------------------------------
# Goal: Track timing and frequency of treatment-related AEs

ae_timeline <- swimmerplot(
  data = swimmerplot_test,
  patientID = "PatientID",
  startTime = "StartTime",
  endTime = "EndTime",
  eventVar = "AdverseEvent",
  eventTimeVar = "EventTime",
  groupVar = "TreatmentArm",
  sortBy = "none",
  colorPalette = "Pastel1",
  plotTitle = "Adverse Event Timeline by Treatment Arm",
  xAxisTitle = "Days from Treatment Start"
)

# ───────────────────────────────────────────────────────────
# Data Preparation Tips
# ───────────────────────────────────────────────────────────

# 1. Patient ID:
#    - Must be unique for each patient
#    - Can be alphanumeric (e.g., "PT-001", "Patient_A")
#    - Special characters allowed

# 2. Time Variables:
#    - StartTime: Usually 0 (treatment start or enrollment)
#    - EndTime: Positive numeric value (days, weeks, or months)
#    - Milestones: Numeric values between start and end
#    - Events: Numeric values when events occurred

# 3. Response Variable:
#    - Common values: "CR", "PR", "SD", "PD"
#    - Can use custom categories
#    - Will be color-coded in plot

# 4. Censoring:
#    - 0 = event occurred (death, progression)
#    - 1 = censored (ongoing at last follow-up)
#    - Used to show open vs closed symbols

# 5. Grouping Variable:
#    - Categorical variable for stratification
#    - Examples: treatment arm, biomarker status, stage
#    - Keep to 2-4 categories for clarity

# 6. Date Handling:
#    - Convert dates to numeric (days from baseline)
#    - Use consistent units across all time variables
#    - Handle missing dates appropriately (NA values)

# Example data preparation:
# raw_data <- read.csv("trial_data.csv")
# processed_data <- raw_data %>%
#   mutate(
#     StartTime = 0,
#     EndTime = as.numeric(LastFollowup - Enrollment),
#     Milestone1 = as.numeric(FirstAssessment - Enrollment),
#     EventTime = as.numeric(AdverseEventDate - Enrollment)
#   )

# ───────────────────────────────────────────────────────────
# Reporting Guidelines
# ───────────────────────────────────────────────────────────

# When including swimmer plots in publications, report:
# 1. Number of patients included
# 2. Follow-up duration (median and range)
# 3. Response evaluation criteria (e.g., RECIST 1.1)
# 4. Milestone definitions
# 5. Event marker definitions
# 6. Censoring approach
# 7. Color coding scheme
# 8. Sorting method used

# Example caption:
# "Figure 1. Swimmer plot showing individual patient treatment duration,
# response, and key clinical milestones. Each horizontal bar represents one
# patient (n=30). Bar color indicates best overall response per RECIST 1.1
# (green=CR/PR, yellow=SD, red=PD). Triangles mark clinical milestones:
# treatment initiation (▲), first response assessment (●), and disease
# progression (■). Open symbols indicate censored patients. Patients are
# sorted by treatment duration (longest to shortest). Vertical dashed line
# indicates 6-month timepoint."

# ───────────────────────────────────────────────────────────
# References and Resources
# ───────────────────────────────────────────────────────────

# Visualization Guidelines:
# - Dancey JE, et al. Guidelines for the Development and Incorporation
#   of Biomarker Studies in Early Clinical Trials of Novel Agents.
#   Clin Cancer Res. 2010.
# - Korn EL, et al. Clinical trial designs for cytostatic agents:
#   are new approaches needed? J Clin Oncol. 2001.

# Response Criteria:
# - Eisenhauer EA, et al. New response evaluation criteria in solid
#   tumours: Revised RECIST guideline (version 1.1). Eur J Cancer. 2009.
# - Seymour L, et al. iRECIST: guidelines for response criteria for use
#   in trials testing immunotherapeutics. Lancet Oncol. 2017.

# Statistical Considerations:
# - Swimmer plots are descriptive, not inferential
# - Complement with Kaplan-Meier curves for time-to-event analysis
# - Consider waterfall plots for best response visualization
# - Use forest plots for subgroup analyses
