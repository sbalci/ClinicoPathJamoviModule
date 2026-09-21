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
  milestone1Date = "TreatmentStart",
  milestone1Name = "Treatment",
  milestone2Date = "FirstAssessment",
  milestone2Name = "First Assessment",
  milestone3Date = "BestResponse",
  milestone3Name = "Best Response",
  milestone4Date = "Progression",
  milestone4Name = "Progression"
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
  milestone1Date = "ImmunotherapyStart",
  milestone1Name = "IO Initiation",
  milestone2Date = "FirstResponse",
  milestone2Name = "First Response (8-12 wk)",
  milestone3Date = "ConfirmedResponse",
  milestone3Name = "Confirmed Response (12-16 wk)",
  showEventMarkers = TRUE,
  eventVar = "irAE",
  eventTimeVar = "irAE_Time",
  groupVar = "PDL1_Status",
  censorVar = "Censored",
  sortOrder = "duration_desc",
  colorPalette = "default",
  referenceLines = "custom",
  customReferenceTime = 180,
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
  milestone1Date = "SurgeryDate",
  milestone1Name = "Surgery",
  milestone2Date = "Discharge",
  milestone2Name = "Hospital Discharge",
  milestone3Date = "FirstVisit",
  milestone3Name = "First Follow-up Visit",
  milestone4Date = "ComplicationDate",
  milestone4Name = "Complication Onset",
  showEventMarkers = TRUE,
  eventVar = "ComplicationType",
  eventTimeVar = "ComplicationDate",
  groupVar = "SurgeryType",
  sortOrder = "duration_desc",
  colorPalette = "contrast"
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
  milestone1Date = "TreatmentStart",
  milestone1Name = "Treatment Initiation",
  milestone2Date = "FirstAssessment",
  milestone2Name = "8-Week Assessment",
  milestone3Date = "BestResponse",
  milestone3Name = "Best Response",
  milestone4Date = "Progression",
  milestone4Name = "Disease Progression",
  showEventMarkers = TRUE,
  eventVar = "AdverseEvent",
  eventTimeVar = "EventTime",
  groupVar = "Group",
  censorVar = "Censored",
  sortOrder = "duration_desc",
  colorPalette = "contrast",
  referenceLines = "custom",
  customReferenceTime = 180,
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
  milestone1Date = "Diagnosis",
  milestone1Name = "Diagnosis",
  milestone2Date = "Surgery",
  milestone2Name = "Surgical Resection",
  milestone3Date = "ChemoStart",
  milestone3Name = "Adjuvant Chemo",
  milestone4Date = "Recurrence",
  milestone4Name = "Disease Recurrence",
  milestone5Date = "Death",
  milestone5Name = "Death",
  showEventMarkers = TRUE,
  eventVar = "EventType",
  eventTimeVar = "EventTime",
  groupVar = "Stage",
  censorVar = "Censored",
  sortOrder = "response",
  colorPalette = "viridis",
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
  milestone1Date = "TreatmentStart",
  milestone1Name = "Treatment Start",
  showEventMarkers = TRUE,
  eventVar = "Event1_Type",
  eventTimeVar = "Event1_Time",
  groupVar = "TreatmentLine",
  sortOrder = "patient_id",
  colorPalette = "viridis"
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
  milestone1Date = "TreatmentStart",
  milestone1Name = "Treatment",
  milestone2Date = "FirstAssessment",
  milestone2Name = "Assessment",
  timeUnit = "weeks"
)

# Display timeline in months
months_plot <- swimmerplot(
  data = swimmerplot_test,
  patientID = "PatientID",
  startTime = "StartTime",
  endTime = "EndTime",
  responseVar = "Response",
  timeUnit = "months"
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
  milestone1Date = "TreatmentStart",
  milestone1Name = "Treatment Initiation",
  milestone2Date = "FirstAssessment",
  milestone2Name = "First Response Assessment",
  milestone3Date = "BestResponse",
  milestone3Name = "Best Overall Response",
  milestone4Date = "Progression",
  milestone4Name = "Disease Progression",
  showEventMarkers = TRUE,
  eventVar = "AdverseEvent",
  eventTimeVar = "EventTime",
  censorVar = "Censored",
  groupVar = "TreatmentArm",
  sortOrder = "duration_desc",
  colorPalette = "default",
  showLegend = TRUE,
  referenceLines = "custom",
  customReferenceTime = 180,
  timeUnit = "days"
)

# ───────────────────────────────────────────────────────────
# Example 11: Date/Time Format Handling
# ───────────────────────────────────────────────────────────

# Load data with actual dates
data(swimmerplot_dates)

# Timeline using the REAL date columns. The dataset also carries numeric
# StartTime/EndTime day offsets; this example deliberately uses the Date columns
# with timeType = "datetime", which is what dateFormat applies to. (It used to
# pass timeType = "raw" and the numeric *_Days milestones, so every line about
# date handling was describing something the call never did.)
dates_plot <- swimmerplot(
  data = swimmerplot_dates,
  patientID = "PatientID",
  startTime = "EnrollmentDate",
  endTime = "LastVisitDate",
  responseVar = "Response",
  milestone1Date = "TreatmentStartDate",
  milestone1Name = "Treatment Start",
  milestone2Date = "FirstResponseDate",
  milestone2Name = "First Response",
  milestone3Date = "ProgressionDate",
  milestone3Name = "Progression",
  groupVar = "Cohort",
  censorVar = "Censored",
  timeType = "datetime",
  dateFormat = "ymd",
  timeUnit = "days",
  timeDisplay = "relative"
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
  sortOrder = "duration_desc"
)

# Sort by response category
sorted_response <- swimmerplot(
  data = swimmerplot_test,
  patientID = "PatientID",
  startTime = "StartTime",
  endTime = "EndTime",
  responseVar = "Response",
  sortOrder = "response"
)

# Ordered by patient ID (there is no 'unsorted' option; patients are always ordered)
no_sort <- swimmerplot(
  data = swimmerplot_test,
  patientID = "PatientID",
  startTime = "StartTime",
  endTime = "EndTime",
  responseVar = "Response",
  sortOrder = "patient_id"
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
# - PR (Partial Response): >=30% decrease in sum of target lesions
# - SD (Stable Disease): Neither PR nor PD criteria met
# - PD (Progressive Disease): >=20% increase or new lesions

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
#    - Consider colorblind-friendly palettes (viridis, contrast)

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
#    - Patient ID: orders by identifier, numerically when every ID is a number

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
    milestone1Date = "ImmunotherapyStart",
    milestone1Name = "IO Start",
    milestone2Date = "FirstResponse",
    milestone2Name = "First Response",
    milestone3Date = "ConfirmedResponse",
    milestone3Name = "Confirmed",
    showEventMarkers = TRUE,
    eventVar = "irAE",
    eventTimeVar = "irAE_Time",
    groupVar = "PDL1_Status",
    timeUnit = "months"
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
  milestone1Date = "SurgeryDate",
  milestone1Name = "Surgery",
  milestone2Date = "Discharge",
  milestone2Name = "Discharge",
  milestone3Date = "FirstVisit",
  milestone3Name = "Follow-up",
  showEventMarkers = TRUE,
  eventVar = "ComplicationType",
  eventTimeVar = "ComplicationDate",
  groupVar = "ASA_Score",
  sortOrder = "duration_desc",
  referenceLines = "custom",
  customReferenceTime = 30
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
    showEventMarkers = TRUE,
    eventVar = "Event1_Type",
    eventTimeVar = "Event1_Time",
    groupVar = "TreatmentLine",
    sortOrder = "duration_desc"
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
  showEventMarkers = TRUE,
  eventVar = "AdverseEvent",
  eventTimeVar = "EventTime",
  groupVar = "TreatmentArm",
  sortOrder = "patient_id",
  colorPalette = "monochrome"
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
