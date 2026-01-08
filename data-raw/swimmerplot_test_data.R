# ═══════════════════════════════════════════════════════════
# Test Data Generation: swimmerplot
# ═══════════════════════════════════════════════════════════
#
# Generate comprehensive test datasets for the swimmerplot jamovi function
# Creates multiple scenarios for clinical timeline visualization testing

library(tibble)
library(dplyr)
library(here)
library(writexl)
library(jmvReadWrite)

set.seed(42) # For reproducibility

# ───────────────────────────────────────────────────────────
# Dataset 1: Main Clinical Trial Dataset
# ───────────────────────────────────────────────────────────
# Realistic oncology trial with 30 patients, multiple milestones,
# response assessments, and varying follow-up times

n_patients <- 30

swimmerplot_test <- tibble(
  PatientID = paste0("PT", sprintf("%03d", 1:n_patients)),
  StartTime = 0, # All start at time 0

  # Treatment duration (days): 30-365 days
  TreatmentDuration = sample(30:365, n_patients, replace = TRUE),

  # Response assessment (CR/PR/SD/PD)
  Response = sample(c("CR", "PR", "SD", "PD"), n_patients, replace = TRUE,
                   prob = c(0.15, 0.35, 0.30, 0.20)),

  # Milestone 1: Treatment Start (always at 0)
  TreatmentStart = 0,

  # Milestone 2: First Response Assessment (6-12 weeks)
  FirstAssessment = sample(42:84, n_patients, replace = TRUE),

  # Milestone 3: Best Response (8-24 weeks)
  BestResponse = sapply(1:n_patients, function(i) {
    sample(56:168, 1)
  }),

  # Milestone 4: Disease Progression (or NA if no progression)
  Progression = sapply(1:n_patients, function(i) {
    if (runif(1) < 0.6) { # 60% have progression
      sample(90:300, 1)
    } else {
      NA_real_
    }
  }),

  # Milestone 5: Death or Last Follow-up
  LastFollowup = sapply(1:n_patients, function(i) {
    if (runif(1) < 0.3) { # 30% died
      sample(180:365, 1)
    } else {
      TreatmentDuration[i]
    }
  }),

  # Censoring status (1 = event/death, 0 = censored)
  Censored = ifelse(runif(n_patients) < 0.3, 0, 1),

  # Treatment arm grouping
  TreatmentArm = sample(c("Arm A", "Arm B"), n_patients, replace = TRUE),

  # Adverse events (multiple possible)
  AdverseEvent = sample(c("None", "Grade 1-2", "Grade 3-4"), n_patients,
                       replace = TRUE, prob = c(0.4, 0.4, 0.2)),

  # Event time (when adverse event occurred, NA if none)
  EventTime = sapply(1:n_patients, function(i) {
    if (AdverseEvent[i] != "None") {
      sample(7:TreatmentDuration[i], 1)
    } else {
      NA_real_
    }
  })
) %>%
  # EndTime is the maximum of treatment duration and last follow-up
  mutate(EndTime = pmax(TreatmentDuration, LastFollowup, na.rm = TRUE))

# ───────────────────────────────────────────────────────────
# Dataset 2: Oncology Immunotherapy Timeline
# ───────────────────────────────────────────────────────────
# Focused on immunotherapy response patterns with durable responses

n_immuno <- 25

swimmerplot_immuno <- tibble(
  PatientID = paste0("IM", sprintf("%03d", 1:n_immuno)),
  StartTime = 0,

  # Immunotherapy duration (often longer for responders)
  EndTime = c(
    sample(180:730, 10, replace = TRUE), # Durable responders (>6 months)
    sample(60:180, 10, replace = TRUE),  # Moderate responders
    sample(30:90, 5, replace = TRUE)     # Non-responders
  ),

  # Response (higher CR/PR rate for immunotherapy)
  Response = c(
    sample(c("CR", "PR"), 10, replace = TRUE, prob = c(0.4, 0.6)),
    sample(c("PR", "SD"), 10, replace = TRUE, prob = c(0.5, 0.5)),
    rep("PD", 5)
  ),

  # Treatment start
  ImmunotherapyStart = 0,

  # First response assessment (typically 8-12 weeks for immunotherapy)
  FirstResponse = sample(56:84, n_immuno, replace = TRUE),

  # Confirmed response (12-16 weeks)
  ConfirmedResponse = sapply(1:n_immuno, function(i) {
    if (Response[i] %in% c("CR", "PR")) {
      sample(84:112, 1)
    } else {
      NA_real_
    }
  }),

  # Immune-related adverse events
  irAE = sample(c("None", "Skin", "GI", "Endocrine", "Pneumonitis"),
               n_immuno, replace = TRUE, prob = c(0.4, 0.2, 0.15, 0.15, 0.1)),

  irAE_Time = sapply(1:n_immuno, function(i) {
    if (irAE[i] != "None") {
      sample(14:EndTime[i], 1)
    } else {
      NA_real_
    }
  }),

  # PD-L1 status grouping
  PDL1_Status = sample(c("Negative", "Low (1-49%)", "High (≥50%)"),
                      n_immuno, replace = TRUE, prob = c(0.3, 0.4, 0.3)),

  Censored = ifelse(Response %in% c("CR", "PR"), 1, 0)
)

# ───────────────────────────────────────────────────────────
# Dataset 3: Surgery Outcomes Timeline
# ───────────────────────────────────────────────────────────
# Surgical patients with perioperative events and recovery milestones

n_surgery <- 20

swimmerplot_surgery <- tibble(
  PatientID = paste0("SX", sprintf("%03d", 1:n_surgery)),
  StartTime = 0,

  # Total follow-up (30 days to 1 year post-surgery)
  EndTime = sample(30:365, n_surgery, replace = TRUE),

  # Outcome
  Outcome = sample(c("No Complications", "Minor Complications", "Major Complications"),
                  n_surgery, replace = TRUE, prob = c(0.6, 0.25, 0.15)),

  # Surgery date (day 0)
  SurgeryDate = 0,

  # Hospital discharge (2-14 days post-op)
  Discharge = sample(2:14, n_surgery, replace = TRUE),

  # First follow-up visit (14-30 days)
  FirstVisit = sample(14:30, n_surgery, replace = TRUE),

  # Complication onset (if any)
  ComplicationDate = sapply(1:n_surgery, function(i) {
    if (Outcome[i] != "No Complications") {
      sample(1:Discharge[i] + 5, 1)
    } else {
      NA_real_
    }
  }),

  # Type of complication
  ComplicationType = sapply(1:n_surgery, function(i) {
    if (Outcome[i] != "No Complications") {
      sample(c("Infection", "Bleeding", "Wound Dehiscence", "DVT"), 1)
    } else {
      "None"
    }
  }),

  # Surgery type grouping
  SurgeryType = sample(c("Abdominal", "Thoracic", "Vascular"),
                      n_surgery, replace = TRUE),

  # ASA score grouping
  ASA_Score = sample(c("ASA I-II", "ASA III", "ASA IV"),
                    n_surgery, replace = TRUE, prob = c(0.4, 0.4, 0.2)),

  Censored = 1 # All followed to end
)

# ───────────────────────────────────────────────────────────
# Dataset 4: Multi-Milestone Rich Dataset
# ───────────────────────────────────────────────────────────
# Complex timeline with all 5 milestones utilized

n_complex <- 20

swimmerplot_milestones <- tibble(
  PatientID = paste0("ML", sprintf("%03d", 1:n_complex)),
  StartTime = 0,
  EndTime = sample(180:540, n_complex, replace = TRUE),

  Response = sample(c("CR", "PR", "SD", "PD"), n_complex, replace = TRUE),

  # All 5 milestones with clinical relevance
  Diagnosis = 0,

  Surgery = sample(14:42, n_complex, replace = TRUE),

  ChemoStart = sapply(1:n_complex, function(i) {
    Surgery[i] + sample(14:28, 1)
  }),

  Recurrence = sapply(1:n_complex, function(i) {
    if (runif(1) < 0.5) {
      sample(90:EndTime[i], 1)
    } else {
      NA_real_
    }
  }),

  Death = sapply(1:n_complex, function(i) {
    if (runif(1) < 0.3) {
      EndTime[i]
    } else {
      NA_real_
    }
  }),

  # Event markers
  EventType = sample(c("None", "Toxicity", "Hospitalization", "Secondary Surgery"),
                    n_complex, replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1)),

  EventTime = sapply(1:n_complex, function(i) {
    if (EventType[i] != "None") {
      sample(30:EndTime[i], 1)
    } else {
      NA_real_
    }
  }),

  Stage = sample(c("Stage I", "Stage II", "Stage III", "Stage IV"),
                n_complex, replace = TRUE),

  Censored = ifelse(is.na(Death), 1, 0)
)

# ───────────────────────────────────────────────────────────
# Dataset 5: Event Markers Dataset
# ───────────────────────────────────────────────────────────
# Focus on multiple events per patient

n_events <- 15

swimmerplot_events <- tibble(
  PatientID = paste0("EV", sprintf("%03d", 1:n_events)),
  StartTime = 0,
  EndTime = sample(180:365, n_events, replace = TRUE),

  Response = sample(c("PR", "SD", "PD"), n_events, replace = TRUE),

  TreatmentStart = 0,

  # Multiple events possible
  Event1_Type = sample(c("Lab Abnormality", "Dose Reduction", "Treatment Delay"),
                      n_events, replace = TRUE),
  Event1_Time = sample(14:60, n_events, replace = TRUE),

  Event2_Type = sapply(1:n_events, function(i) {
    if (runif(1) < 0.7) {
      sample(c("Lab Abnormality", "Dose Reduction", "Treatment Delay"), 1)
    } else {
      "None"
    }
  }),
  Event2_Time = sapply(1:n_events, function(i) {
    if (Event2_Type[i] != "None") {
      sample(Event1_Time[i]:EndTime[i], 1)
    } else {
      NA_real_
    }
  }),

  Event3_Type = sapply(1:n_events, function(i) {
    if (runif(1) < 0.4) {
      sample(c("Hospitalization", "ER Visit"), 1)
    } else {
      "None"
    }
  }),
  Event3_Time = sapply(1:n_events, function(i) {
    if (Event3_Type[i] != "None" && !is.na(Event2_Time[i])) {
      sample(Event2_Time[i]:EndTime[i], 1)
    } else if (Event3_Type[i] != "None") {
      sample(Event1_Time[i]:EndTime[i], 1)
    } else {
      NA_real_
    }
  }),

  TreatmentLine = sample(c("First Line", "Second Line", "Third Line+"),
                        n_events, replace = TRUE),

  Censored = 1
)

# ───────────────────────────────────────────────────────────
# Dataset 6: Small Dataset for Edge Case Testing
# ───────────────────────────────────────────────────────────

swimmerplot_small <- tibble(
  PatientID = c("PT001", "PT002", "PT003", "PT004", "PT005"),
  StartTime = c(0, 0, 0, 0, 0),
  EndTime = c(90, 180, 45, 270, 150),
  Response = c("CR", "PR", "PD", "SD", "PR"),

  Milestone1 = c(0, 0, 0, 0, 0),
  Milestone2 = c(30, 42, 21, 56, 35),
  Milestone3 = c(60, 90, NA, 120, 90),

  EventVar = c("AE Grade 2", "None", "AE Grade 3", "AE Grade 1", "None"),
  EventTime = c(15, NA, 10, 80, NA),

  Group = c("A", "B", "A", "B", "A"),
  Censored = c(1, 1, 0, 1, 1)
)

# ───────────────────────────────────────────────────────────
# Dataset 7: Date/Time Format Dataset
# ───────────────────────────────────────────────────────────
# Uses actual date values instead of numeric days

n_dates <- 20
base_date <- as.Date("2023-01-01")

# Create enrollment dates first
enrollment_dates <- base_date + sample(0:90, n_dates, replace = TRUE)
follow_up_days <- sample(90:365, n_dates, replace = TRUE)

swimmerplot_dates <- tibble(
  PatientID = paste0("DT", sprintf("%03d", 1:n_dates)),

  # Dates instead of numeric
  EnrollmentDate = enrollment_dates,
  LastVisitDate = enrollment_dates + follow_up_days,

  # Calculate numeric time from dates
  StartTime = 0,
  EndTime = follow_up_days,

  Response = sample(c("CR", "PR", "SD", "PD"), n_dates, replace = TRUE),

  # Milestone dates
  TreatmentStartDate = enrollment_dates,
  FirstResponseDate = enrollment_dates + sample(42:84, n_dates, replace = TRUE),

  # Calculate progression date
  ProgressionDays = sapply(1:n_dates, function(i) {
    if (runif(1) < 0.5) {
      sample(90:follow_up_days[i], 1)
    } else {
      NA_real_
    }
  }),

  ProgressionDate = enrollment_dates + ProgressionDays,

  # Convert milestone dates to numeric days from enrollment
  Milestone1_Days = 0,
  Milestone2_Days = as.numeric(FirstResponseDate - EnrollmentDate),
  Milestone3_Days = ProgressionDays,

  Cohort = sample(c("Cohort 1", "Cohort 2", "Cohort 3"),
                 n_dates, replace = TRUE),

  Censored = ifelse(is.na(ProgressionDate), 1, 0)
) %>%
  select(-ProgressionDays) # Remove temporary column

# ───────────────────────────────────────────────────────────
# Dataset 8: Grouped Comparison Dataset
# ───────────────────────────────────────────────────────────
# Clear differences between groups for comparative visualization

n_grouped <- 24

swimmerplot_grouped <- tibble(
  PatientID = paste0("GP", sprintf("%03d", 1:n_grouped)),
  StartTime = 0,

  # Group A: Better outcomes (longer duration)
  # Group B: Worse outcomes (shorter duration)
  Group = rep(c("Experimental", "Control"), each = 12),

  EndTime = c(
    sample(180:450, 12, replace = TRUE), # Experimental: 6-15 months
    sample(60:240, 12, replace = TRUE)   # Control: 2-8 months
  ),

  Response = c(
    sample(c("CR", "PR", "SD"), 12, replace = TRUE, prob = c(0.3, 0.5, 0.2)),
    sample(c("PR", "SD", "PD"), 12, replace = TRUE, prob = c(0.2, 0.4, 0.4))
  ),

  TreatmentStart = 0,

  FirstAssessment = sample(42:84, n_grouped, replace = TRUE),

  BestResponse = sapply(1:n_grouped, function(i) {
    sample(84:EndTime[i]/2, 1)
  }),

  Progression = sapply(1:n_grouped, function(i) {
    if (Group[i] == "Experimental") {
      if (runif(1) < 0.4) sample(180:EndTime[i], 1) else NA_real_
    } else {
      if (runif(1) < 0.7) sample(90:EndTime[i], 1) else NA_real_
    }
  }),

  AdverseEvent = sample(c("None", "Grade 1-2", "Grade 3+"),
                       n_grouped, replace = TRUE),

  EventTime = sapply(1:n_grouped, function(i) {
    if (AdverseEvent[i] != "None") {
      sample(7:EndTime[i], 1)
    } else {
      NA_real_
    }
  }),

  Biomarker = sample(c("Positive", "Negative"), n_grouped, replace = TRUE),

  Censored = ifelse(is.na(Progression), 1, 0)
)

# ═══════════════════════════════════════════════════════════
# Save all datasets in multiple formats
# ═══════════════════════════════════════════════════════════

datasets <- list(
  "swimmerplot_test" = swimmerplot_test,
  "swimmerplot_immuno" = swimmerplot_immuno,
  "swimmerplot_surgery" = swimmerplot_surgery,
  "swimmerplot_milestones" = swimmerplot_milestones,
  "swimmerplot_events" = swimmerplot_events,
  "swimmerplot_small" = swimmerplot_small,
  "swimmerplot_dates" = swimmerplot_dates,
  "swimmerplot_grouped" = swimmerplot_grouped
)

# Save as .rda files
for (name in names(datasets)) {
  assign(name, datasets[[name]])
  save(list = name, file = here::here("data", paste0(name, ".rda")))
  message("✓ Saved ", name, ".rda")
}

# Save as .csv files
for (name in names(datasets)) {
  write.csv(datasets[[name]],
           here::here("data", paste0(name, ".csv")),
           row.names = FALSE)
  message("✓ Saved ", name, ".csv")
}

# Save as .xlsx files
for (name in names(datasets)) {
  writexl::write_xlsx(datasets[[name]],
                     here::here("data", paste0(name, ".xlsx")))
  message("✓ Saved ", name, ".xlsx")
}

# Save as .omv files (jamovi format)
for (name in names(datasets)) {
  jmvReadWrite::write_omv(datasets[[name]],
                         here::here("data", paste0(name, ".omv")))
  message("✓ Saved ", name, ".omv")
}

message("\n═══════════════════════════════════════════════════════════")
message("✓ All swimmerplot test datasets generated successfully!")
message("═══════════════════════════════════════════════════════════")
message("\nDatasets created:")
message("  1. swimmerplot_test (30 patients, clinical trial)")
message("  2. swimmerplot_immuno (25 patients, immunotherapy)")
message("  3. swimmerplot_surgery (20 patients, surgical outcomes)")
message("  4. swimmerplot_milestones (20 patients, 5 milestones)")
message("  5. swimmerplot_events (15 patients, multiple events)")
message("  6. swimmerplot_small (5 patients, edge cases)")
message("  7. swimmerplot_dates (20 patients, date format)")
message("  8. swimmerplot_grouped (24 patients, group comparison)")
message("\nTotal: 8 datasets × 4 formats = 32 files")
message("═══════════════════════════════════════════════════════════")
