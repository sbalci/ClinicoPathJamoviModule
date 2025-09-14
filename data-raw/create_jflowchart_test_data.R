# Create comprehensive test data for flowchart function
# This script generates various datasets for testing the flowchart function

library(dplyr)

# Set seed for reproducibility
set.seed(123)

# 1. Basic Clinical Trial Flow Data
clinical_trial_flow <- data.frame(
  step1 = rep("Assessed for Eligibility", 1),
  count1 = 1500,
  step2 = rep("Randomized", 1),
  count2 = 1200,
  step3 = rep("Allocated to Treatment", 1),
  count3 = 600,
  step4 = rep("Allocated to Control", 1),
  count4 = 600,
  step5 = rep("Completed Study", 1),
  count5 = 1050,
  stringsAsFactors = FALSE
)

# 2. Cancer Treatment Pathway Data
cancer_pathway_flow <- data.frame(
  node1 = rep("Initial Diagnosis", 1),
  n1 = 800,
  node2 = rep("Staging Complete", 1),
  n2 = 750,
  node3 = rep("Treatment Started", 1),
  n3 = 720,
  node4 = rep("First Response Assessment", 1),
  n4 = 680,
  node5 = rep("Completed Protocol", 1),
  n5 = 620,
  node6 = rep("Long-term Follow-up", 1),
  n6 = 580,
  stringsAsFactors = FALSE
)

# 3. Biomarker Discovery Study Flow
biomarker_discovery_flow <- data.frame(
  stage_label = c("Sample Collection", "Quality Control", "Biomarker Testing", 
                  "Statistical Analysis", "Validation Cohort", "Final Results"),
  participant_count = c(2500, 2350, 2280, 2200, 1850, 1800),
  stringsAsFactors = FALSE
)

# 4. Multi-arm Clinical Trial Flow
multiarm_trial_flow <- data.frame(
  phase1 = rep("Screened Patients", 1),
  phase1_n = 2000,
  phase2 = rep("Eligible Patients", 1),
  phase2_n = 1600,
  phase3 = rep("Randomized", 1),
  phase3_n = 1500,
  phase4 = rep("Arm A (Low Dose)", 1),
  phase4_n = 375,
  phase5 = rep("Arm B (High Dose)", 1),
  phase5_n = 375,
  phase6 = rep("Arm C (Combination)", 1),
  phase6_n = 375,
  phase7 = rep("Arm D (Control)", 1),
  phase7_n = 375,
  stringsAsFactors = FALSE
)

# 5. Diagnostic Test Validation Flow
diagnostic_validation_flow <- data.frame(
  step_description = c("Suspected Cases", "Initial Screening", "Confirmatory Test",
                       "Expert Review", "Final Diagnosis", "Quality Assurance"),
  case_numbers = c(5000, 4200, 3800, 3600, 3500, 3450),
  stringsAsFactors = FALSE
)

# 6. Epidemiological Study Flow
epidemiology_study_flow <- data.frame(
  cohort_stage = c("Population Identified", "Contacted for Participation", 
                   "Baseline Assessment", "Follow-up Year 1", 
                   "Follow-up Year 3", "Follow-up Year 5", "Final Analysis"),
  cohort_size = c(50000, 35000, 28000, 26500, 24000, 22000, 21500),
  stringsAsFactors = FALSE
)

# 7. Laboratory Workflow Data
lab_workflow_flow <- data.frame(
  process_step = c("Sample Received", "Initial Processing", "Quality Check",
                   "Primary Analysis", "Secondary Analysis", "Results Reported"),
  sample_count = c(1200, 1180, 1150, 1120, 1100, 1095),
  stringsAsFactors = FALSE
)

# 8. Minimal Test Case (Edge case)
minimal_flow <- data.frame(
  node_a = "Start",
  count_a = 100,
  node_b = "End",
  count_b = 80,
  stringsAsFactors = FALSE
)

# 9. Complex Research Pipeline
research_pipeline_flow <- data.frame(
  pipeline_step = c("Literature Review", "Study Design", "Ethics Approval",
                    "Recruitment", "Data Collection", "Data Cleaning",
                    "Primary Analysis", "Secondary Analysis", "Manuscript Preparation"),
  milestone_count = c(150, 120, 100, 95, 90, 88, 85, 82, 80),
  stringsAsFactors = FALSE
)

# 10. Patient Journey Flow (Real-world Evidence)
patient_journey_flow <- data.frame(
  journey_point = c("First Hospital Visit", "Diagnostic Workup", "Treatment Decision",
                    "Treatment Initiation", "Response Assessment", "Ongoing Care"),
  patient_numbers = c(3000, 2800, 2600, 2400, 2200, 2000),
  stringsAsFactors = FALSE
)

# Create a comprehensive dataset with multiple flow types
jflowchart_comprehensive_data <- rbind(
  data.frame(
    flow_type = "Clinical Trial",
    step1 = "Assessed for Eligibility", count1 = 1500,
    step2 = "Randomized", count2 = 1200,
    step3 = "Allocated to Treatment", count3 = 600,
    step4 = "Allocated to Control", count4 = 600,
    step5 = "Completed Study", count5 = 1050,
    stringsAsFactors = FALSE
  ),
  data.frame(
    flow_type = "Cancer Pathway",
    step1 = "Initial Diagnosis", count1 = 800,
    step2 = "Staging Complete", count2 = 750,
    step3 = "Treatment Started", count3 = 720,
    step4 = "First Response Assessment", count4 = 680,
    step5 = "Completed Protocol", count5 = 620,
    stringsAsFactors = FALSE
  ),
  data.frame(
    flow_type = "Biomarker Discovery",
    step1 = "Sample Collection", count1 = 2500,
    step2 = "Quality Control", count2 = 2350,
    step3 = "Biomarker Testing", count3 = 2280,
    step4 = "Statistical Analysis", count4 = 2200,
    step5 = "Final Results", count5 = 1800,
    stringsAsFactors = FALSE
  )
)

# Save all datasets
save(clinical_trial_flow, file = "data/clinical_trial_flow.rda")
save(cancer_pathway_flow, file = "data/cancer_pathway_flow.rda") 
save(biomarker_discovery_flow, file = "data/biomarker_discovery_flow.rda")
save(multiarm_trial_flow, file = "data/multiarm_trial_flow.rda")
save(diagnostic_validation_flow, file = "data/diagnostic_validation_flow.rda")
save(epidemiology_study_flow, file = "data/epidemiology_study_flow.rda")
save(lab_workflow_flow, file = "data/lab_workflow_flow.rda")
save(minimal_flow, file = "data/minimal_flow.rda")
save(research_pipeline_flow, file = "data/research_pipeline_flow.rda")
save(patient_journey_flow, file = "data/patient_journey_flow.rda")
save(jflowchart_comprehensive_data, file = "data/jflowchart_comprehensive_data.rda")

# Display summary
cat("=== FLOWCHART TEST DATA CREATED ===\n\n")

cat("1. Clinical Trial Flow:\n")
print(clinical_trial_flow)
cat("\n")

cat("2. Cancer Pathway Flow:\n")
print(cancer_pathway_flow)
cat("\n")

cat("3. Biomarker Discovery Flow:\n")
print(biomarker_discovery_flow)
cat("\n")

cat("4. Minimal Flow (Edge Case):\n")
print(minimal_flow)
cat("\n")

cat("5. Comprehensive Dataset Summary:\n")
print(jflowchart_comprehensive_data)

cat("\n=== DATASETS SAVED SUCCESSFULLY ===\n")
cat("Use these datasets to test the flowchart function with various configurations:\n")
cat("- clinical_trial_flow: Standard 5-step clinical trial\n")
cat("- cancer_pathway_flow: 6-step cancer treatment pathway\n")
cat("- biomarker_discovery_flow: Research pipeline with variable naming\n")
cat("- multiarm_trial_flow: Complex multi-arm trial design\n")
cat("- diagnostic_validation_flow: Diagnostic test validation process\n")
cat("- epidemiology_study_flow: Large cohort longitudinal study\n")
cat("- lab_workflow_flow: Laboratory process workflow\n")
cat("- minimal_flow: Minimal 2-step flow for edge case testing\n")
cat("- research_pipeline_flow: 9-step research pipeline\n")
cat("- patient_journey_flow: Real-world patient journey\n")
cat("- jflowchart_comprehensive_data: Combined dataset for multiple flow types\n")