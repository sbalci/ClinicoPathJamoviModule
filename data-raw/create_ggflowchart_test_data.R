# Create comprehensive test data for ggflowchart function
# This script generates various datasets for testing the ggflowchart function
# ggflowchart requires 'from' and 'to' node connections for edge-based flowcharts

library(dplyr)

# Set seed for reproducibility
set.seed(456)

# 1. Simple Process Flow Data
simple_process_flow <- data.frame(
  from_node = c("Start", "Process A", "Process B", "Decision", "Process C", "Decision"),
  to_node = c("Process A", "Process B", "Decision", "Process C", "End", "End"),
  process_type = c("Initial", "Analysis", "Analysis", "Decision", "Final", "Final"),
  stringsAsFactors = FALSE
)

# 2. Clinical Trial Decision Flow  
clinical_decision_flow <- data.frame(
  from_step = c("Screening", "Screening", "Eligibility", "Eligibility", 
                "Randomization", "Randomization", "Treatment A", "Treatment B", 
                "Treatment A", "Treatment B"),
  to_step = c("Eligibility", "Excluded", "Randomization", "Not Eligible",
              "Treatment A", "Treatment B", "Follow-up", "Follow-up", 
              "Analysis", "Analysis"),
  decision_type = c("Process", "Exclusion", "Process", "Exclusion", 
                    "Allocation", "Allocation", "Process", "Process",
                    "Final", "Final"),
  stringsAsFactors = FALSE
)

# 3. Research Pipeline Flow
research_pipeline_flow <- data.frame(
  pipeline_from = c("Literature Review", "Study Design", "Ethics Review", 
                    "Data Collection", "Data Cleaning", "Primary Analysis", 
                    "Secondary Analysis", "Manuscript", "Peer Review"),
  pipeline_to = c("Study Design", "Ethics Review", "Data Collection", 
                  "Data Cleaning", "Primary Analysis", "Secondary Analysis", 
                  "Manuscript", "Peer Review", "Publication"),
  phase = c("Planning", "Planning", "Approval", "Execution", "Execution", 
            "Analysis", "Analysis", "Dissemination", "Dissemination"),
  stringsAsFactors = FALSE
)

# 4. Laboratory Workflow
lab_workflow_flow <- data.frame(
  lab_from = c("Sample Receipt", "Initial Processing", "Quality Check", 
               "Primary Test", "Quality Check", "Secondary Test", 
               "Results Review", "Results Review"),
  lab_to = c("Initial Processing", "Quality Check", "Primary Test", 
             "Results Review", "Secondary Test", "Results Review", 
             "Report", "Archive"),
  workflow_type = c("Intake", "Processing", "QC", "Analysis", "QC", 
                    "Analysis", "Output", "Storage"),
  stringsAsFactors = FALSE
)

# 5. Data Analysis Pipeline
data_analysis_pipeline <- data.frame(
  analysis_from = c("Raw Data", "Data Import", "Data Cleaning", "Exploratory Analysis",
                    "Model Building", "Model Validation", "Results Interpretation"),
  analysis_to = c("Data Import", "Data Cleaning", "Exploratory Analysis", "Model Building",
                  "Model Validation", "Results Interpretation", "Report Generation"),
  analysis_phase = c("Input", "Preparation", "Preparation", "Exploration",
                     "Modeling", "Validation", "Output"),
  stringsAsFactors = FALSE
)

# 6. Patient Care Pathway
patient_care_pathway <- data.frame(
  care_from = c("Admission", "Initial Assessment", "Diagnosis", "Treatment Plan",
                "Treatment Implementation", "Monitoring", "Evaluation"),
  care_to = c("Initial Assessment", "Diagnosis", "Treatment Plan", "Treatment Implementation",
              "Monitoring", "Evaluation", "Discharge"),
  care_category = c("Intake", "Assessment", "Planning", "Implementation",
                    "Implementation", "Review", "Outcome"),
  stringsAsFactors = FALSE
)

# 7. Software Development Flow
software_dev_flow <- data.frame(
  dev_from = c("Requirements", "Design", "Development", "Testing", "Testing",
               "Deployment", "Maintenance"),
  dev_to = c("Design", "Development", "Testing", "Bug Fix", "Deployment",
             "Maintenance", "Updates"),
  dev_stage = c("Planning", "Planning", "Implementation", "Quality", "Quality",
                "Production", "Support"),
  stringsAsFactors = FALSE
)

# 8. Minimal Flow (Edge Case)
minimal_ggflow <- data.frame(
  start_node = c("Input", "Process"),
  end_node = c("Process", "Output"),
  simple_type = c("Flow", "Flow"),
  stringsAsFactors = FALSE
)

# 9. Complex Decision Tree
complex_decision_tree <- data.frame(
  decision_from = c("Problem", "Problem", "Option A", "Option A", "Option B", "Option B",
                    "Sub-option A1", "Sub-option A2", "Sub-option B1", "Sub-option B2"),
  decision_to = c("Option A", "Option B", "Sub-option A1", "Sub-option A2", 
                  "Sub-option B1", "Sub-option B2", "Result A1", "Result A2", 
                  "Result B1", "Result B2"),
  decision_level = c("Root", "Root", "Level 1", "Level 1", "Level 1", "Level 1",
                     "Final", "Final", "Final", "Final"),
  stringsAsFactors = FALSE
)

# 10. Manufacturing Process
manufacturing_process <- data.frame(
  process_from = c("Raw Materials", "Mixing", "Processing", "Quality Control", 
                   "Quality Control", "Packaging", "Final Inspection"),
  process_to = c("Mixing", "Processing", "Quality Control", "Packaging", "Rework",
                 "Final Inspection", "Shipping"),
  manufacturing_type = c("Input", "Production", "Production", "QC", "Rework",
                         "Packaging", "Output"),
  stringsAsFactors = FALSE
)

# 11. Multi-group Workflow (for testing grouping functionality)
multi_group_workflow <- data.frame(
  workflow_from = c("Start A", "Process A1", "Start B", "Process B1", "Start C", "Process C1",
                    "Process A1", "Process B1", "Process C1"),
  workflow_to = c("Process A1", "End A", "Process B1", "End B", "Process C1", "End C",
                  "End A", "End B", "End C"),
  group_category = c("Group A", "Group A", "Group B", "Group B", "Group C", "Group C",
                     "Group A", "Group B", "Group C"),
  stringsAsFactors = FALSE
)

# 12. Comprehensive dataset with multiple workflow types
ggflowchart_comprehensive_data <- rbind(
  data.frame(
    workflow_type = "Simple Process",
    from_var = simple_process_flow$from_node,
    to_var = simple_process_flow$to_node,
    group_var = simple_process_flow$process_type,
    stringsAsFactors = FALSE
  ),
  data.frame(
    workflow_type = "Clinical Decision",
    from_var = clinical_decision_flow$from_step,
    to_var = clinical_decision_flow$to_step,
    group_var = clinical_decision_flow$decision_type,
    stringsAsFactors = FALSE
  ),
  data.frame(
    workflow_type = "Research Pipeline",
    from_var = research_pipeline_flow$pipeline_from,
    to_var = research_pipeline_flow$pipeline_to,
    group_var = research_pipeline_flow$phase,
    stringsAsFactors = FALSE
  )
)

# Save all datasets
save(simple_process_flow, file = "data/simple_process_flow.rda")
save(clinical_decision_flow, file = "data/clinical_decision_flow.rda")
save(research_pipeline_flow, file = "data/research_pipeline_flow.rda")
save(lab_workflow_flow, file = "data/lab_workflow_flow.rda")
save(data_analysis_pipeline, file = "data/data_analysis_pipeline.rda")
save(patient_care_pathway, file = "data/patient_care_pathway.rda")
save(software_dev_flow, file = "data/software_dev_flow.rda")
save(minimal_ggflow, file = "data/minimal_ggflow.rda")
save(complex_decision_tree, file = "data/complex_decision_tree.rda")
save(manufacturing_process, file = "data/manufacturing_process.rda")
save(multi_group_workflow, file = "data/multi_group_workflow.rda")
save(ggflowchart_comprehensive_data, file = "data/ggflowchart_comprehensive_data.rda")

# Display summary
cat("=== GGFLOWCHART TEST DATA CREATED ===\n\n")

cat("1. Simple Process Flow:\n")
print(simple_process_flow)
cat("\n")

cat("2. Clinical Decision Flow:\n")
print(clinical_decision_flow)
cat("\n")

cat("3. Research Pipeline Flow:\n")
print(research_pipeline_flow)
cat("\n")

cat("4. Lab Workflow Flow:\n")
print(lab_workflow_flow)
cat("\n")

cat("5. Minimal ggFlow (Edge Case):\n")
print(minimal_ggflow)
cat("\n")

cat("6. Multi-group Workflow:\n")
print(multi_group_workflow)
cat("\n")

cat("7. Comprehensive Dataset Summary:\n")
print(ggflowchart_comprehensive_data)

cat("\n=== DATASETS SAVED SUCCESSFULLY ===\n")
cat("Use these datasets to test the ggflowchart function with various configurations:\n")
cat("- simple_process_flow: Basic workflow with 3 process types\n")
cat("- clinical_decision_flow: Medical decision tree with exclusions\n")
cat("- research_pipeline_flow: Complete research workflow\n")
cat("- lab_workflow_flow: Laboratory processing workflow\n")
cat("- data_analysis_pipeline: Data science workflow\n")
cat("- patient_care_pathway: Healthcare process flow\n")
cat("- software_dev_flow: Software development lifecycle\n")
cat("- minimal_ggflow: Minimal 2-node flow for edge case testing\n")
cat("- complex_decision_tree: Multi-level decision structure\n")
cat("- manufacturing_process: Industrial process workflow\n")
cat("- multi_group_workflow: Multi-category workflow for grouping tests\n")
cat("- ggflowchart_comprehensive_data: Combined dataset for multiple workflow types\n")