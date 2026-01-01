files <- c(
# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

files <- c(
  "data/BreastCancer.rda", "data/arcDiagram.rda", "data/bayesdca_test_data.rda",
  "data/breast_cancer_data.rda", "data/cancer_biomarker_data.rda", "data/cardiac_troponin_data.rda",
  "data/colon.rda", "data/combined_data.rda", "data/covid_screening_data.rda",
  "data/data_longitudinal.rda", "data/data_percentage.rda", "data/data_raw.rda",
  "data/data_sets.rda", "data/data_subgroup.rda", "data/date_formats.rda",
  "data/dca_test.rda", "data/dca_test_data.RData", "data/dca_test_data.rda",
  "data/decision_panel_test_data.RData", "data/histopathology.rda",
  "data/histopathologyDescriptives.rda", "data/hospital_admission_hourly.rda",
  "data/ihc_test_data.rda", "data/medical_research_data.rda", "data/melanoma.rda",
  "data/mi_ruleout_data.rda", "data/modelbuilder_test_data.RData", "data/modelbuilder_test_data.rda",
  "data/nogold_standard.rda", "data/oncology_data.rda", "data/oncology_response_data.RData",
  "data/oncology_response_data.rda", "data/patientTimelines.rda", "data/patientTimelinesDates.rda",
  "data/patient_treatment_long.rda", "data/patient_treatment_wide.rda", "data/percent_no_time.rda",
  "data/percent_with_time.rda", "data/raw_data.rda", "data/raw_with_time.rda",
  "data/roc_analysis_test_data.RData", "data/rocdata.rda", "data/sepsis_biomarker_data.rda",
  "data/stage_migration_test_data.rda", "data/swimmer_data.rda", "data/swimmer_data_date_formats.rda",
  "data/swimmer_data_raw.rda", "data/swimmer_plot_base_data.rda", "data/swimmer_plot_milestone_data.rda",
  "data/tb_diagnosis_data.rda", "data/thyroid_function_data.rda", "data/thyroid_nodule_data.rda",
  "data/treatmentResponse.rda", "data/treatment_pathways.rda", "data/tumor_response_data.rda",
  "data/tumor_response_examples.rda"
)

for (file_path in files) {
  e <- new.env()
  load(file_path, envir = e)
  cat("File:", file_path, "Objects:", ls(e), "\n")
}
