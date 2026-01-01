#' @title Histopathology Data
#'
#' @description A simulated dataset for histopathological research, containing patient demographics,
#' clinical findings, and hypothetical molecular data. This dataset is intended for
#' demonstration and testing of analysis functions.
#' @usage data(histopathology)
#' @format A data frame with 250 rows and 38 variables:
#' \describe{
#'   \item{ID}{Numeric. Unique patient identifier.}
#'   \item{Name}{Character. Patient's name (simulated).}
#'   \item{Sex}{Character. Patient's sex (e.g., "Male", "Female").}
#'   \item{Age}{Numeric. Patient's age in years.}
#'   \item{Race}{Character. Patient's race (e.g., "White", "Black").}
#'   \item{PreinvasiveComponent}{Character. Presence of preinvasive component (e.g., "Present", "Absent").}
#'   \item{LVI}{Character. Lymphovascular invasion status (e.g., "Present", "Absent").}
#'   \item{PNI}{Character. Perineural invasion status (e.g., "Present", "Absent").}
#'   \item{LastFollowUpDate}{POSIXct. Date and time of the last follow-up.}
#'   \item{Death}{Character. Death status (e.g., "YANLIŞ" for False/No, "DOĞRU" for True/Yes). Indicates if the patient died.}
#'   \item{Group}{Character. Treatment or control group (e.g., "Control", "Treatment").}
#'   \item{Grade}{Numeric. Tumor grade (e.g., 1, 2, 3).}
#'   \item{TStage}{Numeric. Tumor stage (e.g., 1, 2, 3, 4).}
#'   \item{`Anti-X-intensity`}{Numeric. Intensity of Anti-X marker staining.}
#'   \item{`Anti-Y-intensity`}{Numeric. Intensity of Anti-Y marker staining.}
#'   \item{LymphNodeMetastasis}{Character. Lymph node metastasis status (e.g., "Present", "Absent").}
#'   \item{Valid}{Character. Validity status (e.g., "YANLIŞ", "DOĞRU"). Meaning needs context.}
#'   \item{Smoker}{Character. Smoking status (e.g., "YANLIŞ", "DOĞRU").}
#'   \item{Grade_Level}{Character. Tumor grade categorized (e.g., "low", "high", "moderate").}
#'   \item{SurgeryDate}{POSIXct. Date and time of surgery.}
#'   \item{DeathTime}{Character. Time to death category (e.g., "Within1Year").}
#'   \item{int}{Character. Interval string, likely representing the duration between two dates.}
#'   \item{OverallTime}{Numeric. Overall time, likely survival time in some unit (e.g., months).}
#'   \item{Outcome}{Numeric. Outcome variable, likely coded (e.g., 0 for alive, 1 for deceased).}
#'   \item{Mortality5yr}{Character. 5-year mortality status (e.g., "Alive", "Dead").}
#'   \item{`Rater 1`}{Numeric. Rating from Rater 1.}
#'   \item{`Rater 2`}{Numeric. Rating from Rater 2.}
#'   \item{`Rater 3`}{Numeric. Rating from Rater 3.}
#'   \item{`Rater A`}{Numeric. Rating from Rater A.}
#'   \item{`Rater B`}{Numeric. Rating from Rater B.}
#'   \item{`New Test`}{Numeric. Result of a new diagnostic test.}
#'   \item{`Golden Standart`}{Numeric. Result of a gold standard diagnostic test.}
#'   \item{MeasurementA}{Numeric. A continuous measurement.}
#'   \item{MeasurementB}{Numeric. Another continuous measurement.}
#'   \item{`Disease Status`}{Character. Disease status (e.g., "Ill", "Healthy").}
#'   \item{Measurement1}{Numeric. Measurement from a test or observation.}
#'   \item{Measurement2}{Numeric. Another measurement from a test or observation.}
#'   \item{Outcome2}{Character. A secondary outcome measure with levels like "DOD" (Dead of Disease), "DOOC" (Dead of Other Cause), "AWD" (Alive with Disease), "AWOD" (Alive Without Disease).}
#' }
#' @examples
#' data(histopathology)
#' str(histopathology)
# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

"histopathology"

#' @title No Gold Standard Test Data
#'
#' @description A simulated dataset for testing diagnostic test analysis without a gold standard.
#' Contains results from 5 diagnostic tests performed on 300 patients, where the true disease
#' status is unknown (as would be the case in real-world scenarios without a gold standard).
#' 
#' @usage data(nogoldstandard_test_data)
#' @format A data frame with 300 rows and 8 variables:
#' \describe{
#'   \item{patient_id}{Numeric. Unique patient identifier.}
#'   \item{test1_result}{Factor. Result of diagnostic test 1 with levels "negative", "positive".
#'   Simulated with 90% sensitivity and 85% specificity.}
#'   \item{test2_result}{Factor. Result of diagnostic test 2 with levels "negative", "positive".
#'   Simulated with 75% sensitivity and 95% specificity.}
#'   \item{test3_result}{Factor. Result of diagnostic test 3 with levels "negative", "positive".
#'   Simulated with 85% sensitivity and 85% specificity. Contains ~5% missing values.}
#'   \item{test4_result}{Factor. Result of diagnostic test 4 with levels "negative", "positive".
#'   Simulated with 70% sensitivity and 98% specificity. Contains ~5% missing values.}
#'   \item{test5_result}{Factor. Result of diagnostic test 5 with levels "negative", "positive".
#'   Simulated with 95% sensitivity and 70% specificity. Contains ~5% missing values.}
#'   \item{age}{Numeric. Patient's age in years (mean 50, SD 15).}
#'   \item{sex}{Factor. Patient's sex with levels "Male", "Female".}
#' }
#' @details
#' This dataset was simulated with a true disease prevalence of approximately 30%, though
#' this information would not be known in practice. The tests have varying performance
#' characteristics to demonstrate the nogoldstandard analysis methods.
#' 
#' Test characteristics (used in simulation, unknown in practice):
#' - Test 1: High sensitivity (90%), moderate specificity (85%)
#' - Test 2: Moderate sensitivity (75%), high specificity (95%)
#' - Test 3: Balanced (85% sensitivity, 85% specificity)
#' - Test 4: Low sensitivity (70%), very high specificity (98%)
#' - Test 5: High sensitivity (95%), low specificity (70%)
#' 
#' @examples
#' data(nogoldstandard_test_data)
#' str(nogoldstandard_test_data)
#' table(nogoldstandard_test_data$test1_result, nogoldstandard_test_data$test2_result)
"nogoldstandard_test_data"

#' @title No Gold Standard Test Data (Small)
#'
#' @description A smaller subset of the nogoldstandard_test_data for quick testing and examples.
#' Contains the first 50 patients from the full dataset.
#' 
#' @usage data(nogoldstandard_test_data_small)
#' @format A data frame with 50 rows and 8 variables (same structure as nogoldstandard_test_data).
#' @seealso \code{\link{nogoldstandard_test_data}} for the full dataset.
#' @examples
#' data(nogoldstandard_test_data_small)
#' str(nogoldstandard_test_data_small)

#' @title CONSORT Flow Chart Examples
#'
#' @description A dataset containing example CONSORT flow chart parameters for different types of clinical trials.
#' This dataset provides realistic numbers for creating CONSORT flowcharts and demonstrates
#' typical scenarios in clinical research.
#' @usage data(consort_examples_data)
#' @format A data frame with 5 rows and 18 variables:
#' \describe{
#'   \item{trial_name}{Character. Name of the clinical trial.}
#'   \item{trial_type}{Character. Type of trial (e.g., "Phase III RCT", "Pilot RCT").}
#'   \item{initial_assessed}{Numeric. Total number of participants initially assessed for eligibility.}
#'   \item{not_eligible}{Numeric. Number of participants who were not eligible.}
#'   \item{not_eligible_reasons}{Character. Detailed reasons for exclusion during screening.}
#'   \item{randomized}{Numeric. Total number of participants randomized to treatment arms.}
#'   \item{arm1_label}{Character. Label for the first treatment arm.}
#'   \item{arm1_allocated}{Numeric. Number of participants allocated to arm 1.}
#'   \item{arm1_received}{Numeric. Number of participants who received arm 1 intervention.}
#'   \item{arm1_lost_followup}{Numeric. Number of participants lost to follow-up in arm 1.}
#'   \item{arm1_analyzed}{Numeric. Number of participants analyzed in arm 1.}
#'   \item{arm2_label}{Character. Label for the second treatment arm.}
#'   \item{arm2_allocated}{Numeric. Number of participants allocated to arm 2.}
#'   \item{arm2_received}{Numeric. Number of participants who received arm 2 intervention.}
#'   \item{arm2_lost_followup}{Numeric. Number of participants lost to follow-up in arm 2.}
#'   \item{arm2_analyzed}{Numeric. Number of participants analyzed in arm 2.}
#'   \item{exclusion_reasons}{Character. Reasons for post-randomization exclusions.}
#'   \item{study_id}{Character. Unique study identifier.}
#'   \item{duration_months}{Numeric. Study duration in months.}
#'   \item{primary_endpoint}{Character. Primary endpoint of the study.}
#'   \item{study_population}{Character. Description of the study population.}
#' }
#' @source Simulated data based on typical clinical trial designs and CONSORT reporting standards.
#' @references 
#' Schulz KF, Altman DG, Moher D, for the CONSORT Group (2010). 
#' CONSORT 2010 Statement: updated guidelines for reporting parallel group randomised trials. 
#' BMJ 2010;340:c332.
#' @examples
#' data(consort_examples_data)
#' str(consort_examples_data)
#' 
#' # Example usage with consort function:
#' # Large cardiovascular trial example
#' cv_trial <- consort_examples_data[1, ]
#' 
#' \dontrun{
#' consort(
#'   data = data.frame(),
#'   initialN = cv_trial$initial_assessed,
#'   notEligibleN = cv_trial$not_eligible,
#'   notEligibleText = cv_trial$not_eligible_reasons,
#'   randomizedN = cv_trial$randomized,
#'   arm1Label = cv_trial$arm1_label,
#'   arm1N = cv_trial$arm1_allocated,
#'   arm1ReceivedN = cv_trial$arm1_received,
#'   arm1LostN = cv_trial$arm1_lost_followup,
#'   arm1AnalyzedN = cv_trial$arm1_analyzed,
#'   arm2Label = cv_trial$arm2_label,
#'   arm2N = cv_trial$arm2_allocated,
#'   arm2ReceivedN = cv_trial$arm2_received,
#'   arm2LostN = cv_trial$arm2_lost_followup,
#'   arm2AnalyzedN = cv_trial$arm2_analyzed,
#'   excludedText = cv_trial$exclusion_reasons
#' )
#' }
#' head(histopathology)
#' summary(histopathology$Age)
#' hist(histopathology$TStage)
"histopathology"

#' Patient Timeline Data for Clinical Oncology
#'
#' A dataset containing simulated patient timeline data for clinical oncology research.
#' This dataset represents typical patient journeys through cancer treatment,
#' including diagnosis, treatment, response assessment, and outcomes. The data
#' simulates a realistic clinical study where patients enroll at different timepoints
#' and have varied treatment durations and outcomes.
#'
#' @format A data frame with 30 rows and 15 variables:
#' \describe{
#'   \item{PatientID}{Patient identifier, formatted as PT001, PT002, etc.}
#'   \item{StartTime}{Numeric. Time at which observation began, varies between patients to
#'                    represent staggered enrollment (some values greater than 0)}
#'   \item{EndTime}{Numeric. Time at which observation ended, in the same units as StartTime}
#'   \item{BestResponse}{Factor with levels: CR (Complete Response), PR (Partial Response),
#'                       SD (Stable Disease), PD (Progressive Disease), NE (Not Evaluable)}
#'   \item{Surgery}{Numeric. Time of surgery relative to study start. May be negative
#'                  for surgeries that occurred before enrollment}
#'   \item{TreatmentStart}{Numeric. Time when treatment began relative to study start}
#'   \item{ResponseAssessment}{Numeric. Time of response assessment relative to study start}
#'   \item{Progression}{Numeric. Time of disease progression, NA if no progression}
#'   \item{Death}{Numeric. Time of death, NA if patient alive at last follow-up}
#'   \item{Risk}{Factor with levels: High, Medium, Low. Risk classification for the patient}
#'   \item{Age}{Numeric. Patient age in years}
#'   \item{ECOG}{Integer (0-3). ECOG performance status}
#'   \item{ResponseDuration}{Numeric. Duration of response, calculated as Progression - ResponseAssessment}
#'   \item{FollowUpDuration}{Numeric. Total duration of follow-up, from StartTime to EndTime}
#' }
#'
#' @note There is also a date-based version of this dataset where time variables are represented
#'       as actual dates instead of numeric values, named 'patientTimelinesDates'.
#'
#' @examples
#' data(patientTimelines)
#'
#' # Show staggered entry of patients into the study
#' hist(patientTimelines$StartTime,
#'      main = "Patient Enrollment Times",
#'      xlab = "Start Time")
#'
#' # Basic swimmer plot
#' swimmerplot(
#'   data = patientTimelines,
#'   patientID = "PatientID",
#'   start = "StartTime",
#'   end = "EndTime",
#'   event = "BestResponse"
#' )
#'
#' # With milestones
#' swimmerplot(
#'   data = patientTimelines,
#'   patientID = "PatientID",
#'   start = "StartTime",
#'   end = "EndTime",
#'   event = "BestResponse",
#'   milestone1Name = "Surgery",
#'   milestone1Date = "Surgery",
#'   milestone2Name = "Treatment",
#'   milestone2Date = "TreatmentStart"
#' )
"patientTimelines"

#' Patient Timeline Data with Dates for Clinical Oncology
#'
#' A dataset containing simulated patient timeline data for clinical oncology research,
#' with time variables represented as actual dates. This dataset represents typical
#' patient journeys through cancer treatment, including diagnosis, treatment,
#' response assessment, and outcomes. The data simulates a realistic clinical study
#' where patients enroll over a 6-month period and have varied treatment durations
#' and outcomes.
#'
#' @format A data frame with 30 rows and 15 variables:
#' \describe{
#'   \item{PatientID}{Patient identifier, formatted as PT001, PT002, etc.}
#'   \item{StartDate}{Date. Date at which observation began, varies across patients
#'                    to represent staggered enrollment}
#'   \item{EndDate}{Date. Date at which observation ended}
#'   \item{BestResponse}{Factor with levels: CR (Complete Response), PR (Partial Response),
#'                       SD (Stable Disease), PD (Progressive Disease), NE (Not Evaluable)}
#'   \item{Surgery}{Date. Date of surgery, may be before the study enrollment date}
#'   \item{TreatmentStart}{Date. Date when treatment began}
#'   \item{ResponseAssessment}{Date. Date of response assessment}
#'   \item{Progression}{Date. Date of disease progression, NA if no progression}
#'   \item{Death}{Date. Date of death, NA if patient alive at last follow-up}
#'   \item{Risk}{Factor with levels: High, Medium, Low. Risk classification for the patient}
#'   \item{Age}{Numeric. Patient age in years}
#'   \item{ECOG}{Integer (0-3). ECOG performance status}
#'   \item{ResponseDuration}{Numeric. Duration of response in days, calculated as
#'                          Progression - ResponseAssessment}
#'   \item{FollowUpDuration}{Numeric. Total duration of follow-up in days, from
#'                          StartDate to EndDate}
#' }
#'
#' @note This is the date-based version of the patientTimelines dataset.
#'
#' @examples
#' data(patientTimelinesDates)
#'
#' # Show staggered entry of patients into the study
#' hist(as.numeric(patientTimelinesDates$StartDate - min(patientTimelinesDates$StartDate)),
#'      main = "Patient Enrollment Days from Study Start",
#'      xlab = "Days")
#'
#' # Basic swimmer plot with date data
#' swimmerplot(
#'   data = patientTimelinesDates,
#'   patientID = "PatientID",
#'   start = "StartDate",
#'   end = "EndDate",
#'   event = "BestResponse",
#'   timetype = "datetime",
#'   timetypedata = "ymd",
#'   timetypeoutput = "months"
#' )
#'
#' # With absolute time display to show the actual enrollment pattern
#' swimmerplot(
#'   data = patientTimelinesDates,
#'   patientID = "PatientID",
#'   start = "StartDate",
#'   end = "EndDate",
#'   event = "BestResponse",
#'   timetype = "datetime",
#'   timetypedata = "ymd",
#'   timetypeoutput = "months",
#'   startType = "absolute"
#' )
"patientTimelinesDates"

#' @title Tumor Treatment Response Data
#'
#' @description A simulated dataset representing patient responses to cancer treatment.
#' It includes patient identifiers and a numeric value indicating treatment response,
#' likely as a percentage change in tumor size or a similar metric.
#' The data object loaded will be named 'treatmentResponse'.
#' @usage data(treatmentResponse)
#' @format A data frame with 250 rows and 2 variables:
#' \describe{
#'   \item{PatientID}{Character. Unique patient identifier (e.g., "PT0001").}
#'   \item{ResponseValue}{Numeric. A numeric value representing the treatment response.
#'                        This could be a percentage change from baseline, where negative
#'                        values typically indicate tumor shrinkage and positive values
#'                        indicate tumor growth.}
#' }
#' @examples
#' data(treatmentResponse)
#' str(treatmentResponse)
#' head(treatmentResponse)
#' summary(treatmentResponse$ResponseValue)
#' hist(treatmentResponse$ResponseValue, main="Histogram of Treatment Response")
"treatmentResponse"

#' @title Wisconsin Breast Cancer Data
#'
#' @description A dataset containing clinical measurements related to breast cancer cells,
#' originally from the UCI Machine Learning Repository. It is often used for classification tasks.
#' @usage data(BreastCancer)
#' @format A data frame with 699 rows and 11 variables:
#' \describe{
#'   \item{Id}{Numeric. Sample code number.}
#'   \item{Cl.thickness}{Numeric. Clump thickness (1-10).}
#'   \item{Cell.size}{Numeric. Uniformity of cell size (1-10).}
#'   \item{Cell.shape}{Numeric. Uniformity of cell shape (1-10).}
#'   \item{Marg.adhesion}{Numeric. Marginal adhesion (1-10).}
#'   \item{Epith.c.size}{Numeric. Single epithelial cell size (1-10).}
#'   \item{Bare.nuclei}{Numeric. Bare nuclei (1-10). Note: contains NAs in original dataset, may be preprocessed here.}
#'   \item{Bl.cromatin}{Numeric. Bland chromatin (1-10).}
#'   \item{Normal.nucleoli}{Numeric. Normal nucleoli (1-10).}
#'   \item{Mitoses}{Numeric. Mitoses (1-10).}
#'   \item{Class}{Character. Class of the tumor ("benign" or "malignant").}
#' }
#' @source Dr. W. H. Wolberg, University of Wisconsin Hospitals
#' @examples
#' data(BreastCancer)
#' str(BreastCancer)
#' head(BreastCancer)
#' summary(BreastCancer$Cl.thickness)
#' table(BreastCancer$Class)
"BreastCancer"

#' @title Arc Diagram Example Data
#'
#' @description A dataset designed for creating arc diagrams, typically used to visualize
#' relationships or flows between entities.
#' @usage data(arcDiagram)
#' @format A data frame with 21 rows and 4 variables:
#' \describe{
#'   \item{source}{Character. The starting node or entity in a relationship.}
#'   \item{target}{Character. The ending node or entity in a relationship.}
#'   \item{weight}{Numeric. The strength or magnitude of the relationship between source and target.}
#'   \item{group}{Character. A grouping variable for the relationships, often used for coloring arcs.}
#' }
#' @examples
#' data(arcDiagram)
#' str(arcDiagram)
#' head(arcDiagram)
#' summary(arcDiagram$weight)
"arcDiagram"

#' @title Bayesian Decision Curve Analysis Test Data
#'
#' @description A dataset for demonstrating Bayesian Decision Curve Analysis (DCA).
#' It typically includes an outcome variable and predictions from one or more models or tests.
#' @usage data(bayesdca_test_data)
#' @format A data frame with 500 rows and 4 variables:
#' \describe{
#'   \item{outcome}{Character. The true outcome status (e.g., presence or absence of a condition).}
#'   \item{model_prediction}{Numeric. Predicted probability or risk score from a statistical model.}
#'   \item{binary_test}{Integer. Results of a binary diagnostic test, likely coded as 0 or 1.}
#'   \item{weak_test}{Integer. Results of another binary diagnostic test, potentially with lower accuracy, likely coded as 0 or 1.}
#' }
#' @examples
#' data(bayesdca_test_data)
#' str(bayesdca_test_data)
#' head(bayesdca_test_data)
#' summary(bayesdca_test_data$model_prediction)
"bayesdca_test_data"

#' @title Breast Cancer Screening Data
#'
#' @description A simulated dataset for breast cancer screening research. It includes
#' patient identifiers, results from various screening modalities, cancer status,
#' and risk factors.
#' @usage data(breast_cancer_data)
#' @format A data frame with 2000 rows and 10 variables:
#' \describe{
#'   \item{patient_id}{Integer. Unique patient identifier.}
#'   \item{clinical_exam}{Character. Result of clinical breast examination (e.g., "Normal", "Abnormal").}
#'   \item{mammography}{Character. Mammography result (e.g., "BIRADS 1", "BIRADS 4").}
#'   \item{ultrasound}{Character. Ultrasound result (e.g., "Normal", "Suspicious").}
#'   \item{mri}{Character. MRI result (e.g., "Normal", "Suspicious").}
#'   \item{cancer_status}{Character. Actual cancer status, the gold standard (e.g., "Cancer", "No Cancer").}
#'   \item{age}{Integer. Patient's age in years.}
#'   \item{family_history}{Character. Family history of breast cancer (e.g., "Yes", "No").}
#'   \item{brca_mutation}{Character. BRCA mutation status (e.g., "Positive", "Negative").}
#'   \item{breast_density}{Character. Breast density category (e.g., "A", "B", "C", "D" or "Fatty", "Scattered", "Heterogeneously Dense", "Extremely Dense").}
#' }
#' @examples
#' data(breast_cancer_data)
#' str(breast_cancer_data)
#' head(breast_cancer_data)
#' summary(breast_cancer_data$age)
#' table(breast_cancer_data$cancer_status, breast_cancer_data$mammography)
"breast_cancer_data"

#' @title Cancer Biomarker Data
#'
#' @description A simulated dataset containing patient demographics and levels of
#' various cancer biomarkers, along with cancer status and stage. Useful for
#' evaluating diagnostic or prognostic performance of biomarkers.
#' @usage data(cancer_biomarker_data)
#' @format A data frame with 500 rows and 11 variables:
#' \describe{
#'   \item{patient_id}{Integer. Unique patient identifier.}
#'   \item{age}{Integer. Patient's age in years.}
#'   \item{age_group}{Character. Age group of the patient (e.g., "<50", "50-70", ">70").}
#'   \item{sex}{Character. Sex of the patient (e.g., "Male", "Female").}
#'   \item{ca125}{Numeric. Level of Cancer Antigen 125 biomarker.}
#'   \item{he4}{Numeric. Level of Human Epididymis Protein 4 biomarker.}
#'   \item{cea}{Numeric. Level of Carcinoembryonic Antigen biomarker.}
#'   \item{ca199}{Numeric. Level of Carbohydrate Antigen 19-9 biomarker.}
#'   \item{roma_score}{Numeric. Risk of Ovarian Malignancy Algorithm score.}
#'   \item{cancer_status}{Character. Diagnosis of cancer (e.g., "Cancer", "Benign").}
#'   \item{stage}{Character. Cancer stage if applicable (e.g., "I", "II", "III", "IV", or "Benign").}
#' }
#' @examples
#' data(cancer_biomarker_data)
#' str(cancer_biomarker_data)
#' head(cancer_biomarker_data)
#' summary(cancer_biomarker_data$ca125)
#' table(cancer_biomarker_data$cancer_status)
"cancer_biomarker_data"

#' @title Cardiac Troponin Data for MI Diagnosis
#'
#' @description A simulated dataset containing patient demographics, risk factors,
#' and serial high-sensitivity cardiac troponin measurements at 0, 3, and 6 hours.
#' Includes myocardial infarction (MI) status for diagnostic research.
#' @usage data(cardiac_troponin_data)
#' @format A data frame with 500 rows and 11 variables:
#' \describe{
#'   \item{patient_id}{Integer. Unique patient identifier.}
#'   \item{age}{Integer. Patient's age in years.}
#'   \item{sex}{Character. Sex of the patient (e.g., "Male", "Female").}
#'   \item{hs_troponin_0h}{Numeric. High-sensitivity cardiac troponin level at baseline (0 hours).}
#'   \item{hs_troponin_3h}{Numeric. High-sensitivity cardiac troponin level at 3 hours.}
#'   \item{hs_troponin_6h}{Numeric. High-sensitivity cardiac troponin level at 6 hours.}
#'   \item{delta_troponin}{Numeric. Change in troponin levels, possibly between 0h and 3h or 0h and 6h.}
#'   \item{diabetes}{Character. Diabetes status (e.g., "Yes", "No").}
#'   \item{hypertension}{Character. Hypertension status (e.g., "Yes", "No").}
#'   \item{smoking}{Character. Smoking status (e.g., "Yes", "No").}
#'   \item{mi_status}{Character. Myocardial infarction status (e.g., "MI", "No MI").}
#' }
#' @examples
#' data(cardiac_troponin_data)
#' str(cardiac_troponin_data)
#' head(cardiac_troponin_data)
#' summary(cardiac_troponin_data$hs_troponin_0h)
#' table(cardiac_troponin_data$mi_status)
"cardiac_troponin_data"

#' @title Colon Cancer Staging and Survival Data
#'
#' @description Data from one of the first successful trials of adjuvant chemotherapy
#' for colon cancer. Contains information on patient demographics, tumor characteristics,
#' treatment, and survival. This dataset is part of the `survival` R package.
#' @usage data(colon)
#' @format A data frame with 1858 rows and 16 variables:
#' \describe{
#'   \item{id}{Numeric. Patient identifier.}
#'   \item{study}{Numeric. Study identifier (always 1 for this dataset).}
#'   \item{rx}{Character. Treatment regimen: "Obs" (Observation), "Lev" (Levamisole), "Lev+5FU" (Levamisole and 5-Fluorouracil).}
#'   \item{sex}{Numeric. Sex: 0=Female, 1=Male.}
#'   \item{age}{Numeric. Age in years.}
#'   \item{obstruct}{Numeric. Obstruction of colon by tumor: 0=No, 1=Yes.}
#'   \item{perfor}{Numeric. Perforation of colon: 0=No, 1=Yes.}
#'   \item{adhere}{Numeric. Adherence to nearby organs: 0=No, 1=Yes.}
#'   \item{nodes}{Numeric. Number of lymph nodes with detectable cancer.}
#'   \item{status}{Numeric. Censoring status: 0=Alive (censored), 1=Dead or Recurrence (event).}
#'   \item{differ}{Numeric. Differentiation of tumor: 1=Well, 2=Moderate, 3=Poor.}
#'   \item{extent}{Numeric. Extent of local spread: 1=Submucosa, 2=Muscle, 3=Serosa, 4=Contiguous structures.}
#'   \item{surg}{Numeric. Time from surgery to registration: 0=Short (<1 month), 1=Long (>=1 month).}
#'   \item{node4}{Numeric. More than 4 positive lymph nodes: 0=No (<=4), 1=Yes (>4).}
#'   \item{time}{Numeric. Days from registration to event (death or recurrence) or censoring.}
#'   \item{etype}{Numeric. Event type: 1=Recurrence, 2=Death.}
#' }
#' @source Original source: Moertel CG et al. (1990). Levamisole and fluorouracil for adjuvant therapy of resected colon carcinoma. New England Journal of Medicine, 322, 352-358. Included in the `survival` R package.
#' @examples
#' data(colon)
#' str(colon)
#' head(colon)
#' summary(colon$age)
#' table(colon$rx, colon$status)
"colon"

#' @title Combined Clinical and Event Data
#'
#' @description A simulated dataset that combines longitudinal patient data,
#' potentially including clinical observations, biomarker measurements, and specific events
#' over time. Suitable for analyses involving time-varying covariates or event history.
#' @usage data(combined_data)
#' @format A data frame with 54 rows and 8 variables:
#' \describe{
#'   \item{patient_id}{Character. Unique patient identifier.}
#'   \item{data_type}{Character. Type of data record (e.g., "Clinical", "Biomarker", "Treatment").}
#'   \item{start_time}{Integer. Start time for the observation or event interval.}
#'   \item{end_time}{Integer. End time for the observation or event interval.}
#'   \item{response_status}{Character. Patient's response status at a given time (e.g., "CR", "PR", "SD", "PD").}
#'   \item{on_study}{Logical. Indicates if the patient was on study at the time of the record (TRUE/FALSE).}
#'   \item{event_time}{Integer. Time at which a specific event occurred.}
#'   \item{event_type}{Character. Type of event recorded (e.g., "Adverse Event", "Progression", "Dose Change").}
#' }
#' @examples
#' data(combined_data)
#' str(combined_data)
#' head(combined_data)
#' table(combined_data$data_type, combined_data$response_status)
"combined_data"

#' @title COVID-19 Screening Data
#'
#' @description A simulated dataset for evaluating COVID-19 screening strategies.
#' It includes patient identifiers, results from various screening tests (rapid antigen,
#' PCR, chest CT), symptom scores, actual COVID-19 status, and demographic/risk information.
#' @usage data(covid_screening_data)
#' @format A data frame with 1000 rows and 8 variables:
#' \describe{
#'   \item{patient_id}{Integer. Unique patient identifier.}
#'   \item{rapid_antigen}{Character. Result of rapid antigen test (e.g., "Positive", "Negative").}
#'   \item{pcr}{Character. Result of PCR test (e.g., "Positive", "Negative").}
#'   \item{chest_ct}{Character. Result of chest CT scan (e.g., "Normal", "Abnormal", "Not Performed").}
#'   \item{symptom_score}{Integer. Clinical symptom score, potentially based on a standardized checklist.}
#'   \item{covid_status}{Character. Actual COVID-19 status, confirmed by a gold standard (e.g., "Positive", "Negative").}
#'   \item{age}{Integer. Patient's age in years.}
#'   \item{risk_group}{Character. Patient's risk group (e.g., "High", "Medium", "Low").}
#' }
#' @examples
#' data(covid_screening_data)
#' str(covid_screening_data)
#' head(covid_screening_data)
#' table(covid_screening_data$covid_status, covid_screening_data$rapid_antigen)
"covid_screening_data"

#' @title Longitudinal Measurement Data
#'
#' @description A sample dataset representing longitudinal measurements for multiple patients
#' over several time points.
#' @usage data(data_longitudinal)
#' @format A data frame with 140 rows and 3 variables:
#' \describe{
#'   \item{PatientID}{Character. Unique identifier for each patient.}
#'   \item{Time}{Integer. The time point at which the measurement was taken (e.g., day, week, month).}
#'   \item{Measurement}{Numeric. The value of the measurement recorded at the given time point for the patient.}
#' }
#' @examples
#' data(data_longitudinal)
#' str(data_longitudinal)
#' head(data_longitudinal)
#' summary(data_longitudinal$Measurement)
#' # Plot measurements for a specific patient (if plotting package loaded)
#' # if (requireNamespace("ggplot2", quietly = TRUE) && "PT001" %in% data_longitudinal$PatientID) {
#' #   ggplot2::ggplot(data_longitudinal[data_longitudinal$PatientID == "PT001",],
#' #                   ggplot2::aes(x = Time, y = Measurement)) +
#' #     ggplot2::geom_line() + ggplot2::geom_point() +
#' #     ggplot2::ggtitle("Measurements for Patient PT001")
#' # }
"data_longitudinal"

#' @title Patient Response Percentage Data
#'
#' @description A dataset containing patient identifiers and their corresponding
#' treatment response values, likely expressed as percentages.
#' @usage data(data_percentage)
#' @format A data frame with 10 rows and 2 variables:
#' \describe{
#'   \item{PatientID}{Character. Unique identifier for each patient.}
#'   \item{Response}{Integer. The treatment response value, presumably a percentage (e.g., percent change in tumor size).}
#' }
#' @examples
#' data(data_percentage)
#' str(data_percentage)
#' head(data_percentage)
#' summary(data_percentage$Response)
"data_percentage"

#' @title Raw Measurement Data Over Time
#'
#' @description A dataset containing raw measurement values for patients at different
#' time points. This could represent various clinical or experimental raw readings.
#' @usage data(data_raw)
#' @format A data frame with 15 rows and 3 variables:
#' \describe{
#'   \item{PatientID}{Character. Unique identifier for each patient.}
#'   \item{Time}{Integer. The time point at which the measurement was taken (e.g., day, cycle, visit number).}
#'   \item{Measurement}{Integer. The raw value of the measurement recorded.}
#' }
#' @examples
#' data(data_raw)
#' str(data_raw)
#' head(data_raw)
#' summary(data_raw$Measurement)
"data_raw"

#' @title Subgroup Analysis Data
#'
#' @description A dataset containing patient responses, potentially to a treatment,
#' along with a grouping variable for subgroup analysis.
#' @usage data(data_subgroup)
#' @format A data frame with 12 rows and 3 variables:
#' \describe{
#'   \item{PatientID}{Character. Unique identifier for each patient.}
#'   \item{Response}{Integer. The response value recorded for the patient.}
#'   \item{Group}{Character. The subgroup to which the patient belongs (e.g., "GroupA", "GroupB").}
#' }
#' @examples
#' data(data_subgroup)
#' str(data_subgroup)
#' head(data_subgroup)
#' table(data_subgroup$Group)
#' summary(data_subgroup$Response)
"data_subgroup"

#' @title Date Format Handling Example Data
#'
#' @description A dataset showcasing various date formats and patient-related information.
#' It appears designed to test or demonstrate date parsing and handling capabilities,
#' as well as general data manipulation.
#' @usage data(date_formats)
#' @format A data frame with 40 rows and 15 variables:
#' \describe{
#'   \item{PatientID}{Character. Unique patient identifier.}
#'   \item{StartDate}{Date. A start date, likely for a treatment or observation period.}
#'   \item{Duration}{Numeric. Duration, possibly in days or months, related to an event or period.}
#'   \item{Age}{Numeric. Patient's age.}
#'   \item{Weight}{Numeric. Patient's weight.}
#'   \item{ECOG}{Integer. ECOG (Eastern Cooperative Oncology Group) performance status score.}
#'   \item{PriorTherapy}{Character. Information about prior therapies (e.g., "Yes", "No", type of therapy).}
#'   \item{EndDate}{Date. An end date, corresponding to the StartDate.}
#'   \item{BestResponse}{Character. Best response to treatment (e.g., "CR", "PR", "SD", "PD").}
#'   \item{StartDate_YMD}{Date. Start date in Year-Month-Day format.}
#'   \item{StartDate_DMY}{Date. Start date in Day-Month-Year format.}
#'   \item{StartDate_MDY}{Date. Start date in Month-Day-Year format.}
#'   \item{EndDate_YMD}{Date. End date in Year-Month-Day format.}
#'   \item{EndDate_DMY}{Date. End date in Day-Month-Year format.}
#'   \item{EndDate_MDY}{Date. End date in Month-Day-Year format.}
#' }
#' @examples
#' data(date_formats)
#' str(date_formats)
#' head(date_formats[, 1:8]) # Show first few columns due to width
#' summary(date_formats$Age)
#' table(date_formats$BestResponse)
"date_formats"

#' @title Decision Curve Analysis (DCA) Test Data
#'
#' @description A dataset designed for performing Decision Curve Analysis (DCA).
#' It includes patient characteristics, an outcome variable (cardiac_event), and
#' predicted probabilities from several different risk models.
#' @usage data(dca_test)
#' @format A data frame with 50 rows and 17 variables:
#' \describe{
#'   \item{patient_id}{Integer. Unique patient identifier.}
#'   \item{age}{Integer. Patient's age in years.}
#'   \item{sex}{Character. Patient's sex.}
#'   \item{diabetes}{Character. Diabetes status (e.g., "Yes", "No").}
#'   \item{hypertension}{Character. Hypertension status (e.g., "Yes", "No").}
#'   \item{smoking}{Character. Smoking status (e.g., "Yes", "No").}
#'   \item{cholesterol}{Integer. Cholesterol level.}
#'   \item{troponin}{Numeric. Troponin level.}
#'   \item{creatinine}{Numeric. Creatinine level.}
#'   \item{cardiac_event}{Character. The outcome variable, indicating if a cardiac event occurred.}
#'   \item{basic_model}{Numeric. Predicted probability of a cardiac event from a basic model.}
#'   \item{enhanced_model}{Numeric. Predicted probability from an enhanced model.}
#'   \item{biomarker_model}{Numeric. Predicted probability from a model including a biomarker.}
#'   \item{miscalibrated_model}{Numeric. Predicted probability from a deliberately miscalibrated model.}
#'   \item{poor_model}{Numeric. Predicted probability from a poorly performing model.}
#'   \item{risk_category}{Character. A categorized risk based on some criteria.}
#'   \item{hospital}{Character. Hospital identifier or group.}
#' }
#' @examples
#' data(dca_test)
#' str(dca_test)
#' head(dca_test)
#' summary(dca_test$basic_model)
#' table(dca_test$cardiac_event)
"dca_test"

#' @title Comprehensive Decision Curve Analysis (DCA) Data
#'
#' @description A larger and more comprehensive dataset for Decision Curve Analysis,
#' including patient demographics, clinical risk factors, multiple risk model predictions,
#' and outcome variables in both numeric and character formats.
#' @usage data(dca_test_data)
#' @format A data frame with 800 rows and 19 variables:
#' \describe{
#'   \item{patient_id}{Integer. Unique patient identifier.}
#'   \item{age}{Integer. Patient's age in years.}
#'   \item{sex}{Character. Patient's sex.}
#'   \item{diabetes}{Character. Diabetes status.}
#'   \item{hypertension}{Character. Hypertension status.}
#'   \item{smoking}{Character. Smoking status.}
#'   \item{cholesterol}{Integer. Cholesterol level.}
#'   \item{troponin}{Numeric. Troponin level.}
#'   \item{creatinine}{Numeric. Serum creatinine level.}
#'   \item{cardiac_event_numeric}{Integer. Numeric indicator of cardiac event (e.g., 0 or 1).}
#'   \item{cardiac_event}{Character. Character indicator of cardiac event (e.g., "Yes", "No").}
#'   \item{true_risk}{Numeric. A simulated true underlying risk score for the patient.}
#'   \item{basic_model}{Numeric. Predicted probability from a basic risk model.}
#'   \item{enhanced_model}{Numeric. Predicted probability from an enhanced risk model.}
#'   \item{biomarker_model}{Numeric. Predicted probability from a model incorporating a biomarker.}
#'   \item{miscalibrated_model}{Numeric. Predicted probability from a model designed to be miscalibrated.}
#'   \item{poor_model}{Numeric. Predicted probability from a model with poor discrimination.}
#'   \item{risk_category}{Character. Categorized risk level based on some criteria.}
#'   \item{hospital}{Character. Hospital or center identifier.}
#' }
#' @examples
#' data(dca_test_data)
#' str(dca_test_data)
#' head(dca_test_data)
#' summary(dca_test_data$enhanced_model)
#' table(dca_test_data$hospital, dca_test_data$cardiac_event)
"dca_test_data"

#' @title Hourly Hospital Admission Data
#'
#' @description A dataset simulating hourly hospital admissions, including patient vitals
#' and department information. Useful for time series analysis or operational research.
#' @usage data(hospital_admission_hourly)
#' @format A data frame with 1200 rows and 9 variables:
#' \describe{
#'   \item{PatientID}{Character. Unique patient identifier.}
#'   \item{AdmissionDate}{Date. The date of hospital admission.}
#'   \item{Hour}{Integer. The hour of hospital admission (0-23).}
#'   \item{AdmissionTime}{POSIXct. Combined date and time of admission.}
#'   \item{Department}{Character. The hospital department to which the patient was admitted (e.g., "Emergency", "Cardiology").}
#'   \item{HeartRate}{Integer. Patient's heart rate at admission.}
#'   \item{SystolicBP}{Integer. Patient's systolic blood pressure at admission.}
#'   \item{OxygenSaturation}{Integer. Patient's oxygen saturation (SpO2) at admission.}
#'   \item{PainScore}{Integer. Patient's reported pain score at admission (e.g., on a 0-10 scale).}
#' }
#' @examples
#' data(hospital_admission_hourly)
#' str(hospital_admission_hourly)
#' head(hospital_admission_hourly)
#' summary(hospital_admission_hourly$HeartRate)
#' table(hospital_admission_hourly$Department)
"hospital_admission_hourly"

#' @title Immunohistochemistry (IHC) Test Data
#'
#' @description A dataset containing simulated Immunohistochemistry (IHC) marker results
#' for a set of samples. Each marker's expression is likely categorical (e.g., "Positive",
#' "Negative", "Low", "High", or specific scoring).
#' @usage data(ihc_test_data)
#' @format A data frame with 100 rows and 9 variables:
#' \describe{
#'   \item{SampleID}{Character. Unique identifier for each sample.}
#'   \item{ER}{Character. Estrogen Receptor status or score.}
#'   \item{PR}{Character. Progesterone Receptor status or score.}
#'   \item{HER2}{Character. HER2/neu status or score.}
#'   \item{Ki67}{Character. Ki-67 proliferation index, possibly as a category or percentage range.}
#'   \item{p53}{Character. p53 protein expression status or score.}
#'   \item{CD3}{Character. CD3 (T-cell marker) expression status or score.}
#'   \item{CD20}{Character. CD20 (B-cell marker) expression status or score.}
#'   \item{CD45}{Character. CD45 (leukocyte common antigen) expression status or score.}
#' }
#' @examples
#' data(ihc_test_data)
#' str(ihc_test_data)
#' head(ihc_test_data)
#' table(ihc_test_data$ER)
#' table(ihc_test_data$HER2, ihc_test_data$Ki67)
"ihc_test_data"

#' @title General Medical Research Data
#'
#' @description A comprehensive simulated dataset for general medical research,
#' including patient demographics, visit information, clinical measurements, lab results,
#' and various health scores. This dataset is structured to represent data collected
#' over multiple visits for some patients.
#' @usage data(medical_research_data)
#' @format A data frame with 890 rows and 32 variables:
#' \describe{
#'   \item{PatientID}{Character. Unique patient identifier.}
#'   \item{VisitNumber}{Integer. The sequential number of the patient's visit.}
#'   \item{VisitDate}{Character. Date of the visit. Should ideally be parsed as Date.}
#'   \item{StudyCenter}{Character. Identifier for the study center or hospital.}
#'   \item{AgeGroup}{Character. Categorized age group of the patient.}
#'   \item{Gender}{Character. Gender of the patient.}
#'   \item{TreatmentGroup}{Character. Assigned treatment group (e.g., "Treatment A", "Control").}
#'   \item{DiagnosisPrimary}{Character. Primary diagnosis for the patient.}
#'   \item{DiseaseStage}{Character. Stage of the primary disease.}
#'   \item{ComorbidityCount}{Integer. Number of comorbidities.}
#'   \item{BMICategory}{Character. Body Mass Index category (e.g., "Underweight", "Normal", "Overweight").}
#'   \item{SystolicBP}{Integer. Systolic blood pressure (mmHg).}
#'   \item{DiastolicBP}{Integer. Diastolic blood pressure (mmHg).}
#'   \item{HeartRate}{Integer. Heart rate (beats per minute).}
#'   \item{Temperature}{Numeric. Body temperature (e.g., Celsius or Fahrenheit).}
#'   \item{RespiratoryRate}{Integer. Respiratory rate (breaths per minute).}
#'   \item{OxygenSaturation}{Integer. Oxygen saturation (SpO2, percent).}
#'   \item{HbA1c}{Numeric. Glycated hemoglobin level (%).}
#'   \item{Creatinine}{Numeric. Creatinine level (e.g., mg/dL or µmol/L).}
#'   \item{eGFR}{Integer. Estimated Glomerular Filtration Rate.}
#'   \item{Hemoglobin}{Numeric. Hemoglobin level (e.g., g/dL).}
#'   \item{WBC}{Numeric. White Blood Cell count (e.g., cells/µL).}
#'   \item{Platelets}{Integer. Platelet count (e.g., cells/µL).}
#'   \item{TotalCholesterol}{Integer. Total cholesterol level (e.g., mg/dL).}
#'   \item{LDLCholesterol}{Integer. LDL (low-density lipoprotein) cholesterol level.}
#'   \item{HDLCholesterol}{Integer. HDL (high-density lipoprotein) cholesterol level.}
#'   \item{Triglycerides}{Integer. Triglycerides level.}
#'   \item{PainScore}{Integer. Patient-reported pain score (e.g., 0-10 scale).}
#'   \item{QualityOfLife}{Integer. Quality of Life score from a standardized questionnaire.}
#'   \item{FunctionalStatus}{Integer. Functional status score.}
#'   \item{MedicationAdherence}{Integer. Medication adherence score or percentage.}
#'   \item{ClinicalScore}{Integer. A composite or overall clinical score.}
#' }
#' @examples
#' data(medical_research_data)
#' str(medical_research_data)
#' head(medical_research_data)
#' summary(medical_research_data$AgeGroup) # Example, assuming Age is present, use AgeGroup
#' hist(medical_research_data$SystolicBP)
"medical_research_data"

#' @title Melanoma Survival Data
#'
#' @description Data from a study of survival in patients with malignant melanoma.
#' Patients had their tumors surgically removed between 1962 and 1977.
#' This dataset is part of the `boot` R package, originally from Andersen et al. (1993).
#' @usage data(melanoma)
#' @format A data frame with 205 rows and 7 variables:
#' \describe{
#'   \item{time}{Numeric. Survival time in days since the operation.}
#'   \item{status}{Numeric. Censoring status: 1 indicates death from melanoma, 2 indicates alive at last follow-up (censored), 3 indicates death from other causes (censored).}
#'   \item{sex}{Numeric. Sex of the patient: 1 = Male, 0 = Female.}
#'   \item{age}{Numeric. Age of the patient in years at the time of operation.}
#'   \item{year}{Numeric. Year of the operation.}
#'   \item{thickness}{Numeric. Tumor thickness in millimeters (mm).}
#'   \item{ulcer}{Numeric. Ulceration status of the tumor: 0 = Absent, 1 = Present.}
#' }
#' @source Andersen, P.K., Borgan, O., Gill, R.D. and Keiding, N. (1993) Statistical Models Based on Counting Processes. Springer-Verlag.
#' Data included in the `boot` R package.
#' @examples
#' data(melanoma)
#' str(melanoma)
#' head(melanoma)
#' summary(melanoma$time)
#' table(melanoma$status, melanoma$ulcer)
"melanoma"

#' @title Myocardial Infarction (MI) Rule-Out Data
#'
#' @description A simulated dataset for evaluating diagnostic strategies for ruling out
#' myocardial infarction (MI) in patients presenting with chest pain. It includes
#' patient demographics, risk factors, ECG findings, serial troponin levels, and
#' CT angiography results, along with the final MI status.
#' @usage data(mi_ruleout_data)
#' @format A data frame with 800 rows and 11 variables:
#' \describe{
#'   \item{patient_id}{Integer. Unique patient identifier.}
#'   \item{ecg}{Character. Electrocardiogram (ECG) findings (e.g., "Normal", "Ischemic changes", "Non-specific").}
#'   \item{troponin_initial}{Character. Initial cardiac troponin level category (e.g., "Normal", "Slightly Elevated", "Elevated").}
#'   \item{troponin_3hr}{Character. Cardiac troponin level at 3 hours category (e.g., "Normal", "Slightly Elevated", "Elevated").}
#'   \item{ct_angiography}{Character. Result of CT angiography (e.g., "Negative for CAD", "Positive for CAD", "Not Performed").}
#'   \item{mi_status}{Character. Final diagnosis of myocardial infarction (e.g., "MI Confirmed", "MI Ruled Out").}
#'   \item{age}{Integer. Patient's age in years.}
#'   \item{chest_pain}{Character. Type of chest pain experienced by the patient (e.g., "Typical Angina", "Atypical Angina", "Non-cardiac").}
#'   \item{diabetes}{Character. Diabetes mellitus status (e.g., "Yes", "No").}
#'   \item{smoking}{Character. Current smoking status (e.g., "Current Smoker", "Former Smoker", "Never Smoked").}
#'   \item{prior_cad}{Character. History of prior coronary artery disease (e.g., "Yes", "No").}
#' }
#' @examples
#' data(mi_ruleout_data)
#' str(mi_ruleout_data)
#' head(mi_ruleout_data)
#' table(mi_ruleout_data$mi_status)
#' summary(mi_ruleout_data$age)
"mi_ruleout_data"

#' @title Model Builder Test Data
#'
#' @description A dataset for testing and developing predictive models, particularly
#' for cardiovascular events. It contains patient demographics, clinical risk factors,
#' lab values, and an outcome variable.
#' @usage data(modelbuilder_test_data)
#' @format A data frame with 600 rows and 16 variables:
#' \describe{
#'   \item{patient_id}{Character. Unique patient identifier.}
#'   \item{hospital}{Character. Hospital or study center identifier.}
#'   \item{age}{Integer. Patient's age in years.}
#'   \item{sex}{Character. Patient's sex (e.g., "Male", "Female").}
#'   \item{diabetes}{Character. Diabetes status (e.g., "Yes", "No").}
#'   \item{hypertension}{Character. Hypertension status (e.g., "Yes", "No").}
#'   \item{smoking}{Character. Smoking status (e.g., "Yes", "No", "Former").}
#'   \item{cholesterol}{Integer. Total cholesterol level.}
#'   \item{bmi}{Numeric. Body Mass Index.}
#'   \item{systolic_bp}{Integer. Systolic blood pressure.}
#'   \item{family_history}{Character. Family history of cardiovascular disease (e.g., "Yes", "No").}
#'   \item{troponin}{Numeric. Cardiac troponin level.}
#'   \item{creatinine}{Numeric. Serum creatinine level.}
#'   \item{cardiovascular_event}{Character. Outcome variable indicating if a cardiovascular event occurred (e.g., "Yes", "No").}
#'   \item{true_risk}{Numeric. A simulated true underlying risk score for the patient.}
#'   \item{risk_category}{Character. A pre-calculated risk category based on certain criteria.}
#' }
#' @examples
#' data(modelbuilder_test_data)
#' str(modelbuilder_test_data)
#' head(modelbuilder_test_data)
#' summary(modelbuilder_test_data$bmi)
#' table(modelbuilder_test_data$cardiovascular_event)
"modelbuilder_test_data"

#' @title No Gold Standard Diagnostic Test Data
#'
#' @description A simulated dataset for evaluating diagnostic tests in the absence
#' of a perfect gold standard. It includes results from multiple imperfect tests
#' and, for simulation purposes, a 'true' disease status.
#' @usage data(nogold_standard)
#' @format A data frame with 200 rows and 6 variables:
#' \describe{
#'   \item{caseID}{Integer. Unique identifier for each case or patient.}
#'   \item{disease}{Character. The true underlying disease status (e.g., "Present", "Absent"). In real "no gold standard" scenarios, this would be unknown and estimated.}
#'   \item{test1}{Character. Result of the first diagnostic test (e.g., "pos", "neg", or "Positive", "Negative").}
#'   \item{test2}{Character. Result of the second diagnostic test.}
#'   \item{test3}{Character. Result of the third diagnostic test.}
#'   \item{test4}{Character. Result of the fourth diagnostic test.}
#' }
#' @examples
#' data(nogold_standard)
#' str(nogold_standard)
#' head(nogold_standard)
#' table(nogold_standard$test1, nogold_standard$test2)
"nogold_standard"

#' jcomplexupset Test Data
#'
#' A comprehensive dataset for testing Complex UpSet plot visualizations.
#' Contains binary variables representing treatment modalities, biomarkers,
#' complications, and response indicators for 200 cancer patients.
#'
#' @format A data frame with 200 rows with 23 variables:
#' \describe{
#'   \item{PatientID}{Patient identifier (1-200)}
#'   \item{Age}{Patient age in years (25-85)}
#'   \item{Sex}{Patient sex: "Male", "Female"}
#'   \item{Surgery}{Logical: received surgical treatment}
#'   \item{Chemotherapy}{Logical: received chemotherapy}
#'   \item{Radiotherapy}{Logical: received radiotherapy}
#'   \item{Immunotherapy}{Logical: received immunotherapy}
#'   \item{TargetedTherapy}{Logical: received targeted therapy}
#'   \item{HER2_Positive}{Logical: HER2 positive status}
#'   \item{ER_Positive}{Logical: Estrogen receptor positive status}
#'   \item{PR_Positive}{Logical: Progesterone receptor positive status}
#'   \item{PDL1_Positive}{Logical: PD-L1 positive status}
#'   \item{Infection}{Logical: developed infection complication}
#'   \item{Bleeding}{Logical: developed bleeding complication}
#'   \item{Nausea}{Logical: experienced nausea}
#'   \item{Fatigue}{Logical: experienced fatigue}
#'   \item{Complete_Response}{Logical: achieved complete response}
#'   \item{Partial_Response}{Logical: achieved partial response}
#'   \item{Stable_Disease}{Logical: had stable disease}
#'   \item{Progressive_Disease}{Logical: had progressive disease}
#'   \item{TumorSize}{Numeric: tumor size in centimeters}
#'   \item{SurvivalMonths}{Numeric: survival time in months}
#'   \item{TreatmentCost}{Numeric: treatment cost in dollars}
#' }
#' @source Simulated data for testing jcomplexupset function
#' @examples
#' data(jcomplexupset_test_data)
#' # Analyze treatment combinations
#' treatment_vars <- c("Surgery", "Chemotherapy", "Radiotherapy", "Immunotherapy")
#' summary(jcomplexupset_test_data[, treatment_vars])
"jcomplexupset_test_data"

#' Molecular Subtype Data
#'
#' A dataset containing molecular markers and pathway alterations for 150 
#' cancer samples, suitable for analyzing mutation patterns and pathway 
#' disruptions using UpSet plots.
#'
#' @format A data frame with 150 rows and 15 variables:
#' \describe{
#'   \item{SampleID}{Sample identifier (1-150)}
#'   \item{BRCA1_Mutation}{Logical: BRCA1 mutation present}
#'   \item{BRCA2_Mutation}{Logical: BRCA2 mutation present}
#'   \item{TP53_Mutation}{Logical: TP53 mutation present}
#'   \item{PIK3CA_Mutation}{Logical: PIK3CA mutation present}
#'   \item{KRAS_Mutation}{Logical: KRAS mutation present}
#'   \item{EGFR_Mutation}{Logical: EGFR mutation present}
#'   \item{PI3K_Pathway}{Logical: PI3K pathway altered}
#'   \item{WNT_Pathway}{Logical: WNT pathway altered}
#'   \item{RB_Pathway}{Logical: RB pathway altered}
#'   \item{DNA_Repair}{Logical: DNA repair pathway altered}
#'   \item{Grade}{Numeric: tumor grade (1-3)}
#'   \item{Stage}{Numeric: tumor stage (1-4)}
#'   \item{Age_Group}{Character: "Young", "Middle", "Elderly"}
#' }
#' @source Simulated molecular profiling data
#' @examples
#' data(molecular_subtype_data)
#' # Analyze mutation patterns
#' mutation_vars <- c("BRCA1_Mutation", "BRCA2_Mutation", "TP53_Mutation", "PIK3CA_Mutation")
#' table(rowSums(molecular_subtype_data[, mutation_vars]))
"molecular_subtype_data"

#' Diagnostic Test Data
#'
#' A dataset representing diagnostic test utilization patterns for 180 
#' clinical cases, including imaging, laboratory tests, and specialist 
#' consultations.
#'
#' @format A data frame with 180 rows and 15 variables:
#' \describe{
#'   \item{CaseID}{Case identifier (1-180)}
#'   \item{CT_Scan}{Logical: CT scan performed}
#'   \item{MRI}{Logical: MRI performed}
#'   \item{PET_Scan}{Logical: PET scan performed}
#'   \item{Ultrasound}{Logical: Ultrasound performed}
#'   \item{Biopsy}{Logical: Biopsy performed}
#'   \item{CBC}{Logical: Complete blood count performed}
#'   \item{Chemistry_Panel}{Logical: Chemistry panel performed}
#'   \item{Tumor_Markers}{Logical: Tumor markers tested}
#'   \item{Genetic_Testing}{Logical: Genetic testing performed}
#'   \item{Oncology}{Logical: Oncology consultation}
#'   \item{Surgery}{Logical: Surgery consultation}
#'   \item{Radiology}{Logical: Radiology consultation}
#'   \item{Pathology}{Logical: Pathology consultation}
#'   \item{Diagnosis_Confirmed}{Logical: Diagnosis confirmed}
#'   \item{Time_to_Diagnosis}{Numeric: Time to diagnosis in days}
#' }
#' @source Simulated diagnostic workflow data
#' @examples
#' data(diagnostic_test_data)
#' # Analyze imaging combinations
#' imaging_vars <- c("CT_Scan", "MRI", "PET_Scan", "Ultrasound")
#' colSums(diagnostic_test_data[, imaging_vars])
"diagnostic_test_data"

#' @title No Gold Standard Test Data
#'
#' @description A simulated dataset for testing diagnostic test analysis without a gold standard.
#' Contains results from 5 diagnostic tests performed on 300 patients, where the true disease
#' status is unknown (as would be the case in real-world scenarios without a gold standard).
#' 
#' @usage data(nogoldstandard_test_data)
#' @format A data frame with 300 rows and 8 variables:
#' \describe{
#'   \item{patient_id}{Numeric. Unique patient identifier.}
#'   \item{test1_result}{Factor. Result of diagnostic test 1 with levels "negative", "positive".
#'   Simulated with 90% sensitivity and 85% specificity.}
#'   \item{test2_result}{Factor. Result of diagnostic test 2 with levels "negative", "positive".
#'   Simulated with 75% sensitivity and 95% specificity.}
#'   \item{test3_result}{Factor. Result of diagnostic test 3 with levels "negative", "positive".
#'   Simulated with 85% sensitivity and 85% specificity. Contains ~5% missing values.}
#'   \item{test4_result}{Factor. Result of diagnostic test 4 with levels "negative", "positive".
#'   Simulated with 70% sensitivity and 98% specificity. Contains ~5% missing values.}
#'   \item{test5_result}{Factor. Result of diagnostic test 5 with levels "negative", "positive".
#'   Simulated with 95% sensitivity and 70% specificity. Contains ~5% missing values.}
#'   \item{age}{Numeric. Patient's age in years (mean 50, SD 15).}
#'   \item{sex}{Factor. Patient's sex with levels "Male", "Female".}
#' }
#' @details
#' This dataset was simulated with a true disease prevalence of approximately 30%, though
#' this information would not be known in practice. The tests have varying performance
#' characteristics to demonstrate the nogoldstandard analysis methods.
#' 
#' Test characteristics (used in simulation, unknown in practice):
#' - Test 1: High sensitivity (90%), moderate specificity (85%)
#' - Test 2: Moderate sensitivity (75%), high specificity (95%)
#' - Test 3: Balanced (85% sensitivity, 85% specificity)
#' - Test 4: Low sensitivity (70%), very high specificity (98%)
#' - Test 5: High sensitivity (95%), low specificity (70%)
#' 
#' @examples
#' data(nogoldstandard_test_data)
#' str(nogoldstandard_test_data)
#' table(nogoldstandard_test_data$test1_result, nogoldstandard_test_data$test2_result)
"nogoldstandard_test_data"

#' @title No Gold Standard Test Data (Small)
#'
#' @description A smaller subset of the nogoldstandard_test_data for quick testing and examples.
#' Contains the first 50 patients from the full dataset.
#' 
#' @usage data(nogoldstandard_test_data_small)
#' @format A data frame with 50 rows and 8 variables (same structure as nogoldstandard_test_data).
#' @seealso \code{\link{nogoldstandard_test_data}} for the full dataset.
#' @examples
#' data(nogoldstandard_test_data_small)
#' str(nogoldstandard_test_data_small)
