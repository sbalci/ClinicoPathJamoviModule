# ═══════════════════════════════════════════════════════════
# Data Documentation: decision function test datasets
# ═══════════════════════════════════════════════════════════

#' @title Test Data for Medical Decision Analysis
#' @name decision_test_datasets
#' @description
#' Test datasets for evaluating diagnostic test performance using the decision function.
#' These datasets cover various clinical scenarios including screening tests, diagnostic
#' tests, biomarkers, imaging, and pathology evaluations with different prevalence rates
#' and test characteristics.
#'
#' @format Data frames with variables for gold standard reference and test results:
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{GoldStandard/Reference}{True disease status (gold standard)}
#'   \item{NewTest/Test}{Test result being evaluated}
#'   \item{Additional clinical variables}{Age, symptoms, risk factors, etc.}
#' }
#'
#' @details
#' All datasets use realistic clinical scenarios with appropriate sensitivity, specificity,
#' and disease prevalence values. Datasets include edge cases (small/large samples,
#' perfect/poor performance, rare/common diseases) for comprehensive testing.
#'
#' @examples
#' # Load basic test dataset
#' data(decision_test)
#' head(decision_test)
#'
#' # Evaluate diagnostic test
#' decision(
#'   data = decision_test,
#'   gold = "GoldStandard",
#'   goldPositive = "Positive",
#'   newtest = "NewTest",
#'   testPositive = "Positive"
#' )
NULL

#' @rdname decision_test_datasets
#' @format decision_test: Basic diagnostic test evaluation (n=200, 30% prevalence)
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{GoldStandard}{True disease status (Negative/Positive)}
#'   \item{NewTest}{Test result (Negative/Positive, Sens=0.85, Spec=0.90)}
#'   \item{age}{Patient age in years}
#'   \item{sex}{Patient sex (Female/Male)}
#' }
"decision_test"

#' @rdname decision_test_datasets
#' @format decision_screening: Cancer screening test with low prevalence (n=250, 5% prevalence)
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{Biopsy}{Gold standard biopsy result (Benign/Malignant)}
#'   \item{ScreeningTest}{Screening test result (Negative/Positive, Sens=0.92, Spec=0.88)}
#'   \item{age}{Patient age in years}
#'   \item{risk_factor}{Cancer risk factor (Low/Medium/High)}
#' }
"decision_screening"

#' @rdname decision_test_datasets
#' @format decision_diagnostic: Clinical diagnostic test with high prevalence (n=180, 60% prevalence)
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{GoldStandard}{True disease status (Absent/Present)}
#'   \item{ClinicalTest}{Clinical test result (Negative/Positive, Sens=0.88, Spec=0.85)}
#'   \item{symptom_severity}{Symptom severity score (1-10)}
#'   \item{duration_days}{Symptom duration in days}
#' }
"decision_diagnostic"

#' @rdname decision_test_datasets
#' @format decision_biomarker: Cardiac biomarker test evaluation (n=220, 35% prevalence)
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{Angiography}{Gold standard angiography (No_MI/MI)}
#'   \item{Troponin}{Troponin test result (Normal/Elevated, Sens=0.95, Spec=0.92)}
#'   \item{chest_pain}{Chest pain severity (None/Mild/Severe)}
#'   \item{ecg_changes}{ECG findings (Normal/Abnormal)}
#' }
"decision_biomarker"

#' @rdname decision_test_datasets
#' @format decision_imaging: Medical imaging test performance (n=160, 40% prevalence)
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{Pathology}{Pathology diagnosis (Benign/Malignant)}
#'   \item{CT_Scan}{CT scan result (Normal/Abnormal, Sens=0.87, Spec=0.83)}
#'   \item{tumor_size_cm}{Tumor size in centimeters}
#'   \item{location}{Tumor location (Lung/Liver/Kidney/Pancreas)}
#' }
"decision_imaging"

#' @rdname decision_test_datasets
#' @format decision_infectious: Infectious disease rapid test (n=200, 25% prevalence)
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{Culture}{Gold standard culture result (Negative/Positive)}
#'   \item{RapidTest}{Rapid test result (Negative/Positive, Sens=0.82, Spec=0.95)}
#'   \item{fever}{Fever present (No/Yes)}
#'   \item{symptom_onset_days}{Days since symptom onset}
#' }
"decision_infectious"

#' @rdname decision_test_datasets
#' @format decision_small: Small sample dataset (n=30, 40% prevalence)
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{GoldStandard}{True disease status (Negative/Positive)}
#'   \item{NewTest}{Test result (Negative/Positive, Sens=0.85, Spec=0.85)}
#' }
"decision_small"

#' @rdname decision_test_datasets
#' @format decision_large: Large sample dataset (n=500, 30% prevalence)
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{GoldStandard}{True disease status (Negative/Positive)}
#'   \item{NewTest}{Test result (Negative/Positive, Sens=0.88, Spec=0.90)}
#'   \item{age}{Patient age in years}
#'   \item{comorbidities}{Number of comorbidities}
#' }
"decision_large"

#' @rdname decision_test_datasets
#' @format decision_perfect: Perfect test performance (n=150, 35% prevalence)
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{GoldStandard}{True disease status (Negative/Positive)}
#'   \item{PerfectTest}{Perfect test result (Sens=1.00, Spec=1.00)}
#'   \item{age}{Patient age in years}
#' }
"decision_perfect"

#' @rdname decision_test_datasets
#' @format decision_poor: Poor test performance (n=140, 30% prevalence)
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{GoldStandard}{True disease status (Negative/Positive)}
#'   \item{PoorTest}{Poor performing test (Sens=0.60, Spec=0.65)}
#' }
"decision_poor"

#' @rdname decision_test_datasets
#' @format decision_rare: Rare disease scenario (n=300, 2% prevalence)
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{GoldStandard}{True disease status (Negative/Positive)}
#'   \item{NewTest}{Test result (Negative/Positive, Sens=0.90, Spec=0.95)}
#'   \item{genetic_marker}{Genetic marker status (Absent/Present)}
#' }
"decision_rare"

#' @rdname decision_test_datasets
#' @format decision_common: Common disease scenario (n=180, 75% prevalence)
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{GoldStandard}{True disease status (Negative/Positive)}
#'   \item{NewTest}{Test result (Negative/Positive, Sens=0.85, Spec=0.88)}
#' }
"decision_common"

#' @rdname decision_test_datasets
#' @format decision_missing: Dataset with missing values (n=150, ~5% missing)
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{GoldStandard}{True disease status with missing values}
#'   \item{NewTest}{Test result with missing values}
#'   \item{age}{Patient age in years}
#' }
"decision_missing"

#' @rdname decision_test_datasets
#' @format decision_multilevel: Three-level variables with indeterminate results (n=180)
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{GoldStandard}{True disease status (Negative/Positive)}
#'   \item{NewTest}{Test result (Negative/Positive/Indeterminate, ~10% indeterminate)}
#' }
"decision_multilevel"

#' @rdname decision_test_datasets
#' @format decision_pathology: Pathology frozen section evaluation (n=190, 45% prevalence)
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{Biopsy}{Permanent section diagnosis (Benign/Malignant)}
#'   \item{FrozenSection}{Frozen section diagnosis (Benign/Malignant, Sens=0.93, Spec=0.96)}
#'   \item{specimen_type}{Specimen type (Core_Biopsy/Excision/FNA)}
#'   \item{tumor_grade}{Tumor grade (Low/Intermediate/High)}
#' }
"decision_pathology"

#' @rdname decision_test_datasets
#' @format decision_pointofcare: Point-of-care test evaluation (n=210, 28% prevalence)
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{LabTest}{Laboratory test result (Negative/Positive)}
#'   \item{PointOfCare}{Point-of-care test result (Negative/Positive, Sens=0.78, Spec=0.92)}
#'   \item{setting}{Testing setting (Emergency/Clinic/Home)}
#'   \item{urgency}{Clinical urgency (Routine/Urgent/Critical)}
#' }
"decision_pointofcare"
