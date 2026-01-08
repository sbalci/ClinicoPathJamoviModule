# ═══════════════════════════════════════════════════════════
# Dataset Documentation: nogoldstandard Test Data
# ═══════════════════════════════════════════════════════════
# Generated: 2026-01-06 | Seed: 42
# Function: Analysis Without Gold Standard

#' nogoldstandard Test Data - Basic Two-Test Analysis
#'
#' @description
#' Basic dataset with 200 patients for analyzing two diagnostic tests without
#' a gold standard reference. Tests have moderate sensitivity (0.85, 0.80) and
#' specificity (0.85, 0.90).
#'
#' @format A data frame with 200 rows and 5 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT001-PT200)}
#'   \item{Test1}{Factor: First test result ("Negative", "Positive"), Sens=0.85, Spec=0.85}
#'   \item{Test2}{Factor: Second test result ("Negative", "Positive"), Sens=0.80, Spec=0.90}
#'   \item{age}{Numeric: Patient age in years (mean 55, SD 12)}
#'   \item{sex}{Factor: "Male" or "Female"}
#' }
#'
#' @details
#' Simulated with latent disease prevalence of 30%. Test characteristics based
#' on realistic diagnostic scenarios. Suitable for demonstrating latent class
#' analysis and composite reference standard methods.
#'
#' @examples
#' data(nogoldstandard_test)
#' nogoldstandard(data = nogoldstandard_test, test1 = "Test1",
#'                test1Positive = "Positive", test2 = "Test2",
#'                test2Positive = "Positive", method = "latent_class")
#'
#' @source Generated test data for ClinicoPath package
"nogoldstandard_test"

#' nogoldstandard Pathology Data - Inter-Pathologist Agreement
#'
#' @description
#' Three-pathologist dataset with 180 patients for assessing diagnostic
#' agreement without a gold standard. Pathologists have varying sensitivity
#' (0.88, 0.85, 0.82) and high specificity (0.92, 0.90, 0.93).
#'
#' @format A data frame with 180 rows and 6 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT001-PT180)}
#'   \item{Pathologist1}{Factor: First pathologist diagnosis ("Benign", "Malignant"), Sens=0.88, Spec=0.92}
#'   \item{Pathologist2}{Factor: Second pathologist diagnosis ("Benign", "Malignant"), Sens=0.85, Spec=0.90}
#'   \item{Pathologist3}{Factor: Third pathologist diagnosis ("Benign", "Malignant"), Sens=0.82, Spec=0.93}
#'   \item{tumor_site}{Factor: Tumor location (Lung, Breast, Colon, Prostate)}
#'   \item{specimen_quality}{Factor: Specimen quality (Adequate, Limited, Poor)}
#' }
#'
#' @details
#' Simulated with 25% malignancy prevalence. Pathologists show realistic
#' variation in diagnostic accuracy. Ideal for pathology agreement studies
#' using latent class analysis.
#'
#' @examples
#' data(nogoldstandard_pathology)
#' nogoldstandard(data = nogoldstandard_pathology,
#'                test1 = "Pathologist1", test1Positive = "Malignant",
#'                test2 = "Pathologist2", test2Positive = "Malignant",
#'                test3 = "Pathologist3", test3Positive = "Malignant",
#'                clinicalPreset = "pathology_agreement")
#'
#' @source Generated test data for ClinicoPath package
"nogoldstandard_pathology"

#' nogoldstandard Tumor Marker Data - Four-Marker Panel
#'
#' @description
#' Four tumor marker dataset with 220 patients for evaluating marker panel
#' performance without gold standard. Markers: CA125, HE4, CEA, AFP with
#' varying sensitivity (0.75-0.68) and specificity (0.88-0.85).
#'
#' @format A data frame with 220 rows and 7 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT001-PT220)}
#'   \item{CA125}{Factor: CA125 level ("Normal", "Elevated"), Sens=0.75, Spec=0.88}
#'   \item{HE4}{Factor: HE4 level ("Normal", "Elevated"), Sens=0.70, Spec=0.85}
#'   \item{CEA}{Factor: CEA level ("Normal", "Elevated"), Sens=0.68, Spec=0.90}
#'   \item{AFP}{Factor: AFP level ("Normal", "Elevated"), Sens=0.72, Spec=0.87}
#'   \item{age}{Numeric: Patient age in years (mean 62, SD 10)}
#'   \item{risk_category}{Factor: Risk level (Low, Moderate, High)}
#' }
#'
#' @details
#' Simulated with 20% cancer prevalence (screening context). Multiple markers
#' enable composite reference and latent class analysis comparisons.
#'
#' @examples
#' data(nogoldstandard_tumormarker)
#' nogoldstandard(data = nogoldstandard_tumormarker,
#'                test1 = "CA125", test1Positive = "Elevated",
#'                test2 = "HE4", test2Positive = "Elevated",
#'                test3 = "CEA", test3Positive = "Elevated",
#'                test4 = "AFP", test4Positive = "Elevated",
#'                clinicalPreset = "tumor_markers")
#'
#' @source Generated test data for ClinicoPath package
"nogoldstandard_tumormarker"

#' nogoldstandard Screening Data - Five-Test Panel
#'
#' @description
#' Comprehensive five-test screening dataset with 250 patients. Tests include
#' imaging, clinical exam, biomarker, questionnaire, and AI algorithm with
#' varying characteristics (Sens: 0.82-0.60, Spec: 0.92-0.75).
#'
#' @format A data frame with 250 rows and 8 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT001-PT250)}
#'   \item{Imaging}{Factor: Imaging result ("Normal", "Abnormal"), Sens=0.82, Spec=0.90}
#'   \item{ClinicalExam}{Factor: Clinical exam ("Normal", "Abnormal"), Sens=0.65, Spec=0.85}
#'   \item{Biomarker}{Factor: Biomarker test ("Normal", "Abnormal"), Sens=0.70, Spec=0.88}
#'   \item{Questionnaire}{Factor: Risk questionnaire ("Negative", "Positive"), Sens=0.60, Spec=0.75}
#'   \item{AI_Algorithm}{Factor: AI prediction ("Negative", "Positive"), Sens=0.88, Spec=0.92}
#'   \item{age}{Numeric: Patient age in years (mean 58, SD 15)}
#'   \item{screening_round}{Numeric: Screening round number (1-5)}
#' }
#'
#' @details
#' Simulated with 15% disease prevalence (screening setting). Five tests with
#' diverse characteristics demonstrate comprehensive evaluation methods.
#'
#' @examples
#' data(nogoldstandard_screening)
#' nogoldstandard(data = nogoldstandard_screening,
#'                test1 = "Imaging", test1Positive = "Abnormal",
#'                test2 = "ClinicalExam", test2Positive = "Abnormal",
#'                test3 = "Biomarker", test3Positive = "Abnormal",
#'                test4 = "Questionnaire", test4Positive = "Positive",
#'                test5 = "AI_Algorithm", test5Positive = "Positive",
#'                clinicalPreset = "screening_evaluation")
#'
#' @source Generated test data for ClinicoPath package
"nogoldstandard_screening"

#' nogoldstandard High Agreement Data
#'
#' @description
#' Dataset with 150 patients where two tests show high agreement (95%
#' concordance). Tests have good sensitivity (0.90, 0.88) and specificity
#' (0.90, 0.88).
#'
#' @format A data frame with 150 rows and 3 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT001-PT150)}
#'   \item{Test1}{Factor: First test ("Negative", "Positive"), Sens=0.90, Spec=0.90}
#'   \item{Test2}{Factor: Second test ("Negative", "Positive"), Sens=0.88, Spec=0.88, 95% agreement}
#'   \item{age}{Numeric: Patient age in years (mean 60, SD 10)}
#' }
#'
#' @details
#' Simulated with 35% prevalence. High correlation between tests (95% conditional
#' agreement) tests latent class model assumptions about conditional independence.
#'
#' @examples
#' data(nogoldstandard_highagreement)
#' nogoldstandard(data = nogoldstandard_highagreement,
#'                test1 = "Test1", test1Positive = "Positive",
#'                test2 = "Test2", test2Positive = "Positive",
#'                method = "latent_class")
#'
#' @source Generated test data for ClinicoPath package
"nogoldstandard_highagreement"

#' nogoldstandard Low Agreement Data
#'
#' @description
#' Dataset with 140 patients where two tests show low agreement. Tests have
#' moderate and different diagnostic characteristics (Sens: 0.70, 0.65; Spec: 0.80, 0.75).
#'
#' @format A data frame with 140 rows and 3 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT001-PT140)}
#'   \item{Test1}{Factor: First test ("Negative", "Positive"), Sens=0.70, Spec=0.80}
#'   \item{Test2}{Factor: Second test ("Negative", "Positive"), Sens=0.65, Spec=0.75}
#'   \item{age}{Numeric: Patient age in years (mean 58, SD 14)}
#' }
#'
#' @details
#' Simulated with 30% prevalence. Tests have low correlation, representing
#' tests measuring different aspects of disease.
#'
#' @examples
#' data(nogoldstandard_lowagreement)
#' nogoldstandard(data = nogoldstandard_lowagreement,
#'                test1 = "Test1", test1Positive = "Positive",
#'                test2 = "Test2", test2Positive = "Positive")
#'
#' @source Generated test data for ClinicoPath package
"nogoldstandard_lowagreement"

#' nogoldstandard Perfect Agreement Data
#'
#' @description
#' Edge case dataset with 100 patients where two tests are identical (100%
#' agreement). Test characteristics: Sens=0.85, Spec=0.85.
#'
#' @format A data frame with 100 rows and 3 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT001-PT100)}
#'   \item{Test1}{Factor: First test ("Negative", "Positive")}
#'   \item{Test2}{Factor: Second test ("Negative", "Positive"), identical to Test1}
#'   \item{age}{Numeric: Patient age in years (mean 55, SD 12)}
#' }
#'
#' @details
#' Simulated with 30% prevalence. Perfect agreement violates conditional
#' independence assumption of latent class models. Tests handling of
#' degenerate cases.
#'
#' @examples
#' data(nogoldstandard_perfect)
#' nogoldstandard(data = nogoldstandard_perfect,
#'                test1 = "Test1", test1Positive = "Positive",
#'                test2 = "Test2", test2Positive = "Positive")
#'
#' @source Generated test data for ClinicoPath package
"nogoldstandard_perfect"

#' nogoldstandard Rare Disease Data
#'
#' @description
#' Dataset with 300 patients and very low disease prevalence (5%). Three tests
#' with good characteristics (Sens: 0.80-0.75, Spec: 0.90-0.88).
#'
#' @format A data frame with 300 rows and 5 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT001-PT300)}
#'   \item{Test1}{Factor: First test ("Negative", "Positive"), Sens=0.80, Spec=0.90}
#'   \item{Test2}{Factor: Second test ("Negative", "Positive"), Sens=0.75, Spec=0.92}
#'   \item{Test3}{Factor: Third test ("Negative", "Positive"), Sens=0.78, Spec=0.88}
#'   \item{screening_site}{Factor: Screening site (Site_1 to Site_10)}
#' }
#'
#' @details
#' Rare disease setting (5% prevalence) typical of population screening.
#' Tests stability of estimation with few positive cases.
#'
#' @examples
#' data(nogoldstandard_rare)
#' nogoldstandard(data = nogoldstandard_rare,
#'                test1 = "Test1", test1Positive = "Positive",
#'                test2 = "Test2", test2Positive = "Positive",
#'                test3 = "Test3", test3Positive = "Positive")
#'
#' @source Generated test data for ClinicoPath package
"nogoldstandard_rare"

#' nogoldstandard Common Disease Data
#'
#' @description
#' Dataset with 170 patients and high disease prevalence (60%). Two tests with
#' good characteristics (Sens: 0.85, 0.82; Spec: 0.88, 0.85).
#'
#' @format A data frame with 170 rows and 4 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT001-PT170)}
#'   \item{Test1}{Factor: First test ("Negative", "Positive"), Sens=0.85, Spec=0.88}
#'   \item{Test2}{Factor: Second test ("Negative", "Positive"), Sens=0.82, Spec=0.85}
#'   \item{clinical_setting}{Factor: Setting (Inpatient, Outpatient, Emergency)}
#' }
#'
#' @details
#' High prevalence (60%) typical of clinical diagnostic settings. Contrasts
#' with rare disease scenarios for prevalence impact assessment.
#'
#' @examples
#' data(nogoldstandard_common)
#' nogoldstandard(data = nogoldstandard_common,
#'                test1 = "Test1", test1Positive = "Positive",
#'                test2 = "Test2", test2Positive = "Positive")
#'
#' @source Generated test data for ClinicoPath package
"nogoldstandard_common"

#' nogoldstandard All Positive Data
#'
#' @description
#' Edge case dataset with 80 patients where all test results are positive.
#' Tests handling of degenerate scenarios.
#'
#' @format A data frame with 80 rows and 3 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT001-PT080)}
#'   \item{Test1}{Factor: All "Positive"}
#'   \item{Test2}{Factor: All "Positive"}
#'   \item{age}{Numeric: Patient age in years (mean 55, SD 10)}
#' }
#'
#' @details
#' Zero variance scenario. Should trigger appropriate error messages or
#' warnings about inability to estimate test characteristics.
#'
#' @examples
#' data(nogoldstandard_allpositive)
#' # Should produce error or warning
#' \dontrun{
#' nogoldstandard(data = nogoldstandard_allpositive,
#'                test1 = "Test1", test1Positive = "Positive",
#'                test2 = "Test2", test2Positive = "Positive")
#' }
#'
#' @source Generated test data for ClinicoPath package
"nogoldstandard_allpositive"

#' nogoldstandard All Negative Data
#'
#' @description
#' Edge case dataset with 90 patients where all test results are negative.
#' Tests handling of degenerate scenarios.
#'
#' @format A data frame with 90 rows and 3 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT001-PT090)}
#'   \item{Test1}{Factor: All "Negative"}
#'   \item{Test2}{Factor: All "Negative"}
#'   \item{age}{Numeric: Patient age in years (mean 52, SD 11)}
#' }
#'
#' @details
#' Zero variance scenario. Should trigger appropriate error messages or
#' warnings about inability to estimate test characteristics.
#'
#' @examples
#' data(nogoldstandard_allnegative)
#' # Should produce error or warning
#' \dontrun{
#' nogoldstandard(data = nogoldstandard_allnegative,
#'                test1 = "Test1", test1Positive = "Positive",
#'                test2 = "Test2", test2Positive = "Positive")
#' }
#'
#' @source Generated test data for ClinicoPath package
"nogoldstandard_allnegative"

#' nogoldstandard Imbalanced Test Characteristics Data
#'
#' @description
#' Dataset with 160 patients featuring two tests with complementary
#' characteristics: one very sensitive (0.95) but less specific (0.70), one
#' very specific (0.95) but less sensitive (0.70).
#'
#' @format A data frame with 160 rows and 3 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT001-PT160)}
#'   \item{Sensitive_Test}{Factor: High sensitivity test ("Negative", "Positive"), Sens=0.95, Spec=0.70}
#'   \item{Specific_Test}{Factor: High specificity test ("Negative", "Positive"), Sens=0.70, Spec=0.95}
#'   \item{age}{Numeric: Patient age in years (mean 57, SD 13)}
#' }
#'
#' @details
#' Simulated with 28% prevalence. Demonstrates complementary test characteristics
#' and value of combining tests with different strengths.
#'
#' @examples
#' data(nogoldstandard_imbalanced)
#' nogoldstandard(data = nogoldstandard_imbalanced,
#'                test1 = "Sensitive_Test", test1Positive = "Positive",
#'                test2 = "Specific_Test", test2Positive = "Positive",
#'                method = "latent_class")
#'
#' @source Generated test data for ClinicoPath package
"nogoldstandard_imbalanced"

#' nogoldstandard Diagnostic Validation Data
#'
#' @description
#' Dataset with 190 patients for validating a new diagnostic test against two
#' reference tests without a gold standard. Tests have good characteristics
#' (Sens: 0.88-0.82, Spec: 0.90-0.88).
#'
#' @format A data frame with 190 rows and 5 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT001-PT190)}
#'   \item{New_Test}{Factor: Test being validated ("Negative", "Positive"), Sens=0.88, Spec=0.90}
#'   \item{Reference1}{Factor: First reference test ("Negative", "Positive"), Sens=0.85, Spec=0.88}
#'   \item{Reference2}{Factor: Second reference test ("Negative", "Positive"), Sens=0.82, Spec=0.92}
#'   \item{test_site}{Factor: Testing site (Academic, Community, Private)}
#' }
#'
#' @details
#' Simulated with 32% prevalence. Designed for diagnostic test validation
#' studies using latent class or Bayesian methods.
#'
#' @examples
#' data(nogoldstandard_validation)
#' nogoldstandard(data = nogoldstandard_validation,
#'                test1 = "New_Test", test1Positive = "Positive",
#'                test2 = "Reference1", test2Positive = "Positive",
#'                test3 = "Reference2", test3Positive = "Positive",
#'                clinicalPreset = "diagnostic_validation")
#'
#' @source Generated test data for ClinicoPath package
"nogoldstandard_validation"

#' nogoldstandard Missing Data
#'
#' @description
#' Dataset with 150 patients including missing values in test results (~5-8%
#' missingness per test). Three tests with good characteristics.
#'
#' @format A data frame with 150 rows and 5 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT001-PT150)}
#'   \item{Test1}{Factor: First test ("Negative", "Positive"), ~7% missing, Sens=0.85, Spec=0.85}
#'   \item{Test2}{Factor: Second test ("Negative", "Positive"), ~5% missing, Sens=0.80, Spec=0.88}
#'   \item{Test3}{Factor: Third test ("Negative", "Positive"), ~8% missing, Sens=0.82, Spec=0.90}
#'   \item{age}{Numeric: Patient age in years (mean 58, SD 12)}
#' }
#'
#' @details
#' Simulated with 30% prevalence. Missing data introduced randomly to test
#' listwise deletion and missing data handling.
#'
#' @examples
#' data(nogoldstandard_missing)
#' nogoldstandard(data = nogoldstandard_missing,
#'                test1 = "Test1", test1Positive = "Positive",
#'                test2 = "Test2", test2Positive = "Positive",
#'                test3 = "Test3", test3Positive = "Positive")
#'
#' @source Generated test data for ClinicoPath package
"nogoldstandard_missing"

#' nogoldstandard Small Sample Data
#'
#' @description
#' Small dataset with only 30 patients for testing performance with limited
#' sample sizes. Two tests with good characteristics.
#'
#' @format A data frame with 30 rows and 3 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT001-PT030)}
#'   \item{Test1}{Factor: First test ("Negative", "Positive"), Sens=0.85, Spec=0.85}
#'   \item{Test2}{Factor: Second test ("Negative", "Positive"), Sens=0.80, Spec=0.88}
#'   \item{age}{Numeric: Patient age in years (mean 55, SD 10)}
#' }
#'
#' @details
#' Simulated with 30% prevalence. Small sample (n=30) tests stability of
#' estimation and convergence with limited data.
#'
#' @examples
#' data(nogoldstandard_small)
#' nogoldstandard(data = nogoldstandard_small,
#'                test1 = "Test1", test1Positive = "Positive",
#'                test2 = "Test2", test2Positive = "Positive")
#'
#' @source Generated test data for ClinicoPath package
"nogoldstandard_small"

#' nogoldstandard Large Sample Data
#'
#' @description
#' Large dataset with 500 patients for testing computational efficiency and
#' performance with substantial sample sizes. Three tests with good
#' characteristics.
#'
#' @format A data frame with 500 rows and 7 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT0001-PT0500)}
#'   \item{Test1}{Factor: First test ("Negative", "Positive"), Sens=0.87, Spec=0.87}
#'   \item{Test2}{Factor: Second test ("Negative", "Positive"), Sens=0.84, Spec=0.89}
#'   \item{Test3}{Factor: Third test ("Negative", "Positive"), Sens=0.81, Spec=0.91}
#'   \item{age}{Numeric: Patient age in years (mean 59, SD 13)}
#'   \item{sex}{Factor: "Male" or "Female"}
#'   \item{study_center}{Factor: Multi-center study (Center_1 to Center_8)}
#' }
#'
#' @details
#' Simulated with 28% prevalence. Large sample (n=500) from multi-center study
#' tests computational efficiency and precision of estimates.
#'
#' @examples
#' data(nogoldstandard_large)
#' nogoldstandard(data = nogoldstandard_large,
#'                test1 = "Test1", test1Positive = "Positive",
#'                test2 = "Test2", test2Positive = "Positive",
#'                test3 = "Test3", test3Positive = "Positive")
#'
#' @source Generated test data for ClinicoPath package
"nogoldstandard_large"
