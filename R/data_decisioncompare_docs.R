# ═══════════════════════════════════════════════════════════
# Dataset Documentation: decisioncompare Test Data
# ═══════════════════════════════════════════════════════════
# Generated: 2026-01-06 | Seed: 42
# Function: Compare Medical Decision Tests

#' decisioncompare Test Data - Basic Two-Test Comparison
#'
#' @description
#' Basic dataset with 200 patients for comparing two diagnostic tests against
#' a gold standard. Test 1 (Sens=0.85, Spec=0.90), Test 2 (Sens=0.80, Spec=0.85).
#'
#' @format A data frame with 200 rows and 5 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT001-PT200)}
#'   \item{GoldStandard}{Factor: True disease status ("Negative", "Positive"), 30% prevalence}
#'   \item{Test1}{Factor: First test result ("Negative", "Positive"), Sens=0.85, Spec=0.90}
#'   \item{Test2}{Factor: Second test result ("Negative", "Positive"), Sens=0.80, Spec=0.85}
#'   \item{age}{Numeric: Patient age in years (mean 58, SD 12)}
#'   \item{sex}{Factor: "Male" or "Female"}
#' }
#'
#' @details
#' Simulated with 30% disease prevalence. Tests have good characteristics with
#' Test1 slightly superior to Test2. Suitable for demonstrating basic test
#' comparison with confidence intervals and McNemar's test.
#'
#' @examples
#' data(decisioncompare_test)
#' decisioncompare(data = decisioncompare_test, gold = "GoldStandard",
#'                 goldPositive = "Positive", test1 = "Test1",
#'                 test1Positive = "Positive", test2 = "Test2",
#'                 test2Positive = "Positive", ci = TRUE, statComp = TRUE)
#'
#' @source Generated test data for ClinicoPath package
"decisioncompare_test"

#' decisioncompare Three-Test Data
#'
#' @description
#' Dataset with 180 patients comparing three tests with complementary
#' characteristics: high sensitivity (0.90), balanced (0.85/0.88), and high
#' specificity (0.78/0.92).
#'
#' @format A data frame with 180 rows and 5 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT001-PT180)}
#'   \item{GoldStandard}{Factor: True disease status ("Negative", "Positive"), 35% prevalence}
#'   \item{Test1}{Factor: High sensitivity test ("Negative", "Positive"), Sens=0.90, Spec=0.85}
#'   \item{Test2}{Factor: Balanced test ("Negative", "Positive"), Sens=0.85, Spec=0.88}
#'   \item{Test3}{Factor: High specificity test ("Negative", "Positive"), Sens=0.78, Spec=0.92}
#'   \item{test_site}{Factor: Testing site (Site_A, Site_B, Site_C)}
#' }
#'
#' @details
#' Demonstrates three-way comparison with complementary test characteristics.
#' Ideal for radar plot visualization and comprehensive test evaluation.
#'
#' @examples
#' data(decisioncompare_threetest)
#' decisioncompare(data = decisioncompare_threetest, gold = "GoldStandard",
#'                 goldPositive = "Positive", test1 = "Test1",
#'                 test1Positive = "Positive", test2 = "Test2",
#'                 test2Positive = "Positive", test3 = "Test3",
#'                 test3Positive = "Positive", radarplot = TRUE)
#'
#' @source Generated test data for ClinicoPath package
"decisioncompare_threetest"

#' decisioncompare Imaging Data - Multi-Modality Comparison
#'
#' @description
#' Dataset with 220 patients comparing three imaging modalities (CT, MRI,
#' Biomarker) against pathology gold standard for cancer detection.
#'
#' @format A data frame with 220 rows and 5 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT001-PT220)}
#'   \item{Pathology}{Factor: Pathology result ("Benign", "Malignant"), 28% malignant}
#'   \item{CT_Scan}{Factor: CT result ("Normal", "Abnormal"), Sens=0.88, Spec=0.85}
#'   \item{MRI}{Factor: MRI result ("Normal", "Abnormal"), Sens=0.92, Spec=0.90}
#'   \item{Biomarker}{Factor: Biomarker test ("Normal", "Elevated"), Sens=0.80, Spec=0.88}
#'   \item{tumor_size_mm}{Numeric: Tumor size in mm (mean: 35 for malignant, 15 for benign)}
#' }
#'
#' @details
#' Realistic imaging comparison scenario. MRI shows highest sensitivity and
#' specificity. Biomarker offers non-invasive alternative with good specificity.
#'
#' @examples
#' data(decisioncompare_imaging)
#' decisioncompare(data = decisioncompare_imaging, gold = "Pathology",
#'                 goldPositive = "Malignant", test1 = "CT_Scan",
#'                 test1Positive = "Abnormal", test2 = "MRI",
#'                 test2Positive = "Abnormal", test3 = "Biomarker",
#'                 test3Positive = "Elevated", radarplot = TRUE)
#'
#' @source Generated test data for ClinicoPath package
"decisioncompare_imaging"

#' decisioncompare Screening Data
#'
#' @description
#' Dataset with 250 patients comparing screening test (high sensitivity) vs
#' diagnostic test (high specificity) in low prevalence setting (15%).
#'
#' @format A data frame with 250 rows and 5 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT001-PT250)}
#'   \item{Biopsy}{Factor: Biopsy result ("Negative", "Positive"), 15% positive}
#'   \item{ScreeningTest}{Factor: Screening test ("Negative", "Positive"), Sens=0.95, Spec=0.80}
#'   \item{DiagnosticTest}{Factor: Diagnostic test ("Negative", "Positive"), Sens=0.85, Spec=0.92}
#'   \item{age}{Numeric: Patient age in years (mean 62, SD 8)}
#'   \item{risk_score}{Numeric: Risk score (mean: 7 for positive, 3 for negative)}
#' }
#'
#' @details
#' Demonstrates trade-off between sensitivity (screening) and specificity
#' (diagnostic). Low prevalence setting typical of screening programs.
#'
#' @examples
#' data(decisioncompare_screening)
#' decisioncompare(data = decisioncompare_screening, gold = "Biopsy",
#'                 goldPositive = "Positive", test1 = "ScreeningTest",
#'                 test1Positive = "Positive", test2 = "DiagnosticTest",
#'                 test2Positive = "Positive", pp = TRUE, pprob = 0.15)
#'
#' @source Generated test data for ClinicoPath package
"decisioncompare_screening"

#' decisioncompare Inter-Rater Data
#'
#' @description
#' Dataset with 150 patients evaluating three raters (experienced, moderate,
#' junior) against consensus panel gold standard.
#'
#' @format A data frame with 150 rows and 5 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT001-PT150)}
#'   \item{ConsensusPanel}{Factor: Consensus diagnosis ("No Disease", "Disease"), 32% disease}
#'   \item{Rater1}{Factor: Experienced rater ("No Disease", "Disease"), Sens=0.88, Spec=0.90}
#'   \item{Rater2}{Factor: Moderate experience ("No Disease", "Disease"), Sens=0.82, Spec=0.85}
#'   \item{Rater3}{Factor: Junior rater ("No Disease", "Disease"), Sens=0.75, Spec=0.82}
#'   \item{case_difficulty}{Factor: Case difficulty (Easy, Moderate, Difficult)}
#' }
#'
#' @details
#' Inter-rater reliability study with varying expertise levels. Demonstrates
#' performance comparison across different experience levels.
#'
#' @examples
#' data(decisioncompare_raters)
#' decisioncompare(data = decisioncompare_raters, gold = "ConsensusPanel",
#'                 goldPositive = "Disease", test1 = "Rater1",
#'                 test1Positive = "Disease", test2 = "Rater2",
#'                 test2Positive = "Disease", test3 = "Rater3",
#'                 test3Positive = "Disease", statComp = TRUE)
#'
#' @source Generated test data for ClinicoPath package
"decisioncompare_raters"

#' decisioncompare Indeterminate Results Data
#'
#' @description
#' Dataset with 170 patients where Test1 includes indeterminate results
#' (three levels) alongside standard binary Test2.
#'
#' @format A data frame with 170 rows and 4 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT001-PT170)}
#'   \item{GoldStandard}{Factor: True status ("Negative", "Positive"), 30% positive}
#'   \item{Test1}{Factor: Test with indeterminate ("Negative", "Positive", "Indeterminate")}
#'   \item{Test2}{Factor: Standard binary test ("Negative", "Positive"), Sens=0.85, Spec=0.88}
#'   \item{age}{Numeric: Patient age in years (mean 55, SD 13)}
#' }
#'
#' @details
#' Demonstrates handling of indeterminate results. Test1 has ~10% indeterminate
#' results in both disease groups. Use excludeIndeterminate option to control
#' handling.
#'
#' @examples
#' data(decisioncompare_indeterminate)
#' decisioncompare(data = decisioncompare_indeterminate, gold = "GoldStandard",
#'                 goldPositive = "Positive", test1 = "Test1",
#'                 test1Positive = "Positive", test2 = "Test2",
#'                 test2Positive = "Positive", excludeIndeterminate = TRUE)
#'
#' @source Generated test data for ClinicoPath package
"decisioncompare_indeterminate"

#' decisioncompare Perfect Test Data
#'
#' @description
#' Dataset with 120 patients comparing perfect test (Sens=1.0, Spec=1.0)
#' against imperfect test (Sens=0.85, Spec=0.88).
#'
#' @format A data frame with 120 rows and 4 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT001-PT120)}
#'   \item{GoldStandard}{Factor: True status ("Negative", "Positive"), 30% positive}
#'   \item{PerfectTest}{Factor: Perfect test ("Negative", "Positive"), Sens=1.0, Spec=1.0}
#'   \item{ImperfectTest}{Factor: Imperfect test ("Negative", "Positive"), Sens=0.85, Spec=0.88}
#'   \item{age}{Numeric: Patient age in years (mean 60, SD 11)}
#' }
#'
#' @details
#' Edge case demonstrating perfect test performance (100% agreement with gold
#' standard) compared to realistic imperfect test.
#'
#' @examples
#' data(decisioncompare_perfect)
#' decisioncompare(data = decisioncompare_perfect, gold = "GoldStandard",
#'                 goldPositive = "Positive", test1 = "PerfectTest",
#'                 test1Positive = "Positive", test2 = "ImperfectTest",
#'                 test2Positive = "Positive", statComp = TRUE)
#'
#' @source Generated test data for ClinicoPath package
"decisioncompare_perfect"

#' decisioncompare Poor Performance Data
#'
#' @description
#' Dataset with 140 patients featuring two poorly performing tests
#' (Sens: 0.60/0.55, Spec: 0.65/0.70).
#'
#' @format A data frame with 140 rows and 4 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT001-PT140)}
#'   \item{GoldStandard}{Factor: True status ("Negative", "Positive"), 30% positive}
#'   \item{PoorTest1}{Factor: First poor test ("Negative", "Positive"), Sens=0.60, Spec=0.65}
#'   \item{PoorTest2}{Factor: Second poor test ("Negative", "Positive"), Sens=0.55, Spec=0.70}
#'   \item{age}{Numeric: Patient age in years (mean 57, SD 12)}
#' }
#'
#' @details
#' Tests with low accuracy (barely better than chance). Demonstrates handling
#' of poorly performing diagnostic tests.
#'
#' @examples
#' data(decisioncompare_poor)
#' decisioncompare(data = decisioncompare_poor, gold = "GoldStandard",
#'                 goldPositive = "Positive", test1 = "PoorTest1",
#'                 test1Positive = "Positive", test2 = "PoorTest2",
#'                 test2Positive = "Positive", ci = TRUE)
#'
#' @source Generated test data for ClinicoPath package
"decisioncompare_poor"

#' decisioncompare Rare Disease Data
#'
#' @description
#' Dataset with 300 patients and very low disease prevalence (5%), typical of
#' rare disease screening. Tests: high sensitivity (0.90) vs high specificity (0.85/0.95).
#'
#' @format A data frame with 300 rows and 4 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT001-PT300)}
#'   \item{GoldStandard}{Factor: True status ("Negative", "Positive"), 5% positive}
#'   \item{Test1}{Factor: High sensitivity test ("Negative", "Positive"), Sens=0.90, Spec=0.90}
#'   \item{Test2}{Factor: High specificity test ("Negative", "Positive"), Sens=0.85, Spec=0.95}
#'   \item{screening_round}{Numeric: Screening round (1-5)}
#' }
#'
#' @details
#' Rare disease setting (5% prevalence). Demonstrates impact of low prevalence
#' on PPV/NPV. High specificity crucial to minimize false positives.
#'
#' @examples
#' data(decisioncompare_rare)
#' decisioncompare(data = decisioncompare_rare, gold = "GoldStandard",
#'                 goldPositive = "Positive", test1 = "Test1",
#'                 test1Positive = "Positive", test2 = "Test2",
#'                 test2Positive = "Positive", pp = TRUE, pprob = 0.05)
#'
#' @source Generated test data for ClinicoPath package
"decisioncompare_rare"

#' decisioncompare Common Disease Data
#'
#' @description
#' Dataset with 160 patients and high disease prevalence (60%), typical of
#' clinical diagnostic setting.
#'
#' @format A data frame with 160 rows and 4 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT001-PT160)}
#'   \item{GoldStandard}{Factor: True status ("Negative", "Positive"), 60% positive}
#'   \item{Test1}{Factor: First test ("Negative", "Positive"), Sens=0.85, Spec=0.88}
#'   \item{Test2}{Factor: Second test ("Negative", "Positive"), Sens=0.82, Spec=0.85}
#'   \item{clinical_setting}{Factor: Setting (Inpatient, Outpatient)}
#' }
#'
#' @details
#' High prevalence (60%) typical of symptomatic clinical populations. Contrasts
#' with rare disease for prevalence impact on predictive values.
#'
#' @examples
#' data(decisioncompare_common)
#' decisioncompare(data = decisioncompare_common, gold = "GoldStandard",
#'                 goldPositive = "Positive", test1 = "Test1",
#'                 test1Positive = "Positive", test2 = "Test2",
#'                 test2Positive = "Positive", ci = TRUE)
#'
#' @source Generated test data for ClinicoPath package
"decisioncompare_common"

#' decisioncompare Identical Tests Data
#'
#' @description
#' Edge case dataset with 100 patients where Test1 and Test2 are identical.
#' Tests handling of perfect agreement between tests.
#'
#' @format A data frame with 100 rows and 4 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT001-PT100)}
#'   \item{GoldStandard}{Factor: True status ("Negative", "Positive"), 30% positive}
#'   \item{Test1}{Factor: First test ("Negative", "Positive"), Sens=0.85, Spec=0.88}
#'   \item{Test2}{Factor: Identical to Test1}
#'   \item{age}{Numeric: Patient age in years (mean 58, SD 10)}
#' }
#'
#' @details
#' Tests are 100% identical. McNemar's test should show no significant
#' difference (p=1.0).
#'
#' @examples
#' data(decisioncompare_identical)
#' decisioncompare(data = decisioncompare_identical, gold = "GoldStandard",
#'                 goldPositive = "Positive", test1 = "Test1",
#'                 test1Positive = "Positive", test2 = "Test2",
#'                 test2Positive = "Positive", statComp = TRUE)
#'
#' @source Generated test data for ClinicoPath package
"decisioncompare_identical"

#' decisioncompare Missing Data
#'
#' @description
#' Dataset with 150 patients including missing values in gold standard and
#' test results (~5-8% missingness).
#'
#' @format A data frame with 150 rows and 6 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT001-PT150)}
#'   \item{GoldStandard}{Factor: True status with ~5% missing ("Negative", "Positive")}
#'   \item{Test1}{Factor: First test with ~7% missing ("Negative", "Positive"), Sens=0.85, Spec=0.88}
#'   \item{Test2}{Factor: Second test with ~5% missing ("Negative", "Positive"), Sens=0.80, Spec=0.85}
#'   \item{Test3}{Factor: Third test with ~8% missing ("Negative", "Positive"), Sens=0.82, Spec=0.90}
#'   \item{age}{Numeric: Patient age in years (mean 58, SD 12)}
#' }
#'
#' @details
#' Missing data introduced randomly to test listwise deletion and missing
#' data handling warnings.
#'
#' @examples
#' data(decisioncompare_missing)
#' decisioncompare(data = decisioncompare_missing, gold = "GoldStandard",
#'                 goldPositive = "Positive", test1 = "Test1",
#'                 test1Positive = "Positive", test2 = "Test2",
#'                 test2Positive = "Positive")
#'
#' @source Generated test data for ClinicoPath package
"decisioncompare_missing"

#' decisioncompare Small Sample Data
#'
#' @description
#' Small dataset with only 30 patients for testing performance with limited
#' sample sizes.
#'
#' @format A data frame with 30 rows and 4 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT001-PT030)}
#'   \item{GoldStandard}{Factor: True status ("Negative", "Positive"), 30% positive}
#'   \item{Test1}{Factor: First test ("Negative", "Positive"), Sens=0.85, Spec=0.88}
#'   \item{Test2}{Factor: Second test ("Negative", "Positive"), Sens=0.80, Spec=0.85}
#'   \item{age}{Numeric: Patient age in years (mean 55, SD 10)}
#' }
#'
#' @details
#' Small sample (n=30) tests stability of estimates and wide confidence
#' intervals.
#'
#' @examples
#' data(decisioncompare_small)
#' decisioncompare(data = decisioncompare_small, gold = "GoldStandard",
#'                 goldPositive = "Positive", test1 = "Test1",
#'                 test1Positive = "Positive", test2 = "Test2",
#'                 test2Positive = "Positive", ci = TRUE)
#'
#' @source Generated test data for ClinicoPath package
"decisioncompare_small"

#' decisioncompare Large Sample Data
#'
#' @description
#' Large dataset with 500 patients for testing computational efficiency and
#' precise estimates.
#'
#' @format A data frame with 500 rows and 8 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT0001-PT0500)}
#'   \item{GoldStandard}{Factor: True status ("Negative", "Positive"), 28% positive}
#'   \item{Test1}{Factor: First test ("Negative", "Positive"), Sens=0.87, Spec=0.89}
#'   \item{Test2}{Factor: Second test ("Negative", "Positive"), Sens=0.84, Spec=0.87}
#'   \item{Test3}{Factor: Third test ("Negative", "Positive"), Sens=0.81, Spec=0.91}
#'   \item{age}{Numeric: Patient age in years (mean 59, SD 13)}
#'   \item{sex}{Factor: "Male" or "Female"}
#'   \item{study_center}{Factor: Multi-center study (Center_1 to Center_8)}
#' }
#'
#' @details
#' Large sample (n=500) from multi-center study tests computational efficiency
#' and narrow confidence intervals.
#'
#' @examples
#' data(decisioncompare_large)
#' decisioncompare(data = decisioncompare_large, gold = "GoldStandard",
#'                 goldPositive = "Positive", test1 = "Test1",
#'                 test1Positive = "Positive", test2 = "Test2",
#'                 test2Positive = "Positive", test3 = "Test3",
#'                 test3Positive = "Positive", ci = TRUE, radarplot = TRUE)
#'
#' @source Generated test data for ClinicoPath package
"decisioncompare_large"
