# ═══════════════════════════════════════════════════════════
# Dataset Documentation: psychopdaROC Test Data
# ═══════════════════════════════════════════════════════════
# Generated: 2026-01-06 | Seed: 42
# Function: Advanced ROC Analysis with Optimal Cutpoint Determination

#' psychopdaROC Test Data - Basic Diagnostic Test
#'
#' @description
#' Basic diagnostic test dataset with 200 patients for ROC analysis.
#' Contains binary disease status and a continuous biomarker with moderate
#' discrimination (AUC ~0.75-0.80).
#'
#' @format A data frame with 200 rows and 5 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT001-PT200)}
#'   \item{disease_status}{Factor: "Disease" or "Healthy" (30%/70% prevalence)}
#'   \item{biomarker}{Numeric: Continuous biomarker value (mean: 75 for diseased, 50 for healthy)}
#'   \item{age}{Numeric: Patient age in years (mean 60, SD 12)}
#'   \item{sex}{Factor: "Male" or "Female"}
#' }
#'
#' @details
#' Biomarker values follow normal distributions with clear separation between
#' disease groups, suitable for demonstrating basic ROC curve analysis and
#' optimal cutpoint determination using Youden index or other metrics.
#'
#' @examples
#' data(psychopdaROC_test)
#' psychopdaROC(data = psychopdaROC_test, dependentVars = "biomarker",
#'              classVar = "disease_status", positiveClass = "Disease")
#'
#' @source Generated test data for ClinicoPath package
"psychopdaROC_test"

#' psychopdaROC Screening Data - Cancer Detection
#'
#' @description
#' Cancer screening dataset with 250 patients featuring multiple biomarkers
#' (PSA and CA125) for evaluating screening test performance with low disease
#' prevalence (15%).
#'
#' @format A data frame with 250 rows and 6 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT001-PT250)}
#'   \item{cancer}{Factor: "Cancer" or "No_Cancer" (15%/85% prevalence)}
#'   \item{psa_level}{Numeric: PSA level (ng/mL), log-normal distribution}
#'   \item{ca125}{Numeric: CA125 level (U/mL), higher in cancer cases}
#'   \item{age}{Numeric: Patient age in years (mean 65, SD 10)}
#'   \item{risk_factors}{Factor: "None", "Family_History", or "Multiple"}
#' }
#'
#' @details
#' Designed for evaluating screening test characteristics where high sensitivity
#' is prioritized. PSA levels are log-normally distributed (median: 12 for cancer,
#' 4 for no cancer). CA125 shows normal distribution with higher values in cancer
#' cases (mean: 65 vs 25).
#'
#' @examples
#' data(psychopdaROC_screening)
#' psychopdaROC(data = psychopdaROC_screening,
#'              dependentVars = c("psa_level", "ca125"),
#'              classVar = "cancer", positiveClass = "Cancer",
#'              clinicalPreset = "screening")
#'
#' @source Generated test data for ClinicoPath package
"psychopdaROC_screening"

#' psychopdaROC Cardiac Data - Myocardial Infarction Biomarkers
#'
#' @description
#' Cardiac biomarker dataset with 180 patients for ROC analysis of MI diagnosis.
#' Features three key cardiac markers: troponin, creatinine, and BNP with
#' realistic clinical distributions.
#'
#' @format A data frame with 180 rows and 5 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT001-PT180)}
#'   \item{mi_status}{Factor: "MI" or "No_MI" (25%/75% prevalence)}
#'   \item{troponin}{Numeric: Troponin level (ng/mL), mean: 2.5 for MI, 0.3 for No_MI}
#'   \item{creatinine}{Numeric: Creatinine level (mg/dL), mean: 1.3 for MI, 0.9 for No_MI}
#'   \item{bnp}{Numeric: BNP level (pg/mL), mean: 850 for MI, 200 for No_MI}
#' }
#'
#' @details
#' Realistic cardiac biomarker distributions for evaluating diagnostic performance
#' in acute MI. Troponin shows strong discrimination, while creatinine and BNP
#' provide complementary diagnostic information.
#'
#' @examples
#' data(psychopdaROC_cardiac)
#' psychopdaROC(data = psychopdaROC_cardiac,
#'              dependentVars = c("troponin", "creatinine", "bnp"),
#'              classVar = "mi_status", positiveClass = "MI",
#'              method = "maximize_metric", metric = "youden")
#'
#' @source Generated test data for ClinicoPath package
"psychopdaROC_cardiac"

#' psychopdaROC Multi-Biomarker Data
#'
#' @description
#' Multiple biomarker comparison dataset with 220 patients featuring three
#' individual markers and a combined score for ROC analysis and marker comparison.
#'
#' @format A data frame with 220 rows and 6 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT001-PT220)}
#'   \item{diagnosis}{Factor: "Positive" or "Negative" (35%/65% prevalence)}
#'   \item{marker1}{Numeric: First biomarker (mean: 100 for positive, 70 for negative)}
#'   \item{marker2}{Numeric: Second biomarker (mean: 85 for positive, 55 for negative)}
#'   \item{marker3}{Numeric: Third biomarker (mean: 90 for positive, 65 for negative)}
#'   \item{combined_score}{Numeric: Average of three markers}
#' }
#'
#' @details
#' Designed for comparing individual biomarker performance and evaluating
#' combined marker strategies. The combined score typically shows improved
#' discrimination compared to individual markers.
#'
#' @examples
#' data(psychopdaROC_multibiomarker)
#' psychopdaROC(data = psychopdaROC_multibiomarker,
#'              dependentVars = c("marker1", "marker2", "marker3", "combined_score"),
#'              classVar = "diagnosis", positiveClass = "Positive",
#'              clinicalMode = "comprehensive")
#'
#' @source Generated test data for ClinicoPath package
"psychopdaROC_multibiomarker"

#' psychopdaROC Subgroup Analysis Data
#'
#' @description
#' Dataset for subgroup ROC analysis with 200 patients stratified by age group
#' and sex, allowing evaluation of test performance across demographic subgroups.
#'
#' @format A data frame with 200 rows and 5 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT001-PT200)}
#'   \item{disease}{Factor: "Disease" or "Healthy" (30%/70% prevalence)}
#'   \item{test_score}{Numeric: Diagnostic test score (mean: 70 for disease, 45 for healthy)}
#'   \item{age_group}{Factor: "Young", "Middle", or "Elderly"}
#'   \item{sex}{Factor: "Male" or "Female"}
#' }
#'
#' @details
#' Enables subgroup-specific ROC analysis to assess whether test performance
#' varies across age groups or between sexes. Useful for evaluating test
#' generalizability across populations.
#'
#' @examples
#' data(psychopdaROC_subgroup)
#' psychopdaROC(data = psychopdaROC_subgroup,
#'              dependentVars = "test_score", classVar = "disease",
#'              positiveClass = "Disease", subGroup = "age_group")
#'
#' @source Generated test data for ClinicoPath package
"psychopdaROC_subgroup"

#' psychopdaROC Perfect Separation Data
#'
#' @description
#' Dataset with 100 patients showing perfect discrimination between positive
#' and negative cases (AUC = 1.0), useful for testing edge case handling.
#'
#' @format A data frame with 100 rows and 3 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT001-PT100)}
#'   \item{condition}{Factor: "Positive" or "Negative" (50%/50% prevalence)}
#'   \item{perfect_test}{Numeric: Test values (80-100 for positive, 0-20 for negative)}
#' }
#'
#' @details
#' Complete separation between classes with no overlap. Positive cases have
#' values uniformly distributed between 80-100, negative cases between 0-20.
#' Tests ability to handle perfect discrimination scenarios.
#'
#' @examples
#' data(psychopdaROC_perfect)
#' psychopdaROC(data = psychopdaROC_perfect, dependentVars = "perfect_test",
#'              classVar = "condition", positiveClass = "Positive")
#'
#' @source Generated test data for ClinicoPath package
"psychopdaROC_perfect"

#' psychopdaROC Poor Discrimination Data
#'
#' @description
#' Dataset with 150 patients showing no discrimination between case and control
#' groups (AUC ~0.50), useful for testing handling of ineffective biomarkers.
#'
#' @format A data frame with 150 rows and 3 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT001-PT150)}
#'   \item{status}{Factor: "Case" or "Control" (50%/50% prevalence)}
#'   \item{poor_marker}{Numeric: Biomarker with no discriminatory value (mean 50, SD 15)}
#' }
#'
#' @details
#' Both cases and controls have identical distributions (normal, mean=50, SD=15).
#' Tests proper handling and warning messages for biomarkers with no diagnostic value.
#'
#' @examples
#' data(psychopdaROC_poor)
#' psychopdaROC(data = psychopdaROC_poor, dependentVars = "poor_marker",
#'              classVar = "status")
#'
#' @source Generated test data for ClinicoPath package
"psychopdaROC_poor"

#' psychopdaROC Overlapping Distributions Data
#'
#' @description
#' Dataset with 190 patients showing moderate overlap between diseased and
#' non-diseased groups, representing realistic clinical scenarios with
#' imperfect discrimination.
#'
#' @format A data frame with 190 rows and 3 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT001-PT190)}
#'   \item{diagnosis}{Factor: "Diseased" or "Non_Diseased" (40%/60% prevalence)}
#'   \item{test_value}{Numeric: Test values with moderate overlap (mean: 60 vs 55, SD 18)}
#' }
#'
#' @details
#' Small mean difference (5 units) with substantial overlap between groups.
#' Represents realistic clinical scenarios where diagnostic tests show
#' moderate but imperfect discrimination (AUC ~0.60-0.65).
#'
#' @examples
#' data(psychopdaROC_overlap)
#' psychopdaROC(data = psychopdaROC_overlap, dependentVars = "test_value",
#'              classVar = "diagnosis", positiveClass = "Diseased")
#'
#' @source Generated test data for ClinicoPath package
"psychopdaROC_overlap"

#' psychopdaROC Rare Disease Data
#'
#' @description
#' Dataset with 300 patients and very low disease prevalence (5%), representing
#' rare disease screening scenarios where prevalence affects predictive values.
#'
#' @format A data frame with 300 rows and 3 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT001-PT300)}
#'   \item{rare_disease}{Factor: "Disease" or "No_Disease" (5%/95% prevalence)}
#'   \item{biomarker}{Numeric: Biomarker value (mean: 80 for disease, 45 for no disease)}
#' }
#'
#' @details
#' Low prevalence (5%) with good biomarker discrimination. Demonstrates impact
#' of prevalence on positive and negative predictive values even with good
#' sensitivity and specificity.
#'
#' @examples
#' data(psychopdaROC_rare)
#' psychopdaROC(data = psychopdaROC_rare, dependentVars = "biomarker",
#'              classVar = "rare_disease", positiveClass = "Disease")
#'
#' @source Generated test data for ClinicoPath package
"psychopdaROC_rare"

#' psychopdaROC Cost-Benefit Data
#'
#' @description
#' Dataset with 160 patients including cost information for false positive and
#' false negative errors, enabling cost-benefit optimal cutpoint determination.
#'
#' @format A data frame with 160 rows and 5 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT001-PT160)}
#'   \item{outcome}{Factor: "Event" or "No_Event" (28%/72% prevalence)}
#'   \item{risk_score}{Numeric: Risk prediction score (mean: 75 for event, 50 for no event)}
#'   \item{false_positive_cost}{Numeric: Cost of false positive ($100)}
#'   \item{false_negative_cost}{Numeric: Cost of false negative ($1000)}
#' }
#'
#' @details
#' False negative cost is 10× higher than false positive cost, representing
#' clinical scenarios where missing a case is much more costly than a false alarm.
#' Enables cost-ratio optimal cutpoint method.
#'
#' @examples
#' data(psychopdaROC_costbenefit)
#' psychopdaROC(data = psychopdaROC_costbenefit, dependentVars = "risk_score",
#'              classVar = "outcome", positiveClass = "Event",
#'              method = "oc_cost_ratio")
#'
#' @source Generated test data for ClinicoPath package
"psychopdaROC_costbenefit"

#' psychopdaROC Disease Spectrum Data
#'
#' @description
#' Dataset with 170 patients across a disease severity spectrum (Mild, Moderate,
#' Severe) collapsed into binary classification for ROC analysis.
#'
#' @format A data frame with 170 rows and 4 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT001-PT170)}
#'   \item{severity}{Factor: "Mild", "Moderate", or "Severe"}
#'   \item{binary_status}{Factor: "Negative" (Mild) or "Positive" (Moderate/Severe)}
#'   \item{continuous_marker}{Numeric: Marker increasing with severity (40/60/85)}
#' }
#'
#' @details
#' Represents continuous disease spectrum with graded marker values:
#' Mild (mean 40), Moderate (mean 60), Severe (mean 85). Binary classification
#' treats Mild as Negative, Moderate/Severe as Positive.
#'
#' @examples
#' data(psychopdaROC_spectrum)
#' psychopdaROC(data = psychopdaROC_spectrum, dependentVars = "continuous_marker",
#'              classVar = "binary_status", positiveClass = "Positive")
#'
#' @source Generated test data for ClinicoPath package
"psychopdaROC_spectrum"

#' psychopdaROC Time-Dependent Biomarker Data
#'
#' @description
#' Dataset with 140 patients featuring baseline and follow-up biomarker
#' measurements for evaluating time-dependent diagnostic performance.
#'
#' @format A data frame with 140 rows and 5 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT001-PT140)}
#'   \item{outcome}{Factor: "Event" or "No_Event" (32%/68% prevalence)}
#'   \item{baseline_marker}{Numeric: Baseline biomarker (mean: 70 for event, 50 for no event)}
#'   \item{followup_marker}{Numeric: Follow-up biomarker (increases for events, decreases for no events)}
#'   \item{time_to_outcome}{Numeric: Time to outcome in months (1-36)}
#' }
#'
#' @details
#' Follow-up marker changes from baseline: increases by ~15 for events,
#' decreases by ~5 for no events. Enables ROC analysis with change scores
#' or follow-up values.
#'
#' @examples
#' data(psychopdaROC_timedep)
#' psychopdaROC(data = psychopdaROC_timedep,
#'              dependentVars = c("baseline_marker", "followup_marker"),
#'              classVar = "outcome", positiveClass = "Event")
#'
#' @source Generated test data for ClinicoPath package
"psychopdaROC_timedep"

#' psychopdaROC Small Sample Data
#'
#' @description
#' Small dataset with only 30 patients for testing performance with limited
#' sample sizes and assessing stability of cutpoint estimates.
#'
#' @format A data frame with 30 rows and 3 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT001-PT030)}
#'   \item{class}{Factor: "Positive" or "Negative" (50%/50% prevalence)}
#'   \item{marker}{Numeric: Biomarker value (mean: 65 for positive, 45 for negative)}
#' }
#'
#' @details
#' Limited sample size (n=30) tests stability of ROC analysis and cutpoint
#' determination with small datasets. Wide confidence intervals expected.
#'
#' @examples
#' data(psychopdaROC_small)
#' psychopdaROC(data = psychopdaROC_small, dependentVars = "marker",
#'              classVar = "class")
#'
#' @source Generated test data for ClinicoPath package
"psychopdaROC_small"

#' psychopdaROC Imbalanced Classes Data
#'
#' @description
#' Dataset with 200 patients and severe class imbalance (2% event rate),
#' representing extremely rare outcomes where PPV/NPV are critical metrics.
#'
#' @format A data frame with 200 rows and 3 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT001-PT200)}
#'   \item{rare_outcome}{Factor: "Event" or "No_Event" (2%/98% prevalence)}
#'   \item{predictor}{Numeric: Predictor value (mean: 90 for event, 50 for no event)}
#' }
#'
#' @details
#' Extreme class imbalance (2% events) with good predictor discrimination.
#' Tests handling of severely imbalanced data and emphasis on predictive
#' values over sensitivity/specificity.
#'
#' @examples
#' data(psychopdaROC_imbalanced)
#' psychopdaROC(data = psychopdaROC_imbalanced, dependentVars = "predictor",
#'              classVar = "rare_outcome", positiveClass = "Event")
#'
#' @source Generated test data for ClinicoPath package
"psychopdaROC_imbalanced"

#' psychopdaROC Missing Data
#'
#' @description
#' Dataset with 150 patients including missing values in predictors and
#' class variable for testing handling of incomplete data.
#'
#' @format A data frame with 150 rows and 5 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT001-PT150)}
#'   \item{diagnosis}{Factor: "Disease" or "Healthy" with ~5% missing}
#'   \item{test_a}{Numeric: First test with ~8% missing}
#'   \item{test_b}{Numeric: Second test with ~7% missing}
#'   \item{covariate}{Factor: "A", "B", or "C"}
#' }
#'
#' @details
#' Missing data introduced randomly: diagnosis (8 missing), test_a (12 missing),
#' test_b (10 missing). Tests proper handling of missing values in ROC analysis
#' with appropriate warnings or exclusions.
#'
#' @examples
#' data(psychopdaROC_missing)
#' psychopdaROC(data = psychopdaROC_missing,
#'              dependentVars = c("test_a", "test_b"),
#'              classVar = "diagnosis")
#'
#' @source Generated test data for ClinicoPath package
"psychopdaROC_missing"

#' psychopdaROC Constant Predictor Data
#'
#' @description
#' Dataset with 80 patients where the biomarker has no variation (constant value),
#' testing handling of predictors with zero variance.
#'
#' @format A data frame with 80 rows and 3 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT001-PT080)}
#'   \item{outcome}{Factor: "Positive" or "Negative" (50%/50% prevalence)}
#'   \item{constant_marker}{Numeric: Constant value of 50 for all patients}
#' }
#'
#' @details
#' All patients have identical biomarker value (50). Tests error handling
#' for zero-variance predictors that cannot produce ROC curves.
#'
#' @examples
#' data(psychopdaROC_constant)
#' psychopdaROC(data = psychopdaROC_constant, dependentVars = "constant_marker",
#'              classVar = "outcome")
#'
#' @source Generated test data for ClinicoPath package
"psychopdaROC_constant"

#' psychopdaROC Large Sample Data
#'
#' @description
#' Large dataset with 500 patients and multiple biomarkers for testing
#' computational efficiency and performance with substantial sample sizes.
#'
#' @format A data frame with 500 rows and 8 variables:
#' \describe{
#'   \item{patient_id}{Character: Patient identifier (PT0001-PT0500)}
#'   \item{disease_status}{Factor: "Disease" or "No_Disease" (30%/70% prevalence)}
#'   \item{biomarker1}{Numeric: First biomarker (mean: 75 for disease, 50 for no disease)}
#'   \item{biomarker2}{Numeric: Second biomarker (mean: 68 for disease, 48 for no disease)}
#'   \item{age}{Numeric: Patient age in years (mean 62, SD 13)}
#'   \item{sex}{Factor: "Male" or "Female"}
#'   \item{site}{Factor: Research site (Site_1 through Site_10)}
#'   \item{risk_category}{Factor: "Low", "Intermediate", or "High"}
#' }
#'
#' @details
#' Large sample (n=500) with multiple biomarkers and stratification variables.
#' Tests computational efficiency and stability of estimates with adequate
#' sample sizes. Includes multi-site and risk stratification for subgroup analysis.
#'
#' @examples
#' data(psychopdaROC_large)
#' psychopdaROC(data = psychopdaROC_large,
#'              dependentVars = c("biomarker1", "biomarker2"),
#'              classVar = "disease_status", positiveClass = "Disease")
#'
#' @source Generated test data for ClinicoPath package
"psychopdaROC_large"
