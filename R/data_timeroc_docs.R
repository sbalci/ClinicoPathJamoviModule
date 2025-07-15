#' TimeROC Test Datasets
#'
#' Comprehensive collection of test datasets designed for validating time-dependent
#' ROC analysis functions. Each dataset represents different clinical scenarios,
#' biomarker types, and methodological challenges commonly encountered in 
#' time-to-event analysis.
#'
#' @name timeroc_datasets
#' @docType data
#' @usage data(timeroc_cancer_biomarker)
#' @usage data(timeroc_cardiovascular_risk)
#' @usage data(timeroc_multi_biomarker)
#' @usage data(timeroc_landmark_biomarker)
#' @usage data(timeroc_edge_cases)
#' @usage data(timeroc_competing_risks)
NULL

#' Cancer Biomarker Time-Dependent ROC Test Dataset
#'
#' Simulated dataset representing a cancer biomarker study with tumor marker 
#' measurements and survival outcomes. Designed to test basic time-dependent 
#' ROC functionality with realistic cancer progression patterns.
#'
#' @format A data frame with 300 observations and 9 variables:
#' \describe{
#'   \item{patient_id}{Character. Unique patient identifier (CA_001 to CA_300)}
#'   \item{age}{Integer. Patient age at diagnosis (30-90 years)}
#'   \item{sex}{Character. Patient sex ("Male", "Female")}
#'   \item{cancer_stage}{Character. Cancer stage at diagnosis ("I", "II", "III", "IV")}
#'   \item{tumor_biomarker}{Numeric. Continuous tumor biomarker level (higher = worse prognosis)}
#'   \item{follow_up_months}{Numeric. Follow-up time in months (0-60)}
#'   \item{death_event}{Integer. Death indicator (1 = death, 0 = censored)}
#'   \item{treatment_type}{Character. Treatment received ("Surgery", "Surgery+Chemo", "Surgery+Radio", "Palliative")}
#'   \item{hospital_center}{Character. Treatment center ("Center_A" to "Center_E")}
#' }
#'
#' @details
#' This dataset simulates a cohort study following cancer patients for up to 60 months.
#' The tumor biomarker shows realistic stage-dependent distributions with higher
#' values in advanced stages. Survival times are generated based on biomarker level,
#' age, and cancer stage using exponential survival models.
#'
#' **Key Features:**
#' - Realistic biomarker-outcome associations
#' - Stage-stratified survival patterns  
#' - Administrative censoring at 60 months
#' - Random dropout patterns
#' - 245/300 events (81.7% event rate)
#'
#' **Recommended TimeROC Parameters:**
#' - Timepoints: 12, 36, 60 months
#' - Marker: tumor_biomarker
#' - Event: death_event
#' - Time: follow_up_months
#'
#' @source Simulated data generated using create_timeroc_test_data.R
#' @seealso \code{\link{timeroc}}, \code{\link{timeroc_datasets}}
#'
#' @examples
#' \dontrun{
#' # Load the dataset
#' data(timeroc_cancer_biomarker)
#' 
#' # Basic time-dependent ROC analysis
#' result <- timeroc(
#'   data = timeroc_cancer_biomarker,
#'   elapsedtime = "follow_up_months",
#'   outcome = "death_event", 
#'   marker = "tumor_biomarker",
#'   timepoints = "12, 36, 60"
#' )
#' 
#' # View AUC results
#' result$aucTable$asDF
#' }
"timeroc_cancer_biomarker"

#' Cardiovascular Risk Prediction Test Dataset
#'
#' Simulated cardiovascular risk prediction study with multiple biomarkers
#' and clinical risk factors. Designed to test time-dependent ROC analysis
#' in the context of cardiovascular event prediction.
#'
#' @format A data frame with 400 observations and 12 variables:
#' \describe{
#'   \item{patient_id}{Character. Unique patient identifier (CV_0001 to CV_0400)}
#'   \item{age}{Integer. Patient age (30-85 years)}
#'   \item{sex}{Character. Patient sex ("Male", "Female")}
#'   \item{diabetes}{Integer. Diabetes status (1 = yes, 0 = no)}
#'   \item{hypertension}{Integer. Hypertension status (1 = yes, 0 = no)}
#'   \item{smoking_status}{Integer. Smoking status (1 = current smoker, 0 = not)}
#'   \item{troponin_level}{Numeric. Troponin biomarker level (log-normal distribution)}
#'   \item{crp_level}{Numeric. C-reactive protein level (log-normal distribution)}
#'   \item{risk_score}{Numeric. Composite cardiovascular risk score}
#'   \item{follow_up_months}{Numeric. Follow-up time in months (0-36)}
#'   \item{cv_event}{Integer. Cardiovascular event indicator (1 = event, 0 = censored)}
#'   \item{event_type}{Character. Type of cardiovascular event ("MI", "Stroke", "CHF", "Death", "None")}
#'   \item{study_site}{Character. Study recruitment site ("Site_A" to "Site_D")}
#' }
#'
#' @details
#' This dataset represents a prospective cardiovascular risk study with 36-month
#' follow-up. The dataset includes traditional risk factors (age, sex, diabetes,
#' hypertension, smoking) and novel biomarkers (troponin, CRP). The composite
#' risk score combines all risk factors.
#'
#' **Key Features:**
#' - Multiple biomarker comparison capability
#' - Traditional + novel risk factors
#' - Realistic biomarker distributions (log-normal)
#' - 375/400 events (93.8% event rate)
#' - Multi-site study design
#'
#' **Recommended TimeROC Parameters:**
#' - Timepoints: 6, 18, 36 months
#' - Markers: troponin_level, crp_level, risk_score
#' - Event: cv_event
#' - Time: follow_up_months
#'
#' @source Simulated data generated using create_timeroc_test_data.R
#' @seealso \code{\link{timeroc}}, \code{\link{timeroc_datasets}}
#'
#' @examples
#' \dontrun{
#' # Load the dataset
#' data(timeroc_cardiovascular_risk)
#' 
#' # Compare multiple biomarkers
#' troponin_roc <- timeroc(
#'   data = timeroc_cardiovascular_risk,
#'   elapsedtime = "follow_up_months",
#'   outcome = "cv_event",
#'   marker = "troponin_level",
#'   timepoints = "6, 18, 36"
#' )
#' 
#' risk_score_roc <- timeroc(
#'   data = timeroc_cardiovascular_risk,
#'   elapsedtime = "follow_up_months", 
#'   outcome = "cv_event",
#'   marker = "risk_score",
#'   timepoints = "6, 18, 36"
#' )
#' }
"timeroc_cardiovascular_risk"

#' Multi-Biomarker Comparison Test Dataset
#'
#' Specialized dataset with three biomarkers of varying predictive performance
#' designed to test comparative time-dependent ROC analysis. Each biomarker
#' has different signal-to-noise ratios representing excellent, good, and fair
#' predictive ability.
#'
#' @format A data frame with 250 observations and 10 variables:
#' \describe{
#'   \item{subject_id}{Character. Unique subject identifier (MB_001 to MB_250)}
#'   \item{age_years}{Integer. Subject age in years}
#'   \item{gender}{Character. Subject gender ("M", "F")}
#'   \item{biomarker_alpha}{Numeric. Excellent predictor biomarker (expected AUC ~0.85)}
#'   \item{biomarker_beta}{Numeric. Good predictor biomarker (expected AUC ~0.75)}
#'   \item{biomarker_gamma}{Numeric. Fair predictor biomarker (expected AUC ~0.65)}
#'   \item{composite_score}{Numeric. Weighted combination of alpha and beta biomarkers}
#'   \item{follow_up_months}{Numeric. Follow-up time in months (0-48)}
#'   \item{primary_event}{Integer. Primary endpoint event (1 = event, 0 = censored)}
#'   \item{cohort}{Character. Study cohort ("Training", "Validation")}
#'   \item{enrollment_year}{Integer. Year of enrollment (2018-2022)}
#' }
#'
#' @details
#' This dataset is specifically designed to test biomarker ranking and comparison
#' functionality in time-dependent ROC analysis. The three biomarkers have
#' systematically different predictive abilities:
#'
#' - **biomarker_alpha**: Strong signal (high correlation with outcome)
#' - **biomarker_beta**: Moderate signal (medium correlation with outcome)  
#' - **biomarker_gamma**: Weak signal (low correlation with outcome)
#' - **composite_score**: Combined alpha + beta for testing multi-marker models
#'
#' **Key Features:**
#' - Controlled predictive performance differences
#' - Training/validation split capability
#' - 249/250 events (99.6% event rate)
#' - Multi-year enrollment period
#' - Realistic biomarker scales and distributions
#'
#' **Recommended TimeROC Parameters:**
#' - Timepoints: 6, 12, 18 months
#' - Markers: biomarker_alpha, biomarker_beta, biomarker_gamma, composite_score
#' - Event: primary_event
#' - Time: follow_up_months
#'
#' @source Simulated data generated using create_timeroc_test_data.R
#' @seealso \code{\link{timeroc}}, \code{\link{timeroc_datasets}}
#'
#' @examples
#' \dontrun{
#' # Load the dataset
#' data(timeroc_multi_biomarker)
#' 
#' # Compare all biomarkers
#' markers <- c("biomarker_alpha", "biomarker_beta", "biomarker_gamma")
#' results <- list()
#' 
#' for(marker in markers) {
#'   results[[marker]] <- timeroc(
#'     data = timeroc_multi_biomarker,
#'     elapsedtime = "follow_up_months",
#'     outcome = "primary_event",
#'     marker = marker,
#'     timepoints = "6, 12, 18"
#'   )
#' }
#' 
#' # Extract AUC values for comparison
#' sapply(results, function(x) x$aucTable$asDF$auc)
#' }
"timeroc_multi_biomarker"

#' Landmark Biomarker Analysis Test Dataset
#'
#' Specialized dataset for landmark time-dependent ROC analysis where biomarker
#' values change over time. All patients survive to a landmark time (6 months)
#' and subsequent analysis is conditional on landmark survival.
#'
#' @format A data frame with 200 observations and 10 variables:
#' \describe{
#'   \item{patient_id}{Character. Unique patient identifier (LM_001 to LM_200)}
#'   \item{age}{Integer. Patient age}
#'   \item{baseline_biomarker}{Numeric. Biomarker level at study entry}
#'   \item{month6_biomarker}{Numeric. Biomarker level at 6-month landmark}
#'   \item{biomarker_change}{Numeric. Log-ratio change from baseline to 6 months}
#'   \item{total_follow_up_months}{Numeric. Total follow-up time from baseline (6-60 months)}
#'   \item{landmark_eligible}{Logical. All TRUE (all patients survived to landmark)}
#'   \item{post_landmark_event}{Integer. Event after landmark time (1 = event, 0 = censored)}
#'   \item{response_status}{Character. Treatment response ("Complete", "Partial", "Stable", "Progressive")}
#'   \item{treatment_arm}{Character. Treatment assignment ("Experimental", "Standard")}
#' }
#'
#' @details
#' This dataset supports landmark analysis methodology where time-dependent
#' biomarker values are assessed at a fixed landmark time (6 months) and
#' used to predict subsequent events. This approach is common in oncology
#' and transplantation studies.
#'
#' **Key Features:**
#' - All patients survive to landmark time (6 months)
#' - Time-varying biomarker measurements
#' - Biomarker change calculations (log-ratio)
#' - Treatment response classifications
#' - 48/200 post-landmark events (24% event rate)
#' - Randomized treatment arms
#'
#' **Recommended TimeROC Parameters:**
#' - Timepoints: 12, 24, 36 months (from baseline)
#' - Markers: month6_biomarker, biomarker_change
#' - Event: post_landmark_event  
#' - Time: total_follow_up_months
#'
#' @source Simulated data generated using create_timeroc_test_data.R
#' @seealso \code{\link{timeroc}}, \code{\link{timeroc_datasets}}
#'
#' @examples
#' \dontrun{
#' # Load the dataset
#' data(timeroc_landmark_biomarker)
#' 
#' # Landmark analysis using 6-month biomarker value
#' landmark_roc <- timeroc(
#'   data = timeroc_landmark_biomarker,
#'   elapsedtime = "total_follow_up_months",
#'   outcome = "post_landmark_event", 
#'   marker = "month6_biomarker",
#'   timepoints = "12, 24, 36"
#' )
#' 
#' # Analysis of biomarker change
#' change_roc <- timeroc(
#'   data = timeroc_landmark_biomarker,
#'   elapsedtime = "total_follow_up_months",
#'   outcome = "post_landmark_event",
#'   marker = "biomarker_change", 
#'   timepoints = "12, 24, 36"
#' )
#' }
"timeroc_landmark_biomarker"

#' Edge Cases and Quality Testing Dataset
#'
#' Specialized dataset containing various edge cases, outliers, and quality
#' issues designed to test the robustness of time-dependent ROC analysis
#' implementations. Includes extreme values, missing data, and challenging
#' scenarios.
#'
#' @format A data frame with 150 observations and 7 variables:
#' \describe{
#'   \item{id}{Character. Unique identifier (EC_001 to EC_150)}
#'   \item{age}{Integer. Patient age}
#'   \item{scenario}{Integer. Test scenario type (1-5)}
#'   \item{biomarker}{Numeric. Biomarker value with various data quality issues}
#'   \item{time_months}{Numeric. Follow-up time in months}
#'   \item{event_status}{Integer. Event indicator with missing values}
#'   \item{test_timepoints}{Character. Suggested timepoints for testing}
#' }
#'
#' @details
#' This dataset is designed to stress-test time-dependent ROC implementations
#' with various edge cases and data quality issues:
#'
#' **Scenario Types:**
#' - **Scenario 1**: Normal case (baseline comparison)
#' - **Scenario 2**: Very high biomarker values with short survival
#' - **Scenario 3**: Very low biomarker values with long survival  
#' - **Scenario 4**: Extreme outliers (values up to 1000x normal)
#' - **Scenario 5**: Very long follow-up times (rare events)
#'
#' **Quality Issues:**
#' - ~5% missing biomarker values
#' - Extreme outliers requiring robust handling
#' - Wide range of follow-up times
#' - 64/150 events (42.7% event rate)
#' - Various suggested timepoint specifications
#'
#' **Recommended TimeROC Parameters:**
#' - Timepoints: 6, 12, 18 months (test robustness)
#' - Marker: biomarker (with missing values and outliers)
#' - Event: event_status
#' - Time: time_months
#'
#' @source Simulated data generated using create_timeroc_test_data.R
#' @seealso \code{\link{timeroc}}, \code{\link{timeroc_datasets}}
#'
#' @examples
#' \dontrun{
#' # Load the dataset
#' data(timeroc_edge_cases)
#' 
#' # Test robustness with edge cases
#' edge_roc <- timeroc(
#'   data = timeroc_edge_cases,
#'   elapsedtime = "time_months",
#'   outcome = "event_status",
#'   marker = "biomarker",
#'   timepoints = "6, 12, 18"
#' )
#' 
#' # Examine data quality
#' summary(timeroc_edge_cases$biomarker)  # Check for outliers
#' table(timeroc_edge_cases$scenario)     # Distribution of scenarios
#' sum(is.na(timeroc_edge_cases$biomarker))  # Missing values
#' }
"timeroc_edge_cases"

#' Competing Risks Analysis Test Dataset  
#'
#' Simulated study with competing risks where patients can experience either
#' disease-specific death or death from other causes. Designed to test
#' time-dependent ROC analysis in competing risks settings.
#'
#' @format A data frame with 180 observations and 10 variables:
#' \describe{
#'   \item{patient_id}{Character. Unique patient identifier (CR_001 to CR_180)}
#'   \item{age}{Integer. Patient age}
#'   \item{comorbidity_score}{Integer. Comorbidity burden score (Poisson distributed)}
#'   \item{disease_biomarker}{Numeric. Disease-specific biomarker}
#'   \item{time_to_event_months}{Numeric. Time to first event (0-48 months)}
#'   \item{event_type}{Factor. Type of event ("Censored", "Disease_Death", "Other_Death")}
#'   \item{disease_death}{Integer. Disease-specific death indicator (1 = disease death, 0 = other)}
#'   \item{any_death}{Integer. Any death indicator (1 = any death, 0 = censored)}
#'   \item{performance_status}{Integer. Performance status (0-2)}
#'   \item{prior_treatment}{Integer. Prior treatment indicator (1 = yes, 0 = no)}
#' }
#'
#' @details
#' This dataset models a clinical scenario where patients face competing risks
#' of disease-specific mortality versus other-cause mortality. The disease
#' biomarker specifically predicts disease-related death, while age and
#' comorbidity predict competing mortality.
#'
#' **Competing Events:**
#' - **Disease Death**: Primary outcome of interest
#' - **Other Death**: Competing risk (age/comorbidity related)
#' - **Censored**: Administrative censoring at 48 months
#'
#' **Key Features:**
#' - True competing risks structure
#' - Biomarker specific to disease outcome
#' - Comorbidity effects on competing risk
#' - 64/180 disease-specific deaths (35.6% disease event rate)
#' - Multi-factorial risk structure
#'
#' **Recommended TimeROC Parameters:**
#' - Timepoints: 6, 24, 48 months
#' - Marker: disease_biomarker
#' - Event: disease_death (focus on disease-specific outcome)
#' - Time: time_to_event_months
#'
#' @source Simulated data generated using create_timeroc_test_data.R
#' @seealso \code{\link{timeroc}}, \code{\link{timeroc_datasets}}
#'
#' @examples
#' \dontrun{
#' # Load the dataset
#' data(timeroc_competing_risks)
#' 
#' # Time-dependent ROC for disease-specific mortality
#' disease_roc <- timeroc(
#'   data = timeroc_competing_risks,
#'   elapsedtime = "time_to_event_months",
#'   outcome = "disease_death",
#'   marker = "disease_biomarker", 
#'   timepoints = "6, 24, 48"
#' )
#' 
#' # Compare to any-death outcome
#' any_death_roc <- timeroc(
#'   data = timeroc_competing_risks,
#'   elapsedtime = "time_to_event_months",
#'   outcome = "any_death",
#'   marker = "disease_biomarker",
#'   timepoints = "6, 24, 48"
#' )
#' 
#' # Examine competing risks structure
#' table(timeroc_competing_risks$event_type)
#' }
"timeroc_competing_risks"

#' TimeROC Dataset Summary Information
#'
#' Summary table providing overview of all TimeROC test datasets including
#' sample sizes, descriptions, key features, and recommended analysis parameters.
#'
#' @format A data frame with 6 observations and 6 variables:
#' \describe{
#'   \item{Dataset}{Character. Dataset name}
#'   \item{Observations}{Integer. Number of observations}
#'   \item{Description}{Character. Brief dataset description}
#'   \item{Key_Features}{Character. Main features and characteristics}
#'   \item{Primary_Endpoint}{Character. Recommended primary endpoint variable}
#'   \item{Biomarker_Variable}{Character. Recommended biomarker variable(s)}
#' }
#'
#' @details
#' This summary table provides a quick reference for all TimeROC test datasets,
#' helping users select appropriate datasets for their testing needs and
#' understand the key characteristics of each dataset.
#'
#' @source Generated by create_timeroc_test_data.R
#' @seealso \code{\link{timeroc_datasets}}
"timeroc_datasets_summary"

#' TimeROC Test Scenarios Documentation
#'
#' Comprehensive testing scenarios designed to validate different aspects
#' of time-dependent ROC analysis functionality using the TimeROC test datasets.
#'
#' @format A data frame with 10 observations and 4 variables:
#' \describe{
#'   \item{Scenario}{Character. Name of the testing scenario}
#'   \item{Dataset}{Character. Recommended dataset for this scenario}
#'   \item{Expected_Result}{Character. Expected outcome of the analysis}
#'   \item{timepoints}{Character. Recommended timepoints for testing}
#' }
#'
#' @details
#' This documentation provides systematic testing scenarios covering:
#'
#' - Basic time-dependent ROC functionality
#' - Method comparison (incident vs cumulative vs static)
#' - Bootstrap confidence interval validation
#' - Optimal cutoff calculation testing
#' - Multi-biomarker comparison workflows
#' - Landmark analysis validation
#' - Edge case and robustness testing
#' - Competing risks analysis
#' - Clinical interpretation accuracy
#' - Publication-ready visualization quality
#'
#' Each scenario includes the recommended dataset, expected results, and
#' suggested timepoints for comprehensive testing.
#'
#' @source Generated by create_timeroc_test_data.R
#' @seealso \code{\link{timeroc_datasets}}, \code{\link{timeroc}}
"timeroc_test_scenarios"