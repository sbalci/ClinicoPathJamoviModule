#' Enhanced Tools for Data Summary Test Datasets
#'
#' Comprehensive collection of test datasets designed for validating enhanced data summary
#' functions with summarytools integration. Each dataset represents different research scenarios,
#' data types, and analysis challenges commonly encountered in clinical and scientific research,
#' specifically designed to test dfSummary, freq, descr, and ctable functionality.
#'
#' @name toolssummary_datasets
#' @docType data
#' @usage data(toolssummary_clinical_demographics)
#' @usage data(toolssummary_laboratory_results)
#' @usage data(toolssummary_mixed_datatypes)
#' @usage data(toolssummary_timeseries_data)
#' @usage data(toolssummary_edge_cases)
#' @usage data(toolssummary_small_sample)
NULL

#' Clinical Research Demographics with Treatment Groups
#'
#' Simulated clinical research study demographics dataset with multiple treatment groups,
#' comprehensive clinical variables, and realistic missing data patterns. Designed to test
#' demographic table generation, grouped summaries, cross-tabulations, and publication-ready
#' formatting typical in clinical research using summarytools enhanced features.
#'
#' @format A data frame with 300 observations and 16 variables:
#' \describe{
#'   \item{patient_id}{Character. Unique patient identifier (PT_0001 to PT_0300)}
#'   \item{age}{Integer. Patient age at enrollment (18-85 years)}
#'   \item{sex}{Factor. Patient sex ("Male", "Female")}
#'   \item{treatment_group}{Factor. Treatment assignment ("Control", "Treatment A", "Treatment B")}
#'   \item{study_site}{Factor. Study enrollment site ("Site_A" to "Site_F")}
#'   \item{bmi}{Numeric. Body mass index (16-45 kg/m²) with ~5% missing values}
#'   \item{systolic_bp}{Integer. Systolic blood pressure (90-200 mmHg)}
#'   \item{diastolic_bp}{Integer. Diastolic blood pressure (60-120 mmHg)}
#'   \item{diabetes}{Factor. Diabetes status ("No", "Type 1", "Type 2")}
#'   \item{smoking_status}{Factor. Smoking history ("Never", "Former", "Current")}
#'   \item{education}{Ordered Factor. Education level ("Less than HS" < "High School" < "Some College" < "Bachelor's" < "Graduate")}
#'   \item{hemoglobin}{Numeric. Hemoglobin level (8-18 g/dL) with ~3% missing values}
#'   \item{glucose}{Integer. Fasting glucose (70-400 mg/dL)}
#'   \item{cholesterol}{Integer. Total cholesterol (120-350 mg/dL) with ~8% missing values}
#'   \item{enrollment_date}{Date. Study enrollment date (2023-01-01 to 2024-12-31)}
#'   \item{followup_months}{Integer. Follow-up duration (1-24 months)}
#' }
#'
#' @details
#' This dataset simulates a comprehensive clinical research study baseline characteristics
#' table with realistic distributions and appropriate missing data patterns. It's specifically
#' designed to showcase summarytools capabilities including dfSummary comprehensive overviews,
#' freq tables for categorical variables, descr for numeric summaries, and ctable for 
#' cross-tabulations by treatment groups.
#'
#' **Key Features:**
#' - Realistic clinical variable distributions with physiological ranges
#' - Balanced treatment groups for meaningful comparisons
#' - Multiple categorical variables for frequency analysis
#' - Ordered factors for proper categorical handling
#' - Missing data patterns reflecting real clinical studies
#' - Date variables for temporal analysis
#'
#' **summarytools Integration Testing:**
#' - **dfSummary**: Complete data frame overview with variable distributions
#' - **freq**: Categorical variable frequency tables (sex, treatment_group, diabetes)
#' - **descr**: Comprehensive descriptive statistics for numeric variables
#' - **ctable**: Cross-tabulations by treatment group or study site
#'
#' **Recommended Usage Scenarios:**
#' - Grouped summaries by treatment_group or study_site
#' - Cross-tabulation analysis for categorical associations
#' - Missing data pattern assessment
#' - Publication-ready demographic tables
#'
#' @source Simulated data generated using create_toolssummary_test_data.R
#' @seealso \code{\link{toolssummary}}, \code{\link{toolssummary_datasets}}
#'
#' @examples
#' \dontrun{
#' # Load the dataset
#' data(toolssummary_clinical_demographics)
#' 
#' # Basic enhanced summary with summarytools
#' result <- toolssummary(
#'   data = toolssummary_clinical_demographics,
#'   vars = c("age", "sex", "bmi", "diabetes", "treatment_group"),
#'   useSummarytools = TRUE,
#'   showDfSummary = TRUE,
#'   showDescr = TRUE,
#'   showFreq = TRUE
#' )
#' 
#' # Grouped analysis by treatment
#' result_grouped <- toolssummary(
#'   data = toolssummary_clinical_demographics,
#'   vars = c("age", "bmi", "systolic_bp", "diabetes"),
#'   groupVar = "treatment_group",
#'   useSummarytools = TRUE,
#'   showCrosstabs = TRUE
#' )
#' }
"toolssummary_clinical_demographics"

#' Laboratory Results with Longitudinal Structure
#'
#' Simulated laboratory test results dataset with multiple visit timepoints, comprehensive
#' lab panels, and clinical categories. Designed to test descriptive statistics tables,
#' visit-based grouping, longitudinal data presentation, and laboratory data summaries
#' using summarytools enhanced functionality.
#'
#' @format A data frame with 200 observations and 15 variables:
#' \describe{
#'   \item{subject_id}{Character. Unique subject identifier (LAB_0001 to LAB_0050, repeated)}
#'   \item{visit}{Factor. Study visit ("Baseline", "Week 4", "Week 8", "Week 12")}
#'   \item{visit_date}{Date. Date of laboratory assessment}
#'   \item{lab_category}{Factor. Clinical interpretation ("Normal", "Borderline", "Abnormal")}
#'   \item{urgency}{Factor. Test urgency level ("Routine", "STAT", "Priority")}
#'   \item{wbc}{Numeric. White blood cell count (2.0-15.0 × 10³/μL)}
#'   \item{rbc}{Numeric. Red blood cell count (3.5-6.0 × 10⁶/μL)}
#'   \item{hematocrit}{Numeric. Hematocrit percentage (30-55%)}
#'   \item{platelets}{Integer. Platelet count (100-500 × 10³/μL)}
#'   \item{sodium}{Integer. Serum sodium (130-150 mEq/L)}
#'   \item{potassium}{Numeric. Serum potassium (3.0-5.5 mEq/L)}
#'   \item{creatinine}{Numeric. Serum creatinine (0.5-3.0 mg/dL) with ~4% missing}
#'   \item{bun}{Integer. Blood urea nitrogen (7-50 mg/dL)}
#'   \item{alt}{Integer. Alanine aminotransferase (5-100 U/L) with ~2% missing}
#'   \item{ast}{Integer. Aspartate aminotransferase (5-120 U/L)}
#' }
#'
#' @details
#' This dataset represents laboratory test results from a longitudinal clinical study
#' with 50 subjects followed across 4 timepoints. Laboratory values are within realistic
#' clinical ranges with appropriate inter-test correlations and visit-to-visit variability.
#' Perfect for testing summarytools capabilities with repeated measures data.
#'
#' **Key Features:**
#' - Complete blood count (CBC) and chemistry panels
#' - Longitudinal structure with 4 visit timepoints
#' - Clinical categorization and urgency levels
#' - Realistic laboratory value ranges and correlations
#' - Date variables for temporal analysis
#' - Missing data patterns reflecting real laboratory studies
#'
#' **summarytools Integration Testing:**
#' - **dfSummary**: Laboratory panel overview with value distributions
#' - **freq**: Categorical analysis of lab categories and urgency levels
#' - **descr**: Comprehensive statistics for all laboratory values
#' - **ctable**: Cross-tabulations by visit or clinical categories
#'
#' **Recommended Usage Scenarios:**
#' - Laboratory value summaries by visit timepoint
#' - Normal vs abnormal result analysis
#' - Longitudinal laboratory trend assessment
#' - Clinical decision support data presentation
#'
#' @source Simulated data generated using create_toolssummary_test_data.R
#' @seealso \code{\link{toolssummary}}, \code{\link{toolssummary_datasets}}
#'
#' @examples
#' \dontrun{
#' # Load the dataset
#' data(toolssummary_laboratory_results)
#' 
#' # Laboratory values summary with enhanced statistics
#' result <- toolssummary(
#'   data = toolssummary_laboratory_results,
#'   vars = c("wbc", "rbc", "sodium", "potassium", "alt"),
#'   useSummarytools = TRUE,
#'   showDescr = TRUE,
#'   showDfSummary = TRUE
#' )
#' 
#' # Analysis by visit timepoint
#' result_visit <- toolssummary(
#'   data = toolssummary_laboratory_results,
#'   vars = c("wbc", "hematocrit", "platelets", "creatinine"),
#'   groupVar = "visit",
#'   useSummarytools = TRUE,
#'   showCrosstabs = TRUE
#' )
#' }
"toolssummary_laboratory_results"

#' Mixed Data Types with Complex Categorical Variables
#'
#' Comprehensive dataset containing continuous, ordinal, categorical, and date variables
#' with complex missing patterns and diverse scales. Designed to test enhanced table
#' formatting capabilities with mixed variable types, ordered factors, and sophisticated
#' categorical analysis using summarytools comprehensive functionality.
#'
#' @format A data frame with 250 observations and 15 variables:
#' \describe{
#'   \item{record_id}{Integer. Record identifier (1-250)}
#'   \item{data_source}{Factor. Data origin ("EHR", "Registry", "Clinical_Trial", "Survey")}
#'   \item{region}{Factor. Geographic region ("North", "South", "East", "West", "Central")}
#'   \item{score_continuous}{Numeric. Continuous scale score (0-100) with 1 decimal place}
#'   \item{score_ordinal}{Integer. Ordinal rating scale (1-5) with ~8% missing}
#'   \item{measurement_value}{Numeric. Measurement in arbitrary units (50-300) with ~5% missing}
#'   \item{severity}{Ordered Factor. Severity level ("Mild" < "Moderate" < "Severe")}
#'   \item{priority_level}{Ordered Factor. Priority ("Low" < "Medium" < "High" < "Critical")}
#'   \item{binary_flag}{Factor. Binary indicator ("Yes", "No")}
#'   \item{quality_status}{Factor. Quality assessment ("Pass", "Fail", "Pending")}
#'   \item{category_multi}{Factor. Multi-level category ("Type_A" to "Type_H")}
#'   \item{assessment_date}{Date. Assessment date (2024-01-01 to 2024-12-31)}
#'   \item{completion_date}{Date. Completion date (2024-02-01 to 2025-01-31) with ~12% missing}
#'   \item{count_variable}{Integer. Count data (0-20)}
#'   \item{percentage_score}{Numeric. Percentage score (0-100) with 1 decimal place}
#' }
#'
#' @details
#' This dataset is specifically designed to test summarytools ability to handle diverse
#' data types in a single comprehensive analysis. It includes variables with different
#' scales, measurement levels, and complex categorical structures that challenge
#' standard summary approaches.
#'
#' **Key Features:**
#' - Multiple continuous scales with different ranges and precisions
#' - Properly ordered factors with meaningful level hierarchies
#' - Binary and multi-category nominal factors
#' - Date variables for temporal analysis
#' - Complex missing data patterns across variable types
#' - Count data and percentage scores
#'
#' **summarytools Integration Testing:**
#' - **dfSummary**: Comprehensive overview handling all data types
#' - **freq**: Advanced frequency analysis for categorical and ordinal variables
#' - **descr**: Extended descriptive statistics for continuous variables
#' - **ctable**: Cross-tabulations across different categorical structures
#'
#' **Recommended Usage Scenarios:**
#' - Mixed data type comprehensive summaries
#' - Ordered factor analysis and presentation
#' - Multi-source data integration assessment
#' - Quality control and data validation
#'
#' @source Simulated data generated using create_toolssummary_test_data.R
#' @seealso \code{\link{toolssummary}}, \code{\link{toolssummary_datasets}}
#'
#' @examples
#' \dontrun{
#' # Load the dataset
#' data(toolssummary_mixed_datatypes)
#' 
#' # Comprehensive mixed data analysis
#' result <- toolssummary(
#'   data = toolssummary_mixed_datatypes,
#'   vars = c("score_continuous", "severity", "priority_level", "binary_flag"),
#'   useSummarytools = TRUE,
#'   showDfSummary = TRUE,
#'   showDescr = TRUE,
#'   showFreq = TRUE
#' )
#' 
#' # Analysis by data source
#' result_source <- toolssummary(
#'   data = toolssummary_mixed_datatypes,
#'   vars = c("score_continuous", "measurement_value", "severity"),
#'   groupVar = "data_source",
#'   useSummarytools = TRUE,
#'   showCrosstabs = TRUE
#' )
#' }
"toolssummary_mixed_datatypes"

#' Time Series Data for Longitudinal Analysis
#'
#' Longitudinal dataset with repeated measures per subject across multiple timepoints,
#' realistic dropout patterns, and outcome tracking. Designed to test time-based summaries,
#' longitudinal data presentation, missing data patterns over time, and grouped analysis
#' capabilities using summarytools enhanced features.
#'
#' @format A data frame with 200 observations and 12 variables:
#' \describe{
#'   \item{subject_id}{Character. Subject identifier (TS_001 to TS_040)}
#'   \item{timepoint}{Factor. Assessment timepoint ("T1", "T2", "T3", "T4", "T5")}
#'   \item{months_from_baseline}{Integer. Time since baseline (0, 3, 6, 12, 24 months)}
#'   \item{assessment_date}{Date. Date of assessment}
#'   \item{primary_outcome}{Numeric. Primary outcome score (0-100) with time-dependent missing}
#'   \item{secondary_outcome_1}{Numeric. Secondary outcome correlated with primary}
#'   \item{secondary_outcome_2}{Numeric. Independent secondary outcome}
#'   \item{response_status}{Factor. Treatment response ("Responder", "Non-responder")}
#'   \item{compliance_percent}{Numeric. Treatment compliance percentage (75-100%)}
#'   \item{dose_level}{Ordered Factor. Dose level ("Low" < "Medium" < "High")}
#'   \item{adverse_events}{Integer. Count of adverse events (0-5)}
#'   \item{biomarker_level}{Numeric. Biomarker measurement (log-normal distribution)}
#' }
#'
#' @details
#' This dataset represents a longitudinal clinical study with 40 subjects followed
#' over 5 timepoints (24 months total). It includes realistic patterns of outcome
#' changes, dropout over time, and missing data that increase with follow-up duration,
#' making it ideal for testing summarytools longitudinal capabilities.
#'
#' **Key Features:**
#' - 40 subjects with up to 5 timepoints each (200 total observations)
#' - Time-dependent outcome patterns and trends
#' - Realistic dropout patterns (increasing missing data over time)
#' - Multiple correlated and independent outcomes
#' - Compliance and adverse event tracking
#' - Response status categorization
#'
#' **summarytools Integration Testing:**
#' - **dfSummary**: Longitudinal data overview with temporal patterns
#' - **freq**: Time-based frequency analysis and response categorization
#' - **descr**: Outcome statistics across timepoints with trend assessment
#' - **ctable**: Cross-tabulations by timepoint and response status
#'
#' **Recommended Usage Scenarios:**
#' - Longitudinal outcome analysis by timepoint
#' - Dropout pattern assessment over time
#' - Treatment response analysis
#' - Compliance and safety monitoring
#'
#' @source Simulated data generated using create_toolssummary_test_data.R
#' @seealso \code{\link{toolssummary}}, \code{\link{toolssummary_datasets}}
#'
#' @examples
#' \dontrun{
#' # Load the dataset
#' data(toolssummary_timeseries_data)
#' 
#' # Longitudinal outcomes analysis
#' result <- toolssummary(
#'   data = toolssummary_timeseries_data,
#'   vars = c("primary_outcome", "secondary_outcome_1", "compliance_percent"),
#'   useSummarytools = TRUE,
#'   showDescr = TRUE,
#'   showDfSummary = TRUE
#' )
#' 
#' # Analysis by timepoint
#' result_time <- toolssummary(
#'   data = toolssummary_timeseries_data,
#'   vars = c("primary_outcome", "response_status", "adverse_events"),
#'   groupVar = "timepoint",
#'   useSummarytools = TRUE,
#'   showCrosstabs = TRUE
#' )
#' }
"toolssummary_timeseries_data"

#' Edge Cases and Robustness Testing Dataset
#'
#' Specialized dataset containing various edge cases, extreme values, special characters,
#' and challenging data patterns. Designed to test the robustness of enhanced summary
#' implementations, error handling capabilities, and graceful degradation with problematic
#' data using summarytools comprehensive error management.
#'
#' @format A data frame with 150 observations and 12 variables:
#' \describe{
#'   \item{id}{Character. Unique identifier (EDGE_001 to EDGE_150)}
#'   \item{scenario}{Factor. Test scenario type (1-6)}
#'   \item{numeric_extreme}{Numeric. Variable with extreme values, zeros, and systematic missing}
#'   \item{numeric_decimal}{Numeric. Variable with high-precision decimals (8 decimal places)}
#'   \item{categorical_many}{Factor. Categorical variable with 30 different levels}
#'   \item{categorical_few}{Factor. Binary categorical variable ("A", "B")}
#'   \item{categorical_special}{Factor. Categories with missing-like values ("N/A", "Missing", "")}
#'   \item{constant_numeric}{Numeric. Constant value (all 42) for edge case testing}
#'   \item{constant_factor}{Factor. Constant factor (all "SAME") for edge case testing}
#'   \item{text_variable}{Character. Text with varying lengths and special characters}
#'   \item{date_variable}{Date. Date variable spanning 2020-2025}
#'   \item{binary_numeric}{Integer. Binary numeric variable (0, 1)}
#' }
#'
#' @details
#' This dataset is designed to stress-test enhanced summary implementations with
#' various edge cases and challenging data patterns that might cause failures
#' in standard analysis approaches. It tests summarytools robustness and error
#' handling capabilities.
#'
#' **Scenario Types:**
#' - **Scenario 1**: Normal case (baseline comparison)
#' - **Scenario 2**: Extreme values (very large positive and negative numbers)
#' - **Scenario 3**: Very small values (near-zero decimals)
#' - **Scenario 4**: High precision decimals and complex numbers
#' - **Scenario 5**: Zero and edge numeric values
#' - **Scenario 6**: Systematic missing values
#'
#' **Quality Challenges:**
#' - Extreme numeric ranges and precision requirements
#' - Categorical variables with many levels (30 categories)
#' - Special characters and empty strings in categorical data
#' - Systematic missing patterns (every 6th and 7th observation)
#' - Constant values across all observations
#' - Text variables with varying lengths and special characters
#'
#' **summarytools Integration Testing:**
#' - **dfSummary**: Robustness with problematic data structures
#' - **freq**: Handling of many categories and special characters
#' - **descr**: Extreme value processing and missing data management
#' - **ctable**: Edge case cross-tabulation scenarios
#'
#' **Recommended Usage Scenarios:**
#' - Robustness testing for all summary functions
#' - Error handling validation
#' - Special character and encoding testing
#' - Missing data pattern analysis
#'
#' @source Simulated data generated using create_toolssummary_test_data.R
#' @seealso \code{\link{toolssummary}}, \code{\link{toolssummary_datasets}}
#'
#' @examples
#' \dontrun{
#' # Load the dataset
#' data(toolssummary_edge_cases)
#' 
#' # Robustness testing with edge cases
#' result <- toolssummary(
#'   data = toolssummary_edge_cases,
#'   vars = c("numeric_extreme", "categorical_many", "text_variable"),
#'   useSummarytools = TRUE,
#'   showDfSummary = TRUE,
#'   showFreq = TRUE
#' )
#' 
#' # Scenario-based analysis
#' result_scenario <- toolssummary(
#'   data = toolssummary_edge_cases,
#'   vars = c("numeric_extreme", "categorical_special"),
#'   groupVar = "scenario",
#'   useSummarytools = TRUE,
#'   showCrosstabs = TRUE
#' )
#' }
"toolssummary_edge_cases"

#' Small Sample Size Testing Dataset
#'
#' Minimal dataset with very small sample size to test edge cases with limited data,
#' small group sizes, and basic functionality with minimal observations. Designed to
#' validate graceful handling of small samples and ensure summarytools functions
#' work appropriately with limited data scenarios.
#'
#' @format A data frame with 20 observations and 7 variables:
#' \describe{
#'   \item{id}{Integer. Simple identifier (1-20)}
#'   \item{group}{Factor. Three-level grouping variable ("A", "B", "C")}
#'   \item{value_numeric}{Numeric. Primary numeric variable with 1 missing value}
#'   \item{value_integer}{Integer. Integer variable (1-100 range)}
#'   \item{category_binary}{Factor. Binary categorical variable ("Yes", "No")}
#'   \item{category_small}{Factor. Three-level categorical variable ("X", "Y", "Z")}
#'   \item{score}{Numeric. Score variable (0-10 range) with 1 decimal place}
#' }
#'
#' @details
#' This minimal dataset tests enhanced summary functionality with very small sample sizes,
#' which can reveal edge cases in statistical calculations, grouping operations, and
#' summary presentation algorithms. It validates summarytools behavior with limited data.
#'
#' **Key Features:**
#' - Only 20 observations total for minimal data testing
#' - Simple variable structure with basic data types
#' - Single missing value for missing data handling
#' - Small group sizes when stratified (6-7 observations per group)
#' - Basic data types only (numeric, integer, factor)
#'
#' **Common Use Cases:**
#' - Testing minimum sample size handling
#' - Validating small group statistics
#' - Edge case detection in grouping algorithms
#' - Minimum viable summary generation
#'
#' **summarytools Integration Testing:**
#' - **dfSummary**: Minimal data overview and visualization
#' - **freq**: Small sample frequency analysis
#' - **descr**: Descriptive statistics with limited observations
#' - **ctable**: Cross-tabulation with small cell counts
#'
#' **Recommended Usage Scenarios:**
#' - Small sample behavior validation
#' - Edge case testing for grouping operations
#' - Minimum data requirements assessment
#' - Quality control for small datasets
#'
#' @source Simulated data generated using create_toolssummary_test_data.R
#' @seealso \code{\link{toolssummary}}, \code{\link{toolssummary_datasets}}
#'
#' @examples
#' \dontrun{
#' # Load the dataset
#' data(toolssummary_small_sample)
#' 
#' # Basic small sample analysis
#' result <- toolssummary(
#'   data = toolssummary_small_sample,
#'   vars = c("value_numeric", "category_binary", "score"),
#'   useSummarytools = TRUE,
#'   showDfSummary = TRUE
#' )
#' 
#' # Small group analysis
#' result_grouped <- toolssummary(
#'   data = toolssummary_small_sample,
#'   vars = c("value_numeric", "score"),
#'   groupVar = "group",
#'   useSummarytools = TRUE,
#'   showCrosstabs = TRUE
#' )
#' }
"toolssummary_small_sample"

#' Enhanced Tools for Data Summary Dataset Collection Summary
#'
#' Summary table providing comprehensive overview of all enhanced toolssummary test datasets
#' including sample sizes, variable counts, descriptions, key features, summarytools
#' integration capabilities, and recommended usage scenarios for testing enhanced
#' data summary functionality.
#'
#' @format A data frame with 6 observations and 7 variables:
#' \describe{
#'   \item{Dataset}{Character. Dataset name}
#'   \item{Observations}{Integer. Number of observations}
#'   \item{Variables}{Integer. Number of variables}
#'   \item{Description}{Character. Brief dataset description}
#'   \item{Key_Features}{Character. Main features and characteristics}
#'   \item{Primary_Use_Case}{Character. Primary testing scenario}
#'   \item{Recommended_summarytools_Features}{Character. Suggested summarytools functions for testing}
#' }
#'
#' @details
#' This summary table provides a quick reference for all enhanced toolssummary test datasets,
#' helping users select appropriate datasets for their testing needs and understand
#' the summarytools integration capabilities of each dataset.
#'
#' **Total Collection Statistics:**
#' - 6 comprehensive test datasets
#' - 1,120 total observations across all datasets
#' - 77 total variables covering all major data types
#' - Complete summarytools feature coverage (dfSummary, freq, descr, ctable)
#'
#' @source Generated by create_toolssummary_test_data.R
#' @seealso \code{\link{toolssummary_datasets}}, \code{\link{toolssummary}}
"toolssummary_datasets_summary"

#' Enhanced Tools for Data Summary Test Scenarios Documentation
#'
#' Comprehensive testing scenarios designed to validate different aspects of enhanced
#' data summary functionality using summarytools integration with the toolssummary
#' test datasets. Each scenario targets specific summarytools features and analysis
#' capabilities.
#'
#' @format A data frame with 14 observations and 5 variables:
#' \describe{
#'   \item{Scenario}{Character. Name of the testing scenario}
#'   \item{Dataset}{Character. Recommended dataset for this scenario}
#'   \item{summarytools_Feature}{Character. Primary summarytools feature being tested}
#'   \item{Variables}{Character. Suggested variables for testing}
#'   \item{Expected_Result}{Character. Expected outcome of the analysis}
#' }
#'
#' @details
#' This documentation provides systematic testing scenarios covering all major
#' summarytools features and enhanced data summary capabilities:
#'
#' **Core summarytools Features Tested:**
#' - **dfSummary**: Comprehensive data frame overviews with plots and statistics
#' - **freq**: Enhanced frequency tables with cumulative frequencies and formatting
#' - **descr**: Extended descriptive statistics with additional measures
#' - **ctable**: Cross-tabulation tables with chi-square tests and proportions
#'
#' **Advanced Scenarios Covered:**
#' - Basic data summary functionality
#' - Clinical demographics table generation
#' - Laboratory results analysis and presentation
#' - Grouped analysis by categorical variables
#' - Mixed data type handling and formatting
#' - Longitudinal data analysis across timepoints
#' - Cross-tabulation analysis with statistical tests
#' - Missing data assessment and reporting
#' - Edge case robustness testing
#' - Small sample size behavior validation
#' - Multi-format output consistency testing
#' - Error handling and graceful degradation
#'
#' Each scenario includes the recommended dataset, target summarytools features,
#' suggested variables, and expected results for comprehensive validation.
#'
#' @source Generated by create_toolssummary_test_data.R
#' @seealso \code{\link{toolssummary_datasets}}, \code{\link{toolssummary}}
"toolssummary_test_scenarios"