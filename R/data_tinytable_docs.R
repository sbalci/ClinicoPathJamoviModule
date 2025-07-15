#' TinyTable Test Datasets
#'
#' Comprehensive collection of test datasets designed for validating modern table
#' formatting functions using the tinytable package. Each dataset represents different
#' clinical research scenarios, data types, and table formatting challenges commonly
#' encountered in data presentation and publication.
#'
#' @name tinytable_datasets
#' @docType data
#' @usage data(tinytable_clinical_demographics)
#' @usage data(tinytable_laboratory_results)
#' @usage data(tinytable_multimodal_summary)
#' @usage data(tinytable_timeseries_summary)
#' @usage data(tinytable_edge_cases)
#' @usage data(tinytable_small_sample)
NULL

#' Clinical Research Demographics Test Dataset
#'
#' Simulated clinical research study demographics dataset with treatment groups,
#' clinical variables, and laboratory values. Designed to test demographic table
#' generation, grouped summaries, and publication-ready formatting typical in
#' clinical research publications.
#'
#' @format A data frame with 250 observations and 14 variables:
#' \describe{
#'   \item{patient_id}{Character. Unique patient identifier (PT_001 to PT_250)}
#'   \item{age}{Integer. Patient age at enrollment (18-90 years)}
#'   \item{sex}{Factor. Patient sex ("Male", "Female")}
#'   \item{treatment_group}{Factor. Treatment assignment ("Control", "Treatment A", "Treatment B")}
#'   \item{study_site}{Factor. Study enrollment site ("Site_A" to "Site_F")}
#'   \item{bmi}{Numeric. Body mass index (16-45 kg/m²) with ~3% missing values}
#'   \item{systolic_bp}{Integer. Systolic blood pressure (90-200 mmHg)}
#'   \item{diastolic_bp}{Integer. Diastolic blood pressure (60-120 mmHg)}
#'   \item{diabetes}{Factor. Diabetes status ("No", "Type 1", "Type 2")}
#'   \item{smoking_status}{Factor. Smoking history ("Never", "Former", "Current")}
#'   \item{education_level}{Factor. Education ("Less than HS", "High School", "Some College", "Bachelor's", "Graduate")}
#'   \item{hemoglobin}{Numeric. Hemoglobin level (8-18 g/dL) with sex-based differences}
#'   \item{glucose}{Integer. Fasting glucose (70-400 mg/dL)}
#'   \item{cholesterol}{Integer. Total cholesterol (120-350 mg/dL) with ~5% missing values}
#' }
#'
#' @details
#' This dataset simulates a typical clinical research study baseline characteristics
#' table. It includes realistic distributions for demographic and clinical variables
#' commonly reported in medical publications, with appropriate missing data patterns
#' and clinical correlations.
#'
#' **Key Features:**
#' - Realistic clinical variable distributions
#' - Multiple treatment groups for comparison tables
#' - Sex-specific laboratory value ranges
#' - Appropriate missing data patterns (3-5%)
#' - Multiple study sites for subgroup analysis
#'
#' **Recommended TinyTable Usage:**
#' - Table Type: "Grouped Summary" or "Descriptive Statistics"
#' - Grouping Variable: treatment_group or study_site
#' - Variables: age, sex, bmi, systolic_bp, diabetes, smoking_status
#' - Themes: "Clinical" or "Publication" for professional appearance
#'
#' @source Simulated data generated using create_tinytable_test_data.R
#' @seealso \code{\link{tinytable}}, \code{\link{tinytable_datasets}}
#'
#' @examples
#' \dontrun{
#' # Load the dataset
#' data(tinytable_clinical_demographics)
#' 
#' # Basic demographic summary table
#' result <- tinytable(
#'   data = tinytable_clinical_demographics,
#'   vars = c("age", "sex", "bmi", "diabetes"),
#'   table_type = "summary",
#'   table_theme = "clinical"
#' )
#' 
#' # Grouped comparison by treatment
#' result_grouped <- tinytable(
#'   data = tinytable_clinical_demographics,
#'   vars = c("age", "bmi", "systolic_bp", "hemoglobin"),
#'   group_var = "treatment_group",
#'   table_type = "grouped",
#'   table_theme = "publication"
#' )
#' }
"tinytable_clinical_demographics"

#' Laboratory Results Test Dataset
#'
#' Simulated laboratory test results with multiple visit timepoints, test panels,
#' and clinical categories. Designed to test descriptive statistics tables,
#' visit-based grouping, and laboratory data presentation formats.
#'
#' @format A data frame with 180 observations and 15 variables:
#' \describe{
#'   \item{subject_id}{Character. Unique subject identifier (LAB_0001 to LAB_0180)}
#'   \item{visit}{Factor. Study visit ("Baseline", "Week 4", "Week 8", "Week 12")}
#'   \item{visit_date}{Date. Date of laboratory assessment}
#'   \item{lab_category}{Factor. Clinical interpretation ("Normal", "Borderline", "Abnormal")}
#'   \item{urgency}{Factor. Test urgency ("Routine", "STAT", "Priority")}
#'   \item{wbc}{Numeric. White blood cell count (2.0-15.0 × 10³/μL)}
#'   \item{rbc}{Numeric. Red blood cell count (3.5-6.0 × 10⁶/μL)}
#'   \item{hematocrit}{Numeric. Hematocrit percentage (30-55%)}
#'   \item{platelets}{Integer. Platelet count (100-500 × 10³/μL)}
#'   \item{sodium}{Integer. Serum sodium (130-150 mEq/L)}
#'   \item{potassium}{Numeric. Serum potassium (3.0-5.5 mEq/L)}
#'   \item{creatinine}{Numeric. Serum creatinine (0.5-3.0 mg/dL)}
#'   \item{bun}{Integer. Blood urea nitrogen (7-50 mg/dL)}
#'   \item{alt}{Integer. Alanine aminotransferase (5-100 U/L) with ~2% missing values}
#'   \item{ast}{Integer. Aspartate aminotransferase (5-120 U/L)}
#' }
#'
#' @details
#' This dataset represents laboratory test results from a longitudinal clinical study
#' with multiple visit timepoints. Laboratory values are within realistic clinical
#' ranges with appropriate inter-test correlations and visit-to-visit variability.
#'
#' **Key Features:**
#' - Complete blood count (CBC) panel
#' - Basic metabolic panel (chemistry)
#' - Liver function tests
#' - Multiple visit timepoints for longitudinal analysis
#' - Clinical categorization of results
#' - Date variables for temporal analysis
#'
#' **Recommended TinyTable Usage:**
#' - Table Type: "Descriptive Statistics" for laboratory value summaries
#' - Grouping Variable: visit or lab_category
#' - Variables: wbc, rbc, sodium, potassium, alt, ast
#' - Themes: "Clinical" for medical context
#'
#' @source Simulated data generated using create_tinytable_test_data.R
#' @seealso \code{\link{tinytable}}, \code{\link{tinytable_datasets}}
#'
#' @examples
#' \dontrun{
#' # Load the dataset
#' data(tinytable_laboratory_results)
#' 
#' # Laboratory values descriptive statistics
#' result <- tinytable(
#'   data = tinytable_laboratory_results,
#'   vars = c("wbc", "rbc", "sodium", "potassium"),
#'   table_type = "descriptive",
#'   table_theme = "clinical",
#'   precision_digits = 2
#' )
#' 
#' # Laboratory results by visit
#' result_visit <- tinytable(
#'   data = tinytable_laboratory_results,
#'   vars = c("wbc", "hemoglobin", "platelets"),
#'   group_var = "visit",
#'   table_type = "grouped"
#' )
#' }
"tinytable_laboratory_results"

#' Multi-Modal Data Summary Test Dataset
#'
#' Mixed data types dataset with continuous, ordinal, categorical, and date variables.
#' Designed to test comprehensive table formatting with diverse variable types,
#' complex missing patterns, and mixed-scale measurements.
#'
#' @format A data frame with 200 observations and 13 variables:
#' \describe{
#'   \item{record_id}{Integer. Record identifier (1-200)}
#'   \item{data_source}{Factor. Data origin ("EHR", "Registry", "Clinical_Trial", "Survey")}
#'   \item{region}{Factor. Geographic region ("North", "South", "East", "West", "Central")}
#'   \item{score_1}{Numeric. Scale score 0-100 with 1 decimal place}
#'   \item{score_2}{Numeric. Rating scale 1-5 with 2 decimal places and ~8% missing}
#'   \item{measurement_a}{Numeric. Measurement in arbitrary units (50-300) with ~4% missing}
#'   \item{severity}{Ordered Factor. Severity level ("Mild" < "Moderate" < "Severe")}
#'   \item{priority}{Ordered Factor. Priority level ("Low" < "Medium" < "High" < "Critical")}
#'   \item{flag_positive}{Factor. Binary flag ("Yes", "No")}
#'   \item{quality_check}{Factor. Quality assessment ("Pass", "Fail")}
#'   \item{category_type}{Factor. Category classification ("Type_A" to "Type_H")}
#'   \item{start_date}{Date. Study start date}
#'   \item{end_date}{Date. Study end date}
#' }
#'
#' @details
#' This dataset is specifically designed to test tinytable's ability to handle
#' mixed data types in a single table. It includes variables with different
#' scales, ordinal factors, dates, and complex missing patterns.
#'
#' **Key Features:**
#' - Multiple continuous scales (0-100, 1-5, 50-300)
#' - Ordinal factors with proper level ordering
#' - Binary and multi-category factors
#' - Date variables for temporal data
#' - Complex missing data patterns
#' - Multiple data sources for subgroup analysis
#'
#' **Recommended TinyTable Usage:**
#' - Table Type: "Data Summary" or "Custom Format"
#' - Grouping Variable: data_source or region
#' - Variables: score_1, score_2, severity, priority
#' - Themes: "Modern" for contemporary aesthetics
#'
#' @source Simulated data generated using create_tinytable_test_data.R
#' @seealso \code{\link{tinytable}}, \code{\link{tinytable_datasets}}
#'
#' @examples
#' \dontrun{
#' # Load the dataset
#' data(tinytable_multimodal_summary)
#' 
#' # Mixed data types summary
#' result <- tinytable(
#'   data = tinytable_multimodal_summary,
#'   vars = c("score_1", "score_2", "severity", "priority"),
#'   table_type = "summary",
#'   show_missing = TRUE
#' )
#' 
#' # Data source comparison
#' result_source <- tinytable(
#'   data = tinytable_multimodal_summary,
#'   vars = c("score_1", "measurement_a"),
#'   group_var = "data_source",
#'   table_type = "descriptive"
#' )
#' }
"tinytable_multimodal_summary"

#' Time Series Summary Test Dataset
#'
#' Longitudinal dataset with repeated measures per subject across multiple timepoints.
#' Designed to test time-based table formatting, longitudinal data presentation,
#' and outcome tracking over time with realistic dropout patterns.
#'
#' @format A data frame with 150 observations and 10 variables:
#' \describe{
#'   \item{subject_id}{Character. Subject identifier (TS_01 to TS_30)}
#'   \item{timepoint}{Factor. Assessment timepoint ("T1" to "T5")}
#'   \item{months_from_baseline}{Integer. Time since baseline (0, 3, 6, 12, 24 months)}
#'   \item{assessment_date}{Date. Date of assessment}
#'   \item{primary_outcome}{Numeric. Primary outcome score (0-100) with time-dependent missing}
#'   \item{secondary_outcome_1}{Numeric. Secondary outcome correlated with primary}
#'   \item{secondary_outcome_2}{Numeric. Independent secondary outcome}
#'   \item{response_status}{Factor. Treatment response ("Responder", "Non-responder")}
#'   \item{compliance_pct}{Numeric. Treatment compliance percentage (75-100%)}
#'   \item{dose_adjustment}{Factor. Dose modification ("None", "Increase", "Decrease", "Hold")}
#' }
#'
#' @details
#' This dataset represents a longitudinal clinical study with 30 subjects followed
#' over 5 timepoints (24 months). It includes realistic patterns of improvement,
#' dropout, and missing data that increase over time.
#'
#' **Key Features:**
#' - 30 subjects with 5 timepoints each (150 total observations)
#' - Time-dependent outcome improvement pattern
#' - Realistic dropout patterns (increasing missing data over time)
#' - Compliance and dose adjustment tracking
#' - Response status categorization
#' - Multiple correlated outcomes
#'
#' **Recommended TinyTable Usage:**
#' - Table Type: "Grouped Summary" by timepoint
#' - Grouping Variable: timepoint or response_status
#' - Variables: primary_outcome, secondary_outcome_1, compliance_pct
#' - Themes: "Clinical" for longitudinal study reporting
#'
#' @source Simulated data generated using create_tinytable_test_data.R
#' @seealso \code{\link{tinytable}}, \code{\link{tinytable_datasets}}
#'
#' @examples
#' \dontrun{
#' # Load the dataset
#' data(tinytable_timeseries_summary)
#' 
#' # Outcomes by timepoint
#' result <- tinytable(
#'   data = tinytable_timeseries_summary,
#'   vars = c("primary_outcome", "secondary_outcome_1", "compliance_pct"),
#'   group_var = "timepoint",
#'   table_type = "grouped",
#'   show_missing = TRUE
#' )
#' 
#' # Response analysis
#' result_response <- tinytable(
#'   data = tinytable_timeseries_summary,
#'   vars = c("primary_outcome", "secondary_outcome_2"),
#'   group_var = "response_status",
#'   table_type = "descriptive"
#' )
#' }
"tinytable_timeseries_summary"

#' Edge Cases and Quality Testing Dataset
#'
#' Specialized dataset containing various edge cases, extreme values, special
#' characters, and challenging data patterns. Designed to test the robustness
#' of table formatting implementations and error handling capabilities.
#'
#' @format A data frame with 100 observations and 10 variables:
#' \describe{
#'   \item{id}{Character. Unique identifier (EDGE_001 to EDGE_100)}
#'   \item{scenario}{Factor. Test scenario type (1-6)}
#'   \item{numeric_var_1}{Numeric. Variable with extreme values, zeros, and systematic missing}
#'   \item{numeric_var_2}{Numeric. Variable with negative values, small decimals, and irrational numbers}
#'   \item{categorical_var_1}{Factor. Categorical variable with missing and extreme categories}
#'   \item{categorical_var_2}{Factor. Standard categorical variable}
#'   \item{special_characters}{Factor. Categories with special characters and Unicode}
#'   \item{constant_numeric}{Numeric. Constant value (all 42) for edge case testing}
#'   \item{constant_factor}{Factor. Constant factor (all "SAME") for edge case testing}
#'   \item{many_categories}{Factor. Factor with 25 different levels}
#' }
#'
#' @details
#' This dataset is designed to stress-test table formatting implementations
#' with various edge cases and challenging data patterns:
#'
#' **Scenario Types:**
#' - **Scenario 1**: Normal case (baseline comparison)
#' - **Scenario 2**: Extreme values (very large positive and negative numbers)
#' - **Scenario 3**: Very small values (near-zero decimals)
#' - **Scenario 4**: Many decimal places and irrational numbers
#' - **Scenario 5**: Zero and negative zero edge cases
#' - **Scenario 6**: All missing values
#'
#' **Quality Issues:**
#' - Special characters in category names (@#$, Unicode, spaces)
#' - Systematic missing patterns (every 5th and 7th observation)
#' - Constant values across all observations
#' - Factors with many levels (25 categories)
#' - Extreme numeric ranges (0.0001 to 10,000)
#'
#' **Recommended TinyTable Usage:**
#' - All table types for robustness testing
#' - Error handling validation
#' - Special character handling
#' - Missing data pattern testing
#'
#' @source Simulated data generated using create_tinytable_test_data.R
#' @seealso \code{\link{tinytable}}, \code{\link{tinytable_datasets}}
#'
#' @examples
#' \dontrun{
#' # Load the dataset
#' data(tinytable_edge_cases)
#' 
#' # Test robustness with edge cases
#' result <- tinytable(
#'   data = tinytable_edge_cases,
#'   vars = c("numeric_var_1", "numeric_var_2", "categorical_var_1"),
#'   table_type = "summary",
#'   show_missing = TRUE,
#'   precision_digits = 4
#' )
#' 
#' # Test scenario-based grouping
#' result_scenario <- tinytable(
#'   data = tinytable_edge_cases,
#'   vars = c("numeric_var_1", "special_characters"),
#'   group_var = "scenario",
#'   table_type = "raw"
#' )
#' }
"tinytable_edge_cases"

#' Small Sample Size Test Dataset
#'
#' Minimal dataset with very small sample size to test edge cases with minimal
#' data, small group sizes, and basic table formatting functionality.
#'
#' @format A data frame with 15 observations and 6 variables:
#' \describe{
#'   \item{id}{Integer. Simple identifier (1-15)}
#'   \item{group}{Factor. Binary grouping variable ("A", "B")}
#'   \item{value_1}{Numeric. Primary numeric variable with 1 missing value}
#'   \item{value_2}{Numeric. Secondary numeric variable (0-100 range)}
#'   \item{category}{Factor. Three-level factor ("X", "Y", "Z")}
#'   \item{flag}{Factor. Binary flag ("Yes", "No")}
#' }
#'
#' @details
#' This minimal dataset tests table formatting with very small sample sizes,
#' which can reveal edge cases in statistical calculations, grouping operations,
#' and table layout algorithms.
#'
#' **Key Features:**
#' - Only 15 observations total
#' - Simple variable structure
#' - Single missing value for testing
#' - Small group sizes when stratified
#' - Basic data types only
#'
#' **Common Use Cases:**
#' - Testing minimum sample size handling
#' - Validating small group statistics
#' - Edge case detection in grouping algorithms
#' - Minimum viable table formatting
#'
#' **Recommended TinyTable Usage:**
#' - Table Type: "Summary" or "Raw Data Display"
#' - Variables: value_1, value_2, category
#' - Grouping: group (results in very small subgroups)
#' - Themes: Any theme for basic formatting testing
#'
#' @source Simulated data generated using create_tinytable_test_data.R
#' @seealso \code{\link{tinytable}}, \code{\link{tinytable_datasets}}
#'
#' @examples
#' \dontrun{
#' # Load the dataset
#' data(tinytable_small_sample)
#' 
#' # Basic small sample table
#' result <- tinytable(
#'   data = tinytable_small_sample,
#'   vars = c("value_1", "value_2", "category"),
#'   table_type = "summary"
#' )
#' 
#' # Small group analysis
#' result_grouped <- tinytable(
#'   data = tinytable_small_sample,
#'   vars = c("value_1", "value_2"),
#'   group_var = "group",
#'   table_type = "descriptive"
#' )
#' }
"tinytable_small_sample"

#' TinyTable Dataset Summary Information
#'
#' Summary table providing overview of all TinyTable test datasets including
#' sample sizes, variable counts, descriptions, key features, and recommended
#' usage scenarios.
#'
#' @format A data frame with 6 observations and 7 variables:
#' \describe{
#'   \item{Dataset}{Character. Dataset name}
#'   \item{Observations}{Integer. Number of observations}
#'   \item{Variables}{Integer. Number of variables}
#'   \item{Description}{Character. Brief dataset description}
#'   \item{Key_Features}{Character. Main features and characteristics}
#'   \item{Primary_Use_Case}{Character. Primary testing scenario}
#'   \item{Recommended_Table_Types}{Character. Suggested table types for testing}
#' }
#'
#' @details
#' This summary table provides a quick reference for all TinyTable test datasets,
#' helping users select appropriate datasets for their testing needs and
#' understand the key characteristics of each dataset.
#'
#' @source Generated by create_tinytable_test_data.R
#' @seealso \code{\link{tinytable_datasets}}
"tinytable_datasets_summary"

#' TinyTable Test Scenarios Documentation
#'
#' Comprehensive testing scenarios designed to validate different aspects
#' of modern table formatting functionality using the TinyTable test datasets.
#'
#' @format A data frame with 14 observations and 5 variables:
#' \describe{
#'   \item{Scenario}{Character. Name of the testing scenario}
#'   \item{Dataset}{Character. Recommended dataset for this scenario}
#'   \item{Table_Type}{Character. Recommended table type}
#'   \item{Variables}{Character. Suggested variables for testing}
#'   \item{Expected_Result}{Character. Expected outcome of the analysis}
#' }
#'
#' @details
#' This documentation provides systematic testing scenarios covering:
#'
#' - Basic data summary functionality
#' - Clinical demographics table generation
#' - Laboratory results presentation
#' - Grouped analysis capabilities
#' - Mixed data type handling
#' - Longitudinal data formatting
#' - Theme and styling validation
#' - Missing data handling
#' - Edge case robustness testing
#' - Small sample size behavior
#' - Publication-ready formatting
#' - Multi-format output consistency
#' - Error handling validation
#'
#' Each scenario includes the recommended dataset, table type, variables,
#' and expected results for comprehensive validation.
#'
#' @source Generated by create_tinytable_test_data.R
#' @seealso \code{\link{tinytable_datasets}}, \code{\link{tinytable}}
"tinytable_test_scenarios"