#' Test Datasets for Survival Continuous Variable Function
#'
#' @name survivalcont_test_datasets
#' @aliases survivalcont_test survivalcont_ki67 survivalcont_psa survivalcont_hemoglobin
#' @aliases survivalcont_tumorsize survivalcont_age survivalcont_compete survivalcont_dates
#' @aliases survivalcont_landmark survivalcont_multicut survivalcont_expression
#' @aliases survivalcont_small survivalcont_nocutoff survivalcont_extreme
#' @aliases survivalcont_missing survivalcont_constant survivalcont_large
#'
#' @format
#' ## `survivalcont_test`
#' Main test dataset (150 observations):
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{time_months}{Survival time in months}
#'   \item{outcome}{Outcome status (Alive/Dead)}
#'   \item{biomarker}{Continuous biomarker (mean=100, SD=25)}
#'   \item{age}{Patient age}
#'   \item{sex}{Patient sex}
#' }
#'
#' ## `survivalcont_ki67`
#' Ki67 proliferation index data (140 observations):
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{time_months}{Survival time}
#'   \item{outcome}{Outcome status}
#'   \item{ki67_percent}{Ki67 percentage (0-100)}
#'   \item{tumor_grade}{Tumor grade}
#'   \item{stage}{Disease stage}
#' }
#'
#' ## `survivalcont_psa`
#' PSA level data (130 observations):
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{time_months}{Survival time}
#'   \item{outcome}{Outcome status}
#'   \item{psa_level}{PSA level (ng/mL)}
#'   \item{gleason_score}{Gleason score (6-10)}
#'   \item{treatment}{Treatment type}
#' }
#'
#' ## `survivalcont_hemoglobin`
#' Hemoglobin level data (120 observations):
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{time_months}{Survival time}
#'   \item{outcome}{Outcome status}
#'   \item{hemoglobin_gL}{Hemoglobin level (g/L)}
#'   \item{performance_status}{ECOG performance status}
#' }
#'
#' ## `survivalcont_tumorsize`
#' Tumor size data (135 observations):
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{time_months}{Survival time}
#'   \item{outcome}{Outcome status}
#'   \item{tumor_size_cm}{Tumor size in cm}
#'   \item{lymph_nodes}{Lymph node status}
#'   \item{histology}{Histological type}
#' }
#'
#' ## `survivalcont_age`
#' Age as continuous predictor (145 observations):
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{time_months}{Survival time}
#'   \item{outcome}{Outcome status}
#'   \item{age_years}{Patient age in years}
#'   \item{comorbidity_index}{Comorbidity index}
#' }
#'
#' ## `survivalcont_compete`
#' Competing risks data (110 observations):
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{time_months}{Survival time}
#'   \item{outcome}{Outcome (Alive_NED/Alive_Disease/Dead_Disease/Dead_Other)}
#'   \item{biomarker_score}{Continuous biomarker score}
#'   \item{risk_category}{Risk category}
#' }
#'
#' ## `survivalcont_dates`
#' Date-based data (100 observations):
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{diagnosis_date}{Diagnosis date (YYYY-MM-DD)}
#'   \item{followup_date}{Follow-up date (YYYY-MM-DD)}
#'   \item{outcome}{Outcome status}
#'   \item{continuous_marker}{Continuous marker}
#' }
#'
#' ## `survivalcont_landmark`
#' Landmark analysis data (125 observations):
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{time_months}{Survival time}
#'   \item{outcome}{Outcome status}
#'   \item{early_response_score}{Early response score}
#'   \item{baseline_score}{Baseline score}
#' }
#'
#' ## `survivalcont_multicut`
#' Multiple cutoff optimization data (150 observations):
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{time_months}{Survival time}
#'   \item{outcome}{Outcome status}
#'   \item{risk_score}{Risk score with clear stratification}
#' }
#'
#' ## `survivalcont_expression`
#' Gene expression data (115 observations):
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{time_months}{Survival time}
#'   \item{outcome}{Outcome status}
#'   \item{gene_expression}{Gene expression level}
#'   \item{mutation_status}{Mutation status}
#' }
#'
#' ## `survivalcont_small`
#' Small dataset (20 observations):
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{time_months}{Survival time}
#'   \item{outcome}{Outcome status}
#'   \item{continuous_var}{Continuous variable}
#' }
#'
#' ## `survivalcont_nocutoff`
#' No clear cutoff data (80 observations):
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{time_months}{Survival time}
#'   \item{outcome}{Outcome status}
#'   \item{random_marker}{Random marker (no survival correlation)}
#' }
#'
#' ## `survivalcont_extreme`
#' Extreme values data (70 observations):
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{time_months}{Survival time}
#'   \item{outcome}{Outcome status}
#'   \item{extreme_values}{Values with outliers}
#' }
#'
#' ## `survivalcont_missing`
#' Data with missing values (90 observations):
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{time_months}{Survival time (with NAs)}
#'   \item{outcome}{Outcome status (with NAs)}
#'   \item{biomarker}{Biomarker (with NAs)}
#'   \item{covariate}{Categorical covariate}
#' }
#'
#' ## `survivalcont_constant`
#' Constant variable data (50 observations):
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{time_months}{Survival time}
#'   \item{outcome}{Outcome status}
#'   \item{constant_marker}{Constant marker (no variation)}
#' }
#'
#' ## `survivalcont_large`
#' Large dataset for performance testing (500 observations):
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{time_months}{Survival time}
#'   \item{outcome}{Outcome status}
#'   \item{biomarker}{Continuous biomarker}
#'   \item{age}{Patient age}
#'   \item{stage}{Disease stage}
#'   \item{grade}{Tumor grade}
#'   \item{sex}{Patient sex}
#'   \item{site}{Study site}
#' }
#'
#' @examples
#' data(survivalcont_test)
#' \dontrun{
#' survivalcont(
#'   data = survivalcont_test,
#'   elapsedtime = "time_months",
#'   outcome = "outcome",
#'   outcomeLevel = "Dead",
#'   contexpl = "biomarker",
#'   findcut = TRUE,
#'   sc = TRUE
#' )
#' }
#'
#' @source Generated using data-raw/survivalcont_test_data.R (seed = 42)
#' @seealso \code{\link{survivalcont}}
"survivalcont_test"

#' @rdname survivalcont_test_datasets
"survivalcont_ki67"

#' @rdname survivalcont_test_datasets
"survivalcont_psa"

#' @rdname survivalcont_test_datasets
"survivalcont_hemoglobin"

#' @rdname survivalcont_test_datasets
"survivalcont_tumorsize"

#' @rdname survivalcont_test_datasets
"survivalcont_age"

#' @rdname survivalcont_test_datasets
"survivalcont_compete"

#' @rdname survivalcont_test_datasets
"survivalcont_dates"

#' @rdname survivalcont_test_datasets
"survivalcont_landmark"

#' @rdname survivalcont_test_datasets
"survivalcont_multicut"

#' @rdname survivalcont_test_datasets
"survivalcont_expression"

#' @rdname survivalcont_test_datasets
"survivalcont_small"

#' @rdname survivalcont_test_datasets
"survivalcont_nocutoff"

#' @rdname survivalcont_test_datasets
"survivalcont_extreme"

#' @rdname survivalcont_test_datasets
"survivalcont_missing"

#' @rdname survivalcont_test_datasets
"survivalcont_constant"

#' @rdname survivalcont_test_datasets
"survivalcont_large"
