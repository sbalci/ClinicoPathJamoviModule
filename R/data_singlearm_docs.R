#' Test Datasets for Single Arm Survival Function
#'
#' @name singlearm_test_datasets
#' @aliases singlearm_test singlearm_dates singlearm_compete singlearm_causespecific
#' @aliases singlearm_landmark singlearm_dmy singlearm_mdy singlearm_datetime
#' @aliases singlearm_longfu singlearm_shortfu singlearm_persontime
#' @aliases singlearm_small singlearm_censored singlearm_allevents singlearm_early
#' @aliases singlearm_missing singlearm_zerotime singlearm_large
#'
#' @format
#' ## `singlearm_test`
#' Main test dataset for overall survival (150 observations):
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{time_months}{Survival time in months}
#'   \item{outcome}{Outcome status (Alive/Dead)}
#'   \item{age}{Patient age}
#'   \item{stage}{Disease stage (I/II/III/IV)}
#'   \item{sex}{Patient sex}
#' }
#'
#' ## `singlearm_dates`
#' Date-based survival data in YMD format (120 observations):
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{diagnosis_date}{Diagnosis date (YYYY-MM-DD)}
#'   \item{followup_date}{Follow-up date (YYYY-MM-DD)}
#'   \item{outcome}{Outcome status (Alive/Dead)}
#'   \item{treatment}{Treatment type}
#' }
#'
#' ## `singlearm_compete`
#' Competing risks data (100 observations):
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{time_months}{Survival time in months}
#'   \item{outcome}{Outcome (Alive_NED/Alive_Disease/Dead_Disease/Dead_Other)}
#'   \item{biomarker}{Biomarker status}
#'   \item{grade}{Tumor grade}
#' }
#'
#' ## `singlearm_causespecific`
#' Cause-specific survival data (110 observations):
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{time_months}{Survival time in months}
#'   \item{outcome}{Outcome (Alive/Dead_Cancer/Dead_Cardiac/Dead_Other)}
#'   \item{risk_score}{Risk score}
#'   \item{comorbidity}{Comorbidity status}
#' }
#'
#' ## `singlearm_landmark`
#' Landmark analysis data (130 observations):
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{time_months}{Survival time in months}
#'   \item{outcome}{Outcome status (Alive/Dead)}
#'   \item{treatment_response}{Response (CR/PR/SD/PD)}
#'   \item{early_toxicity}{Early toxicity status}
#' }
#'
#' ## `singlearm_dmy`
#' Date-based data in DMY format (90 observations):
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{diagnosis_date}{Diagnosis date (DD-MM-YYYY)}
#'   \item{followup_date}{Follow-up date (DD-MM-YYYY)}
#'   \item{outcome}{Outcome status}
#' }
#'
#' ## `singlearm_mdy`
#' Date-based data in MDY format (85 observations):
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{diagnosis_date}{Diagnosis date (MM/DD/YYYY)}
#'   \item{followup_date}{Follow-up date (MM/DD/YYYY)}
#'   \item{outcome}{Outcome status}
#' }
#'
#' ## `singlearm_datetime`
#' Date-time data with HMS (75 observations):
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{diagnosis_datetime}{Diagnosis datetime}
#'   \item{followup_datetime}{Follow-up datetime}
#'   \item{outcome}{Outcome status}
#'   \item{emergency_admission}{Emergency admission status}
#' }
#'
#' ## `singlearm_longfu`
#' Long follow-up data (95 observations):
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{time_months}{Long-term survival time}
#'   \item{outcome}{Outcome status}
#'   \item{enrollment_year}{Year of enrollment}
#'   \item{clinical_trial}{Trial or registry}
#' }
#'
#' ## `singlearm_shortfu`
#' Short follow-up data (80 observations):
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{time_months}{Short-term survival time}
#'   \item{outcome}{Outcome status}
#'   \item{disease}{Disease type}
#' }
#'
#' ## `singlearm_persontime`
#' Person-time analysis data (140 observations):
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{time_months}{Survival time}
#'   \item{outcome}{Outcome status}
#'   \item{age_group}{Age group}
#'   \item{exposure}{Exposure level}
#' }
#'
#' ## `singlearm_small`
#' Small dataset (15 observations):
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{time_months}{Survival time}
#'   \item{outcome}{Outcome status}
#' }
#'
#' ## `singlearm_censored`
#' All censored data (30 observations):
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{time_months}{Survival time}
#'   \item{outcome}{Outcome (all Alive)}
#' }
#'
#' ## `singlearm_allevents`
#' All events data (25 observations):
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{time_months}{Survival time}
#'   \item{outcome}{Outcome (all Dead)}
#' }
#'
#' ## `singlearm_early`
#' Very early events data (40 observations):
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{time_months}{Short survival time}
#'   \item{outcome}{Outcome status}
#'   \item{rapid_progression}{Rapid progression status}
#' }
#'
#' ## `singlearm_missing`
#' Data with missing values (60 observations):
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{time_months}{Survival time (with NAs)}
#'   \item{outcome}{Outcome status (with NAs)}
#'   \item{covariate1}{Continuous covariate}
#'   \item{covariate2}{Categorical covariate}
#' }
#'
#' ## `singlearm_zerotime`
#' Data with zero time values (35 observations):
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{time_months}{Survival time (including zeros)}
#'   \item{outcome}{Outcome status}
#' }
#'
#' ## `singlearm_large`
#' Large dataset for performance testing (500 observations):
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{time_months}{Survival time}
#'   \item{outcome}{Outcome status}
#'   \item{age}{Patient age}
#'   \item{stage}{Disease stage}
#'   \item{grade}{Tumor grade}
#'   \item{sex}{Patient sex}
#'   \item{biomarker}{Biomarker value}
#'   \item{treatment}{Treatment type}
#'   \item{site}{Study site}
#' }
#'
#' @examples
#' data(singlearm_test)
#' \dontrun{
#' singlearm(
#'   data = singlearm_test,
#'   elapsedtime = "time_months",
#'   outcome = "outcome",
#'   outcomeLevel = "Dead",
#'   sc = TRUE
#' )
#' }
#'
#' @source Generated using data-raw/singlearm_test_data.R (seed = 42)
#' @seealso \code{\link{singlearm}}
"singlearm_test"

#' @rdname singlearm_test_datasets
"singlearm_dates"

#' @rdname singlearm_test_datasets
"singlearm_compete"

#' @rdname singlearm_test_datasets
"singlearm_causespecific"

#' @rdname singlearm_test_datasets
"singlearm_landmark"

#' @rdname singlearm_test_datasets
"singlearm_dmy"

#' @rdname singlearm_test_datasets
"singlearm_mdy"

#' @rdname singlearm_test_datasets
"singlearm_datetime"

#' @rdname singlearm_test_datasets
"singlearm_longfu"

#' @rdname singlearm_test_datasets
"singlearm_shortfu"

#' @rdname singlearm_test_datasets
"singlearm_persontime"

#' @rdname singlearm_test_datasets
"singlearm_small"

#' @rdname singlearm_test_datasets
"singlearm_censored"

#' @rdname singlearm_test_datasets
"singlearm_allevents"

#' @rdname singlearm_test_datasets
"singlearm_early"

#' @rdname singlearm_test_datasets
"singlearm_missing"

#' @rdname singlearm_test_datasets
"singlearm_zerotime"

#' @rdname singlearm_test_datasets
"singlearm_large"
