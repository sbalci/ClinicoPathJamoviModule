#' Test Datasets for Outcome Organizer Function
#'
#' @name outcomeorganizer_test_datasets
#' @aliases outcomeorganizer_os outcomeorganizer_compete outcomeorganizer_pfs
#' @aliases outcomeorganizer_rfs outcomeorganizer_causespecific outcomeorganizer_multistate
#' @aliases outcomeorganizer_dfs outcomeorganizer_small outcomeorganizer_censored
#' @aliases outcomeorganizer_allevents
#'
#' @format
#' ## `outcomeorganizer_os`
#' Overall Survival dataset (150 observations):
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{vital_status}{Vital status (Alive/Dead)}
#'   \item{time_months}{Follow-up time in months}
#'   \item{age}{Patient age}
#'   \item{stage}{Disease stage (I/II/III/IV)}
#' }
#'
#' ## `outcomeorganizer_compete`
#' Competing Risks dataset (120 observations):
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{outcome_status}{Outcome (Alive_NED/Alive_Disease/Dead_Disease/Dead_Other)}
#'   \item{time}{Follow-up time}
#'   \item{treatment}{Treatment type}
#' }
#'
#' ## `outcomeorganizer_pfs`
#' Progression-Free Survival dataset (100 observations):
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{vital_status}{Vital status (Alive/Dead)}
#'   \item{progression}{Progression status (No/Yes)}
#'   \item{time_months}{Follow-up time in months}
#'   \item{biomarker}{Biomarker status}
#' }
#'
#' ## `outcomeorganizer_rfs`
#' Recurrence-Free Survival dataset (110 observations):
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{vital}{Vital status (Alive/Dead)}
#'   \item{recurrence}{Recurrence status (No/Yes)}
#'   \item{fu_time}{Follow-up time}
#'   \item{grade}{Tumor grade}
#' }
#'
#' ## `outcomeorganizer_causespecific`
#' Cause-Specific Survival dataset (90 observations):
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{death_status}{Death status (Alive/Dead_Cancer/Dead_Cardiac/Dead_Other)}
#'   \item{time}{Follow-up time}
#'   \item{risk_score}{Risk score}
#' }
#'
#' ## `outcomeorganizer_multistate`
#' Multistate Model dataset (80 observations):
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{current_state}{Current state (Disease_Free/Local_Recurrence/Metastatic/Dead)}
#'   \item{time}{Follow-up time}
#' }
#'
#' ## `outcomeorganizer_dfs`
#' Disease-Free Survival dataset (95 observations):
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{status}{Status (NED/Disease/Dead)}
#'   \item{time_years}{Follow-up time in years}
#'   \item{surgery_type}{Surgery type}
#' }
#'
#' ## `outcomeorganizer_small`
#' Small dataset (15 observations) for edge case testing:
#' \describe{
#'   \item{id}{Patient identifier}
#'   \item{outcome}{Outcome (Event/NoEvent)}
#'   \item{time}{Follow-up time}
#' }
#'
#' ## `outcomeorganizer_censored`
#' All censored dataset (30 observations):
#' \describe{
#'   \item{id}{Patient identifier}
#'   \item{status}{Status (all Alive)}
#'   \item{time}{Follow-up time}
#' }
#'
#' ## `outcomeorganizer_allevents`
#' All events dataset (25 observations):
#' \describe{
#'   \item{id}{Patient identifier}
#'   \item{status}{Status (all Dead)}
#'   \item{time}{Follow-up time}
#' }
#'
#' @examples
#' data(outcomeorganizer_os)
#' \dontrun{
#' outcomeorganizer(
#'   data = outcomeorganizer_os,
#'   outcome = "vital_status",
#'   time = "time_months",
#'   dead = "Dead",
#'   alive = "Alive"
#' )
#' }
#'
#' @source Generated using data-raw/outcomeorganizer_test_data.R (seed = 42)
#' @seealso \code{\link{outcomeorganizer}}
"outcomeorganizer_os"

#' @rdname outcomeorganizer_test_datasets
"outcomeorganizer_compete"

#' @rdname outcomeorganizer_test_datasets
"outcomeorganizer_pfs"

#' @rdname outcomeorganizer_test_datasets
"outcomeorganizer_rfs"

#' @rdname outcomeorganizer_test_datasets
"outcomeorganizer_causespecific"

#' @rdname outcomeorganizer_test_datasets
"outcomeorganizer_multistate"

#' @rdname outcomeorganizer_test_datasets
"outcomeorganizer_dfs"

#' @rdname outcomeorganizer_test_datasets
"outcomeorganizer_small"

#' @rdname outcomeorganizer_test_datasets
"outcomeorganizer_censored"

#' @rdname outcomeorganizer_test_datasets
"outcomeorganizer_allevents"
