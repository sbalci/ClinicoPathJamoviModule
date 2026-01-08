#' Test Datasets for Time Interval Function
#'
#' A collection of test datasets for the timeinterval function, which calculates
#' time intervals between dates for survival analysis and person-time calculations.
#'
#' @name timeinterval_test_datasets
#' @aliases timeinterval_test timeinterval_ymd timeinterval_dmy timeinterval_mdy
#' @aliases timeinterval_ymdhms timeinterval_landmark timeinterval_quality
#' @aliases timeinterval_extreme timeinterval_small timeinterval_large
#' @aliases timeinterval_trial timeinterval_shortterm timeinterval_longterm
#' @aliases timeinterval_sameday timeinterval_negative timeinterval_allmissing
#' @aliases timeinterval_mixedformat
#'
#' @format
#' ## `timeinterval_test`
#' Main test dataset with diagnosis and follow-up dates (100 patients):
#' \describe{
#'   \item{patient_id}{Character. Patient identifier}
#'   \item{diagnosis_date}{Character. Start date (YYYY-MM-DD format)}
#'   \item{followup_date}{Character. End date (YYYY-MM-DD format)}
#'   \item{age}{Numeric. Patient age}
#'   \item{sex}{Character. Male/Female}
#'   \item{treatment}{Character. Treatment type}
#'   \item{stage}{Character. Disease stage (I-IV)}
#'   \item{event_occurred}{Integer. Event indicator (0/1)}
#' }
#'
#' ## `timeinterval_ymd`
#' Standard YMD format (50 observations):
#' \describe{
#'   \item{id}{Character. Identifier}
#'   \item{start_date}{Character. Start date (YYYY-MM-DD)}
#'   \item{end_date}{Character. End date (YYYY-MM-DD)}
#' }
#'
#' ## `timeinterval_dmy`
#' European DMY format (50 observations):
#' \describe{
#'   \item{id}{Character. Identifier}
#'   \item{start_date}{Character. Start date (DD-MM-YYYY)}
#'   \item{end_date}{Character. End date (DD-MM-YYYY)}
#' }
#'
#' ## `timeinterval_mdy`
#' US MDY format (50 observations):
#' \describe{
#'   \item{id}{Character. Identifier}
#'   \item{start_date}{Character. Start date (MM-DD-YYYY)}
#'   \item{end_date}{Character. End date (MM-DD-YYYY)}
#' }
#'
#' ## `timeinterval_ymdhms`
#' DateTime format with hours/minutes/seconds (50 observations):
#' \describe{
#'   \item{id}{Character. Identifier}
#'   \item{start_datetime}{Character. Start datetime}
#'   \item{end_datetime}{Character. End datetime}
#' }
#'
#' ## `timeinterval_landmark`
#' Data for landmark analysis (80 patients):
#' \describe{
#'   \item{patient_id}{Character. Patient identifier}
#'   \item{enrollment_date}{Character. Study enrollment date}
#'   \item{last_contact}{Character. Last follow-up contact}
#'   \item{treatment_arm}{Character. Treatment group}
#'   \item{biomarker_positive}{Character. Biomarker status}
#' }
#'
#' ## `timeinterval_quality`
#' Dataset with quality issues (60 observations, ~10% missing, some negative intervals)
#'
#' ## `timeinterval_extreme`
#' Dataset with extreme values (40 observations: very short, normal, very long durations)
#'
#' ## `timeinterval_small`
#' Minimal dataset (5 observations)
#'
#' ## `timeinterval_large`
#' Large dataset for performance testing (500 observations)
#'
#' ## `timeinterval_trial`
#' Realistic clinical trial simulation (150 patients):
#' \describe{
#'   \item{patient_id}{Character. Trial patient ID}
#'   \item{enrollment_date}{Character. Enrollment date}
#'   \item{followup_date}{Character. Last follow-up}
#'   \item{treatment_arm}{Character. Trial arm}
#'   \item{site}{Character. Study site}
#'   \item{age}{Numeric. Patient age}
#'   \item{ecog_ps}{Integer. ECOG performance status}
#'   \item{progression}{Integer. Disease progression (0/1)}
#'   \item{death}{Integer. Death event (0/1)}
#' }
#'
#' ## Other Datasets
#' - `timeinterval_shortterm`: Hospital stays (1-14 days)
#' - `timeinterval_longterm`: Long-term study (5-15 years)
#' - `timeinterval_sameday`: Zero-interval (same start and end)
#' - `timeinterval_negative`: Negative intervals (end before start)
#' - `timeinterval_allmissing`: All missing dates
#' - `timeinterval_mixedformat`: Inconsistent date formats
#'
#' @section Supported Date Formats:
#' \itemize{
#'   \item auto: Automatic detection
#'   \item ymd: YYYY-MM-DD (ISO 8601)
#'   \item dmy: DD-MM-YYYY (European)
#'   \item mdy: MM-DD-YYYY (US)
#'   \item ymdhms: YYYY-MM-DD HH:MM:SS
#' }
#'
#' @section Time Units:
#' \itemize{
#'   \item days: Day-level precision
#'   \item weeks: 7-day periods
#'   \item months: 30.44-day months (standardized) or calendar months
#'   \item years: 365.25-day years (standardized) or calendar years
#' }
#'
#' @examples
#' # Load main test dataset
#' data(timeinterval_test)
#' head(timeinterval_test)
#'
#' # Basic time interval calculation
#' \dontrun{
#' timeinterval(
#'   data = timeinterval_test,
#'   dx_date = "diagnosis_date",
#'   fu_date = "followup_date",
#'   time_format = "ymd",
#'   output_unit = "months"
#' )
#' }
#'
#' # Landmark analysis
#' \dontrun{
#' data(timeinterval_landmark)
#' timeinterval(
#'   data = timeinterval_landmark,
#'   dx_date = "enrollment_date",
#'   fu_date = "last_contact",
#'   use_landmark = TRUE,
#'   landmark_time = 6,
#'   output_unit = "months"
#' )
#' }
#'
#' @source Generated using data-raw/timeinterval_test_data.R (seed = 42)
#' @seealso \code{\link{timeinterval}} for the time interval calculation function
"timeinterval_test"

#' @rdname timeinterval_test_datasets
"timeinterval_ymd"

#' @rdname timeinterval_test_datasets
"timeinterval_dmy"

#' @rdname timeinterval_test_datasets
"timeinterval_mdy"

#' @rdname timeinterval_test_datasets
"timeinterval_ymdhms"

#' @rdname timeinterval_test_datasets
"timeinterval_landmark"

#' @rdname timeinterval_test_datasets
"timeinterval_quality"

#' @rdname timeinterval_test_datasets
"timeinterval_extreme"

#' @rdname timeinterval_test_datasets
"timeinterval_small"

#' @rdname timeinterval_test_datasets
"timeinterval_large"

#' @rdname timeinterval_test_datasets
"timeinterval_trial"

#' @rdname timeinterval_test_datasets
"timeinterval_shortterm"

#' @rdname timeinterval_test_datasets
"timeinterval_longterm"

#' @rdname timeinterval_test_datasets
"timeinterval_sameday"

#' @rdname timeinterval_test_datasets
"timeinterval_negative"

#' @rdname timeinterval_test_datasets
"timeinterval_allmissing"

#' @rdname timeinterval_test_datasets
"timeinterval_mixedformat"
