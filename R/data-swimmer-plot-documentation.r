#' Patient Timeline Data for Clinical Oncology
#'
#' A dataset containing simulated patient timeline data for clinical oncology research.
#' This dataset represents typical patient journeys through cancer treatment,
#' including diagnosis, treatment, response assessment, and outcomes. The data
#' simulates a realistic clinical study where patients enroll at different timepoints
#' and have varied treatment durations and outcomes.
#'
#' @format A data frame with 30 rows and 15 variables:
#' \describe{
#'   \item{PatientID}{Patient identifier, formatted as PT001, PT002, etc.}
#'   \item{StartTime}{Numeric. Time at which observation began, varies between patients to
#'                    represent staggered enrollment (some values greater than 0)}
#'   \item{EndTime}{Numeric. Time at which observation ended, in the same units as StartTime}
#'   \item{BestResponse}{Factor with levels: CR (Complete Response), PR (Partial Response),
#'                       SD (Stable Disease), PD (Progressive Disease), NE (Not Evaluable)}
#'   \item{Surgery}{Numeric. Time of surgery relative to study start. May be negative
#'                  for surgeries that occurred before enrollment}
#'   \item{TreatmentStart}{Numeric. Time when treatment began relative to study start}
#'   \item{ResponseAssessment}{Numeric. Time of response assessment relative to study start}
#'   \item{Progression}{Numeric. Time of disease progression, NA if no progression}
#'   \item{Death}{Numeric. Time of death, NA if patient alive at last follow-up}
#'   \item{Risk}{Factor with levels: High, Medium, Low. Risk classification for the patient}
#'   \item{Age}{Numeric. Patient age in years}
#'   \item{ECOG}{Integer (0-3). ECOG performance status}
#'   \item{ResponseDuration}{Numeric. Duration of response, calculated as Progression - ResponseAssessment}
#'   \item{FollowUpDuration}{Numeric. Total duration of follow-up, from StartTime to EndTime}
#' }
#'
#' @note There is also a date-based version of this dataset where time variables are represented
#'       as actual dates instead of numeric values, named 'patientTimelinesDates'.
#'
#' @examples
#' data(patientTimelines)
#'
#' # Show staggered entry of patients into the study
#' hist(patientTimelines$StartTime,
#'      main = "Patient Enrollment Times",
#'      xlab = "Start Time")
#'
#' # Basic swimmer plot
#' swimmerplot(
#'   data = patientTimelines,
#'   patientID = "PatientID",
#'   start = "StartTime",
#'   end = "EndTime",
#'   event = "BestResponse"
#' )
#'
#' # With milestones
#' swimmerplot(
#'   data = patientTimelines,
#'   patientID = "PatientID",
#'   start = "StartTime",
#'   end = "EndTime",
#'   event = "BestResponse",
#'   milestone1Name = "Surgery",
#'   milestone1Date = "Surgery",
#'   milestone2Name = "Treatment",
#'   milestone2Date = "TreatmentStart"
#' )
"patientTimelines"

#' Patient Timeline Data with Dates for Clinical Oncology
#'
#' A dataset containing simulated patient timeline data for clinical oncology research,
#' with time variables represented as actual dates. This dataset represents typical
#' patient journeys through cancer treatment, including diagnosis, treatment,
#' response assessment, and outcomes. The data simulates a realistic clinical study
#' where patients enroll over a 6-month period and have varied treatment durations
#' and outcomes.
#'
#' @format A data frame with 30 rows and 15 variables:
#' \describe{
#'   \item{PatientID}{Patient identifier, formatted as PT001, PT002, etc.}
#'   \item{StartDate}{Date. Date at which observation began, varies across patients
#'                    to represent staggered enrollment}
#'   \item{EndDate}{Date. Date at which observation ended}
#'   \item{BestResponse}{Factor with levels: CR (Complete Response), PR (Partial Response),
#'                       SD (Stable Disease), PD (Progressive Disease), NE (Not Evaluable)}
#'   \item{Surgery}{Date. Date of surgery, may be before the study enrollment date}
#'   \item{TreatmentStart}{Date. Date when treatment began}
#'   \item{ResponseAssessment}{Date. Date of response assessment}
#'   \item{Progression}{Date. Date of disease progression, NA if no progression}
#'   \item{Death}{Date. Date of death, NA if patient alive at last follow-up}
#'   \item{Risk}{Factor with levels: High, Medium, Low. Risk classification for the patient}
#'   \item{Age}{Numeric. Patient age in years}
#'   \item{ECOG}{Integer (0-3). ECOG performance status}
#'   \item{ResponseDuration}{Numeric. Duration of response in days, calculated as
#'                          Progression - ResponseAssessment}
#'   \item{FollowUpDuration}{Numeric. Total duration of follow-up in days, from
#'                          StartDate to EndDate}
#' }
#'
#' @note This is the date-based version of the patientTimelines dataset.
#'
#' @examples
#' data(patientTimelinesDates)
#'
#' # Show staggered enrollment over the study period
#' hist(as.numeric(patientTimelinesDates$StartDate - min(patientTimelinesDates$StartDate)),
#'      main = "Patient Enrollment Days from Study Start",
#'      xlab = "Days")
#'
#' # Basic swimmer plot with date data
#' swimmerplot(
#'   data = patientTimelinesDates,
#'   patientID = "PatientID",
#'   start = "StartDate",
#'   end = "EndDate",
#'   event = "BestResponse",
#'   timetype = "datetime",
#'   timetypedata = "ymd",
#'   timetypeoutput = "months"
#' )
#'
#' # With absolute time display to show the actual enrollment pattern
#' swimmerplot(
#'   data = patientTimelinesDates,
#'   patientID = "PatientID",
#'   start = "StartDate",
#'   end = "EndDate",
#'   event = "BestResponse",
#'   timetype = "datetime",
#'   timetypedata = "ymd",
#'   timetypeoutput = "months",
#'   startType = "absolute"
#' )
"patientTimelinesDates"
