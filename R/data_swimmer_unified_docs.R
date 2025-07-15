#' Basic Swimmer Plot Example Data
#'
#' A simple dataset for demonstrating basic swimmer plot functionality.
#' Contains patient timelines with treatment responses and basic categorization.
#'
#' @format A data frame with 10 rows and 6 variables:
#' \describe{
#'   \item{PatientID}{Character. Unique patient identifiers (PT001-PT010)}
#'   \item{StartTime}{Numeric. Treatment start time (all patients start at 0)}
#'   \item{EndTime}{Numeric. Treatment end time in months (5-15 months)}
#'   \item{Response}{Character. Best response (CR, PR, SD, PD)}
#'   \item{Treatment}{Character. Treatment type (Immunotherapy, Chemotherapy, Combination, Targeted)}
#'   \item{Priority}{Character. Patient priority level (High, Medium, Low)}
#' }
#' 
#' @usage data(swimmer_unified_basic)
#' 
#' @examples
#' data(swimmer_unified_basic)
#' 
#' # Basic swimmer plot
#' swimmerplot(
#'   data = swimmer_unified_basic,
#'   patientID = "PatientID",
#'   startTime = "StartTime", 
#'   endTime = "EndTime",
#'   responseVar = "Response"
#' )
#'
#' @source Generated for ClinicoPath package demonstration
#' @family swimmer plot datasets
"swimmer_unified_basic"

#' Comprehensive Swimmer Plot Example Data
#'
#' A comprehensive dataset demonstrating advanced swimmer plot features including
#' multiple milestones, patient demographics, and clinical characteristics.
#' Designed to showcase the full capabilities of the swimmer plot function.
#'
#' @format A data frame with 20 rows and 11 variables:
#' \describe{
#'   \item{PatientID}{Character. Unique patient identifiers (PT001-PT020)}
#'   \item{StartTime}{Numeric. Treatment start time (all patients start at 0)}
#'   \item{EndTime}{Numeric. Treatment end time in months (6-51 months)}
#'   \item{BestResponse}{Character. Best overall response (CR, PR, SD, PD)}
#'   \item{Surgery}{Numeric. Time of surgery in months (1-9 months, NA if no surgery)}
#'   \item{FirstResponse}{Numeric. Time of first response in months (3-18 months, NA if no response)}
#'   \item{Progression}{Numeric. Time of disease progression in months (4-36 months, NA if no progression)}
#'   \item{DeathLastFU}{Numeric. Time of death or last follow-up (6-18 months, NA if alive)}
#'   \item{TreatmentType}{Character. Type of treatment received}
#'   \item{AgeGroup}{Character. Patient age group (Young, Middle, Elderly)}
#'   \item{ECOG}{Numeric. ECOG performance status (0, 1, 2)}
#' }
#' 
#' @usage data(swimmer_unified_comprehensive)
#' 
#' @examples
#' data(swimmer_unified_comprehensive)
#' 
#' # Comprehensive swimmer plot with milestones
#' swimmerplot(
#'   data = swimmer_unified_comprehensive,
#'   patientID = "PatientID",
#'   startTime = "StartTime",
#'   endTime = "EndTime", 
#'   responseVar = "BestResponse",
#'   milestone1Name = "Surgery",
#'   milestone1Date = "Surgery",
#'   milestone2Name = "First Response", 
#'   milestone2Date = "FirstResponse",
#'   milestone3Name = "Progression",
#'   milestone3Date = "Progression",
#'   milestone4Name = "Death/Last FU",
#'   milestone4Date = "DeathLastFU",
#'   personTimeAnalysis = TRUE,
#'   showInterpretation = TRUE
#' )
#'
#' @source Generated for ClinicoPath package demonstration  
#' @family swimmer plot datasets
"swimmer_unified_comprehensive"

#' DateTime Swimmer Plot Example Data
#'
#' A dataset with actual calendar dates to demonstrate datetime handling
#' capabilities of the swimmer plot function. Shows how to work
#' with real-world date formats in clinical data.
#'
#' @format A data frame with 10 rows and 8 variables:
#' \describe{
#'   \item{PatientID}{Character. Unique patient identifiers (PT001-PT010)}
#'   \item{StartDate}{Character. Treatment start date in YYYY-MM-DD format}
#'   \item{EndDate}{Character. Treatment end date in YYYY-MM-DD format}
#'   \item{BestResponse}{Character. Best overall response (CR, PR, SD, PD)}
#'   \item{Surgery}{Character. Surgery date in YYYY-MM-DD format (NA if no surgery)}
#'   \item{ProgressionAssessment}{Character. Date of progression assessment (NA if no progression)}
#'   \item{EventType}{Character. Type of clinical event recorded}
#'   \item{Site}{Character. Clinical trial site (Center A, B, C)}
#' }
#' 
#' @usage data(swimmer_unified_datetime)
#' 
#' @examples
#' data(swimmer_unified_datetime)
#' 
#' # DateTime swimmer plot with relative display
#' swimmerplot(
#'   data = swimmer_unified_datetime,
#'   patientID = "PatientID", 
#'   startTime = "StartDate",
#'   endTime = "EndDate",
#'   responseVar = "BestResponse",
#'   timeType = "datetime",
#'   dateFormat = "ymd",
#'   timeUnit = "months",
#'   timeDisplay = "relative",
#'   milestone1Name = "Surgery",
#'   milestone1Date = "Surgery"
#' )
#'
#' @source Generated for ClinicoPath package demonstration
#' @family swimmer plot datasets  
"swimmer_unified_datetime"

#' Event Markers Swimmer Plot Example Data  
#'
#' A longitudinal dataset designed to demonstrate event marker functionality.
#' Shows multiple timeline segments per patient with different clinical events,
#' treatments, and adverse event management.
#'
#' @format A data frame with 20 rows and 8 variables:
#' \describe{
#'   \item{PatientID}{Character. Patient identifiers (multiple rows per patient)}
#'   \item{StartTime}{Numeric. Segment start time in months}
#'   \item{EndTime}{Numeric. Segment end time in months}  
#'   \item{Response}{Character. Response during this segment (CR, PR, SD, PD)}
#'   \item{EventType}{Character. Type of clinical event (Treatment Start, Dose Escalation, etc.)}
#'   \item{EventTime}{Numeric. Time when the event occurred}
#'   \item{Severity}{Character. Event severity (None, Mild, Moderate, Severe)}
#'   \item{Cycle}{Numeric. Treatment cycle number}
#' }
#' 
#' @usage data(swimmer_unified_events)
#' 
#' @examples
#' data(swimmer_unified_events)
#' 
#' # Event markers swimmer plot
#' swimmerplot(
#'   data = swimmer_unified_events,
#'   patientID = "PatientID",
#'   startTime = "StartTime", 
#'   endTime = "EndTime",
#'   responseVar = "Response",
#'   showEventMarkers = TRUE,
#'   eventVar = "EventType",
#'   eventTimeVar = "EventTime",
#'   markerSize = 6,
#'   plotTheme = "ggswim"
#' )
#'
#' @source Generated for ClinicoPath package demonstration
#' @family swimmer plot datasets
"swimmer_unified_events"

#' Oncology Clinical Trial Swimmer Plot Data
#'
#' A realistic oncology clinical trial dataset demonstrating comprehensive
#' swimmer plot analysis for regulatory submissions. Includes detailed
#' clinical milestones, tumor characteristics, and biomarker information.
#'
#' @format A data frame with 20 rows and 12 variables:
#' \describe{
#'   \item{PatientID}{Character. Unique patient identifiers (PT001-PT020)}
#'   \item{StartTime}{Numeric. Treatment start time (all patients start at 0)}
#'   \item{EndTime}{Numeric. Treatment end time in months (8-54 months)}
#'   \item{BestResponse}{Character. Best overall response per RECIST (CR, PR, SD, PD)}
#'   \item{Surgery}{Numeric. Time of surgery in months (1-7 months, NA if no surgery)}
#'   \item{Biopsy}{Numeric. Time of biopsy in months (0-4 months)}
#'   \item{FirstResponse}{Numeric. Time of first documented response (6-24 months, NA if no response)}
#'   \item{Progression}{Numeric. Time of disease progression (6-36 months, NA if no progression)}
#'   \item{DeathLastFU}{Numeric. Time of death or last follow-up (8-30 months, NA if alive)}
#'   \item{TumorType}{Character. Primary tumor type (Lung, Breast, Melanoma, Colon, Liver)}
#'   \item{Stage}{Character. Disease stage (IIA, IIB, IIIA, IIIB, IV)}
#'   \item{Biomarker}{Character. Relevant biomarker status (PD-L1+, HER2+, BRAF+, etc.)}
#' }
#' 
#' @usage data(swimmer_unified_oncology)
#' 
#' @examples  
#' data(swimmer_unified_oncology)
#' 
#' # Comprehensive oncology swimmer plot
#' swimmerplot(
#'   data = swimmer_unified_oncology,
#'   patientID = "PatientID",
#'   startTime = "StartTime",
#'   endTime = "EndTime",
#'   responseVar = "BestResponse", 
#'   milestone1Name = "Surgery",
#'   milestone1Date = "Surgery",
#'   milestone2Name = "Biopsy",
#'   milestone2Date = "Biopsy",
#'   milestone3Name = "First Response",
#'   milestone3Date = "FirstResponse",
#'   milestone4Name = "Progression", 
#'   milestone4Date = "Progression",
#'   milestone5Name = "Death/Last FU",
#'   milestone5Date = "DeathLastFU",
#'   referenceLines = "protocol",
#'   showInterpretation = TRUE,
#'   personTimeAnalysis = TRUE,
#'   responseAnalysis = TRUE,
#'   plotTheme = "ggswim",
#'   sortOrder = "duration_desc"
#' )
#'
#' @source Generated for ClinicoPath package demonstration based on realistic oncology trial patterns
#' @family swimmer plot datasets
"swimmer_unified_oncology"