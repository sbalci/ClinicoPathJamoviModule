#' Test Data for rpasurvival Function
#'
#' @description
#' Synthetic datasets for testing and demonstrating the \code{rpasurvival} function
#' (Recursive Partitioning Analysis for Survival Data).
#'
#' @format
#' \strong{rpasurvival_test}: Standard dataset with 200 observations and 11 variables:
#' \describe{
#'   \item{patient_id}{Character. Patient identifier (PT001-PT200)}
#'   \item{time}{Numeric. Survival time in months (range: 0.5-120, mean ~36)}
#'   \item{event}{Factor. Event indicator (0 = censored, 1 = death/event). Event rate ~65\%}
#'   \item{age}{Numeric. Patient age in years (40-85, mean ~65)}
#'   \item{stage}{Ordered factor. Tumor stage (I, II, III, IV)}
#'   \item{grade}{Ordered factor. Tumor grade (G1, G2, G3)}
#'   \item{LVI}{Factor. Lymphovascular invasion (Absent, Present)}
#'   \item{tumor_size}{Numeric. Tumor size in centimeters (0.5-10)}
#'   \item{ki67}{Numeric. Ki-67 proliferation index, percentage (0-100). ~3\% missing}
#'   \item{performance_status}{Ordered factor. ECOG performance status (0, 1, 2)}
#'   \item{treatment}{Factor. Treatment modality (Surgery only, Surgery + Chemo, Surgery + Radio, Trimodal)}
#' }
#'
#' \strong{rpasurvival_small}: Minimal dataset with 50 observations and 6 variables:
#' \describe{
#'   \item{patient_id}{Character. Patient identifier (SM01-SM50)}
#'   \item{time}{Numeric. Survival time in months}
#'   \item{event}{Factor. Event indicator (0, 1)}
#'   \item{age}{Numeric. Patient age in years}
#'   \item{stage}{Factor. Tumor stage (Early, Advanced)}
#'   \item{grade}{Factor. Tumor grade (Low, High)}
#' }
#'
#' \strong{rpasurvival_large}: Large dataset with 500 observations and 11 variables:
#' \describe{
#'   \item{patient_id}{Character. Patient identifier (LG0001-LG0500)}
#'   \item{time}{Numeric. Survival time in months}
#'   \item{event}{Factor. Event indicator (0, 1). Event rate ~70\%}
#'   \item{age}{Numeric. Patient age in years}
#'   \item{stage}{Ordered factor. Detailed tumor stage (IA, IB, IIA, IIB, IIIA, IIIB, IV)}
#'   \item{grade}{Ordered factor. Tumor grade (1, 2, 3)}
#'   \item{LVI}{Factor. Lymphovascular invasion (No, Yes)}
#'   \item{PNI}{Factor. Perineural invasion (No, Yes)}
#'   \item{tumor_size}{Numeric. Tumor size in centimeters}
#'   \item{nodes_positive}{Numeric. Number of positive lymph nodes}
#'   \item{biomarker1}{Numeric. Continuous biomarker 1}
#'   \item{biomarker2}{Numeric. Continuous biomarker 2}
#' }
#'
#' \strong{Edge case datasets} (for testing different event/time coding):
#' \describe{
#'   \item{rpasurvival_edge_truefalse}{30 observations with event coded as TRUE/FALSE}
#'   \item{rpasurvival_edge_12}{30 observations with event coded as 1/2}
#'   \item{rpasurvival_edge_days}{30 observations with time in days}
#'   \item{rpasurvival_edge_years}{30 observations with time in years}
#' }
#'
#' @details
#' These datasets were generated using a seeded random number generator to produce
#' realistic survival data with the following characteristics:
#'
#' \itemize{
#'   \item Survival times follow exponential distribution
#'   \item Event rates are clinically realistic (60-70\%)
#'   \item Prognostic correlations built in (Stage IV → shorter survival)
#'   \item Missing data pattern (~3\% in continuous biomarkers)
#'   \item Events-per-variable (EPV) ratio > 10 for all datasets
#' }
#'
#' The data generation process ensures:
#' \itemize{
#'   \item Non-negative survival times
#'   \item Proper factor level ordering (ordinal variables)
#'   \item Realistic clinical distributions
#'   \item Sufficient sample sizes for RPA analysis
#' }
#'
#' @section File Formats:
#' Each dataset is available in multiple formats:
#' \itemize{
#'   \item \strong{RDA}: Native R format (use \code{data()})
#'   \item \strong{CSV}: Comma-separated values
#'   \item \strong{XLSX}: Excel format
#'   \item \strong{OMV}: jamovi native format
#' }
#'
#' @section Usage Examples:
#' See \code{vignette("rpasurvival-examples")} for comprehensive examples.
#'
#' Basic usage:
#' \preformatted{
#' data(rpasurvival_test)
#' library(ClinicoPath)
#'
#' # Standard RPA analysis
#' rpasurvival(
#'   data = rpasurvival_test,
#'   time = "time",
#'   event = "event",
#'   predictors = c("age", "stage", "grade", "LVI"),
#'   time_unit = "months"
#' )
#'
#' # Test small sample warnings
#' data(rpasurvival_small)
#' rpasurvival(
#'   data = rpasurvival_small,
#'   time = "time",
#'   event = "event",
#'   predictors = c("stage", "grade")
#' )
#'
#' # Test different event coding
#' data(rpasurvival_edge_truefalse)
#' rpasurvival(
#'   data = rpasurvival_edge_truefalse,
#'   time = "time",
#'   event = "event_tf",
#'   predictors = c("stage", "grade"),
#'   eventValue = "TRUE"
#' )
#' }
#'
#' @section Testing Scenarios:
#' The datasets support testing of:
#' \enumerate{
#'   \item \strong{Standard analysis}: Use \code{rpasurvival_test} with 4-6 predictors
#'   \item \strong{Small samples}: Use \code{rpasurvival_small}, expect warnings
#'   \item \strong{Complex trees}: Use \code{rpasurvival_large} with maxdepth=5
#'   \item \strong{Event coding}: Test TRUE/FALSE and 1/2 coding schemes
#'   \item \strong{Time units}: Test days, months, years with time_unit parameter
#'   \item \strong{Missing data}: Verify handling of ~3\% missing values
#'   \item \strong{Mixed predictors}: Continuous, ordinal, and nominal variables
#' }
#'
#' @section Validation:
#' All datasets have been validated for:
#' \itemize{
#'   \item Non-negative survival times
#'   \item Appropriate event rates
#'   \item Stage-survival correlation (higher stage → worse prognosis)
#'   \item Sufficient EPV (events per variable > 10)
#'   \item Realistic clinical distributions
#'   \item Proper factor level ordering
#' }
#'
#' @source
#' Generated synthetically using \code{data-raw/rpasurvival_test_data.R}.
#' Seed: 12345. Generation date: 2026-01-31.
#'
#' @references
#' Liu Y, et al. (2026). Recursive partitioning analysis for survival data.
#'
#' @seealso
#' \code{\link{rpasurvival}} for the main analysis function
#'
#' \code{vignette("rpasurvival-examples")} for comprehensive usage examples
#'
#' @examples
#' # Load standard test data
#' data(rpasurvival_test)
#'
#' # Examine structure
#' str(rpasurvival_test)
#'
#' # Summary statistics
#' summary(rpasurvival_test)
#'
#' # Check event rate
#' table(rpasurvival_test$event)
#' prop.table(table(rpasurvival_test$event))
#'
#' # Check stage distribution
#' table(rpasurvival_test$stage)
#'
#' # Basic RPA analysis
#' \dontrun{
#' library(ClinicoPath)
#' result <- rpasurvival(
#'   data = rpasurvival_test,
#'   time = "time",
#'   event = "event",
#'   predictors = c("age", "stage", "grade", "LVI")
#' )
#' }
#'
#' @name rpasurvival_test_data
#' @aliases rpasurvival_test rpasurvival_small rpasurvival_large
#'   rpasurvival_edge_truefalse rpasurvival_edge_12
#'   rpasurvival_edge_days rpasurvival_edge_years
#' @docType data
#' @keywords datasets
NULL
