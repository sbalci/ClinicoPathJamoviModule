#' Test Data for groomecompare Function
#'
#' @description
#' Synthetic datasets for testing and demonstrating the \code{groomecompare} function
#' (Groome Staging System Comparison for Survival Data).
#'
#' @format
#' \strong{groomecompare_test}: Standard dataset with 150 observations and 7 variables:
#' \describe{
#'   \item{patient_id}{Character. Patient identifier (PT001-PT150)}
#'   \item{time}{Numeric. Survival time in months (exponential distribution, mean ~20)}
#'   \item{event}{Factor. Event indicator (0 = censored, 1 = death/event). Event rate ~60%}
#'   \item{age}{Numeric. Patient age in years (40-80)}
#'   \item{sex}{Factor. Patient sex (Male, Female)}
#'   \item{ypTNM}{Ordered factor. Post-neoadjuvant pathological staging (I, II, III, IV)}
#'   \item{RPA}{Ordered factor. Recursive partitioning risk groups (Low, Intermediate, High Risk)}
#' }
#'
#' \strong{groomecompare_small}: Small sample dataset with 60 observations and 7 variables:
#' \describe{
#'   \item{patient_id}{Character. Patient identifier (SM01-SM60)}
#'   \item{time}{Numeric. Survival time in months}
#'   \item{event}{Factor. Event indicator (0, 1)}
#'   \item{age}{Numeric. Patient age in years}
#'   \item{sex}{Factor. Patient sex}
#'   \item{ypTNM}{Ordered factor. Tumor staging (I, II, III, IV)}
#'   \item{RPA}{Ordered factor. Risk groups (Low, Intermediate, High Risk)}
#' }
#'
#' \strong{groomecompare_large}: Large dataset with 300 observations and 8 variables:
#' \describe{
#'   \item{patient_id}{Character. Patient identifier (LG001-LG300)}
#'   \item{time}{Numeric. Survival time in months}
#'   \item{event}{Factor. Event indicator (0, 1). Event rate ~55%}
#'   \item{age}{Numeric. Patient age in years}
#'   \item{sex}{Factor. Patient sex}
#'   \item{AJCC8}{Ordered factor. AJCC 8th edition staging (IA, IB, IIA, IIB, IIIA, IIIB, IIIC, IV)}
#'   \item{RPA5}{Ordered factor. 5-group RPA classification (Group 1-5)}
#'   \item{grade}{Ordered factor. Tumor grade (1, 2, 3)}
#' }
#'
#' \strong{Special test datasets}:
#' \describe{
#'   \item{groomecompare_unbalanced}{120 observations with unbalanced staging systems (5 vs 2 groups)}
#'   \item{groomecompare_tied}{80 observations with many tied survival times}
#'   \item{groomecompare_identical}{100 observations with identical staging systems (negative control)}
#'   \item{groomecompare_clear_winner}{150 observations where one system is clearly superior}
#'   \item{groomecompare_edge_truefalse}{40 observations with event coded as TRUE/FALSE}
#'   \item{groomecompare_edge_12}{40 observations with event coded as 1/2}
#' }
#'
#' @details
#' These datasets were generated using a seeded random number generator to produce
#' realistic survival data for testing Groome staging system comparison with the
#' following characteristics:
#'
#' \itemize{
#'   \item Survival times follow exponential distribution
#'   \item Event rates are clinically realistic (55-65%)
#'   \item Prognostic correlations built in (advanced stage → shorter survival)
#'   \item Both systems have predictive value but differ in discrimination
#'   \item Sufficient events per variable (EPV > 10) for Cox models
#' }
#'
#' The Groome method (Groome et al., 2001) compares staging systems using four criteria:
#' \enumerate{
#'   \item \strong{Hazard Consistency}: Monotonicity of hazard ratios across stages
#'   \item \strong{Hazard Discrimination}: Range/spread of hazard ratios between stages
#'   \item \strong{Sample Balance}: Distribution of patients across staging groups
#'   \item \strong{Outcome Prediction}: C-index/concordance for outcome prediction
#' }
#'
#' The data generation process ensures:
#' \itemize{
#'   \item Non-negative survival times
#'   \item Proper factor level ordering (ordinal staging variables)
#'   \item Realistic clinical distributions
#'   \item Differential performance between staging systems
#'   \item Sufficient sample sizes for comparison
#' }
#'
#' @section File Formats:
#' Each dataset is available in multiple formats:
#' \itemize{
#'   \item \strong{RDA}: Native R format (use \code{data()})
#'   \item \strong{CSV}: Comma-separated values (data/nonrda/)
#'   \item \strong{XLSX}: Excel format (data/nonrda/)
#'   \item \strong{OMV}: jamovi native format (data/nonrda/)
#' }
#'
#' Multi-sheet workbook \code{groomecompare_all_scenarios.xlsx} contains all test scenarios.
#'
#' @section Usage Examples:
#' See \code{vignette("groomecompare-examples")} for comprehensive examples.
#'
#' Basic usage:
#' \preformatted{
#' data(groomecompare_test)
#' library(ClinicoPath)
#'
#' # Standard Groome comparison
#' groomecompare(
#'   data = groomecompare_test,
#'   time = "time",
#'   event = "event",
#'   stage1 = "ypTNM",
#'   stage2 = "RPA",
#'   stage1name = "ypTNM Staging",
#'   stage2name = "RPA Classification"
#' )
#'
#' # With bootstrap validation
#' groomecompare(
#'   data = groomecompare_test,
#'   time = "time",
#'   event = "event",
#'   stage1 = "ypTNM",
#'   stage2 = "RPA",
#'   bootstrap = TRUE,
#'   nboot = 100,
#'   seed = 12345
#' )
#'
#' # Test different event coding
#' data(groomecompare_edge_truefalse)
#' groomecompare(
#'   data = groomecompare_edge_truefalse,
#'   time = "time",
#'   event = "event_tf",
#'   stage1 = "ypTNM",
#'   stage2 = "RPA",
#'   eventValue = "TRUE"
#' )
#' }
#'
#' @section Testing Scenarios:
#' The datasets support testing of:
#' \enumerate{
#'   \item \strong{Standard comparison}: Use \code{groomecompare_test} with two staging systems
#'   \item \strong{Small samples}: Use \code{groomecompare_small}, test sample size warnings
#'   \item \strong{Complex systems}: Use \code{groomecompare_large} with detailed AJCC8 vs RPA5
#'   \item \strong{Unbalanced groups}: Use \code{groomecompare_unbalanced} (5 groups vs 2 groups)
#'   \item \strong{Tied times}: Use \code{groomecompare_tied} to test tie handling
#'   \item \strong{Identical systems}: Use \code{groomecompare_identical} (negative control, all metrics ~0.5)
#'   \item \strong{Clear winner}: Use \code{groomecompare_clear_winner} where one system dominates
#'   \item \strong{Event coding}: Test TRUE/FALSE and 1/2 coding schemes
#'   \item \strong{All visualizations}: Test radar plots, bar plots, Kaplan-Meier curves
#'   \item \strong{Bootstrap validation}: Test with bootstrap=TRUE, nboot=100-500
#' }
#'
#' @section Validation:
#' All datasets have been validated for:
#' \itemize{
#'   \item Non-negative survival times
#'   \item Appropriate event rates (55-65%)
#'   \item Stage-survival correlation (advanced stage → worse prognosis)
#'   \item Sufficient EPV (events per variable > 10) for Cox models
#'   \item Realistic clinical distributions
#'   \item Proper factor level ordering for ordinal staging
#'   \item Differential performance between systems (for comparison testing)
#' }
#'
#' @section Groome Criteria:
#' The four Groome criteria used for staging system comparison:
#' \itemize{
#'   \item \strong{Hazard Consistency (Rank 1-2)}: Monotonicity of hazard ratios
#'   \item \strong{Hazard Discrimination (Rank 1-2)}: Spread of hazard ratios
#'   \item \strong{Sample Balance (Rank 1-2)}: Distribution across stages
#'   \item \strong{Outcome Prediction (Rank 1-2)}: C-index comparison
#'   \item \strong{Overall Rank}: Sum of individual ranks (lower is better)
#' }
#'
#' @source
#' Generated synthetically using \code{data-raw/groomecompare_test_data.R}.
#' Seed: 12345. Generation date: 2026-01-31.
#'
#' @references
#' Groome PA, Schulze K, Boysen M, Hall S, Mackillop WJ. (2001).
#' A comparison of published head and neck stage groupings in carcinomas
#' of the oral cavity. Head Neck, 23(8):613-624.
#' \doi{10.1002/hed.1089}
#'
#' Balci S, Altinay S. (2025). Comparison of prognostic staging systems
#' in gastrointestinal neuroendocrine tumors using Groome method.
#' Turk Patoloji Derg, 41(1):1-10.
#' \doi{10.5146/tjpath.2023.01590}
#'
#' @seealso
#' \code{\link{groomecompare}} for the main analysis function
#'
#' \code{vignette("groomecompare-examples")} for comprehensive usage examples
#'
#' \code{\link{rpasurvival_test_data}} for RPA survival analysis test data
#'
#' @examples
#' # Load standard test data
#' data(groomecompare_test)
#'
#' # Examine structure
#' str(groomecompare_test)
#'
#' # Summary statistics
#' summary(groomecompare_test)
#'
#' # Check event rate
#' table(groomecompare_test$event)
#' prop.table(table(groomecompare_test$event))
#'
#' # Check staging distributions
#' table(groomecompare_test$ypTNM)
#' table(groomecompare_test$RPA)
#'
#' # Cross-tabulation of staging systems
#' table(groomecompare_test$ypTNM, groomecompare_test$RPA)
#'
#' # Basic Groome comparison
#' \dontrun{
#' library(ClinicoPath)
#' result <- groomecompare(
#'   data = groomecompare_test,
#'   time = "time",
#'   event = "event",
#'   stage1 = "ypTNM",
#'   stage2 = "RPA",
#'   radarplot = TRUE,
#'   barplot = TRUE,
#'   kmplots = TRUE
#' )
#' }
#'
#' @name groomecompare_test_data
#' @aliases groomecompare_test groomecompare_small groomecompare_large
#'   groomecompare_unbalanced groomecompare_tied groomecompare_identical
#'   groomecompare_clear_winner groomecompare_edge_truefalse groomecompare_edge_12
#' @docType data
#' @keywords datasets
NULL
