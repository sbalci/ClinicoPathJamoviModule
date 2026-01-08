#' Test Datasets for Odds Ratio Function
#'
#' @name oddsratio_test_datasets
#' @aliases oddsratio_test oddsratio_diagnostic oddsratio_casecontrol
#' @aliases oddsratio_multiple oddsratio_small oddsratio_large
#'
#' @format
#' ## `oddsratio_test`
#' Main test dataset (200 observations):
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{outcome}{Binary outcome (Alive/Dead)}
#'   \item{treatment}{Treatment group}
#'   \item{stage}{Disease stage (Early/Advanced)}
#'   \item{biomarker_status}{Biomarker status}
#'   \item{age}{Patient age}
#'   \item{tumor_size}{Tumor size (cm)}
#'   \item{psa_level}{PSA level}
#' }
#'
#' @examples
#' data(oddsratio_test)
#' \dontrun{
#' oddsratio(
#'   data = oddsratio_test,
#'   explanatory = "stage",
#'   outcome = "outcome",
#'   outcomeLevel = "Dead"
#' )
#' }
#'
#' @source Generated using data-raw/oddsratio_test_data.R (seed = 42)
#' @seealso \code{\link{oddsratio}}
"oddsratio_test"

#' @rdname oddsratio_test_datasets
"oddsratio_diagnostic"

#' @rdname oddsratio_test_datasets
"oddsratio_casecontrol"

#' @rdname oddsratio_test_datasets
"oddsratio_multiple"

#' @rdname oddsratio_test_datasets
"oddsratio_small"

#' @rdname oddsratio_test_datasets
"oddsratio_large"
