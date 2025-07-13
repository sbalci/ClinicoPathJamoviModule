
#' Manufacturing Quality Control ROC Analysis Example Data
#'
#' A dataset containing quality measurements and defect status for ROC analysis.
#' This dataset simulates a manufacturing quality control scenario.
#'
#' @format A data frame with 350 rows and 7 variables:
#' \describe{
#'   \item{product_id}{Product identifier}
#'   \item{quality_status}{Binary outcome: "Pass" or "Defect"}
#'   \item{dimension_score}{Dimensional accuracy score}
#'   \item{surface_score}{Surface quality score}
#'   \item{strength_score}{Material strength score}
#'   \item{batch_number}{Production batch identifier}
#'   \item{production_line}{Production line: "Line_1", "Line_2", or "Line_3"}
#' }
#'
#' @details
#' This dataset demonstrates ROC analysis in quality control:
#' \itemize{
#'   \item Quality inspection optimization
#'   \item Defect detection thresholds
#'   \item Multi-criteria quality assessment
#'   \item Production line comparison
#' }
#'
#' @source Simulated data for demonstration purposes
#' @keywords datasets
"manufacturing_roc_data"
