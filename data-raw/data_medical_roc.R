
#' Medical Diagnostic ROC Analysis Example Data
#'
#' A dataset containing biomarker measurements and disease status for ROC analysis.
#' This dataset simulates a medical diagnostic scenario with multiple biomarkers
#' of varying discriminatory ability.
#'
#' @format A data frame with 300 rows and 8 variables:
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{disease_status}{Binary outcome: "Healthy" or "Disease"}
#'   \item{biomarker1}{Continuous biomarker with good discriminatory ability}
#'   \item{biomarker2}{Continuous biomarker with moderate discriminatory ability}
#'   \item{biomarker3}{Continuous biomarker with poor discriminatory ability}
#'   \item{age}{Patient age in years}
#'   \item{gender}{Patient gender: "Male" or "Female"}
#'   \item{hospital}{Hospital site: "Hospital_A", "Hospital_B", or "Hospital_C"}
#' }
#' 
#' @details
#' This dataset is designed to demonstrate:
#' \itemize{
#'   \item Basic ROC curve analysis
#'   \item Comparison of multiple biomarkers using DeLong test
#'   \item IDI and NRI calculations
#'   \item Subgroup analysis by hospital
#'   \item Various cutpoint optimization methods
#' }
#'
#' The biomarkers have different levels of discrimination:
#' \itemize{
#'   \item biomarker1: Good discriminator (AUC ≈ 0.85)
#'   \item biomarker2: Moderate discriminator (AUC ≈ 0.75)  
#'   \item biomarker3: Poor discriminator (AUC ≈ 0.60)
#' }
#'
#' @source Simulated data for demonstration purposes
#' @keywords datasets
"medical_roc_data"
