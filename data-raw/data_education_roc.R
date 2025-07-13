
#' Educational Assessment ROC Analysis Example Data
#'
#' A dataset containing assessment scores and pass/fail outcomes for ROC analysis.
#' This dataset simulates an educational scenario with multiple assessment methods.
#'
#' @format A data frame with 250 rows and 7 variables:
#' \describe{
#'   \item{student_id}{Student identifier}
#'   \item{pass_status}{Binary outcome: "Fail" or "Pass"}
#'   \item{exam_score}{Traditional exam score (0-100)}
#'   \item{project_score}{Project-based assessment score (0-100)}
#'   \item{peer_score}{Peer evaluation score (0-100)}
#'   \item{study_hours}{Weekly study hours}
#'   \item{class_section}{Class section: "Section_A", "Section_B", or "Section_C"}
#' }
#'
#' @details
#' This dataset demonstrates ROC analysis in educational assessment, showing:
#' \itemize{
#'   \item Comparison of different assessment methods
#'   \item Optimal cutoff determination for pass/fail decisions
#'   \item Subgroup analysis by class section
#' }
#'
#' @source Simulated data for demonstration purposes
#' @keywords datasets
"education_roc_data"
