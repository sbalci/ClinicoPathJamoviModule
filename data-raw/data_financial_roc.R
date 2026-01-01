
#' Financial Risk Assessment ROC Analysis Example Data
#'
#' A dataset containing risk indicators and default outcomes for ROC analysis.
#' This dataset simulates a financial risk assessment scenario.
#'
#' @format A data frame with 400 rows and 7 variables:
#' \describe{
#'   \item{client_id}{Client identifier}
#'   \item{default_status}{Binary outcome: "No_Default" or "Default"}
#'   \item{credit_score}{Credit history score (300-850)}
#'   \item{income_debt_ratio}{Income to debt ratio}
#'   \item{employment_score}{Employment stability score (0-10)}
#'   \item{loan_amount}{Loan amount in dollars}
#'   \item{client_type}{Client type: "Individual", "Small_Business", or "Corporate"}
#' }
#'
#' @details
#' This dataset demonstrates ROC analysis in financial risk assessment:
#' \itemize{
#'   \item Credit risk modeling
#'   \item Optimal decision thresholds for loan approval
#'   \item Risk score validation and comparison
#'   \item Cost-benefit analysis using different cost ratios
#' }
#'
#' @source Simulated data for demonstration purposes
#' @keywords datasets
# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

"financial_roc_data"
