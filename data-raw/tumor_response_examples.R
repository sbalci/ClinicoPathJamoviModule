#' Example Tumor Response Datasets
#'
#' Example datasets demonstrating different scenarios for tumor response analysis
#'
#' @format A list containing 3 data frames:
#' \describe{
#'   \item{raw_with_time}{Raw tumor measurements with time points (20 obs. of 3 variables)}
#'   \item{percent_with_time}{Pre-calculated response percentages with time (20 obs. of 3 variables)}
#'   \item{percent_no_time}{Pre-calculated response percentages without time (10 obs. of 2 variables)}
#' }
#' @details
#' Each dataset represents a different scenario:
#' \itemize{
#'   \item raw_with_time: Longitudinal raw measurements
#'   \item percent_with_time: Longitudinal percentage changes
#'   \item percent_no_time: Single timepoint percentage changes
#' }
#'
#' @examples
#' data(tumor_response_examples)
#' head(tumor_response_examples$raw_with_time)
#' head(tumor_response_examples$percent_no_time)
# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

"tumor_response_examples"
