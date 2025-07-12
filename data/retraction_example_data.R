#' Example DOI data for retraction checking
#'
#' A dataset containing example DOIs in various formats for testing the 
#' retracted function. Includes valid DOIs, invalid formats, and missing values
#' to demonstrate different scenarios.
#'
#' @format A data frame with 8 rows and 4 variables:
#' \describe{
#'   \item{doi}{DOI strings in various formats}
#'   \item{title}{Paper titles for context}
#'   \item{journal}{Journal names}
#'   \item{year}{Publication year}
#' }
#'
#' @details
#' The dataset includes:
#' - Valid DOIs in different formats (bare, with prefix, full URL)
#' - Invalid DOI formats to test error handling
#' - Missing values to test NA handling
#' - A mix of potentially retracted and valid papers
#'
#' @examples
#' \dontrun{
#' # Load the data
#' data(retraction_example_data)
#' 
#' # Check for retractions
#' result <- retracted(data = retraction_example_data, doi = "doi")
#' 
#' # View results
#' result$summary
#' }
#'
#' @source Example DOIs created for demonstration purposes
"retraction_example_data"