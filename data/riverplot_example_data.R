#' Example data for river plot visualization
#'
#' A dataset containing longitudinal treatment response data for demonstrating 
#' river plots (alluvial diagrams). This dataset tracks patient responses 
#' over multiple timepoints in a clinical trial setting.
#'
#' @format A data frame with 150 rows and 5 variables:
#' \describe{
#'   \item{patient_id}{Patient identifier (character)}
#'   \item{timepoint}{Study timepoint: Baseline, Month3, Month6 (factor)}
#'   \item{treatment_response}{Treatment response category (factor)}
#'   \item{treatment_cost}{Cost of treatment in dollars (numeric)}
#'   \item{therapy_line}{Line of therapy: First, Second, Third (factor)}
#' }
#'
#' @details
#' The dataset represents a longitudinal clinical study with:
#' - 50 patients followed over 3 timepoints
#' - Treatment responses: Complete Response, Partial Response, Stable Disease, Progressive Disease
#' - Variable treatment costs reflecting real-world healthcare economics
#' - Different therapy lines showing treatment progression
#' 
#' This data is ideal for creating:
#' - Alluvial diagrams showing response transitions over time
#' - Weighted river plots using treatment costs
#' - Multi-strata visualizations with therapy lines
#'
#' @examples
#' \dontrun{
#' # Load the data
#' data(riverplot_example_data)
#' 
#' # Basic alluvial plot
#' riverplot(
#'   data = riverplot_example_data,
#'   time = "timepoint",
#'   strata = "treatment_response",
#'   plotType = "alluvial"
#' )
#' 
#' # Weighted river plot with patient tracking
#' riverplot(
#'   data = riverplot_example_data,
#'   id = "patient_id",
#'   time = "timepoint", 
#'   strata = "treatment_response",
#'   weight = "treatment_cost",
#'   plotType = "alluvial",
#'   labelNodes = TRUE
#' )
#' }
#'
#' @source Simulated clinical trial data for demonstration purposes
"riverplot_example_data"