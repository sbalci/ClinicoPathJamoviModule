#' Example wide format data for river plot visualization
#'
#' A dataset in wide format containing patient progression through multiple 
#' treatment stages. This dataset demonstrates how to create river plots 
#' with cross-sectional data where each column represents a different stage.
#'
#' @format A data frame with 60 rows and 5 variables:
#' \describe{
#'   \item{screening}{Initial screening result: Eligible, Borderline, Ineligible (factor)}
#'   \item{enrollment}{Enrollment status: Enrolled, Declined, Excluded (factor)}
#'   \item{treatment}{Treatment assignment: Drug_A, Drug_B, Placebo (factor)}
#'   \item{outcome}{Final outcome: Success, Partial, Failure (factor)}
#'   \item{total_cost}{Total cost across all stages (numeric)}
#' }
#'
#' @details
#' The dataset represents a clinical trial progression funnel with:
#' - 60 participants across 4 stages
#' - Each stage represents a decision point or outcome
#' - Total costs accumulating across the entire patient journey
#' - Realistic proportions reflecting clinical trial attrition
#' 
#' This wide format data is ideal for creating:
#' - Multi-stage funnel visualizations
#' - Treatment allocation flow diagrams
#' - Cross-sectional pathway analysis
#'
#' @examples
#' \dontrun{
#' # Load the data
#' data(riverplot_wide_example_data)
#' 
#' # Multi-stage flow diagram
#' riverplot(
#'   data = riverplot_wide_example_data,
#'   strata = c("screening", "enrollment", "treatment", "outcome"),
#'   plotType = "alluvial",
#'   fillType = "first"
#' )
#' 
#' # Sankey diagram with costs
#' riverplot(
#'   data = riverplot_wide_example_data,
#'   strata = c("screening", "enrollment", "treatment"),
#'   weight = "total_cost",
#'   plotType = "sankey",
#'   labelNodes = TRUE
#' )
#' }
#'
#' @source Simulated clinical trial funnel data for demonstration purposes
"riverplot_wide_example_data"