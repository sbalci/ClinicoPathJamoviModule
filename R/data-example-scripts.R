# Documentation for three datasets whose docs used to sit in data/*.R scripts. A data/x.R next
# to data/x.rda takes precedence in data() and in the LazyData build, and the scripts define no
# object, so the datasets were missing from the installed package.

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
