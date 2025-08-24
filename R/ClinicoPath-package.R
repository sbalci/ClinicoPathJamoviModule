#' ClinicoPath: Comprehensive Analysis for Clinicopathological Research
#'
#' @description 
#' ClinicoPath is a comprehensive jamovi module designed specifically for 
#' clinicopathological research and medical data analysis. It provides an 
#' integrated suite of statistical tools, visualization methods, and reporting 
#' functions tailored for pathology, clinical research, and medical decision-making.
#'
#' @details 
#' ## Main Analysis Categories
#' 
#' ClinicoPath organizes analysis tools into five main functional areas:
#' 
#' ### ClinicoPath Descriptives
#' Comprehensive descriptive statistics and data exploration tools:
#' \itemize{
#'   \item Table One generation with automated statistical tests
#'   \item Cross-tabulation with chi-square and Fisher's exact tests
#'   \item Data quality assessment and missing data analysis
#'   \item Outlier detection and data validation
#'   \item Summary statistics with clinical interpretation
#' }
#' 
#' ### ClinicoPath Survival
#' Advanced survival analysis for time-to-event data:
#' \itemize{
#'   \item Kaplan-Meier survival curves with risk tables
#'   \item Cox proportional hazards regression
#'   \item Competing risks analysis
#'   \item Log-rank and other survival tests
#'   \item Survival power calculations
#'   \item Swimmer plots for treatment timelines
#' }
#' 
#' ### Medical Decision Analysis (meddecide)
#' Diagnostic test evaluation and clinical decision support:
#' \itemize{
#'   \item ROC curve analysis with confidence intervals
#'   \item Sensitivity, specificity, and predictive values
#'   \item Likelihood ratios and diagnostic odds ratios
#'   \item Decision curve analysis for clinical utility
#'   \item Fagan nomograms for Bayesian diagnosis
#'   \item Net reclassification improvement (NRI) and IDI
#' }
#' 
#' ### Statistical Plots (JJStatsPlot)
#' Advanced statistical visualization with publication-ready graphics:
#' \itemize{
#'   \item Between-groups and within-subjects comparisons
#'   \item Correlation matrices and scatter plots
#'   \item Distribution plots with statistical annotations
#'   \item Effect size visualizations
#'   \item Grouped and stratified analyses
#' }
#' 
#' ### Specialized Visualizations
#' Domain-specific plots for clinical research:
#' \itemize{
#'   \item Waterfall plots for treatment response
#'   \item Alluvial diagrams for patient flow
#'   \item Forest plots for meta-analysis
#'   \item Upset plots for set intersections
#'   \item Age pyramids and demographic visualizations
#' }
#' 
#' ## Key Features
#' 
#' ### Integration with jamovi
#' - User-friendly graphical interface
#' - Point-and-click analysis workflow
#' - Integrated results viewer
#' - Export capabilities for reports
#' 
#' ### Clinical Focus
#' - Terminology and methods familiar to clinicians
#' - Pathology-specific analysis options
#' - Medical decision-making frameworks
#' - Regulatory-compliant reporting
#' 
#' ### Reproducible Research
#' - Syntax generation for R users
#' - Version control integration
#' - Comprehensive documentation
#' - Example datasets included
#' 
#' ### Advanced Statistics
#' - Bootstrap confidence intervals
#' - Multiple comparison corrections
#' - Effect size calculations
#' - Power analysis tools
#' 
#' ## Getting Started
#' 
#' ClinicoPath can be used in multiple ways:
#' 
#' 1. **jamovi Module**: Install through jamovi's module library
#' 2. **R Package**: Install and use functions directly in R
#' 3. **Standalone Functions**: Individual analysis components
#' 
#' ## Example Datasets
#' 
#' The package includes several example datasets:
#' \itemize{
#'   \item \code{histopathology}: Histopathological diagnosis data
#'   \item \code{melanoma}: Melanoma survival data
#'   \item \code{treatmentResponse}: Treatment response analysis
#'   \item Various domain-specific test datasets
#' }
#' 
#' ## Package Architecture
#' 
#' ClinicoPath serves as an umbrella package that coordinates multiple 
#' specialized sub-modules:
#' \itemize{
#'   \item \strong{ClinicoPathDescriptives}: Descriptive statistics
#'   \item \strong{jsurvival}: Survival analysis
#'   \item \strong{meddecide}: Medical decision analysis
#'   \item \strong{jjstatsplot}: Statistical visualizations
#' }
#' 
#' Each module can be used independently or as part of the integrated workflow.
#' 
#' @section Author:
#' Serdar Balci MD, Pathologist
#' 
#' \itemize{
#'   \item Email: \email{serdarbalci@serdarbalci.com}
#'   \item ORCID: \href{https://orcid.org/0000-0002-7852-3851}{0000-0002-7852-3851}
#'   \item Website: \url{https://www.serdarbalci.com/}
#' }
#' 
#' @section Citation:
#' To cite ClinicoPath in publications, please use:
#' 
#' Balci, S. (2025). ClinicoPath: Comprehensive Analysis for Clinicopathological Research. 
#' R package version 0.0.3.58. 
#' \url{https://github.com/sbalci/ClinicoPathJamoviModule}
#' 
#' @section License:
#' GPL-2
#' 
#' @section Links:
#' \itemize{
#'   \item Package website: \url{https://sbalci.github.io/ClinicoPathJamoviModule/}
#'   \item GitHub repository: \url{https://github.com/sbalci/ClinicoPathJamoviModule/}
#'   \item Bug reports: \url{https://github.com/sbalci/ClinicoPathJamoviModule/issues/}
#'   \item jamovi library: \url{https://www.jamovi.org/}
#' }
#' 
#' @examples
#' \dontrun{
#' # Load the package
#' library(ClinicoPath)
#' 
#' # Example 1: Descriptive analysis
#' data(histopathology)
#' summary(histopathology)
#' 
#' # Example 2: Survival analysis (requires jamovi interface)
#' # Use jamovi GUI for interactive analysis
#' 
#' # Example 3: ROC analysis
#' # See meddecide module documentation for examples
#' 
#' # Example 4: Statistical plots
#' # See jjstatsplot module documentation for examples
#' }
#' 
#' @keywords package clinicopathology medical-research survival-analysis 
#'          roc-analysis jamovi pathology statistics visualization
#' 
#' @seealso
#' \itemize{
#'   \item \code{\link[survival]{survival-package}} for survival analysis fundamentals
#'   \item \code{\link[pROC]{pROC-package}} for ROC analysis
#'   \item \code{\link[tableone]{tableone}} for descriptive tables
#'   \item \code{\link[ggplot2]{ggplot2-package}} for visualization
#' }
#' 
#' @docType package
#' @aliases ClinicoPath ClinicoPath-package
#' @name ClinicoPath-package
"_PACKAGE"
