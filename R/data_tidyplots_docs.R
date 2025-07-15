#' Comprehensive Test Datasets for Tidyplots Function
#'
#' A collection of datasets designed to showcase the comprehensive features
#' of the tidyplots function, including various plot types, statistical elements,
#' and customization options.
#'
#' @name tidyplots_datasets
#' @docType data
#' @aliases tidyplots_medical tidyplots_education tidyplots_business tidyplots_environmental tidyplots_demo
#'
#' @description
#' These datasets provide comprehensive examples for testing and demonstrating
#' the tidyplots function capabilities across different domains and use cases.
#' Each dataset is designed to highlight specific features and best practices
#' for scientific visualization.
#'
#' @details
#' The collection includes five specialized datasets:
#'
#' \strong{1. Medical Research Dataset (tidyplots_medical):}
#' \itemize{
#'   \item Clinical trial data with treatment effects
#'   \item Variables: patient characteristics, treatments, biomarkers, outcomes
#'   \item Use cases: Treatment comparisons, biomarker analysis, clinical outcomes
#'   \item Sample size: 2,160 observations
#' }
#'
#' \strong{2. Educational Research Dataset (tidyplots_education):}
#' \itemize{
#'   \item Student performance across teaching methods
#'   \item Variables: student demographics, teaching methods, test scores
#'   \item Use cases: Educational effectiveness, performance analysis
#'   \item Sample size: 4,800 observations
#' }
#'
#' \strong{3. Business Analytics Dataset (tidyplots_business):}
#' \itemize{
#'   \item Sales performance and marketing strategy analysis
#'   \item Variables: sales metrics, marketing strategies, regional data
#'   \item Use cases: Business intelligence, sales analysis, performance tracking
#'   \item Sample size: 48,000 observations
#' }
#'
#' \strong{4. Environmental Monitoring Dataset (tidyplots_environmental):}
#' \itemize{
#'   \item Environmental measurements across monitoring stations
#'   \item Variables: temperature, humidity, air quality, location types
#'   \item Use cases: Environmental monitoring, seasonal analysis, pollution tracking
#'   \item Sample size: 1,728 observations
#' }
#'
#' \strong{5. Simple Demo Dataset (tidyplots_demo):}
#' \itemize{
#'   \item Basic dataset for tutorials and quick demonstrations
#'   \item Variables: groups, categories, treatments, scores
#'   \item Use cases: Learning tidyplots, quick examples, method demonstrations
#'   \item Sample size: 100 observations
#' }
#'
#' @format
#' Each dataset is a data frame with the following characteristics:
#'
#' \strong{tidyplots_medical:}
#' \describe{
#'   \item{patient_id}{Unique patient identifier}
#'   \item{treatment}{Treatment group: Control, Drug_A, Drug_B}
#'   \item{gender}{Patient gender: Male, Female}
#'   \item{age_group}{Age category: Young, Middle, Elderly}
#'   \item{age}{Continuous age variable}
#'   \item{bp_reduction}{Primary outcome: Blood pressure reduction}
#'   \item{biomarker_1, biomarker_2}{Secondary biomarker measurements}
#'   \item{qol_score}{Quality of life score (0-100)}
#'   \item{timepoint}{Study timepoint: Baseline, Week_4, Week_8, Week_12}
#'   \item{hospital}{Study site: Hospital_A, Hospital_B, Hospital_C}
#'   \item{response}{Response category: High_Response, Moderate_Response, Low_Response}
#' }
#'
#' \strong{tidyplots_education:}
#' \describe{
#'   \item{student_id}{Unique student identifier}
#'   \item{teaching_method}{Teaching approach: Traditional, Interactive, Online, Hybrid}
#'   \item{subject}{Academic subject: Math, Science, Literature}
#'   \item{school_type}{School type: Public, Private}
#'   \item{grade_level}{Student grade level (9-12)}
#'   \item{pre_test_score, post_test_score}{Test scores before and after intervention}
#'   \item{improvement}{Score improvement (post - pre)}
#'   \item{engagement_score}{Student engagement measure (0-100)}
#'   \item{study_hours}{Weekly study hours}
#'   \item{performance_level}{Performance category: Excellent, Good, Satisfactory, Needs_Improvement}
#' }
#'
#' \strong{tidyplots_business:}
#' \describe{
#'   \item{salesperson_id}{Unique salesperson identifier}
#'   \item{marketing_strategy}{Strategy: Email, Social_Media, Direct_Mail, Phone, Mixed}
#'   \item{product_category}{Product type: Electronics, Clothing, Home_Garden, Sports}
#'   \item{region}{Geographic region: North, South, East, West}
#'   \item{quarter}{Business quarter: Q1, Q2, Q3, Q4}
#'   \item{experience_years}{Years of sales experience}
#'   \item{sales_amount}{Total sales revenue}
#'   \item{leads_generated}{Number of sales leads}
#'   \item{conversion_rate}{Lead conversion percentage}
#'   \item{customer_satisfaction}{Customer satisfaction score (1-10)}
#'   \item{performance_tier}{Performance category: Top_Performer, High_Performer, Average_Performer, Needs_Improvement}
#' }
#'
#' \strong{tidyplots_environmental:}
#' \describe{
#'   \item{station_id}{Monitoring station identifier}
#'   \item{month}{Calendar month}
#'   \item{measurement_type}{Type: Temperature, Humidity, Air_Quality, Noise_Level}
#'   \item{location_type}{Location: Urban, Suburban, Rural}
#'   \item{value}{Measurement value (units vary by type)}
#'   \item{data_quality}{Quality indicator: High, Medium, Low}
#'   \item{alert_level}{Alert status: Normal, Warning, Critical}
#'   \item{season}{Season grouping: Winter, Spring, Summer, Fall}
#' }
#'
#' \strong{tidyplots_demo:}
#' \describe{
#'   \item{group}{Group identifier: Group_A, Group_B, Group_C, Group_D}
#'   \item{category}{Category: Category_1, Category_2}
#'   \item{treatment}{Treatment: Control, Treatment}
#'   \item{time_point}{Time: Baseline, Follow_up}
#'   \item{score}{Main outcome variable}
#'   \item{measurement_1, measurement_2}{Additional continuous variables}
#'   \item{success}{Binary outcome variable}
#'   \item{count_var}{Count/discrete variable}
#' }
#'
#' @examples
#' \dontrun{
#' # Load all datasets
#' data(tidyplots_datasets)
#' 
#' # Load individual datasets
#' data(tidyplots_medical)
#' data(tidyplots_education)
#' data(tidyplots_business)
#' data(tidyplots_environmental) 
#' data(tidyplots_demo)
#'
#' # Basic scatter plot with medical data
#' tidyplots(data = tidyplots_medical,
#'           xvar = "age",
#'           yvar = "bp_reduction",
#'           color = "treatment")
#'
#' # Box plot with statistical elements
#' tidyplots(data = tidyplots_medical,
#'           xvar = "treatment",
#'           yvar = "bp_reduction",
#'           plotType = "boxplot",
#'           color = "treatment",
#'           showMean = TRUE,
#'           showCI = TRUE,
#'           colorScheme = "friendly")
#'
#' # Violin plot with education data
#' tidyplots(data = tidyplots_education,
#'           xvar = "teaching_method",
#'           yvar = "improvement",
#'           plotType = "violin",
#'           color = "school_type",
#'           violinPoints = TRUE,
#'           showMedian = TRUE,
#'           colorScheme = "seaside")
#'
#' # Line plot with environmental data
#' tidyplots(data = tidyplots_environmental[tidyplots_environmental$measurement_type == "Temperature", ],
#'           xvar = "month",
#'           yvar = "value",
#'           plotType = "line",
#'           color = "location_type",
#'           lineType = "mean",
#'           showCI = TRUE,
#'           ciType = "ribbon")
#'
#' # Histogram with demo data
#' tidyplots(data = tidyplots_demo,
#'           xvar = "score",
#'           plotType = "histogram",
#'           color = "group",
#'           histogramBins = 20,
#'           showDistribution = TRUE,
#'           distributionType = "density")
#'
#' # Faceted plot with business data
#' tidyplots(data = tidyplots_business,
#'           xvar = "experience_years",
#'           yvar = "sales_amount",
#'           color = "marketing_strategy",
#'           facet = "region",
#'           plotType = "points",
#'           pointType = "jitter",
#'           showMean = TRUE,
#'           colorScheme = "viridis")
#'
#' # Bar plot with statistical testing
#' tidyplots(data = tidyplots_medical,
#'           xvar = "treatment",
#'           yvar = "qol_score",
#'           plotType = "bar",
#'           color = "treatment",
#'           barType = "mean",
#'           showSEM = TRUE,
#'           showPValue = TRUE,
#'           showSignificance = TRUE)
#' }
#'
#' @source
#' These datasets were created using simulated data with realistic patterns
#' and relationships designed to showcase tidyplots functionality across
#' different research domains.
#'
#' @references
#' Bengler, J. (2024). tidyplots: Streamlined plotting with tidy data.
#' \url{https://jbengler.github.io/tidyplots/}
#'
#' @seealso
#' \code{\link{tidyplots}} for the main plotting function
#'
#' @keywords datasets
#' @concept tidyplots
#' @concept visualization
#' @concept test data
NULL