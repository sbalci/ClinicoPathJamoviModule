#' Example Liver Pathology Data for GEE Analysis
#'
#' @description
#' Simulated dataset of liver biopsies from dogs with multiple samples per subject.
#' This dataset is designed to demonstrate Generalized Estimating Equations (GEE)
#' analysis for correlated/clustered data common in pathology studies.
#'
#' @format A data frame with 135 observations and 10 variables:
#' \describe{
#'   \item{dog_id}{Factor. Unique identifier for each dog (cluster ID)}
#'   \item{sample_number}{Integer. Sequential sample number for each dog (1-4)}
#'   \item{time_point}{Integer. Time sequence for longitudinal analysis}
#'   \item{age}{Numeric. Dog's age in years}
#'   \item{breed}{Factor. Dog breed (Labrador, German Shepherd, Poodle, Mixed)}
#'   \item{sample_method}{Factor. Biopsy method (Fine Needle, Core Biopsy, Surgical)}
#'   \item{pathologist_experience}{Numeric. Years of pathologist experience (5, 10, 15, 20)}
#'   \item{fibrosis_score}{Numeric. Liver fibrosis score (0-6 scale, continuous)}
#'   \item{diagnosis}{Factor. Binary diagnosis (Negative, Positive)}
#'   \item{cell_count}{Integer. Inflammatory cell count (Poisson distributed)}
#' }
#'
#' @details
#' **Study Design:**
#' - 50 dogs with 2-4 liver samples each (unbalanced design)
#' - Samples from the same dog are correlated
#' - Represents typical veterinary pathology study with multiple biopsies
#'
#' **Clinical Context:**
#' In veterinary pathology, multiple liver samples are often taken from the same
#' animal to assess disease distribution and severity. These samples are not
#' independent, requiring GEE or mixed models for proper statistical analysis.
#'
#' **Correlation Structure:**
#' - **Within-dog correlation**: Samples from the same dog share baseline characteristics
#' - **Exchangeable structure**: Recommended for this data (samples equally correlated)
#' - **AR(1) structure**: Can be used if time_point represents sequential sampling
#'
#' **Example Analyses:**
#'
#' 1. **Binary Outcome (Diagnosis):**
#' \preformatted{
#' # Predicting diagnosis from fibrosis score and age
#' geemodel(
#'   data = gee_liver_data,
#'   outcome = 'diagnosis',
#'   predictors = c('fibrosis_score', 'age', 'sample_method'),
#'   cluster_id = 'dog_id',
#'   family = 'binomial',
#'   corstr = 'exchangeable',
#'   robust_se = TRUE
#' )
#' }
#'
#' 2. **Count Outcome (Cell Count):**
#' \preformatted{
#' # Modeling inflammatory cell count
#' geemodel(
#'   data = gee_liver_data,
#'   outcome = 'cell_count',
#'   predictors = c('fibrosis_score', 'breed'),
#'   cluster_id = 'dog_id',
#'   family = 'poisson',
#'   corstr = 'exchangeable',
#'   robust_se = TRUE
#' )
#' }
#'
#' 3. **Continuous Outcome (Fibrosis Score):**
#' \preformatted{
#' # Predicting fibrosis from age and sample method
#' geemodel(
#'   data = gee_liver_data,
#'   outcome = 'fibrosis_score',
#'   predictors = c('age', 'sample_method', 'pathologist_experience'),
#'   cluster_id = 'dog_id',
#'   family = 'gaussian',
#'   corstr = 'exchangeable',
#'   robust_se = TRUE
#' )
#' }
#'
#' **Why GEE is Needed:**
#' - Standard regression assumes independent observations
#' - Multiple samples per dog violate independence assumption
#' - GEE accounts for within-dog correlation
#' - Provides valid standard errors and inference
#'
#' **Data Generation:**
#' Data was simulated with realistic correlations:
#' - Dogs with higher age tend to have higher fibrosis scores
#' - Samples from the same dog share baseline disease severity
#' - Diagnosis probability increases with fibrosis score and age
#' - Sample method affects diagnostic accuracy
#'
#' @source Simulated data generated using \code{data-raw/gee_example_data.R}
#'
#' @examples
#' \donttest{
#' # Load data
#' data(gee_liver_data, package = "ClinicoPath")
#'
#' # View structure
#' str(gee_liver_data)
#'
#' # Check cluster sizes
#' table(table(gee_liver_data$dog_id))
#'
#' # Summary statistics by diagnosis
#' aggregate(fibrosis_score ~ diagnosis, data = gee_liver_data,
#'           FUN = function(x) c(mean = mean(x, na.rm = TRUE),
#'                               sd = sd(x, na.rm = TRUE)))
#' }
#'
#' @seealso
#' - [geemodel()] for GEE analysis function
#' - [geepack::geeglm()] for the underlying GEE implementation
#'
#' @keywords datasets
"gee_liver_data"
