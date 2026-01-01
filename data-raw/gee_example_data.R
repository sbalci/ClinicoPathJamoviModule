# Generate example data for GEE analysis demonstration
# This simulates a pathology study with multiple liver samples per dog

# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

set.seed(42)

# Number of dogs and samples per dog
n_dogs <- 50
samples_per_dog <- c(rep(2, 30), rep(3, 15), rep(4, 5))  # Unbalanced design

# Generate data
gee_liver_data <- data.frame()

for (i in 1:n_dogs) {
  n_samples <- samples_per_dog[i]

  # Dog-level characteristics (same for all samples from same dog)
  age <- round(rnorm(1, mean = 6, sd = 2), 1)
  breed <- sample(c("Labrador", "German Shepherd", "Poodle", "Mixed"), 1,
                  prob = c(0.3, 0.25, 0.20, 0.25))

  # Generate correlated samples from the same dog
  # Dogs with higher age tend to have higher fibrosis scores
  baseline_fibrosis <- max(0, rnorm(1, mean = age * 0.3, sd = 1))

  for (j in 1:n_samples) {
    # Sample-specific characteristics
    # Fibrosis score: correlated within dog, with some sample-level variation
    fibrosis_score <- round(baseline_fibrosis + rnorm(1, mean = 0, sd = 0.5), 1)
    fibrosis_score <- pmax(0, pmin(fibrosis_score, 6))  # Constrain to 0-6 scale

    # Sample method
    sample_method <- sample(c("Fine Needle", "Core Biopsy", "Surgical"), 1,
                           prob = c(0.3, 0.5, 0.2))

    # Pathologist experience (years)
    pathologist_experience <- sample(c(5, 10, 15, 20), 1, prob = c(0.2, 0.3, 0.3, 0.2))

    # Diagnosis: probability increases with fibrosis score and age
    diagnosis_prob <- plogis(-2 + 0.5 * fibrosis_score + 0.1 * age +
                            ifelse(sample_method == "Surgical", 0.5, 0))
    diagnosis <- rbinom(1, 1, diagnosis_prob)
    diagnosis_label <- factor(ifelse(diagnosis == 1, "Positive", "Negative"),
                             levels = c("Negative", "Positive"))

    # Inflammatory cell count (Poisson distributed, increases with fibrosis)
    cell_count <- rpois(1, lambda = exp(1 + 0.3 * fibrosis_score))

    # Time sequence for longitudinal interpretation (if samples are sequential)
    time_point <- j

    gee_liver_data <- rbind(gee_liver_data, data.frame(
      dog_id = sprintf("Dog_%03d", i),
      sample_number = j,
      time_point = time_point,
      age = age,
      breed = breed,
      sample_method = sample_method,
      pathologist_experience = pathologist_experience,
      fibrosis_score = fibrosis_score,
      diagnosis = diagnosis_label,
      cell_count = cell_count
    ))
  }
}

# Convert categorical variables to factors
gee_liver_data$dog_id <- factor(gee_liver_data$dog_id)
gee_liver_data$breed <- factor(gee_liver_data$breed)
gee_liver_data$sample_method <- factor(gee_liver_data$sample_method)

# Add some missing values to make it realistic
missing_indices <- sample(1:nrow(gee_liver_data), size = round(0.02 * nrow(gee_liver_data)))
gee_liver_data$fibrosis_score[missing_indices[1:3]] <- NA
gee_liver_data$cell_count[missing_indices[4:6]] <- NA

# Save the data
use_data_multi_format(gee_liver_data, overwrite = TRUE, save_csv = TRUE)

# Create documentation
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
#' ```
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
#' ```
#'
#' 2. **Count Outcome (Cell Count):**
#' ```
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
#' ```
#'
#' 3. **Continuous Outcome (Fibrosis Score):**
#' ```
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
#' ```
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
#' @source Simulated data generated using `data-raw/gee_example_data.R`
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
