% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_timeroc_docs.R
\docType{data}
\name{timeroc_multi_biomarker}
\alias{timeroc_multi_biomarker}
\title{Multi-Biomarker Comparison Test Dataset}
\format{
A data frame with 250 observations and 10 variables:
\describe{
\item{subject_id}{Character. Unique subject identifier (MB_001 to MB_250)}
\item{age_years}{Integer. Subject age in years}
\item{gender}{Character. Subject gender ("M", "F")}
\item{biomarker_alpha}{Numeric. Excellent predictor biomarker (expected AUC ~0.85)}
\item{biomarker_beta}{Numeric. Good predictor biomarker (expected AUC ~0.75)}
\item{biomarker_gamma}{Numeric. Fair predictor biomarker (expected AUC ~0.65)}
\item{composite_score}{Numeric. Weighted combination of alpha and beta biomarkers}
\item{follow_up_months}{Numeric. Follow-up time in months (0-48)}
\item{primary_event}{Integer. Primary endpoint event (1 = event, 0 = censored)}
\item{cohort}{Character. Study cohort ("Training", "Validation")}
\item{enrollment_year}{Integer. Year of enrollment (2018-2022)}
}
}
\source{
Simulated data generated using create_timeroc_test_data.R
}
\usage{
timeroc_multi_biomarker
}
\description{
Specialized dataset with three biomarkers of varying predictive performance
designed to test comparative time-dependent ROC analysis. Each biomarker
has different signal-to-noise ratios representing excellent, good, and fair
predictive ability.
}
\details{
This dataset is specifically designed to test biomarker ranking and comparison
functionality in time-dependent ROC analysis. The three biomarkers have
systematically different predictive abilities:
\itemize{
\item \strong{biomarker_alpha}: Strong signal (high correlation with outcome)
\item \strong{biomarker_beta}: Moderate signal (medium correlation with outcome)
\item \strong{biomarker_gamma}: Weak signal (low correlation with outcome)
\item \strong{composite_score}: Combined alpha + beta for testing multi-marker models
}

\strong{Key Features:}
\itemize{
\item Controlled predictive performance differences
\item Training/validation split capability
\item 249/250 events (99.6\% event rate)
\item Multi-year enrollment period
\item Realistic biomarker scales and distributions
}

\strong{Recommended TimeROC Parameters:}
\itemize{
\item Timepoints: 6, 12, 18 months
\item Markers: biomarker_alpha, biomarker_beta, biomarker_gamma, composite_score
\item Event: primary_event
\item Time: follow_up_months
}
}
\examples{
\dontrun{
# Load the dataset
data(timeroc_multi_biomarker)

# Compare all biomarkers
markers <- c("biomarker_alpha", "biomarker_beta", "biomarker_gamma")
results <- list()

for(marker in markers) {
  results[[marker]] <- timeroc(
    data = timeroc_multi_biomarker,
    elapsedtime = "follow_up_months",
    outcome = "primary_event",
    marker = marker,
    timepoints = "6, 12, 18"
  )
}

# Extract AUC values for comparison
sapply(results, function(x) x$aucTable$asDF$auc)
}
}
\seealso{
\code{\link{timeroc}}, \code{\link{timeroc_datasets}}
}
\keyword{datasets}
