% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_toolssummary_docs.R
\docType{data}
\name{toolssummary_timeseries_data}
\alias{toolssummary_timeseries_data}
\title{Time Series Data for Longitudinal Analysis}
\format{
A data frame with 200 observations and 12 variables:
\describe{
\item{subject_id}{Character. Subject identifier (TS_001 to TS_040)}
\item{timepoint}{Factor. Assessment timepoint ("T1", "T2", "T3", "T4", "T5")}
\item{months_from_baseline}{Integer. Time since baseline (0, 3, 6, 12, 24 months)}
\item{assessment_date}{Date. Date of assessment}
\item{primary_outcome}{Numeric. Primary outcome score (0-100) with time-dependent missing}
\item{secondary_outcome_1}{Numeric. Secondary outcome correlated with primary}
\item{secondary_outcome_2}{Numeric. Independent secondary outcome}
\item{response_status}{Factor. Treatment response ("Responder", "Non-responder")}
\item{compliance_percent}{Numeric. Treatment compliance percentage (75-100\%)}
\item{dose_level}{Ordered Factor. Dose level ("Low" < "Medium" < "High")}
\item{adverse_events}{Integer. Count of adverse events (0-5)}
\item{biomarker_level}{Numeric. Biomarker measurement (log-normal distribution)}
}
}
\source{
Simulated data generated using create_toolssummary_test_data.R
}
\usage{
toolssummary_timeseries_data
}
\description{
Longitudinal dataset with repeated measures per subject across multiple timepoints,
realistic dropout patterns, and outcome tracking. Designed to test time-based summaries,
longitudinal data presentation, missing data patterns over time, and grouped analysis
capabilities using summarytools enhanced features.
}
\details{
This dataset represents a longitudinal clinical study with 40 subjects followed
over 5 timepoints (24 months total). It includes realistic patterns of outcome
changes, dropout over time, and missing data that increase with follow-up duration,
making it ideal for testing summarytools longitudinal capabilities.

\strong{Key Features:}
\itemize{
\item 40 subjects with up to 5 timepoints each (200 total observations)
\item Time-dependent outcome patterns and trends
\item Realistic dropout patterns (increasing missing data over time)
\item Multiple correlated and independent outcomes
\item Compliance and adverse event tracking
\item Response status categorization
}

\strong{summarytools Integration Testing:}
\itemize{
\item \strong{dfSummary}: Longitudinal data overview with temporal patterns
\item \strong{freq}: Time-based frequency analysis and response categorization
\item \strong{descr}: Outcome statistics across timepoints with trend assessment
\item \strong{ctable}: Cross-tabulations by timepoint and response status
}

\strong{Recommended Usage Scenarios:}
\itemize{
\item Longitudinal outcome analysis by timepoint
\item Dropout pattern assessment over time
\item Treatment response analysis
\item Compliance and safety monitoring
}
}
\examples{
\dontrun{
# Load the dataset
data(toolssummary_timeseries_data)

# Longitudinal outcomes analysis
result <- toolssummary(
  data = toolssummary_timeseries_data,
  vars = c("primary_outcome", "secondary_outcome_1", "compliance_percent"),
  useSummarytools = TRUE,
  showDescr = TRUE,
  showDfSummary = TRUE
)

# Analysis by timepoint
result_time <- toolssummary(
  data = toolssummary_timeseries_data,
  vars = c("primary_outcome", "response_status", "adverse_events"),
  groupVar = "timepoint",
  useSummarytools = TRUE,
  showCrosstabs = TRUE
)
}
}
\seealso{
\code{\link{toolssummary}}, \code{\link{toolssummary_datasets}}
}
\keyword{datasets}
