% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataquality.h.R
\name{dataquality}
\alias{dataquality}
\title{Data Quality Assessment}
\usage{
dataquality(
  data,
  vars,
  check_duplicates = TRUE,
  check_missing = TRUE,
  complete_cases_only = FALSE
)
}
\arguments{
\item{data}{The data as a data frame.}

\item{vars}{Variables to assess for data quality. If none selected,  entire
dataset will be analyzed.}

\item{check_duplicates}{If TRUE, analyzes duplicate values within each
variable or across  the entire dataset.}

\item{check_missing}{If TRUE, provides detailed missing value statistics
and patterns.}

\item{complete_cases_only}{If TRUE, analyzes completeness across all
selected variables simultaneously.}
}
\value{
A results object containing:
\tabular{llllll}{
\code{results$todo} \tab \tab \tab \tab \tab a html \cr
\code{results$text} \tab \tab \tab \tab \tab a html \cr
\code{results$plot} \tab \tab \tab \tab \tab a html \cr
}
}
\description{
This module provides data quality assessment including duplicate detection,
missing value analysis, and data completeness summary (similar to sumvar's
dup() function).
}
\examples{
\donttest{
# Example:
# 1. Load your data frame.
# 2. Select variables to check for data quality issues.
# 3. Choose analysis type (duplicates, missing values, or both).
# 4. Run the dataquality module to see comprehensive data quality report.
}
}
