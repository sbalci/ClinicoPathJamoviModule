% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subgroupforest.h.R
\name{subgroupforest}
\alias{subgroupforest}
\title{Subgroup Analysis Forest Plot}
\usage{
subgroupforest(
  data,
  outcome,
  treatment,
  subgroups,
  time,
  event,
  outcomeType = "survival",
  effectMeasure = "hr",
  confidenceLevel = "0.95",
  showOverall = TRUE,
  showInteraction = TRUE,
  sortBy = "effect",
  showSampleSizes = TRUE,
  logScale = TRUE,
  nullLine = 1
)
}
\arguments{
\item{data}{The data as a data frame.}

\item{outcome}{Primary outcome variable (time-to-event, binary, or
continuous).}

\item{treatment}{Treatment or exposure variable (binary factor).}

\item{subgroups}{Variables defining patient subgroups for analysis.}

\item{time}{Time variable for survival analysis (required if outcome is
survival time).}

\item{event}{Event indicator for survival analysis (1=event, 0=censored).}

\item{outcomeType}{Type of outcome variable for appropriate statistical
analysis.}

\item{effectMeasure}{Statistical measure for treatment effect.}

\item{confidenceLevel}{Confidence level for intervals.}

\item{showOverall}{Display overall treatment effect across all patients.}

\item{showInteraction}{Perform statistical tests for subgroup interactions.}

\item{sortBy}{Method for ordering subgroups in the plot.}

\item{showSampleSizes}{Display sample sizes for each subgroup.}

\item{logScale}{Display effects on log scale (appropriate for ratios).}

\item{nullLine}{Value for null effect reference line (1 for ratios, 0 for
differences).}
}
\value{
A results object containing:
\tabular{llllll}{
\code{results$todo} \tab \tab \tab \tab \tab a html \cr
\code{results$plot} \tab \tab \tab \tab \tab an image \cr
\code{results$summary} \tab \tab \tab \tab \tab a table \cr
\code{results$interactions} \tab \tab \tab \tab \tab a table \cr
\code{results$overall} \tab \tab \tab \tab \tab a table \cr
}

Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:

\code{results$summary$asDF}

\code{as.data.frame(results$summary)}
}
\description{
Subgroup Analysis Forest Plot
}
