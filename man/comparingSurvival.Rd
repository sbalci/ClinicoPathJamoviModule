% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/comparingsurvival.h.R
\name{comparingSurvival}
\alias{comparingSurvival}
\title{Comparing Survival Outcomes}
\usage{
comparingSurvival(
  data,
  times = NULL,
  status = NULL,
  groups = NULL,
  ciyn = FALSE,
  loglogyn = FALSE,
  timeunits = "None"
)
}
\arguments{
\item{data}{.}

\item{times}{.}

\item{status}{.}

\item{groups}{.}

\item{ciyn}{.}

\item{loglogyn}{.}

\item{timeunits}{.}
}
\value{
A results object containing:
\tabular{llllll}{
\code{results$text} \tab \tab \tab \tab \tab a preformatted \cr
\code{results$compsurvTable1} \tab \tab \tab \tab \tab a table \cr
\code{results$compsurvTable2} \tab \tab \tab \tab \tab a table \cr
\code{results$compsurvTable3} \tab \tab \tab \tab \tab a table \cr
\code{results$plot} \tab \tab \tab \tab \tab an image \cr
\code{results$plot2} \tab \tab \tab \tab \tab an image \cr
}

Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:

\code{results$compsurvTable1$asDF}

\code{as.data.frame(results$compsurvTable1)}
}
\description{
Comparing Survival Outcomes
}
