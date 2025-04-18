% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sequentialtests.h.R
\name{sequentialtests}
\alias{sequentialtests}
\title{Sequential Testing Analysis}
\usage{
sequentialtests(
  test1_name = "Screening Test",
  test1_sens = 0.95,
  test1_spec = 0.7,
  test2_name = "Confirmatory Test",
  test2_sens = 0.8,
  test2_spec = 0.98,
  strategy = "serial_positive",
  prevalence = 0.1,
  show_explanation = TRUE,
  show_formulas = FALSE,
  show_nomogram = FALSE
)
}
\arguments{
\item{test1_name}{.}

\item{test1_sens}{.}

\item{test1_spec}{.}

\item{test2_name}{.}

\item{test2_sens}{.}

\item{test2_spec}{.}

\item{strategy}{.}

\item{prevalence}{.}

\item{show_explanation}{.}

\item{show_formulas}{.}

\item{show_nomogram}{.}
}
\value{
A results object containing:
\tabular{llllll}{
\code{results$summary_table} \tab \tab \tab \tab \tab a table \cr
\code{results$individual_tests_table} \tab \tab \tab \tab \tab a table \cr
\code{results$population_flow_table} \tab \tab \tab \tab \tab a table \cr
\code{results$explanation_text} \tab \tab \tab \tab \tab a html \cr
\code{results$formulas_text} \tab \tab \tab \tab \tab a html \cr
\code{results$plot_nomogram} \tab \tab \tab \tab \tab an image \cr
}

Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:

\code{results$summary_table$asDF}

\code{as.data.frame(results$summary_table)}
}
\description{
Function for Sequential Testing Analysis. Analyzes how diagnostic accuracy
changes when applying two tests in sequence (screening followed by
confirmation).
}
\examples{
\donttest{
# example will be added
}
}
