% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jextractstats.h.R
\name{jextractstats}
\alias{jextractstats}
\title{Statistical Data Extraction from ggstatsplot}
\usage{
jextractstats(
  data,
  analysis_type = "between_stats",
  x_var,
  y_var,
  paired_var,
  extract_subtitle = TRUE,
  extract_caption = TRUE,
  extract_pairwise = TRUE,
  extract_descriptive = TRUE,
  show_raw_data = FALSE,
  show_formatted_results = TRUE
)
}
\arguments{
\item{data}{The data as a data frame.}

\item{analysis_type}{Type of statistical analysis to perform and extract.}

\item{x_var}{Independent variable or grouping variable.}

\item{y_var}{Dependent variable or outcome variable.}

\item{paired_var}{Subject identifier for paired/repeated measures.}

\item{extract_subtitle}{Extract statistical details used in plot subtitles.}

\item{extract_caption}{Extract statistical details used in plot captions.}

\item{extract_pairwise}{Extract pairwise comparison results.}

\item{extract_descriptive}{Extract descriptive statistics summary.}

\item{show_raw_data}{Display raw extracted statistical data frames.}

\item{show_formatted_results}{Display formatted statistical results.}
}
\value{
A results object containing:
\tabular{llllll}{
\code{results$text} \tab \tab \tab \tab \tab a preformatted \cr
}
}
\description{
Extract detailed statistical information from ggstatsplot analyses using
statsExpressions package for comprehensive data frame outputs.
}
\examples{
\donttest{
# Example usage:
library(ggstatsplot)
library(statsExpressions)
# Extract statistical details
extract_stats(ggbetweenstats(data, x, y))
}
}
