% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jjwithinstats.h.R
\name{jjwithinstats}
\alias{jjwithinstats}
\title{Box-Violin Plots to Compare Within Groups}
\usage{
jjwithinstats(
  data,
  dep1,
  dep2,
  dep3 = NULL,
  dep4 = NULL,
  pointpath = FALSE,
  centralitypath = FALSE,
  centralityplotting = FALSE,
  centralitytype = "parametric",
  typestatistics = "parametric",
  pairwisecomparisons = FALSE,
  pairwisedisplay = "significant",
  padjustmethod = "holm",
  effsizetype = "biased",
  violin = TRUE,
  boxplot = TRUE,
  point = TRUE,
  mytitle = "Within Group Comparison",
  xtitle = "",
  ytitle = "",
  originaltheme = FALSE,
  resultssubtitle = TRUE
)
}
\arguments{
\item{data}{The data as a data frame.}

\item{dep1}{.}

\item{dep2}{.}

\item{dep3}{.}

\item{dep4}{.}

\item{pointpath}{.}

\item{centralitypath}{.}

\item{centralityplotting}{.}

\item{centralitytype}{.}

\item{typestatistics}{.}

\item{pairwisecomparisons}{.}

\item{pairwisedisplay}{.}

\item{padjustmethod}{.}

\item{effsizetype}{.}

\item{violin}{.}

\item{boxplot}{.}

\item{point}{.}

\item{mytitle}{.}

\item{xtitle}{.}

\item{ytitle}{.}

\item{originaltheme}{.}

\item{resultssubtitle}{.}
}
\value{
A results object containing:
\tabular{llllll}{
\code{results$todo} \tab \tab \tab \tab \tab a html \cr
\code{results$plot} \tab \tab \tab \tab \tab an image \cr
}
}
\description{
Box-Violin Plots to Compare Within Groups
}
\examples{
\donttest{
# example will be added
}
}
