% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vartree.h.R
\name{vartree}
\alias{vartree}
\title{Variable Tree}
\usage{
vartree(
  data,
  vars,
  percvar = NULL,
  percvarLevel,
  summaryvar = NULL,
  prunebelow = NULL,
  pruneLevel1,
  pruneLevel2,
  follow = NULL,
  followLevel1,
  followLevel2,
  excl = FALSE,
  vp = TRUE,
  horizontal = FALSE,
  sline = TRUE,
  varnames = FALSE,
  nodelabel = TRUE,
  pct = FALSE,
  showcount = TRUE,
  legend = FALSE,
  pattern = FALSE,
  sequence = FALSE,
  ptable = FALSE,
  mytitle = "",
  useprunesmaller = FALSE,
  prunesmaller = 5,
  summarylocation = "leafonly"
)
}
\arguments{
\item{data}{The data as a data frame.}

\item{vars}{.}

\item{percvar}{.}

\item{percvarLevel}{.}

\item{summaryvar}{.}

\item{prunebelow}{.}

\item{pruneLevel1}{.}

\item{pruneLevel2}{.}

\item{follow}{.}

\item{followLevel1}{.}

\item{followLevel2}{.}

\item{excl}{.}

\item{vp}{.}

\item{horizontal}{.}

\item{sline}{.}

\item{varnames}{.}

\item{nodelabel}{.}

\item{pct}{.}

\item{showcount}{.}

\item{legend}{.}

\item{pattern}{.}

\item{sequence}{.}

\item{ptable}{.}

\item{mytitle}{.}

\item{useprunesmaller}{.}

\item{prunesmaller}{.}

\item{summarylocation}{.}
}
\value{
A results object containing:
\tabular{llllll}{
\code{results$todo} \tab \tab \tab \tab \tab a html \cr
\code{results$text1} \tab \tab \tab \tab \tab a html \cr
\code{results$text2} \tab \tab \tab \tab \tab a preformatted \cr
}
}
\description{
Function for Generating Tree Summaries of Variables.
}
\examples{
\donttest{
# example will be added
}
}
