% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/riverplot.h.R
\name{riverplot}
\alias{riverplot}
\title{River Plots}
\usage{
riverplot(
  data,
  id = NULL,
  time,
  strata,
  weight = NULL,
  plotType = "alluvial",
  fillType = "first",
  sortStreams = TRUE,
  labelNodes = TRUE,
  curveType = "cardinal",
  showCounts = FALSE,
  showLegend = TRUE,
  mytitle = "",
  xtitle = "",
  ytitle = "",
  originaltheme = FALSE
)
}
\arguments{
\item{data}{The data as a data frame.}

\item{id}{Optional identifier for individual entities in the data.}

\item{time}{Variable representing time points or sequential stages.}

\item{strata}{Variables containing the categories that change over time.}

\item{weight}{Optional numerical variable to determine stream width.}

\item{plotType}{Type of river plot to generate.}

\item{fillType}{Determines how colors are assigned to flows.}

\item{sortStreams}{Sort alluvial streams by frequency.}

\item{labelNodes}{Add labels to nodes.}

\item{curveType}{Type of curve to use for stream paths.}

\item{showCounts}{Display counts on the diagram.}

\item{showLegend}{Display color legend.}

\item{mytitle}{Title for the plot.}

\item{xtitle}{Label for the x-axis.}

\item{ytitle}{Label for the y-axis.}

\item{originaltheme}{Use the ggStatsPlot theme instead of the default
theme.}
}
\value{
A results object containing:
\tabular{llllll}{
\code{results$todo} \tab \tab \tab \tab \tab a html \cr
\code{results$plot} \tab \tab \tab \tab \tab an image \cr
}
}
\description{
'Function for generating river plots (alluvial diagrams) to visualize
flows and transitions over time or between categories. Supports alluvial
diagrams, Sankey diagrams, and stream graphs with customizable aesthetics.'
}
\examples{
\donttest{
# Load example data
data(riverplot_example_data)
data(riverplot_wide_example_data)

# Basic alluvial plot (longitudinal data)
riverplot(
  data = riverplot_example_data,
  time = "timepoint",
  strata = "treatment_response",
  plotType = "alluvial"
)

# Weighted river plot with patient tracking
riverplot(
  data = riverplot_example_data,
  id = "patient_id",
  time = "timepoint",
  strata = "treatment_response",
  weight = "treatment_cost",
  plotType = "alluvial",
  labelNodes = TRUE,
  fillType = "first"
)

# Multi-stage flow (wide format data)
riverplot(
  data = riverplot_wide_example_data,
  strata = c("screening", "enrollment", "treatment", "outcome"),
  plotType = "alluvial",
  fillType = "last",
  showCounts = TRUE
)

# Sankey diagram
riverplot(
  data = riverplot_wide_example_data,
  strata = c("screening", "enrollment", "treatment"),
  weight = "total_cost",
  plotType = "sankey",
  curveType = "cardinal"
)

# Stream plot
riverplot(
  data = riverplot_example_data,
  time = "timepoint",
  strata = "treatment_response",
  weight = "treatment_cost",
  plotType = "stream"
)
}
}
