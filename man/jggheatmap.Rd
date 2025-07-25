% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jggheatmap.h.R
\name{jggheatmap}
\alias{jggheatmap}
\title{Advanced Heatmap Visualization}
\usage{
jggheatmap(
  data,
  matrix_vars,
  row_var = NULL,
  col_var = NULL,
  value_var = NULL,
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  clustering_method = "complete",
  distance_method = "euclidean",
  scaling = "none",
  color_scheme = "blue_red",
  cell_shape = "square",
  show_values = FALSE,
  value_format = "auto",
  text_size = 8,
  show_row_labels = TRUE,
  show_col_labels = TRUE,
  row_label_size = 10,
  col_label_size = 10,
  show_dendrograms = TRUE,
  dendrogram_height = 0.2,
  annotation_var = NULL,
  annotation_colors = "default",
  plot_title = "",
  plot_width = 600,
  plot_height = 600,
  show_colorbar = TRUE,
  colorbar_title = "Value",
  border_color = "white",
  na_color = "grey90",
  output_format = "plot_only"
)
}
\arguments{
\item{data}{.}

\item{matrix_vars}{Variables to include in the heatmap matrix}

\item{row_var}{Variable defining row groupings (for pivot format)}

\item{col_var}{Variable defining column groupings (for pivot format)}

\item{value_var}{Variable containing matrix values (for pivot format)}

\item{cluster_rows}{Perform hierarchical clustering on rows}

\item{cluster_cols}{Perform hierarchical clustering on columns}

\item{clustering_method}{.}

\item{distance_method}{.}

\item{scaling}{.}

\item{color_scheme}{.}

\item{cell_shape}{.}

\item{show_values}{Display numeric values in cells}

\item{value_format}{.}

\item{text_size}{Size of text in cells}

\item{show_row_labels}{.}

\item{show_col_labels}{.}

\item{row_label_size}{.}

\item{col_label_size}{.}

\item{show_dendrograms}{Display clustering dendrograms}

\item{dendrogram_height}{Height of dendrogram relative to plot}

\item{annotation_var}{Variable for row/column annotations}

\item{annotation_colors}{.}

\item{plot_title}{.}

\item{plot_width}{.}

\item{plot_height}{.}

\item{show_colorbar}{Display color scale legend}

\item{colorbar_title}{.}

\item{border_color}{.}

\item{na_color}{.}

\item{output_format}{.}
}
\value{
A results object containing:
\tabular{llllll}{
\code{results$plot} \tab \tab \tab \tab \tab an image \cr
\code{results$matrixtab} \tab \tab \tab \tab \tab a table \cr
\code{results$clustertab} \tab \tab \tab \tab \tab a table \cr
\code{results$interpretation} \tab \tab \tab \tab \tab a html \cr
}

Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:

\code{results$matrixtab$asDF}

\code{as.data.frame(results$matrixtab)}
}
\description{
Advanced Heatmap Visualization
}
