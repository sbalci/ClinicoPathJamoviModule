% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tinytable.h.R
\name{tinytable}
\alias{tinytable}
\title{Modern Table Formatting}
\usage{
tinytable(
  data,
  vars,
  group_var,
  table_type = "summary",
  show_statistics = TRUE,
  show_counts = TRUE,
  show_missing = FALSE,
  table_theme = "clinical",
  table_title = "Data Summary Table",
  table_notes = "",
  output_format = "html",
  column_width = 0.8,
  precision_digits = 2,
  style_alternating = TRUE,
  style_borders = "all",
  font_size = "normal",
  show_interpretation = TRUE
)
}
\arguments{
\item{data}{The data as a data frame.}

\item{vars}{Variables to include in the table display.}

\item{group_var}{Optional variable for grouping rows or creating summary
tables.}

\item{table_type}{Type of table to generate with appropriate formatting.}

\item{show_statistics}{If TRUE, includes statistical summaries (mean, SD,
etc.) for numeric variables.}

\item{show_counts}{If TRUE, shows counts and percentages for categorical
variables.}

\item{show_missing}{If TRUE, includes information about missing values.}

\item{table_theme}{Visual theme for table styling and appearance.}

\item{table_title}{Title for the table.}

\item{table_notes}{Optional notes to display below the table.}

\item{output_format}{Primary output format for the table.}

\item{column_width}{Width of the table as proportion of available space.}

\item{precision_digits}{Number of decimal places for numeric values.}

\item{style_alternating}{If TRUE, applies alternating row background
colors.}

\item{style_borders}{Border style for the table.}

\item{font_size}{Font size for table text.}

\item{show_interpretation}{If TRUE, displays interpretation and usage
guidelines.}
}
\value{
A results object containing:
\tabular{llllll}{
\code{results$todo} \tab \tab \tab \tab \tab a html \cr
\code{results$table} \tab \tab \tab \tab \tab a html \cr
\code{results$interpretation} \tab \tab \tab \tab \tab a html \cr
}
}
\description{
Creates modern, publication-ready tables using the tinytable package.
This module provides lightweight, zero-dependency table formatting with
multiple output formats including HTML, LaTeX, PDF, and Word. Designed
to complement existing ClinicoPath table modules with modern styling,
flexible formatting, and clean aesthetics. Perfect for enhancing data
presentation in clinical research publications and reports.
}
\examples{
\donttest{
# Example:
# 1. Select variables for table display.
# 2. Choose grouping variables for organized presentation.
# 3. Customize styling, themes, and formatting options.
# 4. Generate publication-ready tables in multiple formats.
}
}
