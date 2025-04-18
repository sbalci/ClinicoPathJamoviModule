% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/swimmerplot2.h.R
\name{swimmerplot2}
\alias{swimmerplot2}
\title{Swimmer Plot 2}
\usage{
swimmerplot2(
  data,
  patientID,
  startTime,
  endTime,
  status,
  markerData = FALSE,
  markerVar,
  markerTime,
  showArrows = FALSE,
  arrowFilter,
  sortSubjects = "id",
  laneWidth = 2,
  markerSize = 5,
  useDarkTheme = FALSE,
  customTitle = "Swimmer Plot",
  xLabel = "Time",
  yLabel = "Subject ID"
)
}
\arguments{
\item{data}{The data as a data frame in long format where each row
represents a time segment.}

\item{patientID}{Variable containing subject/patient identifiers.}

\item{startTime}{Start time variable for each lane segment (also used as
marker position by default).}

\item{endTime}{End time variable for each lane segment.}

\item{status}{Response or status variable to color lanes.}

\item{markerData}{Whether to include event markers at the start time of
each segment.}

\item{markerVar}{Variable indicating the type of event to be displayed as
markers.}

\item{markerTime}{Optional separate time variable for marker positions. If
not specified, start time is used.}

\item{showArrows}{Show arrows at the end of lanes to indicate ongoing
status.}

\item{arrowFilter}{Boolean variable indicating which subjects should have
arrows (TRUE = show arrow).}

\item{sortSubjects}{How to sort subjects in the plot.}

\item{laneWidth}{.}

\item{markerSize}{.}

\item{useDarkTheme}{.}

\item{customTitle}{.}

\item{xLabel}{.}

\item{yLabel}{.}
}
\value{
A results object containing:
\tabular{llllll}{
\code{results$todo} \tab \tab \tab \tab \tab a html \cr
\code{results$summary} \tab \tab \tab \tab \tab a table \cr
\code{results$mydataview} \tab \tab \tab \tab \tab a preformatted \cr
\code{results$dataView} \tab \tab \tab \tab \tab a preformatted \cr
\code{results$plot} \tab \tab \tab \tab \tab an image \cr
}

Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:

\code{results$summary$asDF}

\code{as.data.frame(results$summary)}
}
\description{
Creates a swimmer plot to visualize subject response data over time.
}
\examples{
\donttest{
data <- data.frame(
    patient_id = c("PT001", "PT001", "PT002", "PT002"),
    start_time = c(0, 3, 0, 4),
    end_time = c(3, 7, 4, 8),
    event_type = c("Treatment Start", "Dose Modification", "Treatment Start", "Follow-up"),
    response_status = c("PR", "CR", "SD", "PR"),
    on_study = c(FALSE, TRUE, FALSE, TRUE)
)
swimmerplot2(
    data = data,
    patientID = "patient_id",
    startTime = "start_time",
    endTime = "end_time",
    status = "response_status",
    markerData = TRUE,
    markerVar = "event_type"
)
}
}
