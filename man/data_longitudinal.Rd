% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{data_longitudinal}
\alias{data_longitudinal}
\title{Longitudinal Measurement Data}
\format{
A data frame with 140 rows and 3 variables:
\describe{
\item{PatientID}{Character. Unique identifier for each patient.}
\item{Time}{Integer. The time point at which the measurement was taken (e.g., day, week, month).}
\item{Measurement}{Numeric. The value of the measurement recorded at the given time point for the patient.}
}
}
\usage{
data(data_longitudinal)
}
\description{
A sample dataset representing longitudinal measurements for multiple patients
over several time points.
}
\examples{
data(data_longitudinal)
str(data_longitudinal)
head(data_longitudinal)
summary(data_longitudinal$Measurement)
# Plot measurements for a specific patient (if plotting package loaded)
# if (requireNamespace("ggplot2", quietly = TRUE) && "PT001" \%in\% data_longitudinal$PatientID) {
#   ggplot2::ggplot(data_longitudinal[data_longitudinal$PatientID == "PT001",],
#                   ggplot2::aes(x = Time, y = Measurement)) +
#     ggplot2::geom_line() + ggplot2::geom_point() +
#     ggplot2::ggtitle("Measurements for Patient PT001")
# }
}
\keyword{datasets}
