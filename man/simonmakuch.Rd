% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simonmakuch.h.R
\name{simonmakuch}
\alias{simonmakuch}
\title{Simon-Makuch Time-Dependent Survival Analysis}
\usage{
simonmakuch(
  data,
  survivalTime,
  event,
  eventLevel,
  timeDepVariable,
  timeDepTime,
  timeDepStatus,
  exposedLevel,
  enableMultipleTimeDep = FALSE,
  additionalTimeDepVars,
  analysisType = "comprehensive",
  confidenceLevel = 0.95,
  performLandmarkAnalysis = FALSE,
  landmarkTimes = "6, 12, 24",
  landmarkWindow = 1,
  performTimeDependentCox = FALSE,
  timeDependentCovariates,
  testTimeVaryingEffect = FALSE,
  assessImmortalTimeBias = FALSE,
  naiveComparison = FALSE,
  showSimonMakuchPlot = TRUE,
  showLandmarkPlots = FALSE,
  showCumulativeIncidencePlot = FALSE,
  plotTimeRange = "auto",
  showConfidenceIntervals = TRUE,
  showRiskTables = TRUE,
  plotExposureStatus = FALSE,
  performLogRankTest = TRUE,
  performMantelByarTest = FALSE,
  performTimeDepLRTest = FALSE,
  handleTieBreaking = "efron",
  robustVariance = FALSE,
  clusterVariable,
  showSurvivalEstimates = TRUE,
  showHazardRatios = TRUE,
  showLandmarkResults = FALSE,
  showExposurePatterns = FALSE,
  showModelDiagnostics = FALSE,
  performBootstrapValidation = FALSE,
  bootstrapSamples = 500,
  performSensitivityAnalysis = FALSE,
  showExplanations = TRUE,
  showMethodologyNotes = FALSE,
  includeClinicalGuidance = FALSE
)
}
\arguments{
\item{data}{The dataset containing survival and time-dependent variable
information.}

\item{survivalTime}{Time to event or censoring in consistent units (months
recommended). This should be the total follow-up time from study entry.}

\item{event}{Event indicator (1 = event occurred, 0 = censored) or factor
with event levels. For overall survival, this represents death from any
cause.}

\item{eventLevel}{The level indicating event occurrence when using factor
variables.}

\item{timeDepVariable}{The main time-dependent variable of interest (e.g.,
treatment status, biomarker status, disease progression). This variable can
change during follow-up.}

\item{timeDepTime}{Time when the time-dependent variable changes status.
Use 0 for baseline status, and actual time for when the variable changes
(e.g., time of treatment initiation, biomarker conversion).}

\item{timeDepStatus}{Status of the time-dependent variable at each time
point. For example: "Unexposed", "Exposed", or "Pre-treatment",
"Post-treatment".}

\item{exposedLevel}{The level that represents the "exposed" or "active"
status of the time-dependent variable.}

\item{enableMultipleTimeDep}{Enable analysis of multiple time-dependent
variables simultaneously. Useful for complex exposure patterns or multiple
biomarkers.}

\item{additionalTimeDepVars}{Additional time-dependent variables to include
in the analysis. Each should have corresponding time and status variables.}

\item{analysisType}{Type of Simon-Makuch analysis to perform. Comprehensive
includes all methods for thorough time-dependent survival analysis.}

\item{confidenceLevel}{Confidence level for survival estimates and
statistical tests.}

\item{performLandmarkAnalysis}{Perform landmark analysis at specified time
points. This method analyzes survival from specific landmark times,
avoiding immortal time bias when studying time-dependent exposures.}

\item{landmarkTimes}{Comma-separated list of landmark time points for
analysis. Patients must survive to each landmark time to be included in
that analysis.}

\item{landmarkWindow}{Time window around landmark time for determining
exposure status. Exposure status is determined within this window before
the landmark.}

\item{performTimeDependentCox}{Perform Cox regression with time-dependent
covariates. This provides hazard ratios that properly account for changing
exposure status.}

\item{timeDependentCovariates}{Additional covariates to include in
time-dependent Cox regression. These can be baseline variables or other
time-dependent variables.}

\item{testTimeVaryingEffect}{Test whether the effect of the time-dependent
variable changes over time (non-proportional hazards for time-dependent
covariates).}

\item{assessImmortalTimeBias}{Assess and correct for immortal time bias in
the analysis. Compares naive analysis (without proper time-dependent
handling) with corrected Simon-Makuch analysis.}

\item{naiveComparison}{Include comparison with naive analysis that ignores
the time-dependent nature of the exposure. This demonstrates the importance
of proper time-dependent analysis.}

\item{showSimonMakuchPlot}{Display Simon-Makuch survival curves that
properly account for time-dependent exposure status changes during
follow-up.}

\item{showLandmarkPlots}{Display survival curves from each landmark time
point, showing how the effect of exposure varies with timing.}

\item{showCumulativeIncidencePlot}{Display cumulative incidence curves for
time-dependent exposures, useful for understanding exposure patterns over
time.}

\item{plotTimeRange}{Maximum time for survival plots. Use "auto" for
automatic range or specify maximum months (e.g., "60" for 5-year
follow-up).}

\item{showConfidenceIntervals}{Display confidence intervals around survival
curves.}

\item{showRiskTables}{Display at-risk tables below survival curves,
stratified by time-dependent exposure status.}

\item{plotExposureStatus}{Display plot showing how exposure status changes
over time for the study population.}

\item{performLogRankTest}{Perform log-rank test comparing survival between
exposure groups, properly accounting for time-dependent nature.}

\item{performMantelByarTest}{Perform Mantel-Byar test, which is
specifically designed for comparing survival with time-dependent exposures.}

\item{performTimeDepLRTest}{Perform time-dependent log-rank test that
allows for changing effects of exposure over time.}

\item{handleTieBreaking}{Method for handling tied event times in Cox
regression.}

\item{robustVariance}{Use robust (sandwich) variance estimation for Cox
regression to account for potential model misspecification.}

\item{clusterVariable}{Variable identifying clusters for robust variance
estimation (e.g., hospital, physician, family).}

\item{showSurvivalEstimates}{Display table with survival estimates at key
time points, stratified by time-dependent exposure status.}

\item{showHazardRatios}{Display hazard ratios from time-dependent Cox
regression with confidence intervals and p-values.}

\item{showLandmarkResults}{Display detailed results from landmark analysis
at each specified time point.}

\item{showExposurePatterns}{Display summary of exposure patterns, including
time to exposure, exposure duration, and switching patterns.}

\item{showModelDiagnostics}{Display diagnostic plots and statistics for
time-dependent Cox regression models.}

\item{performBootstrapValidation}{Perform bootstrap validation to assess
stability of results and provide bias-corrected estimates.}

\item{bootstrapSamples}{Number of bootstrap samples for validation
analysis.}

\item{performSensitivityAnalysis}{Perform sensitivity analysis with
different assumptions about exposure timing and immortal time handling.}

\item{showExplanations}{Include detailed explanations for Simon-Makuch
methodology and interpretation of results.}

\item{showMethodologyNotes}{Display detailed notes on the statistical
methods used and their appropriate interpretation.}

\item{includeClinicalGuidance}{Include guidance for clinical interpretation
of time-dependent survival analysis results.}
}
\value{
A results object containing:
\tabular{llllll}{
\code{results$welcomeMessage} \tab \tab \tab \tab \tab a html \cr
\code{results$simonMakuchExplanation} \tab \tab \tab \tab \tab a html \cr
\code{results$exposurePatterns} \tab \tab \tab \tab \tab a table \cr
\code{results$survivalEstimates} \tab \tab \tab \tab \tab a table \cr
\code{results$timeDependentCox} \tab \tab \tab \tab \tab a table \cr
\code{results$hazardRatios} \tab \tab \tab \tab \tab a table \cr
\code{results$statisticalTests} \tab \tab \tab \tab \tab a table \cr
\code{results$landmarkResults} \tab \tab \tab \tab \tab a table \cr
\code{results$immortalTimeBias} \tab \tab \tab \tab \tab a table \cr
\code{results$timeVaryingEffects} \tab \tab \tab \tab \tab a table \cr
\code{results$modelDiagnostics} \tab \tab \tab \tab \tab a table \cr
\code{results$bootstrapValidation} \tab \tab \tab \tab \tab a table \cr
\code{results$sensitivityAnalysis} \tab \tab \tab \tab \tab a table \cr
\code{results$landmarkAnalysisExplanation} \tab \tab \tab \tab \tab a html \cr
\code{results$immortalTimeBiasExplanation} \tab \tab \tab \tab \tab a html \cr
\code{results$timeDependentCoxExplanation} \tab \tab \tab \tab \tab a html \cr
\code{results$clinicalGuidance} \tab \tab \tab \tab \tab a html \cr
\code{results$methodologyNotes} \tab \tab \tab \tab \tab a html \cr
\code{results$simonMakuchPlotExplanation} \tab \tab \tab \tab \tab a html \cr
\code{results$simonMakuchPlot} \tab \tab \tab \tab \tab an image \cr
\code{results$landmarkPlots} \tab \tab \tab \tab \tab an image \cr
\code{results$cumulativeIncidencePlot} \tab \tab \tab \tab \tab an image \cr
\code{results$exposureStatusPlot} \tab \tab \tab \tab \tab an image \cr
\code{results$diagnosticPlots} \tab \tab \tab \tab \tab an image \cr
}

Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:

\code{results$exposurePatterns$asDF}

\code{as.data.frame(results$exposurePatterns)}
}
\description{
Performs survival analysis with time-dependent variables using the
Simon-Makuch method. This analysis is specifically designed for situations
where patient characteristics or treatments change during follow-up, such
as biomarker changes, treatment switches, or progression events. The
Simon-Makuch approach properly accounts for these time-varying exposures in
survival estimation and comparison.
Key features include Simon-Makuch plots (modified Kaplan-Meier curves),
landmark analysis, time-dependent Cox regression, and proper handling of
immortal time bias.
}
\examples{
# Example: Analyzing survival with time-dependent treatment status
# simonmakuch(
#   data = patient_data,
#   survivalTime = "follow_up_months",
#   event = "death_status",
#   eventLevel = "Dead",
#   timeDepVariable = "treatment_received",
#   timeDepTime = "treatment_start_time",
#   timeDepStatus = "treatment_status",
#   analysisType = "comprehensive"
# )

}
