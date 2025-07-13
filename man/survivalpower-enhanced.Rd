\name{survivalpower}
\alias{survivalpower}
\title{Enhanced Survival Analysis Power & Sample Size}
\description{
Comprehensive power analysis and sample size calculation for survival studies using 
Lachin-Foulkes and Schoenfeld methods from the gsDesign package. Enhanced with 
visualizations, export functionality, and clinical examples.
}

\usage{
survivalpower(
  calculation_type = c("sample_size", "power", "events", "hazard_ratio"),
  method = c("lachin_foulkes", "schoenfeld"), 
  hazard_control = 0.083,
  hazard_treatment = 0.042,
  hazard_ratio = 0.6,
  study_duration = 24,
  accrual_duration = 12,
  alpha = 0.025,
  beta = 0.1,
  power = 0.9,
  allocation_ratio = 1,
  sample_size_input = 100,
  events_input = 50,
  sided = c("1", "2"),
  entry_type = c("unif", "expo"),
  gamma = 0,
  dropout_rate = 0,
  show_summary = TRUE,
  show_formulas = FALSE,
  show_interpretation = TRUE,
  show_power_plot = FALSE,
  show_timeline_plot = FALSE,
  power_plot_range = "auto",
  export_results = FALSE,
  export_power_curve = FALSE
)
}

\arguments{
  \item{calculation_type}{Type of calculation: "sample_size", "power", "events", or "hazard_ratio"}
  \item{method}{Calculation method: "lachin_foulkes" (full study design) or "schoenfeld" (events-based)}
  \item{hazard_control}{Event hazard rate for control group (events per time unit)}
  \item{hazard_treatment}{Event hazard rate for treatment group (events per time unit)}
  \item{hazard_ratio}{Hazard ratio (treatment/control) for Schoenfeld method}
  \item{study_duration}{Maximum study duration (months or years)}
  \item{accrual_duration}{Patient accrual (recruitment) duration}
  \item{alpha}{Type I error rate (significance level)}
  \item{beta}{Type II error rate (1 - power)}
  \item{power}{Statistical power (1 - beta)}
  \item{allocation_ratio}{Randomization ratio (treatment:control)}
  \item{sample_size_input}{Sample size for power calculation}
  \item{events_input}{Number of events for power calculation}
  \item{sided}{Test type: "1" for one-sided, "2" for two-sided}
  \item{entry_type}{Patient entry pattern: "unif" for uniform, "expo" for exponential}
  \item{gamma}{Rate parameter for exponential entry}
  \item{dropout_rate}{Equal dropout hazard rate for both groups}
  \item{show_summary}{Whether to display comprehensive study design summary}
  \item{show_formulas}{Whether to display mathematical formulas}
  \item{show_interpretation}{Whether to include clinical interpretation}
  \item{show_power_plot}{Whether to display power curve visualization}
  \item{show_timeline_plot}{Whether to display study timeline (Lachin-Foulkes only)}
  \item{power_plot_range}{Sample size range for power plots ("auto" or "min,max")}
  \item{export_results}{Whether to export detailed results}
  \item{export_power_curve}{Whether to export power curve data}
}

\details{
This enhanced version of survivalpower includes several major improvements:

\strong{Calculation Methods:}
\itemize{
  \item \strong{Lachin-Foulkes Method:} Full study design approach accounting for accrual duration, 
        follow-up period, dropout rates, and patient entry patterns
  \item \strong{Schoenfeld Method:} Events-based approximation using asymptotic normal distribution 
        of log-rank statistic for quick estimates
}

\strong{Calculation Types:}
\itemize{
  \item \strong{Sample Size:} Calculate required sample size for desired power
  \item \strong{Power Analysis:} Calculate power given sample size and effect size
  \item \strong{Events Calculation:} Determine number of events needed
  \item \strong{Hazard Ratio Detection:} Minimum detectable effect size
}

\strong{Enhanced Visualizations:}
\itemize{
  \item \strong{Power Curves:} Interactive plots showing power vs sample size relationships
  \item \strong{Study Timeline:} Visual representation of accrual and follow-up periods
  \item \strong{Effect Detection:} Visualization of detectable effect sizes
}

\strong{Export Functionality:}
\itemize{
  \item \strong{Results Export:} Comprehensive results suitable for protocol documents
  \item \strong{Power Curve Export:} Data points for external analysis and custom plotting
  \item \strong{CSV/Excel Compatible:} Ready for regulatory submissions
}

\strong{Clinical Examples:}
See \code{survivalpower_scenarios} dataset for pre-configured clinical trial scenarios
including oncology, cardiology, neurology, and rare disease studies.
}

\value{
A survivalpowerResults object containing:
\itemize{
  \item \strong{power_results:} HTML-formatted comprehensive results
  \item \strong{formulas:} Mathematical formulas and references (optional)
  \item \strong{interpretation:} Clinical interpretation and recommendations (optional)
  \item \strong{power_plot:} Power curve visualization (optional)
  \item \strong{timeline_plot:} Study timeline visualization (optional)
  \item \strong{exported_results:} Detailed results data frame (optional)
  \item \strong{exported_power_curve:} Power curve data points (optional)
  \item \strong{export_summary:} Export summary and usage instructions (optional)
}
}

\examples{
# Example 1: Oncology Phase III Trial - Sample Size Calculation
oncology_trial <- survivalpower(
  calculation_type = "sample_size",
  method = "lachin_foulkes",
  hazard_control = 0.083,      # ~12 month median survival
  hazard_treatment = 0.042,    # ~24 month median survival
  study_duration = 36,         # 3 year study
  accrual_duration = 24,       # 2 year accrual
  alpha = 0.025,              # One-sided 2.5%
  beta = 0.1,                 # 90% power
  show_interpretation = TRUE
)

# Example 2: Cardiology Trial - Events-Based Quick Calculation
cardio_trial <- survivalpower(
  calculation_type = "events",
  method = "schoenfeld",
  hazard_ratio = 0.75,        # 25% risk reduction
  alpha = 0.025,
  beta = 0.2,                 # 80% power
  show_summary = TRUE
)

# Example 3: Power Analysis with Visualization
power_analysis <- survivalpower(
  calculation_type = "power",
  method = "schoenfeld", 
  hazard_ratio = 0.6,
  events_input = 150,
  show_power_plot = TRUE,     # Show power curve
  power_plot_range = "50,400"
)

# Example 4: Complex Study Design with Timeline
complex_study <- survivalpower(
  calculation_type = "sample_size",
  method = "lachin_foulkes",
  hazard_control = 0.05,
  hazard_treatment = 0.03,
  study_duration = 48,
  accrual_duration = 30,
  entry_type = "expo",        # Exponential accrual
  gamma = 1,
  dropout_rate = 0.02,
  allocation_ratio = 2,       # 2:1 randomization
  show_timeline_plot = TRUE,  # Show study phases
  show_formulas = TRUE
)

# Example 5: Export for External Analysis
export_analysis <- survivalpower(
  calculation_type = "sample_size",
  method = "lachin_foulkes",
  hazard_control = 0.083,
  hazard_treatment = 0.042,
  study_duration = 36,
  accrual_duration = 24,
  export_results = TRUE,      # Export main results
  export_power_curve = TRUE,  # Export power curve
  power_plot_range = "200,600"
)

# Example 6: Using Pre-configured Scenarios
data(survivalpower_scenarios)
oncology_scenario <- survivalpower_scenarios[1, ]  # Oncology Phase III

scenario_analysis <- survivalpower(
  calculation_type = "sample_size",
  method = "lachin_foulkes",
  hazard_control = oncology_scenario$Control_Hazard_Rate,
  hazard_treatment = oncology_scenario$Treatment_Hazard_Rate,
  study_duration = oncology_scenario$Study_Duration_Months,
  accrual_duration = oncology_scenario$Accrual_Duration_Months,
  alpha = oncology_scenario$Alpha_Level,
  power = oncology_scenario$Target_Power,
  allocation_ratio = oncology_scenario$Allocation_Ratio,
  dropout_rate = oncology_scenario$Dropout_Rate,
  show_interpretation = TRUE
)
}

\references{
\itemize{
  \item Lachin JM, Foulkes MA (1986). Evaluation of sample size and power for analyses of survival 
        with allowance for nonuniform patient entry, losses to follow-up, noncompliance, and 
        stratification. Biometrics 42:507-519.
  \item Schoenfeld D (1981). The asymptotic properties of nonparametric tests for comparing 
        survival distributions. Biometrika 68:316-319.
  \item Jennison C, Turnbull BW (2000). Group Sequential Methods with Applications to Clinical Trials. 
        Chapman & Hall/CRC.
  \item Anderson KM, Seibold H (2019). gsDesign: An R Package for Group Sequential Design. 
        R package version 3.1.1.
}
}

\seealso{
\code{\link{survival}}, \code{\link{survivalcont}}, \code{\link{powersurvival}}, 
\code{\link{survivalpower_scenarios}}
}

\author{
ClinicoPath Development Team
}

\keyword{survival}
\keyword{power}
\keyword{sample size}
\keyword{clinical trials}