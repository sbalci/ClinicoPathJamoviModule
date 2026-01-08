# ═══════════════════════════════════════════════════════════
# Example Usage: survival
# ═══════════════════════════════════════════════════════════
#
# Comprehensive examples demonstrating survival usage
# for univariate survival analysis with Cox regression and Kaplan-Meier

library(ClinicoPath)

# ═══════════════════════════════════════════════════════════
# Example 1: Basic Univariate Survival Analysis
# ═══════════════════════════════════════════════════════════
# Compare survival between treatment groups

data(survival_test, package = "ClinicoPath")

# Basic survival comparison
result_basic <- survival(
  data = survival_test,
  elapsedtime = "elapsedtime",
  outcome = "outcome",
  explanatory = "treatment"
)

# Clinical Interpretation:
# - Compares survival between Control, Treatment A, and Treatment B
# - Cox proportional hazards regression provides hazard ratios
# - Log-rank test assesses overall difference between groups
# - Median survival time for each group
# - Reports survival at 1, 3, 5 years (if applicable)

# ═══════════════════════════════════════════════════════════
# Example 2: Survival Analysis with Kaplan-Meier Curves
# ═══════════════════════════════════════════════════════════
# Visualize survival curves with risk table

result_km <- survival(
  data = survival_test,
  elapsedtime = "elapsedtime",
  outcome = "outcome",
  explanatory = "treatment",
  sc = TRUE,           # Generate survival curves
  risktable = TRUE,    # Include number at risk table
  ci95 = TRUE,         # Show 95% confidence intervals
  censored = TRUE,     # Mark censored observations
  pplot = TRUE,        # Display p-value on plot
  medianline = "hv"    # Show median survival lines
)

# Clinical Application:
# - Visual assessment of survival differences
# - Risk table shows patients at risk at each time point
# - Confidence intervals indicate precision of estimates
# - Censored marks show when patients were lost to follow-up
# - Median lines facilitate quick comparison of median survival

# Typical Findings:
# - Treatment groups separate early if treatment is effective
# - Wider confidence intervals with fewer patients at risk
# - Plateau in survival curves indicates long-term survivors

# ═══════════════════════════════════════════════════════════
# Example 3: Date-Based Survival Time Calculation
# ═══════════════════════════════════════════════════════════
# Calculate survival time from diagnosis and follow-up dates

data(survival_dates, package = "ClinicoPath")

result_dates <- survival(
  data = survival_dates,
  tint = TRUE,                # Use dates to calculate time
  dxdate = "dxdate",          # Diagnosis date
  fudate = "fudate",          # Follow-up/event date
  outcome = "outcome",
  explanatory = "treatment",
  timetypedata = "ymd",       # Date format: YYYY-MM-DD
  timetypeoutput = "months"   # Output time in months
)

# Clinical Context:
# Real-world data often comes with dates rather than pre-calculated times:
# - dxdate: Date of diagnosis or study entry
# - fudate: Date of death/event or last follow-up
# - Automatically calculates time interval
# - Handles different date formats (YMD, DMY, MDY)
# - Can output in days, weeks, months, or years

# Common Data Sources:
# - Electronic health records (EHR)
# - Cancer registries
# - Clinical trial databases
# - Administrative claims data

# ═══════════════════════════════════════════════════════════
# Example 4: Competing Risks Analysis
# ═══════════════════════════════════════════════════════════
# Multiple types of events (disease-specific vs other causes)

data(survival_competing, package = "ClinicoPath")

result_competing <- survival(
  data = survival_competing,
  elapsedtime = "elapsedtime",
  outcome = "outcome",
  outcomeLevel = "Dead of Disease",  # Event of interest
  dod = "Dead of Disease",           # Disease-specific death
  dooc = "Dead of Other",            # Death from other causes
  awd = "Alive w Disease",           # Alive with disease
  awod = "Alive w/o Disease",        # Alive without disease
  analysistype = "compete",          # Competing risk analysis
  multievent = TRUE,
  explanatory = "treatment",
  sc = TRUE,
  ce = TRUE                          # Cumulative events plot
)

# Clinical Application:
# Competing risks are common in oncology:
# - Cancer patients may die from disease or other causes
# - Elderly patients have higher competing risk mortality
# - Ignoring competing risks overestimates disease-specific mortality

# Analysis Types:
# - "overall": Treats all deaths as events (ignores cause)
# - "cause": Cause-specific (censors competing events)
# - "compete": Fine-Gray subdistribution hazards

# When to Use Competing Risks:
# - Elderly populations with high comorbidity
# - Long-term follow-up studies
# - Diseases with moderate lethality
# - Evaluating cause-specific interventions

# ═══════════════════════════════════════════════════════════
# Example 5: Landmark Analysis
# ═══════════════════════════════════════════════════════════
# Avoid guarantee-time bias with conditional survival analysis

data(survival_landmark, package = "ClinicoPath")

result_landmark <- survival(
  data = survival_landmark,
  elapsedtime = "elapsedtime",
  outcome = "outcome",
  uselandmark = TRUE,
  landmark = 6,  # 6-month landmark time
  explanatory = "response_6mo"
)

# Clinical Context:
# Landmark analysis addresses guarantee-time bias:
# - Patients must survive to 6 months to be classified by 6-month response
# - Excludes patients who died before landmark time
# - Conditional survival: "Given survival to 6 months, what is subsequent survival?"

# Common Landmark Times:
# - 3-6 months: Treatment response assessment
# - 90 days: Post-surgical complications window
# - 1 year: Annual reassessment
# - 2 years: Long-term response evaluation

# Applications:
# - Treatment response predicting long-term survival
# - Time-dependent biomarkers
# - Post-treatment assessments
# - Avoiding immortal time bias

# Reporting:
# "Among patients who survived at least 6 months, those with complete
# response had significantly better subsequent survival (HR=0.42, p<0.001)"

# ═══════════════════════════════════════════════════════════
# Example 6: Stratified Cox Regression
# ═══════════════════════════════════════════════════════════
# Handle non-proportional hazards via stratification

data(survival_stratified, package = "ClinicoPath")

# First, test proportional hazards assumption
result_ph_test <- survival(
  data = survival_stratified,
  elapsedtime = "elapsedtime",
  outcome = "outcome",
  explanatory = "treatment",
  ph_cox = TRUE
)

# If PH assumption violated, use stratification
result_stratified <- survival(
  data = survival_stratified,
  elapsedtime = "elapsedtime",
  outcome = "outcome",
  explanatory = "treatment",  # Variable of interest (must satisfy PH)
  stratified_cox = TRUE,
  strata_variable = "sex"     # Stratify on variable violating PH
)

# Clinical Context:
# Proportional hazards assumption may be violated when:
# - Sex has different baseline hazards (early vs late mortality)
# - Age has time-varying effects
# - Study site has different baseline risks

# When to Stratify:
# 1. Test PH assumption (ph_cox = TRUE)
# 2. If violated (p < 0.05), identify violating variable
# 3. Stratify on that variable
# 4. Analyze treatment effect adjusting for stratification

# Interpretation:
# - Allows different baseline hazards for each stratum
# - Estimates single treatment effect across strata
# - Loses information about stratified variable
# - Alternative: Extended Cox model with time interactions

# ═══════════════════════════════════════════════════════════
# Example 7: Person-Time Analysis
# ═══════════════════════════════════════════════════════════
# Calculate incidence rates accounting for varying follow-up

data(survival_person_time, package = "ClinicoPath")

result_person_time <- survival(
  data = survival_person_time,
  elapsedtime = "elapsedtime",
  outcome = "outcome",
  explanatory = "risk_category",
  person_time = TRUE,
  time_intervals = "12, 36, 60",  # 0-12, 12-36, 36-60, 60+ months
  rate_multiplier = 100           # Events per 100 person-years
)

# Clinical Application:
# Person-time analysis accounts for:
# - Variable follow-up durations
# - Staggered study entry
# - Administrative censoring
# - Loss to follow-up

# Metrics:
# - Total person-time: Sum of all follow-up time
# - Incidence rate: Events / person-time × multiplier
# - Rate ratio: Comparing incidence rates between groups

# Time Intervals:
# Stratified person-time shows if event rate changes over time:
# - 0-12 months: Early events (surgical complications, acute toxicity)
# - 12-36 months: Intermediate (disease recurrence)
# - 36-60 months: Late events (long-term toxicity)
# - 60+ months: Very late events

# Reporting:
# "The incidence rate was 8.5 events per 100 person-years in the high-risk
# group compared to 2.3 per 100 person-years in the low-risk group (rate
# ratio 3.7, 95% CI 2.1-6.4)"

# ═══════════════════════════════════════════════════════════
# Example 8: Restricted Mean Survival Time (RMST)
# ═══════════════════════════════════════════════════════════
# Alternative to hazard ratios when PH assumption fails

data(survival_rmst, package = "ClinicoPath")

result_rmst <- survival(
  data = survival_rmst,
  elapsedtime = "elapsedtime",
  outcome = "outcome",
  explanatory = "treatment",
  rmst_analysis = TRUE,
  rmst_tau = 48  # 48-month time horizon
)

# Clinical Context:
# RMST is useful when:
# - Median survival cannot be estimated (high censoring)
# - Proportional hazards assumption violated
# - Crossing survival curves
# - Need clinically interpretable measure

# Interpretation:
# RMST = average survival time up to specified time horizon (τ)
# - "Patients on Novel Therapy survived an average of 38 months in the
#   first 48 months of follow-up, compared to 29 months for Standard Care
#   (difference: 9 months, 95% CI 4-14 months, p<0.001)"

# Advantages over Hazard Ratios:
# - Clinically intuitive (difference in months/years)
# - Robust to PH assumption violations
# - No proportional hazards required
# - Estimates absolute treatment benefit

# Choosing Time Horizon (τ):
# - Clinical relevance (e.g., 5 years for cancer)
# - Data-driven: 75th percentile of follow-up
# - Pre-specified in protocol
# - Sensitivity analysis with different τ values

# ═══════════════════════════════════════════════════════════
# Example 9: Pairwise Comparisons with Multiple Testing Correction
# ═══════════════════════════════════════════════════════════
# Compare all treatment groups pairwise

result_pairwise <- survival(
  data = survival_test,
  elapsedtime = "elapsedtime",
  outcome = "outcome",
  explanatory = "treatment",  # 3 groups: 3 pairwise comparisons
  pw = TRUE,
  padjustmethod = "holm",     # Holm-Bonferroni correction
  sc = TRUE
)

# Clinical Application:
# When comparing ≥3 groups:
# - Overall log-rank test: Any difference between groups?
# - Pairwise comparisons: Which groups differ?
# - Multiple testing correction prevents inflated Type I error

# Adjustment Methods:
# - "holm": Holm-Bonferroni (step-down, recommended)
# - "bonferroni": Conservative Bonferroni
# - "fdr": False discovery rate (Benjamini-Hochberg)
# - "BH": Same as fdr
# - "BY": Benjamini-Yekutieli (more conservative than BH)
# - "none": No adjustment (not recommended)

# Reporting:
# "Overall survival differed significantly between groups (log-rank p<0.001).
# Pairwise comparisons with Holm correction showed Treatment A vs Control
# (p=0.003), Treatment B vs Control (p<0.001), but no difference between
# Treatment A vs B (p=0.18)"

# ═══════════════════════════════════════════════════════════
# Example 10: KMunicate-Style Plot
# ═══════════════════════════════════════════════════════════
# Publication-quality survival curves following reporting guidelines

result_kmunicate <- survival(
  data = survival_test,
  elapsedtime = "elapsedtime",
  outcome = "outcome",
  explanatory = "treatment",
  kmunicate = TRUE
)

# Clinical Context:
# KMunicate style plot follows best practices:
# - Clear labels and legends
# - Numbers at risk below plot
# - Confidence intervals optional
# - Time axis starts at 0
# - Y-axis shows full range (0-1) or focused range

# Guidelines Addressed:
# - CONSORT: Kaplan-Meier curves for RCTs
# - STROBE: Survival analysis reporting
# - REMARK: Prognostic marker studies

# Essential Elements:
# 1. Numbers at risk at regular intervals
# 2. Censoring marks or no censoring marks (cleaner)
# 3. Confidence intervals (optional, can clutter)
# 4. Median survival time (horizontal line)
# 5. P-value from log-rank test
# 6. Follow-up time clearly indicated

# ═══════════════════════════════════════════════════════════
# Example 11: Cumulative Incidence and Hazard Plots
# ═══════════════════════════════════════════════════════════
# Complementary visualizations of survival data

result_cumulative <- survival(
  data = survival_test,
  elapsedtime = "elapsedtime",
  outcome = "outcome",
  explanatory = "treatment",
  ce = TRUE,  # Cumulative events (1 - survival)
  ch = TRUE   # Cumulative hazard
)

# Clinical Interpretation:

# Cumulative Events:
# - Shows proportion who experienced event
# - Easier to see small differences than survival curves
# - Complement of survival function
# - Useful for failure rate comparisons

# Cumulative Hazard:
# - H(t) = -log(S(t))
# - Nelson-Aalen estimator
# - Linear for constant hazard (exponential distribution)
# - Useful for assessing constant hazard assumption

# When to Use:
# - Cumulative events: When showing "risk" rather than "survival"
# - Cumulative hazard: Assessing distributional assumptions
# - Both: Comprehensive survival analysis report

# ═══════════════════════════════════════════════════════════
# Example 12: Log-Log Plot for Proportional Hazards Assessment
# ═══════════════════════════════════════════════════════════
# Visual assessment of PH assumption

result_loglog <- survival(
  data = survival_test,
  elapsedtime = "elapsedtime",
  outcome = "outcome",
  explanatory = "treatment",
  loglog = TRUE
)

# Clinical Context:
# Log-log plot = log(-log(survival)) vs log(time)
# - If lines are parallel → PH assumption satisfied
# - If lines cross or diverge → PH assumption violated

# Interpretation:
# - Parallel lines: Hazard ratio is constant over time
# - Crossing lines: Hazard ratio changes direction
# - Converging/diverging: Hazard ratio changes magnitude

# Actions if PH Violated:
# 1. Stratified Cox regression (Example 6)
# 2. Time-varying coefficients
# 3. Accelerated failure time models
# 4. Use RMST instead of HR (Example 8)

# ═══════════════════════════════════════════════════════════
# Example 13: Residual Diagnostics for Cox Model
# ═══════════════════════════════════════════════════════════
# Assess model fit and identify influential observations

result_diagnostics <- survival(
  data = survival_test,
  elapsedtime = "elapsedtime",
  outcome = "outcome",
  explanatory = "treatment",
  residual_diagnostics = TRUE
)

# Residual Types:

# Martingale Residuals:
# - Detect functional form of covariates
# - Should scatter around 0
# - Outliers indicate poor fit

# Deviance Residuals:
# - Normalized martingale residuals
# - Identify poorly predicted observations
# - Values > 2 or < -2 warrant investigation

# Schoenfeld Residuals:
# - Test proportional hazards assumption
# - One residual per event per covariate
# - Pattern vs time indicates PH violation

# Score Residuals:
# - Sensitivity to each observation
# - Identify influential subjects
# - Large values indicate high influence

# Clinical Use:
# - Quality control: Identify data errors
# - Model validation: Assess assumptions
# - Sensitivity analysis: Exclude influential cases

# ═══════════════════════════════════════════════════════════
# Example 14: Complete Clinical Trial Analysis
# ═══════════════════════════════════════════════════════════
# Publication-ready survival analysis workflow

# Step 1: Descriptive statistics
data(survival_test, package = "ClinicoPath")

# Step 2: Primary analysis
result_primary <- survival(
  data = survival_test,
  elapsedtime = "elapsedtime",
  outcome = "outcome",
  explanatory = "treatment",
  sc = TRUE,
  kmunicate = TRUE,
  risktable = TRUE,
  ci95 = TRUE,
  pplot = TRUE,
  medianline = "hv",
  cutp = "12, 36, 60",  # 1, 3, 5-year survival
  pw = TRUE,
  padjustmethod = "holm"
)

# Step 3: Test assumptions
result_assumptions <- survival(
  data = survival_test,
  elapsedtime = "elapsedtime",
  outcome = "outcome",
  explanatory = "treatment",
  ph_cox = TRUE,
  loglog = TRUE,
  residual_diagnostics = TRUE
)

# Step 4: Sensitivity analyses
result_rmst_sens <- survival(
  data = survival_test,
  elapsedtime = "elapsedtime",
  outcome = "outcome",
  explanatory = "treatment",
  rmst_analysis = TRUE,
  rmst_tau = 60
)

# Step 5: Person-time metrics
result_pt <- survival(
  data = survival_test,
  elapsedtime = "elapsedtime",
  outcome = "outcome",
  explanatory = "treatment",
  person_time = TRUE,
  time_intervals = "12, 36, 60"
)

# Reporting Checklist (CONSORT):
# ✓ Numbers at risk at regular time intervals
# ✓ Median follow-up time
# ✓ Median survival with 95% CI for each group
# ✓ Hazard ratio with 95% CI and p-value
# ✓ 1, 3, 5-year survival rates
# ✓ Log-rank p-value
# ✓ Censoring pattern described
# ✓ PH assumption tested and reported
# ✓ Adjustment for multiple comparisons

# ═══════════════════════════════════════════════════════════
# Example 15: Subgroup Analysis
# ═══════════════════════════════════════════════════════════
# Explore treatment effects in different subgroups

# Stage I-II (Early stage)
result_early <- survival(
  data = survival_test[survival_test$stage %in% c("I", "II"), ],
  elapsedtime = "elapsedtime",
  outcome = "outcome",
  explanatory = "treatment",
  sc = TRUE
)

# Stage III-IV (Advanced stage)
result_advanced <- survival(
  data = survival_test[survival_test$stage %in% c("III", "IV"), ],
  elapsedtime = "elapsedtime",
  outcome = "outcome",
  explanatory = "treatment",
  sc = TRUE
)

# By sex
result_male <- survival(
  data = survival_test[survival_test$sex == "Male", ],
  elapsedtime = "elapsedtime",
  outcome = "outcome",
  explanatory = "treatment"
)

result_female <- survival(
  data = survival_test[survival_test$sex == "Female", ],
  elapsedtime = "elapsedtime",
  outcome = "outcome",
  explanatory = "treatment"
)

# By age group
result_young <- survival(
  data = survival_test[survival_test$age < 65, ],
  elapsedtime = "elapsedtime",
  outcome = "outcome",
  explanatory = "treatment"
)

result_old <- survival(
  data = survival_test[survival_test$age >= 65, ],
  elapsedtime = "elapsedtime",
  outcome = "outcome",
  explanatory = "treatment"
)

# Cautions for Subgroup Analysis:
# - Pre-specify subgroups to avoid data dredging
# - Adjust for multiple testing
# - Report all subgroup analyses performed
# - Interaction tests more appropriate than separate analyses
# - Interpret with caution if not pre-specified
# - Subgroup findings should be hypothesis-generating

# Reporting:
# "Treatment effect was consistent across pre-specified subgroups (age,
# sex, disease stage). No statistically significant interaction was detected
# (all p-interaction > 0.10)"

# ═══════════════════════════════════════════════════════════
# Reporting Guidelines
# ═══════════════════════════════════════════════════════════

# When reporting survival analysis results:
#
# 1. Study Design
#    - Prospective or retrospective
#    - Inclusion/exclusion criteria
#    - Definition of time origin (diagnosis, randomization, etc.)
#    - Definition of event
#    - Censoring reasons
#
# 2. Descriptive Statistics
#    - Number of patients
#    - Number of events
#    - Number censored
#    - Median follow-up time
#    - Follow-up time range
#
# 3. Primary Results
#    - Median survival with 95% CI for each group
#    - Hazard ratio with 95% CI
#    - P-value from log-rank test
#    - 1, 3, 5-year survival rates (if applicable)
#
# 4. Model Assumptions
#    - Proportional hazards assumption tested (report p-value)
#    - Actions taken if violated
#    - Model fit assessed
#
# 5. Multiple Testing
#    - Adjustment method if multiple comparisons
#    - Pre-specified vs post-hoc analyses
#
# 6. Visualizations
#    - Kaplan-Meier curves
#    - Numbers at risk
#    - Confidence intervals (optional)
#    - Follow-up time indicated

# ═══════════════════════════════════════════════════════════
# Common Clinical Scenarios
# ═══════════════════════════════════════════════════════════

# Oncology RCT:
# - Treatment comparison (Example 1, 2)
# - Competing risks if elderly (Example 4)
# - 1, 3, 5-year survival rates
# - RMST if cure fraction (Example 8)

# Observational Study:
# - Date-based analysis (Example 3)
# - Stratified Cox if confounding (Example 6)
# - Person-time for varying follow-up (Example 7)
# - Subgroup analyses with caution (Example 15)

# Biomarker Study:
# - Landmark analysis if time-dependent (Example 5)
# - PH testing (Example 12)
# - Residual diagnostics (Example 13)

# Registry Data:
# - Competing risks for all-cause mortality (Example 4)
# - Person-time with staggered entry (Example 7)
# - Cause-specific vs overall survival

# ═══════════════════════════════════════════════════════════
# Best Practices
# ═══════════════════════════════════════════════════════════

# 1. Always report median follow-up time
# 2. Check and report proportional hazards assumption
# 3. Use appropriate censoring (administrative, loss to follow-up)
# 4. Consider competing risks in elderly/long follow-up
# 5. Adjust for multiple comparisons when needed
# 6. Report both relative (HR) and absolute (survival %) measures
# 7. Include numbers at risk on Kaplan-Meier plots
# 8. Pre-specify primary endpoint and subgroups
# 9. Use landmark analysis for time-dependent covariates
# 10. Consider RMST when PH assumption violated

