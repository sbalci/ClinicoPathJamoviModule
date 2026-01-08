# ═══════════════════════════════════════════════════════════
# Example Usage: multisurvival
# ═══════════════════════════════════════════════════════════
#
# Comprehensive examples demonstrating multisurvival usage
# for multivariable Cox proportional hazards regression

library(ClinicoPath)

# ═══════════════════════════════════════════════════════════
# Example 1: Basic Multivariable Cox Regression
# ═══════════════════════════════════════════════════════════
# Standard survival analysis with multiple predictors

data(multisurvival_test, package = "ClinicoPath")

# Basic model with categorical and continuous predictors
result_basic <- multisurvival(
  data = multisurvival_test,
  elapsedtime = "elapsedtime",
  outcome = "outcome",
  outcomeLevel = "1",
  explanatory = c("treatment", "stage", "grade"),
  contexpl = c("age", "nodes"),
  timetypeoutput = "months"
)

# Clinical Interpretation:
# - elapsedtime: Follow-up time in months
# - outcome: Event indicator (0=censored, 1=event)
# - explanatory: Categorical predictors (treatment arm, stage, grade)
# - contexpl: Continuous predictors (age, number of positive nodes)

# ═══════════════════════════════════════════════════════════
# Example 2: Date-Based Survival Calculation
# ═══════════════════════════════════════════════════════════
# Calculate survival time from diagnosis and follow-up dates

data(multisurvival_dates, package = "ClinicoPath")

result_dates <- multisurvival(
  data = multisurvival_dates,
  tint = TRUE,  # Calculate time from dates
  dxdate = "dxdate",  # Diagnosis date
  fudate = "fudate",  # Follow-up date
  timetypedata = "ymd",  # Date format: YYYY-MM-DD
  timetypeoutput = "months",  # Output in months
  outcome = "outcome",
  outcomeLevel = "Dead",
  explanatory = c("treatment", "stage"),
  contexpl = "age"
)

# Clinical Application:
# - Common when working with EHR data containing dates
# - Automatically calculates elapsed time from diagnosis to last follow-up
# - Useful for standardizing time units across different data sources

# ═══════════════════════════════════════════════════════════
# Example 3: Hazard Ratio Visualization
# ═══════════════════════════════════════════════════════════
# Generate forest plot of hazard ratios

result_forest <- multisurvival(
  data = multisurvival_test,
  elapsedtime = "elapsedtime",
  outcome = "outcome",
  outcomeLevel = "1",
  explanatory = c("treatment", "stage", "grade", "sex"),
  contexpl = c("age", "nodes", "biomarker"),
  hr = TRUE,  # Show hazard ratio plot
  sty = "t3"  # survminer style forest plot
)

# Interpretation:
# - HR > 1: Increased hazard (worse prognosis)
# - HR < 1: Decreased hazard (better prognosis)
# - HR = 1: No effect
# - 95% CI not crossing 1: Statistically significant

# ═══════════════════════════════════════════════════════════
# Example 4: Kaplan-Meier Survival Curves
# ═══════════════════════════════════════════════════════════
# Visualize survival functions by treatment group

result_km <- multisurvival(
  data = multisurvival_test,
  elapsedtime = "elapsedtime",
  outcome = "outcome",
  outcomeLevel = "1",
  explanatory = "treatment",
  km = TRUE,  # Generate KM plot
  risktable = TRUE,  # Show number at risk
  ci95 = TRUE,  # Show 95% confidence intervals
  censored = TRUE,  # Mark censored observations
  medianline = "hv",  # Show median survival lines
  pplot = TRUE,  # Show p-value on plot
  endplot = 60,  # Plot up to 60 months
  byplot = 12  # Time intervals every 12 months
)

# Clinical Context:
# - Visual comparison of survival curves between groups
# - Risk table shows patients remaining at each time point
# - Median survival indicated by horizontal and vertical lines
# - P-value from log-rank test

# ═══════════════════════════════════════════════════════════
# Example 5: Risk Stratification Analysis
# ═══════════════════════════════════════════════════════════
# Calculate prognostic risk scores and stratify patients

data(multisurvival_risk, package = "ClinicoPath")

result_risk <- multisurvival(
  data = multisurvival_risk,
  elapsedtime = "elapsedtime",
  outcome = "outcome",
  explanatory = c("stage", "grade", "molecular_subtype"),
  contexpl = c("age", "ki67", "tumor_size"),
  calculateRiskScore = TRUE,  # Calculate risk scores
  numRiskGroups = "three",  # Divide into low/intermediate/high risk
  plotRiskGroups = TRUE  # Plot survival by risk group
)

# Clinical Application:
# - Prognostic model development
# - Personalized risk assessment
# - Treatment stratification
# - Clinical trial eligibility (e.g., high-risk patients only)

# Risk Score Calculation:
# - Based on Cox model coefficients
# - Each patient gets a risk score
# - Patients divided into quantile-based risk groups
# - Low risk: Better prognosis
# - High risk: Worse prognosis, may need aggressive treatment

# ═══════════════════════════════════════════════════════════
# Example 6: Competing Risks Analysis
# ═══════════════════════════════════════════════════════════
# Multiple event types: disease death vs. other causes

data(multisurvival_competing, package = "ClinicoPath")

result_competing <- multisurvival(
  data = multisurvival_competing,
  elapsedtime = "elapsedtime",
  outcome = "outcome",
  outcomeLevel = "Dead of Disease",
  dod = "Dead of Disease",  # Dead of disease
  dooc = "Dead of Other",  # Dead of other causes
  awd = "Alive w Disease",  # Alive with disease
  awod = "Alive w/o Disease",  # Alive without disease
  analysistype = "compete",  # Competing risks analysis
  multievent = TRUE,
  explanatory = c("treatment", "stage", "age_group"),
  contexpl = c("age", "charlson_score")
)

# Clinical Context:
# Competing risks important when:
# - Elderly populations (competing mortality from comorbidities)
# - Indolent cancers (other causes compete with cancer death)
# - Long follow-up (more time for competing events)

# Interpretation:
# - Disease-specific mortality: Focus on disease of interest
# - Competing risks: Accounts for patients who die of other causes
# - Cumulative incidence: Probability of each event type

# ═══════════════════════════════════════════════════════════
# Example 7: Landmark Analysis
# ═══════════════════════════════════════════════════════════
# Assess treatment response at a specific time point

data(multisurvival_landmark, package = "ClinicoPath")

result_landmark <- multisurvival(
  data = multisurvival_landmark,
  elapsedtime = "elapsedtime",
  outcome = "outcome",
  uselandmark = TRUE,
  landmark = 3,  # 3-month landmark time
  explanatory = "response_3mo",  # Response at 3 months
  contexpl = "age",
  timetypeoutput = "months"
)

# Clinical Application:
# - Immunotherapy: Early response predicts long-term outcome
# - Chemotherapy: Response at 3 months guides treatment decisions
# - Removes guarantee-time bias (patients must survive to landmark)

# Example Scenarios:
# - Response at 3 months predicts overall survival
# - Treatment changes based on early response
# - Exclude patients who don't reach landmark (early deaths)

# Landmark Time Selection:
# - Immunotherapy: 3-6 months (delayed response possible)
# - Chemotherapy: 2-3 months (rapid response expected)
# - Targeted therapy: 2-4 months

# ═══════════════════════════════════════════════════════════
# Example 8: Stratification for Non-Proportional Hazards
# ═══════════════════════════════════════════════════════════
# Handle violations of proportional hazards assumption

data(multisurvival_stratify, package = "ClinicoPath")

# Step 1: Test proportional hazards assumption
result_ph_test <- multisurvival(
  data = multisurvival_stratify,
  elapsedtime = "elapsedtime",
  outcome = "outcome",
  explanatory = c("sex", "treatment"),
  contexpl = "age",
  ph_cox = TRUE  # Test PH assumption
)

# Step 2: If PH violated for sex, stratify by sex
result_stratified <- multisurvival(
  data = multisurvival_stratify,
  elapsedtime = "elapsedtime",
  outcome = "outcome",
  explanatory = "treatment",  # Treatment effect of interest
  contexpl = "age",
  use_stratify = TRUE,
  stratvar = "sex"  # Stratify by sex (PH violated)
)

# Clinical Interpretation:
# - Proportional hazards: Hazard ratio constant over time
# - Non-proportional: Hazard ratio changes over time
# - Stratification: Allows different baseline hazards for strata

# When to Stratify:
# - Schoenfeld residuals test p < 0.05
# - Visual inspection of log-log plots
# - Known time-varying effects (e.g., treatment effect diminishes)

# Variables Often Requiring Stratification:
# - Sex (different baseline mortality)
# - Treatment effects (may vary over time)
# - Center/site in multi-center trials

# ═══════════════════════════════════════════════════════════
# Example 9: Person-Time Incidence Rate Analysis
# ═══════════════════════════════════════════════════════════
# Calculate incidence rates per person-time at risk

data(multisurvival_persontime, package = "ClinicoPath")

result_persontime <- multisurvival(
  data = multisurvival_persontime,
  elapsedtime = "elapsedtime",
  outcome = "outcome",
  explanatory = "treatment_group",
  person_time = TRUE,
  time_intervals = "12, 36, 60",  # Intervals: 0-12, 12-36, 36-60, 60+
  rate_multiplier = 1000  # Rate per 1000 person-years
)

# Clinical Application:
# - Incidence rate: Events per unit of person-time
# - Accounts for varying follow-up durations
# - Comparable across studies with different follow-up

# Interpretation:
# - Rate per 1000 person-years
# - Example: 50 events per 1000 person-years = 5% annual incidence
# - Rate ratios: Compare incidence between groups

# Typical Time Intervals:
# - Cancer screening: 0-12, 12-36, 36-60 months
# - Chronic disease: Annual intervals
# - Infectious disease: Monthly or quarterly

# ═══════════════════════════════════════════════════════════
# Example 10: Adjusted Survival Curves
# ═══════════════════════════════════════════════════════════
# Show covariate-adjusted survival curves

result_adjusted <- multisurvival(
  data = multisurvival_test,
  elapsedtime = "elapsedtime",
  outcome = "outcome",
  explanatory = c("stage", "grade"),
  contexpl = c("age", "nodes"),
  ac = TRUE,  # Adjusted curves
  adjexplanatory = "treatment",  # Variable for adjusted curve
  ac_method = "average"  # Average covariate values
)

# Adjustment Methods:
# - average: Average over all covariate values
# - conditional: Conditional on specific covariate levels
# - marginal: Marginal survival (population average)

# Clinical Use:
# - Compare treatments adjusting for prognostic factors
# - Show survival for "average" patient in each treatment group
# - Useful when groups differ in baseline characteristics

# ═══════════════════════════════════════════════════════════
# Example 11: Nomogram for Individual Risk Prediction
# ═══════════════════════════════════════════════════════════
# Create nomogram for point-of-care risk assessment

result_nomogram <- multisurvival(
  data = multisurvival_test,
  elapsedtime = "elapsedtime",
  outcome = "outcome",
  explanatory = c("treatment", "stage", "grade"),
  contexpl = c("age", "nodes"),
  showNomogram = TRUE  # Generate nomogram
)

# Clinical Application:
# - Point-of-care prognostic tool
# - Individual patient risk prediction
# - Shared decision-making with patients
# - Treatment selection based on predicted survival

# Using a Nomogram:
# 1. Find points for each variable on top scale
# 2. Sum points across all variables
# 3. Locate total points on bottom scale
# 4. Read predicted survival probability

# Example:
# - Stage III = 60 points
# - Grade 3 = 40 points
# - Age 70 = 30 points
# - Total = 130 points → 50% 5-year survival

# ═══════════════════════════════════════════════════════════
# Example 12: Model Diagnostics and Validation
# ═══════════════════════════════════════════════════════════
# Check model assumptions and fit

result_diagnostics <- multisurvival(
  data = multisurvival_test,
  elapsedtime = "elapsedtime",
  outcome = "outcome",
  explanatory = c("treatment", "stage"),
  contexpl = c("age", "nodes"),
  ph_cox = TRUE,  # Test proportional hazards
  hr = TRUE,  # Show hazard ratios
  showExplanations = TRUE  # Show detailed explanations
)

# Key Diagnostics:
# 1. Proportional Hazards Test (Schoenfeld residuals)
#    - Global test p-value
#    - Individual variable tests
#    - If violated: Consider stratification or time-dependent covariates

# 2. Model Fit Statistics:
#    - Concordance index (C-index): Discrimination ability
#    - Likelihood ratio test: Overall model significance
#    - Wald test: Individual coefficient significance

# 3. Influence Diagnostics:
#    - Outliers: Unusual survival patterns
#    - Influential observations: Large impact on coefficients

# ═══════════════════════════════════════════════════════════
# Example 13: Complete Clinical Trial Analysis Workflow
# ═══════════════════════════════════════════════════════════
# Comprehensive analysis for publication

result_trial <- multisurvival(
  data = multisurvival_test,
  elapsedtime = "elapsedtime",
  outcome = "outcome",
  outcomeLevel = "1",
  timetypeoutput = "months",
  
  # Predictors
  explanatory = c("treatment", "stage", "grade", "sex"),
  contexpl = c("age", "nodes", "biomarker", "performance_status"),
  
  # Visualizations
  hr = TRUE,  # Forest plot
  sty = "t3",  # survminer style
  km = TRUE,  # KM curves
  risktable = TRUE,
  ci95 = TRUE,
  censored = TRUE,
  medianline = "hv",
  pplot = TRUE,
  
  # Plot settings
  endplot = 60,
  byplot = 12,
  
  # Diagnostics
  ph_cox = TRUE,
  
  # Clinical tools
  showNomogram = TRUE,
  
  # Documentation
  showExplanations = TRUE,
  showSummaries = TRUE
)

# Publication Checklist:
# ✓ Baseline characteristics table
# ✓ Kaplan-Meier curves with number at risk
# ✓ Multivariable Cox regression results (HR, 95% CI, p-value)
# ✓ Forest plot of hazard ratios
# ✓ Proportional hazards test results
# ✓ Median survival times with 95% CI
# ✓ Log-rank test p-values
# ✓ Nomogram (if developing prognostic model)

# ═══════════════════════════════════════════════════════════
# Example 14: Subgroup Analysis
# ═══════════════════════════════════════════════════════════
# Treatment effect in specific patient subgroups

# Early stage patients
early_stage <- multisurvival_test[multisurvival_test$stage %in% c("I", "II"), ]

result_early <- multisurvival(
  data = early_stage,
  elapsedtime = "elapsedtime",
  outcome = "outcome",
  explanatory = "treatment",
  contexpl = "age",
  km = TRUE,
  hr = TRUE
)

# Advanced stage patients
advanced_stage <- multisurvival_test[multisurvival_test$stage %in% c("III", "IV"), ]

result_advanced <- multisurvival(
  data = advanced_stage,
  elapsedtime = "elapsedtime",
  outcome = "outcome",
  explanatory = "treatment",
  contexpl = "age",
  km = TRUE,
  hr = TRUE
)

# Clinical Interpretation:
# - Compare treatment effect across subgroups
# - Test for treatment × subgroup interaction
# - Identify patients who benefit most from treatment

# Common Subgroups:
# - Stage (early vs. advanced)
# - Biomarker status (positive vs. negative)
# - Age (<65 vs. ≥65)
# - Performance status (0-1 vs. 2+)

# ═══════════════════════════════════════════════════════════
# Example 15: Sensitivity Analysis
# ═══════════════════════════════════════════════════════════
# Test robustness of results to different assumptions

# Primary analysis (intention-to-treat)
result_itt <- multisurvival(
  data = multisurvival_test,
  elapsedtime = "elapsedtime",
  outcome = "outcome",
  explanatory = "treatment",
  contexpl = "age"
)

# Sensitivity analysis: Different time units
result_years <- multisurvival(
  data = multisurvival_test,
  elapsedtime = "elapsedtime",
  outcome = "outcome",
  explanatory = "treatment",
  contexpl = "age",
  timetypeoutput = "years"
)

# Sensitivity analysis: Landmark at different time point
data(multisurvival_landmark, package = "ClinicoPath")

result_landmark6mo <- multisurvival(
  data = multisurvival_landmark,
  elapsedtime = "elapsedtime",
  outcome = "outcome",
  uselandmark = TRUE,
  landmark = 6,  # 6-month landmark instead of 3
  explanatory = "response_3mo",
  contexpl = "age"
)

# Sensitivity Analyses to Consider:
# 1. Different landmark times
# 2. Different risk group cut-points
# 3. Complete case vs. multiple imputation for missing data
# 4. Per-protocol vs. intention-to-treat
# 5. Different adjustment variables

# ═══════════════════════════════════════════════════════════
# Reporting Guidelines
# ═══════════════════════════════════════════════════════════

# When reporting multivariable survival analysis results:
#
# 1. Study Design and Population
#    - Inclusion/exclusion criteria
#    - Sample size and events
#    - Follow-up duration (median, range)
#    - Censoring pattern
#
# 2. Variables
#    - Outcome definition (event of interest)
#    - Time origin (diagnosis, treatment start, enrollment)
#    - Predictor variables (categorical reference levels, continuous scaling)
#    - Missing data handling
#
# 3. Statistical Methods
#    - Cox proportional hazards regression
#    - Time scale (days, months, years)
#    - Proportional hazards assumption testing (Schoenfeld residuals)
#    - Handling of violations (stratification, time-dependent covariates)
#    - Adjustment for multiple comparisons (if applicable)
#
# 4. Results Presentation
#    - Hazard ratios with 95% confidence intervals
#    - P-values (two-sided)
#    - Number of events/total in each group
#    - Median survival with 95% CI
#    - Kaplan-Meier curves
#    - Model fit statistics (concordance index, likelihood ratio test)
#
# 5. Model Development (if applicable)
#    - Variable selection method
#    - Risk score calculation
#    - Internal validation (bootstrap, cross-validation)
#    - Calibration and discrimination
#    - External validation (if available)

# ═══════════════════════════════════════════════════════════
# Common Clinical Applications
# ═══════════════════════════════════════════════════════════

# 1. Cancer Prognostic Models
#    - Stage, grade, biomarkers → overall survival
#    - Risk stratification for treatment intensity
#    - Nomograms for individual patient counseling

# 2. Treatment Comparison Trials
#    - Randomized controlled trials
#    - Adjusted for baseline prognostic factors
#    - Subgroup analyses

# 3. Observational Studies
#    - Registry data analysis
#    - Real-world effectiveness
#    - Comparative effectiveness research

# 4. Biomarker Studies
#    - Prognostic biomarkers (independent of treatment)
#    - Predictive biomarkers (treatment effect modification)
#    - Companion diagnostics

# 5. Quality Improvement
#    - Hospital outcomes (30-day, 90-day mortality)
#    - Surgical outcomes
#    - Complication-free survival

# ═══════════════════════════════════════════════════════════
# Best Practices
# ═══════════════════════════════════════════════════════════

# 1. Sample Size
#    - Minimum 10 events per predictor variable
#    - More events needed for reliable nomogram
#    - Consider competing risks in elderly populations

# 2. Variable Selection
#    - Pre-specify clinically important variables
#    - Avoid data-driven selection (overfitting)
#    - Justify each variable inclusion

# 3. Model Assumptions
#    - Always test proportional hazards assumption
#    - Check for linearity of continuous predictors
#    - Assess multicollinearity

# 4. Missing Data
#    - Report extent and pattern of missing data
#    - Complete case analysis if <5% missing
#    - Multiple imputation if >5% missing
#    - Sensitivity analysis comparing methods

# 5. Validation
#    - Internal: Bootstrap or cross-validation
#    - External: Independent cohort
#    - Report c-index, calibration plots

# 6. Reporting
#    - Follow CONSORT for RCTs
#    - Follow STROBE for observational studies
#    - Include CONSORT/STROBE flowchart
#    - Report according to REMARK for prognostic markers

# ═══════════════════════════════════════════════════════════
# Common Pitfalls to Avoid
# ═══════════════════════════════════════════════════════════

# 1. Guarantee-Time Bias
#    - Problem: Requiring survival to landmark for inclusion
#    - Solution: Use landmark analysis explicitly

# 2. Overfitting
#    - Problem: Too many variables for number of events
#    - Solution: 10+ events per variable, internal validation

# 3. Proportional Hazards Violation
#    - Problem: Ignoring PH assumption violations
#    - Solution: Test assumption, stratify or use time-dependent covariates

# 4. Multicollinearity
#    - Problem: Highly correlated predictors (e.g., stage and T/N/M)
#    - Solution: Use composite variables, remove redundant predictors

# 5. Missing Data
#    - Problem: Complete case analysis with informative missingness
#    - Solution: Multiple imputation, report missing data patterns

# 6. Competing Risks
#    - Problem: Treating competing events as censored
#    - Solution: Use competing risks analysis (Fine-Gray model)
