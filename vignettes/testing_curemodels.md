# Cure Models for Long-Term Survivors - Testing Checklist

## Test Dataset

**Primary:** `curemodels_test` (250 patients, colorectal cancer)
- Load: `data("curemodels_test", package = "ClinicoPath")`
- Variables:
  - `PatientID`: Integer identifier
  - `FollowUpMonths`: Continuous, follow-up time in months
  - `Recurrence`: Numeric 0/1 event indicator
  - `RecurrenceFactor`: Factor version of Recurrence ("No"/"Yes")
  - `Age`: Continuous, patient age
  - `Sex`: Factor ("Male"/"Female")
  - `Treatment`: Factor ("Surgery Only"/"Surgery+Adjuvant")
  - `Stage`: Factor ("I"/"II"/"III")
  - `TumorSize`: Continuous, tumor size in cm
  - `PerformanceStatus`: Integer (0/1/2)
  - `BackgroundHazard`: Numeric, population expected hazard rate

---

## 1. Mixture Cure Model (smcure)

### T01: Basic Mixture Cure Model (No Predictors)

**Inputs:**
- `time` = FollowUpMonths
- `status` = Recurrence
- `predictors` = [] (none)
- `model_type` = "mixture"

**Expected Outputs:**
- [ ] `modelTable` populated with intercept-only cure coefficients (`$b`) and survival coefficients (`$beta`)
- [ ] `cureTable` has single row with overall cure fraction, CI columns (NA if no bootstrap), uncured median
- [ ] `summary` HTML shows "Mixture Cure Model (smcure)" header, sample size, event count
- [ ] `interpretation` HTML includes cure fraction estimate and copy-ready report sentence
- [ ] `todo` is empty (variables are set)
- [ ] `warnings` is empty or reports minor notes
- [ ] `errors` is empty

**Verify:**
- Cure fraction is between 0 and 1
- Uncured median is positive
- Event count matches `sum(curemodels_test$Recurrence == 1)`

---

### T02: Mixture Cure Model with Predictors (PH)

**Inputs:**
- `time` = FollowUpMonths
- `status` = Recurrence
- `predictors` = [Treatment, Stage]
- `model_type` = "mixture"
- `smcure_model_type` = "ph"

**Expected Outputs:**
- [ ] `modelTable` has rows for cure component (intercept, Treatment, Stage) and survival component (Treatment, Stage)
- [ ] `cureTable` has rows for each predictor combination or group-level cure fractions
- [ ] Coefficients include estimate, std_error, z_value, p_value, ci_lower, ci_upper
- [ ] `interpretation` references predictor effects on cure probability

**Verify:**
- Cure coefficients (`$b`): positive coefficient = higher cure probability (logit link)
- Survival coefficients (`$beta`): PH model, interpreted as log hazard ratios for uncured patients
- z_value = estimate / std_error (approximately)
- p_value corresponds to two-sided test

---

### T03: Mixture Cure Model with AFT

**Inputs:**
- Same as T02 but `smcure_model_type` = "aft"

**Expected Outputs:**
- [ ] `modelTable` survival section labeled as AFT (acceleration factor) not PH
- [ ] `interpretation` mentions acceleration factors rather than hazard ratios

**Verify:**
- Survival coefficients now represent acceleration factors (positive = longer survival)
- Different coefficient values compared to T02 (PH model)

---

### T04: Alternative Link Functions

**Inputs:**
- `time` = FollowUpMonths
- `status` = Recurrence
- `predictors` = [Treatment]
- `model_type` = "mixture"
- `cure_link` = "probit"

**Expected Outputs:**
- [ ] `modelTable` cure component uses probit link
- [ ] `summary` mentions probit link function

**Repeat with:**
- `cure_link` = "cloglog"
- [ ] Analysis completes without error
- [ ] Cure fraction estimates differ slightly from logit link

---

### T05: Mixture Cure Model with Bootstrap CI

**Inputs:**
- `time` = FollowUpMonths
- `status` = Recurrence
- `predictors` = [Treatment]
- `model_type` = "mixture"
- `bootstrap_ci` = TRUE
- `n_bootstrap` = 100

**Expected Outputs:**
- [ ] `cureTable` columns cure_ci_lower and cure_ci_upper are populated (not NA)
- [ ] CI contains the point estimate: cure_ci_lower < cure_fraction < cure_ci_upper
- [ ] `cureFractionPlot` (if enabled) shows error bars

**Verify:**
- CI width is reasonable (not 0-1 trivially)
- Lower bound >= 0, upper bound <= 1
- With 100 bootstraps, expect moderate precision

---

## 2. Non-Mixture Cure Model (flexsurvcure)

### T06: Non-Mixture with Weibull Distribution

**Inputs:**
- `time` = FollowUpMonths
- `status` = Recurrence
- `predictors` = [Treatment]
- `model_type` = "nonmixture"
- `survival_dist` = "weibull"

**Expected Outputs:**
- [ ] `modelTable` populated with flexsurvcure `$res.t` matrix parameters (theta, shape, scale)
- [ ] `cureTable` has cure fraction derived from `theta` parameter
- [ ] `summary` shows "Non-Mixture Cure Model (flexsurvcure)"
- [ ] `interpretation` describes the non-mixture cure model interpretation

**Verify:**
- theta (cure fraction) between 0 and 1
- Weibull shape parameter > 0
- Weibull scale parameter > 0

---

### T07: Non-Mixture with Exponential Distribution

**Inputs:**
- Same as T06 but `survival_dist` = "exponential"

**Expected Outputs:**
- [ ] `modelTable` shows exponential distribution (no shape parameter, only rate)
- [ ] `modelComparison` (if multiple models run) shows different AIC from Weibull

**Verify:**
- Exponential is a special case of Weibull (shape=1); cure fraction may differ
- AIC typically higher than Weibull if data has non-constant hazard

---

### T08: Non-Mixture with Lognormal Distribution

**Inputs:**
- Same as T06 but `survival_dist` = "lognormal"

**Expected Outputs:**
- [ ] `modelTable` shows lognormal parameters (meanlog, sdlog)
- [ ] Analysis completes without error

---

### T09: Non-Mixture with Log-Logistic Distribution

**Inputs:**
- Same as T06 but `survival_dist` = "loglogistic"

**Expected Outputs:**
- [ ] `modelTable` shows log-logistic parameters (shape, scale)
- [ ] Analysis completes without error

---

## 3. cuRe Model (Background Mortality)

### T10: cuRe Model with Background Mortality

**Inputs:**
- `time` = FollowUpMonths
- `status` = Recurrence
- `predictors` = [Treatment]
- `model_type` = "cure"
- `use_background_mortality` = TRUE
- `background_hazard_var` = BackgroundHazard

**Expected Outputs:**
- [ ] `modelTable` populated with cuRe model coefficients (excess hazard component)
- [ ] `cureTable` has cure fraction adjusted for background mortality
- [ ] `summary` mentions background mortality adjustment
- [ ] `interpretation` explains that cure fraction accounts for population mortality

**Verify:**
- Cure fraction may differ from smcure estimate (background mortality absorbed)
- All BackgroundHazard values are positive in the data
- Model converges successfully

---

### T11: cuRe Model without Background Mortality

**Inputs:**
- `time` = FollowUpMonths
- `status` = Recurrence
- `predictors` = [Treatment]
- `model_type` = "cure"
- `use_background_mortality` = FALSE

**Expected Outputs:**
- [ ] Analysis completes; cuRe fitted without `bhazard` argument
- [ ] `summary` does not mention background mortality
- [ ] Cure fraction differs from T10

---

### T12: cuRe Model with Missing Background Hazard Variable

**Inputs:**
- `model_type` = "cure"
- `use_background_mortality` = TRUE
- `background_hazard_var` = NULL (not set)

**Expected Outputs:**
- [ ] `errors` HTML shows error message about needing a background hazard variable
- [ ] No crash; graceful error handling

---

## 4. Nonparametric Cure Model (npcure)

### T13: npcure with Auto Bandwidth

**Inputs:**
- `time` = FollowUpMonths
- `status` = Recurrence
- `model_type` = "npcure"
- `npcure_covariate` = Age
- `npcure_bandwidth` = "auto"
- `npcure_time_points` = 100

**Expected Outputs:**
- [ ] `cureTable` populated with cure probability estimates at evaluation points along Age
- [ ] `summary` shows "Nonparametric Cure Model (npcure)"
- [ ] `interpretation` describes nonparametric cure probability as a function of Age

**Verify:**
- Cure probabilities are between 0 and 1
- Estimates vary smoothly across Age values
- Number of evaluation points approximately matches `npcure_time_points`

---

### T14: npcure with Manual Bandwidth (small/medium/large)

**Inputs:**
- Same as T13 but `npcure_bandwidth` = "small"

**Expected Outputs:**
- [ ] Analysis completes; cure probability estimates are noisier (less smooth) than auto

**Repeat with:**
- `npcure_bandwidth` = "medium"
- [ ] Intermediate smoothness
- `npcure_bandwidth` = "large"
- [ ] Smoother curve, potentially over-smoothed

**Verify:**
- Bandwidth affects smoothness: small < medium < large
- All three complete without error

---

### T15: npcure with Different Covariate

**Inputs:**
- `model_type` = "npcure"
- `npcure_covariate` = TumorSize
- `npcure_bandwidth` = "auto"

**Expected Outputs:**
- [ ] Cure probability estimated as function of TumorSize
- [ ] Higher TumorSize may associate with lower cure probability

---

### T16: npcure without Covariate

**Inputs:**
- `model_type` = "npcure"
- `npcure_covariate` = NULL (not set)

**Expected Outputs:**
- [ ] Error or warning: npcure requires a covariate variable
- [ ] Graceful handling with informative message

---

## 5. Compare All Models

### T17: All Models Simultaneously

**Inputs:**
- `time` = FollowUpMonths
- `status` = Recurrence
- `predictors` = [Treatment]
- `model_type` = "all"
- `npcure_covariate` = Age
- `use_background_mortality` = TRUE
- `background_hazard_var` = BackgroundHazard

**Expected Outputs:**
- [ ] `modelComparison` table has rows for: mixture (smcure), nonmixture (flexsurvcure), cure (cuRe); npcure excluded (no AIC)
- [ ] Columns: model, aic, bic, loglik
- [ ] `modelTable` shows coefficients from all four models (sectioned or tabbed)
- [ ] `cureTable` shows cure fractions from all four models
- [ ] `interpretation` ranks models by AIC and identifies best-fitting

**Verify:**
- AIC values are finite and positive
- BIC values are finite and positive
- loglik values are negative (log-likelihood)
- Models with lower AIC are ranked higher

---

### T18: All Models without Background Hazard (cuRe falls back)

**Inputs:**
- `model_type` = "all"
- `use_background_mortality` = FALSE
- `background_hazard_var` = NULL
- `npcure_covariate` = Age

**Expected Outputs:**
- [ ] mixture, nonmixture, npcure all complete
- [ ] cuRe either fits without bhazard or is skipped with a note
- [ ] `modelComparison` still populated for available models

---

## 6. Display Options

### T19: Cure Fraction Plot

**Inputs:**
- `time` = FollowUpMonths
- `status` = Recurrence
- `predictors` = [Treatment]
- `model_type` = "mixture"
- `plot_cure_fraction` = TRUE

**Expected Outputs:**
- [ ] `cureFractionPlot` renders with bar chart
- [ ] Bars grouped by Treatment levels
- [ ] Colorblind-safe palette (blue/orange)
- [ ] Y-axis: Cure Fraction (0 to 1 or 0% to 100%)
- [ ] Title includes model type

**Verify:**
- Bar heights match cure_fraction values in cureTable
- Error bars present if bootstrap_ci enabled

---

### T20: Survival Curves Plot

**Inputs:**
- `time` = FollowUpMonths
- `status` = Recurrence
- `predictors` = [Treatment]
- `model_type` = "mixture"
- `plot_survival` = TRUE

**Expected Outputs:**
- [ ] `survivalPlot` renders with Kaplan-Meier curve overlaid with fitted model curve
- [ ] X-axis: Time (months), Y-axis: Survival Probability
- [ ] Legend distinguishes observed (KM) vs. fitted (cure model)
- [ ] Curves start at 1.0 and decrease monotonically

**Verify:**
- Fitted curve approaches the cure fraction asymptotically (does not reach 0)
- KM curve and fitted curve are visually close (good fit)

---

### T21: Goodness of Fit

**Inputs:**
- `time` = FollowUpMonths
- `status` = Recurrence
- `predictors` = [Treatment]
- `model_type` = "mixture"
- `goodness_of_fit` = TRUE

**Expected Outputs:**
- [ ] `goodnessOfFit` table populated with rows for each test
- [ ] Columns: test_name, statistic, p_value, interpretation
- [ ] At least one test: likelihood ratio test (cure model vs. standard model)
- [ ] interpretation column contains "Good fit" / "Poor fit" or similar

**Verify:**
- Statistic is non-negative
- p_value between 0 and 1
- interpretation is consistent with p_value

---

### T22: Sensitivity Analysis

**Inputs:**
- `time` = FollowUpMonths
- `status` = Recurrence
- `model_type` = "mixture"
- `sensitivity_analysis` = TRUE
- `cure_threshold` = 60

**Expected Outputs:**
- [ ] `sensitivityAnalysis` HTML shows cure fraction estimates at multiple thresholds (e.g., 36, 48, 60, 72, 84 months)
- [ ] Table or formatted output showing threshold vs. cure fraction
- [ ] Stability assessment: "Cure fraction is [stable/sensitive] across thresholds"

**Verify:**
- Cure fractions at adjacent thresholds are similar if model is robust
- Extreme thresholds (very short) may show different estimates

---

### T23: Custom Cure Threshold

**Inputs:**
- `time` = FollowUpMonths
- `status` = Recurrence
- `model_type` = "mixture"
- `cure_threshold` = 36

**Expected Outputs:**
- [ ] `interpretation` references 36-month cure threshold
- [ ] `cureTable` cure assessment based on 36-month threshold

**Repeat with:**
- `cure_threshold` = 120
- [ ] Interpretation changes to 120-month threshold

---

## 7. Edge Cases

### T24: Insufficient Data (Very Small Sample)

**Inputs:**
- Subset `curemodels_test` to 15 rows
- `model_type` = "mixture"

**Expected Outputs:**
- [ ] `warnings` HTML warns about small sample size
- [ ] If model fits: wide confidence intervals
- [ ] If model fails to converge: `errors` HTML with convergence failure message

---

### T25: Negative Follow-Up Times

**Inputs:**
- Modify data so some `FollowUpMonths` values are negative
- `model_type` = "mixture"

**Expected Outputs:**
- [ ] `errors` HTML shows error about negative/zero follow-up times
- [ ] No crash

---

### T26: Non-Binary Status Variable

**Inputs:**
- `status` = PerformanceStatus (values 0, 1, 2)
- `model_type` = "mixture"

**Expected Outputs:**
- [ ] `errors` HTML shows error about status variable needing to be binary (0/1)
- [ ] No crash

---

### T27: No Predictors for Any Model

**Inputs:**
- `predictors` = [] (empty)
- `model_type` = "mixture"

**Expected Outputs:**
- [ ] Intercept-only model fits successfully
- [ ] `cureTable` has single row with overall cure fraction
- [ ] No error about missing predictors

---

### T28: Variable Names with Spaces

**Inputs:**
- Rename `FollowUpMonths` to `Follow Up Months` in data
- `time` = `Follow Up Months`
- `model_type` = "mixture"

**Expected Outputs:**
- [ ] Analysis completes without error
- [ ] Variable name properly backtick-quoted in formula

---

### T29: All Censored (No Events)

**Inputs:**
- Modify data so all `Recurrence` = 0
- `model_type` = "mixture"

**Expected Outputs:**
- [ ] `errors` HTML: "No events observed; cannot fit cure model"
- [ ] No crash

---

### T30: Extreme Prevalence (>95% Events)

**Inputs:**
- Modify data so >95% of patients have `Recurrence` = 1
- `model_type` = "mixture"

**Expected Outputs:**
- [ ] `warnings` HTML warns about extreme event prevalence
- [ ] Cure fraction near 0 (very few cured)
- [ ] Model may have difficulty converging

---

## 8. Notice Validation

### T31: Welcome Message When Variables Not Set

**Inputs:**
- `time` = NULL (not set)
- `status` = NULL (not set)

**Expected Outputs:**
- [ ] `todo` HTML shows welcome/instructions message
- [ ] No other results populated
- [ ] No errors or crashes

---

### T32: Warning Accumulation

**Inputs:**
- Small dataset (30 patients) with many predictors (4+)
- `model_type` = "mixture"
- `bootstrap_ci` = TRUE
- `n_bootstrap` = 100

**Expected Outputs:**
- [ ] `warnings` HTML may show EPV warning (events per variable < 10)
- [ ] Bootstrap still runs (warning, not error)
- [ ] Multiple warnings concatenated properly in HTML

---

### T33: Model Convergence Failure Handling

**Inputs:**
- Dataset with very few events (<5) and multiple predictors
- `model_type` = "mixture"

**Expected Outputs:**
- [ ] `errors` HTML shows convergence failure message
- [ ] Suggests simplifying model (fewer predictors)
- [ ] No unhandled R error propagates to user

---

## Complete Option Coverage Checklist

| # | Option | Type | Default | Tests Covering It |
|---|--------|------|---------|-------------------|
| 1 | `time` | Variable | (required) | T01-T30 (all analysis tests) |
| 2 | `status` | Variable | (required) | T01-T30 (all analysis tests) |
| 3 | `predictors` | Variables [] | [] | T02-T05 (with), T01/T27 (without), T06-T11, T17-T22 |
| 4 | `model_type` | List | "mixture" | T01-T05 (mixture), T06-T09 (nonmixture), T10-T12 (cure), T13-T16 (npcure), T17-T18 (all) |
| 5 | `smcure_model_type` | List | "ph" | T02 (ph), T03 (aft) |
| 6 | `cure_link` | List | "logit" | T04 (probit, cloglog), T01-T03 (default logit) |
| 7 | `survival_dist` | List | "weibull" | T06 (weibull), T07 (exponential), T08 (lognormal), T09 (loglogistic) |
| 8 | `bootstrap_ci` | Bool | FALSE | T05 (TRUE), T01-T04 (FALSE default) |
| 9 | `n_bootstrap` | Integer | 1000 | T05 (100), T32 (100) |
| 10 | `cure_threshold` | Number | 60 | T22 (60 default), T23 (36, 120) |
| 11 | `plot_cure_fraction` | Bool | FALSE | T19 (TRUE) |
| 12 | `plot_survival` | Bool | FALSE | T20 (TRUE) |
| 13 | `goodness_of_fit` | Bool | FALSE | T21 (TRUE) |
| 14 | `sensitivity_analysis` | Bool | FALSE | T22 (TRUE) |
| 15 | `use_background_mortality` | Bool | FALSE | T10 (TRUE), T11 (FALSE), T12 (TRUE, no var), T17 (TRUE) |
| 16 | `background_hazard_var` | Variable | NULL | T10 (BackgroundHazard), T12 (NULL with use=TRUE), T17 (BackgroundHazard) |
| 17 | `npcure_covariate` | Variable | NULL | T13-T15 (Age, TumorSize), T16 (NULL) |
| 18 | `npcure_bandwidth` | List | "auto" | T13 (auto), T14 (small/medium/large) |
| 19 | `npcure_time_points` | Integer | 100 | T13 (100 default) |

### Coverage Summary

- **All 19 options** are covered by at least one test
- **All 5 model types** tested: mixture (T01-T05), nonmixture (T06-T09), cure (T10-T12), npcure (T13-T16), all (T17-T18)
- **All 4 survival distributions** tested: weibull (T06), exponential (T07), lognormal (T08), loglogistic (T09)
- **All 3 link functions** tested: logit (default), probit (T04), cloglog (T04)
- **All 4 bandwidth options** tested: auto (T13), small/medium/large (T14)
- **Both smcure model types** tested: ph (T02), aft (T03)
- **Edge cases covered**: small sample (T24), negative times (T25), non-binary status (T26), no predictors (T27), spaces in names (T28), all censored (T29), extreme prevalence (T30)
- **Notice validation**: welcome message (T31), warning accumulation (T32), convergence failure (T33)

---

## Quick Smoke Test (Minimum Viable Verification)

```r
data("curemodels_test", package = "ClinicoPath")

# Basic mixture cure model
curemodels(
  data = curemodels_test,
  time = "FollowUpMonths",
  status = "Recurrence",
  predictors = "Treatment",
  model_type = "mixture",
  smcure_model_type = "ph",
  cure_link = "logit"
)

# Non-mixture with bootstrap
curemodels(
  data = curemodels_test,
  time = "FollowUpMonths",
  status = "Recurrence",
  predictors = "Treatment",
  model_type = "nonmixture",
  survival_dist = "weibull",
  bootstrap_ci = TRUE,
  n_bootstrap = 100
)

# All models with all features
curemodels(
  data = curemodels_test,
  time = "FollowUpMonths",
  status = "Recurrence",
  predictors = "Treatment",
  model_type = "all",
  smcure_model_type = "ph",
  cure_link = "logit",
  survival_dist = "weibull",
  bootstrap_ci = FALSE,
  cure_threshold = 60,
  plot_cure_fraction = TRUE,
  plot_survival = TRUE,
  goodness_of_fit = TRUE,
  sensitivity_analysis = TRUE,
  use_background_mortality = TRUE,
  background_hazard_var = "BackgroundHazard",
  npcure_covariate = "Age",
  npcure_bandwidth = "auto",
  npcure_time_points = 100
)
```
