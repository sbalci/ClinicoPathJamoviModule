# Cure Models for Long-Term Survivors Documentation

## 1. Overview

- **Function**: `curemodels`
- **Files**:
  - `jamovi/curemodels.u.yaml` -- UI
  - `jamovi/curemodels.a.yaml` -- Options (19 total)
  - `R/curemodels.b.R` -- Backend
  - `jamovi/curemodels.r.yaml` -- Results (5 tables, 2 plots, 5 HTML)
- **Menu location**: OncoPathD > Cure Models for Long-Term Survivors
- **Summary**: Cure models estimate the proportion of patients who are statistically cured (i.e., whose long-term hazard returns to that of the general population) and characterize the survival distribution of uncured patients. The module implements four modelling frameworks: mixture cure (smcure), non-mixture cure (flexsurvcure), background-mortality-adjusted cure (cuRe), and nonparametric cure (npcure). It supports covariate-adjusted cure fraction estimation, bootstrap confidence intervals, goodness-of-fit testing, sensitivity analysis across cure thresholds, and side-by-side model comparison with AIC/BIC/log-likelihood. Designed for oncology researchers evaluating long-term survivors in colorectal, breast, lung, and other solid tumour cohorts.

---

## 2. Feature Summary

In many cancers, a subset of patients is effectively cured: after sufficient follow-up, their mortality risk returns to that of the general population. Standard survival models (Cox, Kaplan-Meier) assume every patient will eventually experience the event, which misrepresents prognosis when a cure fraction exists. Cure models explicitly partition the population into cured and uncured groups, providing two clinically distinct quantities: the cure fraction (probability of being cured) and the survival function of uncured patients (latency distribution).

The module provides four complementary approaches:

1. **Mixture cure model (smcure)**: Decomposes survival into a cure probability (incidence/logistic component) and a latency distribution for the uncured (PH or AFT). The workhorse model for clinical cure rate estimation.
2. **Non-mixture cure model (flexsurvcure)**: Parametric model where the cure fraction is embedded in the survival function rather than as a separate mixture component. Supports Weibull, exponential, lognormal, and log-logistic distributions.
3. **Background-mortality-adjusted cure model (cuRe)**: Incorporates population mortality tables to separate excess disease hazard from expected background hazard. Essential when long follow-up makes background mortality non-negligible.
4. **Nonparametric cure model (npcure)**: Kernel-smoothing-based estimation that makes no distributional assumptions. Provides cure probability as a function of a covariate without requiring a parametric form.

All four models can be run simultaneously (`model_type = "all"`) for direct comparison via AIC, BIC, and log-likelihood. Clinical interpretation is generated automatically with copy-ready report sentences.

---

## 3. Feature Details

### Input Variables

| Feature | YAML Argument (.a.yaml) | UI Label | Results Section (.r.yaml) | R Function (.b.R) |
|---------|------------------------|----------|--------------------------|-------------------|
| Follow-up Time | `time` (Variable, numeric, continuous) | Follow-up Time | modelTable, cureTable, all plots | `survival::Surv()` time component; `smcure::smcure(Surv(time, status)~...)`; `flexsurvcure::flexsurvcure(Surv(time, status)~...)` |
| Event Status | `status` (Variable, numeric/factor, nominal) | Event Status (0/1) | modelTable, cureTable | Binary event indicator: 0=censored, 1=event; validated for binary content |
| Predictor Variables | `predictors` (Variables, [], numeric/factor) | Predictor Variables (Optional) | modelTable (covariate coefficients), cureTable (group-specific cure fractions) | Covariates for incidence and/or latency models; `smcure::smcure(cureform=~predictors)`; `flexsurvcure::flexsurvcure(~predictors)` |

### Model Specification

| Feature | YAML Argument (.a.yaml) | UI Label | Results Section (.r.yaml) | R Function (.b.R) |
|---------|------------------------|----------|--------------------------|-------------------|
| Model Type | `model_type` (List: mixture/nonmixture/cure/npcure/all, default: mixture) | Cure Model Type | Controls which model(s) are fitted; all result sections adapt | Dispatches to `smcure::smcure()`, `flexsurvcure::flexsurvcure()`, `cuRe::fit.cure.model()`, `npcure::probcure()`, or all four |
| smcure Model Type | `smcure_model_type` (List: ph/aft) | smcure Model Type (PH/AFT) | modelTable (coefficient interpretation changes) | `smcure::smcure(model="ph")` or `smcure::smcure(model="aft")`; PH gives hazard ratios, AFT gives acceleration factors |
| Cure Link Function | `cure_link` (List: logit/probit/cloglog) | Link Function for Cure Probability | modelTable (cure component coefficients) | `smcure::smcure(link=...)` for incidence model; logit is standard, probit/cloglog for sensitivity |
| Survival Distribution | `survival_dist` (List: weibull/exponential/lognormal/loglogistic) | Parametric Survival Distribution | modelTable, modelComparison | `flexsurvcure::flexsurvcure(dist=...)`: Weibull (default, most flexible), exponential (constant hazard), lognormal (log-normal latency), loglogistic (proportional odds) |

### Bootstrap and Confidence Intervals

| Feature | YAML Argument (.a.yaml) | UI Label | Results Section (.r.yaml) | R Function (.b.R) |
|---------|------------------------|----------|--------------------------|-------------------|
| Bootstrap CI | `bootstrap_ci` (Bool, default: FALSE) | Bootstrap Confidence Intervals | cureTable (cure_ci_lower, cure_ci_upper columns populated) | `smcure::smcure(Var=TRUE, nboot=n_bootstrap)` for smcure; manual bootstrap loop for flexsurvcure and cuRe |
| Number of Bootstrap Resamples | `n_bootstrap` (Integer, default: 1000, min: 100, max: 10000) | Number of Bootstrap Resamples | cureTable CI width | Controls bootstrap iterations; higher = more precise CIs but slower computation |

### Threshold and Sensitivity

| Feature | YAML Argument (.a.yaml) | UI Label | Results Section (.r.yaml) | R Function (.b.R) |
|---------|------------------------|----------|--------------------------|-------------------|
| Cure Threshold | `cure_threshold` (Number, default: 60) | Cure Threshold (months) | interpretation (clinical context), cureTable (threshold-based cure assessment) | Defines the minimum follow-up time beyond which patients are considered potentially cured; used in clinical interpretation and sensitivity analysis |
| Sensitivity Analysis | `sensitivity_analysis` (Bool, default: FALSE) | Sensitivity Analysis | sensitivityAnalysis (Html) | Varies the cure threshold (e.g., 36, 48, 60, 72, 84 months) and re-estimates cure fractions; reports stability of cure rate across thresholds |

### Plots and Diagnostics

| Feature | YAML Argument (.a.yaml) | UI Label | Results Section (.r.yaml) | R Function (.b.R) |
|---------|------------------------|----------|--------------------------|-------------------|
| Cure Fraction Plot | `plot_cure_fraction` (Bool, default: FALSE) | Plot Cure Fraction | cureFractionPlot (Image, 600x400) | Bar chart of cure fractions by model and/or predictor group; colorblind-safe palette (blue/orange) |
| Survival Curves Plot | `plot_survival` (Bool, default: FALSE) | Plot Survival Curves | survivalPlot (Image, 700x500) | Kaplan-Meier overlay with fitted cure model survival curves; shows observed vs. model-predicted; separate curves for cured/uncured subpopulations when applicable |
| Goodness of Fit | `goodness_of_fit` (Bool, default: FALSE) | Goodness of Fit Tests | goodnessOfFit (Table: test_name/statistic/p_value/interpretation) | Likelihood ratio test, AIC/BIC comparison, visual residual assessment; tests whether the cure model fits significantly better than a standard survival model |

### cuRe Model Options

| Feature | YAML Argument (.a.yaml) | UI Label | Results Section (.r.yaml) | R Function (.b.R) |
|---------|------------------------|----------|--------------------------|-------------------|
| Use Background Mortality | `use_background_mortality` (Bool, default: FALSE) | Use Background Mortality (cuRe model) | modelTable (cuRe coefficients), interpretation | `cuRe::fit.cure.model(bhazard=...)` when TRUE; separates disease-specific excess hazard from population hazard |
| Background Hazard Variable | `background_hazard_var` (Variable, default: NULL) | Background Hazard Variable | modelTable (cuRe model section) | Numeric variable containing per-patient expected hazard rates from population tables; passed as `bhazard` argument to `cuRe::fit.cure.model()` |

### npcure Model Options

| Feature | YAML Argument (.a.yaml) | UI Label | Results Section (.r.yaml) | R Function (.b.R) |
|---------|------------------------|----------|--------------------------|-------------------|
| npcure Covariate | `npcure_covariate` (Variable, default: NULL) | Covariate for Nonparametric Cure | cureTable (covariate-specific cure probabilities) | `npcure::probcure(x=covariate, t=time, d=status, x0=evaluation_points)`: continuous covariate for kernel smoothing |
| npcure Bandwidth | `npcure_bandwidth` (List: auto/small/medium/large) | Bandwidth Selection | cureTable (affects smoothness of cure probability estimate) | `auto`: cross-validation optimal bandwidth; `small`/`medium`/`large`: fixed fractions of covariate range for under/over-smoothing sensitivity |
| npcure Time Points | `npcure_time_points` (Integer, default: 100, min: 50, max: 500) | Number of Evaluation Time Points | cureTable, cureFractionPlot (resolution) | Controls granularity of nonparametric cure probability estimation grid |

### Core Results

| Feature | Results Section (.r.yaml) | Type | Columns / Content | When Populated |
|---------|--------------------------|------|-------------------|----------------|
| Welcome / Instructions | `todo` (Html) | Html | Instructions when required variables not set | Always, when `time` or `status` is NULL |
| Warnings | `warnings` (Html) | Html | Data validation warnings (EPV, prevalence extremes, convergence) | When data quality issues detected |
| Errors | `errors` (Html) | Html | Fatal errors preventing analysis | When model fitting fails |
| Summary | `summary` (Html) | Html | Model overview, sample size, event count, model type, key findings | Always after successful fit |
| Model Coefficients | `modelTable` (Table) | Table | parameter, estimate, std_error, z_value, p_value, ci_lower, ci_upper | All model types; smcure shows `$b` (cure) and `$beta` (survival) separately; flexsurvcure shows `$res.t` matrix; cuRe shows cure model coefficients |
| Cure Fractions | `cureTable` (Table) | Table | group, cure_fraction, cure_ci_lower, cure_ci_upper, uncured_median | All model types; one row per group (or single row if no predictors) |

### Comparison and Interpretation

| Feature | Results Section (.r.yaml) | Type | Columns / Content | When Populated |
|---------|--------------------------|------|-------------------|----------------|
| Model Comparison | `modelComparison` (Table) | Table | model, aic, bic, loglik | When `model_type = "all"` or multiple distributions compared |
| Goodness of Fit | `goodnessOfFit` (Table) | Table | test_name, statistic, p_value, interpretation | When `goodness_of_fit = TRUE` |
| Sensitivity Analysis | `sensitivityAnalysis` (Html) | Html | Cure fraction stability across threshold values | When `sensitivity_analysis = TRUE` |
| Clinical Interpretation | `interpretation` (Html) | Html | Plain-language interpretation, copy-ready report sentence, cure threshold context, clinical recommendations | Always after successful fit |

---

## 4. Complete Options-to-Results Matrix

An "X" indicates the result is affected by that option (recomputed when the option changes).

| Option | todo | warnings | errors | summary | modelTable | cureTable | cureFractionPlot | survivalPlot | goodnessOfFit | sensitivityAnalysis | modelComparison | interpretation |
|--------|------|----------|--------|---------|------------|-----------|------------------|--------------|---------------|---------------------|-----------------|----------------|
| `time` | X | X | X | X | X | X | X | X | X | X | X | X |
| `status` | X | X | X | X | X | X | X | X | X | X | X | X |
| `predictors` | | X | | X | X | X | X | X | X | X | X | X |
| `model_type` | | | | X | X | X | X | X | X | X | X | X |
| `smcure_model_type` | | | | X | X | X | | | X | | X | X |
| `cure_link` | | | | X | X | X | | | X | | X | X |
| `survival_dist` | | | | X | X | X | | X | X | | X | X |
| `bootstrap_ci` | | | | | | X | X | | | | | |
| `n_bootstrap` | | | | | | X | X | | | | | |
| `cure_threshold` | | | | X | | X | | | | X | | X |
| `plot_cure_fraction` | | | | | | | X | | | | | |
| `plot_survival` | | | | | | | | X | | | | |
| `goodness_of_fit` | | | | | | | | | X | | | |
| `sensitivity_analysis` | | | | | | | | | | X | | |
| `use_background_mortality` | | X | | X | X | X | X | X | X | X | X | X |
| `background_hazard_var` | | X | | X | X | X | X | X | X | X | X | X |
| `npcure_covariate` | | X | | X | X | X | X | | | | | X |
| `npcure_bandwidth` | | | | X | | X | X | | | | | X |
| `npcure_time_points` | | | | | | X | X | | | | | |

---

## 5. Model Type Behavior Summary

| Aspect | `mixture` (smcure) | `nonmixture` (flexsurvcure) | `cure` (cuRe) | `npcure` (npcure) | `all` |
|--------|-------------------|----------------------------|---------------|-------------------|-------|
| R Package | `smcure` | `flexsurvcure` | `cuRe` | `npcure` | All four |
| Cure fraction source | `$b` coefficients (logistic) | `theta` parameter from `$res.t` | `fit.cure.model()` output | `probcure()` kernel estimate | Each model separately |
| Survival coefficients | `$beta` (PH or AFT) | Parametric dist parameters | Excess hazard coefficients | Not applicable | Each model separately |
| Requires predictors | Optional | Optional | Optional | Requires `npcure_covariate` | Varies by model |
| Requires background hazard | No | No | Optional (`use_background_mortality`) | No | Only for cuRe component |
| Bootstrap method | Built-in (`Var=TRUE`) | Manual resampling | Manual resampling | Not supported | Per-model |
| Parametric distribution | Not applicable (semiparametric) | `survival_dist` (Weibull etc.) | Weibull-based | Nonparametric | Varies |
| AIC/BIC available | Yes | Yes | Yes | No (nonparametric) | Yes where applicable |
| Goodness of fit | LR test vs null | LR test + dist comparison | LR test vs no-cure | Not applicable | Per-model |

---

## 6. Data Validation and Edge Cases

The function performs the following checks before model fitting:

| Check | Condition | Action |
|-------|-----------|--------|
| Events per variable (EPV) | EPV < 10 for the number of predictors | Warning: model may be unstable |
| Extreme prevalence | Event rate < 5% or > 95% | Warning: cure model assumptions may not hold |
| Negative or zero times | Any `time <= 0` | Error: all follow-up times must be positive |
| Non-binary status | `status` has more than 2 unique values | Error: status must be binary (0/1) |
| No events | All `status == 0` | Error: no events observed; cannot fit cure model |
| Insufficient sample | N < 30 or events < 10 | Warning: results may be unreliable |
| Variable names with spaces | Column names contain spaces | Handled via backtick quoting in formulas |
| Missing data | NA values in time, status, or predictors | Complete-case analysis with count reported in summary |
| Convergence failure | Model optimisation does not converge | Error in `errors` Html with suggestion to simplify model |
| Background hazard missing | `use_background_mortality = TRUE` but `background_hazard_var = NULL` | Error: background hazard variable required for cuRe model |

---

## 7. Clinical Interpretation Guide

The interpretation HTML includes:

1. **Cure fraction estimate**: "An estimated X% of patients are statistically cured (95% CI: Y% to Z%)."
2. **Uncured median survival**: "Among uncured patients, the median survival time is M months."
3. **Cure threshold context**: "Based on a cure threshold of T months, patients surviving beyond this point have a probability of cure approaching the estimated cure fraction."
4. **Copy-ready report sentence**: A single sentence suitable for direct inclusion in a manuscript Results section.
5. **Model comparison summary** (when `model_type = "all"`): Ranks models by AIC and highlights the best-fitting model.
6. **Clinical recommendations**: Context-specific guidance on interpreting the cure fraction for patient counselling.

---

## 8. References

1. Berkson J, Gage RP. Survival curve for cancer patients following treatment. *J Am Stat Assoc*. 1952;47(259):501-515.
2. Boag JW. Maximum likelihood estimates of the proportion of patients cured by cancer therapy. *J R Stat Soc Series B*. 1949;11(1):15-53.
3. Cai C, Zou Y, Peng Y, Zhang J. smcure: An R-package for estimating semiparametric mixture cure models. *Comput Methods Programs Biomed*. 2012;108(3):1255-1260.
4. Peng Y, Dear KBG. A nonparametric mixture model for cure rate estimation. *Biometrics*. 2000;56(1):237-243.
5. Andersson TML, Dickman PW, Eloranta S, Lambert PC. Estimating and modelling cure in population-based cancer studies within the framework of flexible parametric survival models. *BMC Med Res Methodol*. 2011;11:96.
6. Lopez-Cheda A, Cao R, Jacome MA, Van Keilegom I. Nonparametric incidence estimation and bootstrap bandwidth selection in mixture cure models. *Comput Stat Data Anal*. 2017;105:144-165.
7. Lambert PC, Thompson JR, Weston CL, Dickman PW. Estimating and modeling the cure fraction in population-based cancer survival analysis. *Biostatistics*. 2007;8(3):576-594.
