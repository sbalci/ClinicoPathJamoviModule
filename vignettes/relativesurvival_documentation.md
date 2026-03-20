# Relative Survival Analysis Documentation

## 1. Overview

- **Function**: `relativesurvival`
- **Files**:
  - `jamovi/relativesurvival.u.yaml` -- UI
  - `jamovi/relativesurvival.a.yaml` -- Options (22 total)
  - `R/relativesurvival.b.R` -- Backend (~720 lines)
  - `jamovi/relativesurvival.r.yaml` -- Results (7 tables, 4 plots, 3 HTML)
- **Menu location**: SurvivalT > Advanced Survival > Population-Based Survival Comparison
- **Summary**: Relative survival analysis compares observed survival in a patient cohort to expected survival in a matched general population. It supports four estimation methods (Pohar-Perme, Ederer I/II, Hakulinen), 15 population rate tables (4 from relsurv, 10 WHO-based, 1 custom), net survival estimation, excess mortality, crude probabilities, ICSS age standardization, period analysis, three regression model types (additive, multiplicative, flexible parametric), and four configurable plots. Designed for cancer registry studies and population-based research where cause-of-death information may be unreliable or unavailable.

---

## 2. Feature Summary

Relative survival is a fundamental measure in population-based cancer epidemiology. Instead of requiring individual cause-of-death data (which is often inaccurate), it estimates disease-specific mortality indirectly by comparing the observed survival of patients with the survival expected in a sex-, age-, and calendar-year-matched general population. The ratio of observed to expected survival -- the relative survival -- is interpreted as the net probability of surviving the disease.

This module targets cancer registries, population health researchers, and clinical epidemiologists who need to produce internationally comparable survival statistics. It handles the full pipeline: data preparation with automatic sex/age/year validation, population matching via rate tables, survival estimation across four standard methods, and optional covariate-adjusted analysis through excess hazard or flexible parametric regression.

The analysis outputs include time-specific survival estimates (observed, expected, relative), net survival with confidence intervals, interval excess mortality rates with delta-method CIs and p-values, crude probability decomposition (disease vs. other-cause death), ICSS age-standardized rates, period-based trend analysis, and regression model coefficients with model fit statistics. Four publication-ready plots are generated: observed survival, expected survival, relative/net survival with confidence bands, and bar charts of excess mortality by year.

---

## 3. Feature Details

### Input Variables

| Feature | YAML Argument (.a.yaml) | UI Label | Results Section (.r.yaml) | R Function (.b.R) |
|---------|------------------------|----------|--------------------------|-------------------|
| Follow-up Time | `time` (Variable, numeric, continuous) | Follow-up Time | survivalTable | `.prepareData` -- converts to years/days based on `time_scale` |
| Vital Status | `status` (Variable, factor/numeric, nominal) | Vital Status | survivalTable | `.prepareData` -- validates binary 0/1 or 2-level factor |
| Age at Diagnosis | `age` (Variable, numeric, continuous) | Age at Diagnosis | survivalTable | `.prepareData` -- validates 0-120, converts to days for ratetable |
| Sex | `sex` (Variable, factor, nominal) | Sex | survivalTable | `.prepareData` -- maps male/female/m/f/erkek/kadin/1/2 etc. to `factor(c("male","female"))` |
| Calendar Year | `year` (Variable, numeric, continuous) | Calendar Year | survivalTable | `.prepareData` -- validates 1900-2100, creates `diagdate` as mid-year Date |
| Additional Covariates | `covariates` (Variables, numeric/factor) | Additional Covariates | regressionTable | `.fitRegressionModel` -- used in regression models only |

### Population Matching

| Feature | YAML Argument (.a.yaml) | UI Label | Results Section (.r.yaml) | R Function (.b.R) |
|---------|------------------------|----------|--------------------------|-------------------|
| Population Rate Table | `ratetable` (List, default: `us`) | Population Rate Table | survivalTable (note) | `.getRateTable` -- dispatches to `relsurv::survexp.us/mn/fr/slopop`, bundled WHO tables, or custom; falls back to US if unavailable |

Available rate tables (15 options):
- **Package-bundled** (relsurv): US (`us`), Minnesota (`mn`), French (`fr`), Slovenian (`slovenia`)
- **WHO-based** (ClinicoPath data): Turkey (`turkey`), Germany (`germany`), UK (`uk`), Italy (`italy`), Japan (`japan`), Spain (`spain`), Brazil (`brazil`), South Korea (`south_korea`), China (`china`), India (`india`)
- **Custom** (`custom`): placeholder; falls back to US if no custom table loaded

### Estimation Settings

| Feature | YAML Argument (.a.yaml) | UI Label | Results Section (.r.yaml) | R Function (.b.R) |
|---------|------------------------|----------|--------------------------|-------------------|
| Estimation Method | `method` (List, default: `poharperme`) | Estimation Method | survivalTable (note) | `.calculateRelativeSurvival` -- maps to `relsurv::rs.surv(method=...)` |
| Time Scale | `time_scale` (List, default: `years`) | Time Scale | survivalTable | `.prepareData` -- divides by 12 (months) or 365.25 (days) to get years |

Estimation method options:
- **Pohar-Perme** (`poharperme`): Unbiased net survival estimator (recommended, EUROCARE standard)
- **Ederer I** (`ederer1`): Expected survival matches cohort demographics at baseline
- **Ederer II** (`ederer2`): Expected survival updated at each event time
- **Hakulinen** (`hakulinen`): Expected survival weighted by censoring distribution

### Analysis Options

| Feature | YAML Argument (.a.yaml) | UI Label | Results Section (.r.yaml) | R Function (.b.R) |
|---------|------------------------|----------|--------------------------|-------------------|
| Net Survival | `net_survival` (Bool, default: `true`) | Net Survival | netSurvivalTable | `.displayNetSurvivalTable` -- extracts surv/SE/CI from `rs.surv` at parsed timepoints |
| Excess Mortality | `excess_mortality` (Bool, default: `true`) | Excess Mortality | excessMortalityTable, excessPlot | `.calculateExcessMortality` -- interval excess hazard = `-log(S_net(t)/S_net(t-1))`, delta-method CIs |
| Crude Probability of Death | `crude_probability` (Bool, default: `true`) | Crude Probability of Death | crudeProbTable | `.calculateCrudeProbabilities` -- calls `relsurv::cmp.rel()` for disease vs. other-cause decomposition |
| Age-Standardized Rates | `age_standardized` (Bool, default: `false`) | Age-Standardized Rates | ageStandardizedTable | `.calculateAgeStandardized` -- ICSS weights (0.07/0.12/0.23/0.29/0.29) across 5 age groups, Pohar-Perme per group |
| Period Analysis | `period_analysis` (Bool, default: `false`) | Period Analysis | periodAnalysisTable | `.calculatePeriodAnalysis` -- 5-year diagnosis periods, Pohar-Perme 5-year RS per period |
| Cohort Definition | `cohort_year` (String, default: `""`) | Cohort Definition | periodAnalysisTable | `.calculatePeriodAnalysis` -- filters data to specified year range (e.g., "2010-2015") before period grouping; enabled when `period_analysis` is true |

### Regression Options

| Feature | YAML Argument (.a.yaml) | UI Label | Results Section (.r.yaml) | R Function (.b.R) |
|---------|------------------------|----------|--------------------------|-------------------|
| Regression Model | `regression_model` (List, default: `none`) | Regression Model | regressionTable, modelFit | `.fitRegressionModel` -- dispatches to `relsurv::rsadd` (additive), `relsurv::rsmul` (multiplicative), or `rstpm2::stpm2` (flexible) |
| Spline Degrees of Freedom | `spline_df` (Integer, default: 4, range: 1-10) | Spline Degrees of Freedom | regressionTable | `.fitRegressionModel` -- passed to `rstpm2::stpm2(df=...)` for flexible parametric models; enabled when `regression_model` is `flexible` |

Regression model options:
- **None** (`none`): No regression
- **Additive (Excess Hazard)** (`additive`): `relsurv::rsadd()` with yearly intervals
- **Multiplicative** (`multiplicative`): `relsurv::rsmul()` with yearly intervals
- **Flexible Parametric** (`flexible`): `rstpm2::stpm2()` with per-individual background hazard from population rate table

### Visualization Options

| Feature | YAML Argument (.a.yaml) | UI Label | Results Section (.r.yaml) | R Function (.b.R) |
|---------|------------------------|----------|--------------------------|-------------------|
| Plot Observed Survival | `plot_observed` (Bool, default: `true`) | Plot Observed Survival | observedPlot (Image, 700x450) | `.plotObserved` -- blue step function (KM), y-axis 0-100% |
| Plot Expected Survival | `plot_expected` (Bool, default: `true`) | Plot Expected Survival | expectedPlot (Image, 700x450) | `.plotExpected` -- green step function (survexp), y-axis 0-100% |
| Plot Relative Survival | `plot_relative` (Bool, default: `true`) | Plot Relative Survival | relativePlot (Image, 700x450) | `.plotRelative` -- red step function with CI ribbon, reference lines at 100% and 50% |
| Plot Excess Mortality | `plot_excess` (Bool, default: `true`) | Plot Excess Mortality | excessPlot (Image, 700x450, visible when `plot_excess && excess_mortality`) | `.plotExcess` -- bar chart of yearly excess hazard |

### Output Settings

| Feature | YAML Argument (.a.yaml) | UI Label | Results Section (.r.yaml) | R Function (.b.R) |
|---------|------------------------|----------|--------------------------|-------------------|
| Confidence Level | `confidence_level` (Number, default: 0.95, range: 0.50-0.99) | Confidence Level | survivalTable, netSurvivalTable, excessMortalityTable, crudeProbTable, ageStandardizedTable | used in `rs.surv(conf.int=...)`, `cmp.rel(conf.int=...)`, and all z-critical calculations |
| Time Points for Estimates | `timepoints` (String, default: `"1,3,5,10"`) | Time Points for Estimates | survivalTable, netSurvivalTable, crudeProbTable, ageStandardizedTable | `.parseTimepoints` -- splits on comma/semicolon/whitespace, filters to positive numbers within data range |

---

## 4. Results Outputs

### HTML Outputs

| Name | Title | Description |
|------|-------|-------------|
| `todo` | To Do | Welcome message shown when required variables are not yet selected; clears once all 5 required variables are assigned |
| `notices` | Notices | Color-coded notices (error/strong_warning/warning/info) rendered as styled HTML divs; includes event count warnings, EPV checks, rate table fallback notices, and completion message |
| `summary` | Analysis Summary | Method, N patients, N deaths, median follow-up, rate table label, confidence level |
| `interpretation` | Clinical Interpretation | 5-year net survival interpretation with prognosis category (Excellent/Good/Fair/Poor), key concepts explainer |

### Tables

| Name | Title | Visible | Columns |
|------|-------|---------|---------|
| `survivalTable` | Survival Estimates by Time | Always | Time, Observed Survival, Expected Survival, Relative Survival, RS CI Lower, RS CI Upper |
| `netSurvivalTable` | Net Survival Estimates | `net_survival` | Time, Net Survival, CI Lower, CI Upper, Std. Error |
| `excessMortalityTable` | Excess Mortality Rates | `excess_mortality` | Time Interval, Excess Hazard, CI Lower, CI Upper, p-value |
| `crudeProbTable` | Crude Probability of Death | `crude_probability` | Time, Disease Death, Other Causes Death, Disease CI Lower, Disease CI Upper |
| `ageStandardizedTable` | Age-Standardized Survival | `age_standardized` | Time, Crude Rate, Age-Adjusted Rate, CI Lower, CI Upper |
| `periodAnalysisTable` | Period Analysis Results | `period_analysis` | Period, N Patients, 5-Year RS, CI Lower, CI Upper |
| `regressionTable` | Regression Model Results | `regression_model != none` | Variable, Coefficient, Std. Error, z-value, p-value, CI Lower, CI Upper |
| `modelFit` | Model Fit Statistics | `regression_model != none` | Metric, Value, Interpretation |

### Plots (Images)

| Name | Title | Visible | Render Function | Dimensions |
|------|-------|---------|-----------------|------------|
| `observedPlot` | Observed Survival | `plot_observed` | `.plotObserved` | 700 x 450 |
| `expectedPlot` | Expected Survival | `plot_expected` | `.plotExpected` | 700 x 450 |
| `relativePlot` | Relative Survival | `plot_relative` | `.plotRelative` | 700 x 450 |
| `excessPlot` | Excess Mortality | `plot_excess && excess_mortality` | `.plotExcess` | 700 x 450 |

---

## 5. Data Validation and Guards

| Check | Location | Behavior |
|-------|----------|----------|
| Missing required variables | `.init`, `.run` | Shows welcome message / returns early |
| `relsurv` package available | `.run` | `jmvcore::reject()` with install instructions |
| `survival` package available | `.run` | `jmvcore::reject()` with install instructions |
| < 30 complete cases | `.prepareData` | `jmvcore::reject()` |
| < 10 events | `.run` | `jmvcore::reject()` |
| 10-19 events | `.run` | strong_warning notice |
| 20-49 events | `.run` | warning notice |
| EPV < 10 (regression) | `.run` | strong_warning notice |
| Non-positive follow-up times | `.prepareData` | Removed, table note added |
| Status not binary | `.prepareData` | `jmvcore::reject()` |
| Age outside 0-120 | `.prepareData` | `jmvcore::reject()` |
| Unmapped sex values | `.prepareData` | `jmvcore::reject()` with unrecognized values listed |
| Year outside 1900-2100 | `.prepareData` | `jmvcore::reject()` |
| Rate table unavailable | `.getRateTable` | Falls back to US table with note |
| `rstpm2` not installed | `.fitRegressionModel` | `jmvcore::reject()` for flexible model |
| < 2 age groups | `.calculateAgeStandardized` | Table note, returns early |
| < 30 obs after cohort filter | `.calculatePeriodAnalysis` | Table note, returns early |
| Period group < 10 obs | `.calculatePeriodAnalysis` | Skipped |
| Age group < 5 obs | `.calculateAgeStandardized` | Skipped |

---

## 6. Internal Method Map

| Private Method | Called By | Purpose |
|----------------|----------|---------|
| `.prepareData()` | `.run()` | Data extraction, type coercion, time/status/age/sex/year validation |
| `.getRateTable()` | `.run()` | Rate table dispatch (relsurv or WHO collection) |
| `.calculateRelativeSurvival()` | `.run()` | Core `relsurv::rs.surv()` call |
| `.calculateObservedSurvival()` | `.run()` | `survival::survfit()` for KM curve |
| `.calculateExpectedSurvival()` | `.run()` | `survival::survexp()` for population survival |
| `.displaySurvivalTable()` | `.run()` | Main survival table at parsed timepoints |
| `.displayNetSurvivalTable()` | `.run()` | Net survival table from `rs.surv` output |
| `.calculateExcessMortality()` | `.run()` | Interval excess hazard with delta-method CIs |
| `.calculateCrudeProbabilities()` | `.run()` | `relsurv::cmp.rel()` for cause decomposition |
| `.calculateAgeStandardized()` | `.run()` | ICSS-weighted Pohar-Perme by age group |
| `.calculatePeriodAnalysis()` | `.run()` | 5-year period grouping with per-period RS |
| `.fitRegressionModel()` | `.run()` | Regression dispatch (additive/multiplicative/flexible) |
| `.displayRegressionResults()` | `.fitRegressionModel()` | Extract and display coefficients + fit stats |
| `.generatePlotData()` | `.run()` | Build serializable data.frame for 3 survival plots |
| `.computeExpectedHazard()` | `.fitRegressionModel()` | Per-individual background hazard for stpm2 bhazard |
| `.plotObserved()` | render callback | ggplot2 step plot of KM survival |
| `.plotExpected()` | render callback | ggplot2 step plot of expected survival |
| `.plotRelative()` | render callback | ggplot2 step plot + CI ribbon of net survival |
| `.plotExcess()` | render callback | ggplot2 bar chart of yearly excess hazard |
| `.displaySummary()` | `.run()` | HTML summary of analysis parameters |
| `.displayInterpretation()` | `.run()` | Clinical interpretation with prognosis grading |
| `.stepLookup()` | table/plot methods | Left-continuous step-function value lookup |
| `.stepIdx()` | table/plot methods | Left-continuous step-function index lookup |
| `.parseTimepoints()` | table methods | Parse comma/semicolon/whitespace-separated timepoint string |
| `.methodLabel()` | display/plot methods | Human-readable estimation method name |
| `.addNotice()` | various | Append notice to internal list |
| `.renderNotices()` | `.run()` | Convert notice list to styled HTML output |

---

## 7. Dependencies

| Package | Usage | Required |
|---------|-------|----------|
| `relsurv` | `rs.surv()`, `cmp.rel()`, `rsadd()`, `rsmul()`, `survexp.us/mn/fr`, `slopop` | Yes |
| `survival` | `survfit()`, `survexp()`, `Surv()` | Yes |
| `rstpm2` | `stpm2()` for flexible parametric models | Only when `regression_model = flexible` |
| `jmvcore` | Data handling, reject, composeTerm | Yes (jamovi framework) |
| `ggplot2` | All four plots | Yes |
| `scales` | `percent_format()` in plot axes | Yes |

---

## 8. References

- Pohar Perme M, Stare J, Esteve J. On Estimation in Relative Survival. *Biometrics*, 2012;68:113-120.
- Ederer F, Axtell LM, Cutler SJ. The Relative Survival Rate: A Statistical Methodology. *NCI Monograph*, 1961;6:101-121.
- Hakulinen T. Cancer Survival Corrected for Heterogeneity in Patient Withdrawal. *Biometrics*, 1982;38:933-942.
- Corazziari I, Quinn M, Capocaccia R. Standard cancer patient population for age standardising survival ratios. *Eur J Cancer*, 2004;40:2307-2316. (ICSS weights)
- Dickman PW, Coviello E. Estimating and Modeling Relative Survival. *The Stata Journal*, 2015;15(1):186-215.
