# Testing Relative Survival Analysis

All test datasets: `data/relativesurvival_test.rda` (200 obs, ~116 events)
Generation script: `data-raw/create_relativesurvival_test_data.R`
Existing test file: `tests/testthat/test-relativesurvival.R`

**Dataset columns:**

| Column | Type | Range / Values |
|--------|------|----------------|
| `patient_id` | integer | 1-200 |
| `followup_years` | numeric | 0.08-10.00 |
| `vital_status` | integer | 0 (censored), 1 (dead) |
| `age_at_diagnosis` | integer | 30-90 |
| `sex` | factor | "male", "female" |
| `diagnosis_year` | integer | 2000-2015 |
| `cancer_site` | factor | "Colon", "Breast", "Lung", "Prostate" |
| `stage` | ordered factor | "I", "II", "III", "IV" |
| `grade` | factor | "Well differentiated", "Moderately differentiated", "Poorly differentiated" |
| `comorbidity` | integer | 0-3 |
| `tumor_size` | numeric | 0.5-10+ (continuous, cm) |

---

## 1. BASIC FUNCTIONALITY

| # | Test Description | Variables | Options to Test | Expected Outcome |
|---|-----------------|-----------|-----------------|------------------|
| 1.1 | Minimal required variables only | `time=followup_years`, `status=vital_status`, `age=age_at_diagnosis`, `sex=sex`, `year=diagnosis_year` | All defaults | Returns `jmvcoreClass`; `summary`, `survivalTable`, `interpretation`, `notices` all non-null |
| 1.2 | Welcome message when no variables selected | (none) | All defaults | `todo` HTML contains "Welcome to Relative Survival Analysis"; no error thrown |
| 1.3 | Welcome message with partial variables | `time=followup_years` only (missing `status`, `age`, `sex`, `year`) | All defaults | `todo` HTML rendered; `.run()` returns early |
| 1.4 | All five required + all optional analyses enabled | `time=followup_years`, `status=vital_status`, `age=age_at_diagnosis`, `sex=sex`, `year=diagnosis_year` | `net_survival=TRUE`, `excess_mortality=TRUE`, `crude_probability=TRUE`, `age_standardized=TRUE`, `period_analysis=TRUE` | All tables populated; no errors |
| 1.5 | Summary HTML content correctness | All required vars | Defaults | Summary contains: patient count, death count, median follow-up, method name, rate table label, confidence level |
| 1.6 | Interpretation HTML content | All required vars | Defaults | Interpretation contains "Clinical Interpretation", prognosis category text |
| 1.7 | survivalTable has correct column names | All required vars | Defaults | Columns: `time_point`, `observed`, `expected`, `relative`, `rel_ci_lower`, `rel_ci_upper` |
| 1.8 | survivalTable row count matches requested timepoints | All required vars | `timepoints="1,3,5"` | Table has 3 rows (or fewer if max follow-up < timepoint) |

---

## 2. ESTIMATION METHODS (4 methods)

| # | Test Description | Variables | Options to Test | Expected Outcome |
|---|-----------------|-----------|-----------------|------------------|
| 2.1 | Pohar-Perme method (default) | All required vars | `method="poharperme"` | Runs without error; method note says "Pohar-Perme (Net Survival)" |
| 2.2 | Ederer I method | All required vars | `method="ederer1"` | Runs without error; method note says "Ederer I" |
| 2.3 | Ederer II method | All required vars | `method="ederer2"` | Runs without error; method note says "Ederer II" |
| 2.4 | Hakulinen method | All required vars | `method="hakulinen"` | Runs without error; method note says "Hakulinen" |
| 2.5 | Different methods produce different RS estimates | All required vars | Compare all 4 methods at 5-year timepoint | Relative survival values differ between methods (at least between Pohar-Perme and Ederer I) |
| 2.6 | Method label in plot title | All required vars | `method="ederer2"`, `plot_relative=TRUE` | Relative survival plot title contains "Ederer II" |

---

## 3. RATE TABLES (15 countries)

| # | Test Description | Variables | Options to Test | Expected Outcome |
|---|-----------------|-----------|-----------------|------------------|
| 3.1 | US Population (default, survexp.us) | All required vars | `ratetable="us"` | Runs; summary says "US Population (survexp.us)" |
| 3.2 | Minnesota Population (survexp.mn) | All required vars | `ratetable="mn"` | Runs without error |
| 3.3 | French Population (survexp.fr) | All required vars | `ratetable="fr"` | Runs without error |
| 3.4 | Slovenian Population (slopop) | All required vars | `ratetable="slovenia"` | Runs without error |
| 3.5 | Turkey Population (WHO) | All required vars | `ratetable="turkey"` | Runs; loads from `ratetable_turkey` or `ratetable_who_collection` |
| 3.6 | German Population (WHO) | All required vars | `ratetable="germany"` | Runs; loads from `ratetable_who_collection` |
| 3.7 | UK Population (WHO) | All required vars | `ratetable="uk"` | Runs without error |
| 3.8 | Italian Population (WHO) | All required vars | `ratetable="italy"` | Runs without error |
| 3.9 | Japanese Population (WHO) | All required vars | `ratetable="japan"` | Runs without error |
| 3.10 | Spanish Population (WHO) | All required vars | `ratetable="spain"` | Runs without error |
| 3.11 | Brazilian Population (WHO) | All required vars | `ratetable="brazil"` | Runs without error |
| 3.12 | South Korean Population (WHO) | All required vars | `ratetable="south_korea"` | Runs without error |
| 3.13 | Chinese Population (WHO) | All required vars | `ratetable="china"` | Runs without error |
| 3.14 | Indian Population (WHO) | All required vars | `ratetable="india"` | Runs without error |
| 3.15 | Custom rate table (fallback) | All required vars | `ratetable="custom"` | Falls back to survexp.us with note "ratetable_fallback" |
| 3.16 | WHO rate table unavailable graceful fallback | All required vars | `ratetable="germany"` (when WHO collection not installed) | Falls back to survexp.us with fallback note |

---

## 4. OPTIONAL ANALYSES

### 4A. Net Survival

| # | Test Description | Variables | Options to Test | Expected Outcome |
|---|-----------------|-----------|-----------------|------------------|
| 4A.1 | Net survival table enabled (default) | All required vars | `net_survival=TRUE` | `netSurvivalTable` populated with rows for each timepoint |
| 4A.2 | Net survival table disabled | All required vars | `net_survival=FALSE` | `netSurvivalTable` not visible (visibility rule: `net_survival`) |
| 4A.3 | Net survival values between 0 and 1+ | All required vars | `net_survival=TRUE` | All `net_survival` values > 0; CIs make sense (lower < estimate < upper) |
| 4A.4 | Standard error present | All required vars | `net_survival=TRUE`, `method="poharperme"` | `standard_error` column has non-NA values |

### 4B. Excess Mortality

| # | Test Description | Variables | Options to Test | Expected Outcome |
|---|-----------------|-----------|-----------------|------------------|
| 4B.1 | Excess mortality table enabled (default) | All required vars | `excess_mortality=TRUE` | `excessMortalityTable` populated; interval labels like "0-1 years" |
| 4B.2 | Excess mortality disabled | All required vars | `excess_mortality=FALSE` | `excessMortalityTable` not visible |
| 4B.3 | Excess hazard values plausible | All required vars | `excess_mortality=TRUE` | `excess_hazard` values are finite numbers; yearly intervals up to min(max_followup, 10) |
| 4B.4 | CIs and p-values present when SE available | All required vars | `excess_mortality=TRUE`, `method="poharperme"` | `hazard_ci_lower`, `hazard_ci_upper`, `p_value` columns have values |
| 4B.5 | Short follow-up (< 1 year) | Modified data: all `followup_years` < 0.5 | `excess_mortality=TRUE` | Table note: "Follow-up too short for yearly excess mortality estimates." |

### 4C. Crude Probability of Death

| # | Test Description | Variables | Options to Test | Expected Outcome |
|---|-----------------|-----------|-----------------|------------------|
| 4C.1 | Crude probability enabled (default) | All required vars | `crude_probability=TRUE` | `crudeProbTable` populated; `disease_death` + `other_death` values present |
| 4C.2 | Crude probability disabled | All required vars | `crude_probability=FALSE` | `crudeProbTable` not visible |
| 4C.3 | Disease + other death probabilities sum <= 1 | All required vars | `crude_probability=TRUE` | At each timepoint: `disease_death` + `other_death` <= 1.0 (approximately) |
| 4C.4 | Disease CIs present | All required vars | `crude_probability=TRUE` | `disease_ci_lower` and `disease_ci_upper` have non-NA values |

### 4D. Age Standardization

| # | Test Description | Variables | Options to Test | Expected Outcome |
|---|-----------------|-----------|-----------------|------------------|
| 4D.1 | Age standardization with sufficient groups | All required vars (n=200, ages 30-90) | `age_standardized=TRUE` | `ageStandardizedTable` populated; uses ICSS weights |
| 4D.2 | Age standardization with narrow age range (< 2 groups) | Modified data: all ages 50-54 | `age_standardized=TRUE` | Table note: "Age standardization requires at least 2 age groups" |
| 4D.3 | Age-adjusted vs crude rate differs | All required vars | `age_standardized=TRUE` | `crude_rate` and `age_adjusted` columns have different values (usually) |
| 4D.4 | Age-adjusted CIs present | All required vars | `age_standardized=TRUE` | `adj_ci_lower` and `adj_ci_upper` are populated |
| 4D.5 | ICSS weight note present | All required vars | `age_standardized=TRUE` | Table note mentions ICSS weights: 0.07, 0.12, 0.23, 0.29, 0.29 |

### 4E. Period Analysis

| # | Test Description | Variables | Options to Test | Expected Outcome |
|---|-----------------|-----------|-----------------|------------------|
| 4E.1 | Period analysis with default cohort | All required vars (years 2000-2015) | `period_analysis=TRUE` | `periodAnalysisTable` populated with 5-year period bins |
| 4E.2 | Period analysis disabled | All required vars | `period_analysis=FALSE` | `periodAnalysisTable` not visible |
| 4E.3 | Period analysis with cohort year filter | All required vars | `period_analysis=TRUE`, `cohort_year="2005-2010"` | Only patients diagnosed 2005-2010 included; table rows reflect filtered periods |
| 4E.4 | Period analysis with too-restrictive cohort filter (< 30 patients) | All required vars | `period_analysis=TRUE`, `cohort_year="2015-2015"` | Table note: "Insufficient data for period analysis after cohort filtering" |
| 4E.5 | Period table columns correct | All required vars | `period_analysis=TRUE` | Columns: `period`, `n_patients`, `rel_survival_5y`, `rs_ci_lower`, `rs_ci_upper` |
| 4E.6 | Period bins skip groups with < 10 patients | All required vars, sparse year range | `period_analysis=TRUE` | Period groups with n < 10 are omitted from table |

---

## 5. REGRESSION MODELS

### 5A. No Regression (default)

| # | Test Description | Variables | Options to Test | Expected Outcome |
|---|-----------------|-----------|-----------------|------------------|
| 5A.1 | No regression model (default) | All required vars | `regression_model="none"` | `regressionTable` and `modelFit` not visible |

### 5B. Additive Excess Hazard Model

| # | Test Description | Variables | Options to Test | Expected Outcome |
|---|-----------------|-----------|-----------------|------------------|
| 5B.1 | Additive model with single factor covariate | All required + `covariates=["stage"]` | `regression_model="additive"` | `regressionTable` populated; model note says "Additive Excess Hazard" |
| 5B.2 | Additive model with continuous covariate | All required + `covariates=["tumor_size"]` | `regression_model="additive"` | Runs; coefficient for `tumor_size` present |
| 5B.3 | Additive model with multiple covariates | All required + `covariates=["stage", "tumor_size"]` | `regression_model="additive"` | Multiple coefficient rows in regression table |
| 5B.4 | Additive model without covariates | All required, no `covariates` | `regression_model="additive"` | Regression table note: "require at least one covariate" |

### 5C. Multiplicative Model

| # | Test Description | Variables | Options to Test | Expected Outcome |
|---|-----------------|-----------|-----------------|------------------|
| 5C.1 | Multiplicative model with covariate | All required + `covariates=["stage"]` | `regression_model="multiplicative"` | `regressionTable` populated; model note says "Multiplicative" |
| 5C.2 | Multiplicative model with continuous covariate | All required + `covariates=["tumor_size"]` | `regression_model="multiplicative"` | Coefficient for tumor_size present |

### 5D. Flexible Parametric Model

| # | Test Description | Variables | Options to Test | Expected Outcome |
|---|-----------------|-----------|-----------------|------------------|
| 5D.1 | Flexible model with default spline_df | All required + `covariates=["stage"]` | `regression_model="flexible"`, `spline_df=4` | Requires `rstpm2`; runs if available; model note says "Flexible Parametric" |
| 5D.2 | Flexible model with spline_df=1 (minimum) | All required + `covariates=["stage"]` | `regression_model="flexible"`, `spline_df=1` | Runs with minimal spline complexity |
| 5D.3 | Flexible model with spline_df=10 (maximum) | All required + `covariates=["stage"]` | `regression_model="flexible"`, `spline_df=10` | Runs or gracefully handles convergence issues |
| 5D.4 | Flexible model without rstpm2 installed | All required + `covariates=["stage"]` | `regression_model="flexible"` (rstpm2 absent) | Error: "rstpm2 package is required" |

### 5E. Model Fit Statistics

| # | Test Description | Variables | Options to Test | Expected Outcome |
|---|-----------------|-----------|-----------------|------------------|
| 5E.1 | Model fit table populated | All required + `covariates=["stage"]` | `regression_model="additive"` | `modelFit` table has rows for Log-Likelihood and/or AIC |
| 5E.2 | AIC interpretation | All required + `covariates=["stage"]` | `regression_model="additive"` | AIC row has interpretation "Lower is better" |

### 5F. EPV Check

| # | Test Description | Variables | Options to Test | Expected Outcome |
|---|-----------------|-----------|-----------------|------------------|
| 5F.1 | Low EPV warning (many covariates, few events) | Modified data: few events + `covariates=["stage","grade","cancer_site","comorbidity","tumor_size"]` | `regression_model="additive"` | Notice: "Low Events Per Variable" with EPV value |

---

## 6. TIME SCALES (3 scales)

| # | Test Description | Variables | Options to Test | Expected Outcome |
|---|-----------------|-----------|-----------------|------------------|
| 6.1 | Years (default) | `time=followup_years` | `time_scale="years"` | Time column in survivalTable shows years (1, 3, 5, 10) |
| 6.2 | Months | Derived: `followup_months = followup_years * 12` | `time_scale="months"`, `timepoints="12,36,60"` | Conversion: months / 12 = years internally; analysis runs |
| 6.3 | Days | Derived: `followup_days = round(followup_years * 365.25)` | `time_scale="days"`, `timepoints="365,1096,1826"` | Conversion: days / 365.25 = years internally; analysis runs |
| 6.4 | Time scale affects internal conversion only | Compare years vs months results | Same data, different scales | Relative survival estimates should be approximately equal |

---

## 7. PLOT OPTIONS (4 plots)

| # | Test Description | Variables | Options to Test | Expected Outcome |
|---|-----------------|-----------|-----------------|------------------|
| 7.1 | Observed survival plot enabled (default) | All required vars | `plot_observed=TRUE` | `observedPlot` rendered; returns TRUE |
| 7.2 | Observed survival plot disabled | All required vars | `plot_observed=FALSE` | `observedPlot` not visible |
| 7.3 | Expected survival plot enabled (default) | All required vars | `plot_expected=TRUE` | `expectedPlot` rendered |
| 7.4 | Expected survival plot disabled | All required vars | `plot_expected=FALSE` | `expectedPlot` not visible |
| 7.5 | Relative survival plot enabled (default) | All required vars | `plot_relative=TRUE` | `relativePlot` rendered with CI ribbon |
| 7.6 | Relative survival plot disabled | All required vars | `plot_relative=FALSE` | `relativePlot` not visible |
| 7.7 | Excess mortality plot enabled (default) | All required vars | `plot_excess=TRUE`, `excess_mortality=TRUE` | `excessPlot` rendered as bar chart |
| 7.8 | Excess mortality plot disabled | All required vars | `plot_excess=FALSE` | `excessPlot` not visible |
| 7.9 | Excess plot requires excess_mortality enabled | All required vars | `plot_excess=TRUE`, `excess_mortality=FALSE` | `excessPlot` not visible (visibility: `plot_excess && excess_mortality`) |
| 7.10 | Plot state is serializable (data.frame) | All required vars | All plots enabled | No protobuf serialization errors; all setState uses `as.data.frame()` |

---

## 8. CONFIDENCE LEVELS

| # | Test Description | Variables | Options to Test | Expected Outcome |
|---|-----------------|-----------|-----------------|------------------|
| 8.1 | 95% confidence (default) | All required vars | `confidence_level=0.95` | CIs in survivalTable, netSurvivalTable are 95% width |
| 8.2 | 90% confidence | All required vars | `confidence_level=0.90` | CIs narrower than 95%; summary says "90%" |
| 8.3 | 99% confidence | All required vars | `confidence_level=0.99` | CIs wider than 95% |
| 8.4 | Minimum confidence (0.50) | All required vars | `confidence_level=0.50` | Runs; very narrow CIs |
| 8.5 | Maximum confidence (0.99) | All required vars | `confidence_level=0.99` | Runs; very wide CIs |
| 8.6 | CI width ordering | All required vars | Compare 0.90 vs 0.95 at same timepoint | 95% CI is wider than 90% CI (upper - lower difference) |
| 8.7 | Confidence level propagates to all tables | All required vars | `confidence_level=0.90`, all analyses enabled | Excess mortality CIs, crude probability CIs, age-standardized CIs, regression CIs all use 90% level |

---

## 9. SEX VALUE MAPPING

| # | Test Description | Variables | Options to Test | Expected Outcome |
|---|-----------------|-----------|-----------------|------------------|
| 9.1 | Standard "male"/"female" coding | Default test data | Defaults | Maps correctly; runs without error |
| 9.2 | Abbreviated "m"/"f" coding | Recode: `sex = ifelse(sex=="male","m","f")` | Defaults | Maps to male/female; runs |
| 9.3 | Turkish "erkek"/"kadin" coding | Recode: `sex = ifelse(sex=="male","erkek","kadin")` | Defaults | Maps to male/female; runs |
| 9.4 | Numeric "1"/"2" coding | Recode: `sex = ifelse(sex=="male","1","2")` (as factor) | Defaults | Maps 1->male, 2->female; runs |
| 9.5 | "man"/"woman" coding | Recode: `sex = ifelse(sex=="male","man","woman")` | Defaults | Maps correctly; runs |
| 9.6 | French "homme"/"femme" coding | Recode: `sex = ifelse(sex=="male","homme","femme")` | Defaults | Maps correctly; runs |
| 9.7 | Spanish "masculino"/"femenino" coding | Recode: `sex = ifelse(sex=="male","masculino","femenino")` | Defaults | Maps correctly; runs |
| 9.8 | Mixed case "Male"/"Female" | Recode: `sex = ifelse(sex=="male","Male","Female")` | Defaults | `tolower()` normalizes; maps correctly |
| 9.9 | Upper case "MALE"/"FEMALE" | Recode: `sex = ifelse(sex=="male","MALE","FEMALE")` | Defaults | `tolower()` normalizes; maps correctly |

---

## 10. ERROR HANDLING / EDGE CASES

### 10A. Insufficient Data

| # | Test Description | Data Modification | Expected Error / Behavior |
|---|-----------------|-------------------|--------------------------|
| 10A.1 | Fewer than 30 observations | Subset to n=15 | `reject`: "Insufficient data...minimum of 30" |
| 10A.2 | Exactly 30 observations | Subset to n=30 | Runs (boundary case); may produce unstable estimates |
| 10A.3 | Exactly 29 observations | Subset to n=29 | `reject`: "Insufficient data...minimum of 30" |

### 10B. Too Few Events

| # | Test Description | Data Modification | Expected Error / Behavior |
|---|-----------------|-------------------|--------------------------|
| 10B.1 | Zero events (all censored) | Set all `vital_status=0` | `reject`: "Only 0 events...at least 10 events" |
| 10B.2 | 5 events (< 10 minimum) | Set `vital_status=0` except first 5 | `reject`: "Only 5 events...at least 10 events" |
| 10B.3 | 10 events (boundary) | Set `vital_status=0` except first 10 | Runs; notice: "Low Event Count" (10 < 20 threshold) |
| 10B.4 | 15 events (10-20 range) | Set `vital_status=0` except first 15 | Runs; strong_warning notice about low event count |
| 10B.5 | 25 events (20-50 range) | Set `vital_status=0` except first 25 | Runs; warning notice about moderate event count |
| 10B.6 | 50+ events (adequate) | Default test data (~116 events) | No event count warnings |

### 10C. Invalid Status Values

| # | Test Description | Data Modification | Expected Error / Behavior |
|---|-----------------|-------------------|--------------------------|
| 10C.1 | Status contains value 2 | Set `vital_status[1:5] = 2` | `reject`: "must contain only 0...and 1" |
| 10C.2 | Status contains negative values | Set `vital_status[1:5] = -1` | `reject`: "must contain only 0...and 1" |
| 10C.3 | Status as factor with 2 levels | Convert `vital_status` to factor("alive","dead") | Runs; note about which level is event |
| 10C.4 | Status as factor with 3+ levels | Convert to factor("alive","dead","unknown") | `reject`: "must be a binary variable...Found 3 levels" |

### 10D. Invalid Sex Values

| # | Test Description | Data Modification | Expected Error / Behavior |
|---|-----------------|-------------------|--------------------------|
| 10D.1 | Unmappable sex values ("X"/"Y") | Set `sex = factor(sample(c("X","Y")))` | `reject`: "Could not map sex values...Unrecognized values: x, y" |
| 10D.2 | Mixed valid/invalid sex values | Set `sex[1:10] = "unknown"` | `reject`: "Unrecognized values: unknown (10 observations)" |
| 10D.3 | Single-sex data (all male) | Set all `sex = "male"` | Runs; all matched as male in ratetable |
| 10D.4 | NA values in sex | Set `sex[1:20] = NA` | `naOmit()` removes 20 rows; runs with 180 obs |

### 10E. Invalid Age Values

| # | Test Description | Data Modification | Expected Error / Behavior |
|---|-----------------|-------------------|--------------------------|
| 10E.1 | Negative age | Set `age_at_diagnosis[1] = -5` | `reject`: "Age values must be between 0 and 120 years" |
| 10E.2 | Age > 120 | Set `age_at_diagnosis[1] = 130` | `reject`: "Age values must be between 0 and 120 years" |
| 10E.3 | Age exactly 0 | Set `age_at_diagnosis[1] = 0` | Runs (edge case, 0 is valid) |
| 10E.4 | Age exactly 120 | Set `age_at_diagnosis[1] = 120` | Runs (edge case, 120 is valid) |
| 10E.5 | NA values in age | Set `age_at_diagnosis[1:20] = NA` | `naOmit()` removes rows; runs with remaining data |

### 10F. Invalid Calendar Year Values

| # | Test Description | Data Modification | Expected Error / Behavior |
|---|-----------------|-------------------|--------------------------|
| 10F.1 | Year before 1900 | Set `diagnosis_year[1] = 1800` | `reject`: "Calendar year values appear invalid. Expected years between 1900-2100" |
| 10F.2 | Year after 2100 | Set `diagnosis_year[1] = 2200` | `reject`: "Calendar year values appear invalid" |
| 10F.3 | Year exactly 1900 | Set `diagnosis_year[1] = 1900` | Runs (boundary); may fail if rate table does not cover 1900 |
| 10F.4 | Future year (2025) | Set `diagnosis_year[1] = 2025` | Runs; rate table coverage determines success |

### 10G. Non-Positive Follow-up Times

| # | Test Description | Data Modification | Expected Error / Behavior |
|---|-----------------|-------------------|--------------------------|
| 10G.1 | Zero follow-up time | Set `followup_years[1:5] = 0` | Rows with time <= 0 removed; table note: "non-positive follow-up times were removed" |
| 10G.2 | Negative follow-up time | Set `followup_years[1:5] = -1` | Rows removed; note about removal |
| 10G.3 | All zero follow-up times | Set all `followup_years = 0` | All rows removed; then insufficient data (< 30) error |

### 10H. Missing Package Dependencies

| # | Test Description | Condition | Expected Error / Behavior |
|---|-----------------|-----------|--------------------------|
| 10H.1 | relsurv not installed | `requireNamespace('relsurv')` returns FALSE | `reject`: "relsurv package is required" |
| 10H.2 | survival not installed | `requireNamespace('survival')` returns FALSE | `reject`: "survival package is required" |
| 10H.3 | rstpm2 not installed (flexible model only) | `regression_model="flexible"`, rstpm2 absent | `reject`: "rstpm2 package is required" |

### 10I. Missing Values (NA Handling)

| # | Test Description | Data Modification | Expected Error / Behavior |
|---|-----------------|-------------------|--------------------------|
| 10I.1 | NA in time variable | Set `followup_years[1:10] = NA` | `naOmit()` removes 10 rows; runs with 190 obs |
| 10I.2 | NA in all required variables | Scatter NAs across all 5 required vars | `naOmit()` removes all incomplete cases; check threshold |
| 10I.3 | NA in covariates | Set `stage[1:10] = NA` with `covariates=["stage"]` | `naOmit()` includes covariate columns; removes 10 rows |

---

## 11. CUSTOM TIMEPOINTS

| # | Test Description | Variables | Options to Test | Expected Outcome |
|---|-----------------|-----------|-----------------|------------------|
| 11.1 | Default timepoints "1,3,5,10" | All required vars | `timepoints="1,3,5,10"` | survivalTable has up to 4 rows at years 1, 3, 5, 10 |
| 11.2 | Custom timepoints "1,2,3,5" | All required vars | `timepoints="1,2,3,5"` | 4 rows at years 1, 2, 3, 5 |
| 11.3 | Single timepoint "5" | All required vars | `timepoints="5"` | 1 row at year 5 |
| 11.4 | Many timepoints "1,2,3,4,5,6,7,8,9,10" | All required vars | `timepoints="1,2,3,4,5,6,7,8,9,10"` | Up to 10 rows (pruned if beyond max follow-up) |
| 11.5 | Timepoint beyond max follow-up | All required vars (max ~10 years) | `timepoints="1,5,15,20"` | 15 and 20 dropped (> max_time * 1.1); only 1 and 5 shown |
| 11.6 | Semicolon-separated timepoints | All required vars | `timepoints="1;3;5"` | Parsed correctly (regex splits on `;`) |
| 11.7 | Space-separated timepoints | All required vars | `timepoints="1 3 5"` | Parsed correctly (regex splits on whitespace) |
| 11.8 | Empty timepoints string | All required vars | `timepoints=""` | Falls back to default c(1, 3, 5, 10) |
| 11.9 | Non-numeric timepoints | All required vars | `timepoints="one,three,five"` | Parsed as empty; falls back to default c(1, 3, 5, 10) |
| 11.10 | Decimal timepoints | All required vars | `timepoints="0.5,1.5,2.5"` | 3 rows at 0.5, 1.5, 2.5 years |
| 11.11 | Negative timepoints in string | All required vars | `timepoints="-1,1,3,5"` | -1 filtered out (nums > 0 check); rows at 1, 3, 5 |
| 11.12 | Duplicate timepoints | All required vars | `timepoints="1,3,3,5"` | Deduplication via `unique()`; 3 rows at 1, 3, 5 |

---

## 12. COHORT YEAR OPTION

| # | Test Description | Variables | Options to Test | Expected Outcome |
|---|-----------------|-----------|-----------------|------------------|
| 12.1 | Empty cohort_year (default) | All required vars | `cohort_year=""`, `period_analysis=TRUE` | All diagnosis years included |
| 12.2 | Cohort year range "2005-2010" | All required vars | `cohort_year="2005-2010"`, `period_analysis=TRUE` | Only years 2005-2010 in period analysis |
| 12.3 | Cohort year with comma separator | All required vars | `cohort_year="2005,2010"`, `period_analysis=TRUE` | Parsed as range 2005-2010 |
| 12.4 | Cohort year with semicolon | All required vars | `cohort_year="2005;2010"`, `period_analysis=TRUE` | Parsed as range 2005-2010 |
| 12.5 | Invalid cohort year string | All required vars | `cohort_year="invalid"`, `period_analysis=TRUE` | Parsing yields no numbers; no filtering applied |

---

## 13. SPLINE DEGREES OF FREEDOM

| # | Test Description | Variables | Options to Test | Expected Outcome |
|---|-----------------|-----------|-----------------|------------------|
| 13.1 | Default spline_df=4 | All required + `covariates=["stage"]` | `regression_model="flexible"`, `spline_df=4` | Flexible model runs with 4 df |
| 13.2 | Minimum spline_df=1 | All required + `covariates=["stage"]` | `regression_model="flexible"`, `spline_df=1` | Runs with minimal flexibility |
| 13.3 | Maximum spline_df=10 | All required + `covariates=["stage"]` | `regression_model="flexible"`, `spline_df=10` | Runs or reports convergence issues gracefully |
| 13.4 | spline_df ignored when regression_model != "flexible" | All required + `covariates=["stage"]` | `regression_model="additive"`, `spline_df=8` | spline_df has no effect on additive model |

---

## COMPLETE OPTION COVERAGE CHECKLIST

All 22 options from `jamovi/relativesurvival.a.yaml`:

| Option | Type | Default | Tested In |
|--------|------|---------|-----------|
| - [x] `data` | Data | (required) | All tests |
| - [x] `time` | Variable | (required) | #1.1, #1.2, #6.1-6.4 |
| - [x] `status` | Variable | (required) | #1.1, #10C.1-10C.4 |
| - [x] `age` | Variable | (required) | #1.1, #10E.1-10E.5 |
| - [x] `sex` | Variable | (required) | #1.1, #9.1-9.9, #10D.1-10D.4 |
| - [x] `year` | Variable | (required) | #1.1, #10F.1-10F.4 |
| - [x] `covariates` | Variables | (none) | #5B.1-5B.4, #5C.1-5C.2, #5D.1-5D.4, #5F.1 |
| - [x] `ratetable` | List | "us" | #3.1-3.16 |
| - [x] `method` | List | "poharperme" | #2.1-2.6 |
| - [x] `time_scale` | List | "years" | #6.1-6.4 |
| - [x] `net_survival` | Bool | TRUE | #4A.1-4A.4 |
| - [x] `excess_mortality` | Bool | TRUE | #4B.1-4B.5 |
| - [x] `crude_probability` | Bool | TRUE | #4C.1-4C.4 |
| - [x] `age_standardized` | Bool | FALSE | #4D.1-4D.5 |
| - [x] `period_analysis` | Bool | FALSE | #4E.1-4E.6 |
| - [x] `cohort_year` | String | "" | #12.1-12.5 |
| - [x] `regression_model` | List | "none" | #5A.1, #5B.1-5B.4, #5C.1-5C.2, #5D.1-5D.4 |
| - [x] `spline_df` | Integer | 4 | #13.1-13.4 |
| - [x] `plot_observed` | Bool | TRUE | #7.1-7.2 |
| - [x] `plot_expected` | Bool | TRUE | #7.3-7.4 |
| - [x] `plot_relative` | Bool | TRUE | #7.5-7.6 |
| - [x] `plot_excess` | Bool | TRUE | #7.7-7.9 |
| - [x] `confidence_level` | Number | 0.95 | #8.1-8.7 |
| - [x] `timepoints` | String | "1,3,5,10" | #11.1-11.12 |

---

## EXISTING TEST COVERAGE SUMMARY

Tests already implemented in `tests/testthat/test-relativesurvival.R`:

| Section | Tests Present | Coverage Notes |
|---------|--------------|----------------|
| Basic functionality | 2 tests (#1.1, #1.2 equivalent) | Missing: partial variable, all-analyses-enabled, summary content |
| Estimation methods | 1 test (all 4 methods in loop) | Missing: method comparison, method label in plot |
| Optional analyses | 4 tests (net, excess, crude, age std) | Missing: disabled states, value range checks, edge cases |
| Time scales | 1 test (months + days) | Missing: equivalence check between scales |
| Sex value mapping | 1 test (m/f + Turkish) | Missing: numeric coding, French/Spanish, mixed case |
| Error handling | 6 tests | Missing: zero events, boundary (n=30), factor status, NA handling |
| Confidence levels | 1 test (95 vs 90) | Missing: extremes (0.50, 0.99), CI width ordering, propagation |
| Rate tables | 1 test (us, fr, slovenia) | Missing: 12 WHO tables, custom fallback |
| Regression | 1 test (additive) | Missing: multiplicative, flexible, no-covariate, model fit, EPV |
| Custom timepoints | 1 test | Missing: separators, edge cases, empty/invalid strings |
| Period analysis | 0 tests | **Not covered** |
| Cohort year | 0 tests | **Not covered** |
| Spline df | 0 tests | **Not covered** |
| Plot options | 0 tests | **Not covered** (difficult to test rendering, but state/visibility testable) |

**Total tests currently: 18 (covering ~40% of the checklist above)**

### Priority gaps for new tests:
1. Period analysis + cohort_year (sections 4E, 12) -- no coverage at all
2. Plot state serialization (section 7.10) -- protobuf risk area
3. Remaining WHO rate tables (section 3.5-3.14) -- data availability
4. Flexible parametric regression (section 5D) -- rstpm2 dependency
5. Timepoint edge cases (section 11.5-11.12) -- parser robustness
6. Confidence level propagation (section 8.7) -- cross-table consistency
