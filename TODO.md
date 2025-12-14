## check articles

source .venv/bin/activate

.claude/completions/review_article_stats_save.sh "aqaf082" \
  "/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/temp3/aqaf082.pdf"

.claude/completions/review_article_stats_save.sh "Thyroid-CNN" \
  "/path/paper.pdf" "/path/supplement.html" "/path/notes.md"

.claude/completions/review_article_stats_save.sh "Example-URL" \
  "<https://example.com/article.html>"

> pdftotext

> markitdown path-to-file.pdf -o document.md
<https://github.com/microsoft/markitdown>

> marker_single /path/to/file.pdf --output_dir
marker_single /Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/vignettes-OncoPath/literature/cluster-ihc/carvalho2011.pdf --output_dir /Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/vignettes-OncoPath/literature/cluster-ihc/
<https://github.com/datalab-to/marker>

## Oncoplot follow-ups

- Draft: expose a dedicated per-sample mutation burden result if users need TMB-like summaries outside the marginal plot.
- Draft: add UI gating so `log10TransformTMB` is only available/active when `showTMB` is enabled.
- Draft: penalized/Firth logistic fallback and manual positive-predictor level control for oddsratio LRs/nomogram

> /review-article-stats '/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/temp/untitled folder/Multi-modal convolutional neural network-based thyroid cytology classification and diagnosis - ScienceDirect.md'
'/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/temp/untitled folder/Multi-modal convolutional neural network-based thyroid cytology classification and diagnosis - ScienceDirect.html'
'/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/temp/untitled folder/1-s2.0-S0046817725001558-main.pdf'
'/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/temp/untitled folder/1-s2.0-S0046817725001558-main.md'

claude --no-mcp --no-tools "/review-article-stats Deep-Learning-Based-Prediction" \
  "/Users/.../Deep-Learning-Based-Prediction.md" \
  "/Users/.../Deep-Learning-Based-Prediction.html" \
  "/Users/.../Deep-Learning-Based-Prediction.pdf" \
  "/Users/.../Deep-Learning-Based-Prediction.txt"

## check and update each function

echo "/document-function " | claude
claude "/document-function "

echo "/check-function FUNC_NAME" | claude
echo "/checkpoint FUNC_NAME" | claude
echo "/prepare-translation FUNC_NAME" | claude
echo "/review-function FUNC_NAME" | claude
echo "/fix-function FUNC_NAME" | claude
echo "/document-function FUNC_NAME" | claude

^menuGroup:\s*\S+(?<![TD2])$
^menuGroup:\s*\S+([T])$

(menuGroup:\s.*)T2$
$1D

(menuGroup:\s.*?)(T3|T2|T1|T)$

gemini: /chat share log.json 
gemini: ctrl+s copy mode

use gemini to make Readiness for Clinicians and Pathologists assessment and Use Case Example Generation for each function.



You are an expert R-package and jamovi developer.
You are an expert in biostatistics working with pathologists and clinicians.
Critically evaluate multisurvival function.
Is it mathematically and statistically accurate?
Evaluate if data flow is correct. Are arguments from .a.yaml correctly read. Is the data flow in .b.R correct. Are the results displayed in .r.yaml appropriately. Evaluate if .u.yaml is user friendly and contains all necessary options.
Is it ready to be used by clinicians and pathologists?
Is it ready for release? Do not focus just on testing. Focus on functionality and accuracy. After you are
satisfied with function then update and use tests.
Suggest improvements.
Do not remove functionality.

> fix issues and implement recommendations. favor functionality over explanations and guidence parts.

> how does FUNC_NAME handle varibale with empty spaces and characters in them.
Is it necessary to implement escapeVariableNames logic from modelbuilder to FUNC_NAME.
In tables and plots I see the modified names that is why I am asking
Can we apply labelled logic as in oddsratio

Check this javascript usage <https://github.com/yurismol/jYS/blob/master/jamovi/js/mout.events.js> and <https://github.com/yurismol/jYS/blob/74d32adc0114df6288f38fea7534afc7385a9a1a/jamovi/mout.u.yaml#L39>  to implement it for clinical presets
<https://github.com/yurismol/jYS/blob/74d32adc0114df6288f38fea7534afc7385a9a1a/R/mout.b.R>

jmvtools::prepare();devtools::document();devtools::load_all();data <- readr::read_csv("~/Desktop/survival_pancreas_T2_to_T3_upstage_10072025.csv");stagemigration(data = data, oldStage = T_AJCC8_gr, newStage = T_modified_gr, survivalTime = OverallTime, event = Outcome, eventLevel = "DEAD")

jmvtools::prepare();devtools::document();devtools::load_all();data <- readr::read_csv("/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/data/diagnostic_meta_test.csv");diagnosticmeta(
    data = data,
    study = study_name,
    true_positives = true_positives,
    false_positives = false_positives,
    false_negatives = false_negatives,
    true_negatives = true_negatives,
    covariate = NULL,
    hsroc_analysis = TRUE,
    meta_regression = TRUE,
    heterogeneity_analysis = TRUE)

jmvtools::prepare();devtools::document();devtools::load_all();data <- readr::read_csv("/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/data/ihc_breast_cancer.csv");ihccluster(
    data = data,
    catVars = vars(ER_Status, PR_Status),
    caseId = NULL,
    clinicalVars = vars())

data <- jmvReadWrite::read_omv(fleInp = "/Users/serdarbalci/Desktop/meddecide_debug.omv")

update .u.yaml to make it user friendly. make all relevant features to be together.

remove all dummy code and hardcoded values. make them all work with inputs. implement real function instead of placeholders.

prepare comprehensive test data generator under data-raw and prepare the data  as csv under data folder

To lower the computation make all default checkboxes to be false in .a.yaml


! Rscript -e "jmvtools::prepare()"
! Rscript -e "devtools::document()"
! Rscript -e "devtools::load_all()"

Rscript _updateModules.R




Always use available 'skills' when possible. Keep the output organized. 
Use these skills to update text and analysis in the project: 
  pubmed-database
  biopython
  biorxiv-database
  openalex-database
  citation-management
  scholar-evaluation
  clinical-decision-support
  clinical-reports
  exploratory-data-analysis
  hypogenic
  hypothesis-generation
  literature-review
  peer-review
  scientific-brainstorming
  scientific-critical-thinking
  scientific-visualization
  scientific-schematics
  generate-image
  scientific-slides
  scientific-writing
  research-grants
  statistical-analysis
  statsmodels
  paper-2-web
  matplotlib
  scikit-learn
  scikit-survival
  plotly
  seaborn
  aeon
  docx
  xlsx
  pptx
  latex-posters
  pdf
  markitdown
  histolab
  pathml
  shap
  omero-integration
  pydicom
  pytorch-lightning





update DECSRIPTION, NEWS, README, and function Roxygen documentations.

! Rscript -e "pkgdown::build_site()"
! Rscript -e "pkgdown::build_site(examples = FALSE, lazy = TRUE, preview = FALSE)"

run jmvtools::prepare() to see if there are any errors
run devtools::document() to see if there are any errors

prepare a realistic data to test the features in detail
move csv files under data folder.
move data generation files under data-raw folder.
move documentation files under module specific vignettes folder.

read this study, evaluate it. update omentum study results and pathsampling implementation accordingly.

│ │ - GEE (Generalized Estimating Equations) - Major undertaking                                         │ │
│ │ - Mixed Model ANOVA - Major undertaking                                                              │ │
│ │ - Comprehensive Diagnostic Accuracy Module - Separate module needed                                  │ │
│ │ - Multiple Testing Corrections Suite - Separate module needed

Routinely we have following: BiopsyNumber, Total Number of Blocks, How many of these blocks have tumor, If there is a tumor what is the first block number, if there is a tumor in which blocks is it present. We have these information. We cannot know if there were tumor if we submitted whole omentum.
We do not know if surgeon has sent correct tissue that is beyond us. In macroscopy we take samples/blocks from submitted tissue. 5-10 blocks per case. there will be theoretical false negative due to gross sampling or false negative due to microscopic sampling (small tumors may be lost with trimming). we cannot be certain for this. we are trying to identify the number of minimum sections taken to get the highest correct answer for the patient.
Some tumors (serous) tend to metastasise more than others (endometrioid). So it may be informative to add negative cases to understand the tumor biology, the probability of metastasising in that tumor.
To continue this session, run codex resume 0199d3a9-6a19-7f91-8124-a9d493708b4a.

---

# ClinicoPath Module Enhancement Roadmap

**Version**: 1.0
**Date**: 2025-10-18
**Based on**: Comprehensive module evaluation (TODO2.md)

---

## 📋 **Overview**

This roadmap outlines planned enhancements for the ClinicoPath jamovi module ecosystem, designed specifically to work within **jamovi's tabular data structure**. All features are adapted to work with:

- **Rectangular data frames** (rows = observations, columns = variables)
- **Variable types**: Continuous (numeric), Nominal (factor), Ordinal (ordered factor)
- **Case-by-case data** (no longitudinal/nested structures without reshaping)
- **Single dataset per analysis** (no multi-table joins in UI)

---

## 🎯 **Priority Framework**

- **[H] High Priority**: Clinical impact, immediate user needs, competitive necessity
- **[M] Medium Priority**: Methodological depth, workflow improvements
- **[L] Low Priority**: Polish, edge cases, future enhancements

---

## 📊 **1. meddecide Module Enhancements**

### **Phase 1: Core Decision Analysis (Sprints 1-2)**

#### **[H] ✅ COMPLETED - Decision Curve Analysis (DCA)**

*Clinical net benefit evaluation for prediction models*

**✅ Implementation Status (Completed):**
- ✅ Net benefit calculation across threshold range
- ✅ "Treat All" vs "Treat None" reference strategies
- ✅ Multiple model comparison
- ✅ Bootstrap confidence intervals
- ✅ Clinical impact metrics (interventions avoided, NNS)
- ✅ Optimal threshold identification
- ✅ Weighted AUC calculation
- ✅ Cost-benefit analysis option
- ✅ Decision consequences table (TP, FP, TN, FN)
- ✅ Comprehensive visualization (decision curves, impact plots)
- ✅ Files: `jamovi/decisioncurve.{a,r,u}.yaml`, `R/decisioncurve.b.R` (1366 lines)

**Jamovi Data Structure**:

```yaml
Required Variables:
  - outcome: Binary (0/1) or factor with 2 levels
  - predictor_probability: Continuous (0-1) predicted probabilities

Optional Variables:
  - model_name: Nominal factor for comparing multiple models
  - validation_set: Nominal factor (training/validation split)

Expected Data Format (one row per patient):
| patient_id | outcome | model1_prob | model2_prob | validation_set |
|------------|---------|-------------|-------------|----------------|
| 001        | 1       | 0.65        | 0.72        | training       |
| 002        | 0       | 0.23        | 0.31        | training       |
```

**Implementation**:

- Input: Binary outcome + continuous predicted probabilities
- Output: Net benefit curves, decision tables across threshold range
- Comparison: "Treat all" vs "Treat none" vs model strategies
- R packages: `dcurves`, `rmda`

**UI Elements**:

- Threshold range slider (0.01 to 0.99)
- Harm-to-benefit ratio input
- Multiple model comparison option

---

#### **[H] ✅ COMPLETED - Enhanced Markov Models**

*Multi-cycle decision modeling for chronic diseases*

**Status**: ✅ Completed (PSA verified 2025-01-05)
**Files**: `jamovi/decisiongraph.{a,u}.yaml`, `R/decisiongraph.b.R`

**✅ Completed Features:**
- ✅ Separate discount rates for costs vs utilities (QALYs) - 2025-01-04
- ✅ Checkbox to enable/disable separate rates
- ✅ Default: 3% for costs, 1.5% for utilities
- ✅ Probabilistic Sensitivity Analysis (PSA) with Monte Carlo simulation
- ✅ Multiple processing modes (standard, parallel, chunked for large simulations)
- ✅ Parameter distributions and correlation handling
- ✅ CEAC (Cost-Effectiveness Acceptability Curve) generation
- ✅ EVPI (Expected Value of Perfect Information) calculation
- ✅ Bootstrap validation with 10,000 iterations
- ✅ Performance optimization for memory efficiency

**Jamovi Data Structure**:

```yaml
# Transition Matrix Input (wide format)
Required Variables (per cycle):
  - strategy: Nominal factor (treatment arms)
  - state_from: Nominal factor (health states)
  - state_to: Nominal factor (health states)
  - probability: Continuous (0-1)
  - cost: Continuous (≥0)
  - utility: Continuous (0-1)

Expected Data Format (one row per transition):
| strategy | state_from | state_to  | probability | cost  | utility |
|----------|------------|-----------|-------------|-------|---------|
| Treatment| Healthy    | Sick      | 0.10        | 1000  | 0.8     |
| Treatment| Sick       | Dead      | 0.05        | 5000  | 0.0     |
| Control  | Healthy    | Sick      | 0.20        | 0     | 0.9     |
```

**Implementation**:

- Expand existing `decisiongraph` Markov capabilities
- Cohort trace simulation over time cycles
- Discounting (costs and QALYs)
- Probabilistic sensitivity analysis via parameter distributions
- R packages: `heemod`, `dampack`

**UI Elements**:

- Cycle length input (years/months)
- Time horizon (number of cycles)
- Discount rates (costs, utilities)
- Half-cycle correction toggle

---

### **Phase 2: Prediction Models (Sprints 3-4)**

#### **[H] ✅ COMPLETED - Clinical Prediction Model Builder**

*Logistic regression with integrated calibration and validation*

**✅ Implementation Status (Completed 2025-01-04):**
- ✅ Logistic regression model fitting
- ✅ Coefficient table with odds ratios and CIs
- ✅ AUC (C-statistic) with confidence intervals
- ✅ Brier score calculation
- ✅ Hosmer-Lemeshow calibration test
- ✅ Calibration slope
- ✅ Bootstrap validation with optimism correction
- ✅ ROC curve plot
- ✅ Calibration plot with loess smooth
- ✅ Risk stratification into groups
- ✅ Clinical interpretation guidance
- Files: `jamovi/predmodel.{a,r,u}.yaml`, `R/predmodel.b.R`

**Jamovi Data Structure**:

```yaml
Required Variables:
  - outcome: Binary outcome (disease/event present)
  - predictors: Multiple continuous or categorical variables

Optional Variables:
  - validation_fold: Nominal factor for k-fold CV
  - external_cohort: Nominal factor for external validation

Expected Data Format (one row per patient):
| patient_id | outcome | age | biomarker | stage | validation_fold |
|------------|---------|-----|-----------|-------|-----------------|
| 001        | 1       | 65  | 12.5      | III   | fold_1          |
| 002        | 0       | 52  | 8.2       | II    | fold_2          |
```

**Implementation**:

- Model building: Logistic regression with stepwise/LASSO selection
- Calibration: Hosmer-Lemeshow test, calibration plots, calibration slope
- Discrimination: ROC curve, AUC with CI, Brier score
- Validation: Bootstrap optimism-correction, k-fold CV
- Risk score generation: Point-based scoring systems
- R packages: `rms`, `pROC`, `PredictABEL`, `glmnet`

**Outputs**:

- Coefficient table with odds ratios
- Calibration plot (observed vs predicted)
- ROC curve with optimal cutpoint
- Risk stratification table (low/medium/high)
- TRIPOD-compliant reporting tables

---

#### **[M] ✅ COMPLETED - Model Calibration & Validation Dashboard**

*Comprehensive performance metrics panel*

**✅ Implementation Status (Completed 2025-01-04):**
- ✅ Calibration-in-the-large assessment
- ✅ Calibration slope and intercept
- ✅ Flexible calibration curves (loess/splines)
- ✅ AUC, Brier score, scaled Brier score
- ✅ Decision curve analysis (DCA) with net benefit
- ✅ Subgroup performance analysis
- ✅ Validation type selection (external/temporal/geographic)
- ✅ ROC plot, calibration plot, DCA plot
- ✅ Evidence-based recommendations for recalibration
- Files: `jamovi/modelval.{a,r,u}.yaml`, `R/modelval.b.R`

**Jamovi Data Structure**:

```yaml
# Same as prediction model, but focus on validation metrics
Required Variables:
  - outcome: Binary
  - predicted_probability: Continuous (from existing model)

Optional Variables:
  - time_period: Nominal factor for temporal validation
  - hospital_site: Nominal factor for geographic validation
```

**Implementation**:

- Calibration: Calibration-in-the-large, calibration slope, flexible curves
- Performance: Brier score, scaled Brier score, integrated Brier score
- Net benefit: DCA integration
- Subgroup analysis: Performance by strata
- R packages: `CalibrationCurves`, `riskRegression`

---

### **Phase 3: Health Economics (Sprints 5-6)**

#### **[M] Cost-Effectiveness Analysis**

*Economic evaluation with ICERs and CEACs*

**Jamovi Data Structure**:

```yaml
Required Variables:
  - strategy: Nominal factor (intervention arms)
  - patient_id: Unique identifier
  - total_cost: Continuous (currency)
  - total_qaly: Continuous (quality-adjusted life years)

Optional Variables:
  - psa_iteration: Numeric (for probabilistic sensitivity analysis)
  - subgroup: Nominal factor

Expected Data Format (one row per patient per strategy):
| patient_id | strategy    | total_cost | total_qaly | psa_iteration |
|------------|-------------|------------|------------|---------------|
| 001        | Treatment   | 15000      | 8.5        | 1             |
| 001        | Control     | 5000       | 7.2        | 1             |
| 002        | Treatment   | 18000      | 9.1        | 1             |
```

**Implementation**:

- ICER calculation with confidence intervals
- Cost-effectiveness plane scatter plots
- Cost-effectiveness acceptability curves (CEAC)
- Net monetary benefit at various willingness-to-pay thresholds
- One-way and two-way sensitivity analysis
- R packages: `heemod`, `dampack`, `BCEA`

**Outputs**:

- ICER table with incremental costs/QALYs
- CE plane (scatter plot of cost vs effect)
- CEAC curve (probability cost-effective vs WTP threshold)
- Tornado diagram for sensitivity analysis

---

### **Phase 4: Advanced Features (Sprint 7+)**

#### **[M] ✅ COMPLETED - Time-to-Event Decision Analysis**

*Survival-based ROC and decision metrics*

**Jamovi Data Structure**:

```yaml
Required Variables:
  - time: Continuous (survival time)
  - event: Binary (0=censored, 1=event)
  - marker: Continuous biomarker or risk score

Optional Variables:
  - landmark_time: Continuous (for landmark analysis)

Expected Data Format (one row per patient):
| patient_id | time | event | risk_score | biomarker_level |
|------------|------|-------|------------|-----------------|
| 001        | 24.5 | 1     | 0.72       | 145.2           |
| 002        | 18.3 | 0     | 0.45       | 98.7            |
```

**Implementation**:

- Time-dependent ROC curves at specific time points
- C-index for survival data
- Restricted mean survival time (RMST) differences
- Net benefit for survival endpoints
- Integration with jSurvival module
- R packages: `timeROC`, `survivalROC`, `survRM2`

---

#### **[L] Automated Reporting**

*Manuscript-ready outputs*

**Implementation**:

- STARD/TRIPOD compliance templates
- Natural language summaries of diagnostic performance
- Journal-formatted tables (NEJM, Lancet, JAMA)
- Export to DOCX with embedded tables/figures
- R packages: `reporter`, `officer`, `flextable`

---

## 🏥 **2. jSurvival Module Enhancements**

### **Phase 1: Competing Risks (Sprints 1-2)**

#### **[H] ✅ COMPLETED - Fine-Gray Regression (CRR)**

*Covariate-adjusted competing risks analysis*

**✅ Implementation Status (Completed 2025-01-04):**
- ✅ Fine-Gray subdistribution hazard regression
- ✅ Sub-hazard ratio (sHR) table with confidence intervals
- ✅ Cumulative incidence function (CIF) plots
- ✅ Gray's test for group comparisons
- ✅ Support for multiple competing events
- ✅ Stratified analysis option
- ✅ Comparison to cause-specific hazards
- ✅ Prediction at specified time points
- ✅ Diagnostic plots and influence statistics
- ✅ Bootstrap confidence intervals
- ✅ Color scheme options (default, colorblind, NEJM, Lancet)
- ✅ Comprehensive clinical interpretation
- ✅ Files: `jamovi/finegray.{a,r,u}.yaml`, `R/finegray.b.R` (900+ lines)

**Jamovi Data Structure**:

```yaml
Required Variables:
  - time: Continuous (time to event or censoring)
  - status: Nominal factor with 3+ levels (0=censored, 1=event of interest, 2=competing event)
  - covariates: Multiple continuous or categorical predictors

Optional Variables:
  - strata: Nominal factor for stratified analysis

Expected Data Format (one row per patient):
| patient_id | time | status              | age | stage | treatment |
|------------|------|---------------------|-----|-------|-----------|
| 001        | 24.5 | cancer_death        | 65  | III   | chemo     |
| 002        | 18.3 | other_death         | 72  | II    | surgery   |
| 003        | 36.0 | censored            | 58  | I     | chemo     |
```

**Status Variable Encoding**:

- 0 or "censored" = Censored
- 1 or "event_of_interest" = Event of interest
- 2 or "competing_event" = Competing event
- Multiple competing events: 2, 3, 4, etc.

**Implementation**:

- Fine-Gray subdistribution hazard models
- Cumulative incidence function (CIF) plots with risk tables
- Sub-hazard ratios (sHR) with confidence intervals
- Gray's test for group comparisons
- Stacked CIF plots for multiple competing events
- R packages: `cmprsk`, `riskRegression`, `tidycmprsk`
- Files: Create `/R/finegray.b.R`, `/jamovi/finegray.{a,u,r}.yaml`

**Outputs**:

- SHR table (similar to Cox HR table)
- CIF curves by group
- Risk tables showing cumulative incidence
- Cause-specific vs cumulative incidence comparison

---

#### **[H] ✅ COMPLETED - Enhanced Competing Risk Diagnostics**

*Comprehensive competing risk visualization*

**✅ Implementation Status (Completed 2025-01-04):**
- ✅ Stacked probability plot (CIF1 + CIF2 + Survival)
- ✅ 1-KM vs CIF comparison plot (demonstrates competing risk bias)
- ✅ Color scheme options (default, colorblind-safe, grayscale)
- ✅ Enhanced CIF visualization with customizable colors
- ✅ UI controls for new plots
- Files: `jamovi/competingsurvival.{a,r,u}.yaml`, `R/competingsurvival.b.R`

**Jamovi Data Structure**:

```yaml
# Same as Fine-Gray, but focused on visualization
Required Variables:
  - time: Continuous
  - status: Multi-level factor (censored + multiple event types)
  - group: Nominal factor (optional, for group comparison)
```

**Implementation**:

- CIF curves with customizable colors per event type
- Stacked probability plots (CIF1 + CIF2 + Survival)
- Risk tables showing cumulative incidence by time
- Cause-specific hazard vs cumulative incidence side-by-side
- 1 - KM vs CIF comparison plots

---

### **Phase 2: Model Validation (Sprints 3-4)**

#### **[H] ✅ COMPLETED - Time-Dependent Calibration**

*Survival model performance evaluation*

**✅ Implementation Status (Completed 2025-01-04):**
- ✅ New module: `survivalcalibration`
- ✅ Time-dependent C-index with confidence intervals
- ✅ Integrated Brier score calculation
- ✅ Calibration plot (observed vs predicted survival)
- ✅ Calibration slope, intercept, and E:O ratio
- ✅ Bootstrap validation with optimism correction
- ✅ K-fold cross-validation
- ✅ TRIPOD-compliant validation reporting
- Files: `jamovi/survivalcalibration.{a,r,u}.yaml`, `R/survivalcalibration.b.R`

**Jamovi Data Structure**:

```yaml
Required Variables:
  - time: Continuous (observed survival time)
  - event: Binary (0=censored, 1=event)
  - predicted_surv: Continuous (predicted survival probability at specific time)
  OR
  - linear_predictor: Continuous (from Cox model)

Optional Variables:
  - validation_set: Nominal factor (training/validation/external)
  - calibration_time: Continuous (time point for calibration, e.g., 5 years)

Expected Data Format (one row per patient):
| patient_id | time | event | pred_5yr_surv | validation_set |
|------------|------|-------|---------------|----------------|
| 001        | 24.5 | 1     | 0.65          | training       |
| 002        | 68.0 | 0     | 0.82          | validation     |
```

**Implementation**:

- Calibration plots at specific time points (e.g., 1, 3, 5 years)
- Grouped calibration curves (deciles of predicted risk)
- Calibration slope and intercept
- Bootstrap optimism-correction (internal validation)
- K-fold cross-validation for C-index and Brier score
- TRIPOD-compliant validation reporting
- R packages: `pec`, `riskRegression`, `rms`, `survival`

**Outputs**:

- Calibration plot (observed vs predicted survival)
- Time-dependent C-index with CI over time
- Integrated Brier score
- Calibration metrics table (slope, intercept, E:O ratio)

---

#### **[M] ✅ COMPLETED - Predictive Performance Metrics**

*Comprehensive discrimination and calibration*

**✅ Implementation Status (Completed 2025-01-04):**
- ✅ New module: `reclassmetrics`
- ✅ Net Reclassification Improvement (NRI) - categorical and continuous
- ✅ Integrated Discrimination Improvement (IDI)
- ✅ Bootstrap confidence intervals for all metrics
- ✅ Separate NRI for events and non-events
- ✅ IDI components (integrated sensitivity and specificity)
- ✅ Probability improvement scatter plot
- ✅ Model comparison visualization
- Files: `jamovi/reclassmetrics.{a,r,u}.yaml`, `R/reclassmetrics.b.R`

**Jamovi Data Structure**:

```yaml
# Same as calibration, plus:
Optional Variables:
  - old_model_pred: Continuous (for model comparison/reclassification)
```

**Implementation**:

- Time-dependent C-index (concordance index) with CI
- Integrated Brier score over time range
- Net reclassification improvement (NRI)
- Integrated discrimination improvement (IDI)
- Likelihood ratio tests for nested models
- R packages: `survC1`, `survAUC`, `nricens`, `PredictABEL`

---

### **Phase 3: Advanced Models (Sprints 5-6)**

#### **[M] Parametric Survival Models**

*AFT and flexible parametric models*

**Jamovi Data Structure**:

```yaml
Required Variables:
  - time: Continuous (time to event or censoring)
  - event: Binary (0=censored, 1=event)
  - covariates: Multiple continuous or categorical predictors

Expected Data Format (one row per patient):
| patient_id | time | event | age | treatment | biomarker |
|------------|------|-------|-----|-----------|-----------|
| 001        | 24.5 | 1     | 65  | chemo     | 12.5      |
| 002        | 18.3 | 0     | 72  | surgery   | 8.3       |
```

**Implementation**:

- Accelerated failure time (AFT) models
  - Distributions: Weibull, Log-normal, Log-logistic, Gamma
- Royston-Parmar flexible parametric models (splines on log-hazard)
- Model comparison via AIC/BIC
- Survival, hazard, and hazard ratio curves
- Time-varying effects (interaction with time)
- R packages: `survival::survreg`, `flexsurv`, `rstpm2`

**Outputs**:

- Coefficient table with acceleration factors (AFT)
- Survival curves from parametric models
- Hazard curves (smooth, not step functions)
- Model fit comparison table

---

#### **[M] Frailty & Clustered Data**

*Multi-level survival analysis*

**Jamovi Data Structure**:

```yaml
Required Variables:
  - time: Continuous
  - event: Binary
  - cluster_id: Nominal factor (e.g., hospital, surgeon, family)
  - covariates: Multiple predictors

Optional Variables:
  - cluster_level_var: Continuous (e.g., hospital volume)

Expected Data Format (one row per patient, clustered by hospital):
| patient_id | hospital_id | time | event | age | treatment |
|------------|-------------|------|-------|-----|-----------|
| 001        | Hospital_A  | 24.5 | 1     | 65  | chemo     |
| 002        | Hospital_A  | 18.3 | 0     | 72  | surgery   |
| 003        | Hospital_B  | 36.0 | 1     | 58  | chemo     |
```

**Implementation**:

- Shared frailty models (random effects at cluster level)
- Individual frailty models (unobserved heterogeneity)
- Frailty distributions: Gamma, Log-normal
- Variance partition coefficients (cluster-level variance)
- Robust standard errors (cluster sandwich estimators)
- R packages: `survival::coxph` with `frailty()`, `frailtypack`, `coxme`

**Outputs**:

- HR table with cluster-adjusted SEs
- Frailty variance estimate
- Intra-cluster correlation (ICC)

---

### **Phase 4: Recurrent Events & Multi-State (Sprint 7+)**

#### **[M] Recurrent Event Models**

*Multiple events per patient*

**Jamovi Data Structure**:

```yaml
# Long format: one row per event
Required Variables:
  - patient_id: Identifier (multiple rows per patient)
  - event_number: Numeric (1st, 2nd, 3rd event...)
  - time_to_event: Continuous (time from baseline or previous event)
  - event_occurred: Binary
  - covariates: Patient-level or time-varying

Optional Variables:
  - time_origin: Continuous (for gap-time vs calendar-time)

Expected Data Format (long format, multiple rows per patient):
| patient_id | event_num | time_to_event | event | age | treatment |
|------------|-----------|---------------|-------|-----|-----------|
| 001        | 1         | 5.2           | 1     | 65  | chemo     |
| 001        | 2         | 3.8           | 1     | 65  | chemo     |
| 001        | 3         | 7.1           | 0     | 65  | chemo     |
| 002        | 1         | 12.5          | 1     | 58  | surgery   |
```

**Implementation**:

- Andersen-Gill (AG) model: treats events as independent
- Prentice-Williams-Peterson (PWP) models: stratified by event number
- Gap-time vs calendar-time approaches
- Marginal vs conditional models
- Cumulative mean function plots
- R packages: `survival::coxph` with `cluster()`, `reReg`, `reda`

**Outputs**:

- HR table for recurrent event rate
- Cumulative mean function plot (expected number of events over time)
- Event-specific HRs (PWP models)

---

#### **[L] Multi-State Models**

*State transition modeling (PFS → OS)*

**Jamovi Data Structure**:

```yaml
# Long format: one row per transition
Required Variables:
  - patient_id: Identifier
  - from_state: Nominal factor (state at transition start)
  - to_state: Nominal factor (state at transition end)
  - transition_time: Continuous
  - covariates: Patient-level predictors

Expected Data Format (one row per transition per patient):
| patient_id | from_state  | to_state    | trans_time | age | treatment |
|------------|-------------|-------------|------------|-----|-----------|
| 001        | Progression-Free | Progression | 12.5    | 65  | chemo     |
| 001        | Progression | Death       | 24.8       | 65  | chemo     |
| 002        | Progression-Free | Death   | 18.3       | 58  | surgery   |
```

**Implementation**:

- State transition diagrams (illness-death models)
- Transition-specific hazard ratios
- Transition probability matrices
- State occupation probabilities over time
- Path-specific effects (e.g., PFS → progression → death)
- R packages: `mstate`, `msm`

**Outputs**:

- Transition hazard ratio tables
- State occupation plot (stacked areas)
- Transition probability curves

---

#### **[L] Cure Models**

*Long-term survivor modeling*

**Jamovi Data Structure**:

```yaml
Required Variables:
  - time: Continuous (follow-up time)
  - event: Binary
  - covariates: Predictors of cure and survival

Expected Data Format (one row per patient):
| patient_id | time | event | age | stage | treatment |
|------------|------|-------|-----|-------|-----------|
| 001        | 120  | 0     | 45  | I     | chemo     | # Potential cure
| 002        | 24   | 1     | 72  | IV    | palliate  |
```

**Implementation**:

- Mixture cure models (cured fraction + survival of uncured)
- Non-mixture cure models (promotion time models)
- Cure fraction estimation with CI
- Cure probability by covariates
- R packages: `flexsurvcure`, `smcure`, `cuRe`

---

### **Phase 5: Non-PH Handling (Sprint 8)**

#### **[M] ✅ COMPLETED - Enhanced Non-PH Diagnostics & Solutions**

*Automated proportional hazards violation handling*

**✅ Implementation Status (Completed 2025-01-04):**
- ✅ Automatic PH violation detection (p < 0.05 flagging)
- ✅ Color-coded status indicators (warning/success)
- ✅ Educational content explaining Schoenfeld residuals test
- ✅ Actionable recommendations when PH violated:
  - Stratified Cox model (with R code example)
  - Time-dependent coefficients (with R code example)
  - Alternative approaches (RMST, AFT, landmark analysis)
- ✅ HTML-formatted interpretation output
- Files: `jamovi/survival.r.yaml`, `R/survival.b.R`

**Jamovi Data Structure**:

```yaml
# Same as standard Cox model
Required Variables:
  - time: Continuous
  - event: Binary
  - covariates: Predictors (test each for PH)
```

**Implementation**:

- Schoenfeld residual tests (automated for all covariates)
- Visual PH diagnostics with automatic flagging
- Solutions when PH violated:
  - Time-stratified effects (covariate × time interaction)
  - Weighted Cox regression
  - RMST-based comparisons (primary analysis)
- Automatic suggestions based on diagnostic results
- R packages: `survival`, `survminer`, `survRM2`

**Outputs**:

- PH test table (per covariate)
- Schoenfeld residual plots
- Time-stratified HR curves
- RMST difference table (when PH violated)

---

## 📈 **3. JJStatsPlot Module Enhancements**

### **Phase 1: Bug Fixes & Polish (Sprint 1)**

#### **[H] ✅ COMPLETED - UI/Rendering Fixes**

*Resolve critical display issues*

**✅ Implementation Status (Completed 2025-01-04):**
- ✅ Fixed compressed plots in multi-group comparisons (15% extra height)
- ✅ Improved grouped plot sizing (dynamic width/height calculation)
- ✅ Enhanced plot combination with equal spacing
- ✅ Added subplot annotations (tag_levels = "A")
- ✅ Optimal grid layout for grouped analyses
- ✅ Pairwise comparison parameters correctly forwarded
- Files: `R/jjbetweenstats.b.R`, `R/jjbarstats.b.R`, `R/jjhistostats.b.R`

**Jamovi Data Structure**: No changes (existing analyses)

**Implementation**:

- Fix compressed/mis-scaled plots in multi-group comparisons
  - Issue: Multiple plots shrink horizontally
  - Solution: Dynamic width calculation in `.b.R` files
- Resolve "bars not displayed" bug for significance tests
  - Issue: Pairwise comparison bars missing
  - Solution: Update ggstatsplot wrapper parameters
- Improve export resolution and sizing controls
  - Add UI options for plot width/height (pixels, inches)
  - DPI control slider (72, 150, 300, 600)
  - R packages: Review `ggplot2::ggsave()` parameters

**Files to modify**:

- `/R/jjbetweenstats.b.R` - fix multi-group plot scaling
- `/R/jjwithinstats.b.R` - fix significance bars
- All JJStatsPlot `.b.R` files - add export parameters

---

### **Phase 2: Feature Parity (Sprints 2-3)**

#### **[M] ✅ COMPLETED - Model Coefficient Plots**

*Regression and meta-analysis forest plots*

**✅ Implementation Status (Completed 2025-01-04):**
- ✅ New module: `jjcoefstats`
- ✅ Forest plots using ggstatsplot's ggcoefstats()
- ✅ Support for pre-computed coefficients (term, estimate, SE, CI)
- ✅ Automatic model fitting (lm, glm, Cox, mixed effects)
- ✅ Exponentiation for odds ratios and hazard ratios
- ✅ Sort coefficients by magnitude
- ✅ Multiple color schemes and themes
- ✅ P-value display (numeric or symbols)
- ✅ Model fit metrics (R², AIC, concordance)
- Files: `jamovi/jjcoefstats.{a,r,u}.yaml`, `R/jjcoefstats.b.R`

**Jamovi Data Structure**:

```yaml
# Option 1: Pre-computed coefficients (wide format)
Required Variables:
  - term: Nominal factor (coefficient names)
  - estimate: Continuous (coefficient or log-OR)
  - std_error: Continuous
  - conf_low: Continuous (lower CI)
  - conf_high: Continuous (upper CI)

# Option 2: Raw data for automatic model fitting
Required Variables:
  - outcome: Continuous or binary
  - predictors: Multiple variables

Expected Data Format (pre-computed):
| term        | estimate | std_error | conf_low | conf_high | p_value |
|-------------|----------|-----------|----------|-----------|---------|
| age         | 0.05     | 0.01      | 0.03     | 0.07      | 0.001   |
| treatment   | -0.45    | 0.15      | -0.75    | -0.15     | 0.003   |
```

**Implementation**:

- Full `ggcoefstats()` integration
- Support for multiple model types: lm, glm, Cox, mixed models
- Meta-analysis forest plots (fixed and random effects)
- Multiple model comparison in single plot
- R packages: `ggstatsplot`, `broom`, `meta`

**Outputs**:

- Forest plot with effect sizes and CIs
- Heterogeneity statistics (I², tau²) for meta-analysis
- Model comparison with AIC/BIC

---

#### **[M] 🔄 PARTIALLY COMPLETED - Enhanced Customization**

*User-controlled plot aesthetics*

**✅ Currently Implemented (Existing):**
- ✅ Basic theme toggle (originaltheme)
- ✅ Colorblind-safe palettes (colorblindSafe)
- ✅ Journal-style palettes (jco, npg, lancet, jama, nejm, aaas)
- ✅ Present in most JJStatsPlot modules

**⏳ Enhancement Opportunities (Future):**
- ⏳ P-value symbol conversion (asterisks vs numeric)
- ⏳ Font size controls (axis, title, annotation sliders)
- ⏳ Font family selector (Arial, Times, Helvetica)
- ⏳ Legend position controls (top, bottom, left, right, none)
- ⏳ Centralized appearance configuration

**Jamovi Data Structure**: No changes (applies to all existing plots)

**Implementation**:

- P-value symbol conversion
  - Checkbox: "Use symbols instead of p-values"
  - Options: asterisks (*, **, ***), daggers (†, ‡), NS notation
- Custom color palettes
  - Preset palettes: viridis, ColorBrewer, ggplot2 default
  - Manual color picker for key elements
- Theme presets
  - Journal styles: NEJM, Lancet, JAMA, Nature
  - Publication-ready: theme_classic(), theme_bw()
- Font controls
  - Font size sliders (axis, title, annotation)
  - Font family selector (Arial, Times, Helvetica)
- Legend customization
  - Position: top, bottom, left, right, none
  - Title customization

**UI additions** (in all JJStatsPlot `.u.yaml` files):

- "Appearance" section with nested options
- Color palette dropdown
- Symbol notation checkbox
- Theme preset dropdown

---

### **Phase 3: Advanced Plots (Sprint 4)**

#### **[L] Plot Combining & Layout**

*Multi-panel publication figures*

**Jamovi Data Structure**:

```yaml
# User creates multiple plots, then combines
Option: Select existing plots to combine
  - plot1: Dropdown (select from current plots)
  - plot2: Dropdown
  - layout: Grid layout (1x2, 2x1, 2x2, etc.)
```

**Implementation**:

- Patchwork/cowplot integration for multi-panel layouts
- Aligned axes across panels
- Shared legends
- Panel labeling (A, B, C, etc.)
- R packages: `patchwork`, `cowplot`

---

#### **[L] Extended Plot Types**

*Additional ggstatsplot visualizations*

**Jamovi Data Structure**:

```yaml
# Raincloud plots
Required Variables:
  - continuous_var: Continuous
  - group_var: Nominal factor

# Enhanced correlation matrices
Required Variables:
  - variables: Multiple continuous variables
```

**Implementation**:

- Raincloud plots (violin + box + raw data points)
- Enhanced correlation matrices with hierarchical clustering
- Grouped correlation plots (by third variable)
- Bayesian correlation with BF
- R packages: `ggstatsplot`, `ggdist`, `ggrain`

---

### **Phase 4: Workflow (Sprint 5)**

#### **[L] Export & Reproducibility**

*High-quality outputs*

**Implementation**:

- High-resolution export
  - Format options: PDF (vector), SVG (vector), PNG/TIFF (raster)
  - DPI control: 72 (screen), 300 (print), 600 (high-res)
  - Size presets: Journal column width, full page
- R code export
  - "Export R code" button: saves script to reproduce plot
  - Includes all parameters and data filtering
- Batch plot generation
  - Create multiple plots with varying parameters
  - Export all at once

---

## 📋 **4. ClinicoPathDescriptives Module Enhancements**

### **Phase 1: Effect Sizes & Statistical Rigor (Sprints 1-2)**

#### **[H] ✅ COMPLETED - Comprehensive Effect Sizes**

*Standardized effect measures with confidence intervals*

**✅ Implementation Status (Completed 2025-01-04):**
- ✅ Risk Difference (RD) with 95% CI for 2×2 tables
- ✅ Number Needed to Treat (NNT) with 95% CI
- ✅ Proper CI inversion for NNT (from RD CIs)
- ✅ Edge case handling (RD = 0, CI crosses zero)
- ✅ Integrated into conttables module
- Files: `jamovi/conttables.{a,r,u}.yaml`, `R/conttables.b.R`

**Jamovi Data Structure**:

```yaml
# For 2×2 tables
Required Variables:
  - exposure: Binary factor (exposed/unexposed)
  - outcome: Binary factor (disease/no disease)

Expected Data Format (one row per patient):
| patient_id | exposure | outcome |
|------------|----------|---------|
| 001        | yes      | disease |
| 002        | no       | disease |
| 003        | yes      | healthy |

# For continuous comparisons
Required Variables:
  - group: Nominal factor (2+ levels)
  - continuous_var: Continuous outcome
```

**Implementation**:

- **2×2 tables**:
  - Risk ratio (RR) with 95% CI
  - Odds ratio (OR) with 95% CI
  - Risk difference (RD) with 95% CI
  - Number needed to treat (NNT)
- **Larger contingency tables**:
  - Cramér's V with 95% CI
  - Phi coefficient
- **Continuous variables**:
  - Standardized mean difference (Cohen's d, Hedges' g)
  - Glass's delta
  - Confidence intervals via bootstrap
- **Table 1 enhancements**:
  - SMD column for balance checks
  - Automatic flagging of imbalanced variables (SMD > 0.1 or 0.2)
- R packages: `effectsize`, `DescTools`, `epitools`

**Outputs**:

- Effect size table alongside p-values
- Forest plots for effect sizes
- Interpretation guidelines (small/medium/large effects)

---

#### **[M] ✅ COMPLETED - Multiple Comparison Control**

*FDR and familywise error rate correction*

**✅ Implementation Status (Completed 2025-01-04):**
- ✅ 5 adjustment methods: None, Bonferroni, Holm, BH (FDR), BY
- ✅ Conditional adjusted p-value column (q-values)
- ✅ Method-specific educational content
- ✅ Dynamic table headers based on method
- ✅ Integrated into crosstable module with gtsummary
- Files: `jamovi/crosstable.a.yaml`, `R/crosstable.b.R`

**Jamovi Data Structure**:

```yaml
# Same as existing crosstable/descriptive analyses
# No data structure changes needed
```

**Implementation**:

- Correction methods:
  - False discovery rate: Benjamini-Hochberg, Benjamini-Yekutieli
  - Familywise: Bonferroni, Holm, Hochberg, Hommel
  - Others: Šidák, Dunnett (vs control)
- UI options:
  - Dropdown: "Correction method"
  - Checkbox: "Show both raw and adjusted p-values"
- Outputs:
  - Q-value column (FDR-adjusted)
  - Adjusted p-value column
  - Significance annotation with adjustment method noted
- R packages: `stats::p.adjust()`, `multcomp`

**Table output example**:

```
Variable     | Group A | Group B | p-value | p-adj (BH) | Significant
-------------|---------|---------|---------|------------|-------------
Age          | 45±12   | 48±10   | 0.045   | 0.135      | No
Biomarker    | 12±3    | 18±5    | 0.001   | 0.010      | Yes (q<0.05)
```

---

### **Phase 2: Advanced Data Structures (Sprints 3-4)**

#### **[M] Multiple Imputation Support**

*MI-aware descriptive statistics*

**Jamovi Data Structure**:

```yaml
# Imputed datasets must be stacked in long format
Required Variables:
  - .imp: Numeric (imputation number: 0=original, 1=imp1, 2=imp2, etc.)
  - .id: Patient identifier
  - all other variables as usual

Expected Data Format (long format, m=5 imputations):
| .imp | .id | age | biomarker | outcome |
|------|-----|-----|-----------|---------|
| 0    | 001 | 45  | NA        | disease | # Original data
| 1    | 001 | 45  | 12.5      | disease | # Imputation 1
| 2    | 001 | 45  | 13.1      | disease | # Imputation 2
| ...  | ... | ... | ...       | ...     |
| 5    | 001 | 45  | 12.8      | disease | # Imputation 5
```

**Alternative approach**: Upload separate imputed datasets

- User provides m imputed datasets via file selector
- Module internally stacks and processes

**Implementation**:

- Detect `.imp` variable automatically
- Pool estimates across imputations (Rubin's rules)
- "Table 1" with MI pooling:
  - Means/proportions pooled across imputations
  - Variance incorporating within and between-imputation variance
  - Chi-square tests pooled via D1/D2 statistics
- Cross-tabs with MI:
  - Pooled chi-square p-values
  - Pooled odds ratios/risk ratios
- R packages: `mice`, `mitools`, `miceadds`

**Outputs**:

- Standard Table 1 layout with pooled estimates
- Footnote: "Estimates pooled across 5 imputations using Rubin's rules"
- Fraction of missing information (FMI) column (optional)

---

#### **[M] Survey/Weighted Data**

*Complex survey design support*

**Jamovi Data Structure**:

```yaml
Required Variables:
  - weight: Continuous (sampling weights)

Optional Variables:
  - stratum: Nominal factor (stratification variable)
  - cluster: Nominal factor (primary sampling unit)
  - fpc: Continuous (finite population correction)

Expected Data Format (one row per sampled patient):
| patient_id | weight | stratum | cluster | age | outcome |
|------------|--------|---------|---------|-----|---------|
| 001        | 1.5    | urban   | PSU_01  | 45  | disease |
| 002        | 2.3    | rural   | PSU_02  | 52  | healthy |
```

**Implementation**:

- Design-based estimation
  - Weighted means, proportions with design-adjusted SEs
  - Rao-Scott chi-square tests for contingency tables
  - Taylor linearization for variance
- Survey-specific "Table 1"
  - Weighted estimates
  - Design effects (DEFF)
  - Effective sample sizes
- Subpopulation analysis (domain estimation)
- R packages: `survey`, `srvyr`, `srvTable`

**UI elements**:

- Survey design specification section:
  - Weight variable selector
  - Stratification variable
  - Cluster variable
  - FPC (checkbox + variable)
- Output options:
  - Checkbox: "Show design effects"
  - Checkbox: "Show effective sample sizes"

---

### **Phase 3: Enhanced Outputs (Sprint 5)**

#### **[H] One-Click Export Pipelines**

*Journal-ready table export*

**Jamovi Data Structure**: No changes (export formatting only)

**Implementation**:

- Export formats:
  - DOCX: Editable Word tables via `officer` + `flextable`
  - RTF: Universal rich text format
  - LaTeX: For manuscript integration
  - HTML: Web-friendly tables
  - Excel: XLSX with formatting
- Journal presets:
  - **NEJM**: Minimal borders, specific font sizes
  - **Lancet**: Horizontal rules, footnote style
  - **JAMA**: AMA style guidelines
  - **BMJ**: House style compliance
  - **Nature**: High-density compact tables
- Embedded footnotes:
  - Test types used (e.g., "Chi-square test for categorical, t-test for continuous")
  - Effect size definitions
  - Missing data handling
  - Abbreviations
- Automatic table numbering and captions
  - "Table 1. Baseline Characteristics by Treatment Arm"
- R packages: `flextable`, `officer`, `gt`, `gtsummary`, `huxtable`

**UI elements**:

- "Export" button with format dropdown
- "Journal style" dropdown (affects formatting)
- "Caption" text input
- "Footnotes" text area (multi-line)

**Example workflow**:

1. User creates Table 1 with crosstable/summary functions
2. Clicks "Export" → "DOCX (NEJM style)"
3. Downloads ready-to-paste table for manuscript

---

#### **[M] Biomarker Panel Dashboard**

*Consolidated IHC/biomarker summary*

**Jamovi Data Structure**:

```yaml
Required Variables:
  - biomarker_vars: Multiple continuous or binary variables (e.g., Ki67, ER, PR, HER2)

Optional Variables:
  - outcome: Binary (for ROC analysis)
  - cutpoint: Continuous (pre-defined cutpoints for positivity)
  - strata: Nominal factor (for stratified analysis)

Expected Data Format (one row per patient):
| patient_id | Ki67 | ER_percent | PR_percent | HER2_status | outcome |
|------------|------|------------|------------|-------------|---------|
| 001        | 25   | 80         | 70         | positive    | recurrence |
| 002        | 10   | 90         | 85         | negative    | no_recurrence |
```

**Implementation**:

- Summary panel:
  - Positivity rates (using cutpoints: ER≥1%, PR≥1%, HER2+ per ASCO/CAP)
  - Distribution plots (histogram + density) per biomarker
  - Missing data heatmap
- Co-expression analysis:
  - Co-occurrence heatmap (e.g., ER+/PR+ overlap)
  - UpSet plot for multi-marker combinations
- ROC-aware summaries:
  - Per-biomarker ROC curve if outcome provided
  - Optimal cutpoint detection (Youden index)
- Stratified prevalence:
  - Biomarker positivity by subgroup (e.g., by tumor stage)
- R packages: `ggplot2`, `ComplexHeatmap`, `UpSetR`, `pROC`

**Outputs**:

- Multi-panel dashboard
- Biomarker correlation matrix
- Co-occurrence table
- ROC curves (if outcome provided)

---

### **Phase 4: Visualization Enhancements (Sprint 6)**

#### **[M] Advanced Categorical Plots**

*Publication-quality contingency table visualizations*

**Jamovi Data Structure**:

```yaml
# Marimekko/Mosaic plots
Required Variables:
  - row_var: Nominal factor
  - col_var: Nominal factor

# Risk-difference plots
Required Variables:
  - exposure: Binary factor
  - outcome: Binary factor
  - strata: Nominal factor (for stratified RD)
```

**Implementation**:

- **Marimekko/Mosaic plots**:
  - Tile size proportional to cell count
  - Residual shading (blue=more than expected, red=fewer)
  - Significance highlighting
  - R packages: `ggmosaic`, `vcd`
- **Stacked 100% bars with CI ribbons**:
  - Proportions with bootstrap CIs
  - Grouped or stacked layout
- **Risk-difference dot plots**:
  - Forest plot style
  - RD with 95% CI per stratum
  - Meta-analytic pooling across strata
- **Enhanced alluvial diagrams**:
  - Multi-stage patient flow
  - Highlighting specific pathways
  - R packages: `ggalluvial`, `networkD3`

---

### **Phase 5: Workflow Integration (Sprint 7)**

#### **[L] Inter-Module Handoffs**

*Seamless data transfer between modules*

**Implementation**:

- "Send to jSurvival" button:
  - Transfers selected variables to jSurvival for KM analysis
  - Pre-fills time/event variables if detected
- "Send to jjstatsplot" button:
  - Opens jjstatsplot with variables pre-selected
- "Send to meddecide" button:
  - For diagnostic test variables → DTA analysis
- Internal data passing via jamovi session
- No file export/import needed

**UI elements**:

- "Actions" menu in each analysis
- Dropdown: "Send variables to..."

---

#### **[L] Audit Trail & Reproducibility**

*Analysis provenance tracking*

**Implementation**:

- Auto-attach R code to exports:
  - Each exported table/figure includes R code to reproduce
  - Embedded in DOCX as comment or separate file
- Session info embedding:
  - Package versions, jamovi version, OS, date/time
  - Appended to exports as footnote or metadata
- Analysis provenance:
  - Track variable transformations
  - Record filtering/exclusions
  - Log all parameter choices
- R packages: `sessioninfo`, `codebook`

**Outputs**:

- "Reproducibility Report" section in exports
- Copy-pasteable R code
- sessionInfo() output

---

#### **[L] Performance Optimization**

*Handle large datasets (100k+ rows)*

**Implementation**:

- Chunked summaries:
  - Process data in batches for memory efficiency
  - Use `data.table` for fast aggregation
- Lazy evaluation:
  - Only compute statistics when user requests output
  - Cache intermediate results
- Progress indicators:
  - Progress bar for operations >5 seconds
  - "Cancel" button for long operations
- Efficient algorithms:
  - Use compiled C++ code where possible (via Rcpp)
  - Parallel processing for bootstrap/permutation tests
- R packages: `data.table`, `dtplyr`, `progressr`, `future`

**UI elements**:

- Progress bar during computation
- Estimated time remaining
- "Cancel analysis" button

---

## 🔬 **5. OncoPath Module Enhancements**

### **Phase 1: Response Evaluation (Sprints 1-2)**

#### **[H] ✅ COMPLETED - iRECIST Support**

*Immune-related response criteria*

**Status**: ✅ Completed 2025-01-05 (Phase 11)
**Files**: `jamovi/irecist.{a,r,u}.yaml`, `R/irecist.b.R`
**Implementation**: See `IMPLEMENTATION_SUMMARY_2025-01-05_Phase11.md`

**Jamovi Data Structure**:

```yaml
Required Variables:
  - patient_id: Identifier
  - assessment_time: Continuous (weeks or months from baseline)
  - target_lesion_sum: Continuous (sum of target lesion diameters, mm)
  - new_lesions: Binary (0=no, 1=yes)
  - non_target_status: Nominal factor (CR, non-CR/non-PD, PD)

Optional Variables:
  - tumor_burden_total: Continuous (for burden tracking)

Expected Data Format (one row per assessment per patient):
| patient_id | assess_time | target_sum | new_lesions | non_target |
|------------|-------------|------------|-------------|------------|
| 001        | 0           | 45         | 0           | non-CR     | # Baseline
| 001        | 8           | 52         | 0           | non-CR     | # iUPD (increase)
| 001        | 16          | 48         | 0           | non-CR     | # Confirmation needed
| 001        | 24          | 42         | 0           | PR         | # iCPD ruled out → iPR
```

**Implementation**:

- iRECIST categories:
  - iCR, iPR, iSD, iUPD (unconfirmed progression), iCPD (confirmed progression)
- Pseudoprogression handling:
  - Flag iUPD cases
  - Require confirmation scan ≥4 weeks later
  - Track iUPD → iCPD vs iUPD → iPR/iSD
- Confirmation requirements:
  - Time window for confirmation (default 4-8 weeks)
  - Visual timeline showing iUPD → confirmation
- R packages: Custom implementation based on iRECIST guidelines
- Reference: Seymour et al. (2017) Lancet Oncology

**Outputs**:

- Response category table (per patient, per timepoint)
- Waterfall plot with iRECIST colors
- Swimmer plot with iUPD events marked
- Time to iCPD (censoring iUPD cases)

---

#### **[M] ✅ COMPLETED - Multi-Lesion RECIST Aggregation**

*Automated best overall response*

**Status**: ✅ Completed 2025-01-05 (Phase 12)
**Files**: `jamovi/recist.{a,r,u}.yaml`, `R/recist.b.R`
**Implementation**: See `IMPLEMENTATION_SUMMARY_2025-01-05_Phase12.md`

**Jamovi Data Structure**:

```yaml
# Long format: one row per lesion per assessment
Required Variables:
  - patient_id: Identifier
  - assessment_time: Continuous
  - lesion_id: Identifier
  - lesion_type: Nominal factor (target, non-target, new)
  - lesion_diameter: Continuous (mm, for target lesions)
  - non_target_status: Nominal factor (present, absent, unequivocal PD)

Expected Data Format (long format):
| patient_id | assess_time | lesion_id | lesion_type | diameter | non_target_status |
|------------|-------------|-----------|-------------|----------|-------------------|
| 001        | 0           | L1        | target      | 25       | NA                |
| 001        | 0           | L2        | target      | 20       | NA                |
| 001        | 0           | L3        | non-target  | NA       | present           |
| 001        | 8           | L1        | target      | 15       | NA                |
| 001        | 8           | L2        | target      | 12       | NA                |
| 001        | 8           | L3        | non-target  | NA       | absent            |
```

**Implementation**:

- Target lesion rules:
  - Sum of diameters (max 5 lesions, 2 per organ)
  - CR: All target lesions disappear
  - PR: ≥30% decrease from baseline
  - PD: ≥20% increase from nadir (+5mm absolute)
- Non-target lesion rules:
  - CR: All non-target lesions disappear
  - Non-CR/Non-PD: Persistence
  - PD: Unequivocal progression
- New lesion detection:
  - Appearance of any new lesion = PD
- Best overall response (BOR):
  - Aggregate across all assessments
  - Confirmation requirements (2 consecutive for CR/PR)
- R packages: Custom RECIST logic

**Outputs**:

- BOR table (one row per patient)
- Lesion-level trajectory plots
- Sum of diameters over time

---

### **Phase 2: Timeline Integration (Sprint 3)**

#### **[H] ✅ COMPLETED - Survival Integration**

*One-click KM from swimmer/waterfall data*

**Jamovi Data Structure**:

```yaml
# Swimmer plot data can generate survival endpoints
Required Variables (existing swimmer data):
  - patient_id: Identifier
  - treatment_start: Date or numeric (time)
  - treatment_end: Date or numeric (time)
  - last_follow_up: Date or numeric (time)
  - event_occurred: Binary (death, progression)

Derived Variables (auto-generated):
  - time_to_event: last_follow_up - treatment_start
  - event: Binary (from event_occurred)

Expected Data Format (one row per patient):
| patient_id | start_date | end_date | last_followup | death | progression |
|------------|------------|----------|---------------|-------|-------------|
| 001        | 2023-01-01 | 2023-06-15 | 2024-01-01  | 0     | 1           |
| 002        | 2023-02-01 | 2023-08-20 | 2023-12-01  | 1     | 1           |
```

**Implementation**:

- Automatic endpoint derivation:
  - PFS: time from start to progression or death
  - OS: time from start to death
  - Time on treatment: start to end dates
  - Duration of response: time from response to progression
- "Generate KM curve" button in swimmer plot analysis:
  - Opens jSurvival module with pre-filled variables
  - Auto-detects time/event variables
  - Seamless handoff
- Landmark analysis:
  - KM from specific landmark time (e.g., 12 weeks)
- R packages: Integration with jSurvival

**UI elements**:

- "Survival Analysis" button in swimmer plot
- Dropdown: Select endpoint (PFS, OS, TTP, DOR)
- Automatic variable mapping

---

#### **[M] Cohort Summary Statistics**

*Descriptive metrics from response data*

**Jamovi Data Structure**:

```yaml
# Same as swimmer/waterfall data
# Summaries computed automatically
```

**Implementation**:

- Median time on treatment (with IQR, range)
- Treatment discontinuation reasons (proportion table)
- Dose reduction statistics:
  - Percentage with any dose reduction
  - Number of dose reductions per patient
  - Reasons for dose reduction
- Landmark/milestone analyses:
  - % alive at 6, 12, 24 months
  - % progression-free at 6, 12, 24 months
- Duration of response:
  - Among responders (CR + PR), median DOR
- R packages: Standard R summarization

**Outputs**:

- Summary statistics table
- Milestone survival table
- DOR forest plot (by subgroup)

---

### **Phase 3: Biomarker Heterogeneity (Sprint 4)**

#### **[M] Spatial Heterogeneity Metrics**

*Intratumoral IHC variation*

**Jamovi Data Structure**:

```yaml
# Long format: multiple regions per patient
Required Variables:
  - patient_id: Identifier
  - region_id: Identifier (e.g., core1, core2, periphery)
  - biomarker_score: Continuous (e.g., H-score, % positivity)

Optional Variables:
  - x_coord: Continuous (spatial x coordinate)
  - y_coord: Continuous (spatial y coordinate)
  - region_type: Nominal factor (core, periphery, invasive front)

Expected Data Format (one row per region per patient):
| patient_id | region_id | x_coord | y_coord | Ki67_score | ER_score |
|------------|-----------|---------|---------|------------|----------|
| 001        | core_1    | 10      | 20      | 25         | 80       |
| 001        | core_2    | 45      | 30      | 30         | 85       |
| 001        | periphery | 80      | 50      | 15         | 90       |
```

**Implementation**:

- Spatial autocorrelation:
  - **Moran's I**: Global spatial autocorrelation (positive/negative/random)
  - **Geary's C**: Local spatial autocorrelation
  - Requires x/y coordinates
- Intratumoral heterogeneity indices:
  - Coefficient of variation (CV) across regions
  - Range (max - min)
  - Shannon entropy
- H-score variance analysis:
  - Between-patient vs within-patient variance
  - ICC for region-level scores
- Visualization:
  - Spatial heatmap (if coordinates available)
  - Region-level boxplots per patient
- R packages: `spdep`, `spatstat`, custom functions

**Outputs**:

- Heterogeneity metrics table (per patient)
- Moran's I statistic with p-value
- Spatial heatmap
- Variance components table

---

#### **[M] IHC QC Panels**

*Quality control for IHC scoring*

**Jamovi Data Structure**:

```yaml
# Inter-rater reliability
Required Variables:
  - specimen_id: Identifier
  - rater1_score: Continuous (scorer 1)
  - rater2_score: Continuous (scorer 2)

# Batch effects
Required Variables:
  - specimen_id: Identifier
  - batch: Nominal factor (staining batch or date)
  - biomarker_score: Continuous

# Control tissue performance
Required Variables:
  - run_date: Date or nominal factor
  - control_type: Nominal factor (positive, negative)
  - expected_result: Nominal factor (positive, negative)
  - observed_result: Nominal factor (positive, negative)
  - intensity_score: Continuous (optional)

Expected Data Format (inter-rater):
| specimen_id | rater1_Ki67 | rater2_Ki67 | rater1_ER | rater2_ER |
|-------------|-------------|-------------|-----------|-----------|
| S001        | 25          | 28          | 80        | 85        |
| S002        | 10          | 12          | 90        | 88        |
```

**Implementation**:

- **Bland-Altman plots**:
  - Difference vs mean plots for scorer agreement
  - Limits of agreement (±1.96 SD)
  - Bias detection (systematic over/underscoring)
- **Intraclass correlation coefficient (ICC)**:
  - ICC(2,1) for absolute agreement
  - ICC(3,1) for consistency
  - 95% confidence intervals
- **Batch effect visualization**:
  - Boxplot of scores by batch
  - Linear regression: score ~ batch
  - Levene's test for variance homogeneity
- **Control tissue QC**:
  - Run chart of control performance over time
  - Failure rate (expected vs observed mismatch)
  - Alert when control fails
- R packages: `psych::ICC()`, `BlandAltmanLeh`, `ggplot2`

**Outputs**:

- Bland-Altman plot
- ICC table
- Batch effect plot with p-value
- Control performance timeline

---

### **Phase 4: Meta-Analysis Enhancements (Sprint 5)**

#### **[M] DTA Meta-Analysis Diagnostics**

*Advanced diagnostic test accuracy meta-analysis*

**Jamovi Data Structure**:

```yaml
Required Variables:
  - study_id: Nominal factor (study identifier)
  - true_positive: Numeric (TP count)
  - false_positive: Numeric (FP count)
  - true_negative: Numeric (TN count)
  - false_negative: Numeric (FN count)

Optional Variables:
  - bias_domain: Nominal factor (QUADAS-2 domains)
  - bias_level: Nominal factor (low, high, unclear)
  - subgroup: Nominal factor (for meta-regression)

Expected Data Format (one row per study):
| study_id | TP | FP | TN | FN | bias_patient_selection | bias_index_test |
|----------|----|----|----|----|------------------------|-----------------|
| Study_A  | 45 | 10 | 90 | 5  | low                    | low             |
| Study_B  | 30 | 15 | 75 | 10 | high                   | unclear         |
```

**Implementation**:

- **Influence diagnostics**:
  - Leave-one-out analysis (rerun meta-analysis excluding each study)
  - Cook's distance for influential studies
  - DFBETAs for each parameter
- **GOSH plots** (Graphical Display of Study Heterogeneity):
  - Scatterplot of all possible meta-analysis subsets
  - Outlier detection via clustering
- **Publication bias**:
  - Deeks' asymmetry test (for DTA)
  - Funnel plot with regression line
  - Egger's test adaptation for DTA
- **Subgroup analysis**:
  - Forest plots by QUADAS-2 risk-of-bias domain
  - Meta-regression on bias domains
  - Sensitivity analysis excluding high-risk studies
- R packages: `meta`, `mada`, `diagmeta`, `dmetatools`

**Outputs**:

- Influence plot (study ID vs Cook's distance)
- GOSH plot
- Deeks' funnel plot with p-value
- Subgroup forest plot by bias domain

---

#### **[L] Systematic Review Tools**

*PRISMA-DTA and QUADAS visualization*

**Jamovi Data Structure**:

```yaml
# PRISMA flow diagram
Required Variables (manual input):
  - stage: Nominal factor (identification, screening, eligibility, included)
  - count: Numeric (number of records at each stage)
  - exclusion_reason: Nominal factor (for excluded studies)

# QUADAS-2
Required Variables:
  - study_id: Nominal factor
  - domain: Nominal factor (patient selection, index test, reference standard, flow/timing)
  - risk_of_bias: Nominal factor (low, high, unclear)
  - applicability_concern: Nominal factor (low, high, unclear)

Expected Data Format (QUADAS-2, one row per study per domain):
| study_id | domain          | risk_of_bias | applicability |
|----------|-----------------|--------------|---------------|
| Study_A  | patient_selection | low        | low           |
| Study_A  | index_test      | low          | high          |
| Study_B  | patient_selection | high       | low           |
```

**Implementation**:

- **PRISMA-DTA flow diagram**:
  - Automated diagram generation
  - Box counts from user input
  - Export as editable diagram
- **QUADAS-2/QUADAS-C visualization**:
  - Risk-of-bias summary plot (traffic light plot)
  - Domain-level bar charts (% low/high/unclear)
  - Study-level table
- R packages: `PRISMAstatement`, `robvis`, custom ggplot

**Outputs**:

- PRISMA flowchart (PNG, PDF)
- QUADAS-2 traffic light plot
- Bias summary table

---

### **Phase 5: Genomic Visualization (Sprint 6)**

#### **[M] Clinical Heatmap Analysis**

*ComplexHeatmap-style jamovi module*

**Jamovi Data Structure**:

```yaml
# Wide format: patients × biomarkers/genes
Required Variables:
  - patient_id: Identifier (row names)
  - biomarker_vars: Multiple continuous or categorical variables (heatmap body)

Optional Variables:
  - annotation_vars: Categorical variables for side annotations (age group, stage, etc.)
  - outcome: Binary or categorical (for color annotation)

Expected Data Format (wide format):
| patient_id | Gene_A | Gene_B | Gene_C | Age_Group | Stage | Outcome |
|------------|--------|--------|--------|-----------|-------|---------|
| P001       | 2.5    | 1.2    | 0.8    | <50       | III   | CR      |
| P002       | 1.8    | 3.4    | 1.5    | >=50      | IV    | PD      |
| P003       | 0.5    | 0.9    | 2.1    | <50       | II    | PR      |
```

**Implementation**:

- Heatmap body:
  - Continuous data: Color gradient (red-white-blue, viridis, etc.)
  - Categorical data: Discrete colors
- Row/column clustering:
  - Hierarchical clustering (complete, average, ward.D2 linkage)
  - Distance metrics: Euclidean, correlation, Manhattan
  - Dendrogram display
- Side annotations:
  - Top/bottom/left/right annotations
  - Clinical variables (age, stage, treatment)
  - Outcome status
  - Color-coded legends
- Export:
  - High-resolution PNG, PDF, SVG
  - Journal-ready dimensions
- R packages: Build on existing vignette, use `ComplexHeatmap`, `pheatmap`, or `heatmaply`
- Files: Create `/R/clinicalheatmap.b.R` (if not exists), update jamovi YAML

**Outputs**:

- Publication-ready heatmap
- Clustering dendrogram
- Legend for annotations

---

#### **[L] Oncoplot/Mutational Landscape**

*Genomic alteration visualization*

**Jamovi Data Structure**:

```yaml
# Long format: one row per alteration per patient
Required Variables:
  - patient_id: Identifier
  - gene: Nominal factor (gene name)
  - alteration_type: Nominal factor (mutation, amplification, deletion, fusion)

Optional Variables:
  - variant_classification: Nominal factor (missense, nonsense, frameshift, etc.)
  - vaf: Continuous (variant allele frequency, 0-1)
  - tmb: Continuous (tumor mutational burden, per patient)

Expected Data Format (long format):
| patient_id | gene  | alteration_type | variant_class | vaf  | tmb |
|------------|-------|-----------------|---------------|------|-----|
| P001       | TP53  | mutation        | missense      | 0.45 | 12  |
| P001       | KRAS  | mutation        | missense      | 0.38 | 12  |
| P002       | EGFR  | amplification   | NA            | NA   | 5   |
| P002       | BRCA1 | deletion        | NA            | NA   | 5   |
```

**Implementation**:

- Oncoplot layout:
  - Rows = genes, Columns = patients
  - Alteration types color-coded
  - Gene alteration frequency (% of patients)
  - Patient alteration count (number of genes altered)
- Co-occurrence/mutual exclusivity:
  - Fisher's exact test for gene pairs
  - Significant pairs highlighted
- Top bar annotations:
  - TMB per patient
  - Clinical variables (stage, outcome)
- Copy-number tracks:
  - Amplification/deletion frequency
- R packages: `maftools`, `GenVisR`, custom ggplot
- Files: Create `/R/oncoplot.b.R`, jamovi YAML files

**Outputs**:

- Oncoplot (genes × patients)
- Co-occurrence heatmap
- Gene alteration frequency bar chart
- TMB distribution plot

---

### **Phase 6: Data & Reporting (Sprint 7)**

#### **[L] Clinical Trial Data Standards**

*CDISC/SDTM compliance*

**Jamovi Data Structure**:

```yaml
# Import SDTM-formatted data
# Automatic mapping of SDTM domains to jamovi analyses

Required Variables (RS domain - tumor response):
  - USUBJID: Subject ID
  - RSSTRESC: Response (CR, PR, SD, PD)
  - RSDTC: Assessment date
  - RSTESTCD: Test code (OVRLRESP, BESRSPI, etc.)

Required Variables (TR domain - tumor results):
  - USUBJID: Subject ID
  - TRLINKID: Lesion link ID
  - TRSTRESC: Result (diameter in mm)
  - TRDTC: Assessment date
```

**Implementation**:

- SDTM domain mappers:
  - RS (Response) → waterfall plot data
  - TR (Tumor Results) → lesion trajectory data
  - TU (Tumor Identification) → lesion inventory
- Schema validation:
  - Check for required SDTM variables
  - Validate controlled terminology
  - Flag missing/invalid data
- Automatic transformation:
  - SDTM → jamovi internal format
  - Date parsing (ISO 8601)
- Missingness heatmaps:
  - Visualize missing data patterns in SDTM domains
- R packages: `Tplyr`, `admiral`, `xportr`

**Outputs**:

- SDTM compliance report
- Missingness heatmap
- Transformed data for jamovi analyses

---

#### **[L] One-Click Reporting**

*Bundled figure + table exports*

**Implementation**:

- Export bundles:
  - Swimmer plot + summary table → DOCX
  - Waterfall plot + response table → DOCX
  - DTA meta-analysis → full report with forest plot, SROC, funnel plot
- Journal-specific formatting:
  - Figure dimensions (single column, double column)
  - Font sizes, colors (grayscale for print)
- Automated captions:
  - Figure legends with method descriptions
  - Table footnotes with statistical tests
- R packages: `officer`, `flextable`, `rvg`

**UI elements**:

- "Export Report" button
- Dropdown: Select journal template
- Checkbox: Include methods section

---

## 🛠️ **6. Cross-Module Infrastructure**

### **Shared Enhancements (Ongoing)**

#### **[H] Consistent Error Handling**

*Standardized validation and user feedback*

**Implementation**:

- Validation messages:
  - Clear, actionable error messages
  - Example: "Variable 'age' contains non-numeric values. Please recode or exclude."
- Input data validators:
  - Check variable types before analysis
  - Detect and warn about missing data patterns
  - Flag outliers (optional)
- User-friendly error reporting:
  - HTML-formatted messages (not raw R errors)
  - Suggestions for fixing issues
- R packages: `assertthat`, `validate`, custom validators

**Example validation**:

```r
if (any(is.na(time_var))) {
  stop("Time variable contains missing values. Please exclude or impute before analysis.")
}
```

---

#### **[M] Performance Optimization**

*Responsive analyses for large datasets*

**Implementation**:

- Checkpoint system:
  - Use `private$.checkpoint()` before expensive operations
  - Allow users to cancel long-running analyses
- Progress indicators:
  - Progress bars for bootstrap, permutation tests, simulation
  - Estimated time remaining
- Cancellation support:
  - "Cancel" button during execution
  - Graceful cleanup after cancellation
- R packages: `progressr`, `future`, `furrr` (for parallelization)

**UI elements**:

- Progress bar widget (built-in to jamovi)
- Status messages ("Processing 1000 bootstrap iterations...")

---

#### **[M] Documentation Harmonization**

*Consistent vignette structure across modules*

**Implementation**:

- Vignette template:
  - Introduction
  - Data structure requirements
  - Step-by-step tutorial with screenshots
  - Interpretation guide
  - References
- Cross-module examples:
  - Workflow: Descriptives → Survival → Decision Analysis
  - Example: Create Table 1 → KM curves → Cost-effectiveness
- Video tutorials:
  - Screen recordings for complex analyses
  - Hosted on YouTube or module website
- R packages: `knitr`, `rmarkdown`, `pkgdown`

**Deliverables**:

- Standardized vignette format
- Cross-module workflow vignettes
- Video tutorial series

---

#### **[L] Internationalization**

*Multi-language support*

**Implementation**:

- Translation framework:
  - Existing i18n files: `inst/i18n/en.json`, `inst/i18n/tr.json`
  - Expand to additional languages (Spanish, Chinese, French, German)
- UI text translation:
  - All labels, titles, descriptions in translation files
  - Dynamic loading based on user locale
- Output translation:
  - Table headers, footnotes, interpretation text
- R packages: jamovi i18n system

**Files to update**:

- Add `inst/i18n/es.json`, `inst/i18n/zh.json`, etc.
- Ensure all new features use translation keys

---

## 📅 **Implementation Timeline**

### **Year 1: Foundation & High-Impact Features**

#### **Q1 (Months 1-3)**

- **meddecide**: Decision Curve Analysis, Markov model enhancements
- **jSurvival**: Fine-Gray regression (CRR) with CIF plots
- **JJStatsPlot**: Critical bug fixes (compressed plots, missing bars)
- **Infrastructure**: Consistent error handling framework

#### **Q2 (Months 4-6)**

- **meddecide**: Clinical prediction model builder with calibration
- **jSurvival**: Time-dependent calibration & validation dashboards
- **ClinicoPathDescriptives**: Comprehensive effect sizes
- **ClinicoPathDescriptives**: Multiple comparison control (FDR, Bonferroni)

#### **Q3 (Months 7-9)**

- **OncoPath**: iRECIST support, multi-lesion RECIST aggregation
- **OncoPath**: Survival integration (one-click KM from swimmer plots)
- **ClinicoPathDescriptives**: One-click export pipelines (DOCX, LaTeX)
- **JJStatsPlot**: Enhanced customization (colors, themes, symbols)

#### **Q4 (Months 10-12)**

- **meddecide**: Cost-effectiveness analysis basics (ICER, CEAC)
- **jSurvival**: Parametric survival models (AFT, Royston-Parmar)
- **ClinicoPathDescriptives**: Biomarker panel dashboard
- **Infrastructure**: Performance optimization (progress bars, checkpoints)

---

### **Year 2: Depth & Integration**

#### **Q1 (Months 13-15)**

- **jSurvival**: Recurrent event models (AG, PWP), frailty models
- **ClinicoPathDescriptives**: Multiple imputation support
- **ClinicoPathDescriptives**: Survey/weighted data analysis
- **Documentation**: Cross-module workflow vignettes

#### **Q2 (Months 16-18)**

- **OncoPath**: DTA meta-analysis diagnostics (GOSH plots, influence)
- **OncoPath**: QUADAS-2 visualization, PRISMA-DTA diagrams
- **JJStatsPlot**: Model coefficient plots, meta-analysis forest plots
- **JJStatsPlot**: Plot combining & layout (patchwork)

#### **Q3 (Months 19-21)**

- **jSurvival**: Multi-state models, cure models
- **OncoPath**: Clinical heatmap analysis (ComplexHeatmap-style)
- **OncoPath**: Spatial heterogeneity metrics (Moran's I, CV)
- **meddecide**: Time-to-event decision analysis (survival DCA)

#### **Q4 (Months 22-24)**

- **OncoPath**: Oncoplot/mutational landscape
- **ClinicoPathDescriptives**: Advanced categorical plots (Marimekko, risk-difference)
- **Cross-module**: Inter-module handoffs (send to jSurvival, etc.)
- **Cross-module**: Audit trail & reproducibility (R code export, session info)
- **Infrastructure**: Internationalization expansion (ES, ZH, FR, DE)

---

## 🎯 **Success Metrics**

### **User Adoption**

- Downloads from jamovi library: Target 10k+ downloads/year by Year 2
- Forum activity: Active user questions and discussions
- Citations: Peer-reviewed publications using ClinicoPath modules

### **Feature Coverage**

- % of requested features implemented: >80% of high-priority items by Year 2
- Module completeness: All modules reach parity with standalone R packages

### **Code Quality**

- Test coverage: >80% for core functions
- No critical bugs: Zero P1 bugs in released versions
- Passing CI/CD: All checks pass on main branch

### **Documentation**

- All new features with vignettes: 100% coverage
- Example datasets: At least 2 per module
- Video tutorials: 1 per major feature

### **Performance**

- Analysis speed: <2s for typical datasets (n=500)
- Progress bars: Displayed for all operations >5s
- Responsiveness: No UI freezing on datasets up to 100k rows

### **Interoperability**

- Seamless data flow: One-click variable transfer between modules
- CDISC compliance: Full support for SDTM oncology domains

---

## 📝 **Development Guidelines**

### **Jamovi Data Structure Principles**

1. **Rectangular data only**: All analyses work with single data frames (rows = cases, columns = variables)
2. **Variable types matter**:
   - Continuous: Numeric measurements (age, biomarker levels)
   - Nominal: Unordered categories (treatment arm, tumor type)
   - Ordinal: Ordered categories (stage I/II/III/IV, grade)
3. **No nested data**: For longitudinal/clustered data, use long format (one row per observation)
4. **Missing data**: Use NA, not special codes (99, -999)
5. **Date handling**: Dates should be numeric (days from baseline) or properly parsed Date objects

### **Implementation Checklist for New Features**

For each new feature:

- [ ] **Data structure**: Document required variables and expected format
- [ ] **`.a.yaml`**: Define options, specify variable types (Data, Variable, Variables)
- [ ] **`.u.yaml`**: Create user interface (dropdowns, checkboxes, sliders)
- [ ] **`.r.yaml`**: Define output structure (tables, images, HTML)
- [ ] **`.b.R`**: Implement analysis logic in `.run()` and `.init()`
- [ ] **Validation**: Check variable types, detect missing data, validate inputs
- [ ] **Error handling**: Clear, actionable error messages
- [ ] **Example data**: Create sample dataset in `data/` directory
- [ ] **Vignette**: Write tutorial with step-by-step instructions
- [ ] **Tests**: Unit tests for core functions (if applicable)
- [ ] **i18n**: Add translation keys to `inst/i18n/en.json`

### **Priority Decision Framework**

When prioritizing features, consider:

1. **Clinical impact**: Does this address a real-world clinical research need?
2. **User demand**: Have users requested this in forums/issues?
3. **Feasibility**: Can this be implemented within jamovi's tabular structure?
4. **Dependencies**: Does this require other features to be built first?
5. **Competitive advantage**: Do other jamovi modules lack this feature?

### **Quality Standards**

- **No errors with `jmvtools::check()`**: All modules must pass jamovi validation
- **No errors with `jmvtools::prepare()`**: Module must compile without errors
- **Documentation**: README.Rmd updated, NEWS.md entries for each version
- **Reproducibility**: All analyses reproducible with provided example data
- **Performance**: Use `private$.checkpoint()` before operations >5s

---

## 🔗 **References & Resources**

### **Jamovi Development**

- Official documentation: `./vignettes/dev.jamovi.org-master`
- R6 class system: <https://r6.r-lib.org/>
- jmvcore documentation: <https://github.com/jamovi/jmvcore>

### **Statistical Methods**

- **Decision Curve Analysis**: Vickers & Elkin (2006) Med Decis Making
- **Fine-Gray Regression**: Fine & Gray (1999) JASA
- **iRECIST**: Seymour et al. (2017) Lancet Oncol
- **TRIPOD**: Collins et al. (2015) BMJ
- **QUADAS-2**: Whiting et al. (2011) Ann Intern Med

### **R Packages**

- Decision analysis: `dcurves`, `heemod`, `dampack`
- Survival: `survival`, `cmprsk`, `riskRegression`, `flexsurv`
- Meta-analysis: `meta`, `mada`, `diagmeta`
- Plotting: `ggplot2`, `ggstatsplot`, `ComplexHeatmap`
- Export: `officer`, `flextable`, `gt`, `gtsummary`

---

## 📌 **Conclusion**

This roadmap provides a comprehensive, **jamovi-compatible** enhancement plan for the ClinicoPath module ecosystem. All features are designed to work within jamovi's tabular data structure, with clear specifications for variable types, data formats, and UI elements.

**Key Principles**:

- ✅ Rectangular data frames only
- ✅ Clear variable type specifications (Continuous, Nominal, Ordinal)
- ✅ Long format for repeated measures/clustered data
- ✅ One-click workflows with intuitive UIs
- ✅ Publication-ready outputs

**Next Actions**:

1. Review and approve roadmap
2. Create GitHub issues for high-priority features
3. Set up project milestones aligned with timeline
4. Begin implementation starting with Q1 Year 1 features

For questions or suggestions, please open an issue on the ClinicoPathJamoviModule repository.

---

## Drafts / Next steps (to implement later)
- jjoncoplot: expose a dedicated result for per-sample mutation burden (currently only in plot logic), and add UI enable/disable logic (e.g., enable `log10TransformTMB` only when `showTMB` is TRUE).

---

## chisqposttest Enhancements (Optional - Production-Ready Function)

**Status**: ✅ Function is production-ready and clinically safe (5/5 stars)
**Notice Pattern**: ✅ Recently refactored to use jmvcore::Notice (10 notices implemented)
**Priority**: Medium (enhancements, not fixes)

### Enhancement 1: Bootstrap Confidence Intervals for Phi Coefficient [M]

**Status**: ⏳ Planned for v0.0.32
**Dependencies**: boot package (suggested, not required)

**Implementation**:
- Add `phi_ci` column to `posthocTable` in chisqposttest.r.yaml
- Add `.calculatePhiCI()` private method in chisqposttest.b.R
- Use BCa bootstrap (999 iterations) for accurate interval estimates
- Handle small samples (n<20) gracefully with "n too small" message

**Files to modify**:
- `jamovi/chisqposttest.r.yaml` (line 127 - add phi_ci column)
- `R/chisqposttest.b.R` (line 900 - add helper method, line 594 - compute CIs)

**Clinical value**: Pathologists can report "Moderate association (φ=0.34, 95% CI [0.21, 0.48])" with precision estimates

**Rationale**: Bootstrap BCa CIs provide accurate intervals without parametric assumptions; ~50ms per comparison for n=100

---

### Enhancement 2: Residuals Interpretation Guidance Panel [H]

**Status**: ⏳ Planned for v0.0.32
**Dependencies**: None (pure HTML)

**Implementation**:
- Add `residualsGuidance` Html output to chisqposttest.r.yaml
- Insert blue-bordered guidance panel before residuals table
- Include clinical example: "If 'Grade 3 × Positive' has residual = +3.2..."
- Explain positive vs negative residuals with cutoff value

**Files to modify**:
- `jamovi/chisqposttest.r.yaml` (line 82 - add new Html output)
- `R/chisqposttest.b.R` (line 1220 - add guidance HTML before residuals)

**Clinical value**: Reduces user confusion about standardized residuals; clinicians understand which cells drive significant associations

**Rationale**: Standardized residuals are powerful but often misinterpreted by non-statisticians; contextual help improves usability

---

### Enhancement 3: Power Analysis Warning for Small Samples [M]

**Status**: ⏳ Planned for v0.0.32
**Dependencies**: pwr package (suggested, not required)

**Implementation**:
- Detect underpowered studies (n<50) after assumptions check
- Calculate required n for 80% power to detect medium effect (φ=0.3, Cohen 1988)
- Add WARNING notice with required sample size
- Fallback to heuristic (≥5 observations per cell) if pwr package unavailable

**Files to modify**:
- `R/chisqposttest.b.R` (line 1726 - add after low expected counts warning)
- DESCRIPTION (add pwr to Suggests)

**Clinical value**: Prevents misinterpretation of null results as "no association" when study is simply underpowered

**Rationale**: Small samples common in pathology studies; users need guidance on Type II error risk

---

### Implementation Priority

**High Priority (Next Release v0.0.32)**:
- ✅ Enhancement 2: Residuals Guidance (no dependencies, high clinical value, low risk)

**Medium Priority (Future Release)**:
- ⏳ Enhancement 3: Power Analysis Warning (helps prevent Type II error misinterpretation)
- ⏳ Enhancement 1: Bootstrap CIs (enhances reporting quality)

**Timeline**: Can be implemented independently or together in ~2 hours total

**Note**: These are OPTIONAL enhancements for an already production-ready function. Current version (with Notice pattern) is ready for clinical use.

---

### Related Documentation

- Systematic check report: `/check-function chisqposttest` (2025-01-13)
- Comprehensive review: `/review-function chisqposttest` (2025-01-13)
- Notice pattern implementation: Completed 2025-01-13 (4 ERROR, 2 STRONG_WARNING, 2 WARNING, 2 INFO)

---

---
