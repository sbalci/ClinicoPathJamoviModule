# ClinicoPathDescriptives

## Descriptives

- reportcat
- summarydata
- tableone

## Data Quality

- benford
- checkdata
- dataquality
- outlierdetection

## Descriptive Plots

- agepyramid
- alluvial
- vartree
- venn

## Comparisons

- chisqposttest
- crosstable

## Data Preparation

- categorize

# jjstatsplot

## All-In-One

- statsplot2

## Categorical × Categorical

- jjbarstats
- jjpiestats
- jjsegmentedtotalbar

## Categorical × Continuous

- advancedraincloud
- jjbetweenstats
- jjdotchart
- jjdotplotstats
- jjwithinstats
- lollipop
- raincloud

## Continuous × Continuous

- hullplot
- jjcorrmat
- jjscatterstats

## Distribution

- jjhistostats
- jjridges
- jwaffle

## Lines / Network

- linechart
- jjarcdiagram

# jsurvival

## ClinicoPath Survival

- multisurvival
- singlearm
- survival
- survivalcont

## Data Preparation

- datetimeconverter
- outcomeorganizer
- timeinterval

## General Statistics

- oddsratio

## Penalized Cox-Regression

- lassocox

# meddecide

## Agreement

- agreement

## Decision

- decision
- decisioncombine
- decisioncompare
- nogoldstandard

## Decision Calculators

- cotest
- decisioncalculator
- sequentialtests

## ROC

- enhancedROC
- psychopdaROC

## Prediction Models

- lassologistic

## Decision Curve Analysis

- decisioncurve

## Power (menuGroup: Power)

- kappaSizeCI
- kappaSizeFixedN
- kappaSizePower

# OncoPath

## IHC Heterogeneity

- ihcheterogeneity

## Diagnostic Meta-Analysis

- diagnosticmeta

## Visualization

- swimmerplot

- waterfall

# check articles

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

You are an expert R-package and jamovi developer and an expert in biostatistics working with pathologists and clinicians.
Development guides is under vignettes folder starting with jamovi_
Critically evaluate FUNC_NAME function.
Is it mathematically and statistically accurate?
Evaluate if data flow is correct. Are arguments from .a.yaml correctly read. Is the data flow in .b.R correct. Are the results displayed in .r.yaml appropriately. Evaluate if .u.yaml is user friendly and contains all necessary options.
Is it ready to be used by clinicians and pathologists?
Is it ready for release?
Evaluate it for clinical, statistical, logical, and mathematical problems.
Fix issues and implement recommendations. Do not remove functionality.

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
! Rscript -e "devtools::check()"
! Rscript -e "pkgdown::build_site()"

Rscript _updateModules.R

/check-function FUNC_NAME
/check-function-full FUNC_NAME
/review-function FUNC_NAME
/fix-function FUNC_NAME
/generate-test-data FUNC_NAME
/update-refs FUNC_NAME
/document-function FUNC_NAME
/jamovify-function FUNC_NAME --apply
/security-audit-function FUNC_NAME

fix issues, implement recommendations and enhancements

---
<!-- prepare an agent team to work on functions: lassointro, lassocox, adaptivelasso, highdimcox, ncvregcox, plscox, grouplasso, sparsegrouplasso, pcacox: -->

prepare an agent team to work on function: lassocox:

- all agents are experts in R-package and jamovi development and biostatistics working with pathologists and clinicians.
- lead agent will orchestrate the team, observe agent process, delegate next work, update the sonograph log file. Will make sure no functionality is removed and favor functionality. finally, it will check if jmvtools::prepare();devtools::document() is error free.
- a sonograph function will record the team's process in a log file.
- one agent will check if the function is mathematically and statistically accurate?
- one agent will check if data flow is correct. Are arguments from .a.yaml correctly read. Is the data flow in .b.R correct. Are the results displayed in .r.yaml appropriately. Evaluate if .u.yaml is user friendly and contains all necessary options.
- one agent will check if it is ready to be used by clinicians and pathologists?
- one agent will check if it is ready for release?
- one agent will suggest improvements.
- one agent will update .u.yaml to make it user friendly. make all relevant features to be together.
- one agent will remove all dummy code and hardcoded values. make them all work with inputs. implement real function instead of placeholders.
- one agent will check if .a.yaml follows vignettes/jamovi_a_yaml_guide.md
- one agent will check if .b.R follows vignettes/jamovi_b_R_guide.md
- one agent will check if .r.yaml follows vignettes/jamovi_r_yaml_guide.md
- one agent will check if .u.yaml follows vignettes/jamovi_u_yaml_guide.md
- one agent will check if .js follows vignettes/jamovi_js_guide.md
- one agent will check if .a.yaml follows vignettes/jamovi_actions_guide.md
- one agent will check if .a.yaml follows vignettes/jamovi_formula_guide.md
- one agent will check if .a.yaml follows vignettes/jamovi_i18n_guide.md
- one agent will check if .a.yaml follows vignettes/jamovi_module_patterns_guide.md
- one agent will check if .a.yaml follows vignettes/jamovi_plots_guide.md
- one agent will check if .a.yaml follows vignettes/jamovi_tables_guide.md
- one agent will run /check-function with the function name and make relevant fixes
- one agent will run /check-function-base with the function name and make relevant fixes
- one agent will run /check-function-full with the function name and make relevant fixes
- one agent will run /review-function with the function name and make relevant fixes
- one agent will run /fix-function with the function name and make relevant fixes
- one agent will run /document-function with the function name and prepare documentation
- one agent will run /generate-test-data with the function name and generate test data
- one agent will run /prepare-translation with the function name and generate translation
- one agent will run /social-media-promo with the function name and generate social media promotion text
- one agent will run /update-refs with the function name and update references

---

Always use available 'skills' when possible. Keep the output organized.
Use these skills to update qmd files, codes,text, analysis, and interpretations in the project:
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

update DESCRIPTION, NEWS, README, and function Roxygen documentations after each implementation or daily.

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

---

## 📋 **Overview**

This roadmap outlines planned enhancements for the ClinicoPath jamovi module ecosystem, designed specifically to work within **jamovi's tabular data structure**. All features are adapted to work with:

- **Rectangular data frames** (rows = observations, columns = variables)
- **Variable types**: Continuous (numeric), Nominal (factor), Ordinal (ordered factor)
- **Case-by-case data** (no longitudinal/nested structures without reshaping)
- **Single dataset per analysis** (no multi-table joins in UI)

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

---

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

---

### **Phase 2: Model Validation (Sprints 3-4)**

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

---

---

## 📈 **3. JJStatsPlot Module Enhancements**

### **Phase 1: Bug Fixes & Polish (Sprint 1)**

---

---

### **Phase 2: Feature Parity (Sprints 2-3)**

---

---

#### **[M] Enhanced Customization**

*User-controlled plot aesthetics*

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
  - Positivity rates (using cutpoints: ER>=1%, PR>=1%, HER2+ per ASCO/CAP)
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

---

---

---

---

### **Phase 2: Timeline Integration (Sprint 3)**

---

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

## 🎯 **CRITICAL IMPROVEMENT AREAS (2026 Q1-Q2)**

### **Priority 1: Technical Debt & Code Quality**

#### **[H] ⚠️ Notice Serialization Migration (URGENT)**

**Status:** 34 of 364 files still using deprecated `insert(999, Notice)` pattern
**Impact:** Serialization errors, potential data loss, unreliable notice display
**Effort:** 2-3 weeks

**Files requiring conversion (see CLAUDE.md for pattern):**

```bash
# Identified files with insert(999, issues:
- clinicalheatmap.b.R
- flexparametric.b.R
- greyzoneroc.b.R
- jjcorrmat.b.R
- jjpiestats.b.R
- jjwithinstats.b.R
- oddsratio.b.R
- ordinalroc.b.R
- precisionrecall.b.R
- psychopdaROC.b.R
- [24 additional files - run grep to identify]
```

**Action items:**

- [ ] Complete conversion using waterfall.b.R as reference template
- [ ] Add `.noticeList`, `.addNotice()`, `.renderNotices()` helper methods
- [ ] Convert Notice objects to HTML output items in .r.yaml
- [ ] Test each converted function with jmvtools::prepare()
- [ ] Update CLAUDE.md status tracking

**Reference:** `docs/NOTICE_TO_HTML_CONVERSION_GUIDE.md`, `R/waterfall.b.R` (complete example)

---

#### **[H] ~~Resolve TODO/FIXME Technical Debt~~** ✅ RESOLVED

**Status:** ✅ Verified 2026-01-04 - No TODO/FIXME/HACK comments found in R/ files
**Audit Result:** All files clean, no unresolved technical debt markers

~~**Files identified:**~~
*All previously listed files have been cleaned or the markers were false positives.*

---

#### **[M] Automated Testing Infrastructure**

**Status:** Test guides exist but limited automated unit tests
**Coverage:** Unknown (estimate <20% based on testthat setup)
**Effort:** 4-6 weeks ongoing

**Current state:**

- ❌ Limited automated testthat suite
- ❌ No CI/CD test automation
- ❌ No coverage reporting

**Recommendations:**

```yaml
Phase 1 (Weeks 1-2): Core function testing
  - Add testthat tests for top 20 most-used functions
  - Focus on:
    - Input validation (variable types, missing data)
    - Edge cases (empty data, single observation, all NA)
    - Output structure (table dimensions, column names)
  - Target: 40% coverage of core modules

Phase 2 (Weeks 3-4): Statistical accuracy validation
  - Compare outputs to validated R packages (survival, pROC, meta)
  - Test against published datasets with known results
  - Validate effect sizes, CIs, p-values
  - Target: 100% accuracy for statistical calculations

Phase 3 (Weeks 5-6): CI/CD integration
  - Set up GitHub Actions workflow
  - Run tests on every PR and merge to main
  - Generate coverage reports (codecov.io)
  - Block merges if tests fail or coverage drops
```

**Example test structure:**

```r
# tests/testthat/test-enhancedROC.R
test_that("enhancedROC handles binary outcome correctly", {
  data <- data.frame(
    outcome = factor(c(rep("disease", 50), rep("healthy", 50))),
    predictor = c(rnorm(50, mean = 1), rnorm(50, mean = 0))
  )

  result <- enhancedROC(data, outcome = outcome, predictors = predictor)

  expect_true(result$auc > 0.5)  # Should discriminate
  expect_equal(nrow(result$rocTable), 1)
  expect_named(result$rocTable, c("predictor", "auc", "ci_lower", "ci_upper"))
})
```

**Priority test coverage:**

1. enhancedROC - comprehensive ROC analysis
2. survival - Kaplan-Meier and Cox regression
3. decisioncurve - decision curve analysis
4. diagnosticmeta - meta-analysis
5. conttables - contingency tables with effect sizes

---

### **Priority 2: Documentation & User Experience**

#### **[H] ⚠️ Severe Documentation Gap**

**Status:** Only 6 vignettes for 364 analysis functions (1.6% coverage!)
**Impact:** Users cannot learn/use 98%+ of module features
**Effort:** 8-12 weeks (phased approach required)

**Current vignette inventory:**

```bash
# Existing (6 total):
- General vignettes (domain-based distribution working well)
- Module-specific guides scattered across vignettes/
```

**Phased documentation plan:**

**Phase 1 - High-Impact Quick Wins (Weeks 1-2):**
Create comprehensive guides for top 10 most-used functions:

```
Priority vignettes needed:
1. enhancedROC-comprehensive.qmd - Diagnostic ROC analysis
2. survival-comprehensive.qmd - Kaplan-Meier & Cox regression
3. decisioncurve-comprehensive.qmd - Clinical decision analysis
4. crosstable-comprehensive.qmd - Table One generation
5. conttables-comprehensive.qmd - Contingency tables
6. diagnosticmeta-comprehensive.qmd - Meta-analysis
7. waterfall-comprehensive.qmd - Treatment response plots
8. swimmer-comprehensive.qmd - Patient timelines
9. oddsratio-comprehensive.qmd - Logistic regression
10. agreement-comprehensive.qmd - Inter-rater reliability
```

**Phase 2 - Modular Function Groups (Weeks 3-6):**

```
Survival module cluster (jsurvival-XX-*.qmd):
- jsurvival-01-kaplan-meier.qmd
- jsurvival-02-cox-regression.qmd
- jsurvival-03-competing-risks.qmd
- jsurvival-04-time-dependent-covariates.qmd

ROC/Diagnostic cluster (meddecide-XX-*.qmd):
- meddecide-01-basic-roc.qmd
- meddecide-02-comparative-roc.qmd
- meddecide-03-calibration.qmd
- meddecide-04-decision-curves.qmd

[Repeat for each of 5 main modules]
```

**Phase 3 - Workflow Tutorials (Weeks 7-8):**

```
End-to-end clinical research workflows:
- workflow-01-biomarker-validation.qmd
  (Descriptives → ROC → Decision curve → Publication export)

- workflow-02-survival-analysis.qmd
  (KM curves → Cox regression → Validation → Reporting)

- workflow-03-diagnostic-meta-analysis.qmd
  (Data preparation → Meta-analysis → Publication bias → GRADE)
```

**Vignette template structure:**

```markdown
---
title: "Function Name - Comprehensive Guide"
author: "ClinicoPath Team"
date: "`r Sys.Date()`"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Function Name - Comprehensive Guide}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

## Clinical Use Case
[Why would a pathologist/clinician use this?]

## Data Requirements
[Required variables, format, sample size considerations]

## Step-by-Step Tutorial
[Numbered steps with screenshots]

## Interpreting Results
[How to read each output table/plot]

## Clinical Examples
[2-3 real-world scenarios with interpretation]

## Statistical Details
[Methods, assumptions, limitations]

## References
[Key papers, guidelines]
```

**Automation opportunities:**

- Generate skeleton vignettes from .a.yaml analysis definitions
- Auto-extract option descriptions from YAML to reduce manual writing
- Use AI to draft initial content, human review for accuracy

---

#### **[M] Clinical Presets System Expansion**

**Status:** Only enhancedROC has clinicalPresets feature (massive success!)
**Opportunity:** Expand to 15-20 additional high-value analyses
**Impact:** Reduces cognitive load, prevents misconfiguration, speeds workflow
**Effort:** 3-4 weeks

**Current implementation (enhancedROC):**

```yaml
# jamovi/enhancedROC.a.yaml
- name: clinicalPresets
  type: List
  options:
    - title: 'Custom Configuration'
      name: custom
    - title: 'Biomarker Screening (High Sensitivity)'
      name: biomarker_screening
    - title: 'Diagnostic Test Validation (Balanced)'
      name: diagnostic_validation
    - title: 'Confirmatory Testing (High Specificity)'
      name: confirmatory_testing
    - title: 'Research Analysis (Comprehensive)'
      name: research_comprehensive
```

**Target modules for preset expansion:**

1. **survival.b.R** - Survival analysis presets:

   ```
   - Early-stage cancer (5-year follow-up focus)
   - Advanced cancer (short-term outcomes)
   - Screening cohort (long follow-up, few events)
   - Clinical trial (strict proportional hazards checks)
   ```

2. **diagnosticmeta.b.R** - Meta-analysis presets:

   ```
   - QUADAS-C compliant (cancer screening)
   - High heterogeneity expected (subgroup focus)
   - Publication bias concern (extensive diagnostics)
   - Network meta-analysis ready
   ```

3. **decisioncurve.b.R** - Decision curve presets:

   ```
   - Screening decision (low threshold range 0.01-0.10)
   - Diagnostic decision (mid threshold 0.10-0.50)
   - Treatment decision (high threshold 0.30-0.80)
   ```

4. **conttables.b.R** - Table One presets:

   ```
   - Randomized trial (balance checking, SMD)
   - Observational study (full covariate adjustment)
   - Case-control study (matched pairs emphasis)
   - Diagnostic accuracy (sensitivity/specificity focus)
   ```

5. **oddsratio.b.R** - Logistic regression presets:

   ```
   - Prediction model development (calibration priority)
   - Risk factor identification (parsimonious model)
   - External validation (performance metrics only)
   ```

**Implementation pattern (from enhancedROC.b.R):**

```r
.applyClinicalPresets = function() {
  preset <- self$options$clinicalPresets
  if (is.null(preset) || preset == "custom") return()

  private$.presetConfig <- switch(preset,
    biomarker_screening = list(
      sensitivityThreshold = 0.90,
      specificityThreshold = 0.60,
      youdenOptimization = TRUE,
      rocCurve = TRUE,
      # ... preset configuration
    ),
    # ... other presets
  )
}
```

**JavaScript UI automation** (see CLAUDE.md reference):

```javascript
// jamovi/js/enhancedROC.events.js
onUpdate: function(ui) {
  let preset = ui.clinicalPresets.value();

  if (preset === 'biomarker_screening') {
    ui.sensitivityThreshold.setValue(0.90);
    ui.specificityThreshold.setValue(0.60);
    ui.youdenOptimization.setValue(true);
    ui.rocCurve.setValue(true);
  }
  // ... handle other presets
}
```

---

#### **[M] Consistent Variable Name Handling**

**Status:** Mixed implementation - some modules escape, others don't
**Issue:** Variables with spaces/special characters break in plots/tables
**Files affected:** Unknown (requires systematic audit)
**Effort:** 2-3 weeks

**Problem examples:**

```
User variable names with issues:
- "Age at Diagnosis" (space)
- "ER+" (special character)
- "Stage (AJCC 8th)" (parentheses + space)
- "Ki-67 (%)" (hyphen + parentheses)
```

**Current best practice (from oddsratio.b.R):**

```r
# Apply labelled variable logic to preserve original names
.escapeVar = function(x) {
  if (is.character(x)) {
    x <- gsub("[^A-Za-z0-9_]", "_", make.names(x))
  }
  return(x)
}

# Use jmvcore helpers
lhs <- jmvcore::composeTerm(self$options$dep)
rhs <- jmvcore::composeTerms(modelTerms)
```

**Systematic fix required:**

1. Audit all 364 .b.R files for variable name handling
2. Implement `.escapeVar()` consistently across modules
3. Preserve original names in output using `labelled` package
4. Test with pathology datasets (known to have complex variable names)

**See:** `vignettes/jamovi_module_patterns_guide.md` - Data Handling section

---

### **Priority 3: Feature Enhancements**

#### **[H] enhancedROC Feature Extensions**

**Status:** Comprehensive but missing key clinical workflows
**Opportunity:** Build on successful foundation
**Effort:** 2-3 weeks

**Proposed enhancements:**

1. **Multi-marker combination strategies:**

   ```
   Options to add:
   - Simple sum/average of markers
   - Logistic regression combination
   - Machine learning ensemble (random forest, XGBoost)
   - Clinical algorithm (if marker1 > X, then marker2)
   ```

2. **Time-dependent ROC integration:**

   ```
   # Currently commented out in enhancedROC.a.yaml (lines 544-568)
   # IMPLEMENT THIS - high clinical value!

   Use cases:
   - Biomarker measured at baseline, predict 1/3/5-year survival
   - Validate prognostic scores (Oncotype DX, PREDICT, etc.)
   - Time-varying AUC plots
   ```

3. **Automated reporting enhancements:**

   ```
   Current: Plain text summary
   Proposed:
   - STARD-compliant checklist export
   - Copy-ready methods section
   - Copy-ready results paragraph
   - Journal-formatted table (NEJM, Lancet, JAMA styles)
   ```

4. **Clinical decision integration:**

   ```
   Link to decisioncurve module:
   - "Calculate net benefit" button → pre-fills decisioncurve analysis
   - Automatic threshold recommendation based on clinical context
   - Cost-effectiveness integration (if costs provided)
   ```

5. **Expand clinical presets:**

   ```
   Additional presets needed:
   - PD-L1 scoring (ASCO/CAP guidelines)
   - HER2 testing (binary + continuous IHC)
   - ctDNA detection (ultra-high specificity required)
   - Liquid biopsy validation
   ```

---

#### **[M] Cross-Module Workflow Integration**

**Status:** Modules work independently but no seamless handoffs
**Opportunity:** Reduce copy-paste, errors, and user frustration
**Effort:** 4-5 weeks

**Proposed integration points:**

1. **Descriptives → Survival:**

   ```
   crosstable.b.R / summary.b.R:
   - Add "Send to jSurvival" button
   - Pre-populate time/event variables if detected
   - Transfer grouping variables automatically
   ```

2. **ROC → Decision Curves:**

   ```
   enhancedROC.b.R:
   - "Evaluate clinical utility" button
   - Passes predicted probabilities to decisioncurve
   - Links optimal cutoff to threshold probabilities
   ```

3. **Swimmer → Survival:**

   ```
   swimmer.b.R:
   - "Generate KM curve" button
   - Auto-derives PFS/OS from swimmer data
   - Transfers treatment arms as strata
   ```

4. **Table One → Export:**

   ```
   crosstable.b.R:
   - One-click DOCX export (CONSORT-style)
   - Embedded footnotes with statistical methods
   - Automatic STROBE checklist generation
   ```

**Implementation approach:**

```r
# Add to .b.R files:
.exportToModule = function(targetModule) {
  # Prepare data in format expected by target module
  transferData <- list(
    variables = self$options$selectedVars,
    data = private$.data,
    options = list(...)  # Pre-configured options
  )

  # Use jamovi session state to pass data
  self$results$.setExportData(transferData)

  # Trigger target module (requires jamovi API enhancement)
  # OR: Export as temporary .omv file that user opens
}
```

---

#### **[L] Performance Optimization for Large Datasets**

**Status:** No systematic optimization; users report slowness with >10k rows
**Target:** Sub-2 second response for n=10,000; sub-10s for n=100,000
**Effort:** 3-4 weeks

**Optimization strategies:**

1. **Lazy evaluation & caching:**

   ```r
   # Only recompute when options change
   private$.cache <- list()

   .getCachedResult = function(key, computeFn) {
     if (is.null(private$.cache[[key]])) {
       private$.cache[[key]] <- computeFn()
     }
     return(private$.cache[[key]])
   }
   ```

2. **Progress indicators for long operations:**

   ```r
   # For bootstrap, permutation, cross-validation
   .runBootstrap = function(nIter = 1000) {
     for (i in 1:nIter) {
       # Checkpoint every 100 iterations for cancellation
       if (i %% 100 == 0) {
         private$.checkpoint()
       }
       # ... bootstrap iteration
     }
   }
   ```

3. **Data.table for aggregation:**

   ```r
   # Replace dplyr for large datasets
   library(data.table)

   .aggregateLargeData = function(data) {
     dt <- as.data.table(data)
     dt[, .(mean = mean(value), sd = sd(value)), by = group]
   }
   ```

4. **Parallel processing for independence:**

   ```r
   # Use future/furrr for bootstrap
   library(future)
   library(furrr)

   plan(multisession, workers = 4)

   bootstrap_results <- future_map(1:1000, ~{
     # Bootstrap iteration
   }, .options = furrr_options(seed = TRUE))
   ```

---

### **Priority 4: Quality Assurance & Standards**

#### **[H] Statistical Accuracy Validation**

**Status:** No systematic validation against reference implementations
**Risk:** Incorrect results damage reputation, mislead clinical decisions
**Effort:** 3-4 weeks (one-time audit + ongoing testing)

**Validation protocol:**

1. **Benchmark against validated packages:**

   ```r
   # Test survival.b.R against survival package
   test_that("Cox regression matches survival::coxph", {
     library(survival)

     # Use built-in lung dataset
     data(lung)

     # ClinicoPath result
     cp_result <- survival(data = lung, time = time,
                           event = status, covariates = c(age, sex))

     # Reference result
     ref_result <- coxph(Surv(time, status) ~ age + sex, data = lung)

     # Compare coefficients (within floating point tolerance)
     expect_equal(cp_result$coef, coef(ref_result), tolerance = 1e-6)
     expect_equal(cp_result$hr, exp(coef(ref_result)), tolerance = 1e-6)
   })
   ```

2. **Validate against published datasets:**

   ```
   Use canonical datasets with known results:
   - Mayo Clinic lung cancer (survival analysis)
   - Framingham Heart Study (logistic regression)
   - Scottish thyroid cancer (competing risks)
   - Kidney function eGFR (ROC curves)

   Compare ClinicoPath outputs to published papers
   ```

3. **Edge case testing:**

   ```r
   test_that("survivalhandlesedgecases",{
     # Single event
     # All censored
     # Ties in event times
     # Missing covariates
     # Zero variance covariate
     # Perfect separation in logistic regression
   })
   ```

---

#### **[M] UI/UX Consistency Audit**

**Status:** 364 analyses created over time → inconsistent patterns
**Impact:** Confusing for users, increases learning curve
**Effort:** 2 weeks audit + 3-4 weeks fixes

**Inconsistencies to address:**

1. **Option naming conventions:**

   ```
   Current problems:
   - Some use camelCase, others use snake_case
   - Inconsistent abbreviations (CI vs ci vs confInt)
   - Unclear labels ("Advanced options" vs "Statistical options")

   Standard to adopt:
   - All options: camelCase (jamovi convention)
   - All titles: Title Case with Full Words
   - Grouping: "Analysis Options", "Output Options",
              "Statistical Settings", "Plot Settings"
   ```

2. **Checkbox defaults:**

   ```
   Current: Mixed (some analyses default to many outputs, causing slowness)
   Recommended:
   - Core tables: TRUE by default
   - Advanced tables: FALSE by default
   - Plots: FALSE by default (user opts in)
   - Diagnostic plots: FALSE by default
   ```

3. **CollapseBox organization:**

   ```
   Standard structure for all analyses:
   1. Variable Selection (always first, never collapsed)
   2. Analysis Options (collapsed: false for simple analyses)
   3. Output Options (collapsed: false)
   4. Statistical Options (collapsed: true)
   5. Plot Settings (collapsed: true)
   6. Advanced/Experimental (collapsed: true)
   ```

4. **Help text standardization:**

   ```
   Every option should have description in .a.yaml:

   - name: bootstrapSamples
     title: 'Bootstrap Samples'
     type: Integer
     default: 1000
     min: 100
     max: 10000
     description: 'Number of bootstrap resamples for confidence intervals. Higher values increase precision but slow computation. Recommended: 1000 for exploratory, 5000+ for publication.'
   ```

---

### **Priority 5: Community & Adoption**

#### **[M] Example Dataset Repository**

**Status:** Scattered example data, unclear provenance
**Need:** Curated, documented, clinically realistic datasets
**Effort:** 2-3 weeks

**Proposed structure:**

```
data/
├── README.md (dataset catalog)
├── breast_ihc_validation.csv
│   └── Description: 500 patients, ER/PR/HER2/Ki67 IHC scores
│       Use cases: enhancedROC, agreement, ihccluster
├── lung_cancer_survival.csv
│   └── Description: 228 patients from Mayo Clinic lung cancer study
│       Use cases: survival, cox, competingrisks
├── diagnostic_meta_thyroid.csv
│   └── Description: 24 studies of thyroid FNA diagnostic accuracy
│       Use cases: diagnosticmeta, meta-analysis
├── recist_trial_lesions.csv
│   └── Description: Synthetic trial data with lesion measurements
│       Use cases: waterfall, swimmer, recist
└── ... [15-20 total datasets]

data-raw/
├── generate_breast_ihc.R (data generation scripts)
├── generate_lung_survival.R
└── ...
```

**Dataset requirements:**

- Realistic clinical variable names (with spaces, special characters)
- Missing data patterns typical of real studies
- Adequate sample size for statistical power
- Documented data dictionary
- Clear provenance (simulated vs real de-identified)
- Covers all major module functions

---

#### **[L] Video Tutorial Series**

**Status:** No video content
**Platform:** YouTube (ClinicoPath channel)
**Effort:** 4-6 weeks (1-2 videos/week)

**Proposed series (10 videos, 5-15 min each):**

1. **Getting Started with ClinicoPath** (10 min)
   - Installing jamovi + ClinicoPath
   - Interface overview
   - Loading example data
   - Running first analysis (Table One)

2. **Creating Publication-Ready Table One** (12 min)
   - crosstable module
   - Selecting variables
   - Statistical tests
   - Export to DOCX

3. **Survival Analysis Fundamentals** (15 min)
   - Kaplan-Meier curves
   - Log-rank test
   - Cox regression basics
   - Interpreting hazard ratios

4. **ROC Curve Analysis for Biomarker Validation** (15 min)
   - enhancedROC module
   - Youden index optimization
   - Comparing multiple markers
   - Clinical interpretation

5. **Decision Curve Analysis** (12 min)
   - decisioncurve module
   - Net benefit interpretation
   - Threshold selection
   - Clinical decision making

6. **Diagnostic Test Meta-Analysis** (15 min)
   - diagnosticmeta module
   - Sensitivity/specificity pooling
   - SROC curves
   - Heterogeneity assessment

7. **Treatment Response Visualization** (10 min)
   - waterfall plots
   - swimmer plots
   - RECIST integration

8. **Agreement & Reliability Analysis** (12 min)
   - Cohen's kappa
   - ICC for continuous measures
   - Bland-Altman plots

9. **Advanced Survival: Competing Risks** (15 min)
   - competingsurvival module
   - Cumulative incidence functions
   - Fine-Gray regression

10. **End-to-End Workflow: Biomarker Study** (20 min)
    - Data import → Descriptives → ROC → Decision curves → Export
    - Publication-ready outputs

**Recording setup:**

- Screen recording (1080p minimum)
- Clear narration with clinical context
- Closed captions (accessibility)
- Time-stamped chapters
- Accompanying written transcript

---

## 📌 **IMPLEMENTATION ROADMAP UPDATE (2026)**

### **Q1 2026 (Jan-Mar): Critical Fixes & Foundation**

**Week 1-2:**

- [ ] Complete notice serialization migration (34 remaining files)
- [ ] Resolve critical TODO/FIXME issues (P0 bugs only)

**Week 3-4:**

- [ ] Create automated test framework
- [ ] Write tests for top 5 functions (enhancedROC, survival, decisioncurve, crosstable, diagnosticmeta)

**Week 5-8:**

- [ ] Write 10 high-priority comprehensive vignettes
- [ ] Launch first 2 video tutorials (Getting Started + Table One)

**Week 9-12:**

- [ ] Expand clinical presets to 5 additional modules
- [ ] Implement cross-module integration (3 key workflows)
- [ ] Curate and document 10 example datasets

### **Q2 2026 (Apr-Jun): Enhanced UX & Quality**

**Week 1-3:**

- [ ] UI/UX consistency audit across all 364 functions
- [ ] Standardize option naming, defaults, help text
- [ ] Implement consistent CollapseBox organization

**Week 4-6:**

- [ ] Statistical accuracy validation (benchmark top 20 functions)
- [ ] Performance optimization (n=100k target)
- [ ] Add progress indicators to long-running analyses

**Week 7-9:**

- [ ] Variable name handling standardization (all modules)
- [ ] Systematic .escapeVar() implementation
- [ ] Test with real pathology datasets (complex names)

**Week 10-12:**

- [ ] Complete remaining 8 video tutorials
- [ ] Write 10 additional vignettes (cumulative: 20 total)
- [ ] CI/CD integration with automated testing

**Deliverables:**

- [ ] Consistent UI/UX across all modules
- [ ] 80% test coverage for statistical calculations
- [ ] <2s response time for n=10,000 rows
- [ ] 20 comprehensive vignettes (5.5% coverage)
- [ ] 10 video tutorials complete
- [ ] GitHub Actions CI/CD running

---

## 📊 **SUCCESS METRICS (Updated 2026)**

### **Technical Quality**

- [ ] **Zero** serialization errors in released version
- [ ] **<5** open P0/P1 bugs at any time
- [ ] **80%+** test coverage for core statistical functions
- [ ] **100%** accuracy vs reference implementations
- [ ] **All** 364 functions pass jmvtools::prepare()

### **Documentation**

- [ ] **20+** comprehensive vignettes by Q2 2026 (target: 50+ by year-end)
- [ ] **10** video tutorials by Q2 2026
- [ ] **15+** curated example datasets
- [ ] **100%** of new functions documented before release

### **User Experience**

- [ ] **10+** modules with clinical presets
- [ ] **<2s** analysis response for typical datasets (n <= 10,000)
- [ ] **Consistent** UI patterns across all modules
- [ ] **5+** cross-module workflow integrations

### **Adoption & Impact**

- [ ] **15k+** jamovi library downloads/year (up from 10k target)
- [ ] **50+** citations in peer-reviewed literature
- [ ] **100+** active forum discussions
- [ ] **4.5+/5** user satisfaction rating

---

## 📌 **Conclusion**

This roadmap provides a comprehensive, **jamovi-compatible** enhancement plan for the ClinicoPath module ecosystem. All features are designed to work within jamovi's tabular data structure, with clear specifications for variable types, data formats, and UI elements.

**CRITICAL NEXT STEPS (Start Immediately):**

1. [ ] Complete notice serialization migration (34 files)
2. [ ] Write first 10 comprehensive vignettes
3. [ ] Implement automated testing for top 20 functions
4. [ ] Expand clinical presets to 5 key modules
5. [ ] Create 10 curated example datasets

**Key Principles**:

- Rectangular data frames only
- Clear variable type specifications (Continuous, Nominal, Ordinal)
- Long format for repeated measures/clustered data
- One-click workflows with intuitive UIs
- Publication-ready outputs

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

**Status**: Function is production-ready and clinically safe (5/5 stars)
**Notice Pattern**: Recently refactored to use jmvcore::Notice (10 notices implemented)
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

### /check-function 2026-09-05 (standard profile)

**Done**: Enhancement 1 above is already shipped (`phiCI` option, `.calculatePhiCI()`, `phi_ci` column). Four `.()` fragment
splices (leading-space strings, `'`/`' and '` pieces) rewritten as whole sentences; end-to-end regression test for
variable names with spaces/punctuation/Unicode on both the raw-rows and the weighted `counts` (xtabs formula) paths;
`menuGroup` routed to `ExplorationT` for JamoviTest (move back after testing).

### /fix-function 2026-09-05

**Fixed** (regression tests in `test-chisqposttest-release-review.R`):

- [bug] `.validateAssumptions()` overwrote a "severe" expected-count level with "moderate" whenever n < 20, so the
  panel colour and severity were wrong for exactly the tables that trigger both. Now escalate-only.
- [bug] Fisher wording claimed a low-expected-count fallback even with "Always Fisher's exact" selected
  (`multipleTestingInfo` notice and the detailed-comparison "Method:" line).
- [wording] Clinical summary printed the option key ("after fdr correction"); shared `.posthocMethodLabel()` now
  feeds both the summary and the educational panel.
- [ux] `weightedDataInfo` is written at the top of `.run()` whenever `counts` is set, so the `visible: (counts)`
  panel is never a titled empty box.
- [i18n] All remaining user-facing literals wrapped in `.()` (error divs, both `reject()` messages, post-hoc
  messages, comparison section, `.run()` warnings, export table, residual interpretations, plot title/subtitle).
- [minor] `.plot()` returns `FALSE`, not `NULL`, when the plot option is off.

**Follow-ups (out of scope)**:

- [ ] [i18n] Catalog refresh (`jmvtools::i18nUpdate`) so `catalog.pot`/`tr.po` pick up the ~40 new msgids. Not run
      here: it regenerates the module-wide catalogs, which another session is also touching.
- [ ] [i18n] `test_used` ("Chi-square", "Fisher's exact", "Fisher's exact (Monte Carlo)") doubles as a logic key
      (`startsWith(x, "Fisher")`) and as the Test Method column text, so it is deliberately untranslated. Add a
      separate display label if translated column text is wanted.
- [ ] [reporting] The copy-ready Methods sentence never mentions the pairwise tests or the adjustment method, while
      the Results sentence does; add one clause when `posthoc != "none"`.

### /check-function-full 2026-09-05 (audit, then "fix detected issues")

**Fixed**: STRONG_WARNING notice for an expected count below 1 when the 20%-of-cells rule does not fire; the four
error paths now push an ERROR notice through a shared `.errorBox()` as well as the red `todo` box; WARNING for
non-integer `counts`; one-line INFO "Analysis summary" closes every run (stale serialization comment removed);
invalid `border: 1px border` CSS; three dark accent headings changed to `color: inherit`; `.a.yaml` example now
uses `Grade_Level x Group` (p = 0.010, post-hoc table populated) and the description no longer claims there is
no validation against established packages. Regression tests appended to `test-chisqposttest-release-review.R`.

**Not fixed**: `chisqposttest(data, rows = NULL, cols = NULL, counts = NULL)` from R errors inside
`jmvcore::select()` before the welcome message; that is jmvcore behaviour, not this module.

**Note**: the audit's "no vignette" finding was a tooling error (a failed zsh glob aborted the grep);
`vignettes/explorationt-chisqposttest-comprehensive.Rmd` exists and uses current option names.

### /review-function 2026-09-05

**Fixed**: hardcoded `set.seed(42)` in `.calculatePhiCI()` replaced by a `seed` Integer option (default 42, `.a.yaml`
- `.u.yaml` TextBox enabled by `phiCI` + `posthocTable` clearWith); `is.null()` fallback keeps 42 until
`jmvtools::prepare()` regenerates the header and wrapper. Unused `total_comparisons` assignment removed from
`.robustPairwiseTestsChunked()`.

**Pending prepare()**: `seed` option, the new example, and the reworded description all sit in the yaml only; the
R wrapper gains `seed = 42` after the next `prepare()` + `document()`. Then verify: same seed gives an identical
bootstrap interval, a different seed still runs.

**Follow-ups**:

- [ ] [i18n] tr.po has 11 of 217 `.()` strings translated and 88 not yet in the catalog; run the catalog refresh and
      `/prepare-translation chisqposttest`.
- [ ] [reporting] Pairwise table has no df column; a 2xC sub-table has C-1 df, so the chi-square statistic alone is
      hard to read. Add `df` next to `chi`.
- [ ] [perf] `.generateClinicalSummary()` runs twice when both the clinical summary and report sentences are on;
      compute once in `.run()` and pass to both.
- [ ] [lint] `quotes_linter` (single quotes) fires 15x, all in the generated-style `requireNamespace('jmvcore')`
      line and `asSource()`; style only.

### /release-review-function 2026-09-05

**Verdict**: ready after the user runs `jmvtools::prepare()` + `devtools::document()` (yaml edits pending: `seed`
option, example, description, `version: '1.0.9'`, `posthocTable` clearWith `seed`).

**Independently verified** (scratch `verify_chisqposttest.R`, not committed): omnibus chi-square and p vs hand
formula; adjusted residual vs Agresti eq. 2.4.5; Bonferroni-over-cells critical z vs `qnorm`; all six Holm
adjusted p vs the step-down rule; pairwise 2x3 chi-square and effect size vs hand formula; Cramer's V vs
`vcd::assocstats`; Fisher p vs `stats::fisher.test`; bootstrap CI contains the point estimate; power text vs
`pwr::pwr.chisq.test`. All match.

**Gates passed**: class name, Collate, case duplicates, file naming test, no committed .jmo/.tar.gz, refs resolve,
no duplicate .u.yaml names, headless UI render, rendering contract, state guards, theme, entities.

- [ ] [refs] `agresti2013` in `00refs.yaml` has no `url:`; the other six cited refs do. Add the publisher or DOI link.

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
- Fallback to heuristic (>=5 observations per cell) if pwr package unavailable

**Files to modify**:

- `R/chisqposttest.b.R` (line 1726 - add after low expected counts warning)
- DESCRIPTION (add pwr to Suggests)

**Clinical value**: Prevents misinterpretation of null results as "no association" when study is simply underpowered

**Rationale**: Small samples common in pathology studies; users need guidance on Type II error risk

---

### Implementation Priority

**High Priority (Next Release v0.0.32)**:

**Medium Priority (Future Release)**:

- ⏳ Enhancement 3: Power Analysis Warning (helps prevent Type II error misinterpretation)
- ⏳ Enhancement 1: Bootstrap CIs (enhances reporting quality)

**Timeline**: Can be implemented independently or together in ~2 hours total

**Note**: These are OPTIONAL enhancements for an already production-ready function. Current version (with Notice pattern) is ready for clinical use.

---

### Related Documentation

- Systematic check report: `/check-function chisqposttest` (2025-01-13)
- Comprehensive review: `/review-function chisqposttest` (2025-01-13)

---

---

# Deferred gaps from Balarajah-2026 review (2026-05-07)

Source: [literature/Balarajah-2026-Duodenal-adenocarcinoma-biomarkers-UK-cohort-citation-review.md](literature/Balarajah-2026-Duodenal-adenocarcinoma-biomarkers-UK-cohort-citation-review.md)

The first wave of fixes (Miller–Halpern + Monte-Carlo in `optimalcutpoint`,
panel-wide FDR in `ihcsurvival`, EPV warning + default Schoenfeld in
`multisurvival`) is implemented. The items below remain — each is scoped
as a focused, testable change.

## 1. Centre / cluster frailty option in `multisurvival`

**Why**: 7-centre cohorts (Balarajah 2026, BALLAD, J-BALLAD) need either a
random-effect frailty term or stratified Cox; ignoring centre clustering
underestimates HR standard errors. ClinicoPath has `frailtysurvival` but
it is a separate analysis card, not an option on the standard
multivariable Cox.

**How to apply**:

- `jamovi/multisurvival.a.yaml` — add a new `cluster` Variable option and a
  `cluster_method` List (`none` / `cluster_robust` / `frailty_gamma` /
  `frailty_gaussian` / `stratified`).
- `R/multisurvival.b.R` — extend the `coxformula` builder so the cluster
  term is appended via `cluster()` / `frailty()` / `strata()` depending on
  `cluster_method`. Touch carefully — the formula path interacts with
  Fine-Gray (`survival::finegray`), `finalfit::coxphmulti`, and the
  survminer/forestplot rendering, which all need the additional term to
  survive the round trip.
- Output: surface frailty variance + LR test in a new small table next to
  the main HR table.
- Tests: simulate 7-centre data with frailty variance ≈ 0.3; verify naïve
  Cox vs frailty Cox differ in HR SE and recovered variance ≈ truth.

## 2. Bootstrap-optimism wrapper in `survivalvalidation`

**Why**: The single most effective antidote to univariate-screening +
optimal-cut-point optimism is a bootstrap that re-runs the *entire*
pipeline (cut-point search + variable screen + Cox fit) inside each
resample. Critical for biomarker discovery papers like Balarajah 2026.

**How to apply**:

- `jamovi/survivalvalidation.a.yaml` — add `validation_strategy`
  (`bootstrap_apparent` / `bootstrap_optimism` / `cv_kfold`), `B` (default
  1000), and `include_cutpoint` (Bool, default true).
- `R/survivalvalidation.b.R` — implement Harrell-style 0.632/0.632+
  optimism correction. Resample with replacement; refit the full
  pipeline; compute apparent vs corrected C-index, calibration slope,
  shrinkage factor.
- Dependencies: `rms` (Harrell), `pec`, `riskRegression`.
- Tests: synthetic data with known true C and overfit pipeline; verify
  apparent C is biased high and optimism-corrected C ≈ truth.

## 3. `advancedimputation` ↔ `multisurvival` MI-pooled Cox bridge

**Why**: Balarajah 2026 discards 38 % of patients on adjuvant chemotherapy
information via complete-case analysis. Multiple imputation is mandatory
when missingness is non-trivial and missing-at-random is plausible.
ClinicoPath has `advancedimputation` but no clean way to feed imputed
datasets into a multivariable Cox.

**How to apply**:

- New helper: `R/utils-mi-cox.R` exporting `.miPooledCox(data, formula, m, method)`
  that runs `mice::mice` → `mice::with(coxph)` → `mice::pool` and returns a
  finalfit-compatible HR table.
- `jamovi/multisurvival.a.yaml` — add `useImputation` Bool + `imputationM`
  Integer (default 5) + `imputationSeed` Integer.
- `R/multisurvival.b.R` — when `useImputation` is true, route through the
  helper instead of single-fit Cox. Surface a notice with the fraction of
  missing information (FMI) per covariate.
- Tests: simulate MAR missingness; verify pooled HR ≈ HR on full data and
  pooled SE > complete-case SE.

## 4. RMST / absolute-risk reporting in `survival` / `multisurvival`

**Why**: Clinicians and pathologists prefer "median survival 43 vs 28
months" or "5-yr survival 38 % vs 22 %" over "HR 0.45". Especially
important when PH is questionable and HR misleads.

**How to apply**:

- `jamovi/multisurvival.r.yaml` — add `rmst_table` (columns: stratum,
  RMST, ΔRMST, 95 % CI). Gate behind a new `report_rmst` Bool option.
- `jamovi/multisurvival.a.yaml` — `report_rmst` Bool + `rmst_landmarks`
  String (e.g., `"12, 24, 60"`).
- `R/multisurvival.b.R` — wrap `survRM2::rmst2` per stratum. Also report
  KM-derived absolute survival probabilities at each landmark.
- Repeat the equivalent for the simpler `survival` analysis card.
- Dependencies: `survRM2` (CRAN, lightweight).
- Tests: parametric Weibull simulation with known RMST(t); verify within
  Monte-Carlo error.

## Prioritization (recommended order)

1. **#4 RMST** — smallest, highest clinician value, no architectural risk.
2. **#1 Cluster/frailty** — high impact for multi-centre studies; medium
   effort because of the formula-builder churn.
3. **#3 MI bridge** — high impact; medium effort; needs careful pooling
   semantics.
4. **#2 Bootstrap optimism** — highest impact for biomarker papers;
   largest effort because the pipeline must be reified to a callable.

## meddecide module audit follow-ups (2026-05-14)

Source: `docs/audit/MODULE_AUDIT_REPORT_20260514-1847.md` (audit ran against the
standalone `/Users/serdarbalci/Documents/GitHub/meddecide` working copy; the
three HIGH security findings have already been remediated in
ClinicoPathJamoviModule — XSS in `decision.b.R:625-633` / `:663-672` via
`private$.safeHtmlOutput`, XSS in `decisioncompare.b.R:1816,1841` pre-patched,
and C1 formula-injection in `nogoldstandard.b.R:642` via
`jmvcore::composeTerms` + `jmvcore::asFormula`). The items below are the
out-of-scope findings from the same audit, deferred for separate work.

### [clinical-safety] enhancedROC — add AUC threshold notices

- AUC < 0.5 → ERROR ("worse than chance — verify outcome coding / class
  inversion").
- AUC < 0.7 → STRONG_WARNING ("poor discrimination — interpret cautiously").
- `enhancedROC.b.R` already detects inversion via `.detectInverted`; surface
  the result as a `jmvcore::Notice` banner, not a buried HTML paragraph.
- Run: `/fix-notices enhancedROC`.

### [clinical-safety] psychopdaROC — add AUC threshold + DeLong sample-size notices

- Same AUC < 0.5 / < 0.7 thresholds.
- DeLong's test requires reasonable sample size; surface STRONG_WARNING when
  n_pos × n_neg is small (rough guard: < 50 per class).
- `psychopdaROC.b.R` currently uses `jmvcore::reject` for fatal errors but no
  `jmvcore::Notice` at all (0 uses across 5,601 lines).
- Run: `/fix-notices psychopdaROC`.

### [clinical-safety] nogoldstandard — LCA convergence + small-n notices

- `poLCA` can silently return a degenerate solution when fewer than ~30 starts
  converge. Surface STRONG_WARNING when `best_model` count of successful starts
  < 25 % of `n_starts`.
- STRONG_WARNING when total cases < 100 (Hui-Walter assumption: at least two
  populations with sufficient observations per pattern).
- Run: `/fix-notices nogoldstandard`.

### [statistical-validation] reference-implementation parity reviews

- `/review-function agreement` — kappa2 / ICC / kripp.alpha / Gwet AC parity
  against `irr`/`psych`/`irrCAC`.
- `/review-function psychopdaROC` — DeLong / IDI / NRI / meta-analysis parity
  against `pROC` / `cutpointr` / `metafor` references.
- `/review-function decision` and `/review-function decisioncompare` — Wilson
  vs Clopper-Pearson CIs, McNemar small-sample exact path.
- `/review-function nogoldstandard` — Hui-Walter and Joseph-Gyorkos canonical
  reference parity.
- `/review-function sequentialtests` — parallel-test combined PPV formula
  cross-check (this function is otherwise exemplary).
- `/review-function cotest`, `/review-function decisioncalculator`,
  `/review-function decisioncombine` — Bayes prior-override math; pattern
  enumeration coverage.

### [hygiene] jmvcore migration sweep — 8 functions

- 31 bare `stop()` in `.b.R` (only 16 use `jmvcore::reject`); 8 `na.omit()` on
  jamovi-attributed frames could use `jmvcore::naOmit` to preserve column
  attributes (`measureType`, `values`).
- Affected: `agreement`, `decisioncombine`, `decisioncompare`,
  `kappasizefixedn`, `kappasizepower`, `nogoldstandard`, `psychopdaROC`,
  `sequentialtests` (minor).
- Run per function: `/jamovify-function <name> --pattern=error,na --apply`.

### [hygiene] notice-system consolidation

- `decision`, `decisioncompare`, `decisioncombine`, `nogoldstandard` each
  ship a custom `.addNotice` / `.renderNotices` HTML-rendered notice surface
  that parallels `jmvcore::Notice`. Pick one — `jmvcore::Notice` integrates
  with jamovi's native banner UI; the custom system renders only inside a
  function-private HTML block and won't be picked up by jamovi's
  serialization fixes.
- Reference: `decisioncalculator.b.R` (17 `jmvcore::Notice` uses) and
  `sequentialtests.b.R` (24 uses) — the module's "right" pattern.

### [hygiene] zero-or-stub notice coverage — 10 functions

- `agreement` (0 jmvcore notices; uses `setNote("error", …)` on tables —
  these are below the result panel and easy to miss).
- `cotest` (0; validation silent → user sees NaN).
- `enhancedROC` (0).
- `kappasizeci`, `kappasizefixedn`, `kappasizepower` (0 each).
- `psychopdaROC` (0).
- Plus the four functions with custom notice systems above.
- Run per function: `/fix-notices <name>`.

### [i18n] module-wide internationalization absent

- No `jamovi/i18n/` directory in either `meddecide/` or
  `ClinicoPathJamoviModule/` for these analyses, no `.po` / `.pot` catalogs,
  no `NAMESPACE` `importFrom(jmvcore, .)` (relies on `import(jmvcore)`).
- Even where `.()` is used (`enhancedROC` 143 wraps, `nogoldstandard` 95,
  `decision` 91) no extraction catalog exists, so all strings render English.
- 7 of 13 meddecide functions have zero `.()` wraps: `agreement` (3),
  `cotest`, `decisioncalculator`, `decisioncombine`, `kappasize*`,
  `sequentialtests`.
- Action: bootstrap `jamovi/i18n/{catalog.pot,en.po,tr.po}` once, then
  `/prepare-translation <name>` per function (start with the ones already
  wrapped: `enhancedROC`, `nogoldstandard`, `decision`).

### [integration] output-overdeclaration — verify visibility per preset

- 5 functions declare 2× more outputs than they populate; rest is gated by
  `setVisible(FALSE)` defaults. Users toggling unfamiliar flag combinations
  may see blank result panels.

  | Function | Outputs | Setters | Ratio |
  | --- | --- | --- | --- |
  | `agreement` | 396 | 131 | 3.0× |
  | `enhancedROC` | 173 | 60 | 2.9× |
  | `psychopdaROC` | 158 | 59 | 2.7× |
  | `decisioncombine` | 72 | 21 | 3.4× |
  | `decision` | 68 | 32 | 2.1× |

- Run per function: `/check-function-full <name>` with `check_external=true`
  to verify each flag toggles the right output.

### [testing] add regression tests for 11 missing analyses

- Only `tests/testthat/test-decision.R` (271 LOC) and `tests/testthat/test-roc.R`
  (42 LOC) exist (these live in the meddecide submodule, not in
  ClinicoPathJamoviModule's `tests/`).
- Missing: `agreement`, `cotest`, `decisioncalculator`, `decisioncombine`,
  `decisioncompare`, `enhancedROC`, `kappasizeci`, `kappasizefixedn`,
  `kappasizepower`, `nogoldstandard`, `sequentialtests`.
- Reference: `test-decision.R` already provides utility-function unit tests +
  integration tests against the `histopathology` example dataset.

### [correctness] likelihoodratio: `manualCutpoint` unusable value - DONE (2026-08-14)

Fixed. Root cause was worse than the sweep predicted: it crashed for EVERY
cutpoint method, not just `manual`.

- `manualCutpoint` was `type: Number` with NO `default:`, so it arrived as NULL.
  jamovi then threw `missing value where TRUE/FALSE needed` while comparing it
  for `clearWith` (it is listed in `likelihoodratio.r.yaml:20`) - **before any
  backend code ran**, and including under the default `youden` method, which
  never reads the value. Verified: `manualCutpoint = NULL` and `= NA_real_` both
  crash under `manual` AND `youden`; any real number works.
  Note `jmvcore::OptionNumber$new("x", NULL, default = 0)` still yields NULL, so
  a declared default fixes the GUI and omitted-argument paths but not a caller
  who passes NULL explicitly.
- Fix 1 - `jamovi/likelihoodratio.a.yaml`: `default: 0` on `manualCutpoint`, as
  the Number contract requires.
- Fix 2 - `R/likelihoodratio.b.R`: a default of 0 would then crash differently,
  because a cutpoint outside the data range puts every case on one side, the
  2x2 table collapses and `contingency[2,1]` threw `subscript out of bounds`
  (reproduced). The backend now builds the table over fixed `factor(levels =
  c(0,1))`, and when the split is degenerate it refuses to run and tells the
  user to enter a cutpoint inside the observed range - naming that range - or to
  pick an estimated method. A second guard rejects a non-finite cutpoint from
  any method.
- Regression cover in `tests/testthat/test-likelihoodratio.R` (12 pass). Proven
  to fail against the pre-fix code with `subscript out of bounds`.
- Still open, deliberately: the `.a.yaml` change needs `jmvtools::prepare()` +
  `devtools::document()` to reach the wrapper. Until then `manualCutpoint`
  remains a bare required argument.

### [jamovi/yaml] 220 further defaultless Variable/Variables options (module-wide)

Found 2026-08-14 by the post-regeneration sweep. Same defect class already
fixed here for `vartree` (percvar/summaryvar/prunebelow/follow) and
`categorize` (var), and previously for `agreement()` and `diagnosticmeta()`.

- 220 Variable/Variables options across 89 analyses have no `default:` yet are
  NULL-guarded as optional by their own `.b.R`. Each compiles to a BARE
  REQUIRED wrapper argument, so programmatic R callers - and
  `R CMD check --run-donttest` over any `@examples` - fail with
  `argument "X" is missing, with no default`. The jamovi GUI hides this
  entirely because it never omits an option.
- Worst offenders: `qualityoflife` (13), `qtwist` (9), `epidemiosurvival` (7),
  `decisiongraph` (7), `progressionsurvival` (5), `outbreakanalysis` (5),
  `jvisr` (5), `ihcimmune` (5), `biomarkerdiscovery` (5).
- IMPORTANT - do not fix mechanically. A bare required argument is CORRECT for
  an analysis's mandatory primary input; 314 of 390 wrappers have one by
  design. `alluvial`'s `vars` was checked and is correctly bare (the backend
  hard-rejects fewer than 2 variables, so a NULL default would convert a clear
  error into a silent empty result). The test is whether the backend
  NULL-guards the option as optional - only then does it want `default: NULL`.

### [jamovi/generated] `0000.yaml` accumulates stale and duplicate analysis entries

Found 2026-08-14. `jmvtools::prepare()` adds manifest entries but never prunes
them, so the generated `jamovi/0000.yaml` has drifted from the source tree.

- 20 entries have NO backing source at all - no `jamovi/<name>.a.yaml`, no
  `R/<name>.h.R`, no `R/<name>.b.R` - and ship as menu items with no analysis
  behind them: chisqposttestaddon, datecorrection, decisioncombine1,
  enhancednonparametric, flexiblebaseline, flexparametricadv,
  flexparametricadvanced, ggflowchart, ggoncoplot, jconsort, jflowchart,
  jjriverplot, jjsankeyfier, jjstreamgraph, powercomprisk, powersurvival,
  principalcox, stagemigration1, survivalPowerComprehensive, survivalpower.
  Several look like rename leftovers whose live counterpart exists under a
  different name (`jjriverplot` vs `R/riverplot.b.R`; `ggoncoplot` vs
  `R/jjoncoplot.b.R`; `survivalpower`/`survivalPowerComprehensive` vs
  `R/survivalPower.b.R` and `R/comprehensiveSurvivalPower.b.R`).
- 10 names appear TWICE (420 entries, 410 distinct): samplingerror,
  classification, psychopdaROC, statsplot2, jjridges, patientdashboard,
  populationhealth, precisionrecall, ordinalmixedmodel, enhancedROC.
  `samplingerror` is a byte-for-byte duplicate; the others differ only in
  menuGroup/title, so one copy is a stale pre-rename or pre-dev-route entry
  (e.g. `precisionrecall` under both `meddecideExtraD` and `meddecide`).
  Duplicate manifest entries produce duplicate/contradictory menu items.
- None of the 14 release-reviewed analyses are affected (each appears exactly
  once, under `Exploration`).
- `0000.yaml` is GENERATED - do not hand-edit. Establish which names are dead,
  delete or rename the corresponding sources, then regenerate. Deleting a stale
  entry by hand would be undone by the next `prepare()`.

### [correctness] dev-routed `*2` analyses fail their own tests (pre-existing)

Found 2026-08-14 while verifying the post-`prepare()` regeneration. These are
`menuGroup: ExplorationD` (dev-routed, NOT in the production menu), so they do
not block the ClinicoPathDescriptives release - but they are real failures and
their test files currently only pass by accident of never being run.

| Test file | Result | Failure |
| --- | --- | --- |
| `test-crosstable2.R` | 0 pass / 1 fail / 1 err | `object 'NAgroup' not found` |
| `test-reportcat2.R` | 1 pass / 4 fail | analysis returns non-list; `write_omv` rejects it |
| `test-summarydata2.R` | 1 pass / 4 fail | same as above |

- `NAgroup` appears nowhere in `R/`, `jamovi/`, or `tests/` - it is built
  dynamically, almost certainly `paste0(NA, "group")` from an option that is
  unset on the `sty = 'nejm'` path (`crosstable2.r.yaml` only declares
  `visible:` for `arsenal`/`finalfit`/`gtsummary`, so `nejm` is under-covered).
- `reportcat2`/`summarydata2`: `expect_true(is.list(model))` and
  `inherits(model, 'jmvcoreClass')` both FALSE, then
  `jmvReadWrite::write_omv()` errors with "Input data are either not a data
  frame or have incorrect ... dimensions". Check what the analysis actually
  returns before assuming the test is wrong.
- Confirmed NOT caused by the 2026-08-13/14 release-review work: the
  `.b.R`/`.a.yaml`/`.h.R` for all three are untouched by both the review and
  the module-wide `jmvtools::prepare()` regeneration (`git status` clean for
  those paths). The only session change was a comment-only TODO added to
  `R/reportcat2.b.R` in `2a1410a64`.
- Decide first whether these three dev analyses are still wanted. If they are
  being retired, delete the analyses and their test files rather than fixing
  them; `reportcat2` also still carries the `summary.factor()` `"NAs"` defect
  already fixed in `R/reportcat.b.R`.

### [architecture] split agreement.b.R (10,559 LOC monolith)

- Single `.b.R` with 146 options, 396 declared outputs, 970-line `.run()`
  starting at line 9,584. Helpers are extracted but the file is hard to
  navigate and review.
- Suggested split: one helper file per analysis family
  (`agreement_kappa.R`, `agreement_icc.R`, `agreement_kripp.R`,
  `agreement_gwet.R`, `agreement_bootstrap.R`, …) sourced from `agreement.b.R`,
  keeping the R6 class definition slim.
- This is a refactor, not a behaviour change. Defer until after the
  statistical-parity reviews above.

---

# release-review-function prompt

You are an expert R package and jamovi module developer with advanced expertise in biostatistics, clinical research, pathology, and clinician-facing software.

Your task is to perform a rigorous end-to-end review and improvement of the jamovi analysis `FUNC_NAME`.

Before making changes, read and follow all relevant development guides in the `vignettes/` directory whose filenames begin with `jamovi_`. Also inspect repository-level guidance such as `AGENTS.md`, `CONTRIBUTING.md`, and existing conventions in similar analyses.

## Scope

Identify every file and component associated with `FUNC_NAME`, including:

- The underlying R computation functions
- `.a.yaml` analysis definition
- `.b.R` backend implementation
- `.r.yaml` results definition
- `.u.yaml` user interface definition
- Generated files, tests, documentation, examples, and translations, where applicable

Trace the complete data flow:

```text
User interface → .u.yaml → .a.yaml arguments → .b.R processing
→ statistical computation → .r.yaml results → displayed output
```

## Review requirements

### 1. Mathematical and statistical validity

Critically evaluate whether the analysis is mathematically and statistically correct.

Check:

- Definitions, formulas, estimators, and algorithms
- Assumptions and whether they are stated or tested appropriately
- Handling of categorical, continuous, ordinal, paired, repeated, censored, weighted, and missing data, as relevant
- Factor coding, reference levels, contrasts, transformations, and interactions
- Confidence intervals, standard errors, test statistics, degrees of freedom, and p-values
- Multiple-testing adjustments
- Effect sizes and their interpretation
- Numerical stability, convergence, boundary cases, and singular models
- Agreement between labels, documentation, implementation, and reported statistics
- Whether interpretations avoid causal or clinical claims unsupported by the method

Independently verify important calculations against trusted R packages, published formulas, or small hand-calculated examples when feasible.

### 2. Argument and data flow

Confirm that:

- Every `.a.yaml` option is declared with the correct name, type, default, allowed values, and variable constraints
- Every option is read and used correctly in `.b.R`
- Defaults are consistent across `.a.yaml`, `.u.yaml`, backend code, documentation, and tests
- UI selections reach the intended computation without silent coercion or loss of information
- Variable types and measurement levels are validated appropriately
- Invalid or incompatible option combinations are prevented or handled clearly
- Missing, empty, filtered, and grouped datasets are handled safely
- Recalculation and state restoration work correctly when inputs change
- No declared argument is unused, and no backend behavior lacks a corresponding exposed option unless intentionally internal

### 3. Backend implementation

Review `.b.R` and supporting R code for:

- Logical correctness and consistency with the intended method
- Safe preprocessing and correct row alignment
- Correct subsetting, filtering, grouping, and missing-data handling
- Robust error and warning handling
- Clear, actionable messages for users
- Proper jamovi lifecycle and results-population patterns
- Performance on realistically sized clinical datasets
- Reproducibility and deterministic behavior where expected
- Maintainability and consistency with neighboring analyses
- Avoidance of duplicated or dead code

### 4. Results and presentation

Confirm that `.r.yaml` and the backend produce clinically meaningful, internally consistent output.

Check:

- Tables, plots, headings, footnotes, and notes
- Correct result types, formats, precision, and visibility conditions
- Appropriate units, labels, reference groups, and confidence levels
- Consistency between numerical results and narrative interpretations
- Clear differentiation between estimates, uncertainty, and significance
- Visibility of sample sizes, excluded observations, missing-data handling, and relevant assumptions
- Graceful presentation when a result cannot be computed
- Absence of misleading precision, ambiguous abbreviations, or overstated conclusions

### 5. User interface and usability

Evaluate `.u.yaml` from the perspective of clinicians and pathologists who may not be statisticians.

Ensure that:

- All necessary analytical options are available
- Options are logically grouped and ordered
- Labels use clear clinical language while remaining statistically accurate
- Defaults are safe, conventional, and useful
- Advanced options do not overwhelm routine users
- Mutually incompatible choices are disabled or validated
- Required inputs and expected variable types are obvious
- Help text, tooltips, and option descriptions explain consequences
- The interface follows conventions used by other analyses in the module
- The analysis supports a coherent workflow from data selection to interpretation

Do not add options merely for completeness. Each exposed option must have a defensible clinical or statistical use.

### 6. Clinical readiness and safety

Assess whether the analysis is suitable for use by clinicians and pathologists.

Look specifically for:

- Clinically misleading labels or interpretations
- Incorrect treatment of diagnostic, prognostic, survival, agreement, repeated-measure, or laboratory data, where relevant
- Confusion between statistical significance and clinical importance
- Inadequate reporting of uncertainty
- Unsupported diagnostic or treatment recommendations
- Edge cases common in clinical datasets, including sparse groups, rare events, perfect separation, zero cells, ties, detection limits, and heavy missingness
- Privacy or data-leakage concerns in output or diagnostics
- Situations requiring explicit limitations or warnings

Treat the analysis as decision-support software, not as an autonomous medical decision maker.

### 7. Testing and release readiness

Inspect existing tests and add or improve tests as needed. Include:

- Typical valid analyses
- Independently verifiable numerical reference cases
- Defaults and every meaningful option branch
- Missing data and filtered rows
- Empty selections and insufficient sample sizes
- Constant variables, zero-variance groups, sparse categories, and extreme values
- Invalid and incompatible inputs
- Previously identified regressions
- Result visibility and output structure
- UI-to-backend argument consistency

Run the most relevant tests and package/module checks available in the repository. Regenerate derived jamovi files only through the project’s documented workflow.

## Implementation rules

- Fix confirmed problems and implement justified improvements.
- Do not remove existing functionality.
- Preserve backward compatibility unless retaining a behavior would produce materially incorrect or unsafe results.
- If compatibility conflicts with correctness, document the conflict and choose the safest minimal change.
- Avoid unrelated refactoring.
- Follow the repository’s established coding and formatting conventions.
- Do not edit generated files directly when they have an authoritative source file.
- Do not silently change statistical defaults; explain and test any necessary default change.
- Add clear validation messages rather than allowing cryptic downstream errors.
- Support every substantive conclusion with code evidence, test evidence, or a statistical reference.
- Do not claim release readiness if important checks could not be completed.

## Required deliverables

Complete the implementation, then report:

1. **Overall verdict:** whether `FUNC_NAME` is mathematically valid, clinically appropriate, and release-ready.
2. **Findings:** problems grouped by severity—critical, major, moderate, and minor—with affected files.
3. **Changes made:** concise explanation of each correction or improvement.
4. **Statistical verification:** how important calculations were independently checked.
5. **Data-flow audit:** confirmation of mappings among `.u.yaml`, `.a.yaml`, `.b.R`, and `.r.yaml`, including unused or missing mappings.
6. **Test results:** commands or checks run and their outcomes.
7. **Remaining limitations:** unresolved risks, assumptions, or manual checks.
8. **Release recommendation:** one of:
   - Ready for release
   - Ready after specified minor actions
   - Not ready for release

Begin by locating the relevant development guides and all files associated with `FUNC_NAME`. Then inspect the implementation, establish expected behavior, make targeted corrections, and verify the final result.

## [jamovi/yaml] Leading-`!` in `.r.yaml` visible/enable is silently always-true

Found during the `decision` release review (2026-08). jmvcore routes a `visible:`/`enable:`
expression to the R evaluator only when it matches `^\([\$A-Za-z].*\)$`
(`jmvcore:::Options$public_methods$eval`). An expression starting with `!` fails that regex,
so `eval` returns the **raw string** — which is truthy. The item is permanently visible and
nothing errors.

Empirically reproduced: `benfordOptions$new(var="x")$eval("(!var)")` returns the string
`"(!var)"`, not `FALSE`. Same for `agepyramid` `(!age || !gender)` and `consortdiagram`
`(!participant_id)`. In practice this means "Getting Started" welcome panels sit permanently
above every completed analysis.

Expressions using jamovi's `option:level` syntax (`(!rotation:none)`) fail identically — `:`
is not valid R.

**`.u.yaml` is NOT affected** — those are evaluated by the frontend JavaScript, which handles
`!` and `option:level` correctly. The 36 `.u.yaml` instances are fine; leave them alone.

Fix idiom — restate without the leading `!`, and use `length(x) == 0` for Variable/Variables
options (their value is a list, which R's `&&` rejects):

```yaml
# before (always visible)
visible: (!(gold && newtest && goldPositive && testPositive))
# after
visible: (length(gold) == 0 || length(newtest) == 0 || length(goldPositive) == 0 || length(testPositive) == 0)
```

Each change needs `jmvtools::prepare()` to reach `.h.R`.

- [x] `decision.r.yaml:11` — fixed
- [ ] 25 remaining across 16 files; full list in the grep below
- **CAUTION (verified 2026-08-11):** the list below is over-broad - not every leading-`!` form is
  broken. `jjscatterstats.r.yaml:86`, `visible: (!is.null(colorvar) || !is.null(sizevar) || ...)`,
  was tested directly against the installed jmvcore and resolves **correctly**: FALSE with no
  aesthetic variable set, TRUE with `colorvar`, TRUE with `sizevar`. The
  `(!is.null(x) && x != "")` entries for `decisioncompare` are the same shape. Test each form
  before "fixing" it - rewriting a working expression is a regression, and the whole
  jjscatterstats analysis (7 visible expressions, all forms) was verified sound.

```
advancedtrials.r.yaml:131:      visible: (!biomarker_strategy:all_comers)
agepyramid.r.yaml:11:      visible: (!age || !gender)
aivalidation.r.yaml:95:      visible: (!crossValidation:none)
benford.r.yaml:11:      visible: (!var)
consortdiagram.r.yaml:10:      visible: (!participant_id)
cotest.r.yaml:61:      visible: (!indep)
decisioncompare.r.yaml:21:      visible: (!is.null(test1) && test1 != "")
decisioncompare.r.yaml:65:      visible: (!is.null(test2) && test2 != "")
decisioncompare.r.yaml:109:      visible: (!is.null(test3) && test3 != "")
decisioncompare.r.yaml:256:      visible: (!is.null(stratify) && stratify != "")
epidemiosurvival.r.yaml:140:      visible: (!age_standardization:none)
factoranalysis.r.yaml:69:                  visible: (!rotation:none)
factoranalysis.r.yaml:74:                  visible: (!rotation:none)
factoranalysis.r.yaml:172:            visible: (!scores:none)
explainableai.r.yaml:315:          visible: (!clustering_method:none)
explainableai.r.yaml:342:          visible: (!clustering_method:none)
haralicktexture.r.yaml:116:      visible: (!biomarker_context:general)
haralicktexture.r.yaml:123:      visible: (!biomarker_context:general)
ihcheterogeneity.r.yaml:10:      visible: (!biopsy1)
jjpubr.r.yaml:21:      visible: (!xvar)
jjscatterstats.r.yaml:86:      visible: (!is.null(colorvar) || !is.null(sizevar) || !is.null(shapevar) || !is.null(alphavar) || !is.null(labelvar))
partialcorrelation.r.yaml:41:        visible: (!multipleComparison:none)
partialcorrelation.r.yaml:111:        visible: (!multipleComparison:none)
relativesurvival.r.yaml:133:      visible: (!regression_model:none)
relativesurvival.r.yaml:268:      visible: (!regression_model:none)
tidyplots.r.yaml:10:      visible: (!xvar || !yvar)
```

## [meddecide] decisioncompare: `excludeIndeterminate` — DONE (2026-08-07)

Was a silent no-op: it filtered on `c(positiveLevel, setdiff(levels, positiveLevel))`, i.e. every
level, so equivocal results were still collapsed into Negative and still inflated specificity and
NPV — the exact harm the checkbox promises to prevent. On a 60-case fixture with 20 Equivocal
results, specificity read 0.950 with the option both off *and* on; excluding them gives 0.900.

**Fixed.** Added `goldNegative`, `test1Negative`, `test2Negative`, `test3Negative`
(`type: Level`, mirroring `jamovi/decision.a.yaml`) so the user names which level is a genuine
negative. When supplied, rows outside {positive, negative} are dropped and the retained/excluded
counts are reported; when not supplied the analysis says plainly that it cannot act rather than
pretending to. Identical positive/negative levels are rejected. Regression tests cover both paths.

⚠️ **BREAKING_CHANGE, and it needs regeneration to take effect.** `Level` options cannot carry a
`default:`, so all four become **required arguments** of the `decisioncompare()` wrapper once
`jmvtools::prepare()` recompiles `R/decisioncompare.h.R`. All call sites in this repo were updated
(tests, `R/data_decisioncompare_docs.R` roxygen `@examples`, `inst/examples/`). Third-party scripts
calling `decisioncompare()` must add the four arguments (`NULL` when unused).

The test suite routes through `tests/testthat/helper-decisioncompare.R`, which passes only the
arguments the *currently compiled* wrapper declares — so the suite is green both before and after
regeneration. The roxygen `@examples` and `inst/examples/` deliberately use the post-regeneration
API and will fail `R CMD check --run-donttest` until `prepare()` + `document()` are run.

- [x] [tooling] YAML 1.1 boolean tokens in analysis yamls - DONE 2026-08-10.
      117 tokens across 84 `jamovi/*.a.yaml` / `*.r.yaml` files were quoted (`- name: n` ->
      `- name: 'n'`). Proven inert for the jamovi compiler: `jmvtools::prepare()` before and
      after produced byte-identical output for all 389 `.h.R` files (the only 0000.yaml delta
      was an unrelated menuGroup change). R's `yaml` package now reads every option `name` as a
      string; previously 84 files handed back `FALSE`, silently breaking `tests/generate_tests.R`
      and the audit tests. Found during the kappaSizeFixedN release review.

- [ ] [tests] Rewrite `tests/testthat/test-jjridges.R` against the real `jjridgesClass`.
      The file had never executed: it opened with `library(ClinicoPathJamoviModule)`, which is
      the repository name, not the package (`ClinicoPath`), so testthat reported a single
      file-level error and all twelve tests inside were invisible. Fixing that plus 19
      `ClinicoPathJamoviModule:::` references, a `jmvcore::Output$new(type='html')` call using
      a non-existent argument, and the mock's method environments (re-parented from `baseenv()`
      onto the package namespace so `.()` resolves) got 3 of 12 passing. The rest are defeated
      by the approach: the file builds a fake `self` as a plain list and rebinds extracted
      private methods onto it, which the current R6 class does not support. They now skip with
      a reason. Use `jjridgesClass$new(options = ..., data = ...)` and
      `a$.__enclos_env__$private$...` instead - see `test-jjridges-release-review.R`.
      Found during the jjridges release review, 2026-08-11.

- [x] [tests] ROOT-CAUSED: `invalid 'row.names' length` from `jmvcore::select()` - DONE 2026-08-11.
      Not a flake and not a jmvcore bug: it is jmvcore's non-standard evaluation. The generated
      wrappers run every variable argument through `jmvcore::resolveQuo(jmvcore::enquo(x))`, and
      `resolveQuo` returns a BARE SYMBOL's own NAME (that is the idiom that lets you write
      `dep = age` to mean the column `age`). So `x_var = x_var`, or `dep = vars` where `vars`
      holds a character vector of column names, asks for a column literally called "x_var" /
      "vars". Every requested column is absent, `select()` builds a 0-column data frame, and
      copying the original row names onto it fails with an error that names neither the option
      nor the column. Diagnosed by dumping the `select()` frame with `withCallingHandlers` +
      `sys.frames()`: `columnNames = c("x_var","y_var","group_var")`, `length(out) = 0`,
      `nrow(df) = 120`. Fix in the CALLER: `!!x_var`, or a literal, or `do.call` with a value
      list - all three verified. Fixed the 4 affected call sites (test-hullplot-integration.R,
      test-jjcorrmat-integration.R x8, test-jjcorrmat-basic.R); both suites are now green
      (hullplot 366/0/0, jjcorrmat 347/0/0), and both release-review files carry a regression
      test that pins the contract.

- [ ] [tests] Audit the other bare-symbol variable arguments in the test suite.
      An AST scan of `tests/testthat/test-*.R` against the `resolveQuo` arguments of all 368
      generated wrappers finds 289 bare-symbol variable arguments across 34 files (heaviest:
      test-diagnosticmeta-notices-wilsonci.R 77, test-diagnosticmeta-critical-fixes.R 56,
      test-jggheatmap.R 26, test-finegray-competing-risks.R 21). MOST ARE CORRECT - passing a
      bare symbol is the intended way to name a column. The trap is only the subset where the
      symbol holds a column name rather than being one; those currently pass only because the
      test asserts something weak, or fail with the opaque row.names error. 57 of the 289 have
      `argument name == symbol name`, which is the highest-risk pattern. Scanner:
      /tmp/hp/scan.R (walks each parsed expression, so comments and strings are ignored).
      Found during the hullplot release review, 2026-08-11.

- [ ] [package] `formula.tools` breaks `stats::oneway.test` for the whole R session.
      ROOT CAUSE (this supersedes an earlier, wrong entry that blamed `library(ClinicoPath)`
      masking base functions - see the correction note below).
      `formula.tools` registers an `as.character.formula` S3 method returning ONE deparsed
      string ("y ~ g") where base R returns `c("~", "y", "g")`. `stats::oneway.test`'s second
      guard is `length(as.character(formula)) != 3L`, so it rejects every valid formula with
      "a two-sided formula is required" - for every package in the session, fully-qualified
      calls included.
      CHAIN: firthregression uses `logistf` (requireNamespace-guarded), `logistf` Imports
      `formula.tools`. So the breakage arms itself the first time a user runs Firth regression
      in a jamovi session, and is always armed under `devtools::load_all`, which loads
      DESCRIPTION Imports eagerly.
      BLAST RADIUS is narrow and was measured: ONLY `oneway.test`. `t.test`, `kruskal.test`
      and `bartlett.test` formula methods are unaffected. Downstream, `statsExpressions::
      oneway_anova` (Welch ANOVA) fails, which is why jjbetweenstats' 3+ group subtitle
      takeover falls back - now disclosed to the user rather than silent.
      Already worked around once in `R/ihcheterogeneity.b.R` (Levene's test switched to
      `aov()`); that comment misdiagnosed it as an R6/namespace effect and has been corrected.
      OPTIONS: (a) accept it - the two module call sites are defended and the jjbetweenstats
      fallback is disclosed; (b) drop `logistf` in favour of a Firth implementation that does
      not pull formula.tools (`brglm2::brglmFit`, NOT currently installed); (c) report upstream
      to formula.tools. Reproduce:
        Rscript -e 'd <- data.frame(y=rnorm(9), g=factor(rep(1:3,3)));
                    print(stats::oneway.test(y~g, d)$p.value);
                    loadNamespace("formula.tools");
                    print(try(stats::oneway.test(y~g, d)))'
      CORRECTION: `library(ClinicoPath)` does NOT mask base/stats functions - NAMESPACE
      exports none of them. The masking observed earlier (format() returning a number,
      terms/aov/t.test/setdiff shadowed) is caused by `devtools::load_all()`, whose
      `export_all` argument defaults to TRUE and dumps the whole namespace onto the search
      path. Dev and test scripts should use `devtools::load_all(".", export_all = FALSE)`,
      which is also closer to how jamovi loads the module.
      Found during the jjbetweenstats release review, 2026-08-11.

- [ ] [jjbetweenstats] Findings verified but NOT fixed in the release-review pass.
      A 63-agent adversarial review confirmed 50 findings; the critical one and all 14 majors
      are fixed. These survived verification and remain open, roughly in priority order:
        *padjustmethod defaults to "holm", which double-corrects the Games-Howell pairwise
          p-values ggstatsplot produces (they are already family-wise adjusted).
        * The ggpubr companion panel calls `ggpubr::stat_compare_means()` bare, so it always
          runs a nonparametric test regardless of Type of Statistic, and ignores equal
          variances, the p-adjustment and the confidence level. It also dies on variable names
          containing a space or parenthesis (bare strings where the main path uses rlang::sym).
        *The grouped (Split By) plot drops Title / X-Title / Y-Title and never calls
          .applyTheme, so the colourblind-safe palette is not applied there.
        * plotwidth / plotheight are not applied to either ggpubr panel; plot2's canvas is
          sized from factor levels that no longer carry data; with 2+ dependent variables the
          Split By height ignores the number of split levels.
        *`clearWith` for ggpubrPlot omits grvar even though grvar changes the rows analysed.
        * Listwise deletion across endpoints silently shrinks the sample for complete
          endpoints; the exclusion note does not say the deletion was joint.
        *Degenerate groups (n = 1, or all values identical) render a stats-free plot with no
          message; `dep = character(0)` errors unactionably on the programmatic path.
        * Assumption checking is skipped entirely for the bayes and robust types.
        * asSource() emits a stray blank line; a dead `messages = FALSE` is threaded through
          the multi-dependent-variable pmap.
      Full evidence, including each verifier's refutation attempt, is in the workflow journal:
      .../subagents/workflows/wf_573e7ee4-000/journal.jsonl
      Found during the jjbetweenstats release review, 2026-08-11.

- [ ] [module-wide] Non-finite values (Inf/-Inf) are mishandled differently in every analysis that meets them.
      Three release reviews on 2026-08-11/12 found the same root cause producing three different
      user-visible failures, because `is.na()` is TRUE for NaN but FALSE for Inf and
      `complete.cases()`/`jmvcore::naOmit()` follow `is.na()`:
        *hullplot   - Inf SURVIVED into the group statistics ("Inf +/- NaN") and made the
                       centroid distance infinite, flipping the copy-ready manuscript verdict to
                       "well-separated" for groups that completely overlap. FIXED (is.finite filter
                       + separate disclosure).
        * jjwithinstats - Inf first CRASHED the analysis (`if (NaN > 0)` in the skewness check ->
                       "missing value where TRUE/FALSE needed", no message), and once unblocked
                       reached the paired test and rendered "t(77) = NA, p = NA" beneath a panel
                       reassuring the user that all 78 subjects had been retained. FIXED.
        *jjbetweenstats - not reached in that review; unverified either way.
      The pattern to grep for is any filter written as `!is.na(x)` / `complete.cases()` on a
      numeric column that then feeds sd(), mean(), a test, or an `if (...)` comparison:
        grep -rn "complete.cases\|naOmit\|!is\.na(" R/*.b.R | wc -l   # ~200 call sites
      Worth one sweep rather than N local fixes. The correct idiom for a numeric column is
      `is.finite(x)`, with the non-finite count reported separately from ordinary missingness
      because it signals a data-entry or divide-by-zero problem rather than an absent observation.
      Found during the hullplot and jjwithinstats release reviews, 2026-08-12.

- [ ] **[CORRECTNESS/module-wide] `jmvcore::toNumeric()` is not a coercion function.**
      It only unwraps a jamovi/haven `values` attribute; a plain character or factor column
      falls straight through unchanged. So every `mydata[[v]] <- jmvcore::toNumeric(col)`
      "conversion" loop is a no-op for text columns, and every guard shaped like
      `all(is.na(jmvcore::toNumeric(col)))` is dead code that can never fire. Confirmed by
      reading the jmvcore source and by reproduction: `jjwithinstats` on character
      measurements died in `quantile()` with `non-numeric argument to binary operator`, a
      message naming neither the option nor the variable, raised from `.init()`.
      131 `R/*.b.R` files call `toNumeric`. In the jamovi GUI `permitted: [ numeric ]` keeps
      text columns out of the picker, so the exposure is limited to callers of the exported R
      wrappers (tests, `@examples`, `R CMD check --run-donttest`) — low severity, but it
      presents as a jmvcore bug and costs an afternoon each time. Correct idiom:
      `num <- jmvcore::toNumeric(col); if (!is.numeric(num)) num <- suppressWarnings(as.numeric(as.character(num)))`.
      Fixed in `R/jjwithinstats.b.R` only; worth one sweep across the rest.
      Found 2026-08-12 during the post-review audit pass.

- [ ] **[CRITICAL/module-wide] 3+-group parametric ggstatsplot subtitles render as NOTHING.**
      `logistf` (Imports, needed by the odds-ratio analysis) pulls in `formula.tools`, whose
      `as.character.formula` returns one deparsed string where base R returns
      `c("~","y","g")`. `stats::oneway.test` does `dp <- as.character(formula)` and rejects
      anything of length != 3 with "a two-sided formula is required", so **loading ClinicoPath
      breaks Welch's ANOVA for the whole R session**. ggstatsplot swallows the failure and
      returns `subtitle = NULL`: the user ticks "Statistical results in plot" and gets a figure
      with no statistics and no warning. Measured: `ggbetweenstats` 3 groups -> NULL subtitle;
      2 groups and `ggwithinstats` are unaffected.
      RESOLVED 2026-08-12. One shared internal helper, `withBaseFormulaChar()` in
      `R/ggstatsplot_utils.R` (`@noRd`, so no prepare()/document() needed), applied to
      `jjbetweenstats` (6 call sites), `jjdotplotstats` (4), `statsplot2` (2) and
      `jextractggstats` (1). Measured restorations: jjbetweenstats 3-group effect size now
      moves (eta2p 0.39 vs omega2p 0.37) and `varequal` now switches Welch F(2,77)=24.99 ->
      Fisher F(2,117)=24.65 - both were previously inert. `crosstable` has no such call site
      and `ihcheterogeneity` already routes around it with `aov()`; neither needed changing.
      The helper is registered in `_updateModules_config.yaml` for jjstatsplot and JamoviTest,
      and `check_shared_helper_distribution()` now fails the build if a shipped .b.R calls a
      helper whose file is not distributed.

      NOTE for whoever hits this next: the blast radius is NOT limited to ggstatsplot. Any
      loaded function calling `stats::oneway.test` inherits it - `tableone::CreateTableOne`
      with a `strata` argument returns p = NA instead of p < 0.001. `R/tableone.b.R` is safe
      only because it never passes `strata`.

- [ ] **[DESIGN/jjdotplotstats] "Dot Chart" draws a box-violin plot, duplicating jjbetweenstats.**
      `R/jjdotplotstats.b.R` calls `ggstatsplot::ggbetweenstats`, whose layers are measured as
      GeomPoint + GeomBoxplot + GeomViolin - not a dot chart. `jjbetweenstats` is titled
      "Box-Violin Plots to Compare Between Groups" and sits in the SAME `menuGroup` and
      `menuSubgroup`, so the module ships the same figure twice, one of them mislabelled.
      The `testvalue` / `testvalueline` / `centralityparameter` / `centralityk` options are
      leftovers from `ggstatsplot::ggdotplotstats`, which is what the name and title imply.
      Not swapped in this pass because it is a genuine product decision, not a defect:
      `ggdotplotstats` aggregates to ONE point per group and runs a one-sample t-test of those
      k means against `test.value` (verified: 120 raw rows -> 3 points), which for a 3-group
      biomarker comparison is a one-sample t-test on n = 3 - a statistical downgrade from the
      Welch ANOVA the analysis performs today. Decide: rename the analysis, or re-engine it and
      accept the different question it answers.
      Raised 2026-08-12 during the jjdotplotstats release review.

- [ ] **[MODERATE/jjdotplotstats] Pairwise comparisons are computed and drawn with no control.**
      The figure renders "Pairwise test: Games-Howell, Bars shown: significant, alpha = 0.05"
      with Holm adjustment, but unlike `jjbetweenstats` the analysis exposes no
      `pairwisecomparisons`, `pairwisedisplay` or `padjustmethod` option. The user cannot turn
      the multiple testing off or change the adjustment. Holm is a safe default, so this is a
      gap rather than a wrong number.

- [ ] **[MINOR/jjdotplotstats] Dead configuration.** `jamovi/jjdotplotstats.a.yaml` carries a
      large commented-out `clinicalpreset` option block, and `tests/testthat/test-jjdotplotstats-correctness.R`
      used to test it via `tryCatch(error = NULL)` so the test could only ever fail. Test now
      pins the real contract; the commented block should be deleted or finished.

- [ ] **[MODERATE/statsplot2] 45 pre-existing test failures, none from the shield.**
      Measured identical before and after the withBaseFormulaChar change (118 pass / 14 fail /
      31 err both ways), so they are untouched pre-existing debt: tests pass options the
      analysis does not have (`plotTitle`, `xlab`, `ylab`), reference a missing
      `statsplot2_repeated` fixture, and hit the `expect_s3_class(..., info=)` misuse that
      aborts a whole test_that block (the same bug that hid 94 assertions in jjdotplotstats).
      statsplot2 has never had a release review; worth one.
      Found 2026-08-12.

- [ ] **[MAJOR/jextractggstats] 35 of 43 assertions fail: the suite is red, not crashing.**
      Measured 2026-08-12 in a batch run: `pass=8 fail=31 err=4`. (An earlier note here claimed
      the suite segfaulted R - that was WRONG. Standalone runs produced 52 bytes of output and
      no tally because of how they were backgrounded, not because the interpreter died; the
      same file completes normally when run in-process with the other suites, and a direct
      call to the analysis runs fine.)
      The failures are almost certainly downstream of the `extract_stats(type = ...)` defect
      below - the analysis returns no extracted components, so nearly everything asserted
      about them fails. Fix that first, then re-measure before triaging what remains.
      jextractggstats is `menuGroup: JJStatsPlotD` (umbrella/dev only, absent from every
      submodule), so nothing ships on it today.

- [ ] **[CRITICAL/jextractggstats] The analysis extracts NOTHING: `extract_stats()` has no `type` argument.**
      `R/jextractggstats.b.R` calls it four times as
      `ggstatsplot::extract_stats(ggstats_result, type = "subtitle" | "caption" |
      "pairwise_comparisons" | "descriptive")` (lines 243, 251, 259, 267). In ggstatsplot
      1.0.0 the signature is `extract_stats(p)` - a single argument - so every call raises
      `unused argument (type = "subtitle")`. The surrounding tryCatch swallows it into
      `warning("Error extracting components: ...")`, so the analysis whose entire purpose is
      extracting ggstatsplot components silently produces none. Measured 2026-08-12:
      `names(formals(ggstatsplot::extract_stats))` is exactly `p`, and running the analysis
      emits `Error extracting components: unused argument (type = "subtitle")`.

      Fix: call it ONCE and index the returned named list, which already carries every piece -
      `s <- ggstatsplot::extract_stats(ggstats_result)` then `s$subtitle_data`,
      `s$caption_data`, `s$pairwise_comparisons_data`, `s$descriptive_data`
      (also available: `one_sample_data`, `tidy_data`, `glance_data`).
      Same family as the ggstatsplot 1.0.0 removals that made `effsize.type`/`var.equal`
      inert elsewhere - arguments absorbed by `...` or rejected outright after the API change.
      NOTE: the suite is currently 8 pass / 31 fail / 4 err (see the entry above), most of it
      plausibly caused by this very defect - so fixing this should be measurable as a large
      swing in that tally.
      PRE-EXISTING - unrelated to the withBaseFormulaChar shield added the same day.

- [ ] **[MAJOR/tests] Three test files are silently dead: they `library()` a package that does not exist.**
      The package is `ClinicoPath`; `ClinicoPathJamoviModule` is only the repository name.
      testthat treats an un-installed package at file scope as a SKIP, so the whole file
      reports as neither passed nor failed and looks green in any tally that counts
      failures. Found 2026-08-13 while release-reviewing jjbarstats, whose
      `tests/testthat/test-jjbarstats.R` had 15 test blocks that had never once executed;
      pointing it at `library(ClinicoPath)` turned it into 40 real passing assertions and
      exposed 4 blocks asserting that a `list()` constructor throws.

      Remaining files with the same line:
        - `tests/test-swimmerplot.R`
        - `tests/testthat/test-swimmerplot.R`
        - `tests/testthat/test-jjridges.R`

      Fix: change to `library(ClinicoPath)` and then TRIAGE what it exposes - expect the
      newly-live assertions to fail, since they have never run. Do not simply delete the
      failures. Detection: `grep -rn "ClinicoPathJamoviModule" tests/` should return nothing.

- [x] **[MODERATE/jjsegmentedtotalbar] Explicit "Value Variable counts cases" control.** DONE 2026-08-13.
      `show_statistical_tests` ran a chi-square on cells that are `sum(y_var)`, which is a
      contingency table only when `y_var` is a frequency. Integrality does NOT separate a count
      from a whole-number measurement — measured on random, association-free costs: chi2 = 277.5
      / p = 7.7e-59 in dollars, chi2 = 27750.5 / p = 0 for the SAME money in cents.

      Implemented as `y_is_count` (Bool, `default: false`) in `jjsegmentedtotalbar.a.yaml`,
      gating both the `.u.yaml` control (`enable: (y_is_count)` on `show_statistical_tests`) and
      `.performStatisticalTests()` in the backend. The permanent table footnote naming the
      unit-scaling failure mode was kept; the per-run nag was dropped now that the user affirms.
      NEEDS `jmvtools::prepare()` + `devtools::document()` — until then the option is absent from
      the compiled header and the analysis falls back to "test not run" via `.optionOr()`.

- [x] **[MAJOR/jjbarstats] jjpiestats subtitle swap ported.** DONE 2026-08-13.
      On a sparse 2x2 the plot subtitle reported an uncorrected Pearson chi-squared and the
      assumptions panel had to tell the reader to disregard it. `.exactSubtitle()` (ported from
      `R/jjpiestats.b.R`) now replaces it with Fisher's exact p, the odds ratio and its CI, and
      the panel text is conditional on whether the swap happened. Grouped (`grvar`) charts are a
      patchwork whose per-panel subtitles cannot be replaced, so that path keeps the disclosure
      route. The decision is per dependent variable, so a sparse and a well-powered variable can
      carry different subtitles in the same figure. No yaml change; no regeneration needed.

- [x] **[MAJOR/module-wide] `@import jmvcore` masked `base::format`.** DONE 2026-08-13.
      jmvcore exports its own `format(str, ..., context)` — a string-template interpolator that
      substitutes `{}` placeholders and returns its input untouched otherwise. `@import jmvcore`
      puts it ahead of `base::format` for the WHOLE package (not just the importing file), so
      every unqualified `format(x, digits = 3)` / `big.mark` / `nsmall` silently dropped its
      formatting argument.

      Symptoms: "Y Mean 19.8349757678086" in a clinical summary table; "Cost Analysis (Per 10000
      Patients)" instead of "10,000"; "0.829075514952931 unit increase" in copy-ready text.

      Fixed by qualifying the call sites — **97 sites across 20 files** — with `base::format()`,
      plus a local `.fmtNum()` helper in `linechart.b.R`. `@import jmvcore` was deliberately left
      alone: it is central to the module and the narrower fix is at the call site.
      Regression-guarded by "no source file calls a bare format() with base-format arguments" in
      `tests/testthat/test-linechart-release-review.R`, which scans `R/` on every run.
      Verified no behaviour regression: decisiongraph tests were 48/45/4/2 both before and after.

- [ ] **[MODERATE/module-wide] Statistics in `type: text` columns bypass jamovi's GUI number formatting.**
      Raised 2026-08-13. jamovi lets users set decimal places and p-value format in the results
      GUI, and it applies those to table columns declared `type: number` (with `format: zto,pvalue`
      for p-values). A statistic written into a `type: text` column is a plain string, so the
      user's GUI preference does not reach it and the backend must hard-code the rounding.

      This is *correct* for mixed parameter/value tables — e.g. the kappa power table in
      `agreement.b.R` puts "Cohen's kappa" and 0.412 in the same `value` column, so it cannot be
      `type: number`. But where a column holds only a statistic, converting it to `type: number`
      in the `.r.yaml` would hand formatting back to the GUI and let the hard-coded rounding go.

      Worth a pass per analysis; needs `prepare()` after each `.r.yaml` change. Start by finding
      text columns whose every value is numeric.

- [~] **[MAJOR/module-wide] `.rda` object names that differ from the file name — 31 of 70 fixed.**
      `data(<name>)` loads `data/<name>.rda` and creates whatever objects are inside it. When
      those differ, `data(foo)` succeeds and `foo` still does not exist, so every example,
      vignette or test that follows `data(foo)` with `foo` fails with "object 'foo' not found".

      **Fixed 2026-08-13: 31 files** — statsplot2's two, plus 29 more where the inner name was
      referenced NOWHERE in `R/`, `tests/` or `vignettes/` (so renaming could not break anything).
      28 of those 29 had their FILE name referenced, i.e. they were broken in use. The object was
      renamed in place — data untouched, no regeneration of RNG-based generators — and the
      `save()` calls in `data-raw/` were corrected (32 call sites, 13 files) so the next
      regeneration keeps the fix. Verified: no test regression. jiwillsurvive was 12/39 and
      nomogrammer 85/11/1 both before and after.

      **39 remain, and they must NOT be mass-renamed:**
        - 4 are legitimate multi-object bundles (`onesurvival_test_data` holds 6 datasets,
          `outcomeorganizer_test_data` 9, `outlierdetection_test_data` 9,
          `simon_makuch_examples` 4). Renaming does not apply; only worth changing if something
          references the FILE name as an object.
        - 35 have their INNER name in live use, so renaming breaks working code unless every
          reference is updated at the same time. Automated replacement is unsafe here because
          several inner names are extremely generic — `lung`, `breast`, `colorectal`,
          `summary_stats`, `edge_cases`, `minimal_data`, `histopathology` — and a blind
          text substitution across 1785 source files would corrupt unrelated code.
          These need per-dataset review: rename the object AND update its references together.

      Detection: load each `data/*.rda` into a fresh env and compare `load()`'s return value
      against the file stem.

## Module-wide: `visible: (!option)` in .r.yaml never works

jmvcore only treats a `visible:` string as an expression when it starts with `(`
followed by a letter. A leading `!` fails that routing check, so the string is
returned as-is and read as truthy - the result item is ALWAYS visible. Confirmed
on `jamovi/benford.r.yaml` (welcome panel stayed on screen, empty, once a
variable was selected) and fixed there by moving visibility into `.run()` via
`setVisible()` with `visible: false` in the yaml.

There is no working negation idiom in .r.yaml; every functioning `visible:` in
this module is a plain `(option)`. The remaining occurrences are silent - they
show an item that should be hidden, so nothing errors:

- jamovi/advancedtrials.r.yaml:131
- jamovi/agepyramid.r.yaml:11
- jamovi/aivalidation.r.yaml:95
- jamovi/consortdiagram.r.yaml:10
- jamovi/epidemiosurvival.r.yaml:140
- jamovi/explainableai.r.yaml:315
- jamovi/explainableai.r.yaml:342
- jamovi/factoranalysis.r.yaml:69
- jamovi/factoranalysis.r.yaml:74
- jamovi/factoranalysis.r.yaml:172
- jamovi/haralicktexture.r.yaml:116
- jamovi/haralicktexture.r.yaml:123
- jamovi/ihcheterogeneity.r.yaml:10
- jamovi/jjpubr.r.yaml:21
- jamovi/partialcorrelation.r.yaml:41
- jamovi/partialcorrelation.r.yaml:111
- jamovi/relativesurvival.r.yaml:133
- jamovi/relativesurvival.r.yaml:268
- jamovi/tidyplots.r.yaml:10

Fix pattern: set `visible: false` in the .r.yaml and drive it from the backend
with `self$results$<item>$setVisible(TRUE/FALSE)`.

## jmvcore::format underscored-placeholder sweep — DONE 2026-08-22

Swept all 10 candidate files. Real bugs found and fixed in dendrogram.b.R (6)
and advancedraincloud.b.R (8) — paired placeholder+argument renames, zero
residuals, suites confirm behavior-neutral. The other 8 files were false
alarms: every hit sits inside glue::glue(), which handles underscores fine.
Detection rule going forward: grep hits must be paired with a jmvcore::format
context check. Memory: reference_jmvcore_format_no_underscore_placeholders.md

NOTE (pre-existing, unrelated to the sweep — verified by baseline run without
the sweep edits): test-dendrogram.R FAIL 11/12, test-dendrogram-critical-fixes.R
FAIL 5 + ERROR 13, test-advancedraincloud.R FAIL 4. dendrogram especially needs
its own audit/fix pass.

## dendrogram + advancedraincloud pre-existing failures — DONE 2026-08-22

dendrogram: universal crash fixed (Output option `clustOutput` had no matching
.r.yaml results item) + deleteRows guards + warning->notice; 50/50 green.
advancedraincloud: stale snapshots accepted after diff review; 210/210 green.
raincloud sibling: 2 test ERRORs from bare format() masked by jmvcore::format
under load_all -> base::format; 110/110 green.

## diagnosticmeta fix pass (critical/integration/statistical/code-quality) — DONE 2026-08-22

All audit categories fixed; suite 266 pass / 0 fail / 0 error (was 206/1/3).
BREAKING_CHANGE: zero_cell_correction option keys renamed
`treatment_arm`->`zero_cells`, `empirical`->`reciprocal_n` (old names were
Sweeting-2004 terms for procedures the code does not implement).
Deferred (out of scope, this function):

- [ ] i18n wrap: diagnosticmeta.b.R still has ~1 `.()` call in 3300 lines;
      run /prepare-translation diagnosticmeta (catalog refresh + wrap tables,
      notes, notices, panels) as its own pass. Note: catalog.pot/en.po/tr.po
      still index the DELETED option keys (treatment_arm/empirical titles) and
      lack the renamed ones - the jmvtools::i18nUpdate refresh in that pass
      clears this.
- [ ] The interpretation/about onboarding panels are long static HTML: consider
      trimming to the house style used by newer analyses.
- [ ] jamovi GUI smoke test after `jmvtools::prepare()` (menuGroup currently
      OncoPathT for testing; restore to OncoPath after).

## ihcheterogeneity check pass (/check-function, standard) — DONE 2026-08-23

Suite 194 pass / 0 fail / 0 error (was 175 pass + 11 hidden ERRORs, incl. a
test file that never ran due to a nonexistent CSV). Fixed: spatial-plot CV
pooled between-patient spread (contradicted its own table); Kruskal-Wallis
pseudo-replication (now per-case means, pinned vs kruskal.test); duplicate
rows on data-edit re-runs (clear-first on 6 tables); reference+1-region design
un-blocked (classic biopsy-vs-resection agreement; ICC(2,1) pinned exact vs
psych); zero-row blank screen -> reject; silent-empty spatial tables -> notes;
power-without-reference silent no-op -> warning; hardcoded 15/30 CV bands ->
user threshold everywhere (plot lines included); false "adjusted for sampling
design effects" claim removed; Html setNote latent crash; theme-unsafe white
cards; stale .icc_consistency; dead renderer warning()s.
Follow-up fix pass 2026-08-23 (suite 209 pass / 0 fail / 0 error):

- [x] report_sentences + assumptions gated behind new Bool options
      showReportSentences / showAssumptions (default false); generation skipped
      when off.
- [x] BREAKING_CHANGE: analysis_type level 'bias' removed (merged into
      'reproducibility', retitled "Reproducibility & Bias Assessment") - the
      two were computationally identical.
- [x] sampling_strategy pruned from every clearWith except interpretation
      (it shapes prose only).
Review pass 2026-08-23 (suite 215 pass / 0 fail / 0 error; checktor clean,
lintr bug-set clean, all gates clean; 3-agent adversarial verification):
- [x] Key Findings CV + correlation grades were still on fixed constants
      (15/30, 0.80/0.60) - now on the user thresholds like every other panel;
      spatial table/plot bands likewise; cross-panel pin at thr 5/20/50.
- [x] Regions with <2 cases now listed in a spatial-table note; header text no
      longer says "simulated biopsy samples"; UI label/enable touches.
- [ ] i18n: 10 `.()` wraps in ~2550 lines - needs its own /prepare-translation
      pass (po catalogs also still index the removed 'bias' level title).
- [ ] Restore menuGroup: OncoPath after GUI testing (currently OncoPathT).

## kappaSize family — filed from the kappaSizePower release review (2026-08-23)

- [ ] [statistics] `kappaSizeCI` still detects sparse cells by grepping kappaSize's marginal
      `props[i] * n < 5` warning (`kappaSizeFixedN` and `kappaSizePower` fixed 2026-08-23 with
      Cochran's rule on the `P0..Pn` agreement cells). Verify `CIBinary`'s `.CalcIT` the same
      way, then port `.gofCells()`; also check whether its CI search can walk outside the model
      the way `FixedN*` does (negative agreement-pattern probabilities). 0 `.()` wraps there too.
- [ ] [ui] `jamovi/js/kappasizeci.js` is a lowercase rename leftover that nothing binds; wire as
      `kappaSizeCI.events.js` like its two siblings, or delete.
- [ ] [tooling] `tools/ui_harness/render_ui.sh` cannot render any analysis that has an events
      file (`require is not defined` — the harness serves the compiled `.src.js` without
      bundling). `waterfall`, `agreement`, and all three kappaSize analyses fail identically;
      a data-free calculator without events (`evalue`) renders fine.

## kappaSizeFixedN release review (2026-08-23) - out-of-scope follow-ups

- [ ] **[docs]** `vignettes/function-reference.Rmd` links three `.omv` downloads under
      `master/data/` for kappaSizeFixedN (and the same pattern for kappaSizeCI /
      kappaSizePower), but only 7 `.omv` files are tracked in `data/` module-wide and none of
      them is a kappaSize file. Every one of those download links is dead. Either commit the
      `.omv` artefacts the data-raw scripts produce, or drop the links.
- [ ] **[data]** Two generators write the same three files: `data-raw/kappasizefixedn_test_data.R`
      (verified to reproduce the shipped `.rda` byte-for-byte after this pass) and
      `data-raw/create_kappasizefixedn_test_data.R` (671 lines, different content, same output
      filenames). Whichever runs last wins. Delete one, or rename its outputs.
- [ ] **[data]** `kappasizefixedn_*` are pure prose tables for an analysis with
      `requiresData: FALSE` - no jamovi user can feed them to kappaSizeFixedN. Decide whether
      they should ship at all.
- [ ] **[refs]** `jamovi/kappaSizePower.r.yaml` and `jamovi/kappaSizeCI.r.yaml` name Donner &
      Eliasziw / Rotondi & Donner in their Methodology notices but cite only `kappaSize`. Add
      `donnerEliasziwKappaGOF` and `rotondiDonnerKappaCI` (now in `jamovi/00refs.yaml`) to both.
- [ ] **[docs]** `inst/examples/kappasizepower_example.R` and `kappasizeci_example.R` were not
      reviewed in this pass; check them for the same "minimum detectable kappa" framing that
      `kappasizefixedn_example.R` carried.
- [ ] **[upstream]** `kappaSize`'s `print`/`summary` methods judge sparseness on the outcome
      MARGINALS (`props[i] * n < 5`), which for a binary outcome only tests `props[1]`. That
      line can therefore appear in the Analysis result pane while the module's Notes panel
      (Cochran's rule on the agreement-pattern cells) stays quiet, and vice versa. Consider
      one sentence in the Methodology note explaining the two checks differ.

## kappaSizeCI /check-function pass (2026-08-23) - out-of-scope follow-ups

- [ ] **[i18n]** `R/kappaSizeCI.b.R` has **0 `.()` wraps across 69 prose strings** and 0 entries
      in `jamovi/i18n/tr.po`, while `kappaSizeFixedN` is 32 wraps / 31 po entries and
      `kappaSizePower` 38 / 38. One of the three analyses in the same "Power Analysis by
      meddecide" submenu renders untranslated in a Turkish session. The in-file TODO at
      `R/kappaSizeCI.b.R:502` says "bootstrap jamovi/i18n/" but the catalogs already exist, so
      the note is stale - the actual route is `/prepare-translation kappasizeci`.
- [ ] **[refactor]** `.gofCells` now lives in THREE files (`kappaSizeCI.b.R`,
      `kappaSizeFixedN.b.R`, `kappaSizePower.b.R`). The CI and FixedN bodies are byte-identical;
      Power differs only in `2L` vs `2` and an argument name, and all three agree to 0 (exactly)
      across 100 configurations. They are private R6 methods so the `Collate:` shadowing hazard
      does not apply, but the same 12-line closed form must now be kept in sync by hand. A
      shared helper in `R/utils.R` is the obvious ask from a reviewer.
- [ ] **[cleanup]** `jamovi/kappaSizeCI.a.yaml` carries 40 lines of commented-out schema - an
      inert duplicate of the live `outcome` option plus a `.u.yaml` fragment. Harmless (YAML
      comments) but noise. `remove_placeholders` was off in the profile used.
- [ ] **[lint]** Add a repo-wide gate for a literal `%` in any `.a.yaml` `description:` block.
      It becomes `\%` in the `.h.R` and `\\%` in the `.Rd`, where the parser eats the rest of
      the line - 70 characters vanished from `?kappaSizeCI` this pass and neither
      `tools::checkRd()` nor `R CMD check` flags it.
      **The other live instance is now identified:** `jamovi/decisioncurve.a.yaml`, option
      `weightedAUC`, `description.R`. `man/decisioncurve.Rd` currently renders
      "...it moved from 0.309 to 0.163 as the range widened from 5-20\ alongside it. Curves..."
      - the `%` after "5-20" eats the rest of that source line mid-sentence. One-token fix
      (write "percent"), then `prepare()` + `document()`. `waterfall.a.yaml` also has `%` in two
      `description.jamovi` strings, but its `description.R` strings already say "percent", and
      only the `R` key reaches the `.Rd`, so waterfall is NOT affected.
- [ ] **[style]** `R/kappaSizeFixedN.b.R:106` still pastes `signif(sparse_min, 2)` straight into
      the sparse notice, so it inherits the scientific-notation-in-prose wart that kappaSizeCI
      just fixed with `.fmtCount()` ("below 0.01"). Port `.fmtCount` to FixedN and Power.

## decisioncurve — deferred from the 2026-08-24 release review

- [ ] **i18n**: 0 `.()` wraps (siblings kappaSizeFixedN/kappaSizePower have 32/37). ~110 call
      sites in a 2,819-line file; needs `/prepare-translation decisioncurve` plus a
      `catalog.pot` regeneration for the renamed column titles. No user-visible effect while
      `tr.po` is 0.7% filled.
- [ ] **`tests/testthat.R` is missing** module-wide while `tests/testthat/` holds 801 files, so
      `R CMD check` runs no testthat file for the whole package. Needs its own pass — adding the
      driver switches on 801 unexercised files at once.
- [ ] `.u.yaml` still uses cost-benefit framing that the backend text deliberately softened to
      "exploratory monetary payoff".
- [ ] "Show reference line labels" renders no Treat All label, and the "standard" plot style does
      not visually distinguish the reference lines.
- [ ] Clinical Impact and Resource Utilization describe the same row from two angles
      ("net interventions avoided" vs "% fewer treatments"); both titles are now accurate but a
      note tying them together would help.
- [ ] Decision Consequences shows observed counts beside per-1000 projections with no note.
- [ ] `tools/check_state_guards.py` and `tools/theme_safe_html.py` return "0 findings" for this
      file without their patterns ever matching it — verify they actually cover it.

## enhancedROC — deferred after release review (2026-08-25)

Confirmed defects left open, with the reason each was not fixed in that pass.

- [ ] **21 `addRow(rowKey=)` sites build rows in `.run()`** rather than `.init()`. The correct
      fix (`rows: (predictors)` in `jamovi/enhancedROC.r.yaml` + `setRow(rowKey = predictor)`)
      was written and reverted: it hard-errors (`rowKey 'm1' not found`) until
      `jmvtools::prepare()` regenerates `R/enhancedROC.h.R`. Reapply together with the
      regeneration. Six of the 23 sites are genuinely data-dependent and must stay as `addRow`
      (cutoffAnalysis x3, multiClassAUC x2, decisionImpactSummary).
- [ ] **`sensitivityThreshold` / `specificityThreshold` defaults** changed in `.a.yaml` from 0.8
      to 0 (and `min` 0.1 -> 0) so the Youden search is unconstrained out of the box. Inert until
      `prepare()`; the backend notice that discloses a binding constraint is already live.
- [ ] **"Confidence bands" is pathologically slow** — 9 x `pROC::ci.coords` inside
      `.plotROCCurve`'s `renderFun`, ~87 s for 2 predictors at n=5000, recomputed on every
      redraw and resize. Move the computation into `.run()` and pass it via `setState`.
- [ ] **20 options are disclosed as NOT YET IMPLEMENTED** and guarded, but the jamovi UI gives no
      hint before the user ticks one. Consider greying them or grouping them under an
      "Experimental" collapse.
- [ ] **Notices are outside the translation catalog** — 162 `.()` wraps exist in
      `R/enhancedROC.b.R` but none of the 44 `.addNotice()` sites uses one, so every notice, the
      glossary panel and all plot text are English-only.
- [ ] **`splineKnots` and `nntCalculation` are declared but never read.** `nntCalculation` now
      has `enable: (clinicalImpact)`; both still need either an implementation or removal.
- [ ] **`jamovi/0000.yaml` lists 10 analyses twice** (`enhancedROC`, `psychopdaROC`,
      `classification`, `precisionrecall`, `jjridges`, `ordinalmixedmodel`, `patientdashboard`,
      `populationhealth`, `samplingerror`, `statsplot2`). Generated file, module-wide — belongs
      in a `prepare()` / `_updateModules.R` pass, not a single-analysis fix.

---

# Skill Usage

| Skill | Example |
| --- | --- |
| `$add-r-code` | `$add-r-code add reproducible R output to tableone` |
| `$audit-module` | `$audit-module audit the entire ClinicoPath module` |
| `$check-function` | `$check-function validate enhancedROC with the release profile` |
| `$check-function-full` | `$check-function-full deeply audit enhancedROC without repairing it` |
| `$check-module` | `$check-module validate all analyses` |
| `$checkpoint` | `$checkpoint add cancellation checkpoints to survival` |
| `$create-function` | `$create-function create a diagnostic analysis named newroc` |
| `$document-function` | `$document-function document enhancedROC` |
| `$fix-function` | `$fix-function diagnose and repair enhancedROC` |
| `$fix-notices` | `$fix-notices improve notices in enhancedROC` |
| `$generate-test-data` | `$generate-test-data create diagnostic fixtures for enhancedROC` |
| `$jamovi-playbooks` | `$jamovi-playbooks choose the right workflow for fixing enhancedROC` |
| `$jamovify-function` | `$jamovify-function migrate enhancedROC to safer jmvcore helpers` |
| `$prepare-translation` | `$prepare-translation prepare enhancedROC for Turkish` |
| `$release-review-function` | `$release-review-function review and repair enhancedROC for release` |
| `$review-article-stats` | `$review-article-stats review the statistics in this attached paper` |
| `$review-function` | `$review-function review enhancedROC without making changes` |
| `$security-audit-function` | `$security-audit-function audit enhancedROC for code injection and XSS` |
| `$social-media-promo` | `$social-media-promo write a LinkedIn post about enhancedROC` |
| `$update-refs` | `$update-refs synchronize references for enhancedROC` |

## enhancedROC / psychopdaROC concordance — open decision (2026-08-25)

The two ROC analyses now agree on every shared estimator. One default divergence is left open
BY DESIGN, because closing it either way changes a statistical default:

- [ ] **Direction at shipped defaults.** enhancedROC defaults to `direction: auto` and detects
      the orientation from the data; psychopdaROC offers only `>=` / `<=` and defaults to `>=`.
      On a marker where LOWER values indicate disease, at defaults, they report AUC 0.825 and
      0.175 for the same data. Both warn, and both warnings are good.
      Closing it requires a choice:
        (a) add `auto` to psychopdaROC and default to it — matches enhancedROC, but inherits the
            upward AUC bias of reading direction from the same data used for the estimate
            (a null marker at n=20 reports a mean AUC of about 0.59), OR
        (b) drop `auto` as enhancedROC's default — statistically cleaner, but changes behaviour
            for every existing enhancedROC user.
      The reviewer argued (b) is the better statistics and (a) is the better ergonomics. Not
      changed unilaterally: this is a clinical-defaults decision, not a defect.
      Implementation note for (a): `direction` is read at 18 sites in R/psychopdaROC.b.R and
      `R/utils.R:167` hard-stops on anything but `>=`/`<=`, so `auto` must be resolved ONCE into
      a private field early in `.run()` and every site switched to read that. Needs prepare().

## [ui-schema] 9 dead `enable:` expressions -- controls permanently enabled (2026-08-28)

Found while fixing `decisioncombine` I2. jamovi's client treats a value as a data binding
ONLY if it starts with `(` AND ends with `)` (`isValueDataBound`: `t.startsWith("(") &&
t.endsWith(")")`). A paren-less value is stored as a literal string, and controls disable
only on strict `=== false`, so the control is **permanently enabled** -- silently, with no
compiler error and no console warning.

Fix = wrap each in parentheses (note `(!x)` is correct, `!(x)` is not):

- [ ] `jamovi/advancedbarplot.u.yaml`   `stat_method`         `add_statistics` -> `(add_statistics)`
- [ ] `jamovi/advancedbarplot.u.yaml`   `value_format`        `show_values` -> `(show_values)`
- [x] `jamovi/advancedraincloud.u.yaml` `mcid_value`          `show_mcid` -> `(show_mcid)`
- [x] `jamovi/advancedraincloud.u.yaml` `effect_size_type`    `show_effect_size` -> `(show_effect_size)`
- [x] `jamovi/advancedraincloud.u.yaml` `baseline_group`      `show_change_scores` -> `(show_change_scores)`
- [x] `jamovi/advancedraincloud.u.yaml` `responder_threshold` `show_change_scores` -> `(show_change_scores)`
- [x] `jamovi/advancedraincloud.u.yaml` `cv_band_1`           `show_cv_bands` -> `(show_cv_bands)`
- [x] `jamovi/advancedraincloud.u.yaml` `cv_band_2`           `show_cv_bands` -> `(show_cv_bands)`
- [ ] `jamovi/clinicalalerts.u.yaml`    `custom_thresholds`   `!use_clinical_defaults` -> `(!use_clinical_defaults)`

Gate to add: the python walk in the audit transcript, or extend
`tools/check_uyaml_duplicate_names.py`.

## [docs] `jamovi_u_yaml_guide.md` teaches the broken paren-less `enable:` form (2026-08-28)

- [ ] `vignettes/jamovi_u_yaml_guide.md` lines ~204, 252, 268, 284, 302, 373, 562, 717, 883, 888
      show `enable: performBootstrap` (no parens), which never binds. Fix the examples and
      add two rules to the "Two schema traps" section (~line 413):
      1. the whole expression must be wrapped in outer parens (`isValueDataBound`);
      2. there is NO operator precedence -- operands fold strictly left-to-right, so
         `(a || b && c)` is `((a || b) && c)`. Parenthesize inner groups.
      Distinct from the R-side `.r.yaml visible:` routing trap -- different parser.

## [i18n] jmvtools::i18nUpdate() is incremental, not a full rewrite (2026-08-28)

- [ ] Record in `vignettes/` that `jmvtools::i18nUpdate()` touched only 77 lines per catalog
      and preserved all 193 tr.po translations. Prior guidance assumed it rewrites all
      ~28k entries, which discouraged running it; it is safe to run after adding msgids.

## [schema] 19 phantom analyses registered in jamovi/0000.yaml with no .a.yaml source (2026-08-28)

Found during the `decisioncombine` release review. `jamovi/0000.yaml` registers 420 analyses;
19 have no `jamovi/<name>.a.yaml` at all. `0000.yaml` is GENERATED, so the fix is a
regeneration (`jmvtools::prepare()`), not a hand edit -- but note prepare() appears to add
without removing, so the stale blocks may need deletion at the source before regenerating.

Why it matters beyond tidiness: each phantom's `description:` is still extracted into
`catalog.pot` / `en.po` / `tr.po`, so translators are asked to translate text for analyses
that do not exist. In `decisioncombine1`'s case the stale description promises
"state-of-the-art statistical methods" and "Provides actionable recommendations" -- language
the live analysis deliberately removed in favour of "descriptive ... not a clinical guide".
A menu entry or a translated string making that promise is a clinical-safety wording problem,
not just noise.

- [ ] ggoncoplot, jconsort, jflowchart, ggflowchart, datecorrection, jjstreamgraph,
      jjriverplot, jjsankeyfier, enhancednonparametric, flexiblebaseline, chisqposttestaddon,
      flexparametricadv, principalcox, flexparametricadvanced, powersurvival, powercomprisk,
      survivalPowerComprehensive, decisioncombine1, stagemigration1
- [ ] Gate: assert every `analyses:` entry in 0000.yaml has a matching `.a.yaml`.

## [i18n] decisioncombine Turkish translation is 2/206 and mixes languages in one table header (2026-08-28)

- [ ] Only "Specificity"->"Ozgulluk" and "Accuracy"->"Dogruluk" are translated, both inherited
      from other analyses. They sit in the SAME combinationTable header row as 15 untranslated
      siblings, so a Turkish user sees a half-Turkish header. Translate the whole
      `.metricLabel` family (R/decisioncombine.b.R:126-136) as one atomic unit, or none of it.
- [ ] `i18n-plans/decisioncombine-tr-translation-plan.md` tables 53-64 and 71-86: 8 of 24
      "English source" rows are not msgids anywhere in the catalog and 4 more belong to other
      analyses. Regenerate the tables mechanically from the 206 catalog entries whose `#:`
      refs contain decisioncombine.

## [release-review] Out-of-scope findings from the `survival` release review (2026-09-02)

### [bug] `visible: ((...)` never routes through jmvcore's binding parser — 7 sites outside `survival`

`jmvcore::Options$eval()` routes a `visible:` expression only when it matches
`^\([\$A-Za-z].*\)$`. An expression starting with `((` fails that regex, falls through to
`jmvcore::format()`, and comes back as the RAW STRING; `ResultsElement$.update()` then sets
`visibleValue <- (length(vis) > 0)` — i.e. **always visible**, silently. Verified empirically
against jmvcore. Rewrite each as `(<scalarOption> && (<parenthesised group>))`.

- [ ] `jamovi/multisurvival.r.yaml:1857,1863` — `((ac || hr) && showExplanations)`
- [ ] `jamovi/nonparametric.r.yaml:456,472,511`
- [ ] `jamovi/survivalcont.r.yaml:1048,1054` — starts with `(((`
- [ ] Add a gate: fail on `grep -n "visible: *((" jamovi/*.r.yaml`. The 2026-08-14 sweep fixed
      the leading-`!` variant of this same routing bug but not the leading-`((` variant.

### [bug] `survminer::ggsurvplot(..., facet.by=)` is broken with the installed ggplot2

`surv_summary()` returns only a combined `strata` column, so ggplot2's `combine_vars()` aborts
with "At least one layer must contain all faceting variables" — at PRINT time, inside the
device, so the renderer still returns TRUE and jamovi draws a blank white panel. Fixed in
`R/survival.b.R:.plotAgeStratifiedKM` by drawing the facets from `survfit()` directly.

- [ ] Sweep for other `facet.by` uses: `grep -rn "facet.by" R/*.b.R` and confirm each still
      produces a non-blank image (render to PNG and check the file size, not the return value).

### [robustness] survival: minor items deliberately left alone

- [ ] `R/survival.b.R` `.calculateCalibration()`: `bh_at_time` picks the baseline hazard at the
      NEAREST `basehaz` time to the calibration horizon. The step function should be evaluated
      at the largest time <= the horizon; the nearest-neighbour lookup can read a value from
      just after it.
- [ ] `R/survival.b.R` `.calculateRMST()` uses a hard-coded `1.96`; prefer `stats::qnorm(0.975)`.
- [ ] `personTimeTable` mixes three kinds of row (overall / per-group / per-interval) in one
      `interval` column with no type column, so the person-time column is not additive down the
      table. Add a "Scope" column or split into two tables.
- [ ] `jamovi/survival.u.yaml`: `bootstrapValN` and `rate_multiplier` have no `enable:` while
      their siblings (`seed`, `time_intervals`) do.

## [check-function] singlearm follow-ups (2026-09-02)

### [i18n] one new untranslated string

- [ ] `jamovi/i18n/tr.po`: `msgid "Survival time from dates"` (singlearm's `tint`
      checkbox label) has an empty `msgstr`. It replaced the inherited `.a.yaml` title
      "Using dates to calculate survival time", which the gate rejects as action-phrased.

### [ui] the same action-phrased title still ships in three sibling analyses

- [ ] `survival`, `survivalcont` and `multisurvival` still inherit
      `tint: title: 'Using dates to calculate survival time'` from their `.a.yaml`.
      singlearm was fixed with a `.u.yaml` `label:` override so the shared `.a.yaml`
      title and its existing Turkish translation stayed intact. Do all four together —
      either add the same override to each, or rename the `.a.yaml` title once and
      retranslate. Do not fix them one at a time.

### [gate] `tools/theme_safe_html.py` cannot see concatenated styles

- [ ] The tool reported singlearm CLEAN while `.singlearmNoticeHTML()` set an opaque
      pastel `background-color` on every notice, because the style was assembled with
      `paste0("<div style='background-color: ", style$bg, ...)` and the regex only
      matches a literal hex in the source line. Teach it to flag a `background-color:`
      whose value is an R variable, or at least to report the file for manual review.

## [i18n] singlearm: 65 newly translatable strings await Turkish (2026-09-02)

`/fix-function singlearm --apply` wrapped the whole notice layer in `.()`, so
`jmvtools::i18nUpdate()` harvested 65 new msgids into `catalog.pot` / `en.po` / `tr.po`,
all with an empty Turkish `msgstr`. singlearm now has 329 catalog entries with 16
translated (5%).

- [ ] Translate the 65 new singlearm entries in `jamovi/i18n/tr.po`. Prioritise the
      validation/refusal messages a clinician actually hits: plot-axis and cutpoint
      errors, the date-parsing family, the zero/negative/implausible follow-up checks,
      and the competing-risk median refusal.
- [ ] Two message sources remain untranslated on purpose and are NOT in this count:
      `res$error` from `survival_utils.R::.defineEventIndicator()` (shared by five
      analyses — needs one module-wide pass), and `data_quality$warnings[i]`, whose
      producer `.assessDataQuality()` always returns `character()`. That loop in
      `.run()` is dead code; delete it rather than translating it.

## [process] `.a.yaml version:` is a DERIVED field in this repo (2026-09-02)

`_updateModules.R:514 update_yaml_a_files()` rewrites every analysis `.a.yaml`
`version:` to the first three components of the package version
(`DESCRIPTION 1.0.8.06` -> `version: '1.0.8'`). Per-analysis bumps are therefore
overwritten on the next module update, which is why three separate bumps of
`singlearm`/`survival` (1.0.9, 1.0.10, 1.0.11) all reverted.

- [ ] The release-review gate says "bump `.a.yaml` version whenever the analysis
      changes materially". That is unachievable while the field is synced from
      `DESCRIPTION`. Decide which is authoritative and say so in `CLAUDE.md`:
      either stop syncing `.a.yaml` versions and bump them per analysis, or keep
      the sync and drop the per-analysis-version expectation from the gate.
      Right now 30 analyses share `1.0.8` and cannot be told apart by version.

## multisurvival — /review-function 2026-09-02, fixes applied by /fix-function --apply the same day

Runtime-confirmed bugs (one-line fixes, none applied):

- [x] [BUG] `R/multisurvival.b.R:5087` `.plotRiskGroups` competing-risk branch references undefined `cif_df` (data frame is `cif`) -> "object 'cif_df' not found" whenever risk groups are plotted on a Fine-Gray model. Fix: `max(cif$time, na.rm = TRUE)`.
- [x] [BUG] `R/multisurvival.b.R:3517` `.nomogram` `rms::datadist(mydata[, var_names])` drops to a bare vector for ONE predictor -> nomogram silently blank ("variable x does not have limits defined by datadist"). Fix: `drop = FALSE`.
- [x] [BUG] `R/multisurvival.b.R:2838` `.personTimeAnalysis` uses the option label (`Sex`) to index janitor-cleaned cleanData (`sex`) -> per-group person-time rows never appear for capitalised/spaced names. Fix: index with `cleaneddata$myexplanatory_labelled[1]`, keep the option value for the label.
- [x] [BUG] `R/multisurvival.b.R:4388,5816` `.plot3`/`.plot_adj` call `private$.isCompetingRisk()` without `plotData`; after an .omv reopen a Censored/Event/Competing hand-off is drawn as an ordinary Cox forest / "Adjusted survival". Fix: `private$.isCompetingRisk(plotData)`.
- [x] [BUG] `R/multisurvival.b.R:3705` `.plot_nomogram` reads only `private$.nom_object` (never `image$state`) -> blank nomogram after .omv reopen. Fix: `setState()` the nomogram in `.run()` and read state first.
- [x] [TEST] add regression tests using a CAPITALISED variable name (person-time group rows), a single-predictor nomogram, and Fine-Gray + plotRiskGroups render.

Clinical/statistical text:

- [x] `survMetricsTable` prints "Good/Acceptable/Limited discrimination" cut-offs while the glossary says there are no universal C-index cut-offs; drop the verdict words or align them.
- [x] `.plot_adj` competing-risk branch ignores `byplot` (standard branch honours it).
- [x] `.todo` welcome text says survival time must be "in months" and cites ggstatsplot (no longer used); `analysistype` description omits `dfs`; roxygen class doc claims ML / frailty / spline / decision-tree features that do not exist.
- [ ] Pre-fit and post-fit EPV notices plus the validation "low events" notice overlap (3 notices for one problem on small data); consolidate.
- [x] `.getData()` rejects with an HTML `<div>` message (unique in the module) — verify jamovi renders it, else use plain text.
- [x] finalfit header cell literally reads "Dependent: Surv(mytime, myoutcome)"; relabel with the user's variable names.

i18n / theme / hygiene:

- [ ] 202/209 `.()` strings untranslated in tr.po; `.todo` and all seven "Understanding ..." panels plus `.populateInteractionTables` prose are outside `.()` entirely; 2 `.()` strings carry a leading space, 1 contains `\n`, several are paste0-glued fragments ("with", "out of").
- [ ] dark hex heading colours (#856404, #0056b3, #1976D2, #34495e ...) on translucent tints in ~40 style attrs -> low contrast in dark theme; add `color: inherit`.
- [x] `timetypedata` is in NO clearWith; `timetypeoutput` missing from survMetricsTable/nomogram_display/cox_phTable/riskScoreTable etc. (113 entries added). Still open: `plot_adj` clearWith lists options the renderer never reads.
- [ ] dead code: `.formatErrorMessage`, `.sanitizeStringInput`, `.processDataInChunks`, `.competingRiskCumInc`, `.createHRTable`, `.restoreOriginalNamesInMultiSurvivalTable`, `.assessClinicalSignificance`, DEFAULT_* tree/ML constants, ~300 lines of commented-out methods (5681-5725, 5984-6105, 6505-6758, 7866-7921); `n_complete` unused (`:138`, `:2410`).
- [ ] `R/multisurvival-metrics.R:48` hardcoded bootstrap `seed = 1234` (withr::local_seed) with no user-facing option.
- [x] `warning()` at `:7860` reports a failed clinical summary only to the R console; use a notice. Also: nomogram risk-axis ticks are now adaptive (fixed 0.1-0.9 grid crashed plot.nomogram for a null single predictor) and the draw is guarded.
- [ ] `.init()` hides/restores ~30 items that already carry declarative `visible:` expressions (library-review anti-pattern).
- [x] .u.yaml/.a.yaml labels: `medianline` titled "medianline", `risktable` "Risktable", "Plot risk group survival", "Using dates to calculate survival time"; many Title Case control titles.
- [ ] menuGroup is currently `SurvivalT` (JamoviTest routing for this review); move back to `Survival` after testing.

## multisurvival — implement review recommendations (2026-09-02)

Phase 1 (behaviour + hygiene), each slice verified with parse + full multisurvival tests:

- [x] Delete dead helpers (.formatErrorMessage, .sanitizeStringInput, .processDataInChunks, .competingRiskCumInc, .createHRTable, .restoreOriginalNamesInMultiSurvivalTable, .assessClinicalSignificance, uncalled .addExplanations/.setExplanationContent), unused constants, commented-out method blocks.
- [x] Dark theme: `color: #hex` text on translucent tints -> `color: inherit` (keep borders).
- [x] `seed` option (Integer, default 1234) wired to the optimism bootstrap.
- [x] Consolidate small-sample notices into one post-fit EPV notice; keep the data-validation note.
- [x] Timepoints beyond observed follow-up -> info (not warning); invalid entries stay warnings.
- [x] Nomogram: one risk axis per valid `cutp` timepoint; scoring guide parses every "Total Points" block and drops the hardcoded 12-month header; summary names all timepoints with the unit.
- [x] `.init()`: drop imperative setVisible duplicates; add input-validity guards to renderers; fix survival-plots explanation visible expr to include km.
- [x] `plot_adj` clearWith: drop 4 unused entries; add timetypedata to todo/risk_score_analysis(2)/adjustedSurvTableSummary/adjustedMedianSummary.
- [x] Verify: visibility snapshot diff vs baseline; prepare(); scenarios.
Phase 2 (i18n):
- [x] Wrap remaining user-facing strings in `.()` (welcome, glossary, assumptions, explanation panels, interaction explanation, nomogram guide, narrative summaries); fix leading-space and `\n` strings; no paste0 fragments.
- [x] `jmvtools::i18nUpdate("en")`/("tr"), refresh catalog.pot, fill Turkish msgstr for multisurvival strings; validate placeholders. (725 strings translated; only the 4 literal defaults/package names left untranslated on purpose.)

## multisurvival — /release-review-function 2026-09-02 (after the review/fix passes above)

Verified independently (scratchpad scripts, not committed): finalfit and adjusted-Cox HR/CI strings equal `survival::coxph` (qnorm(0.975)); standardised adjusted-survival table equals manual g-computation via `survfit(newdata)` at every tabulated timepoint (<6e-4); optimism-corrected C-index reproducible with `seed` and equal to a hand-written Harrell bootstrap (apparent to 1e-8, optimism to 1e-6); landmark HRs equal coxph on the manually filtered/shifted data; DFS/OS/cause/compete event counts match the four-bucket coding; person-time overall and per-group rows equal hand sums; nomogram risk axis `1 - s0^exp(lp)` equals `survfit(cph, newdata)` to 1e-8.

Fixed in this pass:

- [x] `.run` completion notice: reverse-KM median follow-up counted competing events as censoring events in competing-risk mode (understated follow-up). Now only the `Censored` level is the reverse-KM event.
- [x] `endplot` (default 60) silently truncated KM/adjusted curves when follow-up is longer (day-scale data cut at day 60). Info notice "Plot horizon shorter than follow-up" when `km || ac` and `endplot < max(mytime)`.
- [x] `.a.yaml` `version:` 1.0.8 -> 1.0.9 (material changes since a665bbb17: seed option, dfs, notice consolidation, nomogram axes). Needs `jmvtools::prepare()` + `devtools::document()`.
- [x] Regression tests appended to `tests/testthat/test-multisurvival-release-review.R` (reverse-KM competing censoring; plot-horizon notice present/absent).

Still open (not blocking release):

- [ ] `.plot`/`.plot3`/`.plotKM` grid.text fallback messages (~10) are outside `.()`; new notice strings above need `jmvtools::i18nUpdate("en"/"tr")`.
- [ ] `.a.yaml` R `usage:` block is fully commented out, so `man/multisurvival.Rd` ships no example.
- [ ] `sty` still lists a commented-out ggstatsplot style (`t2`) and `.r.yaml` carries `# visible: (sty:t2)`; harmless.
- [ ] Landmark keeps events at exactly the landmark time (`mytime >= landmark`, time 0 after shift); matches the Zabor tutorial, documented here only.

## OncoPath release preparation (2026-09-02, from docs/audit/MODULE_AUDIT_REPORT_20260902-2017.md)

- [x] waterfall: person-time column titles; bootstrap gate on evaluable n; disclaimer STRONG_WARNING; missing-inputs ERROR; multiplicity note; warning()/message() -> notices/caption; i18n leaks + fragments; a.yaml sentence-case titles; events.js timeVar override removed; showResponseDuration moved to Clinical Reporting box
- [x] diagnosticmeta: NA column guard; funnel explanation; notices (missing inputs, zero cells under none, bivariate failure, Deeks k, AUC thresholds); I2 from metafor; dead %||%; r.yaml clearWith + dead schema; a.yaml titles; geom_errorbarh -> geom_errorbar(orientation="y"); i18n pass (197 strings)
- [x] swimmerplot: fast-path as.character; head() instead of 1:min; debug_mode; censorVar description; clearWith; multiplicity note; warning() sites; raw-mode INFO; i18n; Export titles; wizard comment
- [x] ihcheterogeneity: notices element + helpers; n<10 / ICC-not-estimable STRONG_WARNING; Levene wording; p=0 -> NA; Spearman inflation all rows; consistent observed r; psych_missing target; reject() .(); htmlEscape notes; interpretation clearWith; titles; tr.po 3 strings; i18n pass (264 strings)
- [x] dark hard-coded text colours -> inherit (waterfall 10, ihc 10, swimmer 4)
- [x] jmvtools::prepare() (0.7 min) + jmvtools::i18nUpdate(); devtools::document() run at the end
- [x] Verify: all four release-review test files pass in the sourced harness (waterfall 111, swimmerplot 100, ihcheterogeneity 65, diagnosticmeta 65 expectations)
- [x] Routed the 4 analyses back to menuGroup: OncoPath
- [ ] USER: run `Rscript _updateModules.R` to push the fixed sources to ../OncoPath, then build/install and smoke-test the four analyses in jamovi
- [ ] Not done (content work): fill the Turkish msgstr for the ~900 new msgids in jamovi/i18n/tr.po
- [ ] Left as is (cosmetic): three vestigial bare `{ }` blocks in diagnosticmeta; `magrittr` in OncoPath Imports (pipes come through dplyr)

### Working notes

- `jmvtools::prepare(".")` took 0.7 min today (memory said 20-25 min); document() timing in scratchpad/document.log
- Harness for tests without an installed package: source R/utils.R + <fn>.h.R + <fn>.b.R, gsub("ClinicoPath:::?", "") in the test file, setwd("tests/testthat") for relative data/yaml paths

## meddecide release review (2026-09-02: /check-module meddecide, agreement, decisioncurve, lassologistic)

- [x] check-module meddecide: 12/12 production analyses complete, parse, byte-identical to ../meddecide; findings filed below
- [x] agreement: cor() failure no longer replaced by an all-zero matrix in case clustering (surfaces via table note); undefined pair correlations counted in a note; warning() in Krippendorff handler removed; 2 regression tests
- [x] decisioncurve: NA ribbon rows filtered (rule curve); modelNames NULL guard; clinical rule bootstrapped like models; simultaneous sup-t band (Mandel & Betensky 2008) computed alongside pointwise, new `ciBand` option selects which is drawn; full i18n pass (2 -> 236 `.()`); release-review test file with dcurves as reference (33 expectations)
- [x] lassologistic: deleteRows() reset for all 10 tables (rows doubled on option toggles); intercept row on original scale; bootstrap replicates use the same stratified/capped fold rule as the main fit; coefficients read off the CV path (no single-lambda refit); precision NA when undefined; entities -> \u{}; importance uses the ZERO_TOL rule + truncation note; variables-vs-terms wording; model-comparison refit failures noted; pROC::plot.roc() explicit; "Sullivan" renamed Max-scaled (key `sullivan` -> `maxscaled`, column `points_sullivan` -> `points_maxscaled`) BREAKING for saved .omv; release-review test file (7 tests, glmnet/pROC reference)
- [ ] USER: `Rscript -e 'Sys.unsetenv("ELECTRON_RUN_AS_NODE"); jmvtools::prepare()'` then `devtools::document()` — decisioncurve `ciBand` option, lassologistic `maxscaled` key/column and MandelBetensky2008 ref are source-only until then; then `jmvtools::i18nUpdate("en"); i18nUpdate("tr")` for the 234 new decisioncurve msgids
- [ ] USER: decide menuGroup routing (agreement/decisioncurve were moved back to `meddecide` during the session); lassologistic left at `meddecide`
- [ ] kappaSizeCI/FixedN/Power: umbrella says `PowerT #meddecide`, ../meddecide ships stale `Power #meddecide` copies — finish testing and re-route, or prune from the submodule before release
- [x] enhancedROC: `splineCalibration`/`splineKnots` implemented (natural cubic spline of logit(p), knots-1 df; curve on the calibration plot; ICI/E50/E90/Emax columns in the calibration summary, Austin & Steyerberg 2019); knots box moved next to its checkbox and gated
- [ ] enhancedROC: the other 12 "NOT YET IMPLEMENTED" toggles (Harrell/Uno C, dynamic AUCs, competing risks, E/O ratio, Nam-D'Agostino, GND, calibration belt/density, optimism correction, external validation, decision impact, NB regression, model updating, transportability, bootstrap pAUC/cutoff CIs) still ship as live controls
- [ ] modelval.b.R:454, predmodel.b.R:433, clinicalnomograms.b.R:868: bare `plot(roc_obj)` is masked by spatstat.explore's plot.roc in the umbrella -> `pROC::plot.roc()`
- [ ] agreement: ~20 remaining `error = function(e) NULL` handlers skip rows silently (honest but silent); review case by case
- [ ] test-decisioncurve.R and test-decisioncurve-comparison.R skip entirely under plain Rscript (`skip_on_cran()`); run with `NOT_CRAN=true`

### Working notes

- jmvcore `.()` truncates any string at a space followed by `[` (translator msgctxt rule) — see memory `reference_jmvcore_translate_bracket_context_truncation`
- `devtools::load_all()` + one test file took ~20 min per run in this session

- [ ] vignettes: 14 generated `explorationt-*-comprehensive.Rmd` files carry the JamoviTest `T` routing suffix as their domain prefix, which no `domain_mapping` entry recognises, so none is distributed to a submodule. Rename to the `clinicopath-descriptives-` domain (filed 2026-09-04 from /check-function-full vartree).

## venn check (2026-09-04: /check-function venn --profile=standard)

- [x] `summary` table doubled on every re-run: `rows: 0` + `addRow()` with no `deleteRows()`, and the top-level `clearWith` covers the variables and plot options but none of the panel toggles (`showGlossary`, `clinicalSummary`, `showSetCalculations`, `showMembershipTable`, `explanatory`, ...). Ticking any of those re-entered `.run()` against the retained rows. Measured: 3 variables -> 3/6/9 rows over three `$run()` calls, then `summary$asDF` died with `duplicate 'row.names'`. Fixed with one `deleteRows()` in the reset block at the top of `.run()` (covers the early returns too); regression test in `test-venn-release-review.R`.
- [x] four HTML headings carried hardcoded hex text colours that fail WCAG in one theme or the other (`#6f42c1` 2.17:1 on dark, `#27ae60` 2.87:1 and `#e67e22` 2.85:1 on light) -> `color: inherit`, matching the rest of the file. Note `tools/theme_safe_html.py` does NOT catch this class (it only looks for an opaque `background-color` with no `color:`); grep `color: *#` yourself.
- [x] `menuGroup: Exploration` -> `ExplorationT` (JamoviTest routing per CLAUDE.md). USER: move it back after testing.
- [x] WON'T DO - `summary` rows are option-determined and the library audits flagged them for `.init()`, but three tests assert the table stays EMPTY when validation fails, and that is the correct UX: a red validation error above a table of named rows with blank counts reads as a half-finished analysis. `deleteRows()` in the reset block gives the same protection against row accumulation without that cost. Revisit only if the empty-on-failure behaviour is itself reconsidered.
- [x] an explicit `<NA>` factor level (`addNA()`, `factor(exclude = NULL)`) was counted as a NEGATIVE, not as missing. Correction to the note filed in the first pass: the comparison does not return `NA` — `Ops.factor` compares level CODES, so `f == var1true` is plain `FALSE` for the NA-level case, `is.na()` is `FALSE` so `naOmit()` keeps the row, and the case inflated `falseCount` and the denominator of every percentage while the CASE EXCLUSION warning stayed silent. Measured on 5 cases with 2 NA-level: variable A reported 3/5 = 60% positive, actually 3/3 = 100% with 2 excluded. Fixed by re-levelling factors without the NA level before `naOmit()`, so the existing exclusion warning discloses them; regression test in `test-venn-release-review.R`. Reachable through the R API; jamovi's own UI does not create `<NA>` levels.
- [x] optional variables 3-7 skipped the "contains only missing values" check that `var1`/`var2` get (`.validateVariables`); an all-NA optional variable errored with `Available levels:` and an empty list instead of naming the real problem. Same branch added for 3-7; the stale `- allow skipping if all NA` comment above the loop (which said the opposite of what the code did) removed. Regression test added.
- [x] `membershipTable` was populated (up to 500 `addRow()` calls) even when hidden. `.generateMembershipTable` now returns straight after `.writeMembershipGroups()` when `showMembershipTable` is off, so the data output costs nothing extra. The Output write moved into that new helper, which is also where the failure notice lives.
- [x] `test-venn-integration.R`: removed 12 `skip_if_not_installed("ClinicoPath")` guards plus a `skip_if_not_installed('jmvReadWrite')` that guarded nothing (ClinicoPath is not *installed* under `devtools::load_all()` or a sourced tree, so all 12 skipped silently in the normal dev loop). Un-skipping exposed that 11 of them asserted only `expect_s3_class(result, "vennResults")` despite names like "logical encoding is correct" and "percentage calculations match expected values" - every expected value was written in the comments and never asserted. Filled them in from those comments: 12 -> 45 expectations. Two setups were broken and are fixed: "all FALSE values" built `factor(rep("No", n))` with no "Yes" LEVEL, so it duplicated the absent-level test instead of testing the zero-positive path (levels now declared); "overlap counts" set the three `calculate*` toggles but never `showSetCalculations`, so the panel it asserted against was never built.

### venn deep audit (2026-09-05: /check-function-full venn)

Differential runs: every option toggled default-vs-changed with its prerequisite enabled in BOTH arms, digesting 24 components including an MD5 of each rendered PNG (styling options act in the renderer, not in `setState()`, so a state-only digest reports them all as non-effective). All 48 options effective. Two are effective only in combination and their controls stay enabled regardless:

- [x] `labelPrecisionDigits` gated with `enable: (regionLabels:percent || regionLabels:both)`. `fillColorMapping` turned out to be a real BUG rather than a gating problem: `build_palette_scale()` returns NULL for both "no palette chosen" AND "mapping off", and the fallback applied afterwards is itself a count -> colour gradient, so unticking a box labelled "Map fill colors to intersection sizes" left the regions still shaded by size. The off state now flattens the scale (low == high). `colorPalette` gated with `enable: (fillColorMapping)`, the real dependency direction. Regression test compares rendered PNGs.
- [x] `membershipGroups` write was wrapped in `try(..., silent = TRUE)`. `type: Output` options are client-driven — jmvtools emits `OptionOutput$new("membershipGroups")` with no value argument and omits the name from the generated wrapper's formals (48 formals, verified), so `venn(membershipGroups = TRUE)` is silently dropped and the branch is reachable from jamovi ONLY. That made it the single path no test, example or `asSource()` call can enter, and the silent `try` left it with no safety net: write fails -> no column -> no message anywhere. Now `tryCatch` with a `.addNotice("WARNING", ...)`. A coverage test reaches the branch by setting the OptionOutput value the way the client does (`o$.__enclos_env__$private$..membershipGroups$value <- list(value = TRUE)`).
- [x] prevalence advisory had a low tail only: `<5%` warned, `>95%` silent. Verified on n=60 — a variable positive in 2% drew two notices, the same variable at 98% and two variables at 100% drew none. Added `Very High Prevalence` alongside `Low Prevalence`; a balanced set still raises neither.
- [ ] i18n half-done: 75 `.()` calls cover the plots and most notices, but every message in the validation channel is English-only — 13 `private$.errors`, 2 `.warnings`, 2 `.info` writes, the `.addNotice` calls at `venn.b.R:1010` and `:2070`, the summary `setNote("levels", ...)`, and the welcome / glossary / set-calculations / copy-ready-sentences HTML. Route through `/prepare-translation venn`.
- [ ] i18n tooling (MODULE-WIDE, not venn-specific): 42 `catalog.pot` msgids are stored as the literal `\u{...}` escape while R parses that to the character before `.()` sees it, so those entries can never match at runtime. venn 4, chisqposttest 13, decisiongraph 12, sequentialtests 7, swimmerplot_html 2, ihcheterogeneity 2, enhancedROC 1, benford 1. Harmless while every `msgstr` is empty; it will silently defeat the first translator who fills them.
- [x] `analysisInfo` moved to the last item in `.r.yaml`, below everything it describes.
- [x] the `todo` item is removed from `.r.yaml` entirely; its one message (the ggvenn fallback) now goes through `.addNotice("INFO", ...)` like every other message, which also drops an HTML `<div>` out of the notice path.
- [x] `viridis` and `RColorBrewer` added to `plotGgVennDiagram`'s `refs:` (both already present in `00refs.yaml` with complete entries).
- [ ] the `membershipGroups` FAILURE branch still has no test: the Output results item cannot be swapped for a throwing stub (`results$...$private$.items[[...]] <-` is rejected), so only the success path is covered.

Clean: all 49 options wired and present in `.u.yaml`; all 19 results items populated; 5/5 `refs:` resolve in `00refs.yaml` with non-empty years; no dead schema, `if (FALSE)`, TODO/FIXME or debug scaffolding; no `.events.js`; roxygen examples verified against the shipped `histopathology` (columns and levels all match); library-review gates all pass. NOTE: the playbook's section E prescribes `jmvcore::Notice` + `self$results$insert()`, which CLAUDE.md forbids (protobuf serialization crash) — venn already uses the sanctioned Preformatted + `.addNotice()`/`.renderNotices()` channel and must not be migrated.

### venn code review (2026-09-05: /review-function venn)

- [x] `seq_linter`: `seq_len(length(group_labels))` -> `seq_along(group_labels)` in `.writeMembershipGroups`. The only real lintr finding; the other 52 are style (quotes/braces/infix spaces/pipe) and non-blocking.
- [x] Set Calculations now enumerates EVERY intersection order from 2 up to k, lowest order first, instead of the C(k,2) pairwise ones plus the single k-way. The row count is bounded by 2^7 - 7 - 1 = 120 because the analysis takes at most seven variables, so no size cap was needed. Regression test walks all 11 intersections of a 4-set run and checks each count against a hand-computed `rowSums(...) == length(idx)`; it fails 4/12 against the previous behaviour. The "Enable calculations" blurb no longer says "pairwise (and all-way)".
- [x] Set Calculations percentages go through one `pct()` helper using `formatC(format = "f", digits = 1)`, so a whole number prints "50.0%" rather than "50%" beside "23.5%". Covers the intersection rows and the union line.
- [x] both `which.max()` calls replaced by "every variable at the maximum". `.generateClinicalSummary` and `.generateClinicalInterpretation` now name all tied variables and switch to a plural sentence ("A, B were equally the most common (30 cases each, 30%)"); the singular wording is kept for the untied case, as a second complete `.()` sentence rather than a spliced fragment.
- [ ] 8 of the 12 `jmvcore::reject()` messages are not wrapped in `.()` (the 7 per-variable "Error processing variable" rejects and the duplicate-variable reject) - part of the i18n item above, listed here because the library gate calls out `reject()` specifically.

Verified during this review, not taken on faith:

- one denominator across engines: ggVennDiagram region labels now read share-of-all-cases (A-only 28 of 100 = "28%"), not its native share-of-union (28/66 = 42.4%), so they agree with the summary table, the ggvenn labels and the ComplexUpset bar labels.
- `sprintf`/`gettextf`: `sprintf_linter` CRASHES on this file (lintr `str2lang` cannot reconstruct the multi-line `sprintf` with an inline `if` at `.generateReportSentences`), so the check was done by walking the AST instead - 34 literal-format calls, 0 format/argument mismatches.
- lintr's two R6 blind spots checked by hand: no `&`/`|` in any scalar `if()`/`while()` condition; `codetools::checkUsage` over every private and public method reports no undefined or unused locals beyond the injected `self`/`private`.
- `<<-` at `venn.b.R:2180` is a FALSE POSITIVE: `add()` is defined in the same method frame as `advisories`, so `<<-` writes that frame, not `.GlobalEnv`. Changing it to `<-` would silently drop every advisory.
- performance is not a concern: `.run()` on 4 sets takes 0.06s at n=20000; the row-wise `paste` used to find the largest intersection costs 0.04s and runs twice per render.

### venn release review (2026-09-05: /release-review-function venn)

- [x] `.a.yaml` `version:` bumped `1.0.8` -> `1.0.9`. `../ClinicoPathDescriptives/jamovi/venn.a.yaml` still says `1.0.8` and its `R/venn.b.R` differs from the umbrella copy, i.e. two builds claimed the same analysis version while behaving differently - exactly what the version gate exists to prevent. **The submodule copy is still stale; `_updateModules.R` has to run before release.**
- [x] `asSource()` emitted a stray blank argument line: jmvcore's `.asArgs()` already returns a leading `"\n    "` and the copy-pasted `paste0(',\n    ', args)` added another. Cosmetic (the syntax parses and round-trips) but it is what the user copies out of the syntax pane. Regression test asserts no blank line, the exact opening, every `varNtrue = NULL`, and that the emitted call re-runs.
- [ ] the same `paste0(',\n    ', args)` idiom is copy-pasted into 6 more analyses and produces the same blank line there: `chisqposttest`, `contTables`, `contTablesPaired`, `crosstable`, `jjbetweenstats`, `statsplot2`. Left alone to keep this review scoped.
- [ ] `tools/ui_harness/render_ui.sh` CANNOT validate `enable:`/`visible:` binding expressions. Control-tested on venn: a deliberately malformed `enable: (regionLabels:percent |& broken` and an `enable: (noSuchOption)` both print exactly the same `placeholder present = false ; errors = undefined` as the correct file. The harness only detects the grey-skeleton / duplicate-control-name class, so a harness pass is NOT evidence that a binding expression parses. Worth stating in `vignettes/jamovi_library_review_guide.md`.

Verified in this pass (independent references, not the code's own arithmetic):

- 34 numeric comparisons in `scratchpad/verify_venn.R`, all MATCH, on n=250 / 3 sets. The reference formulation is deliberately different from the backend's: venn uses a logical matrix + `rowSums`, the check uses base-R set algebra over index sets (`Reduce(intersect/setdiff/union)`), plus `ComplexUpset::upset_data()` as a third opinion. Covered: per-variable true counts and percentages vs `table()`; all 4 intersections of orders 2-3; the 3 unique-member counts; the union; all 7 ggVennDiagram region counts AND percent labels; all 8 ComplexUpset exclusive-intersection sizes including "Outside of known sets".
- Three apparent mismatches in the first run were all faults in the verification script, not the backend (a `grab()` regex matching the TAIL of "A &amp; B" for label "B", and a mis-keyed all-negative region). Corrected, then everything matched.
- naming/case gate clean: `vennClass` is defined where the generated wrapper calls it, all 8 `venn.*` paths carry exact case, no case-colliding tracked paths, `test-zzz-analysis-file-naming.R` passes.
- metadata gate clean: `compilerMode: tame`; no committed `.tar.gz`/`.jmo`; every `clearWith` entry is a declared option; all 4 `renderFun:` are real methods; all 7 `refs:` keys resolve with exact case and carry title/author/url.
- `private$.asArgs` is inherited from `vennBase`, not a phantom method (checked because the phantom-`private$.` bug class is real here).

### crosstable audit fixes (2026-09-05: /check-function crosstable + /check-function-full crosstable)

- [x] `.u.yaml`: `pcat` control was `enable: (sty:finalfit || sty:arsenal)` although the backend honours it in gtsummary since the release review - the Fisher fix was unreachable from the UI. Widened to include gtsummary.
- [x] `.r.yaml`: `errorNotice` cleared on `sty`/`cont`/`pcat`/`p_adjust` too; title "Cross Table" -> "Cross Tables".
- [x] default `sty` nejm -> gtsummary (only style honouring every option); analysis version 1.0.9; NEWS entry.
- [x] `excl = TRUE` was silent: exclusion count now reported (WARNING above the table when > 20% of rows, INFO below otherwise); the > 20% missing-data warning is computed on the pre-exclusion data (it could never fire with exclusion on).
- [x] validator returns typed messages; severity no longer parsed out of the message text with a regex. Low expected counts: STRONG_WARNING when a chi-square is actually run (arsenal/finalfit with chi-square, all tangram styles), WARNING noting the automatic Fisher switch under gtsummary, not raised when Fisher is selected.
- [x] INFO notices moved below the table into a new `notes` Preformatted item; `analysisInfo` folded into it; `varNameWarnings` (warning about names that are handled correctly) removed with its checks.
- [x] SMD table: numeric codes labelled "continuous (numeric codes)"; `.crosstableIsCategorical` header comment no longer claims the SMD table uses it.
- [x] tests: special-character name round-trip (4 engines) + 5 audit-fix regressions in `test-crosstable.R`.
- [x] `/review-function`: `<<-` accumulator -> environment; unused `cleaned_names_mapping`/`sample_size` removed; lintr bug-class linters 0; SMD formulas match `tableone::ExtractSmd` to 6 dp.
- [x] `/review-function` recommendations: four engine branches split into `.tableArsenal/.tableFinalfit/.tableGtsummary/.tableTangram` (snapshot-compared byte-identical over 48 option combinations); `p_adjust`/`showSMD`/`showSummary` in a collapsed Advanced box; new `showSummary` copy-ready paragraph (p-values read from each engine's object, recomputed for tangram and cross-checked against the printed P=); skewness INFO note for Mean (SD) on |skewness| > 1.
- [x] `/release-review-function`: 23/24 independent reference checks matched base R across all four engines (the 24th was tangram printing "P<0.01" where the probe expected "P=0.00"; the number is right); SMD matches `tableone` to 6 dp; UI harness renders the panel with no swallowed exception; duplicate-name, naming, Collate and artefact gates clean. Fixed: phantom "Ghost" column for an empty group level (gtsummary/finalfit/tangram; chi-square NaN), finalfit/arsenal engine crashes on all-missing and single-valued variables (now named in a WARNING and left out where the engine cannot hold them), tangram summary p for continuous variables now uses the rank-based F the table prints.
- [ ] `VariableTargetListBox` suggested in the review does not exist in jamovi (0 hits in the dev docs; 352 `.u.yaml` files use `VariablesListBox` + `maxItemCount: 1`). Dropped.
- [ ] effect sizes / pairwise post-hoc (review enhancement 3): deferred - pairwise chi-square post-hoc already exists as the `chisqposttest` analysis; a per-row effect-size column needs its own design.
- [ ] translation: ~35 strings are `.()`-wrapped; the HTML panels, validator messages, SMD note and welcome text are not, and the new strings are absent from `catalog.pot`/`tr.po`. `/prepare-translation crosstable`.
- [ ] `_updateModules.R` must run before release so ClinicoPathDescriptives picks up 1.0.9.

### categorize audit fixes (2026-09-05: /check-function categorize + /check-function-full categorize)

- [x] friendly "Invalid manual break points" message was unreachable (generic validation fired first); check moved before the computation.
- [x] Break Points / Category Frequencies tables `deleteRows()` before refill (addRow appends; same defect as agepyramid).
- [x] standalone `test-categorize.R` sources `R/utils.R` again (`.fmt()` was missing, 4 tests failed silently); special-character variable-name round-trip test (Ki-67 (%), Unicode, quote) evaluating the generated R code.
- [x] `/check-function-full`: 15/16 options effective by differential run (`addtodata` is an Output, not reachable from the R API); packaged example matches base R and classInt exactly.
- [x] notices added: > 20% missing (WARNING), custom labels selected but empty (INFO), new variable name already in the data (STRONG_WARNING), `Output$set()` failure (WARNING instead of a silent nominal `<var>_cat`).
- [x] imbalance advice is method-aware: under quantile it names the tied value and its share instead of recommending quantiles to a quantile user.
- [x] Missing / Not-categorized rows carry NA (blank) instead of NaN (jamovi prints "NaN") in the percent columns; notices rendered by severity.
- [x] `refs:` now cites classInt (new 00refs entry) and Royston/Altman/Sauerbrei 2006 (`dichotomizing`) beside the module.
- [ ] `.plot` builds the ggplot twice (`ggplot_build()` only to place the break labels); left as is - one 600x400 image, and `density()`/`hist()` peaks do not match ggplot's 30 bins so labels could overlap the tallest bar.
- [ ] translation: no `.()` wrapping anywhere in `categorize.b.R` (welcome text, error boxes, notices, R-code box); `/prepare-translation categorize`.
- [ ] jamovi behaviour when the Output column name equals an existing column is untested in the GUI (the R snippet overwrites; a STRONG_WARNING now says so).

### categorize code review (2026-09-05: /review-function categorize)

- [x] BUG: a single manual cut-off never worked (`length(breaks) > 1` guard skipped the range extension; "Insufficient break points" on screen, two categories in the generated code). Guard is now `>= 1`; regression test.
- [x] Inf/-Inf crashed the run (sd() -> NaN -> `if (NA)`); now treated as missing, counted, and reported.
- [x] messages: fewer than two non-missing values is no longer called "constant value"; single manual break + exclusion gets a specific message; `.validateBreaks()` dropped its unused `method` argument.
- [x] `.a.yaml` help: mean +/- SD cuts at the mean too (four bands, SPSS Visual Binning convention); manual entries sorted/de-duplicated.
- [x] hygiene greps: no `<<-`, `library()`, `par()`/`options()`, non-ASCII; `\value` present; `::` packages all in Imports (`questionr::` is roxygen prose only). lintr bug-class linters 0; lifted-method object_usage/vector_logic 0 real (aes NSE names are in globalVariables).
- [ ] `set.seed(20240101L)` at the Jenks subsample (n > 20000 only): RNG state is saved/restored, the seed pins an approximation rather than a stochastic estimate. Left as is; a user-facing `seed` option would only matter for checking approximation stability on very large data - decide whether to expose it.
- [ ] review recommendations not implemented (separate pass): plain-language **Summary** option (copy-ready sentence with cut points and group sizes, off by default, as in crosstable); move the INFO notes below the tables into a `notes` item so the top panel carries only errors and warnings; "About / caveats" panel (when categorisation is defensible: pre-specified clinical thresholds vs data-driven cut-points, no outcome-based optimal cut-point search here).
- [ ] i18n: none of the backend strings are `.()`-wrapped (`/prepare-translation categorize`); YAML strings are already in tr.po (103 entries).

### categorize release review (2026-09-05: /release-review-function categorize)

- [x] 22 independent reference checks (base R quantile/seq/cut/prop.table/cumsum, classInt fisher) match; mean+/-SD band drop and the closed-lower-bound manual interval verified by hand.
- [x] gates: state guards, duplicate control names, theme-safe HTML, entities, setVisible/warning, rendering contract (6/6), file naming (3/3), wrapper-class/Collate/case checks module-wide, UI harness renders with no JS error; every clearWith entry is an option, renderFun exists, compilerMode tame, refs carry title/author/url, no archives committed.
- [x] 204/204 tests under devtools::load_all() (60 + 97 + 47); `.a.yaml` version 1.0.8 -> 1.0.9.
- [ ] USER: run `jmvtools::prepare()` + `devtools::document()` for the version bump (0000.yaml does not carry the analysis version, so expect a nil or one-line diff); then `_updateModules.R` so ClinicoPathDescriptives ships 1.0.9.
- [ ] manual GUI checks: (a) the added column really appears under the typed name as ordinal/nominal - categorize is the only analysis in the module that calls `Output$set()`; (b) typing the source variable's name as the new name (the strong warning is verified in R, the GUI outcome is not); (c) dark theme rendering of the notice boxes.
- [ ] `data/categorize_test.rda` is not in `_updateModules_config.yaml` data_files; no test depends on it, so it stays umbrella-only unless a vignette needs it.

### statsplot2 check (2026-09-06: /check-function statsplot2, standard profile)

- [x] args wiring: all 13 options read; `distribution` was IGNORED by the split-by (`grvar`) paths - `grouped_ggbetweenstats`/`grouped_ggscatterstats` were called without `type =`, so a "Robust"/"Nonparametric" choice silently became parametric in every split panel (verified: Welch vs Wilcoxon, Pearson vs Spearman now differ).
- [x] outputs wiring: notices/todo/ExplanationMessage/plot all populated; stale notices now cleared at the top of every `.run()` (early returns never re-rendered them).
- [x] validation: removed `.validateDesignDataMatch` - its "perfectly balanced groups suggest independent data" warning fired on every VALID long-format repeated dataset, and its divisibility check is meaningless for wide-format alluvial data; the Subject ID path already validates structure precisely. Group-balance check now keyed on the inferred type (character / integer-coded groups were skipped by `is.factor()`).
- [x] variable safety: `[[ ]]` + `rlang::sym()` throughout, no string formulas; names with spaces, `/`, `'`, `%`, `()` render in every path (smoke harness).
- [x] library-review gates: state guards, theme tool, entities, setVisible (option-driven welcome only), warning(), addRow, `visible:(!`, rendering contract 6/6. Ten dark hex text colours (`#424242/#616161/#757575`) on translucent tints -> `inherit`.
- [x] ggalluvial: `infer.label = TRUE` (deprecated, warned on every render) -> `aes(label = after_stat(stratum))`; benign "Some strata appear at multiple axes" muffled around `print()` only.
- [x] dead code: `.optionOr` (options are compiled), unused `.detectAnalysisType()` in `.init`, empty conditionals in `.plot`; render-path label now matches `.plotTypeLabel` so ERROR notices de-duplicate.
- [x] `jmvtools::prepare()` OK, `devtools::document()` OK; `0000.yaml` byte-identical to its pre-prepare snapshot; `.h.R`/`.Rd` deltas are the two description edits only.
- [x] tests: 16 pre-existing errors were stale repeated-design calls without the (earlier-added) required `subjectID`; fixed at 16 sites (one-arg where data has `patient_id`, subject columns added to three ad-hoc visual datasets, two tests now assert the rejection). 314 expectations, 0 failed, 0 errors, 31 skipped (26 vdiffr doppelgangers never reach a plot via the R wrapper).
- [x] split-by `grouped_ggbetweenstats` hard-coded `p.adjust.method = "bonferroni"` while the unsplit call used ggstatsplot's default (Holm); override removed so both paths report Holm (verified via `extract_stats()`).
- [ ] labelled parity (oddsratio pattern): n/a - plots use raw jamovi column names; no cleaned-name/label mapping exists here.
- [ ] i18n: no backend string is `.()`-wrapped (`/prepare-translation statsplot2`).
- [ ] `.getMissingPackages()` checks packages that are all in Imports, so the "Optional Packages Missing" branch can never fire in jamovi; harmless.

### statsplot2 full audit (2026-09-06: /check-function-full statsplot2, report-only)

- [x] HIGH (fixed 2026-09-06 /fix-function): every numeric read as categorical now posts a WARNING naming the variable and its distinct-value count; new `forceContinuous` option ("Treat all numeric variables as continuous") overrides the inference. Example in the .a.yaml usage block shows it on `histopathology$Grade`.
- [x] MEDIUM (fixed): split levels are `unique(na.omit())`, rows selected with `which()`; the dead empty-level block and its render-only notice are gone. Test asserts no NA stratum and no third panel.
- [x] MEDIUM (fixed): `.run()` now counts complete rows per split level and posts a STRONG_WARNING naming the omitted level(s).
- [x] LOW (fixed): stale `pairwise.comparisons` argument removed from `ggwithinstats`.
- [x] LOW (fixed): the dropped-rows line says the rows appear as an 'NA' stratum for alluvial diagrams.
- [x] LOW (fixed): summary prints "Statistical approach: not applicable (categorical comparison)" for factor x factor.
- [x] LOW (fixed): normality reminder downgraded to INFO; outlier screen now runs within each group (test: an outlier hidden by the pooled IQR is caught).
- [x] LOW (fixed): empty dataset also posts an ERROR notice.
- [x] LOW docs (partly fixed): roxygen title now "Automatic Plot Selection"; `.Rd` gains two runnable `histopathology` examples plus a commented Subject ID example; .a.yaml titles for the three sampling controls are sentence case. Left: comprehensive vignette does not mention `subjectID`/`sampleThreshold`/`sampleSize`; three legacy duplicates (`statsplot2_documentation.md`, `statsplot2-documentation.md`, `testing_statsplot2.md`) still in vignettes/.

### statsplot2 review (2026-09-06: /review-function statsplot2, report-only)

- [x] reference agreement: Welch ANOVA, Kruskal-Wallis, Welch t, Pearson, chi-square, paired t and the alluvial flow counts all match base R to 1e-6 on the same data.
- [x] hygiene greps clean (seed is the user option; `<<-` only in a comment; `jjstatsplot::`/`jmv::` are advisory text inside strings); lintr bug linters 0; codetools on lifted methods 0; stray brace pair in the outlier block removed.
- [x] tooltips added for `subjectID`, `sampleThreshold`, `sampleSize`, `seed`.
- [ ] i18n: left for `/prepare-translation statsplot2` (own playbook; regenerates module-wide catalogs). 0 backend strings wrapped; 36 of 54 catalog entries untranslated in tr.po.
- [x] UI: sampling controls and `forceContinuous` moved into a collapsed "Advanced" box; new "Output" group with `showSummary` / `showExplanations`. Checkbox labels kept: per the library-review guide's exception, a verb is right when the box acts on the data (excluding rows, subsampling, reading numerics as continuous).
- [x] clinician UX: `summary` item ("Result sentence", `showSummary`, default off) quotes method, statistic, df, p, effect size with interval and n from `extract_stats()` - one sentence per split panel; explanation/interpretation panel now behind `showExplanations` (default off, module convention). The figure is built once in `.run()` and cached for the renderer.
- [x] duplicated HTML small-sample block removed; n<10 now escalates the notice to "Very Small Sample" (descriptive statistics only).
- [x] Bayesian: interpretation states the default prior; the sentence reports `prior.distribution`/`prior.scale` from statsExpressions. NEW: a test that fails inside ggstatsplot (e.g. Bayesian one-way comparison errors in performance:: for 3+ groups here) now posts a STRONG_WARNING instead of a silent statistics-free figure.
- [ ] performance: n=30,000 violin 11.7 s full vs 3.3 s sampled (render included); acceptable, sampling stays opt-in.

- [x] fallback subtitle now "No statistical test is available for this combination"; `.safeHtmlOutput` escapes only the four structural characters (apostrophes and slashes rendered as `&#x27;`/`&#x2F;` in the sentence source).
- [ ] USER: GUI smoke pass in jamovi (new Output/Advanced boxes, result sentence, dark theme), then `_updateModules.R` and remove the `T` menu suffix.

### statsplot2 release review (2026-09-06: /release-review-function statsplot2)

- [x] verdict: ready after the two user actions below. Reference agreement extended to 20 quantities (Spearman, Mann-Whitney, robust t1way tr=0.2, Bayes t with rscale 0.707, Friedman, paired Wilcoxon, RM-ANOVA GG, Cramer's V adj.) - see report; the four p-values that differ from base R in the 4th decimal are exact-vs-asymptotic (statsExpressions always uses the asymptotic Wilcoxon) and t-approximation-vs-AS89 (Spearman).
- [x] identifier-like grouping variable (more than 20 levels and more than one level per two rows) is rejected up front; more than 20 groups gets a STRONG_WARNING with the pairwise-test count (200 one-row levels previously ran 25 s then died with "not enough observations").
- [x] `.init()` removed: it wrote install guidance into ExplanationMessage (now hidden unless `showExplanations`) and could never fire in jamovi (every checked package is in Imports); `.run()` already posts the notice.
- [x] example no longer calls `data("histopathology", package = "ClinicoPath")` (lazy data; the literal package name would fail `R CMD check --run-donttest` in jjstatsplot).
- [x] `.a.yaml` version 1.1.0 (working copy had drifted 1.0.9 -> 1.0.8 outside this session); NEWS.md entry added.
- [x] gates: UI harness renders (no JS error), duplicate control names, state guards, theme, entities, warning(), addRow, visible(!, class/Collate/case, file-naming test, rendering contract 6/6, lintr bug+brace 0, codetools 0.
- [x] tests: `devtools::load_all()` + statsplot2 files: 288 expectations, 0 failed, 0 errors, 28 skipped (26 vdiffr doppelgangers never reach a plot via the R wrapper, 2 optional data files). Sourced harness 358/0/0 (over-counts in four files).
- [ ] USER: `Rscript -e 'devtools::document()'` - `man/statsplot2.Rd` still carries the old example line (headers and 0000.yaml were regenerated at 13:23 by a prepare() run outside this session, so they are current).
- [ ] USER: `menuGroup` was moved back to `JJStatsPlot` outside this session (HEAD has `JJStatsPlotT`); left as set. GUI smoke pass, `_updateModules.R` for jjstatsplot, then commit.
- [ ] open (unchanged): i18n via `/prepare-translation statsplot2`; family uses lowercase `showexplanations` in 5 jj* analyses vs camelCase `showExplanations` here and in 57 others; vignette does not mention `subjectID`/sampling options; `cowplot` fallback and ref are dead code (patchwork is in Imports).

### jjbarstats check (2026-09-06: /check-function jjbarstats, standard profile)

- [x] args wiring: 26 options; `pairwisecomparisons` + `pairwisedisplay` REMOVED - not formals of `ggstatsplot::ggbarstats()` 1.0.0 (`...` is "Currently ignored"), so the summary/report/preset/notice/performance-note narrative about post-hoc tests described a test that never ran; all of it deleted. `padjustmethod` was first kept and moved under "Proportion tests" - then removed by the `/check-function-full` pass below (it is dead too, see there).
- [x] outputs: 10/10 items populated. `balloonPlot` item-level `clearWith` removed - `Analysis$.createImage()` returns early on a filled image, so the old PNG stayed after a counts/excl/grvar change.
- [x] escape vars: `composeTerm` in xtabs formulas, `rlang::sym` for ggbarstats, raw names for `[[`, `deparse()` in `asSource`, `htmlEscape` in HTML panels; special-character tests already in edge-cases + correctness files.
- [x] labelled parity: N/A - ggstatsplot wrapper, no finalfit/labelled table; the oddsratio pattern does not apply.
- [x] gates: no `image$state`, entities `&lt;` only, no `setVisible(FALSE)` / `warning()` / `addRow` / `visible: (!`, Imports complete in umbrella and jjstatsplot; three dark `h4` colours removed (theme tool blind spot).
- [x] small: NA no longer counted as an outcome level on the weighted branch; bare `reject()` wrapped in `.()`; UI labels (Bayes factor message + parametric-only `enable:`; "Confidence level"; "Palette"); `.a.yaml` 1.0.9 -> 1.0.10.
- [x] tests: `devtools::document()` clean (`man/jjbarstats.Rd` regenerated); `devtools::load_all()` + 7 jjbarstats files + jjstatsplot-module-review: 304 passed / 0 failed / 2 skipped / 4 errors. The 4 errors (edge-cases: all-NA outcome, single-level outcome, single-level group, zero counts) reproduce identically on a detached HEAD worktree - stale `expect_match(todo)` expectations on `jmvcore::reject()` paths, which the R wrapper raises; converted to `expect_error()` (same decision as the in-flight negative-counts test). Edge-cases file after: 33 / 0 / 0.
- [x] prepare(): clean. `R/statsplot2.h.R` restored from the pre-run snapshot afterwards: prepare wrote `version = c(1,0,8)` from the drifted `jamovi/statsplot2.a.yaml` (worktree 1.0.8, HEAD 1.0.9, NEWS/TODO/header 1.1.0).
- [ ] USER: set `jamovi/statsplot2.a.yaml` `version:` to 1.1.0 (drifted to 1.0.8 again; not touched here). GUI smoke pass in jamovi (Statistical Method box, balloon plot after changing counts, dark theme), `_updateModules.R` for jjstatsplot (tests, example and data-raw generator changed too), remove the `T` suffix, commit.
- [ ] open: presets override `resultssubtitle` / `proportiontest` invisibly (the checkbox keeps showing the manual value); `vignettes/module-development-jamovi.Rmd` still quotes the old jjbarstats yaml with pairwise options as a teaching example; `docs/` pkgdown copies regenerate on the next build; if real post-hoc tests are wanted, `chisqposttest` already exists - link to it from the interpretation panel rather than re-implement.

### ClinicoPathDescriptives module check (2026-09-06: /check-module ClinicoPathDescriptives, standard profile, batch)

- [x] scope: 14 production analyses (`menuGroup: Exploration`): agepyramid, alluvial, benford, categorize, checkdata, chisqposttest, crosstable, dataquality, outlierdetection, reportcat, summarydata, tableone, vartree, venn. Three more sit in `ExplorationT` (nonparametric, pcacomponenttest, pcaloadingtest) and were not checked.
- [x] structure: 14/14 have all five files; 42 yaml + 28 R files parse; every `.h.R` option set matches its `.a.yaml`; ../ClinicoPathDescriptives is byte-identical for `.a/.r/.u.yaml` + `.b.R` (+ `utils.R`); `.h.R` differs only in the two `package=` namespace lines.
- [x] wiring: every option is read in `.b.R` (outlierdetection `sampleThreshold`/`sampleSize` through `.optionOr()`) and has a UI control; no dangling or duplicate `.u.yaml` controls; every `enable:` is paren-wrapped; every result item is referenced (crosstable `tablestyle1-4` via `switch`, chisqposttest `plot` via `renderFun: .plot` reading `self$data`); all `refs:` defined in 00refs.yaml; no phantom `private$.x()`, `$setRows`, i18n `[..]` context, or underscored `jmvcore::format` placeholders.
- [x] notices: 0 live `jmvcore::Notice`/`insert(999`/`warning()`/`stop()` in any of the 14 (every grep hit is a comment). 11 use `.addNotice()` -> Preformatted `notices`; categorize/checkdata/reportcat write Html `notices`/`todo`/`error` directly; outlierdetection/tableone use `jmvcore::reject()`. All 14 have a user-facing path for empty data / wrong type / no variables.
- [ ] MEDIUM (3, one pattern): `vars` in alluvial, summarydata, vartree is the only `Variables`/`Variable` option (3 of 31) without `default: NULL`, so `alluvial(data)` etc. from R throws "argument 'vars' is missing, with no default" instead of showing the welcome pane (GUI unaffected). Fix: add `default: NULL` under each, then `jmvtools::prepare()` (snapshot the dirty `.h.R` + 0000.yaml first) and `_updateModules.R`. summarydata's description says the argument "is required" - decide whether that is intended before changing it.
- [ ] USER: 10 files of these analyses are modified-uncommitted in the umbrella (categorize/tableone `.b.R`; alluvial/categorize/crosstable `.h.R`; alluvial/categorize/chisqposttest/crosstable yaml) but already identical in ../ClinicoPathDescriptives - the shipped module reflects the working tree, not HEAD. Commit them.
- [ ] out of scope (umbrella only, not shipped in this module): `ihcpredict` and `mixedmodelanova` are each defined twice at top level (`R/<name>.b.R` and the generated `R/<name>.h.R` wrapper); Collate order picks one silently (memory: duplicate-toplevel-function-shadowing). Delete the `.b.R` copies.
- [ ] not run: testthat (2-9 files per analysis exist; `devtools::load_all()` takes ~20 min here, outside the static standard profile). Run `/check-module ClinicoPathDescriptives --profile=release` before the next jamovi-library submission.

### jjbarstats full audit (2026-09-06: /check-function-full jjbarstats, after the /check-function pass)

- [x] differential runs (24 options, direct-construction harness, `data/jjbarstats_test.rda`): every option changes an output EXCEPT `excl` (both branches run `complete.cases`; only the setup sentence differed) and `padjustmethod` (holm / none / bonferroni all left the per-bar p-values at 0.03, 0.03, 2.4e-04 = raw; Holm would be 0.052, 0.052, 7e-04 - `p.adjust.method` is forwarded into `statsExpressions::contingency_table()`, whose `...` ignores it). `typestatistics` nonparametric and robust produce a subtitle IDENTICAL to parametric (no such variants exist for contingency tables); only bayes differs.
- [x] FIXED (own regression from the morning pass + notices enforcement): `padjustmethod` removed everywhere; summary/report/about now say the proportion tests are unadjusted; new INFO notices "Rows with missing values excluded" (the `message()` in `.prepareData` never reached the user), "Proportion tests are unadjusted" (k groups x k outcomes tests) and "Several outcomes tested"; the "(cached)" tag (always true after `.getCachedData()`) and the false "(missing values will be handled by statistical functions)" text replaced by the dropped-row count. Tests: `padjustmethod` loops removed, invalid-List-value test now uses `label`.
- [x] agreement: aggregated `counts` run reproduces the row-level subtitle exactly (chi2(4)=24.77, V=0.19, n=300) and identical plot data; sparse 2x2 subtitle Fisher p=0.190 = `fisher.test()`; McNemar path + 3x3 rejection correct; extreme-prevalence notice fires (2.0%); balloon palettes distinct; n=30,000: frequentist plot 1.0 s, Bayes 3.5 s.
- [x] gates: rendering contract 6/6, refs (ggstatsplot, statsExpressions, ggpubr, ClinicoPathJamoviModule) all resolve in 00refs.yaml, no option read outside the schema, prepare()/document() clean, 293 passed / 0 failed / 0 errors / 2 skipped.
- [x] (fixed 2026-09-06, "fix issues") [option] `excl` REMOVED - was non-effective: rows with any NA in a selected variable are dropped on both branches. Either delete the option (the notice now reports the count) or make `excl = FALSE` keep the rows (ggbarstats drops them anyway, so the honest fix is deletion).
- [x] (fixed: list is now Frequentist (chi-squared) / Bayesian; old values rejected by the wrapper) [option] `typestatistics`: "Nonparametric" and "Robust" are aliases of "Parametric" for a contingency table. Collapse the list to Frequentist (chi-squared) / Bayesian, or relabel; the summary already reports "Pearson's chi-squared" for all three.
- [x] (fixed: sparse-2x2 STRONG_WARNING now raised once per run in `.emitDataQualityNotices`, absorbs the old low-expected-count warning, skipped for paired/Bayes; prefix "STRONG WARNING:") [notices] three overlapping warnings on one sparse 2x2 ("Small Group Sizes", "Low Expected Counts", "Chi-squared assumption violated"): merge the second into the third. `.renderNotices` prints STRONG_WARNING and WARNING with the same "WARNING:" prefix - use "STRONG WARNING:" so the paired-data and prevalence notices stand out.
- [x] (fixed: descriptions added, main description rewritten, `R: usage` example restored -> `\donttest`) [docs] six options (`dep`, `group`, `grvar`, `excl`, `typestatistics`, `originaltheme`) have no `description.R` (Rd shows "."); `description.main` carries literal quotes into the Rd; no `\examples` in the help page while `inst/examples/jjbarstats_example.R` exists (re-enable the commented `R: usage` block in the `.a.yaml`).
- [x] (fixed: backend overrides removed; new `jamovi/js/jjbarstats.events.js` sets the checkboxes on preset change in the GUI; `clinicalpreset` description says so) [ux] presets switch `resultssubtitle` (and `proportiontest` for riskfactor) on invisibly - the checkboxes keep showing the manual value. Mirror the preset into the setup text ("Preset X enabled: statistics in subtitle") or drop the overrides.
- [x] verification after "fix issues": prepare() x2 clean (description folded to one paragraph in 0000.yaml), document() clean (`man/jjbarstats.Rd` has `\donttest` example and option descriptions), 7 jjbarstats files + module-review: 284 passed / 0 failed / 0 errors / 2 skipped; run-time notice checks: one STRONG WARNING per sparse 2x2 (subtitle swapped / Fisher p quoted / split-panel caveat), none for paired or Bayes, cell counts for a 3x3; `clinicalpreset` via R leaves `resultssubtitle` FALSE.
- [ ] [upstream] report to ggstatsplot: `ggbarstats(p.adjust.method=)` is documented but has no effect on the proportion-test labels in 1.0.0.
- [x] `jamovi/statsplot2.a.yaml` set to 1.1.0 (reverted to 1.0.8 once at 15:20 by something outside prepare(), which never writes .a.yaml; re-set 15:23 and stable since). USER:  GUI smoke; `_updateModules.R`; remove `T`; commit.

### jjbarstats review (2026-09-06: /review-function jjbarstats, after the three earlier passes)

- [x] hygiene greps: no set.seed / <<- / library() / par() / options() / raw non-ASCII; `\value` present; all `::` packages in Imports. lintr review set: 0 real-bug linters fire (40 quotes_linter + 1 brace_linter are style). codetools on every lifted R6 method: 15 notes, all false positives (glue `{}` interpolation of `prep_time`/`error_context`, purrr `~` lambdas, jamovi render signature `image`/`theme`, unused `e` in handlers). Manual `&`/`|`-in-`if` scan: none. No unreferenced private methods. Rendering contract 6/6.
- [ ] [stats] interpretation guide quotes Cohen's df=1 thresholds (0.1 / 0.3 / 0.5) for Cramer's V; the subtitle's V is the bias-corrected one and for an r x c table the thresholds are 0.1/0.3/0.5 divided by sqrt(min(r, c) - 1) (3x3: 0.07 / 0.21 / 0.35, so V = 0.19 on the example data is near "medium", not "small"). Compute df* per table in `.generateInterpretationGuide()` and print the scaled thresholds; say "bias-corrected".
- [ ] [clinical] McNemar has no discordant-pair guard: `.validatePairedData()` checks total n >= 10 only. Add a STRONG_WARNING when b + c < 25 (exact binomial on the discordant pairs is the right test there) and an ERROR when b + c == 0.
- [ ] [notices] "Small Group Sizes" (< 5 rows per group) is not a chi-squared criterion and now overlaps the expected-count notice; reword as a data note or drop it.
- [ ] [ui] `addGGPubrBalloon` title "Add balloon plot (ggpubr)" starts with an action verb (no .u.yaml override) -> "Balloon plot (ggpubr)"; "Quick Clinical Setup" CollapseBox sits above the VariableSupplier -> move it below (library rule: supplier near the top).
- [ ] [ux] copy-ready report's `<button onclick=...>` is inert in exported documents and may be blocked in the results webview; drop it (the text is selectable).
- [ ] [palette] ggbarstats default palette "Set1" is red/green heavy; expose `palette` (a real ggbarstats formal) with a colour-blind-safe default such as "Dark2".
- [ ] [i18n] 10 `.()` wraps (all `reject()` messages) vs 12 notices and 11 HTML panels in plain English; tr.po carries the yaml strings but every msgstr is empty -> `/prepare-translation jjbarstats`.
- [ ] [jjpiestats] same family: `typestatistics` still offers Nonparametric/Robust, which ggpiestats treats as Parametric on a contingency table (proved for ggbarstats 2026-09-06); `pairwisecomparisons`/`padjustmethod` may be dead there too - run `/check-function jjpiestats` (out of the jjbarstats scope).
- [ ] USER (unchanged): GUI smoke pass, `_updateModules.R`, remove `T`, commit.

### ClinicoPathDescriptives module audit (2026-09-06: /audit-module ClinicoPathDescriptives, standard profile)

- [x] report: `../ClinicoPathDescriptives/docs/audit/MODULE_AUDIT_REPORT_20260906-1526.md` (1,634 lines; 14 analyses, sources byte-identical to the umbrella so every line number applies here too). 14/14 READY, 0 HIGH security, 4 MEDIUM + 4 LOW.
- [ ] MEDIUM security (2 x D, 2 x I): `gtExtras::gt_plt_summary()` emits raw variable names / factor levels into an Html item in `reportcat` (R/reportcat.b.R:253) and `summarydata` (R/summarydata.b.R:380-386) - `<img onerror>` verified live; `chisqposttest` `sprintf("%d")` on fractional weighted counts aborts the run (6 sites); `venn` a set variable named "Group"/"Row" clobbers the membership table's fixed columns (R/venn.b.R:1986-2011). `/security-audit-function` each.
- [ ] robustness: `checkdata` detects Inf/-Inf (L226) then crashes in `.computeSkewness` (L73 via L2030) and L885; `vartree` pruned-branch report describes the hierarchical tree even in pattern/sequence mode; `crosstable` arsenal `digits.count = 1` prints "21.0 (52.5%)"; `alluvial` ggalluvial x-axis shows continuous ticks; `dataquality` rounds before threshold comparison. `/review-function` alluvial, crosstable, dataquality, vartree; `/fix-function checkdata`.
- [ ] package gates (no per-function tool): drop `vcd` from Imports (unused); `outlierdetection` "Robust Mahalanobis" needs `bigutilsr` which is not in this module's Imports (declare it or remove the option); 5 dead helpers ship in `R/utils.R` (`.escapeVariableNames`, `.stripBackticks`, `raw_to_prob`, `bootstrapIDI`, `clinicopath_startup_message`); 1 imperative checkbox title (`tableone` `nonnormal`); 14 synced-but-uncommitted files in the sibling repo.
- [ ] cross-cutting: `vars` without `default: NULL` in alluvial/summarydata/vartree; i18n NONE in categorize/dataquality and PARTIAL in 10 others (`/prepare-translation`); `.run()` > 120 lines in 12/14 (LOW); dark heading colours on tints in checkdata/chisqposttest/dataquality (theme tool blind spot).
- note: two earlier reports today in the same folder (07:15, 14:05) predate 20 source-file changes; this one supersedes them.

### jjbarstats release review (2026-09-06: /release-review-function jjbarstats)

- [x] verdict: READY after the user actions below. Independent verification (base R / effectsize / BayesFactor) agrees with every subtitle quantity: Pearson chi2(4) = 24.77, p = 5.60e-05; bias-corrected Cramer's V 0.187 -> "0.19", two-sided CI [0.07, 0.26]; Fisher p = 0.190, OR 10.29 [0.40, 930.25]; McNemar chi2 = 0.26 / p = 0.61 = `mcnemar.test(correct = FALSE)` (base R default 0.11 / 0.74 - label fixed), Cohen's g 0.04; log(BF01) = -5.76 = `contingencyTableBF(indepMulti, rows, prior 1)`; per-bar proportion tests = raw GOF p (2.40e-04, 0.032, 0.026); aggregated counts reproduce row-level exactly.
- [x] gates: naming/case (wrapper class, Collate, case-only duplicates) OK; versions DESCRIPTION = 0000.yaml = CITATION.cff = 1.0.8.12, analysis 1.0.10; no committed archives; compilerMode tame; clearWith entries and renderFuns real; refs resolve with title/author/url; u.yaml controls == a.yaml options; events.js refs real; duplicate-name, state-guard, theme, entity, setVisible/warning/addRow/visible! checks clean; rendering contract 6/6. UI harness cannot render an analysis with a .events.js (documented blind spot); the two `enable:` bindings use the `(opt)` / `(opt:value)` forms shipped elsewhere in the module.
- [x] fixed in this pass (.b.R only, no regeneration needed): McNemar labelled as "chi-squared without continuity correction" in the summary method line, the assumptions bullet and the discordant-pair notice.
- [x] tests added (release-review file): subtitle vs base R/effectsize, df-scaled V cut-offs (3x3 vs 2x2), discordant-pair guard (b+c = 8 warns, 30 does not) + correction wording, palette reaches ggbarstats as package::palette with no warning and Dark2 != gdoc, removed options rejected. Suites: 284 / 0 / 0 / 2 before the additions; release-review 48, module-review 46, edge-cases 33 after, 0 failures.
- [ ] USER: GUI smoke pass in jamovi (preset flips the checkboxes; sparse 2x2 notice; balloon after changing counts; dark theme; Turkish UI), `Rscript _updateModules.R` for jjstatsplot (ships tests, example, generator, `jamovi/js/jjbarstats.events.js`), remove the `T` suffix, commit.
- [ ] open, non-blocking: i18n (`/prepare-translation jjbarstats`); jjpiestats sibling has the same alias/dead-option pattern; ggstatsplot upstream report on the inert `p.adjust.method`.

### ClinicoPathDescriptives audit fixes (2026-09-06: "fix all detected issues" from MODULE_AUDIT_REPORT_20260906-1526.md)

Acceptance: every finding in the 14 per-function sections and the module-wide gates is fixed or explicitly declined; parse-clean; `jmvtools::prepare()` clean; the 14 analyses' testthat files green; sibling regenerated by `_updateModules.R`.

- [ ] per-function fixes (one agent per analysis, owns only `R/<name>.b.R`, `jamovi/<name>.{a,u,r}.yaml`, `tests/testthat/test-<name>*.R`): security (D/I), jmvcore migration, `default: NULL`, clearWith, enable drift, notices gaps, math/presentation items, labels, theme colours, setVisible(FALSE)-as-error, option-determined addRow in .run, i18n (`.()` sweep + fragments). EXCLUDED on purpose: `.run()` length refactor (report labels it a non-defect; 12 files, pure churn).
- [ ] package gates: `vcd` -> `prune_imports` for ClinicoPathDescriptives in `_updateModules_config.yaml`; `bigutilsr` -> sibling DESCRIPTION Imports (+ importFrom shim so R CMD check does not NOTE); move `raw_to_prob`/`bootstrapIDI` -> `R/psychopdaROC_utilities.R`, `clinicopath_startup_message` -> `R/zzz.R`, `.escapeVariableNames` -> `R/survival_utils.R`, `.stripBackticks` -> new `R/formula_utils.R` shipped to jsurvival/meddecide/JamoviTest.
- [ ] Codex planning step skipped: MCP returned "gpt-6-astra requires a newer Codex CLI"; the audit report's per-finding root causes + fix pointers are the plan.
- [ ] verify: parse all changed files; snapshot `R/*.h.R` + `jamovi/0000.yaml`; `jmvtools::prepare()`; `devtools::load_all()` + testthat for the 14; `jmvtools::i18nUpdate()` catalog refresh; `Rscript _updateModules.R`; NEWS.md entry.
- note: menuGroup left at `Exploration` (not routed to JamoviTest) - moving all 14 would empty the shipped module on the next sync.

## jjbetweenstats — out-of-scope observations (from /check-function, 2026-09-07)

- [ ] **[i18n]** `R/jjbetweenstats.b.R` welcome `todo` string wraps a multi-line literal in
  `.()` (embedded newlines + leading indentation), which the library-review gate forbids.
  NOT reflowed here because the msgid is already registered in `jamovi/i18n/catalog.pot`,
  `en.po` and `tr.po` — changing it orphans the Turkish translation. Fix together with a
  catalog regeneration (`i18nUpdate` → fill → `msgfmt`).
- [ ] **[plots]** `.applyTheme()` appends `scale_fill_viridis_d()`/`scale_color_viridis_d()`
  on top of the scales ggstatsplot already set, so ticking "Use colorblind-safe palette"
  emits "Scale for fill is already present..." into the Analysis Notes on every render.
  Replace the scale instead of stacking it.
- [ ] **[export]** `.generateCopyReadyReport()` renders a `<button onclick=
  'navigator.clipboard.writeText(...)'>`. Script-driven clipboard writes do not survive
  Word/PDF export and are unreliable in the results iframe — the button reads as broken.
  Render the template as selectable text (e.g. a `<pre>`) instead.
- [ ] **[perf]** `.prepareData()` now digests `self$data` values (was dim + names, which
  missed cell edits). Measure on ~1e5 rows; if the digest is material next to the
  ggstatsplot render, key on a cheaper column-wise summary rather than reverting.

### jjbetweenstats — declined during /fix-function (2026-09-07), revisit separately

- [ ] **[stats]** Assumption checks (Levene + Shapiro/skewness) run only when
  `typestatistics == "parametric"`. The nonparametric path gets none, though Kruskal-Wallis
  assumes similar distribution shapes across groups. Adding one is new statistical
  functionality, not a bug fix — scope it deliberately.
- [ ] **[docs]** `man/jjbetweenstats.Rd` has no `\examples`; the whole `description.R.usage`
  block in the `.a.yaml` is commented out (43 lines), presumably to dodge
  `R CMD check --run-donttest`. Decide whether to restore them behind `dontrun: true`.
- [ ] **[ux]** `conflevel` allows `min: 0.5` (a 50% CI) and `ggpubrAddStats` is the only Bool
  defaulting `true`. Both are judgment calls, not defects — confirm they are intended.

### hullplot — remaining after /check-function + /check-function-full (2026-09-07)

- [ ] **[theme]** Module-wide sweep: 43 `.b.R` files still set dark hex text
  (`color: #0c5460`, `#155724`, `#856404`, `#e65100`, `#bf360c`, `#495057`) inside
  `rgba()` translucent panels — unreadable in jamovi's dark theme.
  `tools/theme_safe_html.py` reports these CLEAN (it only flags an opaque hex
  *background* with no `color:`), so it needs a mirror-case rule before the sweep.
  9 further files (`datetimeconverter`, `datevalidator`, `ggprism`, `groupedforest`,
  `missingdata`, `raincloud`, `outlierdetection`, `summarydata2`, `tidydensity`)
  build zebra table rows from an opaque `"#ffffff"`/`"#f8f9fa"` variable, which the
  scanner also misses because the colour is not a literal in the `style=` string.
  (hullplot itself is now clean.)
- [ ] **[deps]** `hullplot.b.R` calls `requireNamespace("V8")` but `V8` is in neither
  `Imports` nor `Suggests`. It currently arrives transitively via `concaveman`
  (which *is* in `Imports`), so the guard works — decide whether to declare it or
  drop the separate check and rely on `concaveman` alone.
- [ ] **[i18n]** The four `jmvcore::reject()` messages are now `.()`-wrapped, but the
  ~100 user-facing HTML strings (welcome panel, interpretation guide, assumptions
  guide, copy-ready summary, notice titles and bodies) are still untranslatable.
  Candidate for `/prepare-translation`; the catalog needs regenerating either way
  now that four new msgids exist.
- [ ] **[ux]** `group_var` and `color_var` permit `numeric`, and both are hard-cast to
  a factor with a discrete palette, so a genuinely continuous variable gets one
  legend key per distinct value. A WARNING notice now fires for both above 10
  levels, which makes the failure visible rather than silent. Tightening
  `permitted:` to `factor` would be stronger but would block legitimate
  integer-coded groups typed as continuous. Decide deliberately.
- [ ] **[results]** The stale-panel bug fixed in hullplot is latent across its siblings.
  `jjscatterstats.r.yaml` declares `todo`, `presetInfo`, `explanations`, `warnings` and
  `plot2` with **no `clearWith` at all**, which is exactly the shape that left hullplot
  showing a previous variable selection's results underneath the "select your variables"
  welcome panel. Sweep the `Continuous vs Continuous` subgroup (`jjcorrmat`, `jscattermore`,
  `jjscatterstats`, `robustcorrelation`) and then the rest of JJStatsPlot.
- [ ] **[stats]** hullplot's outlier detection is a **marginal** 1.5 x IQR rule applied to X
  and Y separately, then unioned. The output panel now says so explicitly, which closes the
  misleading-label problem, but the rule still cannot see a point that is unusual only in its
  X-Y *combination*: on a perfectly correlated cloud a point at marginal z = (1.19, -1.23),
  visibly outside its hull, is reported as "0 potential outliers". Switching to a per-group
  Mahalanobis distance against a chi-square(2) cutoff would match what the plot shows and is
  roughly four lines, but it changes what "outlier" MEANS in a clinical output, so it is a
  deliberate statistical decision rather than a bug fix. Decide before the next release.
- [ ] **[hygiene]** Four stale editor/agent backups are sitting in `R/`
  (`decision.b.R.20260829-002026.bak`, `jjdotchart.b.R.bak-20260907-131327`,
  `jjpiestats.b.R.bak-20260907-073841`, `jjpiestats.b.R.bak-20260907-100147`). `R CMD build`
  only collects `.R/.r/.q/.s/.S` so they are inert, but they are easy to commit by accident.
  Delete them or add the pattern to `.gitignore`.
- [ ] **[build]** `jjbarstats`, `jjdotplotstats`, `jjwithinstats` and `lollipop` have a
  committed `.h.R` whose `version = c(...)` is AHEAD of the `version:` in their
  `.a.yaml` (1.0.10 / 1.0.9 vs 1.0.8). Any `jmvtools::prepare(".")` silently
  rewrites those four headers *down* to 1.0.8 — it did so during this session and
  the change was reverted as out of scope. Either bump the four `.a.yaml` versions
  or accept the downgrade, but the drift will keep reappearing in unrelated diffs
  until one of the two is done.
- [x] **[data]** DONE 2026-09-08. Five bounded columns in two `jjscatterstats_*` demo
  datasets held biologically impossible negative values (56 values total): a negative Ki-67
  labelling index, a negative PD-L1 TPS, negative tumour mutational burden. Root cause was
  two-fold in `data-raw/jjscatterstats_test_data.R`: some columns were never clamped, and
  two that WERE clamped at creation had the clamp undone by a later `case_when` that
  subtracted from them. Fixed with `pmax`/`pmin` at the right point (percentages bounded
  0-100, rates/scores at 0). Verified: the four unaffected datasets regenerate
  content-identical (clamping consumes no RNG), every designed correlation moves by at most
  0.009, and `R/data_jjscatterstats_docs.R` now documents the real ranges.
- [ ] **[docs]** Every `vignettes/<fn>_documentation.md` ("Feature Mapping Specification",
  75 of them) is an UNFILLED template. All 37 rows of the jjscatterstats one read
  `X | UI Control X | self$options$X | Output item / Table` -- the option name restated four
  times, with a constant "Target Result Item" column -- so the table carries nothing that
  `<fn>-documentation.md` does not already give with types and descriptions. Worse, each ends
  in a pre-ticked "Verification Checklist" asserting the backend "references all declared
  options safely" and that results "correspond to populated output containers"; for
  jjscatterstats both claims were false on 2026-09-08 (the ggpubr panels hard-errored on any
  non-syntactic column name, and .plot3 ran a different statistic than its label declared).
  A checklist that ticks itself is worse than none. Either teach `/document-function` to fill
  the mapping from the real `.u.yaml`/`.b.R`/`.r.yaml` wiring, or drop the artefact -- but it
  is a module-wide decision: `README.Rmd` links `<fn>_documentation.html` for every function,
  so deleting the files without updating those links breaks the published site.
  (NOT a jjscatterstats duplicate: `-documentation.md` and `_documentation.md` are two
  distinct `/document-function` outputs and all 75 functions carry both.)
