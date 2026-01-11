- [ ] **Resolve Architecture Mismatch**: Debug and fix the `arm64` vs `x86_64` error encountered during module installation in jamovi. Ensure build environment consistency.
- [ ] **Namespace Synchronization**: Ensure `NAMESPACE` and `DESCRIPTION` are perfectly synced, potentially using the `sync_namespace_description` mode in `_updateModules.R`.


- [ ] **Build pkgdown Site**: Successfully build and deploy the `pkgdown` site, ensuring all vignettes are correctly included and examples are functional.


### 🧪 Stabilization & Testing
- [ ] **Promote 'To be Tested' Functions**: Prioritize unit testing for functions in the "To be Tested" category (e.g., `decisioncurve`, `stagemigration`) to move them to "Stable".
- [ ] **Smoke Test Suite**: Create a minimal CI smoke suite that runs a basic analysis for each "Stable" function to prevent regressions during the massive dependency updates.
- [ ] **Data Reproducibility**: Verify `data-raw` regen scripts for all `.rda` files in `data/` to ensure datasets are reproducible.

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
Critically evaluate the next function.
Is it mathematically and statistically accurate?
Evaluate if data flow is correct. Are arguments from .a.yaml correctly read. Is the data flow in .b.R correct. Are the results displayed in .r.yaml appropriately. Evaluate if .u.yaml is user friendly and contains all necessary options.
Is it ready to be used by clinicians and pathologists?
Is it ready for release? 
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
- [ ] **<2s** analysis response for typical datasets (n ≤ 10,000)
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
