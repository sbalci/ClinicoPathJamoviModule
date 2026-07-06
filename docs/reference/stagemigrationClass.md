# Advanced TNM Stage Migration Analysis

State-of-the-art analysis for validating TNM staging system improvements
using comprehensive statistical methods. This analysis provides
pathologists with robust tools to evaluate whether a new staging system
provides superior prognostic discrimination compared to existing
systems.

## Value

A comprehensive staging validation analysis with statistical
comparisons, clinical interpretation, and advanced visualizations

## Details

This comprehensive staging validation analysis includes:

**Core Migration Analysis:**

- Migration matrices with detailed statistics

- Stage distribution comparisons

- Will Rogers phenomenon detection

- Upstaging and downstaging quantification

**Advanced Discrimination Metrics:**

- Harrell's C-index with confidence intervals

- Net Reclassification Improvement (NRI)

- Integrated Discrimination Improvement (IDI)

- Time-dependent ROC analysis

- Likelihood ratio tests for nested models

**Clinical Utility Assessment:**

- Decision Curve Analysis (DCA)

- Net benefit calculations

- Clinical significance thresholds

- Cancer-type specific interpretations

**Validation Framework:**

- Bootstrap validation with optimism correction

- Cross-validation options

- Stability assessment

- Internal validation metrics

**Advanced Visualizations:**

- Migration heatmaps with flow statistics

- Time-dependent ROC curves

- Calibration plots

- Decision curves

- Forest plots with confidence intervals

**PHASE 1 ENHANCEMENTS - Evidence-Based Assessment Framework:**

- **Will Rogers Evidence Assessment:** Multi-criteria evaluation
  framework

- **Migration Pattern Analysis:** Advanced flow statistics and retention
  rates

- **Survival Pattern Validation:** Upstaged patient survival similarity
  analysis

- **Biological Consistency Checks:** Risk factor profile assessments

- **Landmark Analysis Integration:** Time-based cutoff discrimination
  analysis

- **Clinical Decision Support:** Evidence-based implementation
  recommendations

- **Traffic Light Assessment:** PASS/BORDERLINE/CONCERN/FAIL evidence
  grading

- **Enhanced Heatmap Analytics:** Major flow identification and net
  migration analysis

## Clinical Applications

- TNM staging system validation (7th to 8th edition transitions)

- AJCC staging improvements

- Institution-specific staging modifications

- Multi-institutional staging harmonization

- Biomarker-enhanced staging systems

## Statistical Methods

The analysis implements state-of-the-art methods for staging validation:

- **NRI:** Quantifies net improvement in risk classification

- **IDI:** Measures integrated discrimination improvement

- **C-index:** Harrell's concordance with bootstrap confidence intervals

- **DCA:** Clinical utility across decision thresholds

- **Bootstrap:** Internal validation with bias correction

## Clinical Decision Framework

Results include comprehensive guidance for staging system adoption:

- Statistical significance vs. clinical importance

- Effect size interpretation (small, medium, large improvements)

- Sample size adequacy assessment

- Recommendation confidence levels

- Implementation considerations

## Data Requirements

- **Sample Size:** Minimum 30 patients (100+ recommended)

- **Follow-up:** Adequate survival time for meaningful analysis

- **Staging:** Both old and new staging variables with 2+ levels

- **Events:** Binary event indicator (0/1) or factor with specified
  level

- **Data Quality:** Complete case analysis (missing values removed)

## Troubleshooting

- **"TRUE/FALSE error":** Check for missing values in staging or
  survival variables

- **"Not atomic error":** Disable individual tables to isolate
  problematic components

- **Model fitting errors:** Ensure adequate sample size and event rate
  (5-95%)

- **Stage level errors:** Verify staging variables have multiple
  distinct levels

## See also

[`concordance`](https://rdrr.io/pkg/survival/man/concordance.html) for
C-index calculations,
[`ggsurvplot`](https://rdrr.io/pkg/survminer/man/ggsurvplot.html) for
survival visualizations

## Super classes

[`jmvcore::Analysis`](https://rdrr.io/pkg/jmvcore/man/Analysis.html) -\>
`stagemigrationBase` -\> `stagemigrationClass`

## Methods

### Public methods

- [`stagemigrationClass$clone()`](#method-stagemigrationClass-clone)

Inherited methods

- [`jmvcore::Analysis$.createImage()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.createImage)
- [`jmvcore::Analysis$.createImages()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.createImages)
- [`jmvcore::Analysis$.createPlotObject()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.createPlotObject)
- [`jmvcore::Analysis$.getSessionTemp()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.getSessionTemp)
- [`jmvcore::Analysis$.load()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.load)
- [`jmvcore::Analysis$.render()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.render)
- [`jmvcore::Analysis$.save()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.save)
- [`jmvcore::Analysis$.savePart()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.savePart)
- [`jmvcore::Analysis$.setCheckpoint()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.setCheckpoint)
- [`jmvcore::Analysis$.setParent()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.setParent)
- [`jmvcore::Analysis$.setReadDatasetHeaderSource()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.setReadDatasetHeaderSource)
- [`jmvcore::Analysis$.setReadDatasetSource()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.setReadDatasetSource)
- [`jmvcore::Analysis$.setResourcesPathSource()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.setResourcesPathSource)
- [`jmvcore::Analysis$.setStatePathSource()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.setStatePathSource)
- [`jmvcore::Analysis$addAddon()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-addAddon)
- [`jmvcore::Analysis$asProtoBuf()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-asProtoBuf)
- [`jmvcore::Analysis$asSource()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-asSource)
- [`jmvcore::Analysis$check()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-check)
- [`jmvcore::Analysis$init()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-init)
- [`jmvcore::Analysis$optionsChangedHandler()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-optionsChangedHandler)
- [`jmvcore::Analysis$postInit()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-postInit)
- [`jmvcore::Analysis$print()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-print)
- [`jmvcore::Analysis$readDataset()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-readDataset)
- [`jmvcore::Analysis$run()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-run)
- [`jmvcore::Analysis$serialize()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-serialize)
- [`jmvcore::Analysis$setError()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-setError)
- [`jmvcore::Analysis$setStatus()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-setStatus)
- [`jmvcore::Analysis$translate()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-translate)
- `stagemigrationBase$initialize()`

------------------------------------------------------------------------

### `stagemigrationClass$clone()`

The objects of this class are cloneable with this method.

#### Usage

    stagemigrationClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic staging comparison
stagemigration(
    data = cancer_data,
    oldStage = "old_stage",
    newStage = "new_stage",
    survivalTime = "survival_months",
    event = "outcome",
    eventLevel = "DEAD",
    analysisType = "basic"
)

# Comprehensive analysis with all options
stagemigration(
    data = lung_cancer_cohort,
    oldStage = "tnm7_stage",
    newStage = "tnm8_stage",
    survivalTime = "os_months",
    event = "death",
    eventLevel = "dead",
    analysisType = "comprehensive",
    calculateNRI = TRUE,
    performBootstrap = TRUE,
    bootstrapReps = 1000
)

# PHASE 1 ENHANCED: Evidence-based Will Rogers assessment
stagemigration(
    data = pancreatic_cohort,
    oldStage = "T_AJCC8",
    newStage = "T_modified",
    survivalTime = "overall_survival_months",
    event = "death_status",
    eventLevel = "Dead",
    analysisType = "publication",
    advancedMigrationAnalysis = TRUE,
    showMigrationHeatmap = TRUE,
    cancerType = "other",
    showExplanations = TRUE
)

# Phase 1 Enhanced with landmark analysis for lung cancer
stagemigration(
    data = lung_staging_data,
    oldStage = "stage_7th_edition",
    newStage = "stage_8th_edition",
    survivalTime = "survival_months",
    event = "vital_status",
    eventLevel = "deceased",
    analysisType = "comprehensive",
    advancedMigrationAnalysis = TRUE,
    cancerType = "lung", # Uses lung-specific landmark times: 3,6,12,24 months
    showWillRogersVisualization = TRUE,
    showMigrationSurvivalComparison = TRUE
)
} # }
```
