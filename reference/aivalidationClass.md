# AI Model Validation

Validates AI diagnostic models by calculating performance metrics (AUC,
sensitivity, specificity), comparing multiple models, and assessing
generalization through cross-validation. This simplified architecture
avoids resource limit errors while providing comprehensive diagnostic
performance evaluation essential for medical AI validation.

**Key Features:**

- ROC curve analysis with AUC calculation

- Optimal threshold selection using Youden's J statistic

- Advanced performance metrics (Matthews CC, Youden's J)

- Statistical model comparison using DeLong test

- K-fold cross-validation with stratified sampling

- Bootstrap confidence intervals for robust estimation

- Interactive ROC curve visualization

- Comprehensive methodology and interpretation guidance

**Statistical Methods:**

- AUC (Area Under the ROC Curve) for discrimination assessment

- DeLong method for AUC confidence intervals (default)

- Bootstrap resampling for robust CI estimation

- DeLong test for pairwise AUC comparisons

- Youden's J statistic for optimal threshold selection

- Matthews Correlation Coefficient for balanced performance assessment

**Cross-Validation:**

- 5-fold and 10-fold cross-validation

- Stratified sampling to maintain outcome proportions

- Mean and SD of performance metrics across folds

- Reproducible results with seed control

**Visualization:**

- Multi-predictor ROC curves with AUC labels

- Publication-ready ggplot2 graphics

- Limited to 10 predictors to prevent clutter

## Value

A results object from the `aivalidationClass` containing:

- **performanceTable**: AUC, sensitivity, specificity, optimal
  threshold, Youden's J, MCC

- **comparisonTable**: Pairwise model comparisons with DeLong test
  p-values

- **cvPerformanceTable**: Cross-validated performance metrics (mean and
  SD)

- **rocPlot**: ROC curves visualization with AUC labels

- **methodologyExplanation**: HTML explanation of statistical methods

- **resultsInterpretation**: HTML interpretation with clinical context

## Details

This analysis is specifically designed for validating AI diagnostic
models in medical contexts. It calculates essential performance metrics
and provides statistical tests for comparing multiple diagnostic models
(AI vs human, different AI algorithms, etc.).

**Performance Metrics:**

- **AUC**: Measures overall discrimination ability (0.5 = random, 1.0 =
  perfect)

- **Sensitivity**: True positive rate (crucial for screening tests)

- **Specificity**: True negative rate (important for confirmatory tests)

- **Optimal Threshold**: Determined by Youden's J (maximizes sens +
  spec - 1)

- **Youden's J**: Overall performance measure (0-1, higher is better)

- **Matthews CC**: Balanced metric for imbalanced datasets (-1 to +1)

**Model Comparison:** The DeLong test accounts for correlation between
ROC curves from the same dataset, providing appropriate statistical
comparison of AUC values. Limited to first 5 predictors to avoid
resource issues.

**Cross-Validation:** Provides robust estimates of model generalization
by training on k-1 folds and testing on the remaining fold, repeated k
times. Stratified sampling maintains outcome proportions in each fold,
which is essential for imbalanced datasets.

**Bootstrap Confidence Intervals:** More robust than DeLong method for
small samples or non-normal distributions, but requires more computation
time. Creates multiple resampled datasets and derives CIs from the
distribution of AUC values.

## References

DeLong, E. R., DeLong, D. M., & Clarke-Pearson, D. L. (1988). Comparing
the areas under two or more correlated receiver operating characteristic
curves: a nonparametric approach. *Biometrics*, 44(3), 837-845.

Youden, W. J. (1950). Index for rating diagnostic tests. *Cancer*, 3(1),
32-35.

Matthews, B. W. (1975). Comparison of the predicted and observed
secondary structure of T4 phage lysozyme. *Biochimica et Biophysica Acta
(BBA)-Protein Structure*, 405(2), 442-451.

## Super classes

[`jmvcore::Analysis`](https://rdrr.io/pkg/jmvcore/man/Analysis.html) -\>
`aivalidationBase` -\> `aivalidationClass`

## Methods

### Public methods

- [`aivalidationClass$clone()`](#method-aivalidationClass-clone)

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
- `aivalidationBase$initialize()`

------------------------------------------------------------------------

### `aivalidationClass$clone()`

The objects of this class are cloneable with this method.

#### Usage

    aivalidationClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load example data
data('medical_ai_data', package='ClinicoPath')

# Basic AI validation
aivalidation(
  data = medical_ai_data,
  predictorVars = c('AI_score', 'human_score', 'biomarker1'),
  outcomeVar = 'diagnosis',
  positiveLevel = 'positive',
  compareModels = TRUE
)

# Advanced validation with cross-validation and bootstrap
aivalidation(
  data = medical_ai_data,
  predictorVars = c('AI_score', 'human_score', 'biomarker1'),
  outcomeVar = 'diagnosis',
  positiveLevel = 'positive',
  compareModels = TRUE,
  youdensJ = TRUE,
  matthewsCC = TRUE,
  bootstrapCI = TRUE,
  nBootstrap = 2000,
  crossValidation = '5-fold',
  stratified = TRUE,
  rocPlot = TRUE,
  showExplanations = TRUE,
  showSummaries = TRUE
)
} # }
```
