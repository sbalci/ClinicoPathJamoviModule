# Lasso-Cox Regression Documentation

This document provides a comprehensive overview of the Lasso-Cox Regression module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Lasso-Cox Regression module (`lassocox`) performs variable selection in survival analysis using L1-penalized (LASSO) Cox proportional hazards regression. It is designed for clinicopathological research where the number of potential predictors may be large relative to the sample size.

The module's features can be broadly categorized as follows:

* **Data Suitability Assessment:** Automated traffic-light system evaluating whether data is appropriate for LASSO analysis.
* **Model Fitting:** Cross-validated LASSO Cox regression with automatic lambda selection.
* **Variable Selection:** Identification of important predictors with coefficient shrinkage.
* **Performance Metrics:** C-index, log-rank test, hazard ratio between risk groups.
* **Visualization:** Cross-validation plot, coefficient plot, risk group survival curves.
* **Risk Stratification:** Risk score calculation and patient classification.
* **Educational Output:** Optional explanations, methodology notes, and clinical guidance.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature | YAML Argument (`.a.yaml`) | UI Label | Results Section (`.r.yaml`) | R Function (`.b.R`) |
|---------|---------------------------|----------|----------------------------|---------------------|
| **Input Variables** | | | | |
| Survival time | `elapsedtime` | Time Elapsed | — | `.cleanData` |
| Event status | `outcome` | Outcome | — | `.cleanData` |
| Event level | `outcomeLevel` | Event Level | — | `.cleanData` |
| Candidate predictors | `explanatory` | Explanatory Variables | — | `.cleanData` |
| **Data Suitability** | | | | |
| Suitability assessment | `suitabilityCheck` | Data Suitability Assessment | `suitabilityReport` | `.assessSuitability` |
| Suitability HTML report | — | — | `suitabilityReport` | `.generateSuitabilityHtml` |
| **Model Options** | | | | |
| Lambda selection | `lambda` | Lambda Selection Method | `modelSummary` | `.fitModel` |
| CV folds | `nfolds` | Number of CV Folds | `modelSummary` | `.fitModel` |
| Standardization | `standardize` | Standardize Variables | — | `.cleanData` |
| **Core Results** | | | | |
| Model summary | — | Model Summary | `modelSummary` | `.populateModelSummary` |
| Selected variables | — | Selected Variables | `coefficients` | `.populateCoefficients` |
| Performance metrics | — | Model Performance | `performance` | `.populatePerformance` |
| C-index interpretation | — | — | `performance` | `.interpretCindex` |
| Hazard ratio interpretation | — | — | `performance` | `.interpretHazardRatio` |
| **Plots** | | | | |
| CV plot | `cv_plot` | Cross-validation Plot | `cv_plot` | `.cvPlot` |
| Coefficient plot | `coef_plot` | Coefficient Plot | `coef_plot` | `.coefPlot` |
| Survival plot | `survival_plot` | Risk Group Survival Plot | `survival_plot` | `.survivalPlot` |
| **Output Variables** | | | | |
| Risk score | `riskScore` | Add Risk Score to Data | `riskScore` | `.savePlotData` |
| **Explanatory Output** | | | | |
| Method explanations | `showExplanations` | Show Method Explanations | `lassoExplanation` | `.populateLassoExplanation` |
| Methodology notes | `showMethodologyNotes` | Detailed Methodology Notes | `methodologyNotes` | `.populateMethodologyNotes` |
| Clinical guidance | `includeClinicalGuidance` | Clinical Interpretation Guidance | `clinicalGuidance` | `.populateClinicalGuidance` |
| Variable importance | `showVariableImportance` | Variable Importance Analysis | `variableImportance` | `.populateVariableImportance` |
| Model comparison | `showModelComparison` | Model Comparison Analysis | `modelComparison` | `.populateModelComparison` |
| CV plot explanation | `showExplanations && cv_plot` | Understanding Cross-Validation Plot | `crossValidationExplanation` | `.populateCrossValidationExplanation` |
| Reg. path explanation | `showExplanations && coef_plot` | Understanding Regularization Path | `regularizationPathExplanation` | `.populateRegularizationPathExplanation` |
| Risk score explanation | `showExplanations && survival_plot` | Understanding Risk Scores | `riskScoreExplanation` | `.populateRiskScoreExplanation` |

## Suitability Assessment Checks

The data suitability assessment (`.assessSuitability`) evaluates 6 dimensions:

| Check | Green | Yellow | Red |
|-------|-------|--------|-----|
| Events-Per-Variable (EPV) | EPV >= 20 | 2 <= EPV < 20 | EPV < 2 |
| Regularization Need | p >= n/3 (LASSO indicated) | p <= 10 && EPV >= 20 (standard Cox may suffice) | — |
| Sample Size | n >= 100 | 20 <= n < 100 | n < 20 |
| Event Rate | 20%–80% | 10%–20% or 80%–90% | <10% or >90% |
| Multicollinearity | max \|r\| < 0.7 | 0.7 <= max \|r\| < 0.99 | max \|r\| >= 0.99 |
| Data Quality | No issues | <5% missing | >5% missing or constant predictors |

The overall verdict is determined by the most severe individual check, with a special case: if only the regularization check is yellow (standard Cox may suffice) and everything else is green, the overall verdict remains green.

## Internal Architecture

### Data Flow

```
.init()              → Package checks, welcome message
.run()               → Main pipeline:
  .cleanData()       → Validate time/outcome/predictors, create design matrix
  .assessSuitability() → 6-check traffic-light assessment (advisory)
  .fitModel()        → cv.glmnet → glmnet → coefficients → risk scores
  .populateModelSummary()   → Fill summary table
  .populateCoefficients()   → Fill selected variables table
  .populatePerformance()    → Fill C-index, log-rank, HR table
  .savePlotData()    → Extract plain numerics → setState() (protobuf-safe)
```

### State Management (Protobuf Safety)

Plot state uses only plain numeric vectors/data frames to avoid serialization errors:

| Plot | State Contents |
|------|---------------|
| `cv_plot` | `lambda`, `cvm`, `cvsd`, `cvup`, `cvlo`, `lambda_min`, `lambda_1se` (all numeric) |
| `coef_plot` | `var_names` (character), `var_importance`, `coef_values` (numeric) |
| `survival_plot` | `time`, `status`, `risk_scores` (data.frame of numerics) |

### Dependencies

| Package | Usage |
|---------|-------|
| `glmnet` | `cv.glmnet()`, `glmnet()` — LASSO fitting |
| `survival` | `Surv()`, `survfit()`, `survdiff()`, `coxph()`, `concordance()` |
| `survminer` | `ggsurvplot()` — enhanced survival plots |
| `grid` | Fallback plot error messages |
| `ggplot2` | CV and coefficient plots |

## File Locations

| File | Path | Purpose |
|------|------|---------|
| Analysis definition | `jamovi/lassocox.a.yaml` | Options/parameters |
| Results definition | `jamovi/lassocox.r.yaml` | Output items |
| UI definition | `jamovi/lassocox.u.yaml` | Interface layout |
| Backend | `R/lassocox.b.R` | Implementation |
| Auto-generated header | `R/lassocox.h.R` | Compiled from YAML |
| Test suite | `tests/testthat/test-lassocox.R` | Unit tests |
| Test data generator | `data-raw/create_lassocox_test_data.R` | Synthetic datasets |
