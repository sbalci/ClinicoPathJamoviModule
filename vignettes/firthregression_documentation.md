# Firth's Penalized Likelihood Regression -- Feature Mapping

## Feature Summary

The Firth Regression module (`firthregression`) performs penalized likelihood regression using Firth's Jeffreys-prior bias correction for both logistic (binary outcome) and Cox proportional hazards (survival) models. Unlike LASSO/Elastic Net (which perform variable selection), Firth regression keeps all variables but produces finite, bias-corrected estimates even when standard maximum likelihood fails due to separation or rare events.

Key users include pathologists analyzing rare tumor subtypes with few events, oncologists building prognostic models with small cohorts, and biostatisticians who encounter complete or quasi-complete separation in multivariable models. The module provides profile likelihood confidence intervals (more accurate than Wald for small samples), automatic separation detection, side-by-side comparison with standard models showing bias reduction percentages, and forest plot visualization.

## Feature Details

| Feature | YAML Argument (`.a.yaml`) | UI Label | Results Section (`.r.yaml`) | R Function (`.b.R`) |
|---------|---------------------------|----------|----------------------------|---------------------|
| **Input Variables** | | | | |
| Analysis mode | `analysisType` | Analysis Type | -- | `.init()` dispatch |
| Time variable (Cox) | `time` | Time Variable | -- | `.prepareData()` |
| Outcome variable | `outcome` | Outcome Variable | -- | `.prepareData()` |
| Event level | `outcomeLevel` | Event Level | -- | `.prepareData()` encoding |
| Predictors | `predictors` | Predictor Variables | -- | `.prepareData()` |
| **Data Assessment** | | | | |
| Suitability check | `suitabilityCheck` | Data Suitability Assessment | `suitabilityReport` | `.assessSuitability()` |
| **Confidence Intervals** | | | | |
| CI level | `ciLevel` | Confidence Level | `coefficients` (CI columns) | `.populateCoefficients()` |
| CI method | `ciMethod` | CI Method | `coefficients` (CI columns) | `logistf(pl=)` / note for Cox |
| **Model Options** | | | | |
| Separation detection | `separationCheck` | Separation Detection | `separationDiagnostics` | `.runSeparationCheck()` |
| Standard comparison | `compareStandard` | Compare with Standard Model | `comparisonTable`, `coefficients` (bias_reduction) | `.populateComparison()` |
| **Core Results** | | | | |
| Coefficients | -- | -- | `coefficients` | `.populateCoefficients()` |
| Model fit | `showModelFit` | Model Fit Statistics | `modelFit` | `.populateModelFit()` |
| Notices | -- | -- | `notices` | `.addNotice()` + `.renderNotices()` |
| **Plots** | | | | |
| Forest plot | `forestPlot` | Forest Plot | `forestPlotImage` | `.prepareForestPlot()` + `.renderForestPlot()` |
| Separation plot | `separationPlot` | Separation Diagnostic Plot | `separationPlotImage` | `.prepareSeparationPlot()` + `.renderSeparationPlot()` |
| **Explanatory Output** | | | | |
| Results summary | `showSummary` | Results Summary | `summaryText` | `.showSummaryText()` |
| Method explanations | `showExplanations` | Method Explanations | `explanationText` | `.showExplanations()` |
| Instructions | -- | -- | `instructions` | `.showMessage()` |
