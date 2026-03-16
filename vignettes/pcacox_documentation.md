# PCA Cox Regression -- Feature Mapping

This document provides a comprehensive overview of the PCA Cox module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The PCA Cox module (`pcacox`) performs principal component analysis on high-dimensional predictor sets followed by Cox proportional hazards regression on the extracted components. It addresses the common genomics/proteomics challenge where the number of predictors approaches or exceeds the sample size (p >> n), making standard Cox regression impossible.

The module supports four PCA methods (supervised, standard, sparse, kernel) with automatic fallback when optional packages are unavailable. It includes comprehensive validation (bootstrap optimism-correction, permutation testing), multiple component selection strategies (cross-validation, variance threshold, fixed number), risk stratification, feature importance ranking, and sequential model comparison.

Key users include pathologists analyzing multi-marker panels, oncologists evaluating gene expression signatures, and biostatisticians performing dimensionality reduction for survival prediction.

## Feature Details

| Feature | YAML Argument (`.a.yaml`) | UI Label | Results Section (`.r.yaml`) | R Function (`.b.R`) |
|---------|---------------------------|----------|----------------------------|---------------------|
| **Input Variables** | | | | |
| Survival time | `time` | Survival Time | -- | `.run()` |
| Event status | `status` | Event Status | -- | `.run()` |
| Event level | `outcomeLevel` | Event Level | -- | `.run()` (two-level encoding) |
| Censored level | `censorLevel` | Censored Level | -- | `.run()` (two-level encoding) |
| High-dim predictors | `predictors` | High-Dimensional Predictors | -- | `.performPCA()` |
| Clinical variables | `clinical_vars` | Clinical Variables | -- | `.fitPCCoxModel()` |
| **Data Assessment** | | | | |
| Suitability check | `suitabilityCheck` | Data Suitability Assessment | `suitabilityReport` | `.assessSuitability()` |
| **PCA Configuration** | | | | |
| PCA method | `pca_method` | PCA Method | `technicalDetails` | `.performPCA()` dispatch |
| Component count | `n_components` | Number of Components | `pcaSummary` | `.selectComponents()` |
| Selection method | `component_selection` | Component Selection Method | `crossValidation` | `.selectComponents()` |
| CV folds | `cv_folds` | Cross-Validation Folds | `crossValidation` | `.performCVSelection()` |
| Variance threshold | `variance_threshold` | Variance Threshold | -- | `.selectComponents()` |
| Sparse parameter | `sparse_parameter` | Sparsity Parameter | -- | `.performSparsePCA()` |
| **Preprocessing** | | | | |
| Scale variables | `scaling` | Scale Variables | `technicalDetails` | `.performPCA()` |
| Center variables | `centering` | Center Variables | `technicalDetails` | `.performPCA()` |
| Survival weighting | `survival_weighting` | Survival-Based Weighting | -- | `.performSupervisedPCA()` |
| **Confidence & Validation** | | | | |
| Confidence level | `confidence_level` | Confidence Level | `coxResults`, `modelPerformance` | `.formatCoxResults()`, `.calculateModelPerformance()` |
| Permutation test | `permutation_test` | Permutation Test | `permutationTest` | `.performPermutationTest()` |
| Permutation count | `n_permutations` | Number of Permutations | `permutationTest` | `.performPermutationTest()` |
| Bootstrap validation | `bootstrap_validation` | Bootstrap Validation | `bootstrapValidation` | `.performBootstrapValidation()` |
| Bootstrap count | `n_bootstrap` | Bootstrap Samples | `bootstrapValidation` | `.performBootstrapValidation()` |
| **Core Results** | | | | |
| PCA summary | -- | -- | `pcaSummary` | `.populatePCASummary()` |
| Cox model | -- | -- | `coxResults` | `.formatCoxResults()` |
| Component loadings | -- | -- | `componentLoadings` | PCA method internals |
| Model performance | -- | -- | `modelPerformance` | `.calculateModelPerformance()` |
| Analysis summary | -- | -- | `summary` | `.generateSummary()` |
| Technical details | -- | -- | `technicalDetails` | `.populateTechnicalDetails()` |
| Clinical interpretation | -- | -- | `clinicalInterpretation` | `.populateClinicalInterpretation()` |
| **Additional Analyses** | | | | |
| Feature importance | `feature_importance` | Feature Importance | `featureImportance` | `.calculateFeatureImportance()` |
| Risk score | `risk_score` | Calculate Risk Score | `riskScore` | `.calculateRiskScore()` |
| Model comparison | `show_model_comparison` | Show Model Comparison | `modelComparison` | `.populateModelComparison()` |
| Feature clusters | `pathway_analysis` | Feature Cluster Analysis | `pathwayAnalysis` | `.performLoadingClusterAnalysis()` |
| **Plots** | | | | |
| Scree plot | `plot_scree` | Scree Plot | `screePlot` | `.prepareScreePlot()` + `.plotScree()` |
| Loading plots | `plot_loadings` | Loading Plots | `loadingsPlot` | `.prepareLoadingsPlot()` + `.plotLoadings()` |
| PCA biplot | `plot_biplot` | PCA Biplot | `biplot` | `.prepareBiplot()` + `.plotBiplot()` |
| Survival curves | `plot_survival` | Survival Curves by PC Score | `survivalPlot` | `.prepareSurvivalPlot()` + `.plotSurvival()` |
