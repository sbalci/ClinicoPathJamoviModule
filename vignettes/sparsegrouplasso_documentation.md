# Sparse Group LASSO Cox Regression -- Feature Mapping

This document provides a comprehensive overview of the Sparse Group LASSO module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Sparse Group LASSO (`sparsegrouplasso`) performs penalized Cox proportional hazards regression that combines group-level variable selection with within-group sparsity. It approximates the sparse group LASSO penalty using `glmnet`'s elastic net Cox regression with penalty.factor weighting for group structure.

Key users include pathologists analyzing grouped biomarker panels, oncologists evaluating gene pathway effects on survival, and biostatisticians performing high-dimensional variable selection where predictors have natural groupings. The module supports 5 grouping strategies, 4 model selection criteria, adaptive weighting, and comprehensive validation (stability selection, bootstrap CIs, repeated CV).

The function is classified as Draft: it uses glmnet's elastic net as a principled approximation of the sparse group LASSO formulation, which is clearly disclosed in the explanations panel.

## Feature Details

| Feature | YAML Argument (`.a.yaml`) | UI Label | Results Section (`.r.yaml`) | R Function (`.b.R`) |
|---------|---------------------------|----------|----------------------------|---------------------|
| **Input Variables** | | | | |
| Time to event | `time_var` | Time Variable | -- | `.prepareData()` |
| Event indicator | `event_var` | Event Variable | -- | `.prepareData()` |
| Event level | `outcomeLevel` | Event Level | -- | `.prepareData()` |
| Censored level | `censorLevel` | Censored Level | -- | `.prepareData()` |
| Predictors | `pred_vars` | Predictor Variables | -- | `.prepareData()` -> `.convertToDummies()` |
| **Data Suitability** | | | | |
| Suitability assessment | `suitabilityCheck` | Data Suitability Assessment | `suitabilityReport` | `.assessSuitability()` |
| **Group Definition** | | | | |
| Grouping method | `group_definition` | Group Definition Method | `groupStructure` | `.defineGroups()` |
| Custom group spec | `custom_groups` | Custom Group Specification | `groupStructure` | `.customGrouping()` |
| Pathway variable | `pathway_info` | Pathway Information | `groupStructure` | `.pathwayBasedGrouping()` |
| Correlation threshold | `correlation_threshold` | Correlation Threshold | `groupStructure` | `.correlationBasedGrouping()` |
| Descriptive group names | -- | -- | `groupStructure`, `coefficients` | `.buildGroupNames()` |
| **Penalty & Lambda** | | | | |
| Alpha mixing | `alpha_sgl` | Sparse Group LASSO Alpha | `summary`, `coefficients` | `.fitSparseGroupLASSO()` |
| Lambda method | `lambda_sequence` | Lambda Sequence | `solutionPath` | `.generateLambdaSequence()` |
| Custom lambda values | `custom_lambda` | Custom Lambda Values | `solutionPath` | `.generateLambdaSequence()` |
| Lambda min ratio | `lambda_min_ratio` | Lambda Min Ratio | -- | `.generateLambdaSequence()` |
| Lambda count | `n_lambda` | Number of Lambda Values | -- | `.generateLambdaSequence()` |
| **Cross-Validation** | | | | |
| Selection criterion | `selection_criterion` | Model Selection Criterion | `summary`, `performance` | `.selectOptimalLambda()` |
| CV folds | `cv_folds` | Cross-Validation Folds | `validationResults` | `.fitSparseGroupLASSO()` |
| CV repeats | `cv_repeats` | CV Repeats | `validationResults` | `.fitSparseGroupLASSO()` (repeated averaging) |
| EBIC gamma | `ebic_gamma` | EBIC Gamma Parameter | -- | `.selectOptimalLambda()` |
| **Adaptive Weights** | | | | |
| Weight type | `weight_type` | Adaptive Weight Type | `adaptiveWeights` | `.calculateAdaptiveWeights()` |
| Weight power | `weight_power` | Weight Power | `adaptiveWeights` | `.calculateAdaptiveWeights()` |
| **Preprocessing** | | | | |
| Standardize | `standardize_vars` | Standardize Variables | -- | `.performSparseGroupLASSO()` |
| Center | `center_vars` | Center Variables | -- | `.performSparseGroupLASSO()` |
| Random seed | `seed_value` | Random Seed | -- | `.run()` |
| **Validation & Stability** | | | | |
| Bootstrap CIs | `confidence_intervals` | Calculate Confidence Intervals | `coefficients` (ci_lower, ci_upper) | `.bootstrapConfidenceIntervals()` |
| Bootstrap count | `bootstrap_samples` | Bootstrap Samples | `coefficients`, `stabilityResults` | `.bootstrapConfidenceIntervals()`, `.performStabilitySelection()` |
| CI alpha | `alpha_level` | Alpha Level | `coefficients` | `.bootstrapConfidenceIntervals()` |
| Stability selection | `stability_selection` | Stability Selection | `stabilityResults` | `.performStabilitySelection()` |
| Stability threshold | `stability_threshold` | Stability Threshold | `stabilityResults` | `.performStabilitySelection()` |
| Stability subsample | `stability_subsample` | Stability Subsample Ratio | `stabilityResults` | `.performStabilitySelection()` |
| **Core Results** | | | | |
| Summary table | `show_summary` | Show Summary Table | `summary` | `.populateSummaryTable()` |
| Coefficients table | `show_coefficients` | Show Coefficient Table | `coefficients` | `.populateCoefficientsTable()` |
| Group structure | `show_groups` | Show Group Structure | `groupStructure` | `.populateGroupStructureTable()` |
| Solution path | `show_path` | Show Solution Path | `solutionPath` | `.populateSolutionPath()` |
| Performance | `show_performance` | Show Performance Metrics | `performance`, `comparisonTable` | `.populatePerformanceTable()`, `.populateComparisonTable()` |
| Validation | `show_validation` | Show Validation Results | `validationResults` | `.populateValidationTable()` |
| **Plots** | | | | |
| CV error curve | `plot_cv_error` | Plot CV Error | `cvErrorPlot` | `.plotCVError()` -> `.renderCVErrorPlot()` |
| Coefficient path | `plot_coefficients` | Plot Coefficient Path | `coefficientPlot` | `.plotCoefficientPath()` -> `.renderCoefficientPlot()` |
| Group selection | `plot_groups` | Plot Group Selection | `groupSelectionPlot` | `.plotGroupSelection()` -> `.renderGroupSelectionPlot()` |
| Sparsity pattern | `plot_sparsity` | Plot Sparsity Pattern | `sparsityPlot` | `.plotSparsity()` -> `.renderSparsityPlot()` |
| Stability plot | `plot_stability` | Plot Stability Selection | `stabilityPlot` | `.plotStability()` -> `.renderStabilityPlot()` |
| **Clinical Output** | | | | |
| Explanations | `showExplanations` | Show Explanations | `explanations` | `.populateExplanations()` |
| Instructions | -- | -- | `instructions` | `.init()` |
| Notices fallback | -- | -- | `todo` | `.insertNotice()` fallback |
