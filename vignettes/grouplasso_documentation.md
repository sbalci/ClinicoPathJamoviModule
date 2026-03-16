# Group LASSO Cox Regression — Feature Mapping

This document provides a comprehensive overview of the Group LASSO for Survival Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Group LASSO Cox module (`grouplasso`) performs penalized Cox proportional hazards regression with group-level variable selection via the `grpreg` package. It is designed for clinicopathological research where predictors have natural groupings — categorical variables with multiple dummy codes, biomarker panels, gene pathways, or clinical domains.

The module supports four penalty types (Group LASSO, Group MCP, Group SCAD, Adaptive Group LASSO), three grouping strategies (automatic, factor-based, custom), and comprehensive validation tools including stability selection, nested cross-validation, and permutation testing. A traffic-light suitability assessment helps users evaluate whether their data is appropriate for this analysis.

Key users include pathologists analyzing grouped biomarker panels, oncologists evaluating gene pathway effects on survival, and biostatisticians performing high-dimensional variable selection with structured predictors.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature | YAML Argument (`.a.yaml`) | UI Label | Results Section (`.r.yaml`) | R Function (`.b.R`) |
|---------|---------------------------|----------|----------------------------|---------------------|
| **Input Variables** | | | | |
| Time to event | `time` | Time Variable | — | `.prepareData()` |
| Event indicator | `event` | Event Indicator | — | `.prepareData()` |
| Event level | `outcomeLevel` | Event Level | — | `.prepareData()` |
| Censored level | `censorLevel` | Censored Level | — | `.prepareData()` |
| Predictors | `predictors` | Predictor Variables | — | `.prepareData()` → `.createDesignMatrix()` |
| **Data Suitability** | | | | |
| Suitability assessment | `suitabilityCheck` | Data Suitability Assessment | `suitabilityReport` | `.assessSuitability()` |
| EPV check | — | — | `suitabilityReport` | `.assessSuitability()` (check 1) |
| Reduction need check | — | — | `suitabilityReport` | `.assessSuitability()` (check 2) |
| Sample size check | — | — | `suitabilityReport` | `.assessSuitability()` (check 3) |
| Multicollinearity check | — | — | `suitabilityReport` | `.assessSuitability()` (check 4) |
| Data quality check | — | — | `suitabilityReport` | `.assessSuitability()` (check 5) |
| **Group Definition** | | | | |
| Grouping method | `group_definition` | Group Definition Method | `groupSummary` | `.defineGroups()` |
| Custom group assignments | `group_structure` | Group Structure | `groupSummary` | `.customGrouping()` |
| Factor dummy grouping | `factor_grouping` | Factor Variable Grouping | `groupSummary` | `.automaticGrouping()` |
| **Penalty Specification** | | | | |
| Penalty type selection | `penalty_type` | Penalty Type | `coefficients`, `pathSummary` | `.fitGroupLasso()` |
| Group weight method | `group_weights` | Group Weight Method | `groupSummary` | `.buildGroupMultiplier()` |
| Custom weights | `custom_weights` | Custom Group Weights | `groupSummary` | `.buildGroupMultiplier()` |
| Adaptive weights method | `adaptive_weights_method` | Adaptive Weights Method | — | `.fitAdaptiveGroupLasso()` → `.getInitialEstimates()` |
| **Cross-Validation** | | | | |
| Number of CV folds | `cv_folds` | Cross-Validation Folds | `cvResults` | `.fitGrpregCox()` → `grpreg::cv.grpsurv()` |
| Lambda grid size | `n_lambda` | Number of Lambda Values | `pathSummary` | `.fitGrpregCox()` → `grpreg::cv.grpsurv()` |
| Lambda min ratio | `lambda_min_ratio` | Lambda Min Ratio | — | `.fitGrpregCox()` → `grpreg::cv.grpsurv()` |
| **Algorithm Settings** | | | | |
| Max iterations | `max_iterations` | Maximum Iterations | — | `.fitGrpregCox()` → `grpreg::cv.grpsurv()` |
| Convergence tolerance | `tolerance` | Convergence Tolerance | — | `.fitGrpregCox()` → `grpreg::cv.grpsurv()` |
| Selection threshold | `selection_threshold` | Group Selection Threshold | All tables & summary | `.populateGroupSummary()`, `.populateCoefficients()`, `.populatePerformance()` |
| Standardize variables | `standardize` | Standardize Variables | — | `.createDesignMatrix()` |
| Random seed | `random_seed` | Random Seed | — | `.fitGrpregCox()`, `.stabilitySelection()`, `.nestedCrossValidation()` |
| **Advanced Validation** | | | | |
| Stability selection | `stability_selection` | Stability Selection | `stabilityResults` | `.stabilitySelection()` |
| Bootstrap samples | `bootstrap_samples` | Bootstrap Samples | `stabilityResults` | `.stabilitySelection()` |
| Stability threshold | `stability_threshold` | Stability Threshold | `stabilityResults` | `.populateStabilityResults()` |
| Nested cross-validation | `nested_cv` | Nested Cross-Validation | `nestedCVResults` | `.nestedCrossValidation()` |
| Inner CV folds | `inner_cv_folds` | Inner CV Folds | `nestedCVResults` | `.nestedCrossValidation()` |
| Permutation test | `permutation_test` | Permutation Test | `permutationResults` | `.runPermutationTest()` |
| Number of permutations | `n_permutations` | Number of Permutations | `permutationResults` | `.runPermutationTest()` |
| **Core Results** | | | | |
| Group summary | `show_group_summary` | Show Group Summary | `groupSummary` | `.populateGroupSummary()` |
| Coefficient table | `show_coefficients` | Show Coefficients | `coefficients` | `.populateCoefficients()` |
| Regularization path table | `show_path_summary` | Show Regularization Path | `pathSummary` | `.populatePathSummary()` |
| CV results table | `show_cv_results` | Show CV Results | `cvResults` | `.populateCVResults()` |
| Model performance | — | — | `modelPerformance` | `.populatePerformance()` |
| Performance note | — | — | `modelPerformanceNote` | `.populatePerformance()` |
| **Plots** | | | | |
| Regularization path plot | `plot_regularization_path` | Group Regularization Path | `pathPlot` | `.setPathPlotState()` → `.renderPathPlot()` |
| CV curve plot | `plot_cv_curve` | Cross-Validation Curve | `cvPlot` | `.setCVPlotState()` → `.renderCVPlot()` |
| Group importance plot | `plot_group_importance` | Group Importance Plot | `importancePlot` | `.setImportancePlotState()` → `.renderImportancePlot()` |
| Stability plot | `plot_stability` | Stability Selection Plot | `stabilityPlot` | `.setStabilityPlotState()` → `.renderStabilityPlot()` |
| Group structure plot | `plot_group_structure` | Group Structure Plot | `groupStructurePlot` | `.setGroupStructurePlotState()` → `.renderGroupStructurePlot()` |
| **Clinical Output** | | | | |
| Results summary | `showSummary` | Show Summary | `summary` | `.populateSummary()` |
| Explanations panel | `showExplanations` | Show Explanations | `explanations` | `.populateExplanations()` |
| Instructions | — | — | `instructions` | `.init()` |
| Notices (fallback) | — | — | `todo` | `.insertNotice()` fallback |
