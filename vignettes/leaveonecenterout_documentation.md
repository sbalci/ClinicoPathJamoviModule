# Feature Mapping: leaveonecenterout

## Function Overview

| Property | Value |
|---|---|
| Name | `leaveonecenterout` |
| Menu | meddecideT > Prediction Models |
| Title | Leave-One-Center-Out CV |
| Subtitle | Internal-external validation for multi-institutional models |
| Version | 0.0.37 |

## Feature-to-Code Mapping

| Feature | .a.yaml Option | .u.yaml Control | .r.yaml Output | .b.R Method | Packages |
|---|---|---|---|---|---|
| Outcome encoding | `outcome`, `outcomeLevel` | VariablesListBox + LevelSelector | -- | `.prepareData()` | -- |
| Center folding | `centerVariable` | VariablesListBox | `designSummary` | `.prepareData()`, `.runLOOCV()` | -- |
| Logistic regression | `modelType=logistic` | ComboBox | `perCenterResults` | `.fitGLM()` | -- |
| Logistic LASSO | `useLasso=true` | CheckBox | `perCenterResults` | `.fitLassoLogistic()` | `glmnet` |
| Cox regression | `modelType=cox` | ComboBox | `perCenterResults` | `.fitCox()` | `survival` |
| Cox LASSO | `useLasso=true` | CheckBox | `perCenterResults` | `.fitLassoCox()` | `glmnet`, `survival` |
| Linear regression | `modelType=linear` | ComboBox | `perCenterResults` | `.fitLinear()` | -- |
| AUC evaluation | (automatic) | -- | `perCenterResults.auc` | `.evaluateBinary()` | `pROC` |
| C-index evaluation | (automatic) | -- | `perCenterResults.auc` | `.fitCox()` | `survival` |
| Brier score | `calibrationCheck` | CheckBox | `perCenterResults.brier` | `.evaluateBinary()` | -- |
| Lambda selection | `lambdaMethod` | ComboBox | -- | `.fitLassoLogistic()`, `.fitLassoCox()` | `glmnet` |
| Pooled performance | `pooledPerformance` | CheckBox | `pooledPerformance` | `.populatePooledPerformance()` | -- |
| Forest plot | `forestPlot` | CheckBox | `forestplot` | `.forestPlot()` | `ggplot2` |
| Interpretation | (automatic) | -- | `interpretation` | `.populateInterpretation()` | -- |
| Reproducibility | `random_seed` | TextBox | -- | `set.seed()` | -- |
| Data quality notices | (automatic) | -- | `notices` | `.addNotice()`, `.renderNotices()` | -- |

## Option Effect Matrix

| Option | designSummary | perCenterResults | pooledPerformance | forestplot | interpretation | notices |
|---|---|---|---|---|---|---|
| `outcome` | X | X | X | X | X | X |
| `outcomeLevel` | X | X | X | X | X | X |
| `predictors` | X | X | X | X | X | X |
| `centerVariable` | X | X | X | X | X | X |
| `elapsedtime` | X | X | X | X | X | X |
| `modelType` | X | X | X | X | X | X |
| `useLasso` | X | X | X | X | X | X |
| `lambdaMethod` | X | X | X | X | -- | X |
| `random_seed` | X | X | X | X | X | X |
| `pooledPerformance` | -- | -- | visibility | -- | -- | -- |
| `forestPlot` | -- | -- | -- | visibility | -- | -- |
| `calibrationCheck` | -- | column visibility | -- | -- | -- | -- |

## Visibility Rules

| Element | Condition | Effect |
|---|---|---|
| `pooledPerformance` table | `pooledPerformance == true` | Shows/hides entire table |
| `forestplot` image | `forestPlot == true` | Shows/hides plot |
| `n_events_test` column | `modelType:logistic \|\| modelType:cox` | Hidden for linear |
| `accuracy` column | `modelType:logistic` | Hidden for cox/linear |
| `brier` column | `calibrationCheck == true` | Shows/hides column |
| `lambdaMethod` ComboBox | `useLasso == true` | Enabled/disabled |
| `outcomeLevel` LevelSelector | `outcome` selected | Enabled/disabled |
