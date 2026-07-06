# SCAD/MCP Cox Regression for Variable Selection in Survival Analysis

> **Note:** The
> [`ncvregcox()`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/ncvregcox.md)
> function is designed for use within **jamovi’s GUI**. The code
> examples below show the R syntax for reference. To run interactively,
> use
> [`devtools::load_all()`](https://devtools.r-lib.org/reference/load_all.html)
> and call the R6 class directly:
> `ncvregcoxClass$new(options = ncvregcoxOptions$new(...), data = mydata)`.

## SCAD/MCP Cox Regression

### Overview

The **SCAD/MCP Cox Regression** module (`ncvregcox`) performs variable
selection using non-convex penalties - **SCAD** (Smoothly Clipped
Absolute Deviation) and **MCP** (Minimax Concave Penalty) - for Cox
proportional hazards models. These penalties solve a fundamental problem
with LASSO: **over-penalization of large coefficients**.

Key features:

- **Two non-convex penalty types**: SCAD and MCP
- **Oracle properties**: Consistent selection + unbiased estimation for
  large effects
- **Data suitability assessment** (traffic-light system with 6 checks)
- **Cross-validated lambda selection** (min or 1-SE rule)
- **Regularization path** visualization
- **Variable importance** analysis

------------------------------------------------------------------------

### Why Non-Convex Penalties?

LASSO applies a constant penalty rate regardless of coefficient size,
which means:

- Small (noise) coefficients: shrunk to zero (good)
- Large (true) coefficients: also over-shrunk (bad - introduces bias)

**SCAD** and **MCP** use penalties that flatten out for large
coefficients, solving this problem. The key insight is that once a
coefficient is clearly non-zero, there is no reason to keep penalizing
it.

#### Penalty Comparison

| Penalty | Available Here? | Behavior | Bias for Large Effects | Selection |
|----|:--:|----|----|----|
| **SCAD** | Yes | Penalty flattens at $`\gamma\lambda`$ | Nearly unbiased | Oracle property |
| **MCP** | Yes | Penalty flattens sooner | Nearly unbiased | Oracle property |
| LASSO | No (see `lassocox`) | Constant penalty rate | High bias | Consistent under conditions |
| Ridge | No | Quadratic penalty | Moderate bias | No selection |

> **Note:** LASSO is *not* available in this module. For LASSO-penalized
> Cox regression, use the dedicated **LASSO Cox** (`lassocox`) function,
> which includes its own suitability assessment and is optimized for the
> L1 penalty. This module focuses exclusively on non-convex penalties
> that offer oracle properties LASSO cannot provide.

#### Mathematical Formulation

**SCAD penalty** (Fan & Li, 2001):
``` math
P(\beta) = \begin{cases}
\lambda |\beta| & \text{if } |\beta| \leq \lambda \\
\frac{-(\beta^2 - 2\gamma\lambda|\beta| + \lambda^2)}{2(\gamma-1)} & \text{if } \lambda < |\beta| \leq \gamma\lambda \\
\frac{(\gamma+1)\lambda^2}{2} & \text{if } |\beta| > \gamma\lambda
\end{cases}
```

**MCP** (Zhang, 2010):
``` math
P(\beta) = \begin{cases}
\lambda |\beta| - \frac{\beta^2}{2\gamma} & \text{if } |\beta| \leq \gamma\lambda \\
\frac{\gamma\lambda^2}{2} & \text{if } |\beta| > \gamma\lambda
\end{cases}
```

The default $`\gamma = 3.7`$ for SCAD was recommended by Fan & Li (2001)
based on Bayesian arguments and minimization of a Bayesian risk. For
MCP, the recommended default is $`\gamma = 3.0`$ (Zhang, 2010).

Both penalties behave like LASSO near zero (good for eliminating noise)
but stop penalizing coefficients once they exceed the threshold
$`\gamma\lambda`$. This two-regime behavior is what gives them the
**oracle property**: under regularity conditions, they select the
correct model and estimate non-zero coefficients as if the true model
were known in advance.

------------------------------------------------------------------------

### When to Use SCAD/MCP

| Scenario | Recommendation |
|----|----|
| Suspect strong true effects that LASSO would bias | **SCAD or MCP** |
| Building a model for publication (unbiased coefficients) | **SCAD recommended** |
| Want oracle property without two-stage procedure | **SCAD or MCP** |
| Moderate number of predictors | **MCP** (more aggressive selection) |
| Very high dimensional (p \>\> n) | Consider `lassocox` - LASSO may be more stable |
| Need guaranteed convexity for optimization | Use `lassocox` |

------------------------------------------------------------------------

### Datasets Used in This Guide

All datasets ship with the package and can be loaded directly:

| Dataset | N | Covariates | Description |
|----|----|----|----|
| `ncvregcox_clinical` | 200 | 14 mixed | Clinical study with known effects |
| `ncvregcox_sparse` | 100 | 30 continuous | Many noise variables, few true signals |
| `ncvregcox_small` | 25 | 5 | Small-sample edge case for EPV warnings |
| `ncvregcox_collinear` | 150 | 10 | Extreme multicollinearity (r \> 0.9) |

------------------------------------------------------------------------

### 1. Loading Test Data

``` r

# Load the clinical dataset (n=200, 14 covariates)
data(ncvregcox_clinical, package = "ClinicoPath")
str(ncvregcox_clinical)
#> 'data.frame':    200 obs. of  16 variables:
#>  $ time            : num  13.9 16.5 14.4 8.6 20.4 6.9 15.4 30.8 60 34.7 ...
#>  $ event           : Factor w/ 2 levels "0","1": 2 1 2 2 1 2 1 2 1 1 ...
#>  $ age             : num  73 67 61 60 75 76 68 61 49 50 ...
#>  $ bmi             : num  26.5 28.8 22.8 27.5 31.4 21.7 22.9 27.2 30.7 28.6 ...
#>  $ tumor_diameter  : num  10.5 5.4 3.9 2.2 5.9 1.1 4 3.3 4.2 5.2 ...
#>  $ ldh_level       : num  132 315 258 214 249 143 213 80 216 198 ...
#>  $ crp             : num  3.8 5.6 12.6 0.7 3 16.6 0.3 0.9 5.6 17.1 ...
#>  $ albumin         : num  3.8 3.6 3.3 3.6 3.7 3.4 3.4 3.1 4 3.5 ...
#>  $ cea_level       : num  1.2 9.4 0.7 12.6 1.4 2.5 1.3 4.5 8.1 7 ...
#>  $ wbc_count       : num  6.8 10.4 5.8 2 10.4 12.1 4.5 3.2 6.3 5.4 ...
#>  $ neutrophil_ratio: num  65.2 78.3 49.8 53.4 50.7 51.8 68.5 48.4 58 43 ...
#>  $ platelet_count  : num  166 340 233 310 243 218 278 209 163 291 ...
#>  $ gender          : Factor w/ 2 levels "F","M": 1 2 2 2 2 2 2 1 2 2 ...
#>  $ t_stage         : Factor w/ 4 levels "T1","T2","T3",..: 3 1 2 3 1 1 1 2 2 4 ...
#>  $ n_stage         : Factor w/ 3 levels "N0","N1","N2": 2 2 1 2 1 1 1 1 2 1 ...
#>  $ histology       : Factor w/ 3 levels "Adenocarcinoma",..: 2 1 2 2 1 3 2 1 2 2 ...
cat("N:", nrow(ncvregcox_clinical),
    "Events:", sum(ncvregcox_clinical$event == "1", na.rm = TRUE),
    "Event rate:", round(mean(ncvregcox_clinical$event == "1", na.rm = TRUE), 2), "\n")
#> N: 200 Events: 149 Event rate: 0.74
```

**True effects in `ncvregcox_clinical`:** age (weak), tumor_diameter
(strong), ldh_level (moderate), cea_level (weak-moderate), t_stage T3/T4
(strong), n_stage N2 (strong). All other variables are noise.

``` r

# Load the sparse/high-dimensional dataset (n=100, 30 covariates)
data(ncvregcox_sparse, package = "ClinicoPath")
cat("N:", nrow(ncvregcox_sparse),
    "Events:", sum(ncvregcox_sparse$event == "1", na.rm = TRUE),
    "Covariates:", ncol(ncvregcox_sparse) - 2, "\n")
#> N: 100 Events: 66 Covariates: 30
```

**True effects in `ncvregcox_sparse`:** x1 (+0.8), x5 (-0.6), x12
(+0.7), x20 (-0.4). The remaining 26 variables are noise with
block-correlated structure.

------------------------------------------------------------------------

### 2. Data Suitability Assessment

Before running the penalized regression, the module performs a
comprehensive suitability check with a traffic-light system
(green/yellow/red) across 6 dimensions:

1.  **Events-Per-Variable (EPV)** - are there enough events relative to
    predictors?
2.  **Regularization Need** - is the dimensionality high enough to
    warrant penalization?
3.  **Sample Size** - is n adequate for stable cross-validation?
4.  **Event Rate** - is the event/censoring balance reasonable?
5.  **Multicollinearity** - are predictors highly correlated?
6.  **Data Quality** - how much missing data exists?

``` r

# Run with suitability check enabled (default)
ncvregcox(
  data = ncvregcox_clinical,
  time = "time",
  event = "event",
  outcomeLevel = "1",
  censorLevel = "0",
  covariates = c("age", "bmi", "tumor_diameter", "ldh_level", "cea_level",
                 "t_stage", "crp", "albumin", "wbc_count",
                 "neutrophil_ratio", "platelet_count", "gender",
                 "n_stage", "histology"),
  penalty = "SCAD",
  suitabilityCheck = TRUE,
  cv_folds = 10,
  lambda_type = "min",
  plot_path = FALSE,
  plot_cv = FALSE,
  variable_importance = FALSE
)
#> 
#>  SCAD COX REGRESSION RESULTS
#> ERROR: Analysis Error
#> missing value where TRUE/FALSE needed
#>  SCAD/MCP Cox Regression Analysis
#> 
#>  Non-convex penalized Cox regression for high-dimensional survival data
#>  analysis.
#> 
#>  Current Configuration:
#> 
#>  Penalty Function: Smoothly Clipped Absolute Deviation
#>  (SCAD)Cross-Validation: 10-fold CVLambda Selection: Minimum CV Error
#> 
#>  Key Features:
#> 
#>  Oracle properties for variable selectionAvoids over-penalization of
#>  large coefficientsMaintains sparsity for irrelevant
#>  variablesCross-validation for optimal penalty selectionVariable
#>  importance and stability analysis
#> 
#>  Note: SCAD penalty provides superior variable selection properties
#>  compared to LASSO, particularly for scenarios with large true effects.
#> 
#>  <div style='background-color: #fff3cd; color: #856404; border: 1px
#>  solid #ffeeba; padding: 12px; border-radius: 6px; margin-bottom:
#>  12px;'>Overall: Data is usable but review the flagged items.<table
#>  style='width: 100%; border-collapse: collapse; font-size: 13px;'><tr
#>  style='border-bottom: 2px solid #dee2e6;'><th style='padding: 6px;
#>  text-align: left;'>Status<th style='padding: 6px; text-align:
#>  left;'>Check<th style='padding: 6px; text-align: left;'>Value<th
#>  style='padding: 6px; text-align: left;'>Detail<tr
#>  style='border-bottom: 1px solid #dee2e6;'><td style='padding:
#>  6px;'><span style='color: #28a745; font-size: 18px;'>&#9679;<td
#>  style='padding: 6px;'>Events-Per-Variable (Overall)<td style='padding:
#>  6px;'>10.3 (n_events=144, p=14)<td style='padding: 6px;'>High EPV.
#>  Regularization will perform robustly.<tr style='border-bottom: 1px
#>  solid #dee2e6;'><td style='padding: 6px;'><span style='color: #ffc107;
#>  font-size: 18px;'>&#9679;<td style='padding: 6px;'>Regularization
#>  Need<td style='padding: 6px;'>p=14, EPV=10<td style='padding:
#>  6px;'>Moderate/low dimensionality. Standard Cox may also suffice.<tr
#>  style='border-bottom: 1px solid #dee2e6;'><td style='padding:
#>  6px;'><span style='color: #28a745; font-size: 18px;'>&#9679;<td
#>  style='padding: 6px;'>Sample Size<td style='padding: 6px;'>n=194<td
#>  style='padding: 6px;'>Adequate sample size for penalized
#>  regression.<tr style='border-bottom: 1px solid #dee2e6;'><td
#>  style='padding: 6px;'><span style='color: #28a745; font-size:
#>  18px;'>&#9679;<td style='padding: 6px;'>Event Rate<td style='padding:
#>  6px;'>74.2% (144/194)<td style='padding: 6px;'>Balanced event rate.
#>  Good for model estimation.<tr style='border-bottom: 1px solid
#>  #dee2e6;'><td style='padding: 6px;'><span style='color: #28a745;
#>  font-size: 18px;'>&#9679;<td style='padding:
#>  6px;'>Multicollinearity<td style='padding: 6px;'>Max |r| = 0.22<td
#>  style='padding: 6px;'>No concerning collinearity detected.<tr
#>  style='border-bottom: 1px solid #dee2e6;'><td style='padding:
#>  6px;'><span style='color: #ffc107; font-size: 18px;'>&#9679;<td
#>  style='padding: 6px;'>Data Quality<td style='padding: 6px;'>3.0%
#>  missing<td style='padding: 6px;'>3.0% missing data (6 rows excluded).
#> 
#>  Model Summary                                                                                                    
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Penalty Function                              Selected Lambda    CV Error    Variables Selected    C-index     
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    .                                             .                  .           .                     .           
#>    Smoothly Clipped Absolute Deviation (SCAD)         0.08416588    8.640609                     6    0.6134194   
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Selected Variables                                
#>  ───────────────────────────────────────────────── 
#>    Variable          Coefficient    Hazard Ratio   
#>  ───────────────────────────────────────────────── 
#>    age               1.181535e-4       1.0001182   
#>    tumor_diameter     0.18467984       1.2028333   
#>    cea_level         2.996808e-4       1.0002997   
#>    t_stageT3          0.19009980       1.2093703   
#>    t_stageT4          0.21787428       1.2434307   
#>    genderM           -0.07834696       0.9246436   
#>  ───────────────────────────────────────────────── 
#> 
#> 
#>  Cross-Validation Summary                                                
#>  ─────────────────────────────────────────────────────────────────────── 
#>    Lambda         CV Error    CV Standard Error    Number of Variables   
#>  ─────────────────────────────────────────────────────────────────────── 
#>    0.239707457    8.802058            0.1220994                      0   
#>    0.084165884    8.640609            0.1434420                      6   
#>    0.078493385    8.644875            0.1467205                      6   
#>    0.025703045    9.068479            0.2137819                     15   
#>    0.008416588    9.124440            0.2128288                     18   
#>    0.002756053    9.178434            0.2172549                     18   
#>    9.024832e-4    9.170145            0.2166666                     18   
#>    2.955226e-4    9.169605            0.2166724                     18   
#>    2.397075e-4    9.169916            0.2166935                     18   
#>  ─────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Model Comparison                                                               
#>  ────────────────────────────────────────────────────────────────────────────── 
#>    Model         Lambda        CV Error    Variables    C-index      AIC        
#>  ────────────────────────────────────────────────────────────────────────────── 
#>    Lambda Min    0.08416588    8.640609            6    0.6134194    1236.147   
#>  ────────────────────────────────────────────────────────────────────────────── 
#>    Note. AIC is computed from an unpenalized Cox refit on the selected
#>    variables. Values are approximate and intended for relative comparison
#>    only.
#> 
#> 
#>  Convergence Information                               
#>  ───────────────────────────────────────────────────── 
#>    Converged    Iterations    Tolerance    Algorithm   
#>  ───────────────────────────────────────────────────── 
#>    .            .             .            .           
#>  ───────────────────────────────────────────────────── 
#> 
#> 
#> character(0)
```

The suitability report appears as an HTML table with colored dots
indicating the status of each check. For the clinical dataset (n=200, 14
covariates), you should see mostly green indicators.

------------------------------------------------------------------------

### 3. SCAD Penalty (Default)

SCAD is the recommended starting point - it provides nearly unbiased
estimates for truly important variables while setting noise variables to
zero.

``` r

ncvregcox(
  data = ncvregcox_clinical,
  time = "time",
  event = "event",
  outcomeLevel = "1",
  censorLevel = "0",
  covariates = c("age", "bmi", "tumor_diameter", "ldh_level", "cea_level",
                 "t_stage", "crp", "albumin", "wbc_count",
                 "neutrophil_ratio", "platelet_count", "gender",
                 "n_stage", "histology"),
  penalty = "SCAD",
  gamma = 3.7,
  alpha = 1.0,
  cv_folds = 10,
  lambda_type = "min",
  standardize = TRUE,
  suitabilityCheck = TRUE,
  plot_path = TRUE,
  plot_cv = TRUE,
  variable_importance = TRUE
)
#> 
#>  SCAD COX REGRESSION RESULTS
#> ERROR: Analysis Error
#> missing value where TRUE/FALSE needed
#>  SCAD/MCP Cox Regression Analysis
#> 
#>  Non-convex penalized Cox regression for high-dimensional survival data
#>  analysis.
#> 
#>  Current Configuration:
#> 
#>  Penalty Function: Smoothly Clipped Absolute Deviation
#>  (SCAD)Cross-Validation: 10-fold CVLambda Selection: Minimum CV Error
#> 
#>  Key Features:
#> 
#>  Oracle properties for variable selectionAvoids over-penalization of
#>  large coefficientsMaintains sparsity for irrelevant
#>  variablesCross-validation for optimal penalty selectionVariable
#>  importance and stability analysis
#> 
#>  Note: SCAD penalty provides superior variable selection properties
#>  compared to LASSO, particularly for scenarios with large true effects.
#> 
#>  <div style='background-color: #fff3cd; color: #856404; border: 1px
#>  solid #ffeeba; padding: 12px; border-radius: 6px; margin-bottom:
#>  12px;'>Overall: Data is usable but review the flagged items.<table
#>  style='width: 100%; border-collapse: collapse; font-size: 13px;'><tr
#>  style='border-bottom: 2px solid #dee2e6;'><th style='padding: 6px;
#>  text-align: left;'>Status<th style='padding: 6px; text-align:
#>  left;'>Check<th style='padding: 6px; text-align: left;'>Value<th
#>  style='padding: 6px; text-align: left;'>Detail<tr
#>  style='border-bottom: 1px solid #dee2e6;'><td style='padding:
#>  6px;'><span style='color: #28a745; font-size: 18px;'>&#9679;<td
#>  style='padding: 6px;'>Events-Per-Variable (Overall)<td style='padding:
#>  6px;'>10.3 (n_events=144, p=14)<td style='padding: 6px;'>High EPV.
#>  Regularization will perform robustly.<tr style='border-bottom: 1px
#>  solid #dee2e6;'><td style='padding: 6px;'><span style='color: #ffc107;
#>  font-size: 18px;'>&#9679;<td style='padding: 6px;'>Regularization
#>  Need<td style='padding: 6px;'>p=14, EPV=10<td style='padding:
#>  6px;'>Moderate/low dimensionality. Standard Cox may also suffice.<tr
#>  style='border-bottom: 1px solid #dee2e6;'><td style='padding:
#>  6px;'><span style='color: #28a745; font-size: 18px;'>&#9679;<td
#>  style='padding: 6px;'>Sample Size<td style='padding: 6px;'>n=194<td
#>  style='padding: 6px;'>Adequate sample size for penalized
#>  regression.<tr style='border-bottom: 1px solid #dee2e6;'><td
#>  style='padding: 6px;'><span style='color: #28a745; font-size:
#>  18px;'>&#9679;<td style='padding: 6px;'>Event Rate<td style='padding:
#>  6px;'>74.2% (144/194)<td style='padding: 6px;'>Balanced event rate.
#>  Good for model estimation.<tr style='border-bottom: 1px solid
#>  #dee2e6;'><td style='padding: 6px;'><span style='color: #28a745;
#>  font-size: 18px;'>&#9679;<td style='padding:
#>  6px;'>Multicollinearity<td style='padding: 6px;'>Max |r| = 0.22<td
#>  style='padding: 6px;'>No concerning collinearity detected.<tr
#>  style='border-bottom: 1px solid #dee2e6;'><td style='padding:
#>  6px;'><span style='color: #ffc107; font-size: 18px;'>&#9679;<td
#>  style='padding: 6px;'>Data Quality<td style='padding: 6px;'>3.0%
#>  missing<td style='padding: 6px;'>3.0% missing data (6 rows excluded).
#> 
#>  Model Summary                                                                                                    
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Penalty Function                              Selected Lambda    CV Error    Variables Selected    C-index     
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    .                                             .                  .           .                     .           
#>    Smoothly Clipped Absolute Deviation (SCAD)         0.05537553    8.704506                     8    0.6474593   
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Selected Variables                                 
#>  ────────────────────────────────────────────────── 
#>    Variable          Coefficient     Hazard Ratio   
#>  ────────────────────────────────────────────────── 
#>    age                0.004961548       1.0049739   
#>    tumor_diameter     0.171084252       1.1865907   
#>    cea_level          0.005548493       1.0055639   
#>    t_stageT2          0.086772327       1.0906483   
#>    t_stageT3          0.651178555       1.9177997   
#>    t_stageT4          0.776573765       2.1740108   
#>    genderM           -0.232911880       0.7922234   
#>    n_stageN1         -0.044021742       0.9569332   
#>  ────────────────────────────────────────────────── 
#> 
#> 
#>  Variable Importance                                                 
#>  ─────────────────────────────────────────────────────────────────── 
#>    Variable    Importance Score    Rank    Relative Importance (%)   
#>  ─────────────────────────────────────────────────────────────────── 
#>  ─────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Cross-Validation Summary                                                
#>  ─────────────────────────────────────────────────────────────────────── 
#>    Lambda         CV Error    CV Standard Error    Number of Variables   
#>  ─────────────────────────────────────────────────────────────────────── 
#>    0.239707457    8.805576            0.1225834                      0   
#>    0.078493385    8.760906            0.1459348                      6   
#>    0.055375532    8.704506            0.1561458                      8   
#>    0.025703045    9.133272            0.1911870                     15   
#>    0.008416588    9.238246            0.2018499                     18   
#>    0.002756053    9.201092            0.2019235                     18   
#>    9.024832e-4    9.203028            0.2022063                     18   
#>    2.955226e-4    9.206709            0.2024778                     18   
#>    2.397075e-4    9.206700            0.2024770                     18   
#>  ─────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Model Comparison                                                               
#>  ────────────────────────────────────────────────────────────────────────────── 
#>    Model         Lambda        CV Error    Variables    C-index      AIC        
#>  ────────────────────────────────────────────────────────────────────────────── 
#>    Lambda Min    0.05537553    8.704506            8    0.6474593    1235.889   
#>  ────────────────────────────────────────────────────────────────────────────── 
#>    Note. AIC is computed from an unpenalized Cox refit on the selected
#>    variables. Values are approximate and intended for relative comparison
#>    only.
#> 
#> 
#>  Convergence Information                               
#>  ───────────────────────────────────────────────────── 
#>    Converged    Iterations    Tolerance    Algorithm   
#>  ───────────────────────────────────────────────────── 
#>    .            .             .            .           
#>  ───────────────────────────────────────────────────── 
#> 
#> 
#> character(0)
```

**Expected:** SCAD should select age, tumor_diameter, ldh_level,
cea_level, and t_stage/n_stage. Noise variables should be eliminated
with nearly unbiased coefficient estimates.

------------------------------------------------------------------------

### 4. MCP Penalty

MCP is more aggressive than SCAD - it transitions to zero penalty more
quickly, leading to sparser models.

``` r

ncvregcox(
  data = ncvregcox_clinical,
  time = "time",
  event = "event",
  outcomeLevel = "1",
  censorLevel = "0",
  covariates = c("age", "bmi", "tumor_diameter", "ldh_level", "cea_level",
                 "t_stage", "crp", "albumin", "wbc_count",
                 "neutrophil_ratio", "platelet_count", "gender",
                 "n_stage", "histology"),
  penalty = "MCP",
  gamma = 3.0,
  alpha = 1.0,
  cv_folds = 10,
  lambda_type = "min",
  standardize = TRUE,
  suitabilityCheck = TRUE,
  plot_path = TRUE,
  plot_cv = TRUE,
  variable_importance = TRUE
)
#> 
#>  SCAD COX REGRESSION RESULTS
#> ERROR: Analysis Error
#> missing value where TRUE/FALSE needed
#>  SCAD/MCP Cox Regression Analysis
#> 
#>  Non-convex penalized Cox regression for high-dimensional survival data
#>  analysis.
#> 
#>  Current Configuration:
#> 
#>  Penalty Function: Minimax Concave Penalty (MCP)Cross-Validation:
#>  10-fold CVLambda Selection: Minimum CV Error
#> 
#>  Key Features:
#> 
#>  Oracle properties for variable selectionAvoids over-penalization of
#>  large coefficientsMaintains sparsity for irrelevant
#>  variablesCross-validation for optimal penalty selectionVariable
#>  importance and stability analysis
#> 
#>  Note: MCP penalty provides superior variable selection properties
#>  compared to LASSO, particularly for scenarios with large true effects.
#> 
#>  <div style='background-color: #fff3cd; color: #856404; border: 1px
#>  solid #ffeeba; padding: 12px; border-radius: 6px; margin-bottom:
#>  12px;'>Overall: Data is usable but review the flagged items.<table
#>  style='width: 100%; border-collapse: collapse; font-size: 13px;'><tr
#>  style='border-bottom: 2px solid #dee2e6;'><th style='padding: 6px;
#>  text-align: left;'>Status<th style='padding: 6px; text-align:
#>  left;'>Check<th style='padding: 6px; text-align: left;'>Value<th
#>  style='padding: 6px; text-align: left;'>Detail<tr
#>  style='border-bottom: 1px solid #dee2e6;'><td style='padding:
#>  6px;'><span style='color: #28a745; font-size: 18px;'>&#9679;<td
#>  style='padding: 6px;'>Events-Per-Variable (Overall)<td style='padding:
#>  6px;'>10.3 (n_events=144, p=14)<td style='padding: 6px;'>High EPV.
#>  Regularization will perform robustly.<tr style='border-bottom: 1px
#>  solid #dee2e6;'><td style='padding: 6px;'><span style='color: #ffc107;
#>  font-size: 18px;'>&#9679;<td style='padding: 6px;'>Regularization
#>  Need<td style='padding: 6px;'>p=14, EPV=10<td style='padding:
#>  6px;'>Moderate/low dimensionality. Standard Cox may also suffice.<tr
#>  style='border-bottom: 1px solid #dee2e6;'><td style='padding:
#>  6px;'><span style='color: #28a745; font-size: 18px;'>&#9679;<td
#>  style='padding: 6px;'>Sample Size<td style='padding: 6px;'>n=194<td
#>  style='padding: 6px;'>Adequate sample size for penalized
#>  regression.<tr style='border-bottom: 1px solid #dee2e6;'><td
#>  style='padding: 6px;'><span style='color: #28a745; font-size:
#>  18px;'>&#9679;<td style='padding: 6px;'>Event Rate<td style='padding:
#>  6px;'>74.2% (144/194)<td style='padding: 6px;'>Balanced event rate.
#>  Good for model estimation.<tr style='border-bottom: 1px solid
#>  #dee2e6;'><td style='padding: 6px;'><span style='color: #28a745;
#>  font-size: 18px;'>&#9679;<td style='padding:
#>  6px;'>Multicollinearity<td style='padding: 6px;'>Max |r| = 0.22<td
#>  style='padding: 6px;'>No concerning collinearity detected.<tr
#>  style='border-bottom: 1px solid #dee2e6;'><td style='padding:
#>  6px;'><span style='color: #ffc107; font-size: 18px;'>&#9679;<td
#>  style='padding: 6px;'>Data Quality<td style='padding: 6px;'>3.0%
#>  missing<td style='padding: 6px;'>3.0% missing data (6 rows excluded).
#> 
#>  Model Summary                                                                                       
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Penalty Function                 Selected Lambda    CV Error    Variables Selected    C-index     
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    .                                .                  .           .                     .           
#>    Minimax Concave Penalty (MCP)          0.1470823    8.696754                     1    0.5898372   
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Selected Variables                                
#>  ───────────────────────────────────────────────── 
#>    Variable          Coefficient    Hazard Ratio   
#>  ───────────────────────────────────────────────── 
#>    tumor_diameter      0.1350197        1.144559   
#>  ───────────────────────────────────────────────── 
#> 
#> 
#>  Variable Importance                                                 
#>  ─────────────────────────────────────────────────────────────────── 
#>    Variable    Importance Score    Rank    Relative Importance (%)   
#>  ─────────────────────────────────────────────────────────────────── 
#>  ─────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Cross-Validation Summary                                                
#>  ─────────────────────────────────────────────────────────────────────── 
#>    Lambda         CV Error    CV Standard Error    Number of Variables   
#>  ─────────────────────────────────────────────────────────────────────── 
#>    0.239707457    8.797289            0.1223892                      0   
#>    0.147082273    8.696754            0.1302790                      1   
#>    0.078493385    8.721267            0.1583002                      6   
#>    0.025703045    8.904297            0.1881463                     15   
#>    0.008416588    9.047971            0.1923972                     18   
#>    0.002756053    9.067446            0.1927766                     18   
#>    9.024832e-4    9.061910            0.1922071                     18   
#>    2.955226e-4    9.061425            0.1920592                     18   
#>    2.397075e-4    9.061351            0.1920502                     18   
#>  ─────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Model Comparison                                                              
#>  ───────────────────────────────────────────────────────────────────────────── 
#>    Model         Lambda       CV Error    Variables    C-index      AIC        
#>  ───────────────────────────────────────────────────────────────────────────── 
#>    Lambda Min    0.1470823    8.696754            1    0.5898372    1249.036   
#>  ───────────────────────────────────────────────────────────────────────────── 
#>    Note. AIC is computed from an unpenalized Cox refit on the selected
#>    variables. Values are approximate and intended for relative comparison
#>    only.
#> 
#> 
#>  Convergence Information                               
#>  ───────────────────────────────────────────────────── 
#>    Converged    Iterations    Tolerance    Algorithm   
#>  ───────────────────────────────────────────────────── 
#>    .            .             .            .           
#>  ───────────────────────────────────────────────────── 
#> 
#> 
#> character(0)
```

**Compare with SCAD:** MCP typically selects fewer variables. If both
methods select the same core set of predictors, this provides stronger
evidence that those variables are truly associated with the outcome.

------------------------------------------------------------------------

### 5. Gamma Parameter

The gamma parameter controls how quickly the penalty flattens:

- **SCAD**: gamma must be \> 2. Default 3.7 (Fan & Li recommendation)
- **MCP**: gamma must be \> 1. Default 3.0. Lower gamma = more
  aggressive selection

``` r

# SCAD with lower gamma (more aggressive - penalty flattens sooner)
ncvregcox(
  data = ncvregcox_clinical,
  time = "time",
  event = "event",
  outcomeLevel = "1",
  censorLevel = "0",
  covariates = c("age", "bmi", "tumor_diameter", "ldh_level", "cea_level",
                 "t_stage", "crp", "albumin"),
  penalty = "SCAD",
  gamma = 2.5,
  cv_folds = 10,
  lambda_type = "min",
  suitabilityCheck = FALSE,
  plot_cv = TRUE,
  variable_importance = TRUE
)
#> 
#>  SCAD COX REGRESSION RESULTS
#> ERROR: Analysis Error
#> missing value where TRUE/FALSE needed
#>  SCAD/MCP Cox Regression Analysis
#> 
#>  Non-convex penalized Cox regression for high-dimensional survival data
#>  analysis.
#> 
#>  Current Configuration:
#> 
#>  Penalty Function: Smoothly Clipped Absolute Deviation
#>  (SCAD)Cross-Validation: 10-fold CVLambda Selection: Minimum CV Error
#> 
#>  Key Features:
#> 
#>  Oracle properties for variable selectionAvoids over-penalization of
#>  large coefficientsMaintains sparsity for irrelevant
#>  variablesCross-validation for optimal penalty selectionVariable
#>  importance and stability analysis
#> 
#>  Note: SCAD penalty provides superior variable selection properties
#>  compared to LASSO, particularly for scenarios with large true effects.
#> 
#>  Model Summary                                                                                                    
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Penalty Function                              Selected Lambda    CV Error    Variables Selected    C-index     
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    .                                             .                  .           .                     .           
#>    Smoothly Clipped Absolute Deviation (SCAD)         0.09644289    8.678305                     3    0.6007249   
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Selected Variables                                
#>  ───────────────────────────────────────────────── 
#>    Variable          Coefficient    Hazard Ratio   
#>  ───────────────────────────────────────────────── 
#>    tumor_diameter      0.1874099        1.206122   
#>    t_stageT3           0.1027992        1.108269   
#>    t_stageT4           0.1344784        1.143940   
#>  ───────────────────────────────────────────────── 
#> 
#> 
#>  Variable Importance                                                 
#>  ─────────────────────────────────────────────────────────────────── 
#>    Variable    Importance Score    Rank    Relative Importance (%)   
#>  ─────────────────────────────────────────────────────────────────── 
#>  ─────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Cross-Validation Summary                                                
#>  ─────────────────────────────────────────────────────────────────────── 
#>    Lambda         CV Error    CV Standard Error    Number of Variables   
#>  ─────────────────────────────────────────────────────────────────────── 
#>    0.238896410    8.805812            0.1224040                      0   
#>    0.096442893    8.678305            0.1427981                      3   
#>    0.078227804    8.729936            0.1512203                      5   
#>    0.025616079    9.448408            0.1900001                      8   
#>    0.008388111    9.376077            0.1897541                     10   
#>    0.002746728    9.354002            0.1886092                     10   
#>    8.994296e-4    9.358507            0.1887627                     10   
#>    2.945227e-4    9.357994            0.1887833                     10   
#>    2.388964e-4    9.357994            0.1887833                     10   
#>  ─────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Model Comparison                                                               
#>  ────────────────────────────────────────────────────────────────────────────── 
#>    Model         Lambda        CV Error    Variables    C-index      AIC        
#>  ────────────────────────────────────────────────────────────────────────────── 
#>    Lambda Min    0.09644289    8.678305            3    0.6007249    1242.083   
#>  ────────────────────────────────────────────────────────────────────────────── 
#>    Note. AIC is computed from an unpenalized Cox refit on the selected
#>    variables. Values are approximate and intended for relative comparison
#>    only.
#> 
#> 
#>  Convergence Information                               
#>  ───────────────────────────────────────────────────── 
#>    Converged    Iterations    Tolerance    Algorithm   
#>  ───────────────────────────────────────────────────── 
#>    .            .             .            .           
#>  ───────────────────────────────────────────────────── 
#> 
#> 
#> character(0)
```

``` r

# SCAD with higher gamma (less aggressive - closer to LASSO-like behavior)
ncvregcox(
  data = ncvregcox_clinical,
  time = "time",
  event = "event",
  outcomeLevel = "1",
  censorLevel = "0",
  covariates = c("age", "bmi", "tumor_diameter", "ldh_level", "cea_level",
                 "t_stage", "crp", "albumin"),
  penalty = "SCAD",
  gamma = 6.0,
  cv_folds = 10,
  lambda_type = "min",
  suitabilityCheck = FALSE,
  plot_cv = TRUE,
  variable_importance = TRUE
)
#> 
#>  SCAD COX REGRESSION RESULTS
#> ERROR: Analysis Error
#> missing value where TRUE/FALSE needed
#>  SCAD/MCP Cox Regression Analysis
#> 
#>  Non-convex penalized Cox regression for high-dimensional survival data
#>  analysis.
#> 
#>  Current Configuration:
#> 
#>  Penalty Function: Smoothly Clipped Absolute Deviation
#>  (SCAD)Cross-Validation: 10-fold CVLambda Selection: Minimum CV Error
#> 
#>  Key Features:
#> 
#>  Oracle properties for variable selectionAvoids over-penalization of
#>  large coefficientsMaintains sparsity for irrelevant
#>  variablesCross-validation for optimal penalty selectionVariable
#>  importance and stability analysis
#> 
#>  Note: SCAD penalty provides superior variable selection properties
#>  compared to LASSO, particularly for scenarios with large true effects.
#> 
#>  Model Summary                                                                                                    
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Penalty Function                              Selected Lambda    CV Error    Variables Selected    C-index     
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    .                                             .                  .           .                     .           
#>    Smoothly Clipped Absolute Deviation (SCAD)         0.06803855    8.673157                     5    0.6293740   
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Selected Variables                                
#>  ───────────────────────────────────────────────── 
#>    Variable          Coefficient    Hazard Ratio   
#>  ───────────────────────────────────────────────── 
#>    age               0.002756647        1.002760   
#>    tumor_diameter    0.161776170        1.175597   
#>    cea_level         0.003656958        1.003664   
#>    t_stageT3         0.260487338        1.297562   
#>    t_stageT4         0.362788447        1.437332   
#>  ───────────────────────────────────────────────── 
#> 
#> 
#>  Variable Importance                                                 
#>  ─────────────────────────────────────────────────────────────────── 
#>    Variable    Importance Score    Rank    Relative Importance (%)   
#>  ─────────────────────────────────────────────────────────────────── 
#>  ─────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Cross-Validation Summary                                                
#>  ─────────────────────────────────────────────────────────────────────── 
#>    Lambda         CV Error    CV Standard Error    Number of Variables   
#>  ─────────────────────────────────────────────────────────────────────── 
#>    0.238896410    8.818570            0.1230975                      0   
#>    0.078227804    8.678530            0.1394358                      5   
#>    0.068038554    8.673157            0.1437291                      5   
#>    0.025616079    8.844991            0.1771107                      8   
#>    0.008388111    8.956579            0.1869398                     10   
#>    0.002746728    8.948899            0.1864790                     10   
#>    8.994296e-4    8.942382            0.1861927                     10   
#>    2.945227e-4    8.942564            0.1862079                     10   
#>    2.388964e-4    8.942564            0.1862079                     10   
#>  ─────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Model Comparison                                                               
#>  ────────────────────────────────────────────────────────────────────────────── 
#>    Model         Lambda        CV Error    Variables    C-index      AIC        
#>  ────────────────────────────────────────────────────────────────────────────── 
#>    Lambda Min    0.06803855    8.673157            5    0.6293740    1240.141   
#>  ────────────────────────────────────────────────────────────────────────────── 
#>    Note. AIC is computed from an unpenalized Cox refit on the selected
#>    variables. Values are approximate and intended for relative comparison
#>    only.
#> 
#> 
#>  Convergence Information                               
#>  ───────────────────────────────────────────────────── 
#>    Converged    Iterations    Tolerance    Algorithm   
#>  ───────────────────────────────────────────────────── 
#>    .            .             .            .           
#>  ───────────────────────────────────────────────────── 
#> 
#> 
#> character(0)
```

**Interpretation:** As gamma increases, SCAD behaves more like LASSO
(constant penalty rate). As gamma decreases toward 2, SCAD becomes more
aggressive at removing the penalty from large coefficients. The default
3.7 is a well-studied compromise.

------------------------------------------------------------------------

### 6. Lambda Selection: min vs 1se

The cross-validation procedure identifies the optimal penalty strength
(lambda). Two strategies are available:

- **min**: Lambda that minimizes CV error - selects more variables
- **1se**: Lambda within 1 SE of minimum - more parsimonious (Breiman’s
  rule)

``` r

# Minimum CV error - selects more variables
ncvregcox(
  data = ncvregcox_clinical,
  time = "time",
  event = "event",
  outcomeLevel = "1",
  censorLevel = "0",
  covariates = c("age", "bmi", "tumor_diameter", "ldh_level", "cea_level",
                 "t_stage", "crp", "albumin"),
  penalty = "SCAD",
  lambda_type = "min",
  cv_folds = 10,
  suitabilityCheck = FALSE,
  plot_cv = TRUE,
  variable_importance = TRUE
)
#> 
#>  SCAD COX REGRESSION RESULTS
#> ERROR: Analysis Error
#> missing value where TRUE/FALSE needed
#>  SCAD/MCP Cox Regression Analysis
#> 
#>  Non-convex penalized Cox regression for high-dimensional survival data
#>  analysis.
#> 
#>  Current Configuration:
#> 
#>  Penalty Function: Smoothly Clipped Absolute Deviation
#>  (SCAD)Cross-Validation: 10-fold CVLambda Selection: Minimum CV Error
#> 
#>  Key Features:
#> 
#>  Oracle properties for variable selectionAvoids over-penalization of
#>  large coefficientsMaintains sparsity for irrelevant
#>  variablesCross-validation for optimal penalty selectionVariable
#>  importance and stability analysis
#> 
#>  Note: SCAD penalty provides superior variable selection properties
#>  compared to LASSO, particularly for scenarios with large true effects.
#> 
#>  Model Summary                                                                                                    
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Penalty Function                              Selected Lambda    CV Error    Variables Selected    C-index     
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    .                                             .                  .           .                     .           
#>    Smoothly Clipped Absolute Deviation (SCAD)         0.05917646    8.665966                     6    0.6396905   
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Selected Variables                                
#>  ───────────────────────────────────────────────── 
#>    Variable          Coefficient    Hazard Ratio   
#>  ───────────────────────────────────────────────── 
#>    age               0.004421418        1.004431   
#>    tumor_diameter    0.172498178        1.188270   
#>    cea_level         0.005567480        1.005583   
#>    t_stageT3         0.495528032        1.641365   
#>    t_stageT4         0.651017427        1.917491   
#>    crp               9.185679e-4        1.000919   
#>  ───────────────────────────────────────────────── 
#> 
#> 
#>  Variable Importance                                                 
#>  ─────────────────────────────────────────────────────────────────── 
#>    Variable    Importance Score    Rank    Relative Importance (%)   
#>  ─────────────────────────────────────────────────────────────────── 
#>  ─────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Cross-Validation Summary                                                
#>  ─────────────────────────────────────────────────────────────────────── 
#>    Lambda         CV Error    CV Standard Error    Number of Variables   
#>  ─────────────────────────────────────────────────────────────────────── 
#>    0.238896410    8.809200            0.1222014                      0   
#>    0.078227804    8.679232            0.1437798                      5   
#>    0.059176465    8.665966            0.1523670                      6   
#>    0.025616079    8.677389            0.1702619                      8   
#>    0.008388111    8.716028            0.1737965                     10   
#>    0.002746728    8.709594            0.1721789                     10   
#>    8.994296e-4    8.710279            0.1723088                     10   
#>    2.945227e-4    8.710319            0.1723116                     10   
#>    2.388964e-4    8.710319            0.1723116                     10   
#>  ─────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Model Comparison                                                               
#>  ────────────────────────────────────────────────────────────────────────────── 
#>    Model         Lambda        CV Error    Variables    C-index      AIC        
#>  ────────────────────────────────────────────────────────────────────────────── 
#>    Lambda Min    0.05917646    8.665966            6    0.6396905    1241.090   
#>  ────────────────────────────────────────────────────────────────────────────── 
#>    Note. AIC is computed from an unpenalized Cox refit on the selected
#>    variables. Values are approximate and intended for relative comparison
#>    only.
#> 
#> 
#>  Convergence Information                               
#>  ───────────────────────────────────────────────────── 
#>    Converged    Iterations    Tolerance    Algorithm   
#>  ───────────────────────────────────────────────────── 
#>    .            .             .            .           
#>  ───────────────────────────────────────────────────── 
#> 
#> 
#> character(0)
```

``` r

# One SE rule - more parsimonious
ncvregcox(
  data = ncvregcox_clinical,
  time = "time",
  event = "event",
  outcomeLevel = "1",
  censorLevel = "0",
  covariates = c("age", "bmi", "tumor_diameter", "ldh_level", "cea_level",
                 "t_stage", "crp", "albumin"),
  penalty = "SCAD",
  lambda_type = "1se",
  cv_folds = 10,
  suitabilityCheck = FALSE,
  plot_cv = TRUE,
  variable_importance = TRUE
)
#> 
#>  SCAD COX REGRESSION RESULTS
#> ERROR: Analysis Error
#> missing value where TRUE/FALSE needed
#>  SCAD/MCP Cox Regression Analysis
#> 
#>  Non-convex penalized Cox regression for high-dimensional survival data
#>  analysis.
#> 
#>  Current Configuration:
#> 
#>  Penalty Function: Smoothly Clipped Absolute Deviation
#>  (SCAD)Cross-Validation: 10-fold CVLambda Selection: One Standard Error
#>  Rule
#> 
#>  Key Features:
#> 
#>  Oracle properties for variable selectionAvoids over-penalization of
#>  large coefficientsMaintains sparsity for irrelevant
#>  variablesCross-validation for optimal penalty selectionVariable
#>  importance and stability analysis
#> 
#>  Note: SCAD penalty provides superior variable selection properties
#>  compared to LASSO, particularly for scenarios with large true effects.
#> 
#>  Model Summary                                                                                                    
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Penalty Function                              Selected Lambda    CV Error    Variables Selected    C-index     
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    .                                             .                  .           .                     .           
#>    Smoothly Clipped Absolute Deviation (SCAD)         0.06345298    8.652496                     6    0.6363446   
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Selected Variables                                
#>  ───────────────────────────────────────────────── 
#>    Variable          Coefficient    Hazard Ratio   
#>  ───────────────────────────────────────────────── 
#>    age               0.003605210        1.003612   
#>    tumor_diameter    0.176712109        1.193288   
#>    cea_level         0.004875349        1.004887   
#>    t_stageT3         0.389181558        1.475772   
#>    t_stageT4         0.518692542        1.679830   
#>    crp               2.728554e-4        1.000273   
#>  ───────────────────────────────────────────────── 
#> 
#> 
#>  Variable Importance                                                 
#>  ─────────────────────────────────────────────────────────────────── 
#>    Variable    Importance Score    Rank    Relative Importance (%)   
#>  ─────────────────────────────────────────────────────────────────── 
#>  ─────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Cross-Validation Summary                                                
#>  ─────────────────────────────────────────────────────────────────────── 
#>    Lambda         CV Error    CV Standard Error    Number of Variables   
#>  ─────────────────────────────────────────────────────────────────────── 
#>    0.238896410    8.825549            0.1234265                      0   
#>    0.078227804    8.655048            0.1430842                      5   
#>    0.063452983    8.652496            0.1505418                      6   
#>    0.025616079    8.679120            0.1619876                      8   
#>    0.008388111    8.677064            0.1632744                     10   
#>    0.002746728    8.682081            0.1628458                     10   
#>    8.994296e-4    8.679115            0.1626077                     10   
#>    2.945227e-4    8.679117            0.1626077                     10   
#>    2.388964e-4    8.679142            0.1626085                     10   
#>  ─────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Model Comparison                                                               
#>  ────────────────────────────────────────────────────────────────────────────── 
#>    Model         Lambda        CV Error    Variables    C-index      AIC        
#>  ────────────────────────────────────────────────────────────────────────────── 
#>    Lambda Min    0.06345298    8.652496            6    0.6363446    1241.090   
#>  ────────────────────────────────────────────────────────────────────────────── 
#>    Note. AIC is computed from an unpenalized Cox refit on the selected
#>    variables. Values are approximate and intended for relative comparison
#>    only.
#> 
#> 
#>  Convergence Information                               
#>  ───────────────────────────────────────────────────── 
#>    Converged    Iterations    Tolerance    Algorithm   
#>  ───────────────────────────────────────────────────── 
#>    .            .             .            .           
#>  ───────────────────────────────────────────────────── 
#> 
#> 
#> character(0)
```

**Practical tip:** If the 1-SE model drops a variable you consider
clinically important, inspect the CV error plot. If the two lambda
values are close, the difference is likely within noise. If they are far
apart, the 1-SE model is genuinely simpler and may generalize better.

------------------------------------------------------------------------

### 7. Sparse Scenario (Many Noise Variables)

The `ncvregcox_sparse` dataset has 30 covariates but only 4 true
signals. This is where non-convex penalties shine - SCAD/MCP should
recover the true model more accurately than LASSO would.

``` r

data(ncvregcox_sparse, package = "ClinicoPath")

# SCAD should recover x1, x5, x12, x20
ncvregcox(
  data = ncvregcox_sparse,
  time = "time",
  event = "event",
  outcomeLevel = "1",
  censorLevel = "0",
  covariates = paste0("x", 1:30),
  penalty = "SCAD",
  gamma = 3.7,
  cv_folds = 10,
  lambda_type = "min",
  standardize = TRUE,
  suitabilityCheck = TRUE,
  plot_path = TRUE,
  plot_cv = TRUE,
  variable_importance = TRUE
)
#> 
#>  SCAD COX REGRESSION RESULTS
#> ERROR: Analysis Error
#> missing value where TRUE/FALSE needed
#>  SCAD/MCP Cox Regression Analysis
#> 
#>  Non-convex penalized Cox regression for high-dimensional survival data
#>  analysis.
#> 
#>  Current Configuration:
#> 
#>  Penalty Function: Smoothly Clipped Absolute Deviation
#>  (SCAD)Cross-Validation: 10-fold CVLambda Selection: Minimum CV Error
#> 
#>  Key Features:
#> 
#>  Oracle properties for variable selectionAvoids over-penalization of
#>  large coefficientsMaintains sparsity for irrelevant
#>  variablesCross-validation for optimal penalty selectionVariable
#>  importance and stability analysis
#> 
#>  Note: SCAD penalty provides superior variable selection properties
#>  compared to LASSO, particularly for scenarios with large true effects.
#> 
#>  <div style='background-color: #fff3cd; color: #856404; border: 1px
#>  solid #ffeeba; padding: 12px; border-radius: 6px; margin-bottom:
#>  12px;'>Overall: Data is usable but review the flagged items.<table
#>  style='width: 100%; border-collapse: collapse; font-size: 13px;'><tr
#>  style='border-bottom: 2px solid #dee2e6;'><th style='padding: 6px;
#>  text-align: left;'>Status<th style='padding: 6px; text-align:
#>  left;'>Check<th style='padding: 6px; text-align: left;'>Value<th
#>  style='padding: 6px; text-align: left;'>Detail<tr
#>  style='border-bottom: 1px solid #dee2e6;'><td style='padding:
#>  6px;'><span style='color: #ffc107; font-size: 18px;'>&#9679;<td
#>  style='padding: 6px;'>Events-Per-Variable (Overall)<td style='padding:
#>  6px;'>2.2 (n_events=66, p=30)<td style='padding: 6px;'>Adequate for
#>  SCAD/MCP penalized regression, which handles low EPV better than
#>  standard Cox.<tr style='border-bottom: 1px solid #dee2e6;'><td
#>  style='padding: 6px;'><span style='color: #ffc107; font-size:
#>  18px;'>&#9679;<td style='padding: 6px;'>Regularization Need<td
#>  style='padding: 6px;'>p=30, EPV=2<td style='padding:
#>  6px;'>Moderate/low dimensionality. Standard Cox may also suffice.<tr
#>  style='border-bottom: 1px solid #dee2e6;'><td style='padding:
#>  6px;'><span style='color: #28a745; font-size: 18px;'>&#9679;<td
#>  style='padding: 6px;'>Sample Size<td style='padding: 6px;'>n=100<td
#>  style='padding: 6px;'>Adequate sample size for penalized
#>  regression.<tr style='border-bottom: 1px solid #dee2e6;'><td
#>  style='padding: 6px;'><span style='color: #28a745; font-size:
#>  18px;'>&#9679;<td style='padding: 6px;'>Event Rate<td style='padding:
#>  6px;'>66.0% (66/100)<td style='padding: 6px;'>Balanced event rate.
#>  Good for model estimation.<tr style='border-bottom: 1px solid
#>  #dee2e6;'><td style='padding: 6px;'><span style='color: #28a745;
#>  font-size: 18px;'>&#9679;<td style='padding:
#>  6px;'>Multicollinearity<td style='padding: 6px;'>Max |r| = 0.56<td
#>  style='padding: 6px;'>No concerning collinearity detected.<tr
#>  style='border-bottom: 1px solid #dee2e6;'><td style='padding:
#>  6px;'><span style='color: #28a745; font-size: 18px;'>&#9679;<td
#>  style='padding: 6px;'>Data Quality<td style='padding: 6px;'>No missing
#>  data<td style='padding: 6px;'>Complete dataset.
#> 
#>  Model Summary                                                                                                    
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Penalty Function                              Selected Lambda    CV Error    Variables Selected    C-index     
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    .                                             .                  .           .                     .           
#>    Smoothly Clipped Absolute Deviation (SCAD)          0.1022565    7.141957                     7    0.7697919   
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Selected Variables                          
#>  ─────────────────────────────────────────── 
#>    Variable    Coefficient    Hazard Ratio   
#>  ─────────────────────────────────────────── 
#>    x1           1.02484390       2.7866604   
#>    x5          -0.55972980       0.5713634   
#>    x7           0.01118745       1.0112503   
#>    x12          0.74915075       2.1152029   
#>    x20         -0.08282486       0.9205124   
#>    x21          0.03384736       1.0344267   
#>    x28         -0.09379210       0.9104720   
#>  ─────────────────────────────────────────── 
#> 
#> 
#>  Variable Importance                                                 
#>  ─────────────────────────────────────────────────────────────────── 
#>    Variable    Importance Score    Rank    Relative Importance (%)   
#>  ─────────────────────────────────────────────────────────────────── 
#>  ─────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Cross-Validation Summary                                                
#>  ─────────────────────────────────────────────────────────────────────── 
#>    Lambda         CV Error    CV Standard Error    Number of Variables   
#>  ─────────────────────────────────────────────────────────────────────── 
#>    0.384989142    7.798358            0.1459702                      0   
#>    0.126066587    7.171707            0.2587496                      5   
#>    0.102256496    7.141957            0.2707460                      7   
#>    0.041281124    8.556582            0.4099381                     17   
#>    0.013517707    8.874509            0.4651488                     28   
#>    0.004426440    8.840899            0.4666756                     30   
#>    0.001449459    8.848954            0.4671480                     30   
#>    4.746326e-4    8.850772            0.4669219                     30   
#>    3.849891e-4    8.851043            0.4669107                     30   
#>  ─────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Model Comparison                                                              
#>  ───────────────────────────────────────────────────────────────────────────── 
#>    Model         Lambda       CV Error    Variables    C-index      AIC        
#>  ───────────────────────────────────────────────────────────────────────────── 
#>    Lambda Min    0.1022565    7.141957            7    0.7697919    456.9985   
#>  ───────────────────────────────────────────────────────────────────────────── 
#>    Note. AIC is computed from an unpenalized Cox refit on the selected
#>    variables. Values are approximate and intended for relative comparison
#>    only.
#> 
#> 
#>  Convergence Information                               
#>  ───────────────────────────────────────────────────── 
#>    Converged    Iterations    Tolerance    Algorithm   
#>  ───────────────────────────────────────────────────── 
#>    .            .             .            .           
#>  ───────────────────────────────────────────────────── 
#> 
#> 
#> character(0)
```

``` r

# MCP comparison - often sparser than SCAD
ncvregcox(
  data = ncvregcox_sparse,
  time = "time",
  event = "event",
  outcomeLevel = "1",
  censorLevel = "0",
  covariates = paste0("x", 1:30),
  penalty = "MCP",
  gamma = 3.0,
  cv_folds = 10,
  lambda_type = "min",
  standardize = TRUE,
  suitabilityCheck = FALSE,
  plot_path = TRUE,
  plot_cv = TRUE,
  variable_importance = TRUE
)
#> 
#>  SCAD COX REGRESSION RESULTS
#> ERROR: Analysis Error
#> missing value where TRUE/FALSE needed
#>  SCAD/MCP Cox Regression Analysis
#> 
#>  Non-convex penalized Cox regression for high-dimensional survival data
#>  analysis.
#> 
#>  Current Configuration:
#> 
#>  Penalty Function: Minimax Concave Penalty (MCP)Cross-Validation:
#>  10-fold CVLambda Selection: Minimum CV Error
#> 
#>  Key Features:
#> 
#>  Oracle properties for variable selectionAvoids over-penalization of
#>  large coefficientsMaintains sparsity for irrelevant
#>  variablesCross-validation for optimal penalty selectionVariable
#>  importance and stability analysis
#> 
#>  Note: MCP penalty provides superior variable selection properties
#>  compared to LASSO, particularly for scenarios with large true effects.
#> 
#>  Model Summary                                                                                       
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Penalty Function                 Selected Lambda    CV Error    Variables Selected    C-index     
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    .                                .                  .           .                     .           
#>    Minimax Concave Penalty (MCP)          0.1554208    7.062540                     3    0.7549311   
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Selected Variables                          
#>  ─────────────────────────────────────────── 
#>    Variable    Coefficient    Hazard Ratio   
#>  ─────────────────────────────────────────── 
#>    x1            0.9446983       2.5720374   
#>    x5           -0.4297624       0.6506636   
#>    x12           0.7210575       2.0566069   
#>  ─────────────────────────────────────────── 
#> 
#> 
#>  Variable Importance                                                 
#>  ─────────────────────────────────────────────────────────────────── 
#>    Variable    Importance Score    Rank    Relative Importance (%)   
#>  ─────────────────────────────────────────────────────────────────── 
#>  ─────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Cross-Validation Summary                                                
#>  ─────────────────────────────────────────────────────────────────────── 
#>    Lambda         CV Error    CV Standard Error    Number of Variables   
#>  ─────────────────────────────────────────────────────────────────────── 
#>    0.384989142    7.800719            0.1451318                      0   
#>    0.155420781    7.062540            0.2498844                      3   
#>    0.126066587    7.144579            0.2648754                      6   
#>    0.041281124    9.182006            0.4578101                     18   
#>    0.013517707    9.128415            0.4794645                     28   
#>    0.004426440    9.175545            0.4809475                     30   
#>    0.001449459    9.169334            0.4815448                     30   
#>    4.746326e-4    9.171570            0.4816971                     30   
#>    3.849891e-4    9.171611            0.4816985                     30   
#>  ─────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Model Comparison                                                              
#>  ───────────────────────────────────────────────────────────────────────────── 
#>    Model         Lambda       CV Error    Variables    C-index      AIC        
#>  ───────────────────────────────────────────────────────────────────────────── 
#>    Lambda Min    0.1554208    7.062540            3    0.7549311    461.3279   
#>  ───────────────────────────────────────────────────────────────────────────── 
#>    Note. AIC is computed from an unpenalized Cox refit on the selected
#>    variables. Values are approximate and intended for relative comparison
#>    only.
#> 
#> 
#>  Convergence Information                               
#>  ───────────────────────────────────────────────────── 
#>    Converged    Iterations    Tolerance    Algorithm   
#>  ───────────────────────────────────────────────────── 
#>    .            .             .            .           
#>  ───────────────────────────────────────────────────── 
#> 
#> 
#> character(0)
```

**What to look for:** Both SCAD and MCP should identify x1, x5, x12, and
x20 as the important variables. Any noise variables that sneak in are
false positives. The sparse dataset also has correlated blocks (x1-x5
with rho=0.4), so some leakage from x1 to x2-x4 is possible.

------------------------------------------------------------------------

### 8. Elastic Net Mixing

Combine SCAD/MCP with an L2 (ridge-like) penalty by setting alpha \< 1.
This can stabilize selection when predictors are correlated.

``` r

ncvregcox(
  data = ncvregcox_clinical,
  time = "time",
  event = "event",
  outcomeLevel = "1",
  censorLevel = "0",
  covariates = c("age", "bmi", "tumor_diameter", "ldh_level", "cea_level",
                 "t_stage", "crp", "albumin"),
  penalty = "SCAD",
  alpha = 0.7,  # Mix with L2 penalty
  cv_folds = 10,
  lambda_type = "min",
  suitabilityCheck = FALSE,
  plot_cv = TRUE,
  variable_importance = TRUE
)
#> 
#>  SCAD COX REGRESSION RESULTS
#> ERROR: Analysis Error
#> missing value where TRUE/FALSE needed
#>  SCAD/MCP Cox Regression Analysis
#> 
#>  Non-convex penalized Cox regression for high-dimensional survival data
#>  analysis.
#> 
#>  Current Configuration:
#> 
#>  Penalty Function: Smoothly Clipped Absolute Deviation
#>  (SCAD)Cross-Validation: 10-fold CVLambda Selection: Minimum CV Error
#> 
#>  Key Features:
#> 
#>  Oracle properties for variable selectionAvoids over-penalization of
#>  large coefficientsMaintains sparsity for irrelevant
#>  variablesCross-validation for optimal penalty selectionVariable
#>  importance and stability analysis
#> 
#>  Note: SCAD penalty provides superior variable selection properties
#>  compared to LASSO, particularly for scenarios with large true effects.
#> 
#>  Model Summary                                                                                                    
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Penalty Function                              Selected Lambda    CV Error    Variables Selected    C-index     
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    .                                             .                  .           .                     .           
#>    Smoothly Clipped Absolute Deviation (SCAD)          0.1377756    8.676569                     3    0.6069288   
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Selected Variables                                
#>  ───────────────────────────────────────────────── 
#>    Variable          Coefficient    Hazard Ratio   
#>  ───────────────────────────────────────────────── 
#>    tumor_diameter      0.1462820        1.157523   
#>    t_stageT3           0.1095291        1.115753   
#>    t_stageT4           0.1686747        1.183735   
#>  ───────────────────────────────────────────────── 
#> 
#> 
#>  Variable Importance                                                 
#>  ─────────────────────────────────────────────────────────────────── 
#>    Variable    Importance Score    Rank    Relative Importance (%)   
#>  ─────────────────────────────────────────────────────────────────── 
#>  ─────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Cross-Validation Summary                                                
#>  ─────────────────────────────────────────────────────────────────────── 
#>    Lambda         CV Error    CV Standard Error    Number of Variables   
#>  ─────────────────────────────────────────────────────────────────────── 
#>    0.341280585    8.810912            0.1225620                      0   
#>    0.137775561    8.676569            0.1365648                      3   
#>    0.111754005    8.716773            0.1497894                      5   
#>    0.036594398    8.868234            0.1728979                      8   
#>    0.011983016    8.851866            0.1756021                     10   
#>    0.003923897    8.864507            0.1768980                     10   
#>    0.001284899    8.867502            0.1772517                     10   
#>    4.207467e-4    8.867916            0.1773026                     10   
#>    3.412806e-4    8.867930            0.1773055                     10   
#>  ─────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Model Comparison                                                              
#>  ───────────────────────────────────────────────────────────────────────────── 
#>    Model         Lambda       CV Error    Variables    C-index      AIC        
#>  ───────────────────────────────────────────────────────────────────────────── 
#>    Lambda Min    0.1377756    8.676569            3    0.6069288    1242.083   
#>  ───────────────────────────────────────────────────────────────────────────── 
#>    Note. AIC is computed from an unpenalized Cox refit on the selected
#>    variables. Values are approximate and intended for relative comparison
#>    only.
#> 
#> 
#>  Convergence Information                               
#>  ───────────────────────────────────────────────────── 
#>    Converged    Iterations    Tolerance    Algorithm   
#>  ───────────────────────────────────────────────────── 
#>    .            .             .            .           
#>  ───────────────────────────────────────────────────── 
#> 
#> 
#> character(0)
```

> **Note:** `ncvreg` requires alpha \> 0. Pure ridge (alpha = 0) is not
> supported. Values between 0.5 and 1.0 provide a useful range of
> sparsity/stability tradeoffs.

------------------------------------------------------------------------

### 9. Standardization

When covariates are on different scales (e.g., age in years vs. LDH in
U/L), standardization ensures the penalty is applied fairly across all
variables.

``` r

# Without standardization (not recommended for mixed-scale variables)
ncvregcox(
  data = ncvregcox_clinical,
  time = "time",
  event = "event",
  outcomeLevel = "1",
  censorLevel = "0",
  covariates = c("age", "tumor_diameter", "ldh_level", "cea_level"),
  penalty = "SCAD",
  standardize = FALSE,
  cv_folds = 10,
  lambda_type = "min",
  suitabilityCheck = FALSE,
  variable_importance = TRUE
)
#> 
#>  SCAD COX REGRESSION RESULTS
#> ERROR: Analysis Error
#> missing value where TRUE/FALSE needed
#>  SCAD/MCP Cox Regression Analysis
#> 
#>  Non-convex penalized Cox regression for high-dimensional survival data
#>  analysis.
#> 
#>  Current Configuration:
#> 
#>  Penalty Function: Smoothly Clipped Absolute Deviation
#>  (SCAD)Cross-Validation: 10-fold CVLambda Selection: Minimum CV Error
#> 
#>  Key Features:
#> 
#>  Oracle properties for variable selectionAvoids over-penalization of
#>  large coefficientsMaintains sparsity for irrelevant
#>  variablesCross-validation for optimal penalty selectionVariable
#>  importance and stability analysis
#> 
#>  Note: SCAD penalty provides superior variable selection properties
#>  compared to LASSO, particularly for scenarios with large true effects.
#> 
#>  Model Summary                                                                                                    
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Penalty Function                              Selected Lambda    CV Error    Variables Selected    C-index     
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    .                                             .                  .           .                     .           
#>    Smoothly Clipped Absolute Deviation (SCAD)        0.008894582    8.652232                     4    0.6105539   
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Selected Variables                                
#>  ───────────────────────────────────────────────── 
#>    Variable          Coefficient    Hazard Ratio   
#>  ───────────────────────────────────────────────── 
#>    age                0.01556568        1.015687   
#>    tumor_diameter     0.19684926        1.217560   
#>    ldh_level         9.839539e-4        1.000984   
#>    cea_level          0.01771401        1.017872   
#>  ───────────────────────────────────────────────── 
#> 
#> 
#>  Variable Importance                                                 
#>  ─────────────────────────────────────────────────────────────────── 
#>    Variable    Importance Score    Rank    Relative Importance (%)   
#>  ─────────────────────────────────────────────────────────────────── 
#>  ─────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Cross-Validation Summary                                                
#>  ─────────────────────────────────────────────────────────────────────── 
#>    Lambda         CV Error    CV Standard Error    Number of Variables   
#>  ─────────────────────────────────────────────────────────────────────── 
#>    0.236247916    8.843162            0.1216505                      0   
#>    0.077360541    8.717838            0.1398939                      3   
#>    0.025332090    8.655764            0.1471934                      4   
#>    0.008894582    8.652232            0.1479323                      4   
#>    0.008295117    8.652233            0.1479320                      4   
#>    0.002716277    8.652420            0.1477435                      4   
#>    8.894582e-4    8.652512            0.1476790                      4   
#>    2.912575e-4    8.652512            0.1476790                      4   
#>    2.362479e-4    8.652512            0.1476790                      4   
#>  ─────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Model Comparison                                                                
#>  ─────────────────────────────────────────────────────────────────────────────── 
#>    Model         Lambda         CV Error    Variables    C-index      AIC        
#>  ─────────────────────────────────────────────────────────────────────────────── 
#>    Lambda Min    0.008894582    8.652232            4    0.6105539    1279.694   
#>  ─────────────────────────────────────────────────────────────────────────────── 
#>    Note. AIC is computed from an unpenalized Cox refit on the selected
#>    variables. Values are approximate and intended for relative comparison
#>    only.
#> 
#> 
#>  Convergence Information                               
#>  ───────────────────────────────────────────────────── 
#>    Converged    Iterations    Tolerance    Algorithm   
#>  ───────────────────────────────────────────────────── 
#>    .            .             .            .           
#>  ───────────────────────────────────────────────────── 
#> 
#> 
#> character(0)
```

**Caution:** Without standardization, variables with larger numeric
ranges (like LDH ~ 80-600) will be penalized differently from variables
with smaller ranges (like tumor diameter ~ 0.3-20). The default
`standardize = TRUE` is almost always the correct choice.

------------------------------------------------------------------------

### 10. Edge Cases

#### Small Sample (n=25)

The `ncvregcox_small` dataset tests how the module handles very small
samples. The suitability assessment should flag concerns about sample
size and EPV.

``` r

data(ncvregcox_small, package = "ClinicoPath")

cat("N:", nrow(ncvregcox_small),
    "Events:", sum(ncvregcox_small$event == "1", na.rm = TRUE),
    "Covariates:", ncol(ncvregcox_small) - 2, "\n")
#> N: 25 Events: 16 Covariates: 5

ncvregcox(
  data = ncvregcox_small,
  time = "time",
  event = "event",
  outcomeLevel = "1",
  censorLevel = "0",
  covariates = c("age", "marker1", "marker2", "marker3", "grade"),
  penalty = "SCAD",
  gamma = 3.7,
  cv_folds = 5,        # Reduced folds for small sample
  lambda_type = "min",
  standardize = TRUE,
  suitabilityCheck = TRUE,
  plot_path = TRUE,
  plot_cv = TRUE,
  variable_importance = TRUE
)
#> 
#>  SCAD COX REGRESSION RESULTS
#> ERROR: Analysis Error
#> missing value where TRUE/FALSE needed
#>  SCAD/MCP Cox Regression Analysis
#> 
#>  Non-convex penalized Cox regression for high-dimensional survival data
#>  analysis.
#> 
#>  Current Configuration:
#> 
#>  Penalty Function: Smoothly Clipped Absolute Deviation
#>  (SCAD)Cross-Validation: 5-fold CVLambda Selection: Minimum CV Error
#> 
#>  Key Features:
#> 
#>  Oracle properties for variable selectionAvoids over-penalization of
#>  large coefficientsMaintains sparsity for irrelevant
#>  variablesCross-validation for optimal penalty selectionVariable
#>  importance and stability analysis
#> 
#>  Note: SCAD penalty provides superior variable selection properties
#>  compared to LASSO, particularly for scenarios with large true effects.
#> 
#>  <div style='background-color: #f8d7da; color: #721c24; border: 1px
#>  solid #f5c6cb; padding: 12px; border-radius: 6px; margin-bottom:
#>  12px;'>Overall: Some issues require attention before relying on these
#>  results.<table style='width: 100%; border-collapse: collapse;
#>  font-size: 13px;'><tr style='border-bottom: 2px solid #dee2e6;'><th
#>  style='padding: 6px; text-align: left;'>Status<th style='padding: 6px;
#>  text-align: left;'>Check<th style='padding: 6px; text-align:
#>  left;'>Value<th style='padding: 6px; text-align: left;'>Detail<tr
#>  style='border-bottom: 1px solid #dee2e6;'><td style='padding:
#>  6px;'><span style='color: #ffc107; font-size: 18px;'>&#9679;<td
#>  style='padding: 6px;'>Events-Per-Variable (Overall)<td style='padding:
#>  6px;'>3.2 (n_events=16, p=5)<td style='padding: 6px;'>Adequate for
#>  SCAD/MCP penalized regression, which handles low EPV better than
#>  standard Cox.<tr style='border-bottom: 1px solid #dee2e6;'><td
#>  style='padding: 6px;'><span style='color: #ffc107; font-size:
#>  18px;'>&#9679;<td style='padding: 6px;'>Regularization Need<td
#>  style='padding: 6px;'>p=5, EPV=3<td style='padding: 6px;'>Moderate/low
#>  dimensionality. Standard Cox may also suffice.<tr
#>  style='border-bottom: 1px solid #dee2e6;'><td style='padding:
#>  6px;'><span style='color: #dc3545; font-size: 18px;'>&#9679;<td
#>  style='padding: 6px;'>Sample Size<td style='padding: 6px;'>n=25<td
#>  style='padding: 6px;'>Very small sample. Results will be highly
#>  variable.<tr style='border-bottom: 1px solid #dee2e6;'><td
#>  style='padding: 6px;'><span style='color: #28a745; font-size:
#>  18px;'>&#9679;<td style='padding: 6px;'>Event Rate<td style='padding:
#>  6px;'>64.0% (16/25)<td style='padding: 6px;'>Balanced event rate. Good
#>  for model estimation.<tr style='border-bottom: 1px solid #dee2e6;'><td
#>  style='padding: 6px;'><span style='color: #28a745; font-size:
#>  18px;'>&#9679;<td style='padding: 6px;'>Multicollinearity<td
#>  style='padding: 6px;'>Max |r| = 0.30<td style='padding: 6px;'>No
#>  concerning collinearity detected.<tr style='border-bottom: 1px solid
#>  #dee2e6;'><td style='padding: 6px;'><span style='color: #28a745;
#>  font-size: 18px;'>&#9679;<td style='padding: 6px;'>Data Quality<td
#>  style='padding: 6px;'>No missing data<td style='padding:
#>  6px;'>Complete dataset.
#> 
#>  Model Summary                                                                                                    
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Penalty Function                              Selected Lambda    CV Error    Variables Selected    C-index     
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    .                                             .                  .           .                     .           
#>    Smoothly Clipped Absolute Deviation (SCAD)          0.2788590    4.509244                     0                
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Selected Variables                          
#>  ─────────────────────────────────────────── 
#>    Variable    Coefficient    Hazard Ratio   
#>  ─────────────────────────────────────────── 
#>  ─────────────────────────────────────────── 
#> 
#> 
#>  Variable Importance                                                 
#>  ─────────────────────────────────────────────────────────────────── 
#>    Variable    Importance Score    Rank    Relative Importance (%)   
#>  ─────────────────────────────────────────────────────────────────── 
#>  ─────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Cross-Validation Summary                                                
#>  ─────────────────────────────────────────────────────────────────────── 
#>    Lambda         CV Error    CV Standard Error    Number of Variables   
#>  ─────────────────────────────────────────────────────────────────────── 
#>    0.278858994    4.509244            0.4450884                      0   
#>    0.091313749    5.085485            0.9003773                      5   
#>    0.029901136    5.093816            0.9682450                      5   
#>    0.009791274    5.072642            0.9699710                      5   
#>    0.003206201    5.072798            0.9699664                      5   
#>    0.001049886    5.078330            0.9694018                      5   
#>    3.437904e-4    5.070231            0.9668695                      5   
#>    2.788590e-4    5.070230            0.9668691                      5   
#>  ─────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Model Comparison                                                       
#>  ────────────────────────────────────────────────────────────────────── 
#>    Model         Lambda       CV Error    Variables    C-index    AIC   
#>  ────────────────────────────────────────────────────────────────────── 
#>    Lambda Min    0.2788590    4.509244            0                     
#>  ────────────────────────────────────────────────────────────────────── 
#>    Note. AIC is computed from an unpenalized Cox refit on the
#>    selected variables. Values are approximate and intended for
#>    relative comparison only.
#> 
#> 
#>  Convergence Information                               
#>  ───────────────────────────────────────────────────── 
#>    Converged    Iterations    Tolerance    Algorithm   
#>  ───────────────────────────────────────────────────── 
#>    .            .             .            .           
#>  ───────────────────────────────────────────────────── 
#> 
#> 
#> character(0)
```

**Expected suitability flags:**

- **Sample Size**: Yellow or red (n=25 is small)
- **EPV**: Likely yellow (few events relative to 5 covariates)
- **Regularization Need**: Yellow (low dimensionality - standard Cox may
  suffice)

With only 25 observations, cross-validation folds should be reduced
(e.g., 5 instead of 10) to ensure each fold has enough events for
reliable estimation.

#### Extreme Multicollinearity

The `ncvregcox_collinear` dataset has pairs of variables with
correlations exceeding 0.9 (x1/x2 have r ~ 0.995). This is a known
challenge for penalized methods.

``` r

data(ncvregcox_collinear, package = "ClinicoPath")

cat("N:", nrow(ncvregcox_collinear),
    "Events:", sum(ncvregcox_collinear$event == "1", na.rm = TRUE), "\n")
#> N: 150 Events: 102

# Check the correlation structure
num_cols <- ncvregcox_collinear[, paste0("x", 1:10)]
cor_mat <- cor(num_cols, use = "pairwise.complete.obs")
cat("Max pairwise |r|:", round(max(abs(cor_mat[upper.tri(cor_mat)])), 3), "\n")
#> Max pairwise |r|: 0.994
```

``` r

# SCAD under extreme collinearity
ncvregcox(
  data = ncvregcox_collinear,
  time = "time",
  event = "event",
  outcomeLevel = "1",
  censorLevel = "0",
  covariates = paste0("x", 1:10),
  penalty = "SCAD",
  gamma = 3.7,
  cv_folds = 10,
  lambda_type = "min",
  standardize = TRUE,
  suitabilityCheck = TRUE,
  plot_path = TRUE,
  plot_cv = TRUE,
  variable_importance = TRUE
)
#> 
#>  SCAD COX REGRESSION RESULTS
#> ERROR: Analysis Error
#> missing value where TRUE/FALSE needed
#>  SCAD/MCP Cox Regression Analysis
#> 
#>  Non-convex penalized Cox regression for high-dimensional survival data
#>  analysis.
#> 
#>  Current Configuration:
#> 
#>  Penalty Function: Smoothly Clipped Absolute Deviation
#>  (SCAD)Cross-Validation: 10-fold CVLambda Selection: Minimum CV Error
#> 
#>  Key Features:
#> 
#>  Oracle properties for variable selectionAvoids over-penalization of
#>  large coefficientsMaintains sparsity for irrelevant
#>  variablesCross-validation for optimal penalty selectionVariable
#>  importance and stability analysis
#> 
#>  Note: SCAD penalty provides superior variable selection properties
#>  compared to LASSO, particularly for scenarios with large true effects.
#> 
#>  <div style='background-color: #f8d7da; color: #721c24; border: 1px
#>  solid #f5c6cb; padding: 12px; border-radius: 6px; margin-bottom:
#>  12px;'>Overall: Some issues require attention before relying on these
#>  results.<table style='width: 100%; border-collapse: collapse;
#>  font-size: 13px;'><tr style='border-bottom: 2px solid #dee2e6;'><th
#>  style='padding: 6px; text-align: left;'>Status<th style='padding: 6px;
#>  text-align: left;'>Check<th style='padding: 6px; text-align:
#>  left;'>Value<th style='padding: 6px; text-align: left;'>Detail<tr
#>  style='border-bottom: 1px solid #dee2e6;'><td style='padding:
#>  6px;'><span style='color: #28a745; font-size: 18px;'>&#9679;<td
#>  style='padding: 6px;'>Events-Per-Variable (Overall)<td style='padding:
#>  6px;'>10.2 (n_events=102, p=10)<td style='padding: 6px;'>High EPV.
#>  Regularization will perform robustly.<tr style='border-bottom: 1px
#>  solid #dee2e6;'><td style='padding: 6px;'><span style='color: #ffc107;
#>  font-size: 18px;'>&#9679;<td style='padding: 6px;'>Regularization
#>  Need<td style='padding: 6px;'>p=10, EPV=10<td style='padding:
#>  6px;'>Moderate/low dimensionality. Standard Cox may also suffice.<tr
#>  style='border-bottom: 1px solid #dee2e6;'><td style='padding:
#>  6px;'><span style='color: #28a745; font-size: 18px;'>&#9679;<td
#>  style='padding: 6px;'>Sample Size<td style='padding: 6px;'>n=150<td
#>  style='padding: 6px;'>Adequate sample size for penalized
#>  regression.<tr style='border-bottom: 1px solid #dee2e6;'><td
#>  style='padding: 6px;'><span style='color: #28a745; font-size:
#>  18px;'>&#9679;<td style='padding: 6px;'>Event Rate<td style='padding:
#>  6px;'>68.0% (102/150)<td style='padding: 6px;'>Balanced event rate.
#>  Good for model estimation.<tr style='border-bottom: 1px solid
#>  #dee2e6;'><td style='padding: 6px;'><span style='color: #dc3545;
#>  font-size: 18px;'>&#9679;<td style='padding:
#>  6px;'>Multicollinearity<td style='padding: 6px;'>Max |r| = 0.99<td
#>  style='padding: 6px;'>High collinearity. SCAD/MCP can be highly
#>  unstable under extreme collinearity. Consider using Elastic Net
#>  instead.<tr style='border-bottom: 1px solid #dee2e6;'><td
#>  style='padding: 6px;'><span style='color: #28a745; font-size:
#>  18px;'>&#9679;<td style='padding: 6px;'>Data Quality<td
#>  style='padding: 6px;'>No missing data<td style='padding:
#>  6px;'>Complete dataset.
#> 
#>  Model Summary                                                                                                    
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Penalty Function                              Selected Lambda    CV Error    Variables Selected    C-index     
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    .                                             .                  .           .                     .           
#>    Smoothly Clipped Absolute Deviation (SCAD)         0.08179440    7.832010                     3    0.6643347   
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Selected Variables                          
#>  ─────────────────────────────────────────── 
#>    Variable    Coefficient    Hazard Ratio   
#>  ─────────────────────────────────────────── 
#>    x2           0.63972741       1.8959640   
#>    x6           0.37166988       1.4501542   
#>    x9          -0.03617608       0.9644705   
#>  ─────────────────────────────────────────── 
#> 
#> 
#>  Variable Importance                                                 
#>  ─────────────────────────────────────────────────────────────────── 
#>    Variable    Importance Score    Rank    Relative Importance (%)   
#>  ─────────────────────────────────────────────────────────────────── 
#>  ─────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Cross-Validation Summary                                                
#>  ─────────────────────────────────────────────────────────────────────── 
#>    Lambda         CV Error    CV Standard Error    Number of Variables   
#>  ─────────────────────────────────────────────────────────────────────── 
#>    0.287195838    8.146070            0.1556413                      0   
#>    0.094043689    7.838924            0.1924037                      3   
#>    0.081794405    7.832010            0.1950036                      3   
#>    0.030795068    7.922251            0.2112065                      7   
#>    0.010083997    7.894388            0.2174274                      8   
#>    0.003302054    7.903050            0.2189282                      9   
#>    0.001081274    7.899255            0.2192219                     10   
#>    3.540685e-4    7.898646            0.2192452                     10   
#>    2.871958e-4    7.899004            0.2192955                     10   
#>  ─────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Model Comparison                                                               
#>  ────────────────────────────────────────────────────────────────────────────── 
#>    Model         Lambda        CV Error    Variables    C-index      AIC        
#>  ────────────────────────────────────────────────────────────────────────────── 
#>    Lambda Min    0.08179440    7.832010            3    0.6643347    796.5655   
#>  ────────────────────────────────────────────────────────────────────────────── 
#>    Note. AIC is computed from an unpenalized Cox refit on the selected
#>    variables. Values are approximate and intended for relative comparison
#>    only.
#> 
#> 
#>  Convergence Information                               
#>  ───────────────────────────────────────────────────── 
#>    Converged    Iterations    Tolerance    Algorithm   
#>  ───────────────────────────────────────────────────── 
#>    .            .             .            .           
#>  ───────────────────────────────────────────────────── 
#> 
#> 
#> character(0)
```

``` r

# MCP under extreme collinearity
ncvregcox(
  data = ncvregcox_collinear,
  time = "time",
  event = "event",
  outcomeLevel = "1",
  censorLevel = "0",
  covariates = paste0("x", 1:10),
  penalty = "MCP",
  gamma = 3.0,
  cv_folds = 10,
  lambda_type = "min",
  standardize = TRUE,
  suitabilityCheck = TRUE,
  plot_path = TRUE,
  plot_cv = TRUE,
  variable_importance = TRUE
)
#> 
#>  SCAD COX REGRESSION RESULTS
#> ERROR: Analysis Error
#> missing value where TRUE/FALSE needed
#>  SCAD/MCP Cox Regression Analysis
#> 
#>  Non-convex penalized Cox regression for high-dimensional survival data
#>  analysis.
#> 
#>  Current Configuration:
#> 
#>  Penalty Function: Minimax Concave Penalty (MCP)Cross-Validation:
#>  10-fold CVLambda Selection: Minimum CV Error
#> 
#>  Key Features:
#> 
#>  Oracle properties for variable selectionAvoids over-penalization of
#>  large coefficientsMaintains sparsity for irrelevant
#>  variablesCross-validation for optimal penalty selectionVariable
#>  importance and stability analysis
#> 
#>  Note: MCP penalty provides superior variable selection properties
#>  compared to LASSO, particularly for scenarios with large true effects.
#> 
#>  <div style='background-color: #f8d7da; color: #721c24; border: 1px
#>  solid #f5c6cb; padding: 12px; border-radius: 6px; margin-bottom:
#>  12px;'>Overall: Some issues require attention before relying on these
#>  results.<table style='width: 100%; border-collapse: collapse;
#>  font-size: 13px;'><tr style='border-bottom: 2px solid #dee2e6;'><th
#>  style='padding: 6px; text-align: left;'>Status<th style='padding: 6px;
#>  text-align: left;'>Check<th style='padding: 6px; text-align:
#>  left;'>Value<th style='padding: 6px; text-align: left;'>Detail<tr
#>  style='border-bottom: 1px solid #dee2e6;'><td style='padding:
#>  6px;'><span style='color: #28a745; font-size: 18px;'>&#9679;<td
#>  style='padding: 6px;'>Events-Per-Variable (Overall)<td style='padding:
#>  6px;'>10.2 (n_events=102, p=10)<td style='padding: 6px;'>High EPV.
#>  Regularization will perform robustly.<tr style='border-bottom: 1px
#>  solid #dee2e6;'><td style='padding: 6px;'><span style='color: #ffc107;
#>  font-size: 18px;'>&#9679;<td style='padding: 6px;'>Regularization
#>  Need<td style='padding: 6px;'>p=10, EPV=10<td style='padding:
#>  6px;'>Moderate/low dimensionality. Standard Cox may also suffice.<tr
#>  style='border-bottom: 1px solid #dee2e6;'><td style='padding:
#>  6px;'><span style='color: #28a745; font-size: 18px;'>&#9679;<td
#>  style='padding: 6px;'>Sample Size<td style='padding: 6px;'>n=150<td
#>  style='padding: 6px;'>Adequate sample size for penalized
#>  regression.<tr style='border-bottom: 1px solid #dee2e6;'><td
#>  style='padding: 6px;'><span style='color: #28a745; font-size:
#>  18px;'>&#9679;<td style='padding: 6px;'>Event Rate<td style='padding:
#>  6px;'>68.0% (102/150)<td style='padding: 6px;'>Balanced event rate.
#>  Good for model estimation.<tr style='border-bottom: 1px solid
#>  #dee2e6;'><td style='padding: 6px;'><span style='color: #dc3545;
#>  font-size: 18px;'>&#9679;<td style='padding:
#>  6px;'>Multicollinearity<td style='padding: 6px;'>Max |r| = 0.99<td
#>  style='padding: 6px;'>High collinearity. SCAD/MCP can be highly
#>  unstable under extreme collinearity. Consider using Elastic Net
#>  instead.<tr style='border-bottom: 1px solid #dee2e6;'><td
#>  style='padding: 6px;'><span style='color: #28a745; font-size:
#>  18px;'>&#9679;<td style='padding: 6px;'>Data Quality<td
#>  style='padding: 6px;'>No missing data<td style='padding:
#>  6px;'>Complete dataset.
#> 
#>  Model Summary                                                                                       
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Penalty Function                 Selected Lambda    CV Error    Variables Selected    C-index     
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    .                                .                  .           .                     .           
#>    Minimax Concave Penalty (MCP)          0.1081274    7.836369                     2    0.6592224   
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Selected Variables                          
#>  ─────────────────────────────────────────── 
#>    Variable    Coefficient    Hazard Ratio   
#>  ─────────────────────────────────────────── 
#>    x1            0.6128137        1.845617   
#>    x6            0.3362671        1.399713   
#>  ─────────────────────────────────────────── 
#> 
#> 
#>  Variable Importance                                                 
#>  ─────────────────────────────────────────────────────────────────── 
#>    Variable    Importance Score    Rank    Relative Importance (%)   
#>  ─────────────────────────────────────────────────────────────────── 
#>  ─────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Cross-Validation Summary                                                
#>  ─────────────────────────────────────────────────────────────────────── 
#>    Lambda         CV Error    CV Standard Error    Number of Variables   
#>  ─────────────────────────────────────────────────────────────────────── 
#>    0.287195838    8.150055            0.1546802                      0   
#>    0.108127389    7.836369            0.1921363                      2   
#>    0.094043689    7.837032            0.1947836                      3   
#>    0.030795068    8.001839            0.2180236                      6   
#>    0.010083997    7.994475            0.2186889                      8   
#>    0.003302054    7.971119            0.2213949                      9   
#>    0.001081274    7.970342            0.2221683                     10   
#>    3.540685e-4    7.970273            0.2220549                     10   
#>    2.871958e-4    7.970662            0.2221097                     10   
#>  ─────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Model Comparison                                                              
#>  ───────────────────────────────────────────────────────────────────────────── 
#>    Model         Lambda       CV Error    Variables    C-index      AIC        
#>  ───────────────────────────────────────────────────────────────────────────── 
#>    Lambda Min    0.1081274    7.836369            2    0.6592224    799.5745   
#>  ───────────────────────────────────────────────────────────────────────────── 
#>    Note. AIC is computed from an unpenalized Cox refit on the selected
#>    variables. Values are approximate and intended for relative comparison
#>    only.
#> 
#> 
#>  Convergence Information                               
#>  ───────────────────────────────────────────────────── 
#>    Converged    Iterations    Tolerance    Algorithm   
#>  ───────────────────────────────────────────────────── 
#>    .            .             .            .           
#>  ───────────────────────────────────────────────────── 
#> 
#> 
#> character(0)
```

``` r

# Elastic net mixing can stabilize selection under collinearity
ncvregcox(
  data = ncvregcox_collinear,
  time = "time",
  event = "event",
  outcomeLevel = "1",
  censorLevel = "0",
  covariates = paste0("x", 1:10),
  penalty = "SCAD",
  alpha = 0.5,     # Strong L2 component for stability
  gamma = 3.7,
  cv_folds = 10,
  lambda_type = "min",
  standardize = TRUE,
  suitabilityCheck = FALSE,
  plot_cv = TRUE,
  variable_importance = TRUE
)
#> 
#>  SCAD COX REGRESSION RESULTS
#> ERROR: Analysis Error
#> missing value where TRUE/FALSE needed
#>  SCAD/MCP Cox Regression Analysis
#> 
#>  Non-convex penalized Cox regression for high-dimensional survival data
#>  analysis.
#> 
#>  Current Configuration:
#> 
#>  Penalty Function: Smoothly Clipped Absolute Deviation
#>  (SCAD)Cross-Validation: 10-fold CVLambda Selection: Minimum CV Error
#> 
#>  Key Features:
#> 
#>  Oracle properties for variable selectionAvoids over-penalization of
#>  large coefficientsMaintains sparsity for irrelevant
#>  variablesCross-validation for optimal penalty selectionVariable
#>  importance and stability analysis
#> 
#>  Note: SCAD penalty provides superior variable selection properties
#>  compared to LASSO, particularly for scenarios with large true effects.
#> 
#>  Model Summary                                                                                                    
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Penalty Function                              Selected Lambda    CV Error    Variables Selected    C-index     
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    .                                             .                  .           .                     .           
#>    Smoothly Clipped Absolute Deviation (SCAD)          0.1422812    7.837973                     3    0.6631239   
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Selected Variables                          
#>  ─────────────────────────────────────────── 
#>    Variable    Coefficient    Hazard Ratio   
#>  ─────────────────────────────────────────── 
#>    x2           0.59107600       1.8059306   
#>    x6           0.36746139       1.4440640   
#>    x9          -0.04858484       0.9525765   
#>  ─────────────────────────────────────────── 
#> 
#> 
#>  Variable Importance                                                 
#>  ─────────────────────────────────────────────────────────────────── 
#>    Variable    Importance Score    Rank    Relative Importance (%)   
#>  ─────────────────────────────────────────────────────────────────── 
#>  ─────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Cross-Validation Summary                                                
#>  ─────────────────────────────────────────────────────────────────────── 
#>    Lambda         CV Error    CV Standard Error    Number of Variables   
#>  ─────────────────────────────────────────────────────────────────────── 
#>    0.574391676    8.146752            0.1554910                      0   
#>    0.188087378    7.871625            0.1819404                      3   
#>    0.142281204    7.837973            0.1897391                      3   
#>    0.061590137    7.886035            0.2039046                      6   
#>    0.020167993    7.900975            0.2143940                      8   
#>    0.006604108    7.899332            0.2163768                      9   
#>    0.002162548    7.906855            0.2182262                     10   
#>    7.081369e-4    7.911116            0.2188516                     10   
#>    5.743917e-4    7.911180            0.2189333                     10   
#>  ─────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Model Comparison                                                              
#>  ───────────────────────────────────────────────────────────────────────────── 
#>    Model         Lambda       CV Error    Variables    C-index      AIC        
#>  ───────────────────────────────────────────────────────────────────────────── 
#>    Lambda Min    0.1422812    7.837973            3    0.6631239    796.5655   
#>  ───────────────────────────────────────────────────────────────────────────── 
#>    Note. AIC is computed from an unpenalized Cox refit on the selected
#>    variables. Values are approximate and intended for relative comparison
#>    only.
#> 
#> 
#>  Convergence Information                               
#>  ───────────────────────────────────────────────────── 
#>    Converged    Iterations    Tolerance    Algorithm   
#>  ───────────────────────────────────────────────────── 
#>    .            .             .            .           
#>  ───────────────────────────────────────────────────── 
#> 
#> 
#> character(0)
```

**Expected suitability flags:**

- **Multicollinearity**: Red (max \|r\| \> 0.9)
- The suitability report will recommend considering Elastic Net (alpha
  \< 1)

**Key observation:** Under extreme collinearity, SCAD and MCP may
arbitrarily swap which of the correlated variables (x1 vs x2 vs x3) gets
selected. The *group* is correctly identified as important, but the
specific member chosen is unstable. This is a fundamental limitation of
any variable selection method when predictors are nearly identical.
Elastic net mixing (alpha \< 1) can help by distributing the coefficient
across correlated predictors rather than forcing a single winner.

------------------------------------------------------------------------

### Interpreting Results

#### Selected Variables Table

| Column       | Meaning                                          |
|--------------|--------------------------------------------------|
| Variable     | Predictor name                                   |
| Coefficient  | Log hazard ratio (nearly unbiased with SCAD/MCP) |
| Hazard Ratio | exp(coefficient)                                 |

#### Key Advantage Over LASSO

For a true coefficient of 0.8:

- LASSO might estimate 0.5 (biased downward)
- SCAD/MCP might estimate 0.78 (nearly unbiased)

This matters for hazard ratio interpretation in clinical settings. If
tumor diameter truly doubles the hazard (HR = 2.0, beta = 0.69), LASSO
might report HR = 1.5 while SCAD reports HR = 1.95. The SCAD estimate is
more trustworthy for clinical decision-making.

#### Variable Importance

Variables are ranked by absolute coefficient value. With SCAD/MCP, the
ranking is more reliable than LASSO because coefficients are less
biased.

#### Suitability Assessment

The traffic-light table provides a quick pre-analysis diagnostic:

| Color  | Meaning                                     |
|--------|---------------------------------------------|
| Green  | No concerns - proceed with confidence       |
| Yellow | Usable but review the flagged item          |
| Red    | Potential issue - results may be unreliable |

------------------------------------------------------------------------

### SCAD vs MCP: Which to Choose?

| Criterion                | SCAD               | MCP                  |
|--------------------------|--------------------|----------------------|
| Bias for large effects   | Very low           | Very low             |
| Selection aggressiveness | Moderate           | More aggressive      |
| Stability                | Good               | Slightly less stable |
| Recommended gamma        | 3.7 (default)      | 3.0 (default)        |
| Publication standard     | More established   | Gaining adoption     |
| Convergence              | Generally reliable | Can be faster        |

**Practical recommendation**: Start with SCAD (gamma=3.7). Switch to MCP
if you want sparser selection. If both methods agree on the selected
variables, you have strong evidence of a robust signal.

------------------------------------------------------------------------

### Common Pitfalls

1.  **Setting gamma too low for SCAD**: gamma \< 2.5 makes optimization
    unstable. Stick with the default 3.7 unless you have a specific
    reason to change it.

2.  **Comparing SCAD/MCP coefficients directly to LASSO**: SCAD/MCP
    coefficients are less biased. A SCAD coefficient of 0.8 is not
    “bigger” than a LASSO coefficient of 0.5 for the same variable - the
    LASSO version is just more shrunk.

3.  **Not reporting the penalty type**: Always specify whether you used
    SCAD or MCP, along with the gamma value and lambda selection method.

4.  **Using SCAD/MCP for very high dimensional data (p \>\> 10n)**: In
    extremely high-dimensional settings, LASSO or Elastic Net may be
    more stable. SCAD/MCP work best when p is moderately large relative
    to n.

5.  **Ignoring the suitability assessment**: The traffic-light report is
    there for a reason. A red flag on multicollinearity or sample size
    means the selected model may not be trustworthy. Consider the
    suggested remedies (reduce covariates, use elastic net mixing,
    increase sample size).

6.  **Post-selection inference**: Coefficients from SCAD/MCP are
    *selected* coefficients. Standard confidence intervals and p-values
    from a refit Cox model on the selected variables are **not valid**
    for inference because the selection step was not accounted for. If
    formal inference is needed, consider methods such as selective
    inference (Lee et al., 2016) or sample splitting. The coefficients
    reported here are appropriate for prediction and variable ranking,
    but should not be interpreted as confirmed causal effects without
    further validation.

------------------------------------------------------------------------

### Related ClinicoPath Functions

| Function | Use When |
|----|----|
| **LASSO Cox** (`lassocox`) | Standard LASSO with suitability assessment |
| **Adaptive LASSO** (`adaptivelasso`) | Two-stage adaptive weights approach |
| **High-Dimensional Cox** (`highdimcox`) | Multiple regularization methods unified |
| **PLS Cox** (`plscox`) | Dimensionality reduction for very high p |
| **Multivariable Survival** | Standard Cox, no regularization needed |

------------------------------------------------------------------------

### References

- Fan J, Li R. Variable selection via nonconcave penalized likelihood
  and its oracle properties. *J Am Stat Assoc*. 2001;96(456):1348-1360.
- Zhang CH. Nearly unbiased variable selection under minimax concave
  penalty. *Ann Stat*. 2010;38(2):894-942.
- Breheny P, Huang J. Coordinate descent algorithms for nonconvex
  penalized regression, with applications to biological feature
  selection. *Ann Appl Stat*. 2011;5(1):232-253.
- Fan J, Li R. Variable selection for Cox’s proportional hazards model
  and frailty model. *Ann Stat*. 2002;30(1):74-99.
- Lee JD, Sun DL, Sun Y, Taylor JE. Exact post-selection inference, with
  application to the lasso. *Ann Stat*. 2016;44(3):907-927.
