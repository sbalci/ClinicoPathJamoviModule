# High-Dimensional Cox Regression - Comprehensive Guide

> **Not yet released.** The `highdimcox` analysis is on a development
> menu route, so it does not appear in the jamovi menus of ClinicoPath
> or of any of its submodules. It is documented here ahead of a future
> release, and its options, defaults and output may still change. The R
> function is exported, so the examples below run from an R console;
> what is not yet available is the jamovi analysis itself.

> **Note:** The
> [`highdimcox()`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/highdimcox.md)
> function is designed for use within **jamovi’s GUI**. The code
> examples below show the R syntax for reference.

## High-Dimensional Cox Regression

### Overview

The **High-Dimensional Cox Regression** module (`highdimcox`) provides a
unified interface for regularized Cox regression when the number of
predictors is large relative to sample size. It supports **LASSO**,
**Ridge**, **Elastic Net**, and **Adaptive LASSO** regularization, with
optional stability selection for robust variable identification.

This module is designed for:

- **Genomic survival studies** (gene expression panels, mutation data)
- **Proteomic/metabolomic data** (high-dimensional biomarker panels)
- **Radiomics studies** (texture and morphology features from imaging)
- **Any p \>\> n survival scenario**

Key features:

- **Multiple regularization methods** in one interface
- **Cross-validation** with configurable lambda selection (min, 1se)
- **Stability selection** via bootstrap for robust variable
  identification
- **Variable importance** and coefficient visualization
- **Model diagnostics** and performance metrics
- **Data suitability assessment** with traffic-light checks

------------------------------------------------------------------------

### Regularization Methods Explained

When you have more predictors than observations (or close to it),
standard Cox regression fails. Regularization adds a penalty to prevent
overfitting.

| Method | Penalty | Alpha | Selects Variables? | Best For |
|----|----|----|----|----|
| **LASSO** | $`\lambda \sum |\beta_j|`$ | 1.0 | Yes (sets to zero) | Sparse models |
| **Ridge** | $`\lambda \sum \beta_j^2`$ | 0.0 | No (shrinks all) | Prediction, collinearity |
| **Elastic Net** | Mixed L1+L2 | 0.5 | Yes (grouped) | Correlated groups |
| **Adaptive LASSO** | Weighted L1 | 1.0 | Yes (oracle) | Publication models |

The **Elastic Net** (default, alpha=0.5) is the recommended starting
point - it combines the variable selection of LASSO with the stability
of Ridge.

------------------------------------------------------------------------

### Datasets Used in This Guide

| Dataset | N | Events | Predictors | Description |
|----|----|----|----|----|
| Synthetic “genomic” | 150 | ~75 | 100 genes + 5 clinical | Gene expression survival study |
| Synthetic “proteomic” | 80 | ~50 | 50 proteins + 3 clinical | Smaller proteomic study |

------------------------------------------------------------------------

### 1. LASSO Regularization

Pure LASSO selects the sparsest model - ideal when you expect few true
predictors.

``` r

# Simulate a genomic survival study (150 patients, 100 genes)
set.seed(42)
n <- 150
p_genes <- 100

# Gene expression matrix
gene_matrix <- matrix(rnorm(n * p_genes), nrow = n)
colnames(gene_matrix) <- paste0("GENE_", sprintf("%03d", 1:p_genes))

# True effects: 6 genes affect survival
true_effects <- rep(0, p_genes)
true_effects[c(5, 12, 27, 43, 68, 91)] <- c(0.8, -0.6, 0.5, -0.4, 0.7, -0.5)

# Clinical variables
age <- rnorm(n, 62, 10)
gender <- factor(sample(c("Male", "Female"), n, replace = TRUE))
stage <- factor(sample(c("I", "II", "III"), n,
  replace = TRUE,
  prob = c(0.3, 0.4, 0.3)
))
grade <- factor(sample(1:3, n, replace = TRUE))
treatment <- factor(sample(c("A", "B"), n, replace = TRUE))

# Survival times
lp <- gene_matrix %*% true_effects + 0.02 * (age - 62) + 0.3 * (as.numeric(stage) - 1)
surv_time <- rweibull(n, shape = 1.3, scale = 30 * exp(-as.numeric(lp) * 0.2))
censor_time <- runif(n, 6, 48)
time <- pmax(pmin(surv_time, censor_time), 0.1)
event <- factor(ifelse(surv_time <= censor_time, "Dead", "Alive"),
  levels = c("Alive", "Dead")
)

data_genomic <- data.frame(
  survival_months = time,
  vital_status = event,
  age = age, gender = gender, stage = stage,
  grade = grade, treatment = treatment,
  as.data.frame(gene_matrix)
)

cat(
  "N:", n, "Events:", sum(event == "Dead"),
  "Event rate:", round(mean(event == "Dead"), 2), "\n"
)
#> N: 150 Events: 90 Event rate: 0.6
cat("Predictors:", ncol(data_genomic) - 2, "\n")
#> Predictors: 105
```

``` r

# All gene predictors (no clinical for clean LASSO demonstration)
gene_vars <- paste0("GENE_", sprintf("%03d", 1:p_genes))

highdimcox(
  data = data_genomic,
  elapsedtime = "survival_months",
  outcome = "vital_status",
  outcomeLevel = "Dead",
  censorLevel = "Alive",
  predictors = gene_vars,
  regularization_method = "lasso",
  cv_method = "cv_1se",
  cv_folds = 10,
  show_regularization_path = TRUE,
  show_cv_plot = TRUE,
  show_variable_importance = TRUE,
  show_coefficients_table = TRUE
)
#> Error:
#> ! Argument 'predictors' contains 'gene_vars' which is not present in the dataset
```

------------------------------------------------------------------------

### 2. Ridge Regularization

Ridge keeps all variables but shrinks coefficients - better for
prediction when you believe many variables contribute small effects.

``` r

highdimcox(
  data = data_genomic,
  elapsedtime = "survival_months",
  outcome = "vital_status",
  outcomeLevel = "Dead",
  censorLevel = "Alive",
  predictors = gene_vars,
  regularization_method = "ridge",
  cv_method = "cv_min",
  cv_folds = 10,
  show_cv_plot = TRUE,
  show_coefficients_table = TRUE,
  show_model_diagnostics = TRUE
)
#> Error:
#> ! Argument 'predictors' contains 'gene_vars' which is not present in the dataset
```

Note: Ridge regression **never** sets coefficients to exactly zero. All
variables are retained. A table note explains this behavior.

------------------------------------------------------------------------

### 3. Elastic Net (Default)

The recommended starting point - combines LASSO selection with Ridge
stability.

``` r

pred_vars <- c(
  "age", "gender", "stage",
  paste0("GENE_", sprintf("%03d", 1:p_genes))
)

highdimcox(
  data = data_genomic,
  elapsedtime = "survival_months",
  outcome = "vital_status",
  outcomeLevel = "Dead",
  censorLevel = "Alive",
  predictors = pred_vars,
  regularization_method = "elastic_net",
  alpha_value = 0.5,
  cv_method = "cv_1se",
  cv_folds = 10,
  show_regularization_path = TRUE,
  show_cv_plot = TRUE,
  show_variable_importance = TRUE,
  show_coefficients_table = TRUE
)
#> Error:
#> ! Argument 'predictors' contains 'pred_vars' which is not present in the dataset
```

#### Alpha Tuning

The alpha parameter controls the balance between L1 (LASSO) and L2
(Ridge). Higher alpha → more variable selection; lower alpha → more
shrinkage.

``` r

# More LASSO-like (alpha = 0.8)
highdimcox(
  data = data_genomic,
  elapsedtime = "survival_months",
  outcome = "vital_status",
  outcomeLevel = "Dead",
  censorLevel = "Alive",
  predictors = gene_vars,
  regularization_method = "elastic_net",
  alpha_value = 0.8,
  cv_folds = 10,
  show_coefficients_table = TRUE,
  show_cv_plot = TRUE
)
#> Error:
#> ! Argument 'predictors' contains 'gene_vars' which is not present in the dataset
```

------------------------------------------------------------------------

### 4. Adaptive LASSO

Two-stage approach: first fits Ridge to get initial coefficient
estimates, then uses inverse-coefficient weights to penalize unimportant
variables more heavily. This gives oracle variable selection properties.

``` r

highdimcox(
  data = data_genomic,
  elapsedtime = "survival_months",
  outcome = "vital_status",
  outcomeLevel = "Dead",
  censorLevel = "Alive",
  predictors = gene_vars,
  regularization_method = "adaptive_lasso",
  cv_method = "cv_1se",
  cv_folds = 10,
  show_regularization_path = TRUE,
  show_cv_plot = TRUE,
  show_variable_importance = TRUE,
  show_coefficients_table = TRUE
)
#> Error:
#> ! Argument 'predictors' contains 'gene_vars' which is not present in the dataset
```

------------------------------------------------------------------------

### 5. Stability Selection

Bootstrap-based stability selection identifies variables consistently
selected across many random subsamples. This follows Meinshausen &
Buhlmann (2010) with a fixed lambda across all subsamples for proper
error control.

``` r

highdimcox(
  data = data_genomic,
  elapsedtime = "survival_months",
  outcome = "vital_status",
  outcomeLevel = "Dead",
  censorLevel = "Alive",
  predictors = gene_vars,
  regularization_method = "elastic_net",
  alpha_value = 0.5,
  cv_folds = 10,
  stability_selection = TRUE,
  subsampling_iterations = 100,
  subsampling_ratio = 0.5,
  stability_threshold = 0.8,
  show_variable_importance = TRUE,
  show_coefficients_table = TRUE
)
#> Error:
#> ! Argument 'predictors' contains 'gene_vars' which is not present in the dataset
```

Variables selected in \>80% of bootstrap iterations are considered
robustly important.

#### Custom Stability Parameters

``` r

# Lower threshold and different subsampling ratio
highdimcox(
  data = data_genomic,
  elapsedtime = "survival_months",
  outcome = "vital_status",
  outcomeLevel = "Dead",
  censorLevel = "Alive",
  predictors = gene_vars,
  regularization_method = "lasso",
  cv_folds = 10,
  stability_selection = TRUE,
  subsampling_iterations = 200,
  subsampling_ratio = 0.6,
  stability_threshold = 0.6,
  show_coefficients_table = TRUE
)
#> Error:
#> ! Argument 'predictors' contains 'gene_vars' which is not present in the dataset
```

------------------------------------------------------------------------

### 6. CV Method Comparison

The choice of CV lambda selection affects model complexity.

``` r

# Minimum CV error - more variables, better fit but risk of overfitting
highdimcox(
  data = data_genomic,
  elapsedtime = "survival_months",
  outcome = "vital_status",
  outcomeLevel = "Dead",
  censorLevel = "Alive",
  predictors = gene_vars,
  regularization_method = "lasso",
  cv_method = "cv_min",
  cv_folds = 10,
  show_coefficients_table = TRUE,
  show_cv_plot = TRUE
)
#> Error:
#> ! Argument 'predictors' contains 'gene_vars' which is not present in the dataset
```

``` r

# 1-SE rule - more parsimonious, better generalization
highdimcox(
  data = data_genomic,
  elapsedtime = "survival_months",
  outcome = "vital_status",
  outcomeLevel = "Dead",
  censorLevel = "Alive",
  predictors = gene_vars,
  regularization_method = "lasso",
  cv_method = "cv_1se",
  cv_folds = 10,
  show_coefficients_table = TRUE,
  show_cv_plot = TRUE
)
#> Error:
#> ! Argument 'predictors' contains 'gene_vars' which is not present in the dataset
```

------------------------------------------------------------------------

### 7. Data Suitability Assessment

The suitability check runs 6 diagnostics with traffic-light indicators:

1.  **Events-Per-Variable (EPV)** - green \>=10, yellow \>=1, red \<1
2.  **Regularization Need** - green if p \>= n/3
3.  **Sample Size** - green \>=100, yellow \>=30, red \<30
4.  **Event Rate** - green 20 - 80%, yellow otherwise
5.  **Multicollinearity** - max pairwise \|r\| (skipped if p \> 2000)
6.  **Data Quality** - missing data and constant predictors

``` r

highdimcox(
  data = data_genomic,
  elapsedtime = "survival_months",
  outcome = "vital_status",
  outcomeLevel = "Dead",
  censorLevel = "Alive",
  predictors = gene_vars,
  suitabilityCheck = TRUE,
  show_coefficients_table = TRUE
)
#> Error:
#> ! Argument 'predictors' contains 'gene_vars' which is not present in the dataset
```

``` r

# Disable suitability assessment
highdimcox(
  data = data_genomic,
  elapsedtime = "survival_months",
  outcome = "vital_status",
  outcomeLevel = "Dead",
  censorLevel = "Alive",
  predictors = gene_vars,
  suitabilityCheck = FALSE,
  show_coefficients_table = TRUE
)
#> Error:
#> ! Argument 'predictors' contains 'gene_vars' which is not present in the dataset
```

------------------------------------------------------------------------

### 8. Smaller Dataset (Proteomic)

``` r

# 80 patients, 50 protein markers
set.seed(123)
n2 <- 80
p_prot <- 50

prot_matrix <- matrix(rnorm(n2 * p_prot, mean = 10, sd = 3), nrow = n2)
colnames(prot_matrix) <- paste0("PROT_", sprintf("%02d", 1:p_prot))

# 4 proteins with true effects
true_prot <- rep(0, p_prot)
true_prot[c(3, 17, 28, 42)] <- c(0.15, -0.12, 0.18, -0.10)

lp2 <- prot_matrix %*% true_prot
surv_time2 <- rweibull(n2, shape = 1.1, scale = 24 * exp(-as.numeric(lp2) * 0.5))
censor_time2 <- runif(n2, 6, 36)
time2 <- pmax(pmin(surv_time2, censor_time2), 0.1)
event2 <- factor(ifelse(surv_time2 <= censor_time2, "Dead", "Alive"),
  levels = c("Alive", "Dead")
)

data_proteomic <- data.frame(
  time = time2, status = event2,
  age = rnorm(n2, 55, 12),
  sex = factor(sample(c("M", "F"), n2, replace = TRUE)),
  tumor_size_cm = rnorm(n2, 3.5, 1.2),
  as.data.frame(prot_matrix)
)

cat(
  "N:", n2, "Events:", sum(event2 == "Dead"),
  "Event rate:", round(mean(event2 == "Dead"), 2), "\n"
)
#> N: 80 Events: 52 Event rate: 0.65
```

``` r

prot_vars <- c(
  "age", "sex", "tumor_size_cm",
  paste0("PROT_", sprintf("%02d", 1:p_prot))
)

highdimcox(
  data = data_proteomic,
  elapsedtime = "time",
  outcome = "status",
  outcomeLevel = "Dead",
  censorLevel = "Alive",
  predictors = prot_vars,
  regularization_method = "elastic_net",
  alpha_value = 0.5,
  cv_method = "cv_1se",
  cv_folds = 5,
  show_regularization_path = TRUE,
  show_cv_plot = TRUE,
  show_variable_importance = TRUE,
  show_coefficients_table = TRUE,
  showExplanations = TRUE
)
#> Error:
#> ! Argument 'predictors' contains 'prot_vars' which is not present in the dataset
```

------------------------------------------------------------------------

### 9. Mixed Predictors (Clinical + Genomic)

Demonstrates automatic factor dummy-encoding for mixed predictor types.

``` r

mixed_vars <- c(
  "age", "gender", "stage", "grade", "treatment",
  paste0("GENE_", sprintf("%03d", 1:20))
)

highdimcox(
  data = data_genomic,
  elapsedtime = "survival_months",
  outcome = "vital_status",
  outcomeLevel = "Dead",
  censorLevel = "Alive",
  predictors = mixed_vars,
  regularization_method = "elastic_net",
  alpha_value = 0.5,
  cv_folds = 10,
  show_coefficients_table = TRUE,
  show_variable_importance = TRUE
)
#> Error:
#> ! Argument 'predictors' contains 'mixed_vars' which is not present in the dataset
```

Factor variables like `stage` and `gender` are automatically converted
to dummy variables (e.g., “stage: II”, “stage: III”) in the output.

------------------------------------------------------------------------

### 10. Explanatory Output

``` r

highdimcox(
  data = data_genomic,
  elapsedtime = "survival_months",
  outcome = "vital_status",
  outcomeLevel = "Dead",
  censorLevel = "Alive",
  predictors = gene_vars[1:20],
  regularization_method = "elastic_net",
  cv_folds = 10,
  show_coefficients_table = TRUE,
  showSummaries = TRUE,
  showExplanations = TRUE
)
#> 
#>  HIGH-DIMENSIONAL COX REGRESSION
#> 
#>  <div class='alert alert-warning'>
#> 
#>  Analysis Notes
#> 
#>  Starting in glmnet 5.1, the default Cox tie-handling method will
#>  change from 'breslow' to 'efron' (matching survival::coxph). To
#>  silence this message and lock in the v5.0 default, pass cox.ties =
#>  'breslow' explicitly. To preview the v5.1 behavior, pass cox.ties =
#>  'efron'.Starting in glmnet 5.1, the default Cox tie-handling method
#>  will change from 'breslow' to 'efron' (matching survival::coxph). To
#>  silence this message and lock in the v5.0 default, pass cox.ties =
#>  'breslow' explicitly. To preview the v5.1 behavior, pass cox.ties =
#>  'efron'.Starting in glmnet 5.1, the default Cox tie-handling method
#>  will change from 'breslow' to 'efron' (matching survival::coxph). To
#>  silence this message and lock in the v5.0 default, pass cox.ties =
#>  'breslow' explicitly. To preview the v5.1 behavior, pass cox.ties =
#>  'efron'.Starting in glmnet 5.1, the default Cox tie-handling method
#>  will change from 'breslow' to 'efron' (matching survival::coxph). To
#>  silence this message and lock in the v5.0 default, pass cox.ties =
#>  'breslow' explicitly. To preview the v5.1 behavior, pass cox.ties =
#>  'efron'.Starting in glmnet 5.1, the default Cox tie-handling method
#>  will change from 'breslow' to 'efron' (matching survival::coxph). To
#>  silence this message and lock in the v5.0 default, pass cox.ties =
#>  'breslow' explicitly. To preview the v5.1 behavior, pass cox.ties =
#>  'efron'.Starting in glmnet 5.1, the default Cox tie-handling method
#>  will change from 'breslow' to 'efron' (matching survival::coxph). To
#>  silence this message and lock in the v5.0 default, pass cox.ties =
#>  'breslow' explicitly. To preview the v5.1 behavior, pass cox.ties =
#>  'efron'.Starting in glmnet 5.1, the default Cox tie-handling method
#>  will change from 'breslow' to 'efron' (matching survival::coxph). To
#>  silence this message and lock in the v5.0 default, pass cox.ties =
#>  'breslow' explicitly. To preview the v5.1 behavior, pass cox.ties =
#>  'efron'.Starting in glmnet 5.1, the default Cox tie-handling method
#>  will change from 'breslow' to 'efron' (matching survival::coxph). To
#>  silence this message and lock in the v5.0 default, pass cox.ties =
#>  'breslow' explicitly. To preview the v5.1 behavior, pass cox.ties =
#>  'efron'.Starting in glmnet 5.1, the default Cox tie-handling method
#>  will change from 'breslow' to 'efron' (matching survival::coxph). To
#>  silence this message and lock in the v5.0 default, pass cox.ties =
#>  'breslow' explicitly. To preview the v5.1 behavior, pass cox.ties =
#>  'efron'.Starting in glmnet 5.1, the default Cox tie-handling method
#>  will change from 'breslow' to 'efron' (matching survival::coxph). To
#>  silence this message and lock in the v5.0 default, pass cox.ties =
#>  'breslow' explicitly. To preview the v5.1 behavior, pass cox.ties =
#>  'efron'.Starting in glmnet 5.1, the default Cox tie-handling method
#>  will change from 'breslow' to 'efron' (matching survival::coxph). To
#>  silence this message and lock in the v5.0 default, pass cox.ties =
#>  'breslow' explicitly. To preview the v5.1 behavior, pass cox.ties =
#>  'efron'.Starting in glmnet 5.1, the default Cox tie-handling method
#>  will change from 'breslow' to 'efron' (matching survival::coxph). To
#>  silence this message and lock in the v5.0 default, pass cox.ties =
#>  'breslow' explicitly. To preview the v5.1 behavior, pass cox.ties =
#>  'efron'.<div class='alert alert-success'>
#> 
#>  Analysis completed: 150 observations, 90 events, 20 predictors, 0
#>  selected via elastic_net (C-index=0.500).
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
#>  6px;'>4.5 (n_events=90, p=20)<td style='padding: 6px;'>Adequate for
#>  regularized regression, which handles low EPV better than standard
#>  Cox.<tr style='border-bottom: 1px solid #dee2e6;'><td style='padding:
#>  6px;'><span style='color: #ffc107; font-size: 18px;'>&#9679;<td
#>  style='padding: 6px;'>Regularization Need<td style='padding:
#>  6px;'>p=20, EPV=4<td style='padding: 6px;'>Moderate/low
#>  dimensionality. Standard Cox may also suffice.<tr
#>  style='border-bottom: 1px solid #dee2e6;'><td style='padding:
#>  6px;'><span style='color: #28a745; font-size: 18px;'>&#9679;<td
#>  style='padding: 6px;'>Sample Size<td style='padding: 6px;'>n=150<td
#>  style='padding: 6px;'>Adequate sample size for penalized
#>  regression.<tr style='border-bottom: 1px solid #dee2e6;'><td
#>  style='padding: 6px;'><span style='color: #28a745; font-size:
#>  18px;'>&#9679;<td style='padding: 6px;'>Event Rate<td style='padding:
#>  6px;'>60.0% (90/150)<td style='padding: 6px;'>Balanced event rate.
#>  Good for model estimation.<tr style='border-bottom: 1px solid
#>  #dee2e6;'><td style='padding: 6px;'><span style='color: #28a745;
#>  font-size: 18px;'>&#9679;<td style='padding:
#>  6px;'>Multicollinearity<td style='padding: 6px;'>Max |r| = 0.26<td
#>  style='padding: 6px;'>No concerning collinearity detected.<tr
#>  style='border-bottom: 1px solid #dee2e6;'><td style='padding:
#>  6px;'><span style='color: #28a745; font-size: 18px;'>&#9679;<td
#>  style='padding: 6px;'>Data Quality<td style='padding: 6px;'>No
#>  issues<td style='padding: 6px;'>Complete data with no constant
#>  predictors.
#> 
#>  High-Dimensional Cox Regression Results
#> 
#>  Regularization: elastic_net (α = 0.5)
#> 
#>  Selected Lambda: 4.13e-01
#> 
#>  Variables: 20 candidate variables → 0 selected
#> 
#>  Cross-Validation: 10-fold CV
#> 
#>  Training C-index (optimistic): 0.5
#> 
#>  Selected Variables                    
#>  ───────────────────────────────────── 
#>    Variable    β    HR    Importance   
#>  ───────────────────────────────────── 
#>  ───────────────────────────────────── 
#>    Note. No variables were
#>    selected at the chosen
#>    regularization level. Consider
#>    using a less restrictive lambda
#>    (minimum CV) or a different
#>    regularization method.
#> 
#> 
#>  Regularization Metrics                                                                                                                             
#>  ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Metric                            Value                  Interpretation                                                                          
#>  ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Selected Lambda                   4.13e-01               Optimal regularization strength                                                         
#>    Lambda Min                        1.63e-01               Lambda minimizing CV error                                                              
#>    Lambda 1SE                        4.13e-01               Lambda within 1-SE of minimum                                                           
#>    CV Deviance at Selected Lambda    6.336                  Cross-validated model deviance                                                          
#>    Training C-index (optimistic)     0.5                    Training-set estimate; likely overestimates true discrimination. Validate externally.   
#>    Number of Selected Variables      0                      Variables with non-zero coefficients                                                    
#>    Regularization Method             elastic_net (α=0.5)    Applied regularization strategy                                                         
#>  ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Analysis Summary
#> 
#>  High-dimensional Cox regression analysis was performed on 20 predictor
#>  variables using elastic_net regularization.
#> 
#>  Model Selection: Cross-validation with 10 folds identified an optimal
#>  regularization parameter (λ = 4.13e-01) that selected 0 variables from
#>  the candidate set.
#> 
#>  Training C-index (optimistic): 0.5
#> 
#>  Interpretation: The selected variables represent the most predictive
#>  features for survival outcome after accounting for multiple testing
#>  and overfitting through regularization. Variables with larger absolute
#>  coefficients have stronger associations with survival risk.
#> 
#>  High-Dimensional Cox Regression Methodology
#> 
#>  Overview
#> 
#>  High-dimensional Cox regression extends traditional Cox proportional
#>  hazards modeling to handle datasets where the number of predictors (p)
#>  may exceed or approach the number of observations (n). This scenario
#>  is common in genomic, proteomic, and other high-throughput biomedical
#>  research contexts.
#> 
#>  Regularization Methods
#> 
#>  LASSO (L1): Performs automatic variable selection by shrinking some
#>  coefficients to exactly zeroRidge (L2): Shrinks coefficients toward
#>  zero but retains all variables, useful when predictors are
#>  correlatedElastic Net: Combines L1 and L2 penalties, balancing
#>  variable selection and coefficient shrinkageAdaptive LASSO: Uses
#>  data-driven penalty weights for improved variable selection properties
#> 
#>  Cross-Validation
#> 
#>  The regularization parameter (λ) is selected using cross-validation to
#>  optimize prediction performance. The '1-SE rule' selects a more
#>  parsimonious model by choosing the largest λ within one standard error
#>  of the minimum cross-validation error.
#> 
#>  Stability Selection
#> 
#>  When enabled, stability selection performs variable selection across
#>  multiple bootstrap samples to identify variables that are consistently
#>  selected. This provides a measure of selection confidence and helps
#>  identify the most robust predictive features.
#> 
#>  Clinical Interpretation
#> 
#>  Selected variables and their coefficients can be used to:
#> 
#>  Identify key biomarkers associated with survivalDevelop prognostic
#>  signatures for risk stratificationGuide hypothesis generation for
#>  follow-up studiesBuild personalized survival prediction models
```

------------------------------------------------------------------------

### 11. Full-Feature Demonstration

All options enabled simultaneously.

``` r

highdimcox(
  data = data_proteomic,
  elapsedtime = "time",
  outcome = "status",
  outcomeLevel = "Dead",
  censorLevel = "Alive",
  predictors = prot_vars,
  regularization_method = "elastic_net",
  alpha_value = 0.7,
  cv_method = "cv_min",
  cv_folds = 5,
  stability_selection = TRUE,
  subsampling_iterations = 100,
  subsampling_ratio = 0.5,
  stability_threshold = 0.7,
  suitabilityCheck = TRUE,
  show_regularization_path = TRUE,
  show_cv_plot = TRUE,
  show_variable_importance = TRUE,
  show_coefficients_table = TRUE,
  show_model_diagnostics = TRUE,
  showSummaries = TRUE,
  showExplanations = TRUE
)
#> Error:
#> ! Argument 'predictors' contains 'prot_vars' which is not present in the dataset
```

------------------------------------------------------------------------

### 12. Edge Cases

#### Small sample (near minimum)

``` r

small_data <- data_proteomic[1:35, ]

highdimcox(
  data = small_data,
  elapsedtime = "time",
  outcome = "status",
  outcomeLevel = "Dead",
  censorLevel = "Alive",
  predictors = paste0("PROT_", sprintf("%02d", 1:10)),
  regularization_method = "elastic_net",
  cv_folds = 5,
  show_coefficients_table = TRUE,
  suitabilityCheck = TRUE
)
#> 
#>  HIGH-DIMENSIONAL COX REGRESSION
#> 
#>  <div class='alert alert-warning'>
#> 
#>  Analysis Notes
#> 
#>  Starting in glmnet 5.1, the default Cox tie-handling method will
#>  change from 'breslow' to 'efron' (matching survival::coxph). To
#>  silence this message and lock in the v5.0 default, pass cox.ties =
#>  'breslow' explicitly. To preview the v5.1 behavior, pass cox.ties =
#>  'efron'.Starting in glmnet 5.1, the default Cox tie-handling method
#>  will change from 'breslow' to 'efron' (matching survival::coxph). To
#>  silence this message and lock in the v5.0 default, pass cox.ties =
#>  'breslow' explicitly. To preview the v5.1 behavior, pass cox.ties =
#>  'efron'.Starting in glmnet 5.1, the default Cox tie-handling method
#>  will change from 'breslow' to 'efron' (matching survival::coxph). To
#>  silence this message and lock in the v5.0 default, pass cox.ties =
#>  'breslow' explicitly. To preview the v5.1 behavior, pass cox.ties =
#>  'efron'.Starting in glmnet 5.1, the default Cox tie-handling method
#>  will change from 'breslow' to 'efron' (matching survival::coxph). To
#>  silence this message and lock in the v5.0 default, pass cox.ties =
#>  'breslow' explicitly. To preview the v5.1 behavior, pass cox.ties =
#>  'efron'.Starting in glmnet 5.1, the default Cox tie-handling method
#>  will change from 'breslow' to 'efron' (matching survival::coxph). To
#>  silence this message and lock in the v5.0 default, pass cox.ties =
#>  'breslow' explicitly. To preview the v5.1 behavior, pass cox.ties =
#>  'efron'.Starting in glmnet 5.1, the default Cox tie-handling method
#>  will change from 'breslow' to 'efron' (matching survival::coxph). To
#>  silence this message and lock in the v5.0 default, pass cox.ties =
#>  'breslow' explicitly. To preview the v5.1 behavior, pass cox.ties =
#>  'efron'.Starting in glmnet 5.1, the default Cox tie-handling method
#>  will change from 'breslow' to 'efron' (matching survival::coxph). To
#>  silence this message and lock in the v5.0 default, pass cox.ties =
#>  'breslow' explicitly. To preview the v5.1 behavior, pass cox.ties =
#>  'efron'.<div class='alert alert-success'>
#> 
#>  Analysis completed: 35 observations, 25 events, 10 predictors, 0
#>  selected via elastic_net (C-index=0.500).
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
#>  6px;'>2.5 (n_events=25, p=10)<td style='padding: 6px;'>Adequate for
#>  regularized regression, which handles low EPV better than standard
#>  Cox.<tr style='border-bottom: 1px solid #dee2e6;'><td style='padding:
#>  6px;'><span style='color: #ffc107; font-size: 18px;'>&#9679;<td
#>  style='padding: 6px;'>Regularization Need<td style='padding:
#>  6px;'>p=10, EPV=2<td style='padding: 6px;'>Moderate/low
#>  dimensionality. Standard Cox may also suffice.<tr
#>  style='border-bottom: 1px solid #dee2e6;'><td style='padding:
#>  6px;'><span style='color: #ffc107; font-size: 18px;'>&#9679;<td
#>  style='padding: 6px;'>Sample Size<td style='padding: 6px;'>n=35<td
#>  style='padding: 6px;'>Small sample. CV folds may be somewhat
#>  unstable.<tr style='border-bottom: 1px solid #dee2e6;'><td
#>  style='padding: 6px;'><span style='color: #28a745; font-size:
#>  18px;'>&#9679;<td style='padding: 6px;'>Event Rate<td style='padding:
#>  6px;'>71.4% (25/35)<td style='padding: 6px;'>Balanced event rate. Good
#>  for model estimation.<tr style='border-bottom: 1px solid #dee2e6;'><td
#>  style='padding: 6px;'><span style='color: #28a745; font-size:
#>  18px;'>&#9679;<td style='padding: 6px;'>Multicollinearity<td
#>  style='padding: 6px;'>Max |r| = 0.39<td style='padding: 6px;'>No
#>  concerning collinearity detected.<tr style='border-bottom: 1px solid
#>  #dee2e6;'><td style='padding: 6px;'><span style='color: #28a745;
#>  font-size: 18px;'>&#9679;<td style='padding: 6px;'>Data Quality<td
#>  style='padding: 6px;'>No issues<td style='padding: 6px;'>Complete data
#>  with no constant predictors.
#> 
#>  High-Dimensional Cox Regression Results
#> 
#>  Regularization: elastic_net (α = 0.5)
#> 
#>  Selected Lambda: 5.05e-01
#> 
#>  Variables: 10 candidate variables → 0 selected
#> 
#>  Cross-Validation: 5-fold CV
#> 
#>  Training C-index (optimistic): 0.5
#> 
#>  Selected Variables                    
#>  ───────────────────────────────────── 
#>    Variable    β    HR    Importance   
#>  ───────────────────────────────────── 
#>  ───────────────────────────────────── 
#>    Note. No variables were
#>    selected at the chosen
#>    regularization level. Consider
#>    using a less restrictive lambda
#>    (minimum CV) or a different
#>    regularization method.
#> 
#> 
#>  Regularization Metrics                                                                                                                             
#>  ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Metric                            Value                  Interpretation                                                                          
#>  ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Selected Lambda                   5.05e-01               Optimal regularization strength                                                         
#>    Lambda Min                        5.05e-01               Lambda minimizing CV error                                                              
#>    Lambda 1SE                        5.05e-01               Lambda within 1-SE of minimum                                                           
#>    CV Deviance at Selected Lambda    5.159                  Cross-validated model deviance                                                          
#>    Training C-index (optimistic)     0.5                    Training-set estimate; likely overestimates true discrimination. Validate externally.   
#>    Number of Selected Variables      0                      Variables with non-zero coefficients                                                    
#>    Regularization Method             elastic_net (α=0.5)    Applied regularization strategy                                                         
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
```

#### Below minimum observations

``` r

tiny_data <- data_proteomic[1:20, ]

highdimcox(
  data = tiny_data,
  elapsedtime = "time",
  outcome = "status",
  outcomeLevel = "Dead",
  censorLevel = "Alive",
  predictors = paste0("PROT_", sprintf("%02d", 1:5)),
  show_coefficients_table = TRUE
)
#> 
#>  HIGH-DIMENSIONAL COX REGRESSION
#> 
#>  <div class='alert alert-danger'>
#> 
#>  Validation Error
#> 
#>  At least 30 observations required for high-dimensional analysis (found
#>  20).
#> 
#> character(0)
#> 
#> character(0)
#> 
#>  Selected Variables                    
#>  ───────────────────────────────────── 
#>    Variable    β    HR    Importance   
#>  ───────────────────────────────────── 
#>  ───────────────────────────────────── 
#> 
#> 
#>  Regularization Metrics                
#>  ───────────────────────────────────── 
#>    Metric    Value    Interpretation   
#>  ───────────────────────────────────── 
#>  ─────────────────────────────────────
```

#### All censored (no events)

``` r

no_events <- data_genomic
no_events$vital_status <- factor("Alive", levels = c("Alive", "Dead"))

highdimcox(
  data = no_events,
  elapsedtime = "survival_months",
  outcome = "vital_status",
  outcomeLevel = "Dead",
  censorLevel = "Alive",
  predictors = gene_vars[1:20],
  show_coefficients_table = TRUE
)
#> 
#>  HIGH-DIMENSIONAL COX REGRESSION
#> 
#>  <div class='alert alert-danger'>
#> 
#>  Validation Error
#> 
#>  No rows match event level 'Dead' in the outcome variable.
#> 
#> character(0)
#> 
#> character(0)
#> 
#>  Selected Variables                    
#>  ───────────────────────────────────── 
#>    Variable    β    HR    Importance   
#>  ───────────────────────────────────── 
#>  ───────────────────────────────────── 
#> 
#> 
#>  Regularization Metrics                
#>  ───────────────────────────────────── 
#>    Metric    Value    Interpretation   
#>  ───────────────────────────────────── 
#>  ─────────────────────────────────────
```

------------------------------------------------------------------------

### Interpreting Results

#### Coefficient Table

| Column | Meaning |
|----|----|
| Variable | Predictor name (or “Variable: Level” for factor dummies) |
| Coefficient (β) | Regularized log hazard ratio |
| Hazard Ratio (HR) | exp(coefficient) - HR \> 1 = increased risk |
| Importance | Absolute coefficient value |

#### Variable Importance Plot

Variables ranked by absolute regularized coefficient. Top 25 shown.
Selected variables (non-zero coefficients) are highlighted.

#### Regularization Path

Shows how each variable’s coefficient changes as lambda increases.
Variables that persist at higher lambda values are more robust
predictors.

#### Cross-Validation Plot

Shows partial likelihood deviance vs log(lambda) with confidence bands.
Vertical lines mark lambda.min (red) and lambda.1se (green).

#### Stability Selection

Variables with selection probability \>= threshold are marked as
“stable”. Higher selection frequency = more reliable predictor.

------------------------------------------------------------------------

### Method Selection Guide

| Your Data                            | Recommended Method      | Alpha   |
|--------------------------------------|-------------------------|---------|
| p \>\> n, expect few true predictors | LASSO                   | 1.0     |
| p \>\> n, many small effects         | Ridge                   | 0.0     |
| p \>\> n, correlated groups          | Elastic Net             | 0.3-0.7 |
| p \> n, want publishable model       | Adaptive LASSO          | 1.0     |
| Moderate p, clinical study           | Elastic Net             | 0.5     |
| Prediction is primary goal           | Ridge or Elastic Net    | 0.0-0.5 |
| Interpretation is primary goal       | LASSO or Adaptive LASSO | 0.8-1.0 |

------------------------------------------------------------------------

### Common Pitfalls

1.  **Using Ridge when you need variable selection**: Ridge never sets
    coefficients to exactly zero. For interpretable models, use LASSO or
    Elastic Net.

2.  **Not adjusting CV folds for small samples**: With n \< 100, use
    cv_folds = 5 instead of 10 to ensure adequate events per fold.

3.  **Ignoring stability selection**: A single regularized model may
    give unstable variable selections. Use stability selection
    (subsampling_iterations \>= 200) for robust results.

4.  **Comparing models with different alpha values**: Models with
    different alpha values optimize different objectives. Compare using
    the same alpha or use nested cross-validation.

5.  **Not reporting the regularization method**: Always report which
    method (LASSO/Ridge/Elastic Net), alpha value, CV method, and number
    of selected variables.

6.  **Training C-index overestimates performance**: The reported C-index
    is computed on the training data and is optimistically biased.
    Always validate on external data before clinical use.

------------------------------------------------------------------------

### Related ClinicoPath Functions

| Function | Use When |
|----|----|
| **LASSO Cox** (`lassocox`) | Pure LASSO with suitability assessment and clinical guidance |
| **Adaptive LASSO** (`adaptivelasso`) | Dedicated adaptive LASSO with more stability options |
| **SCAD Cox** (`ncvregcox`) | Non-convex penalties (SCAD, MCP) avoiding LASSO bias |
| **PLS Cox** (`plscox`) | Dimensionality reduction approach for very high p |
| **Multivariable Survival** (`survival`) | Standard Cox when p is small |

------------------------------------------------------------------------

### References

- Zou H, Hastie T. Regularization and variable selection via the elastic
  net. *J R Stat Soc Ser B*. 2005;67(2):301-320.
- Simon N, et al. Regularization paths for Cox’s proportional hazards
  model via coordinate descent. *J Stat Softw*. 2011;39(5):1-13.
- Meinshausen N, Buhlmann P. Stability selection. *J R Stat Soc Ser B*.
  2010;72(4):417-473.
- Tibshirani R. Regression shrinkage and selection via the lasso. *J R
  Stat Soc Ser B*. 1996;58(1):267-288.
- Zou H. The adaptive lasso and its oracle properties. *J Am Stat
  Assoc*. 2006;101(476):1418-1429.
