# Firth's Penalized Likelihood Regression - Comprehensive Guide

> **Note:** The
> [`firthregression()`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/firthregression.md)
> function is designed for use within **jamovi’s GUI**. The code
> examples below show the R syntax for reference.

## Firth’s Penalized Likelihood Regression

### Overview

Firth’s penalized likelihood regression adds a Jeffreys-prior penalty to
the likelihood function, producing finite and bias-corrected coefficient
estimates even when standard maximum likelihood fails. This is essential
for clinical studies with small samples, rare events, or
complete/quasi-complete separation - situations where standard logistic
or Cox regression produces infinite or severely biased estimates.

Unlike LASSO (which selects variables by shrinking some to zero), Firth
regression keeps all variables but corrects first-order bias. It
provides profile likelihood confidence intervals (more accurate than
Wald for small samples), penalized likelihood ratio test p-values, and
automatic separation detection.

The module supports two modes: **Firth logistic regression** (binary
outcomes via the `logistf` package) and **Firth Cox regression**
(survival outcomes via the `coxphf` package). Both are reference
implementations by Georg Heinze.

------------------------------------------------------------------------

### Datasets Used in This Guide

| Dataset | N | Events | Primary Use |
|----|----|----|----|
| `firth_standard` | 120 | 58 (logistic) / 31 (Cox) | Balanced clinical data, both modes |
| `firth_separation` | 80 | 11 (~14%) | Rare events with complete separation |
| `firth_smallcox` | 50 | 23 | Small survival cohort |

------------------------------------------------------------------------

### 1. Firth Logistic Regression

#### Basic logistic analysis

``` r

standard <- read.csv(paste0(data_path, "firth_standard.csv"))
#> Error in `file()`:
#> ! cannot open the connection
str(standard[, c("mortality", "age", "grade", "tumor_size", "lvi", "marker")])
#> Error:
#> ! object 'standard' not found

firthregression(
  data = standard,
  analysisType = "logistic",
  outcome = "mortality",
  outcomeLevel = "Dead",
  predictors = c("age", "grade", "tumor_size", "lvi", "marker"),
  suitabilityCheck = TRUE,
  separationCheck = TRUE,
  compareStandard = TRUE,
  showModelFit = TRUE
)
#> Error in `firthregression()`:
#> ! argument "time" is missing, with no default
```

Look for: coefficients with OR, profile likelihood CIs, p-values from
penalized LR tests, bias reduction %, model fit statistics, separation
diagnostics.

------------------------------------------------------------------------

### 2. Firth Cox Regression

#### Survival analysis mode

``` r

firthregression(
  data = standard,
  analysisType = "cox",
  time = "follow_up_time",
  outcome = "status",
  outcomeLevel = "Dead",
  predictors = c("age", "grade", "tumor_size", "lvi", "marker"),
  suitabilityCheck = TRUE,
  compareStandard = TRUE
)
#> Error:
#> ! object 'standard' not found
```

Look for: coefficients with HR, profile CIs (always used for Cox via
coxphf), comparison with standard Cox model.

------------------------------------------------------------------------

### 3. Confidence Intervals

#### Profile vs Wald CIs

Profile likelihood CIs are recommended (default) as they are more
accurate for small samples. Wald CIs are faster but less reliable near
separation.

``` r

firthregression(
  data = standard,
  analysisType = "logistic",
  outcome = "mortality",
  outcomeLevel = "Dead",
  predictors = c("age", "grade", "tumor_size"),
  ciLevel = 0.95,
  ciMethod = "profile",
  suitabilityCheck = FALSE
)
#> Error in `firthregression()`:
#> ! argument "time" is missing, with no default
```

``` r

firthregression(
  data = standard,
  analysisType = "logistic",
  outcome = "mortality",
  outcomeLevel = "Dead",
  predictors = c("age", "grade", "tumor_size"),
  ciLevel = 0.99,
  ciMethod = "wald",
  suitabilityCheck = FALSE
)
#> Error in `firthregression()`:
#> ! argument "time" is missing, with no default
```

Compare the CI widths: profile CIs are typically asymmetric (reflecting
the actual likelihood shape), while Wald CIs are symmetric.

------------------------------------------------------------------------

### 4. Separation Detection

#### Data with complete separation

The `firth_separation` dataset includes a variable (`margin_positive`)
that perfectly predicts the outcome - all positive-margin patients had
recurrence. Standard logistic regression would produce infinite
coefficients.

``` r

separation <- read.csv(paste0(data_path, "firth_separation.csv"))
#> Error in `file()`:
#> ! cannot open the connection
table(separation$outcome, separation$margin_positive)
#> Error:
#> ! object 'separation' not found

firthregression(
  data = separation,
  analysisType = "logistic",
  outcome = "outcome",
  outcomeLevel = "Recurrence",
  predictors = c("age", "bmi", "grade", "margin_positive"),
  separationCheck = TRUE,
  compareStandard = TRUE,
  suitabilityCheck = TRUE
)
#> Error in `firthregression()`:
#> ! argument "time" is missing, with no default
```

Look for: separation diagnostics table flagging `margin_positive`,
comparison table showing how standard GLM produces extreme estimates
while Firth gives finite values, bias reduction %.

------------------------------------------------------------------------

### 5. Bias Reduction Comparison

#### Firth vs Standard model side-by-side

``` r

firthregression(
  data = standard,
  analysisType = "logistic",
  outcome = "mortality",
  outcomeLevel = "Dead",
  predictors = c("age", "grade", "tumor_size", "lvi", "marker"),
  compareStandard = TRUE,
  showModelFit = TRUE,
  suitabilityCheck = FALSE
)
#> Error in `firthregression()`:
#> ! argument "time" is missing, with no default
```

Look for: comparison table with Firth coefficient, Standard coefficient,
Firth OR, Standard OR, and p-values from both models. The bias_reduction
column in the coefficients table shows % change.

------------------------------------------------------------------------

### 6. Plots

#### Forest plot

``` r

firthregression(
  data = standard,
  analysisType = "logistic",
  outcome = "mortality",
  outcomeLevel = "Dead",
  predictors = c("age", "grade", "tumor_size", "lvi", "marker"),
  forestPlot = TRUE,
  suitabilityCheck = FALSE
)
#> Error in `firthregression()`:
#> ! argument "time" is missing, with no default
```

#### Separation diagnostic plot

``` r

firthregression(
  data = separation,
  analysisType = "logistic",
  outcome = "outcome",
  outcomeLevel = "Recurrence",
  predictors = c("age", "grade", "margin_positive"),
  separationPlot = TRUE,
  suitabilityCheck = FALSE
)
#> Error in `firthregression()`:
#> ! argument "time" is missing, with no default
```

------------------------------------------------------------------------

### 7. Clinical Output

#### Results summary and explanations

``` r

firthregression(
  data = standard,
  analysisType = "logistic",
  outcome = "mortality",
  outcomeLevel = "Dead",
  predictors = c("age", "grade", "tumor_size"),
  showSummary = TRUE,
  showExplanations = TRUE,
  suitabilityCheck = FALSE
)
#> Error in `firthregression()`:
#> ! argument "time" is missing, with no default
```

Look for: natural-language results summary suitable for reports, and
methodological explanations about when and why to use Firth’s method.

------------------------------------------------------------------------

### 8. Small Cox Cohort

``` r

smallcox <- read.csv(paste0(data_path, "firth_smallcox.csv"))
#> Error in `file()`:
#> ! cannot open the connection
table(smallcox$status)
#> Error:
#> ! object 'smallcox' not found

firthregression(
  data = smallcox,
  analysisType = "cox",
  time = "time",
  outcome = "status",
  outcomeLevel = "Dead",
  predictors = c("age", "treatment", "biomarker"),
  suitabilityCheck = TRUE,
  compareStandard = TRUE,
  showSummary = TRUE
)
#> Error:
#> ! object 'smallcox' not found
```

------------------------------------------------------------------------

### References

- Firth D (1993). “Bias reduction of maximum likelihood estimates.”
  *Biometrika*, 80(1), 27-38.
- Heinze G, Schemper M (2001). “A solution to the problem of separation
  in logistic regression.” *Statistics in Medicine*, 20(2), 169-177.
- Heinze G, Ploner M (2023). *logistf: Firth’s Bias-Reduced Logistic
  Regression*. R package.
- Heinze G, Ploner M, Beyea J (2023). *coxphf: Cox Regression with
  Firth’s Penalized Likelihood*. R package.
