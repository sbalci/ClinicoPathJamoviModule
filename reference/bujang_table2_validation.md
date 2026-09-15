# Bujang 2023 Table 2 Validation Data

Validation dataset containing excerpts from Bujang MA (2023) Table 2 for
diagnostic test sample size calculations. Used to validate
Clopper-Pearson exact binomial confidence interval implementation.

## Usage

``` r
bujang_table2_validation
```

## Format

A data frame with 8 rows and 7 variables:

- prevalence:

  Disease prevalence (0.05 to 0.90)

- sensitivity:

  Target sensitivity value

- specificity:

  Target specificity value

- ci_width:

  Desired 95% CI width (0.10 or 0.20)

- n_sens_expected:

  Expected sample size for sensitivity (from Bujang Table 2)

- n_spec_expected:

  Expected sample size for specificity (from Bujang Table 2)

- n_total_expected:

  Expected total sample size (maximum of sens/spec)

## Source

Bujang MA (2023). An Elaboration on Sample Size Planning for Performing
a One-Sample Sensitivity and Specificity Analysis by Basing on
Calculations on a Specified 95% Confidence Interval Width. Diagnostics
13(8):1390. Table 2 (pages 5-6).
[doi:10.3390/diagnostics13081390](https://doi.org/10.3390/diagnostics13081390)

## Details

This dataset provides benchmark values for validating the
Clopper-Pearson sample size calculation implementation. Each row
represents a specific combination of prevalence, target
sensitivity/specificity, and desired confidence interval width, with the
expected sample sizes as published in Bujang (2023).

**Validation Test Cases:**

1.  Prevalence 5%, Sens/Spec 95%, CI width 0.10 -\> N = 940

2.  Prevalence 5%, Sens/Spec 70%, CI width 0.10 -\> N = 3,410

3.  Prevalence 10%, Sens/Spec 95%, CI width 0.10 -\> N = 940

4.  Prevalence 10%, Sens/Spec 90%, CI width 0.20 -\> N = 440

5.  Prevalence 20%, Sens/Spec 90%, CI width 0.20 -\> N = 220

6.  Prevalence 50%, Sens/Spec 90%, CI width 0.20 -\> N = 88

7.  Prevalence 50%, Sens/Spec 80%, CI width 0.20 -\> N = 140

8.  Prevalence 90%, Sens/Spec 95%, CI width 0.20 -\> N = 941

## Usage

These values can be used to programmatically validate that the
`diagnosticsamplesize` module produces results consistent with Bujang's
published tables.

## Examples

``` r
# Load validation data
data(bujang_table2_validation)

# View all test cases
print(bujang_table2_validation)
#>   prevalence sensitivity specificity ci_width n_sens_expected n_spec_expected
#> 1       0.05        0.95        0.95      0.1             940             105
#> 2       0.05        0.70        0.70      0.1            3410             379
#> 3       0.10        0.95        0.95      0.1             940             105
#> 4       0.10        0.90        0.90      0.2             440              49
#> 5       0.20        0.90        0.90      0.2             220              49
#> 6       0.50        0.90        0.90      0.2              88              88
#> 7       0.50        0.80        0.80      0.2             140             140
#> 8       0.90        0.95        0.95      0.2              49             941
#>   n_total_expected
#> 1              940
#> 2             3410
#> 3              940
#> 4              440
#> 5              220
#> 6               88
#> 7              140
#> 8              941

# Test case: Low prevalence (5 percent), high targets (95 percent), narrow CI (0.10)
test1 <- bujang_table2_validation[1, ]
cat("Prevalence:", test1$prevalence * 100, "percent\n")
#> Prevalence: 5 percent
cat("Targets: Sens =", test1$sensitivity, ", Spec =", test1$specificity, "\n")
#> Targets: Sens = 0.95 , Spec = 0.95 
cat("Expected total N:", test1$n_total_expected, "\n")
#> Expected total N: 940 

# Compare low vs. high prevalence scenarios
low_prev <- bujang_table2_validation[bujang_table2_validation$prevalence == 0.05, ]
high_prev <- bujang_table2_validation[bujang_table2_validation$prevalence == 0.90, ]

cat("\nLow prevalence (5 percent) scenarios:\n")
#> 
#> Low prevalence (5 percent) scenarios:
print(low_prev[, c("sensitivity", "specificity", "ci_width", "n_total_expected")])
#>   sensitivity specificity ci_width n_total_expected
#> 1        0.95        0.95      0.1              940
#> 2        0.70        0.70      0.1             3410

cat("\nHigh prevalence (90 percent) scenarios:\n")
#> 
#> High prevalence (90 percent) scenarios:
print(high_prev[, c("sensitivity", "specificity", "ci_width", "n_total_expected")])
#>   sensitivity specificity ci_width n_total_expected
#> 8        0.95        0.95      0.2              941
```
