# Diagnostic Sample Size Planning Examples

Six clinical scenarios demonstrating sample size planning for diagnostic
test accuracy studies using Clopper-Pearson exact binomial confidence
intervals. Based on Bujang MA (2023) Diagnostics 13(8):1390.

## Usage

``` r
diagnostic_sample_size_examples
```

## Format

A data frame with 6 rows and 13 variables:

- scenario:

  Name of the diagnostic test scenario

- population:

  Target population description

- prevalence:

  Disease prevalence in target population (0-1)

- target_sensitivity:

  Target sensitivity value (0-1)

- target_specificity:

  Target specificity value (0-1)

- ci_width:

  Desired 95% CI width

- study_purpose:

  Study purpose: "diagnostic", "screening_sens", or "screening_spec"

- expected_n_sens:

  Expected sample size for sensitivity estimation

- expected_n_spec:

  Expected sample size for specificity estimation

- final_n:

  Final required sample size (maximum of sensitivity/specificity)

- notes:

  Clinical notes and justification

- nonresponse_rate:

  Expected non-response rate (%)

- final_n_adjusted:

  Final sample size adjusted for non-response

## Source

Bujang MA (2023). An Elaboration on Sample Size Planning for Performing
a One-Sample Sensitivity and Specificity Analysis by Basing on
Calculations on a Specified 95% Confidence Interval Width. Diagnostics
13(8):1390.
[doi:10.3390/diagnostics13081390](https://doi.org/10.3390/diagnostics13081390)

## Details

The dataset includes six diverse clinical scenarios:

**1. Colorectal Cancer Blood Test**

- Population: High-risk patients (age \>50, family history)

- Prevalence: 10%

- Purpose: Diagnostic (need excellent sensitivity AND specificity)

- Required N: 940 subjects

**2. COVID-19 Rapid Antigen Test**

- Population: General population (asymptomatic screening)

- Prevalence: 5%

- Purpose: Screening (emphasize sensitivity)

- Required N: 1,880 subjects

**3. AI-Based Diabetic Retinopathy Detection**

- Population: Diabetic patients

- Prevalence: 30%

- Purpose: Diagnostic with moderate precision

- Required N: 147 subjects

**4. Rare Disease Biomarker (Fabry Disease)**

- Population: Suspected patients referred to genetics clinic

- Prevalence: 2% (very low!)

- Purpose: Diagnostic

- Required N: 7,900 subjects (large due to low prevalence)

**5. Lung Cancer LDCT Screening**

- Population: Heavy smokers (\>30 pack-years)

- Prevalence: 15%

- Purpose: Screening (emphasize specificity to reduce false positives)

- Required N: 4,020 subjects

**6. Digital Mammography Screening**

- Population: Women age 50-70 (recalled for further testing)

- Prevalence: 50% (enriched population)

- Purpose: Diagnostic with moderate precision

- Required N: 88 subjects

## Examples

``` r
# Load the example scenarios
data(diagnostic_sample_size_examples)

# View all scenarios
print(diagnostic_sample_size_examples[, c("scenario", "prevalence",
                                           "final_n", "final_n_adjusted")])
#>                                       scenario prevalence final_n
#> 1                 Colorectal Cancer Blood Test       0.10     940
#> 2                  COVID-19 Rapid Antigen Test       0.05    1880
#> 3      AI-Based Diabetic Retinopathy Detection       0.30     147
#> 4 Rare Disease Biomarker (e.g., Fabry Disease)       0.02    7900
#> 5                   Lung Cancer LDCT Screening       0.15    4020
#> 6                Digital Mammography Screening       0.50      88
#>   final_n_adjusted
#> 1             1175
#> 2             2212
#> 3              164
#> 4            10534
#> 5             5025
#> 6              104

# Scenario 1: Colorectal cancer screening
colorectal <- diagnostic_sample_size_examples[1, ]
cat("Scenario:", colorectal$scenario, "\n")
#> Scenario: Colorectal Cancer Blood Test 
cat("Prevalence:", colorectal$prevalence * 100, "percent\n")
#> Prevalence: 10 percent
cat("Required N:", colorectal$final_n, "\n")
#> Required N: 940 
cat("Adjusted N (20 percent non-response):", colorectal$final_n_adjusted, "\n")
#> Adjusted N (20 percent non-response): 1175 

# Demonstrate impact of prevalence on sample size
prevalence_impact <- diagnostic_sample_size_examples[, c("scenario", "prevalence", "final_n")]
prevalence_impact <- prevalence_impact[order(prevalence_impact$prevalence), ]
print(prevalence_impact)
#>                                       scenario prevalence final_n
#> 4 Rare Disease Biomarker (e.g., Fabry Disease)       0.02    7900
#> 2                  COVID-19 Rapid Antigen Test       0.05    1880
#> 1                 Colorectal Cancer Blood Test       0.10     940
#> 5                   Lung Cancer LDCT Screening       0.15    4020
#> 3      AI-Based Diabetic Retinopathy Detection       0.30     147
#> 6                Digital Mammography Screening       0.50      88
```
