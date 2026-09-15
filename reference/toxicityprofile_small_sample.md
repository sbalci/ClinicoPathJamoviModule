# Small Sample Edge Cases Dataset

Minimal adverse event dataset with very small sample size designed for
edge case testing, validation of statistical methods with limited data,
and assessment of function robustness with minimal observations.
Essential for testing graceful degradation and error handling.

## Usage

``` r
toxicityprofile_small_sample
```

## Format

A data frame with 15 adverse events from 15 patients and 8 variables:

- patient_id:

  Character. Simple patient identifier (SML_01 to SML_15)

- treatment_group:

  Factor. Binary treatment assignment ("A", "B")

- adverse_event:

  Factor. Limited adverse events (3 types: "Fatigue", "Nausea", "Rash")

- toxicity_grade:

  Integer. CTCAE grade (1-3 range for testing)

- system_organ_class:

  Factor. Basic SOC categories

- time_to_event:

  Integer. Days to AE onset (1-30 days range)

- patient_age:

  Integer. Adult age range (25-70 years)

- patient_sex:

  Factor. Patient sex ("Male", "Female")

## Source

Simulated data generated using create_toxicityprofile_test_data.R

## Details

This minimal dataset tests the robustness of toxicity profile analysis
with very small sample sizes, which can reveal edge cases in statistical
calculations, visualization algorithms, and summary statistics. It
validates graceful handling of insufficient data scenarios common in
early-phase trials or rare disease studies.

**Clinical Context:**

- Minimal sample size scenario (N=15)

- Simple two-arm comparison

- Limited adverse event types

- Basic severity grading

- Short assessment timeframe

**Key Characteristics:**

- Only 15 total adverse events

- Three basic adverse event types

- Simple binary treatment comparison

- Small cell counts for statistical testing

- Minimal data for visualization algorithms

**Testing Scenarios:**

- Statistical method robustness with small samples

- Visualization algorithm edge cases

- Confidence interval calculation with limited data

- Group comparison with small cell counts

- Error handling and graceful degradation

- Minimum viable analysis requirements

**Expected Behaviors:**

- Appropriate handling of small sample statistics

- Clear visualization despite limited data

- Robust confidence interval calculations

- Appropriate warnings for limited statistical power

- Graceful handling of empty cells in cross-tabulations

**Recommended Analysis Scenarios:**

- Small sample robustness testing

- Edge case validation

- Statistical method verification

- Minimum sample size assessment

- Error handling validation

- Algorithm stability testing

## See also

[`toxicityprofile`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/toxicityprofile.md),
[`toxicityprofile_datasets`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/toxicityprofile_datasets.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the dataset
data(toxicityprofile_small_sample)

# Minimal sample analysis
result <- toxicityprofile(
    data = toxicityprofile_small_sample,
    patientID = "patient_id",
    adverseEvent = "adverse_event",
    grade = "toxicity_grade",
    treatment = "treatment_group",
    plotType = "stacked_bar"
)

# Edge case testing with group comparison
result_comparison <- toxicityprofile(
    data = toxicityprofile_small_sample,
    patientID = "patient_id",
    adverseEvent = "adverse_event",
    grade = "toxicity_grade",
    treatment = "treatment_group",
    groupComparison = TRUE,
    showConfidenceIntervals = TRUE
)

# Minimal visualization testing
result_minimal <- toxicityprofile(
    data = toxicityprofile_small_sample,
    patientID = "patient_id",
    adverseEvent = "adverse_event",
    grade = "toxicity_grade",
    plotType = "dot_plot",
    minIncidence = 1
)
} # }
```
