# Age Pyramid Test Data Generation Summary

**Generated:** 2026-01-03
**Function:** agepyramid
**Seed:** 42

## Overview

This document describes the comprehensive test data generated for the `agepyramid` jamovi function. The test data includes multiple realistic datasets representing different clinical and demographic scenarios.

## Generated Files

### 1. Data Generation Script

**File:** [data-raw/agepyramid_test_data.R](agepyramid_test_data.R)

- Complete R script to regenerate all test datasets
- Includes documentation and usage examples
- Uses seed 42 for reproducibility
- Can be re-run to update datasets

### 2. Test Datasets (6 datasets)

All datasets include:
- `id`: Patient identifier
- `age`: Age in years (continuous numeric)
- `gender`: Gender (categorical: Female/Male)

#### Dataset Details

| Dataset | File | N | Description | Age Range |
|---------|------|---|-------------|-----------|
| **Standard Population** | `agepyramid_test.rda` | 500 | General population with balanced age distribution | 0-96 years |
| **Cancer Patients** | `agepyramid_cancer.rda` | 500 | Clinical population (older, slightly more males) | 34-95 years |
| **Pediatric** | `agepyramid_pediatric.rda` | 300 | Pediatric population only | 0-18 years |
| **Geriatric** | `agepyramid_geriatric.rda` | 200 | Elderly population (65+, more females) | 65-99 years |
| **Reproductive Age** | `agepyramid_reproductive.rda` | 400 | Reproductive age group | 15-50 years |
| **Unbalanced Gender** | `agepyramid_unbalanced.rda` | 300 | Highly unbalanced (70% female) | 18-85 years |

### 3. Data Formats (Standard Dataset)

The main `agepyramid_test` dataset is available in 4 formats:

| Format | File | Size | Use Case |
|--------|------|------|----------|
| **RDA** | `agepyramid_test.rda` | 1.4 KB | Native R format, fastest loading |
| **CSV** | `agepyramid_test.csv` | 8.1 KB | Universal format, Excel-compatible |
| **XLSX** | `agepyramid_test.xlsx` | 13 KB | Excel native format |
| **OMV** | `agepyramid_test.omv` | 4.7 KB | Jamovi native format |

### 4. Test Files (3 files)

Comprehensive test coverage using testthat framework:

| File | Tests | Purpose |
|------|-------|---------|
| [test-agepyramid-basic.R](../../tests/testthat/test-agepyramid-basic.R) | 7 tests | Basic functionality, required arguments, output validation |
| [test-agepyramid-arguments.R](../../tests/testthat/test-agepyramid-arguments.R) | 8 tests | All argument combinations, options, presets |
| [test-agepyramid-edge-cases.R](../../tests/testthat/test-agepyramid-edge-cases.R) | 16 tests | Missing data, small samples, extreme values, error handling |

**Total: 31 automated tests**

### 5. Example Usage File

**File:** [inst/examples/agepyramid_example.R](../../inst/examples/agepyramid_example.R)

- 13 comprehensive examples
- Clinical applications guide
- Best practices and tips
- Ready-to-run code

## Usage

### Loading Test Data

```r
# Load standard dataset
data(agepyramid_test)

# Load specialized datasets
data(agepyramid_cancer)
data(agepyramid_pediatric)
data(agepyramid_geriatric)
data(agepyramid_reproductive)
data(agepyramid_unbalanced)

# View structure
str(agepyramid_test)
head(agepyramid_test)
```

### Basic Function Usage

```r
library(ClinicoPath)

# Basic age pyramid
agepyramid(
  data = agepyramid_test,
  age = "age",
  gender = "gender",
  female = "Female",
  male = "Male"
)

# With custom options
agepyramid(
  data = agepyramid_test,
  age = "age",
  gender = "gender",
  female = "Female",
  male = "Male",
  age_groups = "custom",
  bin_width = 10,
  plot_title = "Population Age Distribution"
)
```

### Running Tests

```r
# Run all agepyramid tests
devtools::test(filter = "agepyramid")

# Run specific test file
testthat::test_file("tests/testthat/test-agepyramid-basic.R")

# Run individual test
testthat::test_file("tests/testthat/test-agepyramid-edge-cases.R",
                    filter = "handles missing data")
```

### Loading Examples

```r
# Run all examples
source("inst/examples/agepyramid_example.R")

# Or run specific sections from the file
```

## Dataset Characteristics

### Standard Population (agepyramid_test)

- **N = 500** (266 Female, 234 Male)
- **Age distribution:**
  - Children (0-18): 20%
  - Young adults (19-35): 25%
  - Middle age (36-55): 30%
  - Older adults (56-75): 20%
  - Elderly (76+): 5%
- **Missing data:** ~3% in age variable
- **Use case:** General testing, documentation examples

### Cancer Population (agepyramid_cancer)

- **N = 500** (225 Female, 275 Male)
- **Mean age:** 65.6 years
- **Distribution:** Normal with mean=65, sd=12
- **Gender:** Slightly more males (55%)
- **Use case:** Clinical population, older cohorts

### Pediatric Population (agepyramid_pediatric)

- **N = 300** (balanced gender)
- **Age range:** 0-18 years
- **Distribution:**
  - Infants (0-2): 20%
  - Preschool (3-5): 15%
  - School age (6-12): 40%
  - Adolescents (13-18): 25%
- **Use case:** Pediatric studies, age_groups="pediatric" testing

### Geriatric Population (agepyramid_geriatric)

- **N = 200** (120 Female, 80 Male)
- **Age range:** 65-99 years
- **Mean age:** ~78 years
- **Gender:** 60% female (reflects higher female life expectancy)
- **Use case:** Geriatric studies, age_groups="geriatric" testing

### Reproductive Age (agepyramid_reproductive)

- **N = 400** (balanced gender)
- **Age range:** 15-50 years
- **Distribution:** Uniform across reproductive years
- **Use case:** Reproductive health studies, age_groups="reproductive" testing

### Unbalanced Gender (agepyramid_unbalanced)

- **N = 300** (220 Female, 80 Male)
- **Gender ratio:** 73% female
- **Mean age:** ~58 years
- **Use case:** Testing with highly unbalanced gender (e.g., breast cancer cohort)

## Test Coverage

### Basic Functionality (7 tests)
✅ Function exists and runs
✅ Handles required arguments
✅ Errors on missing required arguments
✅ Produces expected outputs
✅ Works with complete cases
✅ Accepts different plot engines
✅ Works with age group presets

### Argument Combinations (8 tests)
✅ Respects all bin width options
✅ Works with all age group presets (pediatric, reproductive, geriatric, lifecourse, custom)
✅ Respects custom plot titles
✅ Handles factor vs character gender
✅ Works with different population structures
✅ Handles gender level selection (female and male)
✅ Handles single-gender cohorts
✅ Handles all argument combinations

### Edge Cases (16 tests)
✅ Handles missing data
✅ Handles small sample sizes (n=10)
✅ Handles minimal sample (n=2)
✅ Handles single gender data
✅ Handles unbalanced gender distribution
✅ Handles extreme age values
✅ Handles narrow age range
✅ Handles constant age
✅ Handles non-standard gender labels
✅ Handles negative ages
✅ Handles very large bin widths
✅ Handles very small bin widths
✅ Handles wrong gender level specification
✅ Handles data with outliers
✅ Handles special characters in variable names

## Regenerating Data

To regenerate all test datasets:

```r
# Set working directory to package root
setwd("/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule")

# Run generation script
source("data-raw/agepyramid_test_data.R")
```

This will recreate all datasets with the same seed (42) for reproducibility.

## Integration with Package

### Adding to DESCRIPTION

Ensure these packages are available for testing:

```
Suggests:
    testthat (>= 3.0.0),
    tibble,
    dplyr,
    here,
    writexl,
    jmvReadWrite
```

### Adding to .Rbuildignore

```
^data-raw$
^inst/examples$
```

### Documenting Datasets

Create documentation in `R/data.R`:

```r
#' Standard Age Pyramid Test Data
#'
#' A dataset containing age and gender information for 500 individuals
#' representing a general population structure.
#'
#' @format A data frame with 500 rows and 3 variables:
#' \describe{
#'   \item{id}{Patient identifier (1-500)}
#'   \item{gender}{Gender (Female, Male)}
#'   \item{age}{Age in years (0-100), with ~3% missing}
#' }
#' @source Generated using agepyramid_test_data.R with seed=42
"agepyramid_test"
```

## Clinical Applications

Age pyramids are valuable for:

1. **Demographic Analysis**
   - Understanding patient population structure
   - Comparing enrollment to general population
   - Identifying age-gender imbalances

2. **Epidemiological Studies**
   - Disease incidence visualization
   - Cohort comparisons
   - Resource allocation planning

3. **Clinical Research**
   - Study population descriptions
   - Recruitment bias detection
   - Publication-ready figures

4. **Public Health**
   - Population health assessments
   - Disease surveillance
   - Healthcare planning

## Best Practices

### Age Group Presets
- **Custom:** Maximum flexibility with bin_width (default: 5 years)
  - Small populations: use 5-10 year bins
  - Large populations: use 1-5 year bins
- **Pediatric:** Optimized for ages 0-18 with developmental milestones
  - Breaks: 0, 1, 2, 5, 10, 15, 18, Inf
- **Reproductive:** Focus on reproductive ages 15-50 with 5-year intervals
  - Breaks: 0, 15, 20, 25, 30, 35, 40, 45, 50, Inf
- **Geriatric:** Optimized for ages 65+ with 5-year intervals
  - Breaks: 0, 65, 70, 75, 80, 85, 90, 95, Inf
- **Life Course:** Comprehensive all-age analysis with key life stages
  - Breaks: 0, 5, 15, 25, 45, 65, 75, 85, Inf

### Gender Level Selection
- Female and Male level selectors allow flexible data structures
- Smart defaults: automatically uses first two levels if not specified
- Single-gender cohorts: set only Female or Male level
- The analysis will automatically detect and handle single-gender data

### Plot Customization
- Use descriptive plot titles to communicate the population being analyzed
- Age groups are displayed with readable labels (e.g., "1-5", "6-10", "86+")
- Tables include both counts (n) and percentages (%) for each age-gender group

## Validation

All test datasets have been validated for:
- ✅ Correct variable types
- ✅ Realistic distributions
- ✅ Appropriate age ranges
- ✅ Balanced/unbalanced gender ratios
- ✅ Controlled missing data
- ✅ Clinical plausibility

## Support

For issues or questions:
- Check [agepyramid_example.R](../../inst/examples/agepyramid_example.R) for usage examples
- Run tests: `devtools::test(filter="agepyramid")`
- See function documentation: `?agepyramid`

---

**Generated by:** ClinicoPath Test Data Generator
**Date:** 2026-01-03
**Function:** agepyramid
**Version:** 0.0.32
