---
name: generate-test-data
description: Generate comprehensive test data and test files for a jamovi function
interactive: true
args:
  function_name:
    description: Name of the jamovi function to generate test data for
    required: true
    autocomplete: functions
  --data-type:
    description: Type of test data (auto, survival, descriptive, diagnostic, categorical, continuous)
    required: false
    default: auto
  --n-obs:
    description: Number of observations to generate
    required: false
    default: 100
  --formats:
    description: Comma-separated list of formats (rda,csv,xlsx,omv,all)
    required: false
    default: all
  --generate-tests:
    description: Generate testthat test files
    required: false
    default: true
  --generate-examples:
    description: Generate example usage code
    required: false
    default: true
  --seed:
    description: Random seed for reproducibility
    required: false
    default: 42
usage: /generate-test-data <function_name> [options]
examples:
  /generate-test-data tableone                                    # Auto-detect and generate all formats
  /generate-test-data survival --data-type=survival --n-obs=200   # Survival data with 200 obs
  /generate-test-data diagnostic --formats=csv,omv                # Only CSV and OMV formats
---

# Comprehensive Test Data Generator for Jamovi Functions

Generate realistic test datasets in multiple formats (RDA, CSV, Excel, OMV) with corresponding:
- Data preparation scripts (`data-raw/`)
- Data files (`data/`)
- Test files (`tests/testthat/`)
- Example usage code

## Features

1. **Auto-detection**: Analyzes function arguments to determine data requirements
2. **Multiple formats**: RDA, CSV, XLSX, OMV (jamovi native)
3. **Realistic data**: Generates clinically/statistically realistic data
4. **Complete testing**: Creates comprehensive testthat tests
5. **Documentation**: Generates example usage code and vignettes

## Data Type Detection

When `--data-type=auto`, the command analyzes `.a.yaml` to determine required data:

```yaml
# Analyzes option types to infer data needs
options:
  - name: time      # → Continuous positive variable (survival time)
    type: Variable
  - name: event     # → Binary variable (event indicator)
    type: Variable
  - name: group     # → Categorical variable (grouping)
    type: Variable
  - name: covs      # → Mixed continuous/categorical covariates
    type: Variables
```

**Detected patterns:**
- `time` + `event` → Survival data
- `dep` + `group` → Group comparison data
- `outcome` + `predictors` → Diagnostic/regression data
- `vars` → General descriptive data

## Generated Files Structure

```
project/
├── data-raw/
│   └── {function_name}_test_data.R    # Data generation script
├── data/
│   ├── {function_name}_test.rda       # R data format
│   ├── {function_name}_test.csv       # CSV format
│   ├── {function_name}_test.xlsx      # Excel format
│   └── {function_name}_test.omv       # Jamovi format
├── tests/testthat/
│   ├── test-{function_name}-basic.R           # Basic functionality tests
│   ├── test-{function_name}-arguments.R       # Test all argument combinations
│   ├── test-{function_name}-edge-cases.R      # Edge cases and error handling
│   └── test-{function_name}-integration.R     # Integration with other functions
└── inst/examples/
    └── {function_name}_example.R      # Example usage
```

## Data Generation Scripts

### Example: Survival Data

**File:** `data-raw/survival_test_data.R`

```r
# ═══════════════════════════════════════════════════════════
# Test Data Generation: survival
# ═══════════════════════════════════════════════════════════
#
# This script generates realistic test data for the survival jamovi function
#
# Generated: 2026-01-02
# Seed: 42
# Observations: 100

library(tibble)
library(dplyr)
set.seed(42)

# Sample size
n <- 100

# Generate survival data
survival_test <- tibble(
  # Patient ID
  id = 1:n,

  # Survival time (months, 0-120)
  time = pmax(0.1, rnorm(n, mean = 36, sd = 24)),

  # Event indicator (0 = censored, 1 = event)
  event = rbinom(n, size = 1, prob = 0.6),

  # Treatment group
  treatment = sample(c("Control", "Treatment A", "Treatment B"), n, replace = TRUE),

  # Age (years)
  age = round(rnorm(n, mean = 65, sd = 12)),

  # Tumor stage (I-IV)
  stage = sample(c("I", "II", "III", "IV"), n, replace = TRUE,
                 prob = c(0.2, 0.3, 0.3, 0.2)),

  # Tumor grade (1-3)
  grade = sample(1:3, n, replace = TRUE, prob = c(0.3, 0.5, 0.2)),

  # Biomarker continuous
  biomarker = rnorm(n, mean = 50, sd = 15)
)

# Add realistic correlations
# Higher stage → shorter survival
survival_test <- survival_test %>%
  mutate(
    time = case_when(
      stage == "IV" ~ time * 0.5,
      stage == "III" ~ time * 0.7,
      stage == "II" ~ time * 0.9,
      TRUE ~ time
    )
  )

# Add some missing data (5%)
n_missing <- round(n * 0.05)
survival_test$biomarker[sample(n, n_missing)] <- NA

# Save in multiple formats
# 1. RDA format (native R)
save(survival_test, file = here::here("data", "survival_test.rda"))

# 2. CSV format
write.csv(survival_test, file = here::here("data", "survival_test.csv"), row.names = FALSE)

# 3. Excel format
writexl::write_xlsx(survival_test, path = here::here("data", "survival_test.xlsx"))

# 4. Jamovi format (OMV)
jmvReadWrite::write_omv(survival_test, path = here::here("data", "survival_test.omv"))

# Document the dataset
cat("
Dataset: survival_test
Observations:", n, "
Variables:", ncol(survival_test), "
Missing data: ~5% in biomarker

Variable descriptions:
- id: Patient identifier
- time: Survival time in months
- event: Event indicator (0=censored, 1=event)
- treatment: Treatment group
- age: Patient age in years
- stage: Tumor stage (I-IV)
- grade: Tumor grade (1-3)
- biomarker: Continuous biomarker value

Usage:
  data(survival_test)
  library(ClinicoPath)
  survival(data = survival_test, time = 'time', event = 'event', group = 'treatment')
")
```

### Example: Diagnostic Data

**File:** `data-raw/diagnostic_test_data.R`

```r
# Diagnostic/ROC analysis test data
set.seed(42)
n <- 200

diagnostic_test <- tibble(
  id = 1:n,

  # Gold standard diagnosis (0 = negative, 1 = positive)
  diagnosis = rbinom(n, size = 1, prob = 0.3),

  # Continuous biomarker (higher in cases)
  biomarker_continuous = rnorm(n, mean = 50 + diagnosis * 20, sd = 15),

  # Binary test result
  test_result = rbinom(n, size = 1, prob = 0.5 + diagnosis * 0.3),

  # Covariates
  age = round(rnorm(n, 60, 15)),
  sex = sample(c("Male", "Female"), n, replace = TRUE),

  # Second biomarker for comparison
  biomarker2 = rnorm(n, mean = 100 + diagnosis * 30, sd = 20)
)

# Save in all formats
save(diagnostic_test, file = here::here("data", "diagnostic_test.rda"))
write.csv(diagnostic_test, file = here::here("data", "diagnostic_test.csv"), row.names = FALSE)
writexl::write_xlsx(diagnostic_test, path = here::here("data", "diagnostic_test.xlsx"))
jmvReadWrite::write_omv(diagnostic_test, path = here::here("data", "diagnostic_test.omv"))
```

## Test File Generation

### Basic Functionality Test

**File:** `tests/testthat/test-survival-basic.R`

```r
# ═══════════════════════════════════════════════════════════
# Basic Functionality Tests: survival
# ═══════════════════════════════════════════════════════════

library(testthat)
library(ClinicoPath)

# Load test data
data(survival_test, package = "ClinicoPath")

test_that("survival function exists and runs", {
  # Basic execution test
  result <- survival(
    data = survival_test,
    time = "time",
    event = "event"
  )

  expect_s3_class(result, "survivalClass")
  expect_true("results" %in% names(result))
})

test_that("survival handles required arguments", {
  # Test with minimal required arguments
  result <- survival(
    data = survival_test,
    time = "time",
    event = "event"
  )

  expect_no_error(result)
})

test_that("survival errors on missing required arguments", {
  # Missing time variable
  expect_error(
    survival(data = survival_test, event = "event"),
    regexp = "time.*required|missing.*time",
    ignore.case = TRUE
  )

  # Missing event variable
  expect_error(
    survival(data = survival_test, time = "time"),
    regexp = "event.*required|missing.*event",
    ignore.case = TRUE
  )
})

test_that("survival produces expected outputs", {
  result <- survival(
    data = survival_test,
    time = "time",
    event = "event",
    group = "treatment"
  )

  # Check that main table exists
  expect_true(!is.null(result$results$maintable))

  # Check that plot exists if requested
  # ... additional output checks
})
```

### Argument Testing

**File:** `tests/testthat/test-survival-arguments.R`

```r
# ═══════════════════════════════════════════════════════════
# Argument Combination Tests: survival
# ═══════════════════════════════════════════════════════════

library(testthat)
library(ClinicoPath)
data(survival_test)

test_that("survival respects all argument combinations", {
  # Test with covariates
  result1 <- survival(
    data = survival_test,
    time = "time",
    event = "event",
    covs = c("age", "biomarker")
  )
  expect_no_error(result1)

  # Test with stratification
  result2 <- survival(
    data = survival_test,
    time = "time",
    event = "event",
    strata = "stage"
  )
  expect_no_error(result2)

  # Test with all options
  result3 <- survival(
    data = survival_test,
    time = "time",
    event = "event",
    group = "treatment",
    covs = c("age", "stage"),
    confLevel = 0.99,
    plotKM = TRUE,
    riskTable = TRUE
  )
  expect_no_error(result3)
})

test_that("survival handles factor vs character variables", {
  # Convert to factor
  test_data_factor <- survival_test
  test_data_factor$treatment <- as.factor(test_data_factor$treatment)

  result_factor <- survival(
    data = test_data_factor,
    time = "time",
    event = "event",
    group = "treatment"
  )

  # Convert to character
  test_data_char <- survival_test
  test_data_char$treatment <- as.character(test_data_char$treatment)

  result_char <- survival(
    data = test_data_char,
    time = "time",
    event = "event",
    group = "treatment"
  )

  # Both should work
  expect_no_error(result_factor)
  expect_no_error(result_char)
})
```

### Edge Cases Test

**File:** `tests/testthat/test-survival-edge-cases.R`

```r
# ═══════════════════════════════════════════════════════════
# Edge Cases and Error Handling Tests: survival
# ═══════════════════════════════════════════════════════════

library(testthat)
library(ClinicoPath)

test_that("survival handles missing data correctly", {
  # Create data with missing values
  test_data_na <- survival_test
  test_data_na$time[1:5] <- NA

  # Should either error or warn about missing data
  expect_warning(
    survival(data = test_data_na, time = "time", event = "event"),
    regexp = "missing|NA|removed",
    ignore.case = TRUE
  )
})

test_that("survival handles small sample sizes", {
  # Very small dataset
  small_data <- survival_test[1:10, ]

  result <- survival(
    data = small_data,
    time = "time",
    event = "event"
  )

  # Should complete but may warn
  expect_s3_class(result, "survivalClass")
})

test_that("survival handles zero events", {
  # All censored
  no_events_data <- survival_test
  no_events_data$event <- 0

  # Should error with informative message
  expect_error(
    survival(data = no_events_data, time = "time", event = "event"),
    regexp = "no events|zero events|all censored",
    ignore.case = TRUE
  )
})

test_that("survival handles constant variables", {
  # Constant predictor
  const_data <- survival_test
  const_data$constant_var <- 1

  # Should error or warn
  expect_condition(
    survival(data = const_data, time = "time", event = "event", covs = "constant_var")
  )
})

test_that("survival handles variables with special characters", {
  # Variable names with spaces
  special_data <- survival_test
  names(special_data)[names(special_data) == "time"] <- "time to event"

  result <- survival(
    data = special_data,
    time = "time to event",
    event = "event"
  )

  expect_no_error(result)
})
```

## Auto-Detection Algorithm

```
1. Read jamovi/{function_name}.a.yaml
2. Parse all options with type: Variable or Variables
3. Infer data type from option names:
   - time + event → survival data
   - outcome + predictors → diagnostic/regression
   - dep + group → group comparison
   - vars → descriptive statistics
4. Determine variable types:
   - Continuous: age, time, biomarker, score, value, measure
   - Categorical: group, treatment, stage, grade, sex, category
   - Binary: event, outcome, diagnosis, status, positive
5. Generate realistic distributions:
   - Clinical variables use domain-appropriate ranges
   - Add correlations where expected (e.g., stage → survival)
   - Include ~5% missing data
6. Create multiple test scenarios:
   - Minimal data (n=20)
   - Standard data (n=100)
   - Large data (n=1000)
```

## Output Formats

### RDA (Native R)
- Fastest loading in R/jamovi
- Preserves factors, attributes
- Used by `data()` command

### CSV (Universal)
- Cross-platform compatibility
- Human-readable
- Easy import/export

### XLSX (Excel)
- Clinician-friendly format
- Retains formatting
- Multi-sheet capable

### OMV (Jamovi native)
- Native jamovi format
- Preserves jamovi metadata
- Used for UI testing
- Created with `jmvReadWrite::write_omv()`

## Dependencies

Required packages:
```r
# Data manipulation
library(tibble)
library(dplyr)
library(here)

# File I/O
library(readr)        # CSV
library(writexl)      # Excel
library(jmvReadWrite) # OMV (jamovi)

# Testing
library(testthat)
library(devtools)
```

## Usage Examples

```bash
# Generate all formats for survival function
/generate-test-data survival

# Generate only CSV and OMV for diagnostic test
/generate-test-data diagnostictest --formats=csv,omv

# Generate large dataset without tests
/generate-test-data tableone --n-obs=1000 --generate-tests=false

# Generate with specific seed
/generate-test-data survival --seed=12345

# Generate and run tests immediately
/generate-test-data survival && Rscript -e "devtools::test(filter='survival')"
```

## Integration with Other Commands

**Workflow:**
1. Create function: `/create-function myfunction --wizard`
2. Generate test data: `/generate-test-data myfunction`
3. Check function: `/check-function myfunction --profile=standard`
4. Run tests: `/bash Rscript -e "devtools::test(filter='myfunction')"`

## Performance Notes

- Generation time: ~2-5 seconds per format
- File sizes (n=100):
  - RDA: ~5-10 KB
  - CSV: ~10-20 KB
  - XLSX: ~15-30 KB
  - OMV: ~20-40 KB

## Validation

Generated test files automatically check:
- ✅ Function existence
- ✅ Required arguments
- ✅ Output structure
- ✅ Edge cases
- ✅ Error messages
- ✅ All argument combinations
- ✅ Data type handling
- ✅ Missing data handling

Run tests with:
```bash
Rscript -e "devtools::test(filter='{function_name}')"
```
