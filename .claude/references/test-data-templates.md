# Test Data Generation Templates

Reference templates for `/generate-test-data`. These are example scripts and test files — adapt them for each function.

---

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
# Higher stage -> shorter survival
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
save(survival_test, file = here::here("data", "survival_test.rda"))
write.csv(survival_test, file = here::here("data", "survival_test.csv"), row.names = FALSE)
writexl::write_xlsx(survival_test, path = here::here("data", "survival_test.xlsx"))
jmvReadWrite::write_omv(survival_test, path = here::here("data", "survival_test.omv"))
```

### Example: Diagnostic Data

**File:** `data-raw/diagnostic_test_data.R`

```r
# Diagnostic/ROC analysis test data
set.seed(42)
n <- 200

diagnostic_test <- tibble(
  id = 1:n,
  diagnosis = rbinom(n, size = 1, prob = 0.3),
  biomarker_continuous = rnorm(n, mean = 50 + diagnosis * 20, sd = 15),
  test_result = rbinom(n, size = 1, prob = 0.5 + diagnosis * 0.3),
  age = round(rnorm(n, 60, 15)),
  sex = sample(c("Male", "Female"), n, replace = TRUE),
  biomarker2 = rnorm(n, mean = 100 + diagnosis * 30, sd = 20)
)

# Save in all formats
save(diagnostic_test, file = here::here("data", "diagnostic_test.rda"))
write.csv(diagnostic_test, file = here::here("data", "diagnostic_test.csv"), row.names = FALSE)
writexl::write_xlsx(diagnostic_test, path = here::here("data", "diagnostic_test.xlsx"))
jmvReadWrite::write_omv(diagnostic_test, path = here::here("data", "diagnostic_test.omv"))
```

---

## Test File Templates

### Basic Functionality Test

**File:** `tests/testthat/test-{function}-basic.R`

```r
library(testthat)
library(ClinicoPath)

data({function}_test, package = "ClinicoPath")

test_that("{function} function exists and runs", {
  result <- {function}(
    data = {function}_test,
    # ... required args
  )
  expect_s3_class(result, "{function}Class")
  expect_true("results" %in% names(result))
})

test_that("{function} handles required arguments", {
  result <- {function}(data = {function}_test, ...)
  expect_no_error(result)
})

test_that("{function} errors on missing required arguments", {
  expect_error({function}(data = {function}_test), regexp = "required|missing")
})
```

### Argument Testing

**File:** `tests/testthat/test-{function}-arguments.R`

```r
library(testthat)
library(ClinicoPath)
data({function}_test)

test_that("{function} respects all argument combinations", {
  # Test with covariates
  result1 <- {function}(data = {function}_test, ...)
  expect_no_error(result1)

  # Test with all options
  result2 <- {function}(data = {function}_test, option1 = TRUE, option2 = "value")
  expect_no_error(result2)
})

test_that("{function} handles factor vs character variables", {
  test_factor <- {function}_test
  test_factor$group <- as.factor(test_factor$group)
  result <- {function}(data = test_factor, ...)
  expect_no_error(result)
})
```

### Edge Cases Test

**File:** `tests/testthat/test-{function}-edge-cases.R`

```r
library(testthat)
library(ClinicoPath)

test_that("{function} handles missing data correctly", {
  test_na <- {function}_test
  test_na$var[1:5] <- NA
  expect_warning({function}(data = test_na, ...), regexp = "missing|NA|removed")
})

test_that("{function} handles small sample sizes", {
  small_data <- {function}_test[1:10, ]
  result <- {function}(data = small_data, ...)
  expect_s3_class(result, "{function}Class")
})

test_that("{function} handles variables with special characters", {
  special_data <- {function}_test
  names(special_data)[1] <- "var with spaces"
  result <- {function}(data = special_data, var = "var with spaces")
  expect_no_error(result)
})
```