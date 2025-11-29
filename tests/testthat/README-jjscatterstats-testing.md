# Comprehensive Testing Guide for jjscatterstats

## Overview

This document describes the comprehensive testing approach for `jjscatterstats`, including functional tests, statistical output validation, and visual regression testing.

## Test Files

### 1. `test-jjscatterstats.R` (Existing)
**Purpose**: Basic functional testing

**Coverage**:
- Basic scatter plot functionality
- Grouping variable support
- Different statistical types (parametric, nonparametric, robust, Bayes)
- Customization options
- Edge cases (missing values, empty data)
- Input validation
- Performance optimization
- Theme options
- Results subtitle options

**Status**: ✅ Existing comprehensive coverage

---

### 2. `test-jjscatterstats-enhanced.R` (NEW)
**Purpose**: Enhanced plot modes, ggpubr mode, and statistical output validation

**Coverage**:

#### Enhanced Scatter Plot Mode
- ✅ Color variable mapping
- ✅ Size variable mapping
- ✅ Shape variable mapping
- ✅ Alpha (transparency) variable mapping
- ✅ Label variable mapping
- ✅ Multiple aesthetics combined
- ✅ Different smoothing methods (lm, loess, gam)

#### ggpubr Plot Mode
- ✅ Basic ggpubr scatter plot creation
- ✅ Color palette options (npg, aaas, nejm, lancet, jco, etc.)
- ✅ Correlation statistics overlay
- ✅ Different correlation methods (Pearson, Spearman, Kendall)
- ✅ Smooth trend lines
- ✅ Grouped ggpubr plots

#### Statistical Output Validation
- ✅ Pearson correlation accuracy
- ✅ Spearman correlation accuracy
- ✅ Zero correlation detection
- ✅ Negative correlation handling
- ✅ All statistical test types
- ✅ Handles perfect correlation
- ✅ Handles data with outliers (parametric vs robust)
- ✅ Small sample sizes
- ✅ Constant variables (edge case)

#### Clinical Presets
- ✅ All preset options tested

#### Marginal Distributions
- ✅ Marginal distribution types (histogram, boxplot, density, violin, densigram)

#### Integration Tests
- ✅ Multiple features combined simultaneously

---

### 3. `test-jjscatterstats-visual.R` (NEW)
**Purpose**: Visual regression testing with `vdiffr`

**Coverage**:

#### Basic Plot Modes
- ✅ Parametric plot visual baseline
- ✅ Nonparametric plot visual baseline
- ✅ Robust plot visual baseline

#### Grouped Plots
- ✅ Grouped by treatment

#### Enhanced Plot Modes
- ✅ Color variable visual
- ✅ Size variable visual
- ✅ Shape variable visual
- ✅ Multiple aesthetics visual

#### ggpubr Plot Modes
- ✅ Basic ggpubr plot
- ✅ With correlation statistics
- ✅ With smooth line
- ✅ Grouped ggpubr plots

#### Marginal Distributions
- ✅ Marginal histograms
- ✅ Marginal densities
- ✅ Marginal boxplots

#### Customization
- ✅ Custom titles and labels
- ✅ Original ggstatsplot theme

---

## Running the Tests

### Run All Tests
```R
# From package root directory
devtools::test()

# Or using testthat directly
testthat::test_dir("tests/testthat")
```

### Run Specific Test File
```R
# Run only basic functional tests
testthat::test_file("tests/testthat/test-jjscatterstats.R")

# Run only enhanced mode tests
testthat::test_file("tests/testthat/test-jjscatterstats-enhanced.R")

# Run only visual regression tests
testthat::test_file("tests/testthat/test-jjscatterstats-visual.R")
```

### Run Specific Test
```R
# Run a single test by name
testthat::test_file(
  "tests/testthat/test-jjscatterstats-enhanced.R",
  filter = "enhanced mode works with color variable"
)
```

---

## Visual Regression Testing with vdiffr

### First-Time Setup
```R
# Install vdiffr
install.packages("vdiffr")

# Run visual tests for the first time
devtools::test()

# This will create baseline SVG snapshots in:
# tests/testthat/_snaps/
```

### Managing Visual Test Cases

#### 1. Review New or Changed Plots
```R
# Launch interactive Shiny app to review plots
vdiffr::manage_cases()
```

The Shiny app will show:
- **New cases**: Plots that don't have baselines yet
- **Failed cases**: Plots that differ from baselines
- **Orphaned cases**: Baseline files without corresponding tests

#### 2. Accept or Reject Changes
In the Shiny app:
- Click "Toggle" to see before/after comparison
- Click "Validate" to accept the new plot as baseline
- Click "Delete" to reject and keep old baseline

#### 3. Commit SVG Baselines
```bash
git add tests/testthat/_snaps/
git commit -m "Update visual regression baselines for jjscatterstats"
```

### When Visual Tests Fail

Visual tests fail when:
1. **Intentional changes**: You modified plot appearance
   - **Action**: Review with `vdiffr::manage_cases()`, accept if correct
2. **Unintentional changes**: Plot appearance changed unexpectedly
   - **Action**: Investigate why plot changed, fix code, re-run tests
3. **Platform differences**: SVG rendering differs across systems
   - **Action**: May need platform-specific baselines or tolerance settings

### Skipping Visual Tests

Visual tests automatically skip if `vdiffr` is not installed:
```R
# Visual tests will be skipped if vdiffr is unavailable
# This prevents CI/CD failures when vdiffr is not in environment
```

---

## Test Data

### Reproducible Test Data
All tests use fixed random seeds for reproducibility:
```R
set.seed(42)         # Basic tests
set.seed(12345)      # Visual tests
set.seed(123)        # Statistical validation
```

### Test Data Characteristics
- **Sample size**: 80-100 observations
- **Variables**:
  - Continuous: biomarkers, tumor size, alpha values
  - Categorical: treatment groups, response status
  - Labels: patient IDs
- **Correlations**: Controlled relationships for validation

---

## Coverage Metrics

### Current Test Coverage

| Category | Test File | Coverage |
|----------|-----------|----------|
| **Functional** | `test-jjscatterstats.R` | ✅ Complete |
| **Enhanced Mode** | `test-jjscatterstats-enhanced.R` | ✅ Complete |
| **ggpubr Mode** | `test-jjscatterstats-enhanced.R` | ✅ Complete |
| **Statistical Validation** | `test-jjscatterstats-enhanced.R` | ✅ Complete |
| **Visual Regression** | `test-jjscatterstats-visual.R` | ✅ Complete |

### What's Tested

✅ **Plot Modes**:
- Basic scatter (ggstatsplot)
- Enhanced scatter (with aesthetics)
- ggpubr scatter
- Grouped plots

✅ **Statistical Methods**:
- Parametric (Pearson)
- Nonparametric (Spearman)
- Robust (percentage bend)
- Bayesian

✅ **Statistical Accuracy**:
- Correlation coefficients
- P-values
- Confidence intervals
- Effect sizes

✅ **Edge Cases**:
- Missing data
- Small samples
- Perfect correlation
- Zero correlation
- Outliers
- Constant variables

✅ **Visual Consistency**:
- Plot appearance
- Theme application
- Legend positioning
- Title formatting
- Marginal distributions

---

## Continuous Integration

### GitHub Actions

Add to `.github/workflows/test.yml`:
```yaml
name: R-CMD-check

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: r-lib/actions/setup-r@v2
      - name: Install dependencies
        run: |
          install.packages(c("devtools", "testthat", "vdiffr"))
          devtools::install_deps()
      - name: Run tests
        run: devtools::test()
      - name: Upload test results
        if: failure()
        uses: actions/upload-artifact@v3
        with:
          name: test-results
          path: tests/testthat/*.Rout.fail
```

### Test on Multiple Platforms
```yaml
strategy:
  matrix:
    os: [ubuntu-latest, windows-latest, macos-latest]
    r: ['4.1', '4.2', '4.3']
```

---

## Troubleshooting

### "vdiffr not installed" warnings
```R
# Install vdiffr
install.packages("vdiffr")
```

### Visual tests always failing
```R
# Reset all visual baselines
unlink("tests/testthat/_snaps", recursive = TRUE)
devtools::test()
vdiffr::manage_cases()  # Accept all new baselines
```

### Tests timeout
```R
# Increase timeout for large datasets
options(timeout = 300)  # 5 minutes
```

### Platform-specific failures
```R
# Use platform-specific snapshots
# vdiffr automatically handles this in _snaps/ directory structure
```

---

## Best Practices

### 1. Write Tests Before Fixing Bugs
```R
test_that("jjscatterstats handles [bug description]", {
  # Reproduce the bug
  result <- jjscatterstats(...)

  # Assert expected behavior
  expect_...()
})
```

### 2. Use Descriptive Test Names
```R
# Good
test_that("jjscatterstats enhanced mode works with color variable", { ... })

# Bad
test_that("test1", { ... })
```

### 3. Test One Thing Per Test
```R
# Good - single focus
test_that("jjscatterstats handles missing data", { ... })

# Bad - multiple unrelated assertions
test_that("jjscatterstats various tests", {
  # tests 10 different things
})
```

### 4. Keep Tests Fast
```R
# Use small datasets for most tests
test_data <- data.frame(x = rnorm(100), y = rnorm(100))

# Only use large datasets for performance tests
test_that("... performance", {
  large_data <- data.frame(x = rnorm(10000), y = rnorm(10000))
  ...
})
```

### 5. Clean Up After Tests
```R
test_that("... creates temporary files", {
  temp_file <- tempfile()
  on.exit(unlink(temp_file))  # Cleanup
  ...
})
```

---

## Future Enhancements

### Potential Additional Tests

1. **Performance Benchmarking**
   - Benchmark against ggstatsplot directly
   - Memory usage profiling

2. **Accessibility Testing**
   - Color-blind friendly palette verification
   - Font size readability

3. **Internationalization**
   - Test with non-English locales
   - Special character handling

4. **Export Quality**
   - Test PNG/PDF export quality
   - Resolution verification

---

## References

- [testthat documentation](https://testthat.r-lib.org/)
- [vdiffr documentation](https://vdiffr.r-lib.org/)
- [ggstatsplot documentation](https://indrajeetpatil.github.io/ggstatsplot/)
- [R Packages testing chapter](https://r-pkgs.org/testing-basics.html)

---

## Contact

For questions about testing:
- Review this documentation
- Check existing test files for examples
- Consult jamovi module development guides in `vignettes/`

---

**Last Updated**: 2025-01-27
**Test Coverage**: Comprehensive (functional + statistical + visual)
**Status**: Production-ready ✅
