---
name: generate-test-data
description: Create sample datasets, mock data, and testthat test files for a jamovi function. Outputs RDA, CSV, XLSX, OMV formats
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

**Consult:** `vignettes/jamovi_a_yaml_guide.md` for option types when auto-detecting data requirements
(`type: File`: [`File`](../../vignettes/jamovi_a_yaml_guide.md#file-jamovi-283)). Tests of `File`/`Text`
analyses need a jmvcore that has those classes: [Installing Current jmvtools and jmvcore](../../vignettes/jamovi_module_patterns_guide.md#installing-current-jmvtools-and-jmvcore),
[Testing Under CRAN jmvcore](../../vignettes/jamovi_module_patterns_guide.md#testing-under-cran-jmvcore).

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

## Data Generation & Test File Templates

For complete code templates (survival data, diagnostic data, basic tests, argument tests, edge case tests), read `.claude/references/test-data-templates.md`.

An analysis with a `type: File` option or a `type: Text` result uses the template "File Options and
Text Results" there: the file is written to a `tempfile()` inside the test (no fixture shipped), and the
test file starts with the skip guard, because under CRAN jmvcore the whole wrapper errors, not just the
`File` argument:

```r
skip_if_not(exists("OptionFile", envir = asNamespace("jmvcore"), inherits = FALSE),
            "needs jmvcore with OptionFile (install from jamovi/jamovi main)")
```

(use `"Text"` instead of `"OptionFile"` for Text results).

## Auto-Detection Algorithm

```
1. Read jamovi/{function_name}.a.yaml
2. Parse all options with type: Variable or Variables
   (type: File → the test writes a small file in one of its `extensions` to a
   tempfile(), plus a malformed and a wrong-extension variant; see templates)
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
- ✅ `File` option: unset (`NULL`), valid file, nonexistent path (`needs to be re-selected`), wrong extension or malformed content rejected by the backend (R does not enforce `extensions:`)
- ✅ `Text` result: one string in `$content`, asserted as raw markdown

Run tests with:
```bash
Rscript -e "devtools::test(filter='{function_name}')"
```
