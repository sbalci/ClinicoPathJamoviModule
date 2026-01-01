# .omv File Generation for ClinicoPath Module

## Overview

This directory contains scripts and utilities for generating jamovi-compatible `.omv` files from R data files. The `.omv` format is required for data to be loaded directly in jamovi.

## File Status

**Current Status (as of last update):**
- **606 .omv files** created from 604 unique datasets
- **Coverage: 100%** (all data frames have .omv files)
- **9 list objects** cannot be converted (lists are not supported in jamovi)

### Files Not Converted to .omv

The following files are R lists (not data frames) and cannot be converted to jamovi format:
- `jggstats_comprehensive_data.rda`
- `nomogrammer_error_test_cases.rda`
- `nomogrammer_test_data.rda`
- `oddsratio_test_data.rda`
- `sankey_comprehensive_data.rda`
- `stagemigration_summary_stats.rda`
- `tidyplots_datasets.rda`
- `tumor_response_examples.rda`
- `VALungCancer_list.rda`

These files are used internally by R functions and are not intended for direct jamovi use.

## Key Scripts

### 1. `data_save_helpers.R`
**Purpose:** Provides helper functions for saving data in multiple formats.

**Functions:**
- `save_data_multi_format(data, name)` - Save a single dataset in .rda, .omv, and .csv formats
- `save_multiple_datasets(name1=data1, ...)` - Save multiple datasets at once
- `use_data_multi_format(data)` - Drop-in replacement for `usethis::use_data()`

**Usage in data generation scripts:**
```r
# Load helpers
source("data-raw/data_save_helpers.R")

# Create your data
my_data <- data.frame(x = 1:10, y = letters[1:10])

# Save in all formats
use_data_multi_format(my_data, overwrite = TRUE)
```

### 2. `generate_all_omv_files.R`
**Purpose:** Batch generate .omv files for all existing .rda and .csv files.

**When to use:**
- After adding new data files manually
- To regenerate all .omv files
- When .omv files are missing or corrupted

**Usage:**
```bash
Rscript data-raw/generate_all_omv_files.R
```

### 3. `update_all_data_scripts.R`
**Purpose:** Automatically update existing data generation scripts to use multi-format saving.

**What it does:**
- Adds `source("data-raw/data_save_helpers.R")` to scripts
- Replaces `usethis::use_data()` calls with `use_data_multi_format()`
- Adds .omv generation for direct `save()` calls

**Usage:**
```bash
Rscript data-raw/update_all_data_scripts.R
```

**Results from last run:**
- 166 scripts checked
- 44 scripts updated
- 0 errors
- 122 scripts unchanged (already using multi-format saving)

### 4. `verify_omv_files.R`
**Purpose:** Verify that all data files have corresponding .omv files.

**Usage:**
```bash
Rscript data-raw/verify_omv_files.R
```

**Output:**
- File statistics (.rda, .csv, .omv counts)
- Coverage percentage
- List of missing .omv files
- Summary of datasets with all three formats

### 5. `TEMPLATE_data_generation.R`
**Purpose:** Template showing recommended patterns for creating new datasets.

**Features:**
- Multiple usage examples
- Best practices
- Documentation templates
- Comments explaining each approach

## Workflow for Creating New Data

### Method 1: Using the Template (Recommended)
```r
# 1. Copy the template
cp data-raw/TEMPLATE_data_generation.R data-raw/create_mydata.R

# 2. Edit the script to create your data
# 3. Run the script
Rscript data-raw/create_mydata.R

# This automatically creates:
# - data/mydata.rda
# - data/mydata.omv
# - data/mydata.csv
```

### Method 2: Using Helper Functions Directly
```r
# Load helpers
source("data-raw/data_save_helpers.R")

# Create data
my_dataset <- data.frame(
  id = 1:100,
  value = rnorm(100),
  group = factor(rep(c("A", "B"), 50))
)

# Save in all formats
use_data_multi_format(my_dataset, overwrite = TRUE, save_csv = TRUE)
```

### Method 3: Save Multiple Datasets at Once
```r
source("data-raw/data_save_helpers.R")

# Create multiple datasets
data1 <- data.frame(x = 1:10)
data2 <- data.frame(y = 11:20)
data3 <- data.frame(z = 21:30)

# Save all at once
save_multiple_datasets(
  dataset_one = data1,
  dataset_two = data2,
  dataset_three = data3,
  save_csv = TRUE
)
```

## Best Practices

### Data Creation
1. ✅ **Always set a seed** for reproducibility: `set.seed(123)`
2. ✅ **Use meaningful variable names** that describe the data
3. ✅ **Use factors for categorical variables** with explicit levels
4. ✅ **Use appropriate data types** (Date for dates, logical for TRUE/FALSE)
5. ✅ **Include realistic missing data** when appropriate for testing
6. ✅ **Document your data** in `R/data.R` with roxygen2 comments

### Data Saving
1. ✅ **Always save in all three formats** (.rda, .omv, .csv)
2. ✅ **Use helper functions** from `data_save_helpers.R`
3. ✅ **Verify .omv files** are created successfully
4. ✅ **Test data loading** in jamovi before committing

### Data Documentation
Add documentation in `R/data.R`:
```r
#' My Test Data
#'
#' @description
#' A dataset containing [describe purpose].
#'
#' @format A data frame with X rows and Y columns:
#' \describe{
#'   \item{variable1}{Description of variable 1}
#'   \item{variable2}{Description of variable 2}
#' }
#'
#' @examples
#' data(my_test_data)
#' head(my_test_data)
#' summary(my_test_data)
#'
#' @source [Optional: describe data source or generation method]
"my_test_data"
```

## Troubleshooting

### Issue: .omv file not created
**Possible causes:**
1. Data is not a data.frame (check with `is.data.frame()`)
2. Data contains unsupported types
3. jmvReadWrite package not installed

**Solution:**
```r
# Install jmvReadWrite if needed
install.packages("jmvReadWrite")

# Convert to standard data.frame
data <- as.data.frame(data)

# Try manual creation
jmvReadWrite::write_omv(data, "data/mydata.omv")
```

### Issue: List objects can't be converted
**Explanation:** jamovi only supports data frames, not lists.

**Solution:**
- If the list contains data frames, extract and save them separately
- If the list is for internal R use only, it doesn't need an .omv file

### Issue: Data generation script fails after update
**Possible causes:**
1. Helper file not found
2. Syntax error in updated code

**Solution:**
```bash
# Check the helper file exists
ls data-raw/data_save_helpers.R

# Run the script with verbose output
Rscript data-raw/your_script.R
```

## Maintenance Tasks

### Weekly/Monthly Tasks
- [ ] Run `verify_omv_files.R` to check for missing files
- [ ] Update any new data generation scripts
- [ ] Test data loading in jamovi

### Before Release
- [ ] Run `generate_all_omv_files.R` to ensure all files are up to date
- [ ] Run `verify_omv_files.R` and confirm 100% coverage
- [ ] Test key datasets in jamovi
- [ ] Update documentation if needed

## Technical Details

### File Formats

**`.rda` (R Data File):**
- Native R binary format
- Used by R packages
- Can store any R object (data.frames, lists, functions, etc.)
- Created with `save()` or `usethis::use_data()`

**`.omv` (jamovi Data File):**
- jamovi's native format
- Based on zip archive containing data and metadata
- Only supports data frames (not lists or other objects)
- Created with `jmvReadWrite::write_omv()`

**`.csv` (Comma-Separated Values):**
- Universal plain text format
- Human-readable
- Can be opened in Excel, jamovi, R, etc.
- Created with `write.csv()`

### Package Dependencies

Required packages:
- `jmvReadWrite` - For .omv file creation
- `usethis` - For R package data management (optional if using helpers)
- `tools` - For file path operations

## History

### Updates
- **2026-01-01:** Complete overhaul of data generation system
  - Created helper functions for multi-format saving
  - Updated all 166 data generation scripts
  - Generated 606 .omv files
  - Achieved 100% coverage for all data frames
  - Documented 9 list objects that cannot be converted

### Previous Status
- Before this update, only 59 .omv files existed
- Data generation was inconsistent across scripts
- No standardized approach for creating .omv files

## References

- [jamovi Documentation](https://www.jamovi.org/)
- [jmvReadWrite Package](https://cran.r-project.org/package=jmvReadWrite)
- [R Package Development Guide](https://r-pkgs.org/)

## Support

For issues or questions:
1. Check this README first
2. Review the template script (`TEMPLATE_data_generation.R`)
3. Run verification script to identify specific issues
4. Check the jamovi documentation for .omv format requirements
