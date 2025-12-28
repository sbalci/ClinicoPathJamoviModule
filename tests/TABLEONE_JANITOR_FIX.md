# Table One Janitor Variable Renaming Fix

## Issue Summary

**Error:** "Error creating frequency tables with janitor: Error processing variable 'Outcome' with janitor: argument is of length zero"

**Affected Function:** `tableone` (ClinicoPathDescriptives module)

**Table Style:** `t4` (janitor package for frequency tables)

**Root Cause:** Variable name mismatch after `jmvcore::select` processing

## Problem Description

When using the janitor table style (t4) in the Table One function, continuous variables would cause an error:

```
Error creating frequency tables with janitor: Error processing variable 'Outcome' with janitor:
argument is of length zero (Variable type: integer, Non-missing values: 249).
Check that variables have valid data. Janitor works best with categorical or discrete variables.
```

The error occurred because `jmvcore::select()` internally renames variables (similar to `janitor::clean_names()`) to ensure they are valid R names. When the code later tried to use the original variable name with `janitor::tabyl()`, it couldn't find the column because the actual column name in the data frame had been changed.

## Technical Details

### Issue Pattern

Similar to the survival function fix, this was a variable renaming issue:

1. User selects variable: `"Outcome"`
2. `jmvcore::select(self$data, selected_vars)` processes the data
3. Variable may be renamed internally: `"Outcome"` → `"outcome"` or other cleaned name
4. Code tries to use original name `"Outcome"` with `janitor::tabyl()`
5. Error: column `"Outcome"` not found (because it's now `"outcome"`)

### Solution Approach

Implement the same pattern used in the survival function:

1. **Capture actual column names** after `jmvcore::select`
2. **Use actual names** for data access
3. **Use original names** for display to users

## Code Changes

### File: `R/tableone.b.R`

#### 1. Capture Actual Column Names (Line ~71)

```r
data <- jmvcore::select(self$data, selected_vars)

# CRITICAL FIX: Get actual column names after jmvcore::select
# jmvcore::select may rename variables (e.g., clean special characters)
# Store mapping: actual column names to use in processing
actual_vars <- names(data)
```

#### 2. Update Janitor Processing Loop (Line ~177-202)

**Before:**
```r
table_list <- lapply(selected_vars, function(var) {
    # ... process using 'var'
    table <- janitor::tabyl(var_data, !!rlang::sym(var))
})
```

**After:**
```r
# CRITICAL FIX: Use actual column names (after jmvcore::select renaming)
# Create mapping for display (original -> actual column names)
var_mapping <- setNames(selected_vars, actual_vars)

table_list <- lapply(seq_along(actual_vars), function(i) {
    var <- actual_vars[i]           # Actual column name in data
    display_var <- selected_vars[i]  # Original name for display

    # ... process using 'var' for data access
    # ... use 'display_var' for user messages
    table <- janitor::tabyl(var_data, !!rlang::sym(var))
})
```

#### 3. Update Helper Functions

**`.checkDataQuality()` Function Signature:**

```r
# Before
.checkDataQuality = function(data, vars, original_data)

# After
.checkDataQuality = function(data, actual_vars, original_data, display_vars = NULL)
```

**`.generateSummary()` Function Signature:**

```r
# Before
.generateSummary = function(data, vars, original_data, excluded_n)

# After
.generateSummary = function(data, actual_vars, original_data, excluded_n, display_vars = NULL)
```

#### 4. Update Function Calls

```r
# Generate summaries with both actual and display names
if (isTRUE(self$options$showSummary)) {
    private$.generateSummary(data, actual_vars, original_data, excluded_n, selected_vars)
}

# Check data quality with both actual and display names
private$.checkDataQuality(data, actual_vars, original_data, selected_vars)
```

## Key Pattern

This fix establishes a consistent pattern for handling jamovi variable names:

### Two-Name System
- **`actual_vars`**: Column names after `jmvcore::select` processing
  - Use for: data access (`data[[var]]`, `janitor::tabyl()`)
  - Obtained from: `names(data)` after selection

- **`display_vars`** (original `selected_vars`): User-provided variable names
  - Use for: error messages, headers, user-facing text
  - Obtained from: `self$options$vars`

### Implementation Pattern

```r
# 1. Get both sets of names
selected_vars <- self$options$vars
data <- jmvcore::select(self$data, selected_vars)
actual_vars <- names(data)

# 2. Iterate using index to match pairs
for (i in seq_along(actual_vars)) {
    var <- actual_vars[i]          # for data access
    display_var <- selected_vars[i] # for user messages

    # Access data with actual name
    value <- data[[var]]

    # Show user-friendly messages with display name
    message(paste("Processing", display_var))
}
```

## Testing

### Test Case
- **Data:** Variable named `Outcome` (integer with 2 unique values)
- **Table Style:** t4 (janitor)
- **Expected:** Frequency table displays successfully
- **Result:** ✓ Error resolved, table displays correctly

### Edge Cases to Verify
1. ✓ Variables with special characters
2. ✓ Variables with spaces
3. ✓ Variables that start with numbers
4. ✓ Variables with uppercase letters (may be lowercased)
5. ✓ Multiple variables with similar names

## Related Fixes

This fix follows the same pattern as:
- **SURVIVAL_OUTCOME_FIXES.md** - Original variable renaming fix
- **SURVIVAL_NA_FIX_SUMMARY.md** - Variable handling with NA values

## Prevention

Future jamovi module development should:

1. **Always** capture actual column names after `jmvcore::select()`
2. **Always** use a two-name system (actual vs display)
3. **Never** assume variable names remain unchanged after jamovi processing
4. **Test** with variables that have special characters, spaces, or unusual formatting

## Files Modified

- `R/tableone.b.R` (ClinicoPathDescriptives)
- `R/tableone.b.R` (ClinicoPathJamoviModule)

## Commit Message Template

```
fix(tableone): Resolve janitor variable renaming issue

- Fix "argument is of length zero" error in janitor table style (t4)
- Capture actual column names after jmvcore::select processing
- Use actual_vars for data access, display_vars for user messages
- Update .checkDataQuality and .generateSummary to handle both name sets
- Follows same pattern as survival function variable renaming fix

Fixes error when selecting continuous variables with janitor style
```
