# DateTime Converter Module: Critical Clinical Safety Issues

## Executive Summary

The `datetimeconverter` module has been identified as **not ready for clinical release**. While it implements proper jamovi Notices (8 distinct notice types) and has attractive preview outputs, several critical safety issues would cause silent failures in real-world clinical data. This document catalogs the issues and provides a roadmap for clinical readiness.

---

## Current Status

**Build Status**: ✅ Compiles successfully
**Notice Compliance**: ✅ Full jamovi Notice implementation (8 notices)
**Clinical Safety**: ❌ **BLOCKED** - 5 critical issues
**Test Coverage**: ⚠️ Happy path only, missing failure mode tests
**Recommendation**: **EXPERIMENTAL USE ONLY** until issues are resolved

---

## Critical Issues Identified

### 1. ❌ CRITICAL: Excel/Unix Serial Number Detection Failure

**Problem**: Auto-detection only runs when the incoming column is strictly numeric (R/datetimeconverter.b.R:171-238). As soon as a CSV column contains even one blank or "—", R/Jamovi coerces the field to character, and that branch is skipped. The code falls back to text parsers that default to ymd. The result is an all-NA conversion with only a generic warning.

**Real-World Impact**:
```
# Common Excel export scenario
patient_id,admission_date
1,44562
2,44563
3,           <-- blank causes character coercion
4,44565
```

After CSV import to jamovi:
- Column type: character (due to blank in row 3)
- Serial detection: SKIPPED
- Text parser: Interprets "44562" as string, tries ymd format
- Result: All NA conversions with generic "low success rate" warning
- **Clinical Risk**: All admission dates lost, no specific error about serial numbers

**Current Behavior**:
```r
# R/datetimeconverter.b.R:171-238
if (is.numeric(datetime_vals)) {
    # Excel/Unix detection runs HERE
    # But only if column is purely numeric
}
# If character type, this branch is skipped entirely
```

**Required Fix**:
- Detect digit-only character vectors (e.g., `grepl("^\\d+(\\.\\d+)?$", x)`)
- Detect majority-numeric columns (>80% parseable as numbers)
- Emit specific ERROR notice: "Column appears to contain Excel serial dates but includes non-numeric values"
- Provide actionable guidance about cleaning blanks/special characters

---

### 2. ❌ CRITICAL: Missing Variable Validation

**Problem**: There is no guard that the selected variable still exists in the dataset. `.run()` grabs `data[[datetime_var]]` without checking `datetime_var %in% names(data)` (R/datetimeconverter.b.R:1002-1051), and later attempts to push zero-length vectors into the output columns (R/datetimeconverter.b.R:1224-1286).

**Real-World Impact**:
```
Clinical Scenario:
1. Analyst selects "admission_date" column
2. Dataset is refreshed/reimported with different column names
3. "admission_date" no longer exists
4. Module runs without error check
5. Produces opaque jmvcore length-mismatch error
6. Clinician has no idea what went wrong
```

**Current Behavior**:
```r
# R/datetimeconverter.b.R:1002-1051
datetime_vals <- data[[datetime_var]]  # No existence check
# Later...
if (length(datetime_vals) == 0) {
    # Tries to push zero-length vectors to outputs
    # Results in cryptic jmvcore error
}
```

**Error Message User Sees**:
```
Error: Length mismatch
(No indication that the column doesn't exist)
```

**Required Fix**:
- Add explicit validation: `if (!datetime_var %in% names(data))`
- Emit ERROR notice with available column names
- Similar to datecorrection validation (R/datecorrection.b.R:133-151)
- Return early to prevent downstream errors

---

### 3. ❌ CRITICAL: Limited Timezone Support

**Problem**: Time zone handling is limited to "system" or "UTC" (R/datetimeconverter.h.R:33-45). Multi-centre trials frequently require precise local zones (Europe/Istanbul, America/New_York, etc.). Forcing analysts to change their OS timezone before running the module invites hour-level shifts in infusion/surgery timestamps with no audit trail.

**Real-World Impact**:
```
Multi-Center Trial Scenario:
- Site 1 (New York): Surgery timestamp "2023-06-15 14:30"
- Site 2 (Istanbul): Surgery timestamp "2023-06-15 21:30"
- Analyst (London): Must convert both to comparable times
- Current options: "system" (London) or "UTC"
- Result: Cannot properly align site-specific timestamps
- Risk: Time-to-event calculations are off by hours
```

**Clinical Examples**:
- **Infusion timestamps**: Drug administration times must be exact for pharmacokinetics
- **Surgery start/end times**: Operating room efficiency analysis
- **ICU admission times**: Critical for time-to-intervention metrics
- **Multi-center trials**: Coordinating events across continents

**Current Limitation**:
```yaml
# jamovi/datetimeconverter.a.yaml:96-109
- name: timezone
  type: List
  options:
    - name: system
      title: System Default
    - name: utc
      title: UTC
```

**Required Fix**:
- Replace List with String (text input)
- Support full Olson timezone identifiers (e.g., "America/New_York", "Europe/Istanbul")
- Validate against `OlsonNames()` with helpful error for invalid zones
- Surface applied timezone in ALL preview/output tables for audit trail
- Default to "UTC" for safety (timezone-agnostic)

---

### 4. ❌ CRITICAL: Unsafe Auto-Detection Algorithm

**Problem**: Auto-detection trusts the first 20 non-missing values and adopts the first format with >80% matches (R/datetimeconverter.b.R:56-75). Ambiguous strings such as 01/02/2023, 02/03/2023 will happily be treated as US format (MDY) if they happen to appear first, and every subsequent day/month pair is flipped with no conflict notice.

**Real-World Impact**:
```
Dataset: diagnosis_dates
01/02/2023  <- Could be Jan 2 or Feb 1
02/03/2023  <- Could be Feb 3 or Mar 2
03/04/2023  <- Could be Mar 4 or Apr 3
...
12/11/2023  <- Could be Dec 11 or Nov 12

Auto-Detection:
- Tries MDY first
- All values parse successfully (>80% threshold met)
- Returns "mdy" format
- Result: Every date is wrong if data was actually DMY
- NO conflict notice generated
```

**Clinical Risk**:
- **Treatment dates**: Wrong month = wrong time-to-treatment calculation
- **Diagnosis dates**: Wrong month = wrong survival analysis
- **Follow-up dates**: Wrong month = wrong censoring
- **Trial endpoints**: Wrong month = wrong efficacy assessment

**Current Behavior**:
```r
# R/datetimeconverter.b.R:56-75
# Takes first 20 non-missing values
# Tries formats in fixed order: ymd, dmy, mdy, ...
# Returns first format where >80% parse successfully
# NO check if multiple formats succeed
# NO warning about ambiguous dates
```

**Required Fix**:
- Run detection with BOTH dmy and mdy (and other ambiguous pairs)
- Compare results: Do both succeed with >80% match?
- If yes: Emit STRONG_WARNING about ambiguous format
- Include sample comparisons in notice:
  ```
  Ambiguous date format detected!
  Example: "01/02/2023"
    - As DMY: 2023-02-01 (February 1)
    - As MDY: 2023-01-02 (January 2)

  Please manually select the correct format to avoid errors.
  ```
- Consider requiring manual format selection when ambiguity detected

---

### 5. ❌ CRITICAL: R API Incompleteness

**Problem**: The exported R API cannot reproduce Jamovi behavior. The public `datetimeconverter()` signature (R/datetimeconverter.h.R:589-640) exposes preview/format options but NOT the `corrected_datetime_*` or component output toggles that the analysis actually honors (see their use in tests at tests/testthat/test-datetimeconverter.R:24-118). Scripted pipelines therefore cannot emit the corrected columns even though clinicians are expected to rely on them.

**Real-World Impact**:
```r
# In Jamovi UI:
# User checks "Add Corrected DateTime (Text)" toggle
# Result: New column "admission_date_corrected" appears in dataset

# In R script (trying to reproduce):
results <- datetimeconverter(
    data = study_data,
    datetime_var = "admission_date",
    datetime_format = "dmy"
    # NO WAY to request corrected_datetime_char output!
)
# Result: No new columns added to dataset
# Workflow is not reproducible
```

**Current Signature**:
```r
# R/datetimeconverter.h.R:589-640
datetimeconverter <- function(
    data,
    datetime_var,
    datetime_format = "auto",
    timezone = "system",
    preview_rows = 20,
    extract_year = FALSE,
    extract_month = FALSE,
    # ... preview extraction options ...
    show_quality_metrics = FALSE,
    show_summary = FALSE
    # MISSING: corrected_datetime_char = FALSE
    # MISSING: corrected_datetime_numeric = FALSE
    # MISSING: year_out = FALSE
    # MISSING: month_out = FALSE
    # etc. (all Output toggles missing)
) { ... }
```

**Missing Parameters**:
- `corrected_datetime_char` - Add corrected datetime as text to dataset
- `corrected_datetime_numeric` - Add corrected datetime as numeric to dataset
- `year_out` - Add year component to dataset
- `month_out` - Add month component to dataset
- `monthname_out` - Add month name to dataset
- `day_out` - Add day to dataset
- `hour_out`, `minute_out`, `second_out` - Time components
- `dayname_out`, `weeknum_out`, `quarter_out`, `dayofyear_out` - Derived components

**Required Fix**:
- Add all Output toggles to function signature
- Ensure R API and Jamovi UI have identical behavior
- Update tests to use public API instead of internal `.run()`
- Document reproducible workflow in examples

---

## Test Coverage Gaps

### Current Test Suite (tests/testthat/test-datetimeconverter.R:1-120)

**What's Covered** (✅ Happy Path):
- ISO date formats (YYYY-MM-DD)
- Mixed separators (/, -, ., space)
- Text month names
- Numeric-only Excel serial dates
- Component extraction
- Format specification

**What's MISSING** (❌ Failure Modes):
1. **Character-based serial numbers**: "44562" as string
2. **Mixed numeric/character columns**: Serial numbers with blanks
3. **Missing/renamed columns**: Variable no longer exists
4. **Multi-timezone data**: Site-specific timestamps
5. **Ambiguous date formats**: 01/02/2023 (DMY vs MDY)
6. **Format detection conflicts**: Both dmy and mdy succeed
7. **Public API completeness**: Output toggle behavior
8. **Edge cases**:
   - All blanks in serial column
   - Invalid timezone identifier
   - Empty dataset with valid column name
   - Column exists but is all NA

**Required Test Cases**:
```r
# Test 19: Character-based Excel serial numbers
test_that("Character vector with digit-only values detected as Excel serial", { ... })

# Test 20: Mixed serial numbers with blanks cause specific error
test_that("Excel serial column with blanks emits helpful error", { ... })

# Test 21: Missing variable detection
test_that("Error raised with available column names when variable missing", { ... })

# Test 22: Olson timezone validation
test_that("Invalid timezone identifier rejected with helpful message", { ... })

# Test 23: Ambiguous format detection
test_that("Ambiguous dates (01/02/2023) emit STRONG_WARNING", { ... })

# Test 24: Public API output toggles
test_that("R API can request corrected datetime outputs", { ... })
```

---

## Files Requiring Modification

### Core Implementation
- **R/datetimeconverter.b.R**
  - Expand `.prepareDatetimeInput()` for character-based serial detection
  - Add `datetime_var` existence validation at start of `.run()`
  - Enhance auto-detection to compare dmy/mdy results
  - Validate timezone against `OlsonNames()`

- **R/datetimeconverter.h.R** (auto-generated, edit source)
  - Add Output toggle parameters to exported function signature
  - Update documentation with timezone options

### Configuration
- **jamovi/datetimeconverter.a.yaml**
  - Replace `timezone` List with String for Olson identifiers
  - Add default value "UTC"
  - Update description with timezone examples

### Testing
- **tests/testthat/test-datetimeconverter.R**
  - Add 6+ new test cases for failure modes
  - Update existing tests to use public API
  - Add timezone validation tests
  - Add ambiguous format detection tests

### Documentation
- **jamovi/datetimeconverter.a.yaml** (description field)
  - Mark as "EXPERIMENTAL" until fixes complete
  - Add warning about current limitations
  - Reference this document

- **man/datetimeconverter.Rd** (auto-generated from .h.R)
  - Update examples with timezone specifications
  - Add warnings about ambiguous date formats

---

## Implementation Priority

| Priority | Issue | Impact | Effort | Risk if Unfixed |
|----------|-------|--------|--------|-----------------|
| 🔴 P0 | Missing variable validation | High | Low | Cryptic errors, frustrated users |
| 🔴 P0 | Excel serial detection failure | High | Medium | Silent data loss, all-NA conversions |
| 🟠 P1 | Ambiguous format detection | High | Medium | Wrong dates, flipped day/month |
| 🟠 P1 | R API incompleteness | Medium | Low | Non-reproducible workflows |
| 🟡 P2 | Timezone support | Medium | Medium | Multi-center trial issues |

**Suggested Implementation Order**:
1. **Issue 2**: Missing variable validation (quick win, prevents cryptic errors)
2. **Issue 1**: Excel serial detection (high impact, moderate effort)
3. **Issue 4**: Ambiguous format detection (prevents wrong date interpretation)
4. **Issue 5**: R API completeness (reproducibility requirement)
5. **Issue 3**: Timezone support (multi-center trial enabler)

---

## Detailed Fix Specifications

### Fix 1: Expand Serial Number Detection

**Location**: R/datetimeconverter.b.R `.prepareDatetimeInput()` method

**Current Code** (lines 171-238):
```r
if (is.numeric(datetime_vals)) {
    # Excel/Unix detection logic
}
```

**Required Changes**:
```r
# Step 1: Check if strictly numeric
if (is.numeric(datetime_vals)) {
    # Existing Excel/Unix logic

# Step 2: Check if character vector with digit-only values
} else if (is.character(datetime_vals)) {
    # Count how many values are digit-only (potential serials)
    digit_only <- grepl("^\\d+(\\.\\d+)?$", datetime_vals, perl = TRUE)
    digit_only_prop <- sum(digit_only, na.rm = TRUE) / length(datetime_vals[!is.na(datetime_vals)])

    if (digit_only_prop > 0.8) {
        # Majority are digit-only: likely Excel serials as text
        notice <- jmvcore::Notice$new(
            options = self$options,
            name = 'excelSerialAsCharacter',
            type = jmvcore::NoticeType$ERROR
        )
        notice$setContent(sprintf(
            'Column "%s" appears to contain Excel serial dates but includes non-numeric values.\n• %d of %d values are digit-only numbers.\n• Non-numeric values: %s\n\nFix options:\n• Remove blank rows and special characters from Excel before export\n• Use "Save As CSV" instead of copy-paste\n• Convert Excel column to text format: TEXT(A1, "YYYY-MM-DD")',
            datetime_var,
            sum(digit_only, na.rm = TRUE),
            length(datetime_vals),
            paste(head(datetime_vals[!digit_only & !is.na(datetime_vals)], 3), collapse = ", ")
        ))
        self$results$insert(1, notice)
        return(NULL)
    }
}
```

**New Notice**:
- **Name**: `excelSerialAsCharacter`
- **Type**: ERROR
- **Trigger**: Character column with >80% digit-only values
- **Content**: Specific guidance about Excel serial detection failure

---

### Fix 2: Add Variable Existence Validation

**Location**: R/datetimeconverter.b.R `.run()` method (before line 1002)

**Current Code**:
```r
datetime_vals <- data[[datetime_var]]  # No validation
```

**Required Changes**:
```r
# Validate datetime_var exists in dataset
if (is.null(datetime_var) || length(datetime_var) == 0 || datetime_var == "") {
    notice <- jmvcore::Notice$new(
        options = self$options,
        name = 'noVariableSelected',
        type = jmvcore::NoticeType$ERROR
    )
    notice$setContent('No datetime variable selected.\n• Please select a variable containing datetime information from the left panel.\n• Use the "DateTime Variable" dropdown to choose a column.')
    self$results$insert(1, notice)
    return()
}

if (!datetime_var %in% names(data)) {
    available_vars <- names(data)
    available_preview <- if (length(available_vars) > 10) {
        paste(paste(head(available_vars, 10), collapse = ", "), "...")
    } else {
        paste(available_vars, collapse = ", ")
    }

    notice <- jmvcore::Notice$new(
        options = self$options,
        name = 'variableNotFound',
        type = jmvcore::NoticeType$ERROR
    )
    notice$setContent(sprintf(
        'Selected variable "%s" not found in dataset.\n• The column may have been renamed or removed.\n• Please select a different variable from the left panel.\n\nAvailable variables: %s',
        datetime_var,
        available_preview
    ))
    self$results$insert(1, notice)
    return()
}

datetime_vals <- data[[datetime_var]]  # Now safe to access
```

**New Notices**:
- **Name**: `noVariableSelected`, `variableNotFound`
- **Type**: ERROR
- **Position**: Top (insert 1)

---

### Fix 3: Implement Olson Timezone Support

**Location**: jamovi/datetimeconverter.a.yaml

**Current Configuration**:
```yaml
- name: timezone
  title: Timezone
  type: List
  options:
    - name: system
      title: System Default
    - name: utc
      title: UTC
  default: system
```

**Required Changes**:
```yaml
- name: timezone
  title: Timezone (Olson Identifier)
  type: String
  default: UTC
  description:
      R: >
        Timezone for datetime parsing using Olson identifier (e.g., "America/New_York", "Europe/Istanbul", "Asia/Tokyo").
        Use "UTC" for timezone-independent timestamps (recommended for multi-center trials).
        Use "" (empty string) for system default (not recommended for reproducibility).
        See OlsonNames() for full list of valid identifiers.
```

**Location**: R/datetimeconverter.b.R

**Validation Code** (add to `.run()` after variable validation):
```r
# Validate timezone
tz <- self$options$timezone
if (is.null(tz) || tz == "" || tolower(tz) == "system") {
    tz <- Sys.timezone()
} else if (tolower(tz) == "utc") {
    tz <- "UTC"
} else {
    # Validate against OlsonNames
    valid_zones <- OlsonNames()
    if (!tz %in% valid_zones) {
        notice <- jmvcore::Notice$new(
            options = self$options,
            name = 'invalidTimezone',
            type = jmvcore::NoticeType$ERROR
        )

        # Suggest close matches
        suggestions <- agrep(tz, valid_zones, max.distance = 0.3, value = TRUE)
        suggestion_text <- if (length(suggestions) > 0) {
            sprintf('\n\nDid you mean:\n• %s', paste(head(suggestions, 5), collapse = '\n• '))
        } else {
            ''
        }

        notice$setContent(sprintf(
            'Invalid timezone identifier: "%s"\n• Please use a valid Olson timezone identifier.\n• Examples: "UTC", "America/New_York", "Europe/Istanbul", "Asia/Tokyo"\n• See OlsonNames() in R for complete list.%s',
            tz,
            suggestion_text
        ))
        self$results$insert(1, notice)
        return()
    }
}
```

**Update Parsing Calls**:
```r
# Ensure all lubridate calls use validated timezone
parsed_dates <- lubridate::parse_date_time(datetime_vals, orders = format, tz = tz)
```

**Update Preview Tables**: Add timezone column to show applied timezone for audit trail

---

### Fix 4: Enhance Ambiguous Format Detection

**Location**: R/datetimeconverter.b.R `.autoDetectFormat()` method

**Current Logic**:
```r
# Tries formats in order, returns first with >80% success
```

**Required Changes**:
```r
.autoDetectFormat = function(datetime_vals) {
    # ... existing code ...

    # NEW: Test ambiguous format pairs
    ambiguous_pairs <- list(
        c("dmy", "mdy"),
        c("ymd", "ydm"),
        c("dmy_HMS", "mdy_HMS")
    )

    ambiguity_detected <- FALSE
    ambiguous_examples <- list()

    for (pair in ambiguous_pairs) {
        format1 <- pair[1]
        format2 <- pair[2]

        # Try both formats
        parsed1 <- lubridate::parse_date_time(sample_dates, orders = format1, quiet = TRUE)
        parsed2 <- lubridate::parse_date_time(sample_dates, orders = format2, quiet = TRUE)

        success1 <- sum(!is.na(parsed1))
        success2 <- sum(!is.na(parsed2))

        # Both succeed with >80% match = AMBIGUOUS
        if (success1 / length(sample_dates) > 0.8 &&
            success2 / length(sample_dates) > 0.8) {

            ambiguity_detected <- TRUE

            # Find examples where they differ
            different_indices <- which(!is.na(parsed1) & !is.na(parsed2) & parsed1 != parsed2)
            if (length(different_indices) > 0) {
                idx <- different_indices[1]
                ambiguous_examples[[length(ambiguous_examples) + 1]] <- list(
                    original = sample_dates[idx],
                    format1 = format1,
                    result1 = as.character(parsed1[idx]),
                    format2 = format2,
                    result2 = as.character(parsed2[idx])
                )
            }
        }
    }

    # Emit STRONG_WARNING if ambiguity detected
    if (ambiguity_detected && length(ambiguous_examples) > 0) {
        example <- ambiguous_examples[[1]]

        notice <- jmvcore::Notice$new(
            options = self$options,
            name = 'ambiguousFormatDetected',
            type = jmvcore::NoticeType$STRONG_WARNING
        )
        notice$setContent(sprintf(
            'AMBIGUOUS DATE FORMAT DETECTED!\n\nExample: "%s"\n• As %s: %s\n• As %s: %s\n\nAuto-detection cannot reliably determine the correct format.\n\nRequired action:\n• Manually select the correct format from the "DateTime Format" dropdown\n• Verify sample conversions in the preview table\n• Do NOT rely on auto-detection for clinical analysis',
            example$original,
            toupper(example$format1), example$result1,
            toupper(example$format2), example$result2
        ))
        self$results$insert(1, notice)  # Top priority warning
    }

    # ... rest of existing detection logic ...
}
```

**New Notice**:
- **Name**: `ambiguousFormatDetected`
- **Type**: STRONG_WARNING (clinical risk)
- **Position**: Top (insert 1)
- **Shows**: Concrete examples of different interpretations

---

### Fix 5: Expose Output Toggles in R API

**Location**: R/datetimeconverter.h.R (edit source YAML, regenerate)

**jamovi/datetimeconverter.a.yaml** - Options are already defined (lines 249-343), just need to be exposed in function signature

**Location**: jmvtools internals will auto-generate the signature based on .a.yaml

**Current Signature** (R/datetimeconverter.h.R:589-640):
```r
datetimeconverter <- function(
    data,
    datetime_var,
    datetime_format = "auto",
    timezone = "system",
    preview_rows = 20,
    extract_year = FALSE,
    # ... preview options ...
    show_quality_metrics = FALSE
) { ... }
```

**Required Signature** (should be auto-generated from .a.yaml):
```r
datetimeconverter <- function(
    data,
    datetime_var,
    datetime_format = "auto",
    timezone = "UTC",
    preview_rows = 20,

    # Preview extraction (existing)
    extract_year = FALSE,
    extract_month = FALSE,
    extract_monthname = FALSE,
    extract_day = FALSE,
    extract_hour = FALSE,
    extract_minute = FALSE,
    extract_second = FALSE,
    extract_dayname = FALSE,
    extract_weeknum = FALSE,
    extract_quarter = FALSE,
    extract_dayofyear = FALSE,

    # Display options (existing)
    show_quality_metrics = FALSE,
    show_summary = FALSE,
    show_explanations = FALSE,
    show_glossary = FALSE,

    # NEW: Output toggles (add to data)
    corrected_datetime_char = FALSE,
    corrected_datetime_numeric = FALSE,
    year_out = FALSE,
    month_out = FALSE,
    monthname_out = FALSE,
    day_out = FALSE,
    hour_out = FALSE,
    minute_out = FALSE,
    second_out = FALSE,
    dayname_out = FALSE,
    weeknum_out = FALSE,
    quarter_out = FALSE,
    dayofyear_out = FALSE
) { ... }
```

**Note**: If jmvtools doesn't automatically include Output type options in the function signature, we'll need to investigate jamovi compiler settings or manually add them to the generated .h.R file (though this is not ideal as it's auto-generated).

---

## Clinical Safety Checklist (Post-Fix)

| Safety Requirement | Current Status | Target Status |
|-------------------|---------------|---------------|
| Variable existence validation | ❌ MISSING | ✅ REQUIRED |
| Excel serial detection (numeric) | ✅ WORKS | ✅ MAINTAIN |
| Excel serial detection (character) | ❌ FAILS | ✅ REQUIRED |
| Timezone flexibility (multi-center) | ❌ LIMITED | ✅ REQUIRED |
| Ambiguous format detection | ❌ MISSING | ✅ REQUIRED |
| R API completeness | ❌ INCOMPLETE | ✅ REQUIRED |
| Failure mode test coverage | ❌ MISSING | ✅ REQUIRED |
| Clinical documentation | ⚠️ GENERIC | ✅ SPECIFIC |

---

## Testing Strategy

### New Test Files
- `tests/testthat/test-datetimeconverter-failures.R` - Failure mode coverage
- `tests/testthat/test-datetimeconverter-timezones.R` - Multi-timezone scenarios
- `tests/testthat/test-datetimeconverter-ambiguous.R` - Ambiguous format detection

### Test Data
```r
# Character-based Excel serials with blanks
test_data_serial_blanks <- data.frame(
    patient_id = 1:5,
    admission = c("44562", "44563", "", "44565", "44566")
)

# Ambiguous date formats
test_data_ambiguous <- data.frame(
    event_date = c("01/02/2023", "02/03/2023", "03/04/2023", "12/11/2023")
)

# Multi-timezone data
test_data_multisite <- data.frame(
    site = c("New York", "Istanbul", "Tokyo"),
    surgery_time = c("2023-06-15 14:30", "2023-06-15 21:30", "2023-06-16 03:30"),
    site_tz = c("America/New_York", "Europe/Istanbul", "Asia/Tokyo")
)
```

---

## Verification Commands

### After Fixes
```bash
# Rebuild module
Rscript -e "jmvtools::prepare()"

# Generate documentation
Rscript -e "devtools::document()"

# Run full test suite
Rscript -e "devtools::test()"

# Run specific failure mode tests
Rscript -e "testthat::test_file('tests/testthat/test-datetimeconverter-failures.R')"
```

### Manual Testing Checklist
- [ ] Character column with Excel serials + blanks
- [ ] Missing/renamed variable
- [ ] Invalid timezone identifier
- [ ] Ambiguous date format (01/02/2023)
- [ ] Multi-center timezone conversion
- [ ] R API with output toggles
- [ ] Large dataset (1000+ rows)

---

## Timeline and Effort Estimate

| Task | Effort | Dependencies |
|------|--------|--------------|
| Fix 2: Variable validation | 1 hour | None |
| Fix 1: Serial detection | 3 hours | None |
| Fix 4: Ambiguous detection | 4 hours | Fix 1 |
| Fix 5: R API exposure | 2 hours | jmvtools investigation |
| Fix 3: Timezone support | 3 hours | None |
| Test suite expansion | 4 hours | All fixes |
| Documentation updates | 2 hours | All fixes |
| **TOTAL** | **19 hours** | ~2.5 days |

---

## Status Tracking

- [ ] Issue 1: Excel serial detection expanded
- [ ] Issue 2: Variable validation added
- [ ] Issue 3: Timezone support implemented
- [ ] Issue 4: Ambiguous format detection added
- [ ] Issue 5: R API completed
- [ ] Test suite expanded
- [ ] Documentation updated
- [ ] Clinical review passed
- [ ] **EXPERIMENTAL** status removed

---

## Recommendations

### Immediate Actions (Before Any Clinical Use)
1. Add **EXPERIMENTAL** banner to UI
2. Document known limitations in help text
3. Implement Fix 2 (variable validation) - quick win
4. Create this document for transparency

### Short-Term (Next Sprint)
1. Implement Fixes 1, 2, 4 (core safety)
2. Expand test suite
3. Internal testing with real clinical data

### Medium-Term (Before v1.0 Release)
1. Implement Fixes 3, 5 (feature completeness)
2. External clinical validation study
3. User documentation with clinical examples
4. Peer review by clinical data managers

### Long-Term (Continuous Improvement)
1. Monitor real-world usage patterns
2. Collect user feedback on edge cases
3. Add support for more exotic datetime formats
4. Performance optimization for very large datasets

---

## References

### Related Documents
- `vignettes/jamovi_notices_guide.md` - Notice implementation guidelines
- `DATECORRECTION_CLINICAL_SAFETY_FIXES.md` - Similar safety review for datecorrection
- `DATECORRECTION_NOTICES_IMPLEMENTATION.md` - Notice system patterns

### External Standards
- ISO 8601 (datetime formats)
- IANA Timezone Database (Olson identifiers)
- ICH E6 GCP Guidelines (clinical data integrity)

---

**Document Version**: 1.0
**Date**: 2025-01-13
**Author**: Clinical Safety Code Review
**Status**: Issues documented, fixes pending
**Next Review**: After implementation of all 5 critical fixes
