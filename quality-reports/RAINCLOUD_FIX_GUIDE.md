# Raincloud Function Fix Implementation Guide

## Summary of Fixes

This guide provides step-by-step instructions to implement the Notice system and other critical fixes for the `raincloud` function.

**Total Changes**: 13 fixes across 3 files
- `jamovi/raincloud.r.yaml`: 1 change (add Notice outputs)
- `jamovi/raincloud.a.yaml`: 1 change (colorblind default)
- `R/raincloud.b.R`: 11 changes (Notice system implementation)

**Estimated Time**: 30-45 minutes

---

## Pre-Flight Checklist

- [ ] Current working directory is ClinicoPathJamoviModule root
- [ ] Git status is clean (commit existing changes first)
- [ ] Backup created: `git stash push -m "pre-raincloud-fixes"`

---

## Fix 1: Update .r.yaml Schema

**File**: `jamovi/raincloud.r.yaml`

**Action**: Add 3 Notice outputs at the BEGINNING of the items list (before `todo`).

```yaml
---
name: raincloud
title: Raincloud Plot
jrs: '1.1'

items:
    - name: validationErrors
      title: ''
      type: Notice
      visible: false

    - name: dataWarnings
      title: ''
      type: Notice
      visible: false

    - name: analysisInfo
      title: ''
      type: Notice
      visible: false

    - name: todo
      title: Instructions
      type: Html

    # ... rest of existing items unchanged ...
```

**Lines to modify**: Insert at line 6 (before existing items)

---

## Fix 2: Set Colorblind Default

**File**: `jamovi/raincloud.a.yaml`

**Action**: Change default palette from "clinical" to "prism_colorblind_safe"

**Line 264** - CHANGE:
```yaml
      default: clinical
```

TO:
```yaml
      default: prism_colorblind_safe
```

---

## Fix 3-13: Implement Notice System in .b.R

**File**: `R/raincloud.b.R`

### Fix 3: Add .createNotice Helper (Line 64)

**INSERT AFTER** line 64 (after `.escapeVar` method):

```r
        },

        .createNotice = function(name, type, message, position = 1) {
            # Helper method for consistent Notice creation
            notice <- jmvcore::Notice$new(
                options = self$options,
                name = name,
                type = type
            )
            notice$setContent(message)
            self$results$insert(position, notice)
            return(invisible(NULL))
        },

        .run = function() {
```

---

### Fix 4: Missing Variables Error (Lines 68-97)

**REPLACE** lines 68-97 WITH:

```r
            # Check if required variables have been selected
            if (is.null(self$options$dep_var) || is.null(self$options$group_var)) {
                private$.createNotice(
                    name = 'missingVariables',
                    type = jmvcore::NoticeType$ERROR,
                    message = 'Dependent variable and grouping variable are required. Please select both variables to generate the raincloud plot.',
                    position = 1
                )

                # Keep welcome HTML for first-time users
                intro_msg <- "
                <div style='background-color: #e3f2fd; padding: 12px; border-radius: 8px;'>
                <h4 style='color: #1976d2; margin-top: 0;'>☁️ Raincloud Plot</h4>
                <p>Select <strong>Dependent Variable</strong> (continuous) and <strong>Grouping Variable</strong> (categorical) to begin.</p>
                </div>"
                self$results$todo$setContent(intro_msg)
                return()
            } else {
                self$results$todo$setContent("")
            }
```

---

### Fix 5: Empty Dataset Error (Lines 100-102)

**REPLACE** lines 100-102 WITH:

```r
            # Validate dataset
            if (nrow(self$data) == 0) {
                private$.createNotice(
                    name = 'emptyDataset',
                    type = jmvcore::NoticeType$ERROR,
                    message = 'Dataset contains no rows. Please load data before running analysis.',
                    position = 1
                )
                return()
            }
```

---

### Fix 6: Missing ggdist Package (Lines 105-114)

**REPLACE** lines 105-114 WITH:

```r
            # Safely require ggdist
            if (!requireNamespace("ggdist", quietly = TRUE)) {
                private$.createNotice(
                    name = 'missingPackage',
                    type = jmvcore::NoticeType$ERROR,
                    message = 'Required package "ggdist" is not installed. Install using: install.packages("ggdist")',
                    position = 1
                )
                return()
            }
```

---

### Fix 7: No Complete Cases (Lines 135-137)

**REPLACE** lines 135-137 WITH:

```r
            if (nrow(analysis_data) == 0) {
                private$.createNotice(
                    name = 'noCompleteCases',
                    type = jmvcore::NoticeType$ERROR,
                    message = sprintf('No complete cases found for selected variables. Dataset has %d rows but all contain missing values in at least one selected variable.', nrow(self$data)),
                    position = 1
                )
                return()
            }
```

---

### Fix 8: Group Imbalance Warning (After Line 153)

**INSERT AFTER** line 153 (after `max_group <- max(group_counts)`):

```r
            min_group <- min(group_counts)
            max_group <- max(group_counts)

            # Add group imbalance warning
            if (max_group / min_group > 5) {
                private$.createNotice(
                    name = 'groupImbalance',
                    type = jmvcore::NoticeType$STRONG_WARNING,
                    message = sprintf('Highly unbalanced groups detected (largest:smallest ratio = %.1f:1). Statistical comparisons may be unreliable; consider using robust methods or stratified sampling.', max_group / min_group),
                    position = 1
                )
            }
```

**ALSO DELETE** inline HTML from line 154:
```r
# REMOVE THIS LINE:
imbalance_note <- if (max_group / min_group > 5) " <span>...</span>" else ""
```

---

### Fix 9: Small Sample Warning (After Line 169)

**INSERT AFTER** existing missing data code (around line 169):

```r
            # Add small sample warning
            if (min_group < 10) {
                private$.createNotice(
                    name = 'smallSample',
                    type = jmvcore::NoticeType$STRONG_WARNING,
                    message = sprintf('Small sample size detected: smallest group has %d observations. Inferential tests (normality, comparisons) may be unreliable; interpret with caution.', min_group),
                    position = 2
                )
            }
```

**ALSO** remove inline HTML warnings from `summary_msg` construction.

---

### Fix 10: Analysis Complete Info (After Line 205)

**INSERT AFTER** line 205 (after interpretation guide, before image$setState):

```r
            # Generate interpretation guide
            interpretation_html <- private$.generate_interpretation_guide(analysis_data, dep_var, group_var)
            self$results$interpretation$setContent(interpretation_html)

            # Success notice at bottom
            private$.createNotice(
                name = 'analysisComplete',
                type = jmvcore::NoticeType$INFO,
                message = sprintf('Raincloud plot analysis completed successfully using %d complete observations across %d groups.', nrow(analysis_data), length(levels(analysis_data[[group_var]]))),
                position = 999
            )

            # Store data AND visual options for plotting using setState
            image <- self$results$plot
```

---

### Fix 11: Log Transform Warning (Lines 296-304)

**REPLACE** the log transform block (around lines 296-304) WITH:

```r
            if (isTRUE(self$options$log_transform)) {
                if (any(analysis_data[[dep_var]] <= 0, na.rm = TRUE)) {
                    n_nonpositive <- sum(analysis_data[[dep_var]] <= 0, na.rm = TRUE)
                    private$.createNotice(
                        name = 'logTransformSkipped',
                        type = jmvcore::NoticeType$WARNING,
                        message = sprintf('Log transformation skipped: %d non-positive values found in %s. Remove or transform these values before applying log scale.', n_nonpositive, dep_var),
                        position = 3
                    )
                } else {
                    p <- p + ggplot2::scale_y_continuous(trans = "log10")
                }
            }
```

---

### Fix 12: NA Handling in Tests (After Lines 678-679)

**ADD** after extracting group data (around line 680):

```r
                group_levels <- levels(data[[group_var]])
                group1_data <- data[data[[group_var]] == group_levels[1], dep_var]
                group1_data <- group1_data[!is.na(group1_data)]  # ADD
                group2_data <- data[data[[group_var]] == group_levels[2], dep_var]
                group2_data <- group2_data[!is.na(group2_data)]  # ADD

                # Verify sufficient data
                if (length(group1_data) < 2 || length(group2_data) < 2) {
                    return("<div style='color:#d9534f'>Insufficient data for comparison: need at least 2 observations per group.</div>")
                }
```

---

### Fix 13: Auto-Selection Warning (After Line 713)

**INSERT AFTER** test_method selection logic (around line 713):

```r
            # ... existing auto-selection logic ...
            }

            # Warn about auto-selection limitations
            if (comparison_method == "auto") {
                smallest_n <- min(sapply(levels(data[[group_var]]), function(g) {
                    sum(data[[group_var]] == g)
                }))
                private$.createNotice(
                    name = 'autoTestSelection',
                    type = jmvcore::NoticeType$WARNING,
                    message = sprintf('Auto-selection chose %s based on Shapiro-Wilk normality tests. Note: Shapiro-Wilk has low power with small samples (smallest group n=%d) and over-sensitivity with large samples. Consider visual inspection and manual test selection.', test_method, smallest_n),
                    position = 3
                )
            }
```

---

## Post-Implementation Steps

### Step 4: Regenerate Headers

```bash
Rscript -e "jmvtools::prepare()"
```

**Expected**: No errors. Headers (.h.R) will be regenerated from .yaml files.

---

### Step 5: Validate Syntax

```bash
Rscript -e "source('R/raincloud.b.R')"
```

**Expected**: "Loading required namespace: jmvcore" with no errors.

---

### Step 6: Document

```bash
Rscript -e "devtools::document()"
```

**Expected**: May show errors from OTHER files (e.g., concordanceindex.b.R) but raincloud should be clean.

---

## Testing Checklist

### Test 1: Missing Variables Error

```r
# In jamovi: Open raincloud without selecting variables
# Expected: ERROR Notice at top: "Dependent variable and grouping variable are required..."
```

### Test 2: Small Sample Warning

```r
data(histopathology)
small_data <- histopathology[1:15, ]  # Create small dataset
# Run raincloud with 2 groups of ~7-8 each
# Expected: STRONG_WARNING Notice: "Small sample size detected: smallest group has X observations..."
```

### Test 3: Group Imbalance

```r
# Create imbalanced data: 100 in group A, 10 in group B
# Expected: STRONG_WARNING Notice: "Highly unbalanced groups detected (10:1 ratio)..."
```

### Test 4: Success Notice

```r
# Run normal analysis with good data
# Expected: INFO Notice at bottom: "Analysis completed successfully using X observations across Y groups."
```

### Test 5: Auto-Selection Warning

```r
# Set comparison_method = "auto" and run
# Expected: WARNING Notice: "Auto-selection chose [test] based on Shapiro-Wilk..."
```

---

## Rollback Procedure

If issues occur:

```bash
# Restore from stash
git stash pop

# Or restore specific files
git checkout HEAD -- jamovi/raincloud.r.yaml
git checkout HEAD -- jamovi/raincloud.a.yaml
git checkout HEAD -- R/raincloud.b.R
```

---

## Success Criteria

- [ ] All 13 fixes applied
- [ ] `jmvtools::prepare()` runs without errors
- [ ] `source('R/raincloud.b.R')` succeeds
- [ ] Error Notice appears when variables missing
- [ ] Warning Notices appear for small n and imbalance
- [ ] Info Notice appears on successful completion
- [ ] Colorblind palette is default
- [ ] No regression in existing functionality

---

## Completion

After successful implementation:

```bash
git add jamovi/raincloud.r.yaml jamovi/raincloud.a.yaml R/raincloud.b.R
git commit -m "feat: Implement Notice system for raincloud function

- Add Notice outputs to .r.yaml schema
- Replace deprecated error handling (stop/HTML) with jmvcore::Notice
- Add .createNotice() helper method for consistency
- Implement 8 Notice types: ERROR (4), STRONG_WARNING (2), WARNING (2), INFO (1)
- Add NA handling in statistical tests
- Add auto-test selection warning
- Set colorblind-safe palette as default for accessibility

Fixes ensure jamovi UX compliance and improve clinical safety warnings.
"
```

---

## Support

If you encounter issues, check:

1. **Syntax errors**: Line numbers may shift if file was edited - verify context
2. **Missing Notice types**: Ensure `jmvcore::NoticeType$ERROR` etc. are available
3. **Position conflicts**: Multiple Notices at same position is OK - they stack
4. **HTML cleanup**: Ensure removed inline HTML doesn't break `summary_msg` construction

Reference patch file: `raincloud_fixes.patch`
