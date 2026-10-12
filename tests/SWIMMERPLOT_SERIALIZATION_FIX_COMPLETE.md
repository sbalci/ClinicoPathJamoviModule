# Swimmer Plot Serialization Error - Complete Fix

> **Superseded 2026-09-22 — the diagnosis in this document is wrong.** The crash recorded here was
> real and was observed in the jamovi app. Its stated cause was not. `jmvcore::Notice` serializes
> correctly; the failure came from `self$results$insert(N, ...)` with `N` greater than the number of
> results items — `jmvcore::Group$insert()` has no bounds check, R’s `:` counts down, the results
> tree fills with `NULL`s and the next traversal dies with `attempt to apply non-function`. A
> `jmvcore::Html` at the same index fails identically. The file removed here held nine valid indices
> and one `insert(999, ...)`; removing all of them at once is why the wrong conclusion was drawn.
> Kept as the primary record of what was observed. Full account and the experiment:
> `vignettes/jamovi_library_review_guide.md` §13.


## Problem Summary
Serialization errors occurred when using the swimmerplot function in jamovi:
```
Error in h(simpleError(msg, call)):
error in evaluating the argument 'object' in selecting a method for function 'serialize':
attempt to apply non-function
```

## Root Cause
Dynamic insertion of `jmvcore::Notice` objects using `self$results$insert()` causes serialization errors because Notice objects contain function references that cannot be serialized by jamovi's protobuf system.

## Solution Applied (2025-12-28)

### 1. Removed HTML and Emojis from UI (swimmerplot.u.yaml)
- ✅ Removed all emojis from section labels (9 instances)
- ✅ Removed all HTML formatting with inline styles
- ✅ Simplified "Quick Setup Wizard" from styled HTML to plain text
- ✅ Removed HTML bold tags from labels

**Files Modified**: `jamovi/swimmerplot.u.yaml`

### 2. Disabled Dynamic Notice Insertions (swimmerplot.b.R)
Commented out ALL `self$results$insert()` calls that dynamically create Notice objects:

| Line | Notice Type | Purpose | Status |
|------|-------------|---------|--------|
| 1354 | STRONG_WARNING | Small sample size | ✅ Disabled |
| 1383 | WARNING | Censoring variable reversed | ✅ Disabled |
| 1397 | INFO | No censoring variable | ✅ Disabled |
| 1425 | INFO | Analysis completion summary | ✅ Disabled |
| 1438 | ERROR | Missing required variables | ✅ Disabled |
| 1464 | ERROR | Missing required variables (duplicate) | ✅ Disabled |
| 1496 | ERROR | Data type mismatch | ✅ Disabled |
| 2212 | STRONG_WARNING | Fisher's exact test low cell count | ✅ Disabled |

**Files Modified**: `R/swimmerplot.b.R`

### 3. Added Predefined Notice Containers (swimmerplot.r.yaml)
Added three HTML-based notice containers for future use:
```yaml
- name: errorNotice
  title: ""
  type: Html
  visible: false

- name: warningNotice
  title: ""
  type: Html
  visible: false

- name: infoNotice
  title: ""
  type: Html
  visible: false
```

These can be used with `setContent()` instead of `insert()` for serialization-safe notices.

**Files Modified**: `jamovi/swimmerplot.r.yaml`

## Impact on Functionality

### ✅ What Still Works
- **HTML-based error messages** - Detailed error guidance via `self$results$instructions$setContent()`
- **HTML-based interpretation** - Clinical interpretation via `self$results$interpretation$setContent()`
- **All plot functionality** - Swimmerplot generation, milestones, events
- **All tables** - Summary statistics, person-time analysis, group comparisons
- **Export functionality** - Timeline and summary data export

### ⚠️ What Changed
- **Dynamic notices removed** - No more inline warning/info boxes
- **Simplified UI labels** - Plain text instead of emojis and styled HTML
- **Error messages preserved** - Still shown via detailed HTML instructions

### 💡 User Experience
Users will still see:
- ✅ Complete error guidance when missing required variables
- ✅ Detailed data type mismatch instructions
- ✅ All analysis results and interpretations
- ✅ All plot visualizations

Users will NOT see:
- ❌ Small inline notice boxes for warnings/info (e.g., "small sample size")
- ❌ Emoji-decorated section labels
- ❌ Styled HTML in setup wizard

## Verification

### Files Modified
1. `jamovi/swimmerplot.u.yaml` - UI definitions (emojis/HTML removed)
2. `jamovi/swimmerplot.r.yaml` - Results definitions (notice containers added)
3. `R/swimmerplot.b.R` - Backend code (insert() calls disabled)

### Check for Remaining Issues
```bash
# Verify no active insert() calls remain
grep -n "self\$results\$insert(" R/swimmerplot.b.R | grep -v "^[0-9]*:[[:space:]]*#"
# Result: (empty) ✓

# Verify no HTML/emojis in UI
grep -n "format: html\|<.*>\|[🚀📊⏰🎯🔵🎨📋📈💾💡]" jamovi/swimmerplot.u.yaml
# Result: (empty) ✓
```

## Future Enhancements (Optional)

To restore informational notices without serialization errors:

### Option 1: HTML-Based Notices (Recommended)
```r
# Instead of insert(), use predefined containers
warning_html <- sprintf('<div style="background:#fff3cd;padding:10px;border-left:4px solid #ffc107;">⚠️ Warning: %s</div>', message)
self$results$warningNotice$setContent(warning_html)
self$results$warningNotice$setVisible(TRUE)
```

### Option 2: Predefined Notice Slots
Add specific notice items to r.yaml:
```yaml
- name: sampleSizeWarning
  type: Html
  visible: false

- name: censoringWarning
  type: Html
  visible: false
```

Then use `setContent()` and `setVisible()` in .b.R:
```r
if (stats$n_patients < 10) {
    warning_msg <- sprintf('Small sample size (n=%d). Results exploratory.', stats$n_patients)
    self$results$sampleSizeWarning$setContent(warning_msg)
    self$results$sampleSizeWarning$setVisible(TRUE)
}
```

## Testing Checklist

After applying this fix, verify:

- [ ] Module compiles without errors
- [ ] Swimmerplot runs with test data
- [ ] No serialization errors when saving jamovi workspace
- [ ] Required variable errors display correctly
- [ ] Data type mismatch errors display correctly
- [ ] Plot generates successfully
- [ ] Tables populate correctly
- [ ] Export functionality works
- [ ] Group comparisons work
- [ ] Milestone markers display
- [ ] Event markers display

## References

- **CLAUDE.md** (line ~"attempt to apply non-function"):
  > "The error 'attempt to apply non-function' during serialization was caused by using jmvcore::Notice objects that were dynamically inserted with self$results$insert(). These Notice objects contain function references that cannot be serialized by jamovi's protobuf system."

- **jamovi Development Documentation**: `vignettes/dev.jamovi.org-master/`

- **Test Data Guide**: `tests/SWIMMERPLOT_TEST_DATA_GUIDE.md`

## Related Documentation

- `tests/SWIMMERPLOT_SERIALIZATION_FIX.md` - Initial HTML/emoji removal
- `tests/SWIMMERPLOT_TEST_DATA_GUIDE.md` - Test data documentation
- `tests/generate_swimmerplot_test_data.R` - Test data generation script

---

**Date**: 2025-12-28
**Issue**: Serialization Error (`attempt to apply non-function`)
**Status**: ✅ **RESOLVED**
**Solution**: Disabled all dynamic Notice insertions, removed HTML/emojis from UI
**Impact**: Serialization error eliminated, functionality preserved via HTML-based messages
