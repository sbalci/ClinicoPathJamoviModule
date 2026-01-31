# groomecompare setRows Error Fix

**Date:** 2026-01-31
**Module:** groomecompare (Groome Staging System Comparison)
**Error:** `'setRows' does not exist in this results element`

---

## Error Description

**Error Message:**
```
Error: 'setRows' does not exist in this results element
private$.run()
self$results$summary$setRows
```

**Location:** `R/groomecompare.b.R` line 175

---

## Root Cause

### The Problem

In jamovi, there are two types of table row definitions:

1. **Fixed-size tables** - `rows: N` (where N > 0)
   - Use `setRow(index, values)` to populate individual rows
   - Number of rows is predetermined and cannot change

2. **Dynamic tables** - `rows: 0`
   - Use `setRows(dataframe)` to populate all rows at once
   - Number of rows can vary based on data

### The Mismatch

**groomecompare.r.yaml** defined:
```yaml
- name: summary
  type: Table
  rows: 5        # Fixed size - WRONG for setRows()

- name: cindexcompare
  type: Table
  rows: 2        # Fixed size - WRONG for setRows()
```

**groomecompare.b.R** was using:
```r
self$results$summary$setRows(summaryData)           # Line 175 - Expects rows: 0
self$results$cindexcompare$setRows(cindexData)      # Line 240 - Expects rows: 0
```

**Result:** Error because `setRows()` only works with dynamic tables (`rows: 0`)

---

## Solution

Changed both tables from fixed-size to dynamic in `groomecompare.r.yaml`:

### Change 1: summary table

**Before:**
```yaml
- name: summary
  title: Comparison Summary
  type: Table
  rows: 5       # Fixed size
  columns:
    - name: criterion
      title: Criterion
      type: text
```

**After:**
```yaml
- name: summary
  title: Comparison Summary
  type: Table
  rows: 0       # Dynamic size
  columns:
    - name: criterion
      title: Criterion
      type: text
```

### Change 2: cindexcompare table

**Before:**
```yaml
- name: cindexcompare
  title: C-Index Comparison
  type: Table
  visible: (cindexcompare)
  rows: 2       # Fixed size
  columns:
```

**After:**
```yaml
- name: cindexcompare
  title: C-Index Comparison
  type: Table
  visible: (cindexcompare)
  rows: 0       # Dynamic size
  columns:
```

---

## Why This Fix Works

### Dynamic Tables (rows: 0)

- Support `setRows(dataframe)` method
- Automatically adjust row count based on data
- More flexible for varying data sizes
- Standard pattern in jamovi when using `setRows()`

### Example from groomecompare.b.R

```r
# Create data frame with 5 rows
summaryData <- data.frame(
    criterion = c("Hazard Consistency", "Hazard Discrimination",
                  "Sample Balance", "Outcome Prediction", "Overall Rank"),
    system1 = c(metrics1$consistency, metrics1$discrimination,
               metrics1$balance, metrics1$prediction, metrics1$overall),
    system2 = c(metrics2$consistency, metrics2$discrimination,
               metrics2$balance, metrics2$prediction, metrics2$overall),
    stringsAsFactors = FALSE
)

# With rows: 0, this now works correctly
self$results$summary$setRows(summaryData)
```

---

## Other Tables in groomecompare

All other tables were already correctly defined as dynamic (`rows: 0`):

✅ **Correct (already dynamic):**
- `consistency` - rows: 0
- `discrimination` - rows: 0
- `hrs1` - rows: 0
- `hrs2` - rows: 0
- `samplesize` - rows: 0
- `bootstrap` - rows: 0

---

## jamovi Table Patterns Reference

### When to Use rows: 0 (Dynamic)

Use when:
- Row count varies based on data
- Using `setRows(dataframe)` in .b.R
- Don't know exact row count in advance
- **This is the most common pattern**

```yaml
# .r.yaml
- name: resultstable
  type: Table
  rows: 0          # Dynamic
  columns:
    - name: col1
      type: text
```

```r
# .b.R
data <- data.frame(col1 = c("A", "B", "C"))
self$results$resultstable$setRows(data)  # Works!
```

### When to Use rows: N (Fixed)

Use when:
- Exact row count is known and fixed
- Using `setRow(index, values)` in .b.R for each row
- **Less common pattern**

```yaml
# .r.yaml
- name: fixedtable
  type: Table
  rows: 3          # Fixed - exactly 3 rows
  columns:
    - name: col1
      type: text
```

```r
# .b.R
self$results$fixedtable$setRow(rowNo=1, values=list(col1="A"))
self$results$fixedtable$setRow(rowNo=2, values=list(col1="B"))
self$results$fixedtable$setRow(rowNo=3, values=list(col1="C"))
```

---

## Common Error Patterns

### Error 1: Using setRows() with fixed-size table

```yaml
# .r.yaml
rows: 5          # Fixed size
```

```r
# .b.R
self$results$table$setRows(data)  # ❌ ERROR: 'setRows' does not exist
```

**Fix:** Change to `rows: 0`

### Error 2: Using setRow() with dynamic table index out of bounds

```yaml
# .r.yaml
rows: 0          # Dynamic
```

```r
# .b.R
self$results$table$setRow(rowNo=1, values=list(...))  # ⚠️ Won't error but awkward
```

**Better:** Use `setRows()` with dynamic tables

---

## Validation

**Command:** `jmvtools::prepare()`
**Result:** ✅ Successful - No errors or warnings

**Files Modified:**
- `jamovi/groomecompare.r.yaml` (lines 16 and 192)

**Files Auto-Regenerated:**
- `R/groomecompare.h.R`
- `jamovi/groomecompare.src.js`

---

## Testing Recommendations

### Test Case 1: Basic Functionality
**Action:** Run groomecompare with two staging systems
**Expected:** Summary table populates with 5 rows (Hazard Consistency, etc.)
**Reason:** setRows() now works with rows: 0

### Test Case 2: C-Index Comparison
**Action:** Enable C-Index comparison option
**Expected:** C-Index table populates with 2 rows (one per system)
**Reason:** setRows() now works with rows: 0

### Test Case 3: Missing Inputs
**Action:** Open groomecompare without selecting variables
**Expected:** Instructions visible, no errors
**Reason:** Table definitions don't affect visibility logic

---

## Related jamovi Best Practices

### 1. Default to Dynamic Tables
Unless you have a specific reason for fixed-size tables, always use `rows: 0`.

### 2. Match .r.yaml to .b.R
- If using `setRows(dataframe)` → use `rows: 0`
- If using `setRow(index, values)` → use `rows: N`

### 3. Check All Table Definitions
When creating new functions, verify all tables use `rows: 0` unless specifically needed.

### 4. Common Patterns in ClinicoPath
Most ClinicoPath tables use `rows: 0` for flexibility:
- Survival analysis results (varying by stages/groups)
- Cross-tabulations (varying by factor levels)
- Model results (varying by covariates)

---

## Additional Notes

### Why Were These Originally Fixed-Size?

The original definition with `rows: 5` and `rows: 2` may have been:
1. An early design decision (before data structure was clear)
2. Copy-pasted from a different pattern
3. Confusion about when to use fixed vs. dynamic

### Why Dynamic is Better Here

1. **Flexibility**: If algorithm changes to produce different row counts, no .r.yaml change needed
2. **Simplicity**: `setRows(dataframe)` is simpler than multiple `setRow()` calls
3. **Consistency**: Matches pattern used in other ClinicoPath tables
4. **Maintainability**: Easier to update data structure in one place (.b.R)

---

**Status:** ✅ FIXED

The `setRows()` error in groomecompare has been resolved by converting fixed-size tables to dynamic tables in the .r.yaml definition.
