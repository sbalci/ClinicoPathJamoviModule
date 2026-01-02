---
name: checkpoint
description: Auto-detect and add checkpoint calls before expensive operations
interactive: true
args:
  function_name:
    description: Name of the jamovi function to add checkpoints to
    required: true
    autocomplete: functions
  --auto-detect:
    description: Automatically detect expensive operations (default true)
    required: false
    default: true
  --manual-positions:
    description: Comma-separated line numbers for manual checkpoint placement
    required: false
  --dry-run:
    description: Preview checkpoint placements without modifying files
    required: false
    default: false
  --aggressive:
    description: Add checkpoints more liberally (may add too many)
    required: false
    default: false
usage: /checkpoint <function_name> [--auto-detect] [--dry-run]
examples:
  /checkpoint tableone                          # Auto-detect and add checkpoints
  /checkpoint survival --dry-run                # Preview placements
  /checkpoint diagnostic --manual-positions=145,220,340
---

# Smart Checkpoint Insertion with Auto-Detection

Add `private$.checkpoint()` calls to the specified function before computationally expensive operations. This enables:

1. **Incremental Results**: Pushes computed results to Jamovi so users see tables fill gradually
2. **Change Detection**: Aborts current run if user modifies settings, avoiding wasted computation
3. **Better UX**: Users see progress instead of blank screens during long computations

## Auto-Detection Algorithm

When `--auto-detect=true` (default), the command analyzes the code to identify expensive operations:

### High-Priority Triggers (always add checkpoint)
1. **Loops over data rows/groups**
   - `for (i in 1:nrow(...))`
   - `for (group in unique(...))`
   - `lapply()`, `sapply()` over large vectors

2. **Statistical model fitting**
   - `lm()`, `glm()`, `coxph()`, `survreg()`
   - `lmer()`, `glmer()` (mixed models)
   - `roc()`, `auc()` (ROC analysis)
   - Any function from `survival::`, `lme4::`, `pROC::`

3. **Resampling/bootstrapping**
   - `boot()`, `boot.ci()`
   - Custom bootstrap loops
   - Permutation tests

4. **Large matrix operations**
   - Matrix multiplication on >1000x1000 matrices
   - `eigen()`, `svd()`, `chol()`
   - `cor()`, `cov()` on many variables

### Medium-Priority Triggers (add if --aggressive)
1. **Data transformations**
   - `merge()`, `join()` operations
   - `reshape()`, `pivot_*()`
   - `group_by() %>% summarize()`

2. **Plotting**
   - `ggplot()` with many points (>10,000)
   - Complex plots with facets/animations

3. **Conditional computations**
   - Long `if/else` chains with complex logic
   - `switch()` statements with expensive branches

### Never Add Checkpoints
- Simple assignments
- Accessor methods (`self$options$...`)
- String concatenation
- Arithmetic operations
- Short helper functions

## Detection Patterns

The command scans `R/{function_name}.b.R` for these patterns:

```r
# DETECT: Loop over rows with table update
for (item in items) {
    if (table$getCell(rowKey = item, 'result')$isEmpty) {
        table$setStatus('running')
        # ← CHECKPOINT HERE
        result <- expensiveComputation(item)
        table$setRow(rowKey = item, result)
        table$setStatus('complete')
    }
}

# DETECT: Statistical model fitting
# ← CHECKPOINT HERE
fit <- survival::coxph(formula, data = data)

# DETECT: Bootstrap loop
for (b in 1:n_boot) {
    # ← CHECKPOINT HERE
    boot_sample <- sample(data, replace = TRUE)
    boot_result <- analyze(boot_sample)
}
```

## Dry-Run Output Example

```
🔍 Checkpoint Analysis: tableone
════════════════════════════════

File: R/tableone.b.R

📍 Detected Expensive Operations:

Line 145: for (var in variables)
  Pattern: Loop over data variables
  Severity: HIGH
  Recommendation: Add checkpoint before loop

Line 220: fit <- lm(formula, data)
  Pattern: Statistical model (lm)
  Severity: HIGH
  Recommendation: Add checkpoint before model fitting

Line 340: boot_results <- boot(...)
  Pattern: Bootstrap resampling
  Severity: HIGH
  Recommendation: Add checkpoint before bootstrap

───────────────────────────────────────

📋 Summary:
  - 3 expensive operations detected
  - 3 checkpoint insertions recommended
  - Estimated performance: Moderate (2-5 minute runs)

💡 Recommendation: Add checkpoints
   Run without --dry-run to apply changes
```

## Best Practices

**DO add checkpoints:**
- Before loops that iterate >10 times
- Before statistical model fitting
- Before bootstrap/resampling
- Before complex data transformations
- Inside loops that update tables incrementally

**DON'T add checkpoints:**
- Before fast operations (<100ms)
- More than once every 10 lines of code
- Inside tight inner loops (causes overhead)
- Before simple assignments or accessors
- Use `private$.checkpoint(flush = FALSE)` to only poll for changes without re-pushing results

**Example Pattern**:
```r
for (item in items) {
    if (table$getCell(rowKey = item, 'result')$isEmpty) {
        table$setStatus('running')
        private$.checkpoint()  # Add before expensive operation
        
        result <- expensiveComputation(item)
        table$setRow(rowKey = item, result)
        table$setStatus('complete')
    }
}
```

Analyze the function $ARGUMENTS and add checkpoints before any expensive operations like loops, statistical tests, or data processing steps.
