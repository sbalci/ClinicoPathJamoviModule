---
name: add-R-code
description: Add infrastructure for .generateRCode() in jamovi analysis - creates stubs in .b.R and wires UI in .a.yaml/.r.yaml
interactive: true
args:
  function_name:
    description: Jamovi function name (e.g., summarydata). Must match R/$ARG_function_name.b.R and jamovi/$ARG_function_name.*.yaml files.
    required: true
  --force:
    description: Overwrite existing .generateRCode() stub in .b.R with fresh template (use after template updates).
    required: false
usage: /add-R-code <function_name> [--force]
---

# Add Reproducible R Code Infrastructure

This command sets up the infrastructure for `.generateRCode()` in a jamovi analysis, enabling users to export reproducible R code that uses upstream packages instead of jamovi wrappers.

## What It Does

### 1. **Patches `.a.yaml`** (Analysis Definition)
- Adds `showRCode` Boolean option **BEFORE** the `...` marker
- Default: `false`
- Allows users to toggle R code generation in the UI

### 2. **Patches `.r.yaml`** (Results Definition)
- Adds `rCode` Html output **BEFORE** the `refs:` marker
- Visible when `showRCode` is `true`
- Displays the generated R code in the results panel

### 3. **Patches `.b.R`** (Backend Implementation)
- Adds `.generateRCode()` method **INSIDE** `private = list()`
- Adds conditional call in `.run()`: `if (self$options$showRCode) { private$.generateRCode(...) }`
- Creates a **STUB/TEMPLATE** that must be customized for your analysis

### 4. **Creates Backups**
- All modified files get timestamped backups: `<file>.bak.YYYYMMDD-HHMMSS`

## Important Notes

⚠️ **The generated `.generateRCode()` is a STUB** - you must customize it with analysis-specific code!

### Example Workflow

1. **Run the command**:
   ```bash
   /add-R-code summarydata
   ```

2. **The stub template looks like this**:
   ```r
   .generateRCode = function() {
       # Build R code string
       r_code <- "
   # Your analysis-specific R code here
   # Example: library(package)
   # mydata <- read.csv('data.csv')
   # result <- your_analysis(mydata)
   "

       # Wrap in HTML and set content
       r_code_html <- paste0(
           "<div style='background:#f5f5f5;padding:1em;border:1px solid #ddd;border-radius:4px;'>",
           "<h4>Copy-Ready R Code</h4>",
           "<pre style='background:white;padding:1em;overflow-x:auto;border:1px solid #ccc;'><code>",
           htmltools::htmlEscape(r_code),
           "</code></pre>",
           "</div>"
       )
       self$results$rCode$setContent(r_code_html)
   }
   ```

3. **Customize the function**:
   - Add parameters based on your analysis needs (e.g., `var_list`, `dataset`, `method_name`)
   - Build analysis-specific R code using `sprintf()` to inject parameters
   - Use upstream packages (stats, MASS, mixOmics, ggplot2, etc.)
   - Follow the pattern from `variablebiplot.b.R` (lines 1090-1424) for a complete example

4. **Update the call in `.run()`**:
   - Match the parameters you defined
   - Example: `private$.generateRCode(var_list, dataset)`

## Real-World Example

See `R/variablebiplot.b.R` for a production implementation:
- Lines 973-975: Conditional call `if (self$options$showRCode) { private$.generateRCode(result, method_name, n_obs, n_features) }`
- Lines 1090-1424: Complete implementation with PCA/PLS-DA/LDA code generation

See `R/summarydata.b.R` for another example:
- Lines 170-172: Conditional call with `var_list` and `dataset`
- Lines 596-694: Implementation for descriptive statistics

## File Placement Requirements

### `.a.yaml` - Option goes BEFORE `...`
```yaml
    - name: report_sentences
      type: Bool
      default: false

    - name: showRCode  # ← Added here (BEFORE ...)
      title: Show R Code (Reproducible)
      type: Bool
      default: false

...  # ← NOT after this line!
```

### `.r.yaml` - Output goes BEFORE `refs:`
```yaml
    - name: glossary
      title: Statistical Glossary
      type: Html

    - name: rCode  # ← Added here (BEFORE refs:)
      title: Reproducible R Code
      type: Html
      visible: (showRCode)

refs:  # ← NOT after this line!
    - package1
    - package2
```

### `.b.R` - Method goes INSIDE `private = list()`
```r
summarydataClass <- R6::R6Class("summarydataClass",
    inherit = summarydataBase,
    private = list(
        .run = function() {
            # Your analysis code

            # Add at end of .run():
            if (self$options$showRCode) {
                private$.generateRCode(var_list, dataset)
            }
        },

        .otherMethod = function() {
            # ...
        },

        .generateRCode = function(var_list, dataset) {  # ← Added here (INSIDE private)
            # Your custom implementation
        }
    )  # ← BEFORE this closing paren
)
```

## Usage

```bash
# First time setup
/add-R-code summarydata

# Overwrite existing stub (after template updates)
/add-R-code summarydata --force
```

## After Running

1. ✅ Check that `showRCode` option appears in jamovi UI
2. ✅ Verify `rCode` output slot exists in results
3. ⚠️ **Customize** `.generateRCode()` with your analysis-specific code
4. ⚠️ Update the call in `.run()` to pass correct parameters
5. ✅ Test in jamovi with a sample dataset

## Summary

This command provides **infrastructure only** - think of it as scaffolding. The actual R code generation logic is analysis-specific and must be implemented by you.

For inspiration, see:
- `R/variablebiplot.b.R` - Complex multimethod analysis
- `R/summarydata.b.R` - Descriptive statistics
- Both show proper parameter passing, code building with `sprintf()`, and HTML wrapping
