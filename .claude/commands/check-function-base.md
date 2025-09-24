---
name: check-function-base
description: Perform systematic quality check of a jamovi function
interactive: true
args:
  function_name:
    description: Name of the jamovi function to check
    required: true
    autocomplete: functions
  check_external:
    description: Compare against upstream docs on CRAN/GitHub (reference manuals, pkgdown, NEWS)
    required: false
    default: false
  cran_pkg:
    description: Upstream CRAN package name to compare against (e.g., jmv)
    required: false
  github_repo:
    description: Upstream GitHub repo in owner/name form (e.g., jamovi/jmv)
    required: false
  upstream_fn:
    description: Upstream function name if it differs from SANITIZED_FN
    required: false
usage: /check-function-base <function_name>
---

# Systematic Jamovi Function Quality Check

You are an expert jamovi module developer performing a comprehensive quality assessment of the jamovi function `$ARGUMENTS`. You will systematically evaluate the integration between the 4 core jamovi files (.a.yaml, .b.R, .r.yaml, .u.yaml) and provide actionable recommendations.

## Analysis Target

Function: **`$ARGUMENTS`**

### Argument normalization (safety)

Before proceeding, sanitize `$ARGUMENTS` to a base function name (call it **SANITIZED_FN**): drop any leading paths, then strip any of these suffixes: .a.yaml, .b.R, .r.yaml, .u.yaml. Use **SANITIZED_FN** consistently for all file paths and references below.

Please analyze these files:

- `jamovi/SANITIZED_FN.a.yaml` - Analysis definition (options/arguments)
- `R/SANITIZED_FN.b.R` - Backend implementation
- `jamovi/SANITIZED_FN.r.yaml` - Results definition (outputs)
- `jamovi/SANITIZED_FN.u.yaml` - User interface definition

External sources (if available and check_external=true):

- CRAN reference manual PDF – `https://cran.r-project.org/web/packages/$ARG_cran_pkg/$ARG_cran_pkg.pdf`
- CRAN NEWS – `https://cran.r-project.org/web/packages/$ARG_cran_pkg/NEWS`
- pkgdown reference – `https://$ARG_cran_pkg.tidyverse.org/` or project site if known
- GitHub repo – `https://github.com/$ARG_github_repo` (read `R/*.R`, `man/*.Rd`, `NEWS.md`, `README.md`)

## Systematic Evaluation Framework

### Core Integration Checks

1. **Argument Integration (.a.yaml ↔ .b.R)**
   - All .a.yaml options referenced in .b.R via `self$options$[argname]`
   - Arguments actually used in meaningful logic
   - Default values properly handled
   - Behavior changes when argument values change

2. **Output Population (.r.yaml ↔ .b.R)**
   - All .r.yaml outputs populated in .b.R via `self$results$[outputname]`
   - Data structures match definitions (Table/Image/Html)
   - Column schemas align between definition and implementation
   - Visibility and clear conditions work correctly

3. **Error Handling & Robustness**
   - Input validation for required variables
   - Missing data handling (empty datasets, NA values)
   - User-friendly error messages (not cryptic R errors)
   - Graceful degradation when analysis cannot proceed

4. **Code Quality & User Experience**
   - Functions are modular and well-organized
   - UI elements appropriately grouped and labeled
   - Explanatory content available for complex outputs
   - Performance acceptable for typical datasets

5. **Documentation Consistency (Code ↔ Docs)**
   - Inline comments and roxygen documentation
   - Help files and examples

6. **External Documentation Consistency (CRAN/GitHub)**
   - Function signature & arguments match upstream (names, order, defaults)
   - Behavioral notes (assumptions, auto-switches) match upstream docs
   - Deprecations/renames in upstream are reflected locally
   - NEWS changelog items accounted for (version bumps, argument changes)

7. **Argument Behavior Change Verification (Differential Runs)**
   - For **each** `.a.yaml` option, run the analysis twice: (a) with defaults and (b) with the option toggled/changed to a non-default valid value.
   - Compare the resulting `self$results` contents (tables, figures, html) and any side effects (e.g., column schemas, test selection, filtering).
   - Mark an argument **NON-EFFECTIVE** if no observable change occurs in results or visible logic paths.
   - Pay special attention to flags that gate computation (e.g., `assume_equal_var`, `paired`, `use_bootstrap`), thresholds, and choice selectors.

8. **Output Population Verification (Programmatic)**
   - Crosswalk **every** item in `.r.yaml` to the corresponding setter in `.b.R`:
     - `Html` → `setContent()`, `setVisible()`
     - `Table` → `setRow()`, `addColumn()`, `setNote()`
     - `Image`/`Plot` → `setState()` + renderer function
   - Confirm visibility rules (`visible`, `clearWith`, `refs`) and that conditional panels appear only when enabled.
   - Flag an output **UNPOPULATED** if there is no setter call or if it is always hidden.

9. **Placeholder Detection Heuristics (Does the function do real work?)**
   - Check whether `.b.R` references dataset columns (`self$data`) and options (`self$options$*`) in actual computations (not only in labels).
   - Search for signs of placeholders: constant return values, unused options, empty/zero-length tables, `TODO/FIXME`, stubs that only echo inputs, or results populated with template text.
   - If most options are unused and results are constant regardless of inputs, classify as **PLACEHOLDER / SCAFFOLD** and provide concrete remediation.

## Response Format

### SYSTEMATIC CHECK: `$ARGUMENTS`

**Status**: PASS / MINOR ISSUES / NEEDS WORK  
**Priority**: Critical / High / Medium / Enhancement

#### QUICK SUMMARY

- **Arguments**: X defined → X/X used in .b.R
- **Outputs**: X defined → X/X populated in .b.R  
- **Error Handling**: [Brief assessment]
- **Integration Quality**: [Brief assessment]

#### ARGUMENT BEHAVIOR MATRIX (Does each argument change behavior?)

| Argument (.a.yaml) | Default → Changed Value | Observed Change in Results | Effective? | Evidence (file:line / diff) |
|---|---|---|:---:|---|
| `arg1` | `false` → `true` | Table rows changed (N=…), CI method switched | YES/NO | [pointer] |

#### OUTPUT POPULATION MATRIX (Is each output populated?)

| Output (.r.yaml) | Type | Setter in .b.R | Visibility Rule | Populated? | Notes |
|---|---|---|---|:---:|---|
| `report` | Html | `setContent()` | always | YES/NO |  |

#### PLACEHOLDER ASSESSMENT

- **Data used?** (accesses `self$data` columns in computations): YES/NO  
- **Options used in logic?** (beyond labels): YES/NO  
- **Constant results regardless of inputs?** YES/NO  
- **Placeholder indicators**: [TODOs, empty tables, echo-only behavior, etc.]  
- **Classification**: FUNCTIONAL / PARTIAL PLACEHOLDER / PLACEHOLDER

#### CRITICAL ISSUES (Fix immediately)

1. [Specific issue with file:line reference if possible]

#### INTEGRATION ISSUES (Schema mismatches, unused elements)

1. [Specific issue with exact fix needed]

#### CODE QUALITY ISSUES (Improvements recommended)

1. [Specific suggestion with rationale]

#### STRENGTHS (What's working well)

1. [Positive findings]

#### DOCS CONSISTENCY (Code ↔ Docs)

[Summary of inline comments, roxygen, help files, and examples.]

#### EXTERNAL DOCS COMPARISON (CRAN / GitHub)

State which upstreams were checked (CRAN pkg: `$ARG_cran_pkg`, GitHub: `$ARG_github_repo`) and summarize diffs.

| Aspect             | Local (jamovi) | Upstream (CRAN/GitHub) | Status | Action |
|--------------------|----------------|------------------------|:------:|--------|
| Function signature  |                |                        |        |        |
| Arguments/defaults  |                |                        |        |        |
| Behavior notes     |                |                        |        |        |
| Deprecations       |                |                        |        |        |
| Examples           |                |                        |        |        |

#### ACTIONABLE FIXES

**Immediate (Critical):**

```yaml
# Exact code changes needed
```

**Schema Updates:**

```yaml  
# Specific .yaml file changes
```

**Code Improvements:**

```r
# Specific .b.R improvements
```

**Upstream Sync Tasks:**

- Align argument names/defaults with upstream or document intentional divergence
- Port missing behavior notes (assumptions, edge-case handling) into docs
- Reflect deprecations/renames; add migration notes
- Update examples to match upstream signatures

#### DIFFERENTIAL TEST HARNESS (example)

```r
# Minimal differential-run harness for behavior checks
run_with_opts <- function(opts) {
  # simulate how the .b.R would be called; adapt to your analysis wrapper
  results <- run_analysis(data = demo_df, options = modifyList(default_opts, opts))
  digest_results(results)  # return a compact comparable representation
}

# Compare default vs. changed argument
res_default <- run_with_opts(list())
res_changed <- run_with_opts(list(assume_equal_var = TRUE))  # example toggle

if (!identical(res_default, res_changed)) {
  message("assume_equal_var = effective (results differ).")
} else {
  warning("assume_equal_var = NON-EFFECTIVE (no observable change).")
}
```

#### TESTING CHECKLIST

- [ ] Test with [specific scenario]
- [ ] Validate [specific behavior]
- [ ] Check [edge case]
- [ ] Compare local function signature against upstream (CRAN/GitHub) and reconcile
- [ ] Re-run examples from upstream docs with local function; fix divergences or document them

#### READINESS ASSESSMENT

- **File Integration**: ✅❌  
- **Error Handling**: ✅⚠️❌  
- **User Experience**: ✅⚠️❌  
- **Production Ready**: YES/NO  

Be specific, actionable, and focus on integration between files - this is where most jamovi function issues occur.
