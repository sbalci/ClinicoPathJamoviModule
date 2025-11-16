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
  notices_mode:
    description: Notices check mode — (create), audit-only (report only), or off
    required: false
    default: enforce
    enum: [enforce, audit-only, off]
  notices_min_severity:
    description: Minimum severity to surface (info, warning, strong_warning, error)
    required: false
    default: info
    enum: [info, warning, strong_warning, error]
  clinical_profile:
    description: Apply clinical-pathology notice templates and thresholds (AUC, events-per-variable, etc.)
    required: false
    default: true
  notice_insert_position:
    description: Preferred default insert position for notices (top, mid, bottom, auto)
    required: false
    default: auto
    enum: [top, mid, bottom, auto]
usage: /check-function-base &lt;function_name&gt;
---

# Systematic Jamovi Function Quality Check (with **Notices** audit)

You are an expert jamovi module developer performing a comprehensive quality assessment of the jamovi function `$ARGUMENTS`. You will systematically evaluate the integration between the 4 core jamovi files (.a.yaml, .b.R, .r.yaml, .u.yaml), **and enforce jamovi Notice best‑practices** (via `jmvcore::Notice`) for errors, strong warnings, warnings, and info messages.

> **Why:** Notices surface user‑facing guidance directly in results and are the canonical UX pattern in jamovi (`jmvcore::Notice` and `jmvcore::NoticeType`). Follow the API recommendations and the internal ClinicoPath guide to produce consistent, actionable messages. 

**Jamovi Notices references:**  
• Official API overview (Notice/NoticeType, insert positions)  
• ClinicoPath internal guide (content patterns, clinical thresholds, wording)

## Analysis Target

Function: **`$ARGUMENTS`**

### Argument normalization (safety)

Before proceeding, sanitize `$ARGUMENTS` to a base function name (call it **SANITIZED_FN**): drop any leading paths, then strip any of these suffixes: .a.yaml, .b.R, .r.yaml, .u.yaml. Use **SANITIZED_FN** consistently for all file paths and references below.

Please analyze these files:

- `jamovi/SANITIZED_FN.a.yaml` - Analysis definition (options/arguments)
- `R/SANITIZED_FN.b.R` - Backend implementation
- `jamovi/SANITIZED_FN.r.yaml` - Results definition (outputs)
- `jamovi/SANITIZED_FN.u.yaml` - User interface definition

External sources (if available and `check_external=true`):

- CRAN reference manual PDF – `https://cran.r-project.org/web/packages/$ARG_cran_pkg/$ARG_cran_pkg.pdf`
- CRAN NEWS – `https://cran.r-project.org/web/packages/$ARG_cran_pkg/NEWS`
- pkgdown reference – `https://$ARG_cran_pkg.tidyverse.org/` or project site if known
- GitHub repo – `https://github.com/$ARG_github_repo` (read `R/*.R`, `man/*.Rd`, `NEWS.md`, `README.md`)

---

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
   - User-friendly error messages (via Notices)
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

---

## **Jamovi Notices Audit & Enforcement**

Apply this **in addition** to the framework above. Create, review, or refactor notices in `R/SANITIZED_FN.b.R` using `jmvcore::Notice` and `jmvcore::NoticeType`:

### A. Required notice types & triggers

- **ERROR** (`NoticeType$ERROR`)  
  Use when analysis **cannot proceed** (missing inputs, invalid types, fatal calc errors). Immediately `insert(1, ...)` and `return()`.
- **STRONG_WARNING** (`NoticeType$STRONG_WARNING`)  
  Use when results may be unreliable (assumption violations, very small n/events, extreme prevalence).
- **WARNING** (`NoticeType$WARNING`)  
  Use for minor concerns and methodological notes.
- **INFO** (`NoticeType$INFO`)  
  Use for confirmations, methodology summaries, success/end notices.

> Follow the official jamovi API notice semantics and positioning guidance.  

### B. Positioning policy

Respect `$ARG_notices_mode` and `$ARG_notice_insert_position`:

- `auto` (default): ERROR/STRONG_WARNING at top (`insert(1, ...)`), contextual WARNING before related table, INFO at bottom (`insert(999, ...)`).
- `top`/`mid`/`bottom`: prefer requested band while keeping ERROR at top.

### C. Content rules

- Plain text only (no HTML).  
- **Single-line content only**: do not use `\n` or any other newline characters inside a notice; each notice must fit on a single line.  
- Specific, measurable, and **actionable**: quantify counts/percentages, state implications, and list next steps, but keep everything in one sentence where possible.  
- If you need a “bullet-like” structure, emulate it within a single line using separators such as ` • ` or `; ` instead of real line breaks.  
- Keep names unique (`name='...'`) and deterministic.

### D. Clinical profile (if `$ARG_clinical_profile`)

Add domain‑specific notices for clinical/pathology modules, for example:

- Diagnostic models with **AUC &lt; 0.5** → ERROR; **AUC &lt; 0.7** → STRONG_WARNING with clinical implications.  
- Survival analysis: enforce minimum events (e.g., **&lt; 10 events → ERROR**, 10–19 → STRONG_WARNING, 20–49 → WARNING) and surface EPV considerations.  
- Diagnostic prevalence extremes (**&lt;5% or &gt;95%**) → STRONG_WARNING about PPV/NPV/generalizability.

### E. Minimal code patterns to implement

**Create & insert a notice**
```r
notice <- jmvcore::Notice$new(options=self$options, name='validationError', type=jmvcore::NoticeType$ERROR)
notice$setContent('Time and event variables are required. Please select both and re-run.')
self$results$insert(1, notice)
```

**Priority insertion (error → strong_warning → warning → info)**
```r
position <- 1
for (n in notices_in_priority) { self$results$insert(position, n); position <- position + 1 }
```

**Success summary at bottom**
```r
ok <- jmvcore::Notice$new(options=self$options, name='analysisComplete', type=jmvcore::NoticeType$INFO)
ok$setContent(sprintf('Analysis completed using %d observations.', nrow(self$data)))
self$results$insert(999, ok)
```


> **Current limitation & co-existence with Html outputs:**  
> Notices are currently **single-line only** and cannot contain line breaks. For rich, multi-line explanations, keep using your existing Html results (e.g., summary paragraphs, detailed interpretation blocks) **in addition to** Notices. Use Notices as concise, single-line banners (errors, strong warnings, key info) that point to the more detailed Html content instead of trying to replace it.

---

## Response Format

### SYSTEMATIC CHECK: `$ARGUMENTS`

**Status**: PASS / MINOR ISSUES / NEEDS WORK  
**Priority**: Critical / High / Medium / Enhancement

#### QUICK SUMMARY

- **Arguments**: X defined → X/X used in .b.R  
- **Outputs**: X defined → X/X populated in .b.R  
- **Error Handling (Notices)**: [Brief assessment of ERROR/STRONG_WARNING/WARNING/INFO coverage and placement]  
- **Integration Quality**: [Brief assessment]

#### ARGUMENT BEHAVIOR MATRIX (Does each argument change behavior?)

| Argument (.a.yaml) | Default → Changed Value | Observed Change in Results | Effective? | Evidence (file:line / diff) |
|---|---|---|:---:|---|
| `arg1` | `false` → `true` | Table rows changed (N=…), CI method switched | YES/NO | [pointer] |

#### OUTPUT POPULATION MATRIX (Is each output populated?)

| Output (.r.yaml) | Type | Setter in .b.R | Visibility Rule | Populated? | Notes |
|---|---|---|---|:---:|---|
| `report` | Html | `setContent()` | always | YES/NO |  |

#### **NOTICES COVERAGE MATRIX**

| Trigger | Type | Insert Position | Present? | Message quality | Notes |
|---|---|---|---:|---|---|
| Missing required inputs | ERROR | top (1) | ✅/❌ | specific/actionable |  |
| Low events / small n | STRONG_WARNING/WARNING | top/mid | ✅/❌ | quantifies thresholds |  |
| Assumption violation | STRONG_WARNING | top | ✅/❌ | suggests remedies |  |
| Methodology summary | INFO | bottom (999) | ✅/❌ | concise & numeric |  |

#### PLACEHOLDER ASSESSMENT

- **Data used?** YES/NO  
- **Options used in logic?** YES/NO  
- **Constant results regardless of inputs?** YES/NO  
- **Placeholder indicators**: …  
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

**Code Improvements (.b.R):**
```r
# Implement/adjust notices per findings
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
  results <- run_analysis(data = demo_df, options = modifyList(default_opts, opts))
  digest_results(results)  # compact comparable representation
}

# Compare default vs. changed argument (example)
res_default <- run_with_opts(list())
res_changed <- run_with_opts(list(assume_equal_var = TRUE))
if (!identical(res_default, res_changed)) message("assume_equal_var = effective.") else warning("assume_equal_var = NON-EFFECTIVE.")
```

#### TESTING CHECKLIST

- [ ] Test with [specific scenario]
- [ ] Validate [specific behavior]
- [ ] Check [edge case]
- [ ] Verify notices presence/positioning and message quality
- [ ] Compare local function signature against upstream (CRAN/GitHub) and reconcile
- [ ] Re-run examples from upstream docs with local function; fix divergences or document them

#### READINESS ASSESSMENT

- **File Integration**: ✅❌  
- **Error Handling (Notices)**: ✅⚠️❌  
- **User Experience**: ✅⚠️❌  
- **Production Ready**: YES/NO

---

### Notes for Claude CLI integration

- This file lives in `.claude/commands/` and is executed via `/check-function-base <function_name>`.  
- `$ARGUMENTS` injects whatever follows the command name; multi‑word args are supported.  
- Additional flags can be passed inline, e.g.:  
  `/check-function-base myFn notices_mode=audit-only clinical_profile=false`
