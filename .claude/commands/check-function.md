---
name: check-function
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
usage: /check-function <function_name>
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

### 🔍 **Core Integration Checks**

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

## Response Format

Structure your analysis as:

### 📋 SYSTEMATIC CHECK: `$ARGUMENTS`

**Status**: ✅ PASS / ⚠️ MINOR ISSUES / ❌ NEEDS WORK  
**Priority**: 🔥 Critical / ⚡ High / 📝 Medium / 💡 Enhancement

#### 🔍 QUICK SUMMARY

- **Arguments**: X defined → X/X used in .b.R
- **Outputs**: X defined → X/X populated in .b.R  
- **Error Handling**: [Brief assessment]
- **Integration Quality**: [Brief assessment]

#### ❌ CRITICAL ISSUES (Fix immediately)

1. [Specific issue with file:line reference if possible]

#### ⚠️ INTEGRATION ISSUES (Schema mismatches, unused elements)

1. [Specific issue with exact fix needed]

#### 📝 CODE QUALITY ISSUES (Improvements recommended)

1. [Specific suggestion with rationale]

#### ✅ STRENGTHS (What's working well)

1. [Positive findings]

#### 📖 DOCS CONSISTENCY (Code ↔ Docs)

[Summary of inline comments, roxygen, help files, and examples.]

#### 🌐 EXTERNAL DOCS COMPARISON (CRAN / GitHub)

State which upstreams were checked (CRAN pkg: `$ARG_cran_pkg`, GitHub: `$ARG_github_repo`) and summarize diffs.

| Aspect             | Local (jamovi) | Upstream (CRAN/GitHub) | Status | Action |
|--------------------|----------------|------------------------|:------:|--------|
| Function signature  |                |                        |        |        |
| Arguments/defaults  |                |                        |        |        |
| Behavior notes     |                |                        |        |        |
| Deprecations       |                |                        |        |        |
| Examples           |                |                        |        |        |

#### 🔧 ACTIONABLE FIXES

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

#### 🔎 FETCH COMMANDS (copy/paste; do not execute here)

```bash
# CRAN manual (PDF)
curl -L "https://cran.r-project.org/web/packages/$ARG_cran_pkg/$ARG_cran_pkg.pdf" -o cran-manual.pdf

# CRAN package metadata (json)
curl -L "https://crandb.r-pkg.org/$ARG_cran_pkg" -o cran-meta.json

# GitHub upstream sources
curl -L "https://raw.githubusercontent.com/$ARG_github_repo/HEAD/R/${ARG_upstream_fn:-SANITIZED_FN}.R" -o upstream-fn.R || true
curl -L "https://raw.githubusercontent.com/$ARG_github_repo/HEAD/NEWS.md" -o upstream-NEWS.md || true
curl -L "https://raw.githubusercontent.com/$ARG_github_repo/HEAD/README.md" -o upstream-README.md || true
```

#### 🧪 TESTING CHECKLIST

- [ ] Test with [specific scenario]
- [ ] Validate [specific behavior]
- [ ] Check [edge case]
- [ ] Compare local function signature against upstream (CRAN/GitHub) and reconcile
- [ ] Re-run examples from upstream docs with local function; fix divergences or document them

#### 📊 READINESS ASSESSMENT

- **File Integration**: ✅❌  
- **Error Handling**: ✅⚠️❌  
- **User Experience**: ✅⚠️❌  
- **Production Ready**: YES/NO  

Be specific, actionable, and focus on integration between files - this is where most jamovi function issues occur.
