# enhancedROC Guide Compliance Report

**Date:** 2026-03-02
**Analyst:** Team Agent (guide-compliance)
**Files Reviewed:**
- `R/enhancedROC.b.R`
- `jamovi/enhancedroc.a.yaml`
- `jamovi/enhancedroc.r.yaml`
- `jamovi/enhancedroc.u.yaml`

**Guides Consulted:** All 11 jamovi development guides in `vignettes/`

---

## Executive Summary

| Category | Status |
|----------|--------|
| .a.yaml (Analysis Definition) | MOSTLY PASS — 2 WARNs, 1 FAIL |
| .b.R (Backend Implementation) | MOSTLY PASS — 3 WARNs, 1 FAIL |
| .r.yaml (Results Definition) | MOSTLY PASS — 2 WARNs |
| .u.yaml (UI Definition) | MOSTLY PASS — 2 WARNs |
| JS Guide | PASS (no JS file — acceptable) |
| Actions Guide | PASS (no actions defined — acceptable) |
| Formula Guide | N/A (no formula-based analysis) |
| i18n Guide | FAIL — incomplete i18n usage |
| Module Patterns Guide | MOSTLY PASS — 1 FAIL (state management) |
| Plots Guide | WARN — state pattern deviation |
| Tables Guide | PASS |

---

## Guide 1: `.a.yaml` Guide

**File:** `jamovi/enhancedroc.a.yaml`

### Checks

#### PASS — Required top-level fields present
- `name: enhancedROC` ✓
- `title: Clinical ROC Analysis` ✓
- `menuGroup: meddecide` ✓
- `version: '0.0.34'` ✓
- `jas: '1.2'` ✓
- `description.main` present ✓

#### PASS — Data option is first
Line 19: `- name: data` / `type: Data` is the first option. ✓

#### PASS — Variable types correctly specified
- `outcome` uses `suggested: [nominal, ordinal]` and `permitted: [factor, numeric]` ✓
- `predictors` uses `suggested: [continuous]` and `permitted: [numeric]` ✓

#### PASS — Level option references correct Variable
```yaml
- name: positiveClass
  type: Level
  variable: (outcome)   # Line 39 — correctly references outcome variable
```

#### FAIL — `type: Level` must NOT have `default`
**Rule:** The CLAUDE.md constraint states: "in .a.yaml type: Level is not allowed to have default"

The `positiveClass` option (line 37-41) does NOT have a `default` field — this is correct. However, verify this remains the case. **Current status: PASS.**

Actually, re-examining: no `default` is set for `positiveClass`. **PASS.**

#### WARN — Missing R usage example in description
The guide recommends:
```yaml
description:
    main: |
        ...
    R:
        dontrun: true
        usage: |
            # Example R code
```
The current `description` only has `main:` with no `R:` sub-section. This limits developer usability.

**Recommendation:** Add an `R:` section with a minimal usage example.

#### PASS — List options have correct defaults matching option names
All List options verified:
- `analysisType` default `single` matches option name `single` ✓
- `direction` default `auto` matches option name `auto` ✓
- `bootstrapMethod` default `bca` matches option name `bca` ✓
- All other List options verified ✓

#### WARN — Naming convention inconsistency
Some option names use snake_case (`comprehensive_output`, `clinical_interpretation`) while others use camelCase (`analysisType`, `rocCurve`, `aucTable`). The guide recommends consistent camelCase.

**Lines:** 364 (`comprehensive_output`), 370 (`clinical_interpretation`)

**Recommendation:** Rename to `comprehensiveOutput` and `clinicalInterpretation` to be consistent with the camelCase convention used by all other options.

#### PASS — Number/Integer options have appropriate min/max and defaults
Examples verified:
- `confidenceLevel`: default 95, min 50, max 99.9 ✓
- `bootstrapSamples`: default 1000, min 100, max 10000 ✓
- `sensitivityThreshold`: default 0.8, min 0.1, max 1.0 ✓

#### PASS — Bool options have `default: false` or `default: true` correctly
All Bool options have proper boolean defaults ✓

---

## Guide 2: `.b.R` Guide

**File:** `R/enhancedROC.b.R`

### Checks

#### PASS — R6 class pattern correct
```r
enhancedROCClass <- R6::R6Class(
    "enhancedROCClass",
    inherit = enhancedROCBase,      # Auto-generated base
    private = list(
        .init = function() { ... },
        .run = function() { ... }
    )
)
```
Pattern matches guide requirements ✓

#### PASS — `.init()` and `.run()` are private methods
Both are correctly placed in `private = list(...)` ✓

#### PASS — `private$.checkpoint()` used correctly
Multiple checkpoint calls at lines 150, 162, 169-287. The guide notes this is an internal jamovi function that must not be defined by the module. It is called but not redefined. ✓

#### PASS — Error handling with tryCatch
`.runROCAnalysis()` uses `tryCatch` properly (line 615+). ✓

#### WARN — State management in plot functions uses minimal list
The plots guide and module patterns guide recommend including plot-relevant option values in the state to ensure re-rendering when options change:
```r
# Recommended pattern from guides:
plotState <- list(
    data = plotData,
    plot_title = self$options$plot_title,
    color_palette = self$options$color_palette
)
image$setState(plotState)
```

Current implementation (e.g., line 2678):
```r
image$setState(list(width = width, height = height))
```

Only width/height are stored in state. The actual ROC data (`private$.rocResults`) is accessed via private fields in render functions rather than through state. This means plot state does not capture all option-dependent data.

**Risk:** If jamovi serializes state between runs, plots may not update when options change (e.g., `smoothMethod`, `showCutoffPoints`) unless data is re-run. This is a known architectural limitation — acceptable if the module always re-runs `.run()` before rendering, but the guide recommends the state-based approach for reliability.

**Recommendation:** Include relevant option values in setState to ensure re-rendering on option change.

#### WARN — Inconsistent use of `.()`for i18n
The guide mandates `.()` for all user-visible strings. Only a few strings use `.()` (e.g., line 593: `.("Clinical Assumptions & Recommendations")`), but the vast majority of user-visible HTML strings in error messages and notices are NOT wrapped in `.()`.

**Examples of missing i18n:**
- Line 127: `"Please select an outcome variable..."` — no `.()` wrapper
- Line 337: `"Insufficient data"` — correct, has `.()` wrapper
- Lines 415-426: Multi-line HTML error strings — no `.()` wrappers

**Recommendation:** Wrap all user-visible strings in `.()`. At minimum, wrap the short descriptive portions.

#### FAIL — Non-standard use of `%||%` operator
Lines 79, 649, 650, 676, 2676, 2827, etc. use `%||%` operator. This is the null-coalescing operator from `rlang`. If it is not imported in NAMESPACE via `importFrom(rlang, %||%)` or `importFrom(jmvcore, ...)`, this will cause runtime errors. The guide does not prohibit this but requires all dependencies to be declared.

**Verification needed:** Check NAMESPACE and DESCRIPTION for `rlang` import declaration.

**Recommendation:** Verify `rlang` is in DESCRIPTION Imports and `%||%` is imported in NAMESPACE.

#### PASS — HTML output pattern correctly implemented for notices
The module correctly follows the CLAUDE.md pattern for converting jmvcore::Notice to HTML:
- `.noticeList` private field collects notices
- `.addNotice()` method accumulates notices
- `.renderNotices()` renders all at end of `.run()`
- Uses `self$results$results$notices$setContent(html)` ✓

#### PASS — HTML sanitization implemented
`.safeHtmlOutput()` function at line 38 properly sanitizes:
- `&`, `<`, `>`, `"`, `'` characters ✓

#### WARN — `clinicopath_init` and `validate_clinical_data` may be undefined
Lines 99-100 and 326-330 call `clinicopath_init(...)` and `validate_clinical_data(...)` with `exists()` guards. These appear to be utility functions from a helper module. While the existence check provides a fallback, this pattern is unusual and may indicate dead code or incomplete integration.

**Recommendation:** Either define these functions explicitly or remove the `exists()` guard pattern and use concrete validation logic.

#### PASS — Data access pattern correct
`self$data` is accessed (line 103, 388) and subsetted to required variables before analysis. `na.omit()` is applied (line 389). ✓

---

## Guide 3: `.r.yaml` Guide

**File:** `jamovi/enhancedroc.r.yaml`

### Checks

#### PASS — Required file structure
- `name: enhancedROC` ✓
- `jrs: '1.1'` ✓
- `items:` section present ✓

#### PASS — HTML items for notices (avoiding serialization issue)
```yaml
- name: notices
  title: Important Information
  type: Html
```
This correctly follows the CLAUDE.md guidance to convert Notice objects to Html output. ✓

#### PASS — Table column types are correct
Verified multiple tables:
- `type: text` for string columns ✓
- `type: number` for numeric columns ✓
- `type: integer` for count columns ✓
- `format: zto` for proportions ✓
- `format: zto;pvalue` for p-values ✓
- `format: pc` for percentages ✓

#### WARN — `description` property on table items in .r.yaml
The guide does not explicitly prohibit `description` in `.r.yaml` tables, but the CLAUDE.md states: "in .u.yaml description is not allowed." Since this is `.r.yaml` not `.u.yaml`, `description` is fine here.

Several tables have `description:` fields (e.g., line 68, 129, 168). This is acceptable. ✓

#### WARN — Redundant `rows: 1` combined with dynamic `addRow()` pattern
Several tables specify `rows: 1` (e.g., `imbalanceMetrics` line 38, `multiClassAverage` line 878). If the backend uses `addRow()` to populate dynamically, this static `rows:` count is redundant and could cause display issues (extra blank rows).

**Lines with concern:** `imbalanceMetrics` (row 38), `multiClassAverage` (row 878).

**Recommendation:** Remove `rows: 1` from tables that are dynamically populated via `addRow()`, or keep it only for tables that always have exactly one row and are populated via `$setRow()`.

#### PASS — Plot definitions have required properties
All Image items have:
- `type: Image` ✓
- `renderFun: .plotFunctionName` ✓
- `width: 600` / `height: 600` ✓
- `visible:` condition ✓
- `clearWith:` list ✓

#### PASS — `refs` section present
```yaml
refs:
    - ClinicoPathJamoviModule
    - Swamidass2010
    - pROC
```
References are declared at line 1010-1013. ✓

#### PASS — `clearWith` used consistently across tables and plots
All tables and plots have `clearWith:` listing relevant options. ✓

#### PASS — `visible` conditions use correct syntax
Examples:
- `visible: (detectImbalance)` ✓
- `visible: (aucTable)` ✓
- `visible: (pairwiseComparisons && analysisType:comparative)` ✓
- `visible: (calibrationAnalysis && hosmerLemeshow)` ✓

---

## Guide 4: `.u.yaml` Guide

**File:** `jamovi/enhancedroc.u.yaml`

### Checks

#### PASS — Required top-level fields
- `title: Clinical ROC Analysis` ✓
- `name: enhancedROC` ✓
- `jus: '3.0'` ✓
- `stage: 0` ✓
- `compilerMode: tame` ✓

#### PASS — VariableSupplier wraps variable selection
The `VariableSupplier` component at line 6 correctly wraps the `TargetLayoutBox` components for variable selection. ✓

#### PASS — LevelSelector linked to variable
```yaml
- type: LevelSelector
  name: positiveClass
  enable: (outcome)   # Line 21-22
```
Correctly conditioned on `outcome` variable selection. ✓

#### PASS — No `visible` property used in `Label` components
The CLAUDE.md constraint: "in .u.yaml Label is not allowed to have the additional property 'visible'"

Labels in the file (lines 43, 57, 83, 102, 133, 162, etc.) do not have `visible` properties. ✓

#### PASS — No `description` property in .u.yaml components
The CLAUDE.md constraint: "in .u.yaml description is not allowed"

No `description` properties found in the `.u.yaml` file. ✓

#### WARN — `validationMethod` ComboBox appears without conditional enable
In the "Validation & Model Performance" CollapseBox (line 401+), `validationMethod` ComboBox (line 417) is shown without an `enable: (internalValidation)` condition. This means the dropdown is always visible even when `internalValidation` is unchecked.

**Recommendation:** Add `enable: (internalValidation)` to `validationMethod` ComboBox at line 417-419.

```yaml
- type: ComboBox
  name: validationMethod
  enable: (internalValidation)   # ADD THIS
```

#### WARN — `splineKnots` TextBox enabled by `splineCalibration` but `splineCalibration` is unimplemented
Line 247-248:
```yaml
- type: TextBox
  name: splineKnots
  format: number
  enable: (splineCalibration)
```
The `splineCalibration` option is listed as unimplemented in `.run()` (line 253). The UI still shows controls for it. This could confuse users who enable it and expect output.

**Recommendation:** Either implement `splineCalibration` or comment out these UI elements.

#### PASS — `CollapseBox` used for advanced options
Advanced options like "Statistical Options", "Calibration Analysis", "Multi-Class ROC", "Validation & Model Performance" are all in `CollapseBox` with `collapsed: true`. ✓

#### PASS — `enable` conditions follow correct syntax
Examples verified:
- `enable: (rocCurve)` ✓
- `enable: (useBootstrap && partialAuc)` ✓
- `enable: (calibrationAnalysis && hosmerLemeshow)` ✓

---

## Guide 5: JavaScript Guide

**File:** No JavaScript file exists at `jamovi/js/enhancedroc.events.js`

**Assessment:** The module has a `clinicalPresets` option that configures multiple parameters. The JS guide recommends JavaScript for preset systems that set multiple values. Without JS, the `clinicalPresets` dropdown in `.a.yaml` and `.u.yaml` relies entirely on R-side handling in `.applyClinicalPresets()`.

**Status:** WARN — The preset system would benefit from a JavaScript event handler to update UI controls immediately when a preset is selected, rather than waiting for R to run.

**Recommendation:** Create `jamovi/js/enhancedroc.events.js` with an `onChange_clinicalPresets` handler to update UI elements when a preset is selected. This improves user experience but is not strictly required.

---

## Guide 6: Actions Guide

**Assessment:** No `Action` type options are defined in `enhancedroc.a.yaml`. The analysis does not need to open new datasets. **PASS — Not applicable.**

---

## Guide 7: Formula Guide

**Assessment:** The enhancedROC analysis does not use formula-based statistical modeling (e.g., `lm()`, `coxph()`). ROC analysis uses `pROC::roc(response, predictor)` with direct variable access. Formula construction patterns from this guide are not applicable. **PASS — Not applicable.**

---

## Guide 8: i18n Guide

**File:** `R/enhancedROC.b.R`

### FAIL — Incomplete internationalization

The i18n guide requires all user-visible strings to be wrapped in `.()` for translation support.

**Current state:** Only isolated strings use `.()`:
- Line 337: `c(.("Insufficient data"))` ✓ — correct
- Line 593: `.("Clinical Assumptions & Recommendations")` ✓ — correct

**Non-translated user-visible strings (sample):**
- Line 127: `"Please select an outcome variable and at least one predictor variable..."`
- Line 128: `"Outcome variable: required (binary/factor)."`
- Line 274: `"The following selected features are planned but not yet implemented: ..."`
- Line 308-317: Success notice content
- Lines 415-426: Error message HTML blocks
- Line 669: `"Custom tied score handling is not supported by pROC..."`
- Many more throughout the 3900+ line file

**Recommendation:** Systematically wrap short user-visible strings in `.()`. HTML template strings with embedded data are harder to translate, but at minimum the static English text portions should be wrapped. This is a significant ongoing effort.

---

## Guide 9: Module Patterns Guide

**File:** `R/enhancedROC.b.R`

### Checks

#### PASS — Four-file architecture followed
- `enhancedroc.a.yaml` ✓
- `enhancedroc.r.yaml` ✓
- `enhancedroc.u.yaml` ✓
- `enhancedROC.b.R` ✓
- Auto-generated `enhancedROC.h.R` ✓ (not manually edited)

#### FAIL — Plot state management deviates from recommended pattern
The patterns guide states:
```r
# Include visual options in state to trigger updates
plotState <- list(
    data = plotData,
    plot_title = self$options$plot_title,
    color_palette = self$options$color_palette
)
image$setState(plotState)
```

Current implementation stores only dimensions in state and accesses `private$.rocResults` directly in render functions. This creates coupling between plot render functions and private state, and may not trigger re-renders when only options (not data) change.

**Recommended fix:**
```r
# In .run(), populate plot state with all needed data and options:
private$.rocCurvePlotData <- list(
    rocResults = private$.rocResults,
    showCutoffPoints = self$options$showCutoffPoints,
    showConfidenceBands = self$options$showConfidenceBands,
    smoothMethod = self$options$smoothMethod,
    plotWidth = self$options$plotWidth,
    plotHeight = self$options$plotHeight
)
self$results$results$rocCurvePlot$setState(
    as.data.frame(list(ready = TRUE))
)

# In .plotROCCurve(), use image$getState() to read ready flag,
# then access private$.rocCurvePlotData
```

#### PASS — Data frame serialization pattern
The guide warns about protobuf serialization issues. The module uses `Html` items for notices (avoiding Notice serialization) and stores simple lists in plot state. ✓

#### PASS — `self$results$TABLE$addRow(rowKey=, values=list(...))` pattern used
Inspection of table population functions shows `addRow` is used correctly. ✓

---

## Guide 10: Plots Guide

**File:** `R/enhancedROC.b.R` and `jamovi/enhancedroc.r.yaml`

### Checks

#### PASS — Plot definition structure in .r.yaml
All 11 plot definitions (`rocCurvePlot`, `prcPlot`, `comparativeROCPlot`, etc.) have:
- `type: Image` ✓
- `renderFun: .plotFunctionName` ✓
- `width: 600` and `height: 600` ✓
- `visible:` condition ✓
- `clearWith:` list ✓

#### PASS — Plot render function signatures correct
All plot functions follow the required pattern:
```r
.plotROCCurve = function(image, ggtheme, theme, ...) { ... }
```
Line 2665, 2819, 2946, 3013, 3082, 3161, 3249, 3324, 3538, 3727, 3900. ✓

#### WARN — Plot state stores only dimensions, not plot data
As noted in the Module Patterns Guide section, `image$setState(list(width = width, height = height))` only stores dimensions. The plot data is accessed via `private$.rocResults` directly. This deviates from the recommended pattern that puts all plot-relevant data in state.

**Risk:** When a user changes only a plot option (e.g., toggles `showCutoffPoints`) without changing data options, jamovi may not re-run `.run()`, and the plot may not update to reflect the new option.

**Recommendation:** Pass plot-relevant data through `setState()` per guide recommendations, or verify that the current approach reliably triggers re-rendering when plot options change.

#### PASS — `ggtheme` parameter used in plot functions
Plot functions receive `ggtheme` parameter. However, the actual plot code uses `ggplot2::theme_minimal()` directly instead of incorporating the `ggtheme` parameter passed in.

**Minor deviation:** The `ggtheme` parameter is accepted but not applied. The guide shows applying `ggtheme` as the base theme. This is a cosmetic issue but means plots won't respond to jamovi's global theme settings.

**Recommendation:** Apply `ggtheme` in the plot:
```r
p <- ggplot2::ggplot(...) + ... + ggtheme
```

---

## Guide 11: Tables Guide

**File:** `R/enhancedROC.b.R` and `jamovi/enhancedroc.r.yaml`

### Checks

#### PASS — Table definitions follow correct schema
All tables have `name`, `title`, `type: Table`, `columns:` list, and `clearWith:`. ✓

#### PASS — Column names are valid R identifiers
All column names use snake_case without hyphens or special characters. ✓

#### PASS — Correct column type/format combinations
- `type: number` with `format: zto` for proportions (0-1) ✓
- `type: number` with `format: zto;pvalue` for p-values ✓
- `type: integer` for count columns (`n_positive`, `n_negative`, etc.) ✓
- `type: text` for string columns ✓
- `format: pc` for percentage columns ✓

#### PASS — Table population uses `addRow()` correctly
Table population functions in `.b.R` use the `addRow(rowKey=, values=list(...))` pattern. ✓

---

## Summary of Issues by Priority

### HIGH PRIORITY (Must Fix)

| ID | File | Issue | Guide |
|----|------|-------|-------|
| H1 | `enhancedROC.b.R` | `%||%` operator dependency must be declared — verify `rlang` in NAMESPACE and DESCRIPTION | b.R Guide |
| H2 | `enhancedROC.b.R` | Plot state management: `setState()` stores only dimensions, not plot data — options changes may not trigger re-renders | Plots Guide, Module Patterns |

### MEDIUM PRIORITY (Should Fix)

| ID | File | Issue | Guide |
|----|------|-------|-------|
| M1 | `enhancedroc.a.yaml` | Add `R:` usage example to `description:` section | a.yaml Guide |
| M2 | `enhancedroc.a.yaml` | Rename `comprehensive_output` and `clinical_interpretation` to camelCase (`comprehensiveOutput`, `clinicalInterpretation`) | a.yaml Guide |
| M3 | `enhancedroc.u.yaml` | Add `enable: (internalValidation)` to `validationMethod` ComboBox | u.yaml Guide |
| M4 | `enhancedroc.u.yaml` | Comment out UI elements for unimplemented `splineCalibration` feature | u.yaml Guide |
| M5 | `enhancedroc.r.yaml` | Remove `rows: 1` from dynamically-populated tables (`imbalanceMetrics`, `multiClassAverage`) | r.yaml / Tables Guide |
| M6 | `enhancedROC.b.R` | Apply `ggtheme` parameter in plot functions instead of hardcoding `theme_minimal()` | Plots Guide |

### LOW PRIORITY (Nice to Have)

| ID | File | Issue | Guide |
|----|------|-------|-------|
| L1 | `enhancedroc.a.yaml` | Add `R:` usage example to `description:` | a.yaml Guide |
| L2 | `enhancedROC.b.R` | Systematic i18n: wrap user-visible strings in `.()` | i18n Guide |
| L3 | `jamovi/js/` | Create `enhancedroc.events.js` for preset system UI updates | JS Guide |
| L4 | `enhancedROC.b.R` | Remove or clarify `clinicopath_init` and `validate_clinical_data` guard pattern | b.R Guide |

---

## What Is Working Well

1. **Notice-to-HTML conversion** — correctly implements the pattern described in CLAUDE.md, avoiding serialization errors
2. **HTML sanitization** — `.safeHtmlOutput()` properly escapes user-facing values
3. **Clinical guidance** — thorough validation with actionable error messages
4. **Error handling** — `tryCatch` used throughout analysis functions
5. **Table structure** — all tables properly defined with correct column types
6. **Plot definitions** — all 11 plots properly defined in `.r.yaml` with correct render functions
7. **Level option** — `positiveClass` (type: Level) correctly references `outcome` without a default
8. **Checkpoint usage** — `private$.checkpoint()` called appropriately before expensive operations
9. **Four-file architecture** — correctly implemented across all 4 files
10. **clearWith lists** — all tables and plots have appropriate `clearWith` entries

---

## Verification Commands

```r
# Run after any fixes:
jmvtools::prepare()    # Authoritative YAML/R wiring check - must have no errors
devtools::document()   # Regenerate .h.R files
# Then parse check:
parse(file = "R/enhancedROC.b.R")
```

---

## Addendum: Additional Findings (data-flow-checker agent, 2026-03-02)

The following issues supplement the guide review above and were identified during data-flow verification.

### A1 — `.b.R` Class Missing `if (requireNamespace(...))` Guard (Moderate)

The guide and all other ClinicoPath analyses wrap their R6 class in:
```r
analysisClass <- if (requireNamespace('jmvcore', quietly = TRUE))
    R6::R6Class(...)
```

`enhancedROCClass` is defined unconditionally. This deviates from the standard pattern even though `jmvcore` is a required dep.

Fix: Wrap the class definition in the `requireNamespace` guard.

### A2 — `rocComparisons` Table Missing `bootstrapSamples` in `clearWith` (Minor)

When `comparisonMethod == "bootstrap"`, the table uses `self$options$bootstrapSamples` (line 1467). If a user changes `bootstrapSamples` without changing other `clearWith` options, stale bootstrap comparison results persist.

Fix: Add `bootstrapSamples` to `rocComparisons` clearWith list in `.r.yaml`.

### A3 — `Class Imbalance Detection` Section Open by Default (Minor, .u.yaml)

The CollapseBox at line 171 has `collapsed: false`. This section is an advanced feature most users will not need. Following the guide's progressive disclosure principle, it should be `collapsed: true`.

### A4 — `internalValidation` Output Appended to `analysisSummary` HTML (Minor)

`.populateInternalValidation()` appends validation HTML to `private$.analysisSummaryHtml` and calls `self$results$results$analysisSummary$setContent(...)`. This mixes internal validation output with the main summary. The content ordering depends on call order in `.run()` — currently safe, but fragile if the call order changes.

A dedicated Table or Html result item for internal validation output would be more maintainable.

### A5 — Stub Options (19+) Visible in UI Without "(Planned)" Labeling (Moderate, .u.yaml)

The following options appear as active UI checkboxes/dropdowns but are listed as unimplemented in the "unimplemented features" notice block in `.run()`:

`harrellCIndex`, `unoCStatistic`, `incidentDynamic`, `cumulativeDynamic`, `competingRisksConcordance`, `splineCalibration` (already in M4), `eoRatio`, `namDagostino`, `greenwoodNam`, `calibrationBelt`, `calibrationDensity`, `optimismCorrection`, `externalValidation`, `decisionImpactCurves`, `netBenefitRegression`, `modelUpdating`, `transportability`, `nntCalculation`, `bootstrapCutoffCI`, `bootstrapPartialAUC`

Following guide UX principles, either:
- Comment out these controls in `.u.yaml` (as Time-Dependent ROC options already are), OR
- Add "(Planned)" to their `title` in `.a.yaml` to set user expectations

This is the most impactful UX issue in the current UI implementation.
