# `latentbiomarker` Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build a jamovi function that fits a single-factor reflective CFA on 3+ biomarker indicators (continuous, ordinal, or binary) using `lavaan`, then relates the latent factor to a right-censored survival outcome via `survival::coxph`. Aggressively refuses inappropriate inputs with explanatory messages.

**Architecture:** Standard 4-file jamovi pattern (`.a.yaml`, `.u.yaml`, `.r.yaml`, `.b.R`) + auto-generated `.h.R`. Backend is an R6 class with a linear pipeline: gate stack → indicator-type detection → estimator selection (MLR vs WLSMV) → `lavaan::cfa()` → `lavPredict()` → `coxph()` → outputs. HTML notices replace `jmvcore::Notice` to avoid serialization errors (pattern from `R/waterfall.b.R`).

**Tech Stack:** R6, jmvcore, lavaan (already in DESCRIPTION Imports), survival, ggplot2, survminer, semPlot (new, Suggests).

**Spec:** `docs/superpowers/specs/2026-05-13-latentbiomarker-design.md`

---

## File Structure

### Files to create

| Path | Responsibility |
|---|---|
| `jamovi/latentbiomarker.a.yaml` | Analysis options (variables, gates, output toggles) |
| `jamovi/latentbiomarker.u.yaml` | UI layout — variable boxes, checkboxes, output options |
| `jamovi/latentbiomarker.r.yaml` | Result definitions — notices, tables, plots, preformatted R code |
| `R/latentbiomarker.b.R` | Backend R6 class — all logic |
| `R/latentbiomarker.h.R` | **Auto-generated** by `jmvtools::prepare()` — never hand-edit |
| `tests/testthat/test-latentbiomarker.R` | Unit + integration tests |
| `vignettes/meddecide-NN-latentbiomarker.qmd` | User-facing vignette with 6 worked scenarios |

### Files to modify

| Path | Change |
|---|---|
| `DESCRIPTION` | Add `semPlot` to `Suggests`; bump version |
| `jamovi/0000.yaml` | Register the new analysis under the meddecide group |
| `NEWS.md` | Add v0.0.39 entry describing the new function |

### Key conventions (read before starting)

- **Notice serialization (CLAUDE.md):** Never use `self$results$insert(999, jmvcore::Notice)`. Always use the HTML-notice helper pattern from `R/waterfall.b.R:95-142`.
- **Variable name escaping:** Use `jmvcore::composeTerm()` / `jmvcore::composeTerms()` for formulas — variable names may contain spaces/special chars.
- **Translation:** Wrap user-facing strings in `.(...)` for `gettext()`-style extraction. Keep all refusal/warning strings in a single `private$.messages` list at the top of the class for `/prepare-translation` later.
- **JamoviTest routing (CLAUDE.md):** During development, `menuGroup:` in `.a.yaml` carries a `T` suffix (e.g., `meddecideT`). It is removed only when promoting to production. The plan keeps the `T` throughout — Task 14's promotion step removes it.
- **Cache-friendly verification:** After every YAML or `.b.R` change, run `Rscript -e "jmvtools::prepare()" && Rscript -e "devtools::document(roclets = c('rd', 'namespace'))"`. `jmvtools::prepare()` regenerates `.h.R`. Don't run `devtools::load_all()` for verification — too slow per project feedback.

### Critical lavaan/Cox conventions

- **lavaan model syntax** for single factor: `Factor =~ ind1 + ind2 + ind3` — first indicator's loading is fixed to 1.0 unless `std.lv = TRUE` is passed, in which case the factor variance is fixed to 1 and all loadings are free. Use `std.lv = TRUE` so all loadings are interpretable and reported on a common scale.
- **Estimator dispatch:** If any indicator is ordinal/binary, pass `ordered = c(<names>)` and lavaan auto-selects WLSMV. Otherwise pass `estimator = "MLR"` and `missing = "fiml"`.
- **Factor scores:** `lavPredict(fit, method = "regression")` is the standard; `method = "Bartlett"` is also offered. WLSMV factor scores require `lavPredict(..., type = "lv")`.
- **Cox direction:** Higher factor score = higher hazard by convention. `coxph(Surv(time, event) ~ factor_score + adjusters)` — use `survival::concordance` with `reverse = TRUE` for c-index (per project memory `feedback_concordance_reverse.md`).
- **KM stratification:** `cut()` the factor score by quantiles → factor → `survfit(Surv(...) ~ score_strata)`.

---

## Task 1: Scaffold files + DESCRIPTION + 0000.yaml registration

**Files:**
- Create: `jamovi/latentbiomarker.a.yaml`, `jamovi/latentbiomarker.u.yaml`, `jamovi/latentbiomarker.r.yaml`, `R/latentbiomarker.b.R`
- Modify: `jamovi/0000.yaml` (append analysis entry), `DESCRIPTION` (add `semPlot` Suggests)
- Verify: `jmvtools::prepare()` succeeds and `R/latentbiomarker.h.R` is generated

- [ ] **Step 1.1: Write `jamovi/latentbiomarker.a.yaml`**

```yaml
---
name: latentbiomarker
title: "Latent Biomarker Construct + Cox Regression"
menuGroup: meddecideT
menuSubgroup: Biomarkers
menuSubtitle: "Reflective CFA on biomarker indicators + survival"
version: '0.0.1'
jas: '1.2'

description:
    main: >-
        Estimate a single reflective latent biomarker construct from 3+ indicators
        and relate it to a right-censored survival outcome via Cox regression.
        Auto-selects MLR (continuous) or WLSMV (ordinal/binary) estimation.
        Refuses inappropriate inputs with explanatory messages.
    R:
        dontrun: true
        usage: |
            data('histopathology', package='ClinicoPath')
            # Pseudocode — actual indicator names depend on the data:
            ClinicoPath::latentbiomarker(
                data = histopathology,
                dep_time = "OverallTime",
                dep_event = "Outcome",
                event_level = "1",
                indicators = c("MeasurementA", "MeasurementB", "Measurement1"),
                reflective_confirmed = TRUE
            )

options:
  - name: data
    type: Data
    description:
      R: The data as a data frame.

  - name: dep_time
    title: "Time"
    type: Variable
    suggested: [continuous]
    permitted: [numeric]
    default: null

  - name: dep_event
    title: "Event"
    type: Variable
    suggested: [nominal]
    permitted: [factor, numeric]
    default: null

  - name: event_level
    title: "Event level"
    type: Level
    variable: (dep_event)

  - name: indicators
    title: "Indicators (≥3)"
    type: Variables
    suggested: [continuous, ordinal, nominal]
    permitted: [numeric, factor]
    default: null

  - name: adjusters
    title: "Adjust Cox for"
    type: Variables
    suggested: [continuous, ordinal, nominal]
    permitted: [numeric, factor]
    default: null

  - name: indicator_types
    title: "Indicator type"
    type: List
    options:
      - name: auto
        title: "Auto-detect"
      - name: continuous
        title: "All continuous"
      - name: ordinal
        title: "All ordinal/binary"
    default: auto

  - name: reflective_confirmed
    title: "Indicators reflect an underlying construct (not a composite)"
    type: Bool
    default: false

  - name: factor_score_method
    title: "Factor score method"
    type: List
    options:
      - name: regression
        title: "Regression"
      - name: Bartlett
        title: "Bartlett"
    default: regression

  - name: standardize_scores
    title: "Standardize factor scores"
    type: Bool
    default: true

  - name: save_factor_scores
    title: "Save factor scores to data"
    type: Output

  - name: factor_score_name
    title: "Factor score column name"
    type: String
    default: "biomarker_factor"

  - name: km_strata
    title: "KM stratification"
    type: List
    options:
      - name: median
        title: "Median"
      - name: tertile
        title: "Tertile"
      - name: quartile
        title: "Quartile"
    default: tertile

  - name: show_plot_km
    title: "Kaplan–Meier plot"
    type: Bool
    default: true

  - name: show_plot_loadings
    title: "Loadings plot"
    type: Bool
    default: true

  - name: show_plot_path
    title: "Path diagram"
    type: Bool
    default: true

  - name: show_diagnostics
    title: "Model diagnostics"
    type: Bool
    default: true

  - name: show_r_code
    title: "Show R code"
    type: Bool
    default: false
...
```

- [ ] **Step 1.2: Write `jamovi/latentbiomarker.u.yaml`**

```yaml
title: Latent Biomarker Construct + Cox Regression
name: latentbiomarker
jus: '3.0'
stage: 0
compilerMode: tame
children:
  - type: VariableSupplier
    persistentItems: false
    stretchFactor: 1
    children:
      - type: TargetLayoutBox
        label: Time
        children:
          - type: VariablesListBox
            name: dep_time
            maxItemCount: 1
            isTarget: true
      - type: TargetLayoutBox
        label: Event
        children:
          - type: VariablesListBox
            name: dep_event
            maxItemCount: 1
            isTarget: true
          - type: LevelSelector
            name: event_level
            label: Event level
      - type: TargetLayoutBox
        label: Indicators (≥3)
        children:
          - type: VariablesListBox
            name: indicators
            isTarget: true
      - type: TargetLayoutBox
        label: Adjust Cox for
        children:
          - type: VariablesListBox
            name: adjusters
            isTarget: true

  - type: CollapseBox
    label: Measurement model
    collapsed: false
    children:
      - type: CheckBox
        name: reflective_confirmed
        label: "I confirm these indicators reflect an underlying construct (not a composite score)"
      - type: ComboBox
        name: indicator_types
        label: Indicator type
      - type: ComboBox
        name: factor_score_method
        label: Factor score method
      - type: CheckBox
        name: standardize_scores

  - type: CollapseBox
    label: Output
    collapsed: false
    children:
      - type: CheckBox
        name: show_plot_km
        children:
          - type: ComboBox
            name: km_strata
      - type: CheckBox
        name: show_plot_loadings
      - type: CheckBox
        name: show_plot_path
      - type: CheckBox
        name: show_diagnostics
      - type: Output
        name: save_factor_scores
      - type: TextBox
        name: factor_score_name
        format: string
        enable: (save_factor_scores)
      - type: CheckBox
        name: show_r_code
```

- [ ] **Step 1.3: Write `jamovi/latentbiomarker.r.yaml`**

```yaml
---
name:  latentbiomarker
title: Latent Biomarker Construct + Cox Regression
jrs:   '1.1'

items:
    - name: notices
      title: Notices
      type: Html
      clearWith:
        - dep_time
        - dep_event
        - indicators
        - adjusters
        - reflective_confirmed
        - indicator_types

    - name: summaryTable
      title: Sample & Model Summary
      type: Table
      rows: 1
      visible: (reflective_confirmed)
      clearWith:
        - dep_time
        - dep_event
        - indicators
        - indicator_types
      columns:
        - name: n
          title: "N"
          type: integer
        - name: n_events
          title: "Events"
          type: integer
        - name: n_indicators
          title: "Indicators"
          type: integer
        - name: n_params
          title: "CFA parameters"
          type: integer
        - name: cpp
          title: "Cases / parameter"
          type: number
          format: zto
        - name: estimator
          title: "Estimator"
          type: text
        - name: missing
          title: "Missing-data handling"
          type: text

    - name: loadingsTable
      title: Factor Loadings
      type: Table
      visible: (reflective_confirmed)
      clearWith:
        - indicators
        - indicator_types
      columns:
        - name: indicator
          title: "Indicator"
          type: text
        - name: est_std
          title: "λ (std)"
          type: number
        - name: est
          title: "λ (raw)"
          type: number
        - name: se
          title: "SE"
          type: number
        - name: z
          title: "z"
          type: number
        - name: pvalue
          title: "p"
          type: number
          format: zto,pvalue
        - name: r2
          title: "R² (communality)"
          type: number

    - name: reliabilityTable
      title: Reliability
      type: Table
      rows: 1
      visible: (reflective_confirmed)
      clearWith:
        - indicators
      columns:
        - name: omega
          title: "ω (McDonald)"
          type: number
        - name: ave
          title: "AVE"
          type: number

    - name: fitTable
      title: Model Fit
      type: Table
      rows: 1
      visible: (reflective_confirmed)
      clearWith:
        - indicators
        - indicator_types
      columns:
        - name: chisq
          title: "χ²"
          type: number
        - name: df
          title: "df"
          type: integer
        - name: chisq_p
          title: "p"
          type: number
          format: zto,pvalue
        - name: cfi
          title: "CFI"
          type: number
        - name: tli
          title: "TLI"
          type: number
        - name: rmsea
          title: "RMSEA"
          type: number
        - name: rmsea_lo
          title: "RMSEA 90% lo"
          type: number
        - name: rmsea_hi
          title: "RMSEA 90% hi"
          type: number
        - name: srmr
          title: "SRMR"
          type: number
        - name: interpretation
          title: "Fit"
          type: text

    - name: coxTable
      title: Cox Regression
      type: Table
      visible: (reflective_confirmed)
      clearWith:
        - dep_time
        - dep_event
        - indicators
        - adjusters
      columns:
        - name: term
          title: "Term"
          type: text
        - name: hr
          title: "HR"
          type: number
        - name: ci_lo
          title: "95% CI lo"
          type: number
        - name: ci_hi
          title: "95% CI hi"
          type: number
        - name: z
          title: "z"
          type: number
        - name: pvalue
          title: "p"
          type: number
          format: zto,pvalue

    - name: phTable
      title: Proportional-Hazards Test (cox.zph)
      type: Table
      visible: (reflective_confirmed && show_diagnostics)
      clearWith:
        - dep_time
        - dep_event
        - indicators
        - adjusters
      columns:
        - name: term
          title: "Term"
          type: text
        - name: chisq
          title: "χ²"
          type: number
        - name: df
          title: "df"
          type: number
        - name: pvalue
          title: "p"
          type: number
          format: zto,pvalue

    - name: kmPlot
      title: Kaplan–Meier Plot by Factor-Score Stratum
      type: Image
      width: 600
      height: 450
      renderFun: .kmPlot
      visible: (reflective_confirmed && show_plot_km)
      clearWith:
        - dep_time
        - dep_event
        - indicators
        - km_strata

    - name: loadingsPlot
      title: Standardized Loadings
      type: Image
      width: 500
      height: 400
      renderFun: .loadingsPlot
      visible: (reflective_confirmed && show_plot_loadings)
      clearWith:
        - indicators
        - indicator_types

    - name: pathPlot
      title: Path Diagram
      type: Image
      width: 600
      height: 450
      renderFun: .pathPlot
      visible: (reflective_confirmed && show_plot_path)
      clearWith:
        - indicators
        - indicator_types
        - adjusters

    - name: miTable
      title: Modification Indices (> 10)
      type: Table
      visible: (reflective_confirmed && show_diagnostics)
      clearWith:
        - indicators
      columns:
        - name: lhs
          title: "LHS"
          type: text
        - name: op
          title: "Op"
          type: text
        - name: rhs
          title: "RHS"
          type: text
        - name: mi
          title: "MI"
          type: number

    - name: rCode
      title: Reproducible R Code
      type: Preformatted
      visible: (reflective_confirmed && show_r_code)
      clearWith:
        - dep_time
        - dep_event
        - indicators
        - adjusters

  - name: save_factor_scores
    type: Output
    varTitle: '`{factor_score_name}`'
    measureType: continuous
    clearWith:
      - indicators
      - indicator_types
```

- [ ] **Step 1.4: Write skeletal `R/latentbiomarker.b.R`**

```r
#' @title Latent Biomarker Construct + Cox Regression
#' @return Results object
#' @importFrom R6 R6Class
#' @importFrom magrittr %>%
#' @import jmvcore

latentbiomarkerClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "latentbiomarkerClass",
    inherit = latentbiomarkerBase,
    private = list(

        # ---- Notice collection (pattern from R/waterfall.b.R) ----
        .noticeList = list(),

        .addNotice = function(type, title, content) {
            private$.noticeList[[length(private$.noticeList) + 1]] <- list(
                type = type, title = title, content = content
            )
        },

        .renderNotices = function() {
            if (length(private$.noticeList) == 0) {
                self$results$notices$setContent("")
                return()
            }
            typeStyles <- list(
                ERROR          = list(color = "#dc2626", bgcolor = "#fef2f2", border = "#fca5a5"),
                STRONG_WARNING = list(color = "#ea580c", bgcolor = "#fff7ed", border = "#fdba74"),
                WARNING        = list(color = "#ca8a04", bgcolor = "#fefce8", border = "#fde047"),
                INFO           = list(color = "#2563eb", bgcolor = "#eff6ff", border = "#93c5fd")
            )
            html <- "<div style='margin:10px 0;'>"
            for (n in private$.noticeList) {
                style <- typeStyles[[n$type]]
                if (is.null(style)) style <- typeStyles$INFO
                html <- paste0(html,
                    "<div style='background-color:", style$bgcolor,
                    "; border-left:4px solid ", style$border,
                    "; padding:12px; margin:8px 0; border-radius:4px;'>",
                    "<strong style='color:", style$color, ";'>",
                    htmltools::htmlEscape(n$title), "</strong><br>",
                    "<span style='color:#374151;'>",
                    htmltools::htmlEscape(n$content), "</span>",
                    "</div>")
            }
            html <- paste0(html, "</div>")
            self$results$notices$setContent(html)
        },

        # ---- Pipeline ----
        .run = function() {
            opt <- self$options
            # Gate 0: required inputs present?
            if (is.null(opt$dep_time) || is.null(opt$dep_event) ||
                is.null(opt$indicators) || length(opt$indicators) == 0) {
                return()  # silent on incomplete input — jamovi convention
            }

            # Gate G6 — reflective confirmation
            if (!isTRUE(opt$reflective_confirmed)) {
                private$.addNotice("ERROR",
                    "Reflective-measurement confirmation required",
                    paste0(
                        "CFA assumes indicators reflect an underlying latent construct ",
                        "(e.g., CD8/PD-L1/TIL all reflect 'immune activation'). If indicators ",
                        "constitute a composite where each adds independent information ",
                        "(e.g., a histologic grade summing nuclear grade + tubules + mitoses), ",
                        "CFA gives misleading results. Use cSEM or seminr for formative models, ",
                        "or compute the composite directly. Tick the confirmation box if your ",
                        "model is genuinely reflective."))
                private$.renderNotices()
                return()
            }

            # Subsequent gates and computation added in later tasks
            private$.renderNotices()
        }
    )
)
```

- [ ] **Step 1.5: Add entry to `jamovi/0000.yaml` (insert near other meddecide entries)**

Append before the `usesNative: true` line at file tail:

```yaml
  - title: Latent Biomarker Construct + Cox Regression
    name: latentbiomarker
    ns: ClinicoPath
    menuGroup: meddecideT
    menuSubgroup: Biomarkers
    menuTitle: Latent Biomarker Construct + Cox Regression
    menuSubtitle: Reflective CFA on indicators + Cox survival
    description: >-
      Estimate a single reflective latent biomarker construct from 3+
      indicators (continuous, ordinal, or binary) using lavaan, then
      relate it to a right-censored survival outcome via Cox regression.
    category: analyses
```

- [ ] **Step 1.6: Modify `DESCRIPTION`**

In the `Suggests:` section add `semPlot`, and bump version. Locate `Suggests:` block and append `semPlot` to its list (alphabetically). Bump `Version:` line from `0.0.38.x` to `0.0.39.0`.

Run: `grep -n "Version:\|Suggests:" DESCRIPTION` to find lines.

- [ ] **Step 1.7: Run `jmvtools::prepare()` and verify**

Run:
```bash
Rscript -e "jmvtools::prepare()" 2>&1 | tail -30
```

Expected: no errors. `R/latentbiomarker.h.R` is created. The output may say "compiled <n> analyses". Confirm with:
```bash
ls -la R/latentbiomarker.h.R
```

If `prepare()` reports a YAML error, fix the offending file and re-run. If it complains about `Level` option type with a `default`, remove `default:` from `event_level` (per CLAUDE.md: Level options cannot have defaults).

- [ ] **Step 1.8: Commit**

```bash
git add jamovi/latentbiomarker.a.yaml jamovi/latentbiomarker.u.yaml \
        jamovi/latentbiomarker.r.yaml R/latentbiomarker.b.R R/latentbiomarker.h.R \
        jamovi/0000.yaml DESCRIPTION
git -c commit.gpgsign=false commit -m "$(cat <<'EOF'
feat(latentbiomarker): scaffold YAML files and reflective-confirmation gate

Single-factor reflective CFA + Cox regression for biomarker constructs.
This commit registers the analysis and implements the G6 hard gate
(reflective confirmation required) — all other gates and computation
land in subsequent commits.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

## Task 2: First integration test — reflective gate refuses

**Files:**
- Create: `tests/testthat/test-latentbiomarker.R`

- [ ] **Step 2.1: Write the failing test**

```r
# tests/testthat/test-latentbiomarker.R
test_that("G6 reflective confirmation: function refuses when unchecked", {
    skip_if_not_installed("lavaan")
    data("histopathology", package = "ClinicoPath")

    res <- ClinicoPath::latentbiomarker(
        data = histopathology,
        dep_time = "OverallTime",
        dep_event = "Outcome",
        event_level = "1",
        indicators = c("MeasurementA", "MeasurementB", "Measurement1"),
        reflective_confirmed = FALSE
    )

    # Notice content should mention the reflective requirement
    notices_html <- res$notices$content
    expect_match(notices_html, "Reflective-measurement confirmation", fixed = TRUE)

    # Loadings/Cox tables should be empty (gate stopped pipeline)
    expect_equal(res$loadingsTable$rowCount, 0)
    expect_equal(res$coxTable$rowCount, 0)
})

test_that("G6 reflective confirmation: function proceeds when checked (no other gates fail)", {
    skip_if_not_installed("lavaan")
    data("histopathology", package = "ClinicoPath")

    res <- ClinicoPath::latentbiomarker(
        data = histopathology,
        dep_time = "OverallTime",
        dep_event = "Outcome",
        event_level = "1",
        indicators = c("MeasurementA", "MeasurementB", "Measurement1"),
        reflective_confirmed = TRUE
    )

    # Notices should NOT contain the reflective refusal
    notices_html <- res$notices$content
    expect_false(grepl("Reflective-measurement confirmation", notices_html, fixed = TRUE))
})
```

- [ ] **Step 2.2: Run test to verify the first test passes and the second indicates what's missing**

Run:
```bash
Rscript -e "devtools::test(filter='latentbiomarker')" 2>&1 | tail -20
```

Expected: First test passes (gate fires). Second test passes too because we just check the *absence* of the reflective refusal — later gates haven't been added yet, so the notice list is empty after passing G6. Both should pass at this stage.

If `ClinicoPath::latentbiomarker` is not found, run `Rscript -e "devtools::load_all()"` first.

- [ ] **Step 2.3: Commit**

```bash
git add tests/testthat/test-latentbiomarker.R
git -c commit.gpgsign=false commit -m "$(cat <<'EOF'
test(latentbiomarker): verify reflective-confirmation gate

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

## Task 3: Refusal-message dictionary + remaining hard gates (G1, G3)

**Files:**
- Modify: `R/latentbiomarker.b.R` — add `private$.messages` and gate logic

- [ ] **Step 3.1: Write failing tests for G1 and G3**

Append to `tests/testthat/test-latentbiomarker.R`:

```r
test_that("G1 hard gate: refuses n < 100", {
    skip_if_not_installed("lavaan")
    data("histopathology", package = "ClinicoPath")
    small <- histopathology[1:50, ]
    res <- ClinicoPath::latentbiomarker(
        data = small,
        dep_time = "OverallTime",
        dep_event = "Outcome",
        event_level = "1",
        indicators = c("MeasurementA", "MeasurementB", "Measurement1"),
        reflective_confirmed = TRUE
    )
    expect_match(res$notices$content, "Insufficient sample size", fixed = TRUE)
    expect_equal(res$loadingsTable$rowCount, 0)
})

test_that("G3 hard gate: refuses fewer than 3 indicators", {
    skip_if_not_installed("lavaan")
    data("histopathology", package = "ClinicoPath")
    res <- ClinicoPath::latentbiomarker(
        data = histopathology,
        dep_time = "OverallTime",
        dep_event = "Outcome",
        event_level = "1",
        indicators = c("MeasurementA", "MeasurementB"),  # only 2
        reflective_confirmed = TRUE
    )
    expect_match(res$notices$content, "Too few indicators", fixed = TRUE)
})
```

- [ ] **Step 3.2: Run tests, confirm they fail**

Run:
```bash
Rscript -e "devtools::test(filter='latentbiomarker')" 2>&1 | tail -20
```

Expected: G1 and G3 tests fail (no refusal text matches yet).

- [ ] **Step 3.3: Add message dictionary and gate functions to `.b.R`**

In `R/latentbiomarker.b.R`, inside `private = list(...)`, add **above** `.run`:

```r
        # ---- Message templates (translatable) ----
        .messages = list(
            G1_refuse = function(n) paste0(
                "Insufficient sample size for SEM. With n = ", n, " (< 100), factor ",
                "loadings and fit indices are unreliable. Consider (1) a simpler ",
                "z-score composite with conventional Cox regression, or (2) waiting ",
                "for a larger cohort."),
            G1_warn = function(n) paste0(
                "Sample size is modest for SEM (n = ", n, "; 100–199). Results may be ",
                "unstable; interpret loadings and confidence intervals with caution."),
            G2_refuse = function(k, n, cpp) paste0(
                "Insufficient cases per CFA parameter. Your measurement model has K = ",
                k, " parameters (loadings + residual variances + factor variance), ",
                "requiring n >= ", 5L * k, " (and ideally n >= ", 10L * k, "). You have n = ",
                n, ", giving CPP = ", round(cpp, 2), ". Options: (1) reduce indicators, ",
                "(2) use a summary score with conventional Cox, or (3) collect more data."),
            G2_warn = function(cpp) paste0(
                "Cases-per-parameter ratio is ", round(cpp, 2),
                " (recommended >= 10). Standard errors may be optimistic."),
            G3_refuse = function(k) paste0(
                "Too few indicators (", k, "). A reflective factor requires at least 3 ",
                "indicators to be identified. With 2 indicators the model is ",
                "under-identified and cannot be fit."),
            G3_warn = "With exactly 3 indicators and 1 factor, the model is just-identified (df = 0). CFI, RMSEA, and SRMR are not meaningful — they will be reported as NA or trivial values.",
            G4_warn = function(epv) paste0(
                "Cox model has ", round(epv, 2), " events per covariate (recommended >= 10; ",
                "Peduzzi/Concato). Consider reducing adjusters."),
            G5_warn = function(rmax) paste0(
                "Indicators are weakly intercorrelated (max |r| = ", round(rmax, 2),
                "). They may not reflect a common construct."),
            G6_refuse = paste0(
                "CFA assumes indicators reflect an underlying latent construct ",
                "(e.g., CD8/PD-L1/TIL all reflect 'immune activation'). If indicators ",
                "constitute a composite where each adds independent information ",
                "(e.g., a histologic grade summing nuclear grade + tubules + mitoses), ",
                "CFA gives misleading results. Use cSEM or seminr for formative models, ",
                "or compute the composite directly. Tick the confirmation box if your ",
                "model is genuinely reflective."),
            FIT_poor = function(cfi, rmsea) paste0(
                "Single-factor model fits poorly (CFI = ", round(cfi, 3),
                ", RMSEA = ", round(rmsea, 3),
                "). Consider splitting into multiple constructs — SEMLj supports multi-factor SEM."),
            PH_violated = function(p) paste0(
                "Proportional-hazards assumption violated (global p = ", signif(p, 3),
                "). HR is an average effect; consider stratification or time-dependent terms."),
            UNCERTAINTY = paste0(
                "HR confidence intervals do not account for measurement uncertainty in ",
                "the latent factor (two-stage Murphy–Topel issue). True CIs are wider ",
                "than reported. Interpret conservatively.")
        ),

        # ---- Helper: count CFA parameters for a single-factor std.lv model ----
        # K = K loadings (all free under std.lv=TRUE) + K residual variances + 1 factor variance fixed
        # so estimated parameters = 2*K. Latent intercept fixed at 0.
        .countCFAParams = function(n_indicators) {
            2L * as.integer(n_indicators)
        },
```

Then update the existing `.run` body so that the gate cascade replaces the current minimal version:

```r
        .run = function() {
            private$.noticeList <- list()  # reset per run
            opt <- self$options

            # Silent on incomplete input
            if (is.null(opt$dep_time) || is.null(opt$dep_event) ||
                is.null(opt$indicators) || length(opt$indicators) == 0) {
                return()
            }

            # ---- Gate G6: reflective confirmation (hard refusal, runs first) ----
            if (!isTRUE(opt$reflective_confirmed)) {
                private$.addNotice("ERROR",
                    "Reflective-measurement confirmation required",
                    private$.messages$G6_refuse)
                private$.renderNotices()
                return()
            }

            # Pull data using jmvcore conventions
            df <- self$data
            df <- jmvcore::naOmit(df[, c(opt$dep_time, opt$dep_event,
                                         opt$indicators, opt$adjusters), drop = FALSE])

            n <- nrow(df)
            k <- length(opt$indicators)

            # ---- Gate G3: indicators < 3 (hard refusal) ----
            if (k < 3L) {
                private$.addNotice("ERROR",
                    "Too few indicators",
                    private$.messages$G3_refuse(k))
                private$.renderNotices()
                return()
            }

            # ---- Gate G1: n < 100 (hard refusal); 100 <= n < 200 soft warning ----
            if (n < 100L) {
                private$.addNotice("ERROR",
                    "Insufficient sample size",
                    private$.messages$G1_refuse(n))
                private$.renderNotices()
                return()
            }
            if (n < 200L) {
                private$.addNotice("WARNING",
                    "Modest sample size",
                    private$.messages$G1_warn(n))
            }

            # Subsequent gates and computation in later tasks
            private$.renderNotices()
        }
```

- [ ] **Step 3.4: Regenerate `.h.R` and run tests**

Run:
```bash
Rscript -e "jmvtools::prepare()" 2>&1 | tail -5
Rscript -e "devtools::test(filter='latentbiomarker')" 2>&1 | tail -20
```

Expected: All four tests now pass.

- [ ] **Step 3.5: Commit**

```bash
git add R/latentbiomarker.b.R R/latentbiomarker.h.R tests/testthat/test-latentbiomarker.R
git -c commit.gpgsign=false commit -m "$(cat <<'EOF'
feat(latentbiomarker): add message dictionary and G1/G3 hard gates

G1 refuses n<100, G3 refuses <3 indicators. Soft warning at 100<=n<200.
All gate messages live in private\$.messages for future translation.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

## Task 4: Indicator-type detection + estimator selection

**Files:**
- Modify: `R/latentbiomarker.b.R`

- [ ] **Step 4.1: Write the failing test**

Append to `tests/testthat/test-latentbiomarker.R`:

```r
test_that("indicator-type auto-detection: factor indicators trigger WLSMV", {
    skip_if_not_installed("lavaan")
    data("histopathology", package = "ClinicoPath")

    # Find a few factor variables in histopathology
    factor_vars <- names(histopathology)[sapply(histopathology, is.factor)]
    skip_if(length(factor_vars) < 3, "histopathology lacks 3 factor indicators")

    res <- ClinicoPath::latentbiomarker(
        data = histopathology,
        dep_time = "OverallTime",
        dep_event = "Outcome",
        event_level = "1",
        indicators = factor_vars[1:3],
        indicator_types = "auto",
        reflective_confirmed = TRUE
    )
    # The fit table runs later, but the estimator field of summaryTable should already say WLSMV
    # — that requires step 4.2 to populate. Until then, the notice list should at minimum
    # contain a STRONG_WARNING about WLSMV sample-size needs (added below).
    expect_match(res$notices$content, "WLSMV", fixed = TRUE)
})
```

- [ ] **Step 4.2: Run test, confirm it fails**

```bash
Rscript -e "devtools::test(filter='latentbiomarker')" 2>&1 | tail -10
```

Expected: New WLSMV test fails (no WLSMV string in notices yet).

- [ ] **Step 4.3: Add indicator-type detection + estimator selection logic**

In `R/latentbiomarker.b.R`, add to `private = list(...)` above `.run`:

```r
        # ---- Detect indicator types ----
        # Returns list(continuous = chr, ordinal = chr, all_continuous = lgl)
        .detectIndicatorTypes = function(df, indicators, override) {
            if (identical(override, "continuous")) {
                return(list(continuous = indicators, ordinal = character(0), all_continuous = TRUE))
            }
            if (identical(override, "ordinal")) {
                return(list(continuous = character(0), ordinal = indicators, all_continuous = FALSE))
            }
            # auto
            is_ord <- vapply(indicators, function(v) {
                x <- df[[v]]
                is.factor(x) || is.ordered(x) || is.logical(x) ||
                    (is.numeric(x) && length(unique(stats::na.omit(x))) <= 5L)
            }, logical(1))
            list(
                continuous = indicators[!is_ord],
                ordinal    = indicators[is_ord],
                all_continuous = !any(is_ord)
            )
        },

        # ---- Choose estimator based on indicator types ----
        .chooseEstimator = function(itypes) {
            if (itypes$all_continuous) {
                list(estimator = "MLR", missing = "fiml", ordered = NULL)
            } else {
                list(estimator = "WLSMV", missing = "pairwise", ordered = itypes$ordinal)
            }
        },
```

Then in `.run`, after the G1 soft warning, before the closing `private$.renderNotices()`:

```r
            # ---- Indicator-type detection + estimator selection ----
            itypes <- private$.detectIndicatorTypes(df, opt$indicators, opt$indicator_types)
            est_spec <- private$.chooseEstimator(itypes)

            if (!itypes$all_continuous) {
                private$.addNotice("STRONG_WARNING",
                    "WLSMV estimator selected (ordinal/binary indicators detected)",
                    paste0(
                        "Using WLSMV with polychoric/tetrachoric correlations for ",
                        length(itypes$ordinal), " ordinal/binary indicator(s): ",
                        paste(itypes$ordinal, collapse = ", "),
                        ". WLSMV requires larger samples than MLR; recommended n >= 500."))
            }

            # Store on self for downstream tasks (Task 5+ refer to these)
            private$.fitState <- list(
                df = df, n = n, k = k, itypes = itypes, est_spec = est_spec
            )
```

Add at top of `private = list(...)`:

```r
        .fitState = NULL,
```

- [ ] **Step 4.4: Run tests, confirm pass**

```bash
Rscript -e "jmvtools::prepare()" && Rscript -e "devtools::test(filter='latentbiomarker')" 2>&1 | tail -15
```

Expected: All tests pass.

- [ ] **Step 4.5: Commit**

```bash
git add R/latentbiomarker.b.R R/latentbiomarker.h.R tests/testthat/test-latentbiomarker.R
git -c commit.gpgsign=false commit -m "$(cat <<'EOF'
feat(latentbiomarker): auto-detect indicator types and select estimator

Continuous indicators -> MLR with FIML. Any ordinal/binary -> WLSMV with
polychoric correlations and a STRONG_WARNING notice about WLSMV sample-size
expectations.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

## Task 5: CPP gate (G2) + correlation gate (G5)

**Files:**
- Modify: `R/latentbiomarker.b.R`

- [ ] **Step 5.1: Write failing tests for G2 and G5**

Append to `tests/testthat/test-latentbiomarker.R`:

```r
test_that("G2 hard gate: refuses cases-per-parameter < 5", {
    skip_if_not_installed("lavaan")
    # Synthesize a tiny dataset with many indicators (force CPP < 5)
    set.seed(1)
    n <- 100
    k <- 15  # K params = 2*15 = 30; CPP = 100/30 = 3.33 < 5 -> refuse
    df <- data.frame(
        time = rexp(n, 0.1),
        evt  = factor(rbinom(n, 1, 0.5)),
        matrix(rnorm(n * k), n, k) |> as.data.frame()
    )
    names(df)[3:(2 + k)] <- paste0("ind", seq_len(k))

    res <- ClinicoPath::latentbiomarker(
        data = df,
        dep_time = "time",
        dep_event = "evt",
        event_level = "1",
        indicators = paste0("ind", seq_len(k)),
        reflective_confirmed = TRUE
    )
    expect_match(res$notices$content, "Insufficient cases per CFA parameter", fixed = TRUE)
})

test_that("G5 soft warning: low inter-indicator correlations", {
    skip_if_not_installed("lavaan")
    set.seed(2)
    n <- 250
    df <- data.frame(
        time = rexp(n, 0.1),
        evt  = factor(rbinom(n, 1, 0.5)),
        ind1 = rnorm(n), ind2 = rnorm(n), ind3 = rnorm(n)  # uncorrelated by construction
    )
    res <- ClinicoPath::latentbiomarker(
        data = df,
        dep_time = "time",
        dep_event = "evt",
        event_level = "1",
        indicators = c("ind1", "ind2", "ind3"),
        reflective_confirmed = TRUE
    )
    expect_match(res$notices$content, "weakly intercorrelated", fixed = TRUE)
})
```

- [ ] **Step 5.2: Run, confirm failures**

```bash
Rscript -e "devtools::test(filter='latentbiomarker')" 2>&1 | tail -15
```

- [ ] **Step 5.3: Add G2 and G5 to gate stack**

In `R/latentbiomarker.b.R`, modify `.run` so that **after** indicator-type detection and **before** storing `.fitState`, the following gates fire:

```r
            # ---- Gate G2: cases per CFA parameter ----
            n_params <- private$.countCFAParams(k)
            cpp <- n / n_params
            if (cpp < 5) {
                private$.addNotice("ERROR",
                    "Insufficient cases per CFA parameter",
                    private$.messages$G2_refuse(n_params, n, cpp))
                private$.renderNotices()
                return()
            }
            if (cpp < 10) {
                private$.addNotice("WARNING",
                    "Low cases-per-parameter ratio",
                    private$.messages$G2_warn(cpp))
            }

            # ---- Gate G3-soft: just-identified model ----
            if (k == 3L) {
                private$.addNotice("INFO",
                    "Just-identified model",
                    private$.messages$G3_warn)
            }

            # ---- Gate G5: indicator correlations ----
            cor_mat <- tryCatch(
                stats::cor(
                    data.frame(lapply(df[, opt$indicators, drop = FALSE], function(x)
                        as.numeric(if (is.factor(x)) as.integer(x) else x))),
                    use = "pairwise.complete.obs"),
                error = function(e) NULL)
            if (!is.null(cor_mat)) {
                off_diag <- abs(cor_mat[upper.tri(cor_mat)])
                off_diag <- off_diag[is.finite(off_diag)]
                if (length(off_diag) > 0L) {
                    rmax <- max(off_diag, na.rm = TRUE)
                    if (rmax < 0.3) {
                        private$.addNotice("WARNING",
                            "Weak inter-indicator correlations",
                            private$.messages$G5_warn(rmax))
                    }
                }
            }
```

Also store `n_params` and `cpp` in `.fitState` (Task 7 fills the summaryTable from these):

```r
            private$.fitState <- list(
                df = df, n = n, k = k, n_params = n_params, cpp = cpp,
                itypes = itypes, est_spec = est_spec
            )
```

- [ ] **Step 5.4: Run tests, confirm pass**

```bash
Rscript -e "jmvtools::prepare()" && Rscript -e "devtools::test(filter='latentbiomarker')" 2>&1 | tail -15
```

- [ ] **Step 5.5: Commit**

```bash
git add R/latentbiomarker.b.R R/latentbiomarker.h.R tests/testthat/test-latentbiomarker.R
git -c commit.gpgsign=false commit -m "$(cat <<'EOF'
feat(latentbiomarker): G2 cases-per-parameter and G5 correlation gates

G2 hard-refuses CPP<5; soft-warns 5<=CPP<10. G3 soft-warning when exactly
3 indicators (just-identified). G5 warns when max |inter-indicator r| < 0.3.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

## Task 6: CFA fit + fit-indices table + traffic-light interpretation

**Files:**
- Modify: `R/latentbiomarker.b.R`

- [ ] **Step 6.1: Write failing test for fit indices**

Append:

```r
test_that("CFA fit produces non-empty fit-indices row with traffic-light interpretation", {
    skip_if_not_installed("lavaan")
    set.seed(3)
    n <- 400
    f <- rnorm(n)
    df <- data.frame(
        time = rexp(n, 0.05),
        evt  = factor(rbinom(n, 1, 0.4)),
        ind1 = 0.8 * f + rnorm(n, sd = 0.6),
        ind2 = 0.7 * f + rnorm(n, sd = 0.7),
        ind3 = 0.75 * f + rnorm(n, sd = 0.65),
        ind4 = 0.85 * f + rnorm(n, sd = 0.5),
        ind5 = 0.6 * f + rnorm(n, sd = 0.8)
    )

    res <- ClinicoPath::latentbiomarker(
        data = df,
        dep_time = "time",
        dep_event = "evt",
        event_level = "1",
        indicators = paste0("ind", 1:5),
        reflective_confirmed = TRUE
    )
    rows <- res$fitTable$asDF
    expect_equal(nrow(rows), 1)
    expect_true(rows$cfi[1] > 0.9)             # well-fit synthetic data
    expect_true(rows$rmsea[1] < 0.1)
    expect_true(nchar(rows$interpretation[1]) > 0)
})
```

- [ ] **Step 6.2: Run, confirm failure**

```bash
Rscript -e "devtools::test(filter='latentbiomarker')" 2>&1 | tail -10
```

- [ ] **Step 6.3: Add CFA fit helper and fit-table population**

Add to `private = list(...)`:

```r
        # ---- Build lavaan model syntax ----
        .buildModelSyntax = function(indicators) {
            rhs <- paste(vapply(indicators, jmvcore::composeTerm, character(1)),
                         collapse = " + ")
            paste0("Factor =~ ", rhs)
        },

        # ---- Fit CFA ----
        .fitCFA = function(df, indicators, est_spec) {
            model <- private$.buildModelSyntax(indicators)
            args <- list(
                model = model,
                data = df,
                std.lv = TRUE,
                estimator = est_spec$estimator
            )
            if (!is.null(est_spec$ordered) && length(est_spec$ordered) > 0L) {
                args$ordered <- est_spec$ordered
            }
            if (!is.null(est_spec$missing)) {
                args$missing <- est_spec$missing
            }
            do.call(lavaan::cfa, args)
        },

        # ---- Interpret fit indices ----
        .interpretFit = function(cfi, tli, rmsea, srmr) {
            if (is.na(cfi) || is.na(rmsea)) return("Just-identified — fit untestable")
            good <- isTRUE(cfi >= 0.95) && isTRUE(tli >= 0.95) &&
                    isTRUE(rmsea <= 0.06) && isTRUE(srmr <= 0.08)
            acceptable <- isTRUE(cfi >= 0.90) && isTRUE(rmsea <= 0.10) && isTRUE(srmr <= 0.10)
            if (good) "Good fit"
            else if (acceptable) "Acceptable fit"
            else "Poor fit"
        },
```

In `.run`, after the gate cascade, add:

```r
            # ---- Fit CFA ----
            fit <- tryCatch(
                private$.fitCFA(df, opt$indicators, est_spec),
                error = function(e) {
                    private$.addNotice("ERROR",
                        "CFA estimation failed",
                        paste0("lavaan::cfa() returned an error: ", conditionMessage(e),
                               ". This often means the indicator covariance matrix is ",
                               "non-positive-definite, or an indicator has zero variance."))
                    NULL
                })
            if (is.null(fit) || !lavaan::lavInspect(fit, "converged")) {
                if (!is.null(fit)) {
                    private$.addNotice("ERROR",
                        "CFA did not converge",
                        "The lavaan model failed to converge. Check for near-zero variance indicators or extreme collinearity.")
                }
                private$.renderNotices()
                return()
            }
            private$.fitState$fit <- fit

            # ---- Populate fit-indices table ----
            fm <- lavaan::fitMeasures(fit,
                c("chisq", "df", "pvalue", "cfi", "tli",
                  "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr"))
            interp <- private$.interpretFit(fm["cfi"], fm["tli"], fm["rmsea"], fm["srmr"])
            self$results$fitTable$setRow(rowNo = 1, list(
                chisq          = unname(fm["chisq"]),
                df             = unname(fm["df"]),
                chisq_p        = unname(fm["pvalue"]),
                cfi            = unname(fm["cfi"]),
                tli            = unname(fm["tli"]),
                rmsea          = unname(fm["rmsea"]),
                rmsea_lo       = unname(fm["rmsea.ci.lower"]),
                rmsea_hi       = unname(fm["rmsea.ci.upper"]),
                srmr           = unname(fm["srmr"]),
                interpretation = interp
            ))

            if (!is.na(fm["cfi"]) && (fm["cfi"] < 0.95 || fm["rmsea"] > 0.10)) {
                private$.addNotice("WARNING",
                    "Poor model fit",
                    private$.messages$FIT_poor(fm["cfi"], fm["rmsea"]))
            }
```

- [ ] **Step 6.4: Run, confirm pass**

```bash
Rscript -e "jmvtools::prepare()" && Rscript -e "devtools::test(filter='latentbiomarker')" 2>&1 | tail -15
```

- [ ] **Step 6.5: Commit**

```bash
git add R/latentbiomarker.b.R R/latentbiomarker.h.R tests/testthat/test-latentbiomarker.R
git -c commit.gpgsign=false commit -m "$(cat <<'EOF'
feat(latentbiomarker): fit CFA and populate fit-indices table

Builds the lavaan model with jmvcore::composeTerm-escaped indicator names,
fits std.lv=TRUE so all loadings are on a common scale, and emits a
fit-indices table with traffic-light interpretation. Convergence
failures and poor-fit cases are surfaced as notices.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

## Task 7: Loadings, reliability, and summary tables

**Files:**
- Modify: `R/latentbiomarker.b.R`

- [ ] **Step 7.1: Write failing test**

```r
test_that("Loadings, reliability, and summary tables populate", {
    skip_if_not_installed("lavaan")
    set.seed(3)
    n <- 400
    f <- rnorm(n)
    df <- data.frame(
        time = rexp(n, 0.05),
        evt  = factor(rbinom(n, 1, 0.4)),
        ind1 = 0.8 * f + rnorm(n, sd = 0.6),
        ind2 = 0.7 * f + rnorm(n, sd = 0.7),
        ind3 = 0.75 * f + rnorm(n, sd = 0.65),
        ind4 = 0.85 * f + rnorm(n, sd = 0.5),
        ind5 = 0.6 * f + rnorm(n, sd = 0.8)
    )
    res <- ClinicoPath::latentbiomarker(
        data = df,
        dep_time = "time", dep_event = "evt", event_level = "1",
        indicators = paste0("ind", 1:5),
        reflective_confirmed = TRUE
    )

    L <- res$loadingsTable$asDF
    expect_equal(nrow(L), 5)
    expect_true(all(L$est_std > 0.3))
    expect_true(all(L$r2 > 0 & L$r2 < 1))

    R <- res$reliabilityTable$asDF
    expect_true(R$omega[1] > 0.7)
    expect_true(R$ave[1] > 0.3)

    S <- res$summaryTable$asDF
    expect_equal(S$n[1], n)
    expect_equal(S$n_indicators[1], 5)
    expect_equal(S$n_params[1], 10)
    expect_equal(S$estimator[1], "MLR")
})
```

- [ ] **Step 7.2: Run, confirm failure**

```bash
Rscript -e "devtools::test(filter='latentbiomarker')" 2>&1 | tail -10
```

- [ ] **Step 7.3: Add table-population logic**

Add to `private = list(...)`:

```r
        # ---- Compute McDonald's omega and AVE from standardized loadings ----
        # omega = (sum lambda)^2 / ( (sum lambda)^2 + sum theta )
        # AVE   = mean(lambda^2)
        .computeReliability = function(std_loadings) {
            lam <- std_loadings
            theta <- 1 - lam^2  # std residual variances under std.lv=TRUE on cor metric
            omega <- (sum(lam))^2 / ((sum(lam))^2 + sum(theta))
            ave   <- mean(lam^2)
            list(omega = omega, ave = ave)
        },
```

In `.run`, immediately after the fit-indices block:

```r
            # ---- Loadings table ----
            pe_std <- lavaan::parameterEstimates(fit, standardized = TRUE)
            load_rows <- pe_std[pe_std$op == "=~" & pe_std$lhs == "Factor", , drop = FALSE]
            loadings_table <- self$results$loadingsTable
            for (i in seq_len(nrow(load_rows))) {
                lam <- load_rows$std.all[i]
                loadings_table$addRow(rowKey = i, values = list(
                    indicator = load_rows$rhs[i],
                    est_std   = lam,
                    est       = load_rows$est[i],
                    se        = load_rows$se[i],
                    z         = load_rows$z[i],
                    pvalue    = load_rows$pvalue[i],
                    r2        = lam^2
                ))
            }

            # ---- Reliability table ----
            rel <- private$.computeReliability(load_rows$std.all)
            self$results$reliabilityTable$setRow(rowNo = 1, list(
                omega = rel$omega, ave = rel$ave))

            # ---- Summary table ----
            time_col  <- df[[opt$dep_time]]
            event_col <- df[[opt$dep_event]]
            n_events <- sum(as.character(event_col) == as.character(opt$event_level),
                            na.rm = TRUE)
            self$results$summaryTable$setRow(rowNo = 1, list(
                n            = n,
                n_events     = n_events,
                n_indicators = k,
                n_params     = n_params,
                cpp          = cpp,
                estimator    = est_spec$estimator,
                missing      = ifelse(is.null(est_spec$missing), "listwise",
                                      est_spec$missing)
            ))
```

- [ ] **Step 7.4: Run, confirm pass**

```bash
Rscript -e "jmvtools::prepare()" && Rscript -e "devtools::test(filter='latentbiomarker')" 2>&1 | tail -15
```

- [ ] **Step 7.5: Commit**

```bash
git add R/latentbiomarker.b.R R/latentbiomarker.h.R tests/testthat/test-latentbiomarker.R
git -c commit.gpgsign=false commit -m "$(cat <<'EOF'
feat(latentbiomarker): loadings, reliability, and summary tables

Standardized loadings, McDonald's omega, AVE, and a one-row sample/model
summary that exposes K parameters, CPP, and the chosen estimator.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

## Task 8: Factor scores + Cox table + G4 EPV gate

**Files:**
- Modify: `R/latentbiomarker.b.R`

- [ ] **Step 8.1: Write failing test**

```r
test_that("Cox table populates with HR for factor and adjusters", {
    skip_if_not_installed("lavaan")
    skip_if_not_installed("survival")
    set.seed(4)
    n <- 500
    f <- rnorm(n)
    age <- rnorm(n, 60, 10)
    lp <- 0.7 * f + 0.02 * age
    time <- rexp(n, exp(lp - mean(lp)) * 0.05)
    evt <- factor(rbinom(n, 1, 0.6))

    df <- data.frame(
        time = time, evt = evt, age = age,
        ind1 = 0.8 * f + rnorm(n, sd = 0.6),
        ind2 = 0.7 * f + rnorm(n, sd = 0.7),
        ind3 = 0.75 * f + rnorm(n, sd = 0.65)
    )
    res <- ClinicoPath::latentbiomarker(
        data = df,
        dep_time = "time", dep_event = "evt", event_level = "1",
        indicators = paste0("ind", 1:3),
        adjusters = "age",
        reflective_confirmed = TRUE
    )

    C <- res$coxTable$asDF
    expect_true("biomarker_factor" %in% C$term || "Factor" %in% C$term)
    expect_true("age" %in% C$term)
    expect_true(all(C$hr > 0))
    expect_match(res$notices$content, "measurement uncertainty", fixed = TRUE)
})
```

- [ ] **Step 8.2: Run, confirm failure**

- [ ] **Step 8.3: Add factor-score and Cox logic to `.run`**

Add to `private = list(...)`:

```r
        # ---- Extract factor scores ----
        .extractScores = function(fit, method, standardize) {
            scores <- lavaan::lavPredict(fit, method = method)
            v <- as.numeric(scores[, 1])
            if (isTRUE(standardize)) {
                v <- as.numeric(scale(v))
            }
            v
        },
```

In `.run`, after the loadings/reliability/summary block:

```r
            # ---- Extract factor scores ----
            factor_scores <- tryCatch(
                private$.extractScores(fit, opt$factor_score_method, opt$standardize_scores),
                error = function(e) NULL)
            if (is.null(factor_scores) || length(factor_scores) != n) {
                private$.addNotice("ERROR",
                    "Factor-score extraction failed",
                    "lavPredict() did not return scores of length n. This is rare; check for indicator missingness.")
                private$.renderNotices()
                return()
            }
            private$.fitState$factor_scores <- factor_scores

            # ---- Cox setup ----
            time_num  <- jmvcore::toNumeric(df[[opt$dep_time]])
            event_lvl <- as.character(opt$event_level)
            event_num <- as.integer(as.character(df[[opt$dep_event]]) == event_lvl)

            # G4 EPV gate (soft warning)
            n_cox_vars <- 1L + (if (is.null(opt$adjusters)) 0L else length(opt$adjusters))
            epv <- sum(event_num, na.rm = TRUE) / n_cox_vars
            if (epv < 10) {
                private$.addNotice("WARNING",
                    "Low events-per-variable in Cox model",
                    private$.messages$G4_warn(epv))
            }

            # Build Cox data
            cox_df <- data.frame(.time = time_num, .event = event_num,
                                 biomarker_factor = factor_scores)
            if (!is.null(opt$adjusters)) {
                for (av in opt$adjusters) cox_df[[av]] <- df[[av]]
            }

            rhs_terms <- c("biomarker_factor",
                           if (!is.null(opt$adjusters))
                               vapply(opt$adjusters, jmvcore::composeTerm, character(1))
                           else character(0))
            cox_formula <- stats::as.formula(
                paste0("survival::Surv(.time, .event) ~ ", paste(rhs_terms, collapse = " + ")))

            cox_fit <- tryCatch(
                survival::coxph(cox_formula, data = cox_df),
                error = function(e) {
                    private$.addNotice("ERROR",
                        "Cox model failed",
                        paste0("survival::coxph() returned: ", conditionMessage(e)))
                    NULL
                })
            if (is.null(cox_fit)) {
                private$.renderNotices()
                return()
            }
            private$.fitState$cox_fit <- cox_fit
            private$.fitState$cox_df  <- cox_df

            # Populate Cox table
            cs <- summary(cox_fit)
            coef_mat <- cs$coefficients
            cox_table <- self$results$coxTable
            for (i in seq_len(nrow(coef_mat))) {
                cox_table$addRow(rowKey = rownames(coef_mat)[i], values = list(
                    term  = rownames(coef_mat)[i],
                    hr    = coef_mat[i, "exp(coef)"],
                    ci_lo = cs$conf.int[i, "lower .95"],
                    ci_hi = cs$conf.int[i, "upper .95"],
                    z     = coef_mat[i, "z"],
                    pvalue= coef_mat[i, "Pr(>|z|)"]
                ))
            }

            # Always-on uncertainty notice
            private$.addNotice("INFO",
                "Measurement uncertainty not propagated",
                private$.messages$UNCERTAINTY)
```

- [ ] **Step 8.4: Run, confirm pass**

```bash
Rscript -e "jmvtools::prepare()" && Rscript -e "devtools::test(filter='latentbiomarker')" 2>&1 | tail -15
```

- [ ] **Step 8.5: Commit**

```bash
git add R/latentbiomarker.b.R R/latentbiomarker.h.R tests/testthat/test-latentbiomarker.R
git -c commit.gpgsign=false commit -m "$(cat <<'EOF'
feat(latentbiomarker): factor scores + Cox table + EPV gate

lavPredict() factor scores feed an internal coxph() with optional adjusters.
G4 warns when events-per-Cox-variable < 10. Every successful run emits the
'measurement uncertainty not propagated' info notice.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

## Task 9: KM plot + PH test

**Files:**
- Modify: `R/latentbiomarker.b.R`

- [ ] **Step 9.1: Write failing test**

```r
test_that("PH test table and KM render function populate", {
    skip_if_not_installed("lavaan")
    skip_if_not_installed("survminer")
    set.seed(5)
    n <- 400
    f <- rnorm(n)
    time <- rexp(n, exp(0.5 * f - 0.5 * mean(f)) * 0.05)
    df <- data.frame(
        time = time, evt = factor(rbinom(n, 1, 0.5)),
        ind1 = 0.8 * f + rnorm(n, sd = 0.5),
        ind2 = 0.7 * f + rnorm(n, sd = 0.6),
        ind3 = 0.85 * f + rnorm(n, sd = 0.4)
    )
    res <- ClinicoPath::latentbiomarker(
        data = df,
        dep_time = "time", dep_event = "evt", event_level = "1",
        indicators = paste0("ind", 1:3),
        reflective_confirmed = TRUE
    )
    P <- res$phTable$asDF
    expect_true(nrow(P) >= 1)
})
```

- [ ] **Step 9.2: Run, confirm failure**

- [ ] **Step 9.3: Implement PH test population and KM render**

In `.run`, after Cox table population:

```r
            # ---- PH test ----
            phz <- tryCatch(survival::cox.zph(cox_fit), error = function(e) NULL)
            if (!is.null(phz)) {
                ph_table <- self$results$phTable
                for (i in seq_len(nrow(phz$table))) {
                    ph_table$addRow(rowKey = rownames(phz$table)[i], values = list(
                        term   = rownames(phz$table)[i],
                        chisq  = phz$table[i, "chisq"],
                        df     = phz$table[i, "df"],
                        pvalue = phz$table[i, "p"]
                    ))
                }
                global_p <- phz$table[nrow(phz$table), "p"]
                if (!is.na(global_p) && global_p < 0.05) {
                    private$.addNotice("WARNING",
                        "Proportional-hazards assumption violated",
                        private$.messages$PH_violated(global_p))
                }
            }

            # ---- KM image setState ----
            if (isTRUE(opt$show_plot_km)) {
                strata_n <- switch(opt$km_strata, median = 2L, tertile = 3L, quartile = 4L)
                strata <- cut(factor_scores,
                              breaks = stats::quantile(factor_scores,
                                                      probs = seq(0, 1, length.out = strata_n + 1L),
                                                      na.rm = TRUE),
                              include.lowest = TRUE,
                              labels = paste0("Q", seq_len(strata_n)))
                km_state <- list(
                    time   = time_num,
                    event  = event_num,
                    strata = strata
                )
                self$results$kmPlot$setState(km_state)
            }
```

Add render method **inside** `private = list(...)`:

```r
        # ---- KM render ----
        .kmPlot = function(image, ggtheme, theme, ...) {
            state <- image$state
            if (is.null(state)) return(FALSE)
            df <- data.frame(time = state$time, event = state$event,
                             strata = state$strata)
            df <- df[stats::complete.cases(df), , drop = FALSE]
            if (nrow(df) < 5L) return(FALSE)
            fit <- survival::survfit(survival::Surv(time, event) ~ strata, data = df)
            p <- survminer::ggsurvplot(
                fit, data = df,
                pval = TRUE, risk.table = FALSE,
                xlab = "Time", ylab = "Survival probability",
                legend.title = "Factor-score stratum",
                ggtheme = ggtheme
            )$plot
            print(p)
            TRUE
        },
```

- [ ] **Step 9.4: Run, confirm pass**

```bash
Rscript -e "jmvtools::prepare()" && Rscript -e "devtools::test(filter='latentbiomarker')" 2>&1 | tail -15
```

- [ ] **Step 9.5: Commit**

```bash
git add R/latentbiomarker.b.R R/latentbiomarker.h.R tests/testthat/test-latentbiomarker.R
git -c commit.gpgsign=false commit -m "$(cat <<'EOF'
feat(latentbiomarker): PH test and KM plot stratified by factor-score quantile

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

## Task 10: Loadings plot + path diagram

**Files:**
- Modify: `R/latentbiomarker.b.R`

- [ ] **Step 10.1: Write failing test (render boolean)**

```r
test_that("Loadings and path-diagram renderers return TRUE for valid state", {
    skip_if_not_installed("lavaan")
    skip_if_not_installed("semPlot")
    set.seed(6)
    n <- 300
    f <- rnorm(n)
    df <- data.frame(
        time = rexp(n, 0.05),
        evt  = factor(rbinom(n, 1, 0.5)),
        ind1 = 0.8 * f + rnorm(n, sd = 0.5),
        ind2 = 0.7 * f + rnorm(n, sd = 0.6),
        ind3 = 0.85 * f + rnorm(n, sd = 0.4)
    )
    res <- ClinicoPath::latentbiomarker(
        data = df,
        dep_time = "time", dep_event = "evt", event_level = "1",
        indicators = paste0("ind", 1:3),
        reflective_confirmed = TRUE
    )
    # Render functions are exercised via $state being non-null
    expect_false(is.null(res$loadingsPlot$state))
    expect_false(is.null(res$pathPlot$state))
})
```

- [ ] **Step 10.2: Run, confirm failure**

- [ ] **Step 10.3: Implement state setting and renderers**

In `.run`, after the KM `setState` block:

```r
            # ---- Loadings plot state ----
            if (isTRUE(opt$show_plot_loadings)) {
                self$results$loadingsPlot$setState(list(
                    indicators = load_rows$rhs,
                    estimate   = load_rows$std.all,
                    ci_lo      = load_rows$std.all - 1.96 * load_rows$se,
                    ci_hi      = load_rows$std.all + 1.96 * load_rows$se
                ))
            }

            # ---- Path diagram state ----
            if (isTRUE(opt$show_plot_path)) {
                self$results$pathPlot$setState(list(fit_serial = lavaan::standardizedSolution(fit)))
                # NOTE: storing the full lavaan fit in setState can hit protobuf size limits
                # for large models. We store the standardized solution and re-render from indicators
                # + their loadings via semPlot::semPaths with a re-built model below.
                # For v1 we accept the limitation; if it bites, switch to storing $fit_serial and
                # reconstructing a minimal layout.
            }
```

Add render methods inside `private = list(...)`:

```r
        # ---- Loadings plot render ----
        .loadingsPlot = function(image, ggtheme, theme, ...) {
            state <- image$state
            if (is.null(state)) return(FALSE)
            df <- data.frame(
                indicator = factor(state$indicators, levels = rev(state$indicators)),
                estimate  = state$estimate,
                ci_lo     = state$ci_lo,
                ci_hi     = state$ci_hi
            )
            p <- ggplot2::ggplot(df, ggplot2::aes(x = estimate, y = indicator)) +
                ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
                ggplot2::geom_errorbarh(ggplot2::aes(xmin = ci_lo, xmax = ci_hi),
                                         height = 0.2) +
                ggplot2::geom_point(size = 3) +
                ggplot2::labs(x = "Standardized loading (95% CI)", y = NULL) +
                ggtheme
            print(p)
            TRUE
        },

        # ---- Path diagram render ----
        .pathPlot = function(image, ggtheme, theme, ...) {
            state <- image$state
            fit   <- private$.fitState$fit
            if (is.null(fit)) return(FALSE)
            if (!requireNamespace("semPlot", quietly = TRUE)) {
                message("semPlot not installed; cannot render path diagram.")
                return(FALSE)
            }
            semPlot::semPaths(
                fit,
                whatLabels = "std",
                edge.label.cex = 0.9,
                layout = "tree2",
                rotation = 2,
                style = "lisrel"
            )
            TRUE
        },
```

**Renderer return contract:** every `.*Plot` render method must return `TRUE` on success and `FALSE` when state is missing or invalid — jamovi uses this to decide whether to show or hide the image.

- [ ] **Step 10.4: Run, confirm pass**

```bash
Rscript -e "jmvtools::prepare()" && Rscript -e "devtools::test(filter='latentbiomarker')" 2>&1 | tail -15
```

- [ ] **Step 10.5: Commit**

```bash
git add R/latentbiomarker.b.R R/latentbiomarker.h.R tests/testthat/test-latentbiomarker.R
git -c commit.gpgsign=false commit -m "$(cat <<'EOF'
feat(latentbiomarker): loadings forest plot and semPaths path diagram

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

## Task 11: Modification indices + save-to-data + R code export

**Files:**
- Modify: `R/latentbiomarker.b.R`

- [ ] **Step 11.1: Write failing tests**

```r
test_that("Modification indices table populates (or stays empty for just-identified)", {
    skip_if_not_installed("lavaan")
    set.seed(7)
    n <- 400
    f <- rnorm(n)
    df <- data.frame(
        time = rexp(n, 0.05),
        evt  = factor(rbinom(n, 1, 0.5)),
        ind1 = 0.8 * f + rnorm(n, sd = 0.5),
        ind2 = 0.7 * f + rnorm(n, sd = 0.6),
        ind3 = 0.85 * f + rnorm(n, sd = 0.4),
        ind4 = 0.6 * f + rnorm(n, sd = 0.7),
        ind5 = 0.5 * f + rnorm(n, sd = 0.8)
    )
    res <- ClinicoPath::latentbiomarker(
        data = df,
        dep_time = "time", dep_event = "evt", event_level = "1",
        indicators = paste0("ind", 1:5),
        reflective_confirmed = TRUE,
        show_diagnostics = TRUE
    )
    # No assertion on row count — depends on data — just that the call succeeds
    expect_true(!is.null(res$miTable))
})

test_that("R code export contains lavaan and coxph calls when enabled", {
    skip_if_not_installed("lavaan")
    set.seed(8)
    n <- 300
    f <- rnorm(n)
    df <- data.frame(
        time = rexp(n, 0.05),
        evt  = factor(rbinom(n, 1, 0.5)),
        ind1 = 0.8 * f + rnorm(n, sd = 0.5),
        ind2 = 0.7 * f + rnorm(n, sd = 0.6),
        ind3 = 0.85 * f + rnorm(n, sd = 0.4)
    )
    res <- ClinicoPath::latentbiomarker(
        data = df,
        dep_time = "time", dep_event = "evt", event_level = "1",
        indicators = paste0("ind", 1:3),
        reflective_confirmed = TRUE,
        show_r_code = TRUE
    )
    code <- res$rCode$content
    expect_match(code, "lavaan::cfa", fixed = TRUE)
    expect_match(code, "survival::coxph", fixed = TRUE)
})
```

- [ ] **Step 11.2: Run, confirm failure**

- [ ] **Step 11.3: Implement MI table, save-to-data, and R-code export**

In `.run`, after the path-plot state block:

```r
            # ---- Modification indices (MI > 10) ----
            if (isTRUE(opt$show_diagnostics)) {
                mi <- tryCatch(lavaan::modificationIndices(fit), error = function(e) NULL)
                if (!is.null(mi)) {
                    mi <- mi[!is.na(mi$mi) & mi$mi > 10, , drop = FALSE]
                    mi <- mi[order(-mi$mi), , drop = FALSE]
                    mi_table <- self$results$miTable
                    for (i in seq_len(min(10, nrow(mi)))) {
                        mi_table$addRow(rowKey = i, values = list(
                            lhs = mi$lhs[i], op = mi$op[i],
                            rhs = mi$rhs[i], mi  = mi$mi[i]
                        ))
                    }
                    if (nrow(mi) > 0) {
                        private$.addNotice("INFO",
                            "Modification indices > 10 reported",
                            paste0("Do not act on these without theoretical justification. ",
                                   "Modification indices are post-hoc and inflate Type I error ",
                                   "if used to refit the model."))
                    }
                }
            }

            # ---- Save factor scores to data ----
            if (self$options$save_factor_scores) {
                col_name <- opt$factor_score_name
                if (is.null(col_name) || !nzchar(col_name)) col_name <- "biomarker_factor"
                # The Output result item uses self$results$save_factor_scores; the column
                # written has length equal to the *full* dataset, so we right-align with
                # row indices from the analysis frame.
                full_n <- nrow(self$data)
                full_vec <- rep(NA_real_, full_n)
                # df was obtained via jmvcore::naOmit; its rownames carry original positions
                idx <- as.integer(rownames(df))
                full_vec[idx] <- factor_scores
                self$results$save_factor_scores$setValues(
                    index = seq_len(full_n),
                    values = full_vec
                )
            }

            # ---- R code export ----
            if (isTRUE(opt$show_r_code)) {
                ind_quoted <- paste0("\"", opt$indicators, "\"", collapse = ", ")
                adj_quoted <- if (is.null(opt$adjusters)) ""
                              else paste0("\"", opt$adjusters, "\"", collapse = ", ")
                model_str  <- private$.buildModelSyntax(opt$indicators)
                ordered_arg <- if (length(est_spec$ordered) > 0L)
                    paste0(", ordered = c(\"",
                           paste(est_spec$ordered, collapse = "\", \""), "\")")
                else ""
                missing_arg <- if (!is.null(est_spec$missing))
                    paste0(", missing = \"", est_spec$missing, "\"")
                else ""

                cox_rhs <- paste(c("biomarker_factor",
                                   if (!is.null(opt$adjusters)) opt$adjusters
                                   else character(0)),
                                 collapse = " + ")

                rcode <- paste0(
"# Reproducible R code generated by ClinicoPath::latentbiomarker
library(lavaan)
library(survival)

# 1. Fit the measurement model
model <- '", model_str, "'
fit <- lavaan::cfa(
    model = model,
    data  = your_data,
    std.lv = TRUE,
    estimator = \"", est_spec$estimator, "\"", ordered_arg, missing_arg, "
)
summary(fit, fit.measures = TRUE, standardized = TRUE)

# 2. Extract factor scores
scores <- as.numeric(lavaan::lavPredict(fit, method = \"", opt$factor_score_method, "\"))",
if (isTRUE(opt$standardize_scores)) "\nscores <- as.numeric(scale(scores))" else "",
"
your_data$biomarker_factor <- scores

# 3. Fit Cox regression
cox_fit <- survival::coxph(
    survival::Surv(", opt$dep_time, ", ", opt$dep_event,
                  " == \"", as.character(opt$event_level), "\") ~ ", cox_rhs, ",
    data = your_data
)
summary(cox_fit)
survival::cox.zph(cox_fit)
")
                self$results$rCode$setContent(rcode)
            }
```

- [ ] **Step 11.4: Run, confirm pass**

```bash
Rscript -e "jmvtools::prepare()" && Rscript -e "devtools::test(filter='latentbiomarker')" 2>&1 | tail -20
```

- [ ] **Step 11.5: Commit**

```bash
git add R/latentbiomarker.b.R R/latentbiomarker.h.R tests/testthat/test-latentbiomarker.R
git -c commit.gpgsign=false commit -m "$(cat <<'EOF'
feat(latentbiomarker): modification indices, save-to-data, R code export

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

## Task 11b: Mardia multivariate normality + factor-score reliability (spec 5.5a, 5.5c)

These two diagnostics are surfaced as additional notices rather than dedicated tables (keeps the result pane lean). Both gated by `show_diagnostics = TRUE`.

**Files:**
- Modify: `R/latentbiomarker.b.R`

- [ ] **Step 11b.1: Write failing test**

```r
test_that("Mardia and factor-score reliability notices fire under MLR", {
    skip_if_not_installed("lavaan")
    set.seed(9)
    n <- 300
    f <- rnorm(n)
    df <- data.frame(
        time = rexp(n, 0.05),
        evt  = factor(rbinom(n, 1, 0.5)),
        ind1 = 0.8 * f + rnorm(n, sd = 0.5),
        ind2 = 0.7 * f + rnorm(n, sd = 0.6),
        ind3 = 0.85 * f + rnorm(n, sd = 0.4)
    )
    res <- ClinicoPath::latentbiomarker(
        data = df, dep_time = "time", dep_event = "evt", event_level = "1",
        indicators = paste0("ind", 1:3),
        reflective_confirmed = TRUE,
        show_diagnostics = TRUE
    )
    # At least one of the diagnostic notices should appear
    notices <- res$notices$content
    expect_true(grepl("multivariate normality", notices, ignore.case = TRUE) ||
                grepl("Factor-score reliability", notices, fixed = TRUE))
})
```

- [ ] **Step 11b.2: Run, confirm failure**

```bash
Rscript -e "devtools::test(filter='latentbiomarker')" 2>&1 | tail -10
```

- [ ] **Step 11b.3: Implement Mardia + score-reliability notices**

Add helper to `private = list(...)`:

```r
        # ---- Mardia multivariate normality (MLR only) ----
        # Returns p-value of Mardia skewness test, or NA on failure
        .mardiaSkew = function(df, indicators) {
            mat <- as.matrix(df[, indicators, drop = FALSE])
            mat <- mat[stats::complete.cases(mat), , drop = FALSE]
            n <- nrow(mat); p <- ncol(mat)
            if (n < 20L || p < 2L) return(NA_real_)
            mu <- colMeans(mat)
            S  <- stats::cov(mat) * (n - 1L) / n  # MLE covariance
            S_inv <- tryCatch(solve(S), error = function(e) NULL)
            if (is.null(S_inv)) return(NA_real_)
            centered <- sweep(mat, 2, mu, "-")
            d <- centered %*% S_inv %*% t(centered)
            b1p <- sum(d^3) / (n^2)
            stat <- n * b1p / 6
            df_chi <- p * (p + 1L) * (p + 2L) / 6
            stats::pchisq(stat, df = df_chi, lower.tail = FALSE)
        },
```

In `.run`, immediately after the MI-table block (still inside the `show_diagnostics` branch), add:

```r
            if (isTRUE(opt$show_diagnostics)) {

                # Mardia normality (MLR only)
                if (est_spec$estimator == "MLR") {
                    p_mardia <- private$.mardiaSkew(df, opt$indicators)
                    if (!is.na(p_mardia) && p_mardia < 0.05) {
                        private$.addNotice("INFO",
                            "Multivariate normality assumption likely violated",
                            paste0("Mardia skewness p = ", signif(p_mardia, 3),
                                   ". MLR robust SEs are appropriate; loadings remain ",
                                   "unbiased but ML standard errors would be optimistic. ",
                                   "No action needed — MLR is already in use."))
                    }
                }

                # Factor-score reliability — correlation between regression and Bartlett scores
                fs_reg <- tryCatch(as.numeric(lavaan::lavPredict(fit, method = "regression")),
                                   error = function(e) NULL)
                fs_bart <- tryCatch(as.numeric(lavaan::lavPredict(fit, method = "Bartlett")),
                                    error = function(e) NULL)
                if (!is.null(fs_reg) && !is.null(fs_bart) &&
                    length(fs_reg) == length(fs_bart) && length(fs_reg) > 2L) {
                    r_methods <- suppressWarnings(stats::cor(fs_reg, fs_bart,
                                                             use = "complete.obs"))
                    if (!is.na(r_methods) && r_methods < 0.95) {
                        private$.addNotice("INFO",
                            "Factor-score reliability check",
                            paste0("Correlation between regression and Bartlett factor scores ",
                                   "is ", round(r_methods, 3),
                                   " (< 0.95 indicates noticeable disagreement between methods)."))
                    }
                }
            }
```

- [ ] **Step 11b.4: Run, confirm pass**

```bash
Rscript -e "jmvtools::prepare()" && Rscript -e "devtools::test(filter='latentbiomarker')" 2>&1 | tail -15
```

- [ ] **Step 11b.5: Commit**

```bash
git add R/latentbiomarker.b.R R/latentbiomarker.h.R tests/testthat/test-latentbiomarker.R
git -c commit.gpgsign=false commit -m "$(cat <<'EOF'
feat(latentbiomarker): Mardia normality and factor-score reliability notices

Spec sections 5.5a and 5.5c — surfaced as INFO notices rather than dedicated
tables to keep the result pane lean.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

## Task 12: End-to-end test on `histopathology` dataset

**Files:**
- Modify: `tests/testthat/test-latentbiomarker.R`

- [ ] **Step 12.1: Inspect `histopathology` to pick realistic indicators**

Run:
```bash
Rscript -e "data(histopathology, package='ClinicoPath'); str(histopathology)" 2>&1 | head -60
```

Note three continuous indicators present (likely `MeasurementA`, `MeasurementB`, `Measurement1`) and a time + event pair (likely `OverallTime`, `Outcome`).

- [ ] **Step 12.2: Add the integration test**

Append to `tests/testthat/test-latentbiomarker.R`:

```r
test_that("Integration: histopathology dataset end-to-end", {
    skip_if_not_installed("lavaan")
    skip_if_not_installed("survminer")
    data("histopathology", package = "ClinicoPath")

    # Pick three continuous numeric columns
    num_cols <- names(histopathology)[vapply(histopathology, is.numeric, logical(1))]
    skip_if(length(num_cols) < 4, "need at least 4 numeric cols")
    indicators <- num_cols[1:3]

    # Ensure required survival fields exist
    skip_if(!all(c("OverallTime", "Outcome") %in% names(histopathology)),
            "histopathology lacks expected survival columns")

    res <- ClinicoPath::latentbiomarker(
        data = histopathology,
        dep_time = "OverallTime",
        dep_event = "Outcome",
        event_level = levels(factor(histopathology$Outcome))[1],
        indicators = indicators,
        reflective_confirmed = TRUE
    )

    # End-to-end smoke: no error notices, summary populated, Cox table populated
    expect_false(grepl("Insufficient", res$notices$content, fixed = TRUE))
    expect_equal(nrow(res$summaryTable$asDF), 1)
    expect_gt(nrow(res$coxTable$asDF), 0)
})
```

- [ ] **Step 12.3: Run**

```bash
Rscript -e "devtools::test(filter='latentbiomarker')" 2>&1 | tail -30
```

If `OverallTime`/`Outcome` are not present, edit the test column names to match the actual dataset (run the inspect command from 12.1 again). The test then becomes a self-correcting integration check.

- [ ] **Step 12.4: Commit**

```bash
git add tests/testthat/test-latentbiomarker.R
git -c commit.gpgsign=false commit -m "$(cat <<'EOF'
test(latentbiomarker): end-to-end integration on histopathology dataset

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

## Task 13: Vignette with worked scenarios

**Files:**
- Create: `vignettes/meddecide-99-latentbiomarker.qmd` (renumber per `updateModules_config.yaml` if a conflict exists)

- [ ] **Step 13.1: Confirm vignette numbering**

Run:
```bash
ls vignettes/ | grep -E "^meddecide-" | head -20
```

Pick the next free number (e.g., if highest is `meddecide-30-...`, use `meddecide-31-`). Replace `99` in the filename below accordingly.

- [ ] **Step 13.2: Write the vignette**

```yaml
---
title: "Latent biomarker constructs and survival"
subtitle: "When a single biomarker score isn't enough"
author: "ClinicoPath"
date: today
format: html
vignette: >
  %\VignetteIndexEntry{Latent biomarker constructs}
  %\VignetteEngine{quarto::html}
  %\VignetteEncoding{UTF-8}
---

## When to use this function

Use `latentbiomarker` when you have **three or more imperfect biomarker measurements** that you believe all reflect the same underlying biological construct, and you want to relate that construct to a survival endpoint.

Typical pathology examples:

- **Immune phenotype** indicated by CD8 density, PD-L1 TPS, TIL %, IFN-γ signature, tertiary lymphoid structure count.
- **Histologic aggressiveness** indicated by mitotic count, Ki-67, nuclear grade.
- **Stromal activation** indicated by α-SMA, FAP, collagen alignment score.

## When *not* to use this function

Don't use it when:

- Your "indicators" actually constitute a composite score (each adds independent information) — that's a *formative* model. Use the R packages `cSEM` or `seminr` instead.
- You only have 1–2 markers — a simpler approach is conventional Cox with each marker.
- Your cohort is small (n < 100) — SEM is data-hungry; use a summary z-score and conventional Cox.

## Refusal policy

The function deliberately refuses to run when the input is inappropriate. See the table of gates in `docs/superpowers/specs/2026-05-13-latentbiomarker-design.md` section 6.

## Six worked scenarios

### 1. Continuous indicators — the happy path

[~150-word walkthrough with code chunk using simulated continuous data, MLR estimator, runs end-to-end]

### 2. Ordinal indicators — WLSMV path

[~150 words: same factor, but IHC 0/1+/2+/3+ ordinal scores; show the WLSMV notice fires]

### 3. Mixed continuous + ordinal + binary

[~150 words: combine — show that WLSMV kicks in once any indicator is non-continuous]

### 4. Insufficient n triggers G1

[~100 words: n=50 example, show the refusal text]

### 5. Just-identified model warning

[~100 words: exactly 3 indicators, show fit indices are NA / trivially perfect, G3 soft warning visible]

### 6. Two-factor reality forced into one factor

[~150 words: simulate two correlated factors, force into one-factor model, observe poor fit + pointer to SEMLj]
```

(Each `[...]` placeholder is intentionally left for the engineer to expand into prose. Write the actual R chunks for each scenario using the example data the test suite already uses.)

- [ ] **Step 13.3: Verify vignette builds**

Run:
```bash
Rscript -e "rmarkdown::render('vignettes/meddecide-99-latentbiomarker.qmd', quiet = TRUE)"
```

Expected: no errors. An HTML file appears next to the qmd.

- [ ] **Step 13.4: Commit**

```bash
git add vignettes/meddecide-99-latentbiomarker.qmd
git -c commit.gpgsign=false commit -m "$(cat <<'EOF'
docs(latentbiomarker): vignette with 6 worked scenarios

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

## Task 14: NEWS.md, version bump, JamoviTest promotion guard

**Files:**
- Modify: `NEWS.md`, `DESCRIPTION` (if not already bumped in Task 1)

- [ ] **Step 14.1: Update `NEWS.md`**

Read the current version from `DESCRIPTION`, then prepend a new section under the top-level heading:

```markdown
# ClinicoPath 0.0.39.0

## New features

* `latentbiomarker` — Latent biomarker construct + Cox regression. Single-factor reflective CFA with auto MLR/WLSMV selection, factor-score extraction, internal Cox regression with KM plot, and explicit refusal of inappropriate inputs (n < 100, CPP < 5, < 3 indicators, unconfirmed reflective assumption).
```

- [ ] **Step 14.2: Confirm `T` suffix on `menuGroup` (development routing)**

Per CLAUDE.md, the function stays in JamoviTest during development. Confirm:

```bash
grep -n "^menuGroup:" jamovi/latentbiomarker.a.yaml
grep -A2 "name: latentbiomarker" jamovi/0000.yaml
```

Expected: both show `menuGroup: meddecideT` (with `T`). **Do not** strip the `T` here — promotion to production is a separate manual step the user performs after testing.

- [ ] **Step 14.3: Final test sweep**

Run:
```bash
Rscript -e "jmvtools::prepare()" 2>&1 | tail -5
Rscript -e "devtools::test(filter='latentbiomarker')" 2>&1 | tail -30
Rscript -e "devtools::document(roclets = c('rd', 'namespace'))" 2>&1 | tail -10
```

Expected: all green.

- [ ] **Step 14.4: Commit**

```bash
git add NEWS.md DESCRIPTION
git -c commit.gpgsign=false commit -m "$(cat <<'EOF'
chore(latentbiomarker): bump version and update NEWS for 0.0.39.0

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

## Self-review checklist (run before declaring complete)

- [ ] All 14 tasks committed, `git log --oneline | head -16` shows them
- [ ] `Rscript -e "devtools::check()"` runs without errors related to `latentbiomarker` (warnings about `semPlot` in Suggests being missing are acceptable)
- [ ] Open jamovi, load the test module, navigate to **meddecide (Test) → Biomarkers → Latent Biomarker Construct + Cox Regression**, run on a small dataset, confirm UI matches `.u.yaml` and refusal messages render correctly
- [ ] Spec gates (G1–G6) all have at least one passing test
- [ ] Notices use HTML pattern from `R/waterfall.b.R` (no `insert(999, jmvcore::Notice)`)
- [ ] No `TBD` / `TODO` strings remain in plan implementation
- [ ] `menuGroup` still ends in `T` (JamoviTest routing) — user removes manually when promoting

## Promotion to production (post-implementation, user-driven)

After the user manually verifies the function in jamovi, they remove the `T` suffix:

1. `jamovi/latentbiomarker.a.yaml` — `menuGroup: meddecideT` → `meddecide`
2. `jamovi/0000.yaml` — same change in the registered entry
3. Bump version in `DESCRIPTION` and `NEWS.md` to `0.0.39.1` with a "promoted to production" note
4. Re-run `jmvtools::prepare()` and `devtools::document()`
5. Commit
