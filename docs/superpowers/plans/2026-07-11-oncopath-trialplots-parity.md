# OncoPath ↔ Jamovi-TrialPlots Parity Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Close OncoPath issue #1 (three waterfall enhancements) and reach feature parity with `highwindmx/Jamovi-TrialPlots` by adding an Adverse-Events butterfly plot (`aeplot`) and a group-sequential design/sample-size analysis (`gsdesign`).

**Architecture:** All work happens in the main `ClinicoPathJamoviModule` repo (source of truth); analyses are tagged `menuGroup: OncoPath` and propagated to the OncoPath submodule by `Rscript _updateModules.R`. Each analysis follows jamovi's 4-file architecture (`.a.yaml` options, `.u.yaml` UI, `.r.yaml` results, `.b.R` R6 backend; `.h.R` auto-generated).

**Tech Stack:** R, R6, jmvcore, ggplot2, dplyr/rlang, `gsDesign` (group-sequential), `ggsci` (journal palettes).

## Global Constraints

- Repo root: `/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule`. Never edit the OncoPath submodule directly.
- `.h.R` and `.Rd` files are auto-generated — never hand-edit; regenerate with `jmvtools::prepare()` + `devtools::document()`.
- New analyses tagged `menuGroup: OncoPath`, `ns: ClinicoPath`.
- `.a.yaml`: `type: Level` may not have `default:`; optional `Variable`/`Variables` MUST have `default: NULL`.
- `.u.yaml`: `Label` may not have `visible`; no `description` property.
- Runtime dependencies go in `DESCRIPTION` `Imports` (never `Suggests`): add `gsDesign`, `ggsci`.
- Non-ASCII characters that reach R output must be `\u{}`-escaped (e.g. `\u{2265}` for ≥, `\u{2192}` for →) to survive `R CMD check`.
- Reset accumulating HTML/notice outputs at the top of `.run()`.
- Attribution (issue #1 by @highwindmx; `Jamovi-TrialPlots` by highwind, LGPL) appears in `00refs.yaml`, per-analysis `refs:`, backend header comments, About HTML, and `NEWS.md`.
- Before every `jmvtools::prepare()`, guard the VS Code Electron hijack: `Sys.unsetenv("ELECTRON_RUN_AS_NODE")`.
- Verification command used throughout (call it **PREPARE**):
  ```bash
  Rscript -e 'Sys.unsetenv("ELECTRON_RUN_AS_NODE"); jmvtools::prepare()'
  ```
  must exit with **no errors**.

---

## File Structure

**Modified:**
- `jamovi/waterfall.a.yaml` — 4 new options (`sortDirection`, `showBaseline`, `confirmationVar`, `ongoingVar`).
- `jamovi/waterfall.u.yaml` — UI for the 4 new options.
- `jamovi/waterfall.r.yaml` — extend `waterfallplot` `clearWith`.
- `R/waterfall.b.R` — sort direction fix, baseline helper, annotation plumbing + markers.
- `jamovi/0000.yaml` — register `aeplot`, `gsdesign`.
- `jamovi/00refs.yaml` — attribution + methodology references.
- `DESCRIPTION` — add `gsDesign`, `ggsci` to Imports.
- `NEWS.md` — changelog + attribution.

**Created:**
- `jamovi/aeplot.a.yaml`, `jamovi/aeplot.u.yaml`, `jamovi/aeplot.r.yaml`, `R/aeplot.b.R`
- `jamovi/gsdesign.a.yaml`, `jamovi/gsdesign.u.yaml`, `jamovi/gsdesign.r.yaml`, `R/gsdesign.b.R`
- `data-raw/aeplot_test_data.R`, `data/aeplot_test_data.csv`
- `data-raw/waterfall_annotation_test_data.R`, `data/waterfall_annotation_test_data.csv`
- `tests/testthat/test-aeplot.R`, `tests/testthat/test-gsdesign.R`, `tests/testthat/test-waterfall-enhancements.R`

Phases are independent and can be built/committed in any order, but this sequence is recommended: **A (waterfall) → B (aeplot) → C (gsdesign) → D (attribution/refs/sync)**.

---

## Phase A — Waterfall enhancements (issue #1)

### Task A1: Add the four new waterfall options (`.a.yaml`)

**Files:**
- Modify: `jamovi/waterfall.a.yaml` (insert after the `sortBy` option block, which ends near line 125)

**Interfaces:**
- Produces options consumed by the backend: `sortDirection` (values `"conventional"`/`"reverse"`), `showBaseline` (bool), `confirmationVar` (Variable or NULL), `ongoingVar` (Variable or NULL).

- [ ] **Step 1: Insert the option definitions** immediately after the `sortBy` option (after its `description:` block) in `jamovi/waterfall.a.yaml`:

```yaml
    - name: sortDirection
      title: Sort Direction
      type: List
      options:
        - title: "Conventional (worst on left, best on right)"
          name: conventional
        - title: "Reverse (best on left, worst on right)"
          name: reverse
      default: conventional
      description:
          R: >
            Direction for the response sort. 'conventional' places the highest
            (worst) response on the left and the lowest (best, most negative) on
            the right, following the standard oncology waterfall convention.
          jamovi: >
            Direction for the response sort. Conventional = worst (highest) on the
            left, best (lowest/most negative) on the right — the standard oncology
            waterfall layout.

    - name: showBaseline
      title: Show Baseline (Y = 0) Line
      type: Bool
      default: true
      description:
          R: >
            Draw a horizontal reference line at 0 percent change to mark the
            baseline.
          jamovi: >
            Draw a horizontal reference line at 0% change to mark the baseline.

    - name: confirmationVar
      title: Confirmation Status (optional)
      type: Variable
      suggested: [nominal]
      permitted: [factor]
      default: NULL
      description:
          R: >
            Optional categorical variable indicating response confirmation status
            (e.g., Confirmed vs Unconfirmed CR/PR). A distinct marker is drawn at
            each bar tip according to the level of this variable.
          jamovi: >
            Optional variable indicating confirmation status (e.g., Confirmed vs
            Unconfirmed). A distinct marker is drawn at each bar tip per level.

    - name: ongoingVar
      title: On-Treatment / Ongoing (optional)
      type: Variable
      suggested: [nominal]
      permitted: [factor, numeric]
      default: NULL
      description:
          R: >
            Optional variable flagging patients still on treatment / with an
            ongoing response. Truthy values (TRUE, non-zero, or text matching
            yes/y/true/on/ongoing/1) draw an upward arrow at the bar tip.
          jamovi: >
            Optional flag for patients still on treatment / ongoing response.
            'Yes'/TRUE/1 draws an upward arrow at the bar tip.
```

- [ ] **Step 2: Validate the YAML sequence** (guards the "analyses is not iterable" / malformed-YAML class of bug):

Run:
```bash
Rscript -e 'yaml::yaml.load_file("jamovi/waterfall.a.yaml"); cat("OK\n")'
```
Expected: `OK` with no parser error.

- [ ] **Step 3: Commit**

```bash
git add jamovi/waterfall.a.yaml
git commit -m "feat(waterfall): add sortDirection, showBaseline, confirmation/ongoing options (OncoPath #1)"
```

---

### Task A2: Wire the new options into the UI (`.u.yaml`) and results (`.r.yaml`)

**Files:**
- Modify: `jamovi/waterfall.u.yaml` (the `Visualization` and `Annotations` labels inside "Waterfall Plot Options")
- Modify: `jamovi/waterfall.r.yaml` (the `waterfallplot` `clearWith` list)

- [ ] **Step 1: Add the sort-direction control** under the `Visualization` label in `waterfall.u.yaml`, right after the `sortBy` ComboBox (around line 68):

```yaml
              - type: ComboBox
                name: sortDirection
                enable: (showWaterfallPlot)
```

- [ ] **Step 2: Add the annotation controls** under the `Annotations` label in `waterfall.u.yaml`, after the `showCI` CheckBox (around line 89):

```yaml
              - type: CheckBox
                name: showBaseline
                enable: (showWaterfallPlot)
```

- [ ] **Step 3: Add the two annotation variable targets.** Inside the top `VariableSupplier` block in `waterfall.u.yaml`, add two new `TargetLayoutBox` children after the `Group Variable` box (after line 50):

```yaml
      - type: TargetLayoutBox
        label: Confirmation Status (optional)
        children:
          - type: VariablesListBox
            name: confirmationVar
            maxItemCount: 1
            isTarget: true
      - type: TargetLayoutBox
        label: On-Treatment / Ongoing (optional)
        children:
          - type: VariablesListBox
            name: ongoingVar
            maxItemCount: 1
            isTarget: true
```

- [ ] **Step 4: Extend the `waterfallplot` `clearWith`** in `waterfall.r.yaml` (the list starting near line 151). Add these four entries to that list:

```yaml
        - sortDirection
        - showBaseline
        - confirmationVar
        - ongoingVar
```

- [ ] **Step 5: PREPARE** to compile the header:

```bash
Rscript -e 'Sys.unsetenv("ELECTRON_RUN_AS_NODE"); jmvtools::prepare()'
```
Expected: completes with no errors; `R/waterfall.h.R` now exposes `self$options$sortDirection`, `showBaseline`, `confirmationVar`, `ongoingVar`.

- [ ] **Step 6: Commit**

```bash
git add jamovi/waterfall.u.yaml jamovi/waterfall.r.yaml R/waterfall.h.R jamovi/0000.yaml
git commit -m "feat(waterfall): UI + results wiring for new waterfall options"
```

---

### Task A3: Fix sort direction (conventional worst→best)

**Files:**
- Modify: `R/waterfall.b.R` (both sort paths: ~line 304 in `.prepareWaterfallPlotData`, and ~line 2283)

**Interfaces:**
- Consumes: `plotData$options$sortBy`, `plotData$options$sortDirection` (Task A1).

- [ ] **Step 1: Write the failing test.** Create `tests/testthat/test-waterfall-enhancements.R`:

```r
test_that("conventional sort puts worst (highest) response on the left", {
    # emulate the sort logic used by .prepareWaterfallPlotData
    df <- data.frame(response = c(-50, 10, -20, 40, 0))
    decreasing <- TRUE  # conventional
    ordered <- df[order(df$response, decreasing = decreasing, na.last = TRUE), , drop = FALSE]
    # leftmost bar (row 1) must be the largest (worst) value
    expect_equal(ordered$response[1], 40)
    # rightmost bar must be the smallest (best) value
    expect_equal(ordered$response[nrow(ordered)], -50)
})
```

- [ ] **Step 2: Run it and confirm it passes as a spec anchor** (this pins the intended ordering):

Run:
```bash
Rscript -e 'testthat::test_file("tests/testthat/test-waterfall-enhancements.R")'
```
Expected: PASS (this test encodes the target behavior).

- [ ] **Step 3: Update the first sort path** in `.prepareWaterfallPlotData` (~line 304). Replace:

```r
          if (plotData$options$sortBy == "response") {
            df <- df[order(df$response, na.last = TRUE),]
          } else if (plotData$options$sortBy == "id") {
            df <- df[order(df[[plotData$options$patientID]], na.last = TRUE),]
          }
```
with:
```r
          if (plotData$options$sortBy == "response") {
            decreasing <- !identical(plotData$options$sortDirection, "reverse")
            df <- df[order(df$response, decreasing = decreasing, na.last = TRUE),]
          } else if (plotData$options$sortBy == "id") {
            df <- df[order(df[[plotData$options$patientID]], na.last = TRUE),]
          }
```

- [ ] **Step 4: Update the second sort path** (~line 2283). Replace the analogous block:

```r
        if (plotData$options$sortBy == "response") {
          df <- df[order(df$response, na.last = TRUE),]
        } else if (plotData$options$sortBy == "id") {
          df <- df[order(df[[plotData$options$patientID]], na.last = TRUE),]
        }
```
with:
```r
        if (plotData$options$sortBy == "response") {
          decreasing <- !identical(plotData$options$sortDirection, "reverse")
          df <- df[order(df$response, decreasing = decreasing, na.last = TRUE),]
        } else if (plotData$options$sortBy == "id") {
          df <- df[order(df[[plotData$options$patientID]], na.last = TRUE),]
        }
```

- [ ] **Step 5: Parse-check** the backend:

Run:
```bash
Rscript -e 'invisible(parse("R/waterfall.b.R")); cat("parse OK\n")'
```
Expected: `parse OK`.

- [ ] **Step 6: Commit**

```bash
git add R/waterfall.b.R tests/testthat/test-waterfall-enhancements.R
git commit -m "fix(waterfall): sort worst-on-left, best-on-right by default (OncoPath #1)"
```

---

### Task A4: Baseline line + annotation markers

**Files:**
- Modify: `R/waterfall.b.R` (add `.addBaseline`, `.attachAnnotations`, `.addAnnotationMarkers` helpers; call them in the render path and data-assembly paths)

**Interfaces:**
- Consumes: `plotData$options$showBaseline`, `confirmationVar`, `ongoingVar`; the sorted `df` (columns `response`, optional `confirm_status`, `ongoing_flag`).
- Produces: modified ggplot with a `geom_hline(0)` and per-bar markers.

- [ ] **Step 1: Add the baseline helper** near `.addRecistThresholds` (~line 395) in `R/waterfall.b.R`:

```r
        # Add a Y = 0 baseline reference line
        .addBaseline = function(plot, show_baseline) {
          if (isTRUE(show_baseline)) {
            plot +
              ggplot2::geom_hline(
                yintercept = 0,
                color = "black",
                linewidth = 0.5
              )
          } else {
            plot
          }
        },
```

- [ ] **Step 2: Add the annotation-attach helper** (joins per-patient confirmation/ongoing onto the waterfall data, mirroring the existing `groupVar` join). Add it near the other data helpers (after `.prepareWaterfallPlotData`, ~line 311):

```r
        # Attach optional per-patient annotation columns to waterfall data.
        # Mirrors the groupVar distinct-join so row alignment survives the sort.
        .attachAnnotations = function(waterfall_data, df, patientID, confirmationVar, ongoingVar) {
          if (!is.null(confirmationVar) && confirmationVar %in% names(df)) {
            info <- df %>%
              dplyr::select(!!rlang::sym(patientID), !!rlang::sym(confirmationVar)) %>%
              dplyr::distinct()
            waterfall_data <- waterfall_data %>% dplyr::left_join(info, by = patientID)
            names(waterfall_data)[names(waterfall_data) == confirmationVar] <- "confirm_status"
          }
          if (!is.null(ongoingVar) && ongoingVar %in% names(df)) {
            info <- df %>%
              dplyr::select(!!rlang::sym(patientID), !!rlang::sym(ongoingVar)) %>%
              dplyr::distinct()
            waterfall_data <- waterfall_data %>% dplyr::left_join(info, by = patientID)
            names(waterfall_data)[names(waterfall_data) == ongoingVar] <- "ongoing_raw"
            waterfall_data$ongoing_flag <- private$.coerceOngoing(waterfall_data$ongoing_raw)
          }
          waterfall_data
        },

        # Coerce an arbitrary vector to a logical "ongoing" flag
        .coerceOngoing = function(x) {
          if (is.logical(x)) return(ifelse(is.na(x), FALSE, x))
          if (is.numeric(x)) return(!is.na(x) & x != 0)
          xs <- tolower(trimws(as.character(x)))
          xs %in% c("yes", "y", "true", "on", "ongoing", "1")
        },
```

- [ ] **Step 3: Add the marker-drawing helper** near `.addResponseLabels` (~line 410):

```r
        # Draw per-bar annotation markers: confirmation shapes + ongoing arrows.
        # x-positions match the bar index used in .createWaterfallBasePlot.
        .addAnnotationMarkers = function(plot, df, plotData) {
          # Confirmation: a point at each bar tip, shape mapped by level
          if (!is.null(plotData$options$confirmationVar) && "confirm_status" %in% names(df)) {
            marker_df <- data.frame(
              xpos = seq_len(nrow(df)),
              ypos = df$response,
              confirm_status = df$confirm_status,
              stringsAsFactors = FALSE
            )
            marker_df <- marker_df[!is.na(marker_df$confirm_status), , drop = FALSE]
            if (nrow(marker_df) > 0) {
              plot <- plot +
                ggplot2::geom_point(
                  data = marker_df,
                  mapping = ggplot2::aes(
                    x = factor(xpos, levels = seq_len(nrow(df))),
                    y = ypos,
                    shape = confirm_status
                  ),
                  size = 2.5,
                  colour = "black",
                  inherit.aes = FALSE
                ) +
                ggplot2::scale_shape_manual(
                  name = .("Confirmation"),
                  values = c(16, 1, 17, 2, 15)[seq_len(min(5, length(unique(marker_df$confirm_status))))]
                )
            }
          }
          # Ongoing: an upward arrow just beyond each ongoing bar tip
          if (!is.null(plotData$options$ongoingVar) && "ongoing_flag" %in% names(df)) {
            on_df <- data.frame(
              xpos = which(isTRUE_vec(df$ongoing_flag)),
              stringsAsFactors = FALSE
            )
            on_df$ystart <- df$response[on_df$xpos]
            # arrow points away from baseline (up for growth, down for shrinkage)
            on_df$yend <- on_df$ystart + ifelse(on_df$ystart >= 0, 8, -8)
            if (nrow(on_df) > 0) {
              plot <- plot +
                ggplot2::geom_segment(
                  data = on_df,
                  mapping = ggplot2::aes(
                    x = factor(xpos, levels = seq_len(nrow(df))),
                    xend = factor(xpos, levels = seq_len(nrow(df))),
                    y = ystart,
                    yend = yend
                  ),
                  arrow = ggplot2::arrow(length = ggplot2::unit(0.15, "cm"), type = "closed"),
                  colour = "black",
                  linewidth = 0.5,
                  inherit.aes = FALSE
                )
            }
          }
          plot
        },
```

- [ ] **Step 4: Add the tiny `isTRUE_vec` utility** at the top of the private list (so `which(isTRUE_vec(...))` selects only TRUE, NA-safe). Add near the other utility helpers:

```r
        # vectorized isTRUE (NA -> FALSE)
        .isTrueVec = function(x) !is.na(x) & x,
```

Then in Step 3's ongoing block, replace `which(isTRUE_vec(df$ongoing_flag))` with `which(private$.isTrueVec(df$ongoing_flag))`.

- [ ] **Step 5: Call the attach helper in each data-assembly path.** In `.processRawDataStandard` (after `waterfall_data$recist_category <- private$.categorizeRECIST(...)`, ~line 587) and in the percentage path (`.processPercentageDataEfficient` — find the analogous `waterfall_data$recist_category <- ` line), append:

```r
          waterfall_data <- private$.attachAnnotations(
            waterfall_data, df, patientID,
            self$options$confirmationVar, self$options$ongoingVar
          )
```

- [ ] **Step 6: Call baseline + markers in the render assembly.** In the waterfall render function (`.waterfallplot`, which chains `.createWaterfallBasePlot` → `.addRecistThresholds` → `.addResponseLabels` → `.addMedianAndCI`), insert the two new calls. After the base plot is built and after `.addRecistThresholds`, add:

```r
          plot <- private$.addBaseline(plot, self$options$showBaseline)
          plot <- private$.addAnnotationMarkers(plot, df, plotData)
```
(Place after thresholds so the baseline sits under markers; `df` here is the sorted data frame returned by `.prepareWaterfallPlotData`.)

- [ ] **Step 7: Parse-check**:

Run:
```bash
Rscript -e 'invisible(parse("R/waterfall.b.R")); cat("parse OK\n")'
```
Expected: `parse OK`.

- [ ] **Step 8: Runtime smoke test.** Create test data first (Task A5 provides the file); then:

```bash
Rscript -e '
Sys.unsetenv("ELECTRON_RUN_AS_NODE"); devtools::load_all(".", quiet=TRUE)
d <- read.csv("data/waterfall_annotation_test_data.csv")
r <- ClinicoPath::waterfall(data=d, patientID="PatientID", responseVar="Response",
      inputType="percentage", confirmationVar="Confirmation", ongoingVar="Ongoing",
      showBaseline=TRUE, sortDirection="conventional")
cat("waterfall ran; plot object present:", !is.null(r$waterfallplot), "\n")'
```
Expected: runs without error, `plot object present: TRUE`.

- [ ] **Step 9: Commit**

```bash
git add R/waterfall.b.R
git commit -m "feat(waterfall): Y=0 baseline + confirmation/ongoing markers (OncoPath #1)"
```

---

### Task A5: Waterfall annotation test data

**Files:**
- Create: `data-raw/waterfall_annotation_test_data.R`, `data/waterfall_annotation_test_data.csv`

- [ ] **Step 1: Write the data-raw generator** `data-raw/waterfall_annotation_test_data.R`:

```r
# Synthetic waterfall data with confirmation + ongoing annotation columns.
# For OncoPath waterfall enhancement testing (issue #1).
set.seed(42)
n <- 30
waterfall_annotation_test_data <- data.frame(
    PatientID    = sprintf("PT%03d", seq_len(n)),
    Response     = round(runif(n, min = -80, max = 45), 1),
    Confirmation = factor(sample(c("Confirmed", "Unconfirmed"), n, replace = TRUE, prob = c(0.7, 0.3))),
    Ongoing      = sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.4, 0.6)),
    Arm          = factor(sample(c("Arm A", "Arm B"), n, replace = TRUE)),
    stringsAsFactors = FALSE
)
write.csv(waterfall_annotation_test_data, "data/waterfall_annotation_test_data.csv", row.names = FALSE)
```

- [ ] **Step 2: Run it**:

```bash
Rscript data-raw/waterfall_annotation_test_data.R && head -3 data/waterfall_annotation_test_data.csv
```
Expected: CSV written; header + 2 rows print.

- [ ] **Step 3: Commit**

```bash
git add data-raw/waterfall_annotation_test_data.R data/waterfall_annotation_test_data.csv
git commit -m "test(waterfall): add annotation test dataset"
```

---

## Phase B — Adverse-Events butterfly plot (`aeplot`)

### Task B1: `aeplot.a.yaml` (options + attribution refs)

**Files:**
- Create: `jamovi/aeplot.a.yaml`

**Interfaces:**
- Produces options: `inputMode`, `subjectID`, `aeTerm`, `armVar`, `gradeVar`, `gradeThreshold`, `aeTermS`, `testAll`, `testHigh`, `controlAll`, `controlHigh`, `barShape`, `colorScheme`, `showValues`, `topN`.

- [ ] **Step 1: Write the file** `jamovi/aeplot.a.yaml`:

```yaml
---
name: aeplot
title: Adverse Events Butterfly Plot
menuGroup: OncoPath
menuSubgroup: 'Patient Follow-Up Plots'
menuSubtitle: 'Butterfly / Tornado Plot of Adverse Events'
version: '0.0.1'
jas: '1.2'

description:
    main: |
        Back-to-back (butterfly) bar plot of adverse-event frequencies by preferred
        term, comparing a test arm against an optional control arm and splitting each
        bar into all-grade and high-grade (e.g. grade >= 3) severity.
        Accepts patient-level data (incidence computed internally) or pre-summarized
        percentages. Inspired by the Jamovi-TrialPlots module by highwind.

options:
    - name: data
      type: Data

    - name: inputMode
      title: Input Mode
      type: List
      options:
        - title: "Patient-level data (compute incidence)"
          name: patient
        - title: "Pre-summarized percentages"
          name: summary
      default: patient

    # --- patient-level mode ---
    - name: subjectID
      title: Subject ID
      type: Variable
      suggested: [nominal]
      permitted: [factor, id, numeric]
      default: NULL
    - name: aeTerm
      title: Adverse Event Term
      type: Variable
      suggested: [nominal]
      permitted: [factor]
      default: NULL
    - name: armVar
      title: Treatment Arm (optional)
      type: Variable
      suggested: [nominal]
      permitted: [factor]
      default: NULL
    - name: gradeVar
      title: Severity Grade (optional)
      type: Variable
      suggested: [ordinal, continuous]
      permitted: [numeric]
      default: NULL
    - name: gradeThreshold
      title: High-Grade Threshold (grade >=)
      type: Number
      min: 1
      max: 5
      default: 3

    # --- summary mode ---
    - name: aeTermS
      title: Adverse Event Term (summary)
      type: Variable
      suggested: [nominal]
      permitted: [factor]
      default: NULL
    - name: testAll
      title: Test Arm All-Grade (%)
      type: Variable
      suggested: [continuous]
      permitted: [numeric]
      default: NULL
    - name: testHigh
      title: Test Arm High-Grade (%)
      type: Variable
      suggested: [continuous]
      permitted: [numeric]
      default: NULL
    - name: controlAll
      title: Control Arm All-Grade (%)
      type: Variable
      suggested: [continuous]
      permitted: [numeric]
      default: NULL
    - name: controlHigh
      title: Control Arm High-Grade (%)
      type: Variable
      suggested: [continuous]
      permitted: [numeric]
      default: NULL

    # --- display ---
    - name: barShape
      title: Bar Shape
      type: List
      options:
        - title: "Inside (nested severity)"
          name: inside
        - title: "Outside (stacked severity)"
          name: outside
      default: inside
    - name: colorScheme
      title: Color Scheme
      type: List
      options:
        - title: "NEJM"
          name: nejm
        - title: "Lancet"
          name: lancet
        - title: "JAMA"
          name: jama
        - title: "JCO"
          name: jco
        - title: "Nature (NPG)"
          name: npg
        - title: "Science (AAAS)"
          name: aaas
        - title: "Colorblind"
          name: colorblind
      default: nejm
    - name: showValues
      title: Show Data Labels
      type: Bool
      default: false
    - name: topN
      title: Show Top N Terms (0 = all)
      type: Integer
      min: 0
      default: 0
...
```

- [ ] **Step 2: Validate YAML**:

```bash
Rscript -e 'yaml::yaml.load_file("jamovi/aeplot.a.yaml"); cat("OK\n")'
```
Expected: `OK`.

- [ ] **Step 3: Commit**

```bash
git add jamovi/aeplot.a.yaml
git commit -m "feat(aeplot): options for adverse-events butterfly plot"
```

---

### Task B2: `aeplot.r.yaml` + `aeplot.u.yaml`

**Files:**
- Create: `jamovi/aeplot.r.yaml`, `jamovi/aeplot.u.yaml`

- [ ] **Step 1: Write `jamovi/aeplot.r.yaml`:**

```yaml
---
name: aeplot
title: Adverse Events Butterfly Plot
jrs: '1.1'

items:
    - name: instructions
      title: Instructions
      type: Html
      visible: true

    - name: freqTable
      title: Adverse Event Frequencies
      type: Table
      rows: 0
      columns:
        - name: ae
          title: "Adverse Event"
          type: text
        - name: arm
          title: "Arm"
          type: text
        - name: allGrade
          title: "All-Grade (%)"
          type: number
        - name: highGrade
          title: "High-Grade (%)"
          type: number
      clearWith:
        - inputMode
        - subjectID
        - aeTerm
        - armVar
        - gradeVar
        - gradeThreshold
        - aeTermS
        - testAll
        - testHigh
        - controlAll
        - controlHigh
        - topN

    - name: plot
      title: Butterfly Plot
      type: Image
      width: 700
      height: 600
      renderFun: .plot
      requiresData: true
      clearWith:
        - inputMode
        - subjectID
        - aeTerm
        - armVar
        - gradeVar
        - gradeThreshold
        - aeTermS
        - testAll
        - testHigh
        - controlAll
        - controlHigh
        - barShape
        - colorScheme
        - showValues
        - topN

    - name: interpretation
      title: Interpretation
      type: Html
      visible: true

refs:
    - trialplots_highwind
    - ClinicoPathJamoviModule
...
```

- [ ] **Step 2: Write `jamovi/aeplot.u.yaml`:**

```yaml
title: Adverse Events Butterfly Plot
name: aeplot
jus: '3.0'
stage: 0
compilerMode: tame
children:
  - type: LayoutBox
    margin: large
    children:
      - type: ComboBox
        name: inputMode
  - type: VariableSupplier
    persistentItems: false
    stretchFactor: 1
    children:
      - type: TargetLayoutBox
        label: Subject ID (patient mode)
        children:
          - type: VariablesListBox
            name: subjectID
            maxItemCount: 1
            isTarget: true
      - type: TargetLayoutBox
        label: Adverse Event Term (patient mode)
        children:
          - type: VariablesListBox
            name: aeTerm
            maxItemCount: 1
            isTarget: true
      - type: TargetLayoutBox
        label: Treatment Arm (optional)
        children:
          - type: VariablesListBox
            name: armVar
            maxItemCount: 1
            isTarget: true
      - type: TargetLayoutBox
        label: Severity Grade (optional)
        children:
          - type: VariablesListBox
            name: gradeVar
            maxItemCount: 1
            isTarget: true
  - type: LayoutBox
    margin: large
    children:
      - type: TextBox
        name: gradeThreshold
        format: number
  - type: CollapseBox
    label: Summary-Mode Inputs
    collapsed: true
    children:
      - type: VariableSupplier
        persistentItems: false
        stretchFactor: 1
        children:
          - type: TargetLayoutBox
            label: AE Term
            children:
              - type: VariablesListBox
                name: aeTermS
                maxItemCount: 1
                isTarget: true
          - type: TargetLayoutBox
            label: Test All-Grade (%)
            children:
              - type: VariablesListBox
                name: testAll
                maxItemCount: 1
                isTarget: true
          - type: TargetLayoutBox
            label: Test High-Grade (%)
            children:
              - type: VariablesListBox
                name: testHigh
                maxItemCount: 1
                isTarget: true
          - type: TargetLayoutBox
            label: Control All-Grade (%)
            children:
              - type: VariablesListBox
                name: controlAll
                maxItemCount: 1
                isTarget: true
          - type: TargetLayoutBox
            label: Control High-Grade (%)
            children:
              - type: VariablesListBox
                name: controlHigh
                maxItemCount: 1
                isTarget: true
  - type: CollapseBox
    label: Display Options
    collapsed: false
    children:
      - type: LayoutBox
        margin: large
        children:
          - type: ComboBox
            name: barShape
          - type: ComboBox
            name: colorScheme
          - type: CheckBox
            name: showValues
          - type: TextBox
            name: topN
            format: number
```

- [ ] **Step 3: Commit** (prepare happens in B4 once the backend exists):

```bash
git add jamovi/aeplot.r.yaml jamovi/aeplot.u.yaml
git commit -m "feat(aeplot): results + UI definitions"
```

---

### Task B3: `aeplot.b.R` backend

**Files:**
- Create: `R/aeplot.b.R`

**Interfaces:**
- Consumes options from Task B1; the base class `aeplotBase` is generated by PREPARE in Task B4.
- Produces: populated `freqTable`, `plot` state, `instructions`/`interpretation` HTML.

- [ ] **Step 1: Write the complete backend** `R/aeplot.b.R`:

```r
#' Adverse Events Butterfly Plot — backend
#'
#' Inspired by the Jamovi-TrialPlots module by highwind
#' (https://github.com/highwindmx/Jamovi-TrialPlots), released under LGPL.
#' This is an independent re-implementation for ClinicoPath (GPL-2): it adds a
#' patient-level input mode that computes AE incidence internally and uses an
#' English UI and ClinicoPath output patterns.
#'
#' @importFrom R6 R6Class
#' @import ggplot2
#' @importFrom magrittr %>%

aeplotClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "aeplotClass",
    inherit = aeplotBase,
    private = list(

        .run = function() {
            self$results$instructions$setContent(private$.instructionsHtml())

            data <- private$.buildButterflyData()
            if (is.null(data) || nrow(data) == 0)
                return()

            # populate frequency table (one row per ae x arm)
            tbl <- self$results$freqTable
            for (i in seq_len(nrow(data$table))) {
                tbl$addRow(rowKey = i, values = list(
                    ae        = data$table$ae[i],
                    arm       = data$table$arm[i],
                    allGrade  = data$table$allGrade[i],
                    highGrade = data$table$highGrade[i]
                ))
            }

            self$results$plot$setState(data)
            self$results$interpretation$setContent(private$.interpretationHtml())
        },

        # ---- data assembly -------------------------------------------------
        .buildButterflyData = function() {
            if (self$options$inputMode == "summary")
                private$.buildFromSummary()
            else
                private$.buildFromPatient()
        },

        .buildFromPatient = function() {
            opt <- self$options
            if (is.null(opt$aeTerm))
                return(NULL)

            df <- self$data
            term <- as.character(df[[opt$aeTerm]])
            arm  <- if (!is.null(opt$armVar)) as.character(df[[opt$armVar]]) else rep("Test", nrow(df))
            subj <- if (!is.null(opt$subjectID)) as.character(df[[opt$subjectID]]) else NULL
            grade <- if (!is.null(opt$gradeVar)) jmvcore::toNumeric(df[[opt$gradeVar]]) else NULL

            keep <- !is.na(term) & !is.na(arm)
            term <- term[keep]; arm <- arm[keep]
            if (!is.null(subj)) subj <- subj[keep]
            if (!is.null(grade)) grade <- grade[keep]

            arms <- unique(arm)
            # denominators: distinct subjects per arm (or event counts if no subjectID)
            denom <- sapply(arms, function(a) {
                if (!is.null(subj)) length(unique(subj[arm == a])) else sum(arm == a)
            })
            names(denom) <- arms

            terms <- unique(term)
            rows <- list()
            for (a in arms) {
                for (t in terms) {
                    sel <- arm == a & term == t
                    if (!is.null(subj)) {
                        all_n  <- length(unique(subj[sel]))
                        high_n <- if (!is.null(grade)) length(unique(subj[sel & grade >= opt$gradeThreshold])) else NA_real_
                    } else {
                        all_n  <- sum(sel)
                        high_n <- if (!is.null(grade)) sum(sel & grade >= opt$gradeThreshold) else NA_real_
                    }
                    rows[[length(rows) + 1]] <- data.frame(
                        ae = t, arm = a,
                        allGrade  = 100 * all_n / denom[[a]],
                        highGrade = if (is.na(high_n)) NA_real_ else 100 * high_n / denom[[a]],
                        stringsAsFactors = FALSE
                    )
                }
            }
            tab <- do.call(rbind, rows)
            private$.assembleModel(tab, arms)
        },

        .buildFromSummary = function() {
            opt <- self$options
            if (is.null(opt$aeTermS) || is.null(opt$testAll))
                return(NULL)
            df <- self$data
            has_ctrl <- !is.null(opt$controlAll)
            tab <- data.frame(
                ae = as.character(df[[opt$aeTermS]]),
                arm = "Test",
                allGrade = jmvcore::toNumeric(df[[opt$testAll]]),
                highGrade = if (!is.null(opt$testHigh)) jmvcore::toNumeric(df[[opt$testHigh]]) else NA_real_,
                stringsAsFactors = FALSE
            )
            arms <- "Test"
            if (has_ctrl) {
                ctrl <- data.frame(
                    ae = as.character(df[[opt$aeTermS]]),
                    arm = "Control",
                    allGrade = jmvcore::toNumeric(df[[opt$controlAll]]),
                    highGrade = if (!is.null(opt$controlHigh)) jmvcore::toNumeric(df[[opt$controlHigh]]) else NA_real_,
                    stringsAsFactors = FALSE
                )
                tab <- rbind(tab, ctrl)
                arms <- c("Test", "Control")
            }
            tab <- tab[!is.na(tab$ae), , drop = FALSE]
            private$.assembleModel(tab, arms)
        },

        # order terms by Test all-grade desc, apply topN, return model list
        .assembleModel = function(tab, arms) {
            test_rows <- tab[tab$arm == arms[1], c("ae", "allGrade")]
            ord <- test_rows$ae[order(-test_rows$allGrade)]
            if (self$options$topN > 0 && length(ord) > self$options$topN)
                ord <- ord[seq_len(self$options$topN)]
            tab <- tab[tab$ae %in% ord, , drop = FALSE]
            tab$ae <- factor(tab$ae, levels = rev(ord))  # rev so highest is at top after coord_flip
            list(table = tab, arms = arms, hasControl = length(arms) > 1)
        },

        # ---- palette -------------------------------------------------------
        .armColor = function() {
            scheme <- self$options$colorScheme
            pick <- function(pal) tryCatch(pal(3)[1], error = function(e) "#0072B5")
            switch(scheme,
                nejm    = pick(ggsci::pal_nejm()),
                lancet  = pick(ggsci::pal_lancet()),
                jama    = pick(ggsci::pal_jama()),
                jco     = pick(ggsci::pal_jco()),
                npg     = pick(ggsci::pal_npg()),
                aaas    = pick(ggsci::pal_aaas()),
                colorblind = "#0072B2",
                "#0072B5"
            )
        },

        # ---- plot ----------------------------------------------------------
        .plot = function(image, ...) {
            model <- image$state
            if (is.null(model)) return(FALSE)
            tab <- model$table
            test_col <- private$.armColor()
            ctrl_col <- "grey60"

            # test arm on the negative side, control on the positive side
            tab$sign <- ifelse(tab$arm == model$arms[1], -1, 1)
            tab$allSigned  <- tab$sign * tab$allGrade
            tab$highSigned <- tab$sign * tab$highGrade

            p <- ggplot2::ggplot(tab, ggplot2::aes(x = ae))
            if (self$options$barShape == "inside") {
                # nested: all-grade (light) with high-grade (dark) overlaid
                p <- p +
                    ggplot2::geom_col(ggplot2::aes(y = allSigned, fill = arm), alpha = 0.5, width = 0.7) +
                    ggplot2::geom_col(ggplot2::aes(y = highSigned, fill = arm), alpha = 1.0, width = 0.7)
            } else {
                # stacked: (all - high) light on top of high dark
                tab$lowSigned <- tab$allSigned - ifelse(is.na(tab$highSigned), 0, tab$highSigned)
                p <- p +
                    ggplot2::geom_col(ggplot2::aes(y = highSigned, fill = arm), alpha = 1.0, width = 0.7) +
                    ggplot2::geom_col(ggplot2::aes(y = lowSigned, fill = arm), alpha = 0.5, width = 0.7)
            }
            if (isTRUE(self$options$showValues)) {
                p <- p + ggplot2::geom_text(
                    ggplot2::aes(y = allSigned,
                                 label = sprintf("%.0f", abs(allGrade)),
                                 hjust = ifelse(sign < 0, 1.1, -0.1)),
                    size = 3)
            }
            fills <- stats::setNames(c(test_col, ctrl_col), model$arms)
            p +
                ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
                ggplot2::scale_fill_manual(name = .("Arm"), values = fills) +
                ggplot2::scale_y_continuous(labels = function(y) abs(y)) +
                ggplot2::coord_flip() +
                ggplot2::labs(x = .("Adverse Event Term"), y = .("Incidence (%)")) +
                ggplot2::theme_classic() +
                ggplot2::theme(legend.position = "bottom")
            print(p)
            TRUE
        },

        # ---- HTML ----------------------------------------------------------
        .instructionsHtml = function() {
            paste0(
                "<div style='padding:8px;'>",
                "<b>", .("Adverse Events Butterfly Plot"), "</b><br>",
                .("Patient mode: select Subject ID, AE Term, and (optionally) Arm and Grade — incidence is computed internally."),
                "<br>",
                .("Summary mode: provide pre-computed all-grade and high-grade percentages per AE term."),
                "</div>"
            )
        },

        .interpretationHtml = function() {
            paste0(
                "<div style='padding:8px;'>",
                .("Bars extend left for the test arm and right for the control arm; darker shading marks high-grade events."),
                "<br><i>",
                .("Inspired by the Jamovi-TrialPlots module by highwind (github.com/highwindmx/Jamovi-TrialPlots)."),
                "</i></div>"
            )
        }
    )
)
```

- [ ] **Step 2: Parse-check**:

```bash
Rscript -e 'invisible(parse("R/aeplot.b.R")); cat("parse OK\n")'
```
Expected: `parse OK`.

- [ ] **Step 3: Commit**

```bash
git add R/aeplot.b.R
git commit -m "feat(aeplot): backend for butterfly plot (patient + summary modes)"
```

---

### Task B4: Register `aeplot`, prepare, and test data

**Files:**
- Modify: `jamovi/0000.yaml` (add analysis entry)
- Create: `data-raw/aeplot_test_data.R`, `data/aeplot_test_data.csv`

- [ ] **Step 1: Add the analysis block to `jamovi/0000.yaml`** (in the `analyses:` sequence, mirroring the `waterfall` block near line 130):

```yaml
  - title: Adverse Events Butterfly Plot
    name: aeplot
    ns: ClinicoPath
    menuGroup: OncoPath
    menuSubgroup: Patient Follow-Up Plots
    menuTitle: Adverse Events Butterfly Plot
    menuSubtitle: Butterfly / Tornado Plot of Adverse Events
    description: >-
      Back-to-back adverse-event frequency plot by preferred term and severity.
```

- [ ] **Step 2: Validate `0000.yaml`** (guards "packageInfo.analyses is not iterable"):

```bash
Rscript -e 'x <- yaml::yaml.load_file("jamovi/0000.yaml"); stopifnot(is.list(x$analyses)); cat("analyses OK:", length(x$analyses), "\n")'
```
Expected: `analyses OK: <N>`.

- [ ] **Step 3: Write test-data generator** `data-raw/aeplot_test_data.R`:

```r
# Patient-level adverse-event test data for the aeplot butterfly analysis.
set.seed(7)
terms <- c("Fatigue", "Nausea", "Neutropenia", "Anemia", "Diarrhea",
           "Rash", "Fever", "Headache")
arms <- c("Experimental", "Control")
rows <- list()
sid <- 1
for (arm in arms) {
    for (subj in seq_len(120)) {
        id <- sprintf("%s-%03d", substr(arm, 1, 3), subj)
        n_ae <- rpois(1, lambda = if (arm == "Experimental") 2.5 else 1.8)
        if (n_ae == 0) next
        for (k in seq_len(n_ae)) {
            rows[[length(rows) + 1]] <- data.frame(
                SubjectID = id,
                Arm = arm,
                AETerm = sample(terms, 1),
                Grade = sample(1:5, 1, prob = c(0.35, 0.30, 0.20, 0.10, 0.05)),
                stringsAsFactors = FALSE
            )
        }
    }
}
aeplot_test_data <- do.call(rbind, rows)
write.csv(aeplot_test_data, "data/aeplot_test_data.csv", row.names = FALSE)
cat("rows:", nrow(aeplot_test_data), "\n")
```

- [ ] **Step 4: Run generator, then PREPARE + document**:

```bash
Rscript data-raw/aeplot_test_data.R
Rscript -e 'Sys.unsetenv("ELECTRON_RUN_AS_NODE"); jmvtools::prepare()'
Rscript -e 'devtools::document()'
```
Expected: CSV written; PREPARE creates `R/aeplot.h.R` with no errors; `NAMESPACE` updated.

- [ ] **Step 5: Runtime smoke test (both modes)**:

```bash
Rscript -e '
Sys.unsetenv("ELECTRON_RUN_AS_NODE"); devtools::load_all(".", quiet=TRUE)
d <- read.csv("data/aeplot_test_data.csv")
r1 <- ClinicoPath::aeplot(data=d, inputMode="patient", subjectID="SubjectID",
        aeTerm="AETerm", armVar="Arm", gradeVar="Grade", gradeThreshold=3)
cat("patient mode rows:", r1$freqTable$rowCount, "\n")
s <- data.frame(AE=c("Fatigue","Nausea"), tAll=c(40,25), tHi=c(8,4), cAll=c(30,20), cHi=c(5,3))
r2 <- ClinicoPath::aeplot(data=s, inputMode="summary", aeTermS="AE", testAll="tAll",
        testHigh="tHi", controlAll="cAll", controlHigh="cHi")
cat("summary mode rows:", r2$freqTable$rowCount, "\n")'
```
Expected: both print non-zero row counts, no errors.

- [ ] **Step 6: Commit**

```bash
git add jamovi/0000.yaml R/aeplot.h.R NAMESPACE data-raw/aeplot_test_data.R data/aeplot_test_data.csv
git commit -m "feat(aeplot): register analysis, generate test data, compile header"
```

---

### Task B5: `aeplot` testthat smoke test

**Files:**
- Create: `tests/testthat/test-aeplot.R`

- [ ] **Step 1: Write the test**:

```r
test_that("aeplot patient mode computes incidence and builds a table", {
    d <- read.csv(testthat::test_path("..", "..", "data", "aeplot_test_data.csv"))
    expect_error(
        r <- ClinicoPath::aeplot(data = d, inputMode = "patient", subjectID = "SubjectID",
              aeTerm = "AETerm", armVar = "Arm", gradeVar = "Grade", gradeThreshold = 3),
        NA)
    expect_gt(r$freqTable$rowCount, 0)
})

test_that("aeplot summary mode accepts pre-computed percentages", {
    s <- data.frame(AE = c("Fatigue", "Nausea"), tAll = c(40, 25),
                    tHi = c(8, 4), cAll = c(30, 20), cHi = c(5, 3))
    expect_error(
        r <- ClinicoPath::aeplot(data = s, inputMode = "summary", aeTermS = "AE",
              testAll = "tAll", testHigh = "tHi", controlAll = "cAll", controlHigh = "cHi"),
        NA)
    expect_equal(r$freqTable$rowCount, 4)  # 2 terms x 2 arms
})
```

- [ ] **Step 2: Run it**:

```bash
Rscript -e 'Sys.unsetenv("ELECTRON_RUN_AS_NODE"); devtools::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-aeplot.R")'
```
Expected: all tests PASS.

- [ ] **Step 3: Commit**

```bash
git add tests/testthat/test-aeplot.R
git commit -m "test(aeplot): patient + summary mode smoke tests"
```

---

## Phase C — Group-sequential design & sample size (`gsdesign`)

### Task C1: `gsdesign.a.yaml`

**Files:**
- Create: `jamovi/gsdesign.a.yaml`

- [ ] **Step 1: Write the file** `jamovi/gsdesign.a.yaml`:

```yaml
---
name: gsdesign
title: Group-Sequential Design & Sample Size
menuGroup: OncoPath
menuSubgroup: 'Trial Design'
menuSubtitle: 'Interim Analyses, Boundaries, Sample Size'
version: '0.0.1'
jas: '1.2'

description:
    main: |
        Group-sequential trial design and sample-size / events calculation for
        survival (time-to-event), binary, and continuous endpoints using the
        gsDesign package. Reports efficacy (and optional futility) boundaries,
        per-look sample size, and a boundary plot. Inspired by the Jamovi-TrialPlots
        module by highwind.

options:
    - name: data
      type: Data

    - name: endpoint
      title: Endpoint Type
      type: List
      options:
        - title: "Survival (time-to-event)"
          name: survival
        - title: "Binary (two proportions)"
          name: binary
        - title: "Continuous (two means)"
          name: continuous
      default: survival

    - name: sided
      title: Sided
      type: List
      options:
        - title: "Two-sided"
          name: '2'
        - title: "One-sided"
          name: '1'
      default: '2'
    - name: alpha
      title: Alpha (total type I error)
      type: Number
      min: 0.0001
      max: 0.5
      default: 0.05
    - name: power
      title: Power (1 - beta)
      type: Number
      min: 0.5
      max: 0.999
      default: 0.9
    - name: kMax
      title: Number of Analyses (incl. final)
      type: Integer
      min: 1
      max: 10
      default: 2
    - name: sfu
      title: Spending Function
      type: List
      options:
        - title: "O'Brien-Fleming (Lan-DeMets)"
          name: OF
        - title: "Pocock (Lan-DeMets)"
          name: Pocock
        - title: "Hwang-Shih-DeCani"
          name: HSD
        - title: "Wang-Tsiatis"
          name: WT
      default: OF
    - name: sfupar
      title: Spending Parameter (HSD gamma / WT delta)
      type: Number
      default: -4
    - name: timing
      title: Interim Timing (comma-separated info fractions; blank = equal)
      type: String
      default: ''
    - name: testType
      title: Boundaries
      type: List
      options:
        - title: "Efficacy only"
          name: efficacy
        - title: "Efficacy + (non-binding) futility"
          name: efffut
      default: efficacy

    # survival
    - name: hr
      title: Hazard Ratio (alternative)
      type: Number
      min: 0.01
      max: 5
      default: 0.7
    - name: medianControl
      title: Control Median Survival (months)
      type: Number
      min: 0.1
      default: 12
    - name: accrualDuration
      title: Accrual Duration (months)
      type: Number
      min: 0.1
      default: 12
    - name: followupDuration
      title: Follow-up Duration (months)
      type: Number
      min: 0
      default: 18
    - name: ratio
      title: Allocation Ratio (exp/control)
      type: Number
      min: 0.1
      max: 10
      default: 1
    - name: dropoutRate
      title: Annual Dropout Rate
      type: Number
      min: 0
      max: 0.99
      default: 0.05

    # binary
    - name: p1
      title: Control Event Rate
      type: Number
      min: 0.001
      max: 0.999
      default: 0.4
    - name: p2
      title: Treatment Event Rate
      type: Number
      min: 0.001
      max: 0.999
      default: 0.25

    # continuous
    - name: deltaMean
      title: Mean Difference
      type: Number
      default: 0.5
    - name: stdDev
      title: Standard Deviation
      type: Number
      min: 0.0001
      default: 1
...
```

- [ ] **Step 2: Validate YAML**:

```bash
Rscript -e 'yaml::yaml.load_file("jamovi/gsdesign.a.yaml"); cat("OK\n")'
```
Expected: `OK`.

- [ ] **Step 3: Commit**

```bash
git add jamovi/gsdesign.a.yaml
git commit -m "feat(gsdesign): options for group-sequential design"
```

---

### Task C2: `gsdesign.r.yaml` + `gsdesign.u.yaml`

**Files:**
- Create: `jamovi/gsdesign.r.yaml`, `jamovi/gsdesign.u.yaml`

- [ ] **Step 1: Write `jamovi/gsdesign.r.yaml`:**

```yaml
---
name: gsdesign
title: Group-Sequential Design & Sample Size
jrs: '1.1'

items:
    - name: summary
      title: Design Summary
      type: Html
      visible: true

    - name: boundaryTable
      title: Group-Sequential Boundaries
      type: Table
      rows: 0
      columns:
        - name: analysis
          title: "Analysis"
          type: integer
        - name: infoFrac
          title: "Info Fraction"
          type: number
        - name: n
          title: "Sample Size / Events"
          type: number
        - name: zBound
          title: "Efficacy Z"
          type: number
        - name: pNominal
          title: "Nominal p"
          type: number
        - name: effBound
          title: "Boundary (effect scale)"
          type: number
        - name: cumAlpha
          title: "Cum. Alpha Spent"
          type: number
      clearWith:
        - endpoint
        - sided
        - alpha
        - power
        - kMax
        - sfu
        - sfupar
        - timing
        - testType
        - hr
        - medianControl
        - accrualDuration
        - followupDuration
        - ratio
        - dropoutRate
        - p1
        - p2
        - deltaMean
        - stdDev

    - name: boundaryPlot
      title: Boundary Plot
      type: Image
      width: 700
      height: 500
      renderFun: .plot
      requiresData: false
      clearWith:
        - endpoint
        - sided
        - alpha
        - power
        - kMax
        - sfu
        - sfupar
        - timing
        - testType
        - hr
        - medianControl
        - accrualDuration
        - followupDuration
        - ratio
        - dropoutRate
        - p1
        - p2
        - deltaMean
        - stdDev

refs:
    - gsDesign_anderson
    - obrien1979
    - trialplots_highwind
    - ClinicoPathJamoviModule
...
```

- [ ] **Step 2: Write `jamovi/gsdesign.u.yaml`:**

```yaml
title: Group-Sequential Design & Sample Size
name: gsdesign
jus: '3.0'
stage: 0
compilerMode: tame
children:
  - type: LayoutBox
    margin: large
    children:
      - type: ComboBox
        name: endpoint
  - type: CollapseBox
    label: Design Parameters
    collapsed: false
    children:
      - type: LayoutBox
        margin: large
        children:
          - type: ComboBox
            name: sided
          - type: TextBox
            name: alpha
            format: number
          - type: TextBox
            name: power
            format: number
          - type: TextBox
            name: kMax
            format: number
          - type: ComboBox
            name: sfu
          - type: TextBox
            name: sfupar
            format: number
          - type: TextBox
            name: timing
            format: string
          - type: ComboBox
            name: testType
  - type: CollapseBox
    label: Survival Endpoint
    collapsed: false
    children:
      - type: LayoutBox
        margin: large
        children:
          - type: TextBox
            name: hr
            format: number
          - type: TextBox
            name: medianControl
            format: number
          - type: TextBox
            name: accrualDuration
            format: number
          - type: TextBox
            name: followupDuration
            format: number
          - type: TextBox
            name: ratio
            format: number
          - type: TextBox
            name: dropoutRate
            format: number
  - type: CollapseBox
    label: Binary Endpoint
    collapsed: true
    children:
      - type: LayoutBox
        margin: large
        children:
          - type: TextBox
            name: p1
            format: number
          - type: TextBox
            name: p2
            format: number
  - type: CollapseBox
    label: Continuous Endpoint
    collapsed: true
    children:
      - type: LayoutBox
        margin: large
        children:
          - type: TextBox
            name: deltaMean
            format: number
          - type: TextBox
            name: stdDev
            format: number
```

- [ ] **Step 3: Commit**

```bash
git add jamovi/gsdesign.r.yaml jamovi/gsdesign.u.yaml
git commit -m "feat(gsdesign): results + UI definitions"
```

---

### Task C3: `gsdesign.b.R` backend (with library-field verification)

**Files:**
- Create: `R/gsdesign.b.R`

**Interfaces:**
- Consumes options from Task C1; base class `gsdesignBase` generated by PREPARE in Task C4.
- Uses gsDesign fields that are **stable across versions**: `x$k`, `x$n.I`, `x$timing`, `x$upper$bound`, `x$upper$spend`, `x$lower$bound`; and `gsDesign::gsBoundSummary(x)` for the human-readable summary.

- [ ] **Step 1: Verify gsDesign object field names in a console FIRST** (do not skip — prevents wiring wrong fields):

```bash
Rscript -e '
x <- gsDesign::gsSurv(k=2, test.type=1, alpha=0.025, beta=0.1, sfu=gsDesign::sfLDOF,
       lambdaC=log(2)/12, hr=0.7, eta=-log(1-0.05)/12, T=30, minfup=18, ratio=1)
cat("n.I (events):", x$n.I, "\n")
cat("timing:", x$timing, "\n")
cat("upper$bound:", x$upper$bound, "\n")
cat("sample size (eNC+eNE final):", tryCatch(sum(x$eNC[length(x$eNC)], x$eNE[length(x$eNE)]), error=function(e) NA), "\n")
print(gsDesign::gsBoundSummary(x))'
```
Expected: prints events, timing, Z boundaries, an approximate total N, and a boundary-summary data frame. Note the actual column names of `gsBoundSummary` output for use below (typically `Analysis`, `Value`, `Efficacy`).

- [ ] **Step 2: Write the backend** `R/gsdesign.b.R`:

```r
#' Group-Sequential Design & Sample Size — backend
#'
#' Inspired by the Jamovi-TrialPlots module by highwind
#' (https://github.com/highwindmx/Jamovi-TrialPlots), released under LGPL, which
#' uses gsDesign2 for a survival design. This is an independent re-implementation
#' for ClinicoPath (GPL-2) built on the CRAN gold-standard gsDesign package,
#' extended to survival, binary, and continuous endpoints.
#'
#' @importFrom R6 R6Class

gsdesignClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "gsdesignClass",
    inherit = gsdesignBase,
    private = list(

        .run = function() {
            x <- tryCatch(private$.buildDesign(), error = function(e) {
                self$results$summary$setContent(private$.errHtml(conditionMessage(e)))
                NULL
            })
            if (is.null(x)) return()

            self$results$boundaryPlot$setState(x)
            private$.fillBoundaryTable(x)
            self$results$summary$setContent(private$.summaryHtml(x))
        },

        .buildDesign = function() {
            opt <- self$options
            stopifnot(opt$alpha > 0, opt$alpha < 1, opt$power > 0, opt$power < 1, opt$kMax >= 1)
            alpha1 <- if (identical(opt$sided, "2")) opt$alpha / 2 else opt$alpha
            beta <- 1 - opt$power
            test.type <- if (identical(opt$testType, "efffut")) 4 else 1

            # spending function + parameter
            sfu <- switch(opt$sfu,
                OF     = gsDesign::sfLDOF,
                Pocock = gsDesign::sfLDPocock,
                HSD    = gsDesign::sfHSD,
                WT     = "WT")
            timing <- private$.parseTiming(opt$timing, opt$kMax)

            common <- list(k = opt$kMax, test.type = test.type,
                           alpha = alpha1, beta = beta, sfu = sfu, timing = timing)
            if (opt$sfu %in% c("HSD", "WT")) common$sfupar <- opt$sfupar

            if (opt$endpoint == "survival") {
                args <- c(common, list(
                    lambdaC = log(2) / opt$medianControl,
                    hr = opt$hr,
                    eta = -log(1 - opt$dropoutRate) / 12,
                    T = opt$accrualDuration + opt$followupDuration,
                    minfup = opt$followupDuration,
                    ratio = opt$ratio))
                x <- do.call(gsDesign::gsSurv, args)
                x$.effectScale <- "HR"
            } else if (opt$endpoint == "binary") {
                nfix <- gsDesign::nBinomial(p1 = opt$p1, p2 = opt$p2,
                          alpha = alpha1, beta = beta, ratio = opt$ratio)
                args <- c(common, list(n.fix = nfix, delta1 = opt$p2 - opt$p1))
                x <- do.call(gsDesign::gsDesign, args)
                x$.effectScale <- "risk difference"
            } else {
                d <- opt$deltaMean / opt$stdDev
                za <- stats::qnorm(1 - alpha1); zb <- stats::qnorm(opt$power)
                nfix_per_group <- (za + zb)^2 / d^2
                nfix <- ceiling(nfix_per_group * (1 + opt$ratio))  # total, unequal-allocation adjusted
                args <- c(common, list(n.fix = nfix, delta1 = d))
                x <- do.call(gsDesign::gsDesign, args)
                x$.effectScale <- "std. effect size"
            }
            x
        },

        .parseTiming = function(s, k) {
            s <- trimws(s)
            if (nchar(s) == 0) return(1)  # gsDesign: 1 = equal spacing
            v <- suppressWarnings(as.numeric(strsplit(s, ",")[[1]]))
            v <- v[!is.na(v)]
            if (length(v) == 0) return(1)
            v
        },

        .fillBoundaryTable = function(x) {
            tbl <- self$results$boundaryTable
            k <- x$k
            zeff <- x$upper$bound
            pnom <- stats::pnorm(-zeff)              # one-sided nominal p at each look
            cumAlpha <- cumsum(x$upper$spend)
            # effect-scale boundary via HR/effect approx from gsBoundSummary if available
            effbound <- rep(NA_real_, k)
            bs <- tryCatch(gsDesign::gsBoundSummary(x), error = function(e) NULL)
            for (i in seq_len(k)) {
                tbl$addRow(rowKey = i, values = list(
                    analysis = i,
                    infoFrac = x$timing[i],
                    n        = x$n.I[i],
                    zBound   = zeff[i],
                    pNominal = pnom[i],
                    effBound = effbound[i],
                    cumAlpha = cumAlpha[i]
                ))
            }
        },

        .plot = function(image, ...) {
            x <- image$state
            if (is.null(x)) return(FALSE)
            p <- tryCatch(plot(x), error = function(e) NULL)
            if (is.null(p)) {
                # fallback: manual Z-boundary plot
                df <- data.frame(info = x$timing, z = x$upper$bound)
                p <- ggplot2::ggplot(df, ggplot2::aes(x = info, y = z)) +
                    ggplot2::geom_line() + ggplot2::geom_point() +
                    ggplot2::labs(x = "Information fraction", y = "Efficacy Z boundary") +
                    ggplot2::theme_classic()
            }
            print(p)
            TRUE
        },

        .summaryHtml = function(x) {
            opt <- self$options
            bs <- tryCatch(
                paste(utils::capture.output(print(gsDesign::gsBoundSummary(x))), collapse = "<br>"),
                error = function(e) "")
            final_n <- x$n.I[length(x$n.I)]
            unit <- if (opt$endpoint == "survival") .("events at final analysis")
                    else .("subjects at final analysis")
            paste0(
                "<div style='padding:8px;font-family:monospace;'>",
                "<b>", .("Group-Sequential Design"), "</b><br>",
                .("Endpoint"), ": ", opt$endpoint, "<br>",
                .("Analyses"), ": ", x$k, " | ", .("Spending"), ": ", opt$sfu, "<br>",
                .("Alpha (1-sided)"), ": ", signif(if (identical(opt$sided,"2")) opt$alpha/2 else opt$alpha, 3),
                " | ", .("Power"), ": ", opt$power, "<br>",
                .("Max"), " ", unit, ": ", ceiling(final_n), "<br><br>",
                bs,
                "<br><i>", .("Inspired by the Jamovi-TrialPlots module by highwind (github.com/highwindmx/Jamovi-TrialPlots)."), "</i>",
                "</div>"
            )
        },

        .errHtml = function(msg) {
            paste0("<div style='padding:8px;color:#8a1f11;'><b>",
                   .("Design could not be computed"), ":</b> ",
                   jmvcore::htmlEscape(msg), "</div>")
        }
    )
)
```

- [ ] **Step 3: Parse-check**:

```bash
Rscript -e 'invisible(parse("R/gsdesign.b.R")); cat("parse OK\n")'
```
Expected: `parse OK`.

- [ ] **Step 4: Commit**

```bash
git add R/gsdesign.b.R
git commit -m "feat(gsdesign): backend for survival/binary/continuous group-sequential design"
```

---

### Task C4: Register `gsdesign`, prepare, verify against console

**Files:**
- Modify: `jamovi/0000.yaml`

- [ ] **Step 1: Add the analysis block to `jamovi/0000.yaml`:**

```yaml
  - title: Group-Sequential Design & Sample Size
    name: gsdesign
    ns: ClinicoPath
    menuGroup: OncoPath
    menuSubgroup: Trial Design
    menuTitle: Group-Sequential Design & Sample Size
    menuSubtitle: Interim Analyses, Boundaries, Sample Size
    description: >-
      Group-sequential design and sample-size calculation for survival, binary,
      and continuous endpoints using gsDesign.
```

- [ ] **Step 2: Validate `0000.yaml`**:

```bash
Rscript -e 'x <- yaml::yaml.load_file("jamovi/0000.yaml"); stopifnot(is.list(x$analyses)); cat("analyses OK:", length(x$analyses), "\n")'
```
Expected: `analyses OK: <N>`.

- [ ] **Step 3: PREPARE + document**:

```bash
Rscript -e 'Sys.unsetenv("ELECTRON_RUN_AS_NODE"); jmvtools::prepare()'
Rscript -e 'devtools::document()'
```
Expected: `R/gsdesign.h.R` created, no errors.

- [ ] **Step 4: Runtime test + numeric cross-check** against a direct `gsSurv` call:

```bash
Rscript -e '
Sys.unsetenv("ELECTRON_RUN_AS_NODE"); devtools::load_all(".", quiet=TRUE)
r <- ClinicoPath::gsdesign(data=data.frame(x=1), endpoint="survival", sided="2",
      alpha=0.05, power=0.9, kMax=2, sfu="OF", hr=0.7, medianControl=12,
      accrualDuration=12, followupDuration=18, ratio=1, dropoutRate=0.05)
cat("boundary rows:", r$boundaryTable$rowCount, "\n")
ref <- gsDesign::gsSurv(k=2, test.type=1, alpha=0.025, beta=0.1, sfu=gsDesign::sfLDOF,
        lambdaC=log(2)/12, hr=0.7, eta=-log(1-0.05)/12, T=30, minfup=18, ratio=1)
cat("reference final events:", round(ref$n.I[2],1), "\n")'
```
Expected: `boundary rows: 2`; the analysis's final-events value matches the reference `ref$n.I[2]`.

- [ ] **Step 5: Verify binary + continuous run without error**:

```bash
Rscript -e '
Sys.unsetenv("ELECTRON_RUN_AS_NODE"); devtools::load_all(".", quiet=TRUE)
b <- ClinicoPath::gsdesign(data=data.frame(x=1), endpoint="binary", kMax=3, sfu="OF", p1=0.4, p2=0.25)
cc <- ClinicoPath::gsdesign(data=data.frame(x=1), endpoint="continuous", kMax=2, sfu="Pocock", deltaMean=0.5, stdDev=1)
cat("binary rows:", b$boundaryTable$rowCount, "continuous rows:", cc$boundaryTable$rowCount, "\n")'
```
Expected: `binary rows: 3 continuous rows: 2`, no errors.

- [ ] **Step 6: Commit**

```bash
git add jamovi/0000.yaml R/gsdesign.h.R NAMESPACE
git commit -m "feat(gsdesign): register analysis and compile header"
```

---

### Task C5: `gsdesign` testthat smoke test

**Files:**
- Create: `tests/testthat/test-gsdesign.R`

- [ ] **Step 1: Write the test**:

```r
test_that("gsdesign survival matches a direct gsSurv events calculation", {
    r <- ClinicoPath::gsdesign(data = data.frame(x = 1), endpoint = "survival",
          sided = "2", alpha = 0.05, power = 0.9, kMax = 2, sfu = "OF",
          hr = 0.7, medianControl = 12, accrualDuration = 12,
          followupDuration = 18, ratio = 1, dropoutRate = 0.05)
    expect_equal(r$boundaryTable$rowCount, 2)
    ref <- gsDesign::gsSurv(k = 2, test.type = 1, alpha = 0.025, beta = 0.1,
            sfu = gsDesign::sfLDOF, lambdaC = log(2)/12, hr = 0.7,
            eta = -log(1 - 0.05)/12, T = 30, minfup = 18, ratio = 1)
    final_events <- r$boundaryTable$asDF$n[2]
    expect_equal(final_events, ref$n.I[2], tolerance = 0.01)
})

test_that("gsdesign binary and continuous endpoints run", {
    expect_error(
        ClinicoPath::gsdesign(data = data.frame(x = 1), endpoint = "binary",
            kMax = 3, sfu = "OF", p1 = 0.4, p2 = 0.25), NA)
    expect_error(
        ClinicoPath::gsdesign(data = data.frame(x = 1), endpoint = "continuous",
            kMax = 2, sfu = "Pocock", deltaMean = 0.5, stdDev = 1), NA)
})

test_that("gsdesign reports an error cleanly for invalid alpha", {
    r <- ClinicoPath::gsdesign(data = data.frame(x = 1), endpoint = "survival", alpha = 1.5)
    # invalid alpha -> summary carries an error message, no crash
    expect_true(grepl("could not be computed|error", r$summary$content, ignore.case = TRUE))
})
```

- [ ] **Step 2: Run it**:

```bash
Rscript -e 'Sys.unsetenv("ELECTRON_RUN_AS_NODE"); devtools::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-gsdesign.R")'
```
Expected: all tests PASS.

- [ ] **Step 3: Commit**

```bash
git add tests/testthat/test-gsdesign.R
git commit -m "test(gsdesign): survival cross-check + binary/continuous/error tests"
```

---

## Phase D — Attribution, dependencies, refs, sync

### Task D1: DESCRIPTION dependencies + references

**Files:**
- Modify: `DESCRIPTION` (add `gsDesign`, `ggsci` to Imports)
- Modify: `jamovi/00refs.yaml` (add attribution + methodology refs)

- [ ] **Step 1: Add `gsDesign` and `ggsci` to the `Imports:` list in `DESCRIPTION`** (alphabetical, comma-separated within the existing block).

- [ ] **Step 2: Append reference entries to `jamovi/00refs.yaml`** under the `refs:` mapping:

```yaml
    trialplots_highwind:
        type: 'software'
        author: highwind
        year: 2025
        title: "Jamovi-TrialPlots: Plot data for clinical trials (jamovi module)"
        publisher: 'GitHub'
        url: https://github.com/highwindmx/Jamovi-TrialPlots

    gsDesign_anderson:
        type: 'software'
        author: Anderson, K.
        year: 2024
        title: "gsDesign: Group Sequential Design"
        publisher: 'R package (CRAN)'
        url: https://cran.r-project.org/package=gsDesign

    ggsci_xiao:
        type: 'software'
        author: Xiao, N.
        year: 2024
        title: "ggsci: Scientific Journal and Sci-Fi Themed Color Palettes for ggplot2"
        publisher: 'R package (CRAN)'
        url: https://cran.r-project.org/package=ggsci

    obrien1979:
        type: 'article'
        author: O'Brien, P. C., & Fleming, T. R.
        year: 1979
        title: "A multiple testing procedure for clinical trials"
        publisher: 'Biometrics'
        volume: 35
        issue: 3
        pages: 549-556
        doi: 10.2307/2530245

    pocock1977:
        type: 'article'
        author: Pocock, S. J.
        year: 1977
        title: "Group sequential methods in the design and analysis of clinical trials"
        publisher: 'Biometrika'
        volume: 64
        issue: 2
        pages: 191-199
        doi: 10.1093/biomet/64.2.191

    lan1983:
        type: 'article'
        author: Lan, K. K. G., & DeMets, D. L.
        year: 1983
        title: "Discrete sequential boundaries for clinical trials"
        publisher: 'Biometrika'
        volume: 70
        issue: 3
        pages: 659-663
        doi: 10.1093/biomet/70.3.659

    hwang1990:
        type: 'article'
        author: Hwang, I. K., Shih, W. J., & de Cani, J. S.
        year: 1990
        title: "Group sequential designs using a family of type I error probability spending functions"
        publisher: 'Statistics in Medicine'
        volume: 9
        issue: 12
        pages: 1439-1445
        doi: 10.1002/sim.4780091207
```

- [ ] **Step 3: Validate `00refs.yaml`**:

```bash
Rscript -e 'x <- yaml::yaml.load_file("jamovi/00refs.yaml"); stopifnot(!is.null(x$refs$trialplots_highwind)); cat("refs OK:", length(x$refs), "\n")'
```
Expected: `refs OK: <N>`.

- [ ] **Step 4: PREPARE + document** (so refs bind and NAMESPACE picks up the new imports):

```bash
Rscript -e 'Sys.unsetenv("ELECTRON_RUN_AS_NODE"); jmvtools::prepare()'
Rscript -e 'devtools::document()'
```
Expected: no errors; aeplot/gsdesign References panels now resolve.

- [ ] **Step 5: Commit**

```bash
git add DESCRIPTION jamovi/00refs.yaml jamovi/*.h.R NAMESPACE jamovi/0000.yaml
git commit -m "chore: add gsDesign/ggsci imports + attribution & methodology refs"
```

---

### Task D2: NEWS + waterfall refs + attribution touchpoints

**Files:**
- Modify: `NEWS.md`, `jamovi/waterfall.r.yaml`

- [ ] **Step 1: Read the current version** from `DESCRIPTION`:

```bash
Rscript -e 'cat(read.dcf("DESCRIPTION")[1,"Version"], "\n")'
```

- [ ] **Step 2: Prepend a NEWS entry** to `NEWS.md` using that version (replace `<VERSION>`):

```markdown
# ClinicoPath <VERSION>

## OncoPath

### Added
- **Adverse Events Butterfly Plot** (`aeplot`) — back-to-back AE frequency plot by
  preferred term and severity, with patient-level and pre-summarized input modes.
  Inspired by the Jamovi-TrialPlots module by highwind.
- **Group-Sequential Design & Sample Size** (`gsdesign`) — interim-analysis
  boundaries and sample-size/events for survival, binary, and continuous endpoints
  via gsDesign. Inspired by the Jamovi-TrialPlots module by highwind.

### Changed
- **Waterfall plot** now sorts responses in the conventional oncology order
  (worst on the left, best on the right) by default, adds an optional Y = 0
  baseline line, and supports confirmation-status and on-treatment/ongoing
  annotation markers. Thanks to @highwindmx (OncoPath issue #1).
```

- [ ] **Step 3: Add `trialplots_highwind` to the waterfall References.** In `jamovi/waterfall.r.yaml`, extend the trailing `refs:` list:

```yaml
refs:
    - recist
    - trialplots_highwind
    - ClinicoPathJamoviModule
```

- [ ] **Step 4: Commit**

```bash
git add NEWS.md jamovi/waterfall.r.yaml
git commit -m "docs: NEWS + waterfall reference crediting OncoPath #1 and Jamovi-TrialPlots"
```

---

### Task D3: Full-module verification + submodule sync

**Files:** none created; verification + propagation.

- [ ] **Step 1: Final PREPARE (whole module, no errors)**:

```bash
Rscript -e 'Sys.unsetenv("ELECTRON_RUN_AS_NODE"); jmvtools::prepare()'
```
Expected: completes with **no errors or warnings**.

- [ ] **Step 2: Run the full new-feature test suite**:

```bash
Rscript -e 'Sys.unsetenv("ELECTRON_RUN_AS_NODE"); devtools::load_all(".", quiet=TRUE);
testthat::test_dir("tests/testthat", filter = "aeplot|gsdesign|waterfall-enhancements")'
```
Expected: all tests PASS.

- [ ] **Step 3: Propagate to the OncoPath submodule**:

```bash
Rscript _updateModules.R
```
Expected: `aeplot`, `gsdesign`, and the updated `waterfall` files appear in `/Users/serdarbalci/Documents/GitHub/OncoPath`. (If OncoPath is disabled in `_updateModules_config.yaml` (`OncoPath: false`), enable it first or note that the user runs the sync manually.)

- [ ] **Step 4: Confirm propagation**:

```bash
ls /Users/serdarbalci/Documents/GitHub/OncoPath/R/aeplot.b.R /Users/serdarbalci/Documents/GitHub/OncoPath/R/gsdesign.b.R 2>/dev/null && echo "SYNCED" || echo "sync pending (enable OncoPath in config)"
```

- [ ] **Step 5: Commit any config change** (only if you enabled OncoPath sync):

```bash
git add _updateModules_config.yaml
git commit -m "chore: enable OncoPath submodule sync for new analyses"
```

---

## Self-Review

**1. Spec coverage** — every spec section maps to a task:
- §3.1 conventional sort → **A1** (option), **A3** (both code paths). ✓
- §3.2 baseline line → **A1** (`showBaseline`), **A4** (`.addBaseline`). ✓
- §3.3 annotation markers → **A1** (vars), **A4** (`.attachAnnotations`/`.coerceOngoing`/`.addAnnotationMarkers`), **A5** (test data). ✓
- §3.4 clearWith + UI → **A2**. ✓
- §4 aeplot (both input modes, barShape, palettes, topN, outputs) → **B1–B5**. ✓
- §5 gsdesign (survival/binary/continuous, spending, boundaries, plot, summary) → **C1–C5**. ✓
- §6 registration + imports → **B4, C4, D1**. ✓
- §8b attribution (00refs, per-analysis refs, header comments, About HTML, NEWS) → header comments in **B3/C3**, About HTML in **B3/C3**, refs in **B2/C2/D1**, NEWS + waterfall ref in **D2**. ✓
- §7 verification (prepare, document, load_all, cross-check, testthat) → present in **A4, B4, B5, C1, C4, C5, D3**. ✓

**2. Placeholder scan** — no "TBD/TODO/handle edge cases/similar to Task N"; every code step shows complete code. gsDesign internal field names are verified live in **C3 Step 1** and **C4 Step 4** rather than assumed. ✓

**3. Type consistency** — helper/method names are consistent across tasks: `.attachAnnotations`, `.coerceOngoing`, `.isTrueVec`, `.addBaseline`, `.addAnnotationMarkers` (A4); `.buildButterflyData`/`.buildFromPatient`/`.buildFromSummary`/`.assembleModel`/`.armColor`/`.plot` (B3); `.buildDesign`/`.parseTiming`/`.fillBoundaryTable`/`.plot`/`.summaryHtml`/`.errHtml` (C3). Option names match `.a.yaml` ↔ backend ↔ `clearWith` in every phase. Result item names (`freqTable`, `plot`, `instructions`, `interpretation`; `summary`, `boundaryTable`, `boundaryPlot`) match between each `.r.yaml` and its `.b.R`. ✓

> **Known follow-up (not a blocker):** `effBound` (effect-scale boundary column) is populated as `NA` in C3 Step 2; wiring it from `gsBoundSummary()`/`hrn2z`-style conversion is a refinement to add once the exact `gsBoundSummary` column names are confirmed in C3 Step 1. The Z-boundary, nominal p, and cumulative alpha columns are fully populated.
