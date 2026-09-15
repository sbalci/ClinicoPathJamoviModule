# OncoPath ↔ Jamovi-TrialPlots Parity & Waterfall Issue #1 — Design

**Date:** 2026-07-11
**Author:** Serdar Balcı (with Claude Code)
**Status:** Approved for implementation
**Source issue:** https://github.com/sbalci/OncoPath/issues/1
**Reference module:** https://github.com/highwindmx/Jamovi-TrialPlots

---

## 1. Goal & scope

Two threads, both requested by the user:

1. **Close OncoPath issue #1** — three waterfall-plot enhancements requested by user `highwindmx`.
2. **Reach feature parity with `Jamovi-TrialPlots`** — add the two analyses OncoPath lacks:
   - Adverse-Events butterfly plot (`aeplot`)
   - Group-sequential design & sample size (`gsdesign`)

The reference's Waterfall, Spider, and Swimmer plots are **already present and richer** in OncoPath (`waterfall` — which also does the spider plot — and `swimmerplot`), so those are only *enhanced*, not rebuilt.

### Success criteria

- Issue #1's three requests are visibly satisfied in the waterfall plot.
- Two new analyses run in jamovi without errors, in both/all configured modes.
- `jmvtools::prepare()` produces **no errors**; `devtools::document()` clean.
- New analyses have test data and `testthat` smoke tests.
- `gsdesign` boundary output matches a hand-verified `gsDesign` call.

### Non-goals

- Full RECIST v1.1 compliance for the waterfall (a separate `waterfallrecist` analysis already exists).
- Adaptive designs / futility-only designs beyond what `gsDesign` exposes simply.
- Translating the reference's Chinese UI verbatim — OncoPath uses English labels.

---

## 2. Architecture & source-of-truth

**The main `ClinicoPathJamoviModule` repo is the source of truth.**

- `R/waterfall.b.R` in the main repo is byte-identical to the OncoPath submodule copy.
- Analyses are routed to the OncoPath submodule by the tag `menuGroup: OncoPath` in their `.a.yaml` and their registration block in `jamovi/0000.yaml`.
- `Rscript _updateModules.R` propagates OncoPath-tagged analyses into `/Users/serdarbalci/Documents/GitHub/OncoPath`.

**Therefore every edit in this spec is made in `/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/`**, then synced. No files are edited directly in the OncoPath submodule.

Standard jamovi 4-file architecture per analysis:
- `.a.yaml` — options; `.u.yaml` — UI; `.r.yaml` — results; `.b.R` — R6 backend; `.h.R` — auto-generated (never hand-edited).

Conventions to honor (from `CLAUDE.md` / memory):
- `.a.yaml`: `type: Level` cannot have `default:`; optional `Variable`/`Variables` need `default: NULL`.
- `.u.yaml`: `Label` may not have `visible`; no `description` property.
- Non-ASCII strings that reach R output are `\u{}`-escaped to survive `R CMD check`.
- Runtime dependencies (`gsDesign`, `ggsci`) go in `Imports`, not `Suggests`.
- Reset accumulating notice/HTML outputs at the top of `.run()`.

---

## 3. Component 1 — Waterfall enhancements (issue #1)

**Files:** `R/waterfall.b.R`, `jamovi/waterfall.a.yaml`, `jamovi/waterfall.u.yaml`, `jamovi/waterfall.r.yaml`, `NEWS.md`.

### 3.1 Conventional sort order (request 1)

**Current (incorrect):** `df[order(df$response, na.last = TRUE), ]` sorts ascending, so the *best* (most negative) response ends up on the **left** — opposite of the oncology convention.

**Design:**
- New option `sortDirection` (List): `conventional` (default) = worst/highest on left → best/lowest on right; `reverse` = the opposite.
- When `sortBy == "response"`:
  - `conventional` → `order(df$response, decreasing = TRUE, na.last = TRUE)`
  - `reverse` → `order(df$response, decreasing = FALSE, na.last = TRUE)`
- Apply in **both** sort paths: `.prepareWaterfallPlotData` (~line 304) and the second path (~line 2283).
- This is a deliberate default-behavior change; document under NEWS "Changed".

### 3.2 Y=0 baseline reference line (request 2)

- New option `showBaseline` (Bool, default `true`).
- New helper `.addBaseline(plot, show)` adding `geom_hline(yintercept = 0, color = "black", linewidth = 0.5)`. Drawn after bars so it reads as the baseline. RECIST threshold lines and median line remain independent.

### 3.3 Response annotation markers (request 3)

Two optional variables, both `default: NULL`, only affecting the **waterfall** plot (not spider):

- **`confirmationVar`** (`type: Variable`): categorical confirmed-status per patient. Rendered as a point/shape at the bar tip, shape mapped by factor **level** (`scale_shape_manual`, legend "Confirmation"). Mapping by level (not by a hard-coded "confirmed" string) keeps it robust to arbitrary labels; supports up to ~5 levels, extra levels fall back to a default shape with a note.
- **`ongoingVar`** (`type: Variable`): on-treatment / ongoing-response flag. Rendered as an upward arrow at the bar tip (`geom_segment` + `ggplot2::arrow()`) for "ongoing" patients. "Ongoing" determined by truthy coercion, documented in the option help:
  - logical → `TRUE`;
  - numeric → non-zero;
  - character/factor → matches `^(yes|y|true|on|ongoing|1)$` (case-insensitive).

**Data plumbing:** `.cleandata()` keeps only selected columns (known pitfall), so `confirmationVar`/`ongoingVar` must be explicitly added to the waterfall data selection and carried through `.optimizeForLargeDatasets` and the sort so row alignment is preserved. Markers are positioned using the same `x = factor(seq_len(nrow(df)))` index used by the bars, after sorting.

### 3.4 Wiring

- Add `sortDirection`, `showBaseline`, `confirmationVar`, `ongoingVar` to the waterfall image's `clearWith` in `.r.yaml` so the plot re-renders on change.
- Add UI controls in `.u.yaml` (VariableSupplier targets for the two variables; a "Display" box for the sort/baseline options).

---

## 4. Component 2 — Adverse-Events butterfly plot (`aeplot`)

**New files:** `R/aeplot.b.R`, `jamovi/aeplot.a.yaml`, `jamovi/aeplot.u.yaml`, `jamovi/aeplot.r.yaml`; registration in `jamovi/0000.yaml`; test data in `data-raw/aeplot_test_data.R` + `data/`.
**Menu:** `menuGroup: OncoPath`, `menuSubgroup: 'Patient Follow-Up Plots'`, title "Adverse Events Butterfly Plot".
**Dependency:** add `ggsci` to `Imports`.

### 4.1 Options (`.a.yaml`)

- `inputMode` (List): `patient` (default) / `summary`.

**Patient mode** (compute incidence internally):
- `subjectID` (`Variable`, `default: NULL`) — subject identifier; denominator for incidence.
- `aeTerm` (`Variable`, nominal) — Preferred Term.
- `armVar` (`Variable`, `default: NULL`) — treatment arm; if omitted → single-arm (one-sided plot).
- `gradeVar` (`Variable`, `default: NULL`) — severity grade (numeric/ordinal).
- `gradeThreshold` (Number, default 3) — grade ≥ threshold counts as "high grade".

**Summary mode** (pre-computed %, matches reference):
- `aeTermS` (`Variable`, nominal), `testAll`, `testHigh`, `controlAll` (`default: NULL`), `controlHigh` (`default: NULL`) — numeric percentages.

**Display:**
- `barShape` (List): `inside` (default, nested severity bars) / `outside` (stacked).
- `colorScheme` (List): `nejm` (default), `lancet`, `jama`, `jco`, `npg` (Nature), `aaas`, `colorblind`.
- `showValues` (Bool, default `false`) — data labels.
- `topN` (Integer, default 0 = all) — keep only the N most frequent terms (by test all-grade %).

### 4.2 Backend logic (`.b.R`)

- Reset notice outputs at top of `.run()`.
- **Patient mode:** for each (`aeTerm`, arm), incidence % = 100 × (distinct subjects with ≥1 event of that term) / (distinct subjects in arm). High-grade % uses events with `grade ≥ gradeThreshold`. Requires `subjectID`; if absent, fall back to raw event counts and emit an INFO notice that values are counts, not incidence.
- Assemble the butterfly frame (test arm on the negative x-side, control on positive), sort terms by test all-grade descending, apply `topN`.
- **Plot** via `ggplot2` + `coord_flip`, `geom_hline(0)`, absolute-value y labels, ggsci palette (test arm colored, control arm grey), nested vs stacked per `barShape`, optional `geom_text` labels. English axis/legend labels.

### 4.3 Outputs (`.r.yaml`)

- `plot` (Image) — the butterfly plot; `renderFun: .plot`; `clearWith` all options.
- `freqTable` (Table) — AE term, arm, all-grade %, high-grade %.
- `interpretation` (Html) — how to read the plot + input-mode notes.

---

## 5. Component 3 — Group-sequential design & sample size (`gsdesign`)

**New files:** `R/gsdesign.b.R`, `jamovi/gsdesign.a.yaml`, `jamovi/gsdesign.u.yaml`, `jamovi/gsdesign.r.yaml`; registration in `jamovi/0000.yaml`.
**Menu:** `menuGroup: OncoPath`, `menuSubgroup: 'Trial Design'`, title "Group-Sequential Design & Sample Size".
**Dependency:** add `gsDesign` to `Imports` (already installed).
**Data:** parameter-driven calculator — runs with no variables selected (a dataset is still attached by jamovi but ignored).

### 5.1 Options (`.a.yaml`)

**Common:**
- `endpoint` (List): `survival` (default) / `binary` / `continuous`.
- `sided` (List): `2` (default) / `1`.
- `alpha` (Number, default 0.05) — total type-I error (two-sided interpretation via `sided`).
- `power` (Number, default 0.9).
- `kMax` (Integer, default 2) — number of analyses including the final (so 1 interim).
- `sfu` (List): `OF` O'Brien-Fleming (default) / `Pocock` / `WT` Wang-Tsiatis / `HSD` Hwang-Shih-DeCani.
- `sfupar` (Number, default -4) — spending parameter for `WT`/`HSD` (ignored otherwise).
- `timing` (String, default empty = equally spaced) — comma-separated information fractions for interims.
- `testType` (List): `efficacy` (default, `test.type = 1`) / `efffut` (efficacy + non-binding futility, `test.type = 4`).

**Survival:** `hr` (0.7), `medianControl` (12, months), `accrualDuration` (12), `followupDuration` (18), `ratio` (1), `dropoutRate` (0.05, annual).
**Binary:** `p1` (0.4, control event rate), `p2` (0.25, treatment event rate).
**Continuous:** `deltaMean` (mean difference), `stdDev` (SD).

### 5.2 Backend logic (`.b.R`)

- Validate parameters (0<alpha<1, 0<power<1, kMax≥1, HR>0, rates in (0,1), etc.); emit actionable notices on bad input and stop cleanly.
- Build spending args (`sfu` → `gsDesign` spending function object; pass `sfupar` where relevant).
- **Alpha convention:** `gsDesign`/`gsSurv` take a **one-sided** alpha. Compute `alpha1 <- if (sided == 2) alpha/2 else alpha` and pass `alpha1` everywhere. Report both the one-sided and two-sided alpha in the summary so the user is not misled.
- **survival:** `gsDesign::gsSurv(k=kMax, test.type, alpha=alpha1, beta=1-power, sfu, timing, hr, lambdaC=log(2)/medianControl, eta=-log(1-dropoutRate)/12, T=accrualDuration+followupDuration, minfup=followupDuration, ratio)`.
- **binary:** `n.fix <- gsDesign::nBinomial(p1, p2, alpha=…, beta=1-power, ratio)`; `x <- gsDesign::gsDesign(k, test.type, alpha, beta, sfu, timing, n.fix=n.fix)`; per-look N = `x$n.I`.
- **continuous:** `d <- deltaMean/stdDev`; `n.fix` from two-sample formula (per-group), total via `ratio`; feed to `gsDesign::gsDesign(..., n.fix=n.fix)`.
- Populate boundary table row-per-analysis; populate summary HTML; render boundary plot.

### 5.3 Outputs (`.r.yaml`)

- `boundaryTable` (Table): analysis #, information fraction, N (and events, for survival), efficacy Z boundary, nominal p-value, effect-scale boundary (HR / proportion / mean diff), cumulative α spent.
- `summary` (Html): endpoint, design, max sample size, max events (survival), expected sample size under H1 (and H0 if futility), assumptions echoed back.
- `boundaryPlot` (Image): `plot(gsobj)` (gsDesign returns a ggplot) showing the boundaries across looks; `clearWith` all options.

---

## 6. Registration & sync

- Add `aeplot` and `gsdesign` analysis blocks to `jamovi/0000.yaml` (validate the YAML sequence to avoid the "packageInfo.analyses is not iterable" crash).
- Add `import(gsDesign)` / `importFrom(ggsci, …)` as needed via roxygen `@import`/`@importFrom` tags so `devtools::document()` writes `NAMESPACE`.
- Add `ggsci` and `gsDesign` to `DESCRIPTION` `Imports`.
- After implementation, run `Rscript _updateModules.R` to propagate OncoPath-tagged analyses into the submodule repo.

---

## 7. Verification plan

Per component, in order:

1. `Rscript -e "jmvtools::prepare()"` → **no errors** (compiles `.h.R` + `0000.yaml`). Guard against the VS Code `ELECTRON_RUN_AS_NODE` hijack (`Sys.unsetenv`).
2. `Rscript -e "devtools::document()"` → clean `NAMESPACE`/`.Rd`.
3. Parse-check each new `.b.R` (`parse()`), then `devtools::load_all()` for a runtime smoke test.
4. **Waterfall:** run on a dataset with `confirmation`/`ongoing` columns; verify (a) worst-on-left sort, (b) visible Y=0 line, (c) confirmation shapes + ongoing arrows appear and align to correct bars; verify `reverse` toggles direction.
5. **aeplot:** run patient mode (compute incidence) and summary mode (reference-style %); verify butterfly symmetry, sort, both `barShape` values, palette switching, `topN`, single-arm fallback.
6. **gsdesign:** for each endpoint, compare the boundary table to a hand-run `gsDesign`/`gsSurv` call in a console; verify boundary plot renders; verify bad inputs produce clean notices, not tracebacks.
7. `testthat` smoke tests: `tests/testthat/test-aeplot.R`, `test-gsdesign.R`, and waterfall assertions (sort direction, baseline present, markers drawn without error).

---

## 8. Risks & mitigations

| Risk | Mitigation |
|---|---|
| Changing waterfall default sort surprises existing users | Document in NEWS; keep `reverse` option; conventional is the academically-correct default the issue explicitly requested. |
| Marker columns dropped by `.cleandata()` | Explicitly add to selection and carry through sort/optimize (known pitfall, §3.3). |
| `plot(gsDesign obj)` API differences across versions | Wrap in `tryCatch`; fall back to a manual boundary ggplot if the built-in plot errors. |
| ggsci not in Imports → runtime failure for jamovi users | Add to `Imports` (memory: runtime deps must be Imports, not Suggests). |
| `0000.yaml` malformed while adding two analyses | Validate YAML with a parser before `prepare()` (memory: "analyses is not iterable"). |
| Continuous/binary effect-scale boundary math errors | Unit-check against `gsDesign` console output in verification step 6. |

---

## 8b. Attribution & licensing

Both threads originate from another author's work and **must be credited appropriately** in the shipped module.

### Provenance

- **Issue #1** — *"Better Waterfall plot suggestions"*, filed 2025-12-01 on `sbalci/OncoPath` by GitHub user **highwindmx**. Source of the three waterfall enhancements (Component 1). Link: https://github.com/sbalci/OncoPath/issues/1
- **`Jamovi-TrialPlots`** (package `TrialPlots`) by **highwind** (`highwindmx@126.com`), license **LGPL**. Source of the concepts for `aeplot` and `gsdesign` (Components 2–3). Link: https://github.com/highwindmx/Jamovi-TrialPlots

### Licensing

- ClinicoPath is **GPL-2**; the reference is **LGPL**. LGPL is compatible with (can be incorporated into) a GPL-2 work, so there is no license conflict.
- The two new analyses are **independent re-implementations of the ideas**, not copies of the reference source: different engine (`gsDesign` vs `gsDesign2`), jamovi-idiomatic patient-level input mode, English UI, ClinicoPath notice/HTML patterns. Ideas/features are not copyrightable; attribution is given as good practice and good faith regardless.

### Where attribution appears in the deliverable

1. **`jamovi/00refs.yaml`** — new reference entries:
   - `trialplots_highwind` (type `software`/`misc`): author `highwind`, title *Jamovi-TrialPlots: Plot data for clinical trials*, year 2025, url the repo.
   - `gsDesign_anderson` (the `gsDesign` R package), `ggsci_xiao` (the `ggsci` package).
   - Methodology: `obrien1979` (O'Brien & Fleming 1979), `pocock1977`, `hwang1990` (Hwang-Shih-DeCani spending), `lan1983` (Lan-DeMets alpha spending).
2. **Per-analysis `refs:` key** in `aeplot.a.yaml` and `gsdesign.a.yaml` listing the relevant refs above (renders in jamovi's References output).
3. **Backend file-header comment** in `R/aeplot.b.R` and `R/gsdesign.b.R`: a short block naming the inspiration (`Jamovi-TrialPlots` by highwind, LGPL) and the OncoPath issue where relevant.
4. **"About" / interpretation HTML** in each new analysis: one line — *"Inspired by the Jamovi-TrialPlots module by highwind (github.com/highwindmx/Jamovi-TrialPlots)."*
5. **`NEWS.md`** entries:
   - Waterfall: *"Enhanced waterfall plot (conventional sort, baseline line, confirmation/on-treatment markers) — thanks to @highwindmx (OncoPath issue #1)."*
   - New analyses: *"Added Adverse Events Butterfly Plot and Group-Sequential Design analyses, inspired by the Jamovi-TrialPlots module by highwind."*
6. **Design doc** (this file) header already links the issue and reference repo.

---

## 9. File-change summary

**Edited (main repo):**
- `R/waterfall.b.R`, `jamovi/waterfall.a.yaml`, `jamovi/waterfall.u.yaml`, `jamovi/waterfall.r.yaml`
- `jamovi/0000.yaml`, `jamovi/00refs.yaml` (attribution + methodology refs), `DESCRIPTION`, `NAMESPACE` (via document), `NEWS.md`

**New (main repo):**
- `R/aeplot.b.R`, `jamovi/aeplot.a.yaml`, `jamovi/aeplot.u.yaml`, `jamovi/aeplot.r.yaml`
- `R/gsdesign.b.R`, `jamovi/gsdesign.a.yaml`, `jamovi/gsdesign.u.yaml`, `jamovi/gsdesign.r.yaml`
- `data-raw/aeplot_test_data.R`, `data/aeplot_test_data.csv` (+ waterfall annotation test data)
- `tests/testthat/test-aeplot.R`, `tests/testthat/test-gsdesign.R`

**Propagated afterward:** `Rscript _updateModules.R` → OncoPath submodule.
