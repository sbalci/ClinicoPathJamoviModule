# Clinical First-Wave — Implementation Summary

**Date:** 2026-07-11 · **Module version:** 0.0.47
Implements the recommended first wave from `clinical-needs-review-2026.md`: three new
specialty analyses and one methodological improvement to a released function. Each new
analysis is a matched four-file jamovi set (`.a.yaml` / `.r.yaml` / `.u.yaml` / `.b.R`) plus
a `data-raw/` generator, a `data/*.csv` example, and a `tests/testthat/` test. All backends
and YAML parse; every option / result / UI-control name cross-checks; each engine was
validated against a published or base-R oracle before the backend was written.

---

## B1 · Residual Cancer Burden (RCB) — `residualcancerburden` *(new)*
**Placement:** menuGroup `OncoPathD` → menuSubgroup *Response Evaluation*
**Refs:** `symmans2007rcb` · **Audience:** pathology + oncology

Computes the Symmans (2007) RCB index and class from six post-neoadjuvant pathology
variables (tumour-bed dimensions, cellularity, in-situ fraction, positive-node count, largest
nodal metastasis). Cohort mode (one row per patient) or single-case calculator; optional
survival linkage across RCB classes.

- **Formula:** f_inv = (1 − %CIS/100)·(%CA/100); d_prim = √(d1·d2);
  RCB = 1.4·(f_inv·d_prim)^0.17 + [4·(1 − 0.75^LN)·d_met]^0.17.
- **Classes:** RCB-0 (pCR) = 0; RCB-I ≤ 1.36; RCB-II 1.36–3.28; RCB-III > 3.28.
- **Validation:** MD Anderson worked example (d1=24, d2=18, %CA=10, %CIS=5, LN=3, d_met=4)
  → **RCB = 3.031, class RCB-II** (matches the published calculator). Boundary cases and a
  cohort log-rank linkage (χ² = 35.2, df = 3, p = 1.1e-07) behave correctly.
- Two `Output` write-backs: RCB index (continuous) and RCB class (nominal).

## B2 · Lymph Node Ratio — `lymphnoderatio` *(new)*
**Placement:** menuGroup `OncoPathD` → menuSubgroup *Response Evaluation*
**Audience:** pathology + oncology

LNR = positive / examined nodes, with nodal-yield adequacy against a minimum (default 12) and
survival by LNR strata. Strata use established thresholds (default 0.2 / 0.5) or a data-driven
optimal cutpoint that maximizes the log-rank statistic; Cox HRs are reported relative to the
lowest stratum.

- **Validation:** optimal cutpoint 0.271 (log-rank χ² = 22.5); 3-tier established thresholds
  separate survival (χ² = 13.9, df = 2, p = 9.6e-04); nodal-yield adequacy computed correctly.
- Two `Output` write-backs: LNR (continuous) and LNR stratum (nominal).

## B3 · Hematologic Prognostic Indices — `hematologicindices` *(new)*
**Placement:** menuGroup `OncoPathD` → menuSubgroup *ClinicoPath Biomarker Analysis*
**Refs:** `onodera1984pni`, `mcmillan2013gps` · **Audience:** oncology + general clinical

Derives NLR, PLR, LMR, SII, PNI, CAR, and the (modified) Glasgow Prognostic Score from
routine CBC + biochemistry, with a cohort summary, GPS distribution, and an optional
median/optimal survival split of a chosen index.

- **Formulas:** NLR = N/L; PLR = P/L; LMR = L/M; SII = P·N/L;
  PNI = 10·albumin(g/dL) + 0.005·lymphocytes(/µL); CAR = CRP/albumin.
  GPS/mGPS from CRP > 10 mg/L and albumin < 35 g/L (mGPS requires elevated CRP for any
  positive score).
- **Validation:** PNI(4.0 g/dL, 1600/µL) = 48.0; all four GPS/mGPS cases correct (incl. the
  key distinction that isolated hypoalbuminaemia scores 0 in mGPS but 1 in classic GPS);
  NLR = 4.0, PLR = 200, LMR = 3.0, SII = 1200, CAR = 0.429 on the worked case.
- Albumin unit selector (g/dL or g/L); indices selectable via `NMXList`.

## A2.1 · `waterfall` — censoring-aware duration-of-response *(improvement)*
**File touched:** `R/waterfall.b.R`, `jamovi/waterfall.{a,r,u}.yaml` · **Audience:** oncology

The waterfall backend already computed median time-to-response and a duration-of-response
figure, but summarized DoR with a **naive median that ignores censoring** — responders still
in response at last follow-up were treated as progressed, understating DoR.

- **Change:** added a censoring-aware **Kaplan–Meier median DoR** using the `duration_censored`
  indicator the backend already derived, plus a new option `showResponseDuration` and a
  dedicated **Time-to-Response & Duration-of-Response** table (TTR, naive DoR, KM DoR with
  event count and an explanatory note). Guarded with `requireNamespace("survival")` (already
  a dependency); no new dependency.
- **Validation:** in a 48%-censored simulation the naive median DoR (7.55) understated the
  Kaplan–Meier median (9.79) by 2.24 time units; with progression events present both the
  naive and KM medians resolve correctly, and the all-censored edge case yields NA (guarded,
  row omitted).

---

## Files added / changed

| Analysis | Type | New files |
|----------|------|-----------|
| `residualcancerburden` | new | a/r/u.yaml, b.R, data-raw + data/*.csv, test-*.R |
| `lymphnoderatio` | new | a/r/u.yaml, b.R, data-raw + data/*.csv, test-*.R |
| `hematologicindices` | new | a/r/u.yaml, b.R, data-raw + data/*.csv, test-*.R |
| `waterfall` | improved | edits to a/r/u.yaml + b.R (no new files) |

New refs in `jamovi/00refs.yaml`: `symmans2007rcb`, `onodera1984pni`, `mcmillan2013gps`.
No new DESCRIPTION Imports (all four use base R + `survival`, already a dependency).

## Remaining build steps (run on a machine with jamovi + jmvtools)
```r
jmvtools::prepare()   # generates .h.R for the three new analyses;
                      # regenerates waterfall.h.R with the new option;
                      # registers residualcancerburden / lymphnoderatio / hematologicindices in 0000.yaml
devtools::document()  # NAMESPACE exports + man/*.Rd
devtools::load_all()
for (a in c("residualcancerburden","lymphnoderatio","hematologicindices","waterfall"))
    testthat::test_file(sprintf("tests/testthat/test-%s.R", a))
```

## Status
- **Implemented this round:** B1 (RCB), B2 (LNR), B3 (hematologic indices), A2.1 (waterfall KM DoR).
- **Deferred** (from the review's later tiers): A3.1 crosstable SMD column, A1.1 ihcscoring
  cutpoint/reproducibility panel, A2.2 survival subgroup forest, B4 ctDNA/MRD, B5 tumor budding,
  B6 synoptic completeness, B7 multifocal concordance, and the remaining Part-A improvements.

---

## Second wave — Part-A improvements (this round)

Following the first wave, the highest-value **improvements to existing functions** were
addressed. Two turned out to be already covered on close inspection of the source.

| # | Item | Outcome |
|---|------|---------|
| A3.1 | `crosstable`: SMD balance-diagnostic column | **Implemented** |
| A2.3 | `stagemigration`: Win Ratio as first-class output | **Already present** — cross-link note added |
| A1.2 | `agreement`: per-category agreement | **Already present** (Per-Category Item-Modal Agreement) — deprioritized |

### A3.1 · `crosstable` — standardized mean differences *(improvement)*
**Files touched:** `R/crosstable.b.R`, `jamovi/crosstable.{a,r,u}.yaml`, `tests/testthat/test-crosstable.R`
**Audience:** general clinical

`crosstable` renders grouped "Table 1" through five style engines (tableone, finalfit,
gtsummary, arsenal, tangram). Weaving SMD into every engine would be fragile, so the SMD is
computed **independently from the raw data** and shown in a dedicated, engine-agnostic table
gated by a new `showSMD` option.

- **Engine:** continuous SMD = (m₁ − m₂) / √((s₁² + s₂²)/2); binary = the two-proportion form;
  categorical (>2 levels) = the Yang & Dalton (2012) multinomial SMD via `MASS::ginv`
  (already a dependency). Requires exactly two groups; each row is flagged negligible
  (|SMD| < 0.1) / small / notable.
- **Validation:** continuous SMD −0.49, binary ±0.08, 3-level categorical 0.175; the
  categorical path reproduces the binary magnitude exactly (0.0806), and the two-group guard
  yields an empty table with an explanatory note for 3+ groups.
- No new dependency; no change to any of the five existing render paths.

### A2.3 · `stagemigration` — Win Ratio cross-link *(minor)*
`stagemigration` (OncoPathT, Testing) already implements Win Ratio as a first-class feature
with its own option, controls, and result tables. Added a note on `winRatioOverview` pointing
to the standalone `winratio` for general two-group composites. No structural change.

### Notes on deferred Part-A items
- **A1.1 `ihcscoring` optimal cutpoint** — genuine gap (only fixed cutpoints exist today), but
  medium-effort: needs an outcome variable and a maxstat/Youden engine wired into the IHC
  workflow. Not a drop-in.
- **A2.2 `survival` subgroup forest** — the standalone `subgroupforest` and `groupedforest`
  analyses already deliver this capability; wiring a subgroup panel into the *released*
  `survival` is a larger change to production code and was left for a deliberate pass.

New DESCRIPTION Imports this round: none (SMD uses `MASS`, already a dependency).

### A1.1 · `ihcscoring` — outcome-linked optimal cutpoint *(improvement)*
**Files touched:** `R/ihcscoring.b.R`, `jamovi/ihcscoring.{a,r,u}.yaml`, `tests/testthat/test-ihcscoring.R`
**Audience:** pathology

The IHC scoring function had a "Cutpoint Optimization Plot" whose backend was a **placeholder
drawing a simulated ROC curve from `rnorm()`** — it optimized nothing. Replaced with a real,
outcome-linked optimal-cutpoint analysis, gated by a new `optimal_cutpoint` option so default
behavior is unchanged.

- **Engine:** for a **binary** outcome, Youden's index via `cutpointr` (cutpoint, sensitivity,
  specificity, AUC); for a **survival** outcome, the maximally-selected log-rank statistic via
  `maxstat` (cutpoint, max standardized statistic, confirmatory log-rank χ²/p). The score to
  dichotomize is user-selectable (H-score / Allred total / proportion).
- **Outputs:** an `optimalCutpointTable` and a **real** cutpoint plot — an ROC curve (binary)
  or Kaplan–Meier curves split at the optimal cutpoint (survival) — replacing the placeholder.
  Both carry a note that data-driven cutpoints are optimistic and need external validation.
- **Validation:** binary cut = 51 (Se 0.84, Sp 0.67, AUC 0.81); survival cut = 87 (log-rank
  χ² = 50.6, p < 1e-4); Allred-score path cut = 7.0 (AUC 0.79). Guards for < 10 complete
  observations and single-class outcomes.
- Both `cutpointr` and `maxstat` were already DESCRIPTION Imports (used by `optimalcutpoint`);
  no new dependency. The reproducibility-panel half of the review item is deferred.
