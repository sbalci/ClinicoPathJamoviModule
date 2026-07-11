# ClinicoPath Jamovi Module — Analysis Gap Review

**Date:** 2026-07-10 · **Module version:** 0.0.47 · **Analyses in catalog:** 370 (`jamovi/*.a.yaml` + `R/*.b.R`)

> **Update 2026-07-10:** All three Tier-1 gaps have been implemented — `winratio`,
> `fragilityindex`, and `interruptedtimeseries` — see `tier1-gaps-implementation-summary.md`.
> All three Tier-2 gaps have now also been implemented — `joinpoint` (Joinpoint/APC trend),
> `standardizedratio` (SIR/SMR), and `door` (Desirability of Outcome Ranking) — see
> `tier2-gaps-implementation-summary.md`. All three Tier-3 gaps have now also been
> implemented — `evalue` (E-value sensitivity analysis), `gcomputation` (parametric
> g-formula), and `circos` (chord diagram) — see `tier3-gaps-implementation-summary.md`.
> All nine recommended gaps (Tier-1 through Tier-3) are now implemented.

## Method

Enumerated all 370 registered analyses and their menu groups/subgroups, then cross-checked a
list of modern clinicopathology / diagnostic / trial candidate methods against (a) analysis
names and titles, (b) `.b.R` backend text, and (c) the existing `TODO.md` roadmap — to
separate genuine gaps from methods already implemented or embedded inside another analysis.

**Already covered (do NOT duplicate):** method comparison incl. Bland–Altman, Passing–Bablok,
Deming (`methodcomparison`); causal inference incl. propensity score, IPTW, matching,
doubly-robust, covariate balance (`treatmenteffects`); trend tests incl. Cochran–Armitage
(`conttables`, `desctools`); landmark analysis (`landmarkanalysis` + 14 others); decision-curve
/ net-benefit (`decisioncurve`, `bayesdca`, `timedependentdca`); NRI/IDI (`idi`,
`netreclassification`, `reclassmetrics`); calibration (`survivalcalibration`, `modelbuilder`).

---

## Tier 1 — highest value, genuine gaps, strong fit

### 1. Win Ratio & hierarchical composite endpoints
- **Status:** only referenced inside `stagemigration`; no standalone endpoint analysis.
- **What:** Finkelstein–Schoenfeld / Pocock win ratio, win odds, and net benefit for
  prioritized (hierarchically ordered) composite outcomes — e.g. death > progression >
  biomarker worsening.
- **Why:** increasingly requested/required in oncology and cardiology trials; no equivalent
  exists in jamovi. Fits the survival + oncology audience directly.
- **Implementation:** R package `WINS` or `WR`; inputs are one row per subject with an ordered
  set of outcome/time columns + treatment arm.

### 2. Fragility Index & Fragility Quotient
- **Status:** absent (`fragility` → 0 files).
- **What:** minimum number of event/non-event reversals that would flip a significant 2×2 trial
  result to non-significance; fragility quotient normalizes by sample size.
- **Why:** pairs naturally with the module's existing critical-appraisal tooling
  (`retracted`, scholar/bibliometrics). High teaching value, small implementation surface.
- **Implementation:** `fragility` R package, or a compact self-contained loop over Fisher tests.
  Inputs: a 2×2 table (events/n per arm).

### 3. Interrupted Time Series (segmented regression)
- **Status:** absent (`interrupted time series` / `segmented regression` → 0 files).
- **What:** level- and slope-change estimation around an intervention point, with
  Newey–West / AR-adjusted errors and a fitted counterfactual.
- **Why:** the standard design for lab quality-improvement, policy, and turnaround-time
  interventions — a hospital/pathology-lab use case not covered by the existing time-series or
  control-chart tools (`labcontrolcharts` is SPC, not causal ITS).
- **Implementation:** `nlme::gls` / `sandwich` + `segmented`. Inputs: time, outcome,
  intervention indicator (± seasonal terms).

---

## Tier 2 — good fit, fills a partial gap

### 4. Joinpoint / Age–Period–Cohort trend analysis
- **Status:** only fragmentary inside `epidemiosurvival`; no dedicated trend analysis.
- **What:** joinpoint regression of incidence/mortality trends with annual percent change (APC)
  and average APC; optional APC decomposition.
- **Why:** the canonical cancer-registry epidemiology output; complements the survival suite.
- **Implementation:** `segmented` for joinpoints; `apc`/`Epi` for age–period–cohort.

### 5. Standardized Incidence / Mortality Ratio (SIR / SMR)
- **Status:** referenced only inside `survival` / `epidemiosurvival`; no dedicated tool.
- **What:** indirect standardization giving SIR/SMR with exact Poisson CIs against a reference
  population — observed vs expected events by strata.
- **Why:** core registry / occupational-cohort analysis; small, well-defined.
- **Implementation:** exact Poisson (`epitools`, `popEpi`). Inputs: observed counts,
  person-time, reference rates by stratum.

### 6. DOOR — Desirability of Outcome Ranking
- **Status:** absent.
- **What:** ordinal composite that ranks each patient by overall desirability
  (efficacy + safety combined), reporting the probability a random treated patient has a more
  desirable outcome than a control patient.
- **Why:** growing use in antimicrobial and oncology benefit–risk analysis; niche but distinctive.

---

## Tier 3 — worth considering
- **E-value** (VanderWeele) — sensitivity of an association to unmeasured confounding; no
  dedicated analysis (grep hits were false positives on the word "value"). Small, fits the
  causal-inference and critical-appraisal theme.
- **Target-trial emulation helper / g-computation** — extends `treatmenteffects` (currently
  propensity/IPTW/matching only).
- **Circos / circular genome plot** — genomic visualization beyond `jjoncoplot`'s oncoprint.

---

## Recommendation
Start with **Win Ratio**, **Fragility Index**, and **Interrupted Time Series** — each is a
genuine gap (verified against all 370 analyses), each has a clean jamovi data shape, each maps
onto an existing audience (trials, critical appraisal, lab QI), and none duplicates or
fragments an existing analysis.
