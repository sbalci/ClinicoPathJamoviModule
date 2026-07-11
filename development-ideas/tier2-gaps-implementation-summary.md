# Tier-2 Analysis Gaps — Implementation Summary

**Date:** 2026-07-10 · **Module version:** 0.0.47
Follows `analysis-gap-review-2026.md` (Tier-2 recommendations) and
`tier1-gaps-implementation-summary.md`.

Three new analyses, each a matched four-file jamovi set (`.a.yaml` options, `.r.yaml`
results, `.u.yaml` UI, `.b.R` backend) plus a `data-raw/` generator, a `data/*.csv`
example, and a `tests/testthat/` test. All backends and YAML parse cleanly, and every
option / result / UI-control name cross-checks.

---

## 1. Joinpoint Trend Analysis — `joinpoint`
**Placement:** menuGroup `SurvivalD` → menuSubgroup *Specialized Survival Methods*
**Refs:** `kim2000joinpoint`

Segmented log-linear regression of a rate over time. The number of joinpoints (up to a
user maximum) is selected by BIC via the `segmented` package (`selgmented`); each segment
is summarized by its **Annual Percent Change** APC = 100·(exp(slope)−1), and the whole
period by a duration-weighted **Average Annual Percent Change (AAPC)**. Trend plot with
joinpoint markers, optional log-scale y-axis.

| File | Notes |
|------|-------|
| `jamovi/joinpoint.{a,r,u}.yaml` | Options: time, rate, maxJoinpoints (0–5), conf_level, showSegments/showAAPC/showPlot/logScale/showSummary/showExplanation. |
| `R/joinpoint.b.R` | `joinpointClass`; robust to 0-joinpoint selection (falls back to single OLS slope). |
| `data-raw/joinpoint_test_data.R` + `data/joinpoint_test_data.csv` | 2000–2020 incidence, trend change ~2010. |
| `tests/testthat/test-joinpoint.R` | Change-point detection + no-change-point case. |

**Validation (R 4.5.3, `segmented`):** on a series with a true change at 2010 (APC
−1.98% then +3.05%), recovered a joinpoint at 2010.5 with segment APCs −1.63% / +3.19%
(both p<0.001) and AAPC 0.63%; a flat series correctly returned 0 joinpoints (APC −2.04%,
true −1.98%).

> Note: uses BIC selection (segmented) rather than the NCI Joinpoint permutation test, so
> the selected number of joinpoints may differ for borderline cases. Documented in the
> analysis' Methodology output.

---

## 2. Standardized Incidence / Mortality Ratio — `standardizedratio`
**Placement:** menuGroup `SurvivalD` → menuSubgroup *Specialized Survival Methods*
**Refs:** `breslowday1987`

Indirect standardization: expected events = reference rate × person-time (or supplied
directly), SIR/SMR = ΣO / ΣE, with **exact Poisson** confidence intervals and a test
against 1 (via `stats::poisson.test`). Per-stratum table and a forest plot.

| File | Notes |
|------|-------|
| `jamovi/standardizedratio.{a,r,u}.yaml` | Options: inputMode (rate\|expected), observed, personTime, refRate, expected, stratum, ratioType (sir\|smr), conf_level, perStratum/showPlot/showSummary/showExplanation. |
| `R/standardizedratio.b.R` | `standardizedratioClass`. |
| `data-raw/standardizedratio_test_data.R` + `data/standardizedratio_test_data.csv` | Age-stratified cohort. |
| `tests/testthat/test-standardizedratio.R` | Rate×person-time and expected-column inputs. |

**Validation:** SIR = 1.500, 95% CI [1.094, 2.007], p = 0.0101 for O=45, E=30 — matches
`stats::poisson.test` exactly; stratified sums reproduce the same overall ratio.

---

## 3. Desirability of Outcome Ranking — `door`
**Placement:** menuGroup `meddecideD` → menuSubgroup *Study Design*
**Refs:** `evans2015door`

Two-group comparison on an ordinal DOOR outcome. The **DOOR probability** = P(random
index patient more desirable than random reference patient) + ½·P(tie) — the
Mann-Whitney / AUC estimand — with a placement-based (DeLong-type) confidence interval.
Handles either rank direction; distribution table and stacked-bar plot by group.

| File | Notes |
|------|-------|
| `jamovi/door.{a,r,u}.yaml` | Options: group, refLevel (Level), doorRank, rankDirection (lower\|higher), conf_level, showDistribution/showPlot/showSummary/showExplanation. |
| `R/door.b.R` | `doorClass`. |
| `data-raw/door_test_data.R` + `data/door_test_data.csv` | 4-level DOOR (alive-no-event → dead) by arm. |
| `tests/testthat/test-door.R` | DOOR probability equals Mann-Whitney AUC. |

**Validation:** DOOR probability = 0.734 matches the Mann-Whitney AUC exactly; direction
handling is symmetric (lower-better vs higher-better on flipped data give identical
results).

---

## Remaining build steps (run on a machine with jamovi + jmvtools)
```r
# install new runtime deps first (all on CRAN):
#   segmented        (joinpoint)   -- added to DESCRIPTION Imports
install.packages("segmented")

jmvtools::prepare()   # generates .h.R for joinpoint, standardizedratio, door;
                      # registers all three in jamovi/0000.yaml
devtools::document()  # NAMESPACE exports + man/*.Rd
devtools::load_all()
for (a in c("joinpoint","standardizedratio","door"))
    testthat::test_file(sprintf("tests/testthat/test-%s.R", a))
```

New DESCRIPTION Import added this round: `segmented`. (Tier-1 round added `sandwich`,
`lmtest`.)

## Status
- **Tier-1** (winratio, fragilityindex, interruptedtimeseries) — implemented.
- **Tier-2** (joinpoint, standardizedratio, door) — implemented (this document).
- **Tier-3** (E-value, g-computation / target-trial emulation, Circos) — still open.
