# Tier-3 Analysis Gaps — Implementation Summary

**Date:** 2026-07-11 · **Module version:** 0.0.47
Follows `analysis-gap-review-2026.md` and the Tier-1 / Tier-2 summaries. This completes
all nine recommended gaps.

Each analysis is a matched four-file jamovi set (`.a.yaml`, `.r.yaml`, `.u.yaml`, `.b.R`)
plus a `data-raw/` generator, a `data/*.csv` example, and a `tests/testthat/` test. All
backends and YAML parse; every option / result / UI-control name cross-checks.

> Note on prior coverage: `treatmenteffects.a.yaml` lists "E-values" as a
> sensitivity-analysis menu option, but no backend in the module actually computes an
> E-value, a g-formula standardization, or a chord diagram — all three were genuine gaps.

---

## 1. E-value for Unmeasured Confounding — `evalue`
**Placement:** menuGroup `meddecideD` → menuSubgroup *Study Design*
**Refs:** `vanderweele2017evalue`

The E-value (VanderWeele & Ding, 2017): minimum association a confounder would need with
both exposure and outcome to explain away an observed effect. Computes E-values for the
point estimate and the CI limit nearest the null.

| File | Notes |
|------|-------|
| `jamovi/evalue.{a,r,u}.yaml` | Options: effectType (RR/OR/HR/SMD), estimate, ci_lower, ci_upper, rare, trueValue, showPlot/showSummary/showExplanation. |
| `R/evalue.b.R` | `evalueClass`; OR→RR (sqrt for common outcome), HR→RR (VanderWeele), SMD→RR (Chinn OR = exp(1.81·d)); bias bounding-curve plot. |
| `data-raw/evalue_test_data.R` + `data/evalue_test_data.csv` | Illustrative estimate table. |
| `tests/testthat/test-evalue.R` | Canonical example, CI-crosses-null, OR conversion. |

**Validation:** reproduces the published RR = 3.9 example exactly — point E-value 7.26,
CI-limit E-value ≈ 3.0; protective RR = 0.80 gives 1.81 (CI 1.43); CI crossing the null
returns E-value = 1; OR/HR/SMD conversions correct.

---

## 2. G-computation (Parametric G-formula) — `gcomputation`
**Placement:** menuGroup `meddecideD` → menuSubgroup *Study Design*
**Refs:** `robins1986gformula`, `hernan2020whatif`

Marginal causal effect of a binary point treatment by standardization: fit an outcome
model on treatment + covariates, predict each subject's outcome under A=1 and A=0, average
over the covariate distribution to get E[Y¹] and E[Y⁰]. Percentile bootstrap CIs.

| File | Notes |
|------|-------|
| `jamovi/gcomputation.{a,r,u}.yaml` | Options: outcome, outcomeType (continuous/binary), outcomeEvent, treatment, treatmentLevel, covariates, interactions, bootstrap_n, conf_level, showCounterfactual/showPlot/showSummary/showExplanation. |
| `R/gcomputation.b.R` | `gcomputationClass`; linear or logistic outcome model, optional treatment×covariate interactions; reports mean/risk difference and (binary) risk ratio. |
| `data-raw/gcomputation_test_data.R` + `data/gcomputation_test_data.csv` | Confounded 800-subject cohort (stage/age confound treatment→death). |
| `tests/testthat/test-gcomputation.R` | Recovers adjusted effect below the confounded crude. |

**Validation:** on a continuous simulation with a true ATE = 2.0, g-computation gave 2.085
(95% CI [1.985, 2.205]) while the confounded crude estimate was 4.15; on a binary
simulation it recovered the true marginal RD = 0.179 and RR = 1.446 exactly (crude 0.324).

---

## 3. Circos Chord Diagram — `circos`
**Placement:** menuGroup `OncoPathD` → menuSubgroup *ClinicoPath Descriptives*
**Refs:** `gu2014circlize`

Circular chord diagram (via `circlize`) of flows / co-occurrence between categories:
state transitions, gene co-mutation, referral patterns. Input is an edge list
(from/to/value) or two cross-tabulated categoricals; directional or symmetric.

| File | Notes |
|------|-------|
| `jamovi/circos.{a,r,u}.yaml` | Options: inputMode (edges/crosstab), fromVar, toVar, valueVar, directional, symmetric, gridPalette, transparency, showLabels, showMatrix, showExplanation. |
| `R/circos.b.R` | `circosClass`; base-graphics `.plot` (draws + returns TRUE, no ggplot); runtime `addColumn` adjacency table. |
| `data-raw/circos_test_data.R` + `data/circos_test_data.csv` | Referral-flow edge list. |
| `tests/testthat/test-circos.R` | Edge-list and cross-tabulation modes. |

**Validation:** matrix building matches `table()` for cross-tabulation; symmetric mode
correctly combines both directions into the upper triangle; chord diagrams render without
error via `circlize` 0.4.18.

---

## Remaining build steps (run on a machine with jamovi + jmvtools)
```r
# install new runtime dep first (CRAN):
install.packages("circlize")     # added to DESCRIPTION Imports (evalue/gcomputation use base R)

jmvtools::prepare()   # generates .h.R for evalue, gcomputation, circos;
                      # registers all three in jamovi/0000.yaml
devtools::document()  # NAMESPACE exports + man/*.Rd
devtools::load_all()
for (a in c("evalue","gcomputation","circos"))
    testthat::test_file(sprintf("tests/testthat/test-%s.R", a))
```

New DESCRIPTION Import this round: `circlize`. (Tier-1 added `sandwich`, `lmtest`;
Tier-2 added `segmented`.)

## Status — all recommended gaps implemented
- **Tier-1** — winratio, fragilityindex, interruptedtimeseries.
- **Tier-2** — joinpoint, standardizedratio, door.
- **Tier-3** — evalue, gcomputation, circos (this document).
