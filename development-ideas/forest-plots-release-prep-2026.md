# Forest-Plot Analyses — Release-Preparation & Bug Fixes

**Date:** 2026-07-11 · **Module version:** 0.0.47
Prepares the two standalone forest-plot analyses — **`subgroupforest`** and **`groupedforest`**
(both currently `SurvivalD` / Draft) — toward release quality by fixing methodological and
correctness bugs found on source review, wiring an orphaned output, and adding tests + demo data.

Each fix was validated by mirroring the corrected computation against a reference
(base-R model, empirical estimate, or published method) before editing the backend.

---

## `subgroupforest` — treatment effect across patient subgroups

| # | Issue found | Severity | Fix |
|---|-------------|----------|-----|
| 1 | **Risk-ratio "modified Poisson" was not modified** — used naive model-based Poisson CIs (which overstate binomial variance), and reported the **p-value from the *logistic* model even in the RR branch** (estimate/p mismatch). | High (wrong CIs & p) | New `.modifiedPoisson()` helper: Poisson working model with **robust HC0 sandwich SEs** (Zou 2004), Wald CI + p on the same scale. Applied in both subgroup and overall branches. |
| 2 | **Heterogeneity was computed then silently discarded** (`het_text` built, then a `# You can add this…` comment — never rendered). | Medium (missing output) | Added a `heterogeneity` HTML result item to `.r.yaml` and rendered Cochran's Q, I², and p, with a note distinguishing it from the formal interaction test. |
| 3 | **Heterogeneity log-transformed *all* estimates**, including continuous mean differences — `log()` of a ≤ 0 mean difference → `NaN`, wiping out the whole statistic. | Medium (crash on continuous) | `.calculateHeterogeneity()` is now **scale-aware**: log scale for ratio measures (HR/OR/RR), identity scale for mean differences; drops non-finite/zero-variance strata. |
| 4 | **Hardcoded 1.96** to recover SEs from CIs regardless of the chosen confidence level. | Low | Uses `qnorm(1 − (1 − conf)/2)` at the actual confidence level. |

**Validation:** modified-Poisson RR = 1.257 with robust SE 0.134 matches the empirical risk
ratio exactly; the old naive SE (0.177) was inflated. Scale-aware heterogeneity on continuous
mean differences including −2 computes Q = 48.0, I² = 95.8% cleanly (old code → NaN).
Added a demo dataset (`data-raw/subgroupforest_test_data.R` + `data/subgroupforest_test_data.csv`,
500-subject RCT-style cohort with a built-in treatment×biomarker interaction) — the analysis
had none — and tests for the RR, continuous, and heterogeneity paths.

## `groupedforest` — grouped Cox forest (treatment vs control per subgroup)

| # | Issue found | Severity | Fix |
|---|-------------|----------|-----|
| 1 | **Overall interaction significance used the *minimum* per-term p-value** across interaction terms — with a multi-level grouping variable this inflates the type-I error and is not a valid single test. | High (wrong inference) | Replaced with a **joint likelihood-ratio test** (interaction model vs additive model) reporting χ², df, and p; per-term rows retained as descriptive detail with a note. |
| 2 | Treatment coefficient located by **unanchored `grepl(treatment_var, term)`** — a covariate whose name contains the treatment-variable string (or regex metacharacters in the name) could match the wrong row. | Low–Medium | Exact prefix match: `startsWith(term, treatment_var)` excluding interaction terms (`:`), so only the treatment coefficient is selected. |

**Validation:** on a simulated 3-level grouping with a real interaction, the joint LR test gives
χ² = 19.46, df = 2, p = 5.9e-05 — a single correctly df-adjusted test — whereas the old min-p
approach reported p = 6.4e-04 (one of two component terms). `startsWith("trt")` isolates only
the `trtTx` coefficient, excluding `grpB`/`grpC`/interaction terms. Added a test for the LR-based
interaction output; the existing 19-test suite and 6 demo datasets are retained.

---

## Dependencies
No new DESCRIPTION Imports — `sandwich` (added earlier for interrupted-time-series), `broom`,
and `survival` are all already present.

## Remaining build steps (on a machine with jamovi + jmvtools)
```r
jmvtools::prepare()   # regenerates subgroupforest.h.R (new 'heterogeneity' result) and
                      # groupedforest.h.R; no new registration needed (both already in 0000.yaml)
devtools::document()
devtools::load_all()
for (a in c("subgroupforest","groupedforest"))
    testthat::test_file(sprintf("tests/testthat/test-%s.R", a))
# regenerate the .rda demo data:
source("data-raw/subgroupforest_test_data.R")
```

## Note on release status
Both remain `SurvivalD` (Draft). These fixes remove the correctness blockers; promotion to
released (drop the `D`) or Testing (`…T`) is the module author's call once the recompiled
versions are exercised in the jamovi UI.
