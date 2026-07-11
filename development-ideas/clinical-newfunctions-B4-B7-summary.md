# Clinical New Functions B4–B7 — Implementation Summary

**Date:** 2026-07-11 · **Module version:** 0.0.47
Completes the four remaining new-function candidates from `clinical-needs-review-2026.md`.
Each is a genuine gap (verified absent from the catalog), placed under `OncoPathD` (Draft),
built as a full 4-file jamovi analysis + generator + demo CSV + tests, and oracle-validated
before writing the backend. **No new package dependencies** — all use `survival`/`stats`,
already imported.

| # | Analysis | menuSubgroup | What it does | Validation |
|---|----------|--------------|--------------|-----------|
| B4 | **ctdnadynamics** — ctDNA / MRD Dynamics | ClinicoPath Biomarker Analysis | Paired baseline/follow-up VAF → clearance vs persistence (follow-up ≤ threshold = cleared/MRD-neg), log₂ fold-change, clearance rate, and **landmark survival by MRD status** (log-rank + Cox HR). | Clearance classification; median log₂FC cleared −9.49 vs persistent −0.27; MRD⁺ HR = 3.48 (log-rank p = 1.9e-04). |
| B5 | **tumorbudding** — Tumor Budding (ITBCC) | IHC Analysis | ITBCC 2016 grade from hotspot bud count: **Bd1** 0–4, **Bd2** 5–9, **Bd3** ≥10; area-normalized to the standard 0.785 mm² field; hotspot = densest field per case; optional survival by grade. | Grade cutoffs correct at all boundaries; 12 buds/1.0 mm² and 6/0.5 mm² both → 9.4 → Bd2; hotspot = max field; cohort log-rank across grades p = 4.1e-04. |
| B6 | **synopticcompleteness** — Synoptic Report Completeness | ClinicoPath Data Quality | Audits structured-report completeness: per-report, per-element (worst-first), and by subspecialty/pathologist, with an optional completeness **trend over time**. | Overall 84.6%, 34% fully complete on the validation set; trend +0.82%/month (p = 2.8e-04), recovering the built-in improvement. |
| B7 | **multifocalconcordance** — Multifocal / Primary-Metastasis Concordance | ClinicoPath Biomarker Analysis | Concordance of biomarkers/mutations across foci or paired primary/met: per-marker concordance rate, **Cohen's κ** for paired designs, case-level clonality (fully/partially/discordant), directional discordance. | Recovers built-in rates: HER2 92%→94% (κ=0.82), ER 80%→81% (κ=0.49), PDL1 68%→64% (κ=0.90). |

## Design notes
- **B4** uses a **landmark** framing for the survival linkage (MRD status defined at a fixed
  assessment timepoint), which avoids the immortal-time bias of treating clearance as a
  time-zero property; a `requireNamespace("survival")` guard degrades gracefully.
- **B5** separates raw count, area normalization, and hotspot selection so a case with several
  fields is graded on its densest field; the ITBCC cutoffs are applied to the normalized count.
- **B6** pairs naturally with the earlier `interruptedtimeseries` analysis (cross-referenced in
  the output) for a formal pre/post evaluation of a reporting-template rollout.
- **B7** works for both paired (primary vs metastasis; adds Cohen's κ) and multi-focus (≥2 foci;
  concordance = all foci identical) designs, detected automatically from the foci-per-case counts.

## Each analysis ships
`jamovi/<name>.{a,r,u}.yaml` + `R/<name>.b.R` + `data-raw/<name>_test_data.R` +
`data/<name>_test_data.csv` + `tests/testthat/test-<name>.R`. All files parse; all cross-checks
clean (backend options/results ⊆ YAML); no 0000.yaml name collisions.

## Build steps (on a machine with jamovi + jmvtools)
```r
jmvtools::prepare()    # generates the four .h.R and registers each in jamovi/0000.yaml
devtools::document()   # NAMESPACE exports + .Rd
devtools::load_all()
for (a in c("ctdnadynamics","tumorbudding","synopticcompleteness","multifocalconcordance"))
    testthat::test_file(sprintf("tests/testthat/test-%s.R", a))
# regenerate .rda demo data (usethis):
for (a in c("ctdnadynamics","tumorbudding","synopticcompleteness","multifocalconcordance"))
    source(sprintf("data-raw/%s_test_data.R", a))
```

## Review status
With B4–B7 done, **every item in `clinical-needs-review-2026.md` is now resolved** — implemented,
already-covered, or explicitly deferred (A1.2 agreement level-collapse; A3.3 reference-interval
cross-linking; the A1.1 reproducibility-panel half). See that file's summary table for the
per-item disposition.
