# psychopdaROC release checklist

Current statistical revision: **1.0.90**. The default single-AUC inference is logit
DeLong; independent-row legacy DeLong remains selectable. Patient mean/median mode
uses one summary per identified patient across all outputs. Holm p-values supplement
raw paired tests. Unsupported IDI/NRI significance tests and within-cohort meta-analysis
inference have been removed. See the [statistical repair report](../development-ideas/psychopdaROC-statistical-fixes-2026-10-10/report.md)
for current tests and the repeated original calibration design. Earlier results below
remain historical and do not imply native validation of this revision.


Historical source review: analysis **1.0.89**, module **1.0.81.01**. That release review
repairs decision-curve instability under large marker offsets and exposes logistic
fitting warnings/failures. See the [earlier release-review report](../development-ideas/psychopdaROC-release-review-2026-10-10/report.md)
for numerical evidence and the final test results. Generated metadata and the
intended-module copy still need updating; **clinical release is not signed off**.

Current evidence: the focused source suite passed 1,076 assertions with two existing
perfect-AUC pROC warnings. Final touched-path checks passed 383 assertions in existing
files and all 39 assertions in the four new regression blocks. The 20-check scoped
release gate and real-client UI harness pass; normal regeneration and native checks
on the regenerated revision remain pending.

The following evidence is historical for analysis **1.0.88**, checked 2026-10-10. The analysis remains in
`meddecideT` / JamoviTest. The three post-fix review defects and explicit positive/negative
class selection are repaired. **Clinical release is not signed off**; the statistical
limits below remain explicit.

## Executed checks for 1.0.88

| Check | Evidence and scope |
|---|---|
| Focused source suite | **1,040 assertions passed**, no failures/errors/skips, 263 blocks across 14 files. Four expected pROC/GLM warnings from the existing separated decision-curve fixture. |
| Final installed regression file | **141 assertions passed**, 10 blocks, no failures/errors/warnings/skips. Covers both selectors, exclusion parity across 13 optional tables, adversarial marker/outcome names, fallback direction, exact power targets, serialization, optional prose and cached-image invalidation. |
| Final presentation changes | The horizontal effect plot and native missing-cell serialization followed the full source suite. The final installed file and desktop/export checks cover these changes. |
| Preparation and documentation | Normal target `jmvtools::prepare()` succeeded with jamovi 28.3. Generated header and help were regenerated, including examples that now supply `negativeClass`. |
| Isolated package check | Zero errors, zero warnings, one NOTE for unused umbrella dependencies. An initial warning came from a dataset help file omitted from the temporary package; copying its existing help resolved it. Full umbrella, manuals and vignettes are outside this check. |
| Release gate | 20 scoped checks, zero blocking findings, one advisory for 13 unused shared references without URLs. All 16 references used by this analysis resolve. State and theme scanners pass. |
| Translation preparation | All **626 currently extracted target entries**, including **313 marked backend messages**, have nonempty Turkish translations; no target entry is fuzzy. Existing translations are preserved. `msgfmt`, placeholder parity and generated runtime JSON checks pass. Native Turkish layout is not verified. |
| Native jamovi 28.3 | The isolated `ROCRepairCheck` 1.0.88 module builds/installs. Both class selectors start empty, reject the same level, retain selections on reopening and exclude unselected/missing outcomes with counts. Four adversarial names produce all six distinct DeLong and effect-size comparisons. Optional guidance toggles, missing results and the horizontal plot render correctly. |
| Native save/export | The `.omv` saves/reopens and contains the corrected plot. Native PDF export succeeds: nine pages, six comparisons, explicit outcome exclusions, missing-result markers and the horizontal figure visually inspected. This is an isolated target probe, not a full intended-distribution deployment. |

The [current repair report](../development-ideas/psychopdaROC-explicit-classes-fixes-2026-10-10/report.md)
records the exact evidence and performance measurements. The [1.0.87 repair report](../development-ideas/psychopdaROC-full-check-fixes-2026-10-10/report.md)
and [post-fix review](../development-ideas/psychopdaROC-post-fix-review-2026-10-10/report.md)
remain historical evidence. See the [saved analysis](../development-ideas/psychopdaROC-explicit-classes-fixes-2026-10-10/native-explicit-classes.omv),
[native PDF](../development-ideas/psychopdaROC-explicit-classes-fixes-2026-10-10/native-explicit-classes.pdf),
and [translation plan](../i18n-plans/psychopdaROC-tr-translation-plan.md).

## Reproduce the focused tests

Run from the repository root:

```r
options(device = function(...) grDevices::pdf(tempfile(fileext = ".pdf"), ...))
devtools::load_all(quiet = TRUE)
testthat::test_dir("tests/testthat", filter = "psychopdaROC", reporter = "summary")
```

```sh
python3 development-ideas/psychopdaROC-release-check-2026-10-10/scoped-gate.py
Rscript --vanilla development-ideas/psychopdaROC-explicit-classes-fixes-2026-10-10/run-regressions.R
```

The installed-runtime harness uses a temporary library path recorded in its source. Regenerate and install the isolated target before running it; do not infer installed behavior from `load_all()`.

## Synthetic release data

Run `Rscript --vanilla data-raw/psychopdaROC_release_data.R` to regenerate [data/psychopdaROC_release.csv](../data/psychopdaROC_release.csv). A matching compressed `data/psychopdaROC_release.rda` supports `data(psychopdaROC_release)` without changing names or delimiters. The CSV byte-for-byte reproduction check passes. The fixture contains 240 independent synthetic patients (80 positive, 160 negative), correlated markers, six missing candidate-marker values, affine/inverse controls and names containing punctuation and Unicode.

Use `outcome` as the class variable, explicitly select `Positive` and `Negative` in their respective class selectors, and start with `marker_a` and `marker_b`. Select diagnostic plots, comparisons and intervals individually; checkboxes start unchecked. Historical fixtures in `data-raw/non-rda/` remain source archives.

## Statistical scope and remaining release evidence

- The supported scope is exploratory analysis of independent rows. The prior 1.0.85 audit
  used 1,000 replicates per regime: nominal 95% interval coverage was 90.3% for 10 positive/
  50 negative independent rows and 68.5% for repeated-patient rows; repeated-patient
  single/paired type I errors were 34.7%/31.9%. These four failures remain failures.
  Their fixed three-Monte-Carlo-SE tolerance remains 0.0206761. No new calibration
  simulation or changed acceptance criterion is claimed by this repair.
- Clustered inference and general IDI/NRI calibration are not supplied. Selected cutpoints
  report apparent performance and require validation appropriate to their intended use.
- Normal target preparation, native save/reopen and full-results PDF export now pass.
  Complete intended-module distribution, broader native transitions, light/dark themes,
  resizing, Turkish layout and direct individual-image export remain release-level checks.
  The current saved `.omv` contains the correct PNG, extracted without modification;
  the native PDF embeds that figure. Earlier individual-image export attempts stalled.

The [independent-review repair report](../development-ideas/psychopdaROC-review-fixes-2026-10-10/report.md)
retains the numerical matrix and unchanged simulation criteria. Scope disclosure does
not turn failed calibration checks into passes. No production promotion is part of these repairs.
