# Release review: `lassocox`

Date: 2026-09-01. Analysis version: **0.0.6**. Review scope: the
`lassocox` analysis and its tests, examples, datasets, and user documentation.

## 1. Overall verdict

The current estimator is mathematically consistent with its labels and is suitable
for **exploratory survival-model development**. Event coding is explicit, categorical
predictors are encoded transparently, the selected coefficients and scores come from
the same penalized path, and the displayed C-index is correctly labeled as apparent
development performance. Clinical language now avoids significance, causal, stability,
and utility claims that the analysis cannot support. The function is ready after the
minor repository-level release actions in section 8; it is not a validated clinical
prediction tool.

## 2. Findings

### Critical

No unresolved critical findings.

### Major

- **Fixed — shipped examples could not be called through the exported wrapper.** All
  five calls omitted the required `censorLevel` argument. Each now states both endpoint
  levels explicitly ([`inst/examples/lassocox_example.R`](../inst/examples/lassocox_example.R#L20)).
- **Fixed — the bundled breast-cancer RDA exposed the wrong object name.**
  `data/lassocox_breast_cancer.rda` contained `breast_cancer_data`, so
  `data(lassocox_breast_cancer)` did not create the documented object. The observations
  are unchanged; only the stored object name is corrected. A regression test verifies
  the object and endpoint levels
  ([`tests/testthat/test-lassocox-release-fixes.R`](../tests/testthat/test-lassocox-release-fixes.R#L64)).
- **Fixed — documentation described outputs that no longer exist.** The comprehensive
  guide claimed post-selection confidence intervals and p-values, risk-group hazard
  ratios and log-rank p-values, and threshold-based clinical utility. It now documents
  the penalized coefficient columns and the single apparent C-index actually reported
  ([`vignettes/jsurvival-lassocox-comprehensive.Rmd`](../vignettes/jsurvival-lassocox-comprehensive.Rmd#L445)).
- **Fixed — two stress fixtures were presented as fitted-model examples.** The lung and
  small-cohort fixtures deliberately contain follow-up rounded to zero; the lung fixture
  also has only two censored observations. Their examples now explicitly demonstrate
  safe input rejection rather than implying that model and comparison output will be
  produced ([`inst/examples/lassocox_example.R`](../inst/examples/lassocox_example.R#L42)).

### Moderate

- **Fixed — tuning and validation claims were too strong.** The guide no longer says
  `lambda.1se` guarantees better generalization or treats tuning cross-validation as
  validation of the final pipeline. It calls for bootstrap optimism correction or
  nested cross-validation that repeats preprocessing and tuning, followed by external
  validation where feasible
  ([`vignettes/jsurvival-lassocox-comprehensive.Rmd`](../vignettes/jsurvival-lassocox-comprehensive.Rmd#L476)).
- **Fixed — path inclusion was described as stability.** Documentation and examples now
  identify it as the fraction of the fitted lambda path with a nonzero coefficient, not
  a bootstrap selection frequency
  ([`inst/examples/lassocox_example.R`](../inst/examples/lassocox_example.R#L156)).
- **Fixed — the test guide was stale.** It now includes `censorLevel`, `path_plot`,
  encoding, reproducibility, and R-code output; it also covers empty models, complete-case
  exclusions, and the encoded-column rule for a single multi-level factor
  ([`vignettes/testing_lassocox.md`](../vignettes/testing_lassocox.md#L18)).

### Minor

- **Fixed — compatibility pages reported version 0.0.5.** Both now point to the canonical
  safety guide and identify analysis version 0.0.6
  ([`vignettes/lassocox-documentation.md`](../vignettes/lassocox-documentation.md#L1)).

## 3. Changes made

- Corrected the stored object name in `data/lassocox_breast_cancer.rda` without
  changing any row, column, value, class, or factor level.
- Repaired every shipped R example and all 12 comprehensive-guide calls to supply
  explicit event and censor levels.
- Rewrote stale output and clinical interpretation sections to match the penalized
  estimator, apparent performance, empty-model behavior, and validation scope.
- Added release regressions for the documented dataset name and endpoint-level arguments.
- Preserved the earlier 0.0.6 implementation repairs: validated Cox refits, safe
  proportional-hazards diagnostics, encoded-column sufficiency, exact lambda-rule
  reporting, empty-model preservation, deterministic stratified folds, and safe
  explanatory output.

## 4. Statistical verification

An independent 140-observation dataset with 98 events, two numeric predictors, and
one ordered factor was encoded and fitted separately with `glmnet` and `survival`.
The backend and independent calculation agreed exactly:

| Quantity | Backend | Independent calculation | Absolute difference |
|---|---:|---:|---:|
| Encoded design | — | — | 0 maximum element difference |
| Stratified fold assignment | — | — | Identical |
| Selected lambda | 0.0454745939 | 0.0454745939 | 0 |
| Penalized coefficients | — | — | 0 maximum difference |
| Linear predictor | — | — | 0 maximum difference |
| Harrell C-index | 0.7060044272 | 0.7060044272 | 0 |
| Penalized hazard ratios | `exp(beta)` | `exp(beta)` | 0 maximum difference |

The fit retained three of four encoded columns. The verification also confirmed
that the backend uses `reverse = TRUE` for a higher linear predictor to mean higher
hazard ([`R/lassocox.b.R`](../R/lassocox.b.R#L806)), and that coefficients, scores,
and lambda all come from the same `cv.glmnet` path
([`R/lassocox.b.R`](../R/lassocox.b.R#L677)).

## 5. Data-flow audit

| UI/options group | Backend path | Results |
|---|---|---|
| `elapsedtime`, `outcome`, `outcomeLevel`, `censorLevel`, `explanatory` | `.cleanData()` validates explicit endpoint levels, performs complete-case filtering, and creates the design matrix | notices, model summary, coefficient/performance tables, all downstream outputs |
| `lambda`, `nfolds`, `standardize`, `random_seed` | `.makeStratifiedFoldId()` and `.fitModel()`; `alpha = 1`, Cox family, Breslow ties | selected rule/lambda, coefficients, CV plot, path plot, reproducibility table and R code |
| `suitabilityCheck` | `.assessSuitability()` | advisory suitability HTML |
| `cv_plot`, `coef_plot`, `survival_plot`, `path_plot` | plain serializable state plus `.cvPlot()`, `.coefPlot()`, `.survivalPlot()`, `.pathPlot()` | four images |
| `riskScore` | result-output enable state gates score export; excluded rows receive `NA` | aligned output column |
| `showEncoding`, `showReproducibility`, `showRCode` | encoding/provenance/code population methods | encoding table, reproducibility table, R code HTML |
| `showSummary`, `showExplanations`, `showMethodologyNotes`, `includeClinicalGuidance`, `showVariableImportance`, `showModelComparison` | corresponding population methods | optional narrative, guidance, descriptive and comparison outputs |

All 25 declared options, including `data` and the Output option, have a UI or runtime
consumer. All 22 declared result items are populated or rendered; there are no unknown
backend result references, invalid `clearWith` names, or missing render methods.

Fixed backend behavior without a separate control is deliberate and documented: binary
event/censor mapping, complete-case analysis, treatment coding with the first factor level
as reference, separate penalization of factor indicators, removal of unusable constant
columns, `alpha = 1`, Breslow tie handling, an apparent C-index, and a descriptive median
risk split. No unused option or unexplained hidden statistical branch was found.

## 6. Test results

- Eight `test-lassocox*.R` files passed with zero failures, errors, or skips. The
  previous 442 assertions in 91 blocks plus two new blocks give **447 assertions in
  93 blocks**. One environment warning reports that `survival` was built under a newer
  R version; it does not affect results.
- The result-rendering contract passed **6 assertions in 4 blocks**.
- Five shipped examples executed: three fitted workflows populated model summaries,
  and two documented stress-fixture workflows produced the expected strictly-positive-
  time validation notice.
- The comprehensive R Markdown guide was extracted and parsed: all **12 of 12**
  `lassocox()` calls contain both endpoint-level arguments.
- UI harness: `placeholder present = false`, title correct, `errors = undefined`.
- State-guard scan: 0 unguarded image-state reads. Duplicate UI-name scan: clean.
  Dark-theme HTML scan: 0 findings. Named-entity and imperative-visibility scans: clean.
- R/YAML parsing and the scoped `git diff --check` passed. Canonical analysis/file/class
  casing passed; no tracked `.tar.gz` or `.jmo` artifacts were found.
- The eight reference keys resolve with title, author, and URL metadata; 322 active
  LASSO messages have Turkish translations and no stale LASSO reference remains.

## 7. Remaining limitations

- Performance is apparent. The lambda cross-validation tunes the penalty but does not
  validate the final preprocessing/selection/fitting pipeline.
- The development-sample median risk groups are descriptive. They do not define a
  validated clinical cutoff and report no inferential comparison.
- Coefficients are penalized estimates. The analysis intentionally does not report
  post-selection p-values or confidence intervals.
- Factor indicators are selected separately. Use a grouped penalty when whole-factor
  selection is required, and consider elastic net when correlated columns should tend
  to remain together.
- The analysis does not supply calibration, time-dependent prediction accuracy,
  optimism correction, decision-curve analysis, or frozen-model external prediction.
- A complete source-tree `devtools::load_all()` could not be run in this environment
  because the unrelated package dependency `eurostat` is absent. The LASSO suite was
  run against the isolated installed release candidate. A full package check remains
  an integration gate.
- The working tree contains unrelated TableOne work, including a Collate entry whose
  source file is currently untracked. That repository-wide condition must be resolved
  before packaging, but it is outside this function review.

## 8. Release recommendation

**Ready after specified minor actions.** Before packaging:

1. Move `menuGroup: SurvivalT` back to the intended production route after review.
2. Resolve the unrelated package Collate/untracked-file condition and run the full
   package check in an environment with all Imports available.
3. Keep the user-facing scope explicit: exploratory model development, followed by
   full-process internal validation and external validation before clinical use.

No additional `jmvtools::prepare()` or `devtools::document()` run is required for the
release-review edits in this pass because they changed no YAML or roxygen source.
