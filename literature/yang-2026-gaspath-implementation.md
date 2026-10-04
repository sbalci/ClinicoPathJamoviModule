# GASPath validation roadmap implementation

Implemented 2026-10-04 following the [article review](yang-2026-gaspath-citation-review.md)
and [baseline function audit](yang-2026-gaspath-function-audit.md). The review and audit describe
the pre-change repository; this document records the resulting implementation.

## Changes delivered

| Roadmap area | Implementation |
|---|---|
| PR numerical correctness | Shared tie-aware `R/utils-diagnostic-validation.R` helper used by `enhancedROC`, `psychopdaROC`, and `precisionrecall`; recall-zero endpoint, distinct trapezoidal area and average precision, finite-score filtering and explicit single-class behavior. A perfect classifier now has area 1 in psychopdaROC. |
| Clinical impact | `enhancedROC` separates fixed raw-score `impactCutoffs`, population prevalence, and `utilityThreshold`. Tables and plots use those same inputs. Expected cells, flags, PPV/NPV, flags per detected case and tests per detected case are reported. Inverse net benefit/Youden are named as indices. |
| Rare prevalence | Lower prevalence limits and expanded prevalence plotting grid; bounded scenario lists and explicit population size. `decisioncalculator` reports observed and projected workload with fractional expectations preserved. |
| Frozen evaluation | `R/enhancedROC-validation.R` evaluates supplied score columns without fitting models or choosing thresholds. Explicit positive/negative definitions, direction and model/threshold provenance are required. Common complete cases support paired model comparisons. |
| Cluster and site uncertainty | Joint whole-patient bootstrap, conditional on fixed sites when supplied. Reports slide-weighted estimates, patient/event denominators, metric-specific valid/failed draws, paired differences and unavailable/degenerate interval status. Discrimination, fixed-cutoff metrics and applicable probability calibration metrics are included. |
| Cohort audit | Frozen output includes a denominator ledger, reference/model/sampling/threshold declarations and partition-overlap checks. `decision` and `decisioncompare` add optional validation-context tables while preserving explicit category mapping. |
| Zero-miss planning | `diagnosticsamplesize` computes the exact two-sided lower sensitivity bound and required independent positives conditional on no misses, with expected recruitment at target prevalence. This is not assurance or power. |
| Draft method honesty | `precisionrecall` implements the selected area method, percentile bootstrap and paired model-swap permutation test; removes unsupported nonlinear interpolation and BCa choices. Pairwise differences use the same paired rows as their tests. Plot and table baselines use each model's analyzable rows. |
| User guidance | [Rare-disease AI validation guide](../vignettes/meddecide-rare-disease-ai-validation.md), NEWS entry, generated wrappers/manuals and compiled UI updates. All seven edited analyses remain in testing menu groups. |

## Implementation choices and limits

The roadmap's provisional option/table names were consolidated into `validationPanel`,
`validationContext`, `denominatorLedger`, `frozenMetrics`, and `screeningWorkload`.
Patient clustering is selected by supplying `patientID`; absent patient IDs mean row resampling.
Site inference is conditional on the supplied sites, not a random-hospital population.
The legacy ROC panels retain their independent-row methods and are hidden in frozen mode.
No implicit one-versus-rest conversion is introduced.

The ledger distinguishes missing reference from missing/nonfinite scores. Extra reference
categories are rejected. A missing score does not identify its cause: technical failure versus
other missingness requires separately collected workflow data. Partition overlap stops analysis;
absence of a supplied manifest is explicitly recorded as unassessed. The report stores aggregate
results, not an identifier manifest. Reported flags are not measured review minutes or unique
additional patient reviews. Supplied-prevalence scenarios assume transportable Se/Sp and do not
propagate uncertainty in the assumed prevalence.

Bootstrap intervals are approximate percentile intervals. Too few valid draws or degenerate
samples suppress intervals rather than implying certainty at perfect empirical performance.
The coverage pilot below shows that this safeguard does not establish nominal coverage.
No production promotion is justified by the current validation. Runtime depends strongly on
patients, slides, models and replicates; the benchmark below measures point PR summaries only,
not a full bootstrap runtime estimate.

## Numerical and integration validation

- New pure-helper tests: 57 passing assertions, including endpoint/tie/direction PR cases,
  rare-prevalence workload arithmetic, cluster pairing, RNG preservation, invalid draws,
  probability boundaries, explicit mapping and partition consistency.
- New compiled-backend tests: 41 passing assertions, exercising generated options/results,
  workload and provenance tables, frozen comparisons, planner, PR method selection and
  model-specific plot baselines. Optional ID defaults remain `NULL` in the generated R API.
- Generated artifacts: isolated official jamovi compilation for seven analyses; YAML,
  generated JavaScript and R/manual syntax checks. No hand edits to generated R headers.
- Existing regression suites: see the final verification totals below.

## Coverage pilot and performance

Reproduce with [gaspath-validation-coverage.R](../inst/examples/gaspath-validation-coverage.R).
Results are saved in [bootstrap coverage](yang-2026-gaspath-bootstrap-coverage.csv) and
[PR performance](yang-2026-gaspath-performance.csv).

The pilot uses 100 simulations in each of four settings: 300 independent patients, prevalence
0.05 or 0.20, equal or unequal repeated-slide counts, and 199 bootstrap draws. Binary scores
have true sensitivity 0.8, specificity 0.9 and AUC 0.85. Slides within a patient are identical.
Among returned intervals, coverage ranges from 0.88 to approximately 0.98; Monte Carlo standard
errors are approximately 0.014–0.033. Unequal-cluster AUC coverage is 0.88 and 0.90. Some rare-event
sensitivity intervals are unavailable; the CSV separates conditional coverage from coverage
counts over all simulations. These findings support an explicit undercoverage warning and
continued testing status, not a universal 95% coverage claim.

On this machine, tie-aware PR summaries for 50 score columns × 50,000 rows took 0.532 seconds;
the score matrix occupied 20,000,216 bytes. This does not benchmark model training or the full
patient bootstrap. The article's original patient-level model performance was not reproduced.

## Final regression verification

| Check | Passing assertions | Notes |
|---|---:|---|
| GASPath pure helpers | 57 | No failures/warnings |
| GASPath compiled backends | 41 | No failures/warnings; includes final PR baseline fix |
| enhancedROC review fixes | 191 | No failures/warnings |
| enhancedROC smoothing and PR | 42 | No failures/warnings |
| enhancedROC release fixes | 34 | No failures/warnings |
| psychopdaROC concordance fixes | 27 | No failures/warnings |
| enhancedROC release review | 122 + 2 | 122 numerical/behavior assertions passed; the stale static option-read assertion was replaced and rerun separately with two passing assertions. Two expected pROC warnings for perfect AUC variance. |
| decisioncompare explicit levels | 365 | No failures/warnings |
| Diagnostic comparator | 47 | No failures/warnings |
| Reader-study adjacent drafts | 62 | No failures/warnings |
| Existing precisionrecall smoke test | 2 | One OMV-export check skipped: the existing test attempts to export a results group as tabular data, producing a warning. The compiled-backend tests independently exercise PR calculations. |

Old clinical-impact tests were updated to the new estimand: population prevalence cannot set
the score cutoff, and concordance comparisons must supply the same predictions, score cutoff,
utility and population. Wrong-direction marker tests now check the hand-calculated net benefit
rather than assuming its sign must match whether AUC is above 0.5.

Package loading also emitted existing environment warnings about TMB/glmmTMB version mismatch
and an unavailable X display. No full-package release check or interactive desktop jamovi review
was performed. The focused compiled checks and isolated UI compilation are the verification
scope, not a claim that every analysis in the module has been release-tested.
