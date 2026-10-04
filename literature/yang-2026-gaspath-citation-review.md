# GASPath: statistical review and ClinicoPath development roadmap

Reviewed 4 October 2026 using `review-article-stats`. Scope: article and supplement review,
independent summary arithmetic, and inspection of current repository implementations.
This document proposes changes; it does not implement analysis-function changes.

## Assessment

**Read in full: yes, for AI validation design and teaching. Major concerns remain before
clinical-readiness claims can be accepted.** The distinction between discrimination,
predictive value, and operational benefit is the central teaching point. A positive-only
external sample cannot establish external specificity. A consecutive consultation series
is more informative for local alert burden, but does not establish population-screening
performance or improved patient outcomes.

Strengths include a staged evaluation, an explicit comparison with alternative models,
and attention to a clinically difficult rare-disease task. The appropriate next step is
independent confirmation and a measured workflow evaluation, rather than discarding the
promising detection results because clinical utility is still uncertain.

### Article facts and methods

Development used 1,629 slides, including 309 GAS slides from 96 cases. External testing
used 249 GAS-only slides across 12 centers. Local evaluation included 7,056 consecutive
consultation slides and 45 GAS slides. Default specificity was 0.905; at threshold 0.95,
open-category specificity was 0.956, with all positives detected. The reference combined
expert consensus, morphology, and ancillary assays. Internal and local collection dates
overlap. [Article: Methods and Stage III](https://doi.org/10.1002/2056-4538.70113).

| Method | Role | Assessment needed |
|---|---|---|
| MIL/foundation-model classification | Prediction | Independent patient partitions and frozen inference pipeline |
| Ten-fold evaluation, 6:2:2 splits | Internal validation | Clarify repeated splits versus disjoint folds |
| Accuracy, F1, ROC/PR AUC, κ | Performance | Metric definitions, averaging and sampling units |
| Argmax and probability cutoffs | Decision rules | Prespecification and threshold provenance |
| Clopper–Pearson CIs | Reported uncertainty | Applicability to each metric |
| K–S and Mann–Whitney tests | TP/FP score comparison | Independence and limited clinical interpretation |
| Unadjusted tests and ablations | Exploratory comparison | Multiplicity and dependent evaluation sets |

These methods are described in the article's System evaluation, Statistical analyses,
and Ablation sections. [Full text](https://pathsocjournals.onlinelibrary.wiley.com/doi/full/10.1002/2056-4538.70113).

## Citation and source verification

| Field | Verified value |
|---|---|
| Title | Artificial intelligence-assisted histopathological diagnosis of endocervical gastric-type adenocarcinoma: a multicenter model development and validation study |
| Authors | Jing Yang, Qiming He, Jing Peng, et al.; complete list in accompanying BibTeX |
| Journal | The Journal of Pathology: Clinical Research |
| Year / volume / issue / article | 2026 / 12 / 5 / e70113 |
| First published | 1 September 2026 |
| DOI | 10.1002/2056-4538.70113 |
| PMID / PMCID | 42676291 / PMC13531234 |
| Publisher / ISSN | Wiley / 2056-4538 |
| License | CC BY-NC-ND 4.0; this is an independent critique, not a modified reproduction |
| Retraction status | No linked correction/retraction/expression of concern found in the checked PubMed, Crossref, publisher records or DOI-specific web search as of 4 October 2026; Retraction Watch was not independently queried |

Metadata sources: [PubMed](https://pubmed.ncbi.nlm.nih.gov/42676291/),
[Crossref DOI record](https://api.crossref.org/works/10.1002%2F2056-4538.70113),
[Europe PMC full-text XML](https://www.ebi.ac.uk/europepmc/webservices/rest/PMC13531234/fullTextXML).
The supplement was retrieved from the official
[Europe PMC supplementary-files endpoint](https://www.ebi.ac.uk/europepmc/webservices/rest/PMC13531234/supplementaryFiles).
The direct Wiley supplement route failed; the archived PDF supplied the material instead.
No supplied substantive source was left unread because of that failure. Individual patient
predictions, a partition manifest, and a frozen runnable model were not available for this review.
The archived supplement could not be byte-compared with the inaccessible Wiley copy.
Its SHA256 is `902633f02d3d7af2eab8202b19db3f47aa310d5023424c8c35e08b5aba59f89f`.
Retraction checking was a dated discovery check, not a guarantee that no notice exists.
[Validated BibTeX](yang-2026-gaspath.bib) preserves all 25 authors.

## Independent prevalence and workload arithmetic

These are **inferences from rounded summaries**, not author-reported workload counts.
Use the full open-category denominator: 7,011 negative slides and prevalence
45/7,056 = **0.6378%**. Do not use the restricted-category specificity with this denominator.

| Quantity | Default argmax | GAS probability threshold 0.95 |
|---|---:|---:|
| Rounded specificity input | 0.905 | 0.956 |
| Inferred false-positive slide flags | 666.045 ≈ **666** | 308.484 ≈ **308** |
| Inferred total positive slide flags | 711.045 ≈ **711** | 353.484 ≈ **353** |
| Inferred PPV | **6.33%** | **12.73%** |
| Positive flags per 1,000 slides | 100.77 | 50.10 |
| Flagged slides per detected GAS slide | 15.80 | 7.86 |
| Integer FP counts compatible with rounding | 663–669 | 305–311 |

The rounding-compatible ranges are **not confidence intervals**. Computation:

```text
FP = (N − N_positive) × (1 − specificity)
flags = TP + FP
PPV = TP / flags
```

The user's “roughly 660” inference is reasonable; **approximately 666** is the direct
calculation. These flags need not equal unique patients, extra consultations, minutes of
work, ancillary tests, or avoidable harm. Repeated slides and ordinary review already being
performed both matter. Measuring incremental workload requires a comparator workflow.

Under an independent-positive-slide binomial model, 45/45 gives a two-sided exact 95% CI
of **92.13%–100%**. This is a conditional illustration, not a cluster-adjusted interval and
not a guarantee of future perfect sensitivity. Clopper–Pearson applies to a binomial
probability; see [R `binom.test`](https://search.r-project.org/R/refmans/stats/html/binom.test.html).

Reproduce using [the R script](yang-2026-gaspath-arithmetic.R); calculated values are in
[the CSV](yang-2026-gaspath-arithmetic.csv). No simulated row-level data were passed off as
the study's observations.

## Critical evaluation

### Major issues and questions for the authors

1. **Resolve patient independence before interpreting precision.** Different blocks from
   one patient remain correlated. Request anonymized patient-to-partition counts, a
   cross-cohort overlap audit, and confirmation that every slide from a patient stayed in
   one partition. Temporal overlap is a reason to verify independence, not evidence that
   leakage occurred. Split by patient before any tile extraction or supervised tuning.

2. **Separate external sensitivity from external diagnostic performance.** Specificity
   needs external negative specimens representative of the intended use. Do not combine
   internal specificity and external sensitivity as though they came from one validation
   cohort. Report both pooled and center-specific estimates with denominators; an unweighted
   mean of center sensitivities answers a different question from a slide-pooled estimate.

3. **Correct and document uncertainty calculations.** AUC and F1 are not simple binomial
   proportions. Request calculation code and the denominator/resampling unit for every CI.
   For independent binary ROC data, DeLong or appropriate bootstrap procedures are available;
   clustered slides require resampling patients or another justified clustered estimator.
   Repeated validation folds are dependent and cannot simply multiply the independent sample
   size. [pROC CI documentation](https://search.r-project.org/CRAN/refmans/pROC/html/ci.auc.html).

4. **Verify the metric specification against code.** Supplement equation 23 prints
   `2TP/(TP+FP+FN)`; the correct F1 is `2TP/(2TP+FP+FN)`. The printed expression gives 2
   for perfect classification. This establishes a formula/reporting error, **not proof that
   reported numerical F1 values used that formula**. The supplement's actual section 1 is
   about visualization despite the landing-page description suggesting CIs. No detailed CI
   procedure was located in its extracted text. [Supplement, pp. 4 and 10](https://www.ebi.ac.uk/europepmc/webservices/rest/PMC13531234/supplementaryFiles).

5. **Freeze thresholds before the final evaluation.** Confirm when the 0.95 cutoff was
   selected, by whom, and using which data. An exploratory cutoff remains useful, but needs
   a new untouched evaluation cohort before its apparent improvement is treated as validated.
   A five-class argmax rule is not equivalent to a binary cutoff of 0.5.

6. **Do not equate score separation with clinical benefit.** Comparing TP and FP score
   distributions conditions on positive predictions. Significant tests do not establish
   calibration, a safe review threshold, time savings, or net benefit. Report calibration
   and PPV at the deployment prevalence, then evaluate AI-assisted versus usual practice
   using prespecified review-time and diagnostic-error endpoints.

7. **Retain all intended-use specimens in the denominator ledger.** Supplement Table S12
   includes 5,155 slides in the additional-category group; the four non-GAS internal
   categories total 1,856. The restricted five-category subset therefore contains 1,901
   slides, with prevalence 45/1,901 = 2.37%, materially different from the full cohort.
   [Supplement, p. 26](https://www.ebi.ac.uk/europepmc/webservices/rest/PMC13531234/supplementaryFiles).
   Request explicit counts for technical failures, unresolved reference diagnoses and
   exclusions, with sensitivity analyses. An unfamiliar but verified non-GAS category is
   different from an indeterminate reference or failed AI inference.

8. **Document reference independence and clinical scope.** A composite pathology reference
   can be a defensible diagnostic standard; expert involvement alone does not automatically
   make sensitivity/specificity inappropriate. Establish AI-blinded adjudication, consistent
   verification, and resolution of disagreements. If the estimand is agreement with an
   expert opinion rather than disease status, label it PPA/NPA. A consultation workflow's
   prevalence is local and should not be called a universal real-world disease prevalence.

### Reporting rubric

This is the requested skill's subjective 0–2 reporting/analysis rubric, **not a validated
risk-of-bias score or an estimate of model accuracy**. Unresolved reporting can lower a score
without demonstrating that the underlying conduct was wrong.

| Aspect | Score | Recommendation |
|---|---:|---|
| Design–method alignment | 1 | Distinguish diagnostic validation from clinical impact |
| Assumptions and diagnostics | 0 | Verify grouped partitions and correlated observations |
| Sample size and precision | 1 | Plan precision from independent positive patients |
| Multiplicity | 0 | Prespecify primary comparison; adjust confirmatory families |
| Model specification and setting effects | 1 | Audit scanner/site shortcuts and frozen model selection |
| Missingness and exclusions | 1 | Publish a complete specimen flow and failure ledger |
| Effect sizes and CIs | 0 | Resolve CI method and formula reporting |
| Validation and calibration | 1 | Add independent negative cohorts and calibration assessment |
| Reproducibility and transparency | 1 | Supply executable metric code and split/prediction manifests |
| **Total** | **6/18** | **Major reporting/inference concerns; promising detection evidence** |

The applicable reporting framework is prediction-model/diagnostic validation, with STROBE
useful for cohort flow. CONSORT is relevant to a future randomized impact evaluation;
PRISMA is not the framework for this original model study. Use
[TRIPOD+AI](https://www.tripod-statement.org/) and
[TRIPOD-Cluster](https://www.bmj.com/content/380/bmj-2022-071058) to document model evaluation,
clustering and performance heterogeneity. This review is not a completed formal PROBAST assessment.

## ClinicoPath coverage and development priorities

Coverage reflects source inspection, not a new end-to-end certification of every analysis.
Concrete evidence and proposed edits are recorded below. Existing capabilities should be
extended rather than duplicated.
The [detailed function audit](yang-2026-gaspath-function-audit.md) records option, UI,
result and backend locations. `enhancedROC`, `psychopdaROC` and `decisioncalculator` are
in production routes; `decision` and `decisioncompare` are in testing routes. Draft
`precisionrecall`, `modelval` and `leaveonecenterout` are not validated production substitutes.

### Coverage matrix

| Need | Existing function | Coverage | Remaining requirement |
|---|---|:---:|---|
| Binary confusion-matrix estimates | `decision`, `decisioncompare` | ✅ | Explicit positive/negative definitions and appropriate comparator mode |
| Custom score cutoffs and ROC | `enhancedROC` | ✅ | Frozen external scores and explicit score direction |
| Target-prevalence PPV/NPV | `enhancedROC`, `decisioncalculator` | ✅ | Point estimates available; uncertainty in assumed prevalence is separate |
| Review counts and burden | `enhancedROC` | 🟡 | Existing impact tables; separate observed/projected counts and utility |
| Brier score and calibration | `enhancedROC` | ✅ | Available for suitable probability inputs; clustered uncertainty is separate |
| AP and trapezoidal PR-AUC | `enhancedROC` | ✅ | Existing point estimates through `detectImbalance`; clustered CIs separate |
| PR consistency across analyses | `psychopdaROC`, draft `precisionrecall` | 🟡 | Numerical/option defects require focused validation |
| Clustered reader agreement | `pathagreement` | 🟡 | Existing categorical patient resampling does not cover score AUC/calibration |
| Patient-clustered score validation | ROC workflows | ❌ | Common cluster-aware metric engine |
| External cohort/provenance audit | Multiple individual analyses | 🟡 | Integrated cohort/unit/threshold/source ledger absent |
| MIL training, image tiling, foundation encoding | No equivalent tabular analysis | ❌ | Keep outside jamovi; import frozen scores and audit metadata |

For immediate use, first create a documented GAS/non-GAS disease variable and an explicit
GAS-flag/non-GAS-flag prediction variable. Inspect and approve every mapped category;
never silently pool unknown labels. Use `decision`/`decisioncompare` for the selected binary
pair, and `enhancedROC` for continuous scores, fixed cutoffs and calibration. Keep the full
cohort and restricted subset as separate analyses. Record any missing/indeterminate rows
before filtering. Positive-only external data support a sensitivity estimate alone; do not
manufacture specificity or PPV by adding synthetic negatives.

### P0 — Repair clinical-impact estimands and naming

**Target:** `enhancedROC`; see `R/enhancedROC.b.R:5393` onward and
`jamovi/enhancedROC.r.yaml:1083` onward.

The current backend uses prevalence to choose a classification/utility threshold, clamps
it to 0.01–0.99, and calculates impact counts and PPV from the observed sample. Its note
suggests changing the prevalence input to change the desired utility threshold. That mixes
three distinct inputs. Inverse net benefit is also not a literal number of slides requiring
review. This should be corrected before expanding the panel.
The `clinicalMetrics` table already uses the proper Bayes adjustment; the mismatch is in
the impact panel, so a repair must preserve the correct existing estimates.

- `.a.yaml`: add separate `impactCutoffs`, `utilityThreshold`, `workloadPopulationSize`,
  `analysisUnit`, and `impactPopulation` (`observed`/`target`). Retain `prevalence` as the
  target-population proportion only. Preserve existing option names with an explicit migration
  notice where old saved behavior changes.
- `.b.R`: classify at the supplied frozen cutoff; compute observed counts directly. For
  target scenarios compute `TP=N*p*Se`, `FP=N*(1-p)*(1-Sp)`, with FN/TN analogues. Calculate
  utility at its separate harm/benefit threshold. Never label a transported expected count
  as an observed integer count. Suppress undefined ratios with a reason.
- `.r.yaml`: add `populationSource`, `unit`, `expectedFP`, `expectedFlags`, `flagsPer1000`,
  `flagsPerDetectedPositive`, and `sourcePrecision`. Use decimal counts for projections.
  Rename inverse net benefit to state exactly what it is, or remove it from workload results.
- `.u.yaml`: separate classification, target population and utility controls. Show units,
  denominator, and observed-versus-projected labels beside results.
- Dependencies/performance: base R; linear in rows per cutoff. Avoid repeated risk fitting
  on validation outcomes; accept externally supplied probabilities for clinical validation.

### P0 — Validate and repair PR integration

**Targets:** `psychopdaROC` and draft `precisionrecall`.

`R/psychopdaROC.b.R:1663` integrates observed-threshold PR points without a recall-zero
endpoint. Its `2:length(recall)` loop also needs a single-threshold guard. The draft
`precisionrecall` exposes an integration-method choice that must be connected to computation.
Direct execution of the current helper with scores `c(.1,.2,.8,.9)`, truth
`c("N","N","P","P")` and direction `>=` returned **0.5**, although this perfect
classifier should have area 1. `enhancedROC` already handles ties/endpoints and distinguishes
AP from trapezoidal area in `.computePRMetrics` (`R/enhancedROC.b.R:3002–3067`): extract
and independently verify that implementation as a shared helper before adding another one.
The draft's BCa option also currently returns percentile intervals, and its permutation
comparison branch returns unavailable. Correct these claims or disable unsupported choices.

- `.a.yaml`: specify `prAreaMethod` with clearly distinct average-precision and trapezoidal
  definitions; reuse/repair the draft's existing `aucMethod` rather than adding a duplicate.
  Expose only interval/comparison methods that are actually implemented.
- `.b.R`: implement one tested, tie-aware helper with explicit endpoints, direction and
  degenerate-class handling. AP and trapezoidal PR area must not silently share one result.
- `.r.yaml`: print method, positive count, negative count and prevalence baseline alongside
  the area. Return unavailable with a reason when the estimand cannot be calculated.
- `.u.yaml`: use consistent method labels and explain the baseline; retain the draft route
  until validated.
- Dependencies/performance: base R or a pinned validated PR implementation; sort once,
  O(n log n), instead of rescanning every row for every unique score.

### P1 — Add patient-cluster uncertainty and fixed-rule comparison

**Target:** extend `enhancedROC`, sharing only validated primitives with `pathagreement`.

- `.a.yaml`: add `clusterID`, `siteID`, `resampleUnit`, `bootstrapReplicates`, and `seed`;
  require a declared inferential unit and support comparison of frozen predictions.
- `.b.R`: resample entire patient clusters, carrying paired model scores together. Keep
  thresholds frozen. Define whether inference is conditional on the observed sites or
  intended to generalize across sites. Handle failed/one-class replicates explicitly.
  Report a boundary warning when an all-success empirical bootstrap degenerates; do not
  present a 100%–100% interval as evidence of certainty.
- `.r.yaml`: add cluster counts, successful/failed draws, interval method, paired metric
  differences and a boundary-status column.
- `.u.yaml`: distinguish patient clustering from optional site resampling; provide a runtime
  estimate and reproducible seed control.
- Dependencies/performance: base R resampling plus existing metric packages. Batch/cached
  sufficient counts for fixed-threshold metrics; benchmark score sorting across bootstrap draws.
  This estimates uncertainty of a frozen model, not variability from training it again.

### P1 — Cohort and reference audit trail

**Targets:** `enhancedROC`, `decision`, and `decisioncompare`, with a shared metadata helper.

- `.a.yaml`: introduce `cohortRole`, `samplingDesign`, `referenceDescription`, `thresholdSource`,
  `modelVersion`, and `analysisUnit`; add optional patient/site/partition metadata for validation.
- `.b.R`: validate disease definitions before metric computation; report absent disease
  classes without inventing estimates. Audit duplicate IDs across partitions when a manifest
  is provided. Do not infer absence of leakage from missing IDs. Maintain technical-failure,
  unresolved-reference, extra-category and missing-data counts separately.
- `.r.yaml`: add `validationContext`, `denominatorLedger`, and `partitionAudit` outputs.
- `.u.yaml`: keep positive and negative selection explicit. If approved multi-category
  grouping is later added, require disjoint enumerated sets and a visible mapping ledger;
  do not restore implicit one-versus-rest behavior.
- Dependencies/performance: base R; O(n) validation. Store optional metadata without PHI.

### P2 — Rare-prevalence scenarios, precision planning, and teaching

**Targets:** `enhancedROC`, `decisioncalculator`, `diagnosticsamplesize`.

- `.a.yaml`: permit validated target proportions below 0.001 and a bounded prevalence grid;
  add positive-patient precision targets where supported by the chosen interval method.
- `.b.R`: retain full numeric precision, validate 0≤p≤1 and define boundary behavior. Keep
  sample size planning separate from evidence that a fixed model has already been validated.
- `.r.yaml`: display expected PPV, false flags, missed cases and required independent positives
  per scenario; distinguish sampling CIs from sensitivity-analysis ranges.
- `.u.yaml`: accept sufficiently precise prevalence input with percent/proportion units explicit.
- Dependencies/performance: base R; small scenario grids. Add a teaching vignette using the
  accompanying summary arithmetic, without pretending to reproduce original patient data.

Also lower the prevalence-plot grid where appropriate: the current `enhancedROC`
PPV-versus-prevalence plot begins at 1%, above this study's local prevalence. Transported
calculations assume sensitivity/specificity apply in the target setting; prevalence adjustment
alone cannot correct changes in case mix, specimen quality or reference verification.

MIL training remains out of scope for this tabular module. An import specification should
carry slide ID, deidentified patient ID, site, split, reference, inference status, model
version and frozen scores. This is an integration boundary, not a proposal to bundle a
foundation model into jamovi.

## Test plan, dependencies and ranked backlog

1. **Clinical impact regression tests:** reproduce 666.045 and 308.484 expected FP above;
   change prevalence while keeping cutoff fixed and check PPV/counts; change utility alone
   and verify that the confusion matrix does not change. Test empty flagged sets, Se=0/1,
   Sp=0/1, prevalence boundaries and scenario/observed labels.
2. **PR numerical tests:** perfect/inverted classifiers, one positive, ties, constant scores,
   duplicated scores, missing rows, reversed direction and all-positive/all-negative inputs.
   Compare AP and trapezoidal results to independently hand-calculated fixtures and a trusted
   reference implementation. Assert a perfect classifier has area 1.
3. **Cluster tests:** duplicated slides within patients, unequal cluster sizes, inconsistent
   within-patient reference labels, paired-model covariance, seed reproducibility and no-event
   replicates. Use coverage simulations across prevalence/cluster-size conditions; report
   Monte Carlo error and boundary failures.
4. **Provenance/lifecycle tests:** changed cutoffs, cohort, reference mode and prevalence clear
   stale rows; saved options reopen with correct migration behavior. Verify explicit positive
   and negative mappings and ensure unresolved categories never silently become negative.
5. **Performance:** benchmark 50,000 rows, 50 score columns and realistic bootstrap settings;
   ensure the implementation avoids storing all tile-level data or unnecessary prediction copies.

Existing R dependencies should suffice for workload arithmetic and metadata. Use existing
`pROC` for suitable ROC tasks; it does not itself provide the proposed patient-cluster engine
or a general multiclass AUC CI. Add a PR dependency only after selecting the exact area
definition and testing its conventions. Regenerate `.h.R` and jamovi UI through the official
compiler during implementation; never edit generated bases manually.

**Ranked backlog:** (1) impact estimand/naming repair, (2) PR numerical repair, (3) patient
clustering and paired frozen-model comparisons, (4) cohort/reference provenance, (5) extended
prevalence scenarios and precision-planning teaching. Calibration itself is already available;
the needed additions concern its sampling design, uncertainty, and validation provenance.

## Validation performed and limits

The arithmetic script was executed successfully, including fixed-value checks and exact
binomial CI calculation. This review did not train GASPath, reconstruct original slide-level
predictions, or run the repository-wide package test suite. No analysis implementation was
changed. Clinical-readiness questions require data or documentation beyond summary metrics.
The coverage audit additionally executed the deterministic PR counterexample described above.

## Skills and agents used

| Skill / agent | Contribution |
|---|---|
| `review-article-stats` | Full review structure, source mapping and implementation roadmap |
| `statistical-analysis` | Metric assumptions, precision, multiplicity and estimands |
| `peer-review` | Reporting completeness and actionable author questions |
| `scientific-critical-thinking` | Bias, generalizability and distinction between claims and evidence |
| `citation-management` and `pdf` (citation agent) | DOI/BibTeX checks, supplement extraction and visual verification of key pages |
| Citation agent (`citation_context`) | Citation-management/PDF workflow, official metadata and supplement verification |
| Coverage agent (`coverage_audit`) | Actual YAML/backend inspection and bounded numerical probes |

The article-review playbook explicitly requests independent citation and repository-coverage
work; the main reviewer integrated those results. No raw patient data were supplied, so no
raw-data reanalysis or survival-analysis workflow was invoked.
The playbook's named `pubmed-database` skill was not present in the available skill catalog;
official PubMed XML and Crossref records were checked directly instead. No MeSH-based
interpretation was required for the statistical review.

## Implementation follow-up

The roadmap was subsequently implemented in testing routes. See the
[implementation and validation record](yang-2026-gaspath-implementation.md) for changes,
verification and remaining inference limitations.
