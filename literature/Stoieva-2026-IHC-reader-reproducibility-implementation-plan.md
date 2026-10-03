# Implementation plan: diagnostic accuracy with reader reproducibility

Date: 2026-10-03. Status: implemented in the test routes; validation and remaining desktop checks are recorded in [the implementation record](./Stoieva-2026-IHC-reader-reproducibility-implementation.md).

Based on the [article review](./Stoieva-2026-IHC-interrater-agreement-citation-review.md) and a follow-up inspection of the current `pathagreement` data preparation, bootstrap helper, output lifecycle, and tests.

## Intended outcome

A researcher supplies independent reader scores and, optionally, a reference diagnosis. ClinicoPath reports each reader's diagnostic accuracy, agreement between readers, and which cases cross the clinical cutoff. Every estimate identifies its denominator, class/cutoff definition, and uncertainty method.

Extend **`pathagreement`** as the main workflow. Reuse existing agreement and diagnostic calculations after validating their exact estimands. Add small numerical helpers outside the large R6 class. Improve the shared specific-agreement interval path in **`agreement`**, and repair the separate draft **`cohenskappa`** weighting defect in a bounded change.

The first deliverable comprises phases 0–5 below. The kappa-balanced score is a separate, opt-in experimental phase and does not delay the established accuracy/agreement workflow.

## Implementation decisions

| Topic | Decision |
|---|---|
| Input layout | Wide data: one specimen/case per row, one column per independent reader; optional reference, case ID, and patient/cluster ID. |
| Legacy behavior | New workflow defaults to off. Preserve existing categorical analyses and macro-average outputs; make the latter's labels explicit. |
| Reader scope | At least two selected readers. Report pairwise agreement for each requested pair. Inference concerns these selected readers; population-of-readers inference is outside this increment. |
| Reference | Optional for agreement; required for diagnostic accuracy. Explicit positive and negative labels; all other labels remain indeterminate/excluded with counts. Consensus derived from the selected readings is not automatically assigned as the reference. |
| Binary inputs | Explicit reader-positive and reader-negative labels, validated consistently across columns. Never infer positivity from factor order. |
| Numeric scores | One prespecified cutoff and an explicit `>=`, `>`, `<=`, or `<` rule applied to every reader. The cutoff initially remains unset. |
| Ordinal scores | Require a common declared ordering and a named boundary category. Apply the inequality to order; do not treat nominal codes or ordinal spacing as measurements. |
| Near-cutoff cases | Numeric: either member of a reader pair lies within the prespecified inclusive band. Ordinal: either uses a category in an explicitly selected boundary set. No band/set means no near-cutoff analysis. |
| Missingness | Pairwise agreement uses both-observed readings; accuracy uses that reader plus the reference. Offer an additional shared-case accuracy view using all selected readers plus reference. Counts must reconcile to the original case rows. |
| Repeated specimens | Point estimates remain specimen-weighted. With patient/cluster ID selected, resample whole clusters and show both specimen N and cluster N. Do not silently switch to patient-weighted estimates. |
| Confidence intervals | Independent-case accuracy/proportion intervals use an identified validated method. Ratios, BA, and clustered estimates use the resampling rules in phase 4. No interval is shown without an identified method and estimable inputs. |
| R API compatibility | New options have defaults. Initially use validated String options for newly introduced category labels, with available levels displayed beside them; avoid adding mandatory `Level` arguments to the existing generated wrapper. |
| Existing routing | Keep `pathagreement` and `agreement` in their current test routes. Handle draft repair routing according to the repair playbook when that work starts. Production promotion remains a separate release step. |

Label String options are an intentional refinement of the review's proposed `Level` selectors: a `Level` option cannot have a default and would make an otherwise optional feature break existing R calls. Reject unknown labels with a message listing valid choices. If a default-compatible selector is later introduced, validate the generated wrapper before replacing these controls.

## Phase 0 — Establish the baseline and protect existing behavior

**Effort:** small. **Dependency:** none.

- [x] Record the current working-tree changes and relevant file hashes. Preserve the unrelated `nogoldstandard` work and generated metadata.
- [x] Run the existing focused `pathagreement` tests and record pre-existing failures separately.
- [x] Inspect the real R wrapper for nominal, ordered, incomplete, and empty input; compile and load the real-client GUI panels. Full desktop interaction is tracked separately below.
- [x] Capture expected values for the existing macro-average table and all currently supported missing-data branches.
- [x] Confirm generation and test toolchain availability without updating packages or module minimum versions as part of planning.

**Relevant existing tests:** `test-pathagreement-regressions.R`, `test-pathagreement-wiring.R`, `test-pathagreement-lifecycle.R`, and `helper-pathagreement.R`.

**Acceptance:** a recorded baseline identifies which behavior must remain stable, and tests can exercise both R6 construction and the exported wrapper.

## Phase 1 — Add reader data preparation and numerical helpers

**Effort:** medium. **Dependency:** phase 0.

**New files:**

- `R/pathagreement-reader-data.R`: validated input preparation, row mapping, class/cutoff conversion, and analysis masks.
- `R/pathagreement-reader-metrics.R`: binary accuracy and pairwise agreement summaries, returning ordinary R objects with no jamovi dependencies.
- `tests/testthat/test-pathagreement-reader-data.R`.
- `tests/testthat/test-pathagreement-reader-metrics.R`.

**Contract:** retain original row index, case ID, optional cluster ID, original readings, derived binary calls, reference classification, and exclusion reasons. Return explicit masks for each reader's accuracy, each reader pair, each near-cutoff subset, and the shared-case comparison. Return counts alongside every statistic; undefined results include a reason.

- [x] Remove metadata rows once and apply the same row mapping to readings, reference, case IDs, and cluster IDs.
- [x] Preserve missing observations until the particular estimate's mask is selected.
- [x] Calculate TP/FP/FN/TN separately from the reader-by-reader four-cell table.
- [x] Calculate sensitivity, specificity, BA, observed agreement, discordance rate, Cohen kappa, and positive/negative specific agreement.
- [x] Handle absent reference classes, no positive/negative calls, nonfinite numeric scores, unequal margins, negative kappa, and degenerate kappa explicitly.
- [x] Validate selected cluster IDs; missing cluster IDs cannot be silently excluded from inference. Use an internal unique row key even when supplied case labels repeat.

**Why this precedes UI work:** current `.initializeRows()` and `.prepareData()` construct complete-case data across all selected readers, while some existing pairwise methods use a separate available-case matrix. New reader accuracy cannot be built solely on `.data_matrix` without losing usable cases. Likewise, the current early return when there are no jointly complete rows must not suppress estimable new outputs.

**Acceptance:** a third reader's missing value does not remove a usable first/second-reader pair or first-reader/reference observation. Masks and exclusions reconcile exactly. Swapping readers preserves symmetric agreement and swaps the two discordance directions.

## Phase 2 — Add explicit binary reference accuracy to `pathagreement`

**Effort:** medium. **Dependency:** phase 1.

| File | Work |
|---|---|
| `jamovi/pathagreement.a.yaml` | Add an off-by-default `readerWorkflow` selector with `off`, `binary`, `ordinal_cutoff`, and `numeric_cutoff` values. Add explicit reference and reader labels with empty defaults, `referenceAccuracy` toggle, and available/shared-case reporting choice. Reuse `referenceStandard`. |
| `R/pathagreement.b.R` | Dispatch the new workflow before factor-only validation and before the no-complete-cases return. Use the new helpers; retain legacy calculations when the new workflow is off. |
| `jamovi/pathagreement.r.yaml` | Add `readerDataSummary` and `readerAccuracyTable`. Columns include reader, analysis population, reference class counts, TP/FP/FN/TN, exclusions, Se/Sp/BA, CI limits, and CI method/status. Label the old table as macro-averaged. |
| `jamovi/pathagreement.u.yaml` | Add a “Reader Diagnostic Accuracy” panel, optional reference target, explicit label controls, and case-population choice. Show available label values safely. |

- [x] Create rows determined by selected readers/options in `.init()` and fill them in `.run()`.
- [x] Update all relevant `clearWith` lists and reset stale rows when readers, reference, labels, or modes change.
- [x] Keep reference-reader agreement distinct from interreader agreement.
- [x] Allow agreement results without a reference; explain why accuracy is unavailable without treating the entire analysis as failed.
- [x] Keep the old R wrapper callable with its existing arguments.

**Numerical acceptance:** for TP=9, FN=1, TN=6, FP=4, disease-positive Se=0.90, Sp=0.60, BA=0.75. The existing macro-average calculation stays explicitly separate. Swapping reference and reader class definitions together swaps Se/Sp without changing BA.

**New tests:** `test-pathagreement-binary-accuracy.R`; extend existing wiring and wrapper tests. Cross-check independent-case intervals against `stats::binom.test()` or the explicitly selected existing diagnostic interval engine.

## Phase 3 — Add cutoff disagreement and case inspection

**Effort:** large. **Dependency:** phases 1–2.

| File | Work |
|---|---|
| `.a.yaml` | Add optional numeric `readerScores`, an initially unset cutoff input, `positiveRule`, optional numeric band width, ordinal boundary label, explicit ordinal near-boundary category set, pair selection (`all` or one selected pair), and a displayed-case limit. Reuse categorical `vars` and `caseID`. |
| `.b.R` | Derive calls without overwriting scores. In numeric mode, avoid running legacy factor-only calculations accidentally. For ordinal mode, retain original-score weighted agreement alongside the derived binary agreement. Compute full totals before limiting displayed cases. |
| `.r.yaml` | Add `readerCutoffSummary`, `cutoffAgreementTable`, and `cutoffDisagreementCases`. (Implemented with `readerSettings` serving as the unified cutoff/method summary.) Show the exact rule, category order where relevant, pair, population/band, N, four-cell counts, agreement metrics, and directional discordance. Case rows show both original scores, calls, IDs, and band membership. |
| `.u.yaml` | Add a “Clinical Cutoff Agreement” panel with mode-appropriate controls. Keep the cutoff unset until entered. Separate ordinal category selection from numeric band width. |

Use a default-empty String option for an unset numeric threshold and parse a single finite number in R without evaluation. For the ordinal boundary set, accept exact labels one per line and validate against the declared scale (the implemented single-line GUI additionally accepts semicolons); this avoids assigning distances to ordinal categories.

- [x] Make both all-case and near-cutoff denominators visible.
- [x] Count reader-1-positive/reader-2-negative separately from the reverse direction. Neither direction is called an error without a reference.
- [x] Keep aggregate totals independent of the displayed-case limit.
- [x] Use structured jamovi tables so normal copy/export operations work.
- [x] Bound pairwise computation: all-pair point summaries are allowed; expensive intervals can use a selected pair.
- [x] Treat absent bands, empty bands, tied cutoff values, and missing readings distinctly.

**Acceptance:** all four inequality rules classify exact-cutoff values correctly; ordinal reordering changes results only when the declared clinical order changes; nominal inputs cannot enter ordinal-boundary mode. Changing the case-display limit never changes estimates. A fixture with no rows complete across three readers still returns every estimable pair.

**New tests:** `test-pathagreement-clinical-cutoff.R`; expand option invalidation and saved-result lifecycle tests.

## Phase 4 — Add joint case/cluster uncertainty and repair related calculations

**Effort:** large. **Dependency:** phases 1–3 for integrated outputs.

### Shared resampling

Create `R/utils-reader-resampling.R`, used by both `pathagreement` and `agreement`. Include focused tests in `test-reader-resampling.R`. This shared helper accepts the point-estimator callback, the analysis data, confidence level, number of draws, seed, and optional cluster ID.

- [x] Draw whole case rows, or whole clusters with all their case rows. Keep every selected reading and the reference together.
- [x] Use `withr::local_seed()` and restore the caller's RNG state.
- [x] Return attempted, valid, undefined, and boundary/constant draw counts alongside intervals and method/status.
- [x] Parameterize the confidence level and resampling unit; do not reuse the current `pathagreement-bootstrap.R` helper unchanged, because it fixes 95% quantiles and filters nonfinite values.
- [x] Start with percentile intervals for prespecified estimands; do not offer BCa until the corresponding case/cluster jackknife and boundary behavior are validated.
- [x] For the first implementation, suppress a bootstrap CI if its point estimate is undefined, any resample makes that estimand undefined, or a constant resampling distribution would imply false precision. Show the reason and draw counts. Negative finite kappa is valid for agreement and must remain in its bootstrap distribution.
- [x] Use validated binomial intervals for independent-case Se/Sp and simple observed-agreement/discordance proportions. With clustering selected, do not present independent-case binomial intervals as cluster-aware.
- [x] Keep accuracy, BA, agreement and any later composite on identical resampled rows whenever joint uncertainty is required.

The conservative invalid-draw policy may make intervals unavailable for sparse data. Improving this with a validated boundary-aware method is preferable to silently conditioning on successful draws. A minimum of two observed clusters is necessary to resample; small-cluster coverage must also be evaluated and described rather than promised.

### `agreement` specific-agreement intervals

Modify `agreement.a.yaml`, `.b.R`, `.r.yaml`, and `.u.yaml` to add the validated case/cluster bootstrap path, its method labels, confidence level, seed and cluster option. The existing effective-N Wilson approximation is already labelled approximate: retain that disclosure for legacy results rather than describing it as an undisclosed defect. Default new reports to the validated path once coverage checks pass. **Implementation result:** the simulations found undercoverage, so the bootstrap remains explicitly exploratory and the existing approximate Wilson default is retained. This is a deliberate evidence-based restriction, not a claim of validated nominal coverage.

### Draft `cohenskappa` repair

As a separate bounded change, replace string weight arguments with correct numeric matrices or a verified equivalent API, extract the weighted estimate and matching variance, and preserve declared ordinal order. Check both the UI choice and the resulting table labels. Extend `test-cohenskappa.R`; do not promote the draft as part of the repair.

**Acceptance:** deterministic results for fixed seed; unchanged global RNG state; cluster membership remains intact including duplicate sampled clusters; matching independent-reference estimates; honest behavior at undefined/boundary values. Seeded coverage simulations must include unequal margins, rare categories, and clustered specimens. Report empirical coverage with Monte Carlo uncertainty; revise the method or restrict its documented scope when nominal coverage is unsupported.

## Phase 5 — Package the workflow, examples, and release checks

**Effort:** medium. **Dependency:** phases 1–4.

- [x] Generate a deterministic synthetic IHC reader dataset with reference diagnoses, numeric and ordinal scores, near-cutoff disagreements, missing readings, and repeated specimens. Use fictional case IDs.
- [x] Put the generator in `data-raw/` and the compressed example in `data/`; document the dataset using the repository's `data-*.R` convention.
- [x] Add a guide showing independent reads → reader-specific accuracy → original-scale agreement → binary cutoff agreement → disagreement inspection. Explain which population each denominator describes.
- [x] Link existing `kappaSizeCI`, `kappaSizePower`, and `kappaSizeFixedN` with their actual design assumptions; do not add duplicate planning calculators.
- [x] Document the score/cutoff, reference, missingness policy, selected readers, sampling unit, CI method, seed and resample count in the reproducible R example and analysis output.
- [x] Regenerate headers and Rd files using `jmvtools::prepare()` and `devtools::document()`; never edit generated files manually.
- [x] Run focused numerical, integration, wrapper, rendering and lifecycle tests, then required package/release checks appropriate to the final scope.
- [ ] Complete the remaining desktop checks: table copying, dark/light themes, and a saved analysis opened in the desktop. Automated partial-selection/toggle tests, real protobuf save/restore, data-only `.omv` round-trip and real-client option-panel checks are complete.
- [x] Benchmark 50,000 rows × 50 readers for point summaries; benchmark selected-pair bootstrap separately. Checkpoint long loops and avoid large saved states.
- [x] Inspect updater routing/dependencies with `Rscript _updateModules.R --dry-run` before any later synchronization. Keep this first deliverable in JamoviTest until promotion review is complete.

**Acceptance:** a saved example reproduces the stated numbers and case lists; legacy analyses remain callable; all relevant tests and release checks pass; helper files are discovered by the updater from real callers. Existing unrelated failures are recorded separately and are not hidden by changes to this feature.

No new runtime package is expected. Verify declared dependencies in every destination module: likely `irr`/`irrCAC` or existing kappa engines, `stats`, `boot` where used, `withr`, and the chosen existing diagnostic interval package. Do not add imports without actual calls.

## Phase 6 — Optional exploratory kappa-balanced score

**Effort:** medium, plus methodological verification. **Dependency:** phases 1–5.

The original review could not inspect the paper's full-text rule for associating reader accuracy with reader-reader kappa. Verify that definition before presenting any output as a reproduction of the published score. Failure to retrieve that definition does not block the core workflow.

- [x] Add an off-by-default experimental option to the four `pathagreement` files.
- [x] Restrict the first version to one selected reader pair and unweighted binary Cohen kappa.
- [x] If defining a module-specific extension, require an explicit choice of which member of the pair supplies BA and label that choice. Do not silently average reader accuracies or call it the paper's exact implementation.
- [x] Compute both BA and kappa using the same cases complete for the selected pair and reference; report the two components, Se/Sp, cutoff, reader choice and N.
- [x] Calculate `sqrt(BA * kappa)` only for finite components in [0,1]. Preserve valid zero values; show unavailable for negative/undefined kappa with an explanation.
- [x] Apply joint resampling only under the documented invalid-draw policy. Negative kappa draws make the composite undefined even though they remain valid agreement estimates.
- [x] Add no clinical acceptance bands, automatic biomarker ranking, or claim that this is a reporting standard.

**Acceptance:** the known mathematical examples are reproduced; negative and degenerate kappa behave explicitly; changing the selected accuracy reader is reflected in the label and value. The experimental feature remains separable from the established clinical reporting outputs.

## Delivery sequence and completion criteria

| Increment | Contents | Completion evidence |
|---|---|---|
| 1 | Baseline, masks, numerical helpers | Independent hand-calculated fixtures and legacy baseline tests |
| 2 | Binary reference accuracy | Explicit disease-positive Se/Sp, counts, class labels and case populations |
| 3 | Numeric/ordinal cutoff disagreement | Correct equality rules, complete pair totals and traceable case lists |
| 4 | Joint uncertainty and bounded agreement repairs | Reference comparisons, seeded simulations, invalid-draw reporting and RNG tests |
| 5 | Examples, documentation and test-module integration | Reproducible saved example, lifecycle/UI checks and release-gate evidence |
| 6 | Optional experimental composite | Verified source definition or explicitly labelled extension, component traceability and domain checks |

Implement increments in this order. The draft weighted-kappa repair can be a separate small commit within increment 4. Avoid broad refactoring of unrelated agreement, clustering, ROC, latent-class, or sample-size features.

**Core workflow is complete when:** independent original readings remain available; diagnostic and agreement estimands are correctly separated; cutoff disagreements and exclusions are traceable; every estimate states its case population; intervals respect the sampling unit; and documented examples pass both numerical and jamovi lifecycle checks.
