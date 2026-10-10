# Learning from external jamovi audits

Use an external report to improve our review questions and regression fixtures. It is evidence
about the reported build, not a diagnosis or a verification of our modules. Start with the
[library review guide](jamovi_library_review_guide.md); this guide supplies the additional
lessons from jYS and records exactly what was, and was not, verified.

## Source and evidence boundary

Read on 2026-10-10, supplied by the user from Downloads:

| Source | Scope | SHA-256 |
|---|---|---|
| `2026-10-03 jYS.md` | jYS 1.1.5, audit dated 2026-10-03, following a 2026-09-21 review | `b1c4c6f3e5e322c9b48e4efacc86297d44d083a3078115fb381097885a01b709` |
| `MODIFICATIONS_REPORT_EN.md` | jYS 1.1.5, remediation account dated 2026-10-10 | `87a52d7a640c62d06dc8bfc4e023ce74458da322912148a451b362dc4f44e212` |

The audit contains **35 findings: 1 HIGH, 14 MEDIUM, 18 LOW and 2 INFO**. The modification
report addresses the 15 HIGH/MEDIUM findings and an additional Schoenfeld plot persistence/export
problem. Its statement that all 15 findings were resolved does **not** close the whole audit.
It reports 186 passing tests across 14 suites; neither those tests nor the external implementation
were supplied or executed here. The earlier jYS report and its repository history were not
provided, so recurrence attributed to that earlier round remains the reviewer's account.

The original audit's positive observations remain useful: small drawing-state payloads, shared
ROC rendering, declared imports, safe formulas, checkpoints, native level selectors, and coverage
of every analysis. They are properties to preserve. Its statement that absence of private render
caches meant every plot exported correctly was broader than its evidence: the later remediation
account reports export failures. A static rendering check cannot certify persistence or export.

This learning pass changes documentation and playbooks. It does not close any of our findings,
annotate the external reports, update our submodule scoreboard, change routing, regenerate a
module, or assert a shipped defect count. The source reports remain unchanged.

## 1. Trace invalidation through every result dependency

An explicit `clearWith` list is a cache contract, not just a list of valid option names. Trace
each Table, Image, Output and nested Array item through preprocessing, model fitting, helpers,
and drawing options. Include choices that affect any of those stages: event/reference levels,
start time, subject ID, transformations, selection, folds, seed, multiplicity and plot thresholds.
The default clears on any option change; selective reuse is an optimisation that needs evidence.
[jamovi state documentation](https://dev.jamovi.org/tutorial/tuts0203-state)

Record a dependency matrix: **option → computation/helper → result → invalidation → probe**.
Changing state in `.run()` is not by itself evidence that a previously rendered image or a
filled Output column was invalidated. State reuse guarded by `is.null(state)`, table emptiness,
or `isNotFilled()` makes a missing dependency especially consequential. Do not remove the
renderer NULL guard: it still protects legitimately absent state on early exit or restoration.

Verification must reuse an existing analysis: run A, change one relevant option to B, then compare
its updated tables, plot data/rendered result and saved columns with a fresh analysis at B.
Choose data for which the changed option has an observable effect. A fresh A run and a fresh B
run alone cannot expose a stale cache. Also test valid → invalid → valid transitions and nested
array items. A display-only toggle may reuse numerical work only after its independence is traced.

`tools/release_gate.py::check_clearwith` checks that listed names exist. It does **not** establish
completeness. Analysis-specific transition tests are needed; there is no generic semantic gate.

## 2. Keep spreadsheet row identity separate from vector position

Preserve the original row names before filtering, complete-case selection, sorting or joining.
Pair values and IDs from the **same prediction frame in the same order**. If preprocessing resets
row names, retain a separate original-ID vector and carry it through each transformation.
The jamovi Output API maps these IDs back to spreadsheet rows.
[Output API](https://dev.jamovi.org/api/output)

```r
# clean_data retains the original jamovi row names; predictions use this exact order.
stopifnot(length(predictions) == nrow(clean_data))
self$results$predictions$setRowNums(rownames(clean_data))
self$results$predictions$setValues(predictions)
```

Never index a vector of length `nrow(self$data)` with original row IDs: a filter can leave IDs
51, 54 and 80 in a three-row frame. That is neither positional indexing nor a safe padding scheme.

Test nonconsecutive IDs, removal of leading rows, an internal missing value, reordered prediction
rows, and the no-eligible-rows path. Verify exact ID/value pairs and missing cells on omitted rows,
including after a filter changes. Equal vector lengths or a successful `setValues()` call prove
neither alignment nor clearing of old saved values.

## 3. Validate the metric, encoding and evaluation population together

- Decode class indices with the model's class labels before comparison. A factor's integer
  codes (1/2) and a binary training target (0/1) are different representations. Check perfect,
  fully wrong and mixed predictions against hand-counted confusion matrices, then repeat with
  reordered factor levels and probability columns. Apply the same check to permutation importance.
  The jYS mismatch counts false negatives divided by **all cases** under its stated coding;
  calling that the false-negative *rate* would use the wrong denominator.
- Match probability columns by class name, not assumed position. State whether a saved column
  is P(selected positive class) or P(predicted class). Probabilities must be finite and in [0,1];
  mutually exclusive multiclass probabilities must sum to one within a justified tolerance.
  Scores/logits can be legitimate ROC inputs, but must not be labelled probabilities or used
  as `1 - p`. Removing an output activation does not validate a separate hidden-layer derivative.
- Distinguish training, held-out, cross-validation and OOB predictions in every table, ROC curve,
  misclassified-case listing and saved column. In a ranger probability forest, `prediction.error`
  is a Brier score. Accuracy comes from classified labels; `1 - Brier` is not accuracy. Obtain
  in-sample predictions through the prediction method when that is what the label promises.
  [ranger reference](https://imbs-hl.github.io/ranger/reference/ranger.html),
  [prediction reference](https://imbs-hl.github.io/ranger/reference/predict.ranger.html)
- Preprocessing learned from data, including imputation, scaling and feature selection, belongs
  inside each training fold. Fixing the seed does not fix leakage. Label post-selection refit
  inference as such. Distinguish mean fold/repeat AUC from AUC of pooled or averaged predictions.
- Scope the seed **before the first stochastic operation**, including selection or fold assignment,
  and restore the caller's RNG state on normal and error exits. Probe both an existing
  `.Random.seed` and its initial absence. A seed near the final fit is too late if preprocessing
  already used randomness. See [RNG rule](jamovi_library_review_guide.md#28-rule-seeding-the-rng-must-not-outlive-the-function-that-did-it).

Object-class assertions, CV accuracy above 50%, or CV close to training accuracy are not numerical
oracles. Real generalisation gaps can be large. Require exact small fixtures and evaluate the
displayed values through the analysis interface. Do not require training and OOB metrics to
differ on every dataset: verify their provenance and arithmetic instead.

## 4. Match survival calculations to the data unit and estimand

The LOW survival finding contains substantive validation lessons; severity in an external report
does not set the priority of a confirmed wrong number in our module.

- Distinguish a changing covariate value from a time-varying coefficient such as `x + tt(x)`.
  UI labels, data requirements and explanatory text must describe the model actually fitted.
- `cmprsk::cuminc` accepts failure times/status, not a start-stop/subject-ID interface. Do not feed
  interval rows to it as independent subjects. With unsupported delayed-entry/counting-process
  data, suppress the test and explain why. Collapsing to the last row is not a general repair:
  it can discard entry times and change the estimand. This limitation follows from the documented
  interface; validate any alternative separately. [cmprsk manual, pp. 4–5](https://cran.r-project.org/web/packages/cmprsk/cmprsk.pdf)
- Test survival at the origin and before/at/after the first event. A cumulative-hazard step
  function needs a zero value before its first jump. Check plotting, bootstrap and table paths.
- For a baseline-standardised curve, average over the intended subjects and their defined
  baseline covariates, not over interval rows. Splitting an unchanged interval must not give
  that subject extra standardisation weight. Changing-covariate trajectories need their own
  defined target; do not silently replace them with baseline values.
- Carry subject identity through expanded risk sets and check the variance method actually used.
  A column titled Robust SE must contain that variance estimate. `finegray()` documents `id`
  for delayed-entry/time-dependent data; identify original subjects after its row expansion.
  [survival finegray documentation](https://stat.ethz.ch/R-manual/R-devel/library/survival/html/finegray.html)
- Seed threshold-CI bootstraps as well as model bootstraps. Cite the ROC method actually used;
  a method label and a resolvable citation key are not evidence that the citation supports it.

Event and censor levels must be explicit, distinct and visible in the result. Avoid substring
guesses, English-word heuristics and silent first-level defaults. An INFO notice makes an
automatic choice visible but does not establish that the clinical coding is correct. Preserve
our explicit-selection convention; when selection is incomplete, wait for input, and when an
explicit configuration is inconsistent, reject it with an actionable message.

## 5. Validate input bounds and UI dependencies in both layers

Check counts after QC/exclusion, not just the original selection. Use integer bounds and
`seq_len()`/`seq_along()`; `1:0` executes two iterations. If an interaction order exceeds the
remaining marker count, reject it or disclose a deliberately supported reduction. Handle zero
remaining markers before entering the loop. A UI lower bound alone does not validate a saved
analysis or a direct R call; validate in the backend too.

Use structural row keys, including variable and subgroup identity when needed. Never use a
p-value, a translated label or an empty display string as identity. Reset failed computations to
a numeric missing value (`NA_real_`/`NaN`); add significance only after checking that p is a finite
numeric scalar in [0,1]. Format missing values at the presentation boundary. Test consecutive
variables where the first succeeds and the next fails, equal p-values, and two failed tests.

Shared controls must remain available whenever **any** output consumes them: multiplicity can
affect HWE without association testing; a time horizon can affect CIF without KM. Verify every
option name and every List value used by an `enable:` expression. Nest child counts/seeds under
their parent, group advanced settings, and test toggling a feature off and back on. Disable a
control without overwriting the user's selection; event handlers are not a substitute for bounds
or backend validation. Preserve option names/values when changing labels to keep saved analyses
compatible.

Multiplicity tests must use the actual family of comparisons, including model selection within
markers; notes must describe implemented behaviour. Propagate alpha and shared QC/sex coding to
all displays, including ternary plots and population metrics. A LOW label does not excuse a wrong
denominator, degrees of freedom or significance threshold.

## 6. Test restoration and export as separate lifecycle paths

Prepare result titles, visibility and drawing data before rendering. A renderer must not prepare
another image or mutate unrelated tables/HTML: otherwise exporting the second image first changes
the answer. Store small drawing data, retain NULL guards and trace `requiresData` through helpers.

For dynamic image arrays, distinguish option/data-shape keys that can be recreated in `.init()`
from model-derived keys that need a restoration strategy. Use stable unique keys and prove that
the results tree and each item's state survive serialization. Parent-state fallback must use the
same key and handle a missing parent/key. Unconditional clearing must not destroy state needed
by a later render. A `.savePart` override from another module is **not** a reusable fix: first
reproduce the problem on the target jamovi/jmvcore build, inspect the native path, and prefer a
supported API or a minimal upstream bug report.

Verify independently: initial render, resize without `.run()`, protobuf restoration into a fresh
analysis, each array item exported first through `.createImage()`/the native export path, and
actual `.omv` reopen plus PNG/PDF export in jamovi. Test zero/one/multiple items and awkward keys.
Inspect the exported content: file existence or nonzero size does not prove it contains the plot.
An R lifecycle test and a desktop round trip are separate evidence; report either as unverified
if it was not performed. [Array API](https://dev.jamovi.org/api/array)

## 7. Reconcile proposed remedies before teaching them

| Report recommendation or claim | Lesson adopted here |
|---|---|
| Delete state-NULL guards | Distinguish stale-cache reuse guards in `.run()` from required NULL guards in renderers. Fix dependencies first. |
| Deterministically assign distinct event/censor levels | Prefer explicit clinical coding. Determinism and a notice do not establish correctness. |
| CV accuracy should be close to training accuracy | Use independently counted predictions; closeness is not a correctness invariant. |
| All plots export because no renderer reads private caches | Test restoration and native export separately, including arrays. |
| Override `.savePart` | Treat as an unverified, version-specific workaround until reproduced locally. |
| No compilation; no em dashes; English/Russian catalog parity | These are jYS project policies, not imported rules. Follow our build workflow and supported catalogs. Key equality does not prove translation quality or placeholder correctness. |
| Delete an unused import or `NAMESPACE` line | Trace current routing/callers, edit source import declarations and regenerate; never hand-edit generated `NAMESPACE`. |
| Use native notices everywhere | Preserve our current notice decisions. Validate native insertion and error propagation; do not infer a migration mandate. |

## 8. Finding classification and check coverage

Numbers follow the audit's `### [SEVERITY]` headings. **NEW** means a new explicit lesson in this
learning record; **KNOWN** means an existing local rule reinforced or refined. Both are external
lessons, not our RECURRED/REGRESSION/WRONG-DONE dispositions. Root-cause codes below describe the
local teaching/check gap, not an inferred commit history in jYS. No sibling defect counts were
measured in this documentation-only pass. Recipe numbers refer to sections above.

| Finding | Class / lesson status | Local gap | Check or existing rule |
|---|---|---|---|
| 1 HIGH | Class encoding, NEW | NO-CHECK | Recipe 3: exact confusion-matrix and permutation fixtures |
| 2 MEDIUM | Cache dependency completeness, KNOWN | NO-CHECK | Recipe 1; `check_clearwith` checks names only |
| 3 MEDIUM | Output row identity, KNOWN | Partial check | Recipe 2: extend existing focused tests to exact filtered ID/value mapping |
| 4 MEDIUM | OOB metric/provenance, NEW | NO-CHECK | Recipe 3: Brier and accuracy checked separately |
| 5 MEDIUM | Post-QC dimensions and counts, KNOWN | NO-CHECK | Recipe 5: zero/one marker, excessive order, zero count |
| 6 MEDIUM | Seed before selection, KNOWN | NO-CHECK | Recipe 3; `check_bare_set_seed` does not check ordering |
| 7 MEDIUM | Scores labelled probabilities, NEW | NO-CHECK | Recipe 3: domain, simplex and class-label checks |
| 8 MEDIUM | Failed p-values and row identity, KNOWN | NO-CHECK | Recipe 5: consecutive failure and duplicate-value fixtures |
| 9 MEDIUM | Time-varying coefficient label, NEW | NO-RULE | Recipe 4; estimator-to-label trace |
| 10 MEDIUM | Unsupported Gray test design, NEW | NO-CHECK | Recipe 4; start-stop rejection/suppression fixture |
| 11 MEDIUM | Guessed clinical event levels, KNOWN | NO-CHECK | Recipe 4; reorder and rename levels, GUI/R parity |
| 12 MEDIUM | Shared multiplicity control, NEW | NO-CHECK | Recipe 5; HWE enabled, association disabled |
| 13 MEDIUM | Detached counts/seeds/save controls, KNOWN | NO-CHECK | Recipe 5; UI parent/child transition checks |
| 14 MEDIUM | Flat UI and invalid List value, KNOWN | NO-CHECK | Recipe 5; parse names/values and inspect panel |
| 15 MEDIUM | Silent partial failures, KNOWN | NO-CHECK | Review guide §§8,16,25; force each failure path |
| 16 LOW | Unchanged release version, KNOWN | NO-CHECK | `check_versions` compares files, not change history; review release diff |
| 17 LOW | Surplus `requiresData`, KNOWN | Existing check | `check_requires_data`; trace helpers, then lifecycle test |
| 18 LOW | Heavy image state/refitting, KNOWN | Partial check | `check_state_payload`; measured serialization size and renderer trace |
| 19 LOW | Survival origin, subject weighting, variance, ID, seeded CI, citation, NEW | NO-CHECK | Recipe 4: separate numeric/design fixtures for all six subpoints |
| 20 LOW | Multiplicity, overstated note, df, alpha, QC and level fallback, NEW | NO-CHECK | Recipe 5; independent family/df/QC calculations and text trace |
| 21 LOW | Post-selection inference, leakage, probability meaning, AUC aggregation, softmax derivative, NEW | NO-CHECK | Recipe 3; fold isolation and finite-difference gradient checks where applicable |
| 22 LOW | Theme/palette and too few colours, KNOWN | Partial check | Review guide §23; light/dark render, >8 groups, no missing colours |
| 23 LOW | Unused packages/citations, KNOWN | Existing check | `check_unused_imports`, `check_refs`; routing and caller review |
| 24 LOW | Private package API/broad import, KNOWN | Manual check | Exported generic dispatch; source roxygen review, installed-namespace smoke |
| 25 LOW | Fragmented/unextractable translation, KNOWN | Partial check | Review guide §9; extractor plus non-English/pseudo-catalog execution |
| 26 LOW | Long methods/duplicated builders, KNOWN | Manual check | Behaviour-preserving extraction only within authorised scope |
| 27 LOW | Tutorial-length footnotes, KNOWN | Manual check | Keep numeric caveats; link longer teaching material |
| 28 LOW | Labels/casing/spelling, KNOWN | Partial check | Review guide §12; preserve option identifiers |
| 29 LOW | Panel structure/shared horizon, KNOWN | NO-CHECK | Recipe 5; test each consumer without the other consumers enabled |
| 30 LOW | Event handlers erase choices, NEW | NO-CHECK | Recipe 5; off/on restores selection |
| 31 LOW | Numeric CI stored as text, KNOWN | Partial check | Review guide §§6,21; numeric lower/upper columns and clipboard check |
| 32 LOW | Renderer changes sibling results, NEW | NO-CHECK | Recipe 6; render/export items in reverse order |
| 33 LOW | Error on untouched analysis, KNOWN | Manual check | Review guide §25; empty, partial and invalid states |
| 34 INFO | Dead helpers/unsafe lookup scope, KNOWN | Manual check | Review guide §11; trace callers before removal |
| 35 INFO | Citation source vs pinned remote, KNOWN | Manual check | Distinguish canonical citation from installed source/version |
| Additional remediation item | Array persistence/export, KNOWN | NO-CHECK | Recipe 6; reported fix unverified locally |

The gate and test paths named here are **coverage references, not new passing results**.
Existing focused examples include `tests/testthat/test-lassocox-output-rows.R`,
`test-timeinterval-output-column.R`, `test-multisurvival-output-clearwith.R`, and
`test-multisurvival-render-without-data.R`. Inspect and adapt them to the target; they do not
cover every analysis. No general detector currently proves row provenance, encoding, estimator
semantics, dependency completeness, clinical level choice or native export. These need executable
analysis-specific oracles/transition fixtures plus the relevant desktop checks; document that gap
instead of promoting a heuristic WARN to a universal FAIL.
