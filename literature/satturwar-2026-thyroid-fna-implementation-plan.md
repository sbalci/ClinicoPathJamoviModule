# Implementation plan: comparator agreement and reader studies

Status: implemented in testing/draft routes on 4 October 2026; native saved-analysis
reopening and release promotion remain outstanding. See the
[implementation record](satturwar-2026-thyroid-fna-implementation.md) for validation and limits.
Follow-up on 4 October 2026: the user's explicit-positive-and-negative requirement supersedes
phase 0's original compatibility policy for `decisioncompare`. Analysis 1.1.0 requires both
levels in every reference mode and rejects extra observed levels unless exclusion is selected.
Historical configurations needing new selections stop before recalculation.

Prepared 4 October 2026. Builds on the
[article review](satturwar-2026-thyroid-fna-citation-review.md).

## Architecture decision

**Keep `decision` and `decisioncompare` as binary test versus binary reference analyses.**
Their existing positive/negative selectors and 2 × 2 calculation paths remain intact.
Add comparator interpretation to those functions first. Implement nondiagnostic-result
analysis as a separate, opt-in extension of the `pathagreement` reader workflow.

The previous review's suggestion to propagate a 3 × 2 workflow into the diagnostic
wrappers is superseded by this plan. A three-outcome table remains a useful mathematical
description, but it will not become the matrix consumed by the existing binary engine.

| Concern | Decision |
|---|---|
| `goldPositive` / `goldNegative` identify two reference states | Preserve names, types, level selection and eligible reference states |
| `testPositive` / `testNegative` identify two test calls | Preserve selection and the ordinary determinate-only 2 × 2 analysis |
| Nondiagnostic call is neither positive nor negative | Keep it in a separate status field/count ledger in the advanced reader workflow |
| Unknown reference classification | Exclude from reference-conditioned metrics; report the exclusion separately |
| Bethesda III+ comprises several original categories | Use a documented binary recode for the reference and existing ordinal-cutoff logic for reader ratings; do not turn the current single-level selectors into multi-level selectors |
| Users need agreement with consensus | Add reference interpretation and appropriate labels without changing the matrix dimensions |
| Multireader/multimodality data | Extend the existing reader workflow and case/cluster resampling |

### Why altering the binary table is unsafe

`decision` explicitly checks for a 2 × 2 table in
[`R/decision.b.R`](../R/decision.b.R) around line 769. It extracts TP/FP/FN/TN around
line 1871 and reuses them for multiple outputs. Those outputs include predictive values,
likelihood ratios, prevalence adjustment, Fagan plots and saved classifications.

For the article's microscopy counts, the legitimate determinate table is:

| Test call | Reference positive | Reference negative |
|---|---:|---:|
| Positive | 108 | 13 |
| Negative | 63 | 67 |

Separately, there are 9 nondiagnostic calls among reference-positive reads and 25 among
reference-negative reads. Adding them to FN and FP would give the desired all-eligible
rates 108/180 and 67/105, but would falsely change positive-call agreement from
108/121 = 89.26% to 108/146 = 73.97%. The additional 25 records are not positive calls.

Likewise, with nondiagnostic results present, the positive-call rate in reference-negative
cases is not necessarily `1 - negative-call agreement`. The usual binary likelihood-ratio
identities cannot be carried over by replacing sensitivity and specificity alone.

**Implementation invariant:** never create artificial FP/FN counts, insert an abstention
row into `epiR::epi.tests()`, or overwrite ordinary binary metrics with values calculated
using broader denominators.

## Data and output contracts

The advanced reader preparation layer will preserve two independent concepts:

```text
reference_call   positive | negative | unavailable
reference_state  eligible | missing | excluded_level
test_call        positive | negative | unavailable
test_state       determinate | nondiagnostic | missing | unmapped
```

`unavailable` is an internal missing call value, not a third reference class. Raw values,
original row IDs, case ID, reader ID and modality remain available to explain exclusions.
An unrecognized test label is not automatically nondiagnostic. In explicit advanced mode,
it triggers a mapping error; the user must map or deliberately exclude it.

Three output groups keep the interpretation visible:

1. **Binary comparison:** current 2 × 2 counts and estimates on determinate eligible pairs.
2. **Result availability:** counts of determinate, nondiagnostic and missing readings,
   split by the two reference states; reference exclusions reported separately.
3. **Agreement including nondiagnostic calls:** optional estimates with numerator,
   denominator and explicit population definition. No binary LR, Fagan, Youden or
   disease-risk calculations are derived from this group.

Thus the interface need not present a new 3 × 2 diagnostic table. The status table and
the binary table preserve the same information while keeping their calculation paths distinct.

For nonmissing observed readings with an eligible reference, define
`a = positive/positive`, `b = positive/negative`, `c = negative/positive`,
`d = negative/negative`, `u_pos` and `u_neg` as the nondiagnostic counts.

| Output | Formula | Population |
|---|---|---|
| Determinate PPA | `a / (a + c)` | Reference-positive, determinate readings |
| Determinate NPA | `d / (b + d)` | Reference-negative, determinate readings |
| Positive-call agreement, nondiagnostic retained | `a / (a + c + u_pos)` | Reference-positive observed readings |
| Negative-call agreement, nondiagnostic retained | `d / (b + d + u_neg)` | Reference-negative observed readings |
| Definitive agreement, nondiagnostic retained | `(a + d) / (a + b + c + d + u_pos + u_neg)` | Observed readings with eligible reference |
| Determinate-result yield | `(a + b + c + d) / (a + b + c + d + u_pos + u_neg)` | Same observed-reading population |

Missing readings are counted separately and excluded from those estimands. The UI must
say “observed readings” rather than imply every enrolled case contributed. A future
all-enrolled failure policy would be a distinct, explicit analysis, not a hidden imputation.

In disease-reference mode, the ordinary sensitivity/specificity table remains conditional
on determinate results. The extra section uses the explicit all-observed agreement labels
above; it does not silently redefine the familiar binary statistics.

## Work packages

### 0. Lock down the existing binary contracts

**Size:** small. **Dependency:** none.

- Preserve `gold`, `goldPositive`, `goldNegative`, `newtest`, `testPositive`, `testNegative`
  and all `decisioncompare` test selectors.
- Record current behavior for two-level factors, explicit selected levels in multilevel
  factors, missing values, zero cells, prevalence adjustment and saved classifications.
- **Superseded by the follow-up fix:** `decisioncompare` now uses explicit positive/negative
  mappings in every mode. Other observed levels require explicit exclusion; missing negative
  mappings stop analysis. Previously implicit one-versus-rest calls require user selections.
- Extend existing `test-decision-release-review.R`, `test-decision-full-check.R`,
  `test-decisioncompare-release-review.R` and argument tests where necessary.

**Acceptance:** identical estimates for explicitly mapped binary data. Argument names remain
unchanged, but selected `decisioncompare` variables now require non-NULL negative levels.

### 1. Add comparator interpretation to existing binary analyses

**Targets:** `decision`, `decisioncompare`, `pathagreement`. **Size:** medium.
**Dependency:** package 0.

| Surface | Planned change |
|---|---|
| `.a.yaml` | Add `referenceType` List (`legacy`, `disease`, `consensus`, `method`) and optional `referenceDescription` / `targetDefinition` String options |
| Backend | Centralize metric definitions and user-facing labels in a shared `utils-diagnostic-comparator.R`; preserve numerical return keys where needed for compatibility |
| `.r.yaml` | Add target/comparator/positivity/population definition output; expose numerator/denominator for agreement estimates |
| `.u.yaml` | Add “Reference interpretation” before metric choices; use neutral “Reference positive/negative level” labels without renaming option identifiers |
| Reporting | Update tables, CI tables, plots, footnotes, notices, welcome/help text, manuscript paragraphs, case labels and generated R calls together |

Compatibility policy: `legacy` is the serialized default and the generated R argument
has a default. Its display label is “Not specified”; a visible note identifies that
the interpretation has not been declared. Existing calculations remain compatible.
Explicit consensus/method mode selects agreement terminology and suppresses clinical
post-test-risk, prevalence-transport and Fagan interpretations. Explicit disease mode
retains the established diagnostic outputs. Do not guess reference type from column names.

Directional PPA/NPA and symmetric PSA/NSA remain separate metrics. In explicit comparator
mode, TP/FP/FN/TN descriptions become positive/negative matches and discordances; saved
classifications must follow the declared mode rather than implying known disease truth.
Legacy classification values remain supported for old analyses.

**Acceptance:** swapping comparator and test changes directional agreement appropriately,
while symmetric agreement remains unchanged; no disease-risk prose in explicit comparator
mode; disabling disease-only outputs also clears previously populated results.

### 2. Add nondiagnostic status accounting to `pathagreement`

**Size:** medium. **Dependency:** packages 0–1.

Keep the standard reader workflow unchanged until the user enables the new option.
Start with one explicit nondiagnostic label, rather than adding a new `Level` argument
or an ambiguous delimited list of labels. Multiple raw codes can be recoded to a common
label before analysis.

| Surface | Planned change |
|---|---|
| `jamovi/pathagreement.a.yaml` | Add `readerNondiagnostic = FALSE`, `readerNondiagnosticLevel = ''`, and `readerAllObservedAgreement = FALSE`; optional values use Bool/String defaults |
| `R/pathagreement-reader-data.R` | Preserve raw labels and status before missing-value filtering; remove the declared nondiagnostic category before applying ordinal cutoffs; retain existing binary reference selection |
| New `R/pathagreement-reader-status.R` | Pure status/count/population helpers with distinct determinate and all-observed masks |
| `R/pathagreement-reader-metrics.R` | Keep `.paReaderAccuracy()` semantics; introduce separate named statistics for status-aware agreement |
| `R/pathagreement-reader-report.R` | Fill availability and optional extended-agreement tables; do not replace binary estimates |
| `.r.yaml` | Add `readerAvailability` and `readerObservedAgreement` tables, including reader, population, numerator, denominator, estimate, CI and method |
| `.u.yaml` | “Nondiagnostic results” panel with exact label, observed values/mapping preview and optional all-observed summary |

Do not add third reference or disease levels. A reference outside the chosen positive
and negative levels remains excluded. Preserve its raw value in the exclusion report.

For the thyroid example, prepare the reference as II → negative and III–VI → positive;
I remains an explicitly excluded reference label. Reader ratings retain the original
categories: mark I as nondiagnostic, then apply the III cutoff to the remaining ordered
categories. Histology uses its separate benign/malignant reference and a separately
declared positivity threshold. No thyroid-specific recoding is applied automatically.

**Acceptance:** microscopy produces the ordinary 251-read determinate table plus 34
nondiagnostic reads and 285 observed eligible reads. The optional rates are 108/180
and 67/105; the ordinary determinate rates remain 108/171 and 67/80. All-nondiagnostic
data can populate availability and extended agreement even if the ordinary binary table
is unavailable. Missing values and unknown labels must never increase nondiagnostic counts.

### 3. Add fixed-reader paired modality comparisons

**Target:** `pathagreement`. **Size:** large. **Dependency:** packages 1–2.

- Add optional long input with `readerID`, `modality`, and `rating`; reuse `caseID`,
  reference and patient-cluster options. Keep the current wide reader input supported.
- Validate one record per case × reader × modality, a case-consistent reference, and
  nonempty IDs. Reject ambiguous duplicates instead of choosing the first observation.
- Begin with a complete matched grid and equal fixed-reader weights. Report incomplete
  cases excluded from comparison; retain per-reader available-case summaries separately.
  A broader incomplete-grid estimator is a later extension with its own weighting policy.
- Select the comparison population before resampling. For determinate-only contrasts,
  require determinate calls in all selected comparison cells. For all-observed contrasts,
  allow declared nondiagnostic calls but require recorded readings in every cell.
- Reuse `.readerResample()` in `R/utils-reader-resampling.R`. Resample whole cases or
  patients, keeping all readers and modalities together and calculating contrasts in
  the same draw. Preserve replicated sampling units; never deduplicate drawn case IDs.
- Use the resampler directly for pooled contrasts. The existing per-reader report code
  substitutes exact binomial intervals for some independent-case proportions; that
  substitution must not overwrite a clustered pooled estimate or contrast interval.
- Add reader-by-modality tables and paired percentage-point differences with their
  populations, case/read counts, CI method and fixed-reader scope. Do not manufacture
  p-values from percentile intervals. Initially offer a prespecified contrast with CI;
  exploratory multiple contrasts are identified as such until a validated simultaneous
  inference method is added.

**Acceptance:** correlation-preserving simulation checks; row-order invariance; case and
patient bootstrap; duplicate IDs; incomplete grids; different abstention rates; zero
denominators; invalid/constant bootstrap draws; single-reader agreement with the existing
paired calculations. Statistical simulation coverage is required before promotion.

**Boundary:** no random-reader generalization is claimed. Random-reader MRMC inference
is a later method-validation project, not a checkbox backed by case bootstrapping.

### 4. Add timing effect tables

**Target:** `jjwithinstats`. **Size:** medium. **Dependency:** independent of packages 2–3.

- `.a.yaml`: optional paired effects, effect definition, CI method and bootstrap settings.
- `.b.R`: mean paired change, median paired change or Hodges–Lehmann shift, each accurately
  labeled; preserve existing paired tests and multiplicity settings. Offer time ratios
  only for positive times with a specified estimand.
- `.r.yaml`: contrast, number of pairs, units, estimate, CI, method and adjusted p-value
  where the existing paired test supports it.
- `.u.yaml`: “Paired changes” controls near existing comparisons.
- Test known shifts, ties, missing pairs, all-zero changes, skew and deterministic seeds.

One row must still be an independent case. Do not stack the three readers and treat their
rows as independent cases. Joint timing inference across readers can subsequently reuse
the validated case-level resampling design from package 3. Mixed models remain a separate
extension and cannot identify modality effects independently of perfectly confounded period.

### 5. Add category-transition reporting and educational examples

**Targets:** `agreement` / `pathagreement`, relevant vignette. **Size:** medium.

- Add ordered baseline/comparison roles, separate adequacy status and explicit threshold.
- Reuse existing paired agreement bootstrap and directional-discordance infrastructure;
  add unchanged/up/down and threshold-crossing counts with clear denominators.
- Preserve all declared categories, including an unused category V. Do not place Bethesda
  I at the bottom of an ordinal disease-severity scale.
- Provide a synthetic case × reader × modality example and the existing article count
  fixture. Label synthetic records clearly; never expand the article's aggregate tables
  into purported patient-level data for inference.
- Explain reference meaning, binary selection, nondiagnostic status, fixed readers,
  and the difference between a threshold change and a comparator change.

### 6. Repair adjacent drafts and add design planning separately

These are follow-up tasks, not prerequisites for comparator labels or status accounting:

- `geemodel`: align Gamma/log labeling and family; wire valid ordering/waves; test directly
  against package fits before using it for timing ratios.
- `mixedmodelanova`: explicit crossed random-effects construction and checked contrasts.
- `ordinalmixedmodel`: replace unsupported CLMM diagnostics/predictions or remove their
  advertised outputs until supported.
- `clinicaltrialdesign`: implement/validate the missing paired/McNemar helpers before
  counting those options as available power analyses.
- `diagnosticsamplesize`: comparator-aware single-proportion precision labels first;
  reader-study simulation planning and random-reader MRMC inference later, with case
  counts, reader counts, correlation assumptions and Monte Carlo uncertainty explicit.

## Delivery sequence and release criteria

| Change set | Contents | Completion evidence |
|---|---|---|
| A | Binary contract fixtures + comparator interpretation | Existing binary numbers preserved; labels/clinical outputs correct in every mode |
| B | Opt-in reader status accounting + article fixture | Correct masks, denominators and separate outputs; no artificial binary recoding |
| C | Fixed-reader modality contrasts | Paired identity preserved; clustering simulations and failure reporting validated |
| D | Timing effects | Direct package/reference checks and interpretable effect definitions |
| E | Category transitions + teaching vignette | Correct adequacy handling, direction and empty-category behavior |
| F | Draft repairs, reader-population inference and planning | Separate function-specific review and method validation |

No new statistical dependency is required for A–B. C can reuse the current base-R/`withr`
resampling machinery. D can use existing paired-test and bootstrap dependencies. Verify
actual module dependencies through the updater's dependency resolution when shipping.

For each source change, read the corresponding fix/create/release playbook and relevant
YAML/backend guides; regenerate `.h.R` and documentation through tooling; run the focused
tests, `jmvtools::prepare()`, and lifecycle checks for option changes, resize/export and
saved-analysis reopening. `clearWith` and plot state must include all new interpretation,
population and mapping options. Clinical output toggles must not leave stale results when
the reference mode changes. Keep source ownership and test routing rules from `CLAUDE.md`.

The relevant analyses are currently parked in testing or draft groups except production
`jjwithinstats`. Retain their routing during implementation and run release gates before
promotion. No new File/Text option or application-version increase is needed for this plan.

## Implementation record

Change sets A–E and the bounded draft repairs in F have been implemented. The binary
decision engines retain their existing dichotomous selectors and filtering rules.
Nondiagnostic accounting and paired modality inference live in the opt-in reader workflow.
Random-reader MRMC inference, reader-study simulation power planning, and an incomplete-grid
estimator remain separate method-development projects as specified above. This work does
not promote any new feature to a released production analysis.
