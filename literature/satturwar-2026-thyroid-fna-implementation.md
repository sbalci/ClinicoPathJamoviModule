# Thyroid FNA review: implementation record

Implemented 4 October 2026, following the
[approved plan](satturwar-2026-thyroid-fna-implementation-plan.md).

The [focused reevaluation](decisioncompare-one-vs-rest-review.md) identified unsafe implicit
one-versus-rest recoding. The follow-up fix (analysis 1.1.0) now requires explicit positive
and negative levels in every `decisioncompare` reference mode. Additional observed labels
stop calculation unless the user explicitly selects exclusion. A mapping/count table and
conditional-estimate warning replace the former pooling behavior. Historical calls without
negative mappings must be completed before recalculation; no implicit result migration occurs.

## Delivered behavior

| Analysis | Implementation |
|---|---|
| `decision` | Explicit reference interpretation, target/comparator definition, directional PPA/NPA/OPA, reverse conditional agreement, distinct symmetric agreement, numerators/denominators, exact independent-case intervals, neutral discordance tables and saved classifications |
| `decisioncompare` | The same comparator semantics; paired PPA/NPA/OPA differences on common cases, Newcombe intervals, McNemar tests with a stated Holm family, stratified estimates, and comparator bar/radar/heatmap plots |
| `pathagreement` | Separate availability and mapping ledgers, raw reference exclusions, optional agreement retaining nondiagnostic calls, long case×reader×modality input, matched fixed-reader contrasts, case/patient bootstrap, and ordinal/adequacy transitions |
| `jjwithinstats` | Optional mean/median paired changes, Hodges–Lehmann shift and positive-time geometric mean ratios, pointwise intervals, and existing paired-test/multiplicity results |
| Adjacent drafts | Gamma/log GEE, ordered waves and package QIC; crossed mixed-model intercepts/contrasts; supported-only CLMM output; paired-t planning; comparator-aware precision labels |

`referenceType = "legacy"` remains the default in the three existing comparison analyses.
It identifies an unspecified interpretation. `decisioncompare` now applies the explicit mapping
contract above in every mode. Explicit
consensus/method mode provides agreement outputs and suppresses disease-risk, prevalence
transport and Fagan reporting. Existing R argument names remain unchanged; selected `decisioncompare` variables
now require non-NULL negative levels, and new reference options have defaults. `diagnosticsamplesize` retains disease precision as its default.

The `decision`/`decisioncompare` engines remain binary. Their distinct existing recoding
policies are preserved and displayed. In particular, `decisioncompare` retains its
one-versus-rest behavior unless explicit negative selection and exclusion are requested.
No nondiagnostic row or fabricated false-positive/false-negative count enters either engine.

The reader workflow keeps determinate, nondiagnostic, missing and unmapped states separate.
Advanced input rejects unmapped ratings. All-observed estimates exclude missing readings,
require an eligible binary reference, and disclose their denominators. Long-input contrasts
use two prespecified modalities, equal fixed-reader weights and a complete matched grid;
per-reader available-case summaries stay separate. Bootstrap draws preserve all readings
within each sampled case or patient and retain repeated draws of the same sampling unit.
No p-value is inferred from a percentile interval.

## Numerical validation

The microscopy fixture verifies these different quantities independently:

| Quantity | Oracle |
|---|---:|
| Determinate reads | 251 |
| Nondiagnostic reads with eligible reference | 34 |
| Observed eligible reads | 285 |
| Determinate PPA / NPA | 108/171; 67/80 |
| Positive / negative agreement retaining nondiagnostic calls | 108/180; 67/105 |
| Reference-positive proportion among positive calls | 108/121 |

Tests cover reference/test reversal, unchanged symmetric agreement, zero denominators,
missing/unselected levels, legacy versus disease invariance, paired denominators and Holm
adjustment, aligned saved classifications, mode changes and plot export. Reader tests cover
all-nondiagnostic input, duplicate/inconsistent IDs, incomplete grids, unused ordinal
categories, clustered resampling, row ordering and invalid/constant intervals. Timing and
draft repairs are checked against independent formulas or the supporting R packages.

Focused checks passed: 47 comparator assertions, 47 new reader-study assertions including
the compiled analysis, 29 existing reader-data assertions, 22 existing reader-metric
assertions, 65 paired-timing assertions, and 62 adjacent-draft assertions. Targeted source
harnesses load the generated Options/Analysis/Results classes. The draft suite additionally
uses isolated method checks against independent package fits.

The package-loaded post-documentation run completed with **998 passing assertions,
zero failures, warnings or skips across seven files**: comparator (43), decision release
regressions (43), decision full checks (50), decisioncompare release regressions (102),
decisioncompare numerical validation (633), timing (65), and adjacent drafts (62).
The final comparator source rerun adds the explicit ambiguous-reference checks and
verifies clearing cached disease-table values, bringing that focused suite to 47.
The four existing binary regression files account for 828 passing compatibility checks.

The [coverage script](../inst/examples/pathagreement-reader-coverage.R) ran 200 simulations,
399 bootstrap draws per simulation, 60 patient clusters and three perfectly correlated
fixed readers. PPA, NPA and overall-difference coverage was 94.5%, 95.0% and 94.0%, with
Monte Carlo standard errors 1.61%, 1.54% and 1.68%. All 200 intervals were available for
each metric. These are checks of that design, not a guarantee for other designs; see the
[results](satturwar-2026-reader-coverage-check.csv).

## Build and release boundary

Headers and UI source were generated by the official jamovi compiler. The installed
application did not answer the version probe used by `jmvtools::prepare()`, including an
unsandboxed retry. Its plist reports 28.3.0.0, so generation used the compiler's supported
`--assume-app-version 28.3.0` flag. No headers were hand-edited. The compiler's trailing
whitespace in generated headers is retained; handwritten files pass whitespace checks.

`devtools::document()` regenerated the eight changed manual entries. It completed with
environment warnings about a pre-existing glmmTMB/TMB build-version mismatch and the
unavailable XQuartz display; those packages/display paths are not used by the new
comparator and reader estimators. All nine modified analyses' R sources, generated
headers and YAML schemas parse.

These analyses remain in testing/draft routes. `jjwithinstats` moved to `JJStatsPlotT` under
the repository's modified-analysis policy. No module installation or release promotion
was performed. Native save/reopen/protobuf validation is unavailable because `RProtoBuf`
is absent from this R environment. Focused lifecycle checks exercise generated classes,
option changes, state clearing, and saved-row alignment; they do not substitute for native
saved-document reopening. A full module release check and pkgdown site build are still
required before promotion.

Random-reader MRMC inference, incomplete-grid estimation and reader-study simulation power
planning remain future work. McNemar planning cannot be calculated from the current marginal
proportion inputs; it is explicitly unavailable pending discordance inputs and validation.
The [adjacent draft repair report](satturwar-2026-thyroid-fna-adjacent-draft-repairs.md)
records the bounded repairs and remaining draft debt.

## Teaching material

- [Reference, nondiagnostic and reader-study guide](../vignettes/meddecide-reader-comparator-studies.md)
- [Synthetic reader-study example](../inst/examples/pathagreement-reader-study.R)
- [Paired timing effects guide](../vignettes/jjstatsplot-paired-timing-effects.md)

The article's aggregate counts are arithmetic fixtures only. Individual example records
are synthetic, not reconstructed patient data.
