# SYSTEMATIC CHECK: `pathagreement`

**Status**: RESOLVED IN WORKING TREE  
**Priority**: High  
**Audit date**: 2026-09-11  
**Resolution date**: 2026-09-11

## RESOLUTION

The original audit findings are retained below as pre-fix evidence. The follow-up
repair addressed every detected critical, integration, and code-quality issue:

- Conger's exact kappa now supplies a scalar missing p-value and the clinical
  summary/report render it as unavailable instead of aborting.
- `enhancedErrorGuidance` and the earlier progress-indicator option are absent
  from the schema, generated wrapper, documentation, and UI. Calculation
  failures always provide actionable guidance.
- The Analysis Messages panel remains because it carries user-facing errors,
  warnings, and explanatory information. Each entry now has an explicit
  `error`, `strong_warning`, `warning`, or `info` label. It does not emit runtime
  progress announcements.
- Metadata rows now require a selected case ID, and metadata-only values are
  removed from every rater's shared factor-level union.
- ICC and Krippendorff help now describe the implemented data scope. PABAK,
  Gwet AC1/AC2, weighting, sample-size, and Landis-Koch caveats were corrected.
- Opaque report backgrounds, fixed dark body text, malformed HTML, untranslated
  user-facing sentences, title-case checkbox labels, and obsolete commented
  implementations were repaired. The theme checker now detects
  `background: white` shorthand.

Verification after repair:

- `jmvtools::prepare()` completed cleanly with jamovi 28.2.0.
- 196 focused `pathagreement` expectations passed; one saved-state test skipped
  because `RProtoBuf` is unavailable on the main R library path.
- The repository results-rendering contract passed 6/6 checks.
- State guards, theme-safe HTML, duplicate UI names, reference years, syntax,
  YAML parsing, and pathagreement-scoped `git diff --check` all passed.

## QUICK SUMMARY

The counts and failure descriptions in this section record the original audit
before repair. The current schema has 54 entries (`data` plus 53 analysis
options); the failure-only `enhancedErrorGuidance` option was removed.

- **Arguments**: 55 schema entries (`data` plus 54 analysis options) → 55/55 are read by the wrapper/backend or used as data. Paired valid-data runs found an observable result or pixel change for 54/55. `enhancedErrorGuidance` only changes caught-error text, so it was non-effective in valid runs.
- **Outputs**: 50 top-level result items plus one nested table → 51/51 populated in at least one applicable runtime scenario. No declared output is dead or permanently hidden.
- **Error handling and notices**: Fatal validation uses actionable `jmvcore::reject()` calls. Non-fatal conditions are shown to users in the always-declared `warnings` HTML result. There are no console `message()`/`warning()` progress announcements and no `showProgressIndicators` option. The remaining “Setup Progress” text is instructional welcome content, while framework checkpoints are private. The HTML message panel does not encode ERROR/STRONG_WARNING/WARNING/INFO severity.
- **Integration quality**: Core schema wiring, output lifecycle, plot state, references, and package declarations are strong. A valid combination, `exct = TRUE` with `showClinicalSummary = TRUE`, aborts after producing the kappa row because Conger's result has no p-value and the summary applies `if (is.na(p_value))` to a zero-length value.
- **External comparison**: Not run. `check_external` defaulted to `false`, and no CRAN package, GitHub repository, or upstream function was supplied.

## ARGUMENT BEHAVIOR MATRIX

The harness ran each entry twice with valid supporting options/data. It compared serialized result protobufs and, for visual-only controls, rendered PNG hashes. All 110 paired analyses completed. The later combination audit separately detected the Conger-summary crash.

| Argument | Default → changed value | Observed result change | Effective? | Evidence |
|---|---|---|:---:|---|
| `data` | ratings A → modified ratings | `overviewTable`, `kappaTable` | YES | Runtime digest; data preparation at `R/pathagreement.b.R:465` |
| `vars` | 4 raters → 5 raters | `overviewTable`, `kappaTable` | YES | Runtime digest; selection at `R/pathagreement.b.R:465` |
| `sft` | `false` → `true` | `raterFrequencyTables` | YES | Runtime digest; gate at `R/pathagreement.b.R:352` |
| `heatmap` | `false` → `true` | `heatmapPlot` | YES | Runtime digest; renderer at `R/pathagreement.b.R:2175` |
| `heatmapDetails` | `false` → `true` | `heatmapPlot` pixels | YES | Rendered pixel digest; renderer at `R/pathagreement.b.R:2175` |
| `heatmapTheme` | `viridis` → `cividis` | `heatmapPlot` pixels | YES | Rendered pixel digest; renderer at `R/pathagreement.b.R:2175` |
| `wght` | `unweighted` → `squared` | `kappaTable` | YES | Runtime digest; Cohen weighting at `R/pathagreement.b.R:658` |
| `exct` | `false` → `true` | `overviewTable`, `kappaTable` | YES | Runtime digest; Fleiss/Conger branch at `R/pathagreement.b.R:818` |
| `multiraterMethod` | `auto` → `cohen` | `warnings`, `overviewTable`, `kappaTable` | YES | Runtime digest; method dispatch at `R/pathagreement.b.R:580` |
| `fleissCI` | `true` → `false` | `kappaTable` | YES | Runtime digest; CI gate at `R/pathagreement.b.R:820` |
| `kripp` | `false` → `true` | `krippTable` | YES | Runtime digest; gate at `R/pathagreement.b.R:268` |
| `krippMethod` | `nominal` → `ordinal` | `krippTable` | YES | Runtime digest; calculation at `R/pathagreement.b.R:939` |
| `consensus` | `false` → `true` | `consensusSummary` | YES | Runtime digest; gate at `R/pathagreement.b.R:274` |
| `consensus_method` | `majority` → `super_majority` | `consensusSummary` | YES | Runtime digest; calculation at `R/pathagreement.b.R:1011` |
| `tie_breaking` | `exclude` → `global_mode` | `consensusTable`, `consensusSummary` | YES | Runtime digest; calculation at `R/pathagreement.b.R:1011` |
| `show_consensus_table` | `false` → `true` | `consensusTable` | YES | Runtime digest; population at `R/pathagreement.b.R:1076` |
| `showClinicalSummary` | `false` → `true` | `clinicalSummary`, `reportTemplate` | YES | Runtime digest; gate at `R/pathagreement.b.R:369` |
| `showAboutAnalysis` | `false` → `true` | `aboutAnalysis` | YES | Runtime digest; generator at `R/pathagreement.b.R:3489` |
| `showAssumptions` | `false` → `true` | `assumptions` | YES | Runtime digest; generator at `R/pathagreement.b.R:3530` |
| `showWeightedKappaGuide` | `false` → `true` | `weightedKappaGuide` | YES | Runtime digest; generator at `R/pathagreement.b.R:3577` |
| `showStatisticalGlossary` | `false` → `true` | `statisticalGlossary` | YES | Runtime digest; generator ends at `R/pathagreement.b.R:3756` |
| `styleDistanceMetric` | `agreement` → `correlation` | Seven clustering tables/plots | YES | Runtime digest; distance selection at `R/pathagreement.b.R:1432` |
| `raterCharacteristics` | `false` → `true` | `diagnosticStyleTable`, `styleSummaryTable` | YES | Runtime digest; metadata use at `R/pathagreement.b.R:1547` |
| `identifyDiscordantCases` | `false` → `true` | `discordantCasesTable` | YES | Runtime digest; population at `R/pathagreement.b.R:1572` |
| `caseID` | `NULL` → `id` | `caseDifficultyTable`, `difficultyPlot` labels | YES | Runtime digest; retained-row labeling near `R/pathagreement.b.R:1876` |
| `icc` | `false` → `true` | `iccTable` | YES | Runtime digest; population at `R/pathagreement.b.R:890` |
| `bootstrap` | `false` → `true` | `krippTable` CI | YES | Runtime digest; bootstrap near `R/pathagreement.b.R:967` |
| `bootstrapSamples` | `1000` → `100` | `krippTable` CI | YES | Runtime digest; bootstrap near `R/pathagreement.b.R:967` |
| `seed` | `42` → `7` | `krippTable` CI | YES | Runtime digest; seeded bootstrap near `R/pathagreement.b.R:967` |
| `pairwiseAnalysis` | `false` → `true` | `pairwiseTable`, `pairwisePlot` | YES | Runtime digest; population at `R/pathagreement.b.R:1936` |
| `categoryAnalysis` | `false` → `true` | `categoryTable`, `categoryPlot` | YES | Runtime digest; population at `R/pathagreement.b.R:2010` |
| `outlierAnalysis` | `false` → `true` | `outlierTable` | YES | Runtime digest; population at `R/pathagreement.b.R:2083` |
| `pathologyContext` | `false` → `true` | `outlierTable`, `diagnosticAccuracyTable`, `confusionMatrixPlot` | YES | Runtime digest; reference metrics at `R/pathagreement.b.R:2162` |
| `gwetAC` | `false` → `true` | `gwetACTable` | YES | Runtime digest; population at `R/pathagreement.b.R:3791` |
| `pabak` | `false` → `true` | `pabakTable` | YES | Runtime digest; population at `R/pathagreement.b.R:3914` |
| `sampleSizePlanning` | `false` → `true` | `sampleSizeTable` | YES | Runtime digest; population at `R/pathagreement.b.R:4036` |
| `targetKappa` | `0.8` → `0.6` | `sampleSizeTable` | YES | Runtime digest; calculation at `R/pathagreement.b.R:4036` |
| `targetPrecision` | `0.1` → `0.2` | `sampleSizeTable` | YES | Runtime digest; calculation at `R/pathagreement.b.R:4036` |
| `raterBiasAnalysis` | `false` → `true` | `raterBiasTable`, `biasPlot` | YES | Runtime digest; population at `R/pathagreement.b.R:4132` |
| `agreementTrendAnalysis` | `false` → `true` | `agreementTrendTable`, `trendPlot` | YES | Runtime digest; population at `R/pathagreement.b.R:4264` |
| `caseDifficultyScoring` | `false` → `true` | `caseDifficultyTable`, `difficultyPlot` | YES | Runtime digest; population at `R/pathagreement.b.R:4438` |
| `agreementStabilityAnalysis` | `false` → `true` | `stabilityTable` | YES | Runtime digest; population at `R/pathagreement.b.R:4553` |
| `performClustering` | `false` → `true` | Eleven clustering outputs | YES | Runtime digest; gate at `R/pathagreement.b.R:332` |
| `clusteringMethod` | `ward` → `complete` | Ten clustering outputs | YES | Runtime digest; clustering near `R/pathagreement.b.R:5195` |
| `nStyleGroups` | `3` → `2` | Ten clustering outputs | YES | Runtime digest; group count at `R/pathagreement.b.R:1814` |
| `autoSelectGroups` | `false` → `true` | Ten clustering outputs | YES | Runtime digest; clustering near `R/pathagreement.b.R:5195` |
| `showClusteringHeatmap` | `true` → `false` | Two clustering heatmaps | YES | Runtime digest; plot-state gate at `R/pathagreement.b.R:143` |
| `heatmapColorScheme` | `diagnostic` → `viridis` | `diagnosticStyleHeatmap` pixels | YES | Rendered pixel digest; renderer at `R/pathagreement.b.R:2605` |
| `identifyDiscordant` | `false` → `true` | `discordantCasesCluster` | YES | Runtime digest; population at `R/pathagreement.b.R:5424` |
| `discordantThreshold` | `0.5` → `0.3` | `discordantCasesCluster` | YES | Runtime digest; population at `R/pathagreement.b.R:5424` |
| `referenceStandard` | `NULL` → `reference` | `categoryTable`, `referenceComparison`, seven clustering plots | YES | Runtime digest; aligned reference at `R/pathagreement.b.R:2032` |
| `useMetadataRows` | `false` → `true` | `warnings`, `overviewTable`, `kappaTable` | YES | Runtime digest; extraction at `R/pathagreement.b.R:475` |
| `showInlineComments` | `false` → `true` | `inlineComments` | YES | Runtime digest; gate at `R/pathagreement.b.R:399` |
| `showClusteringInterpretation` | `false` → `true` | `clusteringInterpretation` | YES | Runtime digest; population at `R/pathagreement.b.R:5689` |
| `enhancedErrorGuidance` | `true` → `false` | No change on valid data; only caught-error branches read it | **NO in valid runs** | Reads at `R/pathagreement.b.R:3827`, `3936`, `4058`, `4153`, `4285`, `4460`, `4580` |

## OUTPUT POPULATION MATRIX

The audit used comprehensive five-rater, two-rater, metadata, and no-selection scenarios. “YES” means the item had non-empty content, rows, or image state in at least one applicable run.

| Output | Type | Setter/renderer | Visibility rule | Populated? | Runtime evidence |
|---|---|---|---|:---:|---|
| `todo` | Html | `setContent()` at `R/pathagreement.b.R:3177` | Welcome/insufficient selection | YES | 3,188 characters in welcome run |
| `warnings` | Html | `setContent()` at `R/pathagreement.b.R:423` | Hidden when empty | YES | 356–1,171 characters in warning scenarios |
| `overviewTable` | Table | `setRow()` at `R/pathagreement.b.R:568` | Analysis run | YES | 1 row |
| `kappaTable` | Table | Method setters from `R/pathagreement.b.R:580` | Analysis run | YES | 1 or pairwise rows |
| `iccTable` | Table | `setRow()` from `R/pathagreement.b.R:890` | `icc` | YES | 1 row |
| `pairwiseTable` | Table | `setRow()` from `R/pathagreement.b.R:1936` | `pairwiseAnalysis` | YES | 1–10 rows |
| `categoryTable` | Table | `setRow()` from `R/pathagreement.b.R:2010` | `categoryAnalysis` | YES | 3 rows |
| `outlierTable` | Table | `setRow()` from `R/pathagreement.b.R:2083` | `outlierAnalysis` | YES | 9–36 rows |
| `diagnosticAccuracyTable` | Table | `setRow()` from `R/pathagreement.b.R:2162` | `pathologyContext` | YES | 2–5 rows |
| `diagnosticStyleTable` | Table | `setRow()` from `R/pathagreement.b.R:1378` | `performClustering` | YES | 5 rows |
| `styleSummaryTable` | Table | `setRow()` from `R/pathagreement.b.R:1511` | `performClustering` | YES | 2 rows |
| `discordantCasesTable` | Table | `setRow()` from `R/pathagreement.b.R:1572` | `identifyDiscordantCases` | YES | 15 rows |
| `krippTable` | Table | `setRow()` from `R/pathagreement.b.R:939` | `kripp` | YES | 1 row |
| `consensusTable` | Table | `setRow()` from `R/pathagreement.b.R:1076` | `consensus && show_consensus_table` | YES | 60 rows |
| `consensusSummary` | Table | `setRow()` from `R/pathagreement.b.R:1011` | `consensus` | YES | 8 rows |
| `heatmapPlot` | Image | state plus renderer at `R/pathagreement.b.R:2175` | `heatmap` | YES | Non-null state; rendered in differential checks |
| `pairwisePlot` | Image | state plus renderer at `R/pathagreement.b.R:2272` | `pairwiseAnalysis` | YES | Non-null state |
| `categoryPlot` | Image | state plus renderer at `R/pathagreement.b.R:2318` | `categoryAnalysis` | YES | Non-null state |
| `confusionMatrixPlot` | Image | state plus renderer at `R/pathagreement.b.R:2359` | `pathologyContext` | YES | Non-null state |
| `diagnosticStyleDendrogram` | Image | state plus renderer at `R/pathagreement.b.R:2449` | `performClustering` | YES | Non-null state |
| `diagnosticStyleHeatmap` | Image | state plus renderer at `R/pathagreement.b.R:2605` | clustering + heatmap | YES | Non-null state; pixel-tested |
| `diagnosticStyleCombined` | Image | state plus renderer at `R/pathagreement.b.R:2764` | `performClustering` | YES | Non-null state |
| `raterFrequencyTables` | Group | visibility/population at `R/pathagreement.b.R:354` | `sft` | YES | Filled group |
| `raterFrequencyTables$frequencyTable` | Table | `setRow()` from `R/pathagreement.b.R:1646` | Parent `sft` group | YES | 15 rows on clean five-rater data |
| `crosstabTable` | Table | `setRow()` from `R/pathagreement.b.R:1671` | `sft`; two raters only | YES | 3 rows in two-rater run |
| `clinicalSummary` | Html | `setContent()` at `R/pathagreement.b.R:3331` | `showClinicalSummary` | YES | About 1,500 characters when compatible |
| `reportTemplate` | Html | `setContent()` at `R/pathagreement.b.R:3462` | `showClinicalSummary` | YES | About 2,600 characters when compatible |
| `aboutAnalysis` | Html | `setContent()` at `R/pathagreement.b.R:3527` | `showAboutAnalysis` | YES | 2,263 characters |
| `assumptions` | Html | `setContent()` at `R/pathagreement.b.R:3572` | `showAssumptions` | YES | 2,750 characters |
| `weightedKappaGuide` | Html | `setContent()` at `R/pathagreement.b.R:3663` | guide + weighted kappa | YES | 4,715 characters |
| `statisticalGlossary` | Html | `setContent()` at `R/pathagreement.b.R:3756` | `showStatisticalGlossary` | YES | 7,995 characters |
| `gwetACTable` | Table | `setRow()` from `R/pathagreement.b.R:3791` | `gwetAC` | YES | 2 rows |
| `pabakTable` | Table | `setRow()` from `R/pathagreement.b.R:3914` | `pabak` | YES | 1 row |
| `sampleSizeTable` | Table | `setRow()` from `R/pathagreement.b.R:4036` | `sampleSizePlanning` | YES | 2–3 rows |
| `raterBiasTable` | Table | `setRow()` from `R/pathagreement.b.R:4132` | `raterBiasAnalysis` | YES | 2–5 rows |
| `agreementTrendTable` | Table | `setRow()` from `R/pathagreement.b.R:4264` | `agreementTrendAnalysis` | YES | 5 rows |
| `caseDifficultyTable` | Table | `setRow()` from `R/pathagreement.b.R:4438` | `caseDifficultyScoring` | YES | 60 rows |
| `stabilityTable` | Table | `setRow()` from `R/pathagreement.b.R:4553` | `agreementStabilityAnalysis` | YES | 2 rows |
| `trendPlot` | Image | state plus renderer at `R/pathagreement.b.R:4912` | `agreementTrendAnalysis` | YES | Non-null state |
| `biasPlot` | Image | state plus renderer at `R/pathagreement.b.R:4924` | `raterBiasAnalysis` | YES | Non-null state |
| `difficultyPlot` | Image | state plus renderer at `R/pathagreement.b.R:4936` | `caseDifficultyScoring` | YES | Non-null state |
| `inlineComments` | Html | `setContent()` at `R/pathagreement.b.R:4762` | `showInlineComments` | YES | 4,861 characters |
| `styleGroupSummary` | Table | `setRow()` from `R/pathagreement.b.R:5093` | `performClustering` | YES | 2 rows |
| `styleGroupProfiles` | Table | `addRow()` from `R/pathagreement.b.R:5352` | `performClustering` | YES | 6–18 computed rows |
| `discordantCasesCluster` | Table | `addRow()` from `R/pathagreement.b.R:5424` | clustering + discordant | YES | 17 computed rows |
| `characteristicAssociations` | Table | `addRow()` from `R/pathagreement.b.R:5499` | clustering; metadata-dependent | YES | 2 rows in metadata run |
| `referenceComparison` | Table | `addRow()` from `R/pathagreement.b.R:5650` | clustering + reference | YES | 2 rows |
| `clusteringHeatmap` | Image | state plus renderer at `R/pathagreement.b.R:5745` | clustering + heatmap | YES | Non-null state |
| `clusterDendrogram` | Image | state plus renderer at `R/pathagreement.b.R:5868` | `performClustering` | YES | Non-null state |
| `silhouettePlot` | Image | state plus renderer at `R/pathagreement.b.R:5900` | `performClustering` | YES | Non-null state |
| `clusteringInterpretation` | Html | `setContent()` from `R/pathagreement.b.R:5689` | clustering + interpretation | YES | 1,696 characters |

All data-dependent results include `vars`, `caseID`, and `useMetadataRows` in `clearWith`. Option-dependent row sets are initialized in `.init()`; remaining runtime `addRow()` calls represent data-computed row sets.

## NOTICES COVERAGE MATRIX

The current toolchain cannot safely implement the command's literal `jmvcore::Notice` insertion pattern. The repository's verified guidance states that `type: Notice` does not compile and dynamically inserted `jmvcore::Notice` objects fail protobuf serialization (`vignettes/jamovi_library_review_guide.md:660-706`). The declared `warnings` HTML result is therefore the correct current transport, but it needs explicit severity presentation.

| Trigger | Intended type | Position | Present? | Message quality | Notes |
|---|---|---|:---:|---|---|
| Fewer than 2 raters/cases | ERROR | Top | ✅ via `reject()` | Specific and actionable | `R/pathagreement.b.R:241-245` |
| Non-factor raters | ERROR | Top | ✅ via `reject()` | Names invalid variables and gives remedy | `R/pathagreement.b.R:3028` |
| No complete cases | ERROR | Top | ✅ via `reject()` | Actionable | `R/pathagreement.b.R:501` |
| Fewer than 2 categories | ERROR | Top | ✅ via `reject()` | Specific | `R/pathagreement.b.R:3078` |
| Very small complete-case sample (`n < 10`) | STRONG_WARNING | Top | ✅ via HTML | Quantifies `n`; lacks visual severity | `R/pathagreement.b.R:3055` |
| More than 20% excluded for missing ratings | STRONG_WARNING | Top | ✅ via HTML | Quantifies percentage and implication | `R/pathagreement.b.R:494` |
| Category prevalence above 80% | STRONG_WARNING | Top | ✅ via HTML | Threshold and alternative method supplied | `R/pathagreement.b.R:560` |
| Constant rater/unused levels/inconsistent levels | STRONG_WARNING or WARNING | Contextual | ✅ via HTML | Names raters/categories and explains effect | `R/pathagreement.b.R:3046-3087` |
| Requested method unsupported/fallback | WARNING | Before coefficient | ✅ via HTML | States fallback and alternatives | `R/pathagreement.b.R:599-673`, `736`, `827` |
| Clustering prerequisites/fallbacks | WARNING | Before clustering | ✅ via HTML | Gives minimum raters/cases or fallback reason | `R/pathagreement.b.R:332-341`, `1440-1447` |
| Optional calculation failure | ERROR or WARNING | Related output | ✅ via HTML | Raw package error is included; guidance depends on `enhancedErrorGuidance` | `R/pathagreement.b.R:3791-4590` |
| Methodology/completion summary | INFO | Bottom | ❌ | Conditional educational panels exist, but no concise completion message | Add only if it conveys useful run-specific facts |

The `Analysis Messages` panel should remain. Its contents are data-quality warnings, method fallback explanations, and calculation failures that users need to interpret the results. It no longer carries progress announcements. The welcome panel's “Setup Progress” block is selection guidance, not a warning.

## PLACEHOLDER ASSESSMENT

- **Data used?** YES — ratings, reference standard, case ID, missingness, factor order, and metadata drive calculations.
- **Options used in logic?** YES — 54/55 caused a valid-run output/pixel change; the remaining option is restricted to error branches.
- **Constant results regardless of inputs?** NO.
- **Placeholder indicators**: No active TODO/FIXME stubs, hard-coded statistical results, or input-echo-only outputs were found. Commented historical implementations remain, but active code calls established statistical packages.
- **Classification**: FUNCTIONAL.

## CRITICAL ISSUES

1. **High — Conger's exact kappa crashes the optional clinical summary.** With five valid ordered-factor raters, `exct = TRUE`, and `showClinicalSummary = TRUE`, the analysis returns `argument is of length zero` after populating one kappa row. `irr::kappam.fleiss(..., exact = TRUE)` supplies no scalar `p.value`; `p_value` becomes length zero at `R/pathagreement.b.R:3224`, remains length zero because only vectors longer than one are normalized at `R/pathagreement.b.R:3275`, and then reaches `if (is.na(p_value))` at `R/pathagreement.b.R:3323`. This is a valid UI combination and blocks the full result.

## INTEGRATION ISSUES

1. **Medium — `enhancedErrorGuidance` has no observable valid-run behavior.** The option is exposed under “Technical: System Options,” defaults to `true`, and only selects wording inside seven exception handlers. A user cannot normally predict or meaningfully choose the failure path. Always provide actionable errors, or replace this UI option with internal behavior and cover a deterministic failure scenario.
2. **Medium — metadata extraction requires `caseID`, but the option can be enabled without one and is then silently ignored.** `.prepareData()` only extracts rows when both `useMetadataRows` and `caseID` are set (`R/pathagreement.b.R:475`). With `useMetadataRows = TRUE` and no case-ID selection, `META_` rows remain ratings and the run succeeds. Disable the checkbox until `caseID` is selected or reject/warn with a direct instruction.
3. **Low — shared unused factor levels can leak into metadata frequency displays.** With documented metadata rows and a selected case ID, metadata observations are removed and the coefficient category set correctly remains `Low/Middle/High`. If all rater columns were constructed with a shared union of unused metadata levels, `.dropMetadataRows()` removes only the metadata values occurring in each column (`R/pathagreement.b.R:1779`), leaving other raters' metadata-only levels unused. The frequency table then showed 40 rows instead of 15 and the warning panel correctly identified the unused levels. A normal CSV import with independently inferred levels is covered by regression tests and does not leak.
4. **Medium — ICC documentation promises continuous data that the schema rejects.** `vars` permits only factors (`jamovi/pathagreement.a.yaml:62-63`), `.validateData()` rejects non-factors (`R/pathagreement.b.R:3013-3034`), and the implementation only computes ICC for ordered factors (`R/pathagreement.b.R:890`). The ICC description says “continuous or ordinal” at `jamovi/pathagreement.a.yaml:322-328` and in `man/pathagreement.Rd`. Narrow the text to ordered categorical ratings or extend the schema and implementation for continuous measurements.

## CODE QUALITY ISSUES

1. **Medium — clinical-summary HTML is not fully dark-theme safe.** Three copy boxes use opaque `background: white` without an explicit foreground at `R/pathagreement.b.R:3437`, `3444`, and `3453`; inherited light text can become unreadable. Several panels also force `#333`, `#424242`, `#495057`, or `#666` on translucent backgrounds, including `R/pathagreement.b.R:3311`, `3433-3452`, and `3496-3725`. The automated checker reports no `background-color` violations because these boxes use the `background` shorthand, so this needs a manual fix and checker expansion.
2. **Low — the “About” panel contains malformed HTML.** `What you need:</</h4>` at `R/pathagreement.b.R:3509` has an extra `</`, relying on browser error recovery.
3. **Medium — the enhanced PABAK failure text is statistically misleading.** “PABAK requires balanced categories” at `R/pathagreement.b.R:3939` contradicts the purpose of the prevalence-adjusted/Brennan-Prediger coefficient. State the actual calculation failure and data requirements without claiming balance is required.
4. **Medium — educational text presents 30 cases as a universal reliability threshold.** `R/pathagreement.b.R:3513` and `3544` say 30 cases are sufficient/recommended, while precision depends on category prevalence, number of raters, expected agreement, and target interval width. Point users to the sample-size planning result and describe 30 only as a rough screening heuristic, if retained.
5. **Medium — internationalization is incomplete.** Examples include unwrapped `jmvcore::format()` strings at `R/pathagreement.b.R:763` and `807`, plus large HTML blocks assembled from untranslated fragments. Wrap whole translatable sentences with `.()` and placeholders; avoid translating fragments separately.
6. **Low — several checkbox labels use title case rather than repository sentence-case convention.** Examples include “Clinical Summary,” “Agreement Heatmap,” “Krippendorff's Alpha,” and “Category-Specific Agreement” in `jamovi/pathagreement.u.yaml`.
7. **Low — commented historical implementations add substantial review noise.** Large disabled blocks around `R/pathagreement.b.R:1166-1375` describe obsolete result schemas. They are not active defects, but removing them would make the active data paths easier to audit.

## STRENGTHS

1. All 51 result items are wired and demonstrably populated; all 13 images carry restorable state and guard missing state before rendering.
2. Data-changing options are included in every data-dependent output's `clearWith`, reducing stale-result risk.
3. Fixed and option-determined rows are created in `.init()` and populated with `setRow()`; dynamic `addRow()` use is limited to computed row sets.
4. Cohen, Fleiss/Conger, Krippendorff, ICC, Gwet, Brennan-Prediger/PABAK, and sample-size calculations use established packages and have independent regression comparisons.
5. Bootstrap resampling is seeded and uses whole cases for reference-standard intervals.
6. Ordinal category order is preserved for weighted measures; numeric clustering distances fall back with an explanatory message when their assumptions are unmet.
7. User-provided variable names are HTML-escaped in the warning panel. Only structural HTML entities (`&amp;`, `&gt;`, `&lt;`) are present.
8. Landis-Koch bands are explicitly described as conventions rather than clinical acceptability thresholds.
9. All 21 result reference keys resolve in `jamovi/00refs.yaml` and have usable author/year metadata.
10. Every package reached through `::` is declared in `DESCRIPTION`, including base packages.
11. UI title and analysis title match, and the UI has no duplicate controls or references to removed options.

## DOCS CONSISTENCY

The generated wrapper and `man/pathagreement.Rd` match the current option names, order, and defaults. The weighted-distance, bootstrap, exact-kappa, reference-standard, and case-ID descriptions reflect the active implementation. Remaining inconsistencies are:

- ICC says continuous data are supported, although the input schema and backend accept only factor raters.
- Two commented examples in `jamovi/pathagreement.a.yaml:36-49` call `agreement()` rather than `pathagreement()`.
- The main Rd page has no runnable examples because the YAML example block is commented out.
- Several user-facing HTML strings bypass whole-sentence translation.

## EXTERNAL DOCS COMPARISON

No upstream comparison was requested or run (`check_external = false`; no `cran_pkg`, `github_repo`, or `upstream_fn`).

| Aspect | Local | Upstream | Status | Action |
|---|---|---|:---:|---|
| Function signature | 55 schema entries | Not supplied | N/A | Supply an upstream identifier if parity is expected |
| Arguments/defaults | Wrapper and YAML agree | Not supplied | N/A | None for local consistency |
| Behavior notes | Local YAML/Rd reviewed | Not supplied | N/A | Address local ICC and sample-size wording |
| Deprecations | No target supplied | Not supplied | N/A | None assessed |
| Examples | Commented YAML examples only | Not supplied | N/A | Add runnable local examples |

## ACTIONABLE FIXES

### Immediate

Normalize optional table-cell values before scalar conditionals and add a regression for the exact-summary combination:

```r
p_value <- kappa_table$getCell(rowKey = first_key, "p")$value
if (length(p_value) != 1L || is.na(p_value)) {
    p_value <- NA_real_
} else {
    p_value <- as.numeric(p_value)
}
```

Also make the producer explicit so every kappa row has a scalar p cell:

```r
p = if (length(result$p.value) == 1L) as.numeric(result$p.value) else NA_real_
```

Test `exct = TRUE`, `fleissCI = TRUE/FALSE`, and `showClinicalSummary = TRUE`; require a populated Conger row, visible summary/report, “not available” significance text, and no error.

### Schema and UI updates

```yaml
# Either document the implemented ICC scope:
- name: icc
  title: Intraclass Correlation Coefficient
  description:
      R: Calculate ICC(2,1) from the numeric codes of ordered-factor ratings.

# Or extend vars/backend deliberately before retaining the continuous-data claim.
```

Make metadata extraction conditional in the UI on a selected case-ID variable, or reject `useMetadataRows = TRUE` without `caseID`. Remove `enhancedErrorGuidance` from the UI and always show actionable failure text unless a stable, testable user decision is identified.

### Backend and content improvements

```r
# Safe current-toolchain message approach: keep the declared Html result and
# store severity with each message so the renderer can label/order it.
private$.accumulateMessage(message, severity = "strong_warning")

# Theme-safe report box: inherit both background and text, or set both explicitly.
"<div style='background: rgba(138, 155, 172, 0.06); color: inherit;'>"
```

- Correct the PABAK failure sentence and the malformed heading.
- Replace blanket “30 cases” claims with precision/prevalence guidance tied to the sample-size output.
- Convert fixed dark foreground colors to inherited or paired theme-safe colors.
- Expand `tools/theme_safe_html.py` to detect the `background:` shorthand.
- Translate complete strings, including method labels and HTML sentences.
- Do not add `type: Notice`, `type: Notification`, or dynamically inserted `jmvcore::Notice` objects until the repository's documented compiler/protobuf limitations are re-tested and resolved.

### Upstream sync tasks

- No upstream sync task can be specified without an upstream package/repository.
- If `pathagreement` intentionally mirrors another implementation, rerun with `check_external = true` and explicit identifiers.

## DIFFERENTIAL TEST HARNESS

The audit instantiated `pathagreementClass` and `pathagreementOptions` directly, initialized protobuf support, ran baseline/changed option pairs, serialized each result with state, and compared SHA-256 digests. Plot-only controls were rendered to PNG and compared by pixel hash. A minimal pattern is:

```r
run_with_opts <- function(data, opts) {
    analysis <- pathagreementClass$new(
        data = data,
        options = do.call(pathagreementOptions$new, opts),
        datasetId = "audit",
        analysisId = 1L,
        revision = 1L
    )
    analysis$run()
    analysis
}

digest_results <- function(analysis) {
    vapply(names(analysis$results), function(name) {
        pb <- analysis$results[[name]]$asProtoBuf(
            incAsText = TRUE,
            status = 3L,
            includeState = TRUE
        )
        digest::digest(RProtoBuf::serialize(pb, NULL), algo = "sha256")
    }, character(1))
}
```

## TESTING CHECKLIST

- [x] Run every schema entry in a baseline/changed pair: 55 entries, 110 successful paired runs.
- [x] Verify visual-only settings with rendered pixel hashes.
- [x] Exercise every result item across comprehensive, two-rater, metadata, and welcome scenarios: 51/51 populated.
- [x] Reproduce `exct = TRUE` + `showClinicalSummary = TRUE`: fails with `argument is of length zero`.
- [x] Run focused `pathagreement` tests after `devtools::load_all()`: 186 passed, 1 skipped because `RProtoBuf` was unavailable on that process's main library path.
- [x] Run repository rendering contract: 6 passed.
- [x] Run state guard checker: 0 unguarded image-state reads.
- [x] Run theme checker: 0 findings from its current `background-color` rules; manual shorthand/foreground findings remain.
- [x] Validate UI duplicate names: none.
- [x] Validate reference years/keys: 21/21 valid.
- [x] Check console warning/message calls: none.
- [x] Check HTML entities: only the five permitted structural forms, with three actually used.
- [x] Check package declarations: all namespace-qualified dependencies declared.
- [ ] Add and pass the exact-kappa clinical-summary regression.
- [ ] Verify HTML panels in jamovi light and dark themes after the color fixes.
- [ ] Verify metadata behavior with `useMetadataRows` enabled but no case-ID selection after adding validation/UI gating.
- [ ] Compare against an explicit upstream, if one exists.

## READINESS ASSESSMENT

- **File Integration**: ✅
- **Error Handling (Notices)**: ⚠️ — user-facing HTML/reject paths exist, but severity is unstructured and one valid summary path crashes
- **User Experience**: ⚠️ — explanatory messages are retained; metadata prerequisites and dark-theme HTML need work
- **Production Ready**: **NO**, until the Conger-summary crash is fixed and regression-tested
