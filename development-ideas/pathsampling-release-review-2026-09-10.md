## 1. Overall verdict

**Not ready for release until the generated files and test-module copy are refreshed and checked.**
The repaired backend agrees with the independent reference calculations below. Its outputs are
appropriate as descriptive sampling analyses and explicitly qualified working-model predictions;
they do not establish population disease sensitivity or a clinical sampling protocol. This pass
repaired remaining presentation defects and obsolete statistical guidance. Source analysis version
is now **2.1.0**, still routed to **OncoPathT**. The generated wrapper currently remains at 2.0.0
and carries the earlier result headings. No statistical default was changed during this release pass.

Scope: `pathsampling` in the current working tree, preserving earlier fixes and unrelated edits.
This report distinguishes this pass from the substantial preceding repair work.

## 2. Findings

### Critical

No new wrong-number or crash defect was found in the exercised backend paths. This is a scoped
finding, not a claim of external clinical validation or a successful whole-package release check.

### Major

- **Fixed:** sequence-only results used anatomical labels such as “Unifocal” and “Dispersed
  (multifocal)”. Row labels now describe gap bands and sample-position groups
  ([R/pathsampling.b.R:2520](../R/pathsampling.b.R#L2520)); result titles and the group-count column
  were corrected in [jamovi/pathsampling.r.yaml](../jamovi/pathsampling.r.yaml). YAML-derived
  headings are pending regeneration.
- **Fixed:** the shipped explanatory vignette treated separate lymph nodes as inherently
  independent and claimed dependence always led to underestimation. The replacement explains
  both directions of dependence and observed-detection selection with executable probability
  examples ([vignette:34](../vignettes/general-independent-vs-dependent-sampling-explained.Rmd#L34)).
- **Open release gate:** source schemas and generated artifacts now differ. The older JamoviTest
  analysis still claims 2.0.0; the OncoPath copy is absent. Regeneration and subsequent propagation
  are required before release ([jamovi/pathsampling.a.yaml:6](../jamovi/pathsampling.a.yaml#L6)).

### Moderate

- **Fixed:** rejection of a pooled probability estimate replaced a valid sequence-group
  explanation with an unrelated prediction-unavailable message. Descriptive groups and their
  explanation now remain available ([R/pathsampling.b.R:2572](../R/pathsampling.b.R#L2572)).
- **Fixed:** the UI marked both first-detection inputs universally required, although finite-model,
  node and assumed-q planning branches can run independently. Labels now state their use
  ([jamovi/pathsampling.u.yaml:30](../jamovi/pathsampling.u.yaml#L30)).
- **Fixed:** the automatic heterogeneity option description claimed significant differences even
  though it is a CV heuristic. Its description now specifies the geometric summary, eligibility,
  independence from the selected prediction estimator, and absence of a significance test
  ([jamovi/pathsampling.a.yaml:460](../jamovi/pathsampling.a.yaml#L460)).

### Minor

- **Fixed:** target-probability and recorded-case fraction headings were made more precise in the
  results schema. Their generated counterparts still need regeneration.
- **Fixed:** the translation regression test assumed single-line gettext strings. It now accepts
  wrapped ids/translations, matching valid catalog formatting
  ([tests/testthat/test-pathsampling-audit-regressions.R:226](../tests/testthat/test-pathsampling-audit-regressions.R#L226)).

## 3. Changes made

- Corrected six backend row labels and the sequence-group explanation gate; retained result keys
  and calculations for compatibility.
- Corrected results/UI wording, clarified the heuristic option description, and bumped the
  analysis version to 2.1.0 without changing the package version or production routing.
- Replaced obsolete vignette guidance with tested examples and explicit estimand, observation-window,
  dependence, bootstrap and threshold limitations.
- Added end-to-end regressions for rejected-q descriptive groups, gap-threshold grouping, and
  fitted beta-binomial probabilities; repaired the wrapped-gettext test.
- Added translations for the changed labels. All **264 distinct translated backend messages**
  have nonempty Turkish translations with matching placeholder sets. Existing catalog work was
  preserved. Generated headers, documentation and module metadata were not edited or regenerated
  during this pass.

## 4. Statistical verification

The throwaway `/tmp/pathsampling-release/verify.R` script exercised actual analysis outputs and
compared them with `stats` or an independently fitted VGAM model. Numeric tolerance was `1e-9`,
except the Wilson interval checked at its displayed precision.

| Quantity | Analysis | Independent reference |
|---|---:|---:|
| Binomial P(X >= 1), n=5, q=0.2 | 0.6723200 | 0.6723200 (`stats::pbinom`) |
| Sample position reaching probability 0.95, q=0.2 | 14 | 14 (`stats::qgeom + 1`) |
| Mean hypergeometric P(X >= 2), N=20, K=1 and 4, n=5 | 0.1243550052 | 0.1243550052 (`stats::phyper`) |
| Wilson 95% interval, 36/48 | 61.2%–85.1% | 61.2%–85.1% (`stats::prop.test`, no correction) |
| Beta-binomial P(X >= 1), n=5 | 0.6424765452 | 0.6424765452 (independent `VGAM::vglm` + `dbetabinom.ab`) |

The geometric sample-position convention and finite-population tail definitions were checked
against the official [R geometric manual](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/Geometric.html)
and [R hypergeometric manual](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/Hypergeometric.html).
The vignette additionally demonstrates any-positive probabilities 0.2, 0.36 and 0.4 under three
valid dependence structures, and the observed-window example yields inverse mean 0.3583862 when
the underlying q is 0.1. These illustrate assumptions; they are not clinical validation.

## 5. Data-flow audit

All **61 options, including the dataset**, were traced; all **60 UI controls** correspond to
schema options. All backend option reads resolve to declared options. All **67 result items**
clear on real option names, and all four image `renderFun` methods exist. There are no pathsampling
JavaScript events. The full enabled/default perturbation audit ran without analysis errors;
one homogeneous fixture did not trigger `autoDetectHeterogeneity`, so an additional heterogeneous
fixture verified its notice and toggle explicitly.

**Unused options: none.** An inactive dependent option need not change default results; its
own enabled branch was also exercised. The mapping below lists observed output changes,
including dependent displays and notices. Full UI parent paths and backend lines are retained in
[the data-flow CSV](pathsampling-release-review-2026-09-10-dataflow.csv). Every result's name,
type, visibility, backend assignment, schema line and renderer is listed in
[the output inventory](pathsampling-release-review-2026-09-10-outputs.csv).

| Option (type; default) | UI control | Backend evidence | Computation / observed outputs |
|---|---|---|---|
| `data` (Data; NULL/data) | Dataset | [b.R:64](../R/pathsampling.b.R#L64) | Validate and align case rows; independent branches use their own eligible inputs; `binomialTable, binomialText, bootstrapTable, clinicalSummary, clusteringTable, dataInfo, detectionCurve, empiricalCumulativePlot, empiricalCumulativeTable, empiricalCumulativeText, heterogeneityTest, incrementalYieldTable, keyResults, modelFitTable, modelFitText, multifocalAnalysisText, multifocalProbTable, multifocalTable, multifocalText, notices, obsPredTable, populationDetectionTable, prevalenceTable, probabilityExplanation, recommendTable, recommendText, sampleSizePlanningTable, sensitivityPlot, spatialClusteringText, stratifiedDetectionTable` |
| `analysisContext` (List; general) | ComboBox  | [b.R:128](../R/pathsampling.b.R#L128) | Select the auto estimator and context explanations; `empiricalCumulativeText, recommendText` |
| `totalSamples` (Variable; NULL/data) | VariablesListBox  | [b.R:69](../R/pathsampling.b.R#L69) | Validate first positions/counts; paired denominator; observed sample totals; `clusteringTable, correlationPlot, dataInfo, stageMigrationTable, tumorBurdenInfo` |
| `firstDetection` (Variable; NULL/data) | VariablesListBox  | [b.R:70](../R/pathsampling.b.R#L70) | Detected-case geometric estimate and observed cumulative positions; `binomialTable, binomialText, bootstrapTable, clinicalSummary, clusteringTable, dataInfo, detectionCurve, empiricalCumulativePlot, empiricalCumulativeTable, empiricalCumulativeText, heterogeneityTest, incrementalYieldTable, keyResults, modelFitTable, modelFitText, multifocalAnalysisText, multifocalProbTable, multifocalTable, multifocalText, notices, obsPredTable, populationDetectionTable, prevalenceTable, probabilityExplanation, recommendTable, recommendText, sampleSizePlanningTable, sensitivityPlot, spatialClusteringText, stratifiedDetectionTable` |
| `positiveCount` (Variable; NULL/data) | VariablesListBox  | [b.R:1154](../R/pathsampling.b.R#L1154) | Paired detected-case count estimate and CV screening; `binomialTable, binomialText, clusteringTable, dataInfo, detectionCurve, empiricalCumulativePlot, keyResults, multifocalTable, notices, recommendTable` |
| `positiveSamplesList` (Variable; NULL/data) | VariablesListBox  | [b.R:1155](../R/pathsampling.b.R#L1155) | Parse bounded positions; gap indices and sequence groups; `clusteringTable, multifocalTable, multifocalText, notices, spatialClusteringText` |
| `sampleType` (Variable; NULL/data) | VariablesListBox  | [b.R:1156](../R/pathsampling.b.R#L1156) | Preserve labels; group counts, estimates and heterogeneity checks; `heterogeneityTest, heterogeneityText, notices, prevalenceTable, stratifiedDetectionTable, stratifiedText` |
| `targetConfidence` (Number; 0.95) | TextBox  | [b.R:1165](../R/pathsampling.b.R#L1165) | Select headline target position; reference line in plots; `clinicalSummary, detectionCurve, empiricalCumulativePlot, keyResults, recommendText, sensitivityPlot` |
| `maxSamples` (Integer; 10) | TextBox  | [b.R:61](../R/pathsampling.b.R#L61) | Set prediction/empirical table grid and plot range; `betaBinomialTable, binomialTable, bootstrapTable, detectionCurve, empiricalCumulativePlot, empiricalCumulativeTable, hyperRecommendTable, hypergeometricTable, incrementalYieldTable, multifocalProbTable, obsPredTable, populationDetectionTable, sensitivityPlot` |
| `bootstrapIterations` (Integer; 10000) | TextBox  | [b.R:1167](../R/pathsampling.b.R#L1167) | Number of case resamples for percentile intervals; `bootstrapTable, bootstrapText, empiricalCumulativeTable, keyResults, notices, recommendText, sensitivityPlot` |
| `showBinomialModel` (Bool; TRUE) | CheckBox  | [b.R:1181](../R/pathsampling.b.R#L1181) | Enable/populate the corresponding optional analysis or explanation; `binomialTable, binomialText, dataInfo, detectionCurve, empiricalCumulativePlot, keyResults, notices, recommendTable` |
| `showBootstrap` (Bool; FALSE) | CheckBox  | [b.R:81](../R/pathsampling.b.R#L81) | Enable/populate the corresponding optional analysis or explanation; `bootstrapTable, bootstrapText, clinicalSummary, keyResults, notices, recommendText, sensitivityPlot` |
| `showDetectionCurve` (Bool; FALSE) | CheckBox  | [b.R:1489](../R/pathsampling.b.R#L1489) | Enable/populate the corresponding optional analysis or explanation; `detectionCurve` |
| `showSensitivityCI` (Bool; FALSE) | CheckBox  | [b.R:](../R/pathsampling.b.R#L) | Enable/populate the corresponding optional analysis or explanation; `sensitivityPlot` |
| `setSeed` (Bool; FALSE) | CheckBox  | [b.R:1394](../R/pathsampling.b.R#L1394) | Enable local seed while preserving caller RNG state; `bootstrapTable, empiricalCumulativeTable, sensitivityPlot` |
| `seedValue` (Integer; 42) | TextBox  | [b.R:1410](../R/pathsampling.b.R#L1410) | Local bootstrap seed when setSeed is enabled; `bootstrapTable, empiricalCumulativeTable, sensitivityPlot` |
| `positiveCassettes` (Variable; NULL/data) | VariablesListBox  | [b.R:1882](../R/pathsampling.b.R#L1882) | Positive-sample burden, threshold summaries and correlation; `cassetteDistribution, correlationPlot, correlationStats, correlationText, notices, stageMigrationTable, stageMigrationText, tumorBurdenInfo, tumorBurdenText` |
| `totalFoci` (Variable; NULL/data) | VariablesListBox  | [b.R:2179](../R/pathsampling.b.R#L2179) | Total foci for single-versus-summed comparisons; `distributionComparisonTable, distributionPatternTable, distributionPatternText, notices` |
| `adequacyThreshold` (Integer; 12) | TextBox  | [b.R:252](../R/pathsampling.b.R#L252) | Node count cutoff for Wilson adequacy summary; `effectSizesTable` |
| `maxPositiveSingle` (Variable; NULL/data) | VariablesListBox  | [b.R:2180](../R/pathsampling.b.R#L2180) | Largest single-sample foci count; `distributionComparisonTable, distributionPatternTable, distributionPatternText, notices` |
| `showTumorBurden` (Bool; FALSE) | CheckBox  | [b.R:1882](../R/pathsampling.b.R#L1882) | Enable/populate the corresponding optional analysis or explanation; `cassetteDistribution, notices, tumorBurdenInfo, tumorBurdenText` |
| `showStageMigration` (Bool; FALSE) | CheckBox  | [b.R:84](../R/pathsampling.b.R#L84) | Enable/populate the corresponding optional analysis or explanation; `notices, stageMigrationTable, stageMigrationText` |
| `showCorrelation` (Bool; FALSE) | CheckBox  | [b.R:2107](../R/pathsampling.b.R#L2107) | Enable/populate the corresponding optional analysis or explanation; `correlationPlot, correlationStats, correlationText, notices` |
| `showDistributionPattern` (Bool; FALSE) | CheckBox  | [b.R:85](../R/pathsampling.b.R#L85) | Enable/populate the corresponding optional analysis or explanation; `distributionComparisonTable, distributionPatternTable, distributionPatternText, notices` |
| `distributionThreshold` (Integer; 5) | TextBox  | [b.R:2196](../R/pathsampling.b.R#L2196) | Foci threshold for single-versus-summed classification; `distributionComparisonTable, distributionPatternTable, distributionPatternText` |
| `totalPopulation` (Variable; NULL/data) | VariablesListBox  | [b.R:90](../R/pathsampling.b.R#L90) | Finite-population size and beta-binomial trial denominator; `betaBinomialRecommendTable, betaBinomialTable, betaBinomialText, hyperRecommendTable, hypergeometricTable, hypergeometricText, notices` |
| `successStates` (Variable; NULL/data) | VariablesListBox  | [b.R:91](../R/pathsampling.b.R#L91) | Finite positive-state count and beta-binomial response; `betaBinomialRecommendTable, betaBinomialTable, betaBinomialText, hyperRecommendTable, hypergeometricTable, hypergeometricText, notices` |
| `targetDetections` (Integer; 1) | TextBox  | [b.R:297](../R/pathsampling.b.R#L297) | Tail event X >= target for finite-population predictions; `betaBinomialRecommendTable, betaBinomialTable, hyperRecommendTable, hypergeometricTable, hypergeometricText` |
| `showHypergeometric` (Bool; FALSE) | CheckBox  | [b.R:94](../R/pathsampling.b.R#L94) | Enable/populate the corresponding optional analysis or explanation; `hyperRecommendTable, hypergeometricTable, hypergeometricText, notices` |
| `showBetaBinomial` (Bool; FALSE) | CheckBox  | [b.R:96](../R/pathsampling.b.R#L96) | Enable/populate the corresponding optional analysis or explanation; `betaBinomialRecommendTable, betaBinomialTable, betaBinomialText, notices` |
| `totalLymphNodes` (Variable; NULL/data) | VariablesListBox  | [b.R:101](../R/pathsampling.b.R#L101) | Node-count denominator and adequacy fraction; `adequacyByELN, ajccNStage, effectSizesTable, effectSizesText, lnAnalysisText, lnrClassification, notices` |
| `positiveLymphNodes` (Variable; NULL/data) | VariablesListBox  | [b.R:102](../R/pathsampling.b.R#L102) | Node-count ratio and pancreatic count categories; `adequacyByELN, ajccNStage, lnAnalysisText, lnrClassification, notices` |
| `showLNAnalysis` (Bool; FALSE) | CheckBox  | [b.R:98](../R/pathsampling.b.R#L98) | Enable/populate the corresponding optional analysis or explanation; `adequacyByELN, ajccNStage, lnAnalysisText, lnrClassification, notices` |
| `lnrThreshold1` (Number; 0.1) | TextBox  | [b.R:99](../R/pathsampling.b.R#L99) | First adjustable LNR category boundary; `lnrClassification` |
| `lnrThreshold2` (Number; 0.3) | TextBox  | [b.R:99](../R/pathsampling.b.R#L99) | Second adjustable LNR category boundary; `lnrClassification` |
| `showEffectSizes` (Bool; FALSE) | CheckBox  | [b.R:243](../R/pathsampling.b.R#L243) | Enable/populate the corresponding optional analysis or explanation; `effectSizesTable, effectSizesText, notices` |
| `showOmentumAnalysis` (Bool; FALSE) | CheckBox  | [b.R:2762](../R/pathsampling.b.R#L2762) | Enable/populate the corresponding optional analysis or explanation; `omentumText` |
| `showClinicalSummary` (Bool; FALSE) | CheckBox  | [b.R:2743](../R/pathsampling.b.R#L2743) | Enable/populate the corresponding optional analysis or explanation; `clinicalSummary` |
| `showGuidedInstructions` (Bool; FALSE) | CheckBox  | [b.R:1067](../R/pathsampling.b.R#L1067) | Enable/populate the corresponding optional analysis or explanation; `guidedInstructions` |
| `showConciseInstructions` (Bool; FALSE) | CheckBox  | [b.R:1124](../R/pathsampling.b.R#L1124) | Enable/populate the corresponding optional analysis or explanation; `conciseInstructions` |
| `showEmpiricalCumulative` (Bool; FALSE) | CheckBox  | [b.R:86](../R/pathsampling.b.R#L86) | Enable/populate the corresponding optional analysis or explanation; `empiricalCumulativePlot, empiricalCumulativeTable, empiricalCumulativeText` |
| `showSpatialClustering` (Bool; FALSE) | CheckBox  | [b.R:2478](../R/pathsampling.b.R#L2478) | Enable/populate the corresponding optional analysis or explanation; `clusteringTable, notices, spatialClusteringText` |
| `showStratifiedAnalysis` (Bool; FALSE) | CheckBox  | [b.R:1493](../R/pathsampling.b.R#L1493) | Enable/populate the corresponding optional analysis or explanation; `notices, prevalenceTable, stratifiedDetectionTable, stratifiedText` |
| `showPopulationDetection` (Bool; FALSE) | CheckBox  | [b.R:87](../R/pathsampling.b.R#L87) | Enable/populate the corresponding optional analysis or explanation; `populationDetectionTable, populationDetectionText` |
| `showIncrementalYield` (Bool; FALSE) | CheckBox  | [b.R:2340](../R/pathsampling.b.R#L2340) | Enable/populate the corresponding optional analysis or explanation; `incrementalYieldTable, incrementalYieldText` |
| `fociGapThreshold` (Integer; 2) | TextBox  | [b.R:3088](../R/pathsampling.b.R#L3088) | A strictly larger position gap starts another heuristic group; `multifocalTable` |
| `showMultifocalAnalysis` (Bool; FALSE) | CheckBox  | [b.R:83](../R/pathsampling.b.R#L83) | Enable/populate the corresponding optional analysis or explanation; `multifocalAnalysisText, multifocalProbTable, multifocalTable, multifocalText` |
| `showProbabilityExplanation` (Bool; FALSE) | CheckBox  | [b.R:1492](../R/pathsampling.b.R#L1492) | Enable/populate the corresponding optional analysis or explanation; `probabilityExplanation` |
| `showKeyResults` (Bool; TRUE) | CheckBox  | [b.R:2739](../R/pathsampling.b.R#L2739) | Enable/populate the corresponding optional analysis or explanation; `keyResults` |
| `showRecommendText` (Bool; FALSE) | CheckBox  | [b.R:2700](../R/pathsampling.b.R#L2700) | Enable/populate the corresponding optional analysis or explanation; `recommendText` |
| `showInterpretText` (Bool; FALSE) | CheckBox  | [b.R:2757](../R/pathsampling.b.R#L2757) | Enable/populate the corresponding optional analysis or explanation; `interpretText` |
| `showReferencesText` (Bool; FALSE) | CheckBox  | [b.R:2771](../R/pathsampling.b.R#L2771) | Enable/populate the corresponding optional analysis or explanation; `referencesText` |
| `estimationMethod` (List; auto) | ComboBox  | [b.R:126](../R/pathsampling.b.R#L126) | Shared pooled/subgroup empirical, geometric or context-aware auto rule; `binomialTable, binomialText, dataInfo, detectionCurve, empiricalCumulativePlot, keyResults, modelFitTable, modelFitText, multifocalAnalysisText, multifocalProbTable, notices, obsPredTable, populationDetectionTable, prevalenceTable, recommendTable, sampleSizePlanningTable, stratifiedDetectionTable` |
| `showHeterogeneityTest` (Bool; FALSE) | CheckBox  | [b.R:1588](../R/pathsampling.b.R#L1588) | Enable/populate the corresponding optional analysis or explanation; `heterogeneityTest, heterogeneityText` |
| `showModelFit` (Bool; FALSE) | CheckBox  | [b.R:1490](../R/pathsampling.b.R#L1490) | Enable/populate the corresponding optional analysis or explanation; `modelFitTable, modelFitText` |
| `showObsPred` (Bool; FALSE) | CheckBox  | [b.R:82](../R/pathsampling.b.R#L82) | Enable/populate the corresponding optional analysis or explanation; `obsPredTable, obsPredText` |
| `showSampleSizePlanning` (Bool; FALSE) | CheckBox  | [b.R:221](../R/pathsampling.b.R#L221) | Enable/populate the corresponding optional analysis or explanation; `sampleSizePlanningTable, sampleSizePlanningText` |
| `planningTargetProb` (Number; 0.8) | TextBox  | [b.R:222](../R/pathsampling.b.R#L222) | Target probability in assumed-q sample planning; `sampleSizePlanningTable` |
| `planningAssumedQ` (Number; 0.1) | TextBox  | [b.R:223](../R/pathsampling.b.R#L223) | Assumed independent-trial q in sample planning; `sampleSizePlanningTable` |
| `autoSelectModel` (Bool; FALSE) | CheckBox  | [b.R:1706](../R/pathsampling.b.R#L1706) | Explain applicability; does not rank unlike estimands; `modelSelectionText` |
| `autoDetectHeterogeneity` (Bool; TRUE) | CheckBox  | [b.R:1711](../R/pathsampling.b.R#L1711) | CV screen of inverse group mean first positions; warning only; `notices (heterogeneous supplemental fixture)` |

Backend behavior without a separate user control:

- Fixed 95% bootstrap/Wilson intervals; `targetConfidence` is a detection target, not the CI level.
- Empirical count CV caution/rejection at 0.3/0.5 and geometric between-group CV warning at 0.3;
  these are heuristic screens. The latter does not switch to the prediction estimator.
- Gap-index bands at 0.7 and 1.3; descriptive yield bands at 10%, 5% and 2%; fixed tabulated
  target probabilities 0.80/0.90/0.95/0.99 and selected subgroup sample positions.
- Pancreatic positive-node categories 0, 1–3 and >=4, alongside an explicit disease-specific
  limitation; this is not a general staging selector.
- First-detection analyses condition on eventual observed positives; hypergeometric case curves
  receive equal weights. There is no censoring, survey-weight, or cluster-resampling option.
- Geometric goodness-of-fit and likelihood-ratio heterogeneity tests retain their working-model
  assumptions and asymptotic limitations; they are exploratory, without multiplicity adjustment.
- Model applicability guidance and assumption notices are explanatory. The guidance does not
  automatically rank different estimands. Validation/exclusion notices are always available.

These fixed behaviors are disclosed rather than adding options without a supported use case.

## 6. Test results

- Focused suite: `devtools::load_all(quiet=TRUE)` followed by every
  `tests/testthat/test-pathsampling*.R`. After repairing the gettext test, the affected file was
  rerun against the current backend. Combined final results: **55 test blocks, 201 passed
  expectations, 0 failures, 0 errors, 0 skips**. Five warnings are from the pre-existing direct
  VGAM convergence/zero-variance reference fixtures in `test-pathsampling-methodology.R`.
  Module loading separately emits existing dependency/display startup warnings.
- Repaired audit file alone: **103 passed, 0 failed, 0 errors, 0 warnings**. Log:
  `/tmp/pathsampling-release/audit-final.log`.
- `testthat::test_file("tests/testthat/test-zzz-results-rendering-contract.R")`:
  **6 passed**, no failures. Log: `/tmp/pathsampling-release/contracts.log`.
- `tools/ui_harness/render_ui.sh pathsampling`: final real-client result:
  `HARNESS: placeholder present = false ; title = Pathology Sampling Adequacy Analysis ; errors = undefined`.
  This compiles only the temporary UI copy; it does not regenerate analysis headers.
- Scoped state guard: **0 unguarded image$state reads**. Duplicate UI names: **none**.
  Theme check: **0 pale backgrounds / 0 chips needing a foreground**. No nonstructural named
  HTML entities, raw `warning()` calls, or imperative `setVisible(FALSE)` calls in the backend.
  Fixed rows are initialized before calculation; dynamic rows use the shared update helper.
- All four English and Turkish plots rendered. English/Turkish numeric tables were identical.
  All enabled results serialized successfully through jmvcore/RProtoBuf (**32,825 bytes**).
- The revised vignette rendered with `rmarkdown::render()` to a standalone temporary HTML file.
  Numeric code chunks executed; the illustrative package call intentionally has `eval=FALSE`.
- Target filename/class/Collate casing matched. `compilerMode: tame` is present. All 11 cited
  reference keys resolve with exact case and include title/author/URL fields. Package metadata
  agrees at **1.0.81.01** in DESCRIPTION, 0000.yaml and CITATION.cff. No committed `.jmo` or
  `.tar.gz` archives were found. Source analysis version is deliberately separate at **2.1.0**.

Logs, the numerical scratch script, option perturbations, and rendered artifacts are under
`/tmp/pathsampling-release/`. The prior pass also checked realistic bootstrap performance and
same-instance reruns. Whole-package `devtools::check()` and a refreshed native-module save/reopen
were not run in this scoped pass; neither is claimed to have passed.

## 7. Remaining limitations

1. **Generated files are stale.** New `.r.yaml` headings, the option description, and analysis
   metadata are not active in the generated wrapper. Tests above use the existing compatible
   wrapper and current backend; they do not validate the newly generated representation.
2. **Test-module propagation is pending.** JamoviTest still has its earlier 2.0.0 copy. The source
   is routed to OncoPathT; changing production routing remains the user's release action.
3. **Clinical inference is limited.** Observed-positive curves and bootstrap intervals do not
   correct ascertainment or unequal observation windows. Independence, exchangeability and fitted
   distribution assumptions remain. Heuristic groups and threshold compliance do not define
   disease stage, treatment, or a universal sampling requirement.
4. **Asymptotic/fit limits remain explicit.** Small/sparse groups and boundary fits can limit
   geometric tests and beta-binomial fitting. Warnings and unavailable-result paths are tested;
   no external patient-outcome validation is implied.
5. **Final artifact checks remain.** After regeneration and module propagation, rerun the focused
   tests and inspect a native saved/reopened analysis with plots, empty selections and changed
   options. Rebuild the documentation site through the normal release workflow so its generated
   pages reflect the corrected source vignette and help. This review did not assess unrelated
   package changes in the already dirty working tree.

## 8. Release recommendation

**Not ready for release.** The confirmed source defects are repaired; clear the following gates:

1. Run the schema/documentation generators from the module root:

   ```sh
   Rscript -e 'Sys.unsetenv("ELECTRON_RUN_AS_NODE"); jmvtools::prepare()'
   Rscript -e 'devtools::document()'
   ```

2. Verify the regenerated pathsampling header/help, propagate the refreshed analysis through the
   configured JamoviTest workflow, and rerun its focused tests and native save/reopen smoke check.
3. Refresh generated site documentation and complete the normal package release checks before
   changing the test-menu route to production.

The invoked [release-review-function skill](../.claude/skills/release-review-function/SKILL.md)
explicitly says **“Regeneration is the user's call — ask, do not run it yourself”**. These
module-wide generators were therefore left for the user; they can incorporate unrelated in-flight
YAML edits and produce a wide diff. This is the reason regeneration remains outstanding.
