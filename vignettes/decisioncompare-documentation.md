# Compare Medical Decision Tests - Developer Documentation

## 1. Overview

- **Function**: `decisioncompare`
- **Title**: Compare Medical Decision Tests
- **Module**: `meddecide`
- **Files**:
  - `jamovi/decisioncompare.u.yaml` - User Interface Definition
  - `jamovi/decisioncompare.a.yaml` - Options & Schema Definition
  - `jamovi/decisioncompare.r.yaml` - Results Layout & Tables
  - `R/decisioncompare.b.R` - Backend Implementation
- **Summary**: Function for comparing multiple Medical Decision Tests. Compares sensitivity, specificity, positive predictive value, negative predictive value, and other metrics between different tests against the same gold standard. Includes statistical comparison using McNemar's test (the mid-p test by default, or the asymptotic test without continuity correction; Fagerland et al. 2013) and confidence intervals for differences.

## 1a. Changelog

- **Date**: 2026-10-04
- **Summary**: Analysis 1.1.0 - `referenceType`, `referenceDescription`, `targetDefinition`; explicit positive and negative levels required for every selected variable; level-mapping, reference-definition and comparator outputs added; validation-provenance options (`showValidationContext`, `cohortRole`, `samplingDesign`, `thresholdSource`, `modelVersion`, `analysisUnit`) and the `validationContext` table added (displayed only, not validated); `pprob` bounds widened to 1e-9 to 1 − 1e-9.

- **Date**: 2026-08-29
- **Summary**: Comprehensive documentation suite created & verified against active schemas and backend implementation.

## 2. Options Reference (`.a.yaml`)

Tests are paired within each case, and cases must be independent across rows. This analysis
does not adjust for repeated specimens or lesions from the same patient; those data require
a method that accounts for clustering.

The main statistical comparison table tests overall accuracy. The Differences table also
reports separate, unadjusted McNemar p-values for sensitivity and specificity. Holm adjustment
of overall-accuracy pairwise p-values applies only when more than one pair is informative
(has discordant observations).

For three tests, Cochran's Q uses an exact conditional upper-tail p-value when at most
30 complete cases have differing correctness across tests. The calculation conditions on
each case's number of correct results and assumes exchangeability of test labels under the
null, in addition to independent cases. A deterministic convolution enumerates the column-total
distribution without random simulation. Concordant cases do not affect this p-value. Q is
reported with an empty degrees-of-freedom cell for exact inference.

Above 30 informative cases, the existing chi-square approximation (2 df) is retained and
explicitly identified. The threshold bounds computation; it is not a guarantee that every
larger sample has an accurate asymptotic approximation. Table notes and copy-ready text report
the method and informative count. Zero informative cases still produce no global test.
See [NIST's Cochran test reference](https://www.itl.nist.gov/div898/software/dataplot/refman1/auxillar/cochran.htm)
for the large-sample nature of the chi-square approximation.

Level options have no schema default: the generated R wrapper requires all Level arguments
explicitly. Since analysis 1.1.0 every selected variable needs distinct positive AND negative
levels; pass `NULL` only for the levels of an unselected `test3`. The `NULL` entries below describe
the unset internal option state, not optional arguments in the R wrapper.

| Option | Type | Default | Description |
| :--- | :--- | :--- | :--- |
| `data` | `Data` | `NULL` |  |
| `referenceType` | `List` | `legacy` | Reference interpretation (`legacy` = not specified, `disease`, `consensus`, `method`) |
| `referenceDescription` | `String` | `''` | Comparator description |
| `targetDefinition` | `String` | `''` | Target definition |
| `gold` | `Variable` | `NULL` | Reference / comparator |
| `goldPositive` | `Level` | `NULL` | Reference positive level |
| `goldNegative` | `Level` | `NULL` | Reference negative level (required) |
| `test1` | `Variable` | `NULL` | Test 1 (Required) |
| `test1Positive` | `Level` | `NULL` | Test 1 Positive Level |
| `test1Negative` | `Level` | `NULL` | Test 1 Negative Level |
| `test2` | `Variable` | `NULL` | Test 2 (Required for Comparison) |
| `test2Positive` | `Level` | `NULL` | Test 2 Positive Level |
| `test2Negative` | `Level` | `NULL` | Test 2 Negative Level |
| `test3` | `Variable` | `NULL` | Test 3 (Optional) |
| `test3Positive` | `Level` | `NULL` | Test 3 Positive Level |
| `test3Negative` | `Level` | `NULL` | Test 3 Negative Level |
| `pp` | `Bool` | `FALSE` | Prior probability (prevalence) |
| `pprob` | `Number` | `0.3` | Prior probability (prevalence); bounds 1e-9 to 1 − 1e-9 |
| `od` | `Bool` | `FALSE` | Original data |
| `fnote` | `Bool` | `FALSE` | Footnotes |
| `ci` | `Bool` | `FALSE` | 95 percent CI |
| `plot` | `Bool` | `FALSE` | Comparison plot |
| `excludeIndeterminate` | `Bool` | `FALSE` | Exclude other observed levels (FALSE: extra levels stop the analysis) |
| `radarplot` | `Bool` | `FALSE` | Radar plot |
| `heatmap` | `Bool` | `FALSE` | Concordance heatmap |
| `opa` | `Bool` | `FALSE` | Overall percent agreement with CI |
| `niMargin` | `Number` | `75` | Minimum acceptable OPA (percent) |
| `useOpaCriterion` | `Bool` | `FALSE` | Apply minimum OPA criterion |
| `ciMethod` | `List` | `wilson` | CI Method for Agreement |
| `stratify` | `Variable` | `NULL` | Stratification Variable |
| `statComp` | `Bool` | `FALSE` | Statistical comparison |
| `mcnemarMethod` | `List` | `midp` | McNemar Test (`midp` = mid-p; `asymptotic` = asymptotic chi-squared without continuity correction) |
| `showSummary` | `Bool` | `FALSE` | Summary |
| `showExplanations` | `Bool` | `FALSE` | Explanations |
| `showReportSentence` | `Bool` | `FALSE` | Report sentence |
| `showDescriptiveReport` | `Bool` | `FALSE` | Descriptive report templates |
| `showValidationContext` | `Bool` | `FALSE` | Validation context (shows `validationContext`) |
| `cohortRole` | `String` | `''` | Cohort role (displayed only, not validated) |
| `samplingDesign` | `String` | `''` | Sampling design (displayed only, not validated) |
| `thresholdSource` | `String` | `''` | Threshold source (displayed only, not validated) |
| `modelVersion` | `String` | `''` | Model version (displayed only, not validated) |
| `analysisUnit` | `String` | `''` | Unit of observation (displayed only, not validated) |

## 3. Results Definition (`.r.yaml`)

| Output ID | Type | Title | Description |
| :--- | :--- | :--- | :--- |
| `text1` | `Preformatted` | `Original Data` |  |
| `text2` | `Html` | `Original Data` |  |
| `cTable1` | `Table` | `Test 1 - Recoded Data` |  |
| `epirTable1` | `Table` | `Test 1 - Confidence Intervals` |  |
| `cTable2` | `Table` | `Test 2 - Recoded Data` |  |
| `epirTable2` | `Table` | `Test 2 - Confidence Intervals` |  |
| `cTable3` | `Table` | `Test 3 - Recoded Data` |  |
| `epirTable3` | `Table` | `Test 3 - Confidence Intervals` |  |
| `comparisonTable` | `Table` | `Decision Test Comparison` |  |
| `opaTable` | `Table` | `Overall Percent Agreement (Descriptive)` |  |
| `stratifiedTable` | `Table` | `Stratified Diagnostic Accuracy` |  |
| `mcnemarTable` | `Table` | `Statistical Comparison of Test Accuracy` |  |
| `diffTable` | `Table` | `Differences with 95% Confidence Intervals` |  |
| `plot1` | `Image` | `Test Comparison` |  |
| `plotRadar` | `Image` | `Radar Plot Comparison` |  |
| `plotHeatmap` | `Image` | `Concordance Heatmap` |  |
| `summaryReport` | `Html` | `Summary` |  |
| `reportSentence` | `Html` | `Manuscript-Ready Report` |  |
| `explanationsContent` | `Html` | `Statistical Explanations & Glossary` |  |
| `clinicalReport` | `Html` | `Descriptive Summary & Report Templates` |  |
| `aboutAnalysis` | `Html` | `About This Analysis` |  |
| `notices` | `Html` | `Important Information` |  |
| `levelMapping` | `Table` | `Selected Levels and Exclusions` | Per variable: selected levels and positive/negative/other/missing counts (all modes) |
| `referenceDefinition` | `Table` | `Reference and target` | Reference interpretation, descriptions, level rules, population (all modes) |
| `comparatorCounts` | `Table` | `Binary comparator counts` | consensus/method only |
| `comparatorPopulation` | `Table` | `Comparison populations` | consensus/method only |
| `comparatorMetrics` | `Table` | `Agreement with comparator` | PPA/NPA/OPA, reverse conditionals, specific agreement; exact binomial CI when `ci` |
| `comparatorReport` | `Html` | `Comparator interpretation` | consensus/method only |
| `comparatorDifferences` | `Table` | `Paired agreement differences` | consensus/method with `statComp`; McNemar, Newcombe CI, Holm over pair x metric |
| `comparatorStrata` | `Table` | `Agreement by stratum` | consensus/method with `stratify` |
| `comparatorPlot` / `comparatorRadar` / `comparatorHeatmap` | `Image` | `Comparator agreement (...)` | consensus/method with `plot` / `radarplot` / `heatmap` |
| `validationContext` | `Table` | `Validation context` | With `showValidationContext`: the five provenance strings plus `referenceType`/`referenceDescription`, filled by `.dvProvenance()` from `.init()`; displayed only, not validated (all modes) |

## 4. Architecture & Data Flow Diagram

```mermaid
flowchart TD
  subgraph UI[jamovi UI / .u.yaml]
    U1[User Input & Variables]
    U2[Analysis Settings & Controls]
  end

  subgraph Opts[Options Schema / .a.yaml]
    O1[Options Parsing & Types]
    O2[Default Value Validation]
  end

  subgraph Backend[Backend Logic / R/decisioncompare.b.R]
    B1[Input Validation & Data Sanitization]
    B2[Statistical Computation Engine]
    B3[Result Objects Formatting]
  end

  subgraph Res[Results Schema / .r.yaml]
    R1[Summary & Statistics Tables]
    R2[Visual Plots & Graphics]
    R3[Clinical Interpretation & Notices]
  end

  U1 --> O1
  U2 --> O2
  O1 --> B1
  O2 --> B1
  B1 --> B2
  B2 --> B3
  B3 --> R1
  B3 --> R2
  B3 --> R3
```

## 5. Execution Sequence

```mermaid
sequenceDiagram
  autonumber
  actor User as Clinician / Analyst
  participant UI as jamovi Interface
  participant Backend as R Backend (decisioncompareClass)
  participant Engine as Statistical Packages
  participant Results as Results View

  User->>UI: Selects variables and options
  UI->>Backend: Dispatches .run() with options payload
  Backend->>Backend: Validates observations & factor levels
  Backend->>Engine: Computes statistical models / visual layers
  Engine-->>Backend: Returns model estimates & graphics
  Backend->>Results: Populates tables, charts, and notices
  Results-->>User: Displays formatted tables & interactive plots
```

## 6. Change Impact & Safety Guidelines

- **Data Filtering**: Ensure observations with missing values are handled gracefully according to analysis options.
- **Formula Conflicts**: Use isolated environment calls or base formula methods when interacting with `ggstatsplot` or formula parsers.
- **Safe Deparsing**: Use `deparse(val)` in syntax generation (`asSource()`) to escape column names with spaces or special symbols.
