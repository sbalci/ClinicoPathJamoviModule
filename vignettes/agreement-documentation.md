# Interrater Reliability - Developer Documentation

## 1. Overview

- **Function**: `agreement`
- **Title**: Interrater Reliability
- **Module**: `meddecideT`
- **Files**:
  - `jamovi/agreement.u.yaml` - User Interface Definition
  - `jamovi/agreement.a.yaml` - Options & Schema Definition
  - `jamovi/agreement.r.yaml` - Results Layout & Tables
  - `R/agreement.b.R` - Backend Implementation
- **Summary**: Function for Interrater Reliability.

## 1a. Changelog

- **Date**: 2026-08-29
- **Summary**: Comprehensive documentation suite created & verified against active schemas and backend implementation.

## 2. Options Reference (`.a.yaml`)

| Option | Type | Default | Description |
| :--- | :--- | :--- | :--- |
| `data` | `Data` | `NULL` |  |
| `vars` | `Variables` | `NULL` | Raters |
| `baConfidenceLevel` | `Number` | `0.95` | Confidence Level for LoA |
| `confLevel` | `Number` | `0.95` | Confidence Level for CIs |
| `proportionalBias` | `Bool` | `FALSE` | Test for proportional bias |
| `showBlandAltmanGuide` | `Bool` | `FALSE` | When to use Bland-Altman analysis |
| `blandAltmanPlot` | `Bool` | `FALSE` | Bland-Altman plot |
| `agreementHeatmap` | `Bool` | `FALSE` | Agreement heatmap (confusion matrix) |
| `heatmapColorScheme` | `List` | `bluered` | Heatmap Color Scheme |
| `heatmapShowPercentages` | `Bool` | `TRUE` | Percentages in cells |
| `heatmapShowCounts` | `Bool` | `TRUE` | Counts in cells |
| `heatmapAnnotationSize` | `Number` | `3.5` | Cell Annotation Size |
| `showAgreementHeatmapGuide` | `Bool` | `FALSE` | When to use agreement heatmap |
| `sft` | `Bool` | `FALSE` | Frequency tables |
| `wght` | `List` | `unweighted` | Weighted Kappa (Ordinal Data Only) |
| `exct` | `Bool` | `FALSE` | Exact kappa (3+ raters) |
| `showLevelInfo` | `Bool` | `FALSE` | Level ordering information |
| `kripp` | `Bool` | `FALSE` | Calculate Krippendorff's alpha |
| `krippMethod` | `List` | `nominal` | Data Type for Krippendorff's Alpha |
| `bootstrap` | `Bool` | `FALSE` | Bootstrap confidence intervals |
| `showKrippGuide` | `Bool` | `FALSE` | When to use Krippendorff's alpha |
| `gwet` | `Bool` | `FALSE` | Calculate Gwet's AC1/AC2 |
| `gwetWeights` | `List` | `unweighted` | Weights for Gwet's AC |
| `showGwetGuide` | `Bool` | `FALSE` | When to use Gwet's AC |
| `pabak` | `Bool` | `FALSE` | Calculate PABAK & prevalence/Bias indices |
| `showPABAKGuide` | `Bool` | `FALSE` | When to use PABAK |
| `icc` | `Bool` | `FALSE` | Calculate ICC (continuous data) |
| `showICCGuide` | `Bool` | `FALSE` | When to use ICC |
| `iccType` | `List` | `icc21` | ICC Model |
| `meanPearson` | `Bool` | `FALSE` | Calculate mean Pearson correlation (linear association) |
| `showMeanPearsonGuide` | `Bool` | `FALSE` | When to use mean Pearson correlation |
| `linCCC` | `Bool` | `FALSE` | Lin's concordance correlation coefficient (CCC) |
| `showLinCCCGuide` | `Bool` | `FALSE` | When to use Lin's CCC |
| `tdi` | `Bool` | `FALSE` | Total deviation index (TDI) |
| `tdiCoverage` | `Number` | `90` | Coverage Probability ( percent) |
| `tdiLimit` | `Number` | `10` | Acceptable Limit |
| `showTDIGuide` | `Bool` | `FALSE` | When to use TDI |
| `iota` | `Bool` | `FALSE` | Calculate iota coefficient (multivariate agreement) |
| `iotaStandardize` | `Bool` | `TRUE` | Standardize variables (iota) |
| `showIotaGuide` | `Bool` | `FALSE` | When to use iota coefficient |
| `finn` | `Bool` | `FALSE` | Calculate Finn coefficient (variance-based agreement) |
| `finnLevels` | `Integer` | `3` | Number of Rating Categories (Finn) |
| `finnModel` | `List` | `oneway` | Finn Model Type |
| `showFinnGuide` | `Bool` | `FALSE` | When to use Finn coefficient |
| `lightKappa` | `Bool` | `FALSE` | Calculate light's kappa (3+ raters) |
| `showLightKappaGuide` | `Bool` | `FALSE` | When to use light's kappa |
| `kendallW` | `Bool` | `FALSE` | Calculate Kendall's W (concordance for rankings) |
| `showKendallWGuide` | `Bool` | `FALSE` | When to use Kendall's W |
| `robinsonA` | `Bool` | `FALSE` | Calculate Robinson's A (ordinal agreement index) |
| `showRobinsonAGuide` | `Bool` | `FALSE` | When to use Robinson's A |
| `meanSpearman` | `Bool` | `FALSE` | Calculate mean Spearman rho (average rank correlation) |
| `showMeanSpearmanGuide` | `Bool` | `FALSE` | When to use mean Spearman rho |
| `raterBias` | `Bool` | `FALSE` | Test for rater bias (systematic differences) |
| `showRaterBiasGuide` | `Bool` | `FALSE` | When to use rater bias test |
| `bhapkar` | `Bool` | `FALSE` | Bhapkar test (marginal homogeneity for 2 raters) |
| `showBhapkarGuide` | `Bool` | `FALSE` | When to use Bhapkar test |
| `stuartMaxwell` | `Bool` | `FALSE` | Stuart-Maxwell test (marginal homogeneity for 2 raters) |
| `showStuartMaxwellGuide` | `Bool` | `FALSE` | When to use Stuart-Maxwell test |
| `maxwellRE` | `Bool` | `FALSE` | Maxwell's RE (random error index) |
| `showMaxwellREGuide` | `Bool` | `FALSE` | When to use Maxwell's RE |
| `interIntraRater` | `Bool` | `FALSE` | Inter/Intra-rater reliability (test-retest) |
| `interIntraSeparator` | `String` | `_` | Column Name Separator (Inter/Intra) |
| `showInterIntraRaterGuide` | `Bool` | `FALSE` | When to use inter/Intra-rater reliability |
| `pairwiseKappa` | `Bool` | `FALSE` | Calculate pairwise kappa (vs reference) |
| `referenceRater` | `Variable` | `NULL` | Reference Rater Variable |
| `rankRaters` | `Bool` | `FALSE` | Rank raters by performance |
| `showPairwiseKappaGuide` | `Bool` | `FALSE` | When to use pairwise kappa |
| `allPairsKappa` | `Bool` | `FALSE` | All-pairs Cohen's kappa (every rater pair) |
| `allPairsCI` | `Bool` | `TRUE` | Confidence interval for each pair |
| `showAllPairsKappaGuide` | `Bool` | `FALSE` | When to use all-pairs kappa |
| `itemModalCategoryAgreement` | `Bool` | `FALSE` | Per-category item-modal agreement |
| `showItemModalGuide` | `Bool` | `FALSE` | When to use per-category item-modal agreement |
| `hierarchicalKappa` | `Bool` | `FALSE` | Hierarchical/Multilevel kappa |
| `clusterVariable` | `Variable` | `NULL` | Cluster/Institution Variable |
| `iccHierarchical` | `Bool` | `FALSE` | Hierarchical ICC decomposition |
| `clusterSpecificKappa` | `Bool` | `TRUE` | Cluster-specific kappa estimates |
| `varianceDecomposition` | `Bool` | `TRUE` | Variance component decomposition |
| `shrinkageEstimates` | `Bool` | `FALSE` | Shrinkage (empirical bayes) estimates |
| `testClusterHomogeneity` | `Bool` | `TRUE` | Test cluster homogeneity |
| `clusterRankings` | `Bool` | `FALSE` | Cluster performance rankings |
| `showHierarchicalGuide` | `Bool` | `FALSE` | When to use hierarchical kappa |
| `conditionVariable` | `Variable` | `NULL` | Condition/Method Variable |
| `mixedEffectsComparison` | `Bool` | `FALSE` | Mixed-effects condition comparison |
| `multipleTestCorrection` | `List` | `none` | Multiple Testing Correction |
| `showMixedEffectsGuide` | `Bool` | `FALSE` | When to use mixed-effects comparison |
| `confusionMatrix` | `Bool` | `FALSE` | Confusion matrix table |
| `confusionNormalize` | `List` | `none` | Normalization |
| `showConfusionMatrixGuide` | `Bool` | `FALSE` | When to use confusion matrix |
| `bootstrapCI` | `Bool` | `FALSE` | Bootstrap confidence intervals |
| `nBoot` | `Integer` | `1000` | Number of Bootstrap Samples |
| `showBootstrapCIGuide` | `Bool` | `FALSE` | When to use bootstrap CIs |
| `multiAnnotatorConcordance` | `Bool` | `FALSE` | Multi-annotator concordance |
| `predictionColumn` | `Integer` | `1` | Prediction Column (First Rater) |
| `showConcordanceF1Guide` | `Bool` | `FALSE` | When to use multi-annotator concordance |
| `specificAgreement` | `Bool` | `FALSE` | Specific agreement indices (category-focused) |
| `specificPositiveCategory` | `String` | `` | Positive Category (Binary Analysis) |
| `specificAllCategories` | `Bool` | `TRUE` | Calculate for all categories |
| `specificConfidenceIntervals` | `Bool` | `TRUE` | Include confidence intervals |
| `showSpecificAgreementGuide` | `Bool` | `FALSE` | When to use specific agreement indices |
| `showSummary` | `Bool` | `FALSE` | Plain-language summary |
| `showAbout` | `Bool` | `FALSE` | About this analysis |
| `consensusName` | `String` | `consensus_rating` | Consensus Variable Name |
| `consensusVar` | `Output` | `NULL` | Create Consensus Variable |
| `consensusRule` | `List` | `majority` | Consensus Rule |
| `tieBreaker` | `List` | `exclude` | Tie Handling |
| `loaVariable` | `Bool` | `FALSE` | Create case agreement categorization |
| `detailLevel` | `List` | `detailed` | Detail Level |
| `simpleThreshold` | `Number` | `50` | Majority Threshold ( percent) - Simple Mode |
| `loaThresholds` | `List` | `custom` | Categorization Method - Detailed Mode |
| `loaHighThreshold` | `Number` | `75` | High Threshold ( percent) - Detailed Mode |
| `loaLowThreshold` | `Number` | `56` | Low Threshold ( percent) - Detailed Mode |
| `loaVariableName` | `String` | `agreement_level` | Variable Name for LoA |
| `showLoaTable` | `Bool` | `TRUE` | LoA distribution table |
| `raterProfiles` | `Bool` | `FALSE` | Rater profile plots (distribution comparison) |
| `raterProfileType` | `List` | `boxplot` | Profile Plot Type |
| `raterProfileShowPoints` | `Bool` | `FALSE` | Individual data points |
| `showRaterProfileGuide` | `Bool` | `FALSE` | When to use rater profile plots |
| `agreementBySubgroup` | `Bool` | `FALSE` | Agreement by subgroup (stratified analysis) |
| `subgroupVariable` | `Variable` | `NULL` | Subgroup Variable |
| `subgroupForestPlot` | `Bool` | `TRUE` | Forest plot |
| `subgroupMinCases` | `Integer` | `10` | Minimum Cases per Subgroup |
| `showSubgroupGuide` | `Bool` | `FALSE` | When to use agreement by subgroup |
| `raterClustering` | `Bool` | `FALSE` | Rater clustering (identify rating pattern groups) |
| `clusterMethod` | `List` | `hierarchical` | Clustering Method |
| `clusterDistance` | `List` | `correlation` | Distance Metric |
| `clusterLinkage` | `List` | `average` | Linkage Method (Hierarchical) |
| `nClusters` | `Integer` | `3` | Number of Clusters (K-means) |
| `showDendrogram` | `Bool` | `TRUE` | Dendrogram |
| `showClusterHeatmap` | `Bool` | `TRUE` | Cluster heatmap |
| `showRaterClusterGuide` | `Bool` | `FALSE` | When to use rater clustering |
| `caseClustering` | `Bool` | `FALSE` | Case clustering (identify rating pattern groups) |
| `caseClusterMethod` | `List` | `hierarchical` | Clustering Method |
| `caseClusterDistance` | `List` | `correlation` | Distance Metric |
| `caseClusterLinkage` | `List` | `average` | Linkage Method (Hierarchical) |
| `nCaseClusters` | `Integer` | `3` | Number of Clusters (K-means) |
| `showCaseDendrogram` | `Bool` | `TRUE` | Dendrogram |
| `showCaseClusterHeatmap` | `Bool` | `TRUE` | Cluster heatmap |
| `showCaseClusterGuide` | `Bool` | `FALSE` | When to use case clustering |
| `pairedAgreementTest` | `Bool` | `FALSE` | Compare agreement between two conditions |
| `conditionBVars` | `Variables` | `NULL` | Condition B Raters |
| `pairedBootN` | `Integer` | `2000` | Bootstrap Replications |
| `showPairedAgreementGuide` | `Bool` | `FALSE` | When to use paired agreement comparison |
| `agreementSampleSize` | `Bool` | `FALSE` | Calculate sample size for agreement study |
| `ssMetric` | `List` | `kappa` | Agreement Metric |
| `ssKappaNull` | `Number` | `0.4` | Null Kappa (H0) |
| `ssKappaAlt` | `Number` | `0.7` | Expected Kappa (H1) |
| `ssNRaters` | `Integer` | `2` | Number of Raters |
| `ssNCategories` | `Integer` | `4` | Number of Categories |
| `ssAlpha` | `Number` | `0.05` | Significance Level |
| `ssPower` | `Number` | `0.8` | Desired Power |
| `showSampleSizeGuide` | `Bool` | `FALSE` | When to use sample size calculator |
| `seed` | `Integer` | `42` | Random Seed |

## 3. Results Definition (`.r.yaml`)

| Output ID | Type | Title | Description |
| :--- | :--- | :--- | :--- |
| `welcome` | `Html` | `` |  |
| `irrtableHeading` | `Preformatted` | `Interrater Reliability` |  |
| `irrtable` | `Table` | `Interrater Reliability` |  |
| `contingencyTableHeading` | `Preformatted` | `Data Summary` |  |
| `contingencyTable` | `Table` | `Contingency Table (2 Raters)` |  |
| `ratingCombinationsTable` | `Table` | `Rating Combinations (3+ Raters)` |  |
| `contingencyTableExplanation` | `Html` | `About Contingency Table & Rating Combinations` |  |
| `blandAltmanHeading` | `Preformatted` | `Bland-Altman Method Comparison` |  |
| `blandAltman` | `Image` | `Bland-Altman Plot` |  |
| `agreementHeatmapPlot` | `Image` | `Agreement Heatmap (Confusion Matrix)` |  |
| `agreementHeatmapExplanation` | `Html` | `About Agreement Heatmap` |  |
| `blandAltmanExplanation` | `Html` | `About Bland-Altman Analysis` |  |
| `blandAltmanStats` | `Table` | `Bland-Altman Statistics` |  |
| `krippTableHeading` | `Preformatted` | `Krippendorff's Alpha` |  |
| `krippTable` | `Table` | `Krippendorff's Alpha Results` |  |
| `krippExplanation` | `Html` | `About Krippendorff's Alpha` |  |
| `lightKappaTableHeading` | `Preformatted` | `Additional Categorical Agreement Measures` |  |
| `lightKappaTable` | `Table` | `Light's Kappa Results` |  |
| `lightKappaExplanation` | `Html` | `About Light's Kappa` |  |
| `finnTable` | `Table` | `Finn Coefficient Results (Variance-Based Agreement)` |  |
| `finnExplanation` | `Html` | `About Finn Coefficient` |  |
| `kendallWTable` | `Table` | `Kendall's Coefficient of Concordance (W) Results` |  |
| `kendallWExplanation` | `Html` | `About Kendall's W` |  |
| `robinsonATable` | `Table` | `Robinson's A (Ordinal Agreement Index) Results` |  |
| `robinsonAExplanation` | `Html` | `About Robinson's A` |  |
| `meanSpearmanTable` | `Table` | `Mean Spearman Rho (Average Rank Correlation) Results` |  |
| `meanSpearmanExplanation` | `Html` | `About Mean Spearman Rho` |  |
| `raterBiasHeading` | `Preformatted` | `Marginal Homogeneity Tests` |  |
| `raterBiasTable` | `Table` | `Rater Bias Test Results` |  |
| `raterBiasExplanation` | `Html` | `About Rater Bias Test` |  |
| `bhapkarTable` | `Table` | `Bhapkar Test for Marginal Homogeneity` |  |
| `bhapkarExplanation` | `Html` | `About Bhapkar Test` |  |
| `stuartMaxwellTable` | `Table` | `Stuart-Maxwell Test for Marginal Homogeneity` |  |
| `stuartMaxwellExplanation` | `Html` | `About Stuart-Maxwell Test` |  |
| `pairwiseKappaTable` | `Table` | `Pairwise Kappa (Each Rater vs Reference)` |  |
| `pairwiseKappaExplanation` | `Html` | `About Pairwise Kappa Analysis` |  |
| `allPairsKappaHeading` | `Preformatted` | `All-Pairs Cohen's Kappa` |  |
| `allPairsKappaTable` | `Table` | `All-Pairs Kappa (Every Rater Pair)` |  |
| `allPairsKappaExplanation` | `Html` | `About All-Pairs Kappa Analysis` |  |
| `itemModalAgreementHeading` | `Preformatted` | `Per-Category Item-Modal Agreement` |  |
| `itemModalAgreementTable` | `Table` | `Agreement by Item Modal Category` |  |
| `itemModalAgreementExplanation` | `Html` | `About Per-Category Item-Modal Agreement` |  |
| `hierarchicalHeading` | `Preformatted` | `Hierarchical / Multilevel Agreement` |  |
| `hierarchicalOverallTable` | `Table` | `Hierarchical Kappa - Overall Agreement` |  |
| `clusterSpecificTable` | `Table` | `Cluster-Specific Kappa Estimates` |  |
| `varianceDecompositionTable` | `Table` | `Variance Component Decomposition` |  |
| `hierarchicalICCTable` | `Table` | `Hierarchical ICC Decomposition` |  |
| `homogeneityTestTable` | `Table` | `Cluster Homogeneity Test Results` |  |
| `hierarchicalExplanation` | `Html` | `About Hierarchical/Multilevel Kappa` |  |
| `advancedHeading` | `Preformatted` | `Advanced Agreement Analyses` |  |
| `mixedEffectsTable` | `Table` | `Mixed-Effects Condition Comparison` |  |
| `mixedEffectsVarianceTable` | `Table` | `Mixed-Effects Variance Components` |  |
| `mixedEffectsExplanation` | `Html` | `About Mixed-Effects Condition Comparison` |  |
| `confusionMatrixTable` | `Table` | `Confusion Matrix` |  |
| `perClassMetricsTable` | `Table` | `Per-Class Classification Metrics` |  |
| `confusionMatrixExplanation` | `Html` | `About Confusion Matrix` |  |
| `bootstrapCITable` | `Table` | `Bootstrap Confidence Intervals for Agreement Metrics` |  |
| `bootstrapCIExplanation` | `Html` | `About Bootstrap Confidence Intervals` |  |
| `concordanceF1Table` | `Table` | `Multi-Annotator Concordance Metrics` |  |
| `concordanceF1PerClassTable` | `Table` | `Per-Class Concordance F1` |  |
| `concordanceF1Explanation` | `Html` | `About Multi-Annotator Concordance` |  |
| `gwetHeading` | `Preformatted` | `Chance-Corrected Agreement Variants` |  |
| `gwetTable` | `Table` | `Gwet's AC1/AC2 Results` |  |
| `gwetExplanation` | `Html` | `About Gwet's AC Coefficient` |  |
| `pabakTable` | `Table` | `PABAK & Prevalence/Bias Indices` |  |
| `pabakExplanation` | `Html` | `About PABAK & Prevalence/Bias Indices` |  |
| `iccHeading` | `Preformatted` | `Continuous Agreement Measures` |  |
| `iccTable` | `Table` | `Intraclass Correlation Coefficient (ICC) Results` |  |
| `iccExplanation` | `Html` | `About Intraclass Correlation Coefficient (ICC)` |  |
| `meanPearsonTable` | `Table` | `Mean Pearson Correlation (Linear Association) Results` |  |
| `meanPearsonExplanation` | `Html` | `About Mean Pearson Correlation` |  |
| `linCCCTable` | `Table` | `Lin's Concordance Correlation Coefficient (CCC)` |  |
| `linCCCExplanation` | `Html` | `About Lin's Concordance Correlation Coefficient` |  |
| `tdiTable` | `Table` | `Total Deviation Index (TDI) - Acceptable Agreement Limits` |  |
| `tdiExplanation` | `Html` | `About Total Deviation Index (TDI)` |  |
| `maxwellREHeading` | `Preformatted` | `Error Decomposition & Reliability` |  |
| `maxwellRETable` | `Table` | `Maxwell's Random Error (RE) Index - Error Decomposition` |  |
| `maxwellREExplanation` | `Html` | `About Maxwell's Random Error Index` |  |
| `interIntraRaterIntraTable` | `Table` | `Intra-Rater Reliability (Test-Retest Consistency)` |  |
| `interIntraRaterInterTable` | `Table` | `Inter-Rater Reliability (Between Raters)` |  |
| `interIntraRaterExplanation` | `Html` | `About Inter/Intra-Rater Reliability` |  |
| `iotaTable` | `Table` | `Iota Coefficient Results (Multivariate Agreement)` |  |
| `iotaExplanation` | `Html` | `About Iota Coefficient` |  |
| `weightedKappaGuide` | `Html` | `Weighted Kappa Interpretation Guide` |  |
| `specificAgreementHeading` | `Preformatted` | `Category-Specific Agreement` |  |
| `specificAgreementTable` | `Table` | `Specific Agreement Indices (Category-Focused Agreement)` |  |
| `specificAgreementExplanation` | `Html` | `About Specific Agreement Indices` |  |
| `levelInfoTable` | `Table` | `Level Ordering Information` |  |
| `summary` | `Html` | `Summary` |  |
| `about` | `Html` | `About This Analysis` |  |
| `clinicalUseCases` | `Html` | `Clinical Use Cases & Method Selection Guide` |  |
| `computedVariablesHeading` | `Preformatted` | `Computed Variables` |  |
| `consensusTable` | `Table` | `Consensus Variable Summary` |  |
| `loaTable` | `Table` | `Level of Agreement Distribution` |  |
| `loaDetailTable` | `Table` | `Case-Level Agreement Details` |  |
| `computedVariablesInfo` | `Html` | `Computed Variables Added to Dataset` |  |
| `consensusVar` | `Output` | `Add Consensus Variable to Data` |  |
| `loaOutput` | `Output` | `Add Case Agreement Categorization to Data` |  |
| `raterProfilePlot` | `Image` | `Rater Profile Plots (Rating Distribution by Rater)` |  |
| `raterProfileExplanation` | `Html` | `About Rater Profile Plots` |  |
| `subgroupAgreementTable` | `Table` | `Agreement by Subgroup (Stratified Analysis)` |  |
| `subgroupForestPlotImage` | `Image` | `Forest Plot of Agreement by Subgroup` |  |
| `subgroupExplanation` | `Html` | `About Agreement by Subgroup` |  |
| `raterClusterHeading` | `Preformatted` | `Rater & Case Clustering` |  |
| `raterClusterTable` | `Table` | `Rater Cluster Assignments` |  |
| `raterDendrogram` | `Image` | `Rater Clustering Dendrogram` |  |
| `raterClusterHeatmap` | `Image` | `Rater Similarity Heatmap with Clusters` |  |
| `raterClusterExplanation` | `Html` | `About Rater Clustering` |  |
| `caseClusterTable` | `Table` | `Case Cluster Assignments` |  |
| `caseDendrogram` | `Image` | `Case Clustering Dendrogram` |  |
| `caseClusterHeatmap` | `Image` | `Case Similarity Heatmap with Clusters` |  |
| `caseClusterExplanation` | `Html` | `About Case Clustering` |  |
| `pairedAgreementHeading` | `Preformatted` | `Paired Agreement & Sample Size` |  |
| `pairedAgreementTable` | `Table` | `Paired Agreement Comparison` |  |
| `pairedAgreementExplanation` | `Html` | `About Paired Agreement Comparison` |  |
| `agreementSampleSizeTable` | `Table` | `Sample Size for Agreement Study` |  |
| `agreementSampleSizeExplanation` | `Html` | `About Agreement Sample Size` |  |

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

  subgraph Backend[Backend Logic / R/agreement.b.R]
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
  participant Backend as R Backend (agreementClass)
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

