# Pathology Interrater Reliability

Comprehensive interrater reliability analysis including Cohen's kappa (2
raters), Fleiss' kappa (3+ raters), Krippendorff's alpha, and consensus
analysis. Provides agreement statistics, visualization, and clinical
interpretation for categorical rating data.

## Usage

``` r
pathagreement(
  data,
  vars = NULL,
  sft = FALSE,
  heatmap = FALSE,
  heatmapDetails = FALSE,
  heatmapTheme = "viridis",
  wght = "unweighted",
  exct = FALSE,
  multiraterMethod = "auto",
  fleissCI = TRUE,
  kripp = FALSE,
  krippMethod = "nominal",
  consensus = FALSE,
  consensus_method = "majority",
  tie_breaking = "exclude",
  show_consensus_table = FALSE,
  showClinicalSummary = FALSE,
  showAboutAnalysis = FALSE,
  showAssumptions = FALSE,
  showWeightedKappaGuide = FALSE,
  showStatisticalGlossary = FALSE,
  styleDistanceMetric = "agreement",
  raterCharacteristics = FALSE,
  experienceVar = NULL,
  trainingVar = NULL,
  institutionVar = NULL,
  specialtyVar = NULL,
  identifyDiscordantCases = FALSE,
  caseID = NULL,
  icc = FALSE,
  bootstrap = FALSE,
  bootstrapSamples = 1000,
  pairwiseAnalysis = FALSE,
  categoryAnalysis = FALSE,
  outlierAnalysis = FALSE,
  pathologyContext = FALSE,
  gwetAC = FALSE,
  pabak = FALSE,
  sampleSizePlanning = FALSE,
  targetKappa = 0.8,
  targetPrecision = 0.1,
  raterBiasAnalysis = FALSE,
  agreementTrendAnalysis = FALSE,
  caseDifficultyScoring = FALSE,
  agreementStabilityAnalysis = FALSE,
  performClustering = FALSE,
  clusteringMethod = "ward",
  nStyleGroups = 3,
  autoSelectGroups = FALSE,
  showClusteringHeatmap = TRUE,
  heatmapColorScheme = "diagnostic",
  identifyDiscordant = FALSE,
  discordantThreshold = 0.5,
  raterExperience = NULL,
  raterSpecialty = NULL,
  raterInstitution = NULL,
  raterVolume = NULL,
  referenceStandard = NULL,
  useMetadataRows = FALSE,
  showInlineComments = FALSE,
  showClusteringInterpretation = FALSE,
  enhancedErrorGuidance = TRUE,
  showProgressIndicators = TRUE
)
```

## Arguments

- data:

  The data as a data frame. Each row represents a case/subject, and
  columns represent different raters/observers.

- vars:

  Variables representing different raters/observers. Each variable
  should contain the ratings/diagnoses given by each observer for the
  same set of cases.

- sft:

  Show frequency tables for each rater and cross-tabulation tables for
  pairwise comparisons.

- heatmap:

  Show agreement heatmap visualization with color-coded agreement
  levels.

- heatmapDetails:

  Show detailed heatmap with kappa values and confidence intervals for
  all rater pairs.

- heatmapTheme:

  Choose color scheme for the agreement heatmap visualization.

- wght:

  Weighting scheme for kappa analysis. Use 'squared' or 'equal' only
  with ordinal variables. Weighted kappa accounts for the degree of
  disagreement.

- exct:

  Use exact method for Fleiss' kappa calculation with 3 or more raters.
  More accurate but computationally intensive.

- multiraterMethod:

  Choose specific method for multi-rater agreement analysis or use
  automatic selection.

- fleissCI:

  Calculate 95 percent confidence intervals for Fleiss' kappa using
  asymptotic standard errors.

- kripp:

  Calculate Krippendorff's alpha, a generalized measure of reliability
  for any number of observers and data types.

- krippMethod:

  Measurement level for Krippendorff's alpha calculation. Choose based
  on your data type.

- consensus:

  Perform consensus scoring analysis to determine agreed-upon ratings
  from multiple raters.

- consensus_method:

  Method for determining consensus scores from multiple raters.

- tie_breaking:

  How to handle cases where no consensus can be reached using the
  selected method.

- show_consensus_table:

  Display detailed consensus scoring results including individual rater
  scores and consensus outcomes.

- showClinicalSummary:

  Show clinical summary with plain-language interpretation of agreement
  statistics and their practical implications.

- showAboutAnalysis:

  Show educational information about inter-rater reliability analysis,
  when to use it, and what the outputs mean.

- showAssumptions:

  Show important assumptions, data requirements, common pitfalls, and
  interpretation guidelines for the analysis.

- showWeightedKappaGuide:

  Show explanatory guide for weighted kappa options, including when to
  use linear vs quadratic weighting schemes.

- showStatisticalGlossary:

  Show glossary of statistical terms (kappa, ICC, alpha, etc.) with
  clinical interpretations and usage guidelines.

- styleDistanceMetric:

  Distance metric for measuring diagnostic similarity between raters for
  style clustering.

- raterCharacteristics:

  Include rater background characteristics (experience, training,
  institution) in style analysis.

- experienceVar:

  Optional variable containing rater experience information (years of
  experience, level of training, etc.)

- trainingVar:

  Optional variable containing rater training institution or background
  information

- institutionVar:

  Optional variable containing rater current institution or location
  information

- specialtyVar:

  Optional variable containing rater medical specialty or subspecialty
  information

- identifyDiscordantCases:

  Identify cases that distinguish different diagnostic styles - useful
  for training and consensus development.

- caseID:

  Optional variable containing case identifiers. If not specified, cases
  will be numbered automatically.

- icc:

  Calculate ICC for continuous or ordinal data. Provides additional
  reliability measures beyond kappa.

- bootstrap:

  Calculate bootstrap confidence intervals for Krippendorff's alpha and
  other statistics.

- bootstrapSamples:

  Number of bootstrap samples for confidence interval calculation.

- pairwiseAnalysis:

  Detailed analysis of agreement between each pair of raters.

- categoryAnalysis:

  Agreement analysis for each diagnostic category separately.

- outlierAnalysis:

  Identify cases with unusually poor agreement across raters.

- pathologyContext:

  Calculate pathology-specific metrics including diagnostic accuracy,
  sensitivity, and specificity when gold standard is available.

- gwetAC:

  Calculate Gwet's AC1 and AC2 coefficients, which are more robust than
  kappa for high agreement scenarios and less affected by prevalence.

- pabak:

  Calculate Prevalence-Adjusted Bias-Adjusted Kappa (PABAK) to address
  prevalence and bias issues in agreement studies.

- sampleSizePlanning:

  Perform sample size planning calculations for agreement studies with
  specified precision requirements.

- targetKappa:

  Target kappa value for sample size planning calculations.

- targetPrecision:

  Target precision for confidence interval width in sample size
  planning.

- raterBiasAnalysis:

  Analyze systematic tendencies and biases for each rater compared to
  the consensus or average ratings.

- agreementTrendAnalysis:

  Analyze how agreement changes over time or case sequence, useful for
  training effect assessment.

- caseDifficultyScoring:

  Quantify inherent case difficulty based on inter-rater disagreement
  patterns and provide difficulty scores.

- agreementStabilityAnalysis:

  Bootstrap-based stability measures to assess the consistency of
  agreement statistics across different samples.

- performClustering:

  Identify diagnostic style groups among raters using hierarchical
  clustering. Implements methodology from Usubutun et al. (2012) Modern
  Pathology. Clusters raters based on diagnosis pattern similarity to
  reveal systematic differences in diagnostic approach.

- clusteringMethod:

  Hierarchical clustering linkage method. Ward's method minimizes
  within-group variance and is recommended for identifying distinct
  diagnostic styles (Usubutun 2012).

- nStyleGroups:

  Number of diagnostic style groups to identify. Original study found 3
  groups: conservative (under-diagnosis), moderate (majority), and
  sensitive (aligns with expert).

- autoSelectGroups:

  Use silhouette method or within-cluster sum of squares to
  automatically determine optimal number of style groups.

- showClusteringHeatmap:

  Display Cases × Raters heatmap with hierarchical dendrograms showing
  diagnostic patterns and style groups.

- heatmapColorScheme:

  Color scheme for clustering heatmap. Diagnostic uses distinct colors
  per category as in Usubutun (2012).

- identifyDiscordant:

  Flag cases with high inter-rater disagreement that distinguish
  diagnostic style groups. These cases are useful for training and
  quality assurance discussions.

- discordantThreshold:

  Minimum disagreement proportion for flagging discordant cases. 0.5
  means at least 50 percent of raters disagreed with majority diagnosis.

- raterExperience:

  Years of experience for each rater. Will be tested for association
  with style group membership.

- raterSpecialty:

  Specialty or practice type (e.g., specialist vs generalist,
  subspecialty). Will be tested for association with style group
  membership.

- raterInstitution:

  Training or current practice institution. Will be tested for
  association with style group membership. Usubutun (2012) found no
  association, suggesting diagnostic style is personal rather than
  institutional.

- raterVolume:

  Number of cases seen per month or year. Will be tested for association
  with style group membership.

- referenceStandard:

  Expert consensus or reference standard diagnosis. Used to compare
  style groups and identify which group aligns most closely with expert
  judgment.

- useMetadataRows:

  Enable extraction of rater characteristics from special metadata rows
  in the dataset. Metadata rows should have case_id starting with
  "META\_" (e.g., META_experience, META_specialty). Values in rater
  columns will be extracted as characteristics for association testing.

- showInlineComments:

  Show detailed statistical explanations and interpretations inline with
  results for educational purposes.

- showClusteringInterpretation:

  Display explanatory guide for interpreting clustering results,
  diagnostic style groups, and discordant cases. Useful for
  understanding clinical implications.

- enhancedErrorGuidance:

  Provide detailed error messages and suggestions for resolving common
  issues in agreement analysis.

- showProgressIndicators:

  Display progress indicators for computationally intensive operations
  like bootstrap calculations.

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$todo` |  |  |  |  | a html |
| `results$warnings` |  |  |  |  | a html |
| `results$overviewTable` |  |  |  |  | a table |
| `results$kappaTable` |  |  |  |  | a table |
| `results$iccTable` |  |  |  |  | a table |
| `results$pairwiseTable` |  |  |  |  | a table |
| `results$categoryTable` |  |  |  |  | a table |
| `results$outlierTable` |  |  |  |  | a table |
| `results$diagnosticAccuracyTable` |  |  |  |  | a table |
| `results$diagnosticStyleTable` |  |  |  |  | a table |
| `results$styleSummaryTable` |  |  |  |  | a table |
| `results$discordantCasesTable` |  |  |  |  | a table |
| `results$krippTable` |  |  |  |  | a table |
| `results$consensusTable` |  |  |  |  | a table |
| `results$consensusSummary` |  |  |  |  | a table |
| `results$heatmapPlot` |  |  |  |  | an image |
| `results$pairwisePlot` |  |  |  |  | an image |
| `results$categoryPlot` |  |  |  |  | an image |
| `results$confusionMatrixPlot` |  |  |  |  | an image |
| `results$diagnosticStyleDendrogram` |  |  |  |  | an image |
| `results$diagnosticStyleHeatmap` |  |  |  |  | an image |
| `results$diagnosticStyleCombined` |  |  |  |  | an image |
| `results$raterFrequencyTables$frequencyTable` |  |  |  |  | a table |
| `results$crosstabTable` |  |  |  |  | a table |
| `results$clinicalSummary` |  |  |  |  | a html |
| `results$reportTemplate` |  |  |  |  | a html |
| `results$aboutAnalysis` |  |  |  |  | a html |
| `results$assumptions` |  |  |  |  | a html |
| `results$weightedKappaGuide` |  |  |  |  | a html |
| `results$statisticalGlossary` |  |  |  |  | a html |
| `results$gwetACTable` |  |  |  |  | a table |
| `results$pabakTable` |  |  |  |  | a table |
| `results$sampleSizeTable` |  |  |  |  | a table |
| `results$raterBiasTable` |  |  |  |  | a table |
| `results$agreementTrendTable` |  |  |  |  | a table |
| `results$caseDifficultyTable` |  |  |  |  | a table |
| `results$stabilityTable` |  |  |  |  | a table |
| `results$trendPlot` |  |  |  |  | an image |
| `results$biasPlot` |  |  |  |  | an image |
| `results$difficultyPlot` |  |  |  |  | an image |
| `results$inlineComments` |  |  |  |  | a html |
| `results$styleGroupSummary` |  |  |  |  | a table |
| `results$styleGroupProfiles` |  |  |  |  | a table |
| `results$discordantCasesCluster` |  |  |  |  | a table |
| `results$characteristicAssociations` |  |  |  |  | a table |
| `results$referenceComparison` |  |  |  |  | a table |
| `results$clusteringHeatmap` |  |  |  |  | Heatmap showing diagnostic patterns with dual dendrograms (raters and cases) |
| `results$clusterDendrogram` |  |  |  |  | Dendrogram showing hierarchical relationships between raters |
| `results$silhouettePlot` |  |  |  |  | Silhouette plot showing cluster separation and cohesion |
| `results$clusteringInterpretation` |  |  |  |  | Explanatory guide for understanding diagnostic style groups and cluster results |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$overviewTable$asDF`

`as.data.frame(results$overviewTable)`
