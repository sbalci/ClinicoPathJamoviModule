# Q-TWiST Analysis

Perform Quality-adjusted Time Without Symptoms or Toxicity (Q-TWiST)
analysis to compare treatments using both quantity and quality of life.
Particularly valuable in oncology trials where treatments may extend
survival but with significant toxicity or disease symptoms.

## Usage

``` r
qtwist(
  data,
  time_os,
  event_os,
  event_os_level,
  time_pfs,
  event_pfs,
  event_pfs_level,
  treatment,
  toxicity_method = "fixed_window",
  toxicity_window = 3,
  toxicity_probability = 0.5,
  toxicity_duration_var,
  toxicity_indicator_var,
  toxicity_indicator_level,
  toxicity_start_var,
  toxicity_end_var,
  tau = 24,
  tau_selection = "user_specified",
  utility_tox = 0.5,
  utility_twist = 1,
  utility_rel = 0.5,
  sensitivity_analysis = TRUE,
  utility_range_tox = "0, 0.25, 0.5, 0.75, 1.0",
  utility_range_rel = "0, 0.25, 0.5, 0.75, 1.0",
  threshold_analysis = FALSE,
  confidence_level = 0.95,
  bootstrap_ci = TRUE,
  bootstrap_samples = 1000,
  bootstrap_seed = 2025,
  stratify_by,
  pooled_analysis = TRUE,
  show_state_partition = TRUE,
  show_qtwist_scores = TRUE,
  show_treatment_difference = TRUE,
  show_sensitivity_table = TRUE,
  show_rmst_components = FALSE,
  show_descriptive_stats = TRUE,
  plot_partitioned_survival = TRUE,
  plot_qtwist_comparison = TRUE,
  plot_sensitivity = TRUE,
  plot_km_curves = FALSE,
  plot_color_scheme = "clinical",
  showExplanations = TRUE,
  showClinicalGuidance = TRUE,
  showFormulas = FALSE,
  showReferences = TRUE
)
```

## Arguments

- data:

  Dataset containing overall survival, progression-free survival, and
  toxicity information.

- time_os:

  Time to death or censoring in consistent units (months recommended).
  This represents the total follow-up time from
  randomization/enrollment.

- event_os:

  Event indicator for overall survival. Can be numeric (1=death,
  0=censored) or factor (specify death level below).

- event_os_level:

  The level indicating death when using factor variables (e.g., "Dead",
  "Deceased"). If numeric, use level "1" for death.

- time_pfs:

  Time to disease progression, relapse, or death (whichever comes
  first). Must be in the same units as overall survival time. PFS time
  should be \<= OS time for each patient.

- event_pfs:

  Event indicator for progression-free survival. 1 = progression or
  death, 0 = censored (or specify factor level).

- event_pfs_level:

  The level indicating progression or death when using factor variables.

- treatment:

  Treatment or group variable for comparison. Q-TWiST analysis requires
  exactly 2 treatment groups for comparison. The analysis will use all
  available levels (must be exactly 2 groups).

- toxicity_method:

  Method for defining the toxicity (TOX) period in Q-TWiST analysis.
  Fixed window uses same period for all patients; individual duration
  allows patient-specific toxicity times.

- toxicity_window:

  Duration of toxicity assessment window in months (default: 3 months).
  Commonly used values: 3 months (1 treatment cycle), 6 months (adjuvant
  therapy). This defines the TOX period for all patients when using
  fixed window method.

- toxicity_probability:

  Proportion of patients experiencing grade 3-4 toxicity during the
  window. Used when toxicity indicator variable not provided. Can be
  estimated from pilot data or literature.

- toxicity_duration_var:

  Variable containing individual patient toxicity durations (in months).
  For patients without toxicity, use 0. For those with toxicity, specify
  duration of grade 3-4 adverse events.

- toxicity_indicator_var:

  Optional binary indicator of whether patient experienced grade 3-4
  toxicity. If provided, used to weight toxicity time calculations.

- toxicity_indicator_level:

  Level indicating presence of grade 3-4 toxicity (e.g., "Yes", "Grade
  3-4", 1).

- toxicity_start_var:

  Time when toxicity begins (months from baseline). Use 0 for toxicity
  starting at treatment initiation.

- toxicity_end_var:

  Time when toxicity resolves (months from baseline). TOX duration =
  toxicity_end - toxicity_start.

- tau:

  Restricted time horizon for Q-TWiST analysis in months. All survival
  estimates are restricted to this maximum follow-up time. Common
  values: 24 months (2 years), 36 months (3 years), 60 months (5 years).
  Choose based on median survival and clinical relevance.

- tau_selection:

  Method for selecting time horizon (tau). User-specified allows manual
  entry; automatic methods select based on data characteristics.

- utility_tox:

  Quality-of-life utility weight for time with treatment toxicity. 0 =
  worst possible health, 1 = perfect health. Typical range: 0.3-0.7
  depending on toxicity severity. Literature values: chemotherapy
  toxicity ≈ 0.5, severe toxicity ≈ 0.3.

- utility_twist:

  Quality-of-life utility weight for time without symptoms or toxicity.
  Typically set to 1.0 (perfect health) as the reference state. Should
  generally be \>= utility_tox and \>= utility_rel.

- utility_rel:

  Quality-of-life utility weight for time after disease
  relapse/progression. Accounts for symptomatic disease burden. Typical
  range: 0.3-0.6 depending on disease severity and symptoms. Literature
  values: metastatic disease ≈ 0.5, symptomatic relapse ≈ 0.4.

- sensitivity_analysis:

  Examine how Q-TWiST results vary across different utility weight
  assumptions. Essential for robust conclusions, as utility weights are
  subjective. Shows when treatment preference changes based on
  quality-of-life values.

- utility_range_tox:

  Comma-separated list of utility values for TOX state sensitivity
  analysis. Example: "0, 0.3, 0.5, 0.7, 1.0"

- utility_range_rel:

  Comma-separated list of utility values for REL state sensitivity
  analysis.

- threshold_analysis:

  Find threshold utility values where treatment preference changes.
  Identifies "break-even" points for clinical decision-making.

- confidence_level:

  Confidence level for interval estimation (default: 0.95 for 95 percent
  CI).

- bootstrap_ci:

  Use bootstrap resampling to calculate confidence intervals for Q-TWiST
  treatment differences. Recommended for robust inference.

- bootstrap_samples:

  Number of bootstrap resamples for confidence interval calculation.
  More samples = more precise estimates but longer computation time.
  1000 samples is usually sufficient; use 2000+ for publication.

- bootstrap_seed:

  Random seed for bootstrap sampling (for reproducibility).

- stratify_by:

  Variables for stratified analysis (e.g., site, stage, biomarker
  status). Separate Q-TWiST analyses will be performed for each stratum.

- pooled_analysis:

  Display overall Q-TWiST results in addition to stratified results.

- show_state_partition:

  Display table showing mean time spent in each health state (TOX,
  TWiST, REL) for each treatment arm.

- show_qtwist_scores:

  Display quality-adjusted survival (Q-TWiST) scores for each treatment
  with applied utility weights.

- show_treatment_difference:

  Display Q-TWiST difference between treatment arms with confidence
  intervals and statistical significance.

- show_sensitivity_table:

  Display results across varying utility weight combinations.

- show_rmst_components:

  Show underlying restricted mean survival time calculations for overall
  survival and progression-free survival.

- show_descriptive_stats:

  Display sample sizes, median survival times, and event rates for each
  treatment arm.

- plot_partitioned_survival:

  Display stacked area plot showing TOX, TWiST, and REL components over
  time for each treatment arm.

- plot_qtwist_comparison:

  Bar chart comparing Q-TWiST components (TOX, TWiST, REL) between
  treatment arms with utility weights applied.

- plot_sensitivity:

  Contour plot or heatmap showing Q-TWiST treatment difference across
  varying utility weight combinations.

- plot_km_curves:

  Display standard Kaplan-Meier survival curves for OS and PFS for
  reference alongside Q-TWiST results.

- plot_color_scheme:

  Color scheme for partitioned survival plots. TOX (toxicity), TWiST
  (good quality), REL (relapse) states.

- showExplanations:

  Include detailed explanations of Q-TWiST methodology, interpretation
  guidance, and clinical significance.

- showClinicalGuidance:

  Provide clinical interpretation assistance including treatment
  implications and decision-making considerations.

- showFormulas:

  Display mathematical formulas for Q-TWiST calculations and statistical
  tests.

- showReferences:

  Display key references for Q-TWiST methodology and applications.

## Value

A results object containing:

|                                   |     |     |     |     |          |
|-----------------------------------|-----|-----|-----|-----|----------|
| `results$welcomeMessage`          |     |     |     |     | a html   |
| `results$methodologyExplanation`  |     |     |     |     | a html   |
| `results$descriptiveStats`        |     |     |     |     | a table  |
| `results$rmstComponents`          |     |     |     |     | a table  |
| `results$statePartition`          |     |     |     |     | a table  |
| `results$qtwistScores`            |     |     |     |     | a table  |
| `results$treatmentDifference`     |     |     |     |     | a table  |
| `results$stateDifferences`        |     |     |     |     | a table  |
| `results$sensitivityTable`        |     |     |     |     | a table  |
| `results$thresholdAnalysis`       |     |     |     |     | a table  |
| `results$clinicalInterpretation`  |     |     |     |     | a html   |
| `results$statisticalFormulas`     |     |     |     |     | a html   |
| `results$keyReferences`           |     |     |     |     | a html   |
| `results$partitionedSurvivalPlot` |     |     |     |     | an image |
| `results$qtwistComparisonPlot`    |     |     |     |     | an image |
| `results$sensitivityPlot`         |     |     |     |     | an image |
| `results$kmCurvesPlot`            |     |     |     |     | an image |
| `results$forestPlot`              |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$descriptiveStats$asDF`

`as.data.frame(results$descriptiveStats)`

## Details

**What Q-TWiST Does:** Partitions overall survival into three health
states with different quality-of-life weights:

- TOX: Time with treatment toxicity (typically μ = 0-0.5)

- TWiST: Time without symptoms or toxicity (μ = 1.0, perfect health)

- REL: Time after disease relapse/progression (typically μ = 0-0.5)

**Key Features:**

- State-based survival partitioning with clinical significance

- Utility weight application for quality adjustment

- Sensitivity analysis across utility values

- Bootstrap confidence intervals for treatment differences

- Visual comparison with partitioned survival plots

**Clinical Applications:**

- Adjuvant therapy evaluation (balancing toxicity vs. benefit)

- Treatment comparison when survival similar but quality differs

- Shared decision-making (patient-relevant quality metrics)

- Regulatory submissions (FDA/EMA recognized endpoint)
