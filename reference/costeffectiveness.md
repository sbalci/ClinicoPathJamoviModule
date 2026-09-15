# Cost-Effectiveness Analysis

Cost-Effectiveness Analysis (CEA) for evaluating the economic value of
diagnostic tests, screening programs, and clinical decision strategies.
Calculates Incremental Cost-Effectiveness Ratio (ICER), which represents
the additional cost per additional unit of effectiveness (e.g., cost per
correct diagnosis, cost per QALY gained). The analysis compares new
diagnostic strategies against standard of care or alternative
approaches, visualizing results on the cost-effectiveness plane and
calculating net monetary benefit at various willingness-to-pay
thresholds. Essential for health technology assessment, budget impact
analysis, and justifying adoption of new diagnostic technologies such as
multiplex IHC, AI-assisted pathology, digital pathology platforms, and
molecular tests. Supports deterministic sensitivity analysis (tornado
plots) to identify key cost drivers and probabilistic sensitivity
analysis (Monte Carlo simulation) to quantify uncertainty in
cost-effectiveness conclusions. Particularly valuable for demonstrating
value proposition of advanced diagnostics to hospital administrators,
payers, and policy makers.

## Usage

``` r
costeffectiveness(
  data,
  strategy,
  comparator_level,
  cost,
  effectiveness,
  effectiveness_type = "binary",
  effectiveness_label = "Correct Diagnoses",
  calculate_icer = TRUE,
  calculate_incremental = TRUE,
  dominance_analysis = FALSE,
  net_monetary_benefit = FALSE,
  wtp_threshold = 50000,
  multiple_wtp_thresholds = FALSE,
  wtp_range = "0, 25000, 50000, 100000, 150000",
  confidence_intervals = FALSE,
  ci_method = "bootstrap",
  bootstrap_samples = 1000,
  confidence_level = 0.95,
  deterministic_sensitivity = FALSE,
  sensitivity_parameters =
    "test_cost, follow_up_cost, test_sensitivity, test_specificity",
  sensitivity_range_pct = 20,
  probabilistic_sensitivity = FALSE,
  psa_simulations = 1000,
  cost_distribution = "gamma",
  effect_distribution = "normal",
  voi_analysis = FALSE,
  evpi_population = 10000,
  evppi_parameters = FALSE,
  evppi_focus = "both",
  subgroup_analysis = FALSE,
  subgroup_variable,
  time_horizon = 1,
  discount_costs = FALSE,
  discount_rate_costs = 3,
  discount_effects = FALSE,
  discount_rate_effects = 3,
  plot_ce_plane = TRUE,
  plot_ce_acceptability = FALSE,
  plot_nmb = FALSE,
  plot_tornado = FALSE,
  plot_incremental_frontier = FALSE,
  perspective = "healthcare",
  include_indirect_costs = FALSE,
  currency = "USD",
  cost_year = 2024,
  handling_missing = "complete",
  random_seed = 12345
)
```

## Arguments

- data:

  The data as a data frame.

- strategy:

  Variable indicating which diagnostic strategy was used for each
  observation. Should have at least 2 levels (e.g., "New Test" vs.
  "Standard Care").

- comparator_level:

  Which level of strategy variable represents the comparator
  (baseline/reference) strategy against which others are compared.

- cost:

  Total cost per patient for the diagnostic strategy. Should include all
  relevant costs: test costs, follow-up procedures, treatment costs,
  adverse events. Express in consistent currency units (e.g., USD, EUR).

- effectiveness:

  Effectiveness measure per patient. Can be correct diagnoses (0/1),
  QALYs, life-years gained, cancers detected, etc. Higher values should
  represent better outcomes.

- effectiveness_type:

  Type of effectiveness measure being analyzed. Affects interpretation
  and labeling of ICER results.

- effectiveness_label:

  Custom label for effectiveness measure (e.g., "Cancers Detected",
  "True Positives", "QALYs").

- calculate_icer:

  Calculate Incremental Cost-Effectiveness Ratio. ICER = (Cost_new -
  Cost_comparator) / (Effect_new - Effect_comparator). Represents
  additional cost per additional unit of effectiveness.

- calculate_incremental:

  Display incremental costs and effects relative to comparator strategy.

- dominance_analysis:

  Identify dominated strategies (more costly and less effective) and
  extended dominance (strategies on inefficient frontier).

- net_monetary_benefit:

  Calculate Net Monetary Benefit (NMB) at specified willingness-to-pay
  thresholds. NMB = (Effectiveness × WTP) - Cost. Positive NMB indicates
  cost-effective at that threshold.

- wtp_threshold:

  Willingness-to-pay threshold for primary analysis (e.g., \$50,000 per
  QALY in US, £20,000-30,000 per QALY in UK). Used for net benefit
  calculation.

- multiple_wtp_thresholds:

  Evaluate cost-effectiveness at multiple willingness-to-pay thresholds
  to show robustness of conclusions.

- wtp_range:

  Comma-separated list of willingness-to-pay thresholds for analysis.
  Example: "0, 20000, 30000, 50000, 100000".

- confidence_intervals:

  Calculate confidence intervals for mean costs, mean effects, and ICER
  using bootstrap or parametric methods.

- ci_method:

  Method for confidence interval estimation. Bootstrap is recommended
  for cost data (often skewed). BCa (Bias-Corrected Accelerated)
  provides better coverage for skewed distributions.

- bootstrap_samples:

  Number of bootstrap resamples for confidence interval estimation.

- confidence_level:

  Confidence level for interval estimation.

- deterministic_sensitivity:

  One-way sensitivity analysis varying individual parameters to assess
  impact on ICER. Creates tornado diagram showing most influential
  parameters.

- sensitivity_parameters:

  Comma-separated list of parameter names for deterministic sensitivity
  analysis. These should be column names in the data.

- sensitivity_range_pct:

  Percentage variation around base case for sensitivity analysis. For
  example, 20 percent means vary parameters from 80 percent to 120
  percent of base value.

- probabilistic_sensitivity:

  Monte Carlo simulation to propagate uncertainty in all parameters
  simultaneously. Generates cost-effectiveness acceptability curve
  (CEAC) showing probability of cost-effectiveness at different WTP
  thresholds.

- psa_simulations:

  Number of Monte Carlo simulations for probabilistic sensitivity
  analysis.

- cost_distribution:

  Probability distribution for cost parameter uncertainty. Gamma is
  recommended for costs (non-negative, often right-skewed).

- effect_distribution:

  Probability distribution for effectiveness parameter uncertainty. Beta
  is recommended for proportions, normal for continuous measures.

- voi_analysis:

  Calculate Expected Value of Perfect Information (EVPI) to quantify the
  expected cost of uncertainty in the decision. EVPI represents the
  maximum value of conducting further research to eliminate all
  parameter uncertainty.

- evpi_population:

  Population size affected by the decision over the relevant time
  horizon. EVPI is calculated per-person and then multiplied by
  population to give total value of eliminating uncertainty.

- evppi_parameters:

  Calculate Expected Value of Perfect Parameter Information (EVPPI) for
  specific parameters. Shows which parameters contribute most to
  decision uncertainty.

- evppi_focus:

  Which parameters to include in EVPPI calculation. Useful for
  identifying whether cost or effectiveness uncertainty drives decision
  uncertainty.

- subgroup_analysis:

  Perform cost-effectiveness analysis stratified by subgroups to
  identify populations where new strategy is most cost-effective.

- subgroup_variable:

  Variable defining subgroups for stratified analysis (e.g., disease
  stage, age group, risk category).

- time_horizon:

  Time horizon for cost-effectiveness analysis in years. Short-term for
  diagnostic tests (1 year), longer for treatments affecting survival.

- discount_costs:

  Apply discounting to future costs (relevant for multi-year time
  horizons).

- discount_rate_costs:

  Annual discount rate for future costs (typically 3-5 percent in health
  economics).

- discount_effects:

  Apply discounting to future health effects (QALYs, life-years).

- discount_rate_effects:

  Annual discount rate for future effects (often same as cost discount
  rate).

- plot_ce_plane:

  Scatter plot showing incremental costs (y-axis) vs. incremental
  effects (x-axis). Quadrants indicate dominance relationships.
  Willingness-to-pay threshold shown as diagonal line.

- plot_ce_acceptability:

  CEAC showing probability that each strategy is cost-effective across
  range of willingness-to-pay thresholds. Requires probabilistic
  sensitivity analysis.

- plot_nmb:

  Bar plot showing net monetary benefit for each strategy at specified
  WTP threshold. Strategy with highest NMB is optimal.

- plot_tornado:

  Tornado diagram showing one-way sensitivity analysis results.
  Parameters ordered by impact on ICER, showing range of ICER values
  across parameter uncertainty ranges.

- plot_incremental_frontier:

  Plot showing efficient frontier connecting non-dominated strategies.

- perspective:

  Perspective for cost-effectiveness analysis determines which costs are
  included. Healthcare system includes direct medical costs, societal
  includes productivity losses and patient time costs.

- include_indirect_costs:

  Include indirect costs (productivity losses, patient time) for
  societal perspective.

- currency:

  Currency for cost reporting (e.g., USD, EUR, GBP, JPY).

- cost_year:

  Reference year for cost values (for inflation adjustment if needed).

- handling_missing:

  Method for handling missing cost or effectiveness data.

- random_seed:

  Random seed for bootstrap and Monte Carlo simulations.

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$instructions` |  |  |  |  | a html |
| `results$strategySummary` |  |  |  |  | Summary statistics for each diagnostic strategy |
| `results$incrementalAnalysis` |  |  |  |  | Incremental costs, effects, and ICER relative to comparator |
| `results$netBenefit` |  |  |  |  | Net monetary benefit at specified WTP thresholds |
| `results$multipleWTP` |  |  |  |  | NMB and optimal strategy across WTP range |
| `results$dominanceTable` |  |  |  |  | Identification of dominated and extendedly dominated strategies |
| `results$subgroupCEA` |  |  |  |  | ICER by subgroup |
| `results$sensitivityAnalysis` |  |  |  |  | Impact of individual parameter variation on ICER |
| `results$psaSummary` |  |  |  |  | Monte Carlo simulation results |
| `results$voiSummary` |  |  |  |  | Expected value of perfect information analysis |
| `results$evppiTable` |  |  |  |  | Value of information by parameter group |
| `results$cePlanePlot` |  |  |  |  | Incremental costs vs. incremental effects scatter plot |
| `results$ceAcceptabilityCurve` |  |  |  |  | Probability of cost-effectiveness across WTP thresholds |
| `results$nmbPlot` |  |  |  |  | NMB comparison at specified WTP threshold |
| `results$tornadoPlot` |  |  |  |  | One-way sensitivity analysis results |
| `results$incrementalFrontierPlot` |  |  |  |  | Efficient frontier showing non-dominated strategies |
| `results$interpretation` |  |  |  |  | a html |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$strategySummary$asDF`

`as.data.frame(results$strategySummary)`
