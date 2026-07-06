# Population Health Analytics

Population Health Analytics provides comprehensive cohort-level health
status monitoring and epidemiological analysis with population health
metrics, risk stratification, geographic health mapping, and predictive
health outcome modeling for public health surveillance, population
management, and healthcare system optimization through evidence-based
population health insights.

## Usage

``` r
populationhealth(
  data,
  patientID,
  healthOutcomes,
  demographics,
  geographic,
  timeVariable,
  riskFactors,
  healthcareUtilization,
  population_stratification = TRUE,
  risk_stratification = TRUE,
  geographic_analysis = TRUE,
  temporal_trends = TRUE,
  health_disparities = TRUE,
  predictive_modeling = TRUE,
  population_type = "general",
  analysis_scope = "comprehensive",
  geographic_level = "county",
  time_window = 12,
  risk_threshold = 0.7,
  disparity_analysis = TRUE,
  social_determinants = TRUE,
  intervention_analysis = FALSE,
  comparative_analysis = TRUE,
  quality_metrics = TRUE,
  surveillance_system = TRUE,
  outcome_prediction = TRUE,
  resource_allocation = FALSE,
  population_visualization = TRUE,
  geographic_mapping = TRUE,
  trend_visualization = TRUE,
  disparity_plots = TRUE,
  interactive_dashboard = TRUE
)
```

## Arguments

- data:

  .

- patientID:

  Unique patient identifier for population tracking and analysis

- healthOutcomes:

  Health outcome measures (BMI, blood pressure, lab values, disease
  indicators)

- demographics:

  Demographic characteristics (age, gender, ethnicity, socioeconomic
  status)

- geographic:

  Geographic identifiers (zip code, county, region, coordinates)
  (optional)

- timeVariable:

  Time variables for temporal health trend analysis (optional)

- riskFactors:

  Risk factors (smoking, lifestyle, comorbidities, environmental)
  (optional)

- healthcareUtilization:

  Healthcare utilization metrics (visits, admissions, procedures)
  (optional)

- population_stratification:

  Enable population stratification analysis by demographics and risk
  factors

- risk_stratification:

  Perform population-level risk stratification and early identification

- geographic_analysis:

  Enable geographic health mapping and spatial analysis

- temporal_trends:

  Analyze temporal trends in population health outcomes

- health_disparities:

  Assess health disparities across demographic and socioeconomic groups

- predictive_modeling:

  Enable predictive modeling for population health outcomes

- population_type:

  Type of population for specialized analysis approaches

- analysis_scope:

  Scope of population health analysis

- geographic_level:

  Geographic level for spatial health analysis

- time_window:

  Time window for population health analysis and trending

- risk_threshold:

  Threshold for high-risk population identification

- disparity_analysis:

  Enable advanced health disparity analysis methods

- social_determinants:

  Include social determinants of health in analysis

- intervention_analysis:

  Analyze impact of population health interventions

- comparative_analysis:

  Enable comparison across different population segments

- quality_metrics:

  Calculate population health quality and performance indicators

- surveillance_system:

  Enable public health surveillance and monitoring capabilities

- outcome_prediction:

  Predict future population health outcomes and trends

- resource_allocation:

  Analyze healthcare resource allocation and optimization

- population_visualization:

  Create comprehensive population health visualizations

- geographic_mapping:

  Generate geographic health maps and spatial visualizations

- trend_visualization:

  Create temporal trend visualizations for population health

- disparity_plots:

  Generate health disparity and equity visualizations

- interactive_dashboard:

  Create interactive population health dashboards and monitoring tools

## Value

A results object containing:

|                                          |     |     |     |     |          |
|------------------------------------------|-----|-----|-----|-----|----------|
| `results$todo`                           |     |     |     |     | a html   |
| `results$populationSummary`              |     |     |     |     | a html   |
| `results$demographicsTable`              |     |     |     |     | a table  |
| `results$healthOutcomesTable`            |     |     |     |     | a table  |
| `results$riskStratificationTable`        |     |     |     |     | a table  |
| `results$geographicAnalysisTable`        |     |     |     |     | a table  |
| `results$temporalTrendsTable`            |     |     |     |     | a table  |
| `results$healthDisparitiesTable`         |     |     |     |     | a table  |
| `results$predictiveModelingTable`        |     |     |     |     | a table  |
| `results$surveillanceTable`              |     |     |     |     | a table  |
| `results$qualityMetricsTable`            |     |     |     |     | a table  |
| `results$interventionAnalysisTable`      |     |     |     |     | a table  |
| `results$resourceAllocationTable`        |     |     |     |     | a table  |
| `results$populationReport`               |     |     |     |     | a html   |
| `results$executiveSummary`               |     |     |     |     | a html   |
| `results$populationDashboard`            |     |     |     |     | a html   |
| `results$populationOverviewPlot`         |     |     |     |     | an image |
| `results$geographicHealthMap`            |     |     |     |     | an image |
| `results$temporalTrendsPlot`             |     |     |     |     | an image |
| `results$riskStratificationPlot`         |     |     |     |     | an image |
| `results$healthDisparityPlot`            |     |     |     |     | an image |
| `results$predictiveModelingPlot`         |     |     |     |     | an image |
| `results$interactivePopulationDashboard` |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$demographicsTable$asDF`

`as.data.frame(results$demographicsTable)`
