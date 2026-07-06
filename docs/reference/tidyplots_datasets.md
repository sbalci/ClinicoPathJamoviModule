# Comprehensive Test Datasets for Tidyplots Function

These datasets provide comprehensive examples for testing and
demonstrating the tidyplots function capabilities across different
domains and use cases. Each dataset is designed to highlight specific
features and best practices for scientific visualization.

## Format

Each dataset is a data frame with the following characteristics:

**tidyplots_medical:**

- patient_id:

  Unique patient identifier

- treatment:

  Treatment group: Control, Drug_A, Drug_B

- gender:

  Patient gender: Male, Female

- age_group:

  Age category: Young, Middle, Elderly

- age:

  Continuous age variable

- bp_reduction:

  Primary outcome: Blood pressure reduction

- biomarker_1, biomarker_2:

  Secondary biomarker measurements

- qol_score:

  Quality of life score (0-100)

- timepoint:

  Study timepoint: Baseline, Week_4, Week_8, Week_12

- hospital:

  Study site: Hospital_A, Hospital_B, Hospital_C

- response:

  Response category: High_Response, Moderate_Response, Low_Response

**tidyplots_education:**

- student_id:

  Unique student identifier

- teaching_method:

  Teaching approach: Traditional, Interactive, Online, Hybrid

- subject:

  Academic subject: Math, Science, Literature

- school_type:

  School type: Public, Private

- grade_level:

  Student grade level (9-12)

- pre_test_score, post_test_score:

  Test scores before and after intervention

- improvement:

  Score improvement (post - pre)

- engagement_score:

  Student engagement measure (0-100)

- study_hours:

  Weekly study hours

- performance_level:

  Performance category: Excellent, Good, Satisfactory, Needs_Improvement

**tidyplots_business:**

- salesperson_id:

  Unique salesperson identifier

- marketing_strategy:

  Strategy: Email, Social_Media, Direct_Mail, Phone, Mixed

- product_category:

  Product type: Electronics, Clothing, Home_Garden, Sports

- region:

  Geographic region: North, South, East, West

- quarter:

  Business quarter: Q1, Q2, Q3, Q4

- experience_years:

  Years of sales experience

- sales_amount:

  Total sales revenue

- leads_generated:

  Number of sales leads

- conversion_rate:

  Lead conversion percentage

- customer_satisfaction:

  Customer satisfaction score (1-10)

- performance_tier:

  Performance category: Top_Performer, High_Performer,
  Average_Performer, Needs_Improvement

**tidyplots_environmental:**

- station_id:

  Monitoring station identifier

- month:

  Calendar month

- measurement_type:

  Type: Temperature, Humidity, Air_Quality, Noise_Level

- location_type:

  Location: Urban, Suburban, Rural

- value:

  Measurement value (units vary by type)

- data_quality:

  Quality indicator: High, Medium, Low

- alert_level:

  Alert status: Normal, Warning, Critical

- season:

  Season grouping: Winter, Spring, Summer, Fall

**tidyplots_demo:**

- group:

  Group identifier: Group_A, Group_B, Group_C, Group_D

- category:

  Category: Category_1, Category_2

- treatment:

  Treatment: Control, Treatment

- time_point:

  Time: Baseline, Follow_up

- score:

  Main outcome variable

- measurement_1, measurement_2:

  Additional continuous variables

- success:

  Binary outcome variable

- count_var:

  Count/discrete variable

## Source

These datasets were created using simulated data with realistic patterns
and relationships designed to showcase tidyplots functionality across
different research domains.

## Details

A collection of datasets designed to showcase the comprehensive features
of the tidyplots function, including various plot types, statistical
elements, and customization options.

The collection includes five specialized datasets:

**1. Medical Research Dataset (tidyplots_medical):**

- Clinical trial data with treatment effects

- Variables: patient characteristics, treatments, biomarkers, outcomes

- Use cases: Treatment comparisons, biomarker analysis, clinical
  outcomes

- Sample size: 2,160 observations

**2. Educational Research Dataset (tidyplots_education):**

- Student performance across teaching methods

- Variables: student demographics, teaching methods, test scores

- Use cases: Educational effectiveness, performance analysis

- Sample size: 4,800 observations

**3. Business Analytics Dataset (tidyplots_business):**

- Sales performance and marketing strategy analysis

- Variables: sales metrics, marketing strategies, regional data

- Use cases: Business intelligence, sales analysis, performance tracking

- Sample size: 48,000 observations

**4. Environmental Monitoring Dataset (tidyplots_environmental):**

- Environmental measurements across monitoring stations

- Variables: temperature, humidity, air quality, location types

- Use cases: Environmental monitoring, seasonal analysis, pollution
  tracking

- Sample size: 1,728 observations

**5. Simple Demo Dataset (tidyplots_demo):**

- Basic dataset for tutorials and quick demonstrations

- Variables: groups, categories, treatments, scores

- Use cases: Learning tidyplots, quick examples, method demonstrations

- Sample size: 100 observations

## References

Bengler, J. (2024). tidyplots: Streamlined plotting with tidy data.
<https://jbengler.github.io/tidyplots/>

## See also

[`tidyplots`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/tidyplots.md)
for the main plotting function

## Examples

``` r
if (FALSE) { # \dontrun{
# Load all datasets
data(tidyplots_datasets)

# Load individual datasets
data(tidyplots_medical)
data(tidyplots_education)
data(tidyplots_business)
data(tidyplots_environmental) 
data(tidyplots_demo)

# Basic scatter plot with medical data
tidyplots(data = tidyplots_medical,
          xvar = "age",
          yvar = "bp_reduction",
          color = "treatment")

# Box plot with statistical elements
tidyplots(data = tidyplots_medical,
          xvar = "treatment",
          yvar = "bp_reduction",
          plotType = "boxplot",
          color = "treatment",
          showMean = TRUE,
          showCI = TRUE,
          colorScheme = "friendly")

# Violin plot with education data
tidyplots(data = tidyplots_education,
          xvar = "teaching_method",
          yvar = "improvement",
          plotType = "violin",
          color = "school_type",
          violinPoints = TRUE,
          showMedian = TRUE,
          colorScheme = "seaside")

# Line plot with environmental data
tidyplots(data = tidyplots_environmental[tidyplots_environmental$measurement_type == "Temperature", ],
          xvar = "month",
          yvar = "value",
          plotType = "line",
          color = "location_type",
          lineType = "mean",
          showCI = TRUE,
          ciType = "ribbon")

# Histogram with demo data
tidyplots(data = tidyplots_demo,
          xvar = "score",
          plotType = "histogram",
          color = "group",
          histogramBins = 20,
          showDistribution = TRUE,
          distributionType = "density")

# Faceted plot with business data
tidyplots(data = tidyplots_business,
          xvar = "experience_years",
          yvar = "sales_amount",
          color = "marketing_strategy",
          facet = "region",
          plotType = "points",
          pointType = "jitter",
          showMean = TRUE,
          colorScheme = "viridis")

# Bar plot with statistical testing
tidyplots(data = tidyplots_medical,
          xvar = "treatment",
          yvar = "qol_score",
          plotType = "bar",
          color = "treatment",
          barType = "mean",
          showSEM = TRUE,
          showPValue = TRUE,
          showSignificance = TRUE)
} # }
```
