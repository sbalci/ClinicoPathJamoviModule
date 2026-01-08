# Clinical Lab Data Lollipop Chart Guide

## Dataset Overview
The `clinical_lab_data.csv` contains 300 observations of laboratory values from a clinical study with the following structure:

### Variables
- **patient_id**: Unique patient identifier (1-300)
- **treatment_group**: 4 levels (Control, Drug A, Drug B, Combination)
- **hemoglobin**: Hemoglobin levels (g/dL) - Normal: 12-16 (female), 14-18 (male)
- **white_blood_cells**: WBC count (×10⁹/L) - Normal: 4-11
- **platelet_count**: Platelet count (×10⁹/L) - Normal: 150-400
- **creatinine**: Serum creatinine (mg/dL) - Normal: 0.6-1.2
- **bilirubin**: Total bilirubin (mg/dL) - Normal: 0.3-1.2
- **albumin**: Serum albumin (g/dL) - Normal: 3.5-5.0
- **disease_severity**: 3 levels (Mild, Moderate, Severe)
- **age_group**: 4 levels (18-30, 31-50, 51-70, >70)
- **hospital**: 3 levels (Hospital A, Hospital B, Hospital C)

## Clinical Use Cases with Lollipop Charts

### 1. Treatment Comparison
Compare lab values across different treatment groups to assess efficacy:

```r
# Hemoglobin response to treatment
lollipop(
    data = clinical_lab_data,
    dep = "hemoglobin",
    group = "treatment_group",
    sortBy = "value_desc",
    showMean = TRUE,
    title = "Hemoglobin Levels by Treatment"
)
```

### 2. Disease Severity Assessment
Visualize how lab values correlate with disease severity:

```r
# Albumin as marker of disease severity
lollipop(
    data = clinical_lab_data,
    dep = "albumin",
    group = "disease_severity",
    useHighlight = TRUE,
    highlight = "Severe",
    orientation = "horizontal",
    colorScheme = "clinical",
    title = "Albumin Levels by Disease Severity"
)
```

### 3. Age-Related Changes
Examine age-related patterns in kidney function:

```r
# Creatinine by age with abnormal threshold
lollipop(
    data = clinical_lab_data,
    dep = "creatinine",
    group = "age_group",
    conditionalColor = TRUE,
    colorThreshold = 1.2,  # Upper limit of normal
    lineType = "dashed",
    title = "Creatinine by Age (Abnormal >1.2 mg/dL)"
)
```

### 4. Hospital Benchmarking
Compare laboratory performance across hospitals:

```r
# Platelet counts with lower normal limit
lollipop(
    data = clinical_lab_data,
    dep = "platelet_count",
    group = "hospital",
    baseline = 150,  # Lower normal limit
    useHighlight = TRUE,
    highlight = "Hospital A",
    showValues = TRUE,
    title = "Platelet Counts by Hospital"
)
```

### 5. Infection Monitoring
Track white blood cell counts for infection surveillance:

```r
# WBC with upper limit threshold
lollipop(
    data = clinical_lab_data,
    dep = "white_blood_cells",
    group = "treatment_group",
    conditionalColor = TRUE,
    colorThreshold = 11,  # Upper normal limit
    orientation = "horizontal",
    lineWidth = 2,
    title = "WBC Count by Treatment (ULN: 11)"
)
```

## Advanced Clinical Applications

### Liver Function Panel
```r
# Bilirubin levels with jaundice threshold
lollipop(
    data = clinical_lab_data,
    dep = "bilirubin",
    group = "disease_severity",
    conditionalColor = TRUE,
    colorThreshold = 1.2,  # Jaundice threshold
    sortBy = "value_desc",
    pointSize = 5,
    title = "Bilirubin Levels (Jaundice >1.2 mg/dL)"
)
```

### Multi-Factor Analysis
```r
# Combine age and treatment effects
library(dplyr)
clinical_lab_data %>%
    mutate(group_combined = paste(age_group, treatment_group, sep = "-")) %>%
    lollipop(
        dep = "creatinine",
        group = "group_combined",
        orientation = "horizontal",
        sortBy = "value_desc",
        title = "Creatinine by Age-Treatment Combination"
    )
```

## Clinical Interpretation Guidelines

### Color Coding Recommendations
- **conditionalColor**: Use for abnormal value detection
  - Orange: Above threshold (abnormal high)
  - Blue: Below threshold (normal or abnormal low)
- **useHighlight**: Focus on specific risk groups
  - Highlight "Severe" disease category
  - Highlight specific treatment arms
  - Highlight outlier hospitals

### Baseline Usage
- **Hemoglobin**: baseline = 12 (anemia threshold)
- **Platelets**: baseline = 150 (thrombocytopenia threshold)
- **Albumin**: baseline = 3.5 (hypoalbuminemia threshold)
- **Creatinine**: baseline = 1.2 (renal impairment threshold)

### Sorting Strategies
- **value_desc**: Identify highest risk patients/groups
- **value_asc**: Find lowest performers
- **original**: Maintain clinical protocol order
- **group_alpha**: Standard reporting order

## Quality Control Applications

### Laboratory Performance
```r
# Monitor lab turnaround or quality metrics
lollipop(
    data = clinical_lab_data,
    dep = "hemoglobin",
    group = "hospital",
    showMean = TRUE,
    showValues = TRUE,
    title = "Hemoglobin Testing Consistency by Hospital"
)
```

### Treatment Monitoring
```r
# Track treatment response over severity levels
lollipop(
    data = clinical_lab_data,
    dep = "albumin",
    group = "treatment_group",
    baseline = 3.5,  # Clinical decision point
    conditionalColor = TRUE,
    colorThreshold = 4.0,  # Target level
    title = "Albumin Response to Treatment (Target >4.0)"
)
```

## Data Loading

### From CSV file
```r
clinical_lab_data <- read.csv("data-raw/clinical_lab_data.csv")
```

### From R package (if included)
```r
data("clinical_lab_data", package = "ClinicoPath")
```

### Direct from .rda file
```r
load("temp/data/clinical_lab_data.rda")
```

## Statistical Considerations

- **Sample Size**: 300 patients provide adequate power for group comparisons
- **Group Balance**: Check for unequal group sizes that may affect interpretation
- **Clinical Relevance**: Use domain-specific thresholds and reference ranges
- **Multiple Comparisons**: Consider adjustment when analyzing multiple lab values

## Export Options

Results can be exported for clinical reports:
```r
# Generate and save plot
p <- lollipop(data = clinical_lab_data, dep = "creatinine", group = "age_group")
ggsave("clinical_report_creatinine.png", plot = p, width = 10, height = 6, dpi = 300)
```