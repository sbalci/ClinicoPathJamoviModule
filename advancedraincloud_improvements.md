# Advanced Raincloud Plot - Clinical Research Improvements

## Current Strengths ✅
1. **Longitudinal Support**: Excellent for repeated measures (pre/post treatment)
2. **Likert Scale Mode**: Great for patient-reported outcomes (PROs)
3. **Flexible Positioning**: Good for publication-ready figures
4. **Statistical Integration**: Summary statistics and comparisons included
5. **Fixed R6 Class Issue**: Now properly stores analysis data

## Suggested Improvements (Non-Breaking)

### 1. **Clinical Significance Indicators**
Add options to mark clinically meaningful differences:
- **Minimal Clinically Important Difference (MCID)** reference lines
- **Clinical cutoff values** for biomarkers
- **Normal reference ranges** shading

```yaml
# Add to .a.yaml options:
- name: clinical_cutoff
  title: Clinical Cutoff Value
  type: Number
  default: NULL
  description: Add horizontal line for clinical threshold

- name: reference_range_min
  title: Reference Range Minimum
  type: Number
  default: NULL
  
- name: reference_range_max
  title: Reference Range Maximum
  type: Number
  default: NULL

- name: show_mcid
  title: Show MCID Band
  type: Bool
  default: false
  
- name: mcid_value
  title: MCID Value
  type: Number
  default: NULL
```

### 2. **Effect Size Visualization**
Add visual indicators for effect sizes:
- **Cohen's d** for group comparisons
- **Confidence intervals** for mean differences
- **Standardized mean differences**

```yaml
- name: show_effect_size
  title: Display Effect Size
  type: Bool
  default: false

- name: effect_size_type
  title: Effect Size Type
  type: List
  options:
    - title: "Cohen's d"
      name: cohens_d
    - title: "Hedges' g"
      name: hedges_g
    - title: "Glass's delta"
      name: glass_delta
  default: cohens_d
```

### 3. **Change Score Analysis**
For longitudinal data, add change score calculations:
- **Absolute change** from baseline
- **Percent change** from baseline
- **Responder analysis** (improved/stable/worse)

```yaml
- name: show_change_scores
  title: Show Change Analysis
  type: Bool
  default: false
  
- name: baseline_group
  title: Baseline Group Identifier
  type: String
  default: ""
  
- name: responder_threshold
  title: Response Threshold (%)
  type: Number
  default: 20
```

### 4. **Sample Size Annotations**
Add sample size information directly on plot:
- **N per group** labels
- **Missing data indicators**
- **Dropout annotations** for longitudinal

```yaml
- name: show_sample_size
  title: Display Sample Sizes
  type: Bool
  default: true
  
- name: show_missing_info
  title: Show Missing Data Info
  type: Bool
  default: false
```

### 5. **Clinical Trial Features**
Add specific features for RCTs:
- **Treatment arm labeling**
- **Time point annotations**
- **ITT vs PP population indicators**

```yaml
- name: trial_arms
  title: Treatment Arm Labels
  type: String
  default: ""
  
- name: time_labels
  title: Time Point Labels
  type: String
  default: ""
  
- name: population_type
  title: Analysis Population
  type: List
  options:
    - title: "Intention-to-Treat"
      name: itt
    - title: "Per-Protocol"
      name: pp
    - title: "Modified ITT"
      name: mitt
  default: itt
```

### 6. **Export Options Enhancement**
Add clinical research specific export features:
- **Statistical report generation**
- **Figure legend automation**
- **Methods section text**

```yaml
- name: generate_report
  title: Generate Clinical Report
  type: Bool
  default: false
  
- name: include_methods
  title: Include Methods Text
  type: Bool
  default: false
```

### 7. **Biomarker-Specific Features**
For biomarker studies:
- **Log transformation options**
- **Outlier handling strategies**
- **Assay variability bands**

```yaml
- name: log_transform
  title: Log Transform Y-axis
  type: Bool
  default: false
  
- name: outlier_method
  title: Outlier Handling
  type: List
  options:
    - title: "None"
      name: none
    - title: "Winsorize"
      name: winsorize
    - title: "Trim"
      name: trim
  default: none
  
- name: show_cv_bands
  title: Show CV% Bands
  type: Bool
  default: false
```

### 8. **Publication Enhancements**
Additional features for manuscripts:
- **P-value annotations** with adjustment
- **Statistical method footnotes**
- **Journal-specific formatting**

```yaml
- name: p_value_position
  title: P-value Display Position
  type: List
  options:
    - title: "Above Plot"
      name: above
    - title: "In Legend"
      name: legend
    - title: "None"
      name: none
  default: above
  
- name: journal_style
  title: Journal Style Format
  type: List
  options:
    - title: "Default"
      name: default
    - title: "Nature"
      name: nature
    - title: "NEJM"
      name: nejm
    - title: "Lancet"
      name: lancet
  default: default
```

## Implementation Priority

1. **High Priority** (Most useful for clinical research):
   - Clinical cutoff lines
   - Sample size annotations
   - Effect size display
   - Change score analysis

2. **Medium Priority**:
   - Reference ranges
   - Missing data indicators
   - Trial population labels

3. **Low Priority** (Nice to have):
   - Journal styles
   - CV bands
   - Automated reports

## Code Implementation Example

For clinical cutoff lines, add to `.plot` function:

```r
# Add clinical reference lines
if (!is.null(self$options$clinical_cutoff)) {
    p <- p + ggplot2::geom_hline(
        yintercept = self$options$clinical_cutoff,
        linetype = "dashed",
        color = "red",
        size = 1
    ) +
    ggplot2::annotate(
        "text",
        x = Inf,
        y = self$options$clinical_cutoff,
        label = "Clinical Threshold",
        hjust = 1.1,
        vjust = -0.5,
        color = "red"
    )
}

# Add reference range shading
if (!is.null(self$options$reference_range_min) && 
    !is.null(self$options$reference_range_max)) {
    p <- p + ggplot2::annotate(
        "rect",
        xmin = -Inf, xmax = Inf,
        ymin = self$options$reference_range_min,
        ymax = self$options$reference_range_max,
        alpha = 0.2,
        fill = "green"
    )
}
```

## Benefits for Clinical Research

1. **Regulatory Compliance**: Clear visualization of clinically meaningful changes
2. **Publication Ready**: Journal-specific formatting and complete statistical reporting
3. **Trial Reporting**: CONSORT-compliant visualizations with proper annotations
4. **Biomarker Studies**: Appropriate handling of assay data and reference ranges
5. **Patient Outcomes**: Better visualization of PROs and change over time

These improvements maintain backward compatibility while adding significant value for clinical researchers.