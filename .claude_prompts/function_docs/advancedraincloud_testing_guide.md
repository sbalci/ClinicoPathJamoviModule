# Advanced Raincloud Plot - Comprehensive Testing Guide

## Overview
This guide provides step-by-step instructions for testing all features of the Advanced Raincloud Plot module using the provided test datasets. Each test scenario demonstrates specific clinical research features and validates the implementation.

## Test Datasets
Four datasets are available for comprehensive testing:

1. **`advancedraincloud_data`** - Longitudinal data (900 rows, 15 variables)
2. **`advancedraincloud_baseline`** - Baseline only (300 rows, 13 variables)  
3. **`advancedraincloud_endpoint`** - Week 12 only (300 rows, 13 variables)
4. **`advancedraincloud_change`** - Change scores (130 complete cases, 6 variables)

## Testing Scenarios

### 1. Basic Clinical Cutoff Visualization
**Test:** Clinical threshold lines and sample size annotations

```r
# Load baseline data
data("advancedraincloud_baseline")

# Test clinical cutoff with biomarker data
advancedraincloud(
  data = advancedraincloud_baseline,
  y_var = "biomarker_level",
  x_var = "treatment_arm",
  clinical_cutoff = 100,
  show_sample_size = TRUE,
  show_statistics = TRUE
)
```

**Expected Results:**
- Horizontal red dashed line at biomarker level = 100
- Sample sizes shown for each treatment group (N ≈ 150 each)
- Summary statistics table displayed

### 2. Reference Ranges and MCID Bands
**Test:** Normal reference ranges and minimal clinically important differences

```r
# Test reference ranges for biomarker levels
advancedraincloud(
  data = advancedraincloud_baseline,
  y_var = "biomarker_level", 
  x_var = "treatment_arm",
  reference_range_min = 10,
  reference_range_max = 50,
  show_mcid = TRUE,
  mcid_value = 25,
  log_transform = TRUE,
  show_cv_bands = TRUE
)
```

**Expected Results:**
- Green shaded area between biomarker levels 10-50 (reference range)
- MCID band around reference values
- Log-transformed Y-axis
- CV% bands for assay variability

### 3. Longitudinal Analysis with Connections
**Test:** Repeated measures with patient connections

```r
# Load longitudinal data
data("advancedraincloud_data")

# Test longitudinal connections
advancedraincloud(
  data = advancedraincloud_data,
  y_var = "tumor_size_change",
  x_var = "time_point", 
  id_var = "patient_id",
  fill_var = "treatment_arm",
  show_longitudinal = TRUE,
  clinical_cutoff = -30,
  show_sample_size = TRUE,
  show_missing_info = TRUE
)
```

**Expected Results:**
- Lines connecting individual patients across timepoints
- Color coding by treatment arm
- Clinical cutoff line at -30% (response threshold)
- Sample sizes accounting for missing data
- Missing data information displayed

### 4. Effect Size Analysis
**Test:** Between-group effect size calculations

```r
# Test effect size calculations
advancedraincloud(
  data = advancedraincloud_endpoint,
  y_var = "tumor_size_change",
  x_var = "treatment_arm",
  show_effect_size = TRUE,
  effect_size_type = "cohens_d",
  show_comparisons = TRUE,
  p_value_position = "above"
)
```

**Expected Results:**
- Effect size table with Cohen's d
- Statistical comparison p-values above plot
- Group comparisons results table
- Confidence intervals for effect sizes

### 5. Change Score Analysis
**Test:** Longitudinal change from baseline with responder analysis

```r
# Test change score analysis
advancedraincloud(
  data = advancedraincloud_data,
  y_var = "qol_score",
  x_var = "time_point",
  fill_var = "treatment_arm",
  show_change_scores = TRUE,
  baseline_group = "Baseline",
  responder_threshold = 20,
  show_mcid = TRUE,
  mcid_value = 10
)
```

**Expected Results:**
- Change analysis results table
- Responder classification (improved/stable/worse)
- MCID reference band at ±10 points
- Baseline as reference timepoint

### 6. Biomarker Analysis with Outlier Handling
**Test:** Log transformation and outlier management

```r
# Test biomarker analysis features
advancedraincloud(
  data = advancedraincloud_data,
  y_var = "biomarker_level",
  x_var = "treatment_arm",
  log_transform = TRUE,
  outlier_method = "winsorize",
  reference_range_min = 10,
  reference_range_max = 50,
  show_cv_bands = TRUE,
  show_statistics = TRUE
)
```

**Expected Results:**
- Log-transformed Y-axis scale
- Outliers winsorized at 5th/95th percentiles
- Reference range shading (10-50 units)
- CV% bands for measurement variability
- Updated statistics reflecting transformations

### 7. Likert Scale Mode for Pain Scores
**Test:** Ordinal data visualization with jittering

```r
# Test Likert scale mode
advancedraincloud(
  data = advancedraincloud_data, 
  y_var = "pain_score",
  x_var = "treatment_arm",
  likert_mode = TRUE,
  show_longitudinal = FALSE,
  show_comparisons = TRUE,
  p_value_position = "legend"
)
```

**Expected Results:**
- Y-axis jittering for ordinal data (0-10 scale)
- Appropriate statistical tests for ordinal data
- P-values displayed in legend
- Clear visualization of discrete pain levels

### 8. Clinical Trial Population Analysis
**Test:** Trial-specific features and labeling

```r
# Test clinical trial features
advancedraincloud(
  data = advancedraincloud_data,
  y_var = "tumor_size_change", 
  x_var = "time_point",
  fill_var = "treatment_arm",
  trial_arms = "Placebo,Experimental Drug",
  time_labels = "Baseline,Month 1,Month 3",
  population_type = "itt",
  show_sample_size = TRUE,
  show_missing_info = TRUE
)
```

**Expected Results:**
- Custom treatment arm labels
- Custom time point labels
- ITT population indicator
- Complete trial population summary

### 9. Publication-Ready Output
**Test:** Journal formatting and comprehensive reporting

```r
# Test publication features
advancedraincloud(
  data = advancedraincloud_endpoint,
  y_var = "biomarker_level",
  x_var = "treatment_arm", 
  journal_style = "nature",
  generate_report = TRUE,
  include_methods = TRUE,
  show_effect_size = TRUE,
  p_value_position = "table"
)
```

**Expected Results:**
- Nature journal formatting applied
- Comprehensive clinical analysis report
- Methods section text generated
- P-values in statistical table format
- Publication-ready figure quality

### 10. Comprehensive Multi-Feature Test
**Test:** Multiple features combined

```r
# Test multiple features together
advancedraincloud(
  data = advancedraincloud_data,
  y_var = "qol_score",
  x_var = "time_point",
  id_var = "patient_id",
  fill_var = "treatment_arm",
  show_longitudinal = TRUE,
  clinical_cutoff = 60,
  reference_range_min = 40,
  reference_range_max = 80,
  show_mcid = TRUE,
  mcid_value = 10,
  show_effect_size = TRUE,
  effect_size_type = "hedges_g",
  show_change_scores = TRUE,
  baseline_group = "Baseline",
  responder_threshold = 15,
  show_sample_size = TRUE,
  show_missing_info = TRUE,
  journal_style = "nejm",
  generate_report = TRUE,
  include_methods = TRUE
)
```

**Expected Results:**
- All clinical features working together
- Longitudinal connections with treatment colors
- Multiple reference indicators
- Comprehensive analysis outputs
- NEJM journal formatting
- Complete clinical report

## Validation Checklist

### Visual Elements
- [ ] Raincloud components render correctly (violin, box, points)
- [ ] Clinical cutoff lines appear as red dashed lines
- [ ] Reference ranges show as green shaded areas
- [ ] MCID bands display appropriately
- [ ] Sample size annotations are legible
- [ ] Longitudinal connections draw between correct points
- [ ] Color coding matches treatment groups

### Statistical Outputs
- [ ] Summary statistics table shows correct values
- [ ] Effect size calculations match expected ranges
- [ ] P-values from group comparisons are reasonable
- [ ] Change score analysis produces logical results
- [ ] Missing data counts are accurate
- [ ] Responder classifications make clinical sense

### Data Handling
- [ ] Missing data handled appropriately
- [ ] Outlier methods work as expected
- [ ] Log transformation applied correctly
- [ ] Likert mode adds appropriate jittering
- [ ] Factor levels maintain correct order

### Clinical Features
- [ ] Clinical cutoffs at expected values
- [ ] Reference ranges encompass appropriate data
- [ ] MCID values align with literature standards
- [ ] Treatment effects show expected directions
- [ ] Response rates match simulated parameters

### Publication Features
- [ ] Journal styles modify appearance appropriately  
- [ ] Methods text includes relevant analysis details
- [ ] Clinical reports provide comprehensive summaries
- [ ] Figure quality suitable for publication
- [ ] Statistical reporting follows guidelines

## Expected Data Characteristics

### Treatment Effects (Ground Truth)
- **Tumor Size Change:** Drug A should show ~25% reduction at Week 12 vs placebo progression
- **Biomarker Levels:** Drug A should reduce levels by ~40% from baseline
- **Quality of Life:** Drug A should improve scores by ~8 points vs placebo decline
- **Pain Scores:** Drug A should reduce pain by ~1.5 points vs placebo increase

### Missing Data Patterns
- **Overall dropout:** ~15% by Week 12
- **Differential dropout:** Higher in placebo group
- **Informative missingness:** Related to treatment response

### Statistical Power
- **Sample size:** N=150 per group provides >80% power for moderate effect sizes
- **Effect sizes:** Should detect Cohen's d > 0.5 for primary endpoints
- **P-values:** Treatment comparisons should be statistically significant

## Troubleshooting

### Common Issues
1. **No longitudinal connections:** Check that `id_var` is specified and `show_longitudinal = TRUE`
2. **Missing clinical indicators:** Verify that cutoff values are within data range
3. **Log transformation errors:** Ensure positive values only for biomarker data
4. **Empty effect size tables:** Check that grouping variables have >1 level

### Performance Notes
- Large datasets (>1000 rows) may render slowly with longitudinal connections
- Complex multi-feature plots may take longer to generate reports
- Missing data patterns can affect statistical power calculations

## Dataset Validation
To verify datasets loaded correctly:

```r
# Check dataset dimensions
sapply(list(advancedraincloud_data, advancedraincloud_baseline, 
           advancedraincloud_endpoint, advancedraincloud_change), 
       function(x) c(nrow(x), ncol(x)))

# Verify treatment balance
table(advancedraincloud_baseline$treatment_arm)

# Check missing data patterns  
colSums(is.na(advancedraincloud_data))

# Validate factor levels
levels(advancedraincloud_data$time_point)
levels(advancedraincloud_data$tumor_responder)
```

This comprehensive testing approach ensures all Advanced Raincloud Plot features work correctly across diverse clinical research scenarios.