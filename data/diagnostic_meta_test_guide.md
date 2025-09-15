# Diagnostic Meta-Analysis Test Data Guide

## Overview

The `diagnostic_meta_test.csv` dataset contains realistic diagnostic test accuracy data for testing the `diagnosticmeta` function. This dataset simulates 25 studies comparing AI algorithms for pathological diagnosis across different patient populations.

## Dataset Structure

### Core Variables (Required)
- **study_name**: Unique study identifier (e.g., "Chen_2020", "Smith_2019")
- **true_positives**: Number of correctly identified positive cases
- **false_positives**: Number of incorrectly identified positive cases
- **false_negatives**: Number of incorrectly identified negative cases
- **true_negatives**: Number of correctly identified negative cases

### ✅ **No Column Renaming Required**
The function automatically handles column name conversion internally. You can use the original CSV column names directly:
- Use **any column names** in your CSV file
- Assign them in the jamovi interface as shown above
- The backend automatically converts them to the format required by statistical packages

### Meta-Regression Variables (Optional)
- **patient_population**: Disease stage ("early_stage", "mixed", "advanced")
- **publication_year**: Study publication year (2018-2022)
- **ai_algorithm**: Algorithm type ("CNN", "ResNet", "Vision_Transformer", "Traditional_ML")
- **sample_size**: Total study sample size (calculated: TP+FP+FN+TN)

## Dataset Characteristics

### Study Distribution
- **25 studies** total
- **Publication years**: 2018-2022 (5-year span)
- **Sample sizes**: 200-1000 patients per study
- **Patient populations**:
  - Early stage: 8 studies (32%)
  - Mixed: 9 studies (36%)
  - Advanced: 8 studies (32%)

### Algorithm Distribution
- **CNN**: 7 studies (28%)
- **ResNet**: 6 studies (24%)
- **Vision_Transformer**: 6 studies (24%)
- **Traditional_ML**: 6 studies (24%)

### Expected Performance Metrics
- **Sensitivity range**: 65-85% (realistic for AI diagnostic tools)
- **Specificity range**: 70-90% (accounting for false positive rates)
- **Built-in heterogeneity**: Different algorithms and populations create natural variation
- **Publication bias simulation**: Slightly higher accuracy in recent studies

## Testing Scenarios

### 1. Basic Bivariate Meta-Analysis
```
Variables to assign:
- Study Identifier: study_name
- True Positives: true_positives
- False Positives: false_positives
- False Negatives: false_negatives
- True Negatives: true_negatives

Expected Results:
- Pooled sensitivity: ~75-80%
- Pooled specificity: ~80-85%
- Confidence intervals: Should show reasonable precision
- Diagnostic odds ratio: 15-25 (good diagnostic performance)
```

### 2. HSROC Analysis
```
Options to enable:
☑ HSROC Analysis

Expected Results:
- HSROC curve parameters
- Threshold and accuracy parameters
- Statistical significance tests
```

### 3. Heterogeneity Assessment
```
Options to enable:
☑ Heterogeneity Assessment

Expected Results:
- Q-statistic significance (expect some heterogeneity)
- I² values: 30-60% (moderate heterogeneity expected)
- τ² values indicating between-study variance
```

### 4. Meta-Regression Testing

#### A. Patient Population Analysis
```
Variables to assign:
- Covariate: patient_population

Expected Results:
- Advanced stage: Higher sensitivity (more detectable disease)
- Early stage: Lower sensitivity (subtle findings)
- Mixed populations: Intermediate values
```

#### B. Algorithm Type Analysis
```
Variables to assign:
- Covariate: ai_algorithm

Expected Results:
- Vision_Transformer: Highest performance (newest technology)
- CNN/ResNet: Good performance
- Traditional_ML: Lower performance
```

#### C. Temporal Trend Analysis
```
Variables to assign:
- Covariate: publication_year

Expected Results:
- Improving accuracy over time
- Significant positive slope for sensitivity
```

### 5. Publication Bias Assessment
```
Options to enable:
☑ Publication Bias Assessment

Expected Results:
- Deeks' funnel plot asymmetry test
- Potential bias detection (mild bias built into data)
- P-value around 0.05-0.10 (borderline significance)
```

### 6. Individual Study Results
```
Options to enable:
☑ Show Individual Study Results

Expected Results:
- 25 rows of study-specific data
- Sensitivity range: 0.65-0.85
- Specificity range: 0.70-0.90
- Sample sizes: 200-1000
```

## Plot Testing

### Forest Plot
- **Studies ordered by sensitivity**
- **Confidence intervals for each study**
- **Clear study labels**
- **Reasonable CI widths** (wider for smaller studies)

### Summary ROC Plot
- **25 study points in ROC space**
- **Points clustered in upper-left quadrant** (good performance)
- **Diagonal reference line**
- **Some scatter indicating heterogeneity**

### Funnel Plot
- **Log diagnostic odds ratio vs precision**
- **Mild asymmetry** (publication bias simulation)
- **Smaller studies with wider scatter**

## Data Quality Features

### Realistic Constraints
- ✅ No zero cells (avoids computational issues)
- ✅ Sensible sensitivity/specificity ranges
- ✅ Appropriate sample sizes for diagnostic studies
- ✅ Balanced distribution across covariates
- ✅ Temporal trends reflecting technological advancement

### Built-in Test Cases
- **Algorithm comparison**: 4 different AI approaches
- **Population heterogeneity**: 3 disease stages
- **Temporal trends**: 5-year publication span
- **Sample size variation**: 5x range (200-1000)
- **Performance variation**: Realistic diagnostic accuracy ranges

## Usage Example

1. Load the dataset in jamovi
2. Run diagnosticmeta with basic settings
3. Enable all analysis options to test comprehensive functionality
4. Try different meta-regression variables to test covariate effects
5. Verify all plots generate correctly
6. Check that confidence level changes affect results appropriately

## Expected Clinical Interpretation

This dataset represents a **meta-analysis of AI diagnostic tools in digital pathology** with:
- **Good overall diagnostic accuracy** (pooled metrics in clinically useful range)
- **Moderate heterogeneity** (expected in multi-institutional studies)
- **Algorithm-dependent performance** (newer methods outperform older ones)
- **Population-specific effects** (disease stage impacts detectability)
- **Potential publication bias** (slight tendency toward positive results)

These characteristics make it an excellent test case for validating all `diagnosticmeta` function features while providing realistic clinical context.