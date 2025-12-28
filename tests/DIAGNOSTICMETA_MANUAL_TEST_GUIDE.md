# Diagnostic Meta-Analysis Manual Testing Guide

## Overview

This guide provides step-by-step instructions for manually testing the `diagnosticmeta` function in jamovi and R. The function performs comprehensive meta-analysis of diagnostic test accuracy studies, particularly useful for pathology research, AI algorithm validation, and biomarker diagnostic accuracy synthesis.

## Quick Start

### In jamovi

1. **Open jamovi** and load test data:
   - Menu: `Data` → `Open` → Browse to `data/diagnostic_meta_test.csv`
   - Or use the simpler example: `data/diagnostic_meta_example.csv`

2. **Navigate to the analysis**:
   - `OncoPathT` → `IHC Analysis` → `Diagnostic Test Meta-Analysis for Pathology`

3. **Assign variables**:
   - Study Identifier: `study_name`
   - True Positives: `true_positives`
   - False Positives: `false_positives`
   - False Negatives: `false_negatives`
   - True Negatives: `true_negatives`

4. **Enable analyses** using checkboxes (see scenarios below)

### In R

```r
library(ClinicoPath)

# Load test data
data_path <- system.file("data", "diagnostic_meta_test.csv", package = "ClinicoPath")
test_data <- read.csv(data_path)

# Run basic analysis
results <- diagnosticmeta(
  data = test_data,
  study = study_name,
  true_positives = true_positives,
  false_positives = false_positives,
  false_negatives = false_negatives,
  true_negatives = true_negatives,
  bivariate_analysis = TRUE
)

# View results
results
```

---

## Test Datasets

### Dataset 1: diagnostic_meta_test.csv (Comprehensive)
- **Purpose**: AI algorithm validation meta-analysis
- **Studies**: 25 studies (2018-2022)
- **Sample sizes**: 200-1000 patients per study
- **Variables**:
  - Core: study_name, true_positives, false_positives, false_negatives, true_negatives
  - Covariates: patient_population, publication_year, ai_algorithm, sample_size
- **Use for**: All testing scenarios, especially meta-regression

### Dataset 2: diagnostic_meta_example.csv (Simple)
- **Purpose**: IHC antibody diagnostic accuracy
- **Studies**: 10 studies (2019-2021)
- **Sample sizes**: 233-1992 patients per study
- **Variables**:
  - Core: study_name, true_positives, false_positives, false_negatives, true_negatives
  - Covariates: ihc_antibody, staining_method, patient_population
- **Use for**: Basic functionality testing, tutorials

---

## Testing Scenarios

### Scenario 1: Basic Bivariate Meta-Analysis ✅

**Purpose**: Test core pooled estimation functionality

**Steps**:
1. Load `diagnostic_meta_test.csv`
2. Assign core variables (study, TP, FP, FN, TN)
3. Enable: ☑ Bivariate Random-Effects Model
4. Enable: ☑ Show Individual Study Results

**Expected Results**:
- **Pooled Sensitivity**: ~75-80% with 95% CI
- **Pooled Specificity**: ~80-85% with 95% CI
- **Positive Likelihood Ratio**: 5-10 (good diagnostic utility)
- **Negative Likelihood Ratio**: 0.2-0.3 (good rule-out capability)
- **Diagnostic Odds Ratio**: 15-25 (strong diagnostic performance)
- **Individual Studies Table**: 25 rows with study-specific metrics

**What to Verify**:
- ✅ All estimates are within valid probability ranges (0-100% for sens/spec)
- ✅ Confidence intervals are reasonable (not excessively wide or narrow)
- ✅ Diagnostic odds ratio is significantly > 1
- ✅ No error messages or warnings about convergence

---

### Scenario 2: HSROC Analysis ✅

**Purpose**: Test hierarchical summary ROC modeling

**Steps**:
1. Load `diagnostic_meta_test.csv`
2. Assign core variables
3. Enable: ☑ HSROC Analysis

**Expected Results**:
- **HSROC Parameters Table** with:
  - Accuracy (Λ): positive value
  - Threshold (θ): value indicating average threshold
  - Shape (β): indicates symmetry of SROC curve
- **Statistical significance** for parameters
- **Standard errors** for all estimates

**What to Verify**:
- ✅ HSROC table is populated with 3+ rows
- ✅ Parameter estimates have confidence intervals
- ✅ Accuracy parameter (Λ) is positive (better than chance)
- ✅ No computational errors

---

### Scenario 3: Heterogeneity Assessment ✅

**Purpose**: Test between-study variance quantification

**Steps**:
1. Load `diagnostic_meta_test.csv`
2. Assign core variables
3. Enable: ☑ Heterogeneity Analysis

**Expected Results**:
- **Q-statistic**: Significant (p < 0.05) indicating heterogeneity exists
- **I² for Sensitivity**: 30-60% (moderate heterogeneity)
- **I² for Specificity**: 20-50% (low to moderate)
- **τ² values**: Positive, indicating between-study variance

**What to Verify**:
- ✅ Heterogeneity table shows separate metrics for sensitivity and specificity
- ✅ I² values are 0-100% (valid percentage range)
- ✅ Q-statistic has degrees of freedom and p-value
- ✅ τ² (tau-squared) is non-negative

---

### Scenario 4: Meta-Regression (Categorical Covariate) ✅

**Purpose**: Test subgroup analysis by algorithm type

**Steps**:
1. Load `diagnostic_meta_test.csv`
2. Assign core variables
3. Assign covariate: `ai_algorithm`
4. Enable: ☑ Meta-Regression
5. Enable: ☑ Bivariate Random-Effects Model (for comparison)

**Expected Results**:
- **Meta-Regression Table** showing:
  - Effect of ai_algorithm on sensitivity
  - Effect of ai_algorithm on specificity
  - Statistical significance (p-values)
- **Interpretation**:
  - Vision_Transformer: Higher performance (newest technology)
  - CNN/ResNet: Moderate performance
  - Traditional_ML: Lower performance

**What to Verify**:
- ✅ Meta-regression table is populated
- ✅ Coefficients and standard errors are shown
- ✅ P-values indicate which algorithms differ significantly
- ✅ Results make clinical sense (newer algorithms ≥ older ones)

---

### Scenario 5: Meta-Regression (Continuous Covariate) ✅

**Purpose**: Test temporal trends in diagnostic accuracy

**Steps**:
1. Load `diagnostic_meta_test.csv`
2. Assign core variables
3. Assign covariate: `publication_year`
4. Enable: ☑ Meta-Regression

**Expected Results**:
- **Positive slope**: Accuracy improving over time (2018→2022)
- **Statistical significance**: p < 0.10 (borderline to significant)
- **Coefficient interpretation**: Each year increase associated with higher sensitivity/specificity

**What to Verify**:
- ✅ Continuous covariate handled correctly
- ✅ Slope coefficient has direction (positive = improving accuracy)
- ✅ Standard error is reasonable
- ✅ P-value indicates strength of temporal trend

---

### Scenario 6: Publication Bias Assessment ✅

**Purpose**: Test Deeks' funnel plot asymmetry test

**Steps**:
1. Load `diagnostic_meta_test.csv`
2. Assign core variables
3. Enable: ☑ Publication Bias Assessment
4. Enable: ☑ Funnel Plot

**Expected Results**:
- **Deeks' Test Results**:
  - Test statistic (bias coefficient)
  - P-value: ~0.05-0.10 (borderline significance, mild bias built into data)
- **Funnel Plot**: Shows log DOR vs effective sample size (ESS)

**What to Verify**:
- ✅ Publication bias table shows Deeks' test results
- ✅ Funnel plot displays correctly
- ✅ ESS is calculated correctly (harmonic mean, not arithmetic)
- ✅ Plot shows some asymmetry (expected with publication bias)

---

### Scenario 7: Forest Plot ✅

**Purpose**: Test visualization of study-specific estimates

**Steps**:
1. Load `diagnostic_meta_test.csv`
2. Assign core variables
3. Enable: ☑ Bivariate Random-Effects Model
4. Enable: ☑ Forest Plot

**Expected Results**:
- **Two forest plots**: One for sensitivity, one for specificity
- **Study-specific estimates**: 25 rows (one per study)
- **Confidence intervals**: Wider for smaller studies
- **Pooled estimate**: Diamond at bottom showing meta-analytic result

**What to Verify**:
- ✅ All 25 studies are displayed
- ✅ Studies are ordered (by sensitivity/specificity or study name)
- ✅ Confidence intervals vary by sample size
- ✅ Pooled estimate is clearly marked
- ✅ No visual rendering errors

---

### Scenario 8: Summary ROC Plot ✅

**Purpose**: Test ROC space visualization

**Steps**:
1. Load `diagnostic_meta_test.csv`
2. Assign core variables
3. Enable: ☑ Bivariate Random-Effects Model
4. Enable: ☑ Summary ROC Plot

**Expected Results**:
- **SROC Curve**: Smooth curve through ROC space
- **Study Points**: 25 points in upper-left quadrant (good performance)
- **Pooled Point**: Large point showing pooled sensitivity/specificity
- **Diagonal Reference**: Line showing chance performance (50/50)
- **Confidence Region**: Optional region around pooled point

**What to Verify**:
- ✅ All 25 studies visible as points
- ✅ Points cluster in upper-left (high sens & spec)
- ✅ SROC curve is smooth (no jagged edges)
- ✅ Pooled point is clearly marked
- ✅ Axes are properly labeled (0-100% scale)

---

### Scenario 9: Color Palette Accessibility ✅

**Purpose**: Test accessible color options

**Steps**:
1. Load `diagnostic_meta_test.csv`
2. Assign core variables
3. Enable plots: ☑ Forest Plot, ☑ Summary ROC Plot, ☑ Funnel Plot
4. Try each color palette:
   - Standard Colors
   - Color-Blind Safe (recommended)
   - High Contrast
   - Viridis (Blue-Purple)
   - Plasma (Purple-Pink)

**Expected Results**:
- **All plots update** with new color scheme
- **Readability maintained** across all palettes
- **Color-blind safe**: Maximum distinguishability

**What to Verify**:
- ✅ Color changes are applied to all plots
- ✅ Colors are distinguishable from each other
- ✅ Text remains readable
- ✅ No rendering errors when switching palettes

---

### Scenario 10: Confidence Level Variation ✅

**Purpose**: Test different confidence levels

**Steps**:
1. Load `diagnostic_meta_test.csv`
2. Assign core variables
3. Enable: ☑ Bivariate Random-Effects Model
4. Try different confidence levels:
   - 90% CI (narrower)
   - 95% CI (default)
   - 99% CI (wider)

**Expected Results**:
- **CI Width Changes**: 99% > 95% > 90%
- **Point Estimates**: Remain the same (only CI width changes)
- **Tables Update**: Show new CI bounds
- **Plots Update**: Forest plot confidence intervals adjust

**What to Verify**:
- ✅ CI bounds change appropriately
- ✅ Point estimates remain constant
- ✅ Higher confidence level → wider intervals
- ✅ All outputs update consistently

---

### Scenario 11: Zero Cell Handling ✅

**Purpose**: Test continuity correction for zero cells

**Steps**:
1. Create or use test data with zero cells (e.g., TP=0 in some studies)
2. Assign core variables
3. Enable: ☑ Bivariate Random-Effects Model
4. Try different zero-cell correction methods:
   - None (Model-Based) - default
   - Constant (+0.5 to all cells)
   - Treatment-Arm (add to zero cells only)
   - Empirical (study-specific)

**Expected Results**:
- **Model-Based**: Handles zeros through bivariate model
- **Constant**: Adds 0.5 to all cells of affected studies
- **Treatment-Arm**: Adds correction only to zero cells
- **Notice**: Warning if zero cells are detected

**What to Verify**:
- ✅ Analysis completes without errors
- ✅ Zero cell correction method is applied
- ✅ Notice/warning appears if zero cells exist
- ✅ Estimates are reasonable (not pushed to extremes)

---

### Scenario 12: Minimum Study Requirements ✅

**Purpose**: Test behavior with insufficient data

**Steps**:
1. Create test data with only 2-3 studies
2. Assign core variables
3. Enable: ☑ Bivariate Random-Effects Model

**Expected Results**:
- **Notice/Warning**: "At least 3 studies recommended for meta-analysis"
- **Results**: May be unstable or not computed
- **Graceful Handling**: No crashes, clear error messages

**What to Verify**:
- ✅ Appropriate warning message appears
- ✅ Function doesn't crash
- ✅ User is informed about data limitations

---

### Scenario 13: Clinical Interpretation ✅

**Purpose**: Test interpretation guidance features

**Steps**:
1. Load `diagnostic_meta_test.csv`
2. Assign core variables
3. Enable: ☑ Bivariate Random-Effects Model
4. Enable: ☑ Show Clinical Interpretation
5. Enable: ☑ Show Analysis Summary

**Expected Results**:
- **Clinical Interpretation Panel**:
  - Guidelines for interpreting likelihood ratios
  - Diagnostic accuracy thresholds
  - Clinical utility recommendations
- **Analysis Summary**:
  - Natural language summary of results
  - Key findings highlighted
  - Clinical implications

**What to Verify**:
- ✅ Interpretation text is clear and clinically relevant
- ✅ Summary reflects actual results
- ✅ Recommendations are appropriate for the data

---

### Scenario 14: Methodology Documentation ✅

**Purpose**: Test transparency and documentation features

**Steps**:
1. Load any test data
2. Enable: ☑ Show Methodology Information

**Expected Results**:
- **Methodology Panel** with:
  - Statistical methods used
  - Software references (mada, metafor packages)
  - Citations for methods
  - Interpretation guidelines

**What to Verify**:
- ✅ Methodology panel is visible
- ✅ Information is accurate and complete
- ✅ References are provided

---

### Scenario 15: Plot Explanations ✅

**Purpose**: Test educational plot annotations

**Steps**:
1. Load `diagnostic_meta_test.csv`
2. Assign core variables
3. Enable all plots
4. Enable: ☑ Show Plot Explanations

**Expected Results**:
- **Enhanced Plot Annotations**:
  - Axis labels explained
  - Key features highlighted
  - Interpretation guidance
- **Below Each Plot**: Text explaining what to look for

**What to Verify**:
- ✅ Explanations appear for each plot
- ✅ Text is educational and clear
- ✅ Guidance helps with interpretation

---

## Edge Cases and Error Handling

### Edge Case 1: Perfect Sensitivity/Specificity
**Data**: All studies have sensitivity = 100% and specificity = 100%
**Expected**: Analysis completes, but notice about lack of heterogeneity

### Edge Case 2: Extremely Imbalanced Studies
**Data**: Some studies have TP=1000, TN=10 (highly imbalanced)
**Expected**: Effective sample size (ESS) is correctly calculated (harmonic mean)

### Edge Case 3: Missing Covariate Data
**Data**: Covariate has NA values for some studies
**Expected**: Notice about missing data, studies with missing covariate excluded from meta-regression

### Edge Case 4: Very Large Sample Sizes
**Data**: Studies with N > 10,000
**Expected**: Analysis completes, very narrow confidence intervals

### Edge Case 5: All Studies Same Result
**Data**: All studies identical (no heterogeneity)
**Expected**: τ² = 0, I² = 0%, heterogeneity tests not significant

---

## Validation Checklist

Use this checklist when testing the function:

### Data Input
- [ ] All required variables assigned correctly
- [ ] Variable names handle special characters
- [ ] Study identifiers are unique
- [ ] No negative cell counts
- [ ] Data types are appropriate (numeric for counts)

### Numerical Accuracy
- [ ] Pooled estimates match manual calculations (when possible)
- [ ] Confidence intervals are symmetric (on logit scale)
- [ ] Likelihood ratios are mathematically correct (LR+ = Sens / (1-Spec))
- [ ] Diagnostic odds ratio = LR+ / LR-
- [ ] Effective sample size (ESS) uses harmonic mean

### Statistical Validity
- [ ] Heterogeneity metrics (I², τ²) are non-negative
- [ ] Q-statistic has correct degrees of freedom (k-1 studies)
- [ ] Meta-regression coefficients have standard errors
- [ ] P-values are in valid range [0, 1]
- [ ] Bivariate model converges without warnings

### Plots
- [ ] All studies appear in plots
- [ ] Axes are correctly scaled and labeled
- [ ] Pooled estimates are clearly marked
- [ ] Confidence intervals are visible
- [ ] Color palettes render correctly
- [ ] No overlapping text or visual glitches

### User Interface
- [ ] Notices appear when appropriate (warnings, errors, info)
- [ ] Options enable/disable results correctly
- [ ] Help text is informative
- [ ] Output updates dynamically when options change

### Clinical Relevance
- [ ] Results are interpretable by clinicians
- [ ] Interpretation guidance is accurate
- [ ] Methodology is transparent
- [ ] Recommendations are evidence-based

---

## Troubleshooting

### Issue: "Analysis requires at least 3 studies"
**Solution**: Ensure your dataset has ≥ 3 unique studies with complete data

### Issue: "Model did not converge"
**Solution**:
1. Check for extreme values (e.g., all zeros in a row)
2. Try different estimation method (REML → ML)
3. Enable zero-cell correction if zeros present

### Issue: "Missing required variable"
**Solution**: Verify all 5 core variables are assigned (study, TP, FP, FN, TN)

### Issue: Plots not displaying
**Solution**:
1. Ensure plot options are enabled (checkboxes)
2. Verify data has sufficient variation for plotting
3. Check that required packages (mada, metafor) are installed

### Issue: Meta-regression results empty
**Solution**:
1. Verify covariate variable is assigned
2. Enable "Meta-Regression" checkbox
3. Ensure covariate has variation (not all same value)

---

## Performance Testing

### Small Dataset (5-10 studies)
**Expected Runtime**: < 5 seconds

### Medium Dataset (10-30 studies)
**Expected Runtime**: 5-15 seconds

### Large Dataset (30+ studies)
**Expected Runtime**: 15-30 seconds

**Note**: HSROC analysis may take longer than bivariate analysis

---

## Reporting Bugs

When reporting issues, please include:

1. **Dataset**: Which test data file you used (or provide your data structure)
2. **Options**: Which analysis options were enabled
3. **Error Message**: Full text of any errors or warnings
4. **Expected vs Actual**: What you expected to see vs what appeared
5. **Environment**: jamovi version and operating system
6. **Screenshots**: Visual issues benefit from screenshots

---

## Additional Resources

- **Function Documentation**: `?diagnosticmeta` in R console
- **mada Package**: `?mada::reitsma` for bivariate models
- **metafor Package**: `?metafor::rma` for meta-analysis methods
- **Test Data Guide**: `data/diagnostic_meta_test_guide.md`

---

## Summary

This guide covers 15 comprehensive testing scenarios plus edge cases. For quick validation:

1. **Basic Functionality**: Scenarios 1-2 (Bivariate + HSROC)
2. **Advanced Features**: Scenarios 4-6 (Meta-regression + Publication Bias)
3. **Visualization**: Scenarios 7-8 (Forest + SROC plots)
4. **Accessibility**: Scenario 9 (Color palettes)
5. **Edge Cases**: Zero cells, minimum studies, perfect performance

Happy testing! 🔬
