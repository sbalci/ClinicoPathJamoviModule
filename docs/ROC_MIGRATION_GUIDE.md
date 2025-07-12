# ROC Analysis Migration Guide

## Overview

The legacy `roc` and `roc2` modules have been replaced by the comprehensive `psychopdaroc` module. This guide helps users migrate their analyses to the enhanced ROC analysis tool.

### Modules Being Replaced
- **`roc`** - Basic ROC analysis with pROC/ROCR packages
- **`roc2`** - Development module with plotROC package integration

## Why Migrate?

The `psychopdaroc` module provides all functionality of the legacy `roc` module plus:

- **Multiple variable comparison** - Analyze several biomarkers simultaneously
- **Advanced statistical methods** - DeLong test, IDI, NRI calculations
- **Enhanced optimization** - 12 cutpoint methods, 16 optimization metrics
- **Subgroup analysis** - Stratified analysis by grouping variables
- **Bootstrap confidence intervals** - More robust statistical inference
- **Better visualizations** - Enhanced plots with more customization options

## Feature Mapping

### Basic ROC Analysis
**Legacy `roc`:**
```
Variables:
- Classification Variable: disease_status
- Test Variable: biomarker1

Options:
- Calculate Youden Index: ✓
- Calculate Optimal Criterion Value: ✓
```

**New `psychopdaroc`:**
```
Variables:
- Class Variable: disease_status
- Test Variables: biomarker1

Cutpoint Method: maximize_metric
Optimization Metric: youden
```

### Plot Equivalents

| Legacy `roc` Plot | New `psychopdaroc` Option | Description |
|-------------------|---------------------------|-------------|
| ROC Curve Plot | `plotROC: true` | Enhanced with confidence bands |
| Sensitivity/Specificity vs. Criterion | `showCriterionPlot: true` | Same functionality, better styling |
| Predictive Values vs. Prevalence | `showPrevalencePlot: true` | Same functionality, enhanced |
| Interactive Dot Diagram | `showDotPlot: true` | Same functionality, improved |

### Optimal Criteria Mapping

| Legacy `roc` Criterion | New `psychopdaroc` Method | Notes |
|------------------------|---------------------------|-------|
| Youden Index | `method: maximize_metric`, `metric: youden` | Enhanced with smoothing options |
| Cost-Ratio Optimized | `method: oc_cost_ratio` | More sophisticated cost-benefit analysis |
| Closest to (0,1) | `method: oc_closest_01` | Same algorithm, better implementation |
| Equal Sens & Spec | `method: oc_equal_sens_spec` | Same functionality |

### Confidence Interval Methods

| Legacy `roc` CI Method | New `psychopdaroc` Equivalent |
|------------------------|-------------------------------|
| DeLong et al. (1988) | Default method (enhanced) |
| Hanley & McNeil (1982) | Available in advanced options |
| Binomial exact | Available in advanced options |
| **New:** Bootstrap CI | `bootstrapCI: true` |

## ROC2 Module Migration

### Interactive ROC Features
**ROC2 had:**
- Interactive ROC plots using plotROC package
- Basic styling and labeling options

**New psychopdaroc equivalent:**
- Interactive ROC: ✓ (enable `interactiveROC` option)
- Enhanced styling and theming
- All statistical analysis included

### ROC2 Feature Mapping

| ROC2 Option | Psychopdaroc Equivalent | Notes |
|-------------|------------------------|-------|
| Interactive plots | `interactiveROC: true` | Enhanced implementation |
| Style ROC | Built-in theming | Better styling options |
| Quantile CI | `bootstrapCI: true` | More robust CI methods |
| Direct labeling | Automatic labeling | Enhanced label positioning |

## Migration Examples

### Example 1: Basic ROC Analysis
**Legacy settings:**
- Classification Variable: `disease_status`
- Test Variable: `biomarker1`
- Calculate Youden Index: ✓
- ROC Curve Plot: ✓

**New equivalent:**
- Class Variable: `disease_status`
- Test Variables: `biomarker1`
- Cutpoint Method: `maximize_metric`
- Optimization Metric: `youden`
- Plot ROC: ✓

### Example 2: Cost-Ratio Optimization
**Legacy settings:**
- Cost Ratio (FP/FN): `2.5`
- Calculate Optimal Criterion Value: ✓

**New equivalent:**
- Cutpoint Method: `oc_cost_ratio`
- Cost Ratio FP: `2.5`

### Example 3: Multiple Plots
**Legacy settings:**
- ROC Curve Plot: ✓
- Plot Sensitivity/Specificity vs. Criterion: ✓
- Plot Predictive Values vs. Prevalence: ✓
- Interactive Dot Diagram: ✓

**New equivalent:**
- Plot ROC: ✓
- Show Criterion Plot: ✓
- Show Prevalence Plot: ✓
- Show Dot Plot: ✓

## Advanced Features (New Capabilities)

### Multi-Variable Comparison
```
Test Variables: biomarker1, biomarker2, biomarker3
DeLong Test: ✓
Combine Plots: ✓
```

### Subgroup Analysis
```
Test Variables: biomarker1
Subgroup Variable: hospital
```

### Bootstrap Confidence Intervals
```
Bootstrap CI: ✓
Bootstrap Runs: 2000
```

### IDI/NRI Analysis
```
Test Variables: biomarker1, biomarker2
Calculate IDI: ✓
Calculate NRI: ✓
Reference Variable: biomarker1
```

## Troubleshooting

### Common Issues

**Q: My ROC curve looks different**
A: The new module uses enhanced algorithms. Results should be equivalent or more accurate.

**Q: I can't find the exact same plot style**
A: Plot styling has been modernized. Functionality is preserved with better visual design.

**Q: The optimal cutpoint values are slightly different**
A: The new module uses more precise optimization algorithms. Small differences are expected and indicate improved accuracy.

### Getting Help

- Check the comprehensive help documentation in `psychopdaroc`
- Use the built-in examples for guidance
- The new module provides more detailed output and explanations

## Benefits of Migration

1. **Future-proof analysis** - Active development and maintenance
2. **Enhanced statistical rigor** - More robust methods and validation
3. **Comprehensive features** - Everything you need in one tool
4. **Better documentation** - Extensive help and examples
5. **Improved performance** - Optimized algorithms and error handling

## Conclusion

The `psychopdaroc` module is a comprehensive upgrade that maintains all functionality of the legacy `roc` module while adding extensive new capabilities. Migration provides immediate benefits and ensures compatibility with future updates.