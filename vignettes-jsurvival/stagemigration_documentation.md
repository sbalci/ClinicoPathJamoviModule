# Advanced TNM Stage Migration Analysis Documentation

## Overview

The `stagemigration` function provides state-of-the-art statistical analysis for validating TNM staging system improvements. This comprehensive analysis suite evaluates whether a new staging system provides superior prognostic discrimination compared to existing systems, incorporating advanced multivariable methods and clinical decision support.

## Key Features

### Core Analysis Components

1. **Basic Migration Analysis**
   - Migration matrix and overview tables
   - Stage distribution comparisons
   - Chi-square and Fisher's exact tests
   - Migration pattern visualization

2. **Advanced Statistical Methods**
   - Time-dependent ROC analysis
   - Net Reclassification Improvement (NRI)
   - Integrated Discrimination Improvement (IDI)
   - Decision Curve Analysis (DCA)
   - Bootstrap validation with optimism correction
   - Cross-validation (k-fold and internal-external)

3. **Multivariable Analysis (NEW)**
   - **Bootstrap Model Selection**: Variable selection stability assessment using 500 bootstrap samples
   - **Advanced Interaction Detection**: Statistical tests for stage-covariate interactions with clinical significance assessment
   - **Comprehensive Model Diagnostics**: Model validation, performance metrics, and residual analysis
   - **Adjusted NRI**: Net reclassification improvement adjusted for covariates
   - **Multivariable Decision Curves**: Clinical utility comparison across multiple models
   - **Personalized Risk Predictions**: Individual patient risk assessments and clinical recommendations

## Recent Major Updates (v0.0.3.64+)

### Enhanced Multivariable Analysis

The multivariable analysis has been significantly expanded with state-of-the-art methods:

#### 1. Bootstrap Model Selection Stability
- **Method**: 500 bootstrap samples with stepwise selection
- **Output**: Selection frequencies, stability metrics, AIC impact assessment
- **Clinical Value**: Identifies most stable prognostic factors
- **Threshold**: Variables selected in >80% of samples considered highly stable

#### 2. Advanced Interaction Detection
- **Method**: Formal likelihood ratio tests for stage-covariate interactions
- **Statistics**: Chi-square, degrees of freedom, p-values
- **Clinical Assessment**: Automatic interpretation of clinical significance
- **Subgroup Analysis**: Identifies patient populations with differential staging benefit

#### 3. Comprehensive Model Diagnostics
- **Validation Metrics**: C-index, calibration, discrimination
- **Residual Analysis**: Martingale and deviance residuals
- **Assumption Testing**: Proportional hazards assumption
- **Performance Assessment**: Cross-validated metrics

#### 4. Adjusted NRI Analysis
- **Innovation**: NRI calculations adjusted for baseline covariates
- **Models Compared**: 
  - Baseline (covariates only)
  - Old staging + covariates
  - New staging + covariates
- **Time Points**: Customizable (default: 12, 24, 60 months)
- **Output**: Event-specific and overall NRI with confidence intervals

#### 5. Multivariable Decision Curve Analysis
- **Models**: Compares clinical utility of multiple model combinations
- **Thresholds**: Net benefit across 0.01-0.99 probability thresholds
- **Comparisons**: 
  - Treat all vs. treat none strategies
  - Model superiority ranges
  - Optimal threshold identification

#### 6. Personalized Risk Predictions
- **Individual Assessment**: Patient-level risk predictions at multiple time points
- **Risk Categories**: Low/Moderate/High/Very High classifications
- **Clinical Recommendations**: Automated treatment intensity guidance
- **Risk Profiles**: Representative patient archetypes (young/low risk, older/high risk, average)
- **Summary Statistics**: Population-level reclassification insights

### Analysis Types

The function supports four analysis scopes:

1. **Basic**: Migration patterns and basic statistics
2. **Standard**: Includes C-index and NRI analysis
3. **Comprehensive**: All available methods (recommended for research)
4. **Publication**: Optimized output for manuscript preparation

### Multifactorial Comparison Types

When multifactorial analysis is enabled:

1. **Adjusted C-index**: Compare discrimination after covariate adjustment
2. **Nested Models**: Likelihood ratio tests comparing staging systems
3. **Stepwise**: Automated variable selection showing staging system importance
4. **Comprehensive**: All multifactorial methods (recommended)

## Clinical Significance Thresholds

### Statistical Significance
- **p-values**: < 0.05 unless otherwise specified
- **Confidence Intervals**: 95% (customizable to 80-99%)

### Clinical Significance
- **C-index improvement**: ≥ 0.02 (2% improvement considered clinically meaningful)
- **NRI improvement**: ≥ 20% (substantial reclassification improvement)
- **Bootstrap selection frequency**: > 80% (high stability variables)
- **Individual risk difference**: > 10% (significant patient-level impact)

### Evidence Strength
- **Strong Evidence**: ≥3/4 positive criteria met across different analyses
- **Model Preference**: Lower AIC/BIC indicates better fit
- **Clinical Utility**: Positive net benefit over treat-all/treat-none strategies

## Configuration Decision Guide

For detailed guidance on selecting the appropriate analysis configuration based on your research context, see the comprehensive **[Stage Migration Analysis Configuration Guide](stagemigration_analysis_guide.md)**.

This guide covers:
- **Analysis scope selection** (Basic/Standard/Comprehensive/Publication)
- **Cancer type optimization** (Lung/Breast/Colorectal/Prostate/etc.)
- **Multifactorial comparison strategies** (Adjusted C-index/Nested models/Stepwise/Comprehensive)
- **Baseline model selection** (Covariates only/Original+covariates/New+covariates)
- **Resource-based configuration** (Limited/Moderate/High resources)
- **Publication target optimization** (High-impact/Specialty/General journals)

## Usage Examples

### Basic Usage
```r
# Minimal setup
jsurvival::stagemigration(
  data = cancer_data,
  oldStage = "tnm7_stage", 
  newStage = "tnm8_stage",
  survivalTime = "os_months",
  event = "death_status",
  eventLevel = "Dead"
)
```

### Comprehensive Analysis
```r
# Full analysis with multifactorial assessment
jsurvival::stagemigration(
  data = cancer_data,
  oldStage = "tnm7_stage", 
  newStage = "tnm8_stage",
  survivalTime = "os_months",
  event = "death_status",
  eventLevel = "Dead",
  analysisType = "comprehensive",
  enableMultifactorialAnalysis = TRUE,
  continuousCovariates = vars(Age, TumorSize),
  categoricalCovariates = vars(Gender, Histology, LymphNodeStatus),
  multifactorialComparisonType = "comprehensive",
  performInteractionTests = TRUE,
  calculateNRI = TRUE,
  performDCA = TRUE,
  performBootstrap = TRUE,
  performCrossValidation = TRUE
)
```

## Output Interpretation

### Tables Generated

1. **Migration Overview**: Basic migration statistics
2. **Migration Summary**: Statistical test results
3. **Stage Distribution**: Before/after comparison
4. **Migration Matrix**: Detailed cross-tabulation
5. **Statistical Comparison**: C-index and other metrics
6. **Multifactorial Model Results**: Adjusted comparisons
7. **Bootstrap Model Selection**: Variable stability assessment
8. **Stage-Covariate Interactions**: Interaction test results
9. **Nested Model Tests**: Likelihood ratio comparisons
10. **Stepwise Selection Results**: Automated variable selection

### Visualizations Available

1. **Migration Heatmap**: Color-coded migration patterns
2. **ROC Curve Comparison**: Time-dependent discrimination
3. **Calibration Plots**: Prediction accuracy assessment
4. **Decision Curves**: Clinical utility visualization
5. **Forest Plots**: Hazard ratios with confidence intervals

## Advanced Features

### Bootstrap Validation
- **Optimism Correction**: Adjusts for overfitting
- **Stable Estimates**: 1000 bootstrap replications (customizable)
- **Confidence Intervals**: Bootstrap-based CIs for all metrics

### Cross-Validation
- **K-fold CV**: Default 5-fold (customizable 3-10)
- **Internal-External CV**: For multi-institutional studies
- **Performance Metrics**: Cross-validated C-index, NRI, calibration

### Multi-Institutional Support
- **Institution Variable**: Enables center-specific analysis
- **External Validation**: Leave-one-center-out validation
- **Heterogeneity Assessment**: Center-to-center variability

### Will Rogers Phenomenon Analysis
- **Detection**: Identifies paradoxical survival improvements
- **Quantification**: Measures magnitude of effect
- **Visualization**: Before/after survival curve comparisons

## Best Practices

### Study Design
1. **Adequate Sample Size**: Minimum 100 events for stable estimates
2. **Balanced Covariates**: Include relevant prognostic factors
3. **Validation Strategy**: Use bootstrap + cross-validation
4. **Time Points**: Choose clinically relevant survival timepoints

### Interpretation Guidelines
1. **Multiple Criteria**: Don't rely on single metric
2. **Clinical Context**: Consider practical significance
3. **Validation Results**: Trust cross-validated performance
4. **Subgroup Analysis**: Examine interaction results

### Reporting Standards
1. **TRIPOD Guidelines**: Follow prediction model reporting standards
2. **Statistical Methods**: Document all analysis choices
3. **Validation**: Report internal and external validation
4. **Clinical Utility**: Include decision curve analysis

## Technical Requirements

### Data Format
- **Staging Variables**: Ordered factors recommended
- **Survival Time**: Numeric (months preferred)
- **Event Variable**: Binary or factor
- **Covariates**: Mixed types supported

### Computational Considerations
- **Memory**: Large datasets may require substantial RAM
- **Processing Time**: Comprehensive analysis can take 5-15 minutes
- **Parallel Processing**: Available for bootstrap operations

## Troubleshooting

### Common Issues

1. **Convergence Problems**
   - Reduce number of covariates
   - Check for collinearity
   - Increase sample size

2. **Missing Results**
   - Verify event rates in subgroups
   - Check for complete separation
   - Ensure adequate follow-up

3. **Performance Issues**
   - Reduce bootstrap repetitions
   - Disable cross-validation for initial exploration
   - Use basic analysis type for large datasets

### Error Messages
- **"Insufficient events"**: Increase sample size or reduce complexity
- **"Model convergence failed"**: Check covariate definitions
- **"Cross-validation failed"**: Reduce CV folds or check institution variable

## Version History

### v0.0.3.64 (Latest)
- **Major Enhancement**: Complete multivariable analysis overhaul
- **New Features**: Bootstrap model selection, personalized predictions, adjusted NRI
- **Performance**: Optimized algorithms for large datasets
- **Documentation**: Comprehensive explanatory outputs

### v0.0.3.63
- **Bug Fixes**: Interaction tests table population
- **Improvements**: Enhanced error handling
- **Validation**: Cross-validation performance optimization

### v0.0.3.62
- **New Features**: Advanced interaction detection
- **Enhancements**: Model diagnostics
- **Fixes**: Bootstrap validation improvements

## References

### Key Literature
1. Pencina MJ, et al. "Evaluating the added predictive ability of a new marker." Stat Med. 2008
2. Vickers AJ, et al. "Decision curve analysis: a novel method for evaluating prediction models." Med Decis Making. 2006
3. Harrell FE Jr. "Regression Modeling Strategies" 2nd Edition. Springer 2015
4. Steyerberg EW. "Clinical Prediction Models" 2nd Edition. Springer 2019
5. Collins GS, et al. "Transparent Reporting of a multivariable prediction model for Individual Prognosis or Diagnosis (TRIPOD)." Ann Intern Med. 2015

### Statistical Methods
- **C-index**: Harrell's concordance index for survival data
- **NRI/IDI**: Pencina et al. methodology with survival extensions
- **Bootstrap**: Efron & Tibshirani resampling methods
- **Decision Curves**: Vickers & Elkin net benefit framework
- **Cross-validation**: Stone-Geisser prediction error estimation

## Support and Citation

### Citation
When using this function, please cite:
```
ClinicoPath: Advanced TNM Stage Migration Analysis [R package]. 
Author: [Your Name]. Year: 2024.
```

### Support
- **Documentation**: This file and function help
- **Issues**: GitHub repository issues page
- **Updates**: Check NEWS.md for version changes

---

*Last updated: July 2024*
*Function version: 0.0.3.64+*