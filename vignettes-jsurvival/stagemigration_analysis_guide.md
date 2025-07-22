# Stage Migration Analysis Configuration Guide

## Overview

The `stagemigration` function provides flexible analysis configurations to match different research scenarios, study designs, and clinical contexts. This guide explains when and how to use each configuration option for optimal results.

## Analysis Scope Configuration

The `analysisType` parameter controls the breadth of analysis performed. Choose based on your research objectives and computational resources.

### **Basic Migration Analysis**
**When to use:**
- Initial exploratory analysis
- Limited computational resources
- Simple staging comparison without advanced metrics
- Quick validation of data structure and migration patterns

**What it includes:**
- Migration matrix and overview tables
- Basic chi-square and Fisher's exact tests
- Stage distribution comparisons
- Simple migration rate calculations

**Best for:**
- Pilot studies
- Data quality checks
- Initial manuscript drafts
- Educational purposes

```r
analysisType = "basic"
```

### **Standard Validation Analysis**
**When to use:**
- Traditional staging validation studies
- When C-index and NRI are primary endpoints
- Moderate computational resources available
- Standard manuscript requirements

**What it includes:**
- All basic analysis components
- C-index comparisons with confidence intervals
- Net Reclassification Improvement (NRI)
- Time-dependent discrimination metrics

**Best for:**
- Most clinical validation studies
- Regulatory submissions
- Standard journal publications
- Comparative effectiveness research

```r
analysisType = "standard"
```

### **Comprehensive Analysis** ⭐ **RECOMMENDED**
**When to use:**
- High-impact research publications
- Comprehensive staging system validation
- When all available evidence is needed
- Research-grade statistical rigor required

**What it includes:**
- All standard analysis components
- Bootstrap validation with optimism correction
- Decision curve analysis
- Advanced calibration assessment
- Will Rogers phenomenon detection
- Proportional hazards testing
- Multiple pseudo R-squared measures

**Best for:**
- High-impact journal submissions (Nature, NEJM, JCO)
- Guidelines development
- Meta-analysis inclusion
- Definitive staging validation studies

```r
analysisType = "comprehensive"
```

### **Publication Ready Analysis**
**When to use:**
- Manuscript preparation with space constraints
- When specific output formatting is needed
- Journal-specific requirements
- Final publication submission

**What it includes:**
- Optimized table layouts for manuscripts
- Publication-quality visualizations
- Condensed statistical reporting
- Journal-ready formatting

**Best for:**
- Final manuscript preparation
- Journal submission requirements
- Space-constrained publications
- Supplement material organization

```r
analysisType = "publication"
```

## Cancer Type Optimization

The `cancerType` parameter optimizes analysis parameters and interpretations for specific cancer types based on literature-derived thresholds.

### **General (Default)**
**When to use:**
- Mixed cancer populations
- Novel cancer types
- When cancer-specific literature is limited
- Cross-cancer validation studies

**Characteristics:**
- Generic clinical significance thresholds
- Standard interpretation guidelines
- Universal statistical approaches

```r
cancerType = "general"
```

### **Lung Cancer**
**When to use:**
- Non-small cell lung cancer (NSCLC)
- Small cell lung cancer (SCLC)
- Lung cancer staging validation

**Optimizations:**
- Shorter time points for aggressive disease (6, 12, 24 months)
- Lower NRI thresholds due to poor baseline prognosis
- Stage-specific survival expectations
- TNM 8th edition considerations

```r
cancerType = "lung"
```

### **Breast Cancer**
**When to use:**
- Invasive breast carcinoma
- Hormone receptor studies
- HER2 status integration

**Optimizations:**
- Extended follow-up periods (12, 24, 60, 120 months)
- Receptor status interaction considerations
- Age-adjusted interpretations
- Subtype-specific thresholds

```r
cancerType = "breast"
```

### **Colorectal Cancer**
**When to use:**
- Colon adenocarcinoma
- Rectal cancer studies
- GI malignancies

**Optimizations:**
- Intermediate time points (12, 36, 60 months)
- Location-specific considerations
- Microsatellite instability factors
- Adjuvant therapy impacts

```r
cancerType = "colorectal"
```

### **Prostate Cancer**
**When to use:**
- Prostate adenocarcinoma
- Gleason score integration
- PSA-based studies

**Optimizations:**
- Extended survival analysis (60, 120, 180 months)
- Biochemical recurrence considerations
- Age-specific thresholds
- Treatment modality interactions

```r
cancerType = "prostate"
```

### **Head and Neck Cancer**
**When to use:**
- Squamous cell carcinoma of head/neck
- HPV status integration
- Site-specific staging

**Optimizations:**
- Intermediate time points
- HPV interaction testing
- Site-specific considerations
- Treatment response factors

```r
cancerType = "headneck"
```

### **Melanoma**
**When to use:**
- Cutaneous melanoma
- Breslow thickness studies
- Sentinel lymph node analysis

**Optimizations:**
- Stage-specific time points
- Thickness-based thresholds
- Ulceration considerations
- Immunotherapy era adjustments

```r
cancerType = "melanoma"
```

### **Other Solid Tumor**
**When to use:**
- Rare cancers
- Sarcomas
- Carcinomas of unknown primary
- Novel histologies

**Optimizations:**
- Conservative thresholds
- Extended validation requirements
- Flexible interpretation guidelines

```r
cancerType = "other"
```

## Multifactorial Comparison Types

The `multifactorialComparisonType` parameter determines the sophistication of covariate-adjusted analysis when `enableMultifactorialAnalysis = TRUE`.

### **Adjusted C-index Comparison**
**When to use:**
- Simple covariate adjustment needed
- Limited computational resources
- When discrimination is the primary endpoint
- Straightforward multivariable analysis

**What it provides:**
- C-index values adjusted for covariates
- Confidence intervals for adjusted metrics
- Basic model comparison statistics
- Simple interpretation framework

**Best for:**
- Initial multivariable exploration
- Resource-limited environments
- Simple research questions
- Educational demonstrations

```r
multifactorialComparisonType = "adjusted_cindex"
enableMultifactorialAnalysis = TRUE
```

### **Nested Model Comparison**
**When to use:**
- Formal statistical testing required
- When model hierarchy is important
- Likelihood ratio tests needed
- Statistical significance emphasis

**What it provides:**
- Likelihood ratio tests for nested models
- Chi-square statistics and p-values
- Model hierarchy assessment
- Formal statistical comparisons

**Best for:**
- Regulatory submissions
- Statistical methodology papers
- When p-values are required
- Hypothesis testing frameworks

```r
multifactorialComparisonType = "nested_models"
enableMultifactorialAnalysis = TRUE
```

### **Stepwise Model Selection**
**When to use:**
- Variable selection is primary goal
- Large number of potential covariates
- When model parsimony is important
- Automated variable selection needed

**What it provides:**
- Automated stepwise selection results
- Variable importance rankings
- Selection stability assessment
- Parsimonious model identification

**Best for:**
- Exploratory research
- High-dimensional data
- Model development studies
- Variable screening applications

```r
multifactorialComparisonType = "stepwise"
enableMultifactorialAnalysis = TRUE
```

### **Comprehensive Comparison** ⭐ **RECOMMENDED**
**When to use:**
- Research-grade analysis required
- All available evidence needed
- High-impact publications
- Complete statistical assessment

**What it provides:**
- All above methods combined
- Bootstrap model selection stability
- Advanced interaction detection
- Comprehensive model diagnostics
- Adjusted NRI calculations
- Multivariable decision curve analysis
- Personalized risk predictions

**Best for:**
- High-impact research publications
- Comprehensive staging validation
- Clinical practice guidelines
- Definitive evidence generation

```r
multifactorialComparisonType = "comprehensive"
enableMultifactorialAnalysis = TRUE
```

## Baseline Model Configuration

The `baselineModel` parameter defines the reference model for multifactorial comparisons, affecting how staging system benefits are quantified.

### **Covariates Only**
**When to use:**
- Want to assess pure staging system contribution
- Interested in added value over clinical factors
- When staging systems should prove their worth
- Clinical factor baseline established

**Interpretation:**
- Shows what staging adds beyond known prognostic factors
- Tests incremental benefit of staging information
- Demonstrates staging system necessity
- Quantifies staging-specific discrimination

**Clinical relevance:**
- "Does staging improve prediction beyond age, comorbidities, etc.?"
- Pure staging system evaluation
- Regulatory approval contexts
- Cost-effectiveness analysis baseline

```r
baselineModel = "covariates_only"
```

### **Original Staging + Covariates**
**When to use:**
- Established staging system exists
- Want to show improvement over current practice
- When old staging is clinical standard
- Incremental improvement demonstration

**Interpretation:**
- Shows benefit of new staging over current standard
- Measures practical clinical improvement
- Demonstrates real-world impact
- Quantifies upgrade necessity

**Clinical relevance:**
- "Should we adopt the new staging system?"
- Practical implementation decisions
- Training and education planning
- System upgrade justification

```r
baselineModel = "original_plus_covariates"
```

### **New Staging + Covariates**
**When to use:**
- New staging system is reference
- Want to demonstrate necessity of comprehensive approach
- When new staging is proposed standard
- Validation of new system completeness

**Interpretation:**
- Shows whether new staging alone is sufficient
- Tests if additional factors improve prediction
- Validates new staging comprehensiveness
- Identifies complementary factors

**Clinical relevance:**
- "Is the new staging system complete?"
- Factor identification for staging refinement
- Completeness assessment
- Future staging development guidance

```r
baselineModel = "new_plus_covariates"
```

## Decision Matrix: Choosing the Right Configuration

### **Research Type-Based Selection**

| Research Type | Analysis Scope | Cancer Type | Multifactorial Type | Baseline Model |
|---------------|----------------|-------------|---------------------|----------------|
| **Pilot Study** | Basic | General | adjusted_cindex | covariates_only |
| **Standard Validation** | Standard | Specific | nested_models | original_plus_covariates |
| **Comprehensive Research** | Comprehensive | Specific | comprehensive | covariates_only |
| **Clinical Implementation** | Standard | Specific | adjusted_cindex | original_plus_covariates |
| **Guideline Development** | Comprehensive | Specific | comprehensive | covariates_only |
| **Regulatory Submission** | Publication | Specific | nested_models | original_plus_covariates |
| **Methodology Development** | Comprehensive | General | comprehensive | covariates_only |

### **Resource-Based Selection**

#### **Limited Resources**
- **Analysis Scope**: Basic or Standard
- **Multifactorial**: adjusted_cindex
- **Computational Time**: < 5 minutes
- **Memory Requirements**: Low
- **Suitable for**: Initial exploration, teaching, pilot studies

#### **Moderate Resources**
- **Analysis Scope**: Standard or Comprehensive
- **Multifactorial**: nested_models or stepwise
- **Computational Time**: 5-15 minutes
- **Memory Requirements**: Moderate
- **Suitable for**: Most research applications

#### **High Resources**
- **Analysis Scope**: Comprehensive
- **Multifactorial**: comprehensive
- **Computational Time**: 15-45 minutes
- **Memory Requirements**: High
- **Suitable for**: Definitive research, high-impact studies

### **Publication Target-Based Selection**

#### **High-Impact Journals (IF > 10)**
```r
analysisType = "comprehensive"
cancerType = "specific_to_study"
multifactorialComparisonType = "comprehensive"
enableMultifactorialAnalysis = TRUE
performBootstrap = TRUE
performCrossValidation = TRUE
calculateNRI = TRUE
performDCA = TRUE
```

#### **Specialty Journals (IF 3-10)**
```r
analysisType = "standard"
cancerType = "specific_to_study"
multifactorialComparisonType = "nested_models"
enableMultifactorialAnalysis = TRUE
calculateNRI = TRUE
performBootstrap = TRUE
```

#### **General Medical Journals (IF < 3)**
```r
analysisType = "standard"
cancerType = "general"
multifactorialComparisonType = "adjusted_cindex"
enableMultifactorialAnalysis = FALSE
```

## Study Design Considerations

### **Single-Center Studies**
- **Cross-validation**: Use k-fold CV
- **Bootstrap**: Essential for internal validation
- **Sample size**: Monitor for adequate power
- **Multifactorial**: Comprehensive if sample size adequate

### **Multi-Center Studies**
- **Institution variable**: Always include
- **Cross-validation**: Use internal-external CV
- **Heterogeneity**: Test for center effects
- **Baseline model**: Include center as covariate

### **Registry-Based Studies**
- **Sample size**: Usually adequate for comprehensive analysis
- **Missing data**: Consider impact on multifactorial analysis
- **Population representativeness**: Use appropriate cancer type
- **Validation**: Bootstrap + cross-validation recommended

### **Clinical Trial Data**
- **Homogeneous population**: May limit generalizability
- **Treatment effects**: Include in covariates
- **Follow-up**: Usually excellent, enable all time-dependent analyses
- **Regulatory focus**: Use publication-ready analysis type

## Common Configuration Examples

### **Example 1: Initial Lung Cancer Staging Validation**
```r
jsurvival::stagemigration(
  data = lung_cancer_data,
  oldStage = "tnm7_stage", 
  newStage = "tnm8_stage",
  survivalTime = "os_months",
  event = "death_status",
  eventLevel = "Dead",
  analysisType = "standard",
  cancerType = "lung",
  enableMultifactorialAnalysis = TRUE,
  continuousCovariates = vars(Age),
  categoricalCovariates = vars(Gender, Histology, SmokingStatus),
  multifactorialComparisonType = "adjusted_cindex",
  baselineModel = "original_plus_covariates",
  calculateNRI = TRUE,
  nriTimePoints = "6, 12, 24"
)
```

### **Example 2: Comprehensive Breast Cancer Research**
```r
jsurvival::stagemigration(
  data = breast_cancer_data,
  oldStage = "tnm7_stage", 
  newStage = "tnm8_stage",
  survivalTime = "os_months",
  event = "death_status",
  eventLevel = "Dead",
  analysisType = "comprehensive",
  cancerType = "breast",
  enableMultifactorialAnalysis = TRUE,
  continuousCovariates = vars(Age, TumorSize),
  categoricalCovariates = vars(Grade, ERStatus, PRStatus, HER2Status),
  multifactorialComparisonType = "comprehensive",
  baselineModel = "covariates_only",
  performInteractionTests = TRUE,
  calculateNRI = TRUE,
  performDCA = TRUE,
  performBootstrap = TRUE,
  performCrossValidation = TRUE,
  nriTimePoints = "12, 24, 60, 120"
)
```

### **Example 3: Multi-Center Colorectal Cancer Validation**
```r
jsurvival::stagemigration(
  data = colorectal_data,
  oldStage = "tnm7_stage", 
  newStage = "tnm8_stage",
  survivalTime = "os_months",
  event = "death_status",
  eventLevel = "Dead",
  analysisType = "comprehensive",
  cancerType = "colorectal",
  institutionVariable = "center",
  enableMultifactorialAnalysis = TRUE,
  continuousCovariates = vars(Age),
  categoricalCovariates = vars(Gender, Location, Grade, MSI_Status),
  multifactorialComparisonType = "comprehensive",
  baselineModel = "covariates_only",
  performCrossValidation = TRUE,
  cvFolds = 5,
  calculateNRI = TRUE,
  performDCA = TRUE,
  nriTimePoints = "12, 36, 60"
)
```

### **Example 4: Pilot Study with Limited Resources**
```r
jsurvival::stagemigration(
  data = pilot_data,
  oldStage = "current_stage", 
  newStage = "proposed_stage",
  survivalTime = "survival_time",
  event = "outcome",
  eventLevel = "Death",
  analysisType = "basic",
  cancerType = "general",
  enableMultifactorialAnalysis = FALSE
)
```

## Performance and Resource Management

### **Computational Complexity by Configuration**

| Configuration | Time Estimate | Memory Usage | Bootstrap Iterations | CV Folds |
|---------------|---------------|--------------|---------------------|----------|
| Basic | < 1 minute | Low | 0 | 0 |
| Standard | 1-5 minutes | Low-Moderate | 0 | 0 |
| Standard + Multifactorial | 5-10 minutes | Moderate | Optional | Optional |
| Comprehensive | 15-30 minutes | High | 1000 | 5 |
| Comprehensive + Multi-center | 30-60 minutes | High | 1000 | Institution-based |

### **Memory Requirements**

- **Small datasets (n < 500)**: All configurations feasible
- **Medium datasets (n = 500-2000)**: Monitor bootstrap operations
- **Large datasets (n > 2000)**: Consider reducing bootstrap iterations
- **Very large datasets (n > 10000)**: Use basic or standard analysis

### **Optimization Strategies**

1. **For Large Datasets**:
   - Use `analysisType = "standard"`
   - Set `bootstrapReps = 500` instead of 1000
   - Disable cross-validation for initial analysis

2. **For Limited Time**:
   - Start with `analysisType = "basic"`
   - Progress to comprehensive if results warrant
   - Use `multifactorialComparisonType = "adjusted_cindex"`

3. **For Memory Constraints**:
   - Disable bootstrap validation initially
   - Reduce number of covariates
   - Use stratified sampling for very large datasets

## Quality Control and Validation

### **Minimum Requirements for Publication**

1. **Sample Size**:
   - Minimum 100 events for stable estimates
   - At least 10 events per covariate in multifactorial analysis
   - Adequate representation in each stage category

2. **Follow-up**:
   - Minimum 5% events at shortest time point of interest
   - Adequate follow-up for time-dependent analyses
   - Consider competing risks if high non-cancer mortality

3. **Statistical Rigor**:
   - Bootstrap validation recommended for all publications
   - Cross-validation for multi-center or large single-center studies
   - Multifactorial analysis when established prognostic factors exist

### **Red Flags Requiring Configuration Changes**

1. **Convergence Issues**:
   - Reduce number of covariates
   - Check for perfect separation
   - Consider regularization methods

2. **Sparse Data**:
   - Use basic analysis type
   - Combine adjacent stage categories
   - Increase follow-up time

3. **High Missing Data**:
   - Address missingness before analysis
   - Consider multiple imputation
   - Document missing data patterns

## Reporting and Interpretation

### **Methods Section Components by Configuration**

#### **Basic Analysis**
- Migration matrix construction
- Chi-square testing methodology
- Stage distribution comparison methods

#### **Standard Analysis**
- C-index calculation methodology
- NRI computation approach
- Time-dependent analysis methods

#### **Comprehensive Analysis**
- All standard components plus:
- Bootstrap validation methodology
- Cross-validation approach
- Decision curve analysis framework
- Advanced calibration assessment

#### **Multifactorial Analysis**
- Covariate selection rationale
- Model building strategy
- Interaction testing approach
- Baseline model justification

### **Results Reporting Standards**

1. **Always Report**:
   - Sample characteristics
   - Migration patterns and rates
   - Primary discrimination metrics

2. **For Standard+ Analyses**:
   - Bootstrap-corrected estimates
   - Cross-validation results
   - Clinical significance assessment

3. **For Multifactorial Analyses**:
   - Model selection results
   - Interaction test outcomes
   - Adjusted performance metrics

## Troubleshooting Common Issues

### **Configuration-Related Problems**

1. **Analysis Takes Too Long**:
   - Reduce analysis scope
   - Disable bootstrap/cross-validation
   - Use adjusted_cindex multifactorial type

2. **Insufficient Memory**:
   - Reduce dataset size
   - Disable comprehensive analysis
   - Use basic configuration

3. **Convergence Failures**:
   - Check covariate collinearity
   - Reduce model complexity
   - Validate data quality

4. **Unrealistic Results**:
   - Verify cancer type selection
   - Check baseline model choice
   - Review covariate definitions

### **Cancer Type-Specific Considerations**

1. **Aggressive Cancers (Lung, Pancreatic)**:
   - Use shorter time points
   - Consider competing risks
   - Expect lower discrimination metrics

2. **Indolent Cancers (Prostate, Thyroid)**:
   - Use extended time points
   - Monitor for adequate events
   - Consider disease-specific outcomes

3. **Heterogeneous Cancers (Sarcoma, CNS)**:
   - Use "other" cancer type
   - Consider subtype-specific analysis
   - Validate results across subtypes

---

*This guide provides comprehensive decision support for configuring stage migration analysis. For specific research scenarios not covered, consider the general principles and consult recent literature for domain-specific best practices.*