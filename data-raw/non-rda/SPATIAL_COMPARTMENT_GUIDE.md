# Spatial/Compartment Analysis in IHC Clustering

## Overview

The spatial compartment analysis feature allows you to compare IHC marker expression patterns across different spatial regions of tumors or different disease states. This is critical for understanding tumor heterogeneity and biological behavior.

## Clinical Applications

### 1. **Tumor Compartments (Matsuoka et al., 2011)**
- **Central parts**: Superficial to deepest third of tumor
- **Invasive fronts**: Deepest fourth of tumor (most aggressive region)
- **Clinical significance**: Loss of adhesion molecules at invasive fronts predicts poor prognosis

### 2. **Disease Progression States**
- **Preinvasive**: Dysplasia, carcinoma in situ
- **Invasive**: Infiltrating carcinoma
- **Clinical significance**: Identify markers that change during malignant transformation

### 3. **Primary vs Metastatic**
- **Primary tumor**: Original site
- **Metastatic**: Lymph node, distant organ metastases
- **Clinical significance**: Understand clonal evolution and therapy resistance

### 4. **Tumor Microenvironment**
- **Core**: Hypoxic center
- **Periphery**: Well-vascularized edge
- **Clinical significance**: Microenvironmental adaptation of tumor cells

## Data Structure

Your data must include:
1. **IHC marker variables** (categorical and/or continuous)
2. **Spatial compartment variable** (factor with ≥2 levels)

Example:
```csv
CaseID,Compartment,ER,PR,HER2,Ki67_Percent
BC001,Central,Positive,Positive,1+,12.5
BC001,Invasive,Negative,Negative,2+,35.2
BC002,Central,Positive,Positive,0,8.1
BC002,Invasive,Positive,Negative,1+,18.9
```

## Analysis Modes

### Mode 1: "between" - Compare clusters between compartments
- Clusters **all cases together** (central + invasive)
- Compares cluster assignments **between compartments**
- Answers: "Do central and invasive regions cluster differently?"
- **Use when**: Same cases appear in multiple compartments

### Mode 2: "within" - Cluster each compartment separately
- Performs **separate clustering** for each compartment
- Answers: "What are the marker patterns within each compartment?"
- **Use when**: Different cases in each compartment OR want compartment-specific patterns

### Mode 3: "both" - Comprehensive analysis
- Performs both between and within analyses
- **Recommended**: Most informative approach

## Statistical Methods

### 1. **Concordance Analysis**
- **Metric**: Cohen's Kappa
- **Interpretation** (Landis & Koch, 1977):
  - κ < 0: Poor agreement
  - κ = 0.0-0.2: Slight agreement
  - κ = 0.2-0.4: Fair agreement
  - κ = 0.4-0.6: Moderate agreement
  - κ = 0.6-0.8: Substantial agreement
  - κ = 0.8-1.0: Almost perfect agreement

### 2. **Marker Difference Testing**
- **Continuous markers**: Mann-Whitney U test + Cohen's d
- **Categorical markers**: Chi-square test + Cramér's V
- **Purpose**: Identify which markers differ significantly between compartments

### 3. **Cluster Quality by Compartment**
- **Silhouette width**: Cluster cohesion
- **Quality ratings**: Excellent (>0.7), Good (>0.5), Fair (>0.25), Poor (≤0.25)

## Output Tables

### 1. **Spatial Compartment Summary**
Shows clustering quality for each compartment:
- N cases per compartment
- N clusters identified
- Average silhouette width
- Quality rating

### 2. **Inter-Compartment Cluster Concordance**
Shows agreement between compartments:
- Concordance percentage
- Cohen's kappa
- Interpretation

### 3. **Cluster Distribution by Compartment**
Shows how clusters distribute across compartments:
- Cluster frequencies
- Percentages
- Identifies compartment-specific clusters

### 4. **Marker Expression Differences by Compartment**
Statistical tests for each marker:
- Test statistic
- p-value
- Effect size (Cohen's d or Cramér's V)

### 5. **Spatial Compartment Heatmap**
Visualizes:
- Marker expression patterns
- Compartment annotations (colored bars)
- Cluster assignments
- Hierarchical relationships

## Clinical Interpretation Examples

### Example 1: Colorectal Cancer (Matsuoka et al., 2011)
**Finding**: Loss of E-cadherin at invasive front
- **Concordance**: κ = 0.35 (Fair agreement)
- **Survival impact**: Independent predictor (HR=3.42, p<0.001)
- **Interpretation**: Different biological behavior at invasive edge
- **Clinical action**: Consider invasive front markers for prognosis

### Example 2: Breast Cancer Progression
**Finding**: ER loss in invasive vs preinvasive regions
- **Concordance**: κ = 0.58 (Moderate agreement)
- **Effect size**: Cramér's V = 0.42 (medium effect)
- **Interpretation**: Dedifferentiation during invasion
- **Clinical action**: May need therapy escalation

### Example 3: Metastatic Evolution
**Finding**: HER2 amplification in metastasis vs primary
- **Concordance**: κ = 0.72 (Substantial agreement)
- **Marker difference**: χ²=15.3, p<0.001, Cramér's V=0.51
- **Interpretation**: Clonal selection or therapy pressure
- **Clinical action**: Re-biopsy metastases for HER2 status

## Recommended Workflow

1. **Prepare data**:
   - Ensure spatial compartment variable is a factor
   - Check for missing values
   - Verify marker types (categorical vs continuous)

2. **Initial clustering** (without spatial analysis):
   - Understand overall marker patterns
   - Determine optimal k

3. **Spatial analysis**:
   - Enable "Perform Spatial Compartment Analysis"
   - Select compartment variable
   - Choose mode: "both" (recommended)

4. **Interpret results**:
   - Check concordance table: High κ → similar biology
   - Check marker differences: Identify changing markers
   - Check cluster distribution: Look for compartment-specific patterns

5. **Clinical validation**:
   - Correlate with survival outcomes
   - Validate with independent cohorts
   - Consider in clinical decision-making

## Mathematical Soundness

### Cohen's Kappa Calculation
```
κ = (p_observed - p_expected) / (1 - p_expected)

where:
p_observed = observed agreement = Σ(diagonal) / total
p_expected = expected agreement = Σ(row_i × col_i) / total²
```

### Effect Sizes
**Cohen's d** (continuous):
```
d = (μ₁ - μ₂) / σ_pooled

where σ_pooled = √[(σ₁² + σ₂²) / 2]

Interpretation:
|d| < 0.2: negligible
|d| = 0.2-0.5: small
|d| = 0.5-0.8: medium
|d| > 0.8: large
```

**Cramér's V** (categorical):
```
V = √(χ² / (n × min(r-1, c-1)))

where:
χ² = chi-square statistic
n = sample size
r, c = number of rows, columns

Interpretation:
V < 0.1: negligible
V = 0.1-0.3: small
V = 0.3-0.5: medium
V > 0.5: large
```

## References

1. **Matsuoka T, et al. (2011)**. Cluster Analysis of Claudin-1 and -4, E-Cadherin, and β-Catenin Expression in Colorectal Cancers. *J Surg Oncol*, 103:674-686.
   - **Key finding**: Invasive front expression is more prognostic
   - **Method**: Hierarchical clustering with Ward's linkage

2. **Landis JR, Koch GG (1977)**. The measurement of observer agreement for categorical data. *Biometrics*, 33:159-174.
   - **Key contribution**: Kappa interpretation guidelines

3. **Cohen J (1988)**. Statistical Power Analysis for the Behavioral Sciences, 2nd ed.
   - **Key contribution**: Effect size guidelines

4. **Palmqvist R, et al. (1998)**. Systematic heterogeneity and prognostic significance of cell proliferation in colorectal cancer. *Br J Cancer*, 77:917-925.
   - **Key contribution**: Tumor compartment definition

## Troubleshooting

### Issue: "Spatial analysis requires at least 2 compartments"
- **Solution**: Check that your compartment variable has ≥2 levels
- **Check**: `table(data$CompartmentVariable)`

### Issue: Kappa = NA in concordance table
- **Cause**: Unequal number of clusters between compartments
- **Solution**: Normal when cluster structures differ
- **Action**: Focus on concordance percentage instead

### Issue: Empty compartment summary table
- **Cause**: Too few cases per compartment (< k clusters)
- **Solution**: Reduce number of clusters OR combine compartments

### Issue: All p-values = 1.0 in marker differences
- **Cause**: No actual differences between compartments
- **Action**: Review data quality OR compartment definition

## Advanced Tips

1. **Multiple Comparisons**: With many markers and compartments, consider Bonferroni correction: p_corrected = p_raw × n_tests

2. **Sample Size**: Minimum 20-30 cases per compartment for reliable clustering

3. **Missing Data**: Spatial analysis uses pairwise distances (handles missing well)

4. **Visualization**: Use spatial heatmap to identify visual patterns before statistical testing

5. **Clinical Validation**: Always validate prognostic findings in independent cohorts

## Example Interpretation Statement

> "Spatial compartment analysis revealed significant discordance between central tumor regions and invasive fronts (κ=0.42, moderate agreement). Loss of E-cadherin expression was significantly more frequent at invasive fronts compared to central regions (χ²=18.5, p<0.001, Cramér's V=0.48, medium effect). Cluster analysis identified three distinct prognostic groups, with invasive front expression patterns showing stronger prognostic value than central tumor markers (multivariate HR=2.66, 95%CI: 1.54-4.60, p<0.001)."