# Spatial Compartment Analysis in IHC Modules

## Overview

Spatial compartment analysis has been integrated into **two complementary IHC analysis modules**:

1. **`ihccluster`** - Multi-marker clustering with spatial compartment comparison
2. **`ihcheterogeneity`** - Single-marker heterogeneity with compartment comparison

Both modules now support flexible spatial/compartment analysis for understanding tumor heterogeneity.

---

## Module Comparison

| Feature | `ihccluster` | `ihcheterogeneity` |
|---------|--------------|---------------------|
| **Primary Focus** | Multi-marker patterns | Single-marker variability |
| **Data Type** | Multiple IHC markers | One biomarker, multiple regions |
| **Analysis** | Clustering + compartment comparison | Heterogeneity + compartment comparison |
| **Output** | Cluster assignments by compartment | ICC, CV, bias by compartment |
| **Use Case** | Molecular subtyping | Sampling reliability |

---

## ihccluster: Multi-Marker Spatial Analysis

### Purpose
Identify distinct molecular subtypes and compare their distribution across tumor compartments.

### Key Features
- ✅ Cluster cases based on multiple IHC markers
- ✅ Compare cluster assignments between compartments (e.g., Central vs Invasive)
- ✅ Test marker expression differences by compartment
- ✅ Calculate inter-compartment concordance (Cohen's κ)
- ✅ Visualize spatial patterns with annotated heatmaps

### When to Use
- **Tumor subtyping studies**: Define molecular subtypes (e.g., Luminal A/B, Triple-negative)
- **Spatial evolution**: Track how marker patterns change from center to invasive front
- **Metastatic progression**: Compare primary tumor vs metastases
- **Microenvironment effects**: Understand marker changes in hypoxic vs vascularized regions

### Example Workflow
```
1. Select categorical markers: ER, PR, HER2, CK5/6, EGFR
2. Select continuous markers: Ki67_Percent, AR_Hscore, p53_Percent
3. Set spatial compartment variable: "Compartment" (Central/Invasive)
4. Enable "Perform Spatial Compartment Analysis"
5. Choose mode: "both" (recommended)
6. Run analysis

Results:
- Overall clustering identifies 4 subtypes
- Compartment summary shows clustering quality per region
- Concordance table: κ=0.42 (moderate agreement)
- Marker differences: E-cadherin loss at invasive front (p<0.001, V=0.48)
- Clinical interpretation: Invasive front dedifferentiation
```

### Output Tables
1. **Spatial Compartment Summary** - Clustering quality per compartment
2. **Inter-Compartment Cluster Concordance** - Cohen's κ between regions
3. **Cluster Distribution by Compartment** - Subtype frequencies
4. **Marker Expression Differences** - Statistical tests per marker
5. **Spatial Heatmap** - Annotated visualization

---

## ihcheterogeneity: Single-Marker Spatial Analysis

### Purpose
Quantify intra-tumoral heterogeneity and assess sampling reliability across compartments.

### Key Features
- ✅ Calculate ICC (Intraclass Correlation) by compartment
- ✅ Calculate CV (Coefficient of Variation) by compartment
- ✅ Test if heterogeneity differs significantly between compartments
- ✅ Identify compartments with highest sampling variability
- ✅ Provide compartment-specific sampling recommendations

### When to Use
- **Biopsy sampling studies**: Assess whether central biopsies represent invasive front
- **Quality control**: Validate reproducibility across tumor regions
- **Minimal sampling design**: Determine which compartment requires more samples
- **Heterogeneity characterization**: Quantify region-specific marker variability

### Example Workflow
```
1. Select reference measurement: Ki67_WholeSlide (0-100%)
2. Select regional measurements: Ki67_Central, Ki67_Invasive, Ki67_Periphery
3. Set spatial region ID: "Region" (Central/Invasive/Periphery)
4. Enable "Compare Spatial Compartments"
5. Enable "Compartment Comparison Tests"
6. Run analysis

Results:
- Central: ICC=0.85 (excellent), CV=12% (acceptable)
- Invasive: ICC=0.62 (moderate), CV=28% (high variability)
- Periphery: ICC=0.78 (good), CV=16% (acceptable)
- Levene's test: F=8.3, p=0.001 → heterogeneity differs by compartment
- Interpretation: Invasive front requires more samples (3-5 vs 2-3)
```

### Output Tables
1. **Spatial Heterogeneity Analysis** - ICC, CV, mean by region
2. **Compartment Heterogeneity Comparison** - Metrics with 95% CIs
3. **Statistical Tests for Compartment Differences** - Levene's & Kruskal-Wallis tests

---

## Complementary Use of Both Modules

### Scenario: Comprehensive Spatial Characterization

**Step 1: Use `ihccluster` for subtyping**
- Input: Multiple IHC markers (ER, PR, HER2, Ki67, etc.)
- Output: Identify 3 molecular subtypes (Luminal, HER2+, Triple-negative)
- Spatial analysis: Subtypes shift from central (70% Luminal) to invasive (40% Luminal)

**Step 2: Use `ihcheterogeneity` for each marker**
- Input: Individual markers (Ki67_Percent) across regions
- Output: Ki67 heterogeneity is highest at invasive front (CV=32%)
- Sampling recommendation: Use 5 measurements at invasive front vs 3 centrally

**Combined Interpretation:**
> "Multi-marker clustering identified three molecular subtypes with significant enrichment of aggressive subtypes at invasive fronts (κ=0.48, p<0.001). Single-marker heterogeneity analysis revealed that Ki67 proliferation index exhibits highest variability at invasive fronts (CV=32% vs 15% central, Levene's p=0.002), necessitating increased sampling density (5 vs 3 measurements) for reliable characterization of this prognostically critical region."

---

## Clinical Applications by Disease

### 1. **Colorectal Cancer** (Matsuoka et al., 2011)
**Compartments**: Central parts vs Invasive fronts

**`ihccluster` Use:**
- Markers: Claudin-1, Claudin-4, E-cadherin, β-catenin (membranous/nuclear)
- Finding: 3 prognostic groups (good/intermediate/poor)
- Spatial: Loss of E-cadherin at invasive front → independent predictor (HR=3.42)

**`ihcheterogeneity` Use:**
- Marker: E-cadherin H-score
- Finding: CV=22% at invasive front vs 12% central (p<0.001)
- Action: Recommend 4-5 measurements at invasive front for reliable scoring

### 2. **Breast Cancer**
**Compartments**: DCIS (preinvasive) vs Invasive carcinoma

**`ihccluster` Use:**
- Markers: ER, PR, HER2, Ki67, AR, p53
- Finding: Luminal A DCIS → 30% convert to Luminal B invasive
- Spatial: κ=0.58 (moderate agreement) → dedifferentiation during invasion

**`ihcheterogeneity` Use:**
- Marker: Ki67 percentage
- Finding: ICC_DCIS=0.82 vs ICC_Invasive=0.65 → invasion increases heterogeneity
- Action: Recommend separate Ki67 scoring for DCIS and invasive components

### 3. **Gastric Cancer**
**Compartments**: Superficial vs Deep invasion layers

**`ihccluster` Use:**
- Markers: CDX2, MUC2, MUC5AC, MUC6 (intestinal/gastric markers)
- Finding: Phenotype switching from intestinal (superficial) to gastric (deep)
- Spatial: κ=0.35 (fair agreement) → significant phenotypic shift

**`ihcheterogeneity` Use:**
- Marker: PD-L1 TPS (Tumor Proportion Score)
- Finding: CV_Deep=45% vs CV_Superficial=22% → deep layer is more heterogeneous
- Action: Deep layer biopsies are more reliable for PD-L1 status

### 4. **Lung Cancer - Primary vs Metastatic**
**Compartments**: Primary tumor vs Brain metastasis

**`ihccluster` Use:**
- Markers: TTF-1, Napsin A, CK7, CK20, p40
- Finding: Maintain adenocarcinoma profile in both sites (κ=0.78)
- Spatial: No significant subtype shift → clonal metastasis

**`ihcheterogeneity` Use:**
- Marker: PD-L1 CPS (Combined Positive Score)
- Finding: ICC_Primary=0.71 vs ICC_Metastasis=0.62 → similar heterogeneity
- Action: Both sites require 3-4 measurements for reliable PD-L1 scoring

---

## Statistical Methods

### ihccluster Spatial Statistics

#### 1. **Cohen's Kappa** (Inter-compartment concordance)
```
κ = (p_observed - p_expected) / (1 - p_expected)

Interpretation (Landis & Koch 1977):
κ < 0: Poor agreement
κ = 0.0-0.2: Slight
κ = 0.2-0.4: Fair
κ = 0.4-0.6: Moderate
κ = 0.6-0.8: Substantial
κ = 0.8-1.0: Almost perfect
```

#### 2. **Mann-Whitney U** (Continuous marker differences)
- Non-parametric test for distributional differences
- Reports: U statistic, p-value, Cohen's d effect size

#### 3. **Chi-square / Fisher's exact** (Categorical marker differences)
- Tests association between compartment and marker level
- Reports: χ² statistic, p-value, Cramér's V effect size

### ihcheterogeneity Spatial Statistics

#### 1. **Intraclass Correlation Coefficient (ICC)**
```
ICC(2,1) = (BMS - WMS) / (BMS + (k-1)×WMS)

where:
BMS = Between-subjects mean square
WMS = Within-subjects mean square
k = number of measurements per subject

Interpretation:
ICC < 0.5: Poor reliability
ICC = 0.5-0.75: Moderate
ICC = 0.75-0.9: Good
ICC > 0.9: Excellent
```

#### 2. **Coefficient of Variation (CV)**
```
CV = (SD / Mean) × 100%

Interpretation:
CV < 10%: Low variability
CV = 10-20%: Acceptable
CV = 20-30%: High variability
CV > 30%: Very high variability
```

#### 3. **Levene's Test** (Variance equality across compartments)
```
Tests H₀: σ²₁ = σ²₂ = ... = σ²ₖ

If p < 0.05: Heterogeneity differs significantly between compartments
```

#### 4. **Kruskal-Wallis Test** (Distributional differences)
```
Tests H₀: Distributions are identical across compartments

If p < 0.05: At least one compartment differs
Follow up with post-hoc pairwise comparisons
```

---

## Data Structure Requirements

### ihccluster Requirements
```csv
CaseID,Compartment,ER,PR,HER2,Ki67_Percent,AR_Hscore
BC001,Central,Positive,Positive,1+,12.5,200
BC001,Invasive,Negative,Negative,2+,35.2,100
BC002,Central,Positive,Positive,0,8.1,250
BC002,Invasive,Positive,Negative,1+,18.9,180
```
- **Format**: Long format (one row per case-compartment combination)
- **Compartment variable**: Factor with ≥2 levels
- **Markers**: Mix of categorical and continuous

### ihcheterogeneity Requirements
```csv
CaseID,Region,Ki67_WholeSlide,Ki67_Region1,Ki67_Region2,Ki67_Region3
BC001,Central,15.2,14.8,15.6,15.1
BC002,Central,22.1,21.5,23.2,21.8
BC003,Invasive,28.5,22.1,31.8,32.2
BC004,Invasive,35.2,29.5,38.1,37.8
```
- **Format**: Wide format (one row per case)
- **Reference**: Optional whole-slide or hotspot measurement
- **Regional measurements**: 2-10 measurements per case
- **Region ID**: Optional factor for spatial grouping

---

## Interpretation Guidelines

### ihccluster Spatial Interpretation

| Kappa | Concordance % | Interpretation | Clinical Action |
|-------|---------------|----------------|-----------------|
| <0.2 | <40% | Poor agreement - different biology | Report compartments separately |
| 0.2-0.4 | 40-60% | Fair - some differences | Consider spatial factors in prognosis |
| 0.4-0.6 | 60-75% | Moderate - detectable shift | Invasive front may be more prognostic |
| 0.6-0.8 | 75-90% | Substantial - mostly similar | Either compartment representative |
| >0.8 | >90% | Almost perfect - homogeneous | Single compartment sufficient |

### ihcheterogeneity Spatial Interpretation

| ICC Difference | CV Difference | Interpretation | Sampling Recommendation |
|----------------|---------------|----------------|-------------------------|
| ΔICC < 0.1 | ΔCV < 5% | Similar reliability | Use same sampling protocol |
| ΔICC = 0.1-0.2 | ΔCV = 5-10% | Modest difference | +1 sample in variable region |
| ΔICC = 0.2-0.3 | ΔCV = 10-20% | Substantial difference | +2 samples in variable region |
| ΔICC > 0.3 | ΔCV > 20% | Very different | Double sampling in variable region |

---

## Example Report Statements

### ihccluster Spatial Report

> "Immunohistochemical clustering analysis identified four distinct molecular subtypes based on expression of ER, PR, HER2, Ki67, and AR (optimal k=4, average silhouette=0.68). Spatial compartment analysis revealed significant discordance between central tumor regions and invasive fronts (Cohen's κ=0.42, 95%CI: 0.28-0.56, moderate agreement). Loss of E-cadherin expression was significantly more frequent at invasive fronts compared to central regions (χ²=18.5, df=1, p<0.001, Cramér's V=0.48, medium effect). The invasive-front-dominant subtype (n=22, 28% of invasive cases vs 12% of central cases) was associated with worse prognosis in multivariable analysis (HR=2.66, 95%CI: 1.54-4.60, p<0.001), indicating that evaluation of invasive front markers provides additional prognostic value beyond central tumor assessment."

### ihcheterogeneity Spatial Report

> "Intra-tumoral heterogeneity analysis of Ki67 proliferation index demonstrated significant variability across spatial compartments. The invasive front exhibited higher heterogeneity (CV=28%, 95%CI: 24-33%) compared to central tumor regions (CV=15%, 95%CI: 12-18%, Levene's test: F(2,147)=8.3, p=0.001). Intraclass correlation coefficients indicated moderate agreement at invasive fronts (ICC=0.62, 95%CI: 0.51-0.72) versus excellent agreement in central regions (ICC=0.85, 95%CI: 0.79-0.90). Based on these findings, we recommend obtaining 4-5 measurements from invasive fronts versus 2-3 from central regions to achieve comparable sampling reliability (target CV <20%). This compartment-specific sampling strategy ensures accurate Ki67 quantification in the prognostically critical invasive front region where tumor heterogeneity is highest."

---

## Troubleshooting

### Common Issues

**Issue**: "Spatial analysis requires at least 2 compartments"
- **Check**: `table(data$CompartmentVariable)` has ≥2 levels
- **Solution**: Ensure your grouping variable is correctly formatted as factor

**Issue**: Kappa = NA in ihccluster
- **Cause**: Different numbers of clusters in each compartment
- **Solution**: Normal - focus on concordance percentage instead

**Issue**: All ICC values = NA in ihcheterogeneity
- **Cause**: <2 measurements per case or no variation
- **Solution**: Need multiple measurements per case within each compartment

**Issue**: Levene's test p-value = 1.0
- **Cause**: Equal variances (no heterogeneity difference)
- **Action**: Report that compartments have similar reliability

---

## References

1. **Matsuoka T, et al. (2011)**. Cluster Analysis of Claudin-1 and -4, E-Cadherin, and β-Catenin Expression in Colorectal Cancers. *J Surg Oncol*, 103:674-686.

2. **Landis JR, Koch GG (1977)**. The measurement of observer agreement for categorical data. *Biometrics*, 33:159-174.

3. **Shrout PE, Fleiss JL (1979)**. Intraclass correlations: uses in assessing rater reliability. *Psychol Bull*, 86:420-428.

4. **McGraw KO, Wong SP (1996)**. Forming inferences about some intraclass correlation coefficients. *Psychol Methods*, 1:30-46.

5. **Polley MY, et al. (2013)**. An International Ki67 Reproducibility Study. *J Natl Cancer Inst*, 105:1897-1906.

6. **Dowsett M, et al. (2011)**. Assessment of Ki67 in breast cancer: recommendations from the International Ki67 in Breast Cancer working group. *J Natl Cancer Inst*, 103:1656-1664.

---

## Summary

**Two complementary tools for spatial IHC analysis:**

- **`ihccluster`**: Multi-marker molecular subtyping with spatial evolution tracking
- **`ihcheterogeneity`**: Single-marker sampling reliability with compartment-specific recommendations

**Both modules share:**
- Flexible compartment definition (not limited to center/periphery)
- Statistically sound methods (Cohen's κ, ICC, Levene's test)
- Clinical interpretation guidelines
- Publication-ready output tables

**Use together for:**
- Comprehensive spatial characterization
- Subtype identification + marker reliability
- Optimize sampling protocols for specific compartments
- Understand biological behavior across tumor landscape