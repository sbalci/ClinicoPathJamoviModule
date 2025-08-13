# Statistical Methods for Digital Pathology jamovi Module: A Comprehensive Technical Guide

## Executive Summary

This report presents a comprehensive analysis of statistical methods used in digital pathology research suitable for implementation as a jamovi module. Based on extensive research across peer-reviewed literature, software documentation, and existing R packages, we identify both classical and advanced statistical approaches specifically tailored for analyzing whole slide images (WSI), region of interest (ROI) annotations, cell segmentation data, and tissue classification results. The findings reveal that 93% of pathology studies employ statistical testing, with Mann-Whitney U tests and Cox regression emerging as the most critical methods for implementation.

## 1. Classical Statistical Tests in Digital Pathology

### Core Tests for Implementation

The analysis of 195 articles from the American Journal of Pathology reveals the following essential classical tests:

**Two-Sample Comparisons:**
- **Mann-Whitney U Test** (Wilcoxon Rank-Sum)
  - Application: Comparing biomarker expression between tumor vs. normal tissue
  - R Implementation: `wilcox.test()` in base R
  - Usage frequency: ~30% of pathology studies
  - Example: PD-L1 expression scores between immune infiltration groups

- **Student's t-test**
  - Application: Continuous measurements (cell counts, intensity values)
  - R Implementation: `t.test()` in base R
  - Note: Often overutilized (32% of studies) despite assumption violations

**Multi-Sample Comparisons:**
- **Kruskal-Wallis Test**
  - Application: Comparing biomarker expression across tumor stages
  - R Implementation: `kruskal.test()` in base R
  
- **ANOVA with Post Hoc Tests**
  - R Implementation: `aov()`, `lm()` functions
  - Critical issue: 68% of studies fail to perform proper post hoc testing
  - Recommended: Tukey HSD, Dunnett's test for multiple comparisons

**Categorical Analysis:**
- **Chi-square and Fisher's Exact Tests**
  - Application: Biomarker positivity vs. survival status
  - R Implementation: `chisq.test()`, `fisher.test()`

## 2. Advanced Statistical Methods with R Implementations

### Survival Analysis Methods

**Cox Proportional Hazards Regression**
- Application: Time-to-event analysis with image-derived features
- R Implementation: `survival` package, `coxph()` function
- Variants:
  - Penalized Cox (LASSO/Ridge) via `glmnet` package
  - Time-varying coefficients for non-proportional hazards
  - Stratified models for nested data structures

**Kaplan-Meier Analysis**
- R Implementation: `survival` package, `survfit()` function
- Enhanced visualization: `survminer` package
- Optimal cutpoint determination: `surv_cutpoint()` function

**Time-Dependent ROC Analysis**
- R Packages: `timeROC`, `survivalROC`, `survAUC`
- Application: Evaluating biomarker discrimination over time
- Methods: Cumulative/Dynamic (C/D), Incident/Dynamic (I/D) approaches

### Machine Learning Methods

**Random Forest Classification**
- R Implementations: `randomForest`, `randomForestSRC`, `ranger`
- QuPath integration confirmed
- Applications: Cell classification, outcome prediction
- Variable importance ranking capabilities

## 3. Whole Slide Image Quantitative Analysis

### Tile-Based Statistical Methods

**Multi-Resolution Hierarchical Analysis**
- Pyramid-based analysis for gigapixel images
- Moving window statistics (recommended: 0.4mm × 0.4mm windows)
- R Implementation: Custom functions with `spatstat` integration

**Computational Statistics for Large Data**
- Incremental/streaming algorithms for memory efficiency
- R Packages: `bigmemory`, `ff` for out-of-core computation

**Quality Control Statistics**
- Stain normalization metrics
- Focus quality assessment
- Artifact detection using robust statistics

## 4. ROI-Based Measurements and Comparisons

### Statistical Methods for Multiple ROIs

**Hierarchical Mixed-Effects Models**
- Structure: Patient > Slide > ROI > Cell hierarchy
- R Implementation: `lme4` package, `lmer()` function
- Example:
```r
model <- lmer(cell_measurement ~ treatment + (1|patient_id/roi_id), data=pathology_data)
```

**ROI Comparison Statistics**
- Paired analysis methods
- FDR correction for multiple ROI comparisons
- Effect size measures (Cohen's d)

## 5. Cell Segmentation Metrics

### Segmentation Quality Assessment

**Reference-Based Metrics:**
- Dice coefficient (F1-score): 0.83-0.84 typical for nuclei
- Jaccard Index (IoU)
- Hausdorff distance for boundary accuracy
- R Implementation: `EBImage` package

**Count Data Analysis:**
- Poisson/Negative binomial models
- Zero-inflation models (ZIP, ZINB)
- R Implementation: `MASS`, `pscl` packages

**Nuclear Morphometry:**
- Shape analysis (area, perimeter, circularity)
- Texture analysis (Haralick features)
- R Implementation: `EBImage`, radiomics packages

## 6. Tissue Classification Validation

### Classification Accuracy Metrics

**Confusion Matrix Analysis:**
- Accuracy, sensitivity, specificity, precision, F1-score
- R Implementation: `caret`, `MLmetrics` packages
- Multi-class metrics: macro/micro-averaged scores

**ROC Analysis:**
- Single and multi-class ROC curves
- R Implementation: `pROC`, `multiROC` packages
- Bootstrap confidence intervals for AUC

## 7. Spatial Statistics for Digital Pathology

### Core Spatial Methods

**Ripley's K Function and Variants**
- Mathematical basis: K(r) = λ⁻¹E[number of points within distance r]
- R Implementation: `spatstat` package
```r
K_result <- Kest(point_pattern, correction="Ripley")
L_result <- Lest(point_pattern, correction="Ripley")
```

**Specialized Package: SpatialQPFs**
- GitHub: Genentech/SpatialQPFs
- Functions: Multi-type K-functions, Clark-Evans analysis, Morisita index
- Installation: `devtools::install_github("Genentech/SpatialQPFs")`

**spatialTIME Package (CRAN)**
- Purpose: Spatial analysis of multiplex immunofluorescence
- Functions: Univariate/bivariate spatial statistics
- Installation: `install.packages("spatialTIME")`
- Jamovi compatibility: High

**Spatial Clustering Detection:**
- Morisita Index for immune cell clustering
- Getis-Ord Gi* statistics for hotspot detection
- R Implementation: `spdep` package

## 8. Hierarchical Data Analysis Methods

### Mixed-Effects Models for Nested Data

**Linear Mixed-Effects Models (LMMs)**
- Two-level model: y_ij = β₀ + β₁x_ij + u_j + ε_ij
- Three-level implementation:
```r
three_level <- lmer(outcome ~ cell_type + roi_area + patient_age + 
                   (1|patient_id/roi_id), data=hierarchical_data)
```

**Variance Component Analysis**
- Intraclass Correlation Coefficient (ICC)
- R Implementation: `performance`, `insight` packages

**Generalized Linear Mixed Models (GLMMs)**
- Logistic mixed models for binary outcomes
- Poisson mixed models for count data
- R Implementation: `glmer()` function in `lme4`

## 9. Survival Analysis for Digital Pathology Biomarkers

### Advanced Survival Methods

**Optimal Cutpoint Determination**
- Maximally selected rank statistics
- Concordance index optimization
- R Implementation: `survminer` package
```r
cutpoint_results <- surv_cutpoint(data, time="time", event="status",
                                 variables=c("nuclear_area", "texture_entropy"))
```

**Machine Learning Survival Models**
- Random survival forests: `randomForestSRC` package
- Gradient boosting: `gbm` package
- Survival SVMs available

## 10. Inter-Observer Agreement Statistics

### Agreement Measures for Pathology

**Cohen's and Fleiss' Kappa**
- Two-rater agreement: Cohen's κ
- Multiple raters: Fleiss' κ
- R Implementation: `psych`, `irr` packages
- Interpretation: κ > 0.61 indicates substantial agreement

**Intraclass Correlation Coefficient (ICC)**
- Types: ICC(1,1), ICC(2,1), ICC(3,1) for different study designs
- R Implementation: `psych`, `ICC` packages
- Interpretation: ICC > 0.75 indicates good reliability

**Bland-Altman Analysis**
- For continuous measurement agreement
- R Implementation: `blandr` package
- Components: Bias, limits of agreement, confidence intervals

## 11. Machine Learning Evaluation Metrics

### Pathology-Specific Metrics

**Segmentation Metrics:**
- Dice coefficient, Jaccard index
- Average surface distance
- H-score for immunohistochemistry

**Imbalanced Dataset Metrics:**
- Area Under Precision-Recall Curve (AUPRC)
- Matthews Correlation Coefficient (MCC)
- Balanced accuracy
- G-mean: √(Sensitivity × Specificity)

## 12. Multiple Testing Corrections

### FDR Control Methods

**Standard Approaches:**
- Benjamini-Hochberg: `p.adjust(method="BH")`
- Storey's q-value: `qvalue` package (Bioconductor)
- Benjamini-Yekutieli for dependent tests

**Advanced Methods:**
- Independent Hypothesis Weighting (IHW): Bioconductor package
- AdaPT: Adaptive p-value thresholding
- Permutation-based FDR for correlated features

## 13. Existing R Packages for Digital Pathology

### High-Priority Packages for jamovi Integration

**Core Statistical Packages:**
1. **spatialTIME** (CRAN) - Spatial analysis for pathology
2. **survival** & **survminer** - Comprehensive survival analysis
3. **lme4** - Hierarchical modeling
4. **pROC** - ROC analysis
5. **psych** & **irr** - Agreement statistics

**Specialized Pathology Packages:**
- **SpatialQPFs** - Advanced spatial statistics
- **QuPath R interfaces** - WSI analysis integration
- **EBImage** (Bioconductor) - Image analysis

**Machine Learning:**
- **randomForest** - Used by QuPath
- **caret** - Comprehensive ML framework
- **MLmetrics** - Evaluation metrics

## Implementation Recommendations for jamovi Module

### Priority 1: Essential Methods
1. Mann-Whitney U Test with assumption checking
2. Cox Proportional Hazards Regression
3. Kaplan-Meier Analysis with optimal cutpoints
4. Chi-square/Fisher's Exact Tests
5. Multiple testing corrections (Bonferroni, BH-FDR)

### Priority 2: Advanced Core Methods
1. Random Forest Classification
2. Linear Mixed-Effects Models for hierarchical data
3. ICC for inter-rater reliability
4. ROC/AUC analysis
5. Spatial point pattern analysis (Ripley's K)

### Priority 3: Specialized Features
1. Time-dependent ROC analysis
2. Morisita index for clustering
3. Bland-Altman plots
4. Zero-inflated count models
5. Penalized regression methods

### Technical Implementation Guidelines

**Data Structure Support:**
- Hierarchical: Patient > Slide > ROI > Cell
- Spatial: X,Y coordinates with cell type annotations
- Censored: Survival time and event indicators
- High-dimensional: Feature matrices from image analysis

**Visualization Requirements:**
- Kaplan-Meier curves with risk tables
- Forest plots for hazard ratios
- Spatial point pattern plots
- ROC curves with confidence bands
- Bland-Altman agreement plots

**Integration Considerations:**
- Minimize external dependencies
- Prioritize CRAN packages over Bioconductor
- Ensure QuPath export compatibility
- Support batch processing for high-throughput analysis
- Include quality control and assumption checking

## Conclusions

Digital pathology research requires a diverse statistical toolkit spanning classical hypothesis testing, advanced survival analysis, spatial statistics, and machine learning evaluation. The proposed jamovi module should prioritize the Mann-Whitney U test and Cox regression as foundational methods, while incorporating specialized tools like spatialTIME and hierarchical modeling capabilities. With 93% of pathology studies using statistical testing but often with methodological issues (68% lacking proper post hoc tests, rare multiple testing corrections), a well-designed jamovi module could significantly improve statistical rigor in the field.

The R ecosystem provides comprehensive support through packages like spatialTIME, survival, lme4, and spatstat, all of which demonstrate high compatibility with jamovi's framework. Implementation should focus on creating an intuitive interface that guides users toward appropriate statistical methods while maintaining the flexibility needed for diverse pathology applications.