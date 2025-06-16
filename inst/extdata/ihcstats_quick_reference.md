# IHC Expression Analysis Quick Reference Guide

## ihcstats Function Overview

The `ihcstats` function implements four landmark research methodologies for comprehensive immunohistochemical analysis:

- **Matsuoka 2011**: Prognostic clustering with survival analysis
- **Carvalho 2011**: Iterative marker selection optimization  
- **Olsen 2006**: Differential diagnosis clustering for sarcomas
- **Sterlacci 2019**: TIL signature analysis for immune profiling

## Quick Start Examples

### 1. Basic IHC Clustering
```r
library(ClinicoPath)
data("sarcoma_ihc_data")

# Simple hierarchical clustering
result <- ihcstats(
  data = sarcoma_ihc_data,
  markers = c("Vimentin", "SMA", "Desmin", "S100"),
  nClusters = 3,
  showDendrogram = TRUE,
  showHeatmap = TRUE
)
```

### 2. Sarcoma Differential Diagnosis (Olsen Method)
```r
# Comprehensive sarcoma panel
olsen_result <- ihcstats(
  data = sarcoma_ihc_data,
  markers = c("Vimentin", "SMA", "Desmin", "S100", "CD34", "MDM2", "CDK4"),
  differentialDiagnosis = TRUE,
  tumorTypeVar = "Tumor_Type",
  antibodyOptimization = TRUE,
  calculateDiagnosticMetrics = TRUE
)
```

### 3. TIL Analysis for Immunotherapy (Sterlacci Method)
```r
data("nsclc_til_data")

# TIL signature analysis
til_result <- ihcstats(
  data = nsclc_til_data,
  markers = c("CD3", "CD4", "CD8", "Granzyme_B", "PD1", "PDL1"),
  sterlacciAnalysis = TRUE,
  tilAnalysisMode = "combined_til",
  supervisedClustering = TRUE,
  reproductibilityTesting = TRUE
)
```

### 4. Prognostic Clustering with Survival (Matsuoka Method)
```r
data("colorectal_ihc_data")

# Prognostic analysis with survival
prog_result <- ihcstats(
  data = colorectal_ihc_data,
  markers = c("p53", "Ki67", "VEGF", "COX2"),
  prognosticClustering = TRUE,
  linkageMethod = "ward.D2",
  survivalTimeVar = "Overall_Survival_Months",
  survivalEventVar = "Death_Event",
  scoringScale = "matsuoka"
)
```

### 5. Optimal Marker Selection (Carvalho Method)
```r
data("renal_ihc_data")

# Iterative marker optimization
carvalho_result <- ihcstats(
  data = renal_ihc_data,
  markers = c("CD117", "Vimentin_Carvalho", "E_Cadherin", "CK7"),
  iterativeClustering = TRUE,
  scoringScale = "carvalho",
  showClusterValidation = TRUE
)
```

## Parameter Reference

### Core Parameters
| Parameter | Description | Options |
|-----------|-------------|---------|
| `data` | Input dataset | data.frame |
| `markers` | IHC marker variables | character vector |
| `nClusters` | Number of clusters | 2-10 (default: 3) |
| `clusterMethod` | Clustering algorithm | "hierarchical", "pam", "kmeans", "pca_kmeans" |
| `distanceMetric` | Distance measure | "gower", "jaccard" |
| `linkageMethod` | Linkage method | "average", "complete", "ward.D2", "single" |

### Scoring Systems
| Parameter | Description | Scale |
|-----------|-------------|-------|
| `scoringScale` | IHC scoring system | "binary", "standard", "carvalho", "matsuoka", "hscore" |
| `computeHScore` | Calculate H-scores | TRUE/FALSE |

**Scoring Scale Details:**
- **binary**: Negative, Positive
- **standard**: 0, 1+, 2+, 3+ (traditional IHC)
- **carvalho**: 0, 1, 2 (Carvalho renal study)
- **matsuoka**: Mild, Moderate, Marked (3-tier system)
- **hscore**: 0-300 continuous scale

### Research Methodology Flags

#### Matsuoka 2011 - Prognostic Clustering
```r
prognosticClustering = TRUE
survivalTimeVar = "survival_months"
survivalEventVar = "death_event"
tumorRegionAnalysis = TRUE
centralRegionVar = "central_markers"
invasiveRegionVar = "invasive_markers"
```

#### Carvalho 2011 - Marker Optimization
```r
iterativeClustering = TRUE
scoringScale = "carvalho"
showClusterValidation = TRUE
optimalKMethod = "silhouette"
```

#### Olsen 2006 - Differential Diagnosis
```r
differentialDiagnosis = TRUE
tumorTypeVar = "tumor_type"
antibodyOptimization = TRUE
calculateDiagnosticMetrics = TRUE
clusterCutHeight = 0.6
```

#### Sterlacci 2019 - TIL Analysis
```r
sterlacciAnalysis = TRUE
tilAnalysisMode = "combined_til"
supervisedClustering = TRUE
reproductibilityTesting = TRUE
immuneSignatureFocus = TRUE
multipleTesting = "bonferroni"
```

### Visualization Options
| Parameter | Description | Output |
|-----------|-------------|--------|
| `showDendrogram` | Hierarchical tree | Dendrogram plot |
| `showHeatmap` | Expression heatmap | Clustered heatmap |
| `showPCAPlot` | PCA biplot | PCA visualization |
| `showScoreDist` | Score distributions | Distribution plots |
| `showClusterValidation` | Validation metrics | Silhouette plots |

### Advanced Options
| Parameter | Description | Purpose |
|-----------|-------------|---------|
| `standardizeData` | Z-score normalization | Data preprocessing |
| `pcaAnalysis` | Principal component analysis | Dimensionality reduction |
| `groupVariable` | Grouping factor | Stratified analysis |
| `significanceThreshold` | P-value cutoff | Statistical testing |

## Result Interpretation

### Key Output Tables

#### 1. Cluster Summary (`clusterSummary`)
- **Cluster**: Cluster number
- **Size**: Number of cases
- **Pattern**: Characteristic expression pattern

#### 2. H-Score Analysis (`hscoreTable`)
- **Marker**: IHC marker name
- **H-Score**: Calculated H-score (0-300)
- **Distribution**: Score distribution summary

#### 3. Survival Analysis (`survivalTable`)
- **Comparison**: Group comparison
- **Hazard Ratio**: HR with 95% CI
- **P-value**: Statistical significance

#### 4. Diagnostic Performance (`diagnosticTable`)
- **Sensitivity**: True positive rate
- **Specificity**: True negative rate
- **PPV**: Positive predictive value
- **NPV**: Negative predictive value

#### 5. TIL Signature (`sterlacciTable`)
- **Cluster Type**: TIL signature classification
- **CD4/CD8 Ratio**: T-cell ratio
- **Immune Signature**: Hot/Cold/Intermediate

### Quality Metrics

#### Clustering Quality
- **Silhouette width**: >0.5 = good clustering
- **Gap statistic**: Higher = better separation
- **Elbow method**: Optimal cluster number

#### Reproducibility
- **Cohen's κ**: >0.6 = good reproducibility
- **0.4-0.6**: Moderate agreement
- **<0.4**: Poor agreement

#### Diagnostic Performance
- **Sensitivity/Specificity**: >80% = good performance
- **PPV/NPV**: Context-dependent interpretation
- **AUC**: >0.7 = acceptable diagnostic value

## Clinical Applications by Tumor Type

### Sarcomas (Olsen Methodology)
**Key Markers**: Vimentin, SMA, Desmin, S100, CD34, MDM2, CDK4
**Purpose**: Differential diagnosis and subtyping
**Workflow**: Differential diagnosis clustering

### Lung Cancer (Sterlacci Methodology)  
**Key Markers**: CD3, CD4, CD8, Granzyme B, PD-1, PD-L1
**Purpose**: TIL quantification and immunotherapy prediction
**Workflow**: TIL signature analysis

### Colorectal Cancer (Matsuoka Methodology)
**Key Markers**: p53, Ki-67, VEGF, COX-2
**Purpose**: Prognostic stratification
**Workflow**: Prognostic clustering with survival

### Renal Tumors (Carvalho Methodology)
**Key Markers**: CD117, Vimentin, E-cadherin, CK7
**Purpose**: Oncocytoma vs. carcinoma distinction
**Workflow**: Iterative marker selection

## Troubleshooting Guide

### Common Issues

#### 1. Factor Coding Errors
**Problem**: "Invalid factor levels"
**Solution**: 
```r
data$marker <- as.factor(data$marker)
levels(data$marker)  # Check levels
```

#### 2. Missing Survival Data
**Problem**: Survival analysis fails
**Solution**: Check for NA values
```r
summary(data[c("survival_time", "event")])
data <- data[complete.cases(data[c("survival_time", "event")]), ]
```

#### 3. Poor Clustering Quality
**Problem**: Low silhouette values
**Solutions**:
- Try different distance metrics
- Adjust number of clusters
- Check data quality
- Consider data transformation

#### 4. Small Cluster Sizes
**Problem**: Unbalanced clusters
**Solutions**:
- Reduce number of clusters
- Check for outliers
- Consider different clustering method

### Performance Tips

#### Data Size Recommendations
- **Optimal**: 50-500 cases
- **Minimum**: 30 cases
- **Maximum**: 1000 cases (may be slow)

#### Marker Selection
- **Recommended**: 5-15 markers
- **Minimum**: 3 markers
- **Maximum**: 30 markers

#### Memory Considerations
- Large datasets: Use focused marker panels
- Multiple analyses: Clear environment between runs
- Visualization: May require additional memory

## Best Practices

### 1. Data Preparation
```r
# Check data structure
str(data)

# Verify factor coding
sapply(data[markers], class)

# Handle missing values
data <- data[complete.cases(data[markers]), ]
```

### 2. Progressive Analysis
```r
# Start simple
basic <- ihcstats(data, markers, nClusters = 3)

# Add complexity gradually
advanced <- ihcstats(data, markers, 
                    prognosticClustering = TRUE,
                    showDendrogram = TRUE)
```

### 3. Validation Strategy
```r
# Cross-validation
set.seed(123)
train_idx <- sample(nrow(data), 0.7 * nrow(data))
train_data <- data[train_idx, ]
test_data <- data[-train_idx, ]

# Train model
model <- ihcstats(train_data, markers)

# Validate on test set
# (Manual validation workflow)
```

### 4. Result Documentation
```r
# Save results
saveRDS(result, "ihc_analysis_results.rds")

# Export tables
write.csv(result$clusterSummary$asDF, "cluster_summary.csv")
write.csv(result$survivalTable$asDF, "survival_analysis.csv")
```

## Literature References

1. **Matsuoka et al. (2011)** - Ward's hierarchical clustering for colorectal cancer prognosis
2. **Carvalho et al. (2011)** - Iterative marker selection for renal oncocytoma diagnosis
3. **Olsen et al. (2006)** - Differential diagnosis clustering for sarcomas using hierarchical cluster analysis
4. **Sterlacci et al. (2019)** - NSCLC clustering with tumor-infiltrating lymphocyte signature analysis

## Support

For additional help:
- Comprehensive vignette: `vignette("ihcstats_comprehensive_guide")`
- Package documentation: `?ihcstats`
- Example datasets: `data(package = "ClinicoPath")`
- GitHub issues: https://github.com/sbalci/ClinicoPathJamoviModule/issues

---
*ClinicoPath Development Team | Last updated: `r Sys.Date()`*