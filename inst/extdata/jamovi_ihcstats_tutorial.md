# jamovi IHC Expression Analysis Tutorial

## Using the ihcstats Function in jamovi

This tutorial demonstrates how to use the IHC Expression Analysis function in jamovi, implementing methodologies from four landmark research papers for comprehensive immunohistochemical data analysis.

## Getting Started

### 1. Loading Data in jamovi

1. **Open jamovi**
2. **File → Open** and select one of the example datasets:
   - `ihc_comprehensive_data.csv` (full dataset, 300 cases)
   - `sarcoma_ihc_data.csv` (sarcoma differential diagnosis)
   - `nsclc_til_data.csv` (lung cancer TIL analysis)
   - `colorectal_ihc_data.csv` (colorectal prognostic analysis)
   - `renal_ihc_data.csv` (renal marker optimization)

### 2. Accessing IHC Expression Analysis

1. **Navigate to**: `Analyses → ExplorationD → IHC Analysis → IHC Expression Analysis`
2. The ihcstats analysis window will open with multiple option sections

## Basic Analysis Workflow

### Step 1: Select IHC Markers

1. **Drag markers** from the variable list to the **"IHC Markers"** box
2. **For sarcoma analysis**: Select Vimentin, SMA, Desmin, S100, CD34
3. **For TIL analysis**: Select CD3, CD4, CD8, Granzyme_B, PD1, PDL1
4. **For prognostic analysis**: Select p53, Ki67, VEGF, COX2

### Step 2: Configure Basic Options

**In "Analysis Options" section:**
- ✅ Check **"Compute H-Score"** to calculate H-scores (0-300 scale)

**In "Clustering Options" section:**
- **Clustering Method**: Choose from:
  - `Hierarchical` (most common)
  - `PAM (Medoids)` (robust to outliers)
  - `K-means` (fast, large datasets)
  - `PCA + K-means` (dimensionality reduction)
- **Distance Metric**: 
  - `Gower` (recommended for mixed data)
  - `Jaccard` (for categorical IHC data)
- **Linkage Method**: 
  - `Average` (balanced)
  - `Complete` (compact clusters)
  - `Ward` (minimum variance)
- **Number of Clusters**: 3-5 (typical range)
- **IHC Scoring Scale**:
  - `Standard` (0, 1+, 2+, 3+)
  - `Binary` (Negative, Positive)
  - `Carvalho` (0, 1, 2)
  - `Matsuoka` (Mild, Moderate, Marked)

## Research Methodology-Specific Analyses

### Matsuoka 2011: Prognostic Clustering

**Use Case**: Colorectal cancer prognosis with survival analysis

**Setup Steps:**
1. **Data**: Load `colorectal_ihc_data.csv`
2. **Markers**: p53, Ki67, VEGF, COX2
3. **In "Clustering Options"**: 
   - Linkage Method: `Ward`
   - IHC Scoring Scale: `Matsuoka`
4. **In "Advanced Options"**:
   - ✅ Check **"Prognostic Clustering"**
5. **In "Survival Analysis"**:
   - **Survival Time Variable**: Overall_Survival_Months
   - **Survival Event Variable**: Death_Event
6. **In "Multi-Region Analysis"** (if available):
   - **Central Region Variable**: p53_Central, Ki67_Central, etc.
   - **Invasive Region Variable**: p53_Invasive, Ki67_Invasive, etc.

**Expected Results:**
- Prognostic Clustering Results table
- Survival Analysis (Cox Regression) table
- Multi-Region Analysis table (if configured)

### Carvalho 2011: Iterative Marker Selection

**Use Case**: Renal oncocytoma diagnosis optimization

**Setup Steps:**
1. **Data**: Load `renal_ihc_data.csv`
2. **Markers**: CD117, Vimentin_Carvalho, E_Cadherin, CK7
3. **In "Clustering Options"**:
   - IHC Scoring Scale: `Carvalho`
4. **In "Advanced Options"**:
   - ✅ Check **"Iterative Clustering"**
   - ✅ Check **"Show Cluster Validation"**
   - ✅ Check **"Perform PCA Analysis"**
   - Optimal K Method: `Silhouette Method`

**Expected Results:**
- Optimal Marker Panel table
- Principal Component Analysis table
- Clustering Quality Assessment table

### Olsen 2006: Differential Diagnosis

**Use Case**: Sarcoma subtype classification

**Setup Steps:**
1. **Data**: Load `sarcoma_ihc_data.csv`
2. **Markers**: Vimentin, SMA, Desmin, S100, CD34, MDM2, CDK4, MyoD1
3. **In "Clustering Options"**:
   - Linkage Method: `Complete`
4. **In "Grouping Options"**:
   - **Grouping Variable**: Tumor_Type
5. **In "Differential Diagnosis (Olsen Method)"**:
   - ✅ Check **"Differential Diagnosis Mode"**
   - **Tumor Type Variable**: Tumor_Type
   - ✅ Check **"Antibody Panel Optimization"**
   - ✅ Check **"Calculate Sensitivity/Specificity"**
   - ✅ Check **"Olsen-style Cluster Heatmap"**
   - **Cluster Cut Height**: 0.5-0.7

**Expected Results:**
- Differential Diagnosis Results table
- Antibody Performance Analysis table
- Optimal Antibody Panel Combinations table

### Sterlacci 2019: TIL Signature Analysis

**Use Case**: NSCLC immune infiltration analysis

**Setup Steps:**
1. **Data**: Load `nsclc_til_data.csv`
2. **Markers**: CD3, CD4, CD8, CD20, Granzyme_B, PD1, PDL1, FOXP3
3. **In "Clustering Options"**:
   - Linkage Method: `Complete`
   - Distance Metric: `Gower`
4. **In "Grouping Options"**:
   - **Grouping Variable**: Histotype
5. **In "Sterlacci TIL Analysis"**:
   - ✅ Check **"Sterlacci TIL Signature Analysis"**
   - ✅ Check **"Supervised Clustering (by Histotype)"**
   - ✅ Check **"Reproducibility Testing (Cohen Kappa)"**
   - ✅ Check **"Focus on Immune Signature Markers"**
   - **TIL Analysis Mode**: `Combined TIL Signature`
   - **Multiple Testing Correction**: `Bonferroni`
   - **Significance Threshold**: 0.05

**Expected Results:**
- Sterlacci TIL Signature Analysis table
- TIL Signature Characterization table
- Supervised Clustering Results table
- Cluster Reproducibility Analysis table

## Visualization Options

### Enable All Visualizations

**In "Visualization Options"**:
- ✅ **Show Dendrogram**: Hierarchical clustering tree
- ✅ **Show Expression Heatmap**: Clustered heatmap of IHC markers
- ✅ **Show PCA Biplot**: Principal component analysis plot
- ✅ **Show Score Distribution**: Distribution of IHC scores
- ✅ **Show Diagnostics**: Diagnostic performance plots

**Tips for Visualizations:**
- **Dendrogram**: Best for understanding cluster relationships
- **Heatmap**: Shows expression patterns clearly
- **PCA Plot**: Good for dimensionality reduction visualization
- **Score Distribution**: Helps assess data quality

## Advanced Configuration

### Multiple Analysis Types in One Run

For comprehensive analysis, you can enable multiple methodologies simultaneously:

1. **Basic Setup**: Select 8-12 core markers
2. **Enable Multiple Methods**:
   - ✅ Prognostic Clustering
   - ✅ Differential Diagnosis
   - ✅ Sterlacci Analysis
   - ✅ Iterative Clustering
3. **Configure Variables**:
   - Survival Time/Event variables
   - Tumor Type variable
   - Grouping variable
4. **Enable All Visualizations**

### Quality Control Settings

**In "Advanced Options"**:
- ✅ **Standardize Data**: For mixed-scale data
- ✅ **Show Cluster Validation**: Quality metrics
- **Optimal K Method**: Choose based on data size
  - `Elbow Method`: Fast, general purpose
  - `Silhouette Method`: More accurate
  - `Gap Statistic`: Best quality, slower

## Interpreting Results

### Key Tables to Review

#### 1. **H-Score Analysis**
- Shows calculated H-scores (0-300) for each marker
- **Interpretation**: Higher scores = stronger expression

#### 2. **Clustering Results**
- Lists clusters with sizes and expression patterns
- **Interpretation**: Look for balanced cluster sizes

#### 3. **Survival Analysis** (if enabled)
- Hazard ratios and p-values for prognostic groups
- **Interpretation**: HR >1 = worse prognosis, p<0.05 = significant

#### 4. **Diagnostic Performance** (if enabled)
- Sensitivity, specificity, PPV, NPV for each marker
- **Interpretation**: >80% = good diagnostic performance

#### 5. **TIL Signature Analysis** (if enabled)
- CD4/CD8 ratios and immune signatures
- **Interpretation**: "Hot" = high immune infiltration

### Quality Metrics

#### **Clustering Quality Assessment**
- **Average Silhouette Width**: >0.5 = good clustering
- **Gap Statistic**: Higher values = better separation

#### **Reproducibility Analysis**
- **Cohen's κ**: >0.6 = good reproducibility
- **Interpretation**: 
  - 0.81-1.00: Almost perfect
  - 0.61-0.80: Substantial
  - 0.41-0.60: Moderate

## Common Workflows

### Workflow 1: Sarcoma Diagnosis
```
Data: sarcoma_ihc_data.csv
Markers: Vimentin, SMA, Desmin, S100, CD34, MDM2
Method: Olsen Differential Diagnosis
Goal: Identify sarcoma subtypes
```

### Workflow 2: Immunotherapy Prediction  
```
Data: nsclc_til_data.csv
Markers: CD3, CD4, CD8, Granzyme_B, PD1, PDL1
Method: Sterlacci TIL Analysis
Goal: Predict immunotherapy response
```

### Workflow 3: Prognostic Stratification
```
Data: colorectal_ihc_data.csv
Markers: p53, Ki67, VEGF, COX2
Method: Matsuoka Prognostic Clustering
Goal: Identify risk groups
```

### Workflow 4: Marker Optimization
```
Data: renal_ihc_data.csv
Markers: CD117, Vimentin_Carvalho, E_Cadherin, CK7
Method: Carvalho Iterative Selection
Goal: Optimize diagnostic panel
```

## Troubleshooting in jamovi

### Common Issues

#### **No Results Appear**
- **Check**: Are IHC markers selected?
- **Solution**: Drag variables to "IHC Markers" box

#### **Error: "Invalid factor levels"**
- **Check**: Variable types in data view
- **Solution**: Ensure IHC markers are categorical/ordinal

#### **Warning: "Small cluster sizes"**
- **Check**: Number of clusters vs. sample size
- **Solution**: Reduce number of clusters or increase sample size

#### **Poor Clustering Quality**
- **Check**: Silhouette values in results
- **Solution**: Try different distance metrics or clustering methods

### Performance Tips

#### **For Large Datasets (>500 cases)**
- Use focused marker panels (5-8 markers)
- Choose K-means clustering
- Disable computationally intensive options

#### **For Small Datasets (<50 cases)**
- Use hierarchical clustering
- Reduce number of clusters (2-3)
- Enable cluster validation

#### **Memory Issues**
- Close other jamovi analyses
- Reduce number of visualizations
- Use smaller datasets for testing

## Export and Reporting

### Saving Results

1. **Copy Tables**: Right-click tables → Copy → paste into reports
2. **Export Plots**: Right-click plots → Save As → choose format
3. **Save Analysis**: File → Save → saves entire jamovi file

### Creating Reports

**Key Elements to Include:**
1. **Methods Section**: 
   - Clustering method used
   - Distance metric
   - Number of clusters
   - Scoring scale
2. **Results Tables**:
   - Cluster summary
   - H-score analysis
   - Survival analysis (if applicable)
3. **Visualizations**:
   - Dendrogram
   - Heatmap
   - Quality metrics
4. **Interpretation**:
   - Clinical significance
   - Diagnostic utility
   - Prognostic value

## Example Reports

### Sarcoma Differential Diagnosis Report

"IHC expression analysis was performed using the Olsen methodology with hierarchical clustering (complete linkage) on a panel of 8 markers (Vimentin, SMA, Desmin, S100, CD34, MDM2, CDK4, MyoD1). The analysis identified 4 distinct clusters corresponding to major sarcoma subtypes. Vimentin showed 95% sensitivity for mesenchymal tumors, while MDM2/CDK4 co-expression was 89% specific for liposarcoma. The diagnostic accuracy of the optimized 6-marker panel was 87% for sarcoma subtype classification."

### TIL Analysis Report

"TIL signature analysis using the Sterlacci methodology revealed 3 distinct immune infiltration patterns in NSCLC. The 'Hot' immune signature (62% of cases) was characterized by high CD8+ TIL count (mean: 34 cells/hpf) and elevated Granzyme B expression. CD4/CD8 ratio was significantly different between clusters (p<0.001). Supervised clustering by histotype showed adenocarcinomas had higher immune infiltration compared to squamous cell carcinomas (p=0.032)."

---

*This tutorial covers the essential features of the ihcstats function in jamovi. For advanced usage and detailed methodology, refer to the comprehensive vignette and research paper references.*