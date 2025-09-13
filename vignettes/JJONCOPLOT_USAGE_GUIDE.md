# JJOncoplot Usage Guide

## Overview
**JJOncoplot** is a comprehensive jamovi function for creating genomic landscape visualizations (oncoplots) with advanced features inspired by the ggoncoplot R package, while avoiding upstream package conflicts.

## ✅ Minimum Requirements

### **REQUIRED**: Sample ID Variable
- **Purpose**: Unique identifier for each sample/patient
- **Data Type**: Text, Factor, or ID variable
- **Examples**: 
  - Patient IDs: `"TCGA-01"`, `"P001"`, `"Sample_A"`
  - Sample Names: `"Tumor_1"`, `"Biopsy_001"`
- **Error Message**: *"Please select a Sample ID Variable to identify each sample uniquely"*

### **REQUIRED**: At least one Gene Variable
- **Purpose**: Binary mutation status indicators
- **Data Type**: Numeric (0/1) or Factor
- **Format**: 
  - `0` = Wild-type (no mutation)
  - `1` = Mutated
- **Examples**: `TP53`, `KRAS`, `PIK3CA`, `EGFR`
- **Error Message**: *"Please select at least one Gene Variable representing mutation status"*

## 📊 Recommended Data Structure

### Example CSV Format:
```csv
SampleID,TP53,KRAS,PIK3CA,PTEN,Age,Stage,MutationType
TCGA-01,1,0,1,0,65,II,SNV
TCGA-02,0,1,0,1,58,III,CNV
TCGA-03,1,1,0,0,72,I,SNV
```

### Variable Assignments:
- **Sample ID Variable**: `SampleID`
- **Gene Variables**: `TP53`, `KRAS`, `PIK3CA`, `PTEN`
- **Clinical Variables** (optional): `Age`, `Stage`
- **Mutation Type Variable** (optional): `MutationType`

## 🎯 Step-by-Step Setup in jamovi

### 1. Load Your Data
- Import CSV file with mutation and clinical data
- Ensure Sample ID column contains unique identifiers
- Verify gene columns use 0/1 coding

### 2. Configure Variables
1. **Sample ID Variable**: Select unique sample identifier column
2. **Gene Variables**: Select all mutation status columns (0/1 format)
3. **Clinical Variables** (optional): Select clinical/demographic variables
4. **Mutation Type Variable** (optional): Select mutation classification column

### 3. Choose Plot Options
- **Plot Type**: 
  - `Classic Oncoplot` (default) - Matrix view of mutations
  - `Gene Frequency Plot` - Bar chart of mutation frequencies
  - `Co-occurrence Plot` - Correlation heatmap
- **Sort Samples By**: `Hierarchical (ggoncoplot)` (recommended)
- **Top N Genes**: 8-10 genes (default: 10)

### 4. Advanced Options
- **Genes to Include**: Specific genes (comma-separated): `"TP53,KRAS,PIK3CA"`
- **Genes to Ignore**: Exclude genes: `"RB1,MYC"`
- **Show Gene Frequency**: ✅ (recommended)
- **Show TMB**: ✅ (Tumor Mutation Burden)
- **Color Scheme**: `Clinical`, `Custom`, or `Default`

## 🧬 Data Quality Checklist

### ✅ Sample ID Variable
- [ ] Contains unique values for each row
- [ ] No missing/empty values
- [ ] Consistent naming format

### ✅ Gene Variables
- [ ] Uses 0/1 coding (0=wild-type, 1=mutated)
- [ ] Numeric or logical data type
- [ ] At least one gene with some mutations (not all zeros)

### ✅ Optional Variables
- [ ] Clinical variables have meaningful categories
- [ ] Mutation type variable uses consistent terms (SNV, CNV, Fusion, etc.)
- [ ] No excessive missing values (>50%)

## 🚨 Common Error Messages & Solutions

### Error: *"Please select a Sample ID Variable..."*
**Solution**: Select a column containing unique sample identifiers in the "Sample ID Variable" field.

### Error: *"Please select at least one Gene Variable..."*
**Solution**: Select one or more columns representing mutation status in the "Gene Variables" field.

### Error: *"The following variables are not found..."*
**Solution**: Check that selected variables exist in your dataset and are spelled correctly.

### Error: *"At least 2 samples are required..."*
**Solution**: Your dataset needs at least 2 rows (samples) for oncoplot creation.

### Error: *"'setData' does not exist in this results element"*
**Solution**: This is now fixed in the latest version. Update your module if you encounter this.

## 🎨 Visualization Features

### Hierarchical Sorting (ggoncoplot style)
- Uses base-2 exponential weighting algorithm
- Samples with mutations in frequently mutated genes appear leftmost
- Most authentic oncoplot ordering method

### Mutation Type Support
- Color-code different mutation types (SNV, CNV, Fusion, Indel)
- Enhanced visualization of mutation spectrum
- Clinical interpretation support

### Marginal Plots
- **Gene Frequency**: Bar plots showing mutation rates per gene
- **TMB (Tumor Mutation Burden)**: Bar plots showing total mutations per sample

### Clinical Integration
- Overlay clinical variables as heatmap annotations
- Sort samples by clinical characteristics
- Comprehensive clinical summaries

## 📈 Example Analysis Workflow

1. **Load test data**: Use `jjoncoplot_test_data.csv` from the data folder
2. **Basic setup**:
   - Sample ID Variable: `SampleID`
   - Gene Variables: Select all 15 genes
   - Sort By: `Hierarchical (ggoncoplot)`
3. **Advanced analysis**:
   - Add `MutationType` as Mutation Type Variable
   - Include clinical variables: `Age`, `Stage`, `Response`
   - Enable TMB and gene frequency plots
4. **Interpretation**:
   - Identify most frequently mutated genes
   - Observe mutation co-occurrence patterns
   - Correlate with clinical characteristics

## 🔬 Advanced Tips

### Gene Selection Strategies
- **Top N approach**: Let algorithm select most frequent genes
- **Targeted approach**: Use "Genes to Include" for specific gene panels
- **Pathway approach**: Focus on specific biological pathways

### Clinical Integration Best Practices
- Include key prognostic variables (stage, grade, response)
- Consider survival variables for correlation analysis  
- Use consistent clinical data coding

### Performance Optimization
- Limit to most relevant genes for large datasets
- Use small test dataset first to verify setup
- Consider sample size limits for visualization clarity

---
*Ready to create publication-quality oncoplots with advanced genomic visualization features!* 🎉