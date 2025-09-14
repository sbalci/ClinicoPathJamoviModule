# Haralick Texture Analysis - Testing Guide

This guide provides comprehensive test datasets for validating the enhanced `haralicktexture` function across different clinical scenarios and edge cases.

## 📁 Generated Test Datasets

### 1. **Ki67 Proliferation Study** (`haralick_ki67_proliferation.csv`)
- **Cases**: 85 patients with breast carcinoma
- **Heterogeneity Level**: High (aggressive tumor patterns)
- **Clinical Context**: Proliferation biomarker analysis
- **Spatial Pattern**: Clustered (tumor regions)
- **Key Variables**: `tumor_grade`, `ki67_percentage`, `survival_months`, `recurrence`

**Testing Scenarios**:
```
Analysis Focus: Prognostic
Biomarker Context: Ki67 Proliferation
Feature Selection: Clinical Priority
Texture Features: entropy, contrast, correlation, energy, homogeneity, variance
Grouping Variable: tumor_grade
Outcome Variable: survival_months or recurrence
Coordinates: x_coord, y_coord
```

**Expected Results**:
- High entropy values (>2.5) should correlate with higher grades
- Spatial clustering should be detected
- Ki67-specific clinical interpretation should appear
- Prognostic correlations with survival should be found

---

### 2. **HER2 Expression Analysis** (`haralick_her2_expression.csv`)
- **Cases**: 62 patients with breast carcinoma
- **Heterogeneity Level**: Moderate
- **Clinical Context**: Therapeutic target assessment
- **Spatial Pattern**: Random distribution
- **Key Variables**: `her2_score`, `treatment_response`

**Testing Scenarios**:
```
Analysis Focus: Correlation
Biomarker Context: HER2 Expression
Feature Selection: Correlation Filter
Correlation Threshold: 0.8
Grouping Variable: her2_score
Outcome Variable: treatment_response
```

**Expected Results**:
- Moderate heterogeneity patterns
- Correlation between texture features and HER2 scores
- HER2-specific therapeutic interpretation
- Treatment response correlation analysis

---

### 3. **PD-L1 Immunotherapy Study** (`haralick_pdl1_immunotherapy.csv`)
- **Cases**: 48 patients with lung adenocarcinoma
- **Heterogeneity Level**: Variable
- **Clinical Context**: Immunotherapy prediction
- **Spatial Pattern**: Gradient distribution
- **Key Variables**: `pdl1_tps`, `pdl1_cps`, `immunotherapy_response`, `tumor_stage`

**Testing Scenarios**:
```
Analysis Focus: Comprehensive
Biomarker Context: PD-L1 Expression
Normality Testing: Yes
Outlier Detection: Yes
Grouping Variable: tumor_stage
Outcome Variable: immunotherapy_response
```

**Expected Results**:
- PD-L1 spatial heterogeneity assessment
- Immunotherapy response predictions
- Comprehensive statistical analysis
- Multiple biomarker score correlations

---

### 4. **CD8+ T-cell Infiltration** (`haralick_cd8_infiltration.csv`)
- **Cases**: 38 patients with colorectal carcinoma
- **Heterogeneity Level**: Low-Moderate
- **Clinical Context**: Immune landscape analysis
- **Spatial Pattern**: Clustered (immune infiltrates)
- **Key Variables**: `cd8_density`, `immune_score`, `tumor_mutation_burden`

**Testing Scenarios**:
```
Analysis Focus: Spatial
Biomarker Context: CD8+ T-cells
Show Spatial Plot: Yes
Coordinates: x_coord, y_coord
Grouping Variable: immune_score
```

**Expected Results**:
- CD8+ immune infiltration patterns
- Spatial clustering detection
- Low-moderate heterogeneity interpretation
- Immune score correlations

---

### 5. **Small Sample Dataset** (`haralick_small_sample.csv`)
- **Cases**: 4 patients (minimal sample)
- **Purpose**: Test warning systems and error handling
- **Heterogeneity Level**: High

**Testing Scenarios**:
```
Analysis Focus: Descriptive
Feature Selection: All
Expected Warnings: Small sample size alerts
```

**Expected Results**:
- ⚠️ Small sample warnings should appear
- Limited statistical analysis
- Cautionary interpretations
- Robust error handling demonstration

---

### 6. **Missing Data Patterns** (`haralick_missing_data.csv`)
- **Cases**: 75 patients with various missing patterns
- **Missing Types**: MCAR (5%), MAR (15%), MNAR (20%)
- **Purpose**: Test robustness with incomplete data

**Testing Scenarios**:
```
Analysis Focus: Comprehensive
All Options: Test robustness
Expected: Missing data warnings and handling
```

**Expected Results**:
- Missing data summary tables
- Impact assessments
- Robust statistical calculations
- Quality control warnings

---

### 7. **High-Dimensional Dataset** (`haralick_high_dimensional.csv`)
- **Cases**: 15 patients
- **Features**: 16 texture measurements
- **Purpose**: Test high feature-to-sample ratio warnings
- **Challenge**: More features than optimal for sample size

**Testing Scenarios**:
```
Feature Selection: Variance Filter or Clinical Priority
Expected: High-dimensional data warnings
Recommendation: Feature reduction suggestions
```

**Expected Results**:
- ⚠️ High-dimensional data warnings
- Feature selection recommendations
- Overfitting prevention alerts
- Automatic feature reduction

---

### 8. **Comprehensive Multi-Biomarker Study** (`haralick_comprehensive_study.csv`)
- **Cases**: 120 patients
- **Biomarkers**: Ki67, HER2, PD-L1, CD8, p53, EGFR
- **Tumor Types**: Breast, Lung, Colorectal, Prostate, Ovarian
- **Purpose**: Full functionality testing

**Testing Scenarios**:
```
Analysis Focus: Comprehensive
Biomarker Context: General (mixed biomarkers)
All Advanced Options: Enabled
Grouping Variable: biomarker_type or tumor_type
Outcome Variable: survival_months or vital_status
```

**Expected Results**:
- Complete statistical analysis suite
- Mixed biomarker interpretations
- Survival analysis correlations
- Full feature demonstration

---

## 🧪 **Systematic Testing Protocol**

### **Phase 1: Basic Functionality**
1. Load `ki67_proliferation` dataset
2. Select core texture features: `entropy`, `contrast`, `correlation`
3. Set Analysis Focus: `Descriptive`
4. Verify basic statistics generation

### **Phase 2: Clinical Context Testing**
1. Load `her2_expression` dataset
2. Set Biomarker Context: `HER2 Expression`
3. Add grouping variable: `her2_score`
4. Verify biomarker-specific interpretation

### **Phase 3: Spatial Analysis Testing**
1. Load `cd8_infiltration` dataset
2. Set Analysis Focus: `Spatial`
3. Add coordinates: `x_coord`, `y_coord`
4. Verify spatial heterogeneity metrics

### **Phase 4: Warning System Testing**
1. Load `small_sample` dataset
2. Attempt comprehensive analysis
3. Verify warning generation
4. Test error handling

### **Phase 5: Performance Testing**
1. Load `comprehensive_study` dataset
2. Select all texture features
3. Enable all analysis options
4. Verify performance optimization

### **Phase 6: Edge Case Testing**
1. Load `missing_data` dataset
2. Test missing data handling
3. Load `high_dimensional` dataset
4. Test feature selection warnings

---

## 📊 **Expected Performance Metrics**

| Dataset | Expected Runtime | Warning Level | Key Features Tested |
|---------|------------------|---------------|-------------------|
| Ki67 Proliferation | <5 seconds | None | Biomarker context, prognostic analysis |
| HER2 Expression | <3 seconds | None | Therapeutic correlation, moderate heterogeneity |
| PD-L1 Immunotherapy | <3 seconds | None | Immunotherapy prediction, comprehensive analysis |
| CD8 Infiltration | <2 seconds | None | Spatial analysis, immune patterns |
| Small Sample | <1 second | ⚠️ Critical | Small sample warnings |
| Missing Data | <4 seconds | ⚠️ Moderate | Missing data handling |
| High Dimensional | <2 seconds | ⚠️ High | Feature selection warnings |
| Comprehensive | <8 seconds | None | Full functionality |

---

## 🔍 **Validation Checklist**

### ✅ **Core Functionality**
- [ ] Basic texture statistics generation
- [ ] Correlation matrix computation
- [ ] Distribution analysis (normality testing)
- [ ] Outlier detection with multiple methods

### ✅ **Clinical Features**
- [ ] Biomarker-specific interpretations (Ki67, HER2, PD-L1, CD8+)
- [ ] Copy-ready clinical summaries
- [ ] Evidence-based thresholds and recommendations

### ✅ **Advanced Analysis**
- [ ] Feature selection (4 methods)
- [ ] Spatial heterogeneity analysis
- [ ] Prognostic modeling correlations
- [ ] Analysis focus switching

### ✅ **Quality Control**
- [ ] Input validation with specific error messages
- [ ] Clinical misuse detection and warnings
- [ ] Missing data impact assessment
- [ ] Sample size adequacy checks

### ✅ **User Experience**
- [ ] Plain-language tooltips and descriptions
- [ ] About panel with comprehensive guidance
- [ ] Collapsible UI sections for organization
- [ ] Performance optimization for large datasets

---

## 🚀 **Quick Start Testing Commands**

```r
# Load jamovi and test basic functionality
library(jmvcore)

# Test 1: Basic Ki67 analysis
data_ki67 <- read.csv("test_data/haralick_ki67_proliferation.csv")
# Run haralicktexture with texture features, Ki67 context

# Test 2: HER2 correlation analysis  
data_her2 <- read.csv("test_data/haralick_her2_expression.csv")
# Run with correlation focus, HER2 context

# Test 3: Spatial analysis
data_cd8 <- read.csv("test_data/haralick_cd8_infiltration.csv") 
# Run with spatial focus, coordinates

# Test 4: Warning generation
data_small <- read.csv("test_data/haralick_small_sample.csv")
# Run comprehensive analysis, expect warnings
```

This comprehensive testing suite ensures the enhanced `haralicktexture` function works correctly across all clinical scenarios, handles edge cases gracefully, and provides meaningful clinical interpretations for digital pathology research.