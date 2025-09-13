# 🔬 Testing the Enhanced Agreement Function with Usubutun Dataset

This guide shows how to reproduce the diagnostic style clustering analysis from **Usubutun et al. (2012)** using our enhanced `agreement` function in jamovi.

## 📊 Dataset Description

### **Original Study Context**
- **Paper**: "Reproducibility of endometrial intraepithelial neoplasia diagnosis is good, but influenced by the diagnostic style of pathologists" (Modern Pathology 2012)
- **Methodology**: Diagnostic style clustering using Ward's linkage hierarchical clustering
- **Participants**: 20 independent pathologists from Turkish institutions
- **Cases**: 62 endometrial biopsies with expert consensus reference standard

### **Test Dataset Characteristics**
- **File**: `usubutun_realistic_data.csv`
- **Cases**: 62 rows (matching original study)
- **Raters**: 20 pathologist reviewers (columns A-T)
- **Categories**: 3 diagnostic categories (Benign, EIN, Cancer)
- **Reference Standard**: Expert consensus column

### **Expected Diagnostic Style Groups**
Based on the original study, pathologists should cluster into **3 style groups**:

1. **Red Group** (n=5): Reviewers A, E, F, K, N 
   - **Style**: More likely to diagnose EIN
   - **Characteristic**: Aggressive diagnostic approach, aligns with expert reference

2. **Yellow Group** (n=11): Reviewers B, C, D, G, H, I, J, L, P, Q, T
   - **Style**: Balanced spectrum of diagnoses
   - **Characteristic**: Majority cluster, moderate approach

3. **Green Group** (n=4): Reviewers M, O, R, S
   - **Style**: Conservative, favors benign diagnoses  
   - **Characteristic**: Under-diagnoses EIN relative to reference

### **Key Discordant Cases**
The following cases should distinguish diagnostic styles:

- **Cases 6, 25, 44, 51, 61**: Red group diagnoses as EIN, others tend toward benign
- **Cases 11, 14, 16, 26, 38**: Green group diagnoses as benign, others as EIN

## 🚀 How to Test in Jamovi

### **Step 1: Load Data**
1. Open jamovi
2. Import `usubutun_realistic_data.csv`
3. Ensure all rater columns (Reviewer_A through Reviewer_T) are set as **Nominal** variables

### **Step 2: Configure Enhanced Agreement Analysis**
1. Go to **Analyses > ClinicoPath > Agreement > Interrater Reliability**
2. Move all reviewer columns (A-T) to **Raters/Observers** box

### **Step 3: Enable Diagnostic Style Clustering**
In the analysis options, configure:

#### **Diagnostic Style Analysis**
```yaml
☑️ Enable Diagnostic Style Clustering (Usubutun Method)
   Clustering Method: Ward's Linkage
   Distance Metric: Percentage Agreement  
   Number of Style Groups: 3
   ☑️ Include Rater Characteristics
   ☑️ Identify Cases Distinguishing Styles
```

#### **Additional Enhanced Features** (Optional)
```yaml
☑️ Gwet's Agreement Coefficients
☑️ PABAK Analysis
☑️ Rater Bias Detection
☑️ Case Difficulty Scoring
☑️ Agreement Stability Analysis
```

#### **Visualization Options**
```yaml
☑️ Agreement Heatmap
   ☑️ Show Detailed Heatmap
   Color Theme: Red-Yellow-Green
☑️ Diagnostic Style Dendrogram
☑️ Diagnostic Style Heatmap
```

#### **Educational Options**
```yaml
☑️ Show Inline Statistical Comments
☑️ Clinical Summary
☑️ About This Analysis
☑️ Enhanced Error Guidance
```

### **Step 4: Expected Results**

#### **Main Agreement Statistics**
- **Overall Kappa**: ~0.58 (matching original study)
- **Agreement %**: ~72% (similar to original 79%)
- **Fleiss' Kappa**: Substantial agreement across 20 raters

#### **Diagnostic Style Clustering Results**
You should see **3 distinct clusters** in the dendrogram:

1. **Conservative Cluster** (Green): Reviewers M, O, R, S
2. **Balanced Cluster** (Yellow): Reviewers B, C, D, G, H, I, J, L, P, Q, T  
3. **Aggressive Cluster** (Red): Reviewers A, E, F, K, N

#### **Style Summary Table**
| Style Group | Members | Avg Agreement % | Predominant Tendency |
|-------------|---------|-----------------|---------------------|
| Conservative | 4 | ~68% | Under-diagnoses EIN |
| Balanced | 11 | ~75% | Moderate approach |
| Aggressive | 5 | ~82% | Over-diagnoses EIN |

#### **Discordant Cases Table**
Should identify **cases 6, 11, 14, 16, 25, 26, 38, 44, 51, 61** as distinguishing different diagnostic styles.

#### **Case Difficulty Analysis**
- **High Difficulty Cases**: 6, 11, 14, 16, 25, 26, 38, 44, 51, 61
- **Easy Cases**: 1-5, 7-10, 12, 18, 20, 22, 24, 27, 29, etc.
- **Cancer Cases**: 54-62 (perfect agreement expected)

## 🎯 Validation Checklist

### ✅ **Core Functionality**
- [ ] Dendrogram shows 3 distinct clusters
- [ ] Heatmap displays color-coded agreement patterns
- [ ] Style groups match expected composition
- [ ] Discordant cases correctly identified

### ✅ **Advanced Features**  
- [ ] Gwet's AC coefficients > standard kappa (due to prevalence)
- [ ] PABAK analysis shows prevalence effects
- [ ] Rater bias analysis identifies systematic tendencies
- [ ] Case difficulty scores correlate with disagreement

### ✅ **Visualization Quality**
- [ ] Dendrogram clearly shows Ward's linkage clustering
- [ ] Heatmap uses intuitive color coding
- [ ] Style groups visually distinct in all plots
- [ ] Publication-quality figure output

### ✅ **Clinical Interpretation**
- [ ] Inline comments explain Usubutun method
- [ ] Clinical summary provides practical insights
- [ ] Recommendations align with pathology practice
- [ ] Educational content enhances understanding

## 📈 Expected Performance Metrics

### **Agreement Statistics** (Should Match Original Study)
- **Fleiss' Kappa**: 0.58 ± 0.05
- **Overall Agreement**: 72-79%
- **Weighted Kappa** (vs Reference): 0.45-0.84 range
- **Spearman Correlation**: 0.37-0.93 range

### **Style Group Statistics**
- **Within-Group Agreement**: Conservative < Balanced < Aggressive
- **Between-Group Disagreement**: Highest for discordant cases
- **Cluster Stability**: Bootstrap confidence > 80%

## 🔧 Troubleshooting

### **If Clustering Doesn't Work**
1. Ensure all variables are **Nominal** (not Ordinal)
2. Check that all 20 raters are included
3. Verify **Ward's linkage** method selected
4. Confirm **3 style groups** specified

### **If Results Don't Match Expected**
1. Check data import (should be 62 rows × 21 columns)
2. Verify **Percentage Agreement** distance metric
3. Ensure **Usubutun method** is enabled
4. Check for missing data patterns

### **Performance Issues**
1. Disable bootstrap options if analysis is slow
2. Use **Show Progress Indicators** to monitor
3. Try with fewer raters first (10-15) to test

## 📚 Research Applications

This enhanced agreement function with Usubutun method enables:

### **Clinical Applications**
- **Pathologist Training**: Identify diagnostic style tendencies
- **Quality Assurance**: Monitor systematic biases
- **Consensus Development**: Focus on problematic cases
- **Interlaboratory Studies**: Compare diagnostic approaches

### **Research Applications**
- **Reproducibility Studies**: Assess diagnostic consistency
- **Method Validation**: Compare agreement measures
- **Educational Research**: Study learning patterns
- **Clinical Decision Analysis**: Understand diagnostic variability

## 🎉 Success Criteria

Your test is **successful** if you can:
1. ✅ Generate a dendrogram matching the original figure
2. ✅ Identify the same 3 diagnostic style groups
3. ✅ Detect the same discordant cases
4. ✅ Produce publication-quality visualizations
5. ✅ Generate clinically meaningful interpretations

This validates that our enhanced `agreement` function successfully implements the **Usubutun diagnostic style clustering methodology** with modern statistical rigor and user-friendly interface!

---

**📞 Need Help?**
- Check the inline statistical comments for guidance
- Review the "About This Analysis" section for methodology details
- Use enhanced error guidance for troubleshooting tips
- Refer to the original Usubutun et al. (2012) paper for context