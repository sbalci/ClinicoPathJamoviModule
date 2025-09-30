# 🚀 Quick Start: Testing ihccluster with Realistic Data

## ⚡ 5-Minute Setup

### Step 1: Open Data in jamovi (30 seconds)

```
1. Launch jamovi
2. File → Open
3. Navigate to: ClinicoPathJamoviModule/data/
4. Open: ihc_breast_cancer.csv
5. Verify: 155 rows loaded
```

### Step 2: Run First Clustering (2 minutes)

```
1. Analyses → OncoPathT → IHC Clustering Analysis
2. Configure:
   ✓ Categorical IHC Markers:
     - ER_Status
     - PR_Status
     - HER2_IHC

   ✓ Continuous IHC Markers:
     - Ki67_Percent

   ✓ Clustering Method: PAM (k-medoids)
   ✓ Automatically select optimal k: YES
   ✓ Show Silhouette Plot: YES
   ✓ Show Heatmap: YES

3. Click anywhere outside options panel to run
```

### Step 3: Verify Results (1 minute)

**Expected outputs:**
- ✅ 3-4 clusters identified
- ✅ Silhouette plot shows cluster quality
- ✅ Heatmap shows marker patterns
- ✅ Cluster sizes table populated
- ✅ Summary text displays method and k

### Step 4: Test NEW Features (1.5 minutes)

**Enable new panels:**
```
In Language section:
✓ Show Plain-Language Summary
✓ Show Analysis Warnings
✓ Show Statistical Glossary
```

**Expected NEW outputs:**
- ✅ Plain-language summary panel (green box)
- ✅ Warnings panel if issues detected (yellow box)
- ✅ Glossary panel with term definitions

---

## 🎯 Test Scenarios (Copy-Paste Ready)

### Scenario A: Basic (Expected: 4 clusters)

| Setting | Value |
|---------|-------|
| Categorical | ER_Status, PR_Status, HER2_IHC |
| Continuous | Ki67_Percent |
| Method | PAM |
| Auto-select k | Yes |

### Scenario B: Comprehensive (Expected: All features)

| Setting | Value |
|---------|-------|
| Categorical | ER_Status, PR_Status, HER2_IHC, CK5_6, EGFR |
| Continuous | Ki67_Percent, AR_Hscore, p53_Percent |
| Case ID | PatientID |
| Method | PAM |
| Clusters | 4 |
| Clinical Vars | Age_Years, Tumor_Grade, Tumor_Stage |
| Survival Time | OS_Months |
| Survival Event | OS_Event |
| **NEW** Natural Summary | Yes |
| **NEW** Warnings | Yes |
| **NEW** Glossary | Yes |

### Scenario C: Edge Case (Expected: Warnings)

| Setting | Value |
|---------|-------|
| Categorical | ER_Status, PR_Status |
| Continuous | Ki67_Percent |
| Method | PAM |
| Clusters | **8** ← Too many! |
| **NEW** Warnings | Yes |

**Expected**: Overfitting warning appears

### Scenario D: Turkish Interface

| Setting | Value |
|---------|-------|
| Categorical | ER_Status, PR_Status, HER2_IHC |
| Continuous | Ki67_Percent |
| Language | **Türkçe** |
| **NEW** Natural Summary | Yes |
| **NEW** Glossary | Yes |

**Expected**: All text in Turkish

---

## ✅ Success Indicators

### Visual Checks

| Element | What to Look For |
|---------|------------------|
| **Silhouette Plot** | Bars mostly positive, average >0.5 |
| **Heatmap** | Clear color blocks by cluster |
| **Cluster Sizes** | 4 groups with 15-70 cases each |
| **Dendrogram** (if hierarchical) | Clear 4-branch structure |

### NEW Features Working

| Feature | Success Indicator |
|---------|-------------------|
| **Natural Summary** | Green panel with plain-language text |
| **Warnings** | Yellow panel appears for k=8 scenario |
| **Glossary** | Gray panel with term definitions |
| **Turkish** | All UI and outputs in Turkish |

### Table Checks

| Table | What to See |
|-------|-------------|
| **Cluster Sizes** | 4 rows with counts and % |
| **Marker Summary** | Statistics differ by cluster |
| **Association Tests** | P-values <0.05 for ER, PR, HER2, Ki67 |
| **Medoid Info** | Patient IDs like BC-0042 |

---

## 🐛 Quick Troubleshooting

### Problem: "No clusters formed"

**Solution**: Check missing data handling
```
Data Preprocessing → Missing Data Method → Pairwise distances
```

### Problem: "Warnings everywhere"

**This is normal!** The dataset includes edge cases to test the warning system. Try:
```
Number of Clusters: 3 or 4 (instead of 8)
```

### Problem: "Results look different each time"

**Solution**: Set random seed
```
Advanced Options → Random Seed: 42
```

### Problem: "Heatmap error"

**Solution**: Install ComplexHeatmap package
```r
# In R console:
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("ComplexHeatmap")
```

---

## 📊 Understanding Results

### Interpreting Clusters

Your 4 clusters likely represent:

| Cluster | IHC Profile | Clinical Group |
|---------|-------------|----------------|
| **C1** | ER+/PR+/HER2-/low Ki67 | Luminal A (~60 cases) |
| **C2** | ER+/PR+/high Ki67 | Luminal B (~40 cases) |
| **C3** | ER-/PR-/HER2 2+/3+ | HER2-enriched (~25 cases) |
| **C4** | ER-/PR-/HER2- | Triple Negative (~25 cases) |

### Silhouette Values

| Range | Interpretation |
|-------|----------------|
| **0.7 - 1.0** | Strong, well-separated clusters ✅ |
| **0.5 - 0.7** | Reasonable structure ✅ |
| **0.3 - 0.5** | Weak clusters ⚠️ |
| **< 0.3** | No substantial structure ❌ |

### NEW: Natural Language Summary

**What it tells you:**
- How many patients in each cluster (in simple terms)
- What the clustering method did
- Clinical interpretation
- What to do next

**Example output:**
> "This analysis grouped 150 patients into 4 distinct clusters based on 4 IHC markers (3 categorical, 1 continuous). Cluster C1: 58 patients (38%), Cluster C2: 42 patients (28%), Cluster C3: 26 patients (17%), Cluster C4: 24 patients (16%). These clusters may represent distinct tumor subtypes or different treatment response patterns."

### NEW: Contextual Warnings

**Common warnings:**
- **Small clusters** (<5 cases): Results may be unstable
- **Imbalanced clusters**: Consider different k
- **Overfitting** (k > n/10): Too many clusters for sample size
- **Low silhouette** (<0.5): Clusters weakly separated

### NEW: Statistical Glossary

**Quick reference for terms like:**
- Gower Distance
- Silhouette Width
- PAM (k-medoids)
- Hierarchical Clustering
- Cramér's V
- Epsilon-squared
- H-score

---

## 🎓 Next Steps

### 1. Compare Methods (15 minutes)

Run same markers with:
- PAM (robust)
- Hierarchical (shows relationships)
- Dimension Reduction (handles many markers)

Compare results - should see similar 4-cluster structure.

### 2. Add Clinical Data (10 minutes)

```
Clinical Variables: Age_Years, Tumor_Grade, Tumor_Stage
```

Check if clusters differ by clinical characteristics.

### 3. Survival Analysis (10 minutes)

```
Survival Time: OS_Months
Survival Event: OS_Event
```

See if clusters have different prognosis.

### 4. Try Consensus Clustering (20 minutes)

```
Advanced Options → Consensus Clustering: Yes
Bootstrap Iterations: 100
```

Assess cluster stability.

### 5. Export Results (5 minutes)

```
Output Tables → Export Cluster Assignments: Yes
```

Then: Data → Export → Save with cluster assignments.

---

## 📖 Full Documentation

- **Comprehensive guide**: `data/README_IHC_TEST_DATA.md`
- **Test scenarios**: `data-raw/test_ihccluster_features.R`
- **Data generation**: `data-raw/generate_ihc_test_data.R`
- **Codebook**: `data/ihc_breast_cancer_codebook.txt`

---

## ✨ NEW Features Checklist

Test all new enhancements:

- [ ] Natural language summary displays (plain-language panel)
- [ ] Warnings appear for k=8 (overfitting)
- [ ] Glossary shows term definitions
- [ ] Turkish translation works (all panels)
- [ ] Patient IDs display in medoid table
- [ ] Export notification appears
- [ ] Color palettes apply correctly
- [ ] All features work together in comprehensive test

---

**Ready to test?** Start with Step 1 above! 🚀

**Questions?** See full docs: `data/README_IHC_TEST_DATA.md`

**Version**: 1.0 | **Date**: 2024-01-15