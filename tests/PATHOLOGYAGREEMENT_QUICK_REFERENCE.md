# Pathology Agreement Test Data - Quick Reference Card

## 📦 What Was Created

### ✅ Test Data Files (6 datasets)
```
data/
├── pathology_agreement_main.csv          (250 cases - common scenarios)
├── pathology_agreement_edge.csv          (188 cases - edge cases)
├── pathology_agreement_multimethod.csv   (100 cases - 4 platforms)
├── pathology_agreement_ai.csv            (70 cases - AI validation)
├── pathology_agreement_multisite.csv     (90 cases - multisite)
└── pathology_agreement_missing.csv       (50 cases - missing data)
```

### ✅ Documentation Files
```
data-raw/
└── generate_pathologyagreement_test_data.R   (data generation script)

tests/
├── PATHOLOGYAGREEMENT_TEST_GUIDE.md          (comprehensive testing guide)
├── PATHOLOGYAGREEMENT_TEST_SUMMARY.md        (detailed summary)
├── PATHOLOGYAGREEMENT_QUICK_REFERENCE.md     (this file)
└── testthat/test-pathologyagreement.R        (automated tests)
```

---

## 🚀 Quick Start (30 seconds)

### Test 1: Excellent Agreement
1. Open jamovi
2. Load: `data/pathology_agreement_main.csv`
3. Analysis → OncoPathT → Agreement → Pathology Agreement
4. Select:
   - Method 1: `Ki67_HALO`
   - Method 2: `Ki67_Aiforia`
5. ✅ Expected: ICC = 0.997, r = 0.990

### Test 2: Multi-Method (4 platforms)
1. Load: `data/pathology_agreement_multimethod.csv`
2. Select:
   - Method 1: `Ki67_HALO`
   - Method 2: `Ki67_Aiforia`
   - Additional: `Ki67_ImageJ`, `Ki67_Manual`
3. ✅ Expected: Correlation matrix + Overall ICC tables appear

### Test 3: Small Sample Warning
1. Load: `data/pathology_agreement_edge.csv`
2. Filter rows where Scenario = "Small Sample"
3. Select: `Method1`, `Method2`
4. ✅ Expected: Warning about small sample (n=8)

---

## 📊 Dataset Cheat Sheet

| Load this file... | To test this feature... | Use these variables... |
|-------------------|-------------------------|------------------------|
| `main.csv` | Basic agreement | Ki67_HALO + Ki67_Aiforia |
| `main.csv` | Systematic bias | Ki67_PlatformA + Ki67_PlatformB |
| `edge.csv` | Perfect agreement | Ki67_HALO + Ki67_Aiforia (Scenario="Perfect") |
| `edge.csv` | Poor agreement | Ki67_Platform1 + Ki67_Platform2 |
| `edge.csv` | Small sample warning | Method1 + Method2 (Scenario="Small Sample") |
| `edge.csv` | Range validation | ER_Percent_HALO + ER_Percent_Manual |
| `multimethod.csv` | 4-platform comparison | All 4 Ki67 variables |
| `ai.csv` | AI validation preset | Ki67_AI + Ki67_Pathologist |
| `multisite.csv` | Multisite preset | Ki67_SiteA + Ki67_SiteB |
| `missing.csv` | Missing data handling | HALO_Score + Aiforia_Score |

---

## 🎯 Expected Results Cheat Sheet

| Scenario | ICC(3,1) | Spearman r | Bias | Interpretation |
|----------|----------|------------|------|----------------|
| Perfect | ~1.000 | >0.99 | ~0% | Excellent |
| Excellent | 0.997 | 0.990 | -0.6% | Excellent |
| Good | 0.991 | 0.968 | -1.0% | Good |
| Moderate | 0.961 | 0.943 | 4.4% | Moderate |
| Systematic Bias | 0.998 | 0.994 | **-8.0%** | High r, significant bias |
| Proportional | 0.991 | 0.988 | -7.3% | Warning: bias varies |
| Poor | <0.50 | <0.60 | Large | Poor |

---

## ⚙️ Settings to Test

### Clinical Presets
- [ ] General Agreement Analysis (default)
- [ ] Biomarker Platform Comparison → use `main.csv` excellent scenario
- [ ] AI vs Pathologist → use `ai.csv`
- [ ] Multisite Validation → use `multisite.csv`

### ICC Types
- [ ] Consistency (default) → recommended for biomarkers
- [ ] Absolute Agreement → for exact concordance

### Correlation Methods
- [ ] Both (default)
- [ ] Spearman only
- [ ] Pearson only

### Display Options
- [ ] Show Plots ✓
- [ ] Show Interpretation ✓
- [ ] Show Plain Language Summary
- [ ] Show Educational Explanations

---

## 🔍 What to Look For

### ✅ Tables Should Appear
1. **Agreement Metrics Table** (always)
   - ICC row
   - CCC row
   - Bland-Altman bias
   - Limits of agreement (2 rows)

2. **Correlation Table** (always)
   - Spearman row (if enabled)
   - Pearson row (if enabled)

3. **Correlation Matrix** (only if 3+ methods)
4. **Overall ICC Table** (only if 3+ methods)

### ✅ Plots Should Appear
1. **Scatter Plot**
   - Blue line = linear fit
   - Red dashed = perfect agreement (y=x)

2. **Bland-Altman Plot**
   - Black line = mean difference
   - Red dashed lines = limits of agreement
   - Blue line = regression (check for proportional bias)

### ✅ Notices/Warnings to Verify

**ERROR (stops analysis):**
- Missing variables
- Empty data
- n < 3

**STRONG WARNING:**
- Very small sample (n<10)
- Negative ICC

**WARNING:**
- Sample size 10-29
- Out-of-range biomarkers (>100%)
- Low bootstrap for high-stakes
- Normality violation
- Proportional bias detected

**INFO:**
- Missing data removed count
- Analysis complete summary

---

## 🔧 Regenerate Data

If you need to regenerate test data:

```r
source("data-raw/generate_pathologyagreement_test_data.R")
```

This will create fresh data with the same statistics (seed=123).

---

## 📖 Full Documentation

For comprehensive testing instructions:
- **Full Guide:** `tests/PATHOLOGYAGREEMENT_TEST_GUIDE.md`
- **Summary:** `tests/PATHOLOGYAGREEMENT_TEST_SUMMARY.md`

For automated testing:
- **Test File:** `tests/testthat/test-pathologyagreement.R`
- **Run:** `devtools::test()`

---

## 🐛 Troubleshooting

| Problem | Solution |
|---------|----------|
| "psych package not installed" | `install.packages("psych")` |
| "epiR package not installed" | `install.packages("epiR")` |
| Files not found | Run data generation script |
| Different statistics | Data regenerated; update expected values |
| Plots don't show | Enable "Show Agreement Plots" |

---

## ✨ Pro Tips

1. **Filter by Scenario** in jamovi to isolate specific test cases
2. **Start with main.csv** for basic functionality testing
3. **Use edge.csv** to verify warning system works
4. **Test multi-method** last (most complex)
5. **Compare your results** with expected values in summary docs

---

## 📞 Need Help?

- File issues: https://github.com/sbalci/ClinicoPathJamoviModule/issues
- Function name: `pathologyagreement`
- Module: OncoPathT → Agreement

---

**Created:** 2024-12-28 | **Version:** 1.0.0
