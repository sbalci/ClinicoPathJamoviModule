# Chi-Square Post-Hoc Test Data Files

This folder contains comprehensive test datasets for the `chisqposttest` module.

## Quick Reference

| File | N | Type | Primary Use |
|------|---|------|-------------|
| `chisqposttest_comprehensive.csv` | 400 | Individual obs | Main testing - all features |
| `chisqposttest_weighted.csv` | 24 | Frequency table | Counts variable testing |
| `chisqposttest_small.csv` | 24 | Individual obs | Small sample warnings |
| `chisqposttest_null.csv` | 180 | Individual obs | Non-significant omnibus |
| `chisqposttest_perfect.csv` | 90 | Individual obs | Perfect association |
| `chisqposttest_all_features.csv` | 400 | Individual obs | Same as comprehensive |

## Dataset Details

### 1. chisqposttest_comprehensive.csv (Primary Test Data)

**Variables:**
- `ID`: Patient identifier (1-400)
- `TreatmentGroup`: Placebo, Drug A, Drug B, Drug C → **STRONG association** with ClinicalResponse
- `ClinicalResponse`: No Response, Response
- `DiseaseType`: Type 1, Type 2, Type 3 → **NULL association** with Biomarker
- `Biomarker`: Negative, Positive
- `TumorGrade`: Well Diff, Moderate Diff, Poor Diff → **MODERATE association** with LymphNodeStatus
- `LymphNodeStatus`: N0, N1, N2
- `RareMutation`: Wild Type, Variant A, B, C → **Triggers Fisher's test** (small expected counts)
- `DrugSensitivity`: Resistant, Sensitive
- `Gender`: Male, Female
- `SideEffect`: No, Yes
- `RareDisease`: Common, Uncommon, Rare, Very Rare → **Unbalanced**
- `Complication`: None, Mild, Severe
- `AgeGroup`: < 40, 40-60, > 60
- `SmokingStatus`: Never, Former, Current

**Missing Data:** ~5% in TreatmentGroup, TumorGrade, Gender

**Key Test Cases:**
```
✅ Strong: TreatmentGroup × ClinicalResponse
   Expected: p < 0.001, post-hoc significant

✅ Moderate: TumorGrade × LymphNodeStatus
   Expected: p < 0.05, some post-hoc pairs significant

❌ Null: DiseaseType × Biomarker
   Expected: p > 0.05, no post-hoc (omnibus NS)

⚠️ Small counts: RareMutation × DrugSensitivity
   Expected: Fisher's exact test automatically used
```

---

### 2. chisqposttest_weighted.csv (Frequency Data)

**Structure:** Summarized contingency table data

**Variables:**
- `PathologyType`: Adenocarcinoma, Squamous Cell, Small Cell, Large Cell (4 levels)
- `TreatmentArm`: Surgery Only, Surgery + Chemo, Surgery + Radio, Chemo Only, Radio Only, Best Supportive (6 levels)
- `Count`: Frequency of each combination (24 rows total)

**Use Case:** Testing the `counts` parameter
```r
chisqposttest(
  data = weighted_data,
  rows = "PathologyType",
  cols = "TreatmentArm",
  counts = "Count"  # ← This is what we're testing
)
```

**Expected Results:**
- ✅ Weighted data information message displayed
- ✅ Analysis treats each row as summarized frequency
- ✅ Total N = sum(Count) = 610

---

### 3. chisqposttest_small.csv (Small Sample)

**Variables:**
- `Drug`: Placebo, Low Dose, High Dose (3 levels)
- `Toxicity`: None, Mild, Severe (3 levels)
- **N = 24** (8 per drug group)

**Expected Warnings:**
- ⚠️ "Very small sample size (n=24)"
- ⚠️ ">20% of cells have expected counts < 5"
- ℹ️ "Consider using Fisher's exact test"

**Use Case:** Testing small sample behavior and warning system

---

### 4. chisqposttest_null.csv (Perfect Null)

**Variables:**
- `GroupA`: Cat 1, Cat 2, Cat 3 (3 levels, 60 each)
- `GroupB`: Type X, Type Y, Type Z (3 levels, 60 each)
- **Perfectly balanced** - no association

**Expected Results:**
- ✅ Omnibus chi-square p ≈ 1.0 (not significant)
- ✅ Post-hoc table HIDDEN
- ✅ Warning: "Post-hoc Testing Not Performed"
- ✅ Educational message about data dredging

**Use Case:** Testing non-significant omnibus path

---

### 5. chisqposttest_perfect.csv (Perfect Association)

**Variables:**
- `Stage`: Early, Intermediate, Advanced (3 levels, 30 each)
- `Risk`: Low, Medium, High (3 levels, 30 each)
- **Perfect diagonal pattern** - extreme association

**Expected Results:**
- ✅ Omnibus chi-square p < 0.001 (highly significant)
- ✅ All pairwise comparisons significant
- ✅ Effect sizes (Phi) very high (≈ 1.0)

**Use Case:** Testing extreme association scenario

---

## Example Usage

### Load in jamovi:
1. File → Open → Data
2. Select any CSV file from above
3. Analyses → Exploration → ClinicoPath Comparisons → Chi-Square Post-Hoc Tests
4. Configure options and run

### Load in R:
```r
# Main comprehensive testing
data <- read.csv("data/chisqposttest_comprehensive.csv")

# Strong association (post-hoc WILL run)
ClinicoPath::chisqposttest(
  data = data,
  rows = "TreatmentGroup",
  cols = "ClinicalResponse",
  posthoc = "bonferroni"
)

# Null association (post-hoc will NOT run)
null_data <- read.csv("data/chisqposttest_null.csv")
ClinicoPath::chisqposttest(
  data = null_data,
  rows = "GroupA",
  cols = "GroupB",
  posthoc = "bonferroni"
)

# Weighted data
weighted_data <- read.csv("data/chisqposttest_weighted.csv")
ClinicoPath::chisqposttest(
  data = weighted_data,
  rows = "PathologyType",
  cols = "TreatmentArm",
  counts = "Count"
)
```

---

## Complete Testing Guide

See: `tests/chisqposttest_TESTING_GUIDE.md` for comprehensive testing instructions

---

**Generated:** 2025-12-23
**Script:** `data-raw/chisqposttest_comprehensive_data.R`
