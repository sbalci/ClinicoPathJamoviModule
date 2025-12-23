# Chi-Square Post-Hoc Test - Comprehensive Testing Guide

**Module:** `chisqposttest`
**Created:** 2025-12-23
**Purpose:** Test all features and edge cases of the chisqposttest module

---

## Test Data Files

All test datasets are located in `data/` folder:

| File | Size | Description | Use Case |
|------|------|-------------|----------|
| `chisqposttest_comprehensive.csv` | 53K, n=400 | Main test dataset with multiple scenarios | Primary testing |
| `chisqposttest_weighted.csv` | 844B, n=24 | Summarized frequency data | Counts variable testing |
| `chisqposttest_small.csv` | 456B, n=24 | Small sample size | Fisher's exact test, warnings |
| `chisqposttest_null.csv` | 3.0K, n=180 | Perfect null association | Non-significant omnibus test |
| `chisqposttest_perfect.csv` | 1.7K, n=90 | Perfect diagonal association | Extreme association |
| `chisqposttest_all_features.csv` | 55K | Combined dataset | Quick comprehensive test |

---

## Test Scenarios by Dataset

### 1. Main Comprehensive Dataset (`chisqposttest_comprehensive.csv`)

#### Variables and Expected Results:

| Rows Variable | Cols Variable | Association | Expected Omnibus | Post-hoc Expected |
|--------------|---------------|-------------|------------------|-------------------|
| `TreatmentGroup` (4 levels) | `ClinicalResponse` (2 levels) | **Strong** | p < 0.001 | Multiple significant pairs |
| `TumorGrade` (3 levels) | `LymphNodeStatus` (3 levels) | **Moderate** | p < 0.05 | Some significant pairs |
| `DiseaseType` (3 levels) | `Biomarker` (2 levels) | **Weak/None** | p > 0.05 | No post-hoc (omnibus NS) |
| `RareMutation` (4 levels) | `DrugSensitivity` (2 levels) | **Moderate** | p < 0.05 | Fisher's test used |
| `RareDisease` (4 levels) | `Complication` (3 levels) | **Moderate** | p < 0.05 | Unbalanced cells |
| `Gender` (2 levels) | `SideEffect` (2 levels) | **Weak** | p > 0.05 | No post-hoc (omnibus NS) |

#### Test Cases:

##### Test 1.1: Strong Association - Post-hoc WILL Run
```
Rows: TreatmentGroup
Cols: ClinicalResponse
Post-hoc: Bonferroni
Significance: 0.05
```
**Expected:**
- ✅ Omnibus chi-square significant (p < 0.001)
- ✅ Post-hoc table visible with 6 pairwise comparisons
- ✅ Multiple comparisons show "Yes" for significant
- ✅ Effect sizes (Phi) vary by comparison
- ✅ Adjusted p-values > raw p-values

##### Test 1.2: Weak Association - Post-hoc Will NOT Run
```
Rows: DiseaseType
Cols: Biomarker
Post-hoc: Bonferroni
Significance: 0.05
```
**Expected:**
- ✅ Omnibus chi-square NOT significant (p > 0.05)
- ✅ Warning message: "Post-hoc Testing Not Performed"
- ✅ Post-hoc table HIDDEN
- ✅ Explanation about Type I error inflation

##### Test 1.3: Small Expected Counts - Fisher's Test
```
Rows: RareMutation
Cols: DrugSensitivity
Post-hoc: Holm
Significance: 0.05
```
**Expected:**
- ✅ Fisher's exact test used for some pairwise comparisons
- ✅ Info message about Fisher's test usage
- ✅ Test Method column shows mix of "Chi-square" and "Fisher's exact"

##### Test 1.4: Missing Data Handling
```
Rows: TreatmentGroup
Cols: ClinicalResponse
Exclude Missing: FALSE
```
**Expected:**
- ✅ Analysis includes NA categories
- ⚠️ May see warning about missing values

```
Rows: TreatmentGroup
Cols: ClinicalResponse
Exclude Missing: TRUE
```
**Expected:**
- ✅ Missing data excluded
- ✅ Sample size reduced
- ✅ No NA in contingency table

---

### 2. Weighted Dataset (`chisqposttest_weighted.csv`)

#### Test 2.1: Counts Variable Feature
```
Rows: PathologyType
Cols: TreatmentArm
Counts: Count
Post-hoc: FDR
```
**Expected:**
- ✅ Weighted data message shown
- ✅ Analysis treats Count as frequency weights
- ✅ Contingency table reflects weighted counts
- ✅ Post-hoc comparisons if significant

---

### 3. Small Sample Dataset (`chisqposttest_small.csv`)

#### Test 3.1: Small Sample Warnings
```
Rows: Drug
Cols: Toxicity
Post-hoc: Bonferroni
```
**Expected:**
- ⚠️ Warning: "Very small sample size (n=24)"
- ⚠️ Warning: ">20% cells have expected counts < 5"
- ✅ Fisher's exact test recommended
- ✅ Post-hoc uses Fisher's test automatically

---

### 4. Null Association Dataset (`chisqposttest_null.csv`)

#### Test 4.1: Non-Significant Omnibus Test
```
Rows: GroupA
Cols: GroupB
Post-hoc: Bonferroni
```
**Expected:**
- ✅ Omnibus chi-square NOT significant (p ≈ 1.0)
- ✅ Post-hoc table HIDDEN
- ✅ Warning: "Post-hoc Testing Not Performed"
- ✅ Educational message about data dredging

---

### 5. Perfect Association Dataset (`chisqposttest_perfect.csv`)

#### Test 5.1: Extreme Association
```
Rows: Stage
Cols: Risk
Post-hoc: Bonferroni
```
**Expected:**
- ✅ Omnibus chi-square highly significant (p < 0.001)
- ✅ All pairwise comparisons significant
- ✅ High effect sizes (Phi close to 1.0)
- ⚠️ Possible numerical issues with perfect separation

---

## Feature Testing Checklist

### Post-hoc Methods
- [ ] **Bonferroni**: Most conservative correction
- [ ] **Holm**: Step-down correction (less conservative)
- [ ] **FDR**: False Discovery Rate control
- [ ] **None**: Disables post-hoc entirely, hides table

### Significance Levels
- [ ] α = 0.05 (default)
- [ ] α = 0.01 (more conservative)
- [ ] α = 0.10 (less conservative)

### Display Options
- [ ] **Show Expected Values**: Displays expected counts in contingency table
- [ ] **Show Residual Plot**: Heatmap of standardized residuals
- [ ] **Show Residuals Analysis**: Text interpretation of residuals
- [ ] **Show Educational Panels**: Explanatory text about methods
- [ ] **Show Detailed Comparison Tables**: Individual 2×2 tables for each pair
- [ ] **Show Assumptions Check**: Validation of chi-square assumptions
- [ ] **Show Clinical Summary**: Natural language summary
- [ ] **Show Example Interpretations**: Example text for reporting
- [ ] **Generate Report Sentences**: Copy-ready text for manuscripts

### Test Selection
- [ ] **Automatic** (recommended): Chi-square or Fisher's based on expected counts
- [ ] **Always Chi-Square**: Force chi-square even with small counts
- [ ] **Always Fisher's Exact**: Force Fisher's test for all comparisons

### Residuals Cutoff
- [ ] 2.0 (default): Standard cutoff for significant residuals
- [ ] 3.0: More conservative cutoff
- [ ] Custom values (1.5 - 4.0)

### Export and Reporting
- [ ] **Export Detailed Results**: Downloadable comprehensive table
- [ ] **Show Glossary**: Statistical terms definitions
- [ ] **Color-Blind Safe Palette**: Accessible visualizations

---

## Edge Cases to Test

### 1. Empty or Invalid Data
- [ ] Test with empty dataset (n=0)
- [ ] Test with single row/column
- [ ] Test with non-factor variables

### 2. Missing Counts Variable
- [ ] Specify non-existent counts variable
- [ ] Specify non-numeric counts variable

### 3. Serialization Issues
- [ ] Run analysis, switch options rapidly
- [ ] Verify no "attempt to apply non-function" errors
- [ ] Check all HTML elements render correctly

### 4. UI Table Visibility
- [ ] Verify post-hoc table HIDDEN when:
  - Post-hoc method = "None"
  - Omnibus test not significant
  - No valid pairwise comparisons possible
- [ ] Verify post-hoc table VISIBLE when:
  - Omnibus significant AND post-hoc method selected

---

## Expected Behavior Summary

| Scenario | Omnibus Test | Post-hoc Method | Post-hoc Table | Message |
|----------|--------------|-----------------|----------------|---------|
| Strong association | p < α | Bonferroni/Holm/FDR | ✅ Visible | Results displayed |
| Weak/null association | p ≥ α | Bonferroni/Holm/FDR | ❌ Hidden | "Not Performed" warning |
| Any association | Any | None | ❌ Hidden | "Disabled" warning |
| Strong association | p < α | None | ❌ Hidden | "Disabled" warning |
| Small expected counts | p < α | Any (not None) | ✅ Visible | Fisher's test used |

---

## Sample Commands for Testing

### In jamovi:
1. Load dataset from Data menu
2. Navigate to: **Exploration** → **ClinicoPath Comparisons** → **Chi-Square Post-Hoc Tests**
3. Drag variables to Rows and Columns
4. Select post-hoc method
5. Toggle display options
6. Verify results

### In R Console:
```r
# Load data
data <- read.csv("data/chisqposttest_comprehensive.csv")

# Test strong association
ClinicoPath::chisqposttest(
  data = data,
  rows = "TreatmentGroup",
  cols = "ClinicalResponse",
  posthoc = "bonferroni",
  sig = 0.05,
  excl = TRUE
)

# Test null association
ClinicoPath::chisqposttest(
  data = read.csv("data/chisqposttest_null.csv"),
  rows = "GroupA",
  cols = "GroupB",
  posthoc = "bonferroni"
)

# Test weighted data
ClinicoPath::chisqposttest(
  data = read.csv("data/chisqposttest_weighted.csv"),
  rows = "PathologyType",
  cols = "TreatmentArm",
  counts = "Count",
  posthoc = "fdr"
)
```

---

## Regression Testing Checklist

After any code changes, verify:

- [ ] No serialization errors
- [ ] Post-hoc table visibility works correctly
- [ ] All HTML elements render properly
- [ ] Educational panels display when enabled
- [ ] Fisher's exact test selection works
- [ ] Multiple testing corrections calculate correctly
- [ ] Residuals analysis displays accurately
- [ ] Export functionality works
- [ ] Missing data handling works as expected
- [ ] Weighted data (counts variable) works correctly
- [ ] Small sample warnings appear appropriately
- [ ] All assumptions checks function properly

---

## Known Issues and Limitations

1. **Notices System**: HTML-based messages only (no jmvcore::Notice objects due to serialization)
2. **Newlines in Notices**: Current notices don't support newlines (using HTML formatting instead)
3. **Very Small Samples**: n < 20 may have unreliable chi-square approximation
4. **Perfect Separation**: Extreme associations may cause numerical issues

---

## Contact

For issues or questions about testing, see:
- GitHub Issues: https://github.com/sbalci/ClinicoPathJamoviModule/issues
- Documentation: https://www.serdarbalci.com/ClinicoPathJamoviModule/

---

**Last Updated:** 2025-12-23
