# Jaccard Distance Binary Conversion Guide

## Overview

When using Jaccard distance for IHC clustering, all data must be converted to binary (0/1) format. The ClinicoPath module performs this conversion automatically and displays a comprehensive notice showing exactly how your data was converted.

**This guide explains:**
1. How automatic binary conversion works
2. How to structure your data correctly
3. Common pitfalls and warnings
4. When conversion may fail or produce incorrect results

---

## Automatic Binary Conversion Notice

When you select **Jaccard distance**, a prominent notice appears at the top of your results showing:

### 1. Categorical Marker Conversion Table

Shows for each categorical IHC marker:
- **Marker name**
- **Original levels** in your data
- **Number of Positive (1)** cases
- **Number of Negative (0)** cases
- **Status** (OK or WARNING)

**Example:**

| Marker | Original Levels | Positive (1) | Negative (0) | Status |
|--------|----------------|--------------|--------------|--------|
| **EMA** | Positive, Negative | 45 | 55 | **OK** |
| **CK7** | pos, neg | 38 | 62 | **OK** |
| **TTF1** | +, - | 52 | 48 | **OK** |
| **p63** | Present, Absent | 0 | 100 | **WARNING: All same** |
| **CD99** | Strong, Weak, Negative | 30 | 70 | **WARNING: Non-standard coding** |

### 2. Continuous Marker Conversion Table

Shows for each continuous marker:
- **Marker name**
- **Range** of values
- **Median** split point
- **Number Above (1)** median
- **Number Below (0)** median

**Example:**

| Marker | Range | Median | Above (1) | Below (0) |
|--------|-------|--------|-----------|-----------|
| **Ki67_Percent** | 0.0 - 95.0 | 25.5 | 50 | 50 |
| **H_Score** | 0.0 - 300.0 | 150.0 | 48 | 52 |
| **CD8_Count** | 5.0 - 250.0 | 45.0 | 51 | 49 |

---

## Data Structure Requirements

### Categorical Markers: Accepted Coding

The conversion recognizes these as **POSITIVE (coded as 1):**

✅ **Acceptable positive values:**
- `"Positive"`
- `"pos"`
- `"+"`
- `"1"`

All other values are treated as **NEGATIVE (coded as 0):**
- `"Negative"`
- `"neg"`
- `"-"`
- `"0"`
- `"Weak"` (treated as negative!)
- `"Moderate"` (treated as negative!)
- `"Absent"`
- Any other text

### ⚠️ Common Mistakes

#### Mistake 1: Non-Standard Positive Coding

**Problem:**
```
Marker: CD99
Levels: "Strong", "Weak", "Negative"
```

**What happens:**
- `"Strong"` → Treated as **NEGATIVE** (0)
- `"Weak"` → Treated as **NEGATIVE** (0)
- `"Negative"` → Treated as **NEGATIVE** (0)
- **Result:** ALL cases coded as 0! Marker provides no information.

**Solution:**
Recode your data before analysis:
```r
data$CD99 <- factor(ifelse(data$CD99 %in% c("Strong", "Weak"), "Positive", "Negative"))
```

#### Mistake 2: Ordinal Intensity Scales

**Problem:**
```
Marker: HER2
Levels: "0", "1+", "2+", "3+"
```

**What happens:**
- `"0"` → Treated as **NEGATIVE** (0)
- `"1+"` → Treated as **NEGATIVE** (0) ❌ Lost information!
- `"2+"` → Treated as **NEGATIVE** (0) ❌ Lost information!
- `"3+"` → Treated as **NEGATIVE** (0) ❌ Lost information!

**Solution:**
**DO NOT use Jaccard distance with ordinal scales.** Use **Gower distance** instead, which preserves ordinal information.

Or, if you must use Jaccard, recode:
```r
data$HER2 <- factor(ifelse(data$HER2 %in% c("2+", "3+"), "Positive", "Negative"))
```

#### Mistake 3: Language-Specific Coding

**Problem:**
```
Marker: ER
Levels: "Positivo", "Negativo" (Spanish)
Levels: "Pozitif", "Negatif" (Turkish)
```

**What happens:**
- Non-English text is treated as **NEGATIVE** (0)
- All cases may appear negative

**Solution:**
Recode to English standard:
```r
data$ER <- factor(ifelse(data$ER == "Positivo", "Positive", "Negative"))
```

---

## Continuous Markers: Median Split

### How It Works

For continuous markers (H-scores, percentages, counts):

1. **Calculate median** of all values
2. **Values > median** → coded as 1 (High)
3. **Values ≤ median** → coded as 0 (Low)

### ⚠️ Information Loss Warning

**Original data:**
```
Ki67: 5%, 10%, 15%, 20%, 25%, 30%, 35%, 40%
Median = 22.5%
```

**After binary conversion:**
```
Ki67: 0, 0, 0, 0, 1, 1, 1, 1
```

**What's lost:**
- The difference between 5% and 20% (both → 0)
- The difference between 25% and 40% (both → 1)
- All nuanced variation within high/low groups

### When Median Split is Problematic

1. **Bimodal distributions:**
```
Ki67 values: 5, 5, 5, 50, 55, 60, 60, 60
Median = 52.5
Low group: 5, 5, 5, 50  ← 50% is "low"? Questionable.
High group: 55, 60, 60, 60
```

2. **Narrow ranges:**
```
H-Score: 145, 148, 150, 152, 155
Median = 150
Result: Almost arbitrary split of similar values
```

3. **Clinical cutoffs ignored:**
```
Ki67: 2%, 8%, 15%, 20%, 30%, 45%
Clinical cutoff: 14% (standard for breast cancer)
Median split: 17.5% (not clinically meaningful)
```

**Solution:** For continuous markers with clinical meaning, use **Gower distance** or pre-categorize using established cutoffs:
```r
data$Ki67_Category <- factor(ifelse(data$Ki67_Percent > 14, "High", "Low"))
```

---

## Status Warnings Explained

### "WARNING: All same"

**What it means:** All cases have the same binary value (all 0 or all 1).

**Example:**
```
p63: All cases coded as "Negative"
→ After conversion: All 0
→ Provides NO clustering information
```

**Action required:**
1. Check if this is biologically expected
2. If not expected, verify data entry
3. Consider removing this marker from analysis
4. If all negative is correct, marker won't help distinguish clusters

### "WARNING: Non-standard coding"

**What it means:** Marker uses values that don't match expected Positive/Negative format.

**Example:**
```
CD99 levels: "Strong", "Moderate", "Weak", "Negative"
```

**Action required:**
1. Verify how the marker was coded in your data
2. Recode to standard Positive/Negative format
3. Or remove marker if recoding is complex

---

## Verification Checklist

Before interpreting results from Jaccard distance clustering, verify:

### ☑ Categorical Markers
- [ ] All markers use standard positive/negative coding
- [ ] No markers show "All same" warning
- [ ] Non-standard coding warnings investigated and resolved
- [ ] Proportion of positive/negative makes biological sense
- [ ] No ordinal scales (0/1+/2+/3+) included

### ☑ Continuous Markers
- [ ] Understand that median split was applied
- [ ] Accept information loss from dichotomization
- [ ] Median split point is reasonable for your data
- [ ] No extreme skewness causing arbitrary splits
- [ ] Clinical cutoffs (if any) are not violated

### ☑ Overall Data Quality
- [ ] Binary conversion table shows expected patterns
- [ ] No markers provide zero information (all 0 or all 1)
- [ ] Sample size adequate for number of markers
- [ ] Missing data patterns checked

---

## When Jaccard is Appropriate vs. Gower

### ✅ Use Jaccard Distance When:

1. **Tissue microarray (TMA) scoring:**
   - Binary present/absent assessment
   - No intensity recorded
   - Example: "Is protein expressed? Yes/No"

2. **Replicating published studies:**
   - Study specifically used Jaccard distance
   - Example: Sterlacci et al. 2019 NSCLC study
   - Need direct methodological comparison

3. **True binary biology:**
   - Genetic mutations (present/absent)
   - Fusion proteins (detected/not detected)
   - Strictly dichotomous phenotypes

4. **Pre-dichotomized data:**
   - Data already categorized as positive/negative
   - Established clinical cutoffs applied
   - No ordinal or continuous information available

### ✅ Use Gower Distance (Default) When:

1. **Mixed data types:**
   - Categorical AND continuous markers
   - Example: ER (pos/neg) + Ki67 (percentage)

2. **Ordinal intensity scales:**
   - 0/1+/2+/3+ scoring
   - Weak/Moderate/Strong
   - Preserves ordering information

3. **Continuous measurements:**
   - H-scores (0-300)
   - Percentage positivity (0-100%)
   - Cell counts
   - Preserves full numeric information

4. **Uncertain about coding:**
   - Data quality questionable
   - Non-standard marker definitions
   - Safer choice when in doubt

5. **Maximum information retention:**
   - Want to preserve all data nuances
   - Publication-quality analysis
   - Exploratory phase

---

## Example: Correct Data Structure

### Scenario: NSCLC IHC Panel

**Correct structure for Jaccard distance:**

```r
# Categorical markers - Standard binary coding
data <- data.frame(
    CaseID = paste0("Case_", 1:100),

    # Correct: Uses "Positive"/"Negative"
    CK7 = factor(sample(c("Positive", "Negative"), 100, replace=TRUE)),
    TTF1 = factor(sample(c("Positive", "Negative"), 100, replace=TRUE)),

    # Correct: Uses "pos"/"neg"
    p63 = factor(sample(c("pos", "neg"), 100, replace=TRUE)),

    # Correct: Uses "+"/"-"
    CK5_6 = factor(sample(c("+", "-"), 100, replace=TRUE)),

    # Continuous - Will be median split (information loss accepted)
    Ki67_Percent = rnorm(100, mean=30, sd=15)
)

# Run clustering
results <- ihccluster(
    data = data,
    catVars = c("CK7", "TTF1", "p63", "CK5_6"),
    contVars = "Ki67_Percent",
    method = "hierarchical",
    distanceMethod = "jaccard",
    nClusters = 3
)

# Binary Conversion Notice will appear showing:
# - All categorical markers: OK status
# - Ki67_Percent: Median split at ~30%
```

**Incorrect structure (will cause warnings):**

```r
# WRONG: Non-standard coding
data_wrong <- data.frame(
    CaseID = paste0("Case_", 1:100),

    # ❌ WRONG: Uses "Strong"/"Weak"/"Negative"
    CK7 = factor(sample(c("Strong", "Weak", "Negative"), 100, replace=TRUE)),

    # ❌ WRONG: Ordinal scale
    HER2 = factor(sample(c("0", "1+", "2+", "3+"), 100, replace=TRUE)),

    # ❌ WRONG: Non-English
    ER = factor(sample(c("Positivo", "Negativo"), 100, replace=TRUE))
)

# Will produce warnings and incorrect results
```

---

## Troubleshooting Conversion Issues

### Issue 1: "All my markers show WARNING status"

**Likely cause:** Data uses non-standard coding throughout.

**Diagnostic steps:**
1. Check original data: `table(data$MarkerName)`
2. Verify factor levels: `levels(data$MarkerName)`
3. Look for language-specific text, abbreviations, or intensity scales

**Solution:**
```r
# Batch recode all markers
standard_markers <- c("CK7", "TTF1", "p63", "CK5_6")
positive_codes <- c("Positive", "pos", "+", "strong", "present", "detected")

for (marker in standard_markers) {
    data[[marker]] <- factor(
        ifelse(tolower(data[[marker]]) %in% tolower(positive_codes),
               "Positive",
               "Negative")
    )
}
```

### Issue 2: "Median split seems arbitrary"

**Likely cause:** Continuous markers have non-normal distributions.

**Diagnostic steps:**
1. Plot histogram: `hist(data$Ki67_Percent)`
2. Check for bimodality, skewness, outliers
3. Verify if clinical cutoffs exist

**Solution:**
```r
# Option 1: Use clinical cutoff instead
data$Ki67_Binary <- factor(
    ifelse(data$Ki67_Percent > 14,  # Standard breast cancer cutoff
           "High",
           "Low")
)

# Option 2: Use Gower distance to preserve continuous data
results <- ihccluster(
    data = data,
    catVars = categorical_markers,
    contVars = "Ki67_Percent",  # Kept as continuous
    method = "hierarchical",
    distanceMethod = "gower"  # Use Gower instead
)
```

### Issue 3: "Results don't match published study"

**Likely cause:** Different binary conversion approach.

**Check:**
1. Published study's exact coding definitions
2. Whether they used different cutoffs
3. Pre-processing steps (log transform, standardization)

**Solution:**
- Match exact methodology from paper
- Contact authors for data coding details
- Document any differences in your methods section

---

## Summary Decision Tree

```
Do you have ordinal intensity scales (0/1+/2+/3+)?
    YES → Use Gower distance
    NO  → Continue

Are all markers truly binary (present/absent)?
    NO  → Use Gower distance
    YES → Continue

Are markers coded as Positive/Negative (or pos/neg or +/-)?
    NO  → Recode first, then use Jaccard
    YES → Continue

Do you have continuous markers?
    YES → Accept median split information loss?
        NO  → Use Gower distance
        YES → Use Jaccard (check conversion notice)
    NO  → Use Jaccard (verify conversion notice)
```

---

## Additional Resources

**Related Documentation:**
- `STERLACCI_2019_FEATURE_ANALYSIS.md` - Technical implementation details
- `STERLACCI_2019_USER_GUIDE.md` - Complete user guide for Phase 1 features
- `IHC_PREDICTION_PATHOLOGIST_GUIDE.md` - IHC prediction workflow

**Key Reference:**
Sterlacci W, et al. (2019). Tissue microarray based analysis of immunohistochemical expression patterns of molecular targets in NSCLC. *Histol Histopathol*.

**Support:**
- GitHub Issues: https://github.com/sbalci/ClinicoPathJamoviModule/issues
- jamovi Forum: https://forum.jamovi.org

---

**Document Version:** 1.0
**Date:** 2025-01-15
**Related Feature:** Jaccard Distance Implementation (Phase 1)
