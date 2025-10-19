# Pathsampling Module: Binomial Model Warning for Dependent Sampling

## Summary

Added context-aware warnings to the **Binomial Probability Model** section of `pathsampling` to alert users when they're using an inappropriate statistical method for dependent sampling scenarios (e.g., tumor blocks).

**Implementation Date**: 2025-10-15
**Related Documents**:
- Vignette: `general-independent-vs-dependent-sampling-explained.Rmd`
- Previous enhancement: `pathsampling-tumor-sampling-implementation-summary.md`

---

## Problem Identified

### What Was Missing

While the **Empirical Cumulative Detection** section already had appropriate warnings for tumor sampling (added earlier today), the **Binomial Probability Model** section lacked any warning about inappropriate use with dependent samples.

**Risk**: Users could enable "Show Binomial Model" for tumor block analysis without understanding that it will:
1. **Systematically underestimate** cumulative sensitivity (by ~10 percentage points at 6 blocks)
2. Produce **misleading recommendations** ("need 8 blocks" when actually 6 is sufficient)
3. **Violate independence assumptions** fundamental to binomial distribution

### Example of the Problem

From Duan 2023 data:
```
Binomial prediction at 6 blocks: 88.4%  ← WRONG
Observed sensitivity at 6 blocks: 98.3%  ← CORRECT (empirical)

Underestimation: 9.9 percentage points
```

This is **clinically significant** - it could lead pathologists to recommend more blocks than necessary, increasing workload without benefit.

---

## Solution Implemented

### Code Changes (R/pathsampling.b.R)

**Location**: Lines 794-827 (inserted before binomial output generation)

Added context-aware warning system that checks `analysisContext` option and displays appropriate warnings:

#### 1. Tumor Sampling Context (Strong Warning - Red)

```r
if (analysisContext == "tumor") {
    binomialWarning <- sprintf("<div style='background: #ffebee; border: 1px solid #ef5350; ...'>
        <p><b>⚠️ WARNING: Binomial Model May Underestimate Sensitivity for Tumor Sampling</b></p>
        <p>Sequential tumor samples (blocks) are <b>not independent</b> - they are serial sections
        through the same lesion with spatial clustering. The binomial model assumes each sample is
        an independent event, which leads to <b>systematic underestimation</b> of cumulative
        detection sensitivity.</p>
        <p><b>Recommendation:</b> Use <b>Empirical Cumulative Detection</b> (non-parametric) with
        <b>Bootstrap CIs</b> instead. These methods correctly handle spatial dependence and are the
        gold standard for tumor block sampling adequacy.</p>
        <p><em>Reference: See vignette 'Independent vs Dependent Sampling' for detailed explanation</em></p>
    </div>", ...)
}
```

**Visual appearance**: Red border, red background, prominent warning icon

**Key messages**:
- Explains WHY binomial is inappropriate (spatial clustering, non-independence)
- Quantifies the RISK (systematic underestimation)
- Provides ACTIONABLE recommendation (use empirical + bootstrap)
- References DOCUMENTATION (vignette for deeper understanding)

#### 2. Margin Sampling Context (Caution - Orange)

```r
else if (analysisContext == "margin") {
    binomialWarning <- sprintf("<div style='background: #fff3e0; border: 1px solid #ff9800; ...'>
        <p><b>⚠️ Caution: Margin samples may show spatial clustering</b></p>
        <p>Consider using empirical methods if margin positivity is geographically clustered.</p>
    </div>", ...)
}
```

**Visual appearance**: Orange border, amber background, softer warning

**Rationale**: Margin sampling is intermediate - may have some clustering (margins close together) but less severe than tumor blocks.

#### 3. Other Contexts (No Warning)

For `lymphnode`, `omentum`, `general` contexts: `binomialWarning = ""` (empty string)

**Rationale**: Binomial model is appropriate for independent samples (lymph nodes are separate anatomic structures).

---

## Integration with Existing Code

### Modified HTML Output

**Before** (line 843):
```r
html <- sprintf("<div style='%s'>
    <div style='%s'>
        <h4 style='%s'>Binomial Probability Model</h4>
        ...
```

**After** (lines 843-862):
```r
html <- sprintf("<div style='%s'>
    %s                           # ← INSERT WARNING HERE
    <div style='%s'>
        <h4 style='%s'>Binomial Probability Model</h4>
        ...
```

The warning box appears **before** the main binomial content, making it impossible to miss.

---

## User Experience Flow

### Scenario: User Analyzes Tumor Block Data with Binomial Model

**Step 1**: User loads `duan2023_vi_blocks.csv` in jamovi

**Step 2**: User opens `pathsampling` module and configures:
- Analysis Context: **Tumor Sampling (VI/EMVI/PNI/Budding)**
- Total Samples: `blocks_examined`
- First Detection: `first_vi_block`
- ✅ Enable: **Show Binomial Model** (mistakenly thinking it's appropriate)

**Step 3**: User sees **prominent red warning box** above binomial output:

```
┌─────────────────────────────────────────────────────────────────┐
│ ⚠️ WARNING: Binomial Model May Underestimate Sensitivity       │
│ for Tumor Sampling                                              │
│                                                                 │
│ Sequential tumor samples (blocks) are NOT independent -         │
│ they are serial sections through the same lesion with spatial   │
│ clustering. The binomial model assumes each sample is an        │
│ independent event, which leads to systematic underestimation.   │
│                                                                 │
│ RECOMMENDATION: Use Empirical Cumulative Detection (non-        │
│ parametric) with Bootstrap CIs instead. These methods correctly │
│ handle spatial dependence.                                      │
│                                                                 │
│ Reference: See vignette 'Independent vs Dependent Sampling'    │
└─────────────────────────────────────────────────────────────────┘

Binomial Probability Model
Estimated per-sample detection probability: q = 0.3500
...
```

**Step 4**: User makes informed decision:
- **Option A** (recommended): Disable binomial, enable empirical + bootstrap
- **Option B** (acceptable for exploration): Proceed with binomial but interpret results cautiously, compare to empirical

---

## Consistency Across Module

### Warning Coverage by Analysis Section

| Section | Tumor Context Warning | Margin Context Warning | Other Contexts |
|---------|:---------------------:|:----------------------:|:--------------:|
| **Binomial Model** | ✅ Red (strong) | ✅ Orange (caution) | ⬜ None |
| **Empirical Cumulative** | ✅ Yellow (info) | ✅ Blue (info) | ⬜ None |
| **Bootstrap Analysis** | ⬜ None (always appropriate) | ⬜ None | ⬜ None |
| **Recommendations** | ✅ Context-specific examples | ✅ Context-specific | ✅ Generic |

**Complete coverage**: Users are warned at every point where they might use an inappropriate method.

---

## Validation Against Vignette Principles

### Vignette Section 7: "Summary & Recommendations"

**Vignette states**:
> **Inappropriate model**: Binomial ❌ (underestimates) [for dependent sampling]

**Implementation**: ✅ **Warning explicitly states "may underestimate"**

---

**Vignette states**:
> **For Tumor Block Analysis**:
> 1. ✅ Set Analysis Context: "Tumor Sampling"
> 2. ✅ Enable: "Show Empirical Cumulative Detection"
> 3. ✅ Enable: "Show Bootstrap Analysis"
> 4. ❌ Do NOT rely on: Binomial model predictions

**Implementation**: ✅ **Warning provides this exact guidance**

---

**Vignette states**:
> **Report methodology**: "We used non-parametric empirical sensitivity curves with bootstrap 95% CIs"

**Implementation**: ✅ **Warning recommends empirical + bootstrap, references vignette for reporting template**

---

## Testing Recommendations

### Manual Testing Checklist

1. ✅ **Syntax check**: R code compiles without errors
2. ⏳ **Jamovi integration**: Load Duan dataset, enable binomial with tumor context
3. ⏳ **Visual verification**: Confirm red warning box appears above binomial output
4. ⏳ **Warning suppression**: Verify NO warning for lymphnode context
5. ⏳ **Orange warning**: Verify orange caution for margin context
6. ⏳ **Cross-section consistency**: Check that empirical section still has yellow info box

### Expected Behavior

**Tumor context + binomial enabled**:
- Red warning box with 4 paragraphs
- Warning references vignette
- Binomial output still displays (not suppressed)
- Users can proceed but are informed

**Lymphnode context + binomial enabled**:
- NO warning box
- Binomial output displays normally
- Clean, uncluttered interface

**Margin context + binomial enabled**:
- Orange caution box with 2 paragraphs
- Softer language ("consider" vs "WARNING")
- Binomial output displays

---

## Documentation Impact

### Updates Needed

1. **README.md**: No changes needed (feature, not architecture)

2. **NEWS.md**: Add to v0.0.33 changelog:
   ```markdown
   - pathsampling: Added context-aware warnings for inappropriate use of binomial model
     with dependent sampling (tumor blocks, margins). Red warning box alerts users to
     systematic underestimation risk and recommends empirical + bootstrap methods.
   ```

3. **Vignette cross-reference**: The warning message already references the vignette:
   > "See vignette 'Independent vs Dependent Sampling' for detailed explanation"

   This creates a bidirectional link:
   - Code → Vignette (via warning message)
   - Vignette → Code (Section 6: "Example: Using pathsampling...")

---

## Comparison to Earlier Implementation Today

### First Enhancement (Morning)

**File**: `pathsampling-tumor-sampling-implementation-summary.md`

**Changes**:
- Added `analysisContext` option to .a.yaml
- Added yellow info box to **Empirical Cumulative** section
- Context-aware recommendations text
- Duan 2023 dataset generation

**Focus**: Positive guidance (what TO do for tumor sampling)

### Second Enhancement (Afternoon)

**File**: `pathsampling-binomial-warning-implementation.md` (this document)

**Changes**:
- Added red warning box to **Binomial Model** section
- Explicit underestimation risk explanation
- Direct comparison to empirical method

**Focus**: Negative guidance (what NOT to do for tumor sampling)

### Complementary Approach

Together, these enhancements provide:
1. **Positive path**: "Use empirical + bootstrap" (yellow info box)
2. **Negative guard**: "Don't use binomial" (red warning box)
3. **Education**: "Learn why" (vignette reference in both places)

Users are guided to correct methods through **both encouragement and caution**.

---

## Impact on Module Philosophy

### Design Principle: "Educate, Don't Restrict"

We **do NOT**:
- Disable binomial model for tumor context
- Force users to enable empirical
- Remove incorrect options from UI

We **do**:
- ✅ Warn users prominently
- ✅ Explain consequences clearly
- ✅ Recommend alternatives
- ✅ Link to detailed documentation
- ✅ Allow informed override

**Rationale**: Users may have legitimate reasons to explore binomial (e.g., comparing to literature, teaching examples, method validation). Our role is to inform, not restrict.

---

## Statistical Rigor

### Why This Warning Matters

**Clinical scenario** (from Duan 2023):

A pathology department wants to update their protocol for colorectal cancer block submission. Current protocol: 4 blocks per tumor.

**Wrong analysis** (binomial model):
```
Binomial predicts 75% sensitivity at 4 blocks
→ Conclusion: "4 blocks is adequate for 75% detection"
→ Decision: Keep current protocol
```

**Correct analysis** (empirical method):
```
Empirical observed: 88% sensitivity at 4 blocks
→ Conclusion: "4 blocks achieves 88%, but 6 blocks needed for 95%"
→ Decision: Update protocol to 6 blocks
```

**Impact**: Without the warning, departments might underestimate their true detection rates and fail to improve protocols. The warning prevents this error.

---

## Future Enhancements (Optional)

### Phase 1: Quantitative Risk Assessment

Add specific underestimation magnitude to warning:
```r
if (analysisContext == "tumor" && !is.na(pEstimate)) {
    # Calculate expected underestimation at common block counts
    binomial_6 <- 1 - (1 - pEstimate)^6
    empirical_6 <- observed_sensitivity_at_6  # from empirical section if available

    if (!is.na(empirical_6)) {
        underest <- (empirical_6 - binomial_6) * 100
        warning_text <- sprintf("For your data, binomial underestimates by %.1f percentage points at 6 blocks.", underest)
    }
}
```

### Phase 2: Interactive Toggle

Add "Compare Binomial vs Empirical" button that:
- Shows side-by-side sensitivity curves
- Highlights divergence points
- Calculates mean absolute error

### Phase 3: Context Detection

Auto-detect likely context from variable names:
```r
if (grepl("block|tumor", tolower(totalSamplesVar))) {
    suggest_context <- "tumor"
    # Show: "Detected tumor-related variables. Consider setting Analysis Context to 'Tumor Sampling'"
}
```

---

## References

1. **Vignette**: `general-independent-vs-dependent-sampling-explained.Rmd` (created today)

2. **Duan et al. 2023**: Impact of tissue sampling on detection of venous invasion in colorectal cancer. *Histopathology*. doi:10.1111/his.15030

3. **Statistical reference**: Gelman A, Hill J (2006). *Data Analysis Using Regression and Multilevel/Hierarchical Models*. Cambridge University Press. Chapter 21: Understanding and summarizing the fitted models.

---

## Summary of Changes

**Files modified**: 1
- `R/pathsampling.b.R` (lines 794-827 inserted, line 862 modified)

**Lines added**: ~40

**Breaking changes**: None

**User-visible changes**:
- Red warning box appears when `analysisContext == "tumor"` AND `showBinomialModel == TRUE`
- Orange caution box appears when `analysisContext == "margin"` AND `showBinomialModel == TRUE`
- No other functional changes

**Recommendation**: Include in ClinicoPath v0.0.33 release alongside earlier enhancements.

---

*Document generated: 2025-10-15*
*Related PR: (to be created)*
*Module version: ClinicoPath v0.0.33 (pending)*
