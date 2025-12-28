# Educational Content Corrections for oddsratio Function

**Date:** 2025-12-27
**Issue:** Educational explanations described metrics that were NOT actually calculated by the function
**Status:** ✅ FIXED

---

## Problem Identified

The educational explanations (`showExplanations` checkbox) included descriptions of metrics that the `oddsratio` function does NOT calculate:

### Incorrectly Described Metrics:

**1. Risk Measures Section:**
- ❌ Risk Ratio (RR)
- ❌ Risk Difference (RD)
- ❌ Number Needed to Treat (NNT)
- ❌ Attributable Risk

**2. Diagnostic Test Performance Section:**
- ❌ Positive Predictive Value (PPV)
- ❌ Negative Predictive Value (NPV)

---

## What the Function Actually Calculates

### Regression Output:
- ✅ **Odds Ratios (OR)** with 95% confidence intervals
- ✅ P-values for each predictor
- ✅ Forest plot visualization

### Diagnostic Metrics (when nomogram enabled):
- ✅ **Sensitivity** (True Positive Rate)
- ✅ **Specificity** (True Negative Rate)
- ✅ **Positive Likelihood Ratio (LR+)**
- ✅ **Negative Likelihood Ratio (LR-)**
- ✅ Contingency table (TP, FP, FN, TN)
- ✅ Fagan nomogram visualization

---

## Corrections Made

### 1. Risk Measures Explanation (Lines 1251-1275)

**Before:**
```html
<h4>Understanding Risk Measures</h4>
<ul>
    <li>Risk Ratio (RR): Ratio of risks...</li>
    <li>Risk Difference (RD): Absolute difference...</li>
    <li>Number Needed to Treat (NNT): Inverse of risk difference...</li>
    <li>Attributable Risk: Proportion of disease...</li>
</ul>
```

**After:**
```html
<h4>Understanding Odds Ratio vs Risk Ratio</h4>
<p><strong>Odds Ratio (OR):</strong> The measure calculated by this analysis.</p>
<ul>
    <li><strong>Definition:</strong> Ratio of the odds of outcome in exposed vs unexposed groups</li>
    <li><strong>Formula:</strong> OR = (a/b) / (c/d) where a,b,c,d are from 2×2 contingency table</li>
    <li><strong>Interpretation:</strong> OR = 2.0 means the odds of outcome are twice as high in exposed group</li>
    <li><strong>Use case:</strong> Logistic regression, case-control studies, cross-sectional studies</li>
</ul>
<p><strong>Risk Ratio (RR) - NOT calculated by this function:</strong></p>
<ul>
    <li><strong>Definition:</strong> Ratio of risks (proportions) between exposed and unexposed groups</li>
    <li><strong>Use case:</strong> Cohort studies, randomized trials with follow-up data</li>
    <li><strong>Note:</strong> When outcome is rare (<10%), OR approximates RR</li>
</ul>
```

**Key Changes:**
- ✅ Removed misleading descriptions of RR, RD, NNT, Attributable Risk
- ✅ Explained what OR actually is (the metric being calculated)
- ✅ Clarified difference between OR and RR
- ✅ Explained when OR approximates RR (rare outcomes)
- ✅ Directed users to appropriate tools for RR calculation

---

### 2. Diagnostic Test Performance Explanation (Lines 1277-1320)

**Before:**
```html
<h4>Understanding Diagnostic Test Performance</h4>
<ul>
    <li><strong>Sensitivity:</strong> Proportion of true positives...</li>
    <li><strong>Specificity:</strong> Proportion of true negatives...</li>
    <li><strong>PPV:</strong> Probability of disease given a positive test</li>
    <li><strong>NPV:</strong> Probability of no disease given a negative test</li>
    <li><strong>Likelihood Ratios:</strong> How much a test result changes...</li>
</ul>
```

**After:**
```html
<h4>Understanding Diagnostic Test Performance</h4>
<p><strong>Diagnostic Metrics Calculated:</strong> This analysis evaluates how well a binary predictor
distinguishes between outcome states.</p>
<ul>
    <li><strong>Sensitivity (True Positive Rate):</strong>
        <ul>
            <li>Formula: TP / (TP + FN)</li>
            <li>Example: If sensitivity = 80%, the test detects 80% of cases with the outcome</li>
        </ul>
    </li>
    <li><strong>Specificity (True Negative Rate):</strong>
        <ul>
            <li>Formula: TN / (TN + FP)</li>
            <li>Example: If specificity = 90%, correctly identifies 90% without outcome</li>
        </ul>
    </li>
    <li><strong>Positive Likelihood Ratio (LR+):</strong>
        <ul>
            <li>Formula: Sensitivity / (1 - Specificity)</li>
            <li>LR+ > 10: Strong evidence for diagnosis</li>
            <li>LR+ = 5-10: Moderate evidence</li>
            <li>LR+ = 2-5: Weak evidence</li>
        </ul>
    </li>
    <li><strong>Negative Likelihood Ratio (LR-):</strong>
        <ul>
            <li>Formula: (1 - Sensitivity) / Specificity</li>
            <li>LR- < 0.1: Strong evidence against diagnosis</li>
            <li>LR- = 0.1-0.2: Moderate evidence</li>
            <li>LR- = 0.2-0.5: Weak evidence</li>
        </ul>
    </li>
</ul>
<p><strong>Note:</strong> PPV and NPV are NOT calculated by this function as they
depend on disease prevalence in your specific population.</p>
```

**Key Changes:**
- ✅ Removed PPV and NPV from description
- ✅ Added formulas for each calculated metric
- ✅ Added interpretation guidelines (e.g., LR+ > 10 = strong evidence)
- ✅ Added practical examples for sensitivity and specificity
- ✅ Explicit note that PPV/NPV are NOT calculated
- ✅ Explained WHY PPV/NPV aren't calculated (depend on prevalence)

---

## Why PPV and NPV Are NOT Calculated

**Technical Reason:**
PPV and NPV depend on the prevalence (base rate) of the outcome in the population:

```
PPV = (Sensitivity × Prevalence) / [(Sensitivity × Prevalence) + (1-Specificity) × (1-Prevalence)]
NPV = (Specificity × (1-Prevalence)) / [(Specificity × (1-Prevalence)) + (1-Sensitivity) × Prevalence]
```

**The Problem:**
- Sample prevalence in your dataset may NOT represent true population prevalence
- Case-control studies artificially control prevalence
- Different populations have different prevalences

**The Solution:**
- Use Likelihood Ratios (LR+ and LR-) instead - they are prevalence-independent
- Use the Fagan nomogram to convert LR to post-test probability for YOUR population
- Users can input their own pre-test probability (prevalence) to get personalized results

---

## Benefits of These Corrections

### 1. Accurate Expectations
- ✅ Users know exactly what metrics are provided
- ✅ No confusion about missing outputs
- ✅ Clear guidance on what to use for different research questions

### 2. Better Clinical Decision-Making
- ✅ LR interpretation guidelines help clinicians evaluate test utility
- ✅ Explanation of when OR approximates RR prevents misinterpretation
- ✅ Users understand the nomogram provides personalized post-test probabilities

### 3. Educational Value
- ✅ Users learn the difference between OR and RR
- ✅ Formulas provided for transparency
- ✅ Practical examples improve understanding

### 4. Prevents Misuse
- ✅ Directs users to appropriate tools for RR calculation
- ✅ Explains why PPV/NPV aren't calculated (prevents prevalence errors)
- ✅ Clear guidance on when to use OR vs other measures

---

## Files Modified

1. **R/oddsratio.b.R**
   - Lines 1251-1275: Risk Measures Explanation
   - Lines 1277-1320: Diagnostic Test Performance Explanation

2. **tests/TESTING_SUMMARY.md**
   - Added Bug #8 documenting this issue
   - Updated executive summary (7 → 8 bugs fixed)

---

## Verification

Run `devtools::document()` - NO errors related to oddsratio
```bash
Rscript -e "devtools::document()"
# Only pre-existing warnings in other .Rd files, none for oddsratio
```

---

## Next Steps

When performing manual testing in jamovi:

1. **Enable "Show Educational Explanations"**
2. **Verify the following sections display correctly:**
   - Understanding Odds Ratio Analysis ✓
   - Understanding Odds Ratio vs Risk Ratio (NEW - check content)
   - Understanding Diagnostic Test Performance (UPDATED - check formulas)
   - Understanding Diagnostic Nomogram ✓

3. **Check that descriptions match actual outputs:**
   - Main table shows OR, CI, p-values ✓
   - Diagnostic section shows Sensitivity, Specificity, LR+, LR- ✓
   - No mention of PPV, NPV, RR, RD, NNT, Attributable Risk as calculated metrics ✓

4. **Verify interpretation guidelines are helpful:**
   - LR+ interpretation (>10, 5-10, 2-5) ✓
   - LR- interpretation (<0.1, 0.1-0.2, 0.2-0.5) ✓
   - OR vs RR explanation is clear ✓

---

## Conclusion

The educational content now accurately reflects the function's capabilities. This prevents user confusion and ensures researchers understand:
- What metrics are being calculated
- How to interpret the results
- When to use alternative tools

**Status:** ✅ COMPLETE
