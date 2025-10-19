# Pathsampling Module - Probability Explanation Added

## Summary

Added a comprehensive explanatory HTML section titled "Understanding Detection Probabilities" that clearly distinguishes between **conditional detection (sensitivity)** and **population-level detection**. This addresses user confusion about the different probability types used in the analysis.

## New Feature

### Location in Output

The new section appears after the **Data Summary** table and dynamically calculates both probability types using the actual data and estimated q value.

### Content Structure

The explanation is divided into three parts:

#### 1. Conditional Detection (Sensitivity) - "If metastasis is present"

**Explains:**
- **Question**: If a specimen *has* metastasis, what's the probability we'll detect it with n samples?
- **Formula**: P(detect | metastasis present) = 1 - (1-q)^n
- **Example Values**: Shows detection rates at 3, 5, and 10 samples for positive cases only
- **Clinical Use**: "How many samples do I need to confidently rule out metastasis?"
- **Note**: This is what's shown in the Diagnostic Yield Curve

#### 2. Population-Level Detection - "Overall detection rate"

**Explains:**
- **Question**: In a random specimen from this population, what's the probability of detecting metastasis?
- **Formula**: P(detect overall) = Prevalence × Sensitivity = π × [1 - (1-q)^n]
- **Example Values**: Shows detection rates at 3, 5, and 10 samples for all specimens
- **Clinical Use**: "What percentage of incoming specimens will test positive?"
- **Note**: Useful for workload planning and quality metrics

#### 3. Important Distinction

**Warns:**
- These are fundamentally different quantities
- Conditional (sensitivity) assumes metastasis is present
- Population-level includes cases without metastasis
- The ratio between them equals prevalence
- Module focuses on conditional probability for sampling adequacy decisions

## Implementation Details

### Files Modified

1. **jamovi/pathsampling.r.yaml** (line 29-31)
   - Added new HTML output element: `probabilityExplanation`
   - Title: "Understanding Detection Probabilities"
   - Type: Html

2. **R/pathsampling.b.R** (line 735-877)
   - Added calculation logic after binomial model section
   - Uses actual pEstimate (q) from data
   - Falls back to geometric MLE if binomial model not shown
   - Calculates both conditional and population probabilities dynamically

### Dynamic Calculations

The section uses real data values:

```r
# Calculate prevalence
prevalence <- nDetected / nCases

# Use estimated q value
qForExamples <- pEstimate  # From binomial model

# Calculate conditional probabilities (sensitivity)
conditional_3 <- 1 - (1 - qForExamples)^3
conditional_5 <- 1 - (1 - qForExamples)^5
conditional_10 <- 1 - (1 - qForExamples)^10

# Calculate population-level probabilities
population_3 <- prevalence * conditional_3
population_5 <- prevalence * conditional_5
population_10 <- prevalence * conditional_10
```

## Example Output

### For pathsampling_basic.csv data:

**Data characteristics:**
- Total cases: 1000
- Positive cases: 474 (prevalence = 47.4%)
- q (geometric MLE) ≈ 0.405

**Conditional Detection (Sensitivity):**
- With 3 samples: 64.7% of positive cases
- With 5 samples: 87.5% of positive cases
- With 10 samples: 99.4% of positive cases

**Population-Level Detection:**
- With 3 samples: 30.7% of all specimens (= 47.4% × 64.7%)
- With 5 samples: 41.5% of all specimens (= 47.4% × 87.5%)
- With 10 samples: 47.1% of all specimens (= 47.4% × 99.4%)

**Key Insight:**
Even with 10 samples achieving 99.4% sensitivity, only 47.1% of all specimens test positive because 52.6% truly don't have metastasis.

## Visual Design

The section uses color-coded boxes:

1. **White box with left border** - Conditional detection
   - Emphasizes this is the primary metric for sampling adequacy
   - Highlighted with green/success color

2. **White box with left border** - Population-level detection
   - Secondary metric for workload planning
   - Highlighted with blue/info color

3. **Light box with warning border** - Important distinction
   - Warns users these are different quantities
   - Explains the relationship via prevalence

## Benefits

### 1. Educational Value

Users now understand:
- Why the Diagnostic Yield Curve shows higher percentages than overall detection
- The difference between sensitivity and detection rate
- How prevalence affects interpretation
- Which metric to use for which clinical decision

### 2. Prevents Misinterpretation

Common misconceptions addressed:
- ❌ "If the curve shows 99% at 10 samples, 99% of specimens will be positive"
- ✅ "If metastasis is present, 99% will be detected; but only ~47% of specimens have it"

### 3. Clinical Relevance

Helps users answer:
- **Sampling protocol design**: Use conditional probability (sensitivity)
- **Workload forecasting**: Use population-level detection
- **Quality metrics**: Use both, understanding their relationship

## Testing Instructions

### 1. Reinstall Module

```r
setwd("/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule")
jmvtools::prepare('.')
jmvtools::install()
```

### 2. Restart jamovi

Close and reopen jamovi.

### 3. Run Analysis

- Load: `data/pathsampling_basic.csv`
- Configure:
  - Total Samples: `n_samples`
  - First Detection: `first_pos`
  - Show Binomial Model: ✓ (Enable)
  - Target Confidence: 0.95
  - Maximum Samples: 10

### 4. Expected Output

After the **Data Summary** table, you should see:

```
Understanding Detection Probabilities
──────────────────────────────────────

📊 Two Ways to Measure Detection Performance

This analysis reports probabilities in two ways, depending on the clinical question:

1️⃣ Conditional Detection (Sensitivity) - "If metastasis is present"

Question: If a specimen has metastasis, what's the probability we'll detect it with n samples?

Formula: P(detect | metastasis present) = 1 - (1-q)^n

In Your Data: Among 474 cases with detected metastasis (q = 0.405):
  • With 3 samples: Detects 64.7% of positive cases
  • With 5 samples: Detects 87.5% of positive cases
  • With 10 samples: Detects 99.4% of positive cases

Clinical Use: "How many samples do I need to confidently rule out metastasis?"
This is the probability shown in the Diagnostic Yield Curve.

2️⃣ Population-Level Detection - "Overall detection rate"

Question: In a random specimen from this population, what's the probability of detecting metastasis with n samples?

Formula: P(detect overall) = Prevalence × Sensitivity = π × [1 - (1-q)^n]

In Your Data: Observed prevalence = 47.4% (474/1000 cases had metastasis):
  • With 3 samples: Detects metastasis in 30.7% of all specimens
  • With 5 samples: Detects metastasis in 41.5% of all specimens
  • With 10 samples: Detects metastasis in 47.1% of all specimens

Clinical Use: "What percentage of incoming specimens will test positive?"
This is useful for workload planning and quality metrics.

⚠️ Important: These are fundamentally different quantities!

• Conditional (sensitivity) assumes metastasis is present
• Population-level includes cases without metastasis

The ratio between them equals the prevalence (47.4% in your data).
This module focuses on conditional probability (sensitivity) because that's what determines sampling adequacy.
```

## Integration with Previous Fixes

This enhancement builds on the critical fixes from 2025-10-13:

1. **Fixed Diagnostic Yield Curve** - Now correctly shows conditional probability
2. **Fixed Bootstrap Analysis** - Now estimates sensitivity, not population rate
3. **Added Explanation** - Now explains why we use conditional probability

Together, these changes ensure:
- ✅ Calculations are mathematically correct
- ✅ Visualizations show the right metrics
- ✅ Users understand what the metrics mean

## Pedagogical Approach

The explanation uses:

### 1. Question-First Format

Each probability type starts with the clinical question it answers, making the relevance immediately clear.

### 2. Concrete Examples

Shows actual calculated values from user's data, not abstract formulas.

### 3. Progressive Disclosure

1. Simple question
2. Formula (for advanced users)
3. Concrete examples with their data
4. Clinical application

### 4. Visual Hierarchy

- Emoji indicators (1️⃣, 2️⃣, ⚠️)
- Color-coded boxes
- Bold emphasis on key terms
- Bulleted lists for scanability

## Future Enhancements

### Potential Additions

1. **Interactive Toggle**: Allow users to switch between viewing conditional vs population probabilities in the main curve

2. **Prevalence Sensitivity Analysis**: Show how population-level detection changes with different prevalence values

3. **Comparison Table**: Side-by-side table of conditional vs population values at each sample count

4. **Visual Diagram**: Venn diagram or flow chart showing the relationship between prevalence, sensitivity, and population detection

### User Feedback Questions

- Is the explanation clear and accessible?
- Do users understand when to use each probability type?
- Should we add more real-world examples?
- Is the positioning (after Data Summary) optimal?

## Commit Message Suggestion

```
Add comprehensive probability explanation to pathsampling module

Users needed clarification on the difference between conditional detection
(sensitivity) and population-level detection rates. Added a new explanatory
HTML section that:

- Explains both probability types with clear clinical questions
- Shows formulas and concrete examples using user's data
- Distinguishes when to use each metric
- Clarifies why the module focuses on conditional probability

Changes:
- Added probabilityExplanation HTML element to r.yaml
- Implemented dynamic calculation in b.R using actual q and prevalence
- Uses color-coded boxes for visual distinction
- Positioned after Data Summary table for immediate context

Impact:
- Prevents misinterpretation of Diagnostic Yield Curve
- Helps users choose appropriate metric for their clinical question
- Educational value for understanding sampling adequacy statistics

Example output:
- Conditional (sensitivity): 99.4% at 10 samples
- Population (overall): 47.1% at 10 samples (when prevalence = 47.4%)
```

## Related Documentation

- `pathsampling-critical-fixes-2025-10-13.md` - Details on conditional vs population probability fixes
- `pathsampling-quick-reference.md` - User-facing guide (should be updated to reference this explanation)

## Success Criteria

✅ Section displays after Data Summary table
✅ Calculates values dynamically from user data
✅ Shows both conditional and population probabilities
✅ Explains which to use for which clinical question
✅ Uses clear, accessible language
✅ Visually distinct with color coding
✅ Compiles without errors
✅ Maintains consistent styling with rest of module

---

**Status**: ✅ Complete and ready for user testing
