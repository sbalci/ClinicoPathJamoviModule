# Pathsampling Enhanced Analysis - Quick Reference Guide

## Quick Start

### Basic Analysis (Original Functionality)
**Required Data**:
- `totalSamples`: Total cassettes submitted per case
- `firstDetection`: Cassette number where tumor first detected

**What You Get**:
- Minimum samples needed for 95% sensitivity
- Binomial probability model
- Bootstrap validation with confidence intervals
- Diagnostic yield curve

**Example**: "4 cassettes recommended for 95% detection probability"

---

### Enhanced Analysis (NEW - Requires Positive Cassette Data)
**Additional Data Needed**:
- `positiveCassettes`: Total number of cassettes with tumor per case

**Enable in UI**:
- Tumor Burden Analysis → ✅ Show tumor burden analysis
- Tumor Burden Analysis → ✅ Show stage migration analysis (ON by default)
- Tumor Burden Analysis → ✅ Show examined vs positive correlation

---

## Data Format Examples

### Example Case Data

| Case | Total Cassettes | First Detection | Positive Cassettes |
|------|----------------|-----------------|-------------------|
| 1    | 10             | 4               | 4                 |
| 2    | 8              | 2               | 6                 |
| 3    | 12             | 1               | 1                 |
| 4    | 6              | 3               | 3                 |

**Interpretation**:
- Case 1: Tumor first seen in cassette #4, total 4 cassettes positive (unifocal/oligofocal)
- Case 2: Tumor first seen in cassette #2, but 6 total positive (multifocal - tumor in later cassettes too)
- Case 3: Tumor first seen in cassette #1, only 1 positive (unifocal)
- Case 4: Tumor first seen in cassette #3, total 3 positive (oligofocal)

---

## Output Interpretation

### 1. Cassette Positivity Statistics

```
Measure                          | Value
--------------------------------|------------------
Mean cassette positivity ratio  | 0.287 (SD: 0.156)
Median cassette positivity ratio| 0.250
Overall cassette positivity     | 60 / 327 = 18.3%
```

**Interpretation**:
- **Mean CPR = 0.287**: On average, 28.7% of cassettes are positive
- **Median CPR = 0.250**: Half of cases have ≤25% cassettes positive
- **Overall = 18.3%**: Across all cases, 18.3% of all cassettes positive

**Clinical Meaning**:
- Low CPR (<0.2): Limited disease, possibly micro-metastases
- Moderate CPR (0.2-0.5): Substantial involvement
- High CPR (>0.5): Extensive disease

---

### 2. Tumor Distribution Pattern

```
Pattern                  | Cases | Percentage
------------------------|-------|------------
Unifocal (1 positive)   | 27    | 45%
Oligofocal (2-3 positive)| 21   | 35%
Multifocal (4+ positive)| 12    | 20%
```

**Interpretation**:
- **45% unifocal**: Nearly half have tumor in only 1 cassette (localized)
- **35% oligofocal**: One-third have tumor in 2-3 cassettes (moderate spread)
- **20% multifocal**: One-fifth have tumor in 4+ cassettes (extensive spread)

**Clinical Meaning**:
- High multifocal rate may indicate aggressive disease
- Distribution pattern may predict prognosis
- Useful for assessing tumor heterogeneity

---

### 3. Stage Migration Analysis

```
Cassettes Examined | Cases | Positive Cases | Positivity Rate
------------------|-------|----------------|----------------
<6                | 30    | 15             | 50.0%
≥6                | 30    | 27             | 90.0%
Absolute difference| -    | -              | 40.0%
```

**Interpretation**:
- **<6 cassettes**: Only 50% of cases detected as positive
- **≥6 cassettes**: 90% of cases detected as positive
- **40% difference**: Examining <6 cassettes misses 40% of positive cases!

**Clinical Meaning**:
- **Large difference (>15%)**: Inadequate sampling causes significant understaging
- **Validates minimum recommendations**: This data strongly supports ≥6 cassette requirement
- **Quality assurance**: Institutional monitoring - are we examining enough cassettes?

---

### 4. Correlation Analysis

```
Statistic                         | Value
----------------------------------|-------------------------
Spearman's rho                    | 0.620
p-value                           | <0.001
Interpretation                    | Significant positive correlation
```

**Correlation Plot**: Scatter plot showing examined vs positive cassettes with regression line

**Interpretation**:
- **ρ = 0.620**: Strong positive correlation
- **p < 0.001**: Highly statistically significant
- **Meaning**: More cassettes examined → more positive cassettes detected

**Clinical Meaning**:
- **Positive correlation (ρ > 0.3, p < 0.05)**: Validates thoroughness principle
- **No correlation (p > 0.05)**: May indicate sampling bias or clustered disease
- **Negative correlation**: Data quality issue - investigate immediately!

---

## Decision Tree: Which Analyses to Use?

### Do you have `positiveCassettes` data?

**NO** → Use original analysis only
- Minimum sampling recommendations
- Binomial model + bootstrap
- Diagnostic yield curve
- **Perfect for**: Determining how many samples needed

**YES** → Enable enhanced analyses

#### Which enhanced analyses should you use?

**Always Enable**:
- ✅ **Stage Migration Analysis** (ON by default)
  - Validates your minimum sampling recommendations
  - Critical for quality assurance
  - Most clinically important

**Enable When**:
- ✅ **Tumor Burden Analysis**: When extent of disease matters
  - Grading/staging implications
  - Prognostic studies
  - Research on tumor heterogeneity

- ✅ **Correlation Analysis**: When investigating sampling practices
  - Research studies
  - Exploratory analysis
  - Validating sampling thoroughness

---

## Common Clinical Scenarios

### Scenario 1: Developing Institutional Guidelines
**Question**: "How many omental cassettes should we require?"

**Use**:
1. Original analysis → Get minimum samples for 95% sensitivity
2. Stage migration → Validate that fewer samples cause understaging
3. Bootstrap CI → Ensure recommendation is robust

**Report**: "Based on 60 cases, 4 cassettes recommended (95% sensitivity). Stage migration analysis shows examining <6 cassettes misses 40% of positive cases, validating this recommendation."

---

### Scenario 2: Quality Assurance Monitoring
**Question**: "Are our pathologists examining enough cassettes?"

**Use**:
1. Stage migration analysis → Compare detection rates
2. Correlation analysis → Test if more sampling = more detection
3. Tumor burden → Assess if we're missing multifocal disease

**Report**: "40% difference in positivity rates between <6 and ≥6 cassettes examined. Strong positive correlation (ρ=0.62, p<0.001) validates thoroughness principle. Recommendation: Enforce minimum 6 cassette examination."

---

### Scenario 3: Research Publication
**Question**: "What's the optimal sampling for omental metastases?"

**Use**:
1. All analyses enabled
2. Compare to published studies (Skala & Hagemann 2015)
3. Provide comprehensive statistics

**Report**: "Binomial model predicts 4 cassettes for 95% sensitivity (bootstrap CI: 3-5). Mean cassette positivity ratio 0.287. Stage migration analysis demonstrates 40% understaging with <6 cassettes. Spearman correlation 0.62 (p<0.001) supports thoroughness principle. Recommendation: Minimum 4 cassettes, optimal 6+ for multifocal disease detection."

---

### Scenario 4: Tumor Biology Study
**Question**: "Does tumor distribution pattern predict outcomes?"

**Use**:
1. Tumor burden analysis → Classify unifocal/oligofocal/multifocal
2. (Future) Survival integration → Test prognostic value

**Report**: "20% of cases showed multifocal distribution (≥4 positive cassettes). Mean CPR in multifocal cases: 0.65 vs 0.12 in unifocal (p<0.001). Distribution pattern may represent biological aggressiveness warranting survival analysis."

---

## Troubleshooting

### "I enabled tumor burden but nothing appears"
**Cause**: `positiveCassettes` variable not selected
**Solution**: Drag `positiveCassettes` variable to the variables box

### "Correlation shows negative relationship"
**Cause**: Possible data entry error or unexpected pattern
**Solution**:
1. Check data quality - are values entered correctly?
2. Plot the scatter plot - look for outliers
3. Verify `positiveCassettes` ≤ `totalSamples` for all cases

### "Stage migration shows no difference"
**Cause**: May indicate uniform sampling or limited data variation
**Solution**:
1. Check median split - is there enough variation in cassette counts?
2. Consider custom thresholds (future enhancement)
3. May actually be good news - consistent sampling across cases!

### "Multifocal percentage seems too high/low"
**Cause**: Cutoffs (1, 2-3, 4+) may not suit your data
**Solution**:
1. Current implementation uses standard cutoffs
2. Review individual case data
3. Consider what's clinically meaningful for your specific context

---

## Default Settings

### Variables
- `totalSamples`: Required for original analysis
- `firstDetection`: Required for original analysis
- `positiveCassettes`: **Optional** - enables enhanced analyses

### Analysis Parameters
- Target confidence: **95%** (diagnostic test standard)
- Maximum samples: **10** (typical range for evaluation)
- Bootstrap iterations: **10,000** (robust estimates)
- Random seed: **42** (reproducibility)

### Display Options
- Show binomial model: ✅ **ON**
- Show bootstrap: ✅ **ON**
- Show detection curve: ✅ **ON**
- Show sensitivity CI: ✅ **ON**

### Tumor Burden Options
- Show tumor burden: ❌ **OFF** (requires positiveCassettes)
- Show stage migration: ✅ **ON** (important for validation)
- Show correlation: ❌ **OFF** (exploratory, enable if needed)

---

## Literature References

### Original Methods
**Skala & Hagemann (2015)** - *Int J Gynecol Pathol*
- Bootstrap methodology for pathology sampling
- Omental cassette adequacy analysis
- Recommends 4 cassettes for 95% sensitivity

### Validation Studies
**Goess et al. (2024)** - *J Surg Oncol*
- Lymph node adequacy analysis
- Identical binomial approach
- Median-based stage migration analysis

**Habib et al. (2024)** - *Ann Surg Oncol*
- IPMN lymph node analysis
- Dual threshold methodology (minimal vs optimal)
- 22% stage migration difference documented

### Statistical Methods
**Buderer (1996)** - *Stat Med*
- Sample size for diagnostic tests
- Sensitivity/specificity calculations

---

## Tips for Effective Use

### 1. Start Simple
- First run: Original analysis only (first detection)
- Understand minimum sampling requirements
- Review bootstrap confidence intervals

### 2. Add Complexity Gradually
- Second run: Enable stage migration
- Validate that inadequate sampling causes understaging
- Use to support recommendations

### 3. Full Analysis for Research
- Enable all analyses
- Generate comprehensive statistics
- Compare to published literature
- Publication-ready results

### 4. Always Check Plots
- **Diagnostic yield curve**: Should show plateau (diminishing returns)
- **Sensitivity CI plot**: Should show narrowing confidence intervals
- **Correlation plot**: Should show clear pattern (or lack thereof)

### 5. Report Confidence Intervals
- Don't just report point estimates
- Bootstrap CIs show uncertainty
- Important for small sample sizes
- Example: "4 cassettes (95% CI: 3-5)"

---

## Quick Command Reference

### In jamovi
1. **Select variables**:
   - Total number of samples taken → totalSamples
   - Sample number where lesion first detected → firstDetection
   - Number of cassettes with tumor (optional) → positiveCassettes

2. **Set parameters** (optional):
   - Target confidence level: 0.95 (default)
   - Maximum samples to evaluate: 10 (default)
   - Bootstrap iterations: 10000 (default)

3. **Enable analyses**:
   - Display Options → all ON by default
   - Tumor Burden Analysis → enable as needed

4. **Interpret results**:
   - Read "Clinical Recommendations" section first
   - Review plots for visual understanding
   - Check "Statistical Interpretation" for details

---

## Example Report Template

```
PATHOLOGY SAMPLING ADEQUACY ANALYSIS
=====================================

Study: [Your Study Name]
Tissue: [e.g., Omentum, Lymph Nodes]
Cases: [n=XX]
Date: [Date]

MINIMUM SAMPLING RECOMMENDATION
-------------------------------
Recommended minimum: X cassettes (95% confidence)
Based on: Binomial model with bootstrap validation
Bootstrap 95% CI: [X-X] cassettes

OBSERVED DETECTION RATES
------------------------
1 cassette: XX%
2 cassettes: XX%
3 cassettes: XX%
4 cassettes: XX% ← Recommended minimum

TUMOR BURDEN (optional)
----------------------
Mean cassette positivity ratio: X.XXX (SD: X.XXX)
Tumor distribution:
  - Unifocal (1 positive): XX%
  - Oligofocal (2-3 positive): XX%
  - Multifocal (4+ positive): XX%

STAGE MIGRATION ANALYSIS
------------------------
<X cassettes: XX% positivity rate
≥X cassettes: XX% positivity rate
Absolute difference: XX%

Interpretation: [Minimal/Moderate/Significant] stage migration effect.
Examining fewer than X cassettes results in XX% understaging.

CONCLUSION
----------
[Your interpretation and recommendations]

REFERENCES
----------
Methods based on:
- Skala & Hagemann (2015) - Int J Gynecol Pathol
- Goess et al. (2024) - J Surg Oncol
- Habib et al. (2024) - Ann Surg Oncol
```

---

## Getting Help

### Documentation
- `pathsampling-enhanced-implementation.md` - Full technical documentation
- `pathsampling-censored-vs-complete-data.md` - Data requirements discussion
- This document - Quick reference for daily use

### Questions to Ask
1. "Do I have positive cassette data?" → Determines which analyses available
2. "What's my clinical question?" → Determines which analyses to enable
3. "Is this for guidelines or research?" → Determines reporting detail level
4. "What do the confidence intervals show?" → Assesses recommendation robustness

### Common Workflow
1. Load data
2. Run original analysis → Get minimum recommendation
3. Enable stage migration → Validate recommendation
4. If needed, enable tumor burden/correlation → Additional insights
5. Generate report → Use template above

---

**Last Updated**: October 10, 2025
**Version**: 2.0 (Enhanced with tumor burden analysis)
**Status**: Production Ready
