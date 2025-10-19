# Pathology Sampling Adequacy - Quick Start Guide

## What This Module Does

Determines the minimum number of pathology samples needed to reliably detect findings (e.g., tumor metastasis in omentum, lymph nodes, margins).

## Quick Decision Tree

```
Do you have your data?
│
├─ YES → Go to "Choose Your Data Level"
│
└─ NO → Go to "Generate Test Data"
```

## Choose Your Data Level

### Level 1: Basic (Minimum Required)

**You have:**
- Total samples examined per case
- Position where first positive finding occurred (or NA if all negative)

**You get:**
- Per-sample detection probability (q) - Geometric MLE method
- Detection probability for different sample thresholds
- Recommended minimum samples for target confidence
- Bootstrap confidence intervals
- Detection curve visualization

**CSV Format:**
```csv
case_id,total_samples,first_positive
1,10,3
2,8,NA
3,12,1
```

### Level 2: Enhanced (Recommended)

**You have:**
- Everything from Level 1 +
- Count of positive samples found

**You get:**
- Everything from Level 1 +
- **Better q estimation** (Empirical Proportion method)
- **Empirical cumulative detection** - Direct observation of detection rates
- **Incremental yield analysis** - Cost-benefit of additional samples

**CSV Format:**
```csv
case_id,total_samples,first_positive,positive_count
1,10,3,3
2,8,NA,0
3,12,1,5
```

### Level 3: Complete (Advanced)

**You have:**
- Everything from Level 2 +
- List of all positive sample positions

**You get:**
- Everything from Level 2 +
- **Spatial clustering analysis** - Are positives clustered or dispersed?
- **Multifocal detection patterns** - Relationship between foci count and detection

**CSV Format:**
```csv
case_id,total_samples,first_positive,positive_count,positive_list
1,10,3,3,"3,5,7"
2,8,NA,0,NA
3,12,1,5,"1,2,3,4,5"
```

### Level 4: Stratified (Tumor Biology)

**You have:**
- Level 2 or 3 data +
- Sample type/subtype grouping

**You get:**
- Everything from Level 2 or 3 +
- **Prevalence by type** - Which types are more common?
- **Type-specific q estimates** - Which types need more samples?
- **Type-specific detection curves** - Separate recommendations per type
- **Population-level detection** - Overall detection accounting for prevalence

**CSV Format:**
```csv
case_id,sample_type,total_samples,first_positive,positive_count
1,Serous,10,3,3
2,Serous,8,NA,0
3,Endometrioid,12,5,2
4,Clear Cell,10,2,4
```

## 5-Minute Setup

### Step 1: Prepare Your Data

**Required columns:**
1. `total_samples` - Number of samples examined (numeric)
2. `first_positive` - Sample number where first positive found (numeric, NA if negative)

**Optional columns for enhanced analysis:**
3. `positive_count` - Total positive samples (numeric, 0 if negative)
4. `positive_list` - Comma-separated positions like "3,5,7" (text, NA if negative)
5. `sample_type` - Categorical grouping (text/factor)

### Step 2: Load in jamovi

1. Open jamovi
2. File → Import → Your CSV file
3. Analyses → OncoPathT → ClinicoPath Descriptives → Pathology Sampling Adequacy Analysis

### Step 3: Configure Analysis

**Minimum setup:**
- Total Samples: Select your total_samples column
- First Detection: Select your first_positive column

**Enhanced setup (if available):**
- Positive Samples Count: Select your positive_count column
- Positive Samples List: Select your positive_list column
- Sample Type: Select your sample_type column

**Options:**
- Target Confidence: 0.95 (default, means 95% detection probability)
- Maximum Samples: 10 (default, adjust based on your data range)
- Bootstrap Iterations: 10000 (default, reduce if slow)

**What to show:**
- ✅ Show Binomial Model
- ✅ Show Bootstrap Analysis
- ✅ Show Diagnostic Yield Curve
- ✅ Show Empirical Cumulative (if you have positive_count)
- ✅ Show Incremental Yield (if you have positive_count)
- ✅ Show Stratified Analysis (if you have sample_type)

### Step 4: Interpret Results

**Key result: Data Summary table**
```
Total cases analyzed: 1000
Cases with positive findings: 474
Overall prevalence: 47.4%
Per-sample detection probability (q): 0.385
Estimation method: Empirical Proportion
```

**Key result: Binomial Model Predictions**
```
Samples | Detection Probability | Marginal Gain
3       | 79.8%                | 31.2%
5       | 91.4%                | 11.6%
7       | 96.2%                | 4.8%
10      | 98.7%                | 2.5%
```

**Interpretation:**
- **q = 0.385** means each sample has 38.5% chance of containing tumor (if tumor present)
- **5 samples** detects 91.4% of positive cases
- **7 samples** detects 96.2% (only 4.8% additional gain)
- **Recommendation**: 7 samples for >95% confidence

## Common Questions

### Q1: What if I only have first positive position?

**Answer:** That's fine! The module will use Geometric MLE method to estimate q. You'll get all basic analyses (detection curves, recommendations, bootstrap CIs).

**Limitation:** Geometric MLE is less accurate than Empirical Proportion, but still statistically valid.

### Q2: What if some cases have positive_count but others don't?

**Answer:** The module will use available data. Cases with positive_count contribute to empirical estimation. Others are excluded from q calculation but included in prevalence.

### Q3: How do I record positive_list for clustered findings?

**Answer:** Just list the sample positions separated by commas.

**Examples:**
- Single positive at sample 5: `"5"`
- Three positives: `"3,5,7"`
- Clustered positives: `"1,2,3"`
- All negative: `NA` or leave blank

### Q4: What sample_type categories should I use?

**Answer:** Any biologically meaningful grouping:

**For ovarian cancer omentum:**
- Serous, Endometrioid, Clear Cell, Mucinous

**For lymph nodes:**
- Primary site (Colon, Pancreas, Lung)
- Stage (T1, T2, T3, T4)
- Grade (Low, High)

**For margins:**
- Tissue type (Breast, Prostate, Colon)
- Margin type (Deep, Peripheral, Radial)

### Q5: How many cases do I need?

**Answer:**
- **Minimum**: 20-30 positive cases for basic q estimation
- **Recommended**: 50+ positive cases for reliable estimates
- **Ideal**: 100+ positive cases for stratified analysis

### Q6: Can I use this for non-oncology sampling?

**Answer:** Yes! Any scenario where you're sampling to detect a finding:
- Infection detection (cultures, biopsies)
- Reject detection (transplant biopsies)
- Dysplasia detection (Barrett's, IBD surveillance)
- Foreign body detection

## Troubleshooting

### Error: "Insufficient positive cases for q estimation"

**Cause:** Less than 5 cases with positive findings

**Solution:**
- Collect more data, OR
- Use literature-based q estimate as reference

### Warning: "Empirical method selected but positiveCount missing"

**Cause:** You selected estimationMethod = "empirical" but didn't provide positive_count variable

**Solution:**
- Change estimationMethod to "auto" or "geometric", OR
- Provide positive_count column

### Plot not displaying

**Cause:** Not enough data points for visualization

**Solution:**
- Need at least 10 cases with positive findings
- Check that first_positive values are numeric, not text

### Bootstrap taking too long

**Cause:** Too many bootstrap iterations or large dataset

**Solution:**
- Reduce bootstrapIterations from 10000 to 1000
- Results are still valid with 1000 iterations

## Example Use Cases

### Use Case 1: Omentum Sampling in Ovarian Cancer

**Clinical Question:** How many omental sections needed to detect metastasis?

**Data Level:** Enhanced (total_samples, first_positive, positive_count)

**Sample Type:** Tumor histology (Serous, Endometrioid, Clear Cell)

**Key Findings:**
- Serous: q=0.49, needs 5 sections for 95% detection
- Endometrioid: q=0.35, needs 7 sections for 95% detection
- Clear Cell: q=0.41, needs 6 sections for 95% detection

**Recommendation:** Standardize on 7 sections for all cases (covers worst case)

### Use Case 2: Lymph Node Sampling in Pancreatic Cancer

**Clinical Question:** How many lymph nodes needed for adequate staging?

**Data Level:** Complete (includes positive_list for clustering analysis)

**Sample Type:** None (single tumor type)

**Key Findings:**
- q=0.28 (lymph node metastasis probability)
- 12 nodes achieves 95% detection
- Clustering analysis shows 65% random distribution
- Incremental yield drops below 2% after 15 nodes

**Recommendation:** Target 12-15 lymph nodes per resection

### Use Case 3: Margin Sampling in Breast Conservation

**Clinical Question:** How many margin sections needed to detect positive margin?

**Data Level:** Stratified (by breast density category)

**Sample Type:** Breast density (Low, Moderate, High)

**Key Findings:**
- Low density: q=0.42, 6 sections for 95%
- Moderate density: q=0.35, 7 sections for 95%
- High density: q=0.29, 9 sections for 95%

**Recommendation:** Adjust sampling based on breast density

## Export Results

### For Publication

**Tables to export:**
1. Data Summary (prevalence, q estimate)
2. Binomial Model Predictions (detection by threshold)
3. Minimum Samples for Target Confidence

**Figures to export:**
1. Diagnostic Yield Curve (detection vs samples)
2. Sensitivity with Confidence Intervals (bootstrap)
3. Empirical Cumulative Detection Curve (if available)

### For Quality Improvement

**Tables to export:**
1. Incremental Yield (cost-benefit analysis)
2. Stage Migration Analysis (detection rate by threshold)
3. Stratified Detection (if using sample types)

## Next Steps

### After Getting Results

1. **Compare to literature values** - Is your q similar to published studies?
2. **Calculate institutional compliance** - What % of cases meet recommended threshold?
3. **Estimate cost impact** - Cost of additional sampling vs. missed diagnoses
4. **Implement protocol changes** - Update sampling guidelines
5. **Monitor over time** - Re-analyze annually to track compliance

### For Protocol Development

**Recommendation template:**

> Based on analysis of [N] cases at [Institution], we recommend [X] samples for [specimen type] to achieve [Y]% detection sensitivity. This recommendation is based on an estimated per-sample detection probability (q) of [Z], consistent with [literature references].

**Example:**

> Based on analysis of 1000 ovarian cancer cases at UCSD, we recommend 7 omental sections to achieve 95% detection sensitivity. This recommendation is based on an estimated per-sample detection probability (q) of 0.35-0.49 depending on histologic subtype, consistent with Maglalang & Fadare (2025).

## Quick Reference Card

### Data Structure
| Column | Type | Required | Example |
|--------|------|----------|---------|
| total_samples | Numeric | ✅ Yes | 10 |
| first_positive | Numeric | ✅ Yes | 3 or NA |
| positive_count | Numeric | Recommended | 3 or 0 |
| positive_list | Text | Optional | "3,5,7" |
| sample_type | Factor | Optional | "Serous" |

### Analysis Selection Guide
| Analysis | Requires | Answers |
|----------|----------|---------|
| Binomial Model | Basic data | What's the detection probability for n samples? |
| Bootstrap CI | Basic data | How confident are we in the estimates? |
| Empirical Cumulative | positive_count | What's the observed detection rate? |
| Incremental Yield | positive_count | What's the benefit of one more sample? |
| Stratified | sample_type | Do different types need different protocols? |
| Spatial Clustering | positive_list | Are findings clustered or dispersed? |
| Multifocal | positive_list | Does number of foci affect detection? |

### Key Statistics Explained
| Statistic | Symbol | Meaning | Typical Range |
|-----------|--------|---------|---------------|
| Per-sample probability | q | Chance sample contains finding (if present) | 0.1-0.6 |
| Prevalence | π | Proportion of positive cases | 0.2-0.8 |
| Sensitivity | Se | Detection rate given finding present | 0.8-0.99 |
| Population detection | π×Se | Overall detection rate | 0.2-0.7 |

### Interpretation Rules
| q value | Interpretation | Samples for 95% |
|---------|----------------|-----------------|
| > 0.5 | High probability | 5-6 |
| 0.3-0.5 | Moderate | 6-9 |
| 0.1-0.3 | Low | 10-20 |
| < 0.1 | Very low | >20 |

## Test Data

### Generate Test Data in R

```r
# Source the enhanced simulation
source("development-ideas/omentum-analysis-simulation-enhanced.R")

# This generates 4 CSV files in data/:
# - pathsampling_basic.csv (Level 1)
# - pathsampling_enhanced.csv (Level 2)
# - pathsampling_complete.csv (Level 3)
# - pathsampling_simulation_full.csv (Validation)
```

### Load Test Data in jamovi

1. File → Import → Browse to `data/pathsampling_enhanced.csv`
2. Analyses → OncoPathT → Pathology Sampling Adequacy Analysis
3. Configure:
   - Total Samples: n_samples
   - First Detection: first_pos
   - Positive Samples Count: pos_count
   - Sample Type: sample_type (for complete dataset)
4. Enable all analysis options
5. Review results

## Support

**Documentation:** See `pathsampling-refactoring-summary.md` for complete technical details

**Simulation code:** See `omentum-analysis-simulation-enhanced.R` for data generation

**Module location:** `jamovi/pathsampling.*.yaml` and `R/pathsampling.b.R`

**Testing checklist:** See testing section in refactoring summary document
