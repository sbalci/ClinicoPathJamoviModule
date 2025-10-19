# Installing Updated Path Sampling Module for Testing

## The Problem

You're seeing this error:
```
Error: 'keyResults' does not exist in this results element
```

This occurs because jamovi is loading an old version of the ClinicoPath module that doesn't include the refactored pathsampling function with the new `keyResults` element.

## The Solution

You need to rebuild and reinstall the module in jamovi.

### Option 1: Quick R Package Install (For R Testing Only)

If you want to test in R (not jamovi GUI):

```r
# In R console, from the module directory:
devtools::document()
devtools::install(upgrade = "never")

# Then test:
library(ClinicoPath)
data <- read.csv("data/pathsampling_enhanced.csv")
result <- ClinicoPath::pathsampling(
    data = data,
    totalSamples = "n_samples",
    firstDetection = "first_pos",
    positiveCount = "pos_count",
    maxSamples = 10
)
```

### Option 2: Build and Install .jmo File (For jamovi GUI)

To test in jamovi, you need to build the .jmo file:

```r
# In R console:
setwd("/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule")
jmvtools::prepare(".")
jmvtools::install()
```

Then in jamovi:
1. Modules → jamovi library → Available
2. Find "ClinicoPath" and click "Install" or "Update"
3. Restart jamovi if needed

### Option 3: Manual .jmo Installation

If automatic installation doesn't work:

```r
# Build the .jmo file
jmvtools::check(".")
```

This creates `build/ClinicoPath.jmo`

Then:
1. Open jamovi
2. Modules → jamovi library → Sideload
3. Browse to `build/ClinicoPath.jmo`
4. Install
5. Restart jamovi

## Verify Installation

After installation, check the version:

In jamovi:
1. Modules → jamovi library → Installed
2. Find ClinicoPath
3. Version should be **0.0.32.03** or later

## Test with Generated Data

1. Open jamovi
2. File → Import → `data/pathsampling_enhanced.csv`
3. Analyses → OncoPathT → ClinicoPath Descriptives → Pathology Sampling Adequacy Analysis
4. Configure:
   - Total Samples: n_samples
   - First Detection: first_pos
   - Positive Samples Count: pos_count
   - Target Confidence: 0.95
   - Maximum Samples: 10
5. Enable:
   - ✓ Show Binomial Model
   - ✓ Show Bootstrap Analysis
   - ✓ Show Empirical Cumulative Detection
   - ✓ Show Incremental Yield
6. Check: q Estimation Method = Auto

Expected results:
- Data Summary table with prevalence and q estimate
- Binomial Model Predictions table
- Empirical Detection Rates table (NEW)
- Incremental Yield table (NEW)
- No errors about missing 'keyResults'

## Troubleshooting

### Error: "keyResults does not exist"

**Cause**: Old module version still loaded

**Solution**:
1. Completely quit jamovi
2. Rebuild module: `jmvtools::install()`
3. Restart jamovi
4. Verify version number

### Error: "positiveCount not found in data"

**Cause**: Using wrong test dataset

**Solution**: Use `pathsampling_enhanced.csv` which includes pos_count column

### Warning: "Empirical method selected but positiveCount missing"

**Cause**: Either:
- Selected wrong variable for positiveCount, OR
- estimationMethod set to "empirical" but no data provided

**Solution**: Set estimationMethod to "auto" (will fallback to Geometric MLE)

### Module won't install

**Cause**: Permission issues or jamovi library locked

**Solution**:
1. Close ALL jamovi instances
2. Remove old module manually:
   - macOS: `~/Library/Application Support/jamovi/modules/ClinicoPath`
   - Windows: `%APPDATA%\jamovi\modules\ClinicoPath`
3. Reinstall using jmvtools::install()

## Quick Test Script (R Only)

Save as `quick_test.R`:

```r
library(ClinicoPath)

# Load test data
data <- read.csv("data/pathsampling_enhanced.csv")

# Run analysis
result <- ClinicoPath::pathsampling(
    data = data,
    totalSamples = "n_samples",
    firstDetection = "first_pos",
    positiveCount = "pos_count",
    positiveSamplesList = NULL,
    sampleType = NULL,
    groupBy = NULL,
    targetConfidence = 0.95,
    maxSamples = 10,
    bootstrapIterations = 1000,
    showBinomialModel = TRUE,
    showBootstrap = TRUE,
    showEmpiricalCumulative = TRUE,
    showIncrementalYield = TRUE,
    estimationMethod = "auto"
)

# Check results
cat("Analysis completed\n")
cat("q estimate:", result$results$dataInfo$asDF()$value[which(result$results$dataInfo$asDF()$measure == "Per-sample detection probability (q)")], "\n")
```

Run with:
```bash
Rscript quick_test.R
```

## Test Data Files

Located in `data/` directory:

1. **pathsampling_basic.csv** - Level 1: Basic analysis
   - Columns: patient_id, n_samples, first_pos
   - Use for: Testing basic geometric MLE method

2. **pathsampling_enhanced.csv** - Level 2: Enhanced analysis
   - Columns: patient_id, n_samples, first_pos, pos_count
   - Use for: Testing empirical proportion method and new analyses

3. **pathsampling_complete.csv** - Level 3: Complete analysis
   - Columns: patient_id, sample_type, n_samples, pos_count, first_pos, pos_samples_string, clustering_index
   - Use for: Testing all new features including spatial clustering

4. **pathsampling_simulation_full.csv** - Validation dataset
   - Includes: metastasis_present_sim (ground truth)
   - Use for: Validation and debugging

## Expected Results Summary

### With pathsampling_enhanced.csv:

**Data Summary:**
- Total cases: 1000
- Positive cases: ~474 (47.4%)
- q estimate: ~0.385 (Empirical Proportion method)

**Binomial Model (at q=0.385):**
- 3 samples: ~79% detection
- 5 samples: ~91% detection
- 7 samples: ~96% detection
- 10 samples: ~99% detection

**Empirical Cumulative (NEW):**
- Should match binomial predictions closely
- Includes 95% bootstrap confidence intervals

**Incremental Yield (NEW):**
- 1→2 samples: High value (~24% gain)
- 5→6 samples: Moderate value (~6% gain)
- 9→10 samples: Low value (<2% gain)

## Success Criteria

✓ Module loads without errors
✓ All input variables accepted
✓ q estimation completes (shows method used)
✓ All enabled tables populate with data
✓ No "keyResults does not exist" error
✓ Empirical cumulative table displays
✓ Incremental yield analysis shows cost-benefit ratings

## Next Steps After Successful Testing

1. Test with pathsampling_complete.csv for:
   - Spatial clustering analysis
   - Multifocal detection patterns

2. Test stratified analysis:
   - Use sample_type variable
   - Enable "Show Stratified Analysis"
   - Verify type-specific q estimates

3. Test edge cases:
   - All negative cases
   - All positive cases
   - Single case datasets
   - Missing data scenarios

4. Create institutional protocol based on results
