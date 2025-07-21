# Will Rogers Features Validation Checklist

## Pre-Test Setup
- [ ] Load test data (`test_stage_migration_data.csv`) in jamovi
- [ ] Set variables:
  - Original Stage: `tnm7_stage` (Factor)
  - New Stage: `tnm8_stage` (Factor)
  - Survival Time: `os_months` (Continuous)
  - Event: `death_status` (Factor)
  - Event Level: "Dead"

## Feature Testing

### 1. Will Rogers Effect Visualization
**Enable**: `Will Rogers Effect Visualization`

**Expected Results**:
- [ ] Bar chart displays showing median survival comparison
- [ ] Identifies T2→T3 as the dominant migration pattern (or most common)
- [ ] Shows "Before Migration" (red) vs "After Migration" (blue) bars
- [ ] Patient counts (n=X) displayed on bars
- [ ] Title includes migration pattern details
- [ ] Annotation explains Will Rogers Paradox
- [ ] No loading screen stuck - plot renders properly

**Error Cases**:
- [ ] With no migration data: Shows "No stage migration detected" message
- [ ] With insufficient data: Shows appropriate error message

### 2. Migration Survival Curve Comparison
**Enable**: `Migration Survival Curve Comparison`

**Expected Results**:
- [ ] Faceted Kaplan-Meier curves display
- [ ] One panel per stage (T2, T3, etc.)
- [ ] Red solid line = Before Migration
- [ ] Blue dashed line = After Migration
- [ ] Sample size annotations show "Before Migration n=X" and "After Migration n=Y"
- [ ] Y-axis shows survival probability (0-100%)
- [ ] X-axis shows time in months
- [ ] Legend clearly identifies line types

**Visual Quality**:
- [ ] Curves are smooth and properly stepped
- [ ] No overlapping text
- [ ] Clear stage labels on facets

### 3. Enhanced Will Rogers Statistical Analysis
**Enable**: `Advanced Migration Analysis` (if not already enabled)

**Expected Results in Table**:
- [ ] Shows rows for each migration pattern (e.g., "T2 (original)" and "T3 (new)")
- [ ] "Period" column shows "With vs without X migrated patients"
- [ ] "N" column shows comparison sample sizes (e.g., "215 vs 178")
- [ ] "Median Survival" shows both values (e.g., "19.5 vs 24.5")
- [ ] **95% CI Lower** populated with values (e.g., "15.2 vs 18.7")
- [ ] **95% CI Upper** populated with values (e.g., "25.8 vs 32.1")
- [ ] "Δ Survival" shows the difference
- [ ] "P-value" shows log-rank test result
- [ ] "Test" column identifies "Log-rank test (T2→T3 migration)"

**Overall Assessment Row**:
- [ ] Shows summary statistics
- [ ] No Turkish locale error ("fonksiyon olmayana uygulama denemesi")
- [ ] Shows migration pattern summary
- [ ] Provides evidence classification

### 4. Sankey Diagram
**Enable**: `Stage Migration Flow Diagram`

**Expected Results**:
- [ ] Flow diagram displays with all patient movements
- [ ] Red arrows for upstaging
- [ ] Green arrows for downstaging
- [ ] Gray arrows for no change
- [ ] All flows labeled with "n=X" including same-stage flows
- [ ] Node boxes show stage name and total count
- [ ] Legend explains color coding

### 5. Dashboard Integration
**View**: Comparative Analysis Dashboard

**Will Rogers Row**:
- [ ] "Bias Assessment" category
- [ ] "Will Rogers Evidence" metric
- [ ] Shows actual result (not "TBD")
- [ ] Clinical relevance assessment
- [ ] Appropriate recommendation

### 6. Explanations and Documentation
**Enable**: `Explanations for Results` and `Show Abbreviation Glossary`

**Dashboard Explanation**:
- [ ] Explains N/A and TBD meanings
- [ ] Provides steps to address TBD values
- [ ] Lists all abbreviations used

**Abbreviation Glossary**:
- [ ] Grid layout displays properly
- [ ] All 8 categories visible
- [ ] Terms are organized and readable
- [ ] Search tip (Ctrl+F) is shown

## Performance Testing

### Response Times
- [ ] Plots generate within 5 seconds
- [ ] Tables populate without significant delay
- [ ] No jamovi freezing or crashes

### Large Dataset (n>1000)
- [ ] All features still work
- [ ] Reasonable performance
- [ ] Memory usage acceptable

## Edge Cases

### No Migration
- [ ] Create dataset where tnm7_stage = tnm8_stage for all patients
- [ ] All features show appropriate "no migration" messages
- [ ] No errors or crashes

### Single Stage Only
- [ ] Filter data to single stage
- [ ] Features handle gracefully
- [ ] Appropriate error messages

### Missing Values
- [ ] Introduce some NA values
- [ ] Features continue to work
- [ ] NA handling is appropriate

## Bug Verification

### Fixed Issues
- [ ] Will Rogers plot displays (not stuck on loading)
- [ ] 95% CI columns show values (not empty)
- [ ] Turkish locale error resolved in Overall Assessment
- [ ] Sankey diagram generates properly
- [ ] All same-stage flows show patient counts

## Final Validation

### Publication Ready
- [ ] All outputs suitable for manuscript inclusion
- [ ] Statistical results are accurate
- [ ] Visualizations are professional quality
- [ ] Terminology is consistent

### Clinical Interpretation
- [ ] Results make clinical sense
- [ ] Will Rogers paradox is clearly demonstrated
- [ ] Recommendations are appropriate
- [ ] Evidence levels are justified

## Sign-off
- [ ] All tests passed
- [ ] No critical bugs found
- [ ] Ready for production use

**Tested by**: ________________  
**Date**: ________________  
**jamovi Version**: ________________  
**Module Version**: ________________