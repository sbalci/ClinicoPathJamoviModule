# Manual Test Protocol for oddsratio Function

## Prerequisites
1. Install the ClinicoPath module in jamovi
2. Load test dataset: `histopathology` (built-in) or create synthetic data

## Test Data Preparation

### Test Dataset 1: histopathology (built-in)
- Variables: New Test, Rater 1, Rater 2, Golden Standart
- All binary variables
- Positive level: "1"

### Test Dataset 2: Create synthetic data (if needed)
Create a dataset with:
- Binary outcome: Mortality5yr (Alive/Dead)
- Binary predictors: Sex (Male/Female), LVI (Absent/Present), PNI (Absent/Present)
- Continuous: Age (numeric)

---

## Test Cases

### TEST 1: Basic Binary Analysis
**Setup:**
- Explanatory: New Test
- Outcome: Golden Standart
- Positive Level: 1

**Expected Results:**
- ✓ Odds Ratio Table displays
- ✓ Forest plot displays
- ✓ Model Performance Metrics display
- ✓ No errors in console

**Verify:**
- [ ] Table shows OR with CI and p-value
- [ ] Plot shows single predictor
- [ ] Model metrics (AIC, C-statistic, H&L) present
- [ ] Positive level correctly selected

---

### TEST 2: Multiple Binary Predictors
**Setup:**
- Explanatory: New Test, Rater 1, Rater 2
- Outcome: Golden Standart
- Positive Level: 1

**Expected Results:**
- ✓ Table shows all 3 predictors
- ✓ Forest plot shows all 3 predictors
- ✓ Both univariable and multivariable ORs

**Verify:**
- [ ] All predictors in correct order
- [ ] Table and plot values match
- [ ] No dependent variable in plot
- [ ] Reference categories marked with "-"

---

### TEST 3: Mixed Variables (Binary + Continuous)
**Setup:**
- Explanatory: Sex, Age, LVI, PNI
- Outcome: Mortality5yr
- Positive Level: Dead

**Expected Results:**
- ✓ Binary variables show OR
- ✓ Continuous variables show OR per unit
- ✓ Plot displays correctly

**Verify:**
- [ ] Age shows "Mean (SD)" format
- [ ] Binary variables show category counts
- [ ] ORs calculated correctly for both types

---

### TEST 4: Educational Explanations
**Setup:**
- Explanatory: New Test, Rater 1
- Outcome: Golden Standart
- Positive Level: 1
- ☑ Show Educational Explanations

**Expected Results:**
- ✓ Three explanation sections appear:
  1. Understanding Odds Ratio Analysis
  2. Understanding Risk Measures
  3. Understanding Diagnostic Test Performance

**Verify:**
- [ ] All three explanation boxes visible
- [ ] Formatted with colored backgrounds
- [ ] Clear, readable text
- [ ] Explanations appear after main results

---

### TEST 5: Diagnostic Nomogram (Default Predictor)
**Setup:**
- Explanatory: New Test, Rater 1, Rater 2
- Outcome: Golden Standart
- Positive Level: 1
- ☑ Show diagnostic nomogram
- Diagnostic Predictor: (leave empty - default to first)

**Expected Results:**
- ✓ Nomogram plot displays
- ✓ Nomogram Details HTML displays
- ✓ INFO notice: "Using 'New Test' (first explanatory variable) for diagnostic metrics..."
- ✓ Likelihood ratios in Model Metrics

**Verify:**
- [ ] Nomogram plot renders
- [ ] Shows pre-test/post-test probability axes
- [ ] Likelihood ratio axis present
- [ ] Nomogram Details box with explanation
- [ ] LR+, LR- values in metrics
- [ ] Sensitivity and specificity shown

---

### TEST 6: Diagnostic Nomogram (Specific Predictor)
**Setup:**
- Explanatory: New Test, Rater 1, Rater 2
- Outcome: Golden Standart
- Positive Level: 1
- ☑ Show diagnostic nomogram
- Diagnostic Predictor: Rater 1

**Expected Results:**
- ✓ INFO notice: "Using 'Rater 1' (from model) for diagnostic metrics..."
- ✓ Nomogram uses Rater 1
- ✓ Likelihood ratios calculated for Rater 1

**Verify:**
- [ ] Correct predictor used
- [ ] Notice mentions the selected predictor
- [ ] Diagnostic metrics match selected variable

---

### TEST 7: Diagnostic Predictor NOT in Model
**Setup:**
- Explanatory: Age, LVI
- Outcome: Mortality5yr
- Positive Level: Dead
- ☑ Show diagnostic nomogram
- Diagnostic Predictor: Sex (not in model)

**Expected Results:**
- ✓ INFO notice: "Using 'Sex' for diagnostic metrics. Note: This variable is NOT in the logistic regression model..."
- ✓ Nomogram still generates
- ✓ Diagnostic metrics calculated independently

**Verify:**
- [ ] Warning/info about predictor not in model
- [ ] Nomogram still works
- [ ] Metrics calculated correctly

---

### TEST 8: All Options Enabled
**Setup:**
- Explanatory: New Test, Rater 1, Rater 2
- Outcome: Golden Standart
- Positive Level: 1
- ☑ Show diagnostic nomogram
- ☑ Show Educational Explanations
- Diagnostic Predictor: New Test

**Expected Results:**
- ✓ All outputs display:
  - Odds Ratio Table
  - Model Performance Metrics (with LRs)
  - Forest Plot
  - Three explanation sections
  - Nomogram plot
  - Nomogram Details
  - Nomogram explanation section (fourth explanation)

**Verify:**
- [ ] All 4 explanation sections visible
- [ ] Nomogram explanation includes diagnostic predictor selection text
- [ ] No visual clutter
- [ ] Proper ordering of sections

---

### TEST 9: Edge Case - Single Continuous Predictor
**Setup:**
- Explanatory: Age
- Outcome: Mortality5yr
- Positive Level: Dead

**Expected Results:**
- ✓ Analysis runs
- ✓ Shows OR per unit age
- ✓ Plot displays correctly

**Verify:**
- [ ] Mean (SD) shown in table
- [ ] OR < 1 or > 1 makes clinical sense
- [ ] CI doesn't cross 1 (if significant)

---

### TEST 10: Edge Case - Non-Binary Diagnostic Predictor
**Setup:**
- Explanatory: Age, Sex, LVI
- Outcome: Mortality5yr
- Positive Level: Dead
- ☑ Show diagnostic nomogram
- Diagnostic Predictor: Age (continuous - should fail)

**Expected Results:**
- ✓ WARNING notice: "Diagnostic predictor 'Age' has ... levels but must be binary..."
- ✓ Nomogram does NOT generate
- ✓ Main analysis still completes

**Verify:**
- [ ] Clear warning message
- [ ] No nomogram
- [ ] Main OR analysis unaffected

---

### TEST 11: Variable Names with Spaces
**Setup:**
- Use dataset with spaces in variable names
- Or rename columns to include spaces

**Expected Results:**
- ✓ Analysis runs correctly
- ✓ Original names preserved in output
- ✓ No parsing errors

**Verify:**
- [ ] Spaces handled correctly
- [ ] Names display properly in table
- [ ] Plot shows correct names

---

### TEST 12: Positive Level Selection
**Setup:**
- Test with outcome "Alive" vs "Dead"
- Set Positive Level: Dead
- Then change to: Alive

**Expected Results:**
- ✓ ORs flip correctly
- ✓ Interpretation changes
- ✓ Plot updates

**Verify:**
- [ ] OR for same predictor inverts (1/OR)
- [ ] Table updates when level changes
- [ ] Clinically meaningful interpretation

---

## Critical Checks

### Visual Consistency
- [ ] Table and plot show SAME odds ratio values
- [ ] No dependent variable in forest plot
- [ ] Reference categories clearly marked
- [ ] Confidence intervals displayed correctly

### Nomogram Functionality
- [ ] Nomogram only appears when checkbox enabled
- [ ] Diagnostic predictor must be binary
- [ ] Clear error messages for invalid predictors
- [ ] Likelihood ratios calculated correctly

### Explanations
- [ ] Only appear when checkbox enabled
- [ ] Fourth explanation (nomogram) only when both checkboxes enabled
- [ ] Well-formatted and readable
- [ ] Diagnostic predictor selection text in nomogram explanation

### Performance
- [ ] No crashes or freezes
- [ ] Reasonable execution time (<5 seconds for small data)
- [ ] Updates smoothly when options change

---

## Bug Reports

### Known Issues
- None currently documented

### If You Find a Bug
1. Note the exact setup (variables, options)
2. Copy error message
3. Take screenshot if visual issue
4. Note expected vs actual behavior

---

## Test Summary Sheet

| Test | Description | Pass | Fail | Notes |
|------|-------------|------|------|-------|
| 1    | Basic binary | ☐ | ☐ | |
| 2    | Multiple binary | ☐ | ☐ | |
| 3    | Mixed variables | ☐ | ☐ | |
| 4    | Explanations | ☐ | ☐ | |
| 5    | Nomogram default | ☐ | ☐ | |
| 6    | Nomogram specific | ☐ | ☐ | |
| 7    | Predictor not in model | ☐ | ☐ | |
| 8    | All options | ☐ | ☐ | |
| 9    | Single continuous | ☐ | ☐ | |
| 10   | Non-binary diagnostic | ☐ | ☐ | |
| 11   | Names with spaces | ☐ | ☐ | |
| 12   | Positive level | ☐ | ☐ | |

**Total Passed:** _____ / 12

**Tester:** ________________
**Date:** ________________
**Module Version:** ________________
