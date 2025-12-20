# Study Diagram Module: Validation Checklist

## Pre-Release Validation Checklist

Use this checklist to validate the studydiagram module before release.

---

## ✅ CODE VALIDATION (COMPLETED)

- [x] R code compiles without errors
- [x] All YAML files are valid
- [x] Notice system properly implemented
- [x] Parameter renaming complete across all files
- [x] No references to removed participant_step format
- [x] Validation method called in .run()
- [x] All input validation checks added

---

## 🔧 NEXT: HEADER REGENERATION (TODO)

- [ ] Run `Rscript -e "jmvtools::prepare('.')"`
- [ ] Verify no compilation errors
- [ ] Check R/studydiagram.h.R exists and is up-to-date
- [ ] Verify notices field exists in header

---

## 🧪 TESTING CHECKLIST (TODO)

### Test 1: Valid Data - Step Summary Format
- [ ] Create test data:
  ```r
  test_data <- data.frame(
    step = c("Screened", "Eligible", "Enrolled", "Completed"),
    count = c(1000, 850, 720, 680),
    reason = c("", "Did not meet criteria", "Declined", "Lost to follow-up")
  )
  ```
- [ ] Run analysis with step_summary format
- [ ] Verify diagram generates successfully
- [ ] Verify retention calculation: 680/1000 = 68%
- [ ] Check plot displays correctly
- [ ] Verify clinical summary generates
- [ ] Check report sentence is copy-ready

### Test 2: Invalid Data - Negative Counts
- [ ] Create test data with negative count
- [ ] Run analysis
- [ ] **Expected**: ERROR notice "Participant counts cannot be negative"
- [ ] **Expected**: No diagram generated
- [ ] Verify error message is helpful

### Test 3: Invalid Data - Increasing Counts
- [ ] Create test data with increasing counts:
  ```r
  bad_data <- data.frame(
    step = c("A", "B", "C"),
    count = c(100, 150, 200)
  )
  ```
- [ ] Run analysis
- [ ] **Expected**: ERROR notice about increasing counts
- [ ] **Expected**: No diagram generated
- [ ] **Expected**: Detailed error message displayed

### Test 4: Invalid Data - NA Values
- [ ] Create test data with NA in counts
- [ ] Run analysis
- [ ] **Expected**: ERROR notice about missing values
- [ ] **Expected**: Count of missing rows reported
- [ ] Verify no diagram generated

### Test 5: Invalid Data - Zero Initial Count
- [ ] Create test data starting with 0:
  ```r
  zero_data <- data.frame(step = c("A", "B"), count = c(0, 0))
  ```
- [ ] Run analysis
- [ ] **Expected**: ERROR notice "Initial participant count is zero"
- [ ] Verify no diagram generated

### Test 6: Exclusion Mapping Format
- [ ] Create test data with participant IDs and reasons
- [ ] Select exclusion_mapping format
- [ ] Map exclusion reasons to steps using new parameter names
- [ ] Verify `exclusions_after_step1` applies to transition 1→2
- [ ] Verify `exclusions_after_step2` applies to transition 2→3
- [ ] Check diagram flow is correct
- [ ] Verify retention calculations

### Test 7: Duplicate Participant IDs
- [ ] Create exclusion_mapping data with duplicate IDs
- [ ] Run analysis
- [ ] **Expected**: STRONG_WARNING notice about duplicates
- [ ] Verify analysis continues (warning, not error)

### Test 8: Missing Required Variables
- [ ] Select step_summary format
- [ ] Don't assign participant_count variable
- [ ] **Expected**: ERROR notice listing missing variables
- [ ] Verify helpful message displayed in todo section

### Test 9: Variable Format Mismatch
- [ ] Select step_summary format
- [ ] Assign exclusion_mapping variables instead
- [ ] **Expected**: INFO notice about format mismatch
- [ ] Verify analysis continues with correct format variables

### Test 10: Success Notice
- [ ] Run successful analysis
- [ ] **Expected**: INFO notice "Study diagram generated successfully. X initial participants, Y in final analysis (Z% retention)"
- [ ] Verify retention percentage is correct

---

## 📊 CLINICAL REVIEW (TODO)

### Real Data Testing
- [ ] Obtain real study data from pathologist/oncologist
- [ ] Test with actual CONSORT trial data
- [ ] Verify output matches hand-calculated flow
- [ ] Check if diagram meets journal submission requirements
- [ ] Validate report sentence is publication-ready
- [ ] Confirm retention calculations are accurate

### CONSORT Compliance
- [ ] Diagram shows all participant flow stages
- [ ] Exclusion counts displayed correctly
- [ ] Exclusion reasons documented
- [ ] Retention percentages accurate
- [ ] Format suitable for journal submission
- [ ] Matches CONSORT 2010 guidelines

---

## 📝 DOCUMENTATION (TODO)

- [ ] Update README with studydiagram usage
- [ ] Create example data files in data/ folder
- [ ] Write vignette showing all three formats
- [ ] Document exclusion_after_step naming convention
- [ ] Add troubleshooting section for common errors

---

## 🎯 RELEASE CRITERIA

### Must Have (Blocking)
- [ ] All code validation passes
- [ ] Headers regenerated successfully
- [ ] Tests 1-10 all pass
- [ ] At least one real data clinical review complete

### Should Have (Important)
- [ ] Example datasets included
- [ ] Basic usage vignette available
- [ ] Common errors documented

### Nice to Have (Optional)
- [ ] Comprehensive vignette with all formats
- [ ] Video tutorial
- [ ] Multiple example datasets

---

## ⚠️ KNOWN LIMITATIONS

Document any limitations discovered during testing:

1. _Add limitations here after testing_
2. _..._

---

## 🐛 ISSUES FOUND

Document any issues found during validation:

1. _Add issues here_
2. _..._

---

## ✅ SIGN-OFF

- [ ] Developer validation complete
- [ ] Clinical review complete
- [ ] Documentation updated
- [ ] All blocking issues resolved
- [ ] Module ready for release

**Validated By**: _______________  
**Date**: _______________  
**Clinical Reviewer**: _______________  
**Date**: _______________  

---

Generated: $(date "+%Y-%m-%d %H:%M:%S")
Module: studydiagram v1.0.0
