# Systematic Jamovi Function Check Template

## Overview
This template provides a comprehensive checklist for evaluating jamovi module functions to ensure they are robust, well-integrated, and state-of-the-art. Use this for each function before release.

## Function Check Protocol: `[FUNCTION_NAME]`

### 1. FILE INTEGRITY CHECK
- [ ] `.a.yaml` - Analysis definition exists and is complete
- [ ] `.b.R` - Backend implementation exists and is complete  
- [ ] `.r.yaml` - Results definition exists and is complete
- [ ] `.u.yaml` - UI definition exists and is complete
- [ ] `.h.R` - Header file generated successfully

### 2. ARGUMENT INTEGRATION CHECK (.a.yaml ↔ .b.R)

#### 2.1 Argument Usage Verification
For each argument in `.a.yaml`:
- [ ] **Argument Name**: `[arg_name]`
  - [ ] Referenced in `.b.R` via `self$options$[arg_name]`
  - [ ] Used in intended function/logic
  - [ ] Default value respected
  - [ ] Type validation implemented (if needed)
  - [ ] Error handling for invalid values

#### 2.2 Behavioral Impact Testing
- [ ] Changing each argument produces expected behavior change
- [ ] Default vs non-default values work correctly
- [ ] Edge cases handled (min/max values, empty selections)
- [ ] Argument combinations work correctly

### 3. OUTPUT GENERATION CHECK (.r.yaml ↔ .b.R)

#### 3.1 Output Definition Verification
For each output in `.r.yaml`:
- [ ] **Output Name**: `[output_name]`
  - [ ] Correctly populated in `.b.R`
  - [ ] Data structure matches definition (Table/Image/Html)
  - [ ] Column definitions match actual output
  - [ ] Visibility conditions work correctly
  - [ ] Clear conditions implemented

#### 3.2 Output Quality Check
- [ ] Tables have appropriate formatting
- [ ] Images render correctly with proper dimensions
- [ ] HTML content is well-formatted and informative
- [ ] Error states handled gracefully
- [ ] Empty data scenarios managed

### 4. USER INTERFACE CHECK (.u.yaml)

#### 4.1 Grouping and Organization
- [ ] Options logically grouped in sections
- [ ] Section titles are descriptive and clear
- [ ] Related options placed together
- [ ] Advanced options appropriately separated

#### 4.2 UI Element Validation
- [ ] All `.a.yaml` arguments have corresponding UI elements
- [ ] UI element types match argument types
- [ ] Dependencies and visibility conditions work
- [ ] Help text is informative and accurate

### 5. EXPLANATORY OUTPUTS CHECK

#### 5.1 Educational Content Coverage
For each major output type:
- [ ] **Tables**: Explanatory content exists
- [ ] **Plots**: Interpretation guidance provided
- [ ] **Statistical Results**: Methodology explanation available
- [ ] **Clinical Context**: Practical interpretation included

#### 5.2 Explanatory Options Integration
- [ ] `showExplanations` option implemented
- [ ] `showMethodologyNotes` option implemented  
- [ ] `includeClinicalGuidance` option implemented
- [ ] Explanatory outputs linked to main results
- [ ] Content is accurate and helpful

### 6. TEST DATA AND COVERAGE

#### 6.1 Test Data Availability
- [ ] Sample dataset exists for testing
- [ ] Dataset covers all argument scenarios
- [ ] Edge cases represented in test data
- [ ] Data format matches function requirements

#### 6.2 Argument Coverage Testing
For each argument in `.a.yaml`:
- [ ] **Argument**: `[arg_name]`
  - [ ] Testable with available data
  - [ ] Multiple values tested
  - [ ] Boundary conditions tested
  - [ ] Error conditions tested

### 7. ERROR HANDLING AND ROBUSTNESS

#### 7.1 Input Validation
- [ ] Missing required variables handled
- [ ] Invalid data types handled
- [ ] Empty datasets handled
- [ ] Insufficient data scenarios managed

#### 7.2 Graceful Failure
- [ ] Error messages are informative
- [ ] No cryptic R errors exposed to users
- [ ] Partial results shown when possible
- [ ] Recovery suggestions provided

### 8. PERFORMANCE AND EFFICIENCY

#### 8.1 Computational Efficiency
- [ ] Large datasets handled appropriately
- [ ] Memory usage is reasonable
- [ ] Processing time is acceptable
- [ ] Progress indication for long operations

#### 8.2 Code Quality
- [ ] Functions are modular and well-organized
- [ ] Code is documented and readable
- [ ] No redundant calculations
- [ ] Appropriate use of caching

### 9. INTEGRATION AND CONSISTENCY

#### 9.1 Module Integration
- [ ] Function fits well within module structure
- [ ] Consistent with other module functions
- [ ] References and citations included
- [ ] Help documentation complete

#### 9.2 Jamovi Standards Compliance
- [ ] Follows jamovi naming conventions
- [ ] Uses standard jamovi UI patterns
- [ ] Consistent with jamovi style guidelines
- [ ] Proper package dependencies declared

### 10. FINAL VALIDATION CHECKLIST

#### 10.1 End-to-End Testing
- [ ] Function works with default settings
- [ ] All major option combinations tested
- [ ] Results are scientifically valid
- [ ] Output is publication-ready

#### 10.2 User Experience
- [ ] Interface is intuitive
- [ ] Help text is sufficient
- [ ] Error messages guide users effectively
- [ ] Results are clearly presented

## SYSTEMATIC TESTING PROCEDURE

### Phase 1: File Structure Validation
1. Verify all 4 files exist and are properly named
2. Check header file generation with `jmvtools::prepare()`
3. Validate YAML syntax and structure

### Phase 2: Argument Mapping Analysis
1. Extract all arguments from `.a.yaml`
2. Search `.b.R` for each argument usage
3. Verify behavioral impact of each argument
4. Test argument combinations

### Phase 3: Output Verification
1. Map all `.r.yaml` outputs to `.b.R` population code
2. Test each output with various input scenarios
3. Validate output formatting and structure
4. Check error handling for each output

### Phase 4: UI/UX Evaluation
1. Review `.u.yaml` organization and grouping
2. Test UI element functionality
3. Validate help text and descriptions
4. Check accessibility and usability

### Phase 5: Educational Content Audit
1. Verify explanatory outputs for each result type
2. Test explanatory option functionality
3. Review content accuracy and helpfulness
4. Ensure clinical relevance

### Phase 6: Comprehensive Testing
1. Test with multiple datasets
2. Test all argument combinations
3. Test error scenarios
4. Performance testing with large data

## DOCUMENTATION TEMPLATE

### Function Assessment Report: `[FUNCTION_NAME]`

**Date**: [DATE]
**Reviewer**: [NAME]
**Version**: [VERSION]

#### Summary
- **Status**: ✅ Pass / ⚠️ Issues Found / ❌ Fail
- **Critical Issues**: [NUMBER]
- **Minor Issues**: [NUMBER]
- **Recommendations**: [NUMBER]

#### Critical Issues Found
1. [Issue description with severity and impact]
2. [Issue description with severity and impact]

#### Recommendations for Improvement
1. [Recommendation with rationale]
2. [Recommendation with rationale]

#### Test Coverage Summary
- **Arguments Tested**: [X/Y]
- **Outputs Verified**: [X/Y]
- **Error Scenarios**: [X/Y]
- **UI Elements**: [X/Y]

#### Sign-off
- [ ] Function meets quality standards
- [ ] Ready for production release
- [ ] Documentation complete
- [ ] Test data adequate

---

## USAGE INSTRUCTIONS

1. **Copy this template** for each function to be evaluated
2. **Replace `[FUNCTION_NAME]`** with actual function name
3. **Work through each checklist item systematically**
4. **Document all issues and recommendations**
5. **Re-test after fixes are implemented**
6. **Obtain sign-off before release**

## QUALITY GATES

### Minimum Requirements for Release
- [ ] All critical issues resolved
- [ ] 100% argument integration verified
- [ ] 100% output generation confirmed
- [ ] Error handling implemented
- [ ] Test data coverage adequate
- [ ] Documentation complete

### Excellence Standards
- [ ] Explanatory outputs comprehensive
- [ ] UI organization optimal
- [ ] Performance optimized
- [ ] Code quality high
- [ ] User experience exceptional