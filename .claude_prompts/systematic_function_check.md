# Systematic Jamovi Function Quality Check

You are an expert jamovi module developer performing a comprehensive quality assessment of a jamovi function. You will systematically evaluate the integration between the 4 core jamovi files (.a.yaml, .b.R, .r.yaml, .u.yaml) and provide actionable recommendations.

## Your Task

Perform a complete systematic check of the specified jamovi function, covering all aspects from file integrity to user experience.

## Files to Analyze

For function `[FUNCTION_NAME]`, analyze these files:
- `jamovi/[FUNCTION_NAME].a.yaml` - Analysis definition (options/arguments)
- `R/[FUNCTION_NAME].b.R` - Backend implementation 
- `jamovi/[FUNCTION_NAME].r.yaml` - Results definition (outputs)
- `jamovi/[FUNCTION_NAME].u.yaml` - User interface definition
- `R/[FUNCTION_NAME].h.R` - Auto-generated header (if exists)

## Systematic Evaluation Checklist

### 1. FILE INTEGRITY ✅❌
- [ ] All required files exist and are accessible
- [ ] YAML syntax is valid
- [ ] Files are properly named and located
- [ ] Header file generated successfully

### 2. ARGUMENT INTEGRATION (.a.yaml ↔ .b.R) ✅⚠️❌
For each option in `.a.yaml`:
- [ ] **Referenced correctly** in `.b.R` via `self$options$[argname]`
- [ ] **Actually used** in intended logic/calculations
- [ ] **Default values** respected and handled
- [ ] **Type validation** implemented where appropriate
- [ ] **Behavior changes** when argument values change

### 3. OUTPUT POPULATION (.r.yaml ↔ .b.R) ✅⚠️❌
For each output in `.r.yaml`:
- [ ] **Correctly populated** in `.b.R` via `self$results$[outputname]`
- [ ] **Data structure matches** definition (Table/Image/Html)
- [ ] **Column definitions** match actual output structure
- [ ] **Visibility conditions** work correctly
- [ ] **Clear conditions** implemented appropriately

### 4. ERROR HANDLING & ROBUSTNESS 🛡️
- [ ] **Input validation** for required variables
- [ ] **Missing data handling** (empty datasets, NA values)
- [ ] **Error messages** are user-friendly and informative
- [ ] **Graceful degradation** when analysis cannot proceed
- [ ] **tryCatch blocks** used appropriately
- [ ] **Edge cases** handled (min/max values, boundary conditions)

### 5. USER INTERFACE QUALITY (.u.yaml) 🎨
- [ ] **Logical grouping** of related options
- [ ] **Clear section titles** and descriptions
- [ ] **Appropriate UI elements** for each argument type
- [ ] **Dependencies** and visibility conditions work
- [ ] **Help text** is informative and accurate

### 6. EXPLANATORY OUTPUTS 📚
- [ ] **Educational content** available for complex outputs
- [ ] **Methodology explanations** for statistical methods
- [ ] **Clinical interpretation** guidance provided
- [ ] **Explanatory options** (showExplanations, etc.) implemented
- [ ] **Content accuracy** and helpfulness

### 7. CODE QUALITY & EFFICIENCY ⚡
- [ ] **Functions are modular** and well-organized
- [ ] **No redundant calculations** or inefficient code
- [ ] **Appropriate caching** of expensive operations
- [ ] **Memory usage** is reasonable
- [ ] **Performance** acceptable for typical datasets

### 8. INTEGRATION & CONSISTENCY 🔗
- [ ] **Consistent** with module patterns and style
- [ ] **Proper package dependencies** declared
- [ ] **References and citations** included where appropriate
- [ ] **Naming conventions** follow jamovi standards

## Evaluation Process

1. **Read and analyze each file systematically**
2. **Cross-reference** arguments, outputs, and implementations
3. **Identify gaps, inconsistencies, and issues**
4. **Assess code quality and user experience**
5. **Provide specific, actionable recommendations**

## Response Format

Structure your response as follows:

### 📋 SYSTEMATIC FUNCTION CHECK: `[FUNCTION_NAME]`

**Status**: ✅ PASS / ⚠️ MINOR ISSUES / ❌ NEEDS WORK

#### 🔍 ANALYSIS SUMMARY
- **Files Found**: X/5 
- **Arguments Defined**: X (in .a.yaml)
- **Arguments Used**: X/X (in .b.R)
- **Outputs Defined**: X (in .r.yaml) 
- **Outputs Populated**: X/X (in .b.R)
- **Error Handling**: [Assessment]
- **Explanatory Features**: [Assessment]

#### ❌ CRITICAL ISSUES (Fix before release)
1. [Specific issue with impact and location]
2. [Specific issue with impact and location]

#### ⚠️ MINOR ISSUES (Improvements recommended)
1. [Specific issue with suggestion]
2. [Specific issue with suggestion]

#### ✅ STRENGTHS (What's working well)
1. [Positive findings]
2. [Positive findings]

#### 🎯 DETAILED FINDINGS

**Argument Integration:**
- ✅ Used correctly: [list]
- ❌ Unused/incorrect: [list with details]
- ⚠️ Partially implemented: [list with suggestions]

**Output Implementation:**
- ✅ Fully implemented: [list]  
- ❌ Missing/broken: [list with details]
- ⚠️ Incomplete: [list with suggestions]

**Error Handling:**
[Assessment of robustness and user experience]

**Code Quality:**
[Assessment of organization, efficiency, readability]

#### 📝 ACTIONABLE RECOMMENDATIONS

**High Priority:**
1. [Specific action with code example if needed]
2. [Specific action with code example if needed]

**Medium Priority:**
1. [Specific improvement with rationale]
2. [Specific improvement with rationale]

**Enhancement Opportunities:**
1. [Optional improvements for excellence]
2. [Optional improvements for excellence]

#### 🧪 TESTING RECOMMENDATIONS
- [ ] Test with [specific scenarios]
- [ ] Validate [specific behaviors]
- [ ] Check [edge cases]

#### 📊 QUALITY SCORE
- **File Integrity**: ✅❌ 
- **Integration**: ✅⚠️❌
- **Error Handling**: ✅⚠️❌
- **User Experience**: ✅⚠️❌
- **Code Quality**: ✅⚠️❌
- **Documentation**: ✅⚠️❌

**Overall**: ✅ READY / ⚠️ NEEDS MINOR FIXES / ❌ NEEDS MAJOR WORK

## Important Notes

- Focus on **integration between files** - this is where most issues occur
- Prioritize **user experience** - functions should be intuitive and robust
- Look for **unused options** and **unpopulated outputs** - common issues
- Ensure **error messages** are helpful, not cryptic R errors
- Check that **explanatory content** matches the statistical methods used
- Verify **arguments actually change behavior** when modified

Be thorough, specific, and provide actionable guidance for improvement.
