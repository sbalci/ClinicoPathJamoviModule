---
name: check-function
description: Perform systematic quality check of a jamovi function
interactive: true
args:
  function_name:
    description: Name of the jamovi function to check
    required: true
    autocomplete: functions
usage: /check-function <function_name>
---

# Systematic Jamovi Function Quality Check

You are an expert jamovi module developer performing a comprehensive quality assessment of the jamovi function `$ARGUMENTS`. You will systematically evaluate the integration between the 4 core jamovi files (.a.yaml, .b.R, .r.yaml, .u.yaml) and provide actionable recommendations.

## Analysis Target

Function: **`$ARGUMENTS`**

Please analyze these files:

- `jamovi/$ARGUMENTS.a.yaml` - Analysis definition (options/arguments)
- `R/$ARGUMENTS.b.R` - Backend implementation
- `jamovi/$ARGUMENTS.r.yaml` - Results definition (outputs)
- `jamovi/$ARGUMENTS.u.yaml` - User interface definition

## Systematic Evaluation Framework

### 🔍 **Core Integration Checks**

1. **Argument Integration (.a.yaml ↔ .b.R)**
   - All .a.yaml options referenced in .b.R via `self$options$[argname]`
   - Arguments actually used in meaningful logic
   - Default values properly handled
   - Behavior changes when argument values change

2. **Output Population (.r.yaml ↔ .b.R)**
   - All .r.yaml outputs populated in .b.R via `self$results$[outputname]`
   - Data structures match definitions (Table/Image/Html)
   - Column schemas align between definition and implementation
   - Visibility and clear conditions work correctly

3. **Error Handling & Robustness**
   - Input validation for required variables
   - Missing data handling (empty datasets, NA values)
   - User-friendly error messages (not cryptic R errors)
   - Graceful degradation when analysis cannot proceed

4. **Code Quality & User Experience**
   - Functions are modular and well-organized
   - UI elements appropriately grouped and labeled
   - Explanatory content available for complex outputs
   - Performance acceptable for typical datasets

## Response Format

Structure your analysis as:

### 📋 SYSTEMATIC CHECK: `$ARGUMENTS`

**Status**: ✅ PASS / ⚠️ MINOR ISSUES / ❌ NEEDS WORK  
**Priority**: 🔥 Critical / ⚡ High / 📝 Medium / 💡 Enhancement

#### 🔍 QUICK SUMMARY

- **Arguments**: X defined → X/X used in .b.R
- **Outputs**: X defined → X/X populated in .b.R  
- **Error Handling**: [Brief assessment]
- **Integration Quality**: [Brief assessment]

#### ❌ CRITICAL ISSUES (Fix immediately)

1. [Specific issue with file:line reference if possible]

#### ⚠️ INTEGRATION ISSUES (Schema mismatches, unused elements)

1. [Specific issue with exact fix needed]

#### 📝 CODE QUALITY ISSUES (Improvements recommended)

1. [Specific suggestion with rationale]

#### ✅ STRENGTHS (What's working well)

1. [Positive findings]

#### 🔧 ACTIONABLE FIXES

**Immediate (Critical):**

```yaml
# Exact code changes needed
```

**Schema Updates:**

```yaml  
# Specific .yaml file changes
```

**Code Improvements:**

```r
# Specific .b.R improvements
```

#### 🧪 TESTING CHECKLIST

- [ ] Test with [specific scenario]
- [ ] Validate [specific behavior]
- [ ] Check [edge case]

#### 📊 READINESS ASSESSMENT

- **File Integration**: ✅❌  
- **Error Handling**: ✅⚠️❌  
- **User Experience**: ✅⚠️❌  
- **Production Ready**: YES/NO  

Be specific, actionable, and focus on integration between files - this is where most jamovi function issues occur.
