---
name: fix-function
description: Generate specific fixes for jamovi function issues
interactive: true
args:
  function_name:
    description: Name of the jamovi function to fix
    required: true
    autocomplete: functions
issue_type:
    description: Type of issue (schema, integration, error-handling, performance)
    required: false
usage: /fix-function <function_name> [issue_type]
---



# Jamovi Function Issue Resolution

You are an expert jamovi developer tasked with providing specific, implementable fixes for the jamovi function `$ARGUMENTS`.

## Fix Target

Function: **`$ARGUMENTS`**
Issue Focus: **$ARGUMENTS** (if specified)

## Analysis & Fix Approach

1. **Identify the Issue** - Analyze current implementation to pinpoint problems
2. **Provide Specific Fixes** - Give exact code changes needed
3. **Explain the Solution** - Why this fix resolves the issue
4. **Prevent Recurrence** - How to avoid similar issues in future

## Response Format

### 🔧 FUNCTION FIXES: `$ARGUMENTS`

#### 📋 **ISSUES IDENTIFIED**

**Critical Issues:**
1. [Specific issue with current impact]

**Schema Mismatches:**
1. [.yaml vs .b.R inconsistencies]

**Integration Problems:**
1. [File integration issues]

#### ✅ **SPECIFIC FIXES**

**Fix 1: [Issue Description]**

*Problem:*
```r
// Current problematic code
```

*Solution:*
```r
// Fixed code
```

*Files to Update:*
- `jamovi/$ARGUMENTS.r.yaml` - [specific changes]
- `R/$ARGUMENTS.b.R` - [specific changes]

**Fix 2: [Issue Description]**

*Problem:*
```yaml
# Current YAML structure
```

*Solution:*
```yaml
# Fixed YAML structure
```

*Rationale:* [Why this fixes the issue]

#### 🔄 **STEP-BY-STEP FIX PROCESS**

1. **Update Schema (.r.yaml)**
   ```yaml
   # Exact changes needed
   ```

2. **Modify Implementation (.b.R)**
   ```r
   # Exact code changes
   ```

3. **Regenerate Headers**
   ```bash
   Rscript -e "jmvtools::prepare()"
   ```

4. **Test Changes**
   - [ ] Verify no compilation errors
   - [ ] Test with sample data
   - [ ] Check error scenarios

#### 🧪 **VALIDATION STEPS**

**Testing Checklist:**
```r
# Test code to validate fixes
library(ClinicoPath)
data('histopathology')

# Test 1: Basic functionality
result <- $ARGUMENTS(data = histopathology, ...)

# Test 2: Error conditions
# ... specific tests
```

**Expected Behavior:**
- [What should work after fixes]
- [What error messages should improve]
- [What new functionality should be available]

#### 🚀 **IMPLEMENTATION COMMANDS**

**Quick Fix Application:**
```bash
# Copy-paste commands to apply fixes
./fix_$ARGUMENTS.sh  # If creating a script
```

**Manual Steps:**
1. Edit `jamovi/$ARGUMENTS.r.yaml` lines [X-Y]
2. Edit `R/$ARGUMENTS.b.R` lines [X-Y]
3. Run `Rscript -e "jmvtools::prepare()"`
4. Test in jamovi or R

#### 📈 **QUALITY IMPROVEMENT**

**Before Fix:**
- Status: ❌ [Issues]
- Problems: [List]

**After Fix:**
- Status: ✅ [Expected outcome]
- Improvements: [List]

#### 🔒 **PREVENTION STRATEGIES**

**Avoid Similar Issues:**
1. [Best practice recommendation]
2. [Validation pattern to follow]
3. [Testing approach to use]

Provide copy-paste ready fixes with clear implementation steps.