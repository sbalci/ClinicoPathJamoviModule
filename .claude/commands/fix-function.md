---
name: fix-function
description: Generate and optionally apply specific fixes for jamovi function issues
interactive: true
args:
  function_name:
    description: Name of the jamovi function to fix
    required: true
    autocomplete: functions
  issue_type:
    description: Type of issue (schema, integration, error-handling, performance, all)
    required: false
    default: all
  --dry-run:
    description: Show fixes without applying them (default true for safety)
    required: false
    default: true
  --apply:
    description: Apply fixes immediately (use with caution)
    required: false
    default: false
  --backup:
    description: Create backups before applying fixes
    required: false
    default: true
usage: /fix-function <function_name> [issue_type] [--dry-run] [--apply]
examples:
  /fix-function reportcat                        # Preview all fixes (dry-run)
  /fix-function reportcat schema --apply         # Apply schema fixes
  /fix-function reportcat --dry-run=false --apply  # Apply all fixes
---



# Jamovi Function Issue Resolution with Safe Dry-Run

You are an expert jamovi developer tasked with providing specific, implementable fixes for the jamovi function `$ARGUMENTS`.

## Fix Target

Function: **`$ARGUMENTS`**
Issue Focus: **$ARGUMENTS** (if specified)

## Dry-Run vs. Apply Mode

### Dry-Run Mode (default, --dry-run or no flags)
**Safe preview mode** - recommended workflow:
- ✅ Analyze all files and identify issues
- ✅ Generate specific fix recommendations
- ✅ Show exact code changes (diffs)
- ✅ Display affected files and line numbers
- ❌ NO files are modified
- ❌ NO changes are applied
- 📋 Output: "Would fix X issues in Y files"

**Use for:**
- Understanding what needs fixing
- Reviewing changes before applying
- Generating fix documentation
- CI/CD validation

### Apply Mode (--apply)
**Active mode** - applies changes:
- ✅ All dry-run analysis
- ✅ Creates timestamped backups (if --backup=true)
- ✅ Applies code changes to files
- ✅ Runs jmvtools::prepare() if needed
- ⚠️ Modifies your source files
- 📋 Output: "Applied X fixes to Y files"

**Use for:**
- Applying reviewed fixes
- Batch corrections
- Automated workflows (with caution)

### Safety Features
- **Default is dry-run** - you must explicitly --apply
- **Automatic backups** - creates .bak files before changes
- **Confirmation prompts** - asks before destructive changes
- **Rollback info** - shows how to restore from backups

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