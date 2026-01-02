---
name: check-module
description: Unified systematic check of jamovi module functions with auto-discovery
interactive: true
args:
  module:
    description: Module name (wip, jjstatsplot, meddecide, jsurvival, OncoPathology, ClinicoPathDescriptives, or 'all' for current module)
    required: false
    default: all
  --batch:
    description: Run in batch mode without confirmations
    required: false
    default: false
  --auto-discover:
    description: Auto-discover functions from filesystem (default true)
    required: false
    default: true
  --functions:
    description: Comma-separated list of specific functions to check (overrides auto-discovery)
    required: false
  --parallel:
    description: Check functions in parallel (experimental)
    required: false
    default: false
  --profile:
    description: Check profile (quick, standard, comprehensive, release)
    required: false
    default: standard
  --output-format:
    description: Output format (dashboard, detailed, json)
    required: false
    default: dashboard
usage: /check-module [module] [--batch] [--auto-discover] [--functions=func1,func2] [--profile=standard]
---

# Unified Module Quality Checker with Auto-Discovery

You are an expert jamovi module developer performing systematic quality assessment across multiple functions.

## Module Auto-Discovery

**Default behavior:** Auto-discover all functions from the current module by scanning:
- `R/*.b.R` files (backend implementations)
- `jamovi/*.a.yaml` files (analysis definitions)
- Cross-reference to ensure 4-file completeness

**Module-specific function lists** (used when module parameter is provided):

### Module: wip
- tableone, summarydata, reportcat, alluvial, agepyramid, venn, vartree, crosstable, benford

### Module: jjstatsplot
- jjhistostats, jjscatterstats, jjcorrmat, jjbetweenstats, jjdotplotstats, jjwithinstats, advancedraincloud, jjbarstats, jjpiestats, jwaffle, statsplot2, jjarcdiagram, linechart, lollipop, raincloud, jjsegmentedtotalbar

### Module: meddecide
- decision, decisioncurve, decisioncompare, decisioncombine, enhancedroc, psychopdaroc, agreement, pathagreement, timedependentdca, diagnosticmeta, nogoldstandard, cotest, sequentialtests

### Module: jsurvival
- survival, multisurvival, survivalcont, condsurvival, finegray, curemodels, survivalpower, jointmodeling

### Module: OncoPathology
- ihcdiagnostic, ihccluster, pathsampling, stagemigration, biomarkerresponse, oddsratio

### Module: ClinicoPathDescriptives
- tableone, crosstable, crosstablepivot, conttables, conttablespaired, checkdata, dataquality, categorize, datecorrection

## Check Profiles

### Quick Profile
- File existence check
- Basic schema validation
- Critical errors only
- **Time:** ~30 seconds per function

### Standard Profile (default)
- All quick checks
- Argument integration (.a.yaml ↔ .b.R)
- Output population (.r.yaml ↔ .b.R)
- Error handling assessment
- **Time:** ~2 minutes per function

### Comprehensive Profile
- All standard checks
- Differential runs (argument effectiveness)
- Placeholder detection
- External docs comparison (if available)
- Code quality metrics
- **Time:** ~5 minutes per function

### Release Profile
- All comprehensive checks
- Notices coverage audit
- Clinical readiness assessment
- Mathematical/statistical correctness
- Production readiness scoring
- **Time:** ~10 minutes per function

## Auto-Discovery Algorithm

```
1. Scan R/*.b.R → extract base names
2. For each basename:
   a. Check jamovi/{basename}.a.yaml exists
   b. Check jamovi/{basename}.r.yaml exists
   c. Check jamovi/{basename}.u.yaml exists
3. Classify functions by completeness:
   - Complete: All 4 files present
   - Partial: Missing 1-2 files
   - Broken: Missing 3+ files
4. If module parameter provided, filter to module-specific list
5. If --functions parameter provided, use explicit list
```

## Execution Order & Workflow

**Per-Function Loop:**

1. **Scan & Discover** — Auto-detect all functions or use provided list
2. **Sort by Priority** — Complete functions first, then partial, then broken
3. **Plan** — Print concise checklist for current function
4. **Confirm** (unless --batch) — Single confirmation per function: *"Proceed to check `<function>`?"*
5. **Execute Checks** — Run all checks for selected profile
6. **Report** — Output findings in selected format
7. **Advance** — Move to next function

**Parallel Mode (--parallel):**
- Launch checks for multiple functions concurrently
- Collect results and aggregate
- Report consolidated dashboard at end

## Response Format

### Dashboard Format (default)

```
📊 MODULE QUALITY DASHBOARD
═══════════════════════════════════════════════════════════════

Module: {module_name}
Functions Checked: {count}
Profile: {profile}
Date: {date}

┌──────────────────┬─────────┬──────────┬──────────┬────────┐
│ Function         │ Status  │ Critical │ Schema   │ Ready? │
├──────────────────┼─────────┼──────────┼──────────┼────────┤
│ tableone         │ ✅      │ 0        │ 0        │ ✅     │
│ summarydata      │ ⚠️      │ 0        │ 2        │ ⚠️     │
│ reportcat        │ ❌      │ 3        │ 5        │ ❌     │
└──────────────────┴─────────┴──────────┴──────────┴────────┘

🔥 CRITICAL ISSUES SUMMARY
──────────────────────────

Functions Needing Immediate Attention:
1. reportcat - Missing required error handling for empty data
2. reportcat - Unpopulated output: summary_table
3. reportcat - Schema mismatch: .a.yaml option 'groupBy' not used in .b.R

⚡ HIGH PRIORITY FIXES
─────────────────────

Schema Mismatches (Common Pattern):
• 5 functions have unused .a.yaml options
• Fix template: Ensure all options referenced or mark as deprecated

Integration Issues (Common Pattern):
• 3 functions have unpopulated .r.yaml outputs
• Fix template: Add setter calls in .run() method

📋 RELEASE READINESS
────────────────────

✅ Production Ready: tableone, summarydata, crosstable
⚠️ Minor Issues: venn, vartree (need error handling improvements)
❌ Needs Work: reportcat, alluvial (critical schema mismatches)
🚫 Missing/Broken: None

📈 QUALITY METRICS
──────────────────

• Overall Module Health: 7.5/10
• Functions Production-Ready: 75%
• Common Issue Patterns: 3 (schema drift, missing error handling, unpopulated outputs)
• Estimated Fix Time: 4-6 hours

🎯 RECOMMENDED NEXT STEPS
──────────────────────────

1. Fix critical issues in reportcat (run: /fix-function reportcat schema)
2. Add error handling to venn, vartree (run: /fix-notices venn vartree)
3. Run release profile on production-ready functions (run: /check-module wip --profile=release --functions=tableone,summarydata,crosstable)
```

### Detailed Format

Full per-function reports with code excerpts, line numbers, and specific fix recommendations.

### JSON Format

Machine-readable output for CI/CD integration:

```json
{
  "module": "wip",
  "profile": "standard",
  "timestamp": "2026-01-02T10:30:00Z",
  "summary": {
    "total_functions": 9,
    "production_ready": 3,
    "needs_work": 2,
    "broken": 0
  },
  "functions": [
    {
      "name": "tableone",
      "status": "ready",
      "critical_issues": 0,
      "schema_issues": 0,
      "files": {
        "a_yaml": true,
        "b_r": true,
        "r_yaml": true,
        "u_yaml": true
      }
    }
  ],
  "issues": []
}
```

## Batch Mode Behavior

When `--batch` is enabled:
- Skip all per-function confirmations
- Run uninterrupted scans across all functions
- Still prompt for **destructive changes** (if any recommended fixes would delete code)
- Produce consolidated summary at end
- Update quality dashboard in single operation

## Parallel Execution

When `--parallel` is enabled:
- Launch up to 4 concurrent check processes
- Each process handles one function independently
- Results aggregated when all complete
- Faster for large modules (10+ functions)
- May use more memory

**Recommendation:** Use parallel mode for comprehensive/release profiles on large modules.

## Examples

```bash
# Check current module with auto-discovery (standard profile)
/check-module

# Check specific module in batch mode
/check-module jjstatsplot --batch

# Quick check of specific functions
/check-module wip --functions=tableone,summarydata --profile=quick

# Comprehensive check with parallel execution
/check-module meddecide --profile=comprehensive --parallel --batch

# Release readiness check for production
/check-module jsurvival --profile=release --output-format=detailed

# Auto-discover all functions and check thoroughly
/check-module all --auto-discover --profile=comprehensive

# JSON output for CI/CD pipeline
/check-module --profile=standard --output-format=json > module-quality.json
```

## Integration with Other Commands

After identifying issues, use targeted commands for fixes:

```bash
# After check-module identifies schema issues
/check-module wip --profile=standard
# → Shows reportcat has schema issues

# Fix specific function
/fix-function reportcat schema

# After check-module identifies missing notices
/check-module wip --profile=release
# → Shows venn needs better error handling

# Add notices
/fix-notices venn --apply

# Comprehensive review of specific function
/review-function tableone
```

## Configuration File Support

Create `.claude/module-check-config.yaml` for persistent settings:

```yaml
default_profile: standard
batch_mode: false
auto_discover: true
parallel_execution: false
output_format: dashboard

module_aliases:
  stats: jjstatsplot
  survival: jsurvival
  decision: meddecide

custom_function_groups:
  core_descriptives:
    - tableone
    - summarydata
    - crosstable

  core_plots:
    - raincloud
    - waterfall
    - swimmerplot
```

## Performance Notes

**Function check times by profile:**
- Quick: ~30s per function
- Standard: ~2min per function
- Comprehensive: ~5min per function
- Release: ~10min per function

**Optimization strategies:**
- Use `--profile=quick` for rapid iteration
- Use `--parallel` for large batches
- Use `--functions=` to target specific issues
- Cache results between runs (future feature)

## Quality Assurance

This command performs the following validations:
- ✅ File existence and completeness
- ✅ YAML syntax validation
- ✅ R code parsing
- ✅ Schema consistency across files
- ✅ Option usage verification
- ✅ Output population verification
- ✅ Error handling completeness
- ✅ Clinical readiness (release profile)
- ✅ Mathematical correctness (release profile)

Focus on identifying patterns that affect multiple functions and provide efficient batch fixes where possible.
