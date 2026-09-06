---
name: check-module
description: Scan and validate all functions in a jamovi module. Auto-discovers functions, batch quality checks, dashboard report
interactive: true
args:
  module:
    description: Module name (jjstatsplot, meddecide, jsurvival, ClinicoPathDescriptives, OncoPath, JamoviTest, or 'all' for every analysis in the current repo). Also accepts a raw menuGroup value (e.g. OncoPathD).
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

**Consult:** `vignettes/jamovi_module_patterns_guide.md` for correct patterns when evaluating functions.

You are an expert jamovi module developer performing systematic quality assessment across multiple functions.

## Module Auto-Discovery

**Default behavior:** Auto-discover all functions from the current module by scanning:
- `R/*.b.R` files (backend implementations)
- `jamovi/*.a.yaml` files (analysis definitions)
- Cross-reference to ensure 4-file completeness

**Module function lists are derived, never hardcoded.** In the umbrella repo every
analysis declares a `menuGroup:` in `jamovi/<name>.a.yaml`, and `_updateModules.R`
ships an analysis to a submodule only when that value matches the module's
production group **exactly** (anchored `menuGroup: <Group>$`). Suffixes route
elsewhere:

| `menuGroup:` value | Ships to | Meaning |
|---|---|---|
| `<Group>` | production submodule | released analysis |
| `<Group>T` | JamoviTest | under test/modification (see CLAUDE.md "JamoviTest Routing") |
| `<Group>D` | umbrella only | development, not shipped |
| `<Group>ExtraD` / `<Group>ExtraT` | umbrella only / JamoviTest | work-in-progress overflow |
| `Power #<module>` | production submodule named after `#` | a released analysis of THAT module (e.g. `Power #meddecide` IS a meddecide analysis); the `#module` is a YAML comment jamovi ignores, so it shows under the shared Power menu |
| `PowerT #<module>` | JamoviTest | that module's analysis, under test |

Module name → production `menuGroup`:

| Module argument | Production group | Sibling repo |
|---|---|---|
| jjstatsplot | `JJStatsPlot` | ../jjstatsplot |
| meddecide | `meddecide` | ../meddecide |
| jsurvival | `Survival` | ../jsurvival |
| ClinicoPathDescriptives | `Exploration` | ../ClinicoPathDescriptives |
| OncoPath | `OncoPath` | ../OncoPath |
| JamoviTest | every `*T` group | ../JamoviTest |

Resolve the list at run time (do not trust any list written into a playbook):

```bash
# production analyses of one module: its own group OR 'Power #<module>' (replace both names)
grep -l -E '^menuGroup: (meddecide|Power #meddecide)[[:space:]]*$' jamovi/*.a.yaml | xargs -n1 basename | sed 's/\.a\.yaml$//'

# whole map: count + names per menuGroup (production, D, T, Extra all visible)
for f in jamovi/*.a.yaml; do g=$(grep -E '^menuGroup:' "$f" | sed -E 's/menuGroup:[[:space:]]*//; s/[[:space:]]+$//'); echo "$g $(basename "$f" .a.yaml)"; done | sort | awk '{a[$1]=a[$1]" "$2; n[$1]++} END{for(k in a) print n[k], k":"a[k]}' | sort -k2
```

When run inside a sibling repo (a shipped submodule), the list is simply
`jamovi/0000.yaml` → `analyses:` → `name`. Always confirm the sibling copy of each
file is byte-identical to the umbrella before debugging it (`diff`), otherwise you
are chasing a stale build (memory: `reference_stale_generated_module_masquerades_as_bug`).

Snapshot for orientation only (2026-09-06; regenerate with the command above):

| Production group | n | Analyses |
|---|---|---|
| OncoPath | 4 | diagnosticmeta, ihcheterogeneity, swimmerplot, waterfall |
| meddecide | 12 | agreement, cotest, decision, decisioncalculator, decisioncombine, decisioncompare, decisioncurve, enhancedROC, lassologistic, nogoldstandard, psychopdaROC, sequentialtests (+ kappaSizeCI, kappaSizePower, kappaSizeFixedN at `Power #meddecide`, i.e. also production) |
| Survival | 9 | datetimeconverter, lassocox, multisurvival, oddsratio, outcomeorganizer, singlearm, survival, survivalcont, timeinterval |
| JJStatsPlot | 1 | statsplot2 (jjbarstats, jjpiestats and 21 others sit at `JJStatsPlotT`) |
| Exploration | 14 | agepyramid, alluvial, benford, categorize, checkdata, chisqposttest, crosstable, dataquality, outlierdetection, reportcat, summarydata, tableone, vartree, venn (+ nonparametric, pcacomponenttest, pcaloadingtest at `ExplorationT`) |

**Scanner false positives to expect** (seen on the 2026-09-02 OncoPath pass):
options read through a constructed name (`self$options[[paste0("milestone", i, "Name")]]`),
options used only by `.r.yaml` `visible:` (`show_*` toggles on Html panels), and
`private$asArgs` / `private$sourcifyOption` reported as phantom methods (jmvcore base
class). Verify each candidate in the code before listing it as a finding.
Also seen on the 2026-09-03 jsurvival pass: private methods defined in leading-comma style
(`,.executeAnalysis = function()`) missed by a `^\s*\.name = function` regex; `Notice$new` hits
that are commented out; `pkg::` hits inside strings/comments (oddsratio's inline epiR re-
implementation); local lists named `results` matching `results$x`; and `warning()` calls in a
file that collects them via `withCallingHandlers(... muffleWarning)` and renders them itself
(lassocox) — those are the module's notice mechanism, not a gap.
Also seen on the 2026-09-06 ClinicoPathDescriptives pass: a `type: Data` option has no
`jmvcore::Option` line in `.h.R` (not a stale header); every `insert(999`/`Notice$new`/`stop(`/
`warning(` hit sat inside a comment — strip comments before counting; an Image whose `renderFun:`
reads `self$data` directly never appears as `self$results$<name>` in `.b.R` (chisqposttest `plot`,
declared `name:  plot` with two spaces, so an exact-string grep misses it); `private$.optionOr("name",
default)` is constructed option access (outlierdetection); `switch(sty, arsenal = "tablestyle1", ...)`
selects a result item by bare string (crosstable). A second 2026-09-06 CPD pass added: "early `return()` without `.renderNotices()`" is a false positive wherever `.addNotice()` itself ends with `private$.renderNotices()` (all 9 CPD notice users) — read the helper before flagging; and a block-style `refs:` regex over-captures the following `- name:` items — resolve refs with a real YAML parse.

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
- **Release gates (jamovi library acceptance):**
  - `Version` >= 1.0.0 in DESCRIPTION + `jamovi/0000.yaml` (pre-1.0 = HIGH gate, cheapest fix)
  - Citation integrity: no used-but-undefined / case-mismatch / empty-author-year refs in `00refs.yaml` (run `/update-refs --all --validate`)
  - Dependency declarations complete, incl. `requireNamespace`-guarded/recommended packages the guard test can't see
  - No declared-but-unused Imports
  - UI label conventions (action-verb→noun, sentence-case controls); named-HTML-symbol-entities → Unicode
  - No orphaned non-analysis source files shipped in the build
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
4. If module parameter provided, keep only analyses whose `.a.yaml` `menuGroup:`
   matches the module's production group EXACTLY (table above) or is
   `Power #<module>`; a raw menuGroup value (e.g. `OncoPathD`, `SurvivalT`,
   `PowerT #meddecide`) is matched literally
5. If --functions parameter provided, use explicit list
6. If the module has a sibling repo, `diff` each analysis's 4 files + .b.R against
   it and report drift before checking anything
```

## Execution Order & Workflow

**Per-Function Loop:**

1. **Scan & Discover** — Auto-detect all functions or use provided list
2. **Sort by Priority** — Complete functions first, then partial, then broken
3. **Plan** — Print concise checklist for current function
4. **Confirm** (unless --batch) — Single confirmation per function: *"Proceed to check `<function>`?"*
   In a non-interactive/autonomous session there is nobody to answer: behave as `--batch` and say so
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
3. Run release profile on production-ready functions (run: /check-module ClinicoPathDescriptives --profile=release --functions=tableone,summarydata,crosstable)
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

# Check one production module in batch mode (4 analyses as of 2026-09)
/check-module OncoPath --batch

# Quick check of specific functions
/check-module ClinicoPathDescriptives --functions=tableone,summarydata --profile=quick

# Everything currently routed to JamoviTest (all *T menuGroups)
/check-module JamoviTest --profile=quick --batch

# A dev menuGroup, matched literally
/check-module OncoPathD --profile=quick --batch

# Comprehensive check with parallel execution
/check-module meddecide --profile=comprehensive --parallel --batch

# Release readiness check for production
/check-module jsurvival --profile=release --output-format=detailed

# JSON output for CI/CD pipeline
/check-module --profile=standard --output-format=json > module-quality.json
```

## Integration with Other Commands

After identifying issues, use targeted commands for fixes:

```bash
# After check-module identifies schema issues
/check-module ClinicoPathDescriptives --profile=standard
# → Shows reportcat has schema issues

# Fix specific function
/fix-function reportcat schema

# After check-module identifies missing notices
/check-module ClinicoPathDescriptives --profile=release
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
