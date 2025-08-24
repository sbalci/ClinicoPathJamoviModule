---
name: check-module-wip
description: Perform systematic check of wip jamovi functions
usage: /check-module-wip [--batch]
---
# Batch Systematic Module Quality Check

You are an expert jamovi module developer performing a comprehensive quality assessment of multiple functions in the wip jamovi module.

## Functions to Analyze

Please systematically check these key functions:

**Functions to Analyze:**

- `tableone`
- `summarydata`
- `reportcat`
- `alluvial`
- `agepyramid`
- `venn`
- `vartree`
- `crosstable`
- `benford`

## Analysis Approach

For each function, perform a focused check on:

1. **File Completeness** - All 4 required files present
2. **Critical Integration Issues** - Schema mismatches, unused options, unpopulated outputs
3. **Error Handling Quality** - User-friendly vs cryptic errors
4. **Production Readiness** - Ready for release vs needs work

## Execution Order & Prompting Policy (Per-Function Loop)

Process **one function at a time** and complete **all steps for that function** before moving to the next.

**Order:** Follow the order in the “Functions to Analyze” list.

**Per‑Function Workflow:**

1. **Plan** — Print a concise checklist for the current function (`<function>`):
   - Files present? (`.a.yaml`, `.u.yaml`, `R/<function>.r`, `jamovi/<function>.js`)
   - Schema match? (options ↔ results ↔ R checks)
   - Integration compile? (compute guards, empty visibility, errors)
   - Render demo (plots/tables) if applicable
   - Ready status decision & issues summary
2. **Single confirmation** — Ask once: *“Proceed to run all checks & fixes for `<function>`?”*  **(skip when Batch Mode is enabled)**  
   If approved, **do not ask again** for sub‑steps within the same function unless a destructive change is proposed.
3. **Execute all checks** — Run the full checklist sequentially for `<function>` without pausing for extra approvals.
4. **Report block** — Output a self‑contained report for `<function>` with findings, diffs, and next actions.
5. **Advance** — Move to the next function and repeat steps 1–4.

**Destructive change rule:** If a change deletes user code or rewrites public APIs, request explicit confirmation for that change only; otherwise continue uninterrupted.

## Batch Mode (Assume‑Yes)

Activate batch mode to auto‑approve the single per‑function confirmation and run uninterrupted scans across all functions.

**How to enable:**

- Pass the flag in your slash command message: `--batch`
- *or* set an environment/context variable visible to the agent: `BATCH_APPROVE=true`

**Behavior when enabled:**

- Skip the **Single confirmation** step for each function.
- Still prompt for approval on **destructive changes** (API rewrites, file deletions).
- At the end, produce a **consolidated summary** before updating the 📊 dashboard.

### 📊 MODULE QUALITY DASHBOARD

| Function    | Status   | Critical Issues | Schema Issues | Ready? |
| ----------- | -------- | --------------- | ------------- | ------ |
| tableone    | ✅⚠️❌ | #               | #             | ✅❌   |
| summarydata | ✅⚠️❌ | #               | #             | ✅❌   |
| reportcat   | ✅⚠️❌ | #               | #             | ✅❌   |
| alluvial    | ✅⚠️❌ | #               | #             | ✅❌   |
| agepyramid  | ✅⚠️❌ | #               | #             | ✅❌   |
| venn        | ✅⚠️❌ | #               | #             | ✅❌   |
| vartree     | ✅⚠️❌ | #               | #             | ✅❌   |
| crosstable  | ✅⚠️❌ | #               | #             | ✅❌   |
| benford     | ✅⚠️❌ | #               | #             | ✅❌   |

### 🔥 CRITICAL ISSUES SUMMARY

**Functions Needing Immediate Attention:**

1. **[function]** - [critical issue]
2. **[function]** - [critical issue]

### ⚡ HIGH PRIORITY FIXES

**Schema Mismatches (Common Pattern):**

- [Pattern description with fix template]

**Integration Issues (Common Pattern):**

- [Pattern description with fix template]

### 📋 RELEASE READINESS

**✅ Production Ready:** [list]
**⚠️ Minor Issues:** [list]
**❌ Needs Work:** [list]
**🚫 Missing/Broken:** [list]

### 🎯 RECOMMENDED WORKFLOW

1. **Per‑Function Sweep:** Iterate functions in listed order, finishing all steps per function (Plan → Single confirmation → Execute → Report) before moving on. **If \`--batch\` is set, auto‑approve the per‑function confirmation.**
2. **Batch Status Update:** After all functions complete, update the 📊 dashboard once.
3. **Targeted Fix PRs:** Group fixes by pattern (schema drift, role constraints, NA policy) and open minimal PRs.

### 📈 QUALITY METRICS

- **Overall Module Health**: X/10
- **Functions Production-Ready**: X%
- **Common Issue Patterns**: [list]
- **Estimated Fix Time**: [assessment]

Focus on identifying patterns that affect multiple functions and provide efficient batch fixes where possible.
