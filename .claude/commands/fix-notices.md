---
name: fix-notices
description: Auto-audit and propose/apply jamovi Notices (jmvcore::Notice) for a given function
interactive: true
args:
  function_name:
    description: Name of the jamovi function to fix (accepts SANITIZED_FN or any of the 4 core filenames)
    required: true
    autocomplete: functions
  --dry-run:
    description: Preview fixes without applying (default true for safety)
    required: false
    default: true
  --apply:
    description: Apply patches to files immediately
    required: false
    default: false
  --backup:
    description: Create timestamped backups before applying changes
    required: false
    default: true
  min_severity:
    description: Minimum notice severity to generate (info, warning, strong_warning, error)
    required: false
    default: info
    enum: [info, warning, strong_warning, error]
  clinical_profile:
    description: Use ClinicoPath clinical thresholds/templates (EPV, AUC, prevalence)
    required: false
    default: true
  insert_position:
    description: Preferred band for insertions (top, mid, bottom, auto)
    required: false
    default: auto
    enum: [top, mid, bottom, auto]
  patch_format:
    description: Output patch format (unified or context)
    required: false
    default: unified
usage: /fix-notices <function_name> [--dry-run] [--apply] [options]
examples:
  /fix-notices coxph                               # Preview notice fixes (safe)
  /fix-notices logreg --apply                      # Apply notice fixes
  /fix-notices diagnostic --dry-run --min_severity=warning
---

# Jamovi Notices Auto-Fixer

Audit `R/SANITIZED_FN.b.R` for missing/weak **jamovi Notices** and propose **minimal patches** to add or improve `jmvcore::Notice` usage, aligned with the official jamovi API and the ClinicoPath internal guide.

**Checks include**
- Missing **ERROR** on fatal preconditions (no selected vars, invalid types, zero rows after filtering)
- Missing **STRONG_WARNING** for reliability threats (assumption violations, few events, extreme prevalence)
- Missing **WARNING** for milder concerns (imputation performed, ties handling, convergence retries)
- Missing **INFO** summarizing key run parameters/sample sizes
- Positioning &amp; naming (unique `name=...`; ERROR at top; INFO at bottom)

## Behavior
1. **Normalize** to `SANITIZED_FN` (strip paths/suffixes .a.yaml/.b.R/.r.yaml/.u.yaml).
2. **Parse &amp; scan** `R/SANITIZED_FN.b.R` (and `jamovi/SANITIZED_FN.r.yaml` if needed).
3. **Detect triggers** using heuristics:
   - Required options referenced without guards (e.g., `self$options$time` used without precheck)
   - Early returns lacking a user-visible notice
   - Clinical edge-cases: EPV, AUC, prevalence, complete separation, NA inflation
4. **Synthesize notices** with correct `NoticeType`, wording, and **insert band** based on `insert_position` (or `auto`), ensuring that each notice message is a **single-line, plain-text string** (no `\n` or other newline characters).
5. **Generate patches** (unified diff) that:
   - Insert `jmvcore::Notice$new(...)` with deterministic, unique names
   - Add `self$results$insert(1|mid|999, notice)` calls
   - Never remove or downgrade existing Html results; Html blocks remain the place for rich, multi-line explanations, while notices act as concise, single-line banners pointing to them
   - Keep code idempotent by guarding with minimal helper blocks if needed
6. If `apply=true`, write the patches; otherwise, emit patch text only. Patches **must not** introduce newline characters inside `notice$setContent()` strings.

## Output
- **Audit summary** (counts by severity; missing vs present)
- **Proposed patches** as a single diff to `R/SANITIZED_FN.b.R` (and `.r.yaml` if needed)
- **Post-patch checklist**

## Content & co-existence rules

- Notices must be **plain text only** (no HTML markup).
- Notices are currently **single-line only**: do not include `\n` or any other newline characters inside `notice$setContent()`.
- Keep messages specific, numeric where possible, and actionable, but compact enough to fit on one line. If you need a “bullet-like” structure, emulate it within one string using separators such as ` • `, commas, or semicolons.
- Do **not** delete or replace existing Html results. Keep Html outputs for detailed, multi-line explanations and use notices as concise, single-line banners that reference or summarize those Html sections.

## Patch templates

### ERROR — missing required inputs
```diff
*** R/SANITIZED_FN.b.R
@@
+    # --- Jamovi Notice: required inputs ---
+    if (is.null(self$options$time) || is.null(self$options$event)) {
+        notice <- jmvcore::Notice$new(options=self$options, name='requiredInputs', type=jmvcore::NoticeType$ERROR)
+        notice$setContent('Time and Event variables are required. Please select both and re-run.')
+        self$results$insert(1, notice)
+        return()
+    }
```

### STRONG_WARNING / ERROR — low events (clinical profile)
```diff
*** R/SANITIZED_FN.b.R
@@
+    # --- Jamovi Notice: events per variable ---
+    if (!is.null(self$options$event) && exists('df', inherits=FALSE)) {
+        ev <- sum(df[[ self$options$event ]] == 1, na.rm=TRUE)
+        if (ev < 10) {
+            n <- jmvcore::Notice$new(options=self$options, name='tooFewEvents', type=jmvcore::NoticeType$ERROR)
+            n$setContent(sprintf('Too few events for reliable modeling (events=%d < 10). Consider simpler models or gathering more data.', ev))
+            self$results$insert(1, n)
+            return()
+        } else if (ev < 20) {
+            n <- jmvcore::Notice$new(options=self$options, name='lowEvents', type=jmvcore::NoticeType$STRONG_WARNING)
+            n$setContent(sprintf('Low number of events may compromise reliability (events=%d). Interpret coefficients and CIs with caution.', ev))
+            self$results$insert(1, n)
+        }
+    }
```

### STRONG_WARNING — extreme prevalence
```diff
*** R/SANITIZED_FN.b.R
@@
+    # --- Jamovi Notice: extreme prevalence ---
+    if (exists('df', inherits=FALSE) && !is.null(self$options$event)) {
+        p <- mean(df[[ self$options$event ]] == 1, na.rm=TRUE)
+        if (p < 0.05 || p > 0.95) {
+            n <- jmvcore::Notice$new(options=self$options, name='extremePrev', type=jmvcore::NoticeType$STRONG_WARNING)
+            n$setContent(sprintf('Outcome prevalence is extreme (%.1f%%). Predictive values and calibration may be unstable.', 100*p))
+            self$results$insert(1, n)
+        }
+    }
```

### INFO — analysis summary (bottom)
```diff
*** R/SANITIZED_FN.b.R
@@
+    # --- Jamovi Notice: completion summary ---
+    {
+        n <- jmvcore::Notice$new(options=self$options, name='analysisComplete', type=jmvcore::NoticeType$INFO)
+        n$setContent(sprintf('Analysis completed on %d rows%s.', nrow(self$data), if (!is.null(self$options$weights)) ' (weights applied)' else ''))
+        self$results$insert(999, n)
+    }
```

## Usage examples
```
/fix-notices coxph
/fix-notices logreg apply=true
/fix-notices diagnostic_test min_severity=warning clinical_profile=true insert_position=top
```

## Post‑patch checklist
- [ ] ERROR at top for fatal conditions
- [ ] STRONG_WARNING/WARNING where needed with numeric thresholds
- [ ] INFO summary at bottom
- [ ] Plain-text, specific, actionable wording
- [ ] Unique `name=` values; no collisions
- [ ] Re-run `/check-function-base` to verify coverage
