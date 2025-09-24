---
name: check-function
description: Run actionable checks and fixes for a jamovi function (favor functionality over prose)
interactive: true
args:
  function_name:
    description: Name of the jamovi function to check
    required: true
    autocomplete: functions
  apply_escape_vars:
    description: Enforce/implement escapeVariableNames logic for variables with spaces/special chars
    required: false
    default: true
  align_labelled_logic:
    description: Apply labelled logic parity (e.g., as in oddsratio) where relevant
    required: false
    default: true
  regroup_ui:
    description: Update .u.yaml to group related UI controls logically and improve usability
    required: false
    default: true
  remove_placeholders:
    description: Remove dummy code/hardcoded values; wire all behavior to inputs
    required: false
    default: true
  defaults_false:
    description: Set all checkbox defaults to false in .a.yaml to reduce compute
    required: false
    default: true
  style_welcome:
    description: Implement decisionpanel-style welcome/intro message
    required: false
    default: true
  run_prepare:
    description: Run jmvtools::prepare() after changes
    required: false
    default: true
  run_document:
    description: Run devtools::document() after changes
    required: false
    default: true
  gen_test_data:
    description: Generate realistic test data (data-raw/) and write CSV to data/
    required: false
    default: true
  move_project_files:
    description: Move CSVs to data/, generators to data-raw/, docs to vignettes/
    required: false
    default: true
  check_external:
    description: Compare against upstream docs on CRAN/GitHub (reference manuals, pkgdown, NEWS)
    required: false
    default: false
  cran_pkg:
    description: Upstream CRAN package name to compare against (e.g., jmv)
    required: false
  github_repo:
    description: Upstream GitHub repo in owner/name form (e.g., jamovi/jmv)
    required: false
  upstream_fn:
    description: Upstream function name if it differs from SANITIZED_FN
    required: false
usage: /check-function <function_name> [--apply_escape_vars] [--align_labelled_logic] [--regroup_ui] [--remove_placeholders] [--defaults_false] [--style_welcome] [--run_prepare] [--run_document] [--gen_test_data] [--move_project_files]
---

# Jamovi Function Check & Fixer

Perform concrete checks and apply fixes for `$ARGUMENTS` across `.a.yaml`, `.b.R`, `.r.yaml`, `.u.yaml`. Minimize prose; output concise checklists, diffs, and exact edits.

## Analysis Target

Function: **`$ARGUMENTS`**

### Argument normalization (safety)

Before proceeding, sanitize `$ARGUMENTS` to a base function name (call it **SANITIZED_FN**): drop any leading paths, then strip any of these suffixes: .a.yaml, .b.R, .r.yaml, .u.yaml. Use **SANITIZED_FN** consistently for all file paths and references below.

Please analyze these files:

- `jamovi/SANITIZED_FN.a.yaml` - Analysis definition (options/arguments)
- `R/SANITIZED_FN.b.R` - Backend implementation
- `jamovi/SANITIZED_FN.r.yaml` - Results definition (outputs)
- `jamovi/SANITIZED_FN.u.yaml` - User interface definition

External sources (if available and check_external=true):

- CRAN reference manual PDF – `https://cran.r-project.org/web/packages/$ARG_cran_pkg/$ARG_cran_pkg.pdf`
- CRAN NEWS – `https://cran.r-project.org/web/packages/$ARG_cran_pkg/NEWS`
- pkgdown reference – `https://$ARG_cran_pkg.tidyverse.org/` or project site if known
- GitHub repo – `https://github.com/$ARG_github_repo` (read `R/*.R`, `man/*.Rd`, `NEWS.md`, `README.md`)

- When `apply_escape_vars=true`, ensure **escapeVariableNames**-style handling for variables with spaces/special characters is present in `.b.R` and UI lookups; otherwise emit exact patch.

### Quick Actions (feature flags)

| Flag | Action |
|---|---|
| apply_escape_vars | Add/verify `escapeVariableNames` (or equivalent) for variable handling across options, results, and rendering. |
| align_labelled_logic | Mirror labelled-handling behavior used in `oddsratio` where applicable (factor levels, labels in tables/plots). |
| regroup_ui | Rewrite `.u.yaml` panels: group related options; co-locate toggles and their parameters; ensure `visible` rules are consistent. |
| remove_placeholders | Replace stubs and hardcoded values with input-driven logic; delete `TODO` scaffolds. |
| defaults_false | Set all checkbox defaults in `.a.yaml` to `false` to reduce compute cost. |
| style_welcome | Introduce decisionpanel-like welcome/intro HTML block with consistent styling. |
| run_prepare | Run `jmvtools::prepare()` and capture errors. |
| run_document | Run `devtools::document()` and capture errors. |
| gen_test_data | Create realistic generators under `data-raw/` and write CSVs into `data/`. |
| move_project_files | Ensure CSVs are under `data/`, generators under `data-raw/`, docs under `vignettes/`. |

## Systematic Evaluation Framework

### Core Checks (Actionable)

1) **Args Wiring (.a.yaml ↔ .b.R)**
   - Map every option: `.a.yaml` → `self$options$*` usage in `.b.R` (no unused options).
   - Enforce defaults; remove hardcoded constants.
   - If `defaults_false=true`, set all checkbox defaults to `false` and verify logic still computes.

2) **Outputs Wiring (.r.yaml ↔ .b.R)**
   - Ensure each output has a setter (`setContent()`, `setRow()`, `addColumn()`, `setState()` + renderer) and visibility rules are honored.
   - Emit a table of UNPOPULATED items with exact fixes.

3) **Variable Safety**
   - Implement/verify `escapeVariableNames` logic when `apply_escape_vars=true`.
   - Add tests for variables with spaces, punctuation, Unicode.

4) **Labelled Logic Parity**
   - When `align_labelled_logic=true`, mirror handling used in `oddsratio` (label display, level ordering, NA handling) and document diffs.

5) **UI Regrouping (.u.yaml)**
   - When `regroup_ui=true`, group related controls; colocate toggles with params; simplify panels; ensure `clearWith`/`refs` are correct.

6) **Remove Placeholders**
   - When `remove_placeholders=true`, replace template text and constant outputs; ensure computations read `self$data` and options.

7) **Welcome/Intro Styling**
   - When `style_welcome=true`, add decisionpanel-style intro (HTML/markdown) visible when no variables are selected.

8) **Validation & Errors**
   - Validate required vars; handle empty data and missing values; surface friendly errors.

9) **Post-Change Checks**
   - If `run_prepare=true`, run `jmvtools::prepare()` and report.
   - If `run_document=true`, run `devtools::document()` and report.

10) **Test Data & Layout**
   - If `gen_test_data=true`, add generator under `data-raw/` producing comprehensive CSV(s) under `data/`.
   - If `move_project_files=true`, move CSVs → `data/`, generators → `data-raw/`, docs → `vignettes/`.

### Output Format (Concise)

- **SUMMARY**: status + counts (args used / outputs populated).
- **ARG EFFECTS**: compact table (default → changed, effect YES/NO).
- **OUTPUT POPULATION**: compact table with missing setters.
- **PATCHES**: exact YAML/R snippets to insert/replace.
- **ERRORS**: results of `prepare()`/`document()`.

### Snippet Templates

**escapeVariableNames utility (inject into `.b.R` if missing):**
```r
.escapeVar <- function(x) {
  # mimic modelbuilder behavior
  gsub("[^A-Za-z0-9_]+", "_", make.names(x))
}
```

**Decision panel welcome block (.r.yaml item + .b.R population):**
```yaml
# in .r.yaml outputs
- name: welcome
  title: "Welcome"
  type: Html
  visible: (len(vars) == 0)
```
```r
# in .b.R
self$results$welcome$setContent("<div class='jmv-welcome'><h3>Start by selecting variables</h3><p>Configure options in the left panel.</p></div>")
```

**Checkbox defaults to false (.a.yaml example):**
```yaml
- name: use_bootstrap
  title: "Bootstrap CIs"
  type: Bool
  default: false
```

**UI regrouping (.u.yaml panel example):**
```yaml
- type: CollapseBox
  label: "Confidence Intervals"
  children:
    - type: CheckBox
      name: use_bootstrap
    - type: TextBox
      name: ci_level
```

### Minimal Differential Harness
(unchanged from previous, keep for quick effectiveness checks.)

#### TESTING CHECKLIST

- [ ] Variables with spaces/special chars
- [ ] Labelled factors parity
- [ ] All outputs populated
- [ ] All checkboxes default false
- [ ] Empty dataset handling
- [ ] prepare()/document() pass cleanly

