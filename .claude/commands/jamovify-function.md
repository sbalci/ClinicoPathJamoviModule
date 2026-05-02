---
name: jamovify-function
description: Scan a jamovi function for places that should use jmvcore helpers (asFormula, composeFormula, naOmit, toNumeric, reject, tryNaN, …) and interactively apply the migrations
interactive: true
args:
  function_name:
    description: Name of the jamovi function to scan (e.g., crosstable, survival, decisiongraph)
    required: true
    autocomplete: functions
  --dry-run:
    description: Show proposed migrations without modifying files (default true)
    required: false
    default: true
  --apply:
    description: Apply migrations after the user confirms each pattern group
    required: false
    default: false
  --pattern:
    description: Restrict to one pattern group (formula, na, numeric, error, term, source, all)
    required: false
    default: all
  --backup:
    description: Write .bak copies before applying changes
    required: false
    default: true
  --run-prepare:
    description: Run jmvtools::prepare() after applying changes to verify the function still compiles
    required: false
    default: true
usage: /jamovify-function <function_name> [--pattern=all] [--dry-run] [--apply] [--backup]
examples:
  /jamovify-function crosstable                       # Scan only, no changes
  /jamovify-function survival --pattern=formula       # Only show formula-related opportunities
  /jamovify-function decisiongraph --apply            # Confirm + apply each migration group
  /jamovify-function tableone --pattern=error --apply # Only convert stop()/warning() to jmvcore::reject
---

# Jamovify a Function — Migrate to jmvcore Helpers

You are an expert jamovi developer. Your job is to scan **one** jamovi function (`$ARGUMENTS`) and surface every place where it could use a `jmvcore` helper instead of base-R or hand-rolled code, then walk the user through applying those migrations.

**Scope — stay focused.**
- Read only the target function's files: `R/<fn>.b.R`, `R/<fn>.h.R` (read-only, generated), `jamovi/<fn>.a.yaml`, `jamovi/<fn>.u.yaml`, `jamovi/<fn>.r.yaml`, and any `jamovi/js/<fn>.js`.
- Do **not** touch unrelated functions. If a finding spans multiple functions, note it and stop.
- Do **not** change behavior. Every migration must be drop-in equivalent — the only intended difference is added safety (e.g., allowlisted formula parsing) or attribute preservation.

**Reference docs** the user already maintains:
- `vignettes/jamovi_module_patterns_guide.md` — primary patterns guide
- `vignettes/jamovi_b_R_guide.md` — backend conventions
- `vignettes/jamovi_formula_guide.md` — formula patterns
- `CLAUDE.md` — project conventions and known pitfalls

---

## Pattern Catalog (what to look for)

Group each finding under one of these. The **Pattern** column is the regex/idea to grep for; the **Replace with** column is the jmvcore call.

### Group: `formula`
| Pattern in code | Replace with | Why |
|---|---|---|
| `stats::as.formula(<string>)`, `as.formula(<string>)` where the string includes `self$options$...` or any user-supplied name | `jmvcore::asFormula(<string>)` | Allowlist-validated parsing. Blocks `y ~ I(system('whoami'))`-style injection. (jamovi 2.7.27+) |
| `as.formula(<string built only from literals>)` | `jmvcore::asFormula(<string>)` | Still safer; same behavior on safe input |
| `paste0(dep, " ~ ", paste(terms, collapse=" + "))` and similar hand-built formula strings | `jmvcore::constructFormula(dep, terms)` or `jmvcore::composeFormula(lhs, rhs)` | Handles backtick-quoting of names with spaces / special chars |
| `paste0("\`", varname, "\`")` (manual backtick quoting of a variable name) | `jmvcore::composeTerm(varname)` | Correct quoting of single terms including interactions |
| `vapply(terms, function(t) paste0("\`", t, "\`"), …)` etc. | `jmvcore::composeTerms(listOfComponents)` | Vector form |
| Hand-coded display strings like `paste(comp, collapse=":")` for showing a term | `jmvcore::stringifyTerm(comp)` | Display-only renderer; respects `getOption("jmvTermSep")` |
| Manual splitting of `"a:b:c"` into components | `jmvcore::decomposeTerm` / `decomposeTerms` | Inverse of compose |
| Hand parsing of `formula(model)` into LHS/RHS lists | `jmvcore::decomposeFormula(formula)` | |

### Group: `na`
| Pattern | Replace with | Why |
|---|---|---|
| `stats::na.omit(df)`, `na.omit(df)` on a data frame whose columns carry jamovi attributes (`measureType`, `values`, labels) | `jmvcore::naOmit(df)` | Drops NA rows **and preserves column attributes** — losing them later breaks labelled-factor handling |

Skip when the input is a plain numeric/logical vector with no attributes — `na.omit` is fine there.

### Group: `numeric`
| Pattern | Replace with | Why |
|---|---|---|
| `as.numeric(factor)` where the factor came from jamovi options (likely has a `values` attribute) | `jmvcore::toNumeric(x)` | Honors the `values` attribute (jamovi factor coding) instead of returning level indices |
| `is.numeric(x) \|\| !is.na(suppressWarnings(as.numeric(x)))` and similar "could this be numeric?" checks | `jmvcore::canBeNumeric(x)` | One call, no warnings |
| `subset(df, select = cols)` followed by manual reattachment of attributes | `jmvcore::select(df, cols)` | Preserves attributes on the data frame and columns |
| `tryCatch(<expr>, error = function(e) NaN)` for a stat computation | `jmvcore::tryNaN(<expr>)` | Same intent, less boilerplate |
| `match(x, table, nomatch = -1)` used purely for branching on "found / not found" | `jmvcore::matchSet(x, table)` | Returns `-1` directly; reads more clearly |

### Group: `error`
| Pattern | Replace with | Why |
|---|---|---|
| `stop(<msg>)` / `stop(sprintf(<msg>, …))` in user-facing validation paths inside `.run()` / `.init()` | `jmvcore::reject(<format>, code = NULL, …)` | Surfaces a structured error to the jamovi UI; supports `{}` placeholders |
| `tryCatch(..., error = function(e) e)` then checking `inherits(e, "error")` | `jmvcore::isError(x)` + `jmvcore::extractErrorMessage(x)` | Idiomatic |
| `sprintf("the %s was %s", a, b)` for user-facing messages | `jmvcore::format("the {} was {}", a, b)` | Same {} placeholder style as `reject`; `context = "R"` quotes for `.asSource()` output |

Do **not** convert internal-only `stop()` calls (programming errors, `match.arg` failures); those are not user-facing.

### Group: `term`
Already covered in `formula` — `composeTerm`/`composeTerms`/`stringifyTerm`/`decomposeTerm`/`decomposeTerms`. Keep the group split so users can opt in/out per pattern type.

### Group: `source`
| Pattern | Replace with | Why |
|---|---|---|
| `.asSource()` / `.sourcifyOption()` building R code via manual `paste0` and `deparse` | `jmvcore::sourcify(object, indent)` for objects, `jmvcore::format(str, …, context = "R")` for templates | Correct quoting of strings, lists, NULLs in generated R code |
| Manual `paste0('"', x, '"')` to quote a string for syntax output | `jmvcore::format("{}", x, context = "R")` | |

### What NOT to flag
- `marshalFormula` / `marshalData` — these are for analyses that accept a user-typed formula option (rare in this codebase). Mention only if you actually see a formula option in `.a.yaml`.
- `toB64` / `fromB64` — only relevant when emitting R code that references variables with non-syntactic names. Mention only if the function already does manual encoding.
- Anything under `.h.R` — that's generated; never edit.

---

## Workflow

### 1. Resolve the function
- Confirm `R/<fn>.b.R` exists. If not, `ls R/ | grep -i <fn>` and ask which one was meant.
- Read the four YAMLs and the `.b.R`. Skip the `.h.R`.

### 2. Scan
For each pattern group selected by `--pattern`:
- Run targeted `grep -n` over the function's files.
- For each hit: capture the **file**, **line number**, the **current line**, and the **proposed replacement line**.
- Skip false positives (see "What NOT to flag" above and the per-pattern caveats).

### 3. Report (always shown, dry-run or not)

Use this exact structure so the user can scan it quickly:

```
🛠  JAMOVIFY: <function_name>
Scanned: R/<fn>.b.R, jamovi/<fn>.a.yaml, jamovi/<fn>.u.yaml, jamovi/<fn>.r.yaml[, jamovi/js/<fn>.js]

── Group: formula  (N findings) ──
  R/<fn>.b.R:123
    -  formula <- as.formula(paste0(dep, " ~ ", rhs))
    +  formula <- jmvcore::asFormula(paste0(dep, " ~ ", rhs))
    Why: user-supplied `dep` flows into the formula; allowlist-validate it.

  R/<fn>.b.R:145
    -  rhs <- paste(vapply(terms, function(t) paste0("`", t, "`"), ""), collapse=" + ")
    +  rhs <- jmvcore::composeTerms(as.list(terms))
    Why: handles names with spaces correctly.

── Group: na  (1 finding) ──
  …

── Group: error  (0 findings) ──

Summary: 6 findings across 3 groups in 1 file.
```

If a group has 0 findings, still print the heading with `(0 findings)` so the user knows it was checked.

### 4. Decide

- If `--dry-run` is set (default), **stop here** and tell the user how to apply:
  ```
  Run with --apply to walk through each group and confirm before changes are written.
  ```
- If `--apply` is set, proceed to step 5.

### 5. Interactive apply (one prompt per non-empty group)

For each group with at least one finding, ask **one** question:

```
Apply N migration(s) in group "formula"?
  [y] yes — apply all in this group
  [n] no — skip this group
  [s] show diff again
  [p] pick — choose specific findings (comma-separated line numbers)
```

When the user accepts:
- If `--backup` is on, copy the file to `<file>.bak.<timestamp>` once per file before the first edit.
- Apply edits with the `Edit` tool one finding at a time. Use enough surrounding context in `old_string` to make each match unique.
- After each group, print `✓ Group "<name>": applied X / N`.

### 6. Verify

If anything was applied:
- Run `Rscript -e 'parse("R/<fn>.b.R"); cat("parse OK\n")'` — fail fast on syntax errors.
- If `--run-prepare` is on, run `Rscript -e 'jmvtools::prepare()'` and report any errors/warnings related to the function.
- Print a final summary:
  ```
  Applied: X migrations across Y files
  Backups: <list of .bak files> (or "none, --backup=false")
  Verification: parse OK, jmvtools::prepare() clean
  Next step: review the diff with `git diff R/<fn>.b.R` and run the function in jamovi.
  ```

If verification fails, **do not** auto-revert — instead, point to the `.bak` files and stop. The user decides.

### 7. File "Out of scope" observations as inline TODOs (when asked)

If the report's "Out of scope" / "Other observations" section listed anything actionable, the user may follow up with "add other observations as TODO items" (or similar). When that happens:

- **Tag each TODO with a category prefix** so future readers can scan: `# TODO (security):`, `# TODO (UX):`, `# TODO (stub):`, `# TODO (jamovify):`, `# TODO (data hygiene):`, `# TODO (cleanup):`, `# TODO (correctness):`, `# TODO (forward-looking):`.
- **Consolidate file-wide patterns into one TODO** with cross-references to all sites it covers. Don't paste 20 near-identical TODOs at 20 sites; pick the most representative location and list the line numbers in the TODO body.
- **Skip positive observations.** Notes like "this code path is fine" or "this pattern is correct as-is" should NOT become TODOs — they create noise without helping future readers. Only file actionable items.
- **Skip already-filed TODOs.** If a TODO already exists for the same concern, don't duplicate it.
- **Place each TODO at the most relevant in-code location**, not at the top of `.run()`. A `.plot()` error TODO goes at the first `.plot` error site; a stub-options TODO goes near the option read; a security-future-proofing TODO goes near the line that would need escaping.
- After inserting, re-run the parse check to confirm the file still parses.

---

## Guardrails

- **Never** edit `.h.R` files; they are autogenerated.
- **Never** widen scope. If you spot an issue outside the target function, mention it in a final "Out of scope" note but don't fix it.
- **Never** change behavior. If a candidate migration could change behavior (e.g., replacing a deliberately label-stripping `na.omit` with `naOmit`), flag it as `⚠ behavior risk` and require explicit user confirmation, not just `[y]` for the group.
- **Never** add `library(jmvcore)` — call helpers as `jmvcore::fn()` to match project style. Helpers used in tight inner loops can be aliased once at the top of `.run()` if clearly beneficial; ask first.
- If the file has fewer than 5 candidates total, prefer showing all diffs inline over batching by group.

## Output style

- Be terse. The user reads many of these reports.
- Quote file paths as `R/<fn>.b.R:123` so they're clickable in the IDE.
- Use the diff arrows `-` / `+` exactly as shown above; no other markdown decoration inside diff blocks.
