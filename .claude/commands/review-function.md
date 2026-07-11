---
name: review-function
description: Code review a jamovi function: statistical correctness, clinical readiness, code quality, performance, maintainability
interactive: true
args:
  function_name:
    description: Name of the jamovi function to review
    required: true
    autocomplete: functions
usage: /review-function <function_name>
examples:
  /review-function tableone
  /review-function survival
  /review-function decision
---

_Note: This command intentionally avoids emoji and uses UI toggles to control visibility of natural‑language outputs._

# Detailed Jamovi Function Code Review

**Consult these guides** for correct patterns:
- `vignettes/jamovi_module_patterns_guide.md` -- comprehensive patterns
- `vignettes/jamovi_b_R_guide.md` -- backend implementation
- `vignettes/jamovi_tables_guide.md`, `jamovi_plots_guide.md` -- output patterns
- `vignettes/jamovi_i18n_guide.md` -- internationalization checklist

You are an expert R package and jamovi developer, and an expert biostatistician working closely with pathologists and clinicians. You are conducting a thorough, critical review of the jamovi function `$ARGUMENTS`, focusing on mathematical and statistical correctness, clinical readiness, code quality, best practices, performance, and maintainability.

## Review Target

Function: **`$ARGUMENTS`**

## Code Review Focus Areas

### Architecture & Design

- R6 class structure and inheritance
- Function modularization and separation of concerns
- Data flow and state management
- Error propagation and handling

### Implementation Quality

- Algorithm efficiency and performance
- Memory usage patterns
- Code readability and maintainability
- Following jamovi and R best practices

### Robustness & Security

- Input validation completeness
- Edge case handling
- Error message quality and helpfulness
- Data sanitization

### CRAN Compliance & Code Hygiene (checktor)

Run the CRAN pre-submission checker `checktor` and interpret the findings for **this function only**. checktor catches issues `R CMD check` misses (hardcoded seeds, global-env writes, missing `\value`, unsafe `library()`, unrestored `par()`, etc.).

**How to run (scoped to the function):**

```r
# install.packages("checktor")   # once
res <- checktor::checktor(".", verbose = FALSE, progress = FALSE)
i   <- checktor::issues(res)     # data.frame: category, check, file, line, location, message
#   NOTE: the `file` column is NA for several checks — parse the `location` column instead.
fn  <- "$ARGUMENTS"
mine <- i[grepl(fn, i$location, ignore.case = TRUE) |
          grepl(paste0("^", fn, "(Class)?\\.Rd$"), basename(i$location)), ]
print(mine[, c("check", "location", "message")])
```

For the whole-package summary use `checktor::tidy(res)` (per-check counts) and `checktor::prescribe(res)` (treatment hints). Always run `checktor` **in addition to** `devtools::check()`, never instead of it.

**Real vs false-positive (learned on this codebase — do not blindly "fix"):**

| Check | Usually REAL — fix | Often FALSE POSITIVE — leave (explain) |
|---|---|---|
| `seed_setting` | hardcoded `set.seed(42)` → user-configurable `seed` **option** (add to `.a.yaml` + `.u.yaml` + `.b.R`, `is.null()` fallback to the old literal); if the arg is arithmetic (`start*100`) compute into a variable first (`set.seed(iter_seed)`) so the literal leaves the `set.seed()` call | — |
| `globalenv_mod` | a `<<-` in a **tryCatch BODY** (not a handler) genuinely writes to `.GlobalEnv` while the method-frame local stays unchanged → a **real latent bug** (e.g. an always-`NA` statistic). Convert to `<-`. | a `<<-` inside an **error/warning-handler closure** writes to the enclosing **method frame**, not `.GlobalEnv` → functionally correct. Refactor only to satisfy the linter, never by naively swapping `<<-`→`<-` inside the handler (that silently drops the write). |
| `globalenv_mod` (`.Random.seed`) | manual `.Random.seed` save/restore → `withr::local_seed(seed)` (with a seed) or `withr::local_preserve_seed()` (no seed). Add `withr` to Imports. | the manual save/restore is *correct* RNG hygiene — only replace it, do not delete it |
| `value_tags` | missing `\value` → add `#' @return …` to the roxygen in the **`.b.R`** (for `xxxClass` generators, `@keywords internal` + one uniform line). Requires `devtools::document()` to regenerate the `.Rd`. | — |
| `library_in_pkg` | real `library()`/`require()` in function bodies → delete if calls are already `pkg::`-namespaced, else `requireNamespace()` guard + namespace; declare the pkg in **Imports** (jamovi users can't install Suggests) | `library(...)` text inside `sprintf()`/string literals that generate copy-ready code for the user |
| `option_changes` | `par()` that changes graphics settings → `oldpar <- graphics::par(no.readonly=TRUE); on.exit(graphics::par(oldpar), add=TRUE)` once per function | `options(datadist=...)` for `rms` — **leave it** (rms needs it to persist for downstream calibrate/nomogram/validate) |
| `tf_usage` | a real `T`/`F` used as a logical shorthand → `TRUE`/`FALSE` | a local **variable or named argument** literally named `T`/`F` (e.g. Kendall tied-count, `gsDesign::nSurv(T=...)`) → **scoped rename**, never `T→TRUE`; preserve output list keys |
| `acronyms`, `title_case` | — | checktor can't see parenthetical acronym expansions, and strips punctuation before the title-case test (so single-quoted `'jamovi'`/`'ggstatsplot'` still fire). Expanding / single-quoting is the CRAN-correct fix even though the finding persists. |
| `globalenv_mod` (`private$x[[k]] <<-`) | — | `private` is an environment reference, so plain `<-` modifies it in place — equivalent to `<<-`; switch to `<-` |

**Refactor patterns that preserve GUI behaviour** (verify each is byte-identical):
- Error handler that sets a fallback: pre-initialise the variable to the fallback **before** the tryCatch and make the handler a no-op (`error = function(e) NULL`); or capture — `err <- tryCatch({ …; NULL }, error = function(e) e); if (!is.null(err)) <fallback>`.
- Handler that *computes* a value: `x <- tryCatch({ <real> }, error = function(e) <fallback>)`.
- A counter/accumulator incremented in a nested function (`i <<- i+1`, `warned <<- TRUE`, `msgs <<- c(...)`): move it to an **environment** (`e <- new.env(); e$i <- 0; … e$i <- e$i + 1`), or, if it is in the same frame (a plain `for`/tryCatch body), just use `<-`.
- If a fix would change numeric output or you are not confident, **leave it and report** — never break clinical GUI functionality to satisfy a linter.

**Verify after fixing:** re-run `checktor`, `jmvtools::prepare()` (wires new options into `.h.R`) + `devtools::document()` (regenerates `.Rd` for `value_tags`), confirm the file parses, then **run the analysis on bundled data via `devtools::load_all()`** and drive the affected code path. For seed fixes, prove the same seed gives identical output and a different seed still runs.

### Static Analysis & Lint Hygiene (lintr)

Run `lintr` scoped to this function and interpret the findings. The repo has a root `.lintr` that disables the pure-style linters (`line_length`, `object_name`, `indentation`, `trailing_whitespace`, `commented_code`, `return`) so the signal is not drowned — forcing defaults yields ~7000 findings per `.b.R`; with `.lintr` applied it drops to a handful. Do **not** re-enable the style linters for this review.

**How to run (scoped to the function).** Use the *review* linter set below, not a bare `lint(path)`. Four of the highest-value bug linters (`sprintf_linter`, `unreachable_code_linter`, `duplicate_argument_linter`, `missing_argument_linter`) are **NOT in lintr's default set**, so a plain `lint(path)` silently skips them. This set enables them while replicating the repo `.lintr` style-suppression (passing `linters=` overrides the `.lintr` `linters:` field, so the style-off config must be repeated here):

```r
# lintr 3.3.x. NOTE: lintr::read_settings() is UNEXPORTED in 3.3.x — do not call it; it errors.
suppressMessages(library(lintr))
fn   <- "$ARGUMENTS"
path <- file.path("R", paste0(fn, ".b.R"))

review_linters <- linters_with_defaults(
  # style noise OFF (mirrors repo .lintr)
  commented_code_linter      = NULL, line_length_linter        = NULL,
  trailing_whitespace_linter = NULL, indentation_linter        = NULL,
  object_name_linter         = NULL, return_linter             = NULL,
  # high-value AST bug-catchers that are NOT default — enable explicitly
  sprintf_linter             = sprintf_linter(),
  unreachable_code_linter    = unreachable_code_linter(),
  duplicate_argument_linter  = duplicate_argument_linter(),
  missing_argument_linter    = missing_argument_linter()
)
df <- as.data.frame(lint(path, linters = review_linters))
print(sort(table(df$linter), decreasing = TRUE))
print(df[, c("line_number", "linter", "message")])
# A .b.R holds exactly one <fn>Class R6 object, so file scope == function scope.
# YAML under jamovi/ is not R and is not linted. Package load prints benign
# "Registered S3 method overwritten by ..." noise on stderr — ignore it
# (filter with `2>/dev/null` or `2>&1 | grep _linter`). A plain `lint(path)`
# (no linters= arg) auto-discovers ./.lintr and is fine for a quick pass, but
# only fires the default-set bug linters (seq_linter, equals_na_linter, T_and_F).
```

**CRITICAL R6 blind spot (verified empirically on this codebase — do not skip).** `lintr` cannot descend into method bodies defined *inside* `R6::R6Class(...)` for the two linters that need whole-function scope analysis. Across all 388 `.b.R` files, `object_usage_linter` and `vector_logic_linter` fire **zero** times — not because the code is clean, but because the linter is structurally blind there (every jamovi method is a `function(){}` passed as a `list()` argument to `R6::R6Class`, which their analysis skips). Consequence:

- **Never conclude "lintr passed, so there are no undefined variables and no `&`-in-`if()` bugs."** Those two classes must be checked **by hand** in `.b.R`, or by temporarily lifting the suspect method into a standalone top-level function and linting *that* (where both linters do fire — confirmed by control test).
- Every other (AST-pattern) linter fires normally inside R6 methods, so their findings are trustworthy.

**Real bugs `lintr` DOES catch inside `.b.R` (fix these).** All seven fire inside R6 method bodies (verified). A `*` marks the four that are **non-default** — they only run because the `review_linters` set above enables them explicitly:

| Linter | Flags | Fix | jamovi caveat |
|---|---|---|---|
| `seq_linter` | `1:ncol(x)`, `1:nrow(x)`, `1:length(x)`, `1:min(...)` | `seq_len(ncol(x))` / `seq_along(x)` / `seq_len(min(...))` | **The flagship.** `.run()` often executes on partially-specified analyses where `nrow(data)==0` or no variables are selected; `1:0` → `c(1,0)` iterates **backwards** (indexes col 1 then col 0) instead of skipping. Behavior-preserving when non-empty; only fixes the empty edge. ~88 real hits repo-wide. |
| `equals_na_linter` | `x == NA`, `x != NA` | `is.na(x)` / `!is.na(x)` | `== NA` is always `NA`, never `TRUE`; silently drops the missing-data branch and can corrupt the reported N. Clinical data carries NA in outcome/time/group columns. |
| `sprintf_linter` * | `sprintf()`/`gettextf()` format vs arg count/type mismatch | Match `%` specifiers to the args and their types | Result captions and table notes are built with `sprintf` wrapped in `.()` translation; a mismatch crashes `.run()` only when that branch runs on real data. |
| `unreachable_code_linter` * | code after `return()`/`stop()`/`next` | Remove dead code or move the `return`/`stop` | A stray early `return()` (debugging residue) makes the analysis silently produce **blank output** in jamovi — no error, just nothing. |
| `duplicate_argument_linter` * | the same named argument twice in one call | Remove the duplicate; keep the intended value | Long `ggplot`/`aes`/option-list chains built from `self$options`; a copy-pasted duplicate silently overrides a user-facing option. |
| `missing_argument_linter` * | empty required argument, e.g. `f(a, , b)` | Supply the arg / remove the stray comma | Leftover comma from refactoring a long `jmvcore`/plot call; errors only when that path runs, so it escapes casual testing. |
| `T_and_F_symbol_linter` | `T`/`F` used as booleans | `TRUE`/`FALSE` | **Confirm it is actually a boolean first** — in pathology data `T` (TNM T-stage), `F` (female), `T` (time) are legitimate identifiers/factor levels and must NOT be rewritten. |

**Style / advisory (do not block release):** `return_linter`, `pipe_consistency_linter` (magrittr `%>%` vs native `|>`), `semicolon_linter`. Suppressed via `.lintr` or non-blocking; mention only if egregious.

**Verify after fixing:** re-run the scoped lint (target: 0 real findings), confirm the file still parses (`parse("R/$ARGUMENTS.b.R")`), then drive the analysis on bundled data via `devtools::load_all()`. For `seq_linter` fixes specifically, exercise the empty/one-column edge case (no variables selected, zero-row filtered data) the fix targets, and confirm identical output on non-empty input.

### Documentation & UX

**Visibility rule:** Natural‑language summaries and educational/explanatory outputs must render **only when** the corresponding UI options are enabled by the user (see the `.u.yaml` checkboxes below). Keep these sections hidden by default unless selected.

- Code comments and self-documentation
- User interface clarity
- Help text and explanatory content
- Accessibility considerations
- Natural‑language summary of results (plain, copy‑ready).
- Explanatory output panel: what the function does, when/how to use it, assumptions/caveats, and a short user guide.

### Performance & Scalability

- Computational complexity
- Memory efficiency
- Large dataset handling
- Optimization opportunities

### Mathematical & Statistical Correctness

- Correctness of formulas, test statistics, and estimators (e.g., OR/HR/RR, AUC, ICC, kappa, RMST, DeLong, Fine–Gray).
- Appropriate choice of statistical methods for the intended design (paired vs independent, one‑sided vs two‑sided, parametric vs non‑parametric, survival vs binary, etc.).
- Proper handling of assumptions (normality, homoscedasticity, proportional hazards, independence, expected cell counts, etc.) and use of robust alternatives where needed.
- Correct construction and interpretation of confidence intervals, p‑values, and effect sizes (including direction and units).
- Treatment of ties, censoring, competing risks, and clustering when applicable.
- Handling of missing data (complete‑case vs imputation vs weighting) and clear communication of what is done.
- Alignment with reference implementations (e.g., base R, survival, pROC, irr, cmprsk, lme4, etc.) for the same inputs.

### Clinical & Release Readiness

- Are defaults clinically sensible for pathologists and oncologists (e.g., common cut‑offs, scales, and summaries)?
- Are outputs interpretable and safe to use in pathology/oncology reports (no misleading labels, correct units, clear directions of effect)?
- Are misuse risks mitigated (e.g., chi‑square with low expected counts, overfitted models, too few events per variable) with strong warnings or errors?
- Are explanatory texts and help sufficient for clinicians without advanced statistics training?
- Has the function behavior been validated against reference analyses and edge cases (documented test set or verification script)?
- Is the function stable and predictable across a range of dataset sizes and realistic clinical scenarios?
- Based on the above, is the function **ready for clinical-facing use and public release**, or does it require further validation/refinement?

### Clinician‑Friendly (Pathologist/Oncologist) Additions

- Plain‑language labels and tooltips for each option (avoid jargon; show examples: e.g., “Select tumor grade (G1/G2/G3)”).
- In‑app micro‑explanations for statistics (what the test answers clinically, assumptions, effect size meaning, minimal sample heuristics).
- Glossary panel (AUC, OR, HR, RMST, FDR, ICC, kappa, DeLong, Fine–Gray, etc.) with 1–2 line clinical interpretations.
- Guided mode (wizard): “Pick your outcome → choose groups → check assumptions → run → interpret outputs.”
- Contextual warnings for misuse (e.g., “Paired test selected but groups are independent”).
- Example interpretations beneath each key result (e.g., “An OR of 2.1 means the odds are ~2× higher in group A”).
- One‑click report sentences (auto‑generated paragraphs with placeholders filled from results; copy to clipboard).
- Defaults tuned to common clinical scenarios; show ‘Recommended’ badges.
- Accessibility & readability: larger font option, color‑blind‑safe palettes, avoid red‑green only.
- **Internationalization (i18n)**: Turkish/English support for labels, messages, help text, and report templates.
  - All user-visible strings wrapped with `.()` in R code
  - NAMESPACE imports `importFrom(jmvcore, .)`
  - Translation catalogs (.po files) exist and are complete
  - Medical terminology follows Turkish pathology standards
  - No hardcoded English-only messages
  - Placeholders use `{name}` format for translator flexibility
  - Complete phrases (not fragmented strings)
  - Reference: `vignettes/jamovi_i18n_guide.md`

### Clinician‑Friendly UX & Explanations

| Area | Status | Notes |
|---|---:|---|
| Plain‑language labels/tooltips | ☐ | |
| Micro‑explanations per option | ☐ | |
| Glossary entries present | ☐ | |
| Guided flow (wizard) | ☐ | |
| Misuse warnings/guards | ☐ | |
| Example interpretations in outputs | ☐ | |
| Report sentence templates | ☐ | |
| Sensible defaults & presets | ☐ | |
| Accessibility (CB‑safe, font) | ☐ | |
| **i18n (TR/EN) coverage** | ☐ | **See detailed checklist below** |
| Natural‑language summary in output | ☐ | |
| About/How‑to section present | ☐ | |
| Caveats & assumptions panel | ☐ | |
| Guidance links/examples | ☐ | |

### Internationalization (i18n) Detailed Checklist

**Reference:** See `vignettes/jamovi_i18n_guide.md` for comprehensive patterns and best practices.

| Area | Status | Notes |
|---|---:|---|
| **Setup** | | |
| NAMESPACE imports `importFrom(jmvcore, .)` | ☐ | Required for `.()` function |
| Translation catalogs exist (en.po, tr.po) | ☐ | In `jamovi/i18n/` |
| catalog.pot template created | ☐ | For Weblate integration |
| **Backend (.b.R files)** | | |
| Error messages wrapped with `.()` | ☐ | `stop(.("message"))` |
| Warning messages wrapped with `.()` | ☐ | `warning(.("message"))` |
| Table notes/labels wrapped with `.()` | ☐ | `table$setNote('key', .("text"))` |
| Dynamic messages use placeholders | ☐ | `.("Found {n} errors")` + `jmvcore::format()` |
| Complete phrases (not fragments) | ☐ | No string concatenation |
| No leading/trailing spaces | ☐ | `.("Message")` not `.(" Message ")` |
| Conditional text uses alternatives | ☐ | Not `.("Std ") + .("Residuals")` |
| Utility functions pass `self` | ☐ | `.()` needs `self` context |
| **YAML files (.a, .r, .u)** | | |
| User-facing strings in YAML | ☐ | Auto-extracted (no `.()` needed) |
| Column titles translatable | ☐ | `title` fields in .r.yaml |
| Option labels translatable | ☐ | `label` fields in .a.yaml |
| UI section labels translatable | ☐ | `label` fields in .u.yaml |
| **Content Quality** | | |
| Medical terms use TR standards | ☐ | Güven Aralığı, Tehlike Oranı, etc. |
| Statistical terms consistent | ☐ | Same term throughout module |
| Clinical terminology correct | ☐ | Matches Turkish pathology texts |
| Abbreviations introduced properly | ☐ | "Güven Aralığı (GA)" then "GA" |
| **Translation Files** | | |
| tr.po translations complete | ☐ | No empty `msgstr` |
| en.po up to date | ☐ | Reflects current code |
| Placeholders match in msgid/msgstr | ☐ | `{n}` in both |
| No fuzzy entries | ☐ | Review `#, fuzzy` markers |
| UTF-8 encoding | ☐ | Turkish characters display |
| **Testing** | | |
| Tested with Turkish language | ☐ | Change jamovi language setting |
| Translations display correctly | ☐ | No � characters |
| Placeholders fill correctly | ☐ | {n} replaced with values |
| Medical terms appropriate | ☐ | For Turkish pathologists |

## Review Response Format

### CODE REVIEW: `$ARGUMENTS`

**Overall Quality**: 1–5 (stars)  

**Maintainability**: HIGH/MEDIUM/LOW  

**Performance**: EXCELLENT/GOOD/NEEDS_WORK  

**User Experience**: EXCELLENT/GOOD/NEEDS_WORK  

**Mathematical/Statistical Correctness**: CORRECT / MINOR_ISSUES / MAJOR_ISSUES  

**Clinical & Release Readiness**: READY / NEEDS_VALIDATION / NOT_READY  

**CRAN Compliance (checktor)**: CLEAN / MINOR / BLOCKERS  — count only *real* findings; list checktor false positives separately  

**Static Analysis (lintr)**: CLEAN / MINOR / REAL_BUGS  — count only linters that fire inside R6 (`seq_linter`, `equals_na_linter`, `sprintf_linter`, `unreachable_code_linter`, `duplicate_argument_linter`, `missing_argument_linter`, `T_and_F_symbol_linter`); note that `object_usage`/`vector_logic` are blind in `.b.R` and were checked manually  

#### STRENGTHS

1. [Specific positive findings with code references]
2. [Well-implemented patterns]
3. [Good practices observed]

#### CRITICAL ISSUES

1. [Mathematical/statistical correctness problems (wrong formulas, tests, CI/p‑value calculations) with file:line references]
2. [Clinical safety or misuse risks (e.g., misleading defaults, lack of guards for low n/events, incorrect labels/units)]
3. [Performance bottlenecks and major design flaws impacting reliability or maintainability]

#### CHECKTOR FINDINGS (CRAN compliance)

**Real issues (fix, with `file:line`):**

1. [check + location + one-line remedy, e.g. `seed_setting waterfall.b.R:2432 → add user 'seed' option`]

**False positives (leave; state why):**

1. [check + location + reason, e.g. `globalenv_mod psychopdaROC.b.R:2791 → <<- writes to method frame, not .GlobalEnv`]

**Excluded by scope:** `missing_examples`, `package_size` (and `example_structure` unless requested).

#### LINTR FINDINGS (static analysis)

**Real bugs (fix, with `file:line`):**

1. [linter + location + one-line remedy, e.g. `seq_linter agreement.b.R:574 → 1:min(nrow, ncol) → seq_len(min(...))`; `equals_na_linter x.b.R:120 → == NA → is.na()`]

**Manual-only — lintr is blind inside R6, so state whether you checked by hand:**

1. Undefined / unused locals — `object_usage_linter` does **not** fire in `.b.R`; report the result of a manual scan (or an extract-to-standalone-function lint).
2. `&` / `|` in scalar `if()` / `while()` — `vector_logic_linter` does **not** fire in `.b.R`; report whether any need `&&` / `||` (and confirm the vectorized `&`/`|` in `ifelse`/`[`/`filter` were left unchanged).

**Style (suppressed via `.lintr`; non-blocking):** `return_linter`, `pipe_consistency_linter`, `line_length`, `object_name`, `indentation`, `trailing_whitespace`, `commented_code`.

#### IMPROVEMENT OPPORTUNITIES

1. [Code quality improvements with examples]
2. [Refactoring suggestions]
3. [Performance optimizations]

#### ENHANCEMENT SUGGESTIONS

1. [Feature improvements]
2. [User experience enhancements]
3. [Future-proofing recommendations]

#### **Clinician‑Friendly Improvements:**

- Provide **Example interpretation** blocks under tables/plots.
- Add **guided mode** that enforces a recommended sequence (variables → assumptions → run → interpret).
- Include **copy‑ready report sentences** with placeholders auto‑filled from results.
- Add **misuse detection** (e.g., warn if expected counts < 5 for chi‑square; suggest Fisher’s exact).
- Offer **clinical presets** (e.g., “2×2 diagnostic test,” “KM survival with median & 95% CI,” “ROC with DeLong CI”).
- Provide **TR/EN translations** and ensure medical terminology is consistent.
- Use **color‑blind‑safe** default palettes and increase table readability (thousands separators, units).

**Natural‑language summaries & Explanatory Outputs:**

- Add a top‑level **Summary** box with a plain‑language paragraph that names the test/model, the comparison, key effect (with CI) and p‑value, and one clinical interpretation sentence.
- Add an **About this analysis** panel that briefly explains what the function does, when to use it, inputs required, and typical outputs (with links to docs).
- Add a **Caveats & assumptions** panel that lists assumptions, data requirements (e.g., expected counts, proportional hazards), and common pitfalls; surface contextual warnings if violated.
- Provide a **How to use** checklist (variables → options → run → interpret), and, if possible, a mini example with mock numbers.

#### SPECIFIC RECOMMENDATIONS

**Architecture:**

```r
# Suggested refactoring
```

**Mathematical/Statistical:**

```r
# Corrections to formulas, test choices, CI/p-value computation, or assumptions handling
```

**Clinical & Release Readiness:**

- [What must change before clinicians/pathologists can safely rely on this function]
- [What validation (comparisons, simulations, unit tests) is still required]
- [A clear recommendation: READY / NEEDS_VALIDATION / NOT_READY, with justification]

#### ACTION ITEMS

- [ ] [Specific actionable item]
- [ ] [Another specific item]
- [ ] Add plain‑language tooltips.
- [ ] Insert example‑interpretation blocks for key outputs.
- [ ] Implement misuse guards (e.g., switch to Fisher’s exact when expected counts < 5).
- [ ] Add natural‑language **Summary** box with copy‑ready text.
- [ ] Add **About this analysis** panel (what/when/how/outputs).
- [ ] Add **Caveats & assumptions** panel with contextual warnings.
- [ ] Run `checktor` and resolve real CRAN findings (seeds → options, `<<-`/`.Random.seed`, missing `\value`, `library()`); note false positives.
- [ ] Re-run `checktor` + `prepare()`/`document()` and drive the analysis on bundled data to confirm no functionality is lost.
- [ ] Run scoped `lintr` and fix real findings (`seq_linter` `1:ncol`/`1:nrow`/`1:min` → `seq_len`/`seq_along`; `== NA` → `is.na`; `sprintf` format/arg mismatches; unreachable code after `return()`).
- [ ] Manually check what `lintr` cannot see inside `.b.R` (undefined/unused vars, `&`/`|` in scalar `if()`), since `object_usage_linter`/`vector_logic_linter` are blind inside `R6::R6Class`.
- [ ] [Enhancement opportunity]
- [ ] [Code quality improvement]

**Performance:**

```r
# Optimization examples
```

**Error Handling:**

```r
# Better error handling patterns
```

**User Experience:**

```yaml
# Panels controlled by checkboxes; render only when enabled.
children:
  - type: ComboBox
    name: test
    label: "Group comparison test"
    options:
      - label: "t‑test (means)"
        value: ttest
      - label: "Mann–Whitney U (medians)"
        value: wilcox
      - label: "Welch t‑test (unequal variances)"
        value: welch
  - type: CheckBox
    name: assume_equal_var
    label: "Assume equal variances"


  - type: CollapseBox
    label: Output Options
    collapsed: true
    children:
      - type: Label
        label: Analysis Output
        fitToGrid: true
        children:
          # When unchecked, Summary/Explanations sections must not be rendered.
          - type: CheckBox
            name: showSummary
            label: "Show Summary (natural‑language)"
          - type: CheckBox
            name: showExplanations
            label: "Show Explanations (educational notes)"
```

```yaml
# .r.yaml (report sentences)
items:
  - name: report
    type: Html
    title: "Report sentence"
  - name: summary
    type: Html
    title: "Summary (natural‑language)"
    visible: false
  - name: explanations
    type: Html
    title: "Explanations"
    visible: false
```

```r
# .b.R (auto‑generated interpretation)
# Auto-generated interpretation sentence (always safe to compute; display controlled by UI)
interp <- sprintf(
  "The %s between %s and %s was %s (%.2f, 95%% CI %.2f–%.2f), p = %.3f.",
  if (test == "ttest") "difference in means" else "difference in distributions",
  g1, g2, stat_name, stat_value, ci_low, ci_high, pval
)
self$results$report$setContent(interp)

# Natural-language summary: only render when user enables 'Show Summary'
if (isTRUE(self$options$showSummary)) {
  summary_text <- sprintf(
    "We compared %s vs %s using %s. The key effect was %s (95%% CI %.2f–%.2f), p = %.3f. Clinically, this suggests %s.",
    g1, g2, stat_name, stat_value, ci_low, ci_high, pval, clinical_hint
  )
  self$results$summary$setVisible(TRUE)
  self$results$summary$setContent(summary_text)
} else {
  self$results$summary$setVisible(FALSE)
}

# Explanations (educational notes): only render when user enables 'Show Explanations'
if (isTRUE(self$options$showExplanations)) {
  expl <- paste0(
    "&lt;b&gt;What does this test answer?&lt;/b&gt; ", test_expl, "&lt;br/&gt;",
    "&lt;b&gt;Assumptions:&lt;/b&gt; ", assumptions_text, "&lt;br/&gt;",
    "&lt;b&gt;Effect size meaning:&lt;/b&gt; ", effect_expl
  )
  self$results$explanations$setVisible(TRUE)
  self$results$explanations$setContent(expl)
} else {
  self$results$explanations$setVisible(FALSE)
}
```

---

## Related Commands

- `/check-function` -- Validate and auto-fix function issues with preset profiles
- `/fix-function` -- Apply targeted fixes for specific issues
- `/document-function` -- Generate comprehensive documentation suite
