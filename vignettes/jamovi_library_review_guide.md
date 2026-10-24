# The jamovi Library Review Guide

**What the jamovi library reviewer actually checks — and how to pass first time.**

This guide is distilled from the real audit reports issued by the jamovi library
reviewer against this project's submodules, with external learning sources identified separately:

| Report | Module | Date |
|---|---|---|
| `jamovi-library-audit/2026-07-13 <Module>.md` (×5) | all five modules (round 1) | 2026-07-13 |
| `jamovi-library-audit/2026-08-17 ClinicoPathDescriptives.md` | ClinicoPathDescriptives | 2026-08-17 |
| `jamovi-library-audit/2026-08-17 jsurvival.md` | jsurvival | 2026-08-17 |
| `jamovi-library-audit/2026-08-17 meddecide.md` | meddecide | 2026-08-17 |
| `jamovi-library-audit/2026-08-18 OncoPath.md` | OncoPath | 2026-08-18 |
| `jamovi-library-audit/2026-08-18 jjstatsplot.md` | jjstatsplot | 2026-08-18 |
| `jamovi-library-audit/2026-09-15 jsurvival.md` | jsurvival (round 3) | 2026-09-15 |
| `jamovi-library-audit/2026-09-16 <Module>.md` (×4) | OncoPath, ClinicoPathDescriptives, jjstatsplot, meddecide (round 4) | 2026-09-16 |
| `jamovi-library-audit/2026-09-22 OncoPath.md` | OncoPath (round 5) | 2026-09-22 |
| `jamovi-library-audit/2026-10-23 jamovi library OncoPath 1.1.2 is on its way.md` | OncoPath (round 7; the report arrives in the library's "on its way" email for a release it is already publishing) | 2026-10-23 |
| `jamovi-library-audit/2026-10-23 jamovi library meddecide 1-1-2 is on its way.md` | meddecide (round 8; the first round to inspect the options panels and results, 20 findings on a build the gate called clean) | 2026-10-23 |
| User-supplied `2026-10-03 jYS.md` and `MODIFICATIONS_REPORT_EN.md` | external jYS 1.1.5; audit plus remediation account, not our closure evidence | 2026-10-03 / 2026-10-10 |

Every rule below is something the reviewer *actually raised*, on real files, with
severity attached. Nothing here is speculative. The only exception is the few items
marked **anticipatory**: they cover the jamovi 28.3 features (File option, Text result,
Image `mode: vector`), come from the release and dev.jamovi.org, and have not been
raised by the reviewer yet. [Section 30](#30-the-official-process-requirements-submission-updates)
is jamovi's own published submission and update process, not reviewer findings.
The external jYS pair is reconciled in [section 31](#31-learning-from-external-audits):
reported remedies and test counts remain attributed until independently verified.

---

## Table of Contents

1. [The pre-submission checklist](#1-the-pre-submission-checklist)
2. [Findings that recur across every module](#2-findings-that-recur-across-every-module)
3. [Rule: render functions must NULL-guard `image$state`](#3-rule-render-functions-must-null-guard-imagestate)
4. [Rule: HTML output must be theme-safe](#4-rule-html-output-must-be-theme-safe)
5. [Rule: `setVisible(FALSE)` is not an error mechanism](#5-rule-setvisiblefalse-is-not-an-error-mechanism)
6. [Rule: row structure that isn't result-dependent belongs in `.init()`](#6-rule-row-structure-that-isnt-result-dependent-belongs-in-init)
7. [Rule: no named HTML entities except the structural five](#7-rule-no-named-html-entities-except-the-structural-five)
8. [Rule: `warning()` is invisible to jamovi users](#8-rule-warning-is-invisible-to-jamovi-users)
9. [Rule: translatable strings are whole sentences](#9-rule-translatable-strings-are-whole-sentences)
10. [Rule: every package used must be declared — including base packages](#10-rule-every-package-used-must-be-declared--including-base-packages)
11. [Rule: dead code must not reference a schema that no longer exists](#11-rule-dead-code-must-not-reference-a-schema-that-no-longer-exists)
12. [Rule: UI label conventions](#12-rule-ui-label-conventions)
13. [Notices: the native element, and the `insert()` trap](#13-notices-the-native-element-and-the-insert-trap)
14. [Encoding review findings as tests](#14-encoding-review-findings-as-tests)
15. [Rule: `requiresData` is a contract with `self$data` at render time](#15-rule-requiresdata-is-a-contract-with-selfdata-at-render-time)
16. [Rule: never wrap `jmvcore::reject()` in a catch-all `tryCatch`](#16-rule-never-wrap-jmvcorereject-in-a-catch-all-trycatch)
17. [Rule: image state holds drawing data, not models or datasets](#17-rule-image-state-holds-drawing-data-not-models-or-datasets)
18. [Why round 3 still found things: how rules decay](#18-why-round-3-still-found-things-how-rules-decay)
19. [Rule: a bare symbol must be importable from the submodule's own namespace](#19-rule-a-bare-symbol-must-be-importable-from-the-submodules-own-namespace)
20. [Rule: a displayed statistic is computed, never defaulted](#20-rule-a-displayed-statistic-is-computed-never-defaulted)
21. [Rule: column `format:` tokens are comma-separated and exact](#21-rule-column-format-tokens-are-comma-separated-and-exact)
22. [Rule: a user's column name is not a regular expression](#22-rule-a-users-column-name-is-not-a-regular-expression)
23. [Rule: plot colours come from jamovi's palette](#23-rule-plot-colours-come-from-jamovis-palette)
24. [Where the debt actually lives: promotion, not release](#24-where-the-debt-actually-lives-promotion-not-release)
25. [Rule: an unconfigured analysis is not an error](#25-rule-an-unconfigured-analysis-is-not-an-error)
26. [Rule: tabular data belongs in a `Table` result](#26-rule-tabular-data-belongs-in-a-table-result)
27. [Rule: the module's own prose must agree with itself](#27-rule-the-modules-own-prose-must-agree-with-itself)
28. [Rule: seeding the RNG must not outlive the function that did it](#28-rule-seeding-the-rng-must-not-outlive-the-function-that-did-it)
29. [Rule: nothing may be derived from which analyses a module holds today](#29-rule-nothing-may-be-derived-from-which-analyses-a-module-holds-today)
30. [The official process: requirements, submission, updates](#30-the-official-process-requirements-submission-updates)
31. [Learning from external audits](#31-learning-from-external-audits)

---

## 1. The pre-submission checklist

Run these before asking for a library review. Each one maps to a finding that was
actually raised, except item 10 (anticipatory).

```bash
# 1. Render functions that read image$state without a NULL guard        [MEDIUM]
python3 tools/check_state_guards.py

# 2. Opaque light-theme backgrounds in HTML output                      [MEDIUM]
python3 tools/theme_safe_html.py            # dry run; must report 0

# 3. Named HTML entities other than &lt; &gt; &amp; &quot; &apos;       [MEDIUM]
grep -oh "&[a-zA-Z][a-zA-Z0-9]\{1,12\};" R/*.R | sort -u

# 4. setVisible(FALSE) used to signal failure rather than option state   [MEDIUM]
grep -n "setVisible(FALSE)" R/*.b.R      # each hit must be option-driven

# 5. addRow() in .run() against a fixed / option-determined row set      [MEDIUM]
#    the bare grep is 620 hits umbrella-wide and so is never read: most are one
#    extra conditional row on top of an .init() skeleton, which does not blink.
#    release_gate.py check_init_row_structure (item 9) asks the table-level
#    question instead - 1 shipped hit across all five modules on 2026-09-23.
python3 tools/release_gate.py --root ../<Module> | grep "fixed rows built"

# 6. Bare warning() reaching a user-relevant condition                   [MEDIUM]
grep -n "^\s*warning(" R/*.b.R

# 7. Spliced .() fragments                                              [LOW]
grep -nE '\.\("[^"]*"\)\s*,\s*[a-zA-Z_]|paste0\(\s*\.\(' R/*.b.R
grep -nE '\.\(\s*"[[:space:],;:.]' R/*.b.R            # leading space/punctuation inside .()
grep -nE '\.\(\s*"[^"]*[[:space:]]"\s*[,)]' R/*.b.R   # trailing space inside .()
# (the old pattern '\.\(" |\ "\)' matched every `collapse = ", "` - 200 hits on
#  jsurvival, burying the 14 real sites the 2026-09-15 audit then found)

# 8. Undeclared packages and unimportable bare symbols (`%>%`)          [LOW..CRITICAL]
#    --vanilla: ~/.Rprofile attaches magrittr outside the umbrella and hides the break
Rscript --vanilla -e 'testthat::test_file("tests/testthat/test-zzz-dependency-declaration.R")'
Rscript --vanilla tools/submodule_smoke.R ../<Module>   # installed namespace of a submodule (section 19)

# 9. requiresData contract, CollapseBox Title Case, .() padding, refs,
#    clearWith, renderFun, entities, versions, NEWS.md heading,
#    catalog scope, " [..]" and \u{} inside .(), translated sprintf
#    specifiers, notice title colours, sentinel $insert() index,
#    description block scalars, bug-report URL, dataset descriptions  [CRITICAL..LOW]
python3 tools/release_gate.py      # FAIL lines block; read every WARN for shipped analyses
python3 tools/release_gate.py --root ../<Module>   # the same checks on the tree the reviewer reads

# 10. jamovi 28.3 features vs module-wide minApp                        [anticipatory]
grep -n '^minApp' jamovi/0000.yaml          # File option / Text result => must be >= 28.3.0
grep -nE 'type: (File|Text) *$|mode: vector' jamovi/*.a.yaml jamovi/*.r.yaml
#    a `type: Text` table COLUMN is not a Text result (false hit); release_gate.py
#    check_min_app (run in item 9) skips columns, FAILs File/Text under minApp < 28.3.0
#    and WARNs on mode: vector

# 11. An ERROR the user sees before they have configured anything      [MEDIUM]
#     each hit must be INFO (partial selection) or silent (nothing selected)
grep -nE '\.addNotice\(\s*["\x27]ERROR' R/*.b.R      # then trace each guard (section 25)
grep -nB3 'return\(NULL\)' R/*.b.R | grep -i 'ERROR'  # fatal via banner - use jmvcore::reject()

# 12. Everything still compiles
Rscript -e 'Sys.unsetenv("ELECTRON_RUN_AS_NODE"); jmvtools::prepare(".")'
```

Plus the cheap metadata gates the reviewer checks first:

- `Version:` is past `1.0` and **identical** in `DESCRIPTION`, `jamovi/0000.yaml`,
  every `.a.yaml`, and `CITATION.cff`.
- `License: GPL (>= 2)` (OSI-approved), and no AGPL-3 package in the newest `build/R*/`.
  The library rejects AGPL-3, and two modules ship one transitively
  ([section 30](#30-the-official-process-requirements-submission-updates)).
- Every key in `jamovi/00refs.yaml` is cited **and** every cited key resolves,
  with exact casing. Every entry has `title`, `author`, `url`. Check this in the
  **generated submodule**, not only the umbrella: the umbrella keeps every entry on
  purpose, and `_updateModules.R` trims each submodule's copy to the keys it cites.
  A missing `url` is only a WARN in `tools/release_gate.py` — treat it as blocking for
  any key a shipped analysis cites.
- Every `clearWith` entry resolves to a real option name.
- Trace **all dependencies** of each cached result, including seed, reference/event level,
  preprocessing and nested image/Output dependencies. `check_clearwith` validates names only;
  compare an existing A → B analysis with a fresh B analysis to detect stale results (§31).
- Saved outputs preserve original spreadsheet row IDs through filtering, missingness and sorting:
  `setRowNums()` and `setValues()` use the same prediction frame and order (§31).
- Numerical tests assert the displayed value against an independent oracle, including class
  coding, metric definition and training/CV/OOB population; an object-class test is insufficient.
- Every `renderFun:` resolves to a real `function(image, ...)` method.
- No committed build artifacts (`*.tar.gz`, `*.jmo`, anything under `build/` or `dist/`),
  and `.gitignore` covers `build/`, `dist/` and `*.jmo` (an official requirement).
- `compilerMode: tame` on every `.u.yaml`.
- *(anticipatory)* A `type: File` option or `type: Text` result needs `minApp: 28.3.0` in
  `jamovi/0000.yaml`. `minApp` is module-wide, so raising it locks every jamovi 28.2-or-older
  user out of the whole module: a deliberate release decision, never a side effect. Setup,
  gating and testing: [jamovi 28.3 Features](jamovi_module_patterns_guide.md#jamovi-283-features-file-text-vector-images)
  and [Installing Current jmvtools and jmvcore](jamovi_module_patterns_guide.md#installing-current-jmvtools-and-jmvcore).
- `NEWS.md` has a heading the **release workflow** can match, for any version it would actually
  publish. `.github/workflows/release.yaml` extracts the release body with
  `(^|[^0-9.])<version>([^0-9.]|$)` and falls back to the placeholder `Release <v>.` on a miss;
  a four-component version publishes nothing at all, and a three-component one with a multi-digit
  last part is a pre-release. `_updateModules.R` rewrites `Version:` in `DESCRIPTION`,
  `CITATION.cff`, `0000.yaml` and every `.a.yaml` on every regeneration and never touches
  `NEWS.md`. **Write the heading before the version bump** - the workflow skips a version whose
  tag exists, so a release cannot be re-cut (2026-09-16 and 2026-09-22 OncoPath; 14 of the 16
  workflow-era releases shipped with the placeholder). The release *body* can still be corrected
  afterwards with `gh release edit v<version> --notes-file <file>`; only the tag is fixed. Round 7
  (2026-10-23): `check_news` warned, the tag went out anyway, and three of the five October
  releases (OncoPath 1.1.2, meddecide 1.1.2, jsurvival 1.1.1) were published as *Latest* with the
  placeholder. The release workflow runs on push and does not read the gate, so run
  `release_gate.py --root <sibling>` and write the heading **before** the version bump. The **newest** heading must also be three-component (`check_news` WARNs on a
  four-component one; older four-component headings are history and are not judged).
- The module's own test suite is green at the commit you tag, and nothing under
  `tests/testthat/_problems/` or `testthat-problems.rds` is tracked. testthat writes both only
  when tests fail, so a committed copy is proof the release shipped a red suite (2026-10-23
  OncoPath: three failing tests). No personal config either: `.claude/settings.local.json` holds
  a local absolute path, and it was tracked in all five siblings. A `.gitignore` entry does not
  untrack a file; `git rm --cached` does. `check_artifacts` FAILs on all three (since 2026-10-24,
  when all six repositories were cleaned).
- User-visible text: notice titles in sentence case, one spelling convention, one name per
  concept, and no pointer to an analysis that has not shipped
  ([section 12](#12-rule-ui-label-conventions), [section 27](#27-rule-the-modules-own-prose-must-agree-with-itself)).
- Every panel puts variables first and every other group in a **top-level** `CollapseBox`
  ([section 12](#12-rule-ui-label-conventions)).
- Every variable role in that first `VariableSupplier` (greyed with `enable:`, never inside a
  CollapseBox); a level is a `Level`, a fixed set of numbers is `Number` options, and a preset
  shows its values in the controls it overrides ([section 12](#12-rule-ui-label-conventions)).
- Nothing hidden or blanked on the way to `reject()`; one report per error; a failed optional
  sub-analysis is a WARNING notice ([section 5](#5-rule-setvisiblefalse-is-not-an-error-mechanism),
  [section 13](#13-notices-the-native-element-and-the-insert-trap)).
- Data in a `Table`, numbers in `number` columns, headings as `Group` titles, help only in the
  empty state, and `ggtheme` rather than a module theme option (sections 6, 11, 23, 25, 26).
  Every one of these is a `release_gate.py` WARN since round 8 (2026-10-24): read them for the
  shipped analyses before the version bump.
- No `library-audit` tag in `README.md`, `DESCRIPTION` or `NEWS.md`, and no test that requires one
  ([section 14](#14-encoding-review-findings-as-tests)).
- `description: main:` is one `>` folded paragraph with no blank line, in every `.a.yaml`
  ([section 27](#27-rule-the-modules-own-prose-must-agree-with-itself)).
- One bug-report URL across `DESCRIPTION`, `jamovi/0000.yaml`, `README.md` and `CITATION.cff`, and
  no two example datasets sharing a `description:`. Every `datasets:` entry has `name`, `path` and
  `description`, `name` is unique, and the file is a small, PII-free `.omv` or `.csv`
  ([section 30](#30-the-official-process-requirements-submission-updates)).
- No `$insert()` at a sentinel index ([section 13](#13-notices-the-native-element-and-the-insert-trap)).
- `jamovi/i18n` holds only this module's strings: the updater copies the umbrella catalog and
  `jmvtools::i18nUpdate()` trims it at build ([section 9](#9-rule-translatable-strings-are-whole-sentences)).

---

## 2. Findings that recur across every module

Sorted by how many of the five audits raised them. **These are house-style
problems, not one-off bugs** — when one function does it, the whole module does.

| # | Finding | Modules hit | Severity |
|---|---|---|---|
| 1 | Render function reads `image$state` with no NULL guard | 3 of 5 | MEDIUM |
| 2 | `.run()` methods far past ~120 lines | 5 of 5 | LOW |
| 3 | Fixed-structure tables built with `addRow()` in `.run()` | 4 of 5; meddecide in **all three** of its rounds | MEDIUM→LOW |
| 4 | Spliced `.()` translation fragments | 5 of 5 | LOW |
| 5 | Named HTML entities that will render literally | 3 of 5 | MEDIUM |
| 6 | `setVisible(FALSE)` used to signal a failure | 3 of 5 | MEDIUM–HIGH |
| 7 | Packages used but not declared in `Imports:` | 2 of 5 | LOW–MEDIUM |
| 8 | HTML that ignores the dark theme (opaque pastels; fixed title hues on a tint) | 2 of 5 | MEDIUM–INFO |
| 9 | Bare `warning()` the user never sees | 1 of 5 | MEDIUM |
| 10 | Dead code referencing commented-out schema | 2 of 5 | MEDIUM |
| 11 | `requiresData: true` on renderers that draw from state only | 5 of 5 (rounds 3–4) | LOW |
| 12 | Translation catalogs inherited from the umbrella | 3 of 5 (round 4) | MEDIUM–LOW |
| 13 | `NEWS.md` behind the `DESCRIPTION` version | 3 of 5 (rounds 4–7); round 7: 3 of the 5 October releases published the placeholder | LOW–INFO |
| 14 | Tabular data drawn as hand-built HTML instead of a `Table` | 5 of 5 (round 5 sweep) | MEDIUM |
| 15 | Red ERROR on an analysis the user has not configured yet; fatal condition via a banner instead of `jmvcore::reject()` | 4 of 5 (rounds 1, 2, 5) | MEDIUM |
| 16 | `description: main:` keeps newlines, so the library listing breaks or truncates | 4 of 5 (round 5 sweep) | LOW |
| 17 | The module's own prose disagrees with itself (capability claims, bug-report URL) | 4 of 6 trees (round 5) | LOW |
| 18 | Bare `set.seed()` leaking a fixed RNG stream into the shared engine process | 1 of 5 (round 4) | LOW |
| 19 | Config or a test derived from which analyses a module holds today (`prune_imports`, hard-coded analysis lists) | 4 of 5 (round 6) | LOW |
| 20 | Committed local scratch: failing-test output, personal editor config | 5 of 5 (`.claude/settings.local.json`, round 7 sweep) | LOW |
| 21 | All-caps notice titles; British and American spelling mixed in one module | caps 3 of 5, spelling 5 of 5 (round 7 sweep) | LOW |
| 22 | Variable boxes buried in CollapseBoxes or placed after other controls | 5 of 5 (round 8 sweep: MD 4, JS 8, OP 2, JJ 2, CPD 1 panels) | MEDIUM |
| 23 | A level or a fixed set of numbers typed into a `String` | MD level 2, number lists 8; JS 10, JJ 1 number lists (round 8) | MEDIUM |
| 24 | Results hidden or blanked on a failure path (`on.exit` hide, blank before `reject()`) | MD 6 (round 8; §5 class, first raised round 2; the OP and JJ hits were detector false positives, fixed) | MEDIUM |
| 25 | Empty `Preformatted` used as a heading | MD 15, JS 40, CPD 2 (round 8; round 2 named two) | LOW |
| 26 | A failed optional sub-analysis reported as a table footnote | MD 102, JS 22, OP 7 (round 8) | LOW |

**The lesson:** when a review names one instance, grep for the class and fix all
of it. The reviewer explicitly rewards this — "several of them by fixing the
class of problem rather than the instance."

External lessons are recorded separately in the [jYS classification table](jamovi_external_audit_learning_guide.md#8-finding-classification-and-check-coverage).
Its 35 findings are not additional hits in the five-module counts above.

---

## 3. Rule: render functions must NULL-guard `image$state`

### Why

A render function can run when `.run()` did **not** set the state. Three real paths:

1. **`.run()` returned early.** Validation failed, so `setState()` was never
   reached — but the plot element still exists and still renders.
2. **Resize / redraw.** jamovi re-invokes the renderer without re-running.
3. **Reopening a saved `.omv`.** `jmvcore`'s `.load()` restores an analysis from
   disk without re-running it.

The concrete case the reviewer walked through for `venn`: the user has `var1` and
`var2` configured and a working diagram on screen, then selects `var3`. That
clears the cached state (it's in the top-level `clearWith`), and validation now
fails because `var3true` hasn't been chosen — so `.run()` returns before any
`setState()`. The renderer's own guards (`is.null(self$options$var1)`,
`nrow(self$data) == 0`) both pass. `image$state` is `NULL`, `results$mydata`
silently yields `NULL`, and a raw ggplot/library error is shown instead of the
clean validation message the analysis already built.

### The house pattern

```r
.plotSomething = function(image, ggtheme, theme, ...) {
    private$.checkpoint()

    plotData <- image$state
    if (is.null(plotData))
        return(FALSE)

    ...
    print(plot)
    TRUE
}
```

`return(FALSE)` — not bare `return()` — is the jamovi convention for "nothing was
drawn". Reference implementations in this repo: `R/agepyramid.b.R`,
`R/benford.b.R`, `R/dataquality.b.R`, `R/outlierdetection.b.R`.

### Sub-field reads

When the renderer reads a sub-field, guard the **parent** before the read:

```r
# WRONG - image$state may be NULL
predicted <- image$state$predicted

# RIGHT
if (is.null(image$state))
    return(FALSE)
predicted <- image$state$predicted
```

### Not enough on its own

A `NULL` state is not the same as an *empty* state. If a downstream builder
returns a bare `data.frame()` with **no columns**, `ggplot2::aes(x = metric)`
still fails because the mapping can't resolve. Guard both:

```r
df <- private$.buildBarPlotData(plotData)
if (is.null(df) || nrow(df) == 0)
    return(FALSE)
```

(That was the `decisioncompare` finding: `.buildBarPlotData()` correctly returned
an empty frame, and the renderer crashed on it anyway.)

### A `private$` field as a *fallback* is fine; as the *only* gate it is not

`.run()` fills `private$` fields; the export path never calls `.run()`. So the
question is never "does this renderer touch a `private$` field" but "can it still
answer when that field is `NULL`".

Safe — state is consulted first, and the private field only refines the answer
(`R/survival.b.R:582`):

```r
isTRUE(state$has_competing) ||
    isTRUE(private$.eventRecode$has_competing) ||
    (isTRUE(self$options$multievent) && identical(self$options$analysistype, "compete"))
```

Broken — the private field is the sole gate, so on export it holds its initial
value and the renderer returns before drawing anything (`R/jjdotchart.b.R:459`,
a HIGH finding):

```r
if (!isTRUE(private$.inputsValid)) return()
```

Also broken, and harder to see: a helper that *could* read state but is called
without it. `multisurvival`'s `.isCompetingRisk(state = NULL)` has the safe shape
above, but `.plot_adj` reaches it through `.adjustedEstimandNote()`, which calls
`private$.isCompetingRisk()` with no argument — so on export the guard silently
degrades to the options-only test the comment right above it warns against. Trace
the *call*, not just the definition.

`check_render_private_state` in `tools/release_gate.py` knows the first pattern
and FAILs on the other two. Where the safety is real but indirect, mark it
`# render-state: <field>` rather than reshaping the code to satisfy a regex.

---

## 4. Rule: HTML output must be theme-safe

### Why

jamovi has a dark theme. Every `Html` result in this module was styled with a
fixed light-theme hex background, and the large majority set **no text colour at
all**. In light mode the inherited text colour is dark and it reads fine. In dark
mode the inherited text colour is light, and it lands on a pale pastel
(`#f8d7da`, `#fff3cd`, `#e3f2fd`, `#f8f9fa`) — low-contrast to genuinely
unreadable. Those blocks carry the error messages, warnings and clinical
interpretations, so the dark-theme user loses exactly the output that matters
most when something has gone wrong.

Blocks that *did* set both a background and a foreground stayed legible, but then
read as a light-theme island pasted into a dark results pane. Either way the
panel ignores the user's chosen theme.

### The rule

> **Never set a `background-color` without also controlling the foreground, and
> prefer a translucent tint over an opaque fill.**

A translucent background *tints* whatever is behind it instead of replacing it,
so one declaration is correct in both themes.

### The transform (and why light theme doesn't change)

Compositing an `rgba` fill over a background is:

```
result = (1 - a) * bg + a * tint
```

Given the original pastel `P` (which was designed against a white pane), solve
for the tint that reproduces it exactly over white:

```
T = (P - (1 - a) * 255) / a
```

`T` stays in `[0, 255]` as long as `a >= max_channel((255 - P) / 255)`. So pick
that minimum alpha with a little headroom and the light theme is **pixel-identical**,
while the same declaration becomes a proper hue-preserving tint over a dark pane.

```
#f8d7da  ->  rgba(216, 33, 50, 0.18)     composites to #f8d7da over white
#fff3cd  ->  rgba(255, 202, 33, 0.23)    composites to #fff3cc over white
#e3f2fd  ->  rgba(33, 152, 239, 0.13)    composites to #e2f2fd over white
#f8f9fa  ->  rgba(138, 155, 172, 0.06)   composites to #f8f9fa over white
```

`tools/theme_safe_html.py` in this repo implements exactly this and is safe to
re-run — it is idempotent, because an `rgba()` value no longer matches the hex
pattern it looks for.

### Writing new panels

```r
# WRONG - opaque pastel, no foreground. Unreadable in dark mode.
"<div style='background-color: #f8d7da; border-left: 4px solid #dc3545;
             padding: 15px;'>"

# RIGHT - translucent tint, foreground follows the pane, saturated accent on
# the border (a saturated border reads correctly in both themes).
"<div style='background-color: rgba(216, 33, 50, 0.18);
             border-left: 4px solid #dc3545;
             padding: 15px; color: inherit;'>"
```

Rules of thumb:

- **Panel tints** (pale fills, HSL lightness above ~0.80): translucent + `color: inherit`.
- **Badges / chips** (saturated opaque fills like `#dc3545`, `#007bff`): keep the
  fill, but *always* set an explicit `color:` — white on a dark chip, near-black
  on a light one. These are deliberate and self-contained.
- **Borders and accents**: leave saturated hexes alone. They read in both themes.
- **Explicit dark text** (`color: #721c24`, `#856404`, `#155724`) inside a panel
  you made translucent: change to `color: inherit`. The semantic colour is
  already carried by the border accent.
- **Saturated text on a tint is not safe either.** Round 4 (2026-09-16 OncoPath [INFO])
  found notice titles coloured by severity (`#dc2626`, `#ea580c`, `#ca8a04`, `#2563eb`)
  above a translucent tint, with a comment claiming they were "saturated enough to read on
  both" themes. Measured (WCAG, against a dark pane `#2b2b2b`): ERROR 2.93:1 and INFO
  2.74:1 — below the 3:1 floor even for large bold text; WARNING was 2.94:1 on white. A
  hue that passes on one background fails on the other. Titles take `color: inherit`; the
  border and the tint carry the severity. Measure a colour claim before writing it in a comment.

```r
# WRONG - a fixed title hue on a translucent tint
"<strong style='color: ", style$color, ";'>", title, "</strong>"
# RIGHT - the title follows the pane; severity lives in the border and tint
"<strong style='color: inherit;'>", title, "</strong>"
```

### Enforce it

`python3 tools/theme_safe_html.py` (opaque pastels; must report 0) and
`python3 tools/release_gate.py` `check_notice_title_colour` (fixed title hues in a
`.renderNotices()`; WARN — 9 shipped renderers in jjstatsplot, meddecide and jsurvival
at round 4). `theme_safe_html.py` cannot see coloured text on a translucent tint.

### What you cannot do

You cannot fix this by declaring a `Notice` in `.r.yaml` — see
[section 13](#13-notices-the-native-element-and-the-insert-trap).

*(anticipatory)* What you can do from jamovi 28.3: move plain narrative with no severity to a
`type: Text` element (module-wide `minApp: 28.3.0`, see [Version Gating: minApp](jamovi_module_patterns_guide.md#version-gating-minapp)).
The Text sanitizer strips style attributes and every tag outside its whitelist, so a module
cannot add CSS to it ([renderer table](jamovi_notices_guide.md#which-text-renderer-notice-setnote-html-or-text)).

---

## 5. Rule: `setVisible(FALSE)` is not an error mechanism

### Why

jamovi already has a presentation for a failed analysis: it greys the results
pane and shows an analysis-level error. **That presentation depends on the
results staying in place.** Removing an element instead makes the pane collapse
and re-expand as the user types through invalid intermediate states, which reads
as the interface glitching rather than as a diagnosable problem.

`setVisible()` is for **option-driven** visibility — showing a table because a
checkbox is ticked. Nothing else.

### The three real failures the reviewer found

**HIGH — `psychopdaroc`: four options silently did nothing.** `.init()` called
`setVisible(FALSE)` on `criterionPlot`, `prevalencePlot`, `dotPlot` and
`precisionRecallPlot`, overriding the `visible: (showCriterionPlot)` binding
already declared in `.r.yaml`. `.run()` restored *some* elements but never those
four. The plots were computed and thrown away. Ticking the checkbox did nothing —
no plot, no error, no explanation.

> The imperative pair (`setVisible(FALSE)` in `.init()` + `setVisible(TRUE)` in
> `.run()`) can drift out of sync. The declarative `visible:` expression cannot.
> Delete the imperative pair and let `.r.yaml` express it.

**MEDIUM — `agreement`: a note written to an element that was just hidden.**

```r
# WRONG - the note is never rendered
self$results$blandAltmanStats$setVisible(FALSE)
self$results$blandAltmanStats$setNote("error", "Requires exactly 2 raters.")
```

A note on a hidden table is not rendered. The user got a silent disappearance and
never saw the (genuinely helpful) explanation.

**MEDIUM — `chisqposttest`: the post-hoc table hidden on failure.** Deleting the
`setVisible(FALSE)` and letting the empty table stand with its explanatory
message beside it is the fix.

### The decision table

| Situation | Correct mechanism |
|---|---|
| Element depends on an option | `visible: (optionName)` in `.r.yaml` — declarative |
| Element depends on several options | `visible: (a \|\| b)` in `.r.yaml` |
| Fatal, user must change something | `jmvcore::reject(.("..."), code = "...")` |
| Non-fatal warning, rest of output still valid | An always-visible `Html` notice element |
| Explanatory / narrative text, no severity *(anticipatory)* | A `type: Text` element (jamovi 28.3+, module-wide `minApp: 28.3.0`); otherwise the `Html` element |
| Deliberate methodological guard (e.g. no post-hoc when omnibus n.s.) | Hide it **and** explain why — this one is fine |
| Onboarding / welcome panel before variables are chosen | `setVisible()` is fine — this is option state |

### Round 8: hiding, and blanking, on the way to `reject()` (2026-10-23 meddecide [MEDIUM])

The reviewer found three shapes the earlier rounds had not named:

- **Hidden until filled.** `decisioncombine` hid three tables at the top of every run and
  turned them back on only when populated, so after any validation failure they vanished.
- **An `on.exit()` visibility rule.** `decisioncalculator`'s `.run()` hid every narrative panel
  still empty on exit, which is exactly the state a validation return leaves.
  `enhancedROC` and `psychopdaROC` had the same `on.exit()`; the reviewer did not name them.
- **Blanking before `reject()`.** The three `kappaSize` analyses wrote `""` / `"<div></div>"` to
  their panels and then rejected.

What jmvcore actually restores between runs (read from jmvcore 2.7.38, jamovi build, 2026-10-24 —
`ResultsElement`, `Html`, `Preformatted` and `Table` `$fromProtoBuf`):

| Carried from the previous run | Restored? |
|---|---|
| visibility set with `setVisible()` | **never** — `fromProtoBuf` does not read it |
| `image$state` | yes, unless a `clearWith` option changed |
| `Preformatted` content | **always**, even when a `clearWith` option changed (only marked stale) |
| `Html` content | yes (marked stale when a `clearWith` option changed) |
| `Table` cells | only for rows that exist after `.init()`, and only when no `clearWith` option changed |

So the reviewer is right that nothing needs a `setVisible()` to *undo* a previous run, and
**wrong** that "nothing an earlier run set with `setContent()` carries into the next one":
`Html`/`Preformatted` content does, by design. Both conclusions still hold:

- Delete the error-driven `setVisible()` and the `on.exit()` rule; let `visible:` decide.
- Clearing a panel at the top of `.run()` is still right **when the run can end normally
  without rewriting it** (an early return for an emptied variable box). Clearing it **right
  before `reject()`** is not: the stale, greyed content under the error *is* jamovi's
  presentation of a failed analysis.

**Enforce it:** `release_gate.py` `check_error_presentation` (WARN) — an `on.exit()` that calls
`setVisible` inside its own argument, and a method that blanks a panel (not an onboarding
`todo`/`welcome`/`instructions` one) and then reaches `reject()` with no `return(` in between.
Both scopes were narrowed after the first sweep: a 400-character window flagged
`ihcheterogeneity`'s `on.exit(private$.renderNotices())` beside an unrelated onboarding
`setVisible`, and `raincloud`'s clearing of its `todo` panel read as a blank. Reconciled on the
audited meddecide build: 6 hits = the reviewer's
`decisioncalculator` and three `kappaSize` sites plus the two `on.exit()`s they missed. It is
**blind to hide-until-filled** (`decisioncombine`): a `setVisible(FALSE)` that a populate method
undoes looks like an onboarding panel to any static rule, so checklist item 4 still needs a
reader for each `setVisible(FALSE)` (21 in meddecide).

---

## 6. Rule: row structure that isn't result-dependent belongs in `.init()`

### Why

jamovi builds results in two phases, and **where you declare a table's structure
decides what the user sees while the analysis runs**. A table declared `rows: 0`
and filled with `addRow()` from `.run()` first appears empty, then restructures
once computation finishes — a visible jump on every single run. Declaring the
structure up front means the table appears complete and only the values fill in.

### The test

> Does the **row set** depend on a computed result, or only on options and data shape?

| Depends on | Where the rows belong |
|---|---|
| Nothing — always the same rows | `.init()` (or `rows: N` in `.r.yaml`) |
| Option values (one row per selected variable) | `.init()` |
| Data shape (numeric vs categorical variable) | `.init()` |
| A computed result (discovered factor-level pairs, computed bins, RECIST categories actually present) | `.run()` — correct as is |

### The pattern

```r
.init = function() {
    table <- self$results$missingVals
    table$addRow(rowKey = "total_obs",      values = list(metric = .("Total observations")))
    table$addRow(rowKey = "missing_vals",   values = list(metric = .("Missing values")))
    table$addRow(rowKey = "complete_cases", values = list(metric = .("Complete cases")))
    table$addRow(rowKey = "unique_vals",    values = list(metric = .("Unique values")))
},

.run = function() {
    ...
    table <- self$results$missingVals
    table$setRow(rowKey = "total_obs",    values = list(value = n_total))
    table$setRow(rowKey = "missing_vals", values = list(value = n_missing))
    ...
}
```

**`setRow()` on a rowKey that does not exist throws.** Every key you `setRow()`
in `.run()` must be created in `.init()` on every path that reaches it. If a row
is genuinely conditional, create it unconditionally in `.init()` and leave it
blank.

### Bonus

A `deleteRows()` call at the top of a population method is a tell — it exists to
stop rows accumulating across runs, and it becomes unnecessary once the rows are
created once in `.init()`. jamovi rebuilds the results skeleton from the schema
on every run anyway.

### Enforce it

`tools/release_gate.py` → `check_init_row_structure` (WARN). The unit it checks is the
**table**, not the `addRow()` call. It flags a table only when all three hold:

- the `.r.yaml` declares no `rows:`,
- nothing reachable from `.init()` (following `private$.helper()` calls transitively) names it,
- **every** `addRow()` it receives uses a literal string `rowKey`.

The third condition is the proof: a literal key means the row set was known when the code was
written. A table that has an `.init()` skeleton and gains one extra *conditional* row in `.run()`
— `waterfall`'s `recist_Unknown`, `checkdata`'s `rare_categories` — does not blink and is not
flagged; a call-level detector reported 8 shipped hits of which 6 were exactly that shape.

**Known blind spot:** a loop index over a fixed-length result
(`for (i in 1:nrow(res)) tbl$addRow(rowKey = i, ...)`) is not a literal key, so it is invisible
here. That is the shape the 2026-09-16 meddecide report named in `decision.b.R` and
`decisioncalculator.b.R` — always-four `epiR` rows written under a loop index. **A 0 from this
check is not proof**; when you touch a table built in `.run()`, ask the question in "The test"
above yourself.

**Round 8 (2026-10-23 meddecide [MEDIUM]), the fourth round running for this module:** 8 tables
whose row set follows from the options alone — one row per selected test (`comparisonTable`),
per pair (`mcnemarTable`, `diffTable`), per pattern (`combinationTable*`), per target prevalence
(`screeningWorkload`). The check reported **0**: every key is computed (`rowKey = row_key`,
`as.character(i)`). A static rule cannot tell an options-derived key from a data-derived one, so
the property needs the runtime test — call `.init()` alone with the analysis's options, record
the row keys, run, and assert the keys did not change. `tests/testthat/test-zzz-init-row-structure-runtime.R`
does this for named tables; add each table you fix to it. Parse an option string such as
`prevalenceScenarios` in `.init()` too, and report a parse error from `.run()`.

This is also the likelier source of the "blink" the same report attributes to missing `clearWith`
(*inferred* from `Table$fromProtoBuf`, not observed in the client): cells are restored only for
rows that exist after `.init()`, so a `rows: 0` table refilled by `addRow()` in `.run()` starts
empty on **every** re-run, display toggle or not.

### Related: don't push numbers through a text column

Two summary tables declared `value` as `type: text` and pushed numbers through
`as.character()`. That gives up jamovi's own number formatting and decimal-place
handling, and text columns don't right-align, so a column of numbers reads
ragged. Split numeric statistics into a `type: number` column.

Round 8 (2026-10-23 meddecide [LOW]) found it again in `agreement`'s sample-size table
(`value = base::format(k0, digits = 3)`). A key/value table mixing a text row (the metric) with
numeric rows needs two columns or two tables; a single `number` column cannot hold the label.
**Enforce it:** `check_text_number_cells` (WARN) — `format()`/`formatC()`/`sprintf()`/`prettyNum()`
into a `value =` cell; the reviewer's 4 sites exactly, 0 in the other four modules.

---

## 7. Rule: no named HTML entities except the structural five

### Why

Only five named entities are **structural** — they stand for characters that have
special meaning in HTML and *must* be escaped:

```
&lt;   &gt;   &amp;   &quot;   &apos;
```

Everything else (`&nbsp;`, `&mdash;`, `&ndash;`, `&rarr;`, `&minus;`, `&alpha;`,
`&kappa;`, `&times;`, `&beta;`, `&ge;`, `&plusmn;`, `&eacute;` …) works only
because jamovi's Html renderer *currently happens to* expand arbitrary named
entities. That behaviour is incidental. **A documented upcoming jamovi fix
corrects that rendering path, after which they display literally** — your
methodology note starts reading `Cohen's &kappa;`.

They also already fail non-HTML export today: copy the panel into Word or export
to PDF and the raw entity text comes through.

### The fix

Use the real character — but written as a `\uXXXX` escape (exactly four hex digits,
**no braces**), because `R CMD check` flags literal non-ASCII bytes in R source.

**Why no braces:** R reads `\u{2265}` and `\u2265` identically, but the jamovi catalog
extractor (`jmvtools::i18nUpdate()`) reads the *source text* and decodes only `\uXXXX`.
Inside `.()` a braced escape reaches the catalog as the literal msgid `\u{2265}…`, which never
matches the runtime string — the sentence stays English in every language even when `tr.po`
"translates" it (round 4: 110 shipped sites across four modules). This table used to show the
braced form. `R` reads at most four hex digits after `\u`, so `\u22655` is `≥5` — no ambiguity.

| Entity | Char | Escape |
|---|---|---|
| `&minus;` | − | `\u2212` |
| `&mdash;` | — | `\u2014` |
| `&ndash;` | – | `\u2013` |
| `&rarr;` | → | `\u2192` |
| `&times;` | × | `\u00D7` |
| `&plusmn;` | ± | `\u00B1` |
| `&alpha;` | α | `\u03B1` |
| `&beta;` | β | `\u03B2` |
| `&kappa;` | κ | `\u03BA` |
| `&ge;` | ≥ | `\u2265` |
| `&eacute;` | é | `\u00E9` |
| `&nbsp;` | (nbsp) | `\u00A0` |

Placeholder tokens such as `[[APPROX]]` replaced by a helper after translation are not an
alternative inside `.()`: a space before `[` truncates the string ([section 9](#9-rule-translatable-strings-are-whole-sentences)).

For `&nbsp;` used purely as a table-cell spacer, the simplest fix is to drop it:
`<td></td>` renders the same in HTML and exports cleanly.

**Caveat for very large HTML literals.** In a string literal longer than ~10,000
characters, `\u` escapes can hit a parse trap in a non-UTF-8 locale. In those
specific cases use HTML *numeric* entities (`&#x2192;`) instead — numeric
entities are part of the HTML spec and are not affected by the named-entity
change. See `reference_nonascii_conversion_pitfalls`.

### What is fine

- `gsub("&nbsp;", " ", x)` — code that *strips* entities is correct as it stands.
- `htmltools::htmlEscape()` output — it produces only the structural five.

*(anticipatory)* A `type: Text` element (jamovi 28.3+) decodes only `&lt; &gt; &amp; &quot; &#39;`:
named **and** numeric entities (`&mdash;`, `&#8212;`) show literally, so the numeric-entity caveat
does not apply there. `&apos;`, safe in Html, is not decoded either; use `&#39;` or a plain apostrophe. Use `\uXXXX`. Text escapes differently too: markdown-escape user text
([`.mdEscape`](jamovi_b_R_guide.md#text-content-population-jamovi-283)), not `htmlEscape()`.

---

## 8. Rule: `warning()` is invisible to jamovi users

### Why

**jamovi does not surface R condition warnings in the results pane.** Every
`warning()` in an analysis is written to a console the user is not looking at.

The reviewer found 24 of them across six analyses in `jjstatsplot`, and several
described a plot that *silently differs from what was requested*: falling back to
plain geoms when `ggrain` errored, disabling covariate mapping when the covariate
had NAs, substituting `'l'` for an invalid `rain.side`, falling back to default
colours when palette generation failed. The user ticks an option, gets a plot
that doesn't reflect it, and there is nothing on screen explaining why.

> That is the failure mode that's hardest to diagnose from a bug report.

### The rule

> **Any condition that changes what the user sees must be reported in the
> results pane.** `warning()` is for genuinely internal diagnostics only, and
> that list should be short and deliberate.

Route user-relevant conditions through the module's notice machinery —
`private$.addNotice()` / `.addAnalysisNote()` feeding an always-visible `Html`
element.

### The awkward case: conditions detected inside `.plot()`

You cannot populate a results element from the render phase. Detect the same
condition in `.run()` — the covariate-NA check and an option-validity check are
both cheap and data-only — and record the note there.

---

## 9. Rule: translatable strings are whole sentences

### Why

A `.()` call wrapping a *fragment* cannot be translated correctly even in
principle. The translator opening the catalog sees:

```
"Adjusted Survival Curves for "          <- trailing space, load-bearing
" label(s) provided for "                <- starts and ends mid-clause
", using defaults"                       <- starts with punctuation
```

They can't tell the padding space is deliberate, and they can't reorder the
pieces — word order around an interpolated value differs between languages, so a
sentence assembled label-then-fragment-then-label often has no correct
translation at all.

Hard-wrapping is the same problem: a paragraph split across seven `.()` calls,
each carrying its own `\n`, bakes the *English* line breaks into the catalog and
hands the translator four disconnected fragments.

### The pattern

```r
# WRONG - fragments spliced with paste0/sprintf, padding inside .()
paste0(.("Treatment arm labels were ignored: "), length(arm_labels),
       .(" label(s) provided for "), length(x_levels), .(" group(s)."))

# WRONG - one paragraph hard-wrapped across many .() calls
.("  rules of thumb, not validated reference ranges, so they may not suit\n"),
.("  paediatric, ICU, oncology or athlete populations. Which checks run is\n"),

# RIGHT - one complete sentence, {} placeholders, wrapping applied in R
jmvcore::format(
    .("Treatment arm labels were ignored: {n_labels} label(s) provided for {n_groups} group(s). Provide one comma-separated label per group."),
    n_labels = length(arm_labels), n_groups = length(x_levels))
```

Rules:

- One `.()` = one complete, self-contained sentence or paragraph.
- Interpolate with `jmvcore::format()` and `{}` placeholders — never `paste0()`,
  `sprintf()` or `glue::glue()` across a `.()` boundary.
- No leading or trailing space inside a `.()` string; put separators in the
  surrounding template.
- No `\n` and no indentation inside a `.()` string; apply layout in R afterwards.

### Also: wrap your `reject()` messages

56 of 77 `jmvcore::reject()` calls in `jsurvival` passed a bare string. Those are
precisely the strings a struggling user reads — the "you selected the wrong
thing" messages — and they stay English on a translated install.

```r
jmvcore::reject(
    jmvcore::format(.("Unsupported date format: {format}"), format = format),
    code = "bad_date_format")
```

**Not in file-level helpers.** `.()` is `self <- eval.parent(str2lang("self"))` —
it only works where `self` is in scope. The 2026-09-15 report suggested wrapping the
two `reject()` calls in `.eventIndicator()`, a top-level helper in
`R/multisurvival.b.R`, with `.()`; doing that literally brings back GitHub issue #122
("object 'self' not found", data-dependent). Either give the helper a `self`
parameter passed from the method, or return a sentinel and call `reject()` from the
R6 method. See `vignettes/jamovi_i18n_guide.md` → "`.()` needs `self` in the caller
frame".

### Coverage should be even

Two thoroughly translated analyses and two untranslated ones in the same menu
"reads as broken rather than as partial." If you internationalise, do the whole
module.

### Round 4: wrapping a string in `.()` is not the end (2026-09-16 OncoPath)

The August coverage work took `diagnosticmeta` from 1 to 214 `.()` calls. It also created
four defects that only show outside English:

1. **Band words spliced into a sentence.** `.("strong")` substituted into
   `.("… provides %s evidence …")`. In Turkish a spliced phrase doubled a postposition
   ("…ölçümleri **ile ile** …"). Bake each band into its own whole sentence and pick with
   `switch()`; don't splice nouns either — Turkish marks case on the noun itself.
2. **A translated word compared with English.** `plr_class <- .("not estimable")` then
   `if (plr_class != "not estimable")` — always TRUE once translated, so Turkish printed
   "LR+ = Inf … tahmin edilemiyor kanıt" and LR− = NaN stopped the summary with
   *missing value where TRUE/FALSE needed*. Branch on untranslated keys or on the numbers;
   `.()` only at the point of display.
3. **`" ["` inside `.()`.** jmvcore's `Translator` splits `"(.*) \\[(.*)\\]"` as a context
   marker. With no catalog entry — any language without a catalog, or the umbrella during
   development — everything from the space-bracket on disappears: the LR notes ended at
   "(specificity". Write `≈` as `\u2248`, a CI as "95% CI %s to %s". A trailing
   ` [ctx]` also becomes msgctxt at extraction (see `jamovi_i18n_guide.md`).
4. **A translation that breaks `sprintf()`.** Turkish puts `%` before the number; the
   translator turned `100%%` into `%%%100` and `50%%` into `%%%50`, leaving `%100` / `%50'`,
   which `sprintf()` rejects with *unrecognised format specification* — in Turkish only
   (`diagnosticmeta`, and jsurvival `singlearm`). A translation must keep every conversion
   of its msgid. Templates formatted twice keep `%%%%` in both.

Plus two catalog rules from the same round:

- **Ship only the module's own strings.** Copying the umbrella's catalog put 31,690 msgids
  (7.3 MB of runtime json) into a module that uses 1,499. `_updateModules.R` `build_module()`
  now runs `jmvtools::i18nUpdate()`, which keeps a translation for every string still used
  and deletes the rest.
- **Braced escapes never translate** — see [section 7](#7-rule-no-named-html-entities-except-the-structural-five).

**Test it without a translator.** A pseudo-catalog that wraps every `.()` literal as
`«…»` makes all four visible in English: nesting `«…«…»…»` is a splice, a space next to a
mark is padding, and leaked `Inf`/`NaN` is a translated-word comparison
(`tests/testthat/test-oncopath-library-audit.R` → "report sentences translate as whole
sentences"). `jmvcore::Options$new()$translate(s)` has no catalog, so
`identical(translate(s), s)` catches the `" ["` truncation for every literal.

### Enforce it

`python3 tools/release_gate.py` (add `--root ../<Module>` for a submodule):

| Check | Level | Shipped count at round 4 |
|---|---|---|
| `check_i18n_padding` — separator inside `.()` | WARN | OncoPath 0; CPD 2, JJ 3, MD 6 |
| `check_i18n_bracket` — `" [..]"` inside `.()` | **FAIL** | 0 everywhere |
| `check_i18n_braced_escape` — `\u{…}` inside `.()` | WARN | OP 21, CPD 32, MD 51, JS 6 |
| `check_i18n_po_formats` — translation drops a conversion | WARN | umbrella 0; CPD 2, MD 2, JS 1 until regenerated |
| `check_i18n_catalog_scope` — catalog msgids unused by the module | WARN | OP 0; CPD 30,405, MD 28,736, JS 17 until regenerated |

Splices and translated-word comparisons have no static detector (a lowercase `.()` word is
usually a legitimate label: 46 hits in OncoPath, 19 of them fine); the pseudo-translation test
is the check.

---

## 10. Rule: every package used must be declared — including base packages

### Why

`grDevices`, `grid`, `stats` and `utils` ship with every R installation and
cannot go missing, so there is no runtime fragility. But `R CMD check` treats an
undeclared `::` call or `importFrom()` as a **declaration error**, not a style
nit:

```
WARNING: '::' or ':::' import not declared from: 'grDevices'
WARNING: Namespace dependencies not required: 'stats' 'utils'
```

That's a check failure standing between you and a clean build, and it's easy to
miss because the module runs perfectly well in jamovi regardless.

### The rule

> If a package is reached via `pkg::fn()` **or** appears in an `importFrom()` in
> `NAMESPACE`, it must be in `Imports:` — base-priority packages included.

### Why the dependency guard missed it

`tests/testthat/test-zzz-dependency-declaration.R` excluded base-priority
packages wholesale. Narrow the exclusion to `base` itself and the guard covers
`grDevices`/`grid`/`stats`/`utils` too.

### Related declaration hygiene

- **Runtime deps belong in `Imports`, never `Suggests`.** jamovi installs
  `Imports` on first run and cannot install a missing package on demand — a
  `requireNamespace()`-guarded runtime dependency in `Suggests` is a broken
  analysis for the user. Accept the CRAN "unused Imports" NOTE.
- **Declared-but-unused packages are a finding too.** `cluster` and `tidyr` in
  OncoPath were held alive only by an `@importFrom` roxygen tag with no call site
  — two packages every user installs for nothing.
- **A `Remotes:` entry must be pinned to a full commit SHA**, and must be removed
  when the dependency is vendored or dropped.
- `Imports` *declares* the dependency; `Remotes` says *where to find it*. List
  every real runtime dependency in `Imports` even when it also appears in
  `Remotes`.
- **A package in `Imports:` is still not in scope.** `Imports:` controls
  installation; only `importFrom()`/`import()` puts a name within reach of the
  code. For bare symbols such as `%>%` the difference is the whole bug — see
  §19.

---

## 11. Rule: dead code must not reference a schema that no longer exists

### Why

Commenting a feature out in `.a.yaml` / `.r.yaml` while leaving its
implementation in the shipping `.b.R` creates a landmine. The bodies reference
option and result names that now resolve to `NULL`, so the day someone
re-enables a single `renderFun:` or call site they get:

```
argument is of length zero          <- if (self$options$use_tree)
attempt to apply non-function       <- self$results$tree_summary$setContent(...)
```

...including inside the `tryCatch` error handler, which then swallows the real
cause and fails on its own. In `multisurvival` this was ~1,470 lines referencing
46 commented-out options and 11 commented-out results.

The same class in the JavaScript: `waterfall.events.js` still called
`ui.clinicalPreset.value()` after `clinicalPreset` had been removed from both the
`.a.yaml` and the `.b.R`. `ui.clinicalPreset` was `undefined`, so **four
handlers bound to live options threw a `TypeError` partway through** — every time
the user picked an input type, changed colour-by, dropped a variable into the
group box, or toggled guided mode. The analysis still computed, so it looked fine.

### The rule

> Code and schema move together. If an option goes, everything that reads it goes
> — `.b.R`, `.u.yaml`, `clearWith` lists, **and the `.events.js`**.

Park experimental work on a branch or in a directory excluded from the build. If
it's close to landing, restore the options and results behind a `visible:`
expression so the compiler keeps you honest.

### Also check

- **Options that are declared and never read.** `messages` in `jjbarstats` was in
  the UI and in `clearWith`, but no longer forwarded to `ggbarstats` — the user
  ticks a checkbox and nothing happens, and toggling it pointlessly clears the
  results.
- **Result elements declared and never populated.** Two `Preformatted` headings
  in `agreement` rendered as empty boxes.
- **Exported helpers with no call site.** Anything exported is a public API
  commitment.
- **Stale TODOs describing bugs you already fixed.** They send the next reader
  hunting for something that isn't there. "Stale TODOs are how a real backlog
  loses credibility."


**Round 8 (2026-10-23 meddecide [LOW]): the same shape, fifteen times.** Round 2 named two empty
`Preformatted` headings in `agreement`; both were fixed and the class was not. Round 8 found 15
more (`irrtableHeading` … `pairedAgreementHeading`), each shown only for its title, so Copy and
Export treat it as content, and each with a long `visible:` repeating its tables' visibility. Turn
each heading and its tables into a `type: Group` with the tables as `items:` — a Group's title is
the section heading, and it hides itself when all its children are hidden. Remove an option whose
own label says it has no effect (`iotaStandardize`), make an index into a list a `Variable`
(`predictionColumn`), and prefer two `Variables` boxes to a column-naming convention
(`interIntraSeparator`). **Enforce it:** `check_empty_preformatted` (WARN; a Preformatted with no
`setContent` directly, through `[["name"]]`, or through a helper) — MD 15, exactly the reviewer's;
JS 40, CPD 2.

---

## 12. Rule: UI label conventions

| Element | Convention | Example |
|---|---|---|
| CheckBox label | Name the **thing**, not the action | "Residual plot", not "Show residual plot" |
| Individual controls (TextBox, ComboBox, CheckBox) | Sentence case | "Ridge height scale", not "Ridge Height Scale" |
| `CollapseBox` / `TargetLayoutBox` headings | Title Case | "Statistical Options" |
| Options panel title (`.u.yaml:1`) | **Must match** `.a.yaml` `title:` | otherwise the user clicks one name and lands on a panel labelled another |
| `menuSubgroup` | Must describe the analysis | a diagnostic-accuracy meta-analysis does not belong under "IHC Analysis" |

| Notice titles and status labels | Sentence case, never capitals | "Bivariate model did not converge", not "PERCENT CHANGE LOOKS LIKE PROPORTIONS" |
| `List` option titles (the choices) | Sentence case, as jmv does | "Equal variances assumed", "Duration (longest first)" |
| One concept, one name | The same words in every analysis of the module | one colour-blind palette name, not "Color-blind safe" / "Colorblind-Safe (Okabe-Ito)" / "Colorblind Safe (Viridis)" |
| Spelling | One convention per module, in `.()` strings and `.a.yaml`/`.r.yaml` titles | not "tumour" in one analysis and "tumor" in the next |

Exception: a verb is correct when the checkbox really performs an action on the
dataset — "Add test pattern to data" is fine.

Variable selection comes **first** in every panel; everything else goes in
collapsed `CollapseBox` groups **at the top level**. 2026-10-23 OncoPath [LOW]:
`diagnosticmeta.u.yaml` put seventeen controls under three always-open `Label`
sections inside one `LayoutBox`, with its only `CollapseBox` between the first two;
opening it pushed the plot and reporting options down. Make each section its own
top-level `CollapseBox` (`collapsed: true` for all but the first), as
`waterfall.u.yaml` and jamovi's own `jmvplots/scat.u.yaml` do. `enable:` conditions
carry over unchanged.

Capitals read as shouting in the results and are harder to translate; 37 of
`waterfall`'s notice titles were capitals while the module's other analyses used
sentence case, so the module read as if different people wrote it. Changing a
title or a `List` title renames no option, so no saved analysis breaks, but each
changed msgid orphans its Turkish translation: re-key `tr.po` (section 9).

**Spelling: American** (project decision, 2026-10-24): `tumor`, `analyzed`, `color`,
`labeled`, `modeled`, `summarize`, `millimeter`. "Analyses" (plural noun) and
"characteristic" are American too; a regex for `analys`/`characteris` flags them
falsely. OncoPath was swept first (2026-10-23 OncoPath [LOW]), then the other four modules on
2026-10-24, and `check_label_case` now FAILs on any British form in shipped text (not only
on a module that mixes both). Sweep with an explicit word map, never a regex on
stems: `analys`, `characteris`, `optimis` and `organis` also match "analyses",
"characteristic", "optimism" and "organism".
Changing a msgid needs the Turkish re-keyed: add the new msgid with the old
translation (sentence-cased when the casing changed) and keep the old entry, because
another module may still use it; `i18nUpdate()` drops it where it is unused.

**Enforce it:** `tools/release_gate.py` `check_label_case` (all-caps `.()` strings;
when added: OncoPath 46, ClinicoPathDescriptives 8, meddecide 4, jjstatsplot and
jsurvival 0; **FAIL** since 2026-10-24, when the sweep reached 0 shipped everywhere), `check_collapsebox_nested` (**FAIL** since 2026-10-24: it found exactly
the reviewer's one site, `diagnosticmeta`, which is regrouped; 0 shipped everywhere, 32 in
parked analyses as promotion debt) and `check_collapsebox_titlecase`. List-title casing
and "one concept, one name" have no detector: judging them needs a reader.

### Round 8: labels a clinician reads (2026-10-23 meddecide [LOW])

| Convention | Wrong | Right |
|---|---|---|
| Variable boxes and group headings Title Case; individual controls sentence case | "Patient identifier" box beside "Outcome Variable"; "Step Size" checkbox | "Patient Identifier"; "Step size" |
| No R notation in a label | "Wald (symmetric, matches psych::cohen.kappa)", "Anticipated kappa (kappa0)" | "Wald (symmetric)", "Anticipated kappa" or "κ₀" (renames no option) |
| Name the thing, not the toggle | "Explain the imbalance warning in full" | "Imbalance warning details" |
| Eponyms possessive | "Fleiss kappa", "weighted Cohen kappa" | "Fleiss' kappa", "weighted Cohen's kappa" |
| One name per statistic | "Youden's Index" / "Youden Index" / "Youden's J" / "Youden index" | one of them, everywhere |
| One scale per neighbourhood | `confLevel` 0.95 beside `tdiCoverage` 90 | percent with `suffix: "%"`, as jmv's `ciWidth` |
| A stated range matches the option | "(0.001-0.999)" on an option accepting 1e-9 to 0.999999999 | the option's real `min`/`max` |
| Two options, two titles | `pp` and `pprob` both "Prior probability (prevalence)" | distinct titles |

**Enforce it:** `check_label_notation` (WARN) — `pkg::fn` or a camelCase argument in brackets in
any `.a/.u/.r.yaml` title or label, a label starting "Explain", and "Fleiss kappa"/"Cohen kappa"
in `.()` text. On the audited build: the reviewer's sites plus the `kappaSize` siblings and three
"(kappaSize)" results titles they did not name. Case, synonyms, units and ranges need a reader.

### Native controls for the value's shape (2026-10-23 meddecide [MEDIUM]/[LOW])

The panel is part of the analysis. The reviewer's stance, stated once in the report: if the native
shape cannot express what you need, open a feature request at
<https://github.com/jamovi/jamovi/issues> rather than building around it.

| Value | Native shape | What the reviewer found |
|---|---|---|
| Every variable role | one top-level `VariableSupplier`; optional roles greyed with `enable:` (jmv `linreg.u.yaml`) | `agreement`: six suppliers inside collapsed boxes, each with its own variable list; `decisioncurve`: one; `decision`/`decisioncompare`: a CollapseBox above the variables |
| A factor level | `type: Level` + `LevelSelector` (`allowNone: true`) | `enhancedROC validationNegative` typed as `String`; `agreement specificPositiveCategory` draws on several variables — a LevelSelector for a multi-variable box is a feature request, not a workaround |
| A fixed number of numbers | one `Number` option each (`partialAUCfrom`/`partialAUCto` already did) | `partialRange "0.8,1.0"`, `treatmentThreshold "0.05,0.5,0.05"`, the `kappaSize` `props` (≤5 values: `p1..p5`, greyed by outcome count) |
| An open-ended list | a `String` is fine — jamovi has no number-list control | `customCutoffs`, `prevalenceScenarios`, `nriThresholds` |
| A preset that overrides controls | grey the overridden controls (`enable: (clinicalPresets:custom)`) or write the preset's values into them from `events.js` (`cotest.events.js` does) | `enhancedROC` presets changed three thresholds while the boxes kept showing the user's values; a notice explaining it afterwards does not fix the panel |
| Free text only printed back | jamovi's Add Note | `cohortRole`, `samplingDesign`, `analysisUnit` …; keep a field that feeds a check (`modelVersion` for a frozen cohort), and give it the `enable:` its siblings have |
| A dependent control | next to the option that enables it | `agreement nBoot` enabled from three other boxes; `psychopdaROC costratioFP` far from its method |
| A section heading | a `Label` **wrapping** its controls; a `CollapseBox` per question | childless Title Case Labels (`psychopdaROC`); `cotest`'s "Display Options" a Label, not a CollapseBox |
| ComboBox choices | from the `.a.yaml` only | `nogoldstandard` repeated `options:` in a different order |

**Enforce it:** `check_variables_first` (exactly the reviewer's four panels; one more variable
role, `conditionBVars`, than they listed), `check_level_as_string` (keyed on option names ending
`Positive`/`Negative`/`Level`/`Category`/`Class`; the reviewer's two), `check_number_list_strings`
(any `String` default of 2–6 numbers; 8 on the audited build — the reviewer's 5 plus 3 open-ended
lists, which take a `# number-list: open-ended` comment above `- name:` once judged) and
`check_uyaml_structure` (`options:` on a ComboBox; childless Title Case Labels). All WARN.
Presets, free text and grouping by the user's question have no detector.

---

## 13. Notices: the native element, and the `insert()` trap

Two separate facts, held apart for a year by one sentence that merged them. Round 2 suggested a
native notice, we rejected it, and round 5 (2026-09-22 OncoPath [LOW]) came back to correct *us*:

> "You were right to reject that suggestion as written, and I owe you a correction. The audit on
> 2026-09-16 pointed you at `type: Notice` in `.r.yaml`. That's the wrong place, and the schema is
> correct to refuse it. jamovi's notices aren't declared in the results definition. They're created
> in R and inserted into the results tree at run time."

### 13.1 `type: Notice` in `.r.yaml` does not compile — still true

Verified against jamovi 28.1.0 / jmvtools 28.3 / jamovi-compiler 0.3.5, re-verified 2026-09-17
(jmvtools 28.3) and 2026-09-19 against the jmvtools 28.3.1 schema:

```
Unable to compile 'nt.r.yaml':
	results.items[0].type is not one of enum values:
	Table,Group,Array,Image,Preformatted,Text,Html,Svg,State,Property,Output,Notification,Action
```

No `Notice`. **`Notification` is worse:** it *is* in the enum, so it compiles, and emits
`self$add(list(name=..., type="Notification"))` — a plain list, because `jmvcore::Notification`
does not exist. It fails at run time instead of at compile time. `Svg` exists upstream but is
undocumented; do not use it yet.

### 13.2 `jmvcore::Notice` at run time works — the old claim here was FALSE

This guide used to say Notice objects "hold function references that jamovi's protobuf layer cannot
serialize". **That is wrong, and it was never measured.** `Notice$asProtoBuf` is two scalar
assignments; `Notice`'s only private fields are `.content` and `.type`. jamovi's own `jmv` uses the
API in four shipping places (`setAnalysisNotice`, `conttables`, `linReg`, `descriptives`).

Measured 2026-09-22, jmvcore 2.7.38 as bundled by jamovi 28.3.0.0, on a real analysis
(`waterfall`, 21 results items), through `Analysis$asProtoBuf(final = TRUE)` — the call the engine
makes to send results to the client and to write the `.omv` — then `RProtoBuf::serialize` and read
back:

| insert index | items after | `NULL`s | serialize |
|---|---|---|---|
| — (baseline) | 21 | 0 | OK, 12419 B |
| `insert(1, Notice)` | 22 | 0 | **OK** — `type` and `content` intact after the round trip |
| `insert(21, …)` (== length) | 22 | 0 | OK |
| `insert(22, …)` (length + 1) | 24 | 1 | **`attempt to apply non-function`** |
| `insert(999, Notice)` | 1978 | 1955 | **`attempt to apply non-function`** |
| **`insert(999, jmvcore::Html)`** | 1978 | 1955 | **fails identically** |

### 13.3 The real defect: `Group$insert()` has no bounds check

```r
Group$insert = function (index, item) {
    ...
    after <- private$.items[index:length(private$.items)]   # 999:21 counts DOWN
    private$.items <- c(before, between, after)
}
```

R's `:` counts down when the left operand is larger, so both slices run off the end and fill with
`NULL`. `Group$asProtoBuf` then walks `private$.items` and calls `item$asProtoBuf(...)`;
`NULL$asProtoBuf` is `NULL`, and `NULL(...)` is `attempt to apply non-function`. The element type
never enters into it. The body of `Group$insert` is byte-identical in every jmvcore from 2.3.4
(2022) to 2.7.38, so this is not version-specific.

**There is no append index.** The traps, all measured:

| You want | Use | Never |
|---|---|---|
| bottom of the results | `self$results$add(item)` | `insert(999, item)` — and `insert(length+1, item)` is the same bug |
| top | `self$results$insert(1, item)` on a **non-empty** group | `insert(1, item)` into an **empty** group — `1:0` is `c(1, 0)`, so it inserts a `NULL` too |
| a notice on an `Array` | `array$setHeader(notice)` | `array$insert(...)` — Arrays have no `insert` |
| the content | `notice$setContent(...)` after construction | `Notice$new(..., content = )` alone — it leaves `.stale = TRUE`, `results$isFilled()` goes FALSE and the analysis never reaches `complete` |

Two more measured properties, both **within one results tree**: three inserts of the same `name`
ship as three elements, and `Group$remove()` removes nothing — its own `after` slice re-includes
the item it was asked to drop. Neither carries over **between run cycles**: each cycle builds a
fresh tree, and `Group$fromProtoBuf()` restores only items that tree already declares, so a
`Notice` inserted in `.run()` appears exactly once per cycle. What *does* pile up across cycles is
the hand-rolled pattern, appending to a declared `Html` item's content: that item is restored
every cycle with its old content (measured 2026-10-24, `tests/testthat/test-zzz-jmvcore-notice-run-cycle.R`;
section 13.6). jamovi's `jmv` sites we have read (`conttables`, `linreg`'s Mahalanobis note) insert
from `.init()` or an `.init*()` helper it calls.

### 13.4 How the false claim happened, and what it cost

Worth reading as a case study in inference dressed as measurement.

1. **2025-11-14 `9ab6a3114`** — the first `jmvcore::Notice` use lands, *and* `.claude/commands/fix-notices.md`
   lands with it, teaching `self$results$insert(999, n)` for "INFO — analysis summary (bottom)".
2. The template is followed faithfully. Active `insert(` calls go 91 → **450, of which 321 at `999`**,
   across 47 files by 2025-12-29.
3. **2025-12-27** — `survival` crashes. `tests/SURVIVAL_SERIALIZATION_FIX.md` blames
   `insert(1, na_notice)`; removing it does not help (the one `insert(999,)` at `survival.b.R:1468`
   is still live). All ten notices are removed; the error goes. `tests/SURVIVAL_ALL_NOTICES_REMOVED.md`
   records the conclusion: *"ALL dynamically inserted `jmvcore::Notice` objects cause serialization errors."*
4. **2025-11-16 `68c2065fa`** had already written that into `CLAUDE.md` as mechanism —
   *"contain function references that cannot be serialized"* — six weeks before the incident it
   purports to explain. Every later write-up cites `CLAUDE.md` as its authority. None tests it.
5. **2026-06-07 `d1b6465f7`** replaces the API with hand-rolled HTML. Today: **80 `.addNotice()`
   helpers, 1,408 call sites, 0 uses of the native element.**

The *symptom* was real and was observed in the app, twice. The *cause* was inferred once, from a
file that mixed nine valid indices with one sentinel, and then propagated into `CLAUDE.md`, this
guide, the notices guide, five command files and a code breadcrumb the reviewer reads.

### 13.5 What this does and does not license

It does **not** license migrating the HTML helpers back to `Notice`. They are kept for reasons the
experiment never touched: notice content renders as **escaped plain text** and takes **no
newlines** (standing user decision, reaffirmed), and this project wants multi-line panels. The
rendering side has never been measured. Change it only on evidence about *rendering*, not about
serialization.

### 13.6 Round 7: the retraction invented a second mechanism

Our 2026-09-22 response retracted the serialization claim and, in the same paragraph, gave a new
reason to keep the HTML helpers: *"Dynamic notices accumulate across run cycles … `jmv` analyses
like `conttables.b.R` only insert notices during `.init()`."* The breadcrumb above three
`.addNotice()` helpers said *"insert() in .run() accumulates across runs (Group$remove cannot drop
items)"*. Nobody ran it. The reviewer disputed it on 2026-10-23.

The experiment took ten minutes (`test-zzz-jmvcore-notice-run-cycle.R`): a `Notice` inserted in
`.run()`, three cycles, each a fresh analysis restored from the previous cycle's protobuf, gives
one notice every time. Appended `Html` content gives one, two, three. Our stated reason was
backwards: the pattern we kept is the one that accumulates, and our own `survival`, `bayesianci`
and `categorize` already reset their notice Html at the top of `.run()` for exactly that reason.

The reviewer's counter-example was not exact either. Both `jmv` sites they cited insert from
`.init()` (`conttables` directly, `linreg` via `.initMahalTable()`), not from `.run()`. Our
location claim was right and our mechanism was wrong; their mechanism was right and their
location was wrong. Write down only what was run.

What stands: one `Notice` holds one plain-text content string with no separate title, which
cannot express a bold title plus a paragraph. The reviewer agrees that is a real limit. Their two
suggestions are both cheap:

1. **Ask jamovi** for a Notice with a title and a multi-paragraph body
  (<https://github.com/jamovi/jamovi/issues>). OncoPath alone has 60+ distinct notices across four
  analyses, each a short title plus a paragraph, ordered by severity.
2. **Until then, one presentation.** OncoPath had two: three analyses drew `rgba()` banners with
   fixed-hue borders, and `swimmerplot` wrote a plain-text block with `ERROR:` / `STRONG WARNING:`
   prefixes. Done 2026-10-24: `R/utils-notices.R` (`notice_append()`, `notice_panel_html(self, …)`)
   is the one renderer. It draws the banner, puts the severity in words in the title (so it does not
   rest on colour alone), sorts most severe first, keeps newlines, and returns `"<div></div>"` for an
   empty panel. That last point fixed a real bug: `waterfall` cleared with `""`, which the client
   skips, so the previous run's notices stayed on screen. A new module with notice panels should
   call these helpers, not copy a renderer.

### Where this leaves you

| Need | Use |
|---|---|
| Fatal validation error | `jmvcore::reject(.("..."), code = "...")` — see [section 25](#25-rule-an-unconfigured-analysis-is-not-an-error) |
| Non-fatal warning shown inline | `type: Html` element + theme-safe styling ([section 4](#4-rule-html-output-must-be-theme-safe)) |
| Narrative / explanatory text, no severity | `type: Text` (jamovi 28.3+, module-wide `minApp: 28.3.0`) |
| A native severity banner | `jmvcore::Notice` + `$setContent()` + `$add()`/`insert(1, …)` — works, but plain text and single-line |
| Tabular reference data | a `Table` result, never HTML ([section 26](#26-rule-tabular-data-belongs-in-a-table-result)) |

### Enforce it

`tools/release_gate.py` `check_sentinel_insert` — **FAIL**, and green at 0 hits in the umbrella and
all five siblings on 2026-09-22, so it is a gate that can stay red-free. It flags any
`$insert(<literal ≥ 100>, …)` in `R/`; the largest results tree measured in this project has 21
items, so a three-digit literal is always the sentinel bug.

### Say so in the code

The breadcrumb above every `.addNotice()` helper used to read *"REJECTED: no native notice
element"*. Those three words were the false part, and the reviewer read them and wrote a finding to
correct them. The channel worked; the content was wrong. Replace with:

```r
# library-audit 2026-09-22 OncoPath [LOW]: a native notice DOES exist - jmvcore::Notice built in R
#   and inserted with $add()/insert(1, ...). Not adopted here because notice content renders as
#   escaped plain text with no newlines, and these panels are multi-line. `type: Notice` in .r.yaml
#   is separately impossible (not in the compiler enum). Guide section 13.
.addNotice = function(type, title, content) {
```



### Round 8: the retracted reason still in the code (2026-10-23 meddecide [LOW])

The reviewer quoted `nogoldstandard.b.R:16-19` — "avoids BOTH the jmvcore::Notice serialization
error from `insert(999, Notice)`" — and set it beside `kappaSizeCI.b.R:892`, which already says
the opposite. The umbrella's correction of 2026-09-22 rewrote the comments it knew about; a
templated comment copied into dozens of files was not among them. The reason left in the code is
the one the reviewer reads as our position.

The same finding repeats the round-7 asks for meddecide: colour as the only severity marker
(`decisioncompare` sets `icon = ""`), an error reported two or three ways (a `<b>Error:</b>`
summary, an ERROR notice and `reject()` for one condition in `psychopdaROC`), a missing package
reported as a banner followed by a normal return (`lassologistic`, glmnet — §25's fatal half),
and untranslated lines in `agreement`'s computed-variables message. Report each error once,
through `reject()`; a failed **optional** sub-analysis is a WARNING `jmvcore::Notice` naming it,
not a `setNote("error", …)` on an empty table (`agreement`, about 30 sites by the reviewer's
count; 102 by the gate, which reads calls that span lines).

**Enforce it:** `check_notice_myths` (WARN) — a comment within two lines of "Notice" that
mentions serialisation and not the correction; on 2026-10-24 meddecide 3 (the reviewer's one, and
two they missed), CPD 29, JJ 10, JS 9, OP 2. `check_setnote_errors` (WARN) for the footnotes.

---

## 14. Encoding review findings as tests

The reviewer noticed, and called out approvingly:

> "the new `test-clinicopath-descriptives-audit.R` alongside it suggests you're
> building a habit of encoding review findings as tests. That's the right
> instinct."

and, on why a whole class of finding did not recur:

> "The dependency regression test is still the standout. `test-zzz-dependency-declaration.R`
> catching the `MASS`/`boot` class of problem is why that finding didn't recur."

### The habit

Every audit finding becomes a test **before** you fix it, at the level of the
**class**, not the instance:

- `tests/testthat/test-zzz-dependency-declaration.R` — every `::` and
  `importFrom` is declared
- `tests/testthat/test-zzz-results-rendering-contract.R` — no unguarded
  `image$state`; no opaque light-theme background; no non-structural HTML entity
- `tests/testthat/test-<module>-library-audit.R` — one `test_that()` per finding
  from that module's report

A test written at the class level is what turns "we fixed the four `venn`
renderers" into "this can't come back anywhere in the module."

### What a finding test must not assert

2026-10-23 OncoPath [LOW]: OncoPath 1.1.2 shipped with three failing audit tests, and their
`tests/testthat/_problems/` output committed alongside. Only one of the three caught a real defect
(the missing `NEWS.md` heading). The other two were bad tests:

- **A breadcrumb in a file users read.** One test required
  `library-audit 2026-09-22 OncoPath [LOW] DONE:` to appear in `README.md` or `DESCRIPTION`, and
  another asserted one in `NEWS.md`, which is the GitHub release body. Breadcrumbs go in code
  comments, never in user-facing files (the library-audit breadcrumb conventions
  already said so). A test that *requires* one there turns review bookkeeping into published
  text. Test the claim the finding was about.
- **The wording instead of the claim.** A test pinned the README heading
  `Adapted RECIST (v1.1 )?Thresholds`. The README was rewritten more accurately, with a section that
  says plainly the analysis is not a RECIST v1.1 implementation, and the test failed. Assert what
  must be true (the disclaimer sentence exists; the unqualified claim does not), never a heading
  someone may improve.
- **A red suite at the tag.** A failing test is a release blocker. If a test is wrong, fix the
  test in the same commit, with a breadcrumb saying why.

`tools/release_gate.py` `check_review_breadcrumbs` (**FAIL** since 2026-10-24) flags a `library-audit` tag in
`README.md`/`README.Rmd`/`DESCRIPTION`/`NEWS.md` and on a test line that greps for one in those
files. `check_artifacts` FAILs on tracked `_problems/` and `testthat-problems.rds`.

---

## 15. Rule: `requiresData` is a contract with `self$data` at render time

**[HIGH] — 2026-09-15 jsurvival: `multisurvival` `plot_adj` and `survMetricsPlot`.**

### Why

Read from jmvcore 2.7.38's own source:

- `Analysis$run()` sets `private$.data <- NULL` as soon as `.run()` returns, unless the
  data frame was handed in by the caller.
- `Analysis$.createImage()` (every redraw) and `Analysis$.savePart()` (*Export…*) re-read
  the dataset **only** when `image$requiresData && is.null(private$.data)`, keep it while the
  renderer runs, and null it afterwards.
- `Analysis$.createPlotObject()` — behind the R-side `image$.render()` — also re-reads it, but
  restores `NULL` in an `on.exit()` *before* the wrapped renderer runs. **A test that renders a
  `requiresData: true` image through `$.render()` never sees the data the flag provides**; use
  `analysis$.createImage(image$.__enclos_env__$private$.renderFun, image)` to reproduce the engine.
- `jmvcore::Image`'s default is `requiresData = FALSE`.

So a render function that runs *outside* `.run()` — on resize, on reopening a saved
`.omv`, on export — sees `self$data` as `NULL` unless its image declares
`requiresData: true`. In `multisurvival`, both renderers called
`private$.cox_model()`, whose cold-cache path goes `.cleandata()` → `.getData()` →
`self$data` → `jmvcore::reject("Data contains no (complete) rows")`. No `tryCatch`, so
the **whole analysis** flipped to an error state instead of drawing a plot. It works
in the run that fits the model (caches still warm), which is why nobody saw it.

It is also invisible to testthat: the R wrapper passes `data =` in, jmvcore never
clears it, and every renderer finds its data.

This is the reasoning of [section 3](#3-rule-render-functions-must-null-guard-imagestate)
("the renderer runs without `.run()`") — section 3 applied it to `image$state` only.

### The rule

| The renderer, **following every `private$` helper it calls**, reads | `requiresData` |
|---|---|
| only `image$state` (and options) | omit it — the default is `FALSE` |
| `self$data`, `.cleandata()`, `.getData()`, or a helper that refits a model from data | `true` |

The opposite mistake is cheaper but still a finding (LOW, 29 images in jsurvival):
`requiresData: true` on a renderer that never touches the data makes jamovi read the
whole dataset from disk before every redraw and every export, for nothing.

**Better than the flag:** compute in `.run()`, store the small drawing frame with
`setState()` ([section 17](#17-rule-image-state-holds-drawing-data-not-models-or-datasets)),
and make the renderer draw only. Refitting in the renderer is a legitimate trade-off
only when the stored object would be huge (the `rms` nomogram) — and then it needs
`requiresData: true`.

### Enforce it

`python3 tools/release_gate.py` traces each `renderFun` through the `private$`
helpers it calls. A production analysis whose renderer reaches the data without
`requiresData: true` is a **FAIL**; the reverse is a WARN count.

**Round 4 (2026-09-16):** the surplus WARN came back in OncoPath (5), ClinicoPathDescriptives (5),
jjstatsplot (7) and meddecide (18) — a WARN only jsurvival had been swept against. OncoPath is 0;
the rest are answered in their own reports. The reviewer named six OncoPath images and was wrong
about one: `waterfall` `.waterfallplot()` → `.annotationTrack()` reads `self$data`. Removing the
flag there raises no error; the annotation tracks silently vanish on resize, reopen and export.
Trace helpers, then prove it on the engine path — a fresh analysis with a counting
`.setReadDatasetSource()`, `private$.data <- NULL` after `init()` (init leaves a 0-row header frame
that suppresses the re-read), state restored, `.createImage()`: 1 read per render with the flag,
0 without, and the output compared. `tests/testthat/test-oncopath-library-audit.R` carries an R port
of the trace for a module's own tests.

---

## 16. Rule: never wrap `jmvcore::reject()` in a catch-all `tryCatch`

**[MEDIUM] — 2026-09-15 jsurvival: `lassocox` `.run()`.**

### Why

`jmvcore::reject()` is `stop(createError(...))` — a plain `simpleError`, the same
class as any library error. So an `error =` handler around validation code catches
every validation message:

- jamovi's own failure presentation (pane greyed, message shown, results left in
  place) never happens — the user gets a hand-built red box on a complete-looking pane;
- a handler that clears outputs (`deleteRows()`, a `.clearAnalysisOutputs()` helper)
  deletes the fixed rows `.init()` built, so tables collapse and restructure while the
  user clicks through an invalid intermediate selection;
- an inner `tryCatch` that re-wraps the message corrupts it:
  *"Error creating design matrix: At least two … engine.. Check factor coding …"*;
- it also swallows the `.checkpoint()` restart, which is error-class too.

### The pattern

```r
# WRONG - every reject() below lands in the handler
.run = function() {
    tryCatch({
        data <- private$.cleanData()          # 33 jmvcore::reject() calls inside
        fit  <- private$.fitModel(data)
        private$.populate(fit)
    }, error = function(e) {
        private$.clearAnalysisOutputs()       # tables collapse
        self$results$todo$setContent(e$message)
    })
}

# RIGHT - validation propagates; tryCatch wraps only the third-party call
.run = function() {
    data <- private$.cleanData()              # rejects reach jamovi verbatim
    fit <- tryCatch(
        glmnet::cv.glmnet(data$x, data$y, family = "cox"),
        error = function(e) jmvcore::reject(
            jmvcore::format(.("The LASSO Cox fit failed: {msg}"), msg = conditionMessage(e)),
            code = "fit_failed"))
    private$.populate(fit)
}
```

If a broad safety net must stay, re-raise first:

```r
error = function(e) {
    if (!is.null(e$code)) stop(e)             # coded reject() and the .checkpoint() restart
    ...
}
```

— but `reject()` **without** `code =` leaves `e$code` `NULL`, indistinguishable from a
library error. That test only works if every `reject()` in the guarded region passes a
`code`. Narrowing the `tryCatch` is the reliable fix.

`withCallingHandlers(warning = …)` that collects warnings into a notice is fine and
worth keeping — it does not intercept errors.

### Where the anti-pattern came from

The `.b.R` template in `.claude/commands/create-function.md` and
`vignettes/jamovi_notices_guide.md` §8 both wrapped the whole `.run()` in `tryCatch`.
Both are corrected. New analyses copy templates faithfully.

---

## 17. Rule: image state holds drawing data, not models or datasets

**[LOW] — 2026-09-15 jsurvival: `multisurvival` `plot`/`plot3`/`plotKM`/`plot_adj`,
`oddsratio` `plot_nomogram`.**

### Why

`image$state` is serialised into the saved `.omv`. A `survival::coxph` fit carries its
model frame, `x` matrix, call and environment; a cleaned dataset is a second copy of
the data. Neither bears any relation to the handful of numbers a plot draws. The
reviewer cites jamovi's image-state guidance: don't store the model object, extract
what the plot needs.

### The rule

```r
# WRONG
self$results$plot$setState(c(cleaneddata, list(cox_model = cox_model)))

# RIGHT - the extracted frame the renderer draws
self$results$plot3$setState(list(
    coef = data.frame(term = ..., HR = ..., lower = ..., upper = ..., p = ...),
    plot_title = self$options$plot_title))       # visual options still belong here
```

- Forest plot → the coefficient table. Adjusted curves / hazard plot → a small
  `time × estimate × group` frame. KM plot → the `summary(survfit)` columns.
- Reference implementations: `multisurvival` `riskGroupPlot` state, `lassocox`
  `.savePlotData()` (vectors and small frames, never the `cv.glmnet` object).
- If the plot truly needs raw rows, use `requiresData: true` + `self$data`
  ([section 15](#15-rule-requiresdata-is-a-contract-with-selfdata-at-render-time)) —
  don't stash the dataset in state.
- Still convert to a base `data.frame` before `setState()` (protobuf).

Detect: `grep -nE 'setState\(.*(model|fit|clean(ed)?[Dd]ata)' R/<fn>.b.R`


**Round 8 (2026-10-23 meddecide [LOW]): rows the plot really draws.** A dot plot of one point per
patient and a rater profile of every rating need the raw rows, so they are drawing data — and
`check_state_payload`, which matches names (`fit`, `model`, `cleanData` …), reported 0 for
`dotImage$setState(rawData)` and `raterProfilePlot$setState(list(plot_data = …))`. But the state
is copied into the `.omv` once per image: the dot plot is an Array, so once per marker. When the
state grows with n × images, prefer `requiresData: true` and build the frame from `self$data` in
the renderer, keeping only scalars (threshold, direction, labels) in state
(<https://dev.jamovi.org/tutorial/tuts0301-image-state-performance>). This is the other side of the
round-5 `waterfall` trade-off, where one value per patient was the cheaper choice. jmvcore itself
prints `WARNING: state object … is too large` above 500 KB of **compressed** state
(`ResultsElement$asProtoBuf`), so the runtime check is
`length(memCompress(serialize(image$state, NULL)))` at a realistic n, not a name match.

---

## 18. Why round 3 still found things: how rules decay

Round 2 closed its findings and wrote every one down as a rule. Round 3 (2026-09-15
jsurvival) still found nine. Four were classes we *already had rules for*. The lessons
are about the rules, not the code:

| Finding | Rule existed? | Why it recurred |
|---|---|---|
| `requiresData` missing / surplus (HIGH + LOW) | No | Every example in the plots and r.yaml guides showed `requiresData: true` as boilerplate, and the plots guide's property table claimed the default is `true` (jmvcore's is `FALSE`). Section 3's "renderers run without `.run()`" was applied to `image$state` and never to `self$data`. |
| Catch-all `tryCatch` swallows `reject()` (MEDIUM) | No — the opposite was taught | The `create-function` template and notices guide §8 wrapped the whole `.run()` in `tryCatch`; `lassocox` followed that shape. |
| Model object / dataset in image state (LOW) | No — the opposite was taught | The plots guide's "Recommended State Structure" had `data = cleanedDataFrame`; the b.R guide stored `fit =` and `model =`; CLAUDE.md's quick reference said nothing about size. |
| Sentence-case `CollapseBox` headings (LOW) | Yes, §12 | Checked by eye only. `lassocox` was new code written after the rule; nothing machine-checked it. |
| Leading space/punctuation in `.()` (LOW) | Yes, §9 + checklist grep | The checklist grep returned **200 hits** on jsurvival — almost all `collapse = ", "` — so the 14 real sites were invisible. And August fixed the named *sites*, not the *class*. |
| Two untranslated `reject()`s (LOW) | Yes, §9 | Left bare on purpose: they sit in a file-level helper whose file header forbids `.()` (#122). The reviewer's suggested fix is unsafe as written — see §9. |
| Dead `ggstatsplot` ref (LOW) | Yes, §1 | `_updateModules.R` `collect_used_refs()` counted a **commented-out** `#   refs: ggstatsplot` as a citation, so the per-submodule trim kept the entry. Fixed in the generator. |
| Ref with no `url` (LOW) | Yes, §1 | `release_gate.py` reports it as one WARN listing 18 umbrella entries; WARN lines get skimmed. |
| Long methods (LOW) | Known | Deferred by decision. |

**What to do differently:**

1. **A rule without a machine check is a wish.** Every new rule gets a line in
   `tools/release_gate.py` or a `test-zzz-*` test the same day. Round 3 added the
   `requiresData` trace, the `CollapseBox` Title Case check and the `.()` padding check.
2. **Test the check against the reviewer's list before trusting it.** A check that
   reports 200 hits is never read; a check that reports 0 may be blind. The new checks
   reproduce this report exactly (2 missing / 29 surplus `requiresData`; 4 headings;
   all 14 `.()` sites — the report names 13 locations; the 14th is `survivalcont.b.R:1554`).
3. **Fix the teaching material, not just the code.** Templates and guide examples are
   copied faithfully. When a finding traces back to an example, the example is the bug.
4. **Fix the class, then scan the whole module.** "The sites flagged in August are all
   fixed; these are a different set of 14" is the reviewer telling us we fixed instances.
5. **Generalise the mechanism, not the symptom.** "jamovi re-renders without `.run()`"
   constrains *every* renderer input — state, `self$data`, caches.
6. **Verify the reviewer's fix too.** The reviewer is usually right about the problem and
   occasionally wrong about the fix (`.()` in a file-level helper here; 2026-08's
   "unused" packages that the umbrella does use).

### Round 4 (2026-09-16 OncoPath)

Eight findings; five were caused by our own earlier remediation.

| Finding | Rule existed? | Why it came back |
|---|---|---|
| `%>%` not importable — `waterfall` could not run (CRITICAL) | No | Round 2's cleanup added `magrittr` to OncoPath's `prune_imports` (commit `55a4e7186`). Every local check passed because `~/.Rprofile` attaches magrittr and `load_all()` sees everything. Now §19, the bare-symbol guard and `tools/submodule_smoke.R` (installed namespace, `--vanilla`). |
| Catalogs 93% another module's strings (MEDIUM) | No | Round 2 answered "no catalogs" by copying the umbrella catalog (`i18n_files`, commit `99779adfe`); nothing trimmed it, and the meddecide audit test asserted the copy. Now `i18nUpdate()` in `build_module()` + `check_i18n_catalog_scope`. |
| `requiresData: true` on state-only renderers (LOW) | Yes, §15 | A WARN, swept in jsurvival only. The reviewer's list included one site that must keep the flag. |
| `.()` padding and spliced band words (LOW) | Yes, §9 | Padding: WARN, skimmed. Fragments: no detector; the August coverage work wrote them, plus a translated-word comparison and a `" ["` string that only fail outside English. |
| `NEWS.md` one release behind (LOW) | No | The updater bumps `Version:` on every regeneration; `NEWS.md` belongs to a person and nothing checked it. Now `check_news`. |
| Long functions (LOW) | Known | Deferred by decision. |
| Two TODOs (INFO) | — | Both hid real defects: `[[APPROX]]` after a space truncated two sentences; the other named ~1,800 untranslated words. |
| Native `Notice` instead of HTML (INFO) | Yes, §13 (rejected) | NO-CHANNEL: the rejection lived in a report the reviewer never sees. The theme point was right — our round-2 comment "titles saturated enough to read on both" was never measured (2.7–2.9:1). |

**What to do differently (round 4):**

1. **Check the tree that ships, in the environment that ships.** `--vanilla`, `release_gate.py
   --root <sibling>`, `submodule_smoke.R`. The CRITICAL passed every umbrella check.
2. **Our fixes are the main source of new findings.** A `prune_imports` entry, a catalog copy, an
   i18n pass and a theme comment each created a finding. Treat remediation commits as code under
   review: a class test, a breadcrumb, and a check on the shipped tree.
3. **Translation work needs a non-English test.** English output cannot show splices, padding,
   translated-word comparisons, `" ["` truncation or broken `%` in a translation; a pseudo-catalog
   (§9) and the Turkish catalog can.
4. **A comment that asserts a measurable property must have been measured.** Contrast, sizes,
   counts.

### Round 4 (2026-09-16 meddecide)

Fifteen findings. The module's **third** audit, and the third time two of the same classes came
back — the interesting part is *which* ones, and why.

| Finding | Rule existed? | Why it came back |
|---|---|---|
| Forest plot exports blank — renderer reads a `private$` cache, not `image$state` (HIGH) | Yes, §3 + §15 | INSTANCE. `psychopdaROC` set the state correctly and then never read it; every *other* plot in the same file reads `image$state`. §3 was written about a missing NULL guard, and nobody generalised it to "the renderer's only input is its state". Now `check_render_private_state`, which traces each `renderFun` through its `private$` helpers. |
| ROC overlays drawn from `private$.rocDataList` (MEDIUM) | Same | Same class, same file, and **worse than a crash**: the guards made `NULL` degrade silently, so the exported figure simply lacked the confidence bands the user ticked. A failure that does not fail is the expensive kind. |
| Fixed-row `epiR` tables still built with `addRow()` in `.run()` (LOW) | Yes, §6 — raised in **all three** meddecide rounds | NO-CHECK. §6 has been written since round 1; the only enforcement was checklist item 5, a grep returning **620 hits** umbrella-wide, so it was never read (the same failure mode as the `.()` padding grep in round 3). Now `check_init_row_structure`, which asks the question at the **table** level: 1 shipped hit across all five modules. |
| `.()` fragments and padded literals (LOW) | Yes, §9 — raised in all three rounds | INSTANCE ×3. Rounds 1 and 2 fixed the *named sites*; the class detector (`check_i18n_padding`) arrived only in round 4 and reads 0 in the umbrella against 6 in the shipped tree — the sibling had not been regenerated, which is why this one is answered PARTIAL rather than DONE. |
| Catalogs inherited from the umbrella, 15 MB, 2/3 of the module's strings missing (LOW) | No | OUR-FIX, exactly as in OncoPath: rounds 1 and 2 said "no catalogs", we answered by copying the umbrella's. The remedy (`i18nUpdate()` at build + `check_i18n_catalog_scope`) also **orphans 576 translated Turkish entries**, because closing the fragment finding rewrote the msgids. Recorded on the response as a measured, deliberate trade: an orphan falls back to English, an unreorderable multi-conversion template makes `sprintf()` raise mid-analysis. |
| Bare `set.seed()` leaks a fixed RNG stream into the engine (LOW) | No | NO-RULE. Three idioms coexisted in one module — `withr::local_seed()`, a hand-rolled save/restore, and a bare call. Nothing user-visible fails; the *next* analysis in the same session quietly resamples from a deterministic stream. Now `check_bare_set_seed`. |
| Six declared dependencies never used (LOW) | Yes, §10 | §10 was written for the *opposite* direction (used but undeclared). `R/zzz_imports.R` existed to silence the R CMD check NOTE, which is precisely what let unused entries sit there. Now `check_unused_imports`; `rlang` was correctly kept — the reviewer missed the `.data` pronouns. |
| Surplus `requiresData: true` on eighteen images (LOW) | Yes, §15 | WARN-SKIMMED, the same as OncoPath the same day: the check existed and reported, and the WARN was not acted on. |
| Tables/plots with no `clearWith` blink on a display toggle (LOW) | Partly, §1 | `check_clearwith` verifies that entries *resolve*; nothing asks whether an element that has **no** `clearWith` should have one. Still no detector — the judgement ("is this option purely cosmetic?") is not mechanical. Recorded as a known gap. |
| Error-handling scaffolding grows a stack per panel open (LOW) | No | NEW, and a real leak: `clinicopath_init()` appended to a package-level vector and captured `sys.calls()`; the only popper was never called. Fixed by making init **set** rather than append. |
| `caret` cited nowhere while `enhancedROC` computes its metrics with it (LOW) | Yes, §1 | `check_refs` already reports dangling keys; the fix is one line in a `refs:` list. |
| Long `.run()` methods (LOW) | Known | Deferred for the fourth time. |
| One-choice combo box, `NEWS.md` heading, three `TODO`s (INFO) | Mixed | The `TODO`s were raised in all three rounds and deferred all three times — NO-CHANNEL: the reasoning lives in the report the reviewer cannot see, so a comment that *records a decision* still reads as forgotten work. If a marker is deliberate, say so on the line and drop the `TODO` keyword. |

**What to do differently (round 4, meddecide):**

1. **A grep with 620 hits is not a check.** Item 5 had been in the checklist since round 1 and the
   class came back three times. The fix was not a better grep but a better *unit*: asking the
   question of the table instead of the `addRow()` call took 620 unreadable hits down to 1
   actionable one, because "one extra conditional row on an `.init()` skeleton" is not the defect.
2. **Silent degradation outranks a crash.** The ROC overlays were `NULL`-guarded so well that the
   export simply lost the bands. Guard to a state the user can *see* — a missing plot, a note —
   not to a plausible-looking wrong one.
3. **State is the renderer's only input.** §3 said "NULL-guard `image$state`"; the defect was a
   renderer that never *read* it. State the mechanism (the renderer runs without `.run()`), not
   the symptom.
4. **Answering "it is missing" by copying the umbrella's version creates the next finding.**
   Twice now, for catalogs. Whatever we copy in has to be trimmed to the module at build time.
5. **A deliberate `TODO` is a contradiction.** Three rounds of "these are intentional" never
   reached the reviewer. Write the decision as a decision; keep the keyword for real debt.

### Round 5 (2026-09-22 OncoPath)

| Finding | Rule existed? | Why it came back |
|---|---|---|
| Red ERROR before the user has configured anything; fatal via a banner (MEDIUM) | Yes, twice — R1 raised the fatal half, R2 the empty-state half, both marked DONE | INSTANCE, and OUR-FIX. R2's fix moved the banners into a new `notices` channel and re-created them there (`0dbd3d348`); `48b8edb13`, **one day before this audit**, fixed `swimmerplot` alone and wrote the reasoning the reviewer quoted back at us, without sweeping. No detector either time. Now §25, and a measured reason the gate is not on yet. |
| Reference tables drawn as HTML, not `Table` (MEDIUM) | No | NO-RULE. `theme_safe_html.py` cannot see them — the sites use `#ddd` borders and an `rgba(…,0.06)` header, and the script only flags opaque backgrounds. The sweep found **97 shipped blocks, not 2**. Now §26. |
| Notices: the reviewer corrects *their own* advice (LOW) | Yes, §13 — and §13 was half wrong | **WRONG-DONE, and the channel worked.** R4's remedy (a breadcrumb the reviewer can read) succeeded exactly as designed: they read it and answered. What failed was its content — three words, `no native notice element`, generalising a verified `.r.yaml`-schema fact into a claim about the platform. The underlying "Notice cannot be serialized" was inferred once in 2025-11, six weeks *before* the incident it explains, and never tested. See §13.4. |
| `stringr` declared, unused (LOW) | Yes, §10 | WARN-SKIMMED + OUR-FIX. `check_unused_imports` fired and named **two** packages — `psych` as well, which the reviewer missed. Our own rework (`8570f416f`, three days earlier) removed the last `str_to_title()`. Two standing written claims are now false: `R/zzz_imports.R` says "Every package below IS used … Do NOT prune them" and `_updateModules_config.yaml` repeats it naming `psych` and `stringr`. |
| `NEWS.md` has no 1.0.83 heading (LOW) | Yes, §1 + `check_news` | WARN-SKIMMED — and the check was **wrong**, not merely weak. It warned on four-component dev builds the release workflow never publishes, so all five modules sat permanently red and the one module about to ship a placeholder was invisible. Now the check mirrors the workflow's own cascade: 5/5 red → 1/5. |
| Module describes itself inconsistently (LOW) | No | NO-RULE, and UMBRELLA-SCOPE for the RECIST half: the careful wording was added where we author (`jamovi/waterfall.a.yaml`, `0000.yaml`) and never reached `DESCRIPTION` / `README.md`, which the updater does not write. Now §27 and three gate checks. |
| Long functions (LOW) | Known | Deferred, by the standing decision recorded in R2, R3 and R4. |
| `waterfall` `requiresData: true` is correct (INFO) | Yes, §15 | **Closed the right way.** Our R4 rejection lived in a breadcrumb in `jamovi/waterfall.r.yaml`; the reviewer read it and agreed on the record. The remedy for NO-CHANNEL works. |

**What to do differently (round 5):**

1. **A WARN that is red everywhere is a broken check, not a known problem.** `check_news` was
   5/5 red because it asked the wrong question. Before adding hits to the backlog, confirm the
   check's question matches the thing that actually breaks — here, what the release workflow
   publishes.
2. **When a suggestion is rejected, write down the narrow fact, not the general one.** `type:
   Notice` does not compile is verified and narrow. *No native notice element* is a claim about
   the platform, and it was wrong. A breadcrumb is read by someone who knows more than we do.
3. **An inferred cause must be labelled as inferred.** The 2025-11 claim was written into
   `CLAUDE.md` as mechanism six weeks before the incident it explains, then cited as authority by
   six later write-ups, none of which tested it. Cost: 80 hand-rolled helpers and 1,408 call
   sites, replacing an API that worked.
4. **Fixing the third instance is not sweeping the class.** `swimmerplot` was fixed one day
   before the audit that found the same shape in two siblings — and the audit missed
   `swimmerplot`'s *own* second instance.

### Round 6 (2026-09-16 meddecide, closing pass)

| Finding | Rule existed? | Why it came back |
|---|---|---|
| `prune_imports` deleted five packages `agreement` calls | §19 (partly) | §19 covers a tag leaving WITH a re-routed analysis. Nobody had written the inverse: the analysis leaves, its packages look unused, and the delete order outlives the routing. |
| Audit test errored on an absent analysis | no | The test's analysis list was a hard-coded snapshot of membership. No rule said membership is derived, not recorded. |

Both are one mechanism, now §29. Neither was a shipped defect: `prune_conflicts()` stopped
the first at plan time and the second only broke a test. What they cost was a day of
reading a clean module as a broken one — the gate said `6 unused Imports`, which looked
like a closed finding regressing, and the suite said eight failures, which looked like
code. Both were reporting a `menuGroup` suffix.

The lesson is narrower than "be careful": a measurement taken while an analysis is parked
is not a measurement of the module. Check the routing before believing the instrument.

### Round 7 (2026-10-23 OncoPath, release 1.1.2)

The report arrived in the library's "1.1.2 is on its way" email, so every finding below shipped.
Both round-5 MEDIUMs held; nothing was above LOW.

| Finding | Rule existed? | Why it came back |
|---|---|---|
| No `NEWS.md` heading for 1.1.2 (LOW) | Yes: §1, `check_news`, and an OncoPath audit test | **RECURRED, WARN-SKIMMED.** Three instruments flagged it (the gate's WARN, the module's own test and the red "R validation" run on the release commit); none can stop `release.yaml`, which runs on push, does not wait for validation, and falls back to a placeholder. Swept: meddecide 1.1.2 and jsurvival 1.1.1 shipped the same placeholder, both as *Latest*. |
| Failing-test output and personal config committed; two bad tests (LOW) | Conventions forbade breadcrumbs in user-facing files; no check | **NEW + OUR-FIX.** Our round-5 remediation wrote the README/DESCRIPTION breadcrumb test and the `NEWS.md` breadcrumb. `.claude/settings.local.json` was tracked in all five siblings, not one. Now §14 and `check_artifacts` / `check_review_breadcrumbs`. |
| All-caps notice titles, mixed spellings, list casing (LOW) | §12 covered controls and CollapseBoxes, not notices or `List` titles | **NEW, NO-RULE.** Swept: caps in 3 of 5 modules, mixed spelling in all five. Now §12 and `check_label_case`; spelling waits for a project decision. |
| Notices hand-styled two ways (LOW) | §13 | **NO-CHANNEL worked; WRONG-DONE content again.** The reviewer read the breadcrumb and answered it. Our reason ("accumulates across runs") was never run, and an experiment shows the opposite (§13.6). The other reason (no title in a Notice) is accepted. |
| `diagnosticmeta` panel mostly ungrouped (LOW) | §12 said "collapsed `CollapseBox` groups", nothing about where | **NEW, NO-CHECK.** Now `check_collapsebox_nested`; it finds exactly the reviewer's one site. |
| `waterfall` description points to an unshipped analysis (LOW) | §27 covered agreement between files, not promises | **NEW.** Now §27 and `check_forward_refs`, which also finds `crosstable`. |
| Long functions, the largest grew (LOW) | Known | DEFERRED, standing decision. The reviewer's next concrete step: lift `diagnosticmeta` `.run()`'s input cleaning (about lines 456–613) into `.prepareMetaData()`. |

**What to do differently (round 7):**

1. **A WARN is only read if someone runs the gate before the bump.** `check_news` was right twice
   and ignored twice; the release workflow publishes on push and never sees it. Run
   `release_gate.py --root <sibling>` before changing `Version:`, and treat a `NEWS.md` WARN there
   as blocking.
2. **A retraction is new code; measure it as such.** Round 5 replaced one unmeasured mechanism
   with another in the same paragraph. Any sentence of the form "X happens because Y" in a response
   or breadcrumb needs a run behind it, or the word *inferred*.
3. **Tests written to close a finding are code under review too.** Two of the three failing tests
   asserted bookkeeping and wording, not behaviour.
4. **Sweep the reviewer's finding across all five siblings before answering.** The reviewer saw
   one tracked `settings.local.json` and one placeholder release; there were five and three.

### Round 8 (2026-10-23 meddecide, release 1.1.2)

Another "on its way" email, so everything below shipped. Every item from 2026-07-13 held. The new
fact is scope: this was the first round to read the options panels and the results as a user
meets them, and `release_gate.py --root ../meddecide` on the audited build (073714f) said
**0 blocking, 1 advisory** — none of the 20 findings was visible to any detector.

| Finding | Rule existed? | Recurrence · why |
|---|---|---|
| Four tables drawn as HTML (HIGH) | §26 (round 5) | **INSTANCE, NO-CHECK.** Round 5 counted 47 meddecide blocks and specified a detector; nothing was built and only OncoPath's two sites were fixed. Now `check_html_tables`. |
| Hidden/blanked on failure (MEDIUM) | §5 (round 2) | **RECURRED, NO-CHECK** (a grep judged by eye). Three new shapes; the reviewer's mechanism is half wrong (§5 Round 8). Now `check_error_presentation`, blind to hide-until-filled. |
| Rows in `.run()` (MEDIUM) | §6 (round 1) | **RECURRED, fourth meddecide round.** The check is blind to computed keys and said 0 for 8 tables. Runtime test only (§6). |
| Variable boxes buried (MEDIUM) | §12 said "variables first" | **NEW (scope).** `check_variables_first`. |
| Level typed as text (MEDIUM) | none | **NEW.** `check_level_as_string`. |
| Number lists as strings (MEDIUM) | none | **NEW.** `check_number_list_strings` (open-ended lists waived by comment). |
| Presets override visible controls (MEDIUM) | none | **NEW**; our own comment described the defect and we answered it with a notice. No detector. |
| Hand-built notices (LOW) | §13 | **NO-CHANNEL + INSTANCE.** The retracted serialisation reason survives in 53 comments across four siblings (`check_notice_myths`). Native Notice still waits on the feature request drafted in round 7. |
| Guidance above results (LOW) | §25 | **NEW sub-shape** (help outside the empty state). `check_default_guidance`. |
| Raw rows in image state (LOW) | §17 | **NEW sub-shape, detector blind** (name-based). Runtime size check (§17). |
| No `clearWith` on display toggles (LOW) | R4 MD, DONE | **Reviewer error.** Every table named — `decision` `cTable`/`nTable`/`ratioTable`, `lassologistic` ×3, `agreement` `irrtable`/`krippTable` — has a `clearWith` placed after `columns:` that excludes the toggles; in the parsed YAML 0 of 53 `agreement` tables lack one, not 49 of 62. The blink, if real, comes from `.run()` rows (§6). Read the parsed tree before accepting a "missing key" finding. |
| Failed sub-analyses as footnotes (LOW) | none (our own TODO named it) | **NEW.** `check_setnote_errors`: 102 in `agreement`, where the reviewer's line grep saw about 30 — the calls span lines. |
| Free-text fields echoed (LOW) | none | **NEW**, plus a missing `enable:` in `decision`. No detector. |
| Panel grouping (LOW) | §12 (round 7) | **RECURRED class, new modules.** `check_uyaml_structure` covers the mechanical part (duplicated ComboBox `options:`, childless Title Case Labels). |
| `agreement` controls and 15 empty headings (LOW) | §11 (round 2 named two) | **INSTANCE.** `check_empty_preformatted`. |
| `plotTheme` over `ggtheme` (LOW) | §23 | **NEW sub-shape.** `check_plot_theme_option`. |
| Labels, casing, spelling (LOW) | §12 (round 7) | **RECURRED** for case/synonyms; the British spellings the reviewer read were swept the next day (a71b5c0). `check_label_notation` covers R notation, "Explain…", bare eponyms. |
| Numbers as text (LOW) | §6 "Related" | **RECURRED.** `check_text_number_cells`. |
| `style: list` (INFO) | none | Optional polish; no rule. |
| Six TODO markers (INFO) | standing | DEFERRED except `enhancedROC.b.R:2344`: a predictor named like `<LOD` displays blank in 18 tables, because table cells are HTML-parsed (memory `reference_table_cells_titles_are_html_parsed`). That one is a bug. |

**What to do differently (round 8):**

1. **The gate only knows the questions we were asked.** A clean gate on a module the reviewer then
   reads in a new way is expected, not reassuring. When a round opens a new dimension (panels,
   results), add a detector per class the same day, reconciled with the reviewer's list.
2. **A specified detector is not a detector.** §26 had a measured spec for six weeks and the class
   came back as a HIGH.
3. **The reviewer also makes reading errors.** The `clearWith` finding is wrong at every named site,
   and the `setContent()` half of the hidden-results mechanism contradicts jmvcore's source. Check
   the parsed YAML and the jmvcore source before accepting a mechanism — and say so in the code.
4. **A comment is the position the reviewer reads.** Correcting `CLAUDE.md` did not correct 53
   copies of the old reason.

---

### External learning (2026-10-03 jYS; remediation account 2026-10-10)

The jYS audit passed schema-name consistency while finding incomplete `clearWith` dependencies,
wrong metric encodings and shifted saved predictions. The subsequent account reports an export
repair despite the audit's positive static rendering assessment. These are different checks:
schema validity, arithmetic, row identity, cache transitions and desktop lifecycle. None proves
the others. See the [external learning guide](jamovi_external_audit_learning_guide.md) for all
35 findings, concrete test recipes, existing detector limits and remedies not adopted verbatim.

## 19. Rule: a bare symbol must be importable from the submodule's own namespace

### Why

During development the umbrella package has everything attached, and
`devtools::load_all()` puts the entire search path within reach. A shipped
submodule has neither. Inside jamovi its code resolves a name from only four
places:

1. its own `R/` definitions,
2. packages attached in every R session (`base`, `stats`, `utils`, `graphics`,
   `grDevices`, `methods`, `datasets`),
3. names its `NAMESPACE` imports through `import(pkg)` or `importFrom(pkg, name)`,
4. explicit `pkg::fn()` calls.

The 2026-09-16 OncoPath audit found `%>%` used 114 times with `magrittr` absent
from `Imports:` and no `importFrom` anywhere. `waterfall` could not run at all —
`.processData()` is on every code path — and `swimmerplot` lost its person-time,
milestone and event-marker tables. Every local check passed, because
`load_all()` and any interactive `library(dplyr)` hide the failure, and
`R CMD check` reports only a NOTE:

```
no visible global function definition for '%>%'
```

A NOTE does not fail a build. The module installed cleanly and broke in users'
hands.

### Proof, not reasoning

```r
f <- function(d) d %>% nrow()
environment(f) <- new.env(parent = baseenv())   # nothing but base in scope
f(data.frame(a = 1:3))
#> Error: could not find function "%>%"
```

Run the same experiment with `:=` inside `dplyr::mutate()` and it **succeeds**:
tidy-eval quotes `:=` and never looks it up. So `:=` costs an `R CMD check`
NOTE, not a failed analysis — which is why the guard allows it by name and flags
`%>%`.

### The rule

> Every symbol called as a function, and every infix operator, must be defined in
> the module's own `R/` or imported by name in its `NAMESPACE`. `Imports:` alone
> is not enough: a package listed there with no `importFrom()` puts nothing in
> scope.

### Where the import belongs

Each submodule owns a hand-maintained `R/zzz_imports.R` carrying roxygen tags for
what `R CMD check` cannot see inside R6 method bodies. That is the natural home:

```r
#' @importFrom magrittr %>%
NULL
```

...together with the package in `Imports:`. For a generated submodule, also check
`_updateModules_config.yaml`:

- a `prune_imports` entry strips the package from the generated `DESCRIPTION`;
- `r_symbol_files` copies **named symbols** out of an umbrella helper file, so a
  roxygen-only re-export block (`#' @importFrom magrittr %>%` above a bare
  `NULL`) has no symbol to select and never travels to the submodule.

Both were true of OncoPath at once.

### Why the dependency guard missed it

`_updateModules_test_dependency_guard.R` matched `pkg::` calls and
`library()`/`require()` against `Imports`/`Depends`. A bare infix operator is
neither shape, so it was invisible. The guard now also walks every call head and
infix operator and requires each to resolve from the module's own definitions,
the always-attached packages, or a `NAMESPACE` import — expanding `import(pkg)`
through `getNamespaceExports()`. Failures name `symbol (file:line, N uses)`.

Function formals count as definitions; without that, every parameter called as
`fun(...)` is a false positive.

### Checking a submodule yourself

An umbrella-side check answers an umbrella-side question. For anything about a
submodule, test against that module's own tree:

```r
# does every called symbol resolve from this module's namespace?
usage <- .dependency_guard_symbol_use("<module>/R")
resolution <- .dependency_guard_importable("<module>/NAMESPACE", usage$defined)
setdiff(names(usage$used),
        c(resolution$importable, .dependency_guard_language_symbols()))
```

The authoritative check is `R CMD check` on the **built** submodule, reading the
NOTEs rather than only the errors. The quick equivalent installs the submodule
into a temporary library and resolves every function called inside every R6
method from the *installed* namespace:

```sh
Rscript --vanilla tools/submodule_smoke.R /Users/serdarbalci/Documents/GitHub/OncoPath
#> UNRESOLVED %>% in 13 function(s): swimmerplotClass (3), waterfallClass (10)   (pre-fix 1.0.81)
```

It also reports packages `NAMESPACE` imports but `DESCRIPTION` does not declare,
and install/load failures such as a stale `Collate:` field.

**Always `--vanilla`.** `~/.Rprofile` attaches magrittr in every directory that has
no `.Rprofile` of its own — every sibling repo. The umbrella's own (commented-out)
`.Rprofile` shadows it, so the same plain `Rscript` command passes in a sibling and
fails in the umbrella. Run the smoke check without `--vanilla` from `/tmp` and the
pre-fix OncoPath reports `PASS`; the script now refuses a session with anything
extra attached.

---

## References

- Audit reports: `jamovi-library-audit/*.md`
- `vignettes/jamovi_module_patterns_guide.md` — the primary development guide
- `vignettes/jamovi_notices_guide.md` — notices, HTML output, theme safety
- `vignettes/jamovi_plots_guide.md` — plots and `image$state`
- `vignettes/jamovi_tables_guide.md` — table structure and `.init()`
- `vignettes/jamovi_i18n_guide.md` — translatable strings
- `vignettes/jamovi_r_yaml_guide.md` — valid result element types
- `tools/theme_safe_html.py` — the theme-safety transform
- `R/waterfall.b.R` — reference Notice → Html conversion (`.addNotice()` / `.renderNotices()`)
- jamovi 28.3 features (anticipatory):
  [install jmvtools/jmvcore](jamovi_module_patterns_guide.md#installing-current-jmvtools-and-jmvcore),
  [minApp gating](jamovi_module_patterns_guide.md#version-gating-minapp),
  [`File` option](jamovi_a_yaml_guide.md#file-jamovi-283),
  [reading it safely](jamovi_b_R_guide.md#reading-a-file-option-jamovi-283),
  [`Text` result](jamovi_r_yaml_guide.md#text-jamovi-283),
  [`mode: vector`](jamovi_plots_guide.md#rendering-mode-raster-vs-vector-jamovi-283),
  [renderer table](jamovi_notices_guide.md#which-text-renderer-notice-setnote-html-or-text)

---

## 20. Rule: a displayed statistic is computed, never defaulted

### Why

The 2026-09-16 CompositeSEM audit opened with this:

> When bootstrapping is off, `cSEM::summarize()` reports `Std_err` as `NA` for
> every path, so both `se1` and `se3` silently fall back to the literal constant
> `0.1`. […] A user who enables moderation without bootstrapping gets a table full
> of plausible-looking significance tests that have no basis in the data.

That is the worst failure a statistics module can have, because it is invisible.
A crash gets reported. A wrong number gets published. Nothing on the screen
distinguishes a standard error the model produced from one someone typed, and the
user has no way to find out.

We had twelve of them. All in `D`/`P` menuGroups, so none reached a user — but
the containment was a naming convention, nothing more:

```r
# R/hierarchicalbayes.b.R:526
corr_se <- 0.15  # Simplified standard error
# ... which then drives credible_lower, credible_upper, and
#     prob_positive <- ifelse(correlation > 0, 0.8, 0.2)

# R/treatmentoptim.b.R:396-397
response <- base_response + rnorm(1, 0, 0.05)   # the POINT ESTIMATE is random
se       <- 0.08                                 # and so is its interval
```

The sharpest form hides in a string, where no numeric check can see it:

```r
# R/treatmentoptim.b.R:494 - a p-value with no test behind it
statistical_difference = "p = 0.032 (significant)",
# R/imagingcorrelation.b.R:1174
description = "Moderate correlation between enhancement pattern and histologic grade (r=0.58, p=0.003)"
```

A *threshold* written into a label is fine and is not this — a plot subtitle reading
"Dashed lines at p = 0.05", or a `decision_criterion` of "HR < 0.8 with p < 0.025",
describes the design rather than reporting a result.

### The rule

A number a user can see is either computed from their data or it is absent.
There is no third option.

When the quantity genuinely is not available — a model class that does not report
an SE, a method that needs bootstrapping the user turned off:

1. Leave the cell `NULL`. `addRow()`/`setRow()` render an empty cell, which reads
   correctly as "not available".
2. Say why, once, in a `setNote()` on that table or a `jmvcore::Notice`:
   *"Confidence intervals require bootstrapping; enable it under Estimation."*
3. If a whole column is only meaningful under some option, gate it:
   `visible: (useBootstrap)` in the `.r.yaml`.

Never `else 0.1`. Never `# Placeholder` feeding `qnorm()`. If a draft analysis
needs stand-in numbers to develop against, that is what the `D` menuGroup is for
— and say so at the top of the file the way `R/populationhealth.b.R:30-55` does,
which inventories its own fabrication and ends `DO NOT promote to production menu`.

### Enforce it

```bash
python3 tools/release_gate.py     # check_fabricated_stats
```

It flags a literal assigned to an `se`/`sd`/`std_err`/`variance`/`sigma` name, and
placeholder comments, in any method that also writes to a result. Shipped hits
FAIL. Unshipped hits are counted as promotion debt (§24) — so a `menuGroup`
rename can no longer quietly promote invented numbers.

---

## 21. Rule: column `format:` tokens are comma-separated and exact

### Why

`format: zto:4` looks like "zto, to 4 decimal places". It is one token spelled
`zto:4`, and jamovi has never heard of it — so the column renders with no `zto`
formatting at all. Nothing warns you. The `.r.yaml` compiles, the analysis runs,
and the table just looks slightly wrong in a way nobody traces back to a colon.

Both sides of jamovi split on a comma and then test exact membership — jmvcore's
`Column$initialize` does `strsplit(format, ",", fixed = TRUE)` and the client does
`I.split(",")` followed by `w.includes("zto")`. We had **519** columns declaring a
format jamovi could not parse, 447 of them in the `zto:N` family, because the
tables guide documented the tokens and never named the separator.

### The rule

Valid tokens, comma-separated, nothing else: `zto`, `pvalue`, `pc`, `log10`,
`dp:N`, `sf:N`.

```yaml
format: zto,pvalue      # correct
format: zto,dp:4        # correct
format: zto:4           # WRONG - one unknown token, zto is lost
format: zto;pvalue      # WRONG - one unknown token, both are lost
format: zto,p:.3        # WRONG - zto survives, the p-value format does not
```

The full grammar, the runnable proof and the table of wrong-to-right rewrites are
in `vignettes/jamovi_tables_guide.md` → *The token grammar*.

Prefer no `dp:` at all where you can. Decimals come from the number-format
preference the user sets once in jamovi and expects every module to honour; `dp:N`
takes that column out of their control.

### Enforce it

```bash
python3 tools/release_gate.py     # check_column_formats
```

---

## 22. Rule: a user's column name is not a regular expression

### Why

Users name columns `Age (years)`, `BMI-1`, `A+B`, `Grade 2/3`. Paste one of those
into a pattern and the regex engine reads the parentheses as a group, the `+` as a
quantifier and the `.` as a wildcard. The match then fails, or — worse — succeeds
against the wrong thing, and a label in the results is silently corrupted.

The 2026-09-16 CompositeSEM audit found it in equation lookup. We had it in
`survival`, where the level name stripped off a Cox term builds the HR table's row
label, and in the RMST table's `Group` column:

```r
# R/survival.b.R:5879 - myfactor is a user column name
level_name <- sub(paste0("^", myfactor), "", term_name)

# R/survival.b.R:1454
gsub(paste0(myfactor, "="), "", names(km_fit$strata))
```

`R/oddsratio.b.R` is the instructive one: line 2168 pastes a name into a pattern,
and line **2169** — the very next line — passes `fixed = TRUE`. The discipline was
there. It just wasn't applied one line up, which is what an automated check is for.

### The rule

When a user-supplied name goes into `grep`/`grepl`/`sub`/`gsub`/`regexpr`:

- matching a literal → `fixed = TRUE`;
- testing a prefix → `startsWith()` / `endsWith()`, which are not regex at all;
- building a model term → `jmvcore::composeTerm()`, which back-ticks for you.

Formula construction in this module is already clean — 125 files use
`composeTerm`/`composeTerms`/`composeFormula` and no raw column name reaches a
formula. Pattern matching is the gap.

Related and distinct: `composeTerm()` output is **not** a `data[[...]]` key. It
returns a backtick-quoted string, so the lookup yields `NULL`. Use the raw name to
index a data frame and the composed name only inside formula text.

---

## 23. Rule: plot colours come from jamovi's palette

### Why

A user sets a colour palette once in jamovi's preferences and expects a document's
plots to agree. The 2026-09-21 jYS audit:

> jamovi carries a plot palette and a results number format that the user sets
> once and expects everywhere. Seven analyses offer their own Brewer palette
> combobox, so a document's plots can disagree with each other and with every
> other module's.

We are further from this than jYS was: `theme$palette` and `jmvcore::colorPalette`
appear **zero** times in `R/`, while 765 render functions already receive `theme`
in their signature and 159 palette-picker options exist across the module.
`ggtheme` is honoured in 253 files, so we respect the theme and ignore only the
palette.

### The rule

`theme` is already a parameter of every render function. Use it:

```r
.plot = function(image, ggtheme, theme, ...) {
    st <- image$state
    if (is.null(st)) return(FALSE)

    pal <- if (identical(self$options$color_palette, 'jamovi'))
               jmvcore::colorPalette(n = nlevels(st$group), pal = theme$palette)
           else
               <the named palette the user picked>

    ggplot(st, aes(x, y, fill = group)) + geom_col() +
        scale_fill_manual(values = pal) + ggtheme
}
```

Offer `jamovi (follow global)` as a choice in every palette option. A named
palette a user deliberately selects is a legitimate override — the rule is that
jamovi's own palette must be *reachable*, not that custom palettes are forbidden.

Remember §17's companion trap: `ggtheme` **replaces** earlier `theme()` and
`scale_*_manual()` calls, so apply your scales and then `ggtheme`, and add any
tweaks after it.


**Round 8 (2026-10-23 meddecide [LOW]):** a module-level theme option over `ggtheme` is the same
defect one layer up — `enhancedROC`'s `plotTheme` (Classic / Modern / Clinical) made its plots
disagree with the document. Draw with the `ggtheme` jamovi passes. **Enforce it:**
`check_plot_theme_option` (WARN; a `List` option whose name contains "theme") — MD 1, OP 1,
CPD 1, JJ 5, JS 0 on 2026-10-24.

---

## 24. Where the debt actually lives: promotion, not release

§18 explained how rules decay between audit rounds. Round 4 showed a second
mechanism, and it is the bigger one.

Checking every class in this guide across all 390 analyses, sorted by whether the
analysis ships (a production `menuGroup`) or not (`D` draft / `P` pending /
`T` JamoviTest):

| Class | Shipped | Unshipped |
|---|---:|---:|
| Malformed `format:` tokens (§21) | 6 | 513 |
| Fabricated statistics (§20) | 0 | 12 |
| Action-verb / Title-Case labels (§12) | 97 | 1,479 |
| Renderer reads a `private$` cache (§3, §15) | 23 | 190 |
| Fitted object or dataset in state (§17) | 1 | 46 |
| Long loop with no `.checkpoint()` | 17 | 96 |
| `prune_imports` entry a parked analysis calls (§29) | 0 | 54 |

The shipped column is small **because the audits swept it**. The unshipped column
has never been swept, and none of it is inert: an analysis is promoted by editing
one line — `menuGroup: SurvivalD` → `menuGroup: Survival` — after which
`_updateModules.R` ships it and every defect it carries.

That is how five of the eight OncoPath findings in round 4 arose: they came out of
our own earlier remediation, travelling into the library with the code.

### The rule

**A sweep fixes the analyses that exist. A gate fixes the ones that don't yet.**
Every rule in this guide should end in a `release_gate.py` check that FAILs on a
shipped hit and *counts* an unshipped one. Those counts print as
`promotion debt` at the end of a gate run, and `tools/promotion_screen.py` folds
them into its ranking, so an analysis carrying this debt scores lower as a
promotion candidate.

Before moving any analysis out of a `D`/`P`/`T` group, run the gate and clear its
hits first. Promotion is a release.

---

## 25. Rule: an unconfigured analysis is not an error

### Why

2026-09-22 OncoPath [MEDIUM]:

> "In both analyses, opening the analysis on a loaded dataset runs `.run()` with no variables
> assigned. That run lands in this branch, so the first thing every user sees is a red
> 'ERROR: Variables required' banner next to the welcome panel. They haven't made a mistake."

This is the third round for it. 2026-07-13 raised the fatal half (*validation errors use raw
styled HTML instead of `jmvcore::reject()`*), 2026-08-18 raised the empty-state half (*"a
first-time user who hasn't picked variables yet can end up looking at error styling"*) and we
marked it DONE — by moving the banners to a dedicated `notices` channel and then re-creating
them there (`0dbd3d348`). One day before this audit we fixed `swimmerplot` alone (`48b8edb13`)
and wrote the reasoning the reviewer quoted back at us, without sweeping the class.

### The rule

Two distinct branches, two distinct mechanisms.

**Empty state.** Nothing assigned → emit **nothing** and let the instructions panel speak.
Partially assigned → **INFO** naming the boxes that are still empty, one whole translatable
sentence each. Never ERROR. Reference: `swimmerplot.b.R:2307-2338`.

```r
# WRONG - fires on open, before the user has done anything
if (!all_provided) {
    private$.addNotice("ERROR", .("Variables required"), .("Select the study identifier and ..."))
    return()
}

# RIGHT
n_set <- sum(!vapply(list(patientID, startTime, endTime), is.null, logical(1)))
if (n_set < 3L) {
    if (n_set > 0L) {                       # partial: guidance, named boxes, INFO
        still_empty <- c(if (is.null(patientID)) .("Patient ID is still empty."),
                         if (is.null(startTime)) .("Start time is still empty."))
        private$.addNotice('INFO', .("Keep going - a few variables to add"),
                           paste(still_empty, collapse = " "))
    }                                       # nothing set: no notice at all
    self$results$instructions$setContent(private$.generateInstructions())
    return()
}
```

Populate static panels (glossary, about, instructions) **before** the early return, or they ship
as empty shells.

**Fatal.** A genuine processing failure is not a banner. `jmvcore::reject()` greys the results and
gives jamovi's own analysis-level error presentation; a banner plus `return(NULL)` leaves the pane
looking like a normal, if empty, set of results. `reject()` is not `stop()` — it preserves the
message, so it is also the fix for a specific message being overwritten by an outer handler.

```r
# WRONG
private$.addNotice("ERROR", .("DATA PROCESSING ERROR"), processed$message); return(NULL)
# RIGHT
jmvcore::reject("{}", msg = processed$message)     # "{}" + msg= when the text carries user data
```

### Measured

Shipped hits when the class was first swept (2026-09-22), against the reviewer's three named sites:
**7 empty-state-as-ERROR** (OncoPath `diagnosticmeta.b.R:379`, `waterfall.b.R:1547`;
ClinicoPathDescriptives `alluvial.b.R:284`; meddecide `decisioncombine.b.R:527,:548`,
`enhancedROC.b.R:364,:727`) and **9 fatal-via-banner** — including **`swimmerplot.b.R:2392,:2410`,
which the reviewer missed** while holding swimmerplot up as the model: they examined only its
empty-state path. Debt: 34 and 13.

### Enforce it

No gate yet, and the honest reason is a measured false-positive rate: a detector keying on
"ERROR-severity emission inside a branch guarded by an unset option" runs at 53% false positives
on the shipped surface (it catches guards that are already unreachable behind a silent early
return, `type: String` options that carry a default, and data-content checks). Two refinements
drive it to 0/15 on this corpus — require the guard's option to be `Variable`/`Variables`/`Level`
in the `.a.yaml`, and suppress a guard whose options a *silent* earlier return already covers —
and it should be built with those before it is turned on. Note the known false **negative**: an
option unset into an `NA_character_` sentinel (`decision.b.R:477`) looks like a data check.


**Round 8 (2026-10-23 meddecide [LOW]): help that is not in the empty state.** `cotest` has no
empty state, and its `showGuidance` defaulted to true, so "Welcome to Co-Testing Analysis" sat
above every result; `enhancedROC`'s instructions were never hidden at all. The reviewer named six
analyses that do it right, and `decisioncurve.b.R:1376-1385` (hide once the outcome and
predictors are set) as the model. In-app help is coming to jamovi, so expect to move this content.
**Enforce it:** `check_default_guidance` (WARN) — a guidance-named `Bool` defaulting to `true`,
and an instructions-named `Html` that is always visible and never `setVisible()`'d; exactly the
two on the audited build.

---

## 26. Rule: tabular data belongs in a `Table` result

### Why

2026-09-22 OncoPath [MEDIUM], on two reference tables in the Clinical Interpretation panel:

> "as HTML they lose everything a jamovi table does. They ignore the results theme … there's no
> **Copy Latex**, and **Copy** puts markup on the clipboard instead of a grid."

And they cannot be translated: a `Table`'s column titles and row labels go through `.()` like any
other string, an HTML literal does not.

### The rule

If the rows carry values — computed or reference — declare a `Table`. Fill computed rows in
`.run()`, static rows in `.init()` (section 6), and put the explaining sentence in `setNote()`.

Exempt, and the reviewer says so explicitly: **empty-state help** — a quick-start panel, a
glossary, an illustration of the expected data format, a two-column layout scaffold. Those are
prose in a grid shape, not data.

**Second-order:** a static table beside numbers the analysis already computed is a missed
opportunity, and sometimes a defect. The reviewer's case is a PPV/NPV table hardcoded at
Sen = 90% / Spe = 80% while `private$.pooled_sensitivity` and `private$.pooled_specificity` sit in
the same object — the one table about predictive values says nothing about the user's own test.
Worse in the same class: `meddecide/R/agreement.b.R:413,:462` print linear and quadratic kappa
weights (`1.00 / 0.75 / 0.50 / 0.00`) as "the weights this analysis is applying", with no caveat.
The formula printed two lines above is `1 - |i - j| / (k - 1)`, so those numbers are correct only
at k = 5. That is [section 20](#20-rule-a-displayed-statistic-is-computed-never-defaulted) wearing
a table's clothes. Counter-example to copy: `meddecide/R/decisioncalculator.b.R:1279` renders the
same panel shape and derives every number.

### Measured

97 HTML `<table>` blocks across the shipped surface of the five modules (187 more in promotion
debt) — **not 2**. Of the shipped ones: 33 carry computed values, 25 are static numeric grids, 27
are all-prose, 12 are exempt help. Per module: meddecide 47, jsurvival 25, jjstatsplot 13,
ClinicoPathDescriptives 9, OncoPath 3.

### Enforce it

Not gated. A bare `grep '<table'` would fail all 97, 12 of which the reviewer explicitly blessed —
~40% noise, which is how a gate gets ignored. The measured pair worth building is: **interpolated
cells** (an R value spliced between `",` and `"`, or a glue `{obj$field}`) — 35 shipped hits, 33/33
precision once "at least one interpolated cell is not a bare `.()` call" is added; plus **static
numeric grid** (a header row and ≥2 threshold-shaped cells) — 16 shipped hits, catching both sites
the reviewer named. Union ≈ 51 shipped at ≈94% precision, with the exemption keyed on the
*calling method name* (`.populateInstructions`, `.populateIntro`, todo-style builders), not on the
markup. Note that the interpolation detector scores **zero** on the file the reviewer was reading —
all three `diagnosticmeta` tables are static literals.


### Round 8: the class reached a HIGH (2026-10-23 meddecide)

The round-5 sweep counted 47 shipped `<table>` blocks in meddecide and none was converted. The
reviewer took the four that are data, not help, as the module's one HIGH: `psychopdaROC`'s
confusion matrices (core output, a stylesheet forcing Arial 14 px), `decisioncalculator`'s
bordered 2×2 summary (repeating `ratioTable`), `decisioncompare`'s `kable(format = "html")`
cross-tabs beside a `table()` printed into a Preformatted, and `lassologistic`'s suitability check
with its verdict carried by red/amber/green text. The fixes they gave are all in the module
already: an Array of `Table` with the criterion levels as `superTitle`d columns renamed in
`.init()`, `decision.r.yaml`'s `rawContingency`/`rawCounts` pattern, and a `setNote()` for a
verdict in words. APA tables do not encode a verdict in colour.

**Enforce it:** `check_html_tables` (WARN) — a `<table` or `kable(format = "html")` in a method
whose **name** is not help-like (`instruction`, `intro`, `welcome`, `guide`, `help`, `about`,
`glossary`, `explan`, `tutorial`, `example`, `usecase`, `methodolog`). On the audited build: the
reviewer's four, plus `agreement::.createSummary` (a computed Raw agreement / Kappa card grid, the
same shape as `decisioncalculator`'s — reviewer missed it) and two in `agreement::.run` (a
two-column welcome layout, exempt; the computed-variables banner, raised under notices). Across the
siblings on 2026-10-24: MD 6, CPD 10, JJ 9, JS 12, OP 0. The interpolation/static-grid pair above
remains the more precise spec; the name exemption is what made a WARN readable today.

---

## 27. Rule: the module's own prose must agree with itself

### Why

2026-09-22 OncoPath [LOW]: four ways a module contradicts itself in the files a prospective user
reads first. `DESCRIPTION`, `README.md` and `jamovi/0000.yaml` are read by different people
through different doors, and only `0000.yaml` is generated from the umbrella — so a correction
made where we author does not reach where the user reads.

### The rule

**Capability claims.** If one file qualifies a named clinical standard, every file must.
OncoPath's `0000.yaml` says the waterfall thresholds are *"adapted from RECIST v1.1, but this is
NOT a RECIST v1.1 implementation"* and explains why; `DESCRIPTION:13` advertises *"RECIST …
criteria analysis"* and `README.md:59` promises *"Built-in … RECIST guidelines"* — 6 claim sites
against 1 disclaimer. For a clinical audience that is a claim they will act on. Worse, and found
in the same sweep: `jsurvival/README.md` advertises **five analyses the module does not contain**
(stage migration, alluvial, subgroup forest, time-dependent ROC, IDI — all of them `D`/`P` in the
umbrella or shipped elsewhere). Not gate-able: judging whether a claim is qualified needs a
reader. It belongs on the release checklist.

**One bug-report URL.** OncoPath sent `DESCRIPTION`'s readers to `sbalci/OncoPath/issues` and the
jamovi library's readers to `sbalci/ClinicoPathJamoviModule/issues`. Four of the six trees
disagreed with themselves. Watch for "Discussions" links that point at `/issues`.

**Description block scalars.** `description: main:` must be **one `>` folded paragraph with no
blank line**. A `|` literal block keeps every newline, and jamovi renders each as a paragraph
break; jmvtools additionally truncates at the first blank line and silently drops the rest.
Measured: `jjdotchart` lost 25 of its 28 lines, `crosstable` 11 of 12, `sequentialtests` 10 of 14 —
none of it visible except in the library listing. Test the **resolved value**, not the block style:
a `>` block containing a blank line folds to a newline too, and a single-line `|` is harmless.
`options[].description.R` is out of scope — it reaches `man/*.Rd`, where newlines do not matter.

**Dataset descriptions.** Two example datasets that describe themselves identically cannot be told
apart in the data picker. OncoPath's `waterfall_percentage_basic.omv` and
`waterfall_raw_longitudinal.omv` — the two input modes the waterfall description itself
distinguishes — both read *"Example analysis for Waterfall Plot."*

**No pointer to what has not shipped.** 2026-10-23 OncoPath [LOW]: the `waterfall` description in
`0000.yaml`, `waterfall.a.yaml` and the in-analysis guide (`R/waterfall.b.R`) ended *"use the
lesion-level RECIST v1.1 analysis. It will be available in upcoming releases."* The library listing
shows that text, and the user who follows it finds an empty menu. End the sentence at what the user
can do now ("sum the target lesions per visit first"), and add the pointer when the analysis ships.
The same sweep found `ClinicoPathDescriptives` `crosstable.a.yaml`: *"… are planned but not yet
available."*

### Enforce it

`check_forward_refs` (WARN; OncoPath 3, ClinicoPathDescriptives 1 when added; 0 in all five siblings since
2026-10-24. Promote to FAIL once the umbrella's generated `0000.yaml` is regenerated).
`tools/release_gate.py`: `check_description_newlines` (23 shipped / 139 debt when added — WARN
until the sweep clears it, then FAIL), `check_issue_urls` (4 of 6 trees red when added),
`check_dataset_descriptions` (1 hit; also FAILs a `datasets:` entry whose `.omv` is missing).
The capability-claim half has no detector and says so above.

---

## 28. Rule: seeding the RNG must not outlive the function that did it

### Why

Every analysis in a jamovi session runs in **one** R process. A bare `set.seed(42)` therefore
does not seed *your* bootstrap — it seeds the engine. The next analysis the user adds, which
resamples without setting a seed of its own because it has no reason to, now draws from a
deterministic stream and returns the same "random" bootstrap CI on every run, for the rest of
the session. Nothing errors and nothing looks wrong; the numbers are simply no longer random.

The 2026-09-16 meddecide audit found the same module using three idioms at once —
`withr::local_seed()` in `nogoldstandard` and `psychopdaROC`, a hand-rolled `.Random.seed`
save/restore in `enhancedROC` and `decisioncurve`, and six bare `set.seed()` calls in
`agreement` and `lassologistic`. Only the third shape leaks.

### The rule

Seed **scoped to the function**, never globally.

```r
# WRONG - the engine keeps this seed after the function returns
.bootstrapCI = function(x) {
    set.seed(self$options$random_seed)
    replicate(1000, mean(sample(x, replace = TRUE)))
}

# RIGHT - withr restores the previous RNG state on exit
.bootstrapCI = function(x) {
    withr::local_seed(self$options$random_seed)
    replicate(1000, mean(sample(x, replace = TRUE)))
}
```

`withr::local_seed()` is the house idiom. `withr::with_seed(seed, expr)` and an explicit
`.Random.seed` save/restore pair are equally correct and are not flagged; three idioms in one
module is a readability problem, not a defect.

The scope must begin **before the first random operation**, including feature selection,
preprocessing, fold allocation and threshold-CI bootstraps. A seed before the final fit can be
too late (external jYS mLR audit, 2026-10-03). Verify repeatability and restoration on normal/error
exits, with and without an existing `.Random.seed`. `check_bare_set_seed` does not prove timing.

**Say which seed produced the numbers.** Whenever the result the user reads depends on the
seed — bootstrap CIs, permutation p-values, MCMC — expose the seed as an option with a default
and print it beside the result, so the run can be reproduced. Which `statsExpressions` results
are seed-dependent is tabulated in the memory note `reference_statsexpressions_seed_dependent_results`.

### Enforce it

`tools/release_gate.py` → `check_bare_set_seed` (WARN). It splits each file into functions,
blanks string literals and comments first (so generated R code in a `.asSource()` template is
not a hit), and flags a function that calls `set.seed()` without also mentioning `.Random.seed`
or `preserve_seed`. Shipped count across all five modules on 2026-09-23: **0**.

---


## 29. Rule: nothing may be derived from which analyses a module holds today

### Why

A module's membership is not a property of the module. It is one line per analysis —
`menuGroup:` in `jamovi/<fn>.a.yaml` — and a `T` / `P` / `D` suffix moves an analysis out
of the production module for as long as someone is working on it. `_updateModules.R` then
deletes that analysis's files from the generated sibling. Membership is a *verb*.

Everything derived from a snapshot of that membership silently becomes a claim about a
moment rather than about the module. Three instances, all of them measured:

1. **The dependency list.** While `agreement` sat at `menuGroup: meddecideT` (2026-09-23),
   six packages it alone used — `DescTools`, `irrCAC`, `lme4`, `lmerTest`, `psych`, `vcd` —
   had no caller left in meddecide. They were added to `prune_imports` in
   `_updateModules_config.yaml` and their `@importFrom` tags deleted from the sibling's
   hand-maintained `R/zzz_imports.R`. Every signal agreed: the gate read
   `Imports: 31 checked, 6 unused`, and the module built and installed cleanly.
   `prune_imports` is a standing DELETE order — the only way to remove an Import — so this
   wrote a temporary state into permanent config. Restoring `menuGroup: meddecide` made
   `prune_conflicts()` raise a plan error naming five of the six. Without that guard
   meddecide would have shipped `agreement` installed but unable to run: the exact shape of
   the 2026-09-16 OncoPath CRITICAL, where pruning `magrittr` left `waterfall` unrunnable.

2. **The audit test's analysis list.** `tests/testthat/test-meddecide-library-audit.R`
   reads `R/<name>.b.R` for a hard-coded list of 15 analyses. With `agreement` parked, its
   files were absent from the sibling and seven tests ERRORED with `cannot open the
   connection`. Nothing in that output said "this analysis is not in this module" — a
   clean module read as eight failures. Bringing `agreement` back took it to zero with no
   change to any test.

3. **A sibling's `importFrom` tags** — §19, already documented: `swimmerplot` got
   jmvcore's `.()` from `waterfall.b.R`, so suffixing `waterfall`'s `menuGroup` with `T`
   took the only tag out of OncoPath.

### The rule

Derive membership; never record it. When you must record it, record *why*, and make the
recording fail loudly when the reason expires.

Concretely, before adding a `prune_imports` entry:

```sh
# Is any analysis of this module parked right now? If yes, the Imports you see are a lie.
grep -l 'menuGroup: <module>[TPD]' jamovi/*.a.yaml
```

WRONG — derived from what ships today, with no record that it was a snapshot:

```yaml
    prune_imports:
      - "DescTools"   # nothing in meddecide calls it
      - "vcd"
```

RIGHT — the entry names the condition under which it stops being true, or it is not made
at all:

```yaml
    prune_imports:
      # `psych` stays: every psych:: in agreement.b.R is a comment or a user-facing string
      # saying the result agrees with psych::cohen.kappa -- not one call. prune_conflicts()
      # PARSES, so comments are not uses; never re-implement it as a grep.
      - "psych"
```

For a test, derive the list instead of writing it down:

```r
# WRONG - a claim about a moment
analyses <- c("agreement", "cotest", "decision", ...)

# RIGHT - a claim about the tree under test
analyses <- sub("[.]a[.]yaml$", "",
                basename(list.files(file.path(root, "jamovi"), pattern = "[.]a[.]yaml$")))
```

and when a specific analysis genuinely must be present, say so by name:

```r
skip_if_not(file.exists(f), "agreement does not ship in this module")
```

A skip naming the analysis is diagnostic. `cannot open the connection` is not.

### A mention is not a use

Deciding "unused" is the load-bearing step, and grep cannot do it. Measured on
`R/agreement.b.R` against R's own parser (`SYMBOL_PACKAGE` tokens), 2026-09-23:

| package | raw grep | line-at-a-time masker | whole-file masker | R parser |
|---|---:|---:|---:|---:|
| `psych` | 7 | 1 | **0** | **0** |
| `vcd` | 18 | 4 | **3** | **3** |
| `DescTools` | 3 | 1 | **1** | **1** |

All seven `psych::` occurrences are comments or user-facing strings. A line-at-a-time
masker still reports one, because `agreement.b.R:7616`
(`<code>psych::cohen.kappa()</code>`) sits inside a multi-line HTML string whose own line
carries no quote character. That single false positive is the whole difference between
"psych is prunable" (true) and "psych is used" (false). Strip strings and comments across
the whole file, or use the parser.

### Enforce it

- `prune_conflicts()` (`_updateModules_utils.R:612`, called from `_updateModules_plan.R`)
  already refuses at **plan** time when a prune entry collides with a file the module will
  ship, so nothing broken reaches disk. It parses, and it deliberately counts only shipped
  files — `tests/testthat/test-update-modules-plan.R:396` and `:410` pin both behaviours.
  That guard fires on the way *back*, after the promotion work is already done.
- `tools/promotion_screen.py` now counts the collision *in advance*, as a promotion
  blocker weighted like any other (§24). Run it before promoting anything:

  ```sh
  python3 tools/promotion_screen.py --no-git   # ends with the prune_imports collision list
  ```

  Measured 2026-09-23: **0 shipped**, **54 parked analyses** across meddecide (29),
  OncoPath (17), jsurvival (4) and ClinicoPathDescriptives (3). Every one of those is a
  plan error waiting for whoever removes its suffix.
- The `learn` sweep that produced this section found the 54 only after the detector was
  widened to `(?:Extra)?[DPT]+$`: a hand-written sweep matching `[TPD]$` saw 30 and missed
  every `...ExtraD` group. Reconcile a detector against a hand list before trusting either.

---
## 30. The official process: requirements, submission, updates

Sections 2–29 come from the reviewer. This one comes from jamovi's own published process. The submission
form (added upstream 2026-09-30, linked from the tutorial 2026-10-07) replaced the old
"email contact@jamovi.org" route:
[Distributing Modules](https://dev.jamovi.org/tutorial/tuts0111-distributing-modules),
[Module Datasets](https://dev.jamovi.org/tutorial/tuts0110-module-datasets) and
[Submit a Module](https://dev.jamovi.org/submit). Copies are vendored (2026-10-08) in
`development-documentations-dev.jamovi.org-master/` as `tuts0111-distributing-modules.md`,
`tuts0110-module-datasets.md` and `submit.md`. The "here" column was measured on 2026-10-08
across the five submodules.

### A `.jmo` is not a release

`jmvtools::install()` writes `<Module>.jmo`, and a tester can side-load it: jamovi library
(the plus icon) → **Side-load** tab. But a `.jmo` runs only on the OS, the CPU architecture
**and** the jamovi series (solid or current) it was built on. That gives eight setups, because
R packages with compiled C/Fortran code are built per R version, OS and architecture. A `.jmo`
built on this Apple Silicon Mac does nothing for a Windows pathologist. Side-load only to
testers whose setup matches yours. Only a library listing reaches everyone, because jamovi
then builds the module for every platform and every future jamovi version.

### Step 1: the library requirements

| Requirement (official wording, condensed) | Here (2026-10-08) | Check |
|---|---|---|
| **Name** distinctive, no dot (jamovi fails to load it), does not embed "jamovi"; renaming later touches `0000.yaml`, `DESCRIPTION` and file names | ClinicoPathDescriptives, jjstatsplot, jsurvival, meddecide, OncoPath: all pass | `grep '^name:' jamovi/0000.yaml` |
| **Documentation**: clear analysis titles and descriptions | §12, §27 | `release_gate.py` |
| **Example data** bundled as module datasets, plus usage notes in `README.md` or the form's comments box | 17 datasets, all `.omv`, all tagged | see "Datasets" below |
| **Stability** with missing data, special characters in variable names, and empty datasets (via input checks) | §22, §25 | testthat |
| **Build**: `jmvtools::install()` runs without errors | checklist item 12 runs `prepare()`; `install()` is the official bar | `jmvtools::install()` |
| **Metadata**: accurate `DESCRIPTION` summary and authors | §27 | — |
| **Clean repository**: no `build/`, `dist/` or `*.jmo` in git; `.gitignore` covers them | nothing committed in any module; all five `.gitignore` list `build/` and `*.jmo`, **none lists `dist/`** | `check_artifacts` FAILs a tracked *or unignored* file under `build/`/`dist/` |
| **Licence**: OSI-approved, declared in `DESCRIPTION`; **AGPL-3 not accepted**; GPL2+ recommended; shipping a GPL package makes a combined work that must be GPL | all five `GPL (>= 2)` | `check_license` (below) |
| **English** at minimum | English is the source language | — |

**Keep the "or later".** `usethis::use_gpl_license(version = 2)` (2024-11-02, every repo) writes
`GPL (>= 2)`, not GPL-2-only, and the `(>= 2)` is what makes the dependencies fit. Each submodule
ships packages under GPL-3, LGPL-3 or Apache-2.0 (`pROC`, `insight`, `timechange`, `RColorBrewer`, …;
18–38 per module, 4 in OncoPath, measured 2026-10-08). None of those combines with GPL-2-only, but all
of them combine with `GPL (>= 2)` taken as GPL-3. Its SPDX form, in `CITATION.cff` and `codemeta.json`,
is `GPL-2.0-or-later`. Bare `GPL-2.0` is the deprecated id for *only* (all six aligned 2026-10-08).

**The licence trap is in the dependencies, not in `DESCRIPTION`.** jamovi's own advice is to
browse the packages in the module's `build/` folder for copyleft licences. On 2026-10-08 the
newest builds held two AGPL-3 packages, both pulled in transitively:

| Module | AGPL-3 package | Pulled in by |
|---|---|---|
| meddecide | `pander` | `epiR` |
| ClinicoPathDescriptives | `RhpcBLASctl` | `bigutilsr` |

The page rules out AGPL-3 as the module's licence and says a GPL dependency makes the whole
a GPL combined work. By the same logic an AGPL-3 dependency makes an AGPL-3 combined work,
but the page does not say so. Both modules are already listed, and no reviewer has raised it.
So this is a question to put to the jamovi team, not a verdict. `check_license` WARNs on it
and FAILs a module whose own licence is AGPL. To trace an AGPL package to the import that
brings it in, resolve
`tools::package_dependencies(<Imports>, recursive = TRUE)` against `installed.packages(fields = "License")`.

### Datasets: the `0000.yaml` contract

- Files sit in `data/` at the module root, as `.omv` (recommended: it keeps variable types,
  descriptions and analysis state) or `.csv`.
- A top-level `datasets:` block has one entry per file. `name` (a **unique** identifier),
  `path` (a filename inside `data/`) and `description` are **required**; `tags` is optional
  and groups datasets in the library.
- Datasets must be **small**, for demonstration only, and **public**: everyone who installs
  the module downloads them. They must hold no sensitive, confidential or personally
  identifiable information. In a pathology project this is the rule with teeth: ship only
  synthetic data or public de-identified data, never an export from a real cohort. The 17
  shipped files were not audited for this on 2026-10-08; the largest is 1.9 MB (jjstatsplot).
- Verify in the app: `jmvtools::install()`, then ☰ → **Open** → **Data Library** → the module.
- Here, `document_module_omv()` (`_updateModules_utils.R`) appends an entry for every
  undocumented `.omv`, and builds `name` and `description` from the file stem. Two files that
  share a stem (the `waterfall_*` pair, §27) therefore get one `name`. `check_dataset_descriptions`
  FAILs a missing required field, a repeated `name`, a format other than `.omv`/`.csv`, or a
  missing file, and WARNs on repeated descriptions. Its rules are pinned by
  `python3 tools/test_release_gate.py`.

### Step 2: submitting

- Use the form at <https://dev.jamovi.org/submit>. It asks for the GitHub repository URL
  (required), a subdirectory (optional; each of our modules sits at its repository root, so
  leave it empty), and comments (optional, up to 5000 characters). Use the comments to
  introduce the module and say how to use its example data.
- jamovi emails the **`maintainer:` in `jamovi/0000.yaml`** to confirm the submission, and
  emails the outcome there too. All five modules name
  `Serdar Balci <serdarbalci@serdarbalci.com>`; that inbox has to be read, or the submission
  stalls unconfirmed.
- Review takes 1–2 weeks, by the jamovi team or a community member. Feedback arrives as an
  email thread or as a GitHub issue on the repository.

### Step 3: updating a listed module (the path this project uses)

1. Increment the version in **both** `jamovi/0000.yaml` and `DESCRIPTION`. jamovi compares
   these to tell users an update exists.
2. Push to GitHub.
3. **Tell the jamovi team** by email or in the jamovi Slack. A push alone publishes nothing.
4. The team publishes, usually once a week, on Mondays. Plan hotfixes around that: a fix pushed
   and announced on a Tuesday can reach users six days later.

Two notes for this project:

- `_updateModules.R` already stamps one version into `DESCRIPTION`, `CITATION.cff`, `0000.yaml`
  and every `.a.yaml` (§1), so step 1 is done on every regeneration.
- `.github/workflows/release.yaml` runs on push, independently of "R validation", and writes the
  placeholder `Release <v>.` when no `NEWS.md` heading matches. 1.1.2 was tagged while validation
  was red on the same commit (2026-10-23). A published body can still be replaced with
  `gh release edit v<version> --notes-file <file>`; the tag cannot be re-cut.
- The page asks for [Semantic Versioning](https://semver.org/), e.g. `1.0.1` → `1.0.2`. Our
  versions are four components and zero-padded (`1.0.83.06`). SemVer allows three components
  and no leading zeros, and §1 already notes that the release workflow publishes nothing for a
  four-component version. Nobody has measured how jamovi orders such versions. Changing the
  scheme touches the updater and all five modules, so that decision is open, not settled here.

---

## 31. Learning from external audits

The user-supplied jYS audit of 2026-10-03 and modification report of 2026-10-10 cover jYS 1.1.5.
The audit has 35 headings (1 HIGH, 14 MEDIUM, 18 LOW, 2 INFO); the modification report addresses
the first 15 plus a plot export/restoration issue. Its 186 reported passing tests were not run
here. Neither document establishes the presence or absence of a defect in our modules.

The [external audit learning guide](jamovi_external_audit_learning_guide.md) contains source
fingerprints, a classification of every finding, detector limits and concrete regression recipes.
Use its checks when the corresponding surface is present:

- A → B cache transitions compared with fresh B, including nested images and Output columns.
- Exact original-row-ID/value pairs after filtering, missingness and reordering.
- Class-label encodings, probability meaning, Brier versus accuracy, and training/CV/OOB provenance.
- Scoped seeding before selection; preprocessing confined to training folds.
- Survival origin, start-stop compatibility, subject-level standardisation and variance labels.
- Shared UI controls, non-destructive enable conditions, structural row keys and numeric missingness.
- Separate resize, protobuf restoration, `.omv` reopen and native PNG/PDF export evidence.

**Enforcement boundary:** existing static gates catch some syntax and state patterns. They cannot
prove these semantic properties. Add analysis-specific numerical or lifecycle tests when working
on an affected analysis; do not mark a module clean because a generic gate passed. A documentation
learning pass records those gaps and updates the teaching material without inventing a fix count
or importing the external module's build restrictions, punctuation rules or unverified overrides.
