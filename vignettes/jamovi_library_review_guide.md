# The jamovi Library Review Guide

**What the jamovi library reviewer actually checks — and how to pass first time.**

This guide is distilled from the real audit reports issued by the jamovi library
reviewer against this project's submodules:

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

Every rule below is something the reviewer *actually raised*, on real files, with
severity attached. Nothing here is speculative. The only exception is the few items
marked **anticipatory**: they cover the jamovi 28.3 features (File option, Text result,
Image `mode: vector`), come from the release and dev.jamovi.org, and have not been
raised by the reviewer yet.

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
13. [The `type: Notice` trap](#13-the-type-notice-trap)
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
grep -n "addRow(rowKey *= *[\"']" R/*.b.R

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
#    specifiers, notice title colours                                    [CRITICAL..LOW]
python3 tools/release_gate.py      # FAIL lines block; read every WARN for shipped analyses
python3 tools/release_gate.py --root ../<Module>   # the same checks on the tree the reviewer reads

# 10. jamovi 28.3 features vs module-wide minApp                        [anticipatory]
grep -n '^minApp' jamovi/0000.yaml          # File option / Text result => must be >= 28.3.0
grep -nE 'type: (File|Text) *$|mode: vector' jamovi/*.a.yaml jamovi/*.r.yaml
#    a `type: Text` table COLUMN is not a Text result (false hit); release_gate.py
#    check_min_app (run in item 9) skips columns, FAILs File/Text under minApp < 28.3.0
#    and WARNs on mode: vector

# 11. Everything still compiles
Rscript -e 'Sys.unsetenv("ELECTRON_RUN_AS_NODE"); jmvtools::prepare(".")'
```

Plus the cheap metadata gates the reviewer checks first:

- `Version:` is past `1.0` and **identical** in `DESCRIPTION`, `jamovi/0000.yaml`,
  every `.a.yaml`, and `CITATION.cff`.
- `License: GPL (>= 2)` (OSI-approved).
- Every key in `jamovi/00refs.yaml` is cited **and** every cited key resolves,
  with exact casing. Every entry has `title`, `author`, `url`. Check this in the
  **generated submodule**, not only the umbrella: the umbrella keeps every entry on
  purpose, and `_updateModules.R` trims each submodule's copy to the keys it cites.
  A missing `url` is only a WARN in `tools/release_gate.py` — treat it as blocking for
  any key a shipped analysis cites.
- Every `clearWith` entry resolves to a real option name.
- Every `renderFun:` resolves to a real `function(image, ...)` method.
- No committed build artifacts (`*.tar.gz`, `*.jmo`).
- `compilerMode: tame` on every `.u.yaml`.
- *(anticipatory)* A `type: File` option or `type: Text` result needs `minApp: 28.3.0` in
  `jamovi/0000.yaml`. `minApp` is module-wide, so raising it locks every jamovi 28.2-or-older
  user out of the whole module: a deliberate release decision, never a side effect. Setup,
  gating and testing: [jamovi 28.3 Features](jamovi_module_patterns_guide.md#jamovi-283-features-file-text-vector-images)
  and [Installing Current jmvtools and jmvcore](jamovi_module_patterns_guide.md#installing-current-jmvtools-and-jmvcore).
- `NEWS.md` has a heading for the exact `DESCRIPTION` version. `_updateModules.R` rewrites
  `Version:` on every regeneration and never writes `NEWS.md`, so after each regeneration the
  submodule's changelog is one release behind until a person writes it (2026-09-16 OncoPath).
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
| 3 | Fixed-structure tables built with `addRow()` in `.run()` | 4 of 5 | MEDIUM |
| 4 | Spliced `.()` translation fragments | 5 of 5 | LOW |
| 5 | Named HTML entities that will render literally | 3 of 5 | MEDIUM |
| 6 | `setVisible(FALSE)` used to signal a failure | 3 of 5 | MEDIUM–HIGH |
| 7 | Packages used but not declared in `Imports:` | 2 of 5 | LOW–MEDIUM |
| 8 | HTML that ignores the dark theme (opaque pastels; fixed title hues on a tint) | 2 of 5 | MEDIUM–INFO |
| 9 | Bare `warning()` the user never sees | 1 of 5 | MEDIUM |
| 10 | Dead code referencing commented-out schema | 2 of 5 | MEDIUM |
| 11 | `requiresData: true` on renderers that draw from state only | 5 of 5 (rounds 3–4) | LOW |
| 12 | Translation catalogs inherited from the umbrella | 3 of 5 (round 4) | MEDIUM–LOW |
| 13 | `NEWS.md` behind the `DESCRIPTION` version | 2 of 5 (round 4) | LOW–INFO |

**The lesson:** when a review names one instance, grep for the class and fix all
of it. The reviewer explicitly rewards this — "several of them by fixing the
class of problem rather than the instance."

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
[section 13](#13-the-type-notice-trap).

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

### Related: don't push numbers through a text column

Two summary tables declared `value` as `type: text` and pushed numbers through
`as.character()`. That gives up jamovi's own number formatting and decimal-place
handling, and text columns don't right-align, so a column of numbers reads
ragged. Split numeric statistics into a `type: number` column.

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

---

## 12. Rule: UI label conventions

| Element | Convention | Example |
|---|---|---|
| CheckBox label | Name the **thing**, not the action | "Residual plot", not "Show residual plot" |
| Individual controls (TextBox, ComboBox, CheckBox) | Sentence case | "Ridge height scale", not "Ridge Height Scale" |
| `CollapseBox` / `TargetLayoutBox` headings | Title Case | "Statistical Options" |
| Options panel title (`.u.yaml:1`) | **Must match** `.a.yaml` `title:` | otherwise the user clicks one name and lands on a panel labelled another |
| `menuSubgroup` | Must describe the analysis | a diagnostic-accuracy meta-analysis does not belong under "IHC Analysis" |

Exception: a verb is correct when the checkbox really performs an action on the
dataset — "Add test pattern to data" is fine.

Variable selection comes **first** in every panel; everything else goes in
collapsed `CollapseBox` groups.

---

## 13. The `type: Notice` trap

The ClinicoPathDescriptives audit suggests replacing hand-styled HTML panels with
`type: Notice` result elements declared in `.r.yaml`, on the grounds that the
serialization problem is specific to *constructing* Notice objects dynamically.

**That reasoning is right, but the option is not available today.** Verified
empirically against jamovi 28.1.0 / jmvtools 28.3 / jamovi-compiler 0.3.5:

```
$ jmvtools::prepare(".")
Unable to compile 'nt.r.yaml':
	results.items[0].type is not one of enum values:
	Table,Group,Array,Image,Preformatted,Html,State,Property,Output,Notification,Action
```

The compiler's `schemas/resultsschema.yaml` enum does not include `Notice`, even
though `compiler.js` has a `Notice` branch in `sourcifyResults` and jamovi's
protobuf defines `ResultsNotice` with `NoticeType {ERROR=0, STRONG_WARNING=1,
WARNING=2, INFO=3}`. The toolchain is mid-migration.

**`Notification` is a worse trap.** It *is* in the enum, so it compiles — and
generates:

```r
self$add(list(`name`="warn", `title`="Warning", `type`="Notification"))
```

a plain list, not a results element, because `jmvcore::Notification` does not
exist. It fails at runtime instead of at compile time.

### Where this leaves you

| Need | Use |
|---|---|
| Fatal validation error | `jmvcore::reject(.("..."), code = "...")` |
| Non-fatal warning shown inline | `type: Html` element + theme-safe styling ([section 4](#4-rule-html-output-must-be-theme-safe)) |
| Narrative / explanatory text, no severity *(anticipatory)* | `type: Text` (jamovi 28.3+, module-wide `minApp: 28.3.0`); module cannot add CSS |
| Dynamic notice | `jmvcore::Notice` **only** if you are not inserting it with `insert()` — see below |

**Do not use `self$results$insert(999, notice)` with a `jmvcore::Notice`.** Notice
objects hold function references that jamovi's protobuf layer cannot serialize;
the symptom is `attempt to apply non-function`. See `R/waterfall.b.R`
(`.addNotice()` / `.renderNotices()`) for the conversion pattern.

**Re-check this section when jmvtools updates.** The moment the compiler enum
gains `Notice`, declarative notices become the right answer for every hand-styled
panel in the module, and the theme-safety problem in [section 4](#4-rule-html-output-must-be-theme-safe)
disappears — jamovi styles notices itself, in whichever theme is active.

Re-test with:

```r
# a scratch module with one `type: Notice` item in its .r.yaml
Sys.unsetenv("ELECTRON_RUN_AS_NODE")   # VS Code sets this and breaks prepare()
jmvtools::prepare(".")
```

**Re-verified 2026-09-17** (jmvtools 28.3, jmvcore 2.7.38) on a clone of OncoPath: `type: Notice`
still fails the schema; `type: Notification` compiles and `<fn>Results$new()` then fails with
*attempt to apply non-function*.

**Re-checked 2026-09-19 against jmvtools 28.3.1 (schema read, `prepare()` not re-run):** the
installed `resultsschema.yaml` enum is now `Table, Group, Array, Image, Preformatted, Text, Html,
Svg, State, Property, Output, Notification, Action`. Still no `Notice`, so the REJECTED comment
below stays valid. `Svg` exists upstream but is undocumented: do not use it yet
([Exists Upstream, Not Yet Documented](jamovi_module_patterns_guide.md#exists-upstream-not-yet-documented)).

### Say so in the code

Round 2 rejected this suggestion for ClinicoPathDescriptives and meddecide, and round 4
(2026-09-16 OncoPath [INFO]) raised it again: the rejection lived only in our responses, which the
reviewer never reads. Every hand-rolled notice helper now carries a comment the reviewer will see:

```r
# library-audit 2026-09-16 OncoPath [INFO] REJECTED: no native notice element - type: Notice fails the
#   .r.yaml schema, type: Notification builds no results object (guide section 13)
.addNotice = function(type, title, content) {
```

Put that line above `.addNotice()` in any analysis you touch. The theme concern that motivates
the suggestion *is* fixable in HTML — [section 4](#4-rule-html-output-must-be-theme-safe).

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

---

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
