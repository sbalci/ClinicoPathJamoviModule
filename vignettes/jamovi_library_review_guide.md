# The jamovi Library Review Guide

**What the jamovi library reviewer actually checks — and how to pass first time.**

This guide is distilled from five real audit reports issued by the jamovi library
reviewer against this project's submodules:

| Report | Module | Date |
|---|---|---|
| `jamovi-library-audit/2026-08-17 ClinicoPathDescriptives.md` | ClinicoPathDescriptives | 2026-08-17 |
| `jamovi-library-audit/2026-08-17 jsurvival.md` | jsurvival | 2026-08-17 |
| `jamovi-library-audit/2026-08-17 meddecide.md` | meddecide | 2026-08-17 |
| `jamovi-library-audit/2026-08-18 OncoPath.md` | OncoPath | 2026-08-18 |
| `jamovi-library-audit/2026-08-18 jjstatsplot.md` | jjstatsplot | 2026-08-18 |

Every rule below is something the reviewer *actually raised*, on real files, with
severity attached. Nothing here is speculative.

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

---

## 1. The pre-submission checklist

Run these before asking for a library review. Each one maps to a finding that was
actually raised.

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
grep -nE '\.\(" |\ "\)' R/*.b.R          # leading/trailing space inside .()

# 8. Undeclared packages, including base-priority ones                   [LOW/MED]
Rscript -e 'testthat::test_file("tests/testthat/test-zzz-dependency-declaration.R")'

# 9. Everything still compiles
Rscript -e 'Sys.unsetenv("ELECTRON_RUN_AS_NODE"); jmvtools::prepare(".")'
```

Plus the cheap metadata gates the reviewer checks first:

- `Version:` is past `1.0` and **identical** in `DESCRIPTION`, `jamovi/0000.yaml`,
  every `.a.yaml`, and `CITATION.cff`.
- `License: GPL (>= 2)` (OSI-approved).
- Every key in `jamovi/00refs.yaml` is cited **and** every cited key resolves,
  with exact casing. Every entry has `title`, `author`, `url`.
- Every `clearWith` entry resolves to a real option name.
- Every `renderFun:` resolves to a real `function(image, ...)` method.
- No committed build artifacts (`*.tar.gz`, `*.jmo`).
- `compilerMode: tame` on every `.u.yaml`.

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
| 8 | Opaque light-theme HTML colours | 1 of 5 (module-wide) | MEDIUM |
| 9 | Bare `warning()` the user never sees | 1 of 5 | MEDIUM |
| 10 | Dead code referencing commented-out schema | 2 of 5 | MEDIUM |

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

### What you cannot do

You cannot fix this by declaring a `Notice` in `.r.yaml` — see
[section 13](#13-the-type-notice-trap).

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

Use the real character — but written as a `\u{}` escape, because `R CMD check`
flags literal non-ASCII bytes in R source.

| Entity | Char | Escape |
|---|---|---|
| `&minus;` | − | `\u{2212}` |
| `&mdash;` | — | `\u{2014}` |
| `&ndash;` | – | `\u{2013}` |
| `&rarr;` | → | `\u{2192}` |
| `&times;` | × | `\u{00D7}` |
| `&plusmn;` | ± | `\u{00B1}` |
| `&alpha;` | α | `\u{03B1}` |
| `&beta;` | β | `\u{03B2}` |
| `&kappa;` | κ | `\u{03BA}` |
| `&ge;` | ≥ | `\u{2265}` |
| `&eacute;` | é | `\u{00E9}` |
| `&nbsp;` | (nbsp) | `\u{00A0}` |

For `&nbsp;` used purely as a table-cell spacer, the simplest fix is to drop it:
`<td></td>` renders the same in HTML and exports cleanly.

**Caveat for very large HTML literals.** In a string literal longer than ~10,000
characters, `\u{}` escapes can hit a parse trap in a non-UTF-8 locale. In those
specific cases use HTML *numeric* entities (`&#x2192;`) instead — numeric
entities are part of the HTML spec and are not affected by the named-entity
change. See `reference_nonascii_conversion_pitfalls`.

### What is fine

- `gsub("&nbsp;", " ", x)` — code that *strips* entities is correct as it stands.
- `htmltools::htmlEscape()` output — it produces only the structural five.

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

### Coverage should be even

Two thoroughly translated analyses and two untranslated ones in the same menu
"reads as broken rather than as partial." If you internationalise, do the whole
module.

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
| Dynamic notice | `jmvcore::Notice` **only** if you are not inserting it with `insert()` — see below |

**Do not use `self$results$insert(999, notice)` with a `jmvcore::Notice`.** Notice
objects hold function references that jamovi's protobuf layer cannot serialize;
the symptom is `attempt to apply non-function`. See
`docs/NOTICE_TO_HTML_CONVERSION_GUIDE.md` and `R/waterfall.b.R` for the
conversion pattern.

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

## References

- Audit reports: `jamovi-library-audit/*.md`
- `vignettes/jamovi_module_patterns_guide.md` — the primary development guide
- `vignettes/jamovi_notices_guide.md` — notices, HTML output, theme safety
- `vignettes/jamovi_plots_guide.md` — plots and `image$state`
- `vignettes/jamovi_tables_guide.md` — table structure and `.init()`
- `vignettes/jamovi_i18n_guide.md` — translatable strings
- `vignettes/jamovi_r_yaml_guide.md` — valid result element types
- `tools/theme_safe_html.py` — the theme-safety transform
- `docs/NOTICE_TO_HTML_CONVERSION_GUIDE.md` — Notice → Html migration
