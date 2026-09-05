---
name: prepare-translation
description: Internationalize a jamovi function: wrap strings for extraction, generate .po files, create Turkish localization plan
interactive: true
args:
  function_name:
    description: Name of the jamovi function (basename without extension)
    required: true
    autocomplete: functions  # requires functions provider, otherwise ignored
  target_lang:
    description: Target language code
    required: false
    default: tr
usage: /prepare-translation <function_name> [--target_lang=tr]
output_file: i18n-plans/$ARGUMENTS-$ARG_target_lang-translation-plan.md
---

# Internationalization (i18n) Preparation & Translation Plan

**Consult:** `vignettes/jamovi_i18n_guide.md` for comprehensive i18n patterns and best practices.
**Official source:** jamovi "Module Translation" tutorial — https://dev.jamovi.org/tutorial/tuts0204-translation/ (jmvtools/compiler commands below were verified against jmvtools 28.3 / jamovi-compiler `i18n.js`).

**Why jamovi has its own system:** R's built-in translation cannot change language "on the fly" within one R process, so jamovi ships its own catalog-based lookup instead of gettext at the R level.

You are an **expert jamovi module developer**. Prepare the specified function for translation and produce a concrete plan for Turkish (TR) localization. Follow the steps below and render all code and shell snippets in fenced blocks.

---

## 0) Argument normalization (safety)

Sanitize `$ARGUMENTS` to **SANITIZED_FN**:

- Drop any leading paths (e.g., `R/foo.b.R` → `foo`).
- Strip known suffixes: `.a.yaml`, `.b.R`, `.r.yaml`, `.u.yaml`, `.yaml`, `.yml`.
- Use **SANITIZED_FN** consistently in all paths.

Target files (expected):

- `jamovi/SANITIZED_FN.a.yaml`  (options)
- `jamovi/SANITIZED_FN.u.yaml`  (UI)
- `jamovi/SANITIZED_FN.r.yaml`  (results)
- `R/SANITIZED_FN.b.R`          (backend)

If any are missing, note them, continue with what’s available, and suggest creation where needed.

---

## 1) NAMESPACE i18n hook

Ensure the **NAMESPACE** makes the translation helper `.` available without the `jmvcore::` prefix (so code can write the terse `.('Too few samples')` instead of `jmvcore::.('Too few samples')`):

```r
importFrom(jmvcore, .)
```

A blanket `import(jmvcore)` (what ClinicoPath's NAMESPACE currently has) also satisfies this. If neither is present, add the `importFrom` line via roxygen (`#' @importFrom jmvcore .`) — never hand-edit NAMESPACE.

---

## 2) Wrap translatable strings (jamovi patterns)

By default **every string in `.a.yaml`, `.r.yaml`, and `.u.yaml` is already translatable** and needs no special treatment. The developer's job is the `R/*.b.R` side: wrap user-visible strings in `.()`. The most common candidates are **error messages and warnings**, then table/column titles, notice text, and narrative sentences.

**IMPORTANT:** Wrapping a string in `.()` performs two roles:
1. **Development time**: tells the jamovi-compiler / jmvtools to extract the string into the catalog "database" of strings to translate
2. **Runtime**: `.()` looks up the translated-string database for the current language and returns the match (falls back to the original text)

**Reference:** See `vignettes/jamovi_i18n_guide.md` for comprehensive i18n patterns and best practices.

### 2.1 Error & warning messages

From jmv **ancova.b.R**:

```r
singularErrorMessage <- .("Singular fit encountered; one or more predictor variables are a linear combination of other predictor variables.")
perfectFitErrorMessage <- .("Residual sum of squares and/or degrees of freedom are zero, indicating a perfect fit")
```

**Apply the same pattern** in your function:

```r
stop(.("Please select at least one variable"))
warning(.("Some groups have zero counts; results may be unstable"))

# With context
if (n < 10) {
    stop(.("Sample size too small: at least 10 observations required"))
}
```

### 2.2 Titles with placeholders (use `{name}` tokens)

From jmv **ancova.b.R**:

```r
postHocTableTitle <- .('Post Hoc Comparisons - {term}')
```

When you need dynamic text, mark the string and later format the placeholder:

```r
# Define template with placeholder
title <- .('Pairwise Comparisons - {grp}')

# Format at runtime
title <- jmvcore::format(title, list(grp = groupName))

# Multiple placeholders
template <- .('Analysis of {n} observations in {k} groups')
msg <- jmvcore::format(template, list(n = nrow(data), k = nlevels(groups)))

# Statistical results
template <- .('Chi-square: χ² = {chi2}, df = {df}, p = {p}')
result <- jmvcore::format(template, list(
    chi2 = round(chisq$statistic, 2),
    df = chisq$parameter,
    p = format.pval(chisq$p.value)
))
```

**Why `{}`?**
- Translators can reorder placeholders for grammar
- Clear separation between structure and content
- Compatible with gettext standards

### 2.3 Table labels & group rows

From jmv **ancova.b.R**:

```r
if (self$options$modelTest) {
    table$addRow(rowKey='.', list(name=.('Overall model')))
    table$addFormat(rowKey='.', col=1, format=Cell.BEGIN_END_GROUP)
}
```

Wrap the human text only:

```r
table$addRow(list(name=.('Descriptive statistics')))
```

### 2.4 Column titles & super‑titles

From jmv **ancova.b.R**:

```r
table$addColumn(name=paste0(ph[i],'1'), title=ph[i], type='text', superTitle=.('Comparison'), combineBelow=TRUE)
```

Wrap `` and `` when they are human‑readable:

```r
table$addColumn(name='md', title=.('Mean Difference'), type='number')
```

> **Rules of thumb**:
> - Wrap complete phrases/sentences (not fragments)
> - Keep variable names/IDs **outside** (not translated)
> - No leading/trailing spaces in translatable strings
> - Prefer single quotes inside `.()` when string contains `{}` placeholders
> - Use complete alternatives for conditional text (not concatenation)

### 2.5 Don't wrap programmatic tokens

Do **not** wrap column **names**, keys, or machine‑only identifiers:

```r
# CORRECT
table$addColumn(
    name = 'p_value',           # NOT wrapped - identifier
    title = .('P-value'),       # Wrapped - display text
    type = 'number',
    superTitle = .('P-values')  # Wrapped - display text
)

# INCORRECT
table$addColumn(
    name = .('p_value'),        # Wrong! Breaks code
    title = .('P-value'),
    type = 'number'
)
```

### 2.6 Best practices for translatable strings

**✅ DO:**
```r
# Complete sentences
.('Analysis completed successfully')

# Complete alternatives for conditionals
if (std) {
    resids <- .('Standardized Residuals')
} else {
    resids <- .('Residuals')
}

# Descriptive with context
.('Mean Difference')
.('Confidence Interval (95%)')
```

**❌ DON'T:**
```r
# Leading/trailing spaces: translators don't know why they are there and
# may trim them, breaking UI formatting
.(' Analysis completed ')  # Bad

# Official "Avoid" example from the jamovi tutorial — a fragment with a
# trailing space assembled by format(); translators cannot reorder or
# grammatically agree the pieces
resids <- format(.('{}Residuals'), ifelse(std, .('Standardized '), ''))  # Bad
```

Prefer the branch-per-complete-string form shown under **DO** above.

### 2.7 Strings requiring `self` context

**IMPORTANT:** `.()` needs the analysis object (the `self` in `self$results$...`). It finds it by **looking for a variable named `self` in the calling function's environment**. Inside R6 member functions (`.init`, `.run`, `private$.foo`) this is transparent. It breaks in **auxiliary functions that are not members of the R6 class** — the runtime error is `object 'self' not found` (root cause of ClinicoPath issue #122 in multisurvival).

Official tutorial example:

```r
# BAD (will fail)
makeSSString <- function(sstype) {
    if (sstype == 1) {
        # Error: .() cannot find 'self'
        return(.('Type 1 Sum of Squares is not suitable for this data set'))
    }
    NULL
}

.run = function() {
    message <- makeSSString(sstype)
}

# GOOD — pass self in explicitly, as a parameter literally named `self`
makeSSString <- function(sstype, self) {
    if (sstype == 1) {
        return(.('Type 1 Sum of Squares is not suitable for this data set'))
    }
    NULL
}

.run = function() {
    message <- makeSSString(sstype, self)
}
```

Quick audit for this trap: any `.(` inside a top-level `function(` in `R/SANITIZED_FN.b.R` that has no `self` formal argument.

---

## 3) Extraction & Update commands

Run at the **module root** in an R console. These are the official jmvtools commands from the tutorial; jmvtools forwards them to the jamovi-compiler (`jmc --i18n <pkg> --create <code>` / `--update [code]`).

### 3.1 First-time setup (skip any file that already exists)

```r
# Base catalog (POT template) — 'catalog' (or 'c') is special-cased and
# writes jamovi/i18n/catalog.pot with header "Language: c\n"
jmvtools::i18nCreate("catalog")

# Language catalogs
jmvtools::i18nCreate("en")   # jamovi/i18n/en.po
jmvtools::i18nCreate("tr")   # jamovi/i18n/tr.po
```

ClinicoPath already has all three (`jamovi/i18n/catalog.pot`, `en.po`, `tr.po`), so for SANITIZED_FN you normally go straight to 3.2.

### 3.2 After wrapping new strings (the routine step)

```r
# No argument = update EVERY .po/.pot in jamovi/i18n/ (catalog.pot, en.po, tr.po, ...)
jmvtools::i18nUpdate()

# Or one catalog at a time
jmvtools::i18nUpdate("tr")

# Verbose compiler output when an entry looks wrong
jmvtools::i18nUpdate(debug = TRUE)
```

What `i18nUpdate` does (verified in this repo, ~1 minute): rescans every `.a/.r/.u.yaml` and every `.()` call in `R/*.b.R`, adds new `msgid`s tagged with `#: R/<fn>.b.R` / `#: <fn>/options/<x>.title` source references, **keeps existing `msgstr`**, and prunes entries whose source is gone. Because it rewrites all three files, review the `git diff` of `jamovi/i18n/` before committing.

Only `catalog.pot` is treated as the source catalog; any other `*.pot` in the folder is skipped with a "skipping unrecognized .pot file" message.

### 3.3 Manual fallback (only if `catalog.pot` is missing or corrupt and `i18nCreate("catalog")` is unavailable)

```bash
cp jamovi/i18n/en.po jamovi/i18n/catalog.pot
# Edit header so it reads exactly:  "Language: c\n"
```

---

## 4) Validate `.po` files and fill Turkish translations

If **tr.po** or **en.po** are attached or pasted:

- Build a **Missing/Untranslated** table listing `msgid` and an **initial Turkish suggestion** (clinical/statistics-aware, plain language).
- For already translated entries, **flag potential improvements** (terminology consistency, tone for clinicians—pathologists/oncologists).

Example table:

| Status        | msgid                            | msgstr (current) | Suggested TR (if empty or weak)       |
| ------------- | ------------------------------- | ---------------- | ------------------------------------ |
| missing       | "Post Hoc Comparisons - {term}" |                  | "Post Hoc Karşılaştırmalar – {term}" |
| existing-weak | "Mean Difference"                | "Ortalama fark"  | "Ortalama Fark"                      |

> Keep Turkish translations clinician‑friendly. Prefer **p‑değeri**, **Güven Aralığı (GA)**, **Duyarlılık/Özgüllük**, **Etki Büyüklüğü**.

**`.po` mechanics that bite (verified in this repo):**

- `{placeholder}` tokens must match **exactly** between `msgid` and `msgstr`.
- A `.()` string ending in ` [ ... ]` is split by the compiler into `msgctxt "..."` + `msgid`, and jmvcore strips the bracket tail at runtime — the text never displays. Gate: `grep -nE '\.\("[^"]*\]"\)' R/SANITIZED_FN.b.R` must be empty; put CIs in parentheses.
- Long msgids (> ~76 chars) are written as `msgid ""` followed by quoted continuation lines. Any fill script must parse entries as blank-line-separated blocks and join the pieces, or it silently skips every long notice.
- Validate before committing: `msgfmt -c -o /dev/null jamovi/i18n/tr.po`.

---

## 5) Consistency & glossary (TR)

```text
t-test → t‑testi
Mann–Whitney U → Mann–Whitney U testi
Confidence Interval (CI) → Güven Aralığı (GA)
Effect size → Etki büyüklüğü
Odds Ratio (OR) → Odds Oranı (OO)
Hazard Ratio (HR) → Tehlike Oranı (TO)
Area Under Curve (AUC) → Eğri Altı Alan (EAA)
False Discovery Rate (FDR) → Yanlış Keşif Oranı (YKO)
```

---

## 6) QA checklist

- Verify all user-visible strings in R backend files are wrapped with `` `.` `` (YAML strings need nothing).
- No translatable string has leading/trailing whitespace; no fragment-and-`format()` assembly.
- Every non-R6 helper that calls `.()` takes `self` as a parameter and every caller passes it.
- Confirm the NAMESPACE imports the translation helper `.` (`importFrom(jmvcore, .)` or `import(jmvcore)`).
- `jmvtools::i18nUpdate()` ran cleanly and the `jamovi/i18n/` diff contains only SANITIZED_FN-related changes.
- Ensure all known YAML files exist; suggest creation if missing.
- Validate `.po` files for untranslated or inconsistent entries.
- Review Turkish translations for clinical accuracy and tone.

---

## 7) Community translation / Weblate

**Official guidance (tutorial):** the jamovi library can take care of much of the catalog-hosting process; contact the jamovi team to have the module included in the community translation effort. The manual Weblate route below is the older self-hosted path — use it only if the team asks for it.

1. Create a dedicated repo: `<modulename>-i18n`
   - Add `catalog.pot`, `README.md`, license.
2. **Collaborators** → add Weblate bot.
3. **Webhooks** → add:\
   Payload URL: `https://hosted.weblate.org/hooks/github/`
4. Ask jamovi dev team to add your `<modulename>-i18n` project to Weblate.

---

## 8) Ready-to-run snippets (copy/paste)

**First time only**

```r
jmvtools::i18nCreate("catalog"); jmvtools::i18nCreate("en"); jmvtools::i18nCreate("tr")
```

**Every time strings change (updates catalog.pot + en.po + tr.po)**

```r
jmvtools::i18nUpdate()
```

**Validate the Turkish catalog**

```bash
msgfmt -c -o /dev/null jamovi/i18n/tr.po
grep -B1 msgctxt jamovi/i18n/tr.po | grep SANITIZED_FN   # must be empty
```

**Quick grep to find unwrapped strings in R (heuristic)**

```bash
# Lines with quoted strings not already wrapped (rough)
grep -nE '\"[^\"\n]+' R/SANITIZED_FN.b.R | grep -v '\\.\('
```

---

## 9) Deliverables

Return a single Markdown plan including:

- Files found/missing for **SANITIZED_FN**
- **Patch suggestions** (R/YAML) with `` .(...) `` wrapping diffs
- Turkish translation table for missing/weak entries
- Glossary and style notes for TR
- Weblate/GitHub setup steps
- QA checklist outcome

Save to: `i18n-plans/$ARGUMENTS-$ARG_target_lang-translation-plan.md`.

---

## Related Commands

- `/review-function` -- Review function including i18n checklist
- `/document-function` -- Generate documentation that includes translation status
