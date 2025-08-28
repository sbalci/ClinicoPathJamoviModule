---
name: prepare-translation
description: Prepare a jamovi function for internationalization and generate a TR (Turkish) translation plan, including code patches, .po updates, and Weblate setup steps
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

Ensure the **NAMESPACE** includes an import for the translation helper `.`:

```r
importFrom(jmvcore, .)
```

If missing, add it and include a minimal rationale:

- The ``.`` function is used to mark strings for extraction into `.po` catalogs.

---

## 2) Wrap translatable strings (jamovi patterns)

Use the `` `.` `` wrapper for all user‑visible strings in `R/*.b.R`. YAML strings are extracted automatically (no wrapper needed).

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
```

### 2.2 Titles with placeholders (use `{name}` tokens)

From jmv **ancova.b.R**:

```r
postHocTableTitle <- .('Post Hoc Comparisons - {term}')
```

When you need dynamic text, mark the string and later format the placeholder (either via `jmvcore::format()` or by supplying `{term}` where the table renderer replaces it):

```r
title <- .('Pairwise Comparisons - {grp}')
# later: title <- jmvcore::format(title, list(grp = groupName))
```

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

> **Rules of thumb**: wrap complete phrases/sentences; keep variable names/IDs **outside**. Prefer single quotes inside `.()` when the string contains `{}` placeholders.

### 2.5 Don’t wrap programmatic tokens

Do **not** wrap column **names**, keys, or machine‑only identifiers:

```r
name='p_value'        # not wrapped
superTitle=.('P-values')  # wrapped
```

---

## 3) Extraction & Update commands

Render shell snippets to create/update catalogs. Use **SANITIZED_FN** as comment context; these commands are run at the **module root**.

### 3.1 Create or update English template (source language)

```r
# In R console
jmvtools::i18nCreate("en")
jmvtools::i18nUpdate("en")
```

This creates/updates:

```
jamovi/i18n/en.po
```

### 3.2 Prepare Weblate template (POT)

Copy `en.po` to `catalog.pot` and set its language header to `c`:

```bash
cp jamovi/i18n/en.po jamovi/i18n/catalog.pot
# Ensure header contains exactly:
# Language: c\n
```

### 3.3 Create/Update Turkish catalog

```r
# In R console
jmvtools::i18nCreate("tr")
jmvtools::i18nUpdate("tr")
```

Outputs:

```
jamovi/i18n/tr.po
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

- Verify all user-visible strings in R backend files are wrapped with `` `.` ``.
- Confirm the NAMESPACE imports the translation helper `.`.
- Ensure all known YAML files exist; suggest creation if missing.
- Validate `.po` files for untranslated or inconsistent entries.
- Review Turkish translations for clinical accuracy and tone.

---

## 7) Weblate integration (GitHub)

1. Create a dedicated repo: `<modulename>-i18n`
   - Add `catalog.pot`, `README.md`, license.
2. **Collaborators** → add Weblate bot.
3. **Webhooks** → add:\
   Payload URL: `https://hosted.weblate.org/hooks/github/`
4. Ask jamovi dev team to add your `<modulename>-i18n` project to Weblate.

---

## 8) Ready-to-run snippets (copy/paste)

**Create/Update catalogs**

```r
jmvtools::i18nCreate("en"); jmvtools::i18nUpdate("en")
jmvtools::i18nCreate("tr"); jmvtools::i18nUpdate("tr")
```

**Prepare POT**

```bash
cp jamovi/i18n/en.po jamovi/i18n/catalog.pot
# Edit header: Language: c\n
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
