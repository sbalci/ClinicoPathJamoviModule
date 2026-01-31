# The Complete Guide to Internationalization (i18n) in jamovi Development

This is the comprehensive guide to implementing internationalization and localization in jamovi module development. Internationalization enables your module to support multiple languages, making statistical analyses accessible to users worldwide. This guide covers everything from basic string marking to complete translation workflows.

## Table of Contents

1. [Introduction: Understanding jamovi i18n](#1-introduction-understanding-jamovi-i18n)
2. [The Translation Architecture](#2-the-translation-architecture)
3. [Marking Strings for Translation](#3-marking-strings-for-translation)
4. [Translation Workflow with jmvtools](#4-translation-workflow-with-jmvtools)
5. [Best Practices for Translatable Strings](#5-best-practices-for-translatable-strings)
6. [Working with .po and .pot Files](#6-working-with-po-and-pot-files)
7. [Advanced Translation Patterns](#7-advanced-translation-patterns)
8. [Clinical Terminology Guidelines](#8-clinical-terminology-guidelines)
9. [Integration with Weblate](#9-integration-with-weblate)
10. [Complete Examples](#10-complete-examples)
11. [Troubleshooting Common Issues](#11-troubleshooting-common-issues)
12. [References and Resources](#12-references-and-resources)

---

## 1. Introduction: Understanding jamovi i18n

### What is Internationalization (i18n)?

Internationalization (i18n) is the process of designing software so it can be adapted to various languages and regions without engineering changes. For jamovi modules, this means:

- **User-facing strings** can be translated into multiple languages
- **Statistical terminology** is presented in the user's preferred language
- **Clinical guidance** reaches healthcare professionals worldwide
- **Accessibility** is improved for non-English speaking researchers

### Why i18n Matters for jamovi Modules

**For Users:**
- Access to statistical analysis in their native language
- Reduced cognitive load when interpreting results
- Increased confidence in understanding methodology
- Better integration into local clinical workflows

**For Developers:**
- Broader user base and greater impact
- Contribution to global research accessibility
- Integration with jamovi's multilingual ecosystem
- Professional-grade software development practice

### The jamovi i18n System

jamovi uses the **gettext** internationalization framework:

```
Source code → String extraction → .pot template → .po translations → Runtime lookup
```

**Key Components:**
- **`.()` function** - Marks strings for translation in R code
- **jmvtools** - Extracts and manages translation catalogs
- **.pot files** - Portable Object Template (translation template)
- **.po files** - Portable Object (language-specific translations)
- **Weblate** - Web-based translation platform (optional)

### Quick Start: The 3-Step Process

1. **Mark strings for translation** using `.()` function
2. **Extract strings** using `jmvtools::i18nCreate()` and `i18nUpdate()`
3. **Translate** the generated .po files (manually or via Weblate)

---

## 2. The Translation Architecture

### How Translation Works at Runtime

```r
# In your .b.R file:
message <- .('Analysis completed successfully')

# At runtime:
# 1. jamovi checks user's language setting (e.g., 'tr' for Turkish)
# 2. Looks up translation in jamovi/i18n/tr.po
# 3. Returns translated string or falls back to original
```

### The `.()` Translation Function

**What it does:**
1. **Development time** - Signals to jmvtools that string should be extracted
2. **Runtime** - Performs translation lookup based on current language

**Source:** `jmvcore` package

**Usage:**
```r
# Import in NAMESPACE
importFrom(jmvcore, .)

# Use in code
.('Your translatable string here')
```

### Automatic vs Manual Translation

**Automatically Translated (no `.()` needed):**
- Strings in `.a.yaml` (analysis options)
- Strings in `.r.yaml` (results definitions)
- Strings in `.u.yaml` (user interface)

**Require Manual Marking with `.()`:**
- Dynamic strings in `.b.R` (backend code)
- Error messages
- Warning messages
- Formatted output strings
- Conditional text

### File Structure

```
your-module/
├── jamovi/
│   ├── your-function.a.yaml    # Auto-extracted
│   ├── your-function.u.yaml    # Auto-extracted
│   ├── your-function.r.yaml    # Auto-extracted
│   └── i18n/
│       ├── catalog.pot         # Translation template
│       ├── en.po               # English (source)
│       ├── tr.po               # Turkish
│       ├── de.po               # German
│       └── ...                 # Other languages
└── R/
    └── your-function.b.R       # Manual `.()` wrapping
```

---

## 3. Marking Strings for Translation

### 3.1 Setting Up NAMESPACE

**First step:** Import the `.()` function

```r
# Add to NAMESPACE file
importFrom(jmvcore, .)
```

**Why:** Makes `.()` available without `jmvcore::` prefix

### 3.2 Basic String Wrapping

**Pattern:**
```r
.('Your translatable string')
```

**Examples:**
```r
# Simple messages
message <- .('Analysis completed successfully')
warning <- .('Some groups have zero counts')
error <- .('Please select at least one variable')

# Table titles
title <- .('Descriptive Statistics')

# Column headers
colTitle <- .('Mean Difference')

# Button labels
buttonLabel <- .('Calculate')
```

### 3.3 Error and Warning Messages

**From jamovi core (ancova.b.R):**
```r
singularErrorMessage <- .("Singular fit encountered; one or more predictor variables are a linear combination of other predictor variables.")

perfectFitErrorMessage <- .("Residual sum of squares and/or degrees of freedom are zero, indicating a perfect fit")
```

**Your implementation:**
```r
# Input validation
if (is.null(self$options$dep)) {
    stop(.("Dependent variable is required"))
}

# Data quality checks
if (any(is.na(data))) {
    warning(.("Missing values detected and removed"))
}

# Statistical warnings
if (n < 30) {
    warning(.("Sample size is small; results may be unreliable"))
}
```

### 3.4 Strings with Placeholders

**Pattern:** Use `{}` for dynamic values

**From jamovi core (ancova.b.R):**
```r
postHocTableTitle <- .('Post Hoc Comparisons - {term}')
```

**Format at runtime:**
```r
# Define translatable template
template <- .('Analysis of {n} observations in {k} groups')

# Fill in values
message <- jmvcore::format(template, list(n = nrow(data), k = nlevels(groups)))
```

**Why `{}`?**
- Translators can reorder placeholders for grammar
- Clear separation between structure and content
- Compatible with gettext standards

**Examples:**
```r
# Multiple placeholders
msg <- .('Found {n_missing} missing values in {var_name}')
msg <- jmvcore::format(msg, list(n_missing = sum(is.na(x)), var_name = 'age'))

# Numeric formatting
msg <- .('Concordance index: {c_index}')
msg <- jmvcore::format(msg, list(c_index = round(c_stat, 3)))

# Conditional content
if (pvalue < 0.05) {
    msg <- .('Significant difference detected (p = {p})')
} else {
    msg <- .('No significant difference (p = {p})')
}
msg <- jmvcore::format(msg, list(p = format.pval(pvalue)))
```

### 3.5 Table Elements

**Column titles and supertitles:**
```r
# From jamovi core (ancova.b.R)
table$addColumn(
    name = paste0(ph[i], '1'),
    title = ph[i],
    type = 'text',
    superTitle = .('Comparison'),  # Translated
    combineBelow = TRUE
)

# Your implementation
table$addColumn(
    name = 'mean_diff',              # Not translated (programmatic)
    title = .('Mean Difference'),    # Translated
    type = 'number',
    superTitle = .('Effect Size')    # Translated
)
```

**Row group labels:**
```r
# From jamovi core (ancova.b.R)
if (self$options$modelTest) {
    table$addRow(
        rowKey = '.',
        list(name = .('Overall model'))  # Translated
    )
    table$addFormat(rowKey = '.', col = 1, format = Cell.BEGIN_END_GROUP)
}

# Your implementation
table$addRow(rowKey = 'desc_stats', list(
    name = .('Descriptive Statistics')  # Translated
))
```

### 3.6 What NOT to Wrap

**Do NOT wrap:**
- Variable names (programmatic identifiers)
- Column/row keys
- Function names
- Object property names
- Numbers (unless part of text)

**Examples:**
```r
# CORRECT
table$addColumn(
    name = 'p_value',              # NOT wrapped - identifier
    title = .('P-value'),          # Wrapped - display text
    type = 'number'
)

# INCORRECT
table$addColumn(
    name = .('p_value'),           # Wrong! - breaks code
    title = .('P-value'),
    type = 'number'
)

# CORRECT - Variable selection
dep <- self$options$dep           # Variable name - not wrapped
if (is.null(dep)) {
    stop(.("Please select a dependent variable"))  # Message - wrapped
}

# INCORRECT
dep <- self$options$.('dep')      # Wrong! - breaks code
```

### 3.7 Strings Requiring `self` Context

**The `.()` function needs access to `self`** (the analysis object).

**In main methods (`.init()`, `.run()`):**
```r
# Works automatically - self is in scope
private$.run <- function() {
    message <- .('Running analysis...')
    # ...
}
```

**In utility functions:**
```r
# PROBLEM: self not in scope
makeSSString <- function(sstype) {
    # This will fail!
    return .('Type 1 Sum of Squares is not suitable for this data set')
}

# SOLUTION: Pass self as parameter
makeSSString <- function(sstype, self) {
    if (sstype == 1) {
        return .('Type 1 Sum of Squares is not suitable for this data set')
    }
    # ...
}

# Call with self
private$.run <- function() {
    msg <- makeSSString(self$options$ssType, self)
    # ...
}
```

---

## 4. Translation Workflow with jmvtools

### 4.1 Initial Setup

**Step 1: Add NAMESPACE import**
```r
# In NAMESPACE file
importFrom(jmvcore, .)
```

**Step 2: Wrap translatable strings**
```r
# In R/*.b.R files
message <- .('Your message here')
```

**Step 3: Create translation directory**
```bash
# Automatically created by jmvtools
# jamovi/i18n/
```

### 4.2 Creating the Base Catalog

**Generate English template:**
```r
# In R console at module root
jmvtools::i18nCreate("en")
```

**Creates:**
```
jamovi/i18n/en.po
```

**Update existing catalog:**
```r
jmvtools::i18nUpdate("en")
```

**What happens:**
- Scans all `.a.yaml`, `.r.yaml`, `.u.yaml` files
- Extracts all `.()` wrapped strings from `.b.R` files
- Creates/updates .po file with all translatable strings

### 4.3 Creating Language-Specific Catalogs

**Create Turkish translation:**
```r
jmvtools::i18nCreate("tr")
```

**Create multiple languages:**
```r
jmvtools::i18nCreate("de")  # German
jmvtools::i18nCreate("fr")  # French
jmvtools::i18nCreate("es")  # Spanish
jmvtools::i18nCreate("pt")  # Portuguese
jmvtools::i18nCreate("ja")  # Japanese
jmvtools::i18nCreate("zh")  # Chinese
```

**Update all catalogs:**
```r
jmvtools::i18nUpdate("tr")
jmvtools::i18nUpdate("de")
jmvtools::i18nUpdate("fr")
# etc.
```

### 4.4 Creating the .pot Template

**For Weblate integration:**
```bash
# Copy en.po to catalog.pot
cp jamovi/i18n/en.po jamovi/i18n/catalog.pot

# Edit catalog.pot header
# Change "Language: en\n" to "Language: c\n"
```

**Why .pot?**
- Universal template for all translations
- Used by Weblate and other translation tools
- Language-neutral (language code: `c`)

### 4.5 Complete Workflow Example

```r
# 1. Initial creation
jmvtools::i18nCreate("en")
jmvtools::i18nCreate("tr")

# 2. After code changes (added new strings)
jmvtools::i18nUpdate("en")
jmvtools::i18nUpdate("tr")

# 3. Create Weblate template
# (In terminal)
# cp jamovi/i18n/en.po jamovi/i18n/catalog.pot
# Edit catalog.pot: Language: c

# 4. Translate tr.po (manually or via Weblate)

# 5. Test translations
# (Install module and change jamovi language setting)
```

---

## 5. Best Practices for Translatable Strings

### 5.1 String Structure

**✅ DO:**
```r
# Complete sentences
.('Analysis completed successfully')

# Clear, standalone messages
.('Please select at least one predictor variable')

# Descriptive labels
.('Confidence Interval (95%)')
```

**❌ DON'T:**
```r
# Leading/trailing spaces
.(' Analysis completed ')  # Bad

# Fragmented sentences
.('Analysis') + ' ' + .('completed')  # Bad

# Embedded formatting
paste0(.('Results for '), varname, .(' variable'))  # Bad
```

### 5.2 Avoid String Concatenation

**❌ Bad - Fragmented translation:**
```r
# Translators can't reorder words
if (std) {
    resids <- .('Standardized ') + .('Residuals')  # Bad!
} else {
    resids <- .('Residuals')
}
```

**✅ Good - Complete alternatives:**
```r
# Each case has complete translatable string
if (std) {
    resids <- .('Standardized Residuals')
} else {
    resids <- .('Residuals')
}
```

**❌ Bad - Embedded variables:**
```r
# Variables inside translation
resids <- .('{}Residuals')
formatted <- format(resids, ifelse(std, .('Standardized '), ''))  # Bad!
```

**✅ Good - Placeholder pattern:**
```r
# Complete string with placeholder
if (std) {
    resids <- .('Standardized Residuals')
} else {
    resids <- .('Residuals')
}
```

### 5.3 Context and Clarity

**Provide enough context for translators:**

**❌ Ambiguous:**
```r
.('Mean')  # Mean what? Average? Unkind?
```

**✅ Clear:**
```r
.('Mean Value')
.('Arithmetic Mean')
.('Group Mean')
```

**Use complete phrases:**
```r
# Good - Complete thought
.('The model did not converge after 100 iterations')

# Bad - Fragmented
.('The model') + ' ' + .('did not converge') + ' ' + .('after') + ' ' + iterations
```

### 5.4 Numbers and Units

**✅ Translate text, keep numbers:**
```r
.('Sample size must be at least {n}')
# Turkish: "Örneklem büyüklüğü en az {n} olmalıdır"
# {n} stays as placeholder
```

**Units in context:**
```r
# Include units in translatable string
.('Time (months)')
.('Tumor size (cm)')
.('Weight (kg)')

# Not separate
# 'Time' + '(months)'  # Bad - can't reorder
```

### 5.5 Consistency in Terminology

**Use consistent terms across module:**
```r
# Pick one and stick with it
.('Confidence Interval')  # Not sometimes "CI", sometimes "Confidence Interval"

# Medical statistics
.('Hazard Ratio')
.('Odds Ratio')
.('Risk Ratio')

# Always use same phrasing
.('Analysis completed')  # Not sometimes "Analysis finished", "Analysis done"
```

### 5.6 Format Specifications

**Avoid hardcoded formats:**
```r
# Bad - US date format embedded
.('Analysis date: MM/DD/YYYY')

# Good - Generic or localized
.('Analysis date')  # Let locale handle format
```

---

## 6. Working with .po and .pot Files

### 6.1 .po File Structure

**Basic .po file format:**
```po
# Translator comments
#: R/myfunction.b.R:45
msgid "Analysis completed successfully"
msgstr "Analiz başarıyla tamamlandı"

#: R/myfunction.b.R:67
msgid "Please select a variable"
msgstr "Lütfen bir değişken seçin"
```

**Components:**
- `#:` - Source location (file:line)
- `msgid` - Original English string
- `msgstr` - Translated string

### 6.2 .pot File (Template)

**Portable Object Template:**
```pot
# SOME DESCRIPTIVE TITLE.
# Copyright (C) YEAR THE PACKAGE'S COPYRIGHT HOLDER
# This file is distributed under the same license as the PACKAGE package.
# FIRST AUTHOR <EMAIL@ADDRESS>, YEAR.
#
msgid ""
msgstr ""
"Project-Id-Version: PACKAGE VERSION\n"
"Report-Msgid-Bugs-To: \n"
"POT-Creation-Date: 2026-01-31 16:00+0000\n"
"PO-Revision-Date: YEAR-MO-DA HO:MI+ZONE\n"
"Last-Translator: FULL NAME <EMAIL@ADDRESS>\n"
"Language-Team: LANGUAGE <LL@li.org>\n"
"Language: c\n"
"MIME-Version: 1.0\n"
"Content-Type: text/plain; charset=UTF-8\n"
"Content-Transfer-Encoding: 8bit\n"

#: R/myfunction.b.R:45
msgid "Analysis completed successfully"
msgstr ""

#: R/myfunction.b.R:67
msgid "Please select a variable"
msgstr ""
```

**Key difference:** `msgstr ""` (empty - template)

### 6.3 Editing .po Files

**Manual editing:**
```po
# Before (untranslated)
msgid "Confidence Interval"
msgstr ""

# After (translated to Turkish)
msgid "Confidence Interval"
msgstr "Güven Aralığı"
```

**Using translation tools:**
- **Poedit** - Desktop app (Windows/Mac/Linux)
- **Weblate** - Web-based (recommended for teams)
- **Lokalize** - KDE translation tool
- **Virtaal** - Simple desktop editor

### 6.4 Plural Forms

**English:**
```po
msgid "{n} observation"
msgid_plural "{n} observations"
msgstr[0] "{n} observation"
msgstr[1] "{n} observations"
```

**Turkish (different plural rules):**
```po
msgid "{n} observation"
msgid_plural "{n} observations"
msgstr[0] "{n} gözlem"
msgstr[1] "{n} gözlem"  # Turkish: same form for plural
```

### 6.5 Quality Checking .po Files

**Check for:**
- Missing translations (empty `msgstr`)
- Placeholder consistency (`{var}` in both msgid and msgstr)
- Encoding (must be UTF-8)
- Fuzzy entries (marked with `#, fuzzy` - need review)

**Command line check:**
```bash
# Check for errors
msgfmt -c -v jamovi/i18n/tr.po

# Find untranslated strings
msgattrib --untranslated jamovi/i18n/tr.po

# Find fuzzy strings
msgattrib --only-fuzzy jamovi/i18n/tr.po
```

---

## 7. Advanced Translation Patterns

### 7.1 Context-Specific Translations

**Same English, different meanings:**
```r
# Statistical "mean"
.('Mean')  # μ, average

# Unkind "mean"
.('Mean behavior')
```

**Solution: Use context markers (gettext feature):**
```r
# In future jamovi versions (if supported)
# pgettext('statistics', 'Mean')  # Ortalama
# pgettext('behavior', 'Mean')    # Kötü niyetli
```

**Current workaround:**
```r
# Be more specific
.('Mean Value')          # Ortalama Değer
.('Arithmetic Mean')     # Aritmetik Ortalama
```

### 7.2 Conditional Messages

**Pattern:**
```r
# Define all alternatives as complete strings
if (method == 'parametric') {
    msg <- .('Using parametric t-test')
} else if (method == 'nonparametric') {
    msg <- .('Using non-parametric Mann-Whitney U test')
} else {
    msg <- .('Using robust rank-based test')
}
```

**Not:**
```r
# Don't fragment
msg <- .('Using') + ' ' + methodName + ' ' + .('test')  # Bad!
```

### 7.3 Dynamic Tables with Translation

**Column titles from options:**
```r
# Get user-selected variables
vars <- self$options$vars

for (var in vars) {
    table$addColumn(
        name = var,                    # Variable name (not translated)
        title = var,                   # Display as-is (user's variable)
        type = 'number',
        superTitle = .('Statistics')   # Translated header
    )
}
```

### 7.4 Formatted Statistical Results

**Pattern:**
```r
# Template with placeholders
template <- .('Chi-square test: χ² = {chi2}, df = {df}, p = {p}')

# Format at runtime
result <- jmvcore::format(template, list(
    chi2 = round(chisq$statistic, 2),
    df = chisq$parameter,
    p = format.pval(chisq$p.value, digits = 3)
))

# Turkish translation would be:
# "Ki-kare testi: χ² = {chi2}, sd = {df}, p = {p}"
# (Note: translator can reorder if needed)
```

### 7.5 HTML and Rich Text

**HTML in translatable strings:**
```r
# Keep HTML structure minimal
html <- .('<p>Analysis completed successfully.</p>')

# Better: Separate structure from content
content <- .('Analysis completed successfully.')
html <- paste0('<p>', content, '</p>')

# For complex HTML, use templates
template <- '
<div class="summary">
  <h3>{title}</h3>
  <p>{message}</p>
</div>
'
html <- jmvcore::format(template, list(
    title = .('Analysis Summary'),
    message = .('Results are shown below.')
))
```

---

## 8. Clinical Terminology Guidelines

### 8.1 Turkish Medical Statistics Glossary

**Essential translations for ClinicoPath module:**

| English | Turkish | Abbreviation |
|---------|---------|--------------|
| Confidence Interval | Güven Aralığı | GA |
| P-value | p-değeri | p |
| Hazard Ratio | Tehlike Oranı | TO |
| Odds Ratio | Odds Oranı | OO |
| Risk Ratio | Risk Oranı | RO |
| Relative Risk | Göreceli Risk | GR |
| Area Under Curve | Eğri Altı Alan | EAA |
| Sensitivity | Duyarlılık | - |
| Specificity | Özgüllük | - |
| Positive Predictive Value | Pozitif Öngörü Değeri | PÖD |
| Negative Predictive Value | Negatif Öngörü Değeri | NÖD |
| Likelihood Ratio | Olabilirlik Oranı | OO |
| Effect Size | Etki Büyüklüğü | - |
| Standard Deviation | Standart Sapma | SS |
| Standard Error | Standart Hata | SH |
| Mean | Ortalama | - |
| Median | Ortanca | - |
| Interquartile Range | Çeyrekler Arası Aralık | ÇAA |
| False Discovery Rate | Yanlış Keşif Oranı | YKO |
| Type I Error | Tip I Hata | α |
| Type II Error | Tip II Hata | β |
| Power | Güç | - |
| Sample Size | Örneklem Büyüklüğü | n |
| Degrees of Freedom | Serbestlik Derecesi | sd |

### 8.2 Survival Analysis Terms

| English | Turkish |
|---------|---------|
| Overall Survival | Genel Sağkalım |
| Disease-Free Survival | Hastalıksız Sağkalım |
| Progression-Free Survival | İlerleme Mevcut Sağkalım |
| Censored | Sansürlü |
| Event | Olay |
| Time to Event | Olaya Kadar Geçen Süre |
| Kaplan-Meier Curve | Kaplan-Meier Eğrisi |
| Log-Rank Test | Log-Rank Testi |
| Cox Proportional Hazards | Cox Orantılı Tehlikeler |
| Competing Risks | Yarışan Riskler |
| Cumulative Incidence | Kümülatif İnsidans |

### 8.3 Pathology-Specific Terms

| English | Turkish |
|---------|---------|
| Tumor Grade | Tümör Derecesi |
| Tumor Stage | Tümör Evresi |
| Lymphovascular Invasion | Lenfovasküler İnvazyon |
| Perineural Invasion | Perinöral İnvazyon |
| Resection Margin | Rezeksiyon Sınırı |
| Lymph Node | Lenf Nodu |
| Metastasis | Metastaz |
| Mitotic Count | Mitoz Sayısı |
| Ki-67 Index | Ki-67 İndeksi |
| Immunohistochemistry | İmmünohistokimya |

### 8.4 Statistical Test Names

| English | Turkish |
|---------|---------|
| t-test | t-testi |
| Chi-square test | Ki-kare testi |
| Fisher's exact test | Fisher kesin testi |
| Mann-Whitney U test | Mann-Whitney U testi |
| Wilcoxon test | Wilcoxon testi |
| Kruskal-Wallis test | Kruskal-Wallis testi |
| ANOVA | Varyans Analizi (ANOVA) |
| Linear Regression | Doğrusal Regresyon |
| Logistic Regression | Lojistik Regresyon |
| Cox Regression | Cox Regresyonu |

### 8.5 Tone and Style Guidelines

**For clinical users (pathologists, oncologists):**

1. **Formal but accessible**
   - Use professional medical terminology
   - Avoid overly casual language
   - Be precise and unambiguous

2. **Consistency**
   - Use same term throughout module
   - Match terminology in Turkish medical literature
   - Align with Turkish pathology/oncology textbooks

3. **Abbreviations**
   - Introduce abbreviation after first full term
   - Example: "Güven Aralığı (GA)" then "GA"
   - Use internationally recognized symbols (p, χ², t, F)

4. **Numbers and formatting**
   - Use international number format (decimal point = ".")
   - Don't translate mathematical notation
   - Keep Greek letters (α, β, μ, σ, χ²)

**Examples:**

✅ Good:
```r
.('Güven Aralığı (GA) %95')
.('p-değeri < 0.05 olduğunda istatistiksel olarak anlamlıdır')
.('Tehlike Oranı (TO) = 1.5, GA 95%: 1.2-1.9')
```

❌ Too casual:
```r
.('GA %95')  # Too abbreviated for first use
.('p anlamlı')  # Too vague
.('TO yüksek')  # Imprecise
```

---

## 9. Integration with Weblate

### 9.1 What is Weblate?

**Weblate** is a web-based translation platform that integrates with GitHub:
- Translators work in web interface (no technical skills needed)
- Automatic .po file updates via pull requests
- Translation memory and suggestions
- Quality checks and validation
- Collaboration features

### 9.2 Setting Up Weblate for Your Module

**Step 1: Create i18n repository**
```bash
# Create separate repo for translations
# Example: ClinicoPath-i18n

mkdir ClinicoPath-i18n
cd ClinicoPath-i18n

# Add catalog.pot
cp ../ClinicoPath/jamovi/i18n/catalog.pot .

# Add README
cat > README.md << 'EOF'
# ClinicoPath Translation Files

This repository contains translation files for the ClinicoPath jamovi module.

## For Translators

Visit the Weblate project to contribute translations:
[Project URL will be added]

## Languages

- Turkish (tr)
- German (de)
- [Add your language]

## Resources

- [ClinicoPath module](https://github.com/sbalci/ClinicoPathJamoviModule)
- [Translation guide](link to guide)
EOF

# Initialize git
git init
git add .
git commit -m "Initial commit: catalog.pot"

# Create GitHub repo and push
gh repo create ClinicoPath-i18n --public
git remote add origin https://github.com/sbalci/ClinicoPath-i18n.git
git push -u origin main
```

**Step 2: Configure GitHub webhooks**
```
Repository Settings → Webhooks → Add webhook
Payload URL: https://hosted.weblate.org/hooks/github/
Content type: application/json
Secret: (leave empty)
Events: Just the push event
Active: ✓
```

**Step 3: Add Weblate collaborator**
```
Repository Settings → Collaborators
Add: weblate
```

**Step 4: Contact jamovi team**

Email jamovi developers to request Weblate project setup:
- Module name: ClinicoPath
- Repository: https://github.com/sbalci/ClinicoPath-i18n
- Maintainer: [your name/email]

### 9.3 Weblate Workflow

```
Developer                Weblate              Translators
─────────                ───────              ───────────
Code changes
    ↓
jmvtools::i18nUpdate()
    ↓
Update catalog.pot
    ↓
Push to GitHub ────────→ Auto-sync
                            ↓
                        Update source
                            ↓
                        Make available ──→ Translate
                            ↓                   ↓
                        Receive ←──────────── Submit
                            ↓
Pull request ←───────── Auto-create
    ↓
Review & merge
    ↓
Copy .po files
back to module
    ↓
Test & release
```

### 9.4 Manual .po File Synchronization

**If not using Weblate:**

```bash
# 1. Generate updated catalogs in main module
cd ClinicoPath
Rscript -e "jmvtools::i18nUpdate('en'); jmvtools::i18nUpdate('tr')"

# 2. Copy to i18n repo
cp jamovi/i18n/*.po ../ClinicoPath-i18n/
cp jamovi/i18n/catalog.pot ../ClinicoPath-i18n/

# 3. Commit and push
cd ../ClinicoPath-i18n
git add .
git commit -m "Update translation catalogs"
git push

# 4. Translators work on .po files

# 5. Pull updated translations
cd ../ClinicoPath-i18n
git pull

# 6. Copy back to module
cp *.po ../ClinicoPath/jamovi/i18n/

# 7. Build and test
cd ../ClinicoPath
jmvtools::prepare()
```

---

## 10. Complete Examples

### 10.1 Simple Function with i18n

**Original (English-only):**
```r
# R/mytest.b.R
mytestClass <- R6::R6Class(
    "mytestClass",
    inherit = mytestBase,
    private = list(
        .run = function() {
            # Validation
            if (is.null(self$options$dep)) {
                stop("Please select a dependent variable")
            }

            # Analysis
            result <- t.test(data[[dep]])

            # Populate results
            table <- self$results$ttest
            table$setRow(rowNo = 1, values = list(
                stat = result$statistic,
                df = result$parameter,
                p = result$p.value
            ))

            # Add note
            note <- "95% confidence interval shown"
            table$setNote('ci', note)
        }
    )
)
```

**With i18n:**
```r
# R/mytest.b.R
mytestClass <- R6::R6Class(
    "mytestClass",
    inherit = mytestBase,
    private = list(
        .run = function() {
            # Validation
            if (is.null(self$options$dep)) {
                stop(.("Please select a dependent variable"))
            }

            # Analysis
            result <- t.test(data[[dep]])

            # Populate results
            table <- self$results$ttest
            table$setRow(rowNo = 1, values = list(
                stat = result$statistic,
                df = result$parameter,
                p = result$p.value
            ))

            # Add note
            note <- .("95% confidence interval shown")
            table$setNote('ci', note)
        }
    )
)

# NAMESPACE
importFrom(jmvcore, .)

# Generate catalogs
# jmvtools::i18nCreate("tr")
# jmvtools::i18nUpdate("tr")

# jamovi/i18n/tr.po (excerpt)
# msgid "Please select a dependent variable"
# msgstr "Lütfen bir bağımlı değişken seçin"
#
# msgid "95% confidence interval shown"
# msgstr "%95 güven aralığı gösterilmiştir"
```

### 10.2 Complex Function with Placeholders

**Backend implementation:**
```r
# R/survival.b.R
survivalClass <- R6::R6Class(
    "survivalClass",
    inherit = survivalBase,
    private = list(
        .run = function() {
            # Get data
            data <- self$data
            time <- self$options$time
            event <- self$options$event

            # Validation with informative messages
            if (is.null(time) || is.null(event)) {
                stop(.("Both time and event variables are required"))
            }

            n_events <- sum(data[[event]])
            n_censored <- nrow(data) - n_events

            if (n_events < 10) {
                template <- .("Warning: Only {n} events observed. At least 10 recommended.")
                msg <- jmvcore::format(template, list(n = n_events))
                warning(msg)
            }

            # Fit model
            fit <- survival::survfit(
                Surv(data[[time]], data[[event]]) ~ 1
            )

            # Summary message
            template <- .("Analysis of {total} observations: {events} events, {censored} censored")
            summary <- jmvcore::format(template, list(
                total = nrow(data),
                events = n_events,
                censored = n_censored
            ))

            # Populate summary table
            summaryTable <- self$results$summary
            summaryTable$setRow(rowNo = 1, values = list(
                n = nrow(data),
                events = n_events,
                censored = n_censored,
                median = summary(fit)$table['median']
            ))

            # Add note
            note_template <- .("Median survival time with {ci}% confidence interval")
            note <- jmvcore::format(note_template, list(
                ci = self$options$ciWidth
            ))
            summaryTable$setNote('median', note)
        }
    )
)
```

**Turkish translation (jamovi/i18n/tr.po):**
```po
msgid "Both time and event variables are required"
msgstr "Hem zaman hem de olay değişkenleri gereklidir"

msgid "Warning: Only {n} events observed. At least 10 recommended."
msgstr "Uyarı: Sadece {n} olay gözlendi. En az 10 önerilir."

msgid "Analysis of {total} observations: {events} events, {censored} censored"
msgstr "{total} gözlemin analizi: {events} olay, {censored} sansürlü"

msgid "Median survival time with {ci}% confidence interval"
msgstr "Medyan sağkalım süresi %{ci} güven aralığı ile"
```

### 10.3 Full Module i18n Workflow

**1. Prepare code:**
```r
# NAMESPACE
importFrom(jmvcore, .)

# R/crosstable.b.R - wrap all user-visible strings
private$.run = function() {
    # Validation
    if (is.null(self$options$rows) || is.null(self$options$cols)) {
        stop(.("Please select both row and column variables"))
    }

    # Check sample size
    if (nrow(self$data) < 20) {
        warning(.("Small sample size: chi-square test may be unreliable"))
    }

    # Perform test
    result <- chisq.test(table(data[[rows]], data[[cols]]))

    # Check expected frequencies
    if (any(result$expected < 5)) {
        msg <- .("Warning: Some expected frequencies < 5. Consider Fisher's exact test.")
        # Store warning
    }

    # Populate table with translated note
    note <- .("Pearson's chi-square test with continuity correction")
    self$results$chitest$setNote('method', note)
}
```

**2. Generate catalogs:**
```r
# In R console
jmvtools::i18nCreate("en")
jmvtools::i18nCreate("tr")
jmvtools::i18nCreate("de")
```

**3. Translate .po files:**
```po
# jamovi/i18n/tr.po
msgid "Please select both row and column variables"
msgstr "Lütfen hem satır hem de sütun değişkenlerini seçin"

msgid "Small sample size: chi-square test may be unreliable"
msgstr "Küçük örneklem büyüklüğü: ki-kare testi güvenilir olmayabilir"

msgid "Warning: Some expected frequencies < 5. Consider Fisher's exact test."
msgstr "Uyarı: Bazı beklenen frekanslar < 5. Fisher kesin testini değerlendirin."

msgid "Pearson's chi-square test with continuity correction"
msgstr "Süreklilik düzeltmeli Pearson ki-kare testi"
```

**4. Create Weblate template:**
```bash
cp jamovi/i18n/en.po jamovi/i18n/catalog.pot
# Edit header: Language: c
```

**5. Test:**
```r
# Build module
jmvtools::prepare()
jmvtools::install()

# Change jamovi language to Turkish
# Open module and verify translations
```

---

## 11. Troubleshooting Common Issues

### 11.1 Strings Not Being Extracted

**Symptom:** `.()` wrapped strings don't appear in .po file

**Causes & Solutions:**

**1. NAMESPACE not updated**
```r
# Check NAMESPACE contains:
importFrom(jmvcore, .)

# If missing, add it and run:
jmvtools::prepare()
```

**2. Forgot to update catalog**
```r
# After adding new strings:
jmvtools::i18nUpdate("en")
jmvtools::i18nUpdate("tr")
```

**3. String syntax error**
```r
# Bad - missing quotes
.("Message')  # Syntax error!

# Good
.("Message")
```

### 11.2 Translations Not Showing at Runtime

**Symptom:** jamovi still shows English despite translated .po file

**Causes & Solutions:**

**1. Module not rebuilt**
```r
# After editing .po files:
jmvtools::prepare()
jmvtools::install()
# Restart jamovi
```

**2. Language setting**
```
# Check jamovi language setting:
# Settings → General → Language
```

**3. .po file errors**
```bash
# Validate .po file
msgfmt -c jamovi/i18n/tr.po

# Check for:
# - Empty msgstr
# - Encoding issues (must be UTF-8)
# - Fuzzy entries (#, fuzzy)
```

**4. Placeholder mismatch**
```po
# Bad - placeholder missing
msgid "Found {n} errors"
msgstr "Hata bulundu"  # Missing {n}!

# Good
msgid "Found {n} errors"
msgstr "{n} hata bulundu"
```

### 11.3 `self` Scope Issues

**Symptom:** Error: "object 'self' not found" in `.()` call

**Cause:** Utility function doesn't have access to `self`

**Solution:**
```r
# Pass self as parameter
myUtility <- function(data, self) {
    msg <- .("Processing data...")
    # ...
}

# Call with self
private$.run = function() {
    myUtility(self$data, self)
}
```

### 11.4 Fuzzy Translations

**Symptom:** .po file has `#, fuzzy` markers

**Cause:** jmvtools detected string changed since last translation

**Example:**
```po
#, fuzzy
msgid "Analysis completed"
msgstr "Analiz tamamlandı"
```

**Solution:**
1. Review if translation still appropriate
2. Update if needed
3. Remove `#, fuzzy` line
4. Rebuild module

### 11.5 Special Characters Not Displaying

**Symptom:** Turkish characters (ğ, ü, ş, ı, ö, ç) show as �

**Cause:** Encoding issue

**Solution:**
```po
# Ensure .po file header has:
"Content-Type: text/plain; charset=UTF-8\n"

# Save file as UTF-8 in editor
```

### 11.6 Plurals Not Working

**Symptom:** Plural forms always show singular

**Current limitation:** jamovi may not fully support gettext plurals

**Workaround:**
```r
# Instead of automatic plurals, use conditionals
if (n == 1) {
    msg <- .("1 observation")
} else {
    template <- .("{n} observations")
    msg <- jmvcore::format(template, list(n = n))
}
```

---

## 12. References and Resources

### Official Documentation

**jamovi Developer Hub:**
- i18n API: https://dev.jamovi.org/api_i18n.html
- jmvtools documentation: https://dev.jamovi.org/api_jmvtools.html

**gettext Resources:**
- GNU gettext manual: https://www.gnu.org/software/gettext/manual/
- .po file format: https://www.gnu.org/software/gettext/manual/html_node/PO-Files.html

### Translation Tools

**Poedit** (Desktop)
- Website: https://poedit.net/
- Free version available
- Windows, Mac, Linux

**Weblate** (Web-based)
- Website: https://weblate.org/
- Hosted service: https://hosted.weblate.org/
- GitHub integration

**Lokalize** (KDE)
- Website: https://apps.kde.org/lokalize/
- Linux

**Command Line Tools:**
```bash
# Install gettext utilities
# Ubuntu/Debian:
sudo apt-get install gettext

# macOS (Homebrew):
brew install gettext

# Commands:
msgfmt    # Compile .po to .mo
msgmerge  # Merge translations
msgattrib # Filter messages
xgettext  # Extract strings (jmvtools does this)
```

### ClinicoPath Module Examples

**See these functions for i18n patterns:**
- `R/crosstable.b.R` - Basic i18n
- `R/survival.b.R` - Complex messages with placeholders
- `R/decisioncurve.b.R` - Clinical terminology

**Translation files:**
- `jamovi/i18n/tr.po` - Turkish translations
- `jamovi/i18n/catalog.pot` - Translation template

### Turkish Medical Terminology

**Resources:**
- Türk Patoloji Derneği terminoloji kılavuzu
- Turkish medical dictionaries
- SNOMED CT Turkish edition

**Key principles:**
- Match terminology in Turkish pathology textbooks
- Align with Turkish medical education standards
- Consult with Turkish-speaking pathologists

### Community Support

**jamovi Forum:**
- https://forum.jamovi.org/
- "Developers" category for technical questions

**GitHub Issues:**
- jamovi core: https://github.com/jamovi/jamovi/issues
- jmvtools: https://github.com/jamovi/jmvtools/issues

**ClinicoPath Module:**
- GitHub: https://github.com/sbalci/ClinicoPathJamoviModule
- Issues: https://github.com/sbalci/ClinicoPathJamoviModule/issues

---

## Appendix A: Quick Reference Card

### Essential Commands

```r
# Setup
importFrom(jmvcore, .)  # In NAMESPACE

# Create catalogs
jmvtools::i18nCreate("en")
jmvtools::i18nCreate("tr")

# Update catalogs
jmvtools::i18nUpdate("en")
jmvtools::i18nUpdate("tr")

# Rebuild module
jmvtools::prepare()
jmvtools::install()
```

### Essential Patterns

```r
# Simple string
.("Your message")

# With placeholder
template <- .("Found {n} errors")
msg <- jmvcore::format(template, list(n = count))

# Conditional
if (condition) {
    msg <- .("Option A message")
} else {
    msg <- .("Option B message")
}

# With self in utility
myFunc <- function(data, self) {
    .("Translatable message")
}
```

### File Checklist

- [ ] `importFrom(jmvcore, .)` in NAMESPACE
- [ ] All user-visible strings wrapped with `.()`
- [ ] No leading/trailing spaces in strings
- [ ] Complete phrases, not fragments
- [ ] Placeholders use `{name}` format
- [ ] Catalogs generated (en.po, tr.po)
- [ ] Translations complete (no empty msgstr)
- [ ] Module rebuilt with `jmvtools::prepare()`
- [ ] Tested in jamovi with language switched

---

## Appendix B: Medical Statistics Glossary (Extended)

### Diagnostic Accuracy

| English | Turkish | Notes |
|---------|---------|-------|
| True Positive | Gerçek Pozitif | TP |
| True Negative | Gerçek Negatif | TN |
| False Positive | Yanlış Pozitif | FP |
| False Negative | Yanlış Negatif | FN |
| Accuracy | Doğruluk | (TP+TN)/(TP+TN+FP+FN) |
| Precision | Kesinlik | TP/(TP+FP) |
| Recall | Duyarlılık | Same as Sensitivity |
| F1 Score | F1 Skoru | Harmonic mean |
| ROC Curve | ROC Eğrisi | Receiver Operating Characteristic |
| Youden Index | Youden İndeksi | Sensitivity + Specificity - 1 |
| Diagnostic Odds Ratio | Tanısal Odds Oranı | DOR |
| Number Needed to Diagnose | Tanı İçin Gerekli Sayı | NND |

### Regression Models

| English | Turkish |
|---------|---------|
| Coefficient | Katsayı |
| Intercept | Kesme Noktası |
| Slope | Eğim |
| Residual | Artık |
| R-squared | R-kare |
| Adjusted R-squared | Düzeltilmiş R-kare |
| Multicollinearity | Çoklu Bağlantı |
| Heteroscedasticity | Değişen Varyans |
| Autocorrelation | Otokorelasyon |

### Effect Sizes

| English | Turkish |
|---------|---------|
| Cohen's d | Cohen d |
| Hedges' g | Hedges g |
| Glass's delta | Glass delta |
| Eta-squared | Eta-kare |
| Omega-squared | Omega-kare |
| Partial eta-squared | Kısmi eta-kare |
| Correlation coefficient | Korelasyon katsayısı |
| Cramér's V | Cramér V |
| Phi coefficient | Phi katsayısı |

---

**Guide Version:** 1.0
**Last Updated:** 2026-01-31
**For jamovi version:** 2.0+
**Author:** ClinicoPath Development Team

---

This guide is part of the ClinicoPath jamovi module documentation suite. For other guides, see `vignettes/README_GUIDES.md`.
