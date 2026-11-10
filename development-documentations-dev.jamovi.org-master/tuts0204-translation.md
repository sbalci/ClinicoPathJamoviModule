<!-- Vendored verbatim from https://github.com/jamovi/dev.jamovi.org/blob/7145eb8/src/content/docs/learn/tutorial/tuts0204-translation.md (fetched 2026-11-09; published at https://dev.jamovi.org). -->

---
layout: ../layouts/BaseLayout.astro
title: Module Translation
description: Learn how to enable multi-language support in your jamovi module.
type: article
---

jamovi supports the translation of modules. Although R does provide a rudimentary translation system, it's unfortunately limited in several significant ways. Perhaps the most significant limitation is that an R process cannot change the language it is using 'on the fly'. As a result, jamovi has had to provide its own translation system.

## Enabling translations

By default, all strings in the `.a.yaml`, `.r.yaml`, and `.u.yaml` are available to be translated, and don't require any special treatment by module developers. As such, the main task for module developers in enabling translation in modules is to ensure strings used in `.R` source files can be translated accordingly. The most common strings that need to be translated typically error messages and warnings.

Strings are marked as translateable using the `.()` function from `jmvcore`. In order to use this without the `jmvcore::` prefix (i.e. allowing the more terse `.('Too few samples')`, rather than having to write `jmvcore::.('Too few samples')`), it's necessary to include the relevant import in the modules `NAMESPACE` file as follows:

```r
importFrom(jmvcore,.)
```

This done, it's simply a matter of wrapping strings that need to be translated in calls to `.()`. Wrapping strings with `.()` performs two roles:
1. It indicates to the `jamovi-compiler` or `jmvtools` that the surrounded string needs translating. This allows the compiler (or jmvtools) to extract the string and place it in a 'database' of strings to translate. 
2. It performs the translation at runtime. The `.()` function looks up a database of translated strings at runtime, and if it finds one suitable for the present language, it will return that to the calling function.

### Recommendations

Translatable strings should not have trailing or leading spaces. Translators might not understand why they are there and may accidentally trim them off, leading to formatting issues in the UI.

**Avoid:**
```r
resids <- format(.('{}Residuals'), ifelse(std, .('Standardized '), ''))
```

**Recommended:**
```r
if (std) {
    resids <- .('Standardized Residuals')
} else {
    resids <- .('Residuals')
}
```

## Limitations: Accessing `self`

The `.()` function requires access to the analysis object (i.e. the `self` in `self$results$...`). In order to access the current analysis, the `.()` function looks for a variable named `self` in the calling function's environment.

For the most part, this happens transparently. If you call `.()` inside a member function of the analysis R6 class (like `.run()`), `self` is defined and it works perfectly.

### Auxiliary Functions

The exception is where the analysis calls auxiliary functions which are **not** members of the analysis R6 class. In these cases, you must pass `self` into the function explicitly.

**BAD (Will Fail):**
```r
makeSSString <- function(sstype) {
    if (sstype == 1) {
        # Error: .() cannot find 'self'
        return(.('Type 1 Sum of Squares is not suitable for this data set'))
    }
    NULL
}

# R6 Class
.run=function() {
    message <- makeSSString(sstype)
}
```

**GOOD:**
```r
makeSSString <- function(sstype, self) {
    if (sstype == 1) {
        # Success: 'self' is passed in explicitly
        return(.('Type 1 Sum of Squares is not suitable for this data set'))
    }
    NULL
}

# R6 Class
.run=function() {
    message <- makeSSString(sstype, self)
}
```

## Creating Translation Files

You can manage the translation catalogs using `jmvtools` from the R console:

```r
# Create the base catalog (POT file)
jmvtools::i18nCreate('catalog')

# Create a specific language file (e.g., 'de' for German)
jmvtools::i18nCreate('de')

# Update existing files after adding new strings to your code
jmvtools::i18nUpdate()
```

The jamovi library is also able to take care of much of this process for you. Feel free to reach out to the jamovi team to participate in the community translation efforts.
