# Internationalization (i18n) Preparation & Translation Plan for `benford`

## 0) Argument Normalization

**SANITIZED_FN**: `benford`

**Target Files Status**:
- ✅ `jamovi/benford.a.yaml` (options) - **EXISTS**
- ✅ `jamovi/benford.u.yaml` (UI) - **EXISTS**
- ✅ `jamovi/benford.r.yaml` (results) - **EXISTS**
- ✅ `R/benford.b.R` (backend) - **EXISTS**

All required files are present.

---

## 1) NAMESPACE i18n Hook

✅ **VERIFIED**: The NAMESPACE already includes the translation helper import:
```r
importFrom(jmvcore,.)
```

The `.` function is properly imported for marking strings for extraction into `.po` catalogs.

---

## 2) Translatable Strings Analysis

### Current State in `R/benford.b.R`

**Strings requiring translation wrapper**:

| Line | Current String | Status | Required Action |
|------|---------------|--------|-----------------|
| 29 | `"Data contains no (complete) rows"` | ❌ Unwrapped | Wrap with `.()` |
| 39 | `"Package documentation"` | ❌ Unwrapped | Wrap with `.()` |
| 79 | `"Error: Variable contains missing or non-numeric values that cannot be analyzed."` | ❌ Unwrapped | Wrap with `.()` |
| 81 | `"Error: Insufficient data for Benford's Law analysis. Please provide more observations."` | ❌ Unwrapped | Wrap with `.()` |
| 83 | `"Analysis error:"` | ❌ Unwrapped | Wrap with `.()` |

### YAML Strings (Automatically Extracted)

**From `benford.a.yaml`**:
- `title: Benford Analysis`
- `menuSubgroup: ClinicoPath Data Quality`
- `title: Variable`
- `title: Number of Digits`
- Descriptions (R and jamovi versions)

**From `benford.u.yaml`**:
- `title: Benford Analysis`
- `label: Variable`

**From `benford.r.yaml`**:
- `title: Benford Analysis`
- `title: To Do`
- `title: Suspects`

---

## 3) Patch Suggestions

### `R/benford.b.R` - Required Changes

```diff
--- a/R/benford.b.R
+++ b/R/benford.b.R
@@ -26,7 +26,7 @@ benfordClass <- if (requireNamespace('jmvcore')) R6::R6Class(
                 return(FALSE)
             }
             if (nrow(self$data) == 0) {
-                stop("Data contains no (complete) rows")
+                stop(.("Data contains no (complete) rows"))
             }
             return(TRUE)
         },
@@ -36,7 +36,7 @@ benfordClass <- if (requireNamespace('jmvcore')) R6::R6Class(
             todo <- glue::glue("
                                <br>
                                See
-                               <a href = 'https://github.com/carloscinelli/benford.analysis'>Package documentation</a> for interpretation.
+                               <a href = 'https://github.com/carloscinelli/benford.analysis'>{doclink}</a> for interpretation.
                                ")
+            doclink <- .("Package documentation")
+            todo <- glue::glue(todo, doclink = doclink)
 
             self$results$todo$setContent(todo)
 
@@ -76,11 +76,11 @@ benfordClass <- if (requireNamespace('jmvcore')) R6::R6Class(
             }, error = function(e) {
                 # User-friendly error messages
                 if (grepl("NA|NaN", e$message)) {
-                    self$results$text$setContent("Error: Variable contains missing or non-numeric values that cannot be analyzed.")
+                    self$results$text$setContent(.("Error: Variable contains missing or non-numeric values that cannot be analyzed."))
                 } else if (grepl("insufficient", e$message, ignore.case = TRUE)) {
-                    self$results$text$setContent("Error: Insufficient data for Benford's Law analysis. Please provide more observations.")
+                    self$results$text$setContent(.("Error: Insufficient data for Benford's Law analysis. Please provide more observations."))
                 } else {
-                    self$results$text$setContent(paste("Analysis error:", e$message))
+                    self$results$text$setContent(paste(.("Analysis error:"), e$message))
                 }
             })
         },
```

---

## 4) Turkish Translation Table

### Missing Translations (New Entries)

| Status | msgid | msgstr (current) | Suggested TR |
|--------|-------|------------------|--------------|
| missing | "Benford Analysis" | | "Benford Analizi" |
| missing | "ClinicoPath Data Quality" | | "ClinicoPath Veri Kalitesi" |
| missing | "Variable" | | "Değişken" |
| missing | "Number of Digits" | | "Basamak Sayısı" |
| missing | "To Do" | | "Yapılacaklar" |
| missing | "Suspects" | | "Şüpheli Değerler" |
| missing | "Data contains no (complete) rows" | | "Veri tam satır içermiyor" |
| missing | "Package documentation" | | "Paket dokümantasyonu" |
| missing | "Error: Variable contains missing or non-numeric values that cannot be analyzed." | | "Hata: Değişken eksik veya sayısal olmayan analiz edilemeyecek değerler içeriyor." |
| missing | "Error: Insufficient data for Benford's Law analysis. Please provide more observations." | | "Hata: Benford Yasası analizi için yetersiz veri. Lütfen daha fazla gözlem sağlayın." |
| missing | "Analysis error:" | | "Analiz hatası:" |
| missing | "The data as a data frame." | | "Veri çerçevesi olarak veri." |
| missing | "a string naming the variable from `data` that contains the continuous values used for the report" | | "Rapor için kullanılan sürekli değerleri içeren `data`'dan değişkeni adlandıran bir metin" |
| missing | "Number of first digits to analyze (default: 2)" | | "Analiz edilecek ilk basamak sayısı (varsayılan: 2)" |
| missing | "Number of first digits to analyze" | | "Analiz edilecek ilk basamak sayısı" |

---

## 5) Turkish Glossary & Consistency Guidelines

### Statistical Terms (TR)
```text
Analysis → Analiz
Variable → Değişken
Data → Veri
Error → Hata
Missing values → Eksik değerler
Non-numeric → Sayısal olmayan
Observations → Gözlemler
Digits → Basamaklar
Suspects → Şüpheli değerler
Data frame → Veri çerçevesi
Continuous → Sürekli
Default → Varsayılan
```

### Benford-Specific Terms
```text
Benford's Law → Benford Yasası
First digits → İlk basamaklar
Fraud detection → Sahtekarlık tespiti
Data quality → Veri kalitesi
Suspicious data → Şüpheli veri
```

### Clinical/Pathology Context
```text
ClinicoPath → ClinicoPath (no translation - brand name)
Report → Rapor
Documentation → Dokümantasyon
```

---

## 6) QA Checklist

- [x] All user-visible strings in R backend identified
- [x] NAMESPACE contains `importFrom(jmvcore, .)`
- [x] All YAML files exist and verified
- [ ] `.po` files need to be created/updated
- [ ] Turkish translations reviewed for clinical accuracy
- [ ] Terminology consistency verified

---

## 7) Ready-to-Run Commands

### Create/Update Catalogs
```r
# In R console at module root
jmvtools::i18nCreate("en")
jmvtools::i18nUpdate("en")
jmvtools::i18nCreate("tr")
jmvtools::i18nUpdate("tr")
```

### Prepare POT Template
```bash
# At module root
cp jamovi/i18n/en.po jamovi/i18n/catalog.pot
# Edit header in catalog.pot to contain:
# "Language: c\n"
```

### Quick String Check
```bash
# Find unwrapped strings in benford.b.R
grep -nE '"[^"]+' R/benford.b.R | grep -v '\.\('
```

### Apply Patches
```bash
# Apply the wrapper changes to benford.b.R
patch -p1 < benford-i18n.patch
```

---

## 8) Implementation Steps

### Phase 1: Code Preparation
1. ✅ Verify NAMESPACE import (DONE)
2. Apply translation wrappers to `R/benford.b.R`
3. Run `jmvtools::prepare()` to verify no compilation errors

### Phase 2: Catalog Generation
1. Create English catalog: `jmvtools::i18nCreate("en")`
2. Update English catalog: `jmvtools::i18nUpdate("en")`
3. Create Turkish catalog: `jmvtools::i18nCreate("tr")`
4. Update Turkish catalog: `jmvtools::i18nUpdate("tr")`
5. Create POT template from en.po

### Phase 3: Translation
1. Fill Turkish translations in `jamovi/i18n/tr.po`
2. Review for clinical terminology consistency
3. Test with Turkish locale

### Phase 4: Weblate Integration
1. Create dedicated repo: `ClinicoPathJamoviModule-i18n`
2. Add `catalog.pot`, `README.md`, LICENSE
3. Add Weblate bot as collaborator
4. Configure webhook:
   - URL: `https://hosted.weblate.org/hooks/github/`
5. Request jamovi team to add project to Weblate

---

## 9) Deliverables Summary

### Files Status
- ✅ All required files exist for `benford` function
- ✅ NAMESPACE properly configured

### Required Actions
1. **Apply wrapper patches** to `R/benford.b.R` (5 strings)
2. **Generate catalogs** using jmvtools commands
3. **Add Turkish translations** (15 new entries)
4. **Test** with Turkish locale
5. **Setup Weblate** for collaborative translation

### Translation Coverage
- **R Backend**: 5 strings need wrapping
- **YAML Files**: 10+ strings automatically extracted
- **Total**: ~15 unique strings for translation

### Risk Assessment
- **Low Risk**: Simple string wrapping changes
- **No Breaking Changes**: Only adding translation markers
- **Backward Compatible**: Works without translations

---

## Notes for Implementation

1. The `glue::glue()` usage for HTML content needs special handling - suggest extracting the link text separately
2. Error messages should be clinician-friendly in Turkish
3. Consider adding context comments for translators about Benford's Law usage in fraud detection
4. Test with various Turkish number formats (comma vs. period for decimals)

---

*Generated: 2025-01-12*
*Module: ClinicoPathJamoviModule*
*Function: benford*
*Target Language: Turkish (tr)*