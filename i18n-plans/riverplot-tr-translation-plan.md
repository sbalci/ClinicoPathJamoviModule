# Internationalization Plan: riverplot → Turkish (TR)

**Function**: `riverplot` (SANITIZED_FN)  
**Target Language**: Turkish (TR)  
**Date**: 2024-09-03  

---

## 0) Argument Normalization

✅ **SANITIZED_FN**: `riverplot`

**Target files** (all found):
- ✅ `jamovi/riverplot.a.yaml` (options)
- ✅ `jamovi/riverplot.u.yaml` (UI)  
- ✅ `jamovi/riverplot.r.yaml` (results)
- ✅ `R/riverplot.b.R` (backend)

---

## 1) NAMESPACE i18n Hook

✅ **Status**: Already present
```r
importFrom(jmvcore,.)
```
The `.` function for marking translatable strings is already imported.

---

## 2) Translatable String Analysis & Patches

### 2.1 Error & Warning Messages

**Current unwrapped strings found:**

```r
# Line 87: Package availability error
"Required packages missing: "

# Line 261: Validation errors  
stop(paste(errors, collapse = "; "))

# Line 713: Package check
stop("Required packages not available")
```

**Patch needed for `R/riverplot.b.R`:**

```diff
# Line 84-90: Package missing error
-                error_msg <- paste0(
-                    "Required packages missing: ", paste(missing_packages, collapse = ", "),
+                error_msg <- paste0(
+                    .("Required packages missing: "), paste(missing_packages, collapse = ", "),
                     "<br><br>Please install with:<br><code>",
-                    "install.packages(c('", paste(missing_packages, collapse = "', '"), "'))",
+                    .("install.packages(c('{packages}'))"),
                     "</code>"
                 )

# Line 261: Validation error
-                stop(paste(errors, collapse = "; "))
+                stop(.("Validation failed: {errors}", errors = paste(errors, collapse = "; ")))

# Line 713: Package availability  
-                stop("Required packages not available")
+                stop(.("Required packages not available"))
```

### 2.2 HTML Content & User Messages

**Current unwrapped HTML strings:**

```diff
# Line 95: Missing dependencies header
-                    "<h4>📦 Missing Dependencies</h4>",
+                    "<h4>📦 ", .("Missing Dependencies"), "</h4>",

# Line 108: Welcome message title
-                    "<h3 style='color: #0277bd; margin-top: 0;'>🌊 River Plots & Alluvial Diagrams</h3>",
+                    "<h3 style='color: #0277bd; margin-top: 0;'>🌊 ", .("River Plots & Alluvial Diagrams"), "</h3>",

# Line 110: Main description
-                    "<p><strong>Visualize flows, transitions, and categorical changes across time or stages:</strong></p>",
+                    "<p><strong>", .("Visualize flows, transitions, and categorical changes across time or stages:"), "</strong></p>",

# Line 120: Required data setup
-                    "<h4 style='color: #ff8f00; margin: 0 0 8px 0;'>📋 Required Data Setup:</h4>",
+                    "<h4 style='color: #ff8f00; margin: 0 0 8px 0;'>📋 ", .("Required Data Setup:"), "</h4>",

# Line 199: Error analysis header
-                    "<h4>⚠️ Analysis Error</h4>",
+                    "<h4>⚠️ ", .("Analysis Error"), "</h4>",

# Line 200-201: Error message content
-                    "<p><strong>Error:</strong> ", e$message, "</p>",
-                    "<p><em>Please check your data format and variable selections.</em></p>",
+                    "<p><strong>", .("Error:"), "</strong> ", e$message, "</p>",
+                    "<p><em>", .("Please check your data format and variable selections."), "</em></p>",

# Line 690: Diagnostics header
-                "<h4 style='color: #495057; margin-top: 0;'>🔍 Data Processing Diagnostics</h4>",
+                "<h4 style='color: #495057; margin-top: 0;'>🔍 ", .("Data Processing Diagnostics"), "</h4>",

# Line 1355: Riverplot object header
-                "<h4 style='color: #495057; margin-top: 0;'>📊 CRAN Riverplot Object Structure</h4>",
+                "<h4 style='color: #495057; margin-top: 0;'>📊 ", .("CRAN Riverplot Object Structure"), "</h4>",
```

### 2.3 Validation Error Messages

**Additional validation errors to wrap:**

```diff
# Line 211-224: Validation error strings
-                errors <- c(errors, "At least one strata variable is required")
+                errors <- c(errors, .("At least one strata variable is required"))

-                errors <- c(errors, "Cannot auto-detect data format: provide time variable for long format or multiple strata for wide format")
+                errors <- c(errors, .("Cannot auto-detect data format: provide time variable for long format or multiple strata for wide format"))

-                errors <- c(errors, "Time variable is required for long format data")
+                errors <- c(errors, .("Time variable is required for long format data"))

-                errors <- c(errors, "Long format requires exactly one strata variable")
+                errors <- c(errors, .("Long format requires exactly one strata variable"))

-                errors <- c(errors, "Wide format requires at least two strata variables")
+                errors <- c(errors, .("Wide format requires at least two strata variables"))

-                errors <- c(errors, "At least 2 rows of data required")
+                errors <- c(errors, .("At least 2 rows of data required"))
```

---

## 3) Extraction & Update Commands

```r
# Create/update English template
jmvtools::i18nCreate("en")
jmvtools::i18nUpdate("en")

# Create/update Turkish catalog
jmvtools::i18nCreate("tr") 
jmvtools::i18nUpdate("tr")
```

```bash
# Prepare Weblate template
cp jamovi/i18n/en.po jamovi/i18n/catalog.pot
# Ensure header contains: Language: c\n
```

---

## 4) Turkish Translation Table

Based on the analysis of riverplot strings requiring translation:

| Status | msgid | Current msgstr | Suggested TR |
|--------|-------|----------------|-------------|
| missing | "River Plots & Alluvial Diagrams" | "" | "Nehir Grafikleri ve Allüvyal Diyagramlar" |
| missing | "Visualize flows, transitions, and categorical changes across time or stages:" | "" | "Akışları, geçişleri ve zaman/aşamalar arası kategrik değişimleri görselleştirin:" |
| missing | "Required packages missing: " | "" | "Gerekli paketler eksik: " |
| missing | "Required packages not available" | "" | "Gerekli paketler mevcut değil" |
| missing | "Missing Dependencies" | "" | "Eksik Bağımlılıklar" |
| missing | "Required Data Setup:" | "" | "Gerekli Veri Kurulumu:" |
| missing | "Analysis Error" | "" | "Analiz Hatası" |
| missing | "Error:" | "" | "Hata:" |
| missing | "Please check your data format and variable selections." | "" | "Veri formatınızı ve değişken seçimlerinizi kontrol edin." |
| missing | "Data Processing Diagnostics" | "" | "Veri İşleme Tanıları" |
| missing | "CRAN Riverplot Object Structure" | "" | "CRAN Riverplot Nesne Yapısı" |
| missing | "At least one strata variable is required" | "" | "En az bir katman değişkeni gereklidir" |
| missing | "Cannot auto-detect data format: provide time variable for long format or multiple strata for wide format" | "" | "Veri formatı otomatik algılanamıyor: uzun format için zaman değişkeni veya geniş format için çoklu katman sağlayın" |
| missing | "Time variable is required for long format data" | "" | "Uzun format veri için zaman değişkeni gereklidir" |
| missing | "Long format requires exactly one strata variable" | "" | "Uzun format tam olarak bir katman değişkeni gerektirir" |
| missing | "Wide format requires at least two strata variables" | "" | "Geniş format en az iki katman değişkeni gerektirir" |
| missing | "At least 2 rows of data required" | "" | "En az 2 satır veri gereklidir" |

---

## 5) Turkish Clinical/Statistical Glossary

```text
River Plot → Nehir Grafiği
Alluvial Diagram → Allüvyal Diyagram  
Flow → Akış
Transition → Geçiş
Strata → Katman/Tabaka
Node → Düğüm
Edge → Kenar/Bağlantı
Time Variable → Zaman Değişkeni
Long Format → Uzun Format
Wide Format → Geniş Format
Data Processing → Veri İşleme
Diagnostics → Tanılar
Dependencies → Bağımlılıklar
Validation → Doğrulama
Error → Hata
Warning → Uyarı
Required → Gerekli
Missing → Eksik
Available → Mevcut
```

**Style Guidelines for Turkish:**
- Use clinical/medical terminology familiar to pathologists and clinicians
- Prefer Turkish equivalents where established (e.g., "Hata" not "Error")
- Maintain consistency with existing ClinicoPath translations
- Use formal tone appropriate for professional medical software

---

## 6) QA Checklist

- ✅ NAMESPACE imports `.` helper
- ✅ All target files exist
- ⚠️ **Action needed**: User-visible strings in R backend need `.()` wrapping
- ✅ Translation catalogs (en.po, tr.po) exist but need updating after patches
- ⚠️ **Action needed**: Apply patches and regenerate catalogs
- ⚠️ **Action needed**: Review/improve existing Turkish translations for riverplot entries

---

## 7) Implementation Steps

### Step 1: Apply R Code Patches
Apply the diff patches above to `R/riverplot.b.R` to wrap all translatable strings.

### Step 2: Update Translation Catalogs
```r
# After applying patches, regenerate catalogs
jmvtools::i18nUpdate("en")
jmvtools::i18nUpdate("tr")
```

### Step 3: Translate Missing Strings
Edit `jamovi/i18n/tr.po` to add Turkish translations using the table above.

### Step 4: Validate Translations
```bash
# Check for untranslated strings
grep -A1 'msgid.*riverplot' jamovi/i18n/tr.po | grep 'msgstr ""'
```

---

## 8) Ready-to-Run Snippets

**Update catalogs after patches**
```r
jmvtools::i18nCreate("en"); jmvtools::i18nUpdate("en")  
jmvtools::i18nCreate("tr"); jmvtools::i18nUpdate("tr")
```

**Find unwrapped strings (heuristic)**
```bash
grep -nE '(stop|warning|paste0?)\s*\([^)]*["\'][^"\']*["\']' R/riverplot.b.R | grep -v '\\.\('
```

**Validate Turkish translations**
```bash
# Count untranslated riverplot entries
grep -c 'msgstr ""' jamovi/i18n/tr.po
```

---

## 9) Priority Translations for riverplot

**High Priority** (User-facing UI elements):
1. "River Plots & Alluvial Diagrams" → "Nehir Grafikleri ve Allüvyal Diyagramlar"
2. Error messages and validation warnings
3. Welcome/help text content

**Medium Priority** (Technical messages):
1. Diagnostic information
2. Package dependency messages  
3. CRAN integration messages

**Low Priority** (Advanced features):
1. Riverplot object structure labels
2. Advanced diagnostic details

---

## Summary

The riverplot function requires significant i18n work with **16+ translatable strings** identified. The function has comprehensive user-facing content including welcome messages, error handling, and diagnostic output that all need Turkish translation. Priority should be given to error messages and main UI elements that users will encounter most frequently.

Current status: **🔴 Needs Implementation** - Patches required before translation can be completed.