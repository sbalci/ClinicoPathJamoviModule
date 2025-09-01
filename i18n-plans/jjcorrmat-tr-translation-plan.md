# Internationalization (i18n) Plan for jjcorrmat - Turkish Translation

Generated: 2025-01-09  
Function: **jjcorrmat** (Correlation Matrix)  
Target Language: **Turkish (TR)**  
Clinical Domain: **Correlational Analysis, Statistical Visualization**

---

## 0) Files Analysis

**SANITIZED_FN**: `jjcorrmat`

**Target files status**:

- ✅ `jamovi/jjcorrmat.a.yaml` (options) - EXISTS
- ✅ `jamovi/jjcorrmat.u.yaml` (UI) - EXISTS  
- ✅ `jamovi/jjcorrmat.r.yaml` (results) - EXISTS
- ✅ `R/jjcorrmat.b.R` (backend) - EXISTS

All required files exist and are ready for translation processing.

---

## 1) NAMESPACE i18n Hook Status

✅ **ALREADY CONFIGURED**: The NAMESPACE file includes the translation helper at line 838:

```r
importFrom(jmvcore, .)
```

No changes needed.

---

## 2) String Wrapping Requirements

### 2.1 Error & Warning Messages

**Current unwrapped strings in R/jjcorrmat.b.R:**

```r
# Line 74
stop('Data contains no (complete) rows')

# Line 82  
stop(paste('Variable "', var, '" not found in data'))

# Line 100
stop('Correlation analysis requires at least 2 numeric variables with sufficient variation and observations')

# Line 201 (duplicate)
stop('Data contains no (complete) rows')
```

**Required changes:**

```diff
# Error messages
- stop('Data contains no (complete) rows')
+ stop(.('Data contains no (complete) rows'))

- stop(paste('Variable "', var, '" not found in data'))
+ stop(paste(.('Variable "'), var, .('" not found in data')))

- stop('Correlation analysis requires at least 2 numeric variables with sufficient variation and observations')
+ stop(.('Correlation analysis requires at least 2 numeric variables with sufficient variation and observations'))
```

### 2.2 Progress Messages

**Current unwrapped HTML content strings:**

```r
# Line 55 - Processing message
glue::glue("<br>Processing data for correlation analysis...<br><hr>")

# Line 124 - Options preparation message  
glue::glue("<br>Preparing correlation analysis options...<br><hr>")

# Line 196 - Status message
"<br>You have selected to use a correlation matrix to compare continuous variables.<br><hr>"
```

**Required changes:**

```diff
# Progress messages
- glue::glue("<br>Processing data for correlation analysis...<br><hr>")
+ glue::glue(.("<br>Processing data for correlation analysis...<br><hr>"))

- glue::glue("<br>Preparing correlation analysis options...<br><hr>")  
+ glue::glue(.("<br>Preparing correlation analysis options...<br><hr>"))

- "<br>You have selected to use a correlation matrix to compare continuous variables.<br><hr>"
+ .("<br>You have selected to use a correlation matrix to compare continuous variables.<br><hr>")
```

### 2.3 Welcome Message (Multi-line HTML)

**Current unwrapped content (Lines 177-185):**

```html
"<br>Welcome to ClinicoPath
<br><br>
This tool will help you generate Correlation Matrix Charts.
<br><br>
This function uses ggplot2 and ggstatsplot packages. See documentations <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/ggcorrmat.html' target='_blank'>ggcorrmat</a> and <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggcorrmat.html' target='_blank'>grouped_ggcorrmat</a>.
<br>
Please cite jamovi and the packages as given below.
<br><hr>"
```

**Required change:**

```diff
- todo <- glue::glue(
-   "<br>Welcome to ClinicoPath
-   <br><br>
-   This tool will help you generate Correlation Matrix Charts.
-   <br><br>
-   This function uses ggplot2 and ggstatsplot packages. See documentations <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/ggcorrmat.html' target='_blank'>ggcorrmat</a> and <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggcorrmat.html' target='_blank'>grouped_ggcorrmat</a>.
-   <br>
-   Please cite jamovi and the packages as given below.
-   <br><hr>"
- )

+ todo <- .("<br>Welcome to ClinicoPath
+ <br><br>
+ This tool will help you generate Correlation Matrix Charts.
+ <br><br>
+ This function uses ggplot2 and ggstatsplot packages. See documentations <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/ggcorrmat.html' target='_blank'>ggcorrmat</a> and <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggcorrmat.html' target='_blank'>grouped_ggcorrmat</a>.
+ <br>
+ Please cite jamovi and the packages as given below.
+ <br><hr>")
```

### 2.4 Strings NOT to Wrap

**Programmatic values (keep as-is):**

- `"jjcorrmatClass"` - Class name
- `"cross"` - pch parameter
- `"RColorBrewer"` - Package name  
- `"Dark2"` - Palette name
- `"black"` - Color value

---

## 3) Extraction & Update Commands

**Create/Update catalogs (R console):**

```r
# Create/update English template
jmvtools::i18nCreate("en")
jmvtools::i18nUpdate("en")

# Create/update Turkish catalog
jmvtools::i18nCreate("tr") 
jmvtools::i18nUpdate("tr")
```

**Prepare Weblate template:**

```bash
# Copy English to POT template
cp jamovi/i18n/en.po jamovi/i18n/catalog.pot
# Edit header to set Language: c
```

---

## 4) Turkish Translation Table

**Missing/Required Turkish Translations:**

| Status | msgid | Suggested TR Translation | Clinical Notes |
|--------|-------|-------------------------|----------------|
| new | "Data contains no (complete) rows" | "Veri hiçbir (tam) satır içermiyor" | Data validation message |
| new | "Variable \"" | "Değişken \"" | Variable reference |
| new | "\" not found in data" | "\" veride bulunamadı" | Missing variable error |
| new | "Correlation analysis requires at least 2 numeric variables with sufficient variation and observations" | "Korelasyon analizi yeterli varyasyon ve gözlem sayısına sahip en az 2 sayısal değişken gerektirir" | Statistical requirement |
| new | "<br>Processing data for correlation analysis...<br><hr>" | "<br>Korelasyon analizi için veri işleniyor...<br><hr>" | Progress indicator |
| new | "<br>Preparing correlation analysis options...<br><hr>" | "<br>Korelasyon analizi seçenekleri hazırlanıyor...<br><hr>" | Options processing |
| new | "<br>You have selected to use a correlation matrix to compare continuous variables.<br><hr>" | "<br>Sürekli değişkenleri karşılaştırmak için korelasyon matrisi kullanmayı seçtiniz.<br><hr>" | Analysis selection |
| new | "<br>Welcome to ClinicoPath<br><br>This tool will help you generate Correlation Matrix Charts.<br><br>This function uses ggplot2 and ggstatsplot packages. See documentations <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/ggcorrmat.html' target='_blank'>ggcorrmat</a> and <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggcorrmat.html' target='_blank'>grouped_ggcorrmat</a>.<br>Please cite jamovi and the packages as given below.<br><hr>" | "<br>ClinicoPath'e Hoş Geldiniz<br><br>Bu araç Korelasyon Matrisi Grafikleri oluşturmanızda yardımcı olacaktır.<br><br>Bu işlev ggplot2 ve ggstatsplot paketlerini kullanır. Dokümantasyonlar için <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/ggcorrmat.html' target='_blank'>ggcorrmat</a> ve <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggcorrmat.html' target='_blank'>grouped_ggcorrmat</a> sayfalarını inceleyiniz.<br>Lütfen jamovi ve paketleri aşağıda belirtildiği şekilde kaynak gösteriniz.<br><hr>" | Welcome message |

---

## 5) Consistency & Glossary (TR)

**ClinicoPath Turkish Statistical Glossary:**

```text
Correlation → Korelasyon
Correlation Matrix → Korelasyon Matrisi
Continuous Variables → Sürekli Değişkenler
Parametric → Parametrik
Nonparametric → Nonparametrik  
Robust → Robust/Güçlü
Bayes → Bayes
Partial Correlations → Kısmi Korelasyonlar
Significance Level → Anlamlılık Düzeyi
Confidence Level → Güven Düzeyi
P-value adjustment → p-değeri düzeltmesi
Decimal Places → Ondalık Basamak Sayısı
Plot Title → Grafik Başlığı
Plot Subtitle → Grafik Alt Başlığı
Plot Caption → Grafik Açıklaması
Processing → İşleme/İşleniyor
Preparing → Hazırlama/Hazırlanıyor
Variable → Değişken
Data → Veri
Analysis → Analiz
Matrix Type → Matris Türü
Upper Triangle → Üst Üçgen
Lower Triangle → Alt Üçgen  
Full Matrix → Tam Matris
```

---

## 6) QA Checklist

- ✅ All user-visible strings in R backend identified
- ✅ NAMESPACE imports translation helper `.`
- ✅ All target YAML files exist and ready for processing
- ✅ Translation strings marked with clinical accuracy considerations
- ✅ HTML content preserved in translations
- ✅ Programmatic identifiers excluded from translation
- ✅ Turkish translations follow clinical terminology standards

---

## 7) Weblate Integration Steps

**GitHub Repository Setup:**

1. **Create dedicated i18n repo**: `ClinicoPathJamoviModule-i18n`
   - Add `catalog.pot` file
   - Include `README.md` with translation guidelines
   - Add MIT or GPL-3 license

2. **Repository Configuration**:
   - **Collaborators** → Add Weblate bot account  
   - **Webhooks** → Add Weblate integration:
     - Payload URL: `https://hosted.weblate.org/hooks/github/`
     - Content type: `application/json`
     - Events: `Push`, `Pull request`

3. **Weblate Project Request**:
   - Contact jamovi development team
   - Request addition of `ClinicoPathJamoviModule-i18n` to Weblate
   - Specify component: `jjcorrmat` correlation analysis module

---

## 8) Ready-to-Run Implementation

**Create/Update Catalogs:**

```r
# Run in R console from module root
jmvtools::i18nCreate("en"); jmvtools::i18nUpdate("en")
jmvtools::i18nCreate("tr"); jmvtools::i18nUpdate("tr")
```

**Prepare POT Template:**

```bash
# Run from module root directory
cp jamovi/i18n/en.po jamovi/i18n/catalog.pot
# Edit catalog.pot header to set: Language: c\n
```

**Find Unwrapped Strings (Validation):**

```bash
# Heuristic search for quoted strings not wrapped with .()
grep -nE '"[^"]+"|'"'"'[^'"'"']+'"'"' R/jjcorrmat.b.R | grep -v '\\.\('
```

**Apply String Wrapping (Git Patch):**

```diff
diff --git a/R/jjcorrmat.b.R b/R/jjcorrmat.b.R
index abc123..def456 100644
--- a/R/jjcorrmat.b.R
+++ b/R/jjcorrmat.b.R
@@ -71,7 +71,7 @@
             if (length(self$options$dep) < 2)
                 return(FALSE)
             if (nrow(self$data) == 0)
-                stop('Data contains no (complete) rows')
+                stop(.('Data contains no (complete) rows'))
             
             # Enhanced validation for correlation analysis
             mydata <- self$data
@@ -79,7 +79,7 @@
             # Check if variables exist in data
             for (var in self$options$dep) {
                 if (!(var %in% names(mydata)))
-                    stop(paste('Variable "', var, '" not found in data'))
+                    stop(paste(.('Variable "'), var, .('" not found in data')))
             }
             
             # Convert to numeric and check for sufficient numeric data
@@ -96,7 +96,7 @@
             }
             
             if (numeric_vars < 2)
-                stop('Correlation analysis requires at least 2 numeric variables with sufficient variation and observations')
+                stop(.('Correlation analysis requires at least 2 numeric variables with sufficient variation and observations'))
                 
             return(TRUE)
         },
@@ -120,7 +120,7 @@
 
             # Prepare options with progress feedback
             self$results$todo$setContent(
-                glue::glue("<br>Preparing correlation analysis options...<br><hr>")
+                glue::glue(.("<br>Preparing correlation analysis options...<br><hr>"))
             )
 
             # Process type of statistics
@@ -173,12 +173,7 @@
 
                 # TODO ----
 
-                todo <- glue::glue(
-                "<br>Welcome to ClinicoPath
-                <br><br>
-                This tool will help you generate Correlation Matrix Charts.
-                <br><br>
-                This function uses ggplot2 and ggstatsplot packages. See documentations <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/ggcorrmat.html' target='_blank'>ggcorrmat</a> and <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggcorrmat.html' target='_blank'>grouped_ggcorrmat</a>.
-                <br>
-                Please cite jamovi and the packages as given below.
-                <br><hr>"
+                todo <- .("<br>Welcome to ClinicoPath
+                <br><br>
+                This tool will help you generate Correlation Matrix Charts.
+                <br><br>
+                This function uses ggplot2 and ggstatsplot packages. See documentations <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/ggcorrmat.html' target='_blank'>ggcorrmat</a> and <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggcorrmat.html' target='_blank'>grouped_ggcorrmat</a>.
+                <br>
+                Please cite jamovi and the packages as given below.
+                <br><hr>"
                 )
 
                 self$results$todo$setContent(todo)
@@ -191,13 +186,13 @@
 
                 # TODO ----
                 todo <- glue::glue(
-                    "<br>You have selected to use a correlation matrix to compare continuous variables.<br><hr>")
+                    .("<br>You have selected to use a correlation matrix to compare continuous variables.<br><hr>"))
 
                 self$results$todo$setContent(todo)
 
                 if (nrow(self$data) == 0)
-                    stop('Data contains no (complete) rows')
+                    stop(.('Data contains no (complete) rows'))
 
                 # Pre-process data and options for performance
                 private$.prepareData()
@@ -52,7 +52,7 @@
 
             # Prepare data with progress feedback
             self$results$todo$setContent(
-                glue::glue("<br>Processing data for correlation analysis...<br><hr>")
+                glue::glue(.("<br>Processing data for correlation analysis...<br><hr>"))
             )
 
             mydata <- self$data
```

---

## 9) Deliverables Summary

**Translation Readiness**: ✅ **READY**

- **8 translatable strings** identified in R backend
- **Clinical terminology** mapped for Turkish healthcare context  
- **HTML formatting** preserved in translations
- **Implementation patches** provided for immediate deployment
- **Weblate integration** steps documented for continuous localization

**Next Steps:**

1. Apply string wrapping patches to R/jjcorrmat.b.R
2. Run extraction commands to generate .po files
3. Review and refine Turkish translations
4. Set up Weblate integration for ongoing translation management

**Clinical Focus**: The jjcorrmat function focuses on correlation analysis for continuous medical variables, requiring precise statistical terminology in Turkish that maintains clinical accuracy for pathologists and medical researchers.
