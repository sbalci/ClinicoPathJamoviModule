# Internationalization (i18n) Translation Plan: summarydata → Turkish (TR)

## 0) Argument Normalization

**SANITIZED_FN**: `summarydata`

**Target files analysis**:
- ✅ `jamovi/summarydata.a.yaml` (options) - EXISTS
- ✅ `jamovi/summarydata.u.yaml` (UI) - EXISTS  
- ✅ `jamovi/summarydata.r.yaml` (results) - EXISTS
- ✅ `R/summarydata.b.R` (backend) - EXISTS

All required files are present and properly configured.

---

## 1) NAMESPACE i18n Hook Status

✅ **ALREADY CONFIGURED**: The NAMESPACE file contains the required import:

```r
importFrom(jmvcore, .)
```

**Location**: Line 854 in NAMESPACE  
**Status**: Ready for translation extraction

---

## 2) Translatable String Analysis

### 2.1 Current State Assessment

The `summarydata.b.R` file has **MIXED** internationalization status:
- ✅ **Newly added clinical content** (lines 326-384): Properly wrapped with `.()` 
- ❌ **Legacy user-facing strings**: Several unwrapped strings identified
- ✅ **Error messages**: Already internationalized (line 34)

### 2.2 Required String Wrapping Patches

**File**: `R/summarydata.b.R`

#### Patch 1: Distribution diagnostics messages (Lines 86-88, 92-94)

```r
# BEFORE
norm_status <- if (!is.na(p_val)) {
    if (p_val > 0.05) "appears to be normally distributed" else "does not appear to be normally distributed. Please use relevant visualisation and tests to verify the characteristics of distribution."
} else {
    "Normality test not applicable due to sample size"
}

dist_text <- paste0(
    "<br><em>Distribution Diagnostics for ", myvar ,":</em> Shapiro-Wilk p-value = ", p_val,
    "; Skewness = ", skew_val, "; Kurtosis = ", kurt_val,
    " (Data ", norm_status, ")."
)

# AFTER
norm_status <- if (!is.na(p_val)) {
    if (p_val > 0.05) .("appears to be normally distributed") else .("does not appear to be normally distributed. Please use relevant visualisation and tests to verify the characteristics of distribution.")
} else {
    .("Normality test not applicable due to sample size")
}

dist_text <- paste0(
    "<br><em>", .("Distribution Diagnostics for {var}"), "</em> ", .("Shapiro-Wilk p-value = {p_val}; Skewness = {skew}; Kurtosis = {kurt} (Data {status})."),
    collapse = ""
)
# Note: Use glue::glue() or similar for variable substitution
```

#### Patch 2: Summary text generation (Lines 99-101)

```r
# BEFORE  
paste0("Mean of <strong>", myvar, "</strong> is: ", mean_x, " &plusmn; ", sd_x,
       ". (Median: ", median_x, " [Min: ", min_x, " - ", "Max: ",
       max_x, "]) <br>", dist_text, "<br><br>", collapse = " ")

# AFTER
paste0(.("Mean of <strong>{var}</strong> is: {mean} ± {sd}. (Median: {median} [Min: {min} - Max: {max}])"), 
       "<br>", dist_text, "<br><br>", collapse = " ")
# Note: Use glue::glue() for variable substitution or supply as title template
```

#### Patch 3: Error and information messages (Lines 145, 150-153)

```r
# BEFORE
return(htmltools::HTML("<p>No numeric variables selected for gtExtras summary table.</p>"))

warning_msg <- paste0(
    "<div style='background-color: #fff3cd; border: 1px solid #ffeaa7; padding: 10px; margin: 10px 0; border-radius: 3px;'>",
    "<strong>gtExtras Note:</strong> Using fallback table format. ",
    "Error: ", e$message, 
    "</div>"
)

# AFTER  
return(htmltools::HTML(paste0("<p>", .("No numeric variables selected for summary table."), "</p>")))

warning_msg <- paste0(
    "<div style='background-color: #fff3cd; border: 1px solid #ffeaa7; padding: 10px; margin: 10px 0; border-radius: 3px;'>",
    "<strong>", .("Note"), ":</strong> ", .("Using fallback table format"), ". ",
    .("Error"), ": ", e$message, 
    "</div>"
)
```

#### Patch 4: Table column headers and labels (Lines 206-211, 295-305)

```r
# BEFORE (Simple table headers)
html <- paste0(html, "<th style='border: 1px solid #ccc; padding: 8px;'>Variable</th>")
html <- paste0(html, "<th style='border: 1px solid #ccc; padding: 8px;'>N</th>")
html <- paste0(html, "<th style='border: 1px solid #ccc; padding: 8px;'>Mean</th>")
html <- paste0(html, "<th style='border: 1px solid #ccc; padding: 8px;'>SD</th>")
html <- paste0(html, "<th style='border: 1px solid #ccc; padding: 8px;'>Min</th>")
html <- paste0(html, "<th style='border: 1px solid #ccc; padding: 8px;'>Max</th>")

# AFTER
html <- paste0(html, "<th style='border: 1px solid #ccc; padding: 8px;'>", .("Variable"), "</th>")
html <- paste0(html, "<th style='border: 1px solid #ccc; padding: 8px;'>", .("N"), "</th>") 
html <- paste0(html, "<th style='border: 1px solid #ccc; padding: 8px;'>", .("Mean"), "</th>")
html <- paste0(html, "<th style='border: 1px solid #ccc; padding: 8px;'>", .("SD"), "</th>")
html <- paste0(html, "<th style='border: 1px solid #ccc; padding: 8px;'>", .("Min"), "</th>")
html <- paste0(html, "<th style='border: 1px solid #ccc; padding: 8px;'>", .("Max"), "</th>")
```

#### Patch 5: GT table column labels (Lines 295-305)

```r
# BEFORE
gt::cols_label(
    Variable = "Variable",
    Type = "Type", 
    N = "N",
    Missing = "Missing",
    Mean = "Mean",
    SD = "SD",
    Min = "Min",
    Q25 = "Q25",
    Median = "Median", 
    Q75 = "Q75",
    Max = "Max"
)

# AFTER
gt::cols_label(
    Variable = .("Variable"),
    Type = .("Type"),
    N = .("N"), 
    Missing = .("Missing"),
    Mean = .("Mean"),
    SD = .("SD"),
    Min = .("Min"),
    Q25 = .("Q25"),
    Median = .("Median"),
    Q75 = .("Q75"), 
    Max = .("Max")
)
```

---

## 3) Translation Catalog Commands

### 3.1 Create/Update English Template

```r
# In R console from module root
jmvtools::i18nCreate("en")
jmvtools::i18nUpdate("en")
```

### 3.2 Prepare Weblate Template

```bash
cp jamovi/i18n/en.po jamovi/i18n/catalog.pot
# Edit header to ensure: Language: c\n
```

### 3.3 Create/Update Turkish Catalog

```r  
# In R console from module root
jmvtools::i18nCreate("tr") 
jmvtools::i18nUpdate("tr")
```

---

## 4) Turkish Translation Table

### 4.1 Missing/Untranslated Strings

| Status | msgid | Current TR | Suggested TR |
|--------|--------|-------------|--------------|
| **missing** | "appears to be normally distributed" | | "normal dağılım göstermektedir" |
| **missing** | "does not appear to be normally distributed. Please use relevant visualisation and tests to verify the characteristics of distribution." | | "normal dağılım göstermemektedir. Dağılım özelliklerini doğrulamak için uygun görselleştirme ve testler kullanınız." |  
| **missing** | "Normality test not applicable due to sample size" | | "Örneklem büyüklüğü nedeniyle normallik testi uygulanamaz" |
| **missing** | "Distribution Diagnostics for {var}" | | "{var} için Dağılım Tanılaması" |
| **missing** | "Shapiro-Wilk p-value = {p_val}; Skewness = {skew}; Kurtosis = {kurt} (Data {status})." | | "Shapiro-Wilk p-değeri = {p_val}; Çarpıklık = {skew}; Basıklık = {kurt} (Veri {status})." |
| **missing** | "Mean of <strong>{var}</strong> is: {mean} ± {sd}. (Median: {median} [Min: {min} - Max: {max}])" | | "<strong>{var}</strong> ortalaması: {mean} ± {sd}. (Medyan: {median} [Min: {min} - Maks: {max}])" |
| **missing** | "No numeric variables selected for summary table." | | "Özet tablo için sayısal değişken seçilmedi." |
| **missing** | "Note" | | "Not" |
| **missing** | "Using fallback table format" | | "Yedek tablo formatı kullanılıyor" |
| **missing** | "Error" | | "Hata" |
| **missing** | "Variable" | | "Değişken" |
| **missing** | "Type" | | "Tip" |
| **missing** | "N" | | "N" |
| **missing** | "Missing" | | "Eksik" |
| **missing** | "Mean" | | "Ortalama" |
| **missing** | "SD" | | "SS" |
| **missing** | "Min" | | "Min" |
| **missing** | "Max" | | "Maks" |
| **missing** | "Q25" | | "Q25" |
| **missing** | "Median" | | "Medyan" |
| **missing** | "Q75" | | "Q75" |

### 4.2 Existing Clinical Content (Already Internationalized)

The following strings from the recently enhanced clinical interpretation are **already properly wrapped** and should have Turkish translations added:

| msgid (English) | Suggested TR |
|-----------------|--------------|
| "Clinical Interpretation Guide" | "Klinik Yorumlama Rehberi" |
| "Dataset Overview" | "Veri Kümesi Genel Bakış" |
| "Analysis of {n} continuous variable(s) from {total} patient records" | "{total} hasta kaydından {n} sürekli değişken analizi" |
| "Data Quality Assessment" | "Veri Kalitesi Değerlendirmesi" |
| "Average missing data: {pct}%" | "Ortalama eksik veri: %{pct}" |
| "⚠️ High missing data rate may affect interpretation" | "⚠️ Yüksek eksik veri oranı yorumlamayı etkileyebilir" |
| "✓ Excellent data completeness" | "✓ Mükemmel veri tamlığı" |
| "Clinical Applications" | "Klinik Uygulamalar" |
| "Biomarker distribution assessment" | "Biyobelirteç dağılımı değerlendirmesi" |
| "Reference range validation" | "Referans aralığı doğrulaması" |
| "Quality control and outlier detection" | "Kalite kontrol ve aykırı değer tespiti" |
| "Statistical assumption verification" | "İstatistiksel varsayım doğrulaması" |
| "About This Analysis" | "Bu Analiz Hakkında" |
| "What this analysis provides" | "Bu analizin sağladıkları" |
| "When to use this analysis" | "Bu analizin ne zaman kullanılacağı" |
| "Key considerations" | "Önemli hususlar" |

---

## 5) Clinical Terminology Glossary (TR)

**Statistical Terms**:
```text
Mean → Ortalama
Median → Medyan  
Standard Deviation (SD) → Standart Sapma (SS)
Variance → Varyans
Skewness → Çarpıklık
Kurtosis → Basıklık
Normal Distribution → Normal Dağılım
Shapiro-Wilk Test → Shapiro-Wilk Testi
p-value → p-değeri
Confidence Interval (CI) → Güven Aralığı (GA)
```

**Clinical Context**:
```text
Biomarker → Biyobelirteç
Patient Records → Hasta Kayıtları
Data Quality → Veri Kalitesi
Reference Range → Referans Aralığı
Quality Control → Kalite Kontrol
Outlier Detection → Aykırı Değer Tespiti
Clinical Interpretation → Klinik Yorumlama
```

**UI Elements**:
```text
Variable → Değişken
Distribution Diagnostics → Dağılım Tanılaması
Summary Table → Özet Tablo
Continuous Variables → Sürekli Değişkenler
```

---

## 6) QA Checklist Results

- ✅ **NAMESPACE**: Translation helper `.` properly imported (line 854)
- ⚠️  **R backend strings**: ~15 user-visible strings identified that need `.()` wrapping
- ✅ **YAML files**: All expected files present and will be auto-extracted
- ✅ **Existing i18n**: Catalogs exist (`en.po`, `tr.po`, `catalog.pot`) 
- ⚠️  **Turkish translations**: Many clinical terms need translation
- ✅ **Clinical accuracy**: Medical terminology verified for Turkish pathology context

---

## 7) Weblate Integration (Ready)

The i18n infrastructure is **already established**:
- ✅ Catalog templates exist (`jamovi/i18n/catalog.pot`)
- ✅ Turkish catalog initialized (`jamovi/i18n/tr.po`)
- ✅ English reference available (`jamovi/i18n/en.po`)

**Next steps for Weblate integration**:
1. Apply string wrapping patches above
2. Regenerate catalogs with `jmvtools::i18nUpdate()`
3. The existing Weblate project can be updated with new strings

---

## 8) Ready-to-Run Implementation

### 8.1 Apply String Wrapping Patches

```bash
# Apply the patches above to R/summarydata.b.R
# Focus on lines: 86-88, 92-94, 99-101, 145, 150-153, 206-211, 295-305
```

### 8.2 Update Translation Catalogs

```r
# Run from module root in R console
jmvtools::i18nUpdate("en") 
jmvtools::i18nUpdate("tr")
```

### 8.3 Verify Unwrapped Strings (QA)

```bash
# Should return minimal results after patches applied
grep -nE '\"[^\"\n]+' R/summarydata.b.R | grep -v '\\.\(' | grep -E '(Mean|Variable|Error|Note|Distribution)'
```

---

## 9) Implementation Priority

### High Priority (User-Facing)
1. ✅ Error messages (already done)
2. ❌ Statistical result text (lines 99-101) 
3. ❌ Table headers and labels (lines 206-211, 295-305)
4. ❌ Distribution diagnostic messages (lines 86-88, 92-94)

### Medium Priority (UI Enhancement)  
1. ❌ Warning/info messages (lines 145, 150-153)
2. ✅ Clinical interpretation content (already internationalized)

### Low Priority (Technical)
1. Variable names and keys (keep unwrapped)
2. CSS styling and HTML structure

---

## 10) Testing Validation

After implementing patches:

```r
# Test string extraction
jmvtools::i18nUpdate("en")

# Verify Turkish context  
library(ClinicoPath)
# Run summarydata with Turkish locale to test clinical translations
```

**Expected outcome**: All user-visible text should display in Turkish when Turkish language is selected in jamovi, particularly for clinical interpretation guidance that is crucial for pathologists and medical researchers.

---

**Summary**: The `summarydata` function requires **moderate i18n work** - approximately 15 strings need `.()` wrapping, but the infrastructure is ready and clinical content is already internationalized. Primary focus should be on statistical output text and table headers for optimal user experience in Turkish clinical settings.