# Turkish Internationalization Plan for advancedraincloud

**Generated:** 2025-08-30  
**Target Language:** Turkish (TR)  
**Function:** advancedraincloud - Advanced Raincloud Plot  

## 0) Sanitized Function Name

**SANITIZED_FN:** `advancedraincloud`

**Target Files Found:**
- ✅ `jamovi/advancedraincloud.a.yaml` (options) - 12.3KB
- ✅ `jamovi/advancedraincloud.u.yaml` (UI) - 5.8KB  
- ✅ `jamovi/advancedraincloud.r.yaml` (results) - 2.0KB
- ✅ `R/advancedraincloud.b.R` (backend) - 67.2KB
- ✅ `R/advancedraincloud.h.R` (header) - 32.2KB (auto-generated)

## 1) NAMESPACE i18n Hook Status

✅ **NAMESPACE correctly includes translation helper:**
```r
importFrom(jmvcore, .)  # Line 835
```

No action required - the translation helper is already properly imported.

## 2) String Wrapping Analysis

### Current Status: 🟡 Partial Implementation

**Strings Already Wrapped:** Many HTML content strings are already wrapped
**Strings Needing Wrapping:** Error messages, validation messages, warnings

### 2.1 Error & Warning Messages to Wrap

**Required Patches for `R/advancedraincloud.b.R`:**

```r
# Line 26 - Validation error
- jmvcore::reject(paste(name, "must be a numeric value between", min, "and", max), code = "")
+ jmvcore::reject(paste(name, .("must be a numeric value between"), min, .("and"), max), code = "")

# Line 34 - Validation error  
- jmvcore::reject(paste(name, "must be a positive numeric value"), code = "")
+ jmvcore::reject(paste(name, .("must be a positive numeric value")), code = "")

# Line 41 - Validation error
- jmvcore::reject(paste(name, "must be a numeric value"), code = "")
+ jmvcore::reject(paste(name, .("must be a numeric value")), code = "")

# Line 110 - Reference range validation
- jmvcore::reject("Reference range minimum must be less than maximum", code = "")
+ jmvcore::reject(.("Reference range minimum must be less than maximum"), code = "")

# Line 171 - Data error
- stop("Error: The provided dataset contains no complete rows. Please check your data and try again.")
+ stop(.("Error: The provided dataset contains no complete rows. Please check your data and try again."))

# Line 210 - No complete cases error
- stop("Error: No complete cases found for the selected variables.")
+ stop(.("Error: No complete cases found for the selected variables."))

# Line 334 - Plot warning
- warning("No groups found in the data")
+ warning(.("No groups found in the data"))

# Line 340 - Plot warning  
- warning("No data available for plotting")
+ warning(.("No data available for plotting"))

# Line 360 - Plot creation error
- stop("Failed to create base plot: ", e$message, ". Please check your variable selections.")
+ stop(.("Failed to create base plot: "), e$message, _(". Please check your variable selections."))

# Line 371 - Large groups warning
- warning("Large number of groups (", n_groups, ") may cause display issues. Consider grouping your data.")
+ warning(.("Large number of groups ("), n_groups, _(") may cause display issues. Consider grouping your data."))

# Line 399 - ID variable warning
- warning("ID variable contains NA values. Longitudinal connections disabled.")
+ warning(.("ID variable contains NA values. Longitudinal connections disabled."))

# Line 414 - Covariate warning
- warning("Covariate variable contains NA values. Covariate mapping disabled.")
+ warning(.("Covariate variable contains NA values. Covariate mapping disabled."))

# Line 426 - Fallback warning
- warning("Using standard geom fallback due to data structure that may cause ggrain issues.")
+ warning(.("Using standard geom fallback due to data structure that may cause ggrain issues."))
```

### 2.2 Validation Parameter Names

**Current parameter name strings (lines 76-102) need wrapping:**

```r
# Wrap user-visible parameter names
- private$.validate_numeric_range(self$options$point_alpha, "Point transparency", 0, 1)
+ private$.validate_numeric_range(self$options$point_alpha, .("Point transparency"), 0, 1)

- private$.validate_numeric_range(self$options$violin_alpha, "Violin transparency", 0, 1) 
+ private$.validate_numeric_range(self$options$violin_alpha, .("Violin transparency"), 0, 1)

- private$.validate_numeric_range(self$options$point_size, "Point size", 0.1, 5)
+ private$.validate_numeric_range(self$options$point_size, .("Point size"), 0.1, 5)

- private$.validate_numeric_range(self$options$boxplot_width, "Boxplot width", 0.1, 1)
+ private$.validate_numeric_range(self$options$boxplot_width, .("Boxplot width"), 0.1, 1)

- private$.validate_numeric_type(self$options$jitter_seed, "Jitter seed")
+ private$.validate_numeric_type(self$options$jitter_seed, .("Jitter seed"))

# Continue for all parameter names...
```

## 3) Translation Commands

### 3.1 Create/Update English Template
```r
# In R console  
jmvtools::i18nCreate("en")
jmvtools::i18nUpdate("en")
```

### 3.2 Prepare POT Template
```bash
cp jamovi/i18n/en.po jamovi/i18n/catalog.pot
# Edit header: Language: c\n
```

### 3.3 Create/Update Turkish Catalog
```r  
# In R console
jmvtools::i18nCreate("tr")  
jmvtools::i18nUpdate("tr")
```

## 4) Turkish Translation Table

**Clinical Raincloud Plot Terminology:**

| Status | msgid | Current msgstr | Suggested Turkish |
|--------|-------|----------------|------------------|
| missing | "Point transparency" | | "Nokta şeffaflığı" |
| missing | "Violin transparency" | | "Keman şeffaflığı" |  
| missing | "Point size" | | "Nokta boyutu" |
| missing | "Boxplot width" | | "Kutu grafiği genişliği" |
| missing | "Jitter seed" | | "Rastgelelik tohumu" |
| missing | "must be a numeric value between" | | "arasında sayısal değer olmalıdır" |
| missing | "must be a positive numeric value" | | "pozitif sayısal değer olmalıdır" |
| missing | "must be a numeric value" | | "sayısal değer olmalıdır" |
| missing | "Reference range minimum must be less than maximum" | | "Referans aralığı minimum değeri maksimum değerinden küçük olmalıdır" |
| missing | "No groups found in the data" | | "Veride grup bulunamadı" |
| missing | "No data available for plotting" | | "Grafik için veri bulunamadı" |
| missing | "Failed to create base plot" | | "Temel grafik oluşturulamadı" |
| missing | "Please check your variable selections" | | "Lütfen değişken seçimlerinizi kontrol edin" |
| missing | "Large number of groups" | | "Çok sayıda grup" |
| missing | "may cause display issues" | | "görüntü sorunlarına neden olabilir" |
| missing | "Consider grouping your data" | | "Verilerinizi gruplandırmayı düşünün" |
| missing | "ID variable contains NA values" | | "ID değişkeni eksik değerler içeriyor" |
| missing | "Longitudinal connections disabled" | | "Boylamsal bağlantılar devre dışı" |
| missing | "Covariate variable contains NA values" | | "Eş değişken eksik değerler içeriyor" |
| missing | "Covariate mapping disabled" | | "Eş değişken eşleme devre dışı" |
| missing | "Using standard geom fallback" | | "Standart geometri yedek kullanılıyor" |
| missing | "Error: No complete cases found for the selected variables" | | "Hata: Seçilen değişkenler için tam veri bulunamadı" |
| missing | "Error: The provided dataset contains no complete rows" | | "Hata: Sağlanan veri seti tam satır içermiyor" |

## 5) Turkish Clinical Terminology Glossary

```text
Advanced Raincloud Plot → Gelişmiş Yağmur Bulutu Grafiği
Longitudinal → Boylamsal  
Transparency → Şeffaflık
Jittering → Rastgelelik/Titreme
Violin plot → Keman grafiği  
Boxplot → Kutu grafiği
Covariate → Eş değişken
Reference range → Referans aralığı
Effect size → Etki büyüklüğü  
Confidence interval → Güven aralığı
Clinical significance → Klinik anlamlılık
MCID → Minimal Klinik Önemli Fark (MKOF)
CV bands → VK bantları (Varyasyon Katsayısı)
```

## 6) QA Checklist Status

- ✅ All jamovi files exist
- ✅ NAMESPACE includes translation helper `.`
- 🟡 **CRITICAL**: Apply string wrapping patches to `R/advancedraincloud.b.R`
- ⏳ Validate `.po` files after patch application
- ⏳ Review Turkish clinical terminology accuracy
- ⏳ Test function with Turkish locale

## 7) Weblate Integration Steps

1. **Create dedicated repository:** `advancedraincloud-i18n`
2. **Add files:**
   - `catalog.pot`
   - `README.md` with clinical context
   - License file
3. **GitHub Settings:**
   - Collaborators → Add Weblate bot
   - Webhooks → `https://hosted.weblate.org/hooks/github/`
4. **Contact jamovi dev team** to add project to Weblate

## 8) Ready-to-Run Commands

**Apply String Wrapping (Priority 1):**
```bash
# Apply the patches listed in section 2.1 to R/advancedraincloud.b.R
```

**Create/Update Catalogs:**
```r
jmvtools::i18nCreate("en"); jmvtools::i18nUpdate("en")
jmvtools::i18nCreate("tr"); jmvtools::i18nUpdate("tr")  
```

**Prepare POT:**
```bash
cp jamovi/i18n/en.po jamovi/i18n/catalog.pot
# Edit header: Language: c\n
```

**Find Remaining Unwrapped Strings:**
```bash
grep -nE '"[^"]*[A-Za-z][^"]*"' R/advancedraincloud.b.R | grep -v '\\.(' | grep -v -E '(html_parts|paste0|style=|color:|background|padding|border|"#)'
```

## 9) Implementation Priority

**🔥 Critical (Implement First):**
1. Apply string wrapping patches for error/warning messages
2. Wrap validation parameter names
3. Run catalog creation commands

**⚡ High Priority:**
1. Create Turkish translations for identified strings
2. Review clinical terminology accuracy
3. Test with sample data

**📝 Medium Priority:**  
1. Set up Weblate integration
2. Create detailed clinical context documentation
3. Add Turkish examples to documentation

## 10) Special Notes for advancedraincloud

**Clinical Context:** This function is designed for advanced raincloud plotting with longitudinal data connections, commonly used in:
- Clinical trial visualization
- Biomarker analysis  
- Patient follow-up studies
- Treatment response assessment

**Turkish Localization Considerations:**
- Use formal medical Turkish terminology
- Maintain consistency with other ClinicoPath modules
- Consider pathologist/oncologist audience
- Preserve technical precision while ensuring readability

**File Size Note:** The backend file (67KB) is quite large with extensive HTML generation - ensure all user-visible content is properly marked for translation while preserving HTML structure and styling.

---

**Next Steps:** Apply the string wrapping patches above, then run the catalog creation commands to generate Turkish translation files.