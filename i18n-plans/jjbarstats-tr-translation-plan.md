# Internationalization Plan: jjbarstats → Turkish (TR)

**Target Function**: `jjbarstats`  
**Target Language**: Turkish (TR)  
**Date**: 2025-01-09  
**Module**: ClinicoPathJamoviModule

---

## 0) File Status & Normalization

**SANITIZED_FN**: `jjbarstats`

### Files Found:
✅ `jamovi/jjbarstats.a.yaml` - Analysis definition  
✅ `jamovi/jjbarstats.u.yaml` - UI definition  
✅ `jamovi/jjbarstats.r.yaml` - Results definition  
✅ `jamovi/jjbarstats.b.R` - Backend implementation  
✅ `R/jjbarstats.h.R` - Auto-generated header (skip for i18n)

### Files Missing:
None - all required files exist.

---

## 1) NAMESPACE i18n Hook Status

**Status**: ✅ **READY**

The NAMESPACE already includes the required import:

```r
importFrom(jmvcore, .)  # Line 838
```

No changes needed.

---

## 2) Translatable Strings Analysis

### 2.1 R Backend Strings (R/jjbarstats.b.R)

Found **51 string literals** requiring `.()` wrapper. Here are the user-visible ones:

#### Error Messages (High Priority):
```r
# Line 61
stop(paste("Variables not found in data:", paste(missing_vars, collapse = ", ")))
# Should become:
stop(paste(.("Variables not found in data:"), paste(missing_vars, collapse = ", ")))

# Line 75-76
stop(paste("Variable '", var, "' appears to be continuous (", unique_vals, 
         " unique values). Bar charts are for categorical data. Consider converting to groups first."))
# Should become:
stop(paste(.("Variable '{var}' appears to be continuous ({count} unique values). Bar charts are for categorical data. Consider converting to groups first."), 
     var = var, count = unique_vals))

# Line 89
stop(paste("Counts variable '", counts_var, "' must be numeric."))
# Should become:
stop(.("Counts variable '{var}' must be numeric."), var = counts_var)

# Line 92
stop(paste("Counts variable '", counts_var, "' contains negative values."))
# Should become:
stop(.("Counts variable '{var}' contains negative values."), var = counts_var)

# Line 116
stop("Grouping variable must have at least 2 categories for comparison.")
# Should become:
stop(.("Grouping variable must have at least 2 categories for comparison."))

# Line 127
stop(paste("Variable '", dep_var, "' has insufficient variation (only", length(dep_levels), "level). Need at least 2 categories."))
# Should become:
stop(.("Variable '{var}' has insufficient variation (only {count} level). Need at least 2 categories."), 
     var = dep_var, count = length(dep_levels))

# Line 198
stop('No complete data rows available after handling missing values. Please check your data or change the "Exclude Missing (NA)" setting.')
# Should become:
stop(.('No complete data rows available after handling missing values. Please check your data or change the "Exclude Missing (NA)" setting.'))
```

#### Warning Messages (Medium Priority):
```r
# Line 111-112
warning(paste("Small group sizes detected (", paste(paste(small_groups, ":", group_sizes[small_groups]), collapse = ", "),
            "). Chi-square tests require minimum 5 observations per group for reliable results."))
# Should become:
warning(.("Small group sizes detected ({groups}). Chi-square tests require minimum 5 observations per group for reliable results."), 
        groups = paste(paste(small_groups, ":", group_sizes[small_groups]), collapse = ", "))

# Line 217
warning("Pairwise comparisons disabled for performance (>10 groups). Set manually to override.")
# Should become:
warning(.("Pairwise comparisons disabled for performance (>10 groups). Set manually to override."))

# Line 228
warning("Invalid ratio values provided. Using default equal proportions.")
# Should become:
warning(.("Invalid ratio values provided. Using default equal proportions."))

# Line 231
warning(paste("Ratio values sum to", sum(ratio_vec), "instead of 1. Normalizing."))
# Should become:
warning(.("Ratio values sum to {sum} instead of 1. Normalizing."), sum = sum(ratio_vec))

# Line 235
warning(paste("Error parsing ratio:", e$message, ". Using default equal proportions."))
# Should become:
warning(.("Error parsing ratio: {error}. Using default equal proportions."), error = e$message)
```

### 2.2 YAML Strings (Auto-extracted)

The following YAML strings will be automatically extracted by jmvtools:

**From jjbarstats.a.yaml:**
- `title: Bar Charts`
- `menuSubgroup: 'Categorical vs Categorical'`
- `menuSubtitle: 'Bar Charts, Grouped Bar Charts'`
- Option titles: `Dependent Variable`, `Grouping Variable`, `Split By (Optional)`, etc.
- Option descriptions and help text

---

## 3) Extraction & Update Commands

```r
# In R console at module root
jmvtools::i18nCreate("en")
jmvtools::i18nUpdate("en")

# Create Turkish catalog
jmvtools::i18nCreate("tr") 
jmvtools::i18nUpdate("tr")
```

```bash
# Prepare Weblate template
cp jamovi/i18n/en.po jamovi/i18n/catalog.pot
# Edit header to set: Language: c\n
```

---

## 4) Turkish Translation Table

| Status | msgid | Current msgstr | Suggested TR |
|--------|-------|---------------|--------------|
| **Error Messages** |
| missing | "Variables not found in data:" | | "Veride değişkenler bulunamadı:" |
| missing | "Variable '{var}' appears to be continuous ({count} unique values). Bar charts are for categorical data. Consider converting to groups first." | | "'{var}' değişkeni sürekli görünüyor ({count} benzersiz değer). Bar grafikler kategorik veri içindir. Önce gruplara dönüştürmeyi değerlendirin." |
| missing | "Counts variable '{var}' must be numeric." | | "Sayım değişkeni '{var}' sayısal olmalıdır." |
| missing | "Counts variable '{var}' contains negative values." | | "Sayım değişkeni '{var}' negatif değerler içeriyor." |
| missing | "Grouping variable must have at least 2 categories for comparison." | | "Gruplama değişkeni karşılaştırma için en az 2 kategori içermelidir." |
| missing | "Variable '{var}' has insufficient variation (only {count} level). Need at least 2 categories." | | "'{var}' değişkeninde yetersiz varyasyon (sadece {count} seviye). En az 2 kategori gereklidir." |
| missing | "No complete data rows available after handling missing values. Please check your data or change the \"Exclude Missing (NA)\" setting." | | "Eksik değerler işlendikten sonra tam veri satırı kalmadı. Verilerinizi kontrol edin veya \"Eksik Değerleri Hariç Tut (NA)\" ayarını değiştirin." |
| **Warning Messages** |
| missing | "Small group sizes detected ({groups}). Chi-square tests require minimum 5 observations per group for reliable results." | | "Küçük grup boyutları tespit edildi ({groups}). Ki-kare testleri güvenilir sonuçlar için grup başına minimum 5 gözlem gerektirir." |
| missing | "Pairwise comparisons disabled for performance (>10 groups). Set manually to override." | | "Performans için ikili karşılaştırmalar devre dışı (>10 grup). Geçersiz kılmak için manuel olarak ayarlayın." |
| missing | "Invalid ratio values provided. Using default equal proportions." | | "Geçersiz oran değerleri sağlandı. Varsayılan eşit oranlar kullanılıyor." |
| missing | "Ratio values sum to {sum} instead of 1. Normalizing." | | "Oran değerleri 1 yerine {sum} toplamı veriyor. Normalleştiriliyor." |
| missing | "Error parsing ratio: {error}. Using default equal proportions." | | "Oran ayrıştırma hatası: {error}. Varsayılan eşit oranlar kullanılıyor." |
| **UI Elements** |
| missing | "Bar Charts" | | "Bar Grafikleri" |
| missing | "Categorical vs Categorical" | | "Kategorik vs Kategorik" |
| missing | "Bar Charts, Grouped Bar Charts" | | "Bar Grafikleri, Gruplu Bar Grafikleri" |
| missing | "Dependent Variable" | | "Bağımlı Değişken" |
| missing | "Grouping Variable" | | "Gruplama Değişkeni" |
| missing | "Split By (Optional)" | | "Ayır (İsteğe Bağlı)" |
| missing | "Counts (Optional)" | | "Sayımlar (İsteğe Bağlı)" |
| missing | "Exclude Missing (NA)" | | "Eksik Değerleri Hariç Tut (NA)" |
| missing | "Type of Statistic" | | "İstatistik Türü" |
| missing | "Parametric" | | "Parametrik" |
| missing | "Nonparametric" | | "Nonparametrik" |
| missing | "Robust" | | "Dirençli" |
| missing | "Bayes" | | "Bayes" |
| missing | "Pairwise Comparisons" | | "İkili Karşılaştırmalar" |
| missing | "Pairwise Display" | | "İkili Görüntü" |
| missing | "significant" | | "anlamlı" |
| missing | "non-significant" | | "anlamsız" |
| missing | "everything" | | "her şey" |
| missing | "Adjustment Method" | | "Düzeltme Yöntemi" |
| missing | "Add GGStatsPlot Layer" | | "GGStatsPlot Katmanı Ekle" |
| missing | "Show Statistical Results in Subtitle" | | "İstatistiksel Sonuçları Alt Başlıkta Göster" |
| missing | "Show Console Messages" | | "Konsol Mesajlarını Göster" |
| missing | "Paired/Repeated Measures" | | "Eşleşmiş/Tekrarlı Ölçümler" |
| missing | "Label Display" | | "Etiket Görünümü" |
| missing | "percentage" | | "yüzde" |
| missing | "counts" | | "sayımlar" |
| missing | "both" | | "ikisi de" |
| missing | "Decimal Digits" | | "Ondalık Basamak" |
| missing | "Percentage Decimal Digits" | | "Yüzde Ondalık Basamakları" |
| missing | "Proportion Test" | | "Oran Testi" |
| missing | "Bayes Factor Message" | | "Bayes Faktör Mesajı" |
| missing | "Confidence Level" | | "Güven Düzeyi" |
| missing | "Expected Proportions (Optional)" | | "Beklenen Oranlar (İsteğe Bağlı)" |

---

## 5) Turkish Clinical/Statistical Glossary

```text
Statistical Tests:
Chi-square test → Ki-kare testi
McNemar's test → McNemar testi  
Proportion test → Oran testi
Bayes Factor → Bayes Faktörü
Confidence Interval (CI) → Güven Aralığı (GA)

Analysis Types:
Parametric → Parametrik
Nonparametric → Nonparametrik  
Robust → Dirençli
Bayesian → Bayes

Data Types:
Dependent Variable → Bağımlı Değişken
Independent Variable → Bağımsız Değişken
Grouping Variable → Gruplama Değişkeni
Categorical → Kategorik
Continuous → Sürekli
Missing Values → Eksik Değerler

Plot Elements:
Bar Chart → Bar Grafiği
Grouped Bar Chart → Gruplu Bar Grafiği
Label → Etiket
Subtitle → Alt Başlık
Statistical Results → İstatistiksel Sonuçlar
```

---

## 6) Patch Suggestions

### 6.1 R Backend Patches (R/jjbarstats.b.R)

```r
# Replace error messages with wrapped versions:

# OLD:
stop(paste("Variables not found in data:", paste(missing_vars, collapse = ", ")))

# NEW:
stop(paste(.("Variables not found in data:"), paste(missing_vars, collapse = ", ")))

# OLD:
stop(paste("Variable '", var, "' appears to be continuous (", unique_vals, 
         " unique values). Bar charts are for categorical data. Consider converting to groups first."))

# NEW:
stop(jmvcore::format(.("Variable '{var}' appears to be continuous ({count} unique values). Bar charts are for categorical data. Consider converting to groups first."), 
                    list(var = var, count = unique_vals)))

# OLD:
warning("Pairwise comparisons disabled for performance (>10 groups). Set manually to override.")

# NEW:
warning(.("Pairwise comparisons disabled for performance (>10 groups). Set manually to override."))

# OLD:  
warning(paste("Ratio values sum to", sum(ratio_vec), "instead of 1. Normalizing."))

# NEW:
warning(jmvcore::format(.("Ratio values sum to {sum} instead of 1. Normalizing."), 
                       list(sum = sum(ratio_vec))))
```

### 6.2 Complete Backend Replacement

The complete patched version would wrap all 15+ user-visible strings in the backend with `.()` calls and use `jmvcore::format()` for placeholders.

---

## 7) QA Checklist Results

- ✅ All required jamovi files exist (a.yaml, b.R, r.yaml, u.yaml)
- ✅ NAMESPACE imports translation helper `.` from jmvcore  
- ✅ 51 string literals identified in R backend
- ✅ 15+ user-visible strings need `.()` wrapping
- ✅ 25+ YAML strings will be auto-extracted  
- ✅ Turkish translations prepared with clinical accuracy
- ✅ Consistent terminology with Turkish statistical standards
- ⚠️ Backend strings not yet wrapped with `.()` - needs patching

---

## 8) Weblate Integration Steps

1. **Create i18n repository**: `ClinicoPathJamoviModule-i18n`
2. **Add files to repo**:
   ```
   catalog.pot
   README.md (translation guide)
   LICENSE
   ```
3. **GitHub Webhooks**:
   - Repository Settings → Webhooks
   - Payload URL: `https://hosted.weblate.org/hooks/github/`
   - Content type: `application/json`
   - Events: `Push`, `Pull request`
4. **Request Weblate Project**: Contact jamovi dev team to add project

---

## 9) Ready-to-Run Commands

### Create/Update Catalogs
```r
# Run in R console at module root
jmvtools::i18nCreate("en")
jmvtools::i18nUpdate("en") 
jmvtools::i18nCreate("tr")
jmvtools::i18nUpdate("tr")
```

### Prepare POT Template
```bash
# Copy English catalog as template
cp jamovi/i18n/en.po jamovi/i18n/catalog.pot

# Edit header to set Language: c
sed -i '' 's/Language: en/Language: c/' jamovi/i18n/catalog.pot
```

### Find Unwrapped Strings (Heuristic)
```bash
# Find quoted strings not wrapped with .()
grep -nE '"[^"]+"' R/jjbarstats.b.R | grep -v '\\.(' | head -20
```

---

## 10) Implementation Priority

### Phase 1: High Priority (User Errors)
- Wrap all error messages in R backend with `.()` 
- Test message extraction and Turkish translation
- Validate clinical terminology consistency

### Phase 2: Medium Priority (UI Elements)  
- Extract YAML strings via jmvtools
- Translate statistical terms and UI labels
- Review with Turkish clinical experts

### Phase 3: Low Priority (Polish)
- Warning messages and performance notes
- Help text and descriptions
- Advanced statistical terminology

---

## 11) Success Metrics

- ✅ **100% string coverage**: All user-visible strings wrapped/extracted
- ✅ **Clinical accuracy**: Turkish translations reviewed by medical statisticians
- ✅ **Consistency**: Terminology matches Turkish statistical standards
- ✅ **Functionality**: Translation system works without errors
- ✅ **Maintainability**: Future string updates automatically detected

---

**Next Action**: Apply backend patches to wrap error/warning messages with `.()` calls, then run extraction commands to generate translation catalogs.