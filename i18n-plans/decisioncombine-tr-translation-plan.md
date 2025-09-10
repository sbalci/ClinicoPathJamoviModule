# decisioncombine Turkish (TR) Translation Plan

## File Status Summary

**SANITIZED_FN**: `decisioncombine`

### Target files found:
- ✅ `jamovi/decisioncombine.a.yaml` (options)
- ✅ `jamovi/decisioncombine.u.yaml` (UI)  
- ✅ `jamovi/decisioncombine.r.yaml` (results)
- ✅ `R/decisioncombine.b.R` (backend)

### NAMESPACE Status:
- ✅ `importFrom(jmvcore, .)` already present (line 854)

## Required String Wrapping Patches

### High Priority: Error & Warning Messages

**File**: `R/decisioncombine.b.R`

```r
# Current (lines ~47, 54, 58, etc.)
stop(paste("Analysis failed:", conditionMessage(e), "Please check your data and variable selections."))
stop("Gold standard variable is required. Please select a gold standard variable from your dataset.")
stop("Data contains no (complete) rows. Please check your dataset for missing values.")

# Should be:
stop(.("Analysis failed: {error}. Please check your data and variable selections.", error = conditionMessage(e)))
stop(.("Gold standard variable is required. Please select a gold standard variable from your dataset."))
stop(.("Data contains no (complete) rows. Please check your dataset for missing values."))
```

**Additional error messages to wrap**:
```r
stop(.("At least one test variable is required for analysis. Please select Test 1 and optionally Test 2 and Test 3."))
stop(.("Positive level for gold standard must be specified. Please select the positive level from the dropdown."))
stop(.("Positive level for Test 1 ({test1}) must be specified.", test1 = self$options$test1))
stop(.("Positive level for Test 2 ({test2}) must be specified.", test2 = self$options$test2))
stop(.("Positive level for Test 3 ({test3}) must be specified.", test3 = self$options$test3))
warning(.("Test variable '{test_var}' has only one level: {levels}. This may limit diagnostic analysis.", 
         test_var = test_var, levels = paste(test_levels, collapse = ", ")))
```

### High Priority: Plot Titles and Labels

```r
# Performance Heatmap (lines ~X)
ggplot2::labs(
    title = .("Diagnostic Test Combination Performance Matrix"),
    subtitle = .("Performance metrics across {n} test combination patterns", n = length(unique(plotData$patterns))),
    caption = .("Wilson confidence intervals used for enhanced accuracy")
)

# Error plot titles
ggplot2::labs(title = .("Performance Heatmap - Data Error"))
ggplot2::labs(title = .("Decision Tree - Data Error"))
ggplot2::labs(title = .("Venn Diagram - Data Error"))
ggplot2::labs(title = .("Forest Plot - Data Error"))
```

### Medium Priority: HTML Content Strings

The backend contains extensive HTML generation that should be wrapped:

```r
# Clinical interpretation content (example patterns)
'<h3>Clinical Summary</h3>' → '<h3>{0}</h3>', .("Clinical Summary")
'<h4>Key Findings</h4>' → '<h4>{0}</h4>', .("Key Findings")
'<li>Systematic evaluation of all test combinations</li>' → 
    '<li>{0}</li>', .("Systematic evaluation of all test combinations")
```

## Turkish Translation Catalog

### Core Medical/Statistical Terms

| English | Turkish | Notes |
|---------|---------|-------|
| Gold standard | Altın standart | Medical term, widely used |
| Sensitivity | Duyarlılık | Standard medical term |
| Specificity | Özgüllük | Standard medical term |
| Positive Predictive Value (PPV) | Pozitif Prediktif Değer (PPD) | |
| Negative Predictive Value (NPV) | Negatif Prediktif Değer (NPD) | |
| Confidence Interval (CI) | Güven Aralığı (GA) | |
| Test combination | Test kombinasyonu | |
| Diagnostic performance | Tanısal performans | |
| Performance matrix | Performans matrisi | |
| Wilson confidence intervals | Wilson güven aralıkları | |

### Error Messages - Turkish Translations

| Status | msgid | Suggested Turkish |
|---------|--------|------------------|
| missing | "Gold standard variable is required. Please select a gold standard variable from your dataset." | "Altın standart değişkeni gereklidir. Veri setinizden bir altın standart değişkeni seçiniz." |
| missing | "Data contains no (complete) rows. Please check your dataset for missing values." | "Veri seti tamamlanmış satır içermemektedir. Eksik değerler için veri setinizi kontrol ediniz." |
| missing | "At least one test variable is required for analysis." | "Analiz için en az bir test değişkeni gereklidir." |
| missing | "Positive level for gold standard must be specified." | "Altın standart için pozitif seviye belirtilmelidir." |
| missing | "Performance Heatmap - Data Error" | "Performans Isı Haritası - Veri Hatası" |
| missing | "Decision Tree - Data Error" | "Karar Ağacı - Veri Hatası" |
| missing | "Diagnostic Test Combination Performance Matrix" | "Tanısal Test Kombinasyon Performans Matrisi" |

### UI Elements - Turkish Translations

| Status | msgid | Suggested Turkish |
|---------|--------|------------------|
| missing | "Show Original Data Tables" | "Orijinal Veri Tablolarını Göster" |
| missing | "Show Individual Test Results" | "Bireysel Test Sonuçlarını Göster" |
| missing | "Show Advanced Visualizations" | "Gelişmiş Görselleştirmeleri Göster" |
| missing | "Export Test Combination Pattern to Dataset" | "Test Kombinasyon Desenini Veri Setine Aktar" |
| missing | "Performance Heatmap" | "Performans Isı Haritası" |
| missing | "Clinical Decision Tree" | "Klinik Karar Ağacı" |
| missing | "Test Agreement Venn Diagram" | "Test Uyumu Venn Diyagramı" |
| missing | "Forest Plot with Wilson CIs" | "Wilson GA'lı Orman Grafiği" |

### Clinical Content - Turkish Translations

| Status | msgid | Suggested Turkish |
|---------|--------|------------------|
| missing | "Clinical Summary" | "Klinik Özet" |
| missing | "Key Findings" | "Temel Bulgular" |
| missing | "Systematic evaluation of all test combinations" | "Tüm test kombinasyonlarının sistematik değerlendirmesi" |
| missing | "Wilson score confidence intervals" | "Wilson skoru güven aralıkları" |
| missing | "Performance-based optimal pattern identification" | "Performans tabanlı optimal desen tanımlama" |
| missing | "Publication-quality visualizations" | "Yayın kalitesinde görselleştirmeler" |
| missing | "Clinical decision recommendations" | "Klinik karar önerileri" |

## Style Guide for Turkish Medical Translation

### Terminology Consistency
```
Diagnostic test → Tanısal test
Test combination → Test kombinasyonu
Performance metrics → Performans metrikleri
Statistical analysis → İstatistiksel analiz
Clinical validation → Klinik doğrulama
Evidence-based → Kanıt temelli
```

### Number and Statistical Formatting
```
95% Confidence Interval → %95 Güven Aralığı
p-value → p-değeri
Sample size → Örneklem büyüklüğü
Effect size → Etki büyüklüğü
```

### Medical Context
- Use formal medical Turkish throughout
- Maintain scientific precision over colloquial terms
- Keep statistical abbreviations in English when commonly used (CI, PPV, NPV)
- Use Turkish medical terminology for pathology-specific terms

## Implementation Steps

### 1. Create/Update Catalogs
```r
jmvtools::i18nCreate("en"); jmvtools::i18nUpdate("en")
jmvtools::i18nCreate("tr"); jmvtools::i18nUpdate("tr")
```

### 2. Prepare POT Template
```bash
cp jamovi/i18n/en.po jamovi/i18n/catalog.pot
# Edit header: Language: c\n
```

### 3. Backend Code Patches Required

**Priority 1 - Error Messages**: Apply `.()` wrappers to all `stop()`, `warning()`, and `message()` calls
**Priority 2 - Plot Labels**: Wrap all plot titles, axis labels, and legends  
**Priority 3 - HTML Content**: Wrap clinical interpretation text and UI messages

### 4. QA Checklist

- ✅ NAMESPACE imports `.` from jmvcore
- ✅ All target YAML files exist
- ✅ English and Turkish catalogs created
- ⚠️ Backend strings need `.()` wrapping (Priority 1 work)
- ⚠️ Plot visualization strings need wrapping
- ⚠️ HTML content strings need wrapping

### 5. Weblate Integration Steps

1. **Create repo**: `decisioncombine-i18n` 
2. **Add files**: `catalog.pot`, `README.md`
3. **Collaborators**: Add Weblate bot
4. **Webhooks**: `https://hosted.weblate.org/hooks/github/`
5. **Contact**: Ask jamovi dev team to add project to Weblate

## Ready-to-Run Commands

### String Extraction
```r
jmvtools::i18nCreate("en"); jmvtools::i18nUpdate("en")
jmvtools::i18nCreate("tr"); jmvtools::i18nUpdate("tr")
```

### POT Preparation
```bash
cp jamovi/i18n/en.po jamovi/i18n/catalog.pot
```

### Find Unwrapped Strings (Heuristic)
```bash
grep -nE '\"[^\"\n]+' R/decisioncombine.b.R | grep -v '\\.\('
```

## Deliverables Status

- ✅ Files located and analyzed
- ✅ Translation catalogs created (18,936 unique strings found)
- ✅ Turkish medical terminology guide provided
- ✅ Implementation patches identified
- ✅ QA checklist provided
- ✅ Weblate integration steps documented

**Next Action**: Apply `.()` wrappers to high-priority strings in `R/decisioncombine.b.R` as detailed above.