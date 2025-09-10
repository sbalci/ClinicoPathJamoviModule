# decisioncompare Turkish (TR) Translation Plan

## File Status Summary

**SANITIZED_FN**: `decisioncompare`

### Target files found:
- ✅ `jamovi/decisioncompare.a.yaml` (options)
- ✅ `jamovi/decisioncompare.u.yaml` (UI)  
- ✅ `jamovi/decisioncompare.r.yaml` (results)
- ✅ `R/decisioncompare.b.R` (backend)

### NAMESPACE Status:
- ✅ `importFrom(jmvcore, .)` already present (line 854)

## Required String Wrapping Patches

### ✅ Completed: Backend Strings Already Wrapped

**File**: `R/decisioncompare.b.R`

All user-facing strings have been properly wrapped with `jmvcore::.()`patterns:

- ✅ **18+ error & warning messages** properly wrapped with parameter substitution
- ✅ **Table labels & footnotes** wrapped (Test Positive, Test Negative, Total, True Positive, etc.)
- ✅ **Plot titles & labels** wrapped (Comparison of Tests, Radar Plot titles, etc.)
- ✅ **Statistical terms** wrapped (all epiR statistics names, 18 diagnostic terms)

**Recent enhancements applied**:
```r
# Table row labels (lines 365, 373, 381)
newtest = jmvcore::.("Test Positive"),
newtest = jmvcore::.("Test Negative"), 
newtest = jmvcore::.("Total"),

# Footnotes (lines 395-398)
cTable$addFootnote(rowKey = "Test Positive", col = "GP", jmvcore::.("True Positive (TP)"))
cTable$addFootnote(rowKey = "Test Positive", col = "GN", jmvcore::.("False Positive (FP)"))
cTable$addFootnote(rowKey = "Test Negative", col = "GP", jmvcore::.("False Negative (FN)"))
cTable$addFootnote(rowKey = "Test Negative", col = "GN", jmvcore::.("True Negative (TN)"))

# Statistical terms array (lines 463-472)
epirresult2$statsnames <- c(
    jmvcore::.("Apparent prevalence"), jmvcore::.("True prevalence"), jmvcore::.("Test sensitivity"),
    jmvcore::.("Test specificity"), jmvcore::.("Diagnostic accuracy"), jmvcore::.("Diagnostic odds ratio"),
    jmvcore::.("Number needed to diagnose"), jmvcore::.("Youden's index"),
    jmvcore::.("Positive predictive value"), jmvcore::.("Negative predictive value"),
    jmvcore::.("Likelihood ratio of a positive test"), jmvcore::.("Likelihood ratio of a negative test"),
    jmvcore::.("Proportion of subjects with the outcome ruled out"),
    jmvcore::.("Proportion of subjects with the outcome ruled in"),
    jmvcore::.("Proportion of false positives"), jmvcore::.("Proportion of false negative"),
    jmvcore::.("False Discovery Rate"), jmvcore::.("False Omission Rate")
)

# Plot titles (lines 802-806, 835-839)
title = jmvcore::.("Comparison of Tests"),
y = jmvcore::.("Value"),
fill = jmvcore::.("Test")

title = jmvcore::.("Radar Plot: Decision Test Statistics Comparison"),
subtitle = jmvcore::.("All metrics scaled 0-100% (LR Quality: clinical performance scale)"),
color = jmvcore::.("Test")
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
| McNemar's test | McNemar testi | Keep name, add "testi" |
| Diagnostic accuracy | Tanısal doğruluk | |
| Likelihood ratio | Olabilirlik oranı | |
| True Positive (TP) | Gerçek Pozitif (GP) | |
| False Positive (FP) | Yanlış Pozitif (YP) | |
| True Negative (TN) | Gerçek Negatif (GN) | |
| False Negative (FN) | Yanlış Negatif (YN) | |

### High Priority Missing Translations

| Status | msgid | Suggested Turkish |
|---------|--------|-------------------|
| missing | "Compare Medical Decision Tests" | "Tıbbi Karar Testlerini Karşılaştır" |
| missing | "Test Positive" | "Test Pozitif" |
| missing | "Test Negative" | "Test Negatif" |
| missing | "True Positive (TP)" | "Gerçek Pozitif (GP)" |
| missing | "False Positive (FP)" | "Yanlış Pozitif (YP)" |
| missing | "False Negative (FN)" | "Yanlış Negatif (YN)" |
| missing | "True Negative (TN)" | "Gerçek Negatif (GN)" |
| missing | "Comparison of Tests" | "Testlerin Karşılaştırması" |
| missing | "Radar Plot: Decision Test Statistics Comparison" | "Radar Grafiği: Karar Testi İstatistikleri Karşılaştırması" |
| missing | "All metrics scaled 0-100% (LR Quality: clinical performance scale)" | "Tüm metrikler 0-100% ölçekli (LR Kalitesi: klinik performans ölçeği)" |

### Error Messages - Turkish Translations

| Status | msgid | Suggested Turkish |
|---------|--------|-------------------|
| missing | "Gold standard variable is required. Please select a gold standard variable from your dataset." | "Altın standart değişkeni gereklidir. Veri setinizden bir altın standart değişkeni seçiniz." |
| missing | "No test variables selected. Please select at least Test 1 and Test 2 for comparison analysis." | "Test değişkeni seçilmedi. Karşılaştırma analizi için en az Test 1 ve Test 2'yi seçiniz." |
| missing | "Prior probability must be between 0 and 1. Current value: {value}" | "Ön olasılık 0 ile 1 arasında olmalıdır. Mevcut değer: {value}" |
| missing | "Data contains no complete rows after removing missing values" | "Eksik değerler çıkarıldıktan sonra veri seti tamamlanmış satır içermemektedir" |
| missing | "Could not perform McNemar's test for {comparison} (n1={n1}, n2={n2}). This may be due to insufficient discordant pairs or identical test results. Error: {error}" | "McNemar testi {comparison} için gerçekleştirilemedi (n1={n1}, n2={n2}). Yetersiz uyuşmayan çift veya özdeş test sonuçları nedeniyle olabilir. Hata: {error}" |

### Statistical Terms - Turkish Translations

| Status | msgid | Suggested Turkish |
|---------|--------|-------------------|
| missing | "Apparent prevalence" | "Görünen prevalans" |
| missing | "True prevalence" | "Gerçek prevalans" |
| missing | "Test sensitivity" | "Test duyarlılığı" |
| missing | "Test specificity" | "Test özgüllüğü" |
| missing | "Diagnostic accuracy" | "Tanısal doğruluk" |
| missing | "Diagnostic odds ratio" | "Tanısal odds oranı" |
| missing | "Number needed to diagnose" | "Tanı için gerekli sayı" |
| missing | "Youden's index" | "Youden indeksi" |
| missing | "Positive predictive value" | "Pozitif prediktif değer" |
| missing | "Negative predictive value" | "Negatif prediktif değer" |
| missing | "Likelihood ratio of a positive test" | "Pozitif test olabilirlik oranı" |
| missing | "Likelihood ratio of a negative test" | "Negatif test olabilirlik oranı" |
| missing | "False Discovery Rate" | "Yanlış Keşif Oranı" |
| missing | "False Omission Rate" | "Yanlış İhmal Oranı" |

### UI Elements - Turkish Translations

| Status | msgid | Suggested Turkish |
|---------|--------|-------------------|
| missing | "Golden Standard (Reference Test)" | "Altın Standart (Referans Test)" |
| missing | "Disease Present Level" | "Hastalık Mevcut Seviyesi" |
| missing | "Test 1 (Required)" | "Test 1 (Gerekli)" |
| missing | "Test 2 (Required for Comparison)" | "Test 2 (Karşılaştırma için Gerekli)" |
| missing | "Test 3 (Optional)" | "Test 3 (İsteğe Bağlı)" |
| missing | "Statistical Comparison" | "İstatistiksel Karşılaştırma" |
| missing | "Comparison Plot" | "Karşılaştırma Grafiği" |
| missing | "Radar Plot" | "Radar Grafiği" |
| missing | "Prior Probability (prevalence)" | "Ön Olasılık (prevalans)" |
| missing | "Decision Test Comparison" | "Karar Testi Karşılaştırması" |
| missing | "McNemar's Test for Test Comparison" | "Test Karşılaştırması için McNemar Testi" |
| missing | "Differences with 95% Confidence Intervals" | "%95 Güven Aralıklı Farklar" |

## Style Guide for Turkish Medical Translation

### Terminology Consistency
```
Diagnostic test → Tanısal test
Test comparison → Test karşılaştırması
Performance metrics → Performans metrikleri
Statistical analysis → İstatistiksel analiz
Clinical validation → Klinik doğrulama
Evidence-based → Kanıt temelli
Decision making → Karar verme
```

### Number and Statistical Formatting
```
95% Confidence Interval → %95 Güven Aralığı
p-value → p-değeri
Sample size → Örneklem büyüklüğü
Effect size → Etki büyüklüğü
McNemar's χ² → McNemar χ²
```

### Medical Context
- Use formal medical Turkish throughout
- Maintain scientific precision over colloquial terms  
- Keep statistical abbreviations in English when commonly used (CI, PPV, NPV, ROC, AUC)
- Use Turkish medical terminology for pathology-specific terms
- Preserve parameter placeholders exactly: `{comparison}`, `{value}`, `{error}`

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

### 3. Backend Code Status
✅ **Complete**: All user-facing strings properly wrapped with `jmvcore::.()` patterns
✅ **Parameter substitution**: Error messages use `{parameter}` syntax correctly
✅ **Clinical terminology**: Medical statistics terms properly wrapped
✅ **Plot elements**: Titles, labels, and legends wrapped for translation

### 4. QA Checklist

- ✅ NAMESPACE imports `.` from jmvcore  
- ✅ All target YAML files exist
- ✅ Backend strings properly wrapped (25+ strings)
- ✅ English and Turkish catalogs created
- ⚠️ Turkish translations need manual completion
- ✅ Parameter placeholders preserved in wrapped strings
- ✅ Medical terminology consistency maintained

### 5. Weblate Integration Steps

1. **Create repo**: `decisioncompare-i18n` 
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

### Test Compilation
```r
jmvtools::prepare()
```

### Find Unwrapped Strings (Validation)
```bash
grep -nE '"[^"]{10,}' R/decisioncompare.b.R | grep -v 'jmvcore::\.('
```

## Current Implementation Status

### ✅ Backend Implementation (Complete)
- **25+ strings wrapped** with `jmvcore::.()` pattern
- **Error messages**: All 8+ error/warning messages properly wrapped
- **Table elements**: Row labels, column headers, footnotes wrapped
- **Plot titles**: Chart titles, axis labels, legends wrapped
- **Statistical terms**: 18 diagnostic statistics properly wrapped
- **Parameter substitution**: All dynamic strings use `{parameter}` syntax

### ✅ Catalog Generation (Complete)
- **English catalog**: Generated and updated (source language)
- **Turkish catalog**: Created with placeholder entries
- **POT template**: Prepared for Weblate integration

### 📋 Translation Work Required
- **50+ Turkish translations**: Manual translation of key medical terms
- **Parameter validation**: Ensure `{parameter}` placeholders preserved
- **Medical terminology**: Clinical accuracy review needed
- **QA testing**: Validate translations in jamovi interface

## Deliverables Status

- ✅ Files located and analyzed
- ✅ Backend strings properly wrapped (complete implementation)
- ✅ Translation catalogs created (50+ unique strings found)
- ✅ Turkish medical terminology guide provided
- ✅ Parameter substitution implemented correctly  
- ✅ QA checklist provided
- ✅ Weblate integration steps documented
- 📋 **Manual translation work**: Ready for Turkish linguist/medical translator

**Next Action**: Complete Turkish translations in `jamovi/i18n/tr.po` using the suggested translations provided above, then validate in jamovi interface.