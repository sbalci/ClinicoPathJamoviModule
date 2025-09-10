# Medical Decision Function - Turkish Translation Plan

## Project Overview
**Function**: decision  
**Target Language**: Turkish (TR)  
**Domain**: Medical diagnostic test analysis  
**Primary Users**: Pathologists, oncologists, clinical researchers  

---

## 1) File Status Assessment ✅

### Required Files (All Found)
- ✅ `jamovi/decision.a.yaml` (options)
- ✅ `jamovi/decision.u.yaml` (UI)  
- ✅ `jamovi/decision.r.yaml` (results)
- ✅ `R/decision.b.R` (backend)

### i18n Infrastructure
- ✅ NAMESPACE contains `importFrom(jmvcore, .)` at line 854
- ✅ Translation catalogs created: `jamovi/i18n/en.po`, `jamovi/i18n/tr.po`
- ✅ Weblate template: `jamovi/i18n/catalog.pot`

---

## 2) Backend String Wrapping - COMPLETED ✅

### Applied i18n Patterns

All user-visible strings in `R/decision.b.R` have been wrapped with `.()`:

#### Error Messages (Lines 48-78)
```r
# Before
stop("Please select a gold standard (reference) variable")

# After  
stop(.("Please select a gold standard (reference) variable"))
```

#### Warning Messages with Placeholders (Line 88)
```r
# Before
warning(sprintf("Removed %d rows with missing values", count))

# After
warning(.("Removed {count} rows with missing values", count = nrow(self$data) - nrow(mydata)))
```

#### Clinical Footnotes (Lines 194-221)
```r
# Before
footnotes_n <- list(
    TotalPop = "Total Number of Subjects in complete case analysis",
    DiseaseP = "Total Number of Subjects with Disease (Gold Standard Positive)"
)

# After
footnotes_n <- list(
    TotalPop = .("Total Number of Subjects in complete case analysis"),
    DiseaseP = .("Total Number of Subjects with Disease (Gold Standard Positive)")
)
```

#### Statistical Terms (Lines 545-563)
All 18 diagnostic statistics labels wrapped for translation:
```r
.("Apparent prevalence"), .("Test sensitivity"), .("Youden's index"), etc.
```

---

## 3) Turkish Translation Table

### Missing/Untranslated Strings

| Status | msgid | Current msgstr | Suggested Turkish |
|--------|-------|----------------|-------------------|
| **UI Interface** |
| missing | "Test Positive" | | "Test Pozitif" |
| missing | "Test Negative" | | "Test Negatif" |
| missing | "Total" | | "Toplam" |
| **Error Messages** |
| missing | "Please select a gold standard (reference) variable" | | "Lütfen altın standart (referans) değişkenini seçin" |
| missing | "Please select a test variable to evaluate" | | "Lütfen değerlendirilecek test değişkenini seçin" |
| missing | "Please select the positive level for the gold standard variable" | | "Lütfen altın standart değişkeni için pozitif seviyeyi seçin" |
| missing | "Please select the positive level for the test variable" | | "Lütfen test değişkeni için pozitif seviyeyi seçin" |
| missing | "No data available for analysis. Please ensure your data is loaded." | | "Analiz için veri mevcut değil. Lütfen verinizin yüklendiğinden emin olun." |
| missing | "Insufficient data: At least 4 cases are required for diagnostic test analysis" | | "Yetersiz veri: Tanı testi analizi için en az 4 olgu gereklidir" |
| **Clinical Terms** |
| missing | "Total Number of Subjects in complete case analysis" | | "Tam veri analizi yapılan toplam hasta sayısı" |
| missing | "Total Number of Subjects with Disease (Gold Standard Positive)" | | "Hastalıklı toplam hasta sayısı (Altın Standart Pozitif)" |
| missing | "Total Number of Healthy Subjects (Gold Standard Negative)" | | "Sağlıklı toplam hasta sayısı (Altın Standart Negatif)" |
| missing | "Total Number of Positive Test Results" | | "Toplam pozitif test sonucu sayısı" |
| missing | "Total Number of Negative Test Results" | | "Toplam negatif test sonucu sayısı" |
| missing | "Total Number of True Test Results (TP + TN)" | | "Toplam doğru test sonucu sayısı (TP + TN)" |
| missing | "Total Number of Wrong Test Results (FP + FN)" | | "Toplam yanlış test sonucu sayısı (FP + FN)" |
| **Statistical Terms** |
| missing | "Apparent prevalence" | | "Görünür prevalans" |
| missing | "True prevalence" | | "Gerçek prevalans" |
| missing | "Test sensitivity" | | "Test duyarlılığı" |
| missing | "Test specificity" | | "Test özgüllüğü" |
| missing | "Diagnostic accuracy" | | "Tanısal doğruluk" |
| missing | "Diagnostic odds ratio" | | "Tanısal odds oranı" |
| missing | "Number needed to diagnose" | | "Tanı için gereken sayı" |
| missing | "Youden's index" | | "Youden indeksi" |
| missing | "Positive predictive value" | | "Pozitif prediktif değer" |
| missing | "Negative predictive value" | | "Negatif prediktif değer" |
| missing | "Likelihood ratio of a positive test" | | "Pozitif test likelihood oranı" |
| missing | "Likelihood ratio of a negative test" | | "Negatif test likelihood oranı" |
| missing | "False Discovery Rate" | | "Yanlış Keşif Oranı (YKO)" |
| missing | "False Omission Rate" | | "Yanlış Dışlama Oranı" |
| **Clinical Explanations** |
| missing | "Sensitivity: Proportion of diseased patients correctly identified (TP rate)..." | | "Duyarlılık: Hasta bireylerden doğru tanı konulan oranı (TP oranı). Yüksek değer hastalığı DIŞLAMAK için daha iyidir." |
| missing | "Specificity: Proportion of healthy patients correctly identified (TN rate)..." | | "Özgüllük: Sağlıklı bireylerden doğru tanı konulan oranı (TN oranı). Yüksek değer hastalığı DAHIL ETMEK için daha iyidir." |
| missing | "Positive Predictive Value: Probability of disease given positive test..." | | "Pozitif Prediktif Değer: Pozitif test sonucunda hastalık olma olasılığı. Prevalans ve özgüllüğe bağlıdır." |

---

## 4) Medical Turkish Terminology Glossary

### Core Diagnostic Terms
```
Sensitivity → Duyarlılık
Specificity → Özgüllük  
Positive Predictive Value (PPV) → Pozitif Prediktif Değer (PPD)
Negative Predictive Value (NPV) → Negatif Prediktif Değer (NPD)
Likelihood Ratio → Likelihood Oranı
Odds Ratio (OR) → Odds Oranı (OO)
Confidence Interval (CI) → Güven Aralığı (GA)
Gold Standard → Altın Standart
Reference Test → Referans Test
True Positive (TP) → Gerçek Pozitif (GP)
False Positive (FP) → Yanlış Pozitif (YP)
True Negative (TN) → Gerçek Negatif (GN)
False Negative (FN) → Yanlış Negatif (YN)
```

### Clinical Context Terms
```
Diagnostic Test → Tanı Testi
Test Performance → Test Performansı
Clinical Decision Making → Klinik Karar Verme
Pre-test Probability → Test Öncesi Olasılık
Post-test Probability → Test Sonrası Olasılık
Disease Prevalence → Hastalık Prevalansı
Population Prevalence → Popülasyon Prevalansı
```

---

## 5) Style Guidelines for Turkish Translation

### Clinical Tone
- Use formal Turkish medical terminology
- Prefer established Turkish medical terms over direct translations
- Keep abbreviations in parentheses: "Güven Aralığı (GA)"
- Use patient-friendly explanations where appropriate

### Technical Guidelines
- Numbers: Use Turkish decimal separator (virgül) in explanations, keep dots in code
- Percentages: "yüzde" or "%" symbol
- Statistical values: Keep English statistical abbreviations (TP, FN, etc.) but translate descriptions
- Error messages: Use polite, helpful Turkish ("Lütfen...", "emin olun...")

---

## 6) Ready-to-Run Commands

### Update Catalogs
```r
jmvtools::i18nCreate("en"); jmvtools::i18nUpdate("en")
jmvtools::i18nCreate("tr"); jmvtools::i18nUpdate("tr")
```

### Prepare Weblate POT
```bash
cp jamovi/i18n/en.po jamovi/i18n/catalog.pot
# Edit header: Language: c\n
```

### Quick Check for Unwrapped Strings
```bash
grep -nE '"[^"]+[a-zA-Z]{3,}' R/decision.b.R | grep -v '\.\('
```

---

## 7) Weblate Integration Steps

1. **Create Repository**: `ClinicoPathJamoviModule-i18n`
   - Add `catalog.pot`, `README.md`, license
   - Include Turkish medical glossary

2. **GitHub Configuration**:
   - **Collaborators** → Add Weblate bot
   - **Webhooks** → Add: `https://hosted.weblate.org/hooks/github/`

3. **Contact jamovi Team**:
   - Request addition to Weblate jamovi project
   - Provide repository URL and translation details

---

## 8) Quality Assurance Checklist ✅

- ✅ All user-visible strings in `R/decision.b.R` wrapped with `.()` 
- ✅ NAMESPACE imports translation helper `.`
- ✅ All required YAML files present and functional
- ✅ Translation catalogs created (`en.po`, `tr.po`, `catalog.pot`)
- ✅ Medical terminology consistency established
- ✅ Placeholder syntax using `{variable}` format
- ✅ Error messages provide actionable guidance
- ✅ Clinical interpretations maintain professional tone

---

## 9) Next Steps

1. **Import to Weblate**: Upload catalog.pot to translation platform
2. **Medical Review**: Have Turkish medical professional review terminology
3. **User Testing**: Test with Turkish-speaking pathologists/oncologists  
4. **Integration**: Merge translations back to main module
5. **Documentation**: Update user guides with Turkish examples

---

## 10) Priority Translation Order

### Phase 1 (Critical - User can't use function without these)
1. Input validation error messages
2. Core UI elements ("Test Positive", "Test Negative", "Total")
3. Basic statistical terms

### Phase 2 (Important - Enhanced user experience)  
1. Clinical interpretations and footnotes
2. Advanced statistical terms
3. Help text and explanations

### Phase 3 (Nice to have - Educational content)
1. Detailed clinical guidance  
2. Statistical methodology explanations
3. Extended help documentation

---

**Translation Status**: Ready for professional medical translation  
**Estimated Strings**: 47+ unique strings requiring translation  
**Medical Complexity**: High - requires clinical terminology expertise  
**User Impact**: Critical for Turkish medical research community  