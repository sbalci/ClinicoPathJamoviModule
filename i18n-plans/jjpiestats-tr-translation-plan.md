# jjpiestats Turkish Translation Plan (TR)

**Generated on:** $(date)  
**Function:** jjpiestats  
**Target Language:** Turkish (TR)  
**Status:** Ready for translation

---

## 0) Function Analysis Summary

**SANITIZED_FN:** `jjpiestats`

### Files Found
✅ **jamovi/jjpiestats.a.yaml** - Options definition (10 parameters)  
✅ **jamovi/jjpiestats.u.yaml** - User interface controls  
✅ **jamovi/jjpiestats.r.yaml** - Results definition (4 output items)  
✅ **R/jjpiestats.b.R** - Backend implementation (539 lines)  

All required files are present and ready for i18n preparation.

---

## 1) NAMESPACE i18n Hook

✅ **Translation helper import already present:**

```r
importFrom(jmvcore, .)  # Line 838
```

The `.` function for string extraction is already available. No changes needed.

---

## 2) Translatable Strings Analysis

### 2.1 Error & Warning Messages (15 strings)

All error and warning messages have been wrapped with `.()` in the backend:

| Category | Count | Examples |
|----------|-------|----------|
| Validation errors | 6 | Variable not found, continuous data, insufficient variation |
| Statistical warnings | 4 | Small group sizes, invalid ratios |
| Data preparation errors | 3 | Empty dataset, missing values, plot preparation failed |
| User guidance | 2 | Performance notes, troubleshooting tips |

### 2.2 User Interface Content (8 strings)

Progress messages and user guidance content:

| Category | Count | Examples |
|----------|-------|----------|
| Welcome messages | 4 | Welcome to ClinicoPath, tool description |
| Status updates | 4 | Data processing, analysis ready, statistical method |

### 2.3 YAML Files (Auto-extracted)

YAML strings are automatically extracted by jamovi's i18n system:
- **10 option titles** in `.a.yaml`
- **8 UI control labels** in `.u.yaml`  
- **4 result titles** in `.r.yaml`

---

## 3) Turkish Translation Suggestions

### 3.1 Error Messages

| Status | msgid | Suggested Turkish Translation |
|--------|-------|-------------------------------|
| new | "Variables not found in data:" | "Değişkenler veri setinde bulunamadı:" |
| new | "Variable '{var}' appears to be continuous ({count} unique values). Pie charts are for categorical data. Consider converting to groups first." | "'{var}' değişkeni sürekli görünüyor ({count} farklı değer). Pasta grafikleri kategorik veriler içindir. Önce gruplara dönüştürmeyi düşünün." |
| new | "Small group sizes detected ({groups}). Chi-square tests require minimum 5 observations per group for reliable results." | "Küçük grup boyutları tespit edildi ({groups}). Ki-kare testleri güvenilir sonuçlar için grup başına minimum 5 gözlem gerektirir." |
| new | "Grouping variable must have at least 2 categories for comparison." | "Karşılaştırma için gruplama değişkeninin en az 2 kategorisi olmalı." |
| new | "Variable '{var}' has insufficient variation (only {count} level). Need at least 2 categories for meaningful pie chart." | "'{var}' değişkeninin varyasyonu yetersiz (sadece {count} seviye). Anlamlı pasta grafiği için en az 2 kategori gerekli." |
| new | "No complete data rows available after handling missing values. Please check your data." | "Eksik değerler işlendikten sonra tam veri satırı kalmadı. Lütfen verinizi kontrol edin." |
| new | "Invalid ratio specification - contains non-numeric values. Using equal proportions." | "Geçersiz oran belirtimi - sayısal olmayan değerler içeriyor. Eşit oranlar kullanılıyor." |
| new | "Ratios sum to {sum} but should sum to 1. Using equal proportions." | "Oranlar toplamı {sum} ama 1 olmalı. Eşit oranlar kullanılıyor." |
| new | "Ratios must be positive. Using equal proportions." | "Oranlar pozitif olmalı. Eşit oranlar kullanılıyor." |
| new | "Error parsing ratio: {error}. Using equal proportions." | "Oran ayrıştırma hatası: {error}. Eşit oranlar kullanılıyor." |
| new | "Dataset is empty. Please ensure your data contains observations." | "Veri seti boş. Lütfen verinizin gözlemler içerdiğinden emin olun." |
| new | "Plot preparation failed: {error}" | "Grafik hazırlama başarısız: {error}" |
| new | "Grouped plot preparation failed: {error}" | "Gruplu grafik hazırlama başarısız: {error}" |

### 3.2 User Interface Messages

| Status | msgid | Suggested Turkish Translation |
|--------|-------|-------------------------------|
| new | "Welcome to ClinicoPath" | "ClinicoPath'e Hoş Geldiniz" |
| new | "This tool will help you generate Pie Charts with statistical analysis." | "Bu araç istatistiksel analiz ile pasta grafikleri oluşturmanıza yardımcı olacak." |
| new | "This function uses ggplot2 and ggstatsplot packages. See documentations ggpiestats and grouped_ggpiestats." | "Bu fonksiyon ggplot2 ve ggstatsplot paketlerini kullanır. ggpiestats ve grouped_ggpiestats belgelerine bakınız." |
| new | "Please cite jamovi and the packages as given below." | "Lütfen jamovi ve paketleri aşağıda verildiği şekilde kaynak gösterin." |
| new | "Processing data for pie chart analysis..." | "Pasta grafiği analizi için veri işleniyor..." |
| new | "Preparing pie chart analysis options..." | "Pasta grafiği analiz seçenekleri hazırlanıyor..." |
| new | "Variable:" | "Değişken:" |
| new | "grouped by" | "şuna göre gruplandı" |
| new | "split by" | "şuna göre bölündü" |
| new | "Pie chart analysis ready" | "Pasta grafiği analizi hazır" |
| new | "Data prepared" | "Veri hazırlandı" |
| new | "observations" | "gözlem" |
| new | "Statistical method" | "İstatistiksel yöntem" |
| new | "analysis" | "analiz" |
| new | "Preparation time" | "Hazırlık süresi" |
| new | "seconds" | "saniye" |

### 3.3 User Guidance Messages

| Status | msgid | Suggested Turkish Translation |
|--------|-------|-------------------------------|
| new | "Performance Note:" | "Performans Notu:" |
| new | "Bayesian analysis is computationally intensive." | "Bayesyen analiz hesaplama açısından yoğundur." |
| new | "Tip:" | "İpucu:" |
| new | "Use Data > Transform to create categorical groups from continuous variables." | "Sürekli değişkenlerden kategorik gruplar oluşturmak için Veri > Dönüştür kullanın." |
| new | "Consider combining small categories or collecting more data." | "Küçük kategorileri birleştirmeyi veya daha fazla veri toplamayı düşünün." |
| new | "Ensure your variables have multiple categories for meaningful pie charts." | "Anlamlı pasta grafikleri için değişkenlerinizin birden çok kategorisi olduğundan emin olun." |
| new | "Error in Pie Chart Analysis" | "Pasta Grafiği Analizinde Hata" |
| new | "General Troubleshooting" | "Genel Sorun Giderme" |
| new | "Ensure dependent variable is categorical" | "Bağımlı değişkenin kategorik olduğundan emin olun" |
| new | "Check that selected variables exist in your dataset" | "Seçilen değişkenlerin veri setinizde mevcut olduğunu kontrol edin" |
| new | "Verify sufficient sample sizes in each category (≥5 recommended)" | "Her kategoride yeterli örneklem büyüklüğü olduğunu doğrulayın (≥5 önerilir)" |
| new | "Confirm variables have adequate variation (≥2 categories)" | "Değişkenlerin yeterli varyasyona sahip olduğunu onaylayın (≥2 kategori)" |

---

## 4) Clinical Terminology Consistency (TR)

### 4.1 Statistical Terms
```text
Pie Chart → Pasta Grafiği
Chi-square test → Ki-kare testi
Fisher's exact test → Fisher'in kesin testi
Bayes Factor → Bayes Faktörü
Confidence Interval (CI) → Güven Aralığı (GA)
p-value → p-değeri
Effect size → Etki büyüklüğü
Contingency table → Olasılık tablosu
Statistical significance → İstatistiksel anlamlılık
```

### 4.2 Data Analysis Terms
```text
Variable → Değişken
Observation → Gözlem
Category → Kategori
Group → Grup
Dataset → Veri seti
Missing values → Eksik değerler
Sample size → Örneklem büyüklüğü
Proportion → Oran
```

### 4.3 User Interface Terms
```text
Analysis → Analiz
Settings → Ayarlar
Options → Seçenekler
Results → Sonuçlar
Output → Çıktı
Processing → İşleme
Preparation → Hazırlık
```

---

## 5) Ready-to-Run Commands

### 5.1 Create/Update Catalogs

```r
# Create English template
jmvtools::i18nCreate("en")
jmvtools::i18nUpdate("en")

# Create Turkish catalog
jmvtools::i18nCreate("tr") 
jmvtools::i18nUpdate("tr")
```

### 5.2 Prepare POT for Weblate

```bash
# Copy English catalog as template
cp jamovi/i18n/en.po jamovi/i18n/catalog.pot

# Ensure POT header contains:
# Language: c\n
```

### 5.3 Validation Commands

```bash
# Check for unwrapped strings (heuristic)
grep -nE '"[^"]+' R/jjpiestats.b.R | grep -v '\.\('

# Validate YAML files
jamovi-compiler --check jamovi/jjpiestats.*.yaml
```

---

## 6) QA Checklist

### 6.1 Technical Validation
- ✅ All user-visible strings wrapped with `.()` in R backend
- ✅ NAMESPACE imports translation helper `.`
- ✅ All jamovi files present and valid
- ✅ Placeholder format `{var}` used correctly
- ✅ Error messages preserve technical context

### 6.2 Translation Quality
- ✅ Turkish translations use clinical terminology
- ✅ Medical terms consistent with Turkish medical literature  
- ✅ User-friendly language for pathologists/clinicians
- ✅ Formal tone appropriate for statistical software
- ✅ Placeholder variables preserved in translations

### 6.3 Clinical Context
- ✅ Statistical method names translated appropriately
- ✅ Error messages provide actionable guidance
- ✅ UI terminology consistent with medical practice
- ✅ Help text maintains clinical accuracy

---

## 7) Weblate Integration Steps

### 7.1 Repository Setup
1. Create dedicated repo: `ClinicoPathJamoviModule-i18n`
2. Add files:
   - `catalog.pot` (template)
   - `README.md` (translation guidelines)
   - `LICENSE` (same as main project)

### 7.2 Weblate Configuration
1. **Collaborators** → Add Weblate bot
2. **Webhooks** → Add:
   - Payload URL: `https://hosted.weblate.org/hooks/github/`
   - Events: Push, Pull request
3. **Contact jamovi team** to add project to Weblate instance

---

## 8) Implementation Priority

### Phase 1: Core Messages (High Priority)
- Error messages (validation, data issues)
- Welcome and status messages  
- Basic troubleshooting tips

### Phase 2: UI Enhancement (Medium Priority)
- Advanced option descriptions
- Detailed help content
- Performance guidance

### Phase 3: Documentation (Low Priority)
- Extended tooltips
- Statistical method explanations
- Citation information

---

## 9) Maintenance Guidelines

### 9.1 Adding New Strings
1. Always wrap with `.()` in R code
2. Use `{placeholder}` format for variables
3. Update translation catalogs with `jmvtools::i18nUpdate()`
4. Test with Turkish locale before release

### 9.2 Translation Updates
1. Review clinical accuracy regularly
2. Update terminology based on user feedback
3. Maintain consistency across all jamovi functions
4. Coordinate with Turkish medical terminology standards

---

## 10) Expected Outcomes

After full implementation:

✅ **Complete Turkish localization** of jjpiestats function  
✅ **Clinical terminology consistency** across all messages  
✅ **User-friendly error reporting** in Turkish  
✅ **Professional medical interface** for Turkish clinicians  
✅ **Maintainable translation workflow** through Weblate  

**Estimated effort:** 2-3 hours for initial translation + 30 minutes per update

---

*This plan provides a comprehensive roadmap for Turkish localization of the jjpiestats function, ensuring clinical accuracy and professional quality for Turkish-speaking pathologists and researchers.*