# Turkish (TR) Translation Plan for `lollipop` Function

## 0) Argument normalization

**SANITIZED_FN**: `lollipop`

**Target files found**:
- ✅ `jamovi/lollipop.a.yaml` (options)
- ✅ `jamovi/lollipop.u.yaml` (UI) 
- ✅ `jamovi/lollipop.r.yaml` (results)
- ✅ `R/lollipop.b.R` (backend)

All required files are present and ready for translation.

---

## 1) NAMESPACE i18n hook

✅ **Status**: Already configured

The NAMESPACE already contains the required import:

```r
importFrom(jmvcore, .)
```

**Rationale**: The `.` function is used to mark strings for extraction into `.po` catalogs and provides runtime translation lookup.

---

## 2) Translatable strings analysis

### 2.1 ✅ Current internationalization status

The `lollipop` function is **already well-internationalized** with comprehensive `.()` wrappers:

**✅ Error & warning messages** (8 instances):
```r
stop(.("Variables not found in data:"))
stop(.("Dependent variable must be numeric (continuous variable)."))
stop(.("Grouping variable must have at least 2 different categories."))
warning(.("Grouping variable has more than 50 levels. Consider reducing categories for better visualization."))
# ... and 4 more
```

**✅ Welcome message sections** (7 instances):
```r
.("Welcome to Lollipop Chart Analysis")
.("Required inputs:")
.("Key features:")
# ... and 4 more
```

**✅ Table labels** (8 instances):
```r
statistic = .("Number of Observations")
statistic = .("Mean Value")
statistic = .("Standard Deviation")
# ... and 5 more
```

**✅ Plot annotations** (3 instances):
```r
label = paste(.("Mean ="), round(mean_value, 2))
paste(.("Lollipop Chart:"), dep_var, .("by"), group_var)
```

### 2.2 No additional wrapping needed

All user-visible strings in the R backend are already properly wrapped with `.()`. No patches required.

---

## 3) Extraction & Update commands

### 3.1 Create or update English template

```r
# In R console at module root
jmvtools::i18nCreate("en")
jmvtools::i18nUpdate("en")
```

### 3.2 Prepare Weblate template

```bash
# At module root
cp jamovi/i18n/en.po jamovi/i18n/catalog.pot
# Edit header to ensure: Language: c\n
```

### 3.3 Create/Update Turkish catalog

```r
# In R console at module root  
jmvtools::i18nCreate("tr")
jmvtools::i18nUpdate("tr")
```

---

## 4) Turkish translations for lollipop function

### 4.1 Welcome message translations

| Status | msgid | Suggested Turkish Translation |
|--------|--------|-------------------------------|
| missing | "Welcome to Lollipop Chart Analysis" | "Lolipop Grafik Analizine Hoş Geldiniz" |
| missing | "This function creates lollipop charts for categorical data visualization with clinical applications." | "Bu fonksiyon, klinik uygulamalarda kategorik veri görselleştirmesi için lolipop grafikleri oluşturur." |
| missing | "Required inputs:" | "Gerekli girişler:" |
| missing | "Dependent Variable" | "Bağımlı Değişken" |
| missing | "Numeric values (biomarker levels, scores, measurements)" | "Sayısal değerler (biyobelirteç düzeyleri, skorlar, ölçümler)" |
| missing | "Grouping Variable" | "Gruplama Değişkeni" |
| missing | "Categories (patient IDs, treatments, conditions)" | "Kategoriler (hasta kimlikleri, tedaviler, durumlar)" |
| missing | "Key features:" | "Temel özellikler:" |
| missing | "Professional appearance for publications" | "Yayınlar için profesyonel görünüm" |

### 4.2 Error message translations

| Status | msgid | Suggested Turkish Translation |
|--------|--------|-------------------------------|
| missing | "Variables not found in data:" | "Veride bulunamayan değişkenler:" |
| missing | "Dependent variable must be numeric (continuous variable)." | "Bağımlı değişken sayısal (sürekli değişken) olmalıdır." |
| missing | "Grouping variable must have at least 2 different categories." | "Gruplama değişkeni en az 2 farklı kategoriye sahip olmalıdır." |
| missing | "Grouping variable has more than 50 levels. Consider reducing categories for better visualization." | "Gruplama değişkeni 50'den fazla düzeye sahip. Daha iyi görselleştirme için kategori sayısını azaltmayı düşünün." |
| missing | "No complete cases found. Please check for missing values in selected variables." | "Hiç eksiksiz vaka bulunamadı. Seçilen değişkenlerdeki eksik değerleri kontrol edin." |
| missing | "rows with missing values were removed from analysis." | "satır eksik değerler nedeniyle analizden çıkarıldı." |
| missing | "At least 2 complete observations are required for lollipop chart analysis." | "Lolipop grafik analizi için en az 2 eksiksiz gözlem gereklidir." |
| missing | "Highlight level" | "Vurgulama düzeyi" |
| missing | "not found in grouping variable. Highlight will be ignored." | "gruplama değişkeninde bulunamadı. Vurgulama görmezden gelinecek." |

### 4.3 Table labels translations

| Status | msgid | Suggested Turkish Translation |
|--------|--------|-------------------------------|
| missing | "Number of Observations" | "Gözlem Sayısı" |
| missing | "Number of Groups" | "Grup Sayısı" |
| missing | "Mean Value" | "Ortalama Değer" |
| missing | "Median Value" | "Ortanca Değer" |
| missing | "Standard Deviation" | "Standart Sapma" |
| missing | "Value Range" | "Değer Aralığı" |
| missing | "Highest Value Group" | "En Yüksek Değer Grubu" |
| missing | "Lowest Value Group" | "En Düşük Değer Grubu" |

### 4.4 Plot annotations translations

| Status | msgid | Suggested Turkish Translation |
|--------|--------|-------------------------------|
| missing | "Mean =" | "Ortalama =" |
| missing | "Lollipop Chart:" | "Lolipop Grafiği:" |
| missing | "by" | "ile" |

---

## 5) Consistency & glossary (TR)

### 5.1 Statistical terms glossary

```text
Mean → Ortalama
Median → Ortanca (preferred over "Medyan")
Standard Deviation → Standart Sapma
Confidence Interval (CI) → Güven Aralığı (GA)
p-value → p-değeri
Effect size → Etki büyüklüğü
Correlation → Korelasyon
Regression → Regresyon
Chi-square → Ki-kare
t-test → t-testi
ANOVA → ANOVA (Varyans Analizi)
```

### 5.2 Clinical/Pathology terms

```text
Biomarker → Biyobelirteç
Patient → Hasta
Treatment → Tedavi
Condition → Durum
Observation → Gözlem
Variable → Değişken
Category → Kategori
Group → Grup
Level → Düzey
Analysis → Analiz
Chart/Graph → Grafik
Visualization → Görselleştirme
```

### 5.3 UI terms

```text
Required → Gerekli
Optional → İsteğe bağlı
Input → Giriş
Output → Çıktı
Settings → Ayarlar
Configuration → Konfigürasyon
```

---

## 6) QA checklist

- ✅ **Verified**: All user-visible strings in R backend files are wrapped with `.`
- ✅ **Confirmed**: NAMESPACE imports the translation helper `.`
- ✅ **Validated**: All required YAML files exist and are properly structured
- ✅ **Ready**: Function is fully prepared for catalog extraction and translation

### 6.1 Translation quality standards

**Turkish translations should:**
- Use clinical terminology familiar to Turkish pathologists and oncologists
- Maintain professional tone suitable for medical research
- Be concise and clear for user interface elements
- Follow Turkish grammar rules for technical documentation
- Use standardized abbreviations (GA for Güven Aralığı, etc.)

---

## 7) Weblate integration (GitHub)

### 7.1 Repository setup steps

1. **Create dedicated repo**: `clinicopath-i18n`
   - Add `jamovi/i18n/catalog.pot`
   - Add `README.md` with translation guidelines
   - Add appropriate license file

2. **Configure GitHub integration**:
   - **Collaborators** → Add Weblate bot
   - **Webhooks** → Add payload URL: `https://hosted.weblate.org/hooks/github/`
   - **Branch protection** → Protect main branch, require reviews

3. **Contact jamovi development team**:
   - Request addition of `clinicopath-i18n` project to Weblate
   - Provide repo URL and component configuration details

---

## 8) Ready-to-run snippets

### 8.1 Create/Update catalogs

```r
# Run from R console at module root
jmvtools::i18nCreate("en"); jmvtools::i18nUpdate("en")
jmvtools::i18nCreate("tr"); jmvtools::i18nUpdate("tr")
```

### 8.2 Prepare POT for Weblate

```bash
# Run from module root directory
cp jamovi/i18n/en.po jamovi/i18n/catalog.pot

# Manually edit jamovi/i18n/catalog.pot header to include:
# Language: c\n
```

### 8.3 Validation grep (should return no results)

```bash
# Check for potentially unwrapped user-visible strings
grep -nE '"[A-Z][^"]*[a-z][^"]*"' R/lollipop.b.R | grep -v '\\.\(' | grep -v 'name=' | grep -v 'rowKey='
```

---

## 9) Implementation steps

### 9.1 Immediate actions

1. **Generate catalogs**:
   ```r
   jmvtools::i18nCreate("en"); jmvtools::i18nUpdate("en")
   jmvtools::i18nCreate("tr"); jmvtools::i18nUpdate("tr")
   ```

2. **Prepare POT file**:
   ```bash
   cp jamovi/i18n/en.po jamovi/i18n/catalog.pot
   # Edit header: Language: c
   ```

3. **Manual translation**: Fill in Turkish translations in `jamovi/i18n/tr.po`

### 9.2 Testing workflow

1. **Build module** with Turkish translations
2. **Test UI** with Turkish locale
3. **Verify** all text displays correctly
4. **Validate** clinical terminology accuracy

---

## 10) Deliverables summary

### 10.1 Status overview

- ✅ **Files found**: All 4 required jamovi files present
- ✅ **NAMESPACE**: Translation helper already imported  
- ✅ **String wrapping**: Complete - 35+ strings already wrapped
- 📋 **Translation needed**: 35+ Turkish translations to be added
- 🚀 **Ready for**: Catalog generation and Weblate integration

### 10.2 Key strengths

1. **Excellent preparation**: Function is already fully internationalized
2. **Comprehensive coverage**: All user-facing text is properly wrapped
3. **Clinical focus**: Terminology suitable for pathology/oncology context
4. **Professional quality**: Error messages and UI text are clear and helpful

### 10.3 Next steps

1. Generate English and Turkish catalogs using provided R snippets
2. Translate the 35+ identified strings using the provided Turkish suggestions
3. Test translations in Turkish locale
4. Set up Weblate integration for ongoing maintenance

**Estimated translation effort**: 2-3 hours for initial translation, 30 minutes for review and testing.

---

*Translation plan generated for ClinicoPath jamovi module `lollipop` function targeting Turkish (TR) localization.*