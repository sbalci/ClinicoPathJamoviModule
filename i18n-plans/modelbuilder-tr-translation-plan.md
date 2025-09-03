# Internationalization Plan: modelbuilder → Turkish (TR)

## Files Analysis

✅ **All required files found:**
- `jamovi/modelbuilder.a.yaml` (options) - Present
- `jamovi/modelbuilder.u.yaml` (UI) - Present  
- `jamovi/modelbuilder.r.yaml` (results) - Present
- `R/modelbuilder.b.R` (backend) - Present

✅ **NAMESPACE check passed** - `importFrom(jmvcore, .)` already present.

## Patch Suggestions - R/modelbuilder.b.R

### 2.1 Error & Warning Messages

**Current unwrapped strings → Wrapped versions:**

```r
# Around line 90
- stop("No data available for analysis")
+ stop(.("No data available for analysis"))

# Around line 95  
- stop("Outcome variable must be specified")
+ stop(.("Outcome variable must be specified"))

# Around line 99
- stop(paste("Outcome variable", outcome_var, "not found in data"))
+ stop(.("Outcome variable '{var}' not found in data", var = outcome_var))

# Around line 107
- stop(paste("Outcome variable must be binary. Found", length(unique_outcomes), "levels:", paste(unique_outcomes, collapse = ", ")))
+ stop(.("Outcome variable must be binary. Found {n} levels: {levels}", 
+       n = length(unique_outcomes), 
+       levels = paste(unique_outcomes, collapse = ", ")))

# Around line 113
- stop(paste("Positive outcome level must be one of:", paste(unique_outcomes, collapse = ", ")))
+ stop(.("Positive outcome level must be one of: {levels}", 
+       levels = paste(unique_outcomes, collapse = ", ")))

# Around line 117
- stop("At least one model type must be selected")
+ stop(.("At least one model type must be selected"))

# Around line 121
- stop("Sample size too small. Minimum 50 observations required")
+ stop(.("Sample size too small. Minimum {min} observations required", min = 50))

# Around line 125
- stop("Too few events. Minimum 10 events required for stable model fitting")
+ stop(.("Too few events. Minimum {min} events required for stable model fitting", min = 10))

# Around line 132 (warning)
- warning(paste("Low events per variable ratio (", round(epv, 1), "). Consider reducing predictors or using penalized regression"))
+ warning(.("Low events per variable ratio ({epv}). Consider reducing predictors or using penalized regression", 
+          epv = round(epv, 1)))
```

### 2.2 HTML Instructions Content

**Large instruction block (lines 1126-1163):**

```r
instructions <- paste0(
    "<html><head></head><body>",
    "<div class='instructions' style='background-color: #f8f9fa; padding: 20px; border-radius: 8px; margin: 20px 0;'>",
    "<h3 style='color: #2e7d32; margin-top: 0;'>🏗️ ", .("Prediction Model Builder"), "</h3>",
    "<p><strong>", .("Build and validate prediction models for medical decision making."), "</strong></p>",
    "<p>", .("Creates logistic regression models that output predicted probabilities for use in Decision Curve Analysis."), "</p>",
    
    "<h4 style='color: #2e7d32;'>", .("Required Steps:"), "</h4>",
    "<ol>",
    "<li><strong>", .("Select Outcome Variable:"), "</strong> ", .("Choose a binary outcome to predict"), "</li>",
    "<li><strong>", .("Specify Positive Level:"), "</strong> ", .("Define which level represents the positive outcome"), "</li>",
    "<li><strong>", .("Choose Model Types:"), "</strong> ", .("Select at least one model to build:"), 
        "<ul>",
        "<li><strong>", .("Basic Clinical Model:"), "</strong> ", .("Core demographic and primary risk factors"), "</li>",
        "<li><strong>", .("Enhanced Clinical Model:"), "</strong> ", .("Additional clinical variables and interactions"), "</li>",
        "<li><strong>", .("Biomarker Model:"), "</strong> ", .("Laboratory values and advanced diagnostics"), "</li>",
        "<li><strong>", .("Custom Model:"), "</strong> ", .("User-defined variable combination"), "</li>",
        "</ul>",
    "</li>",
    "<li><strong>", .("Configure Options:"), "</strong> ", .("Set validation, missing data handling, and output preferences"), "</li>",
    "</ol>",
    
    "<h4 style='color: #2e7d32;'>", .("Advanced Features:"), "</h4>",
    "<ul>",
    "<li>", .("Automatic data splitting for unbiased validation"), "</li>",
    "<li>", .("Multiple imputation for missing data"), "</li>",
    "<li>", .("Cross-validation and bootstrap validation"), "</li>",
    "<li>", .("Comprehensive performance metrics"), "</li>",
    "<li>", .("Seamless integration with Decision Curve Analysis"), "</li>",
    "</ul>",
    
    "<p><strong>", .("The module will create predicted probability columns that can be directly used in Decision Curve Analysis."), "</strong></p>",
    "</div></body></html>"
)
```

### 2.3 Data Summary and Results Content

**Data summary HTML generation (around line 1342):**

```r
data_summary <- paste0(
    "<html><body>",
    "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin: 10px 0;'>",
    "<h4 style='color: #2e7d32; margin-top: 0;'>📊 ", .("Data Summary"), "</h4>",
    "<p><strong>", .("Total Sample Size:"), "</strong> ", n_total, "</p>",
    "<p><strong>", .("Training Set:"), "</strong> ", n_training, " (", round(n_training/n_total*100, 1), "%)</p>",
    if (n_validation > 0) paste0("<p><strong>", .("Validation Set:"), "</strong> ", n_validation, " (", round(n_validation/n_total*100, 1), "%)</p>") else "",
    "<p><strong>", .("Event Rate:"), "</strong> ", round(event_rate, 1), "% (", outcome_positive, ")</p>",
    "<p><strong>", .("Events in Training:"), "</strong> ", n_events_training, "</p>",
    if (n_validation > 0) paste0("<p><strong>", .("Events in Validation:"), "</strong> ", n_events_validation, "</p>") else "",
    missing_summary,
    "</div></body></html>"
)
```

### 2.4 Performance Metrics Display

**Performance metrics HTML (around line 262):**

```r
perf_html <- paste0(
    "<html><body>",
    "<div style='background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin: 10px 0; border-left: 4px solid #17a2b8;'>",
    "<h5 style='color: #17a2b8; margin-top: 0;'>⚡ ", .("Performance Metrics"), "</h5>",
    "<table style='width: 100%; font-size: 12px;'>",
    "<tr><th>", .("Operation"), "</th><th>", .("Time (s)"), "</th><th>", .("Timestamp"), "</th></tr>"
)
```

### 2.5 Warning and Error Messages Throughout Code

Additional error/warning messages that need wrapping:

```r
# Missing data warnings
- warning(paste("glmnet package not available for penalized regression"))
+ warning(.("glmnet package not available for penalized regression"))

# Convergence warnings  
- warning(paste("Model", model_name, "did not converge"))
+ warning(.("Model '{model}' did not converge", model = model_name))

# Separation warnings
- warning(paste("Model", model_name, "may have separation issues"))
+ warning(.("Model '{model}' may have separation issues", model = model_name))

# Performance warnings
- warning("Cross-validation folds should typically be between 3 and 20.")
+ warning(.("Cross-validation folds should typically be between 3 and 20."))

- warning("Bootstrap replications should typically be between 50 and 2000.")
+ warning(.("Bootstrap replications should typically be between 50 and 2000."))
```

## Turkish Translation Table

| Status | msgid | msgstr (suggested TR) |
|--------|-------|----------------------|
| missing | "No data available for analysis" | "Analiz için veri mevcut değil" |
| missing | "Outcome variable must be specified" | "Sonuç değişkeni belirtilmelidir" |
| missing | "Outcome variable '{var}' not found in data" | "'{var}' sonuç değişkeni veri setinde bulunamadı" |
| missing | "Outcome variable must be binary. Found {n} levels: {levels}" | "Sonuç değişkeni ikili olmalıdır. {n} düzey bulundu: {levels}" |
| missing | "Positive outcome level must be one of: {levels}" | "Pozitif sonuç düzeyi şunlardan biri olmalıdır: {levels}" |
| missing | "At least one model type must be selected" | "En az bir model türü seçilmelidir" |
| missing | "Sample size too small. Minimum {min} observations required" | "Örneklem boyutu çok küçük. Minimum {min} gözlem gereklidir" |
| missing | "Too few events. Minimum {min} events required for stable model fitting" | "Çok az olay. Kararlı model oluşturma için minimum {min} olay gereklidir" |
| missing | "Low events per variable ratio ({epv}). Consider reducing predictors or using penalized regression" | "Değişken başına düşük olay oranı ({epv}). Tahmin edicileri azaltmayı veya cezalı regresyon kullanmayı düşünün" |
| missing | "Prediction Model Builder" | "Tahmin Model Oluşturucu" |
| missing | "Build and validate prediction models for medical decision making." | "Tıbbi karar verme için tahmin modelleri oluşturun ve doğrulayın." |
| missing | "Creates logistic regression models that output predicted probabilities for use in Decision Curve Analysis." | "Karar Eğrisi Analizi'nde kullanılmak üzere tahmin edilen olasılıkları çıkaran lojistik regresyon modelleri oluşturur." |
| missing | "Required Steps:" | "Gerekli Adımlar:" |
| missing | "Select Outcome Variable:" | "Sonuç Değişkenini Seçin:" |
| missing | "Choose a binary outcome to predict" | "Tahmin edilecek ikili bir sonuç seçin" |
| missing | "Specify Positive Level:" | "Pozitif Düzeyi Belirtin:" |
| missing | "Define which level represents the positive outcome" | "Hangi düzeyin pozitif sonucu temsil ettiğini tanımlayın" |
| missing | "Choose Model Types:" | "Model Türlerini Seçin:" |
| missing | "Select at least one model to build:" | "Oluşturulacak en az bir model seçin:" |
| missing | "Basic Clinical Model:" | "Temel Klinik Model:" |
| missing | "Core demographic and primary risk factors" | "Temel demografik ve birincil risk faktörleri" |
| missing | "Enhanced Clinical Model:" | "Gelişmiş Klinik Model:" |
| missing | "Additional clinical variables and interactions" | "Ek klinik değişkenler ve etkileşimler" |
| missing | "Biomarker Model:" | "Biyobelirteç Modeli:" |
| missing | "Laboratory values and advanced diagnostics" | "Laboratuvar değerleri ve ileri tanı yöntemleri" |
| missing | "Custom Model:" | "Özel Model:" |
| missing | "User-defined variable combination" | "Kullanıcı tanımlı değişken kombinasyonu" |
| missing | "Configure Options:" | "Seçenekleri Yapılandırın:" |
| missing | "Set validation, missing data handling, and output preferences" | "Doğrulama, eksik veri işleme ve çıktı tercihlerini ayarlayın" |
| missing | "Advanced Features:" | "Gelişmiş Özellikler:" |
| missing | "Automatic data splitting for unbiased validation" | "Tarafsız doğrulama için otomatik veri bölme" |
| missing | "Multiple imputation for missing data" | "Eksik veriler için çoklu veri atama" |
| missing | "Cross-validation and bootstrap validation" | "Çapraz doğrulama ve önyükleme doğrulaması" |
| missing | "Comprehensive performance metrics" | "Kapsamlı performans metrikleri" |
| missing | "Seamless integration with Decision Curve Analysis" | "Karar Eğrisi Analizi ile sorunsuz entegrasyon" |
| missing | "The module will create predicted probability columns that can be directly used in Decision Curve Analysis." | "Modül, Karar Eğrisi Analizi'nde doğrudan kullanılabilecek tahmin edilen olasılık sütunları oluşturacaktır." |
| missing | "Data Summary" | "Veri Özeti" |
| missing | "Total Sample Size:" | "Toplam Örneklem Boyutu:" |
| missing | "Training Set:" | "Eğitim Seti:" |
| missing | "Validation Set:" | "Doğrulama Seti:" |
| missing | "Event Rate:" | "Olay Oranı:" |
| missing | "Events in Training:" | "Eğitimdeki Olaylar:" |
| missing | "Events in Validation:" | "Doğrulamadaki Olaylar:" |
| missing | "Performance Metrics" | "Performans Metrikleri" |
| missing | "Operation" | "İşlem" |
| missing | "Time (s)" | "Süre (s)" |
| missing | "Timestamp" | "Zaman Damgası" |

## Consistency & Glossary (TR)

### Statistical Terms

```text
Prediction Model → Tahmin Modeli
Logistic Regression → Lojistik Regresyon  
Cross-Validation → Çapraz Doğrulama
Bootstrap → Önyükleme
ROC Curve → ROC Eğrisi
AUC (Area Under Curve) → EAA (Eğri Altı Alan)
Calibration → Kalibrasyon
Discrimination → Ayırt Etme
Net Reclassification Index (NRI) → Net Yeniden Sınıflandırma İndeksi (YSİ)
Integrated Discrimination Index (IDI) → Entegre Ayırt Etme İndeksi (EAİ)
Brier Score → Brier Skoru
Confidence Interval (CI) → Güven Aralığı (GA)
Odds Ratio (OR) → Odds Oranı (OO)
P-value → p-değeri
```

### Clinical Terms

```text
Outcome Variable → Sonuç Değişkeni
Predictor Variable → Tahmin Edici Değişken
Biomarker → Biyobelirteç
Clinical Decision Making → Klinik Karar Verme
Risk Factor → Risk Faktörü  
Missing Data → Eksik Veri
Sample Size → Örneklem Boyutu
Event Rate → Olay Oranı
```

## Ready-to-Run Snippets

**Create/Update catalogs**

```r
jmvtools::i18nCreate("en"); jmvtools::i18nUpdate("en")
jmvtools::i18nCreate("tr"); jmvtools::i18nUpdate("tr")
```

**Prepare POT**

```bash
cp jamovi/i18n/en.po jamovi/i18n/catalog.pot
# Edit header to ensure: Language: c\n
```

**Find unwrapped strings (heuristic)**

```bash
grep -nE '\"[^\"]+\"' R/modelbuilder.b.R | grep -v '\\.\(' | grep -E '(stop|warning|error|message)'
```

## QA Checklist

- [ ] **NAMESPACE**: ✅ `importFrom(jmvcore, .)` present
- [ ] **R backend strings**: ❌ Need to wrap ~30 error/warning messages and HTML content
- [ ] **YAML files**: ✅ Present (will be extracted automatically)
- [ ] **Translation quality**: Turkish translations ready with clinical terminology
- [ ] **Consistency**: Turkish glossary follows medical/statistical standards

## Weblate Integration Steps

1. **Create repo**: `ClinicoPath-i18n` 
2. **Add files**: `catalog.pot`, Turkish translations, `README.md`
3. **GitHub settings**: 
   - Add Weblate bot as collaborator
   - Add webhook: `https://hosted.weblate.org/hooks/github/`
4. **Contact jamovi team**: Request addition to Weblate hosted instance

## Implementation Priority

**High Priority (Essential):**
1. Error and warning messages (user-facing errors)
2. Main instruction text (primary user guidance)
3. Data summary labels (displayed in results)

**Medium Priority:**
1. Performance metrics labels
2. Advanced feature descriptions
3. HTML content formatting

**Low Priority:**
1. Debug messages (if any)
2. Internal logging strings

## Estimated Scope

- **Total translatable strings**: ~40-50
- **High-impact strings**: ~25-30 
- **Turkish translation effort**: 2-3 hours for medical accuracy
- **Testing effort**: 1-2 hours for UI verification

The modelbuilder function is a complex medical statistics module requiring precise clinical terminology in Turkish translations. Focus should be on accuracy for medical professionals (pathologists, oncologists) who will use these prediction models for clinical decision-making.