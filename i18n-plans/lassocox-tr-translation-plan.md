# Lassocox Function - Turkish (TR) Translation Plan

Generated on 2026-03-08 for jamovi module ClinicoPath

## 0) Argument Normalization

**SANITIZED_FN**: `lassocox`

**Target Files Status**: All 4 found
- `jamovi/lassocox.a.yaml` (options) - Found
- `jamovi/lassocox.u.yaml` (UI) - Found
- `jamovi/lassocox.r.yaml` (results) - Found
- `R/lassocox.b.R` (backend) - Found

## 1) NAMESPACE i18n Hook

**STATUS**: Already present in NAMESPACE (line 964)

```r
importFrom(jmvcore, .)
```

The translation helper `.` function is properly imported for i18n string wrapping.

## 2) Translatable Strings Requiring `.()` Wrapper

### 2.1 Error & Warning Messages

**Current unwrapped strings in `R/lassocox.b.R`:**

```r
# Line 205
stop("Time variable contains missing values. Please remove or impute missing observations.")

# Line 208
stop("Time variable contains negative values. Survival times must be non-negative.")

# Line 211
warning("Time variable contains zero values. Consider adding a small constant or reviewing data.")

# Line 224-226
stop(paste0("Outcome variable must be binary (exactly 2 levels). Found ",
           length(unique_levels), " levels: ",
           paste(unique_levels, collapse = ", ")))

# Line 230
stop("Please specify a valid outcome level that represents the event of interest.")

# Line 239
stop("At least 2 explanatory variables are required for Lasso regression.")

# Line 255-256
stop(paste0("The following variables are constant and will be removed: ",
           paste(constant_var_names, collapse = ", ")))

# Line 265-266
stop(paste0("Too few complete cases for analysis (", n_complete,
           "). Need at least 10 complete observations."))

# Line 270-271
stop(paste0("Too few events for analysis (", n_events,
           "). Need at least 5 events for reliable estimation."))

# Line 277-278
warning(paste0("Low events per variable ratio (", n_events, " events, ",
              n_predictors, " variables). Consider reducing variables or using stronger regularization."))

# Line 290-291
stop(paste0("Factor variable '", var_name,
           "' has insufficient variation in complete cases."))

# Line 301
stop("No valid predictors remaining after processing. Check variable coding.")

# Line 310
stop("No valid predictors after removing constant/invalid columns.")

# Line 316-317
stop(paste0("Error creating design matrix: ", e$message,
           ". Check factor variable coding and missing values."))

# Line 335
stop("Design matrix contains infinite values. Check for extreme outliers.")

# Line 358
stop("Required packages 'glmnet' and 'survival' not available")

# Line 366
stop("Invalid survival object. Check time and status variables.")

# Line 372-373
warning(paste0("Reduced number of CV folds to ", nfolds,
              " due to small sample size."))

# Line 391
stop("Cross-validation failed. Check data quality and sample size.")

# Line 395
stop(paste0("Error in cross-validation: ", e$message))

# Line 417
stop(paste0("Error fitting final model: ", e$message))

# Line 425
warning("No variables selected by Lasso. Consider using lambda.min or less regularization.")
```

**Recommended patch (error/warning messages):**

```r
# Line 205
stop(.("Time variable contains missing values. Please remove or impute missing observations."))

# Line 208
stop(.("Time variable contains negative values. Survival times must be non-negative."))

# Line 211
warning(.("Time variable contains zero values. Consider adding a small constant or reviewing data."))

# Line 224-226 (use placeholder pattern)
stop(jmvcore::format(.('Outcome variable must be binary (exactly 2 levels). Found {n_levels} levels: {level_list}'),
    list(n_levels = length(unique_levels),
         level_list = paste(unique_levels, collapse = ", "))))

# Line 230
stop(.("Please specify a valid outcome level that represents the event of interest."))

# Line 239
stop(.("At least 2 explanatory variables are required for Lasso regression."))

# Line 255-256
stop(jmvcore::format(.('The following variables are constant and will be removed: {var_list}'),
    list(var_list = paste(constant_var_names, collapse = ", "))))

# Line 265-266
stop(jmvcore::format(.('Too few complete cases for analysis ({n}). Need at least 10 complete observations.'),
    list(n = n_complete)))

# Line 270-271
stop(jmvcore::format(.('Too few events for analysis ({n}). Need at least 5 events for reliable estimation.'),
    list(n = n_events)))

# Line 277-278
warning(jmvcore::format(.('Low events per variable ratio ({n_events} events, {n_vars} variables). Consider reducing variables or using stronger regularization.'),
    list(n_events = n_events, n_vars = n_predictors)))

# Line 290-291
stop(jmvcore::format(.("Factor variable '{var_name}' has insufficient variation in complete cases."),
    list(var_name = var_name)))

# Line 301
stop(.("No valid predictors remaining after processing. Check variable coding."))

# Line 310
stop(.("No valid predictors after removing constant/invalid columns."))

# Line 316-317
stop(jmvcore::format(.('Error creating design matrix: {msg}. Check factor variable coding and missing values.'),
    list(msg = e$message)))

# Line 335
stop(.("Design matrix contains infinite values. Check for extreme outliers."))

# Line 358
stop(.("Required packages 'glmnet' and 'survival' not available"))

# Line 366
stop(.("Invalid survival object. Check time and status variables."))

# Line 372-373
warning(jmvcore::format(.('Reduced number of CV folds to {n} due to small sample size.'),
    list(n = nfolds)))

# Line 391
stop(.("Cross-validation failed. Check data quality and sample size."))

# Line 395
stop(jmvcore::format(.('Error in cross-validation: {msg}'), list(msg = e$message)))

# Line 417
stop(jmvcore::format(.('Error fitting final model: {msg}'), list(msg = e$message)))

# Line 425
warning(.("No variables selected by Lasso. Consider using lambda.min or less regularization."))
```

### 2.2 Table Row Values (String Literals Used as Display Text)

**Current unwrapped strings:**

```r
# .populateModelSummary() - Lines 529-562
statistic = "Total Variables"
statistic = "Selected Variables"
statistic = "Selection Proportion"
statistic = "Optimal Lambda"
statistic = "Sample Size"
statistic = "Number of Events"
statistic = "Censoring Rate"

# .populateCoefficients() - Line 573
variable = "No variables selected"

# .populatePerformance() - Lines 620-651
metric = "C-index"
metric = "Log-rank p-value"
metric = "Hazard Ratio (High vs Low Risk)"
interpretation = "Significant risk stratification"
interpretation = "Non-significant stratification"

# .interpretCindex() - Lines 657-661
"Not available"
"Poor discrimination"
"Fair discrimination"
"Good discrimination"
"Excellent discrimination"

# .interpretHazardRatio() - Lines 665-668
"Not available"
"Weak risk stratification"
"Moderate risk stratification"
"Strong risk stratification"

# .populateModelComparison() - Lines 1674, 1697
model_type = "LASSO Cox"
model_type = "Standard Cox (all variables)"

# .populateVariableImportance() - Line 1297
variable = "No variables selected"
```

**Recommended patch (table values):**

```r
# .populateModelSummary()
statistic = .("Total Variables")
statistic = .("Selected Variables")
statistic = .("Selection Proportion")
statistic = .("Optimal Lambda")
statistic = .("Sample Size")
statistic = .("Number of Events")
statistic = .("Censoring Rate")

# .populateCoefficients()
variable = .("No variables selected")

# .populatePerformance()
metric = .("C-index")
metric = .("Log-rank p-value")
metric = .("Hazard Ratio (High vs Low Risk)")
interpretation = .("Significant risk stratification")
interpretation = .("Non-significant stratification")

# .interpretCindex()
.("Not available")
.("Poor discrimination")
.("Fair discrimination")
.("Good discrimination")
.("Excellent discrimination")

# .interpretHazardRatio()
.("Not available")
.("Weak risk stratification")
.("Moderate risk stratification")
.("Strong risk stratification")

# .populateModelComparison()
model_type = .("LASSO Cox")
model_type = .("Standard Cox (all variables)")

# .populateVariableImportance()
variable = .("No variables selected")
```

### 2.3 Table Notes

```r
# Line 599-600 - currently unwrapped
table$setNote("scale",
    "Coefficients are on the standardized scale (per 1-SD change in the predictor).")

# Line 624-625 - currently unwrapped
table$setNote("cindex",
    "C-index is the apparent (training) value and may be optimistic. External validation is recommended.")
```

**Recommended patch:**

```r
table$setNote("scale",
    .("Coefficients are on the standardized scale (per 1-SD change in the predictor)."))

table$setNote("cindex",
    .("C-index is the apparent (training) value and may be optimistic. External validation is recommended."))
```

### 2.4 Plot Labels and Titles

```r
# .cvPlot() - Lines 696-699
title = "Cross-Validation Plot"
subtitle = "Blue: lambda.min, Green: lambda.1se"
x = "Log Lambda"
y = "Partial Likelihood Deviance"

# .coefPlot() - Lines 722-729
labels = c("Protective", "Risk Factor")
name = "Effect"
title = "Selected Variables and Coefficients"
x = "Variables"
y = "Coefficient"

# .survivalPlot() - Lines 498-499 (risk group labels used in cut())
labels = c("Low Risk", "High Risk")

# .survivalPlot() - Lines 855-859
title = "Survival Curves by Risk Groups"
xlab = "Time"
ylab = "Survival Probability"
legend.title = "Risk Group"
legend.labs = c("Low Risk", "High Risk")

# Fallback base plot - Lines 866-869
xlab = "Time", ylab = "Survival Probability"
main = "Survival Curves by Risk Groups"
legend = c("Low Risk", "High Risk")
```

**Recommended patch (plot labels):**

```r
# .cvPlot()
title = .("Cross-Validation Plot"),
subtitle = .("Blue: lambda.min, Green: lambda.1se"),
x = .("Log Lambda"),
y = .("Partial Likelihood Deviance")

# .coefPlot()
labels = c(.("Protective"), .("Risk Factor")),
name = .("Effect"),
title = .("Selected Variables and Coefficients"),
x = .("Variables"),
y = .("Coefficient")

# .survivalPlot() risk group labels
labels = c(.("Low Risk"), .("High Risk"))

# .survivalPlot() survminer labels
title = .("Survival Curves by Risk Groups"),
xlab = .("Time"),
ylab = .("Survival Probability"),
legend.title = .("Risk Group"),
legend.labs = c(.("Low Risk"), .("High Risk"))
```

### 2.5 Warning Text in Plots (Grid Graphics)

```r
# Line 745
text_warning <- "No risk scores available.\nLASSO selected no variables.\n\nConsider using:\n..."

# Line 777
text_warning <- "Risk scores are uniform.\nNo discrimination possible.\n\nThis can happen when:\n..."

# Line 801
text_warning <- "Survival data not available.\n\nPlease check that:\n..."

# Line 827
text_warning <- "No complete survival data available.\n\nThis can occur when:\n..."

# Line 874
text_warning <- paste("Error creating survival plot:\n", e$message, ...)
```

**Recommended patch:**

```r
# Line 745
text_warning <- .("No risk scores available.\nLASSO selected no variables.\n\nConsider using:\n\u2022 lambda.min instead of lambda.1se\n\u2022 Less regularization (lower lambda)\n\u2022 More explanatory variables")

# Line 777
text_warning <- .("Risk scores are uniform.\nNo discrimination possible.\n\nThis can happen when:\n\u2022 All selected variables have very small coefficients\n\u2022 The model overfits to noise\n\u2022 Lambda value is inappropriate")

# Line 801
text_warning <- .("Survival data not available.\n\nPlease check that:\n\u2022 Time and outcome variables are properly selected\n\u2022 Data contains valid survival information")

# Line 827
text_warning <- .("No complete survival data available.\n\nThis can occur when:\n\u2022 There are missing values in time or outcome variables\n\u2022 Risk score calculation failed\n\u2022 Data filtering removed all observations")

# Line 874
text_warning <- jmvcore::format(.("Error creating survival plot:\n{msg}\n\nPlease check your data and model parameters."),
    list(msg = e$message))
```

### 2.6 HTML Content Blocks

The following methods produce large HTML blocks that contain translatable text. These are currently passed as raw HTML strings. There are two strategies:

**Strategy A (Recommended for now):** Keep HTML blocks as-is and translate them when the `.po` catalog system supports HTML extraction. These blocks are not automatically extracted by `jmvtools::i18nCreate`.

**Strategy B (Full i18n):** Break HTML into individual translatable strings. This is significant refactoring and is recommended as a future improvement.

Affected methods:
- `.populateLassoExplanation()` (lines 973-1029)
- `.populateMethodologyNotes()` (lines 1032-1111)
- `.populateClinicalGuidance()` (lines 1114-1205)
- `.populateCrossValidationExplanation()` (lines 1208-1232)
- `.populateRegularizationPathExplanation()` (lines 1235-1259)
- `.populateRiskScoreExplanation()` (lines 1262-1288)
- `.generateSuitabilityHtml()` (lines 1578-1651)
- `.assessSuitability()` (lines 1342-1576) - detail/label strings within checks
- Welcome message in `.init()` (lines 95-126)
- Error message HTML in `.run()` (lines 184-190)
- Missing dependencies HTML in `.init()` (lines 74-87)

### 2.7 Suitability Assessment Strings

The `.assessSuitability()` method has many user-visible label and detail strings that should be wrapped:

```r
# Labels (used in all check items)
label = "Events-Per-Variable"
label = "Regularization Need"
label = "Sample Size"
label = "Event Rate"
label = "Multicollinearity"
label = "Data Quality"

# Detail strings (many, ~25 unique strings)
detail = "Excellent EPV ratio. Reliable coefficient estimates expected."
detail = "Adequate for LASSO (which handles low EPV better than standard Cox), but interpret with caution."
# ... etc.

# Overall verdicts
overall_text <- "Some issues require attention before relying on these results."
overall_text <- "Data is suitable for LASSO Cox regression (though standard Cox may also be appropriate)."
overall_text <- "Data is usable but review the flagged items."
overall_text <- "Data is well-suited for LASSO Cox regression."
```

**Recommended patch for labels:**

```r
label = .("Events-Per-Variable")
label = .("Regularization Need")
label = .("Sample Size")
label = .("Event Rate")
label = .("Multicollinearity")
label = .("Data Quality")
```

**Note:** All `detail` strings, `value` strings, and `overall_text` strings in `.assessSuitability()` and `.generateSuitabilityHtml()` should also be wrapped with `.()`. Due to the volume (~40 strings), this is best done as a focused refactoring pass.

## 3) Extraction & Update Commands

### 3.1 Create or update English template

```r
# In R console (from module root)
jmvtools::i18nCreate("en")
jmvtools::i18nUpdate("en")
```

This updates: `jamovi/i18n/en.po`

### 3.2 Prepare Weblate template (POT)

```bash
cp jamovi/i18n/en.po jamovi/i18n/catalog.pot
# Ensure header contains exactly:
# Language: c\n
```

### 3.3 Create/Update Turkish catalog

```r
# In R console
jmvtools::i18nCreate("tr")
jmvtools::i18nUpdate("tr")
```

Outputs: `jamovi/i18n/tr.po`

## 4) Turkish Translation Table

### 4.1 YAML-Extracted Strings (from `.a.yaml`, `.r.yaml`, `.u.yaml`)

These are automatically extracted by `jmvtools` and already appear in `tr.po` with empty `msgstr`:

| Status | msgid | Suggested TR |
|--------|-------|--------------|
| missing | "Lasso-Cox Regression" | "Lasso-Cox Regresyonu" |
| missing | "LASSO Cox" | "LASSO Cox" |
| missing | "Standard L1 penalized Cox regression for variable selection" | "Degisken secimi icin standart L1 cezali Cox regresyonu" |
| missing | "Performs Lasso-penalized Cox regression for variable selection in survival analysis." | "Sagkalim analizinde degisken secimi icin Lasso-cezali Cox regresyonu uygular." |
| missing | "Time Elapsed" | "Gecen Sure" |
| missing | "Outcome" | "Sonuc" |
| missing | "Event Level" | "Olay Duzeyi" |
| missing | "Explanatory Variables" | "Aciklayici Degiskenler" |
| missing | "Lambda Selection Method" | "Lambda Secim Yontemi" |
| missing | "Minimum CV Error" | "Minimum CV Hatasi" |
| missing | "1SE Rule" | "1SE Kurali" |
| missing | "Number of CV Folds" | "CV Katlanma Sayisi" |
| missing | "Standardize Variables" | "Degiskenleri Standartlastir" |
| missing | "Data Suitability Assessment" | "Veri Uygunluk Degerlendirmesi" |
| missing | "Cross-validation Plot" | "Capraz Dogrulama Grafigi" |
| missing | "Coefficient Plot" | "Katsayi Grafigi" |
| missing | "Risk Group Survival Plot" | "Risk Grubu Sagkalim Grafigi" |
| missing | "Add Risk Score to Data" | "Veriye Risk Skoru Ekle" |
| missing | "Show Method Explanations" | "Yontem Aciklamalarini Goster" |
| missing | "Detailed Methodology Notes" | "Ayrintili Metodoloji Notlari" |
| missing | "Clinical Interpretation Guidance" | "Klinik Yorum Rehberi" |
| missing | "Variable Importance Analysis" | "Degisken Onem Analizi" |
| missing | "Model Comparison Analysis" | "Model Karsilastirma Analizi" |
| missing | "Model Summary" | "Model Ozeti" |
| missing | "Selected Variables" | "Secilen Degiskenler" |
| missing | "Variable" | "Degisken" |
| missing | "Coefficient" | "Katsayi" |
| missing | "Hazard Ratio" | "Tehlike Orani" |
| missing | "Importance" | "Onem" |
| missing | "Model Performance" | "Model Performansi" |
| missing | "Metric" | "Metrik" |
| missing | "Value" | "Deger" |
| missing | "Interpretation" | "Yorum" |
| missing | "To Do" | "Yapilacaklar" |
| missing | "Understanding LASSO Cox Regression" | "LASSO Cox Regresyonunu Anlama" |
| missing | "LASSO Cox Methodology Notes" | "LASSO Cox Metodoloji Notlari" |
| missing | "Importance Score" | "Onem Skoru" |
| missing | "Selection Frequency" | "Secilme Sikligi" |
| missing | "Stability Rank" | "Kararlilik Sirasi" |
| missing | "LASSO vs Standard Cox Regression" | "LASSO ve Standart Cox Regresyonu Karsilastirmasi" |
| missing | "Model Type" | "Model Tipi" |
| missing | "N Variables" | "Degisken Sayisi" |
| missing | "C-index" | "C-indeksi" |
| missing | "AIC" | "AIC" |
| missing | "Log-Likelihood" | "Log-Olabilirlik" |
| missing | "Understanding Cross-Validation Plot" | "Capraz Dogrulama Grafigini Anlama" |
| missing | "Understanding Regularization Path" | "Duzenlilestirme Yolunu Anlama" |
| missing | "Understanding Risk Scores and Survival Curves" | "Risk Skorlarini ve Sagkalim Egrilerini Anlama" |
| missing | "Calculated Risk Score from Lasso-Cox Regression" | "Lasso-Cox Regresyonundan Hesaplanan Risk Skoru" |
| missing | "Risk Score Based on Lasso-Cox Model" | "Lasso-Cox Modeline Dayali Risk Skoru" |
| missing | "Data Suitability" | "Veri Uygunlugu" |
| missing | "Model Options" | "Model Secenekleri" |
| missing | "Lambda Selection" | "Lambda Secimi" |
| missing | "Cross-validation" | "Capraz Dogrulama" |
| missing | "Plots" | "Grafikler" |
| missing | "Output Options" | "Cikti Secenekleri" |
| missing | "Save Variables" | "Degiskenleri Kaydet" |
| missing | "Penalized Cox Regression" | "Cezali Cox Regresyonu" |

### 4.2 Backend Strings (require `.()` wrapping first)

These strings will appear in `.po` files only after the `.()` wrappers are applied and `jmvtools::i18nUpdate` is re-run:

| Status | msgid | Suggested TR |
|--------|-------|--------------|
| needs-wrap | "Time variable contains missing values. Please remove or impute missing observations." | "Zaman degiskeni eksik degerler iceriyor. Lutfen eksik gozlemleri kaldirin veya tamamlayin." |
| needs-wrap | "Time variable contains negative values. Survival times must be non-negative." | "Zaman degiskeni negatif degerler iceriyor. Sagkalim sureleri negatif olmamalidir." |
| needs-wrap | "Time variable contains zero values. Consider adding a small constant or reviewing data." | "Zaman degiskeni sifir degerler iceriyor. Kucuk bir sabit eklemeyi veya veriyi gozden gecirmeyi dusunun." |
| needs-wrap | "Please specify a valid outcome level that represents the event of interest." | "Lutfen ilgilenilen olayi temsil eden gecerli bir sonuc duzeyini belirtin." |
| needs-wrap | "At least 2 explanatory variables are required for Lasso regression." | "Lasso regresyonu icin en az 2 aciklayici degisken gereklidir." |
| needs-wrap | "No valid predictors remaining after processing. Check variable coding." | "Isleme sonrasi gecerli tahmin edici kalmadi. Degisken kodlamasini kontrol edin." |
| needs-wrap | "Design matrix contains infinite values. Check for extreme outliers." | "Tasarim matrisi sonsuz degerler iceriyor. Asiri aykiri degerleri kontrol edin." |
| needs-wrap | "Cross-validation failed. Check data quality and sample size." | "Capraz dogrulama basarisiz oldu. Veri kalitesini ve orneklem buyuklugunu kontrol edin." |
| needs-wrap | "No variables selected by Lasso. Consider using lambda.min or less regularization." | "Lasso tarafindan hicbir degisken secilmedi. lambda.min kullanmayi veya daha az duzenlilestirme uygulamayi dusunun." |
| needs-wrap | "Total Variables" | "Toplam Degisken" |
| needs-wrap | "Selected Variables" | "Secilen Degiskenler" |
| needs-wrap | "Selection Proportion" | "Secim Orani" |
| needs-wrap | "Optimal Lambda" | "Optimal Lambda" |
| needs-wrap | "Sample Size" | "Orneklem Buyuklugu" |
| needs-wrap | "Number of Events" | "Olay Sayisi" |
| needs-wrap | "Censoring Rate" | "Sansur Orani" |
| needs-wrap | "No variables selected" | "Hicbir degisken secilmedi" |
| needs-wrap | "C-index" | "C-indeksi" |
| needs-wrap | "Log-rank p-value" | "Log-rank p-degeri" |
| needs-wrap | "Hazard Ratio (High vs Low Risk)" | "Tehlike Orani (Yuksek ve Dusuk Risk)" |
| needs-wrap | "Significant risk stratification" | "Anlamli risk tabakalandirmasi" |
| needs-wrap | "Non-significant stratification" | "Anlamli olmayan tabakalandirma" |
| needs-wrap | "Not available" | "Mevcut degil" |
| needs-wrap | "Poor discrimination" | "Zayif ayrim gucu" |
| needs-wrap | "Fair discrimination" | "Orta duzeyde ayrim gucu" |
| needs-wrap | "Good discrimination" | "Iyi ayrim gucu" |
| needs-wrap | "Excellent discrimination" | "Mukemmel ayrim gucu" |
| needs-wrap | "Weak risk stratification" | "Zayif risk tabakalandirmasi" |
| needs-wrap | "Moderate risk stratification" | "Orta duzeyde risk tabakalandirmasi" |
| needs-wrap | "Strong risk stratification" | "Guclu risk tabakalandirmasi" |
| needs-wrap | "LASSO Cox" | "LASSO Cox" |
| needs-wrap | "Standard Cox (all variables)" | "Standart Cox (tum degiskenler)" |
| needs-wrap | "Coefficients are on the standardized scale (per 1-SD change in the predictor)." | "Katsayilar standartlastirilmis olcekte (tahmin edicideki 1-SS degisim basina)." |
| needs-wrap | "C-index is the apparent (training) value and may be optimistic. External validation is recommended." | "C-indeksi gorunen (egitim) degeridir ve iyimser olabilir. Dis dogrulama onerilir." |
| needs-wrap | "Cross-Validation Plot" | "Capraz Dogrulama Grafigi" |
| needs-wrap | "Blue: lambda.min, Green: lambda.1se" | "Mavi: lambda.min, Yesil: lambda.1se" |
| needs-wrap | "Log Lambda" | "Log Lambda" |
| needs-wrap | "Partial Likelihood Deviance" | "Kismi Olabilirlik Sapma" |
| needs-wrap | "Protective" | "Koruyucu" |
| needs-wrap | "Risk Factor" | "Risk Faktoru" |
| needs-wrap | "Effect" | "Etki" |
| needs-wrap | "Selected Variables and Coefficients" | "Secilen Degiskenler ve Katsayilar" |
| needs-wrap | "Variables" | "Degiskenler" |
| needs-wrap | "Coefficient" | "Katsayi" |
| needs-wrap | "Low Risk" | "Dusuk Risk" |
| needs-wrap | "High Risk" | "Yuksek Risk" |
| needs-wrap | "Survival Curves by Risk Groups" | "Risk Gruplarina Gore Sagkalim Egrileri" |
| needs-wrap | "Time" | "Zaman" |
| needs-wrap | "Survival Probability" | "Sagkalim Olasiligi" |
| needs-wrap | "Risk Group" | "Risk Grubu" |
| needs-wrap | "Events-Per-Variable" | "Degisken Basina Olay" |
| needs-wrap | "Regularization Need" | "Duzenlilestirme Ihtiyaci" |
| needs-wrap | "Event Rate" | "Olay Orani" |
| needs-wrap | "Multicollinearity" | "Coklu Dogrusal Baglanti" |
| needs-wrap | "Data Quality" | "Veri Kalitesi" |

### 4.3 Suitability Assessment Detail Strings

| Status | msgid | Suggested TR |
|--------|-------|--------------|
| needs-wrap | "Excellent EPV ratio. Reliable coefficient estimates expected." | "Mukemmel EPV orani. Guvenilir katsayi tahminleri beklenmektedir." |
| needs-wrap | "Adequate for LASSO (which handles low EPV better than standard Cox), but interpret with caution." | "LASSO icin yeterli (standart Cox'tan dusuk EPV'yi daha iyi ele alir), ancak dikkatli yorumlayin." |
| needs-wrap | "Very low EPV. Results may be unreliable even with LASSO regularization. Consider reducing variables or collecting more data." | "Cok dusuk EPV. LASSO duzenlilestirmesiyle bile sonuclar guvenilmez olabilir. Degiskenleri azaltmayi veya daha fazla veri toplamayi dusunun." |
| needs-wrap | "High-dimensional setting. LASSO regularization is strongly indicated." | "Yuksek boyutlu ortam. LASSO duzenlilestirmesi siddetle onerilmektedir." |
| needs-wrap | "Adequate sample size for penalized regression." | "Cezali regresyon icin yeterli orneklem buyuklugu." |
| needs-wrap | "Balanced event rate. Good for model estimation." | "Dengeli olay orani. Model tahmini icin uygundur." |
| needs-wrap | "No concerning collinearity detected." | "Endise verici coklu dogrusal baglanti tespit edilmedi." |
| needs-wrap | "Complete data with no constant predictors." | "Sabit tahmin edicisi olmayan eksiksiz veri." |
| needs-wrap | "Data is well-suited for LASSO Cox regression." | "Veri LASSO Cox regresyonu icin cok uygundur." |
| needs-wrap | "Some issues require attention before relying on these results." | "Bu sonuclara guvenilmeden once bazi sorunlarin ele alinmasi gerekmektedir." |
| needs-wrap | "Data is usable but review the flagged items." | "Veri kullanilabilir ancak isaretlenen maddeleri gozden gecirin." |

## 5) Consistency & Glossary (TR)

```text
Lasso-Cox Regression      -> Lasso-Cox Regresyonu
Variable Selection         -> Degisken Secimi
Survival Analysis          -> Sagkalim Analizi
Cross-validation           -> Capraz Dogrulama
Regularization             -> Duzenlilestirme
Hazard Ratio (HR)          -> Tehlike Orani (TO)
Confidence Interval (CI)   -> Guven Araligi (GA)
C-index                    -> C-indeksi
Risk Score                 -> Risk Skoru
Risk Stratification        -> Risk Tabakalandirmasi
Discrimination             -> Ayrim Gucu
Events Per Variable (EPV)  -> Degisken Basina Olay (DBO)
Multicollinearity          -> Coklu Dogrusal Baglanti
Censoring Rate             -> Sansur Orani
Log-rank test              -> Log-rank testi
Penalized regression       -> Cezali regresyon
Overfitting                -> Asiri uydurma
Sample Size                -> Orneklem Buyuklugu
Coefficient                -> Katsayi
Survival Probability       -> Sagkalim Olasiligi
P-value                    -> p-degeri
Effect Size                -> Etki Buyuklugu
Area Under Curve (AUC)     -> Egri Alti Alan (EAA)
Standard Error (SE)        -> Standart Hata (SH)
```

**Style notes:**
- Use formal but accessible Turkish, suitable for pathologists and oncologists
- Keep established statistical abbreviations in Latin script (e.g., LASSO, Cox, HR, CI)
- Prefer Turkish equivalents for common statistical terms (p-degeri, not p-value)
- For technical terms without established Turkish equivalents, keep the English with Turkish explanation on first use
- Avoid overly academic Turkish; prefer terms used in Turkish medical journals (e.g., Turk Patoloji Dergisi)

## 6) QA Checklist

| Check | Status | Notes |
|-------|--------|-------|
| All 4 target files exist | PASS | `.a.yaml`, `.u.yaml`, `.r.yaml`, `.b.R` all present |
| NAMESPACE imports `.` from jmvcore | PASS | Line 964: `importFrom(jmvcore,.)` |
| YAML strings extracted to `.po` | PASS | All YAML titles appear in `tr.po` (currently untranslated) |
| Backend strings wrapped with `.()` | NEEDS WORK | ~65 user-visible strings in `.b.R` need wrapping |
| HTML content blocks translatable | DEFERRED | Large HTML blocks in explanation methods - defer to Strategy B |
| `tr.po` entries have translations | NEEDS WORK | All lassocox entries currently have empty `msgstr` |
| Glossary consistency | READY | Glossary defined above |
| Clinical accuracy of TR translations | READY | Translations reviewed for clinical context |

## 7) Weblate Integration (GitHub)

1. The module already has `jamovi/i18n/catalog.pot`, `en.po`, and `tr.po`.
2. To set up Weblate:
   - Create repo: `ClinicoPath-i18n`
   - Add `catalog.pot`, `README.md`, license
   - **Collaborators** -> add Weblate bot
   - **Webhooks** -> add: `https://hosted.weblate.org/hooks/github/`
   - Ask jamovi dev team to add the project to Weblate

## 8) Ready-to-Run Snippets

**After applying `.()` wrappers, run:**

```r
jmvtools::i18nCreate("en"); jmvtools::i18nUpdate("en")
jmvtools::i18nCreate("tr"); jmvtools::i18nUpdate("tr")
```

**Prepare POT:**

```bash
cp jamovi/i18n/en.po jamovi/i18n/catalog.pot
# Edit header: Language: c\n
```

**Quick grep to find remaining unwrapped strings (heuristic):**

```bash
grep -nE '"[^"]+' R/lassocox.b.R | grep -vE '\.\(|name\s*=|type\s*=|family\s*=|format\s*=|#|rowKey|colnames|paste0|sprintf|class\s*=|style\s*='
```

## 9) Implementation Priority

### Phase 1 (High Priority - Do Now)
1. Wrap all `stop()` and `warning()` messages with `.()` (~22 strings)
2. Wrap table row display values (statistic, metric, interpretation) with `.()` (~25 strings)
3. Wrap table notes with `.()` (2 strings)
4. Wrap plot labels with `.()` (~15 strings)
5. Run `jmvtools::i18nUpdate("en")` and `jmvtools::i18nUpdate("tr")`

### Phase 2 (Medium Priority - Next Sprint)
1. Wrap suitability assessment labels and detail strings (~40 strings)
2. Wrap plot warning text strings (~5 strings)

### Phase 3 (Lower Priority - Future)
1. Refactor HTML explanation blocks into individual translatable strings
2. Translate the welcome message HTML
3. Fill all `tr.po` entries with reviewed Turkish translations

**Estimated total strings to wrap:** ~110 in `R/lassocox.b.R`
**Estimated total `.po` entries for lassocox:** ~70 (YAML) + ~110 (backend) = ~180
