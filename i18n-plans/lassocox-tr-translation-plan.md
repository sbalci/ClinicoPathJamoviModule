# Lassocox Function - Turkish (TR) Translation Plan

Generated on 2026-03-08 (updated) for jamovi module ClinicoPath

---

## 0) Argument Normalization

**SANITIZED_FN**: `lassocox`

**Target Files Status**: All 4 found
- `jamovi/lassocox.a.yaml` (options) -- Found
- `jamovi/lassocox.u.yaml` (UI) -- Found
- `jamovi/lassocox.r.yaml` (results) -- Found
- `R/lassocox.b.R` (backend) -- Found

---

## 1) NAMESPACE i18n Hook

**STATUS**: Already present in NAMESPACE (line 963)

```r
importFrom(jmvcore, .)
```

The translation helper `.` function is properly imported. No changes needed.

---

## 2) Translatable Strings Requiring `.()` Wrapper in `R/lassocox.b.R`

All user-visible strings in the backend R file must be wrapped with `.()` for translation extraction. YAML strings are extracted automatically and do not need wrapping.

### 2.1 Error & Warning Messages (stop/warning calls)

| Line | Current String | Proposed Change |
|------|---------------|----------------|
| 240 | `stop("At least 2 explanatory variables are required for Lasso regression.")` | `stop(.("At least 2 explanatory variables are required for Lasso regression."))` |
| 254-258 | `stop(paste0("Outcome variable must be binary among observed values. Found ", ...))` | `stop(jmvcore::format(.('Outcome variable must be binary among observed values. Found {n} levels: {levels}'), list(n=length(observed_levels), levels=paste(observed_levels, collapse=", "))))` |
| 267-270 | `stop(paste0("Selected event level ('", event_level_used, "') is not present in observed outcome data."))` | `stop(jmvcore::format(.("Selected event level ('{level}') is not present in observed outcome data."), list(level=event_level_used)))` |
| 279-282 | `stop(paste0("Numeric outcome must have exactly 2 observed values. Found: ", ...))` | `stop(jmvcore::format(.("Numeric outcome must have exactly 2 observed values. Found: {values}"), list(values=paste(observed_levels, collapse=", "))))` |
| 289 | `stop("For numeric outcomes, Event Level must be one of the observed outcome values.")` | `stop(.("For numeric outcomes, Event Level must be one of the observed outcome values."))` |
| 314-317 | `warning(paste0("Removed constant explanatory variables: ", ...))` | `warning(jmvcore::format(.("Removed constant explanatory variables: {vars}"), list(vars=paste(constant_var_names, collapse=", "))))` |
| 320 | `stop("No valid explanatory variables remain after removing constant predictors.")` | `stop(.("No valid explanatory variables remain after removing constant predictors."))` |
| 323 | `warning("Only one non-constant explanatory variable remains; LASSO selection is limited.")` | `warning(.("Only one non-constant explanatory variable remains; LASSO selection is limited."))` |
| 332-335 | `stop(paste0("Too few complete cases for analysis (", n_complete, "). Need at least 10 complete observations."))` | `stop(jmvcore::format(.("Too few complete cases for analysis ({n}). Need at least 10 complete observations."), list(n=n_complete)))` |
| 338-341 | `warning(paste0("Excluded ", n_excluded, " row(s) with missing values..."))` | `warning(jmvcore::format(.("Excluded {n} row(s) with missing values in time/outcome/predictors (complete-case analysis)."), list(n=n_excluded)))` |
| 348 | `stop("Time variable contains non-finite values after filtering. Please correct the input.")` | `stop(.("Time variable contains non-finite values after filtering. Please correct the input."))` |
| 351 | `stop("Time variable contains negative values. Survival times must be non-negative.")` | `stop(.("Time variable contains negative values. Survival times must be non-negative."))` |
| 354 | `warning("Time variable contains zero values. Consider adding a small constant if convergence issues occur.")` | `warning(.("Time variable contains zero values. Consider adding a small constant if convergence issues occur."))` |
| 359 | `stop("Outcome is not binary after complete-case filtering. Check event level and missing-data pattern.")` | `stop(.("Outcome is not binary after complete-case filtering. Check event level and missing-data pattern."))` |
| 364-367 | `stop(paste0("Too few events for analysis (", n_events, "). Need at least 5 events for reliable estimation."))` | `stop(jmvcore::format(.("Too few events for analysis ({n}). Need at least 5 events for reliable estimation."), list(n=n_events)))` |
| 373-376 | `warning(paste0("Low events-per-predictor ratio (..."))` | `warning(jmvcore::format(.("Low events-per-predictor ratio ({n_events} events, {n_pred} predictors). Consider stronger regularization or fewer predictors."), list(n_events=n_events, n_pred=n_predictors)))` |
| 386-389 | `stop(paste0("Factor variable '", var_name, "' has insufficient variation in complete cases."))` | `stop(jmvcore::format(.("Factor variable '{var}' has insufficient variation in complete cases."), list(var=var_name)))` |
| 396 | `stop("No valid predictors remaining after model-matrix encoding.")` | `stop(.("No valid predictors remaining after model-matrix encoding."))` |
| 403 | `stop("No valid predictors after removing degenerate design-matrix columns.")` | `stop(.("No valid predictors after removing degenerate design-matrix columns."))` |
| 407-410 | `stop(paste0("Error creating design matrix: ", e$message, ...))` | `stop(jmvcore::format(.("Error creating design matrix: {msg}. Check factor coding and missing values."), list(msg=e$message)))` |
| 430 | `stop("Design matrix contains infinite values. Check for extreme outliers.")` | `stop(.("Design matrix contains infinite values. Check for extreme outliers."))` |
| 501 | `stop("Required packages 'glmnet' and 'survival' not available")` | `stop(.("Required packages 'glmnet' and 'survival' not available"))` |
| 509 | `stop("Invalid survival object. Check time and status variables.")` | `stop(.("Invalid survival object. Check time and status variables."))` |
| 516-519 | `warning(paste0("Reduced number of CV folds to ", nfolds, ...))` | `warning(jmvcore::format(.("Reduced number of CV folds to {n} due to sample size constraints."), list(n=nfolds)))` |
| 529 | `warning("Could not create stratified CV folds; using default fold assignment.")` | `warning(.("Could not create stratified CV folds; using default fold assignment."))` |
| 554 | `stop("Cross-validation failed. Check data quality and sample size.")` | `stop(.("Cross-validation failed. Check data quality and sample size."))` |
| 558 | `stop(paste0("Error in cross-validation: ", e$message))` | `stop(jmvcore::format(.("Error in cross-validation: {msg}"), list(msg=e$message)))` |
| 580 | `stop(paste0("Error fitting final model: ", e$message))` | `stop(jmvcore::format(.("Error fitting final model: {msg}"), list(msg=e$message)))` |
| 588 | `warning("No variables selected by Lasso. Consider using lambda.min or less regularization.")` | `warning(.("No variables selected by Lasso. Consider using lambda.min or less regularization."))` |
| 647 | `stop("Unable to form two risk groups from risk scores.")` | `stop(.("Unable to form two risk groups from risk scores."))` |

### 2.2 Table Row Values (statistic/metric/variable labels)

| Line | Current String | Proposed Change |
|------|---------------|----------------|
| 681 | `statistic = "Total Variables"` | `statistic = .("Total Variables")` |
| 686 | `statistic = "Selected Variables"` | `statistic = .("Selected Variables")` |
| 691 | `statistic = "Selection Proportion"` | `statistic = .("Selection Proportion")` |
| 696 | `statistic = "Optimal Lambda"` | `statistic = .("Optimal Lambda")` |
| 701 | `statistic = "Sample Size"` | `statistic = .("Sample Size")` |
| 706 | `statistic = "Number of Events"` | `statistic = .("Number of Events")` |
| 711 | `statistic = "Censoring Rate"` | `statistic = .("Censoring Rate")` |
| 716 | `statistic = "Event Level Used"` | `statistic = .("Event Level Used")` |
| 722 | `statistic = "Rows Excluded (Missing Data)"` | `statistic = .("Rows Excluded (Missing Data)")` |
| 736 | `variable = "No variables selected"` | `variable = .("No variables selected")` |
| 783 | `metric = "C-index"` | `metric = .("C-index")` |
| 794 | `metric = "Log-rank p-value"` | `metric = .("Log-rank p-value")` |
| 797 | `"Significant risk stratification"` / `"Non-significant stratification"` | `.("Significant risk stratification")` / `.("Non-significant stratification")` |
| 810 | `metric = "Hazard Ratio (High vs Low Risk)"` | `metric = .("Hazard Ratio (High vs Low Risk)")` |
| 1460 | `variable = "No variables selected"` | `variable = .("No variables selected")` |
| 1845 | `model_type = "Post-LASSO Standard Cox (Selected Variables)"` | `model_type = .("Post-LASSO Standard Cox (Selected Variables)")` |
| 1869 | `model_type = "Standard Cox (all variables)"` | `model_type = .("Standard Cox (all variables)")` |

### 2.3 Interpretation Helper Return Values

| Line | Current String | Proposed Change |
|------|---------------|----------------|
| 821 | `return("Not available")` | `return(.("Not available"))` |
| 822 | `return("Poor discrimination")` | `return(.("Poor discrimination"))` |
| 823 | `return("Fair discrimination")` | `return(.("Fair discrimination"))` |
| 824 | `return("Good discrimination")` | `return(.("Good discrimination"))` |
| 825 | `return("Excellent discrimination")` | `return(.("Excellent discrimination"))` |
| 829 | `return("Not available")` | `return(.("Not available"))` |
| 830 | `return("Weak risk stratification")` | `return(.("Weak risk stratification"))` |
| 831 | `return("Moderate risk stratification")` | `return(.("Moderate risk stratification"))` |
| 832 | `return("Strong risk stratification")` | `return(.("Strong risk stratification"))` |

### 2.4 Table Notes (setNote)

| Line | Current String | Proposed Change |
|------|---------------|----------------|
| 763 | `"Coefficients are on the standardized scale (per 1-SD change in the predictor)."` | `.("Coefficients are on the standardized scale (per 1-SD change in the predictor).")` |
| 788 | `"All performance metrics are apparent (training) values..."` | `.("All performance metrics are apparent (training) values computed on the same data used for model fitting. They may be optimistic. External validation is recommended.")` |
| 808-809 | `"CIs and p-values are from an unpenalized Cox model refitted..."` | `.("CIs and p-values are from an unpenalized Cox model refitted with LASSO-selected variables (post-selection inference). These do not account for the variable selection step and may be anti-conservative.")` |
| 811-812 | `"CIs and p-values could not be computed..."` | `.("CIs and p-values could not be computed (post-selection Cox refit failed or p >= n).")` |
| 1878-1880 | `"C-index values in this table are from the corresponding unpenalized Cox models..."` | `.("C-index values in this table are from the corresponding unpenalized Cox models for comparability. Apparent penalized-model C-index is reported in Model Performance.")` |

### 2.5 Plot Labels & Titles

| Line | Current String | Proposed Change |
|------|---------------|----------------|
| 859 | `title = "Cross-Validation Plot"` | `title = .("Cross-Validation Plot")` |
| 860 | `subtitle = "Blue: lambda.min, Green: lambda.1se"` | `subtitle = .("Blue: lambda.min, Green: lambda.1se")` |
| 861 | `x = "Log Lambda"` | `x = .("Log Lambda")` |
| 862 | `y = "Partial Likelihood Deviance"` | `y = .("Partial Likelihood Deviance")` |
| 889 | `title = "Selected Variables and Coefficients"` | `title = .("Selected Variables and Coefficients")` |
| 891 | `"Coefficients reflect standardized effect sizes."` | `.("Coefficients reflect standardized effect sizes.")` |
| 893 | `x = "Variables"` | `x = .("Variables")` |
| 894 | `y = "Coefficient"` | `y = .("Coefficient")` |
| 937-940 | `labels = c("Protective", "Risk Factor"), name = "Effect"` | `labels = c(.("Protective"), .("Risk Factor")), name = .("Effect")` |
| 1019 | `title = "Survival Curves by Risk Groups"` | `title = .("Survival Curves by Risk Groups")` |
| 1020 | `xlab = "Time"` | `xlab = .("Time")` |
| 1021 | `ylab = "Survival Probability"` | `ylab = .("Survival Probability")` |
| 1022 | `legend.title = "Risk Group"` | `legend.title = .("Risk Group")` |
| 1023 | `legend.labs = c("Low Risk", "High Risk")` | `legend.labs = c(.("Low Risk"), .("High Risk"))` |
| 1029-1030 | Base plot: `xlab = "Time", ylab = "Survival Probability", main = "Survival Curves by Risk Groups"` | Same wrapping pattern |
| 1031 | `legend = c("Low Risk", "High Risk")` | `legend = c(.("Low Risk"), .("High Risk"))` |

### 2.6 Risk Group Labels in Data Functions

| Line | Current String | Proposed Change |
|------|---------------|----------------|
| 482 | `labels = c("Low Risk", "High Risk")` | `labels = c(.("Low Risk"), .("High Risk"))` |
| 488 | `ifelse(risk_scores <= med, "Low Risk", "High Risk")` | `ifelse(risk_scores <= med, .("Low Risk"), .("High Risk"))` |
| 489 | `factor(groups, levels = c("Low Risk", "High Risk"))` | `factor(groups, levels = c(.("Low Risk"), .("High Risk")))` |

**Note**: The `.makeBinaryRiskGroups` method is called from within `private$`, so `self` is available. No special handling needed.

### 2.7 Plot Warning Text Strings

| Line | Current String | Proposed Change |
|------|---------------|----------------|
| 909 | `"No risk scores available.\nLASSO selected no variables..."` | `.("No risk scores available.\nLASSO selected no variables.\n\nConsider using:\n\u2022 lambda.min instead of lambda.1se\n\u2022 Less regularization (lower lambda)\n\u2022 More explanatory variables")` |
| 941 | `"Risk scores are uniform.\nNo discrimination possible..."` | `.("Risk scores are uniform.\nNo discrimination possible.\n\nThis can happen when:\n\u2022 All selected variables have very small coefficients\n\u2022 The model overfits to noise\n\u2022 Lambda value is inappropriate")` |
| 965 | `"Survival data not available..."` | `.("Survival data not available.\n\nPlease check that:\n\u2022 Time and outcome variables are properly selected\n\u2022 Data contains valid survival information")` |
| 991 | `"No complete survival data available..."` | `.("No complete survival data available.\n\nThis can occur when:\n\u2022 There are missing values in time or outcome variables\n\u2022 Risk score calculation failed\n\u2022 Data filtering removed all observations")` |
| 1038 | `"Error creating survival plot:\n"` + `e$message` | Use `jmvcore::format(.("Error creating survival plot:\n{msg}\n\nPlease check your data and model parameters."), list(msg=e$message))` |

### 2.8 HTML Content Blocks (Explanation/Guidance)

The following methods generate large HTML blocks. These strings are long and contain HTML markup. The **visible text** within these blocks should be wrapped individually:

- `.populateLassoExplanation()` (lines 1143-1200) -- HTML content with explanatory text
- `.populateMethodologyNotes()` (lines 1202-1282) -- HTML content with methodology details
- `.populateClinicalGuidance()` (lines 1284-1376) -- HTML content with clinical guidance
- `.populateCrossValidationExplanation()` (lines 1378-1403) -- HTML content
- `.populateRegularizationPathExplanation()` (lines 1405-1424) -- HTML content
- `.populateRiskScoreExplanation()` (lines 1426-1453) -- HTML content

**Approach for HTML blocks**: Rather than wrapping individual phrases inside HTML strings (which is fragile), wrap the entire HTML content string with `.()`. This allows translators to produce a fully translated HTML block.

Example:
```r
# Current
html_content <- "<h3>Understanding LASSO Cox Regression</h3>..."

# Proposed
html_content <- .("<h3>Understanding LASSO Cox Regression</h3>...")
```

### 2.9 Suitability Assessment Strings

The `.assessSuitability()` method (lines 1506-1745) and `.generateSuitabilityHtml()` method (lines 1747-1820) contain many user-visible strings embedded in lists. These include:

| Category | Examples | Count |
|----------|----------|-------|
| Labels | `"Events-Per-Variable"`, `"Regularization Need"`, `"Sample Size"`, `"Event Rate"`, `"Multicollinearity"`, `"Data Quality"` | 6 |
| Detail strings | `"Excellent EPV ratio..."`, `"Adequate for LASSO..."`, etc. | ~18 |
| Overall verdicts | `"Data is well-suited..."`, `"Some issues require attention..."`, `"Data is usable but..."` | 3 |
| Recommendation strings | `"Consider Kaplan-Meier curves..."`, `"Use Multivariable Survival..."`, etc. | 5 |
| HTML text | `"Overall: "`, `"Status"`, `"Check"`, `"Value"`, `"Detail"`, `"Recommendations:"` | 6 |
| Interpretation note | `"This assessment is advisory..."` | 1 |

All of these should be wrapped with `.()`.

### 2.10 Strings That Should NOT Be Wrapped

- Column `name` identifiers: `'p_value'`, `'statistic'`, `'variable'`, etc.
- R object names, function names, package names
- Variable keys: `rowKey`, note keys like `"cindex"`, `"scale"`, `"refit"`
- HTML tag structure (only the text content within tags)
- CSS style strings

---

## 3) Extraction & Update Commands

### 3.1 Create/Update English Template

```r
# In R console, from the module root
jmvtools::i18nCreate("en")
jmvtools::i18nUpdate("en")
```

This creates/updates:
```
jamovi/i18n/en.po
```

### 3.2 Prepare Weblate Template (POT)

```bash
cp jamovi/i18n/en.po jamovi/i18n/catalog.pot
# Edit header: Language: c\n
```

### 3.3 Create/Update Turkish Catalog

```r
jmvtools::i18nCreate("tr")
jmvtools::i18nUpdate("tr")
```

Outputs:
```
jamovi/i18n/tr.po
```

---

## 4) Turkish Translation Table

### 4.1 YAML-Extracted Strings (from .a.yaml, .r.yaml, .u.yaml)

| Status | msgid | msgstr (current) | Suggested TR |
|--------|-------|------------------|-------------|
| untranslated | "Lasso-Cox Regression" | "" | "Lasso-Cox Regresyonu" |
| untranslated | "LASSO Cox" | "" | "LASSO Cox" (keep as-is, proper noun) |
| untranslated | "Variable Selection with Lasso Regularization" | "" | "Lasso Regularizasyonu ile Degisken Secimi" |
| untranslated | "Performs Lasso-penalized Cox regression for variable selection in survival analysis." | "" | "Sagkalim analizinde degisken secimi icin Lasso cezali Cox regresyonu yapar." |
| untranslated | "Time Elapsed" | "" | "Gecen Sure" |
| untranslated | "Outcome" | "" | "Sonuc" |
| untranslated | "Event Level" | "" | "Olay Duzeyi" |
| untranslated | "Explanatory Variables" | "" | "Aciklayici Degiskenler" |
| untranslated | "Lambda Selection Method" | "" | "Lambda Secim Yontemi" |
| untranslated | "Minimum CV Error" | "" | "Minimum CV Hatasi" |
| untranslated | "1SE Rule" | "" | "1SE Kurali" |
| untranslated | "Number of CV Folds" | "" | "CV Katman Sayisi" |
| untranslated | "Random Seed" | "" | "Rastgele Tohum" |
| untranslated | "Standardize Variables" | "" | "Degiskenleri Standartlastir" |
| untranslated | "Data Suitability Assessment" | "" | "Veri Uygunluk Degerlendirmesi" |
| untranslated | "Cross-validation Plot" | "" | "Capraz Dogrulama Grafigi" |
| untranslated | "Coefficient Plot" | "" | "Katsayi Grafigi" |
| untranslated | "Risk Group Survival Plot" | "" | "Risk Grubu Sagkalim Grafigi" |
| untranslated | "Add Risk Score to Data" | "" | "Risk Skorunu Veriye Ekle" |
| untranslated | "Show Method Explanations" | "" | "Yontem Aciklamalarini Goster" |
| untranslated | "Detailed Methodology Notes" | "" | "Ayrintili Metodoloji Notlari" |
| untranslated | "Clinical Interpretation Guidance" | "" | "Klinik Yorum Rehberi" |
| untranslated | "Variable Importance Analysis" | "" | "Degisken Onem Analizi" |
| untranslated | "Model Comparison Analysis" | "" | "Model Karsilastirma Analizi" |
| untranslated | "Model Summary" | "" | "Model Ozeti" |
| untranslated | "Selected Variables" | "" | "Secilen Degiskenler" |
| untranslated | "Model Performance" | "" | "Model Performansi" |
| untranslated | "Statistic" | "" | "Istatistik" |
| untranslated | "Value" | "" | "Deger" |
| untranslated | "Variable" | "" | "Degisken" |
| untranslated | "Coefficient" | "" | "Katsayi" |
| untranslated | "Hazard Ratio" | "" | "Tehlike Orani" |
| untranslated | "Importance" | "" | "Onem" |
| untranslated | "Metric" | "" | "Metrik" |
| untranslated | "Interpretation" | "" | "Yorum" |
| untranslated | "LASSO Cox Methodology Notes" | "" | "LASSO Cox Metodoloji Notlari" |
| untranslated | "LASSO vs Standard Cox Regression" | "" | "LASSO ve Standart Cox Regresyonu Karsilastirmasi" |
| untranslated | "Understanding LASSO Cox Regression" | "" | "LASSO Cox Regresyonunu Anlama" |
| untranslated | "Understanding Cross-Validation Plot" | "" | "Capraz Dogrulama Gradigini Anlama" |
| untranslated | "Understanding Regularization Path" | "" | "Regularizasyon Yolunu Anlama" |
| untranslated | "Understanding Risk Scores and Survival Curves" | "" | "Risk Skorlarini ve Sagkalim Egrilerini Anlama" |
| untranslated | "Importance Score" | "" | "Onem Skoru" |
| untranslated | "Path Inclusion Proportion" | "" | "Yol Dahil Etme Orani" |
| untranslated | "Importance Rank" (labeled Stability Rank) | "" | "Onem Sirasi" |
| untranslated | "Model Type" | "" | "Model Tipi" |
| untranslated | "N Variables" | "" | "Degisken Sayisi" |
| untranslated | "C-index" | "" | "C-indeksi" |
| untranslated | "AIC" | "" | "AIC" (keep as-is) |
| untranslated | "Log-Likelihood" | "" | "Log-Olabilirlik" |
| untranslated | "Penalized Cox Regression" | "" | "Cezali Cox Regresyonu" |
| untranslated | "Save Variables" | "" | "Degiskenleri Kaydet" |
| untranslated | "Cross-validation" | "" | "Capraz Dogrulama" |
| untranslated | "Model Options" | "" | "Model Secenekleri" |
| untranslated | "Plots" | "" | "Grafikler" |
| untranslated | "Output Options" | "" | "Cikti Secenekleri" |
| untranslated | "Explanatory Output" | "" | "Aciklayici Ciktilar" |

### 4.2 Backend R Strings (to be wrapped with `.()` then extracted)

| Status | msgid | Suggested TR |
|--------|-------|-------------|
| new | "Total Variables" | "Toplam Degiskenler" |
| new | "Selected Variables" | "Secilen Degiskenler" |
| new | "Selection Proportion" | "Secim Orani" |
| new | "Optimal Lambda" | "Optimal Lambda" |
| new | "Sample Size" | "Orneklem Buyuklugu" |
| new | "Number of Events" | "Olay Sayisi" |
| new | "Censoring Rate" | "Sansurleme Orani" |
| new | "Event Level Used" | "Kullanilan Olay Duzeyi" |
| new | "Rows Excluded (Missing Data)" | "Haric Tutulan Satirlar (Eksik Veri)" |
| new | "No variables selected" | "Degisken secilmedi" |
| new | "C-index" | "C-indeksi" |
| new | "Log-rank p-value" | "Log-rank p-degeri" |
| new | "Significant risk stratification" | "Anlamli risk siniflamasi" |
| new | "Non-significant stratification" | "Anlamli olmayan siniflama" |
| new | "Hazard Ratio (High vs Low Risk)" | "Tehlike Orani (Yuksek vs Dusuk Risk)" |
| new | "Not available" | "Mevcut degil" |
| new | "Poor discrimination" | "Zayif ayirt edicilik" |
| new | "Fair discrimination" | "Orta ayirt edicilik" |
| new | "Good discrimination" | "Iyi ayirt edicilik" |
| new | "Excellent discrimination" | "Mukemmel ayirt edicilik" |
| new | "Weak risk stratification" | "Zayif risk siniflamasi" |
| new | "Moderate risk stratification" | "Orta risk siniflamasi" |
| new | "Strong risk stratification" | "Guclu risk siniflamasi" |
| new | "Cross-Validation Plot" | "Capraz Dogrulama Grafigi" |
| new | "Blue: lambda.min, Green: lambda.1se" | "Mavi: lambda.min, Yesil: lambda.1se" |
| new | "Log Lambda" | "Log Lambda" |
| new | "Partial Likelihood Deviance" | "Kismi Olabilirlik Sapmasi" |
| new | "Selected Variables and Coefficients" | "Secilen Degiskenler ve Katsayilar" |
| new | "Coefficients reflect standardized effect sizes." | "Katsayilar standartlastirilmis etki buyukluklerini yansitir." |
| new | "Variables" | "Degiskenler" |
| new | "Protective" | "Koruyucu" |
| new | "Risk Factor" | "Risk Faktoru" |
| new | "Effect" | "Etki" |
| new | "Survival Curves by Risk Groups" | "Risk Gruplarina Gore Sagkalim Egrileri" |
| new | "Time" | "Zaman" |
| new | "Survival Probability" | "Sagkalim Olasiligi" |
| new | "Risk Group" | "Risk Grubu" |
| new | "Low Risk" | "Dusuk Risk" |
| new | "High Risk" | "Yuksek Risk" |
| new | "Post-LASSO Standard Cox (Selected Variables)" | "Post-LASSO Standart Cox (Secilen Degiskenler)" |
| new | "Standard Cox (all variables)" | "Standart Cox (tum degiskenler)" |

### 4.3 Suitability Assessment Strings

| Status | msgid | Suggested TR |
|--------|-------|-------------|
| new | "Events-Per-Variable" | "Degisken Basina Olay" |
| new | "Regularization Need" | "Regularizasyon Gereksinimi" |
| new | "Sample Size" | "Orneklem Buyuklugu" |
| new | "Event Rate" | "Olay Orani" |
| new | "Multicollinearity" | "Coklubaglantilik" |
| new | "Data Quality" | "Veri Kalitesi" |
| new | "Excellent EPV ratio. Reliable coefficient estimates expected." | "Mukemmel EPV orani. Guvenilir katsayi tahminleri beklenmektedir." |
| new | "Adequate for LASSO..." | "LASSO icin yeterli (dusuk EPV'yi standart Cox'tan daha iyi idare eder), ancak dikkatli yorumlayin." |
| new | "Very low EPV. Results may be unreliable..." | "Cok dusuk EPV. LASSO regularizasyonu ile bile sonuclar guvenilmez olabilir. Degisken azaltmayi veya daha fazla veri toplamayi dusunun." |
| new | "Data is well-suited for LASSO Cox regression." | "Veri LASSO Cox regresyonu icin cok uygundur." |
| new | "Some issues require attention before relying on these results." | "Bu sonuclara guvenilmeden once bazi sorunlarin giderilmesi gerekmektedir." |
| new | "Data is usable but review the flagged items." | "Veri kullanilabilir ancak isaret edilen ogeler gozden gecirilmelidir." |
| new | "This assessment is advisory..." | "Bu degerlendirme bilgi amacidir. Analiz sonuctan bagimsiz olarak devam eder. Yesil = sorun yok, Sari = dikkatli ilerleyin, Kirmizi = sonuclar guvenilmez olabilir." |
| new | "Status" | "Durum" |
| new | "Check" | "Kontrol" |
| new | "Detail" | "Detay" |
| new | "Recommendations:" | "Oneriler:" |

### 4.4 Error/Warning Messages

| Status | msgid | Suggested TR |
|--------|-------|-------------|
| new | "At least 2 explanatory variables are required for Lasso regression." | "Lasso regresyonu icin en az 2 aciklayici degisken gereklidir." |
| new | "Time variable contains negative values. Survival times must be non-negative." | "Zaman degiskeni negatif degerler iceriyor. Sagkalim sureleri negatif olmamalidir." |
| new | "Cross-validation failed. Check data quality and sample size." | "Capraz dogrulama basarisiz oldu. Veri kalitesini ve orneklem buyuklugunu kontrol edin." |
| new | "No valid explanatory variables remain after removing constant predictors." | "Sabit tahmin ediciler cikarildiktan sonra gecerli aciklayici degisken kalmadi." |
| new | "Design matrix contains infinite values. Check for extreme outliers." | "Tasarim matrisi sonsuz degerler iceriyor. Asiri uc degerleri kontrol edin." |

### 4.5 Table Note Strings

| Status | msgid | Suggested TR |
|--------|-------|-------------|
| new | "Coefficients are on the standardized scale (per 1-SD change in the predictor)." | "Katsayilar standartlastirilmis olcekte (tahmin edicideki 1-SS degisimi basina) verilmistir." |
| new | "All performance metrics are apparent (training) values computed on the same data used for model fitting. They may be optimistic. External validation is recommended." | "Tum performans metrikleri model uydurma icin kullanilan ayni veri uzerinde hesaplanan gorunur (egitim) degerleridir. Iyimser olabilirler. Dis dogrulama onerilir." |
| new | "CIs and p-values are from an unpenalized Cox model refitted with LASSO-selected variables (post-selection inference). These do not account for the variable selection step and may be anti-conservative." | "GA'lar ve p-degerleri, LASSO ile secilen degiskenlerle yeniden uydurulmus cezasiz Cox modelinden elde edilmistir (secim sonrasi cikarim). Degisken secim adimini hesaba katmazlar ve anti-tutucu olabilirler." |

---

## 5) Consistency & Glossary (TR)

```text
t-test              -> t-testi
Mann-Whitney U      -> Mann-Whitney U testi
Confidence Interval (CI) -> Guven Araligi (GA)
Effect size         -> Etki buyuklugu
Odds Ratio (OR)     -> Odds Orani (OO)
Hazard Ratio (HR)   -> Tehlike Orani (TO)
Area Under Curve (AUC) -> Egri Alti Alan (EAA)
False Discovery Rate (FDR) -> Yanlis Kesif Orani (YKO)
C-index             -> C-indeksi
Cross-validation    -> Capraz dogrulama
Lambda              -> Lambda (keep as-is)
LASSO               -> LASSO (keep as-is, acronym)
Regularization      -> Regularizasyon
Variable selection  -> Degisken secimi
Risk score          -> Risk skoru
Survival            -> Sagkalim
Events              -> Olaylar
Censoring           -> Sansurleme
Coefficient         -> Katsayi
Predictor           -> Tahmin edici / Aciklayici degisken
Discrimination      -> Ayirt edicilik
Stratification      -> Siniflama / Katmanlama
Overfitting         -> Asiri uydurma
Sample size         -> Orneklem buyuklugu
Standard error      -> Standart hata
Multicollinearity   -> Coklubaglantilik
Missing data        -> Eksik veri
Complete cases      -> Tam gozlemler
Design matrix       -> Tasarim matrisi
```

### Style Notes for Turkish Translations

1. Use lowercase for general terms, capitalize proper nouns and first words of sentences
2. Keep statistical abbreviations in English parentheses: "Guven Araligi (GA)"
3. Keep method names like LASSO, Cox, Kaplan-Meier unchanged
4. Use formal register (clinical audience: patologlar, onkologlar, klinisyenler)
5. Avoid unnecessary anglicisms when good Turkish equivalents exist
6. Keep `{placeholder}` tokens untranslated -- they are filled at runtime

---

## 6) QA Checklist

- [x] NAMESPACE contains `importFrom(jmvcore, .)` -- line 963
- [x] All 4 target files exist (`.a.yaml`, `.u.yaml`, `.r.yaml`, `.b.R`)
- [x] All user-visible strings in `R/lassocox.b.R` wrapped with `.()` -- **DONE** (168 `.()` calls applied)
- [ ] Run `jmvtools::i18nUpdate("en")` after wrapping to regenerate `en.po`
- [ ] Run `jmvtools::i18nUpdate("tr")` after regeneration to sync `tr.po`
- [ ] Validate `.po` files for untranslated entries -- currently all lassocox entries are untranslated
- [ ] Review Turkish translations for clinical accuracy and consistency with glossary
- [ ] Verify HTML explanation blocks render correctly after translation
- [ ] Test that `.()` calls in helper methods (`.interpretCindex`, `.interpretHazardRatio`) have proper `self` scope

---

## 7) Weblate Integration (GitHub)

1. Create a dedicated repo: `ClinicoPath-i18n`
   - Add `catalog.pot`, `README.md`, license
2. **Collaborators** -> add Weblate bot
3. **Webhooks** -> add:
   Payload URL: `https://hosted.weblate.org/hooks/github/`
4. Ask jamovi dev team to add `ClinicoPath-i18n` project to Weblate

---

## 8) Ready-to-Run Snippets

### Create/Update catalogs

```r
jmvtools::i18nCreate("en"); jmvtools::i18nUpdate("en")
jmvtools::i18nCreate("tr"); jmvtools::i18nUpdate("tr")
```

### Prepare POT

```bash
cp jamovi/i18n/en.po jamovi/i18n/catalog.pot
# Edit header: Language: c\n
```

### Quick grep to find unwrapped strings in R

```bash
# Lines with quoted user-visible strings not already wrapped with .()
grep -nE '(stop|warning|return)\("' R/lassocox.b.R | grep -v '\\.\('
grep -nE 'statistic = "|metric = "|variable = "|model_type = "' R/lassocox.b.R | grep -v '\\.\('
grep -nE 'title = "|subtitle = "|xlab = "|ylab = "|main = "' R/lassocox.b.R | grep -v '\\.\('
grep -nE 'setNote\("[^"]+",\s*"' R/lassocox.b.R | grep -v '\\.\('
```

---

## 9) Summary of Deliverables

### Files Found
- `jamovi/lassocox.a.yaml` -- 19 options with user-visible strings (auto-extracted by jmvtools)
- `jamovi/lassocox.u.yaml` -- 14 UI labels (auto-extracted)
- `jamovi/lassocox.r.yaml` -- 17 result items with titles/column headers (auto-extracted)
- `R/lassocox.b.R` -- **93 user-visible strings requiring `.()` wrapping** (manual)

### Translation Statistics
- Total lassocox-specific msgids identified in `tr.po`: ~55 (from YAML extraction)
- Currently translated: **0**
- Backend strings to be added after `.()` wrapping: ~93
- Estimated total translatable strings: ~148

### Priority Actions
1. **Wrap all 93 backend strings** with `.()` in `R/lassocox.b.R`
2. **Run `jmvtools::i18nUpdate("en")` and `jmvtools::i18nUpdate("tr")`** to regenerate catalogs
3. **Fill Turkish translations** using the tables in Section 4
4. **Verify** with `jmvtools::prepare()` that wrapping does not break functionality

### Risk Notes
- The `.makeBinaryRiskGroups()` method creates translated labels ("Low Risk"/"High Risk") that are used as factor levels in survival analysis. These translated labels must be consistent between the creation site (line 482/488) and usage sites (survival plot legend, line 1023/1076). If translations differ, the legend will not match.
- HTML explanation blocks are long. Consider whether full-block translation or individual-phrase translation is more maintainable. Recommendation: full-block translation for consistency.
- The suitability assessment uses `sprintf()` for dynamic values. After wrapping with `.()`, ensure `sprintf` format specifiers (%d, %.1f, etc.) are preserved in translations.
