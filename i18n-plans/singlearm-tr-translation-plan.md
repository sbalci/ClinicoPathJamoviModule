# Internationalization (i18n) Preparation & Translation Plan for singlearm

## 0) Argument normalization (safety)

**SANITIZED_FN**: `singlearm`

Target files status:
✅ `jamovi/singlearm.a.yaml` (options)
✅ `jamovi/singlearm.u.yaml` (UI) 
✅ `jamovi/singlearm.r.yaml` (results)
✅ `R/singlearm.b.R` (backend)

## 1) NAMESPACE i18n hook

✅ **NAMESPACE already contains the required import**:
```r
importFrom(jmvcore, .)
```

## 2) Wrap translatable strings (jamovi patterns)

### **CRITICAL PATCH REQUIRED**: R/singlearm.b.R needs `.()` wrapping

The following user-visible strings must be wrapped with `.()` for translation:

```diff
--- R/singlearm.b.R.orig
+++ R/singlearm.b.R
@@ -138,11 +138,11 @@
       .safeExecute = function(expr, context = "analysis", silent = FALSE) {
         context_messages <- list(
-            "data_processing" = "Data processing failed. Please check your input variables.",
-            "survival_calculation" = "Survival calculation failed. This may be due to insufficient data or data quality issues.",
-            "plot_generation" = "Plot generation failed. Try adjusting plot parameters or checking data quality.",
-            "baseline_hazard" = "Baseline hazard calculation failed. This may occur with very sparse data.",
-            "person_time" = "Person-time analysis failed. Please check time intervals and event data.",
+            "data_processing" = .("Data processing failed. Please check your input variables."),
+            "survival_calculation" = .("Survival calculation failed. This may be due to insufficient data or data quality issues."),
+            "plot_generation" = .("Plot generation failed. Try adjusting plot parameters or checking data quality."),
+            "baseline_hazard" = .("Baseline hazard calculation failed. This may occur with very sparse data."),
+            "person_time" = .("Person-time analysis failed. Please check time intervals and event data."),
         )
         
@@ -147,7 +147,7 @@
           stop(user_msg)
         } else if (!silent) {
-            warning(paste(user_msg, "Technical details:", e$message))
+            warning(paste(user_msg, .("Technical details:"), e$message))
         }
         return(NULL)
       },

@@ -215,13 +215,13 @@
         warnings <- character(0)
         
         if (dq$n_events < 10) {
-          warnings <- c(warnings, "Very few events observed - results may be unreliable")
+          warnings <- c(warnings, .("Very few events observed - results may be unreliable"))
         }
         if (dq$event_rate < 15) {
-          warnings <- c(warnings, "Low event rate - consider longer follow-up")
+          warnings <- c(warnings, .("Low event rate - consider longer follow-up"))
         }
         if (dq$max_time < 12) {
-          warnings <- c(warnings, "Short follow-up duration - median survival may not be reached")
+          warnings <- c(warnings, .("Short follow-up duration - median survival may not be reached"))
         }

@@ -1686,8 +1686,8 @@
           plot <- survminer::ggsurvplot(
             survival_fit,
-            title = "Survival of the Whole Group",
-            subtitle = "Based on Kaplan-Meier estimates",
+            title = .("Survival of the Whole Group"),
+            subtitle = .("Based on Kaplan-Meier estimates"),
             xlab = paste0(.("Time"), " (", self$options$timetypeoutput, ")"),
             ylab = .("Survival Probability"),

@@ -1755,7 +1755,7 @@
           plot <- survminer::ggsurvplot(
             survival_fit,
-            title = "Cumulative Events of the Whole Group",
+            title = .("Cumulative Events of the Whole Group"),
             fun = "event",
             xlab = paste0(.("Time"), " (", self$options$timetypeoutput, ")"),

@@ -1820,7 +1820,7 @@
           plot <- survminer::ggsurvplot(
             survival_fit,
-            title = "Cumulative Hazard of the Whole Group",
+            title = .("Cumulative Hazard of the Whole Group"),
             fun = "cumhaz",
             xlab = paste0(.("Time"), " (", self$options$timetypeoutput, ")"),

@@ -1956,7 +1956,7 @@
             plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = time, y = hazard)) +
               ggplot2::geom_step(color = "#A23B72", linewidth = 1.2) +
               ggplot2::labs(
-                title = "Baseline Hazard Function",
+                title = .("Baseline Hazard Function"),
                 x = paste0(.("Time"), " (", self$options$timetypeoutput, ")"),

@@ -2049,8 +2049,8 @@
               ggplot2::labs(
-                title = "Smoothed Hazard Function",
-                subtitle = paste0("LOESS smoothing with span = ", round(adaptive_span, 2)),
+                title = .("Smoothed Hazard Function"),
+                subtitle = paste0(.("LOESS smoothing with span = "), round(adaptive_span, 2)),
                 x = paste0(.("Time"), " (", self$options$timetypeoutput, ")"),

@@ -2097,39 +2097,39 @@
         # Add rows to table
         quality_table$addRow(rowKey = 1, values = list(
-          metric = "Sample Size",
+          metric = .("Sample Size"),
           value = paste(dq$n_total, .("subjects")),
         ))
         
         quality_table$addRow(rowKey = 2, values = list(
-          metric = "Number of Events",
+          metric = .("Number of Events"),
           value = paste(dq$n_events, .("events")),
         ))
         
         quality_table$addRow(rowKey = 3, values = list(
-          metric = "Event Rate",
+          metric = .("Event Rate"),
           value = paste0(dq$event_rate, "%"),
         ))
         
         quality_table$addRow(rowKey = 4, values = list(
-          metric = "Follow-up Duration",
+          metric = .("Follow-up Duration"),
           value = paste0(dq$min_time, "-", dq$max_time, " ", self$options$timetypeoutput),
         ))
         
         quality_table$addRow(rowKey = 5, values = list(
-          metric = "Median Follow-up",
+          metric = .("Median Follow-up"),
           value = paste(dq$median_followup, self$options$timetypeoutput),
         ))
         
         quality_table$addRow(rowKey = 6, values = list(
-          metric = "Dataset Memory Usage",
+          metric = .("Dataset Memory Usage"),
           value = dataset_size,
         ))
         
         quality_table$addRow(rowKey = 7, values = list(
-            metric = "Time Variable Completeness",
+            metric = .("Time Variable Completeness"),
             value = paste0(round(time_complete, 1), "%"),
         ))
          
         quality_table$addRow(rowKey = 8, values = list(
-            metric = "Outcome Variable Completeness", 
+            metric = .("Outcome Variable Completeness"), 
             value = paste0(round(outcome_complete, 1), "%"),
         ))
```

## 3) Extraction & Update commands

**Create/Update catalogs**:
```r
jmvtools::i18nCreate("en"); jmvtools::i18nUpdate("en")
jmvtools::i18nCreate("tr"); jmvtools::i18nUpdate("tr")
```

**Prepare POT**:
```bash
cp jamovi/i18n/en.po jamovi/i18n/catalog.pot
# Edit header: Language: c\n
```

## 4) Turkish translations for singlearm

### **Missing/Untranslated entries requiring Turkish translation:**

| Status | msgid | Current msgstr | Suggested Turkish Translation |
|--------|--------|----------------|-------------------------------|
| missing | "Single Arm Survival" | "" | "Tek Grup Sağkalım Analizi" |
| missing | "1, 3, 5 year Survival" | "" | "1, 3, 5 Yıllık Sağkalım" |
| missing | "1, 3, 5 year Survival Table" | "" | "1, 3, 5 Yıllık Sağkalım Tablosu" |
| missing | "1, 3, 5-yr Survival Natural Language Summary" | "" | "1, 3, 5 Yıllık Sağkalım Doğal Dil Özeti" |
| missing | "Median Survival Analysis" | "" | "Medyan Sağkalım Analizi" |
| missing | "Person-Time Analysis" | "" | "Kişi-Zaman Analizi" |
| missing | "Baseline Hazard Analysis" | "" | "Temel Tehlike Analizi" |
| missing | "Data Quality Assessment" | "" | "Veri Kalitesi Değerlendirmesi" |
| missing | "Show Risk Table" | "" | "Risk Tablosu Göster" |
| missing | "Show Censored Observations" | "" | "Sansürlenmiş Gözlemleri Göster" |
| missing | "Survival of the Whole Group" | "" | "Tüm Grubun Sağkalımı" |
| missing | "Based on Kaplan-Meier estimates" | "" | "Kaplan-Meier tahminlerine dayalı" |
| missing | "Cumulative Events of the Whole Group" | "" | "Tüm Grubun Kümülatif Olayları" |
| missing | "Cumulative Hazard of the Whole Group" | "" | "Tüm Grubun Kümülatif Tehlikesi" |
| missing | "Baseline Hazard Function" | "" | "Temel Tehlike Fonksiyonu" |
| missing | "Smoothed Hazard Function" | "" | "Düzeltilmiş Tehlike Fonksiyonu" |
| missing | "LOESS smoothing with span = " | "" | "Açıklık = olan LOESS düzeltme " |
| missing | "Sample Size" | "" | "Örnek Boyutu" |
| missing | "Number of Events" | "" | "Olay Sayısı" |
| missing | "Event Rate" | "" | "Olay Oranı" |
| missing | "Follow-up Duration" | "" | "Takip Süresi" |
| missing | "Median Follow-up" | "" | "Medyan Takip" |
| missing | "Dataset Memory Usage" | "" | "Veri Seti Bellek Kullanımı" |
| missing | "Time Variable Completeness" | "" | "Zaman Değişkeni Tamlığı" |
| missing | "Outcome Variable Completeness" | "" | "Sonuç Değişkeni Tamlığı" |
| missing | "subjects" | "" | "kişi" |
| missing | "events" | "" | "olay" |

### **Error Messages needing translation:**

| msgid | Suggested Turkish Translation |
|-------|------------------------------|
| "Data processing failed. Please check your input variables." | "Veri işleme başarısız oldu. Giriş değişkenlerinizi kontrol edin." |
| "Survival calculation failed. This may be due to insufficient data or data quality issues." | "Sağkalım hesaplama başarısız oldu. Bu, yetersiz veri veya veri kalitesi sorunları nedeniyle olabilir." |
| "Plot generation failed. Try adjusting plot parameters or checking data quality." | "Grafik oluşturma başarısız oldu. Grafik parametrelerini ayarlamayı veya veri kalitesini kontrol etmeyi deneyin." |
| "Baseline hazard calculation failed. This may occur with very sparse data." | "Temel tehlike hesaplama başarısız oldu. Bu, çok seyrek verilerle oluşabilir." |
| "Person-time analysis failed. Please check time intervals and event data." | "Kişi-zaman analizi başarısız oldu. Zaman aralıklarını ve olay verilerini kontrol edin." |
| "Technical details:" | "Teknik ayrıntılar:" |
| "Very few events observed - results may be unreliable" | "Çok az olay gözlendi - sonuçlar güvenilir olmayabilir" |
| "Low event rate - consider longer follow-up" | "Düşük olay oranı - daha uzun takip düşünün" |
| "Short follow-up duration - median survival may not be reached" | "Kısa takip süresi - medyan sağkalıma ulaşılamayabilir" |

## 5) Consistency & glossary (TR)

**Clinical/Statistical Terms for Turkish translations:**

```text
Survival Analysis → Sağkalım Analizi
Single Arm → Tek Grup
Median Survival → Medyan Sağkalım  
Hazard Function → Tehlike Fonksiyonu
Baseline Hazard → Temel Tehlike
Cumulative Hazard → Kümülatif Tehlike
Person-Time → Kişi-Zaman
Follow-up → Takip
Event Rate → Olay Oranı
Confidence Interval (CI) → Güven Aralığı (GA)
Risk Table → Risk Tablosu
Censored → Sansürlenmiş
Kaplan-Meier → Kaplan-Meier
Sample Size → Örnek Boyutu
Data Quality → Veri Kalitesi
Completeness → Tamlık
Memory Usage → Bellek Kullanımı
```

## 6) QA checklist

- ✅ All target jamovi files exist
- ✅ NAMESPACE contains translation import
- ❌ **CRITICAL**: User-visible strings in R backend need `.()` wrapping (see patch above)
- ❌ **REQUIRED**: Turkish translations missing for all singlearm entries
- ✅ Existing i18n infrastructure in place

## 7) Weblate integration (GitHub)

The module already has i18n infrastructure. For Weblate integration:

1. Create dedicated repo: `clinicopath-singlearm-i18n`
2. Add `catalog.pot`, `README.md`, license
3. **Collaborators** → add Weblate bot
4. **Webhooks** → add: `https://hosted.weblate.org/hooks/github/`
5. Request jamovi dev team to add project to Weblate

## 8) Ready-to-run snippets (copy/paste)

**Apply the R backend patch first**, then:

```r
# Create/Update catalogs
jmvtools::i18nCreate("en"); jmvtools::i18nUpdate("en")
jmvtools::i18nCreate("tr"); jmvtools::i18nUpdate("tr")
```

```bash
# Prepare POT
cp jamovi/i18n/en.po jamovi/i18n/catalog.pot
# Edit header to set: Language: c\n
```

```bash
# Quick grep to find remaining unwrapped strings
grep -nE '"[A-Z][^"]*[^_]"' R/singlearm.b.R | grep -v '\.('
```

## 9) Implementation Priority

**IMMEDIATE ACTIONS REQUIRED:**

1. **Apply the R backend patch** to wrap user-visible strings with `.()` 
2. **Update translation catalogs** with the new wrapped strings
3. **Add Turkish translations** for all identified missing entries
4. **Test compilation** with `jmvtools::prepare()`

**Files to modify:**
- `R/singlearm.b.R` (apply patch for string wrapping)
- `jamovi/i18n/tr.po` (add Turkish translations)

This plan ensures the `singlearm` function will be properly internationalized and ready for Turkish clinical users.