# Internationalization (i18n) Preparation & Translation Plan

## 0) Argument normalization (safety)

**SANITIZED_FN**: `jjsegmentedtotalbar`

Target files (expected):
- `jamovi/jjsegmentedtotalbar.a.yaml` ✅ (options)
- `jamovi/jjsegmentedtotalbar.u.yaml` ✅ (UI)  
- `jamovi/jjsegmentedtotalbar.r.yaml` ✅ (results)
- `R/jjsegmentedtotalbar.b.R` ✅ (backend)

All required files are present.

---

## 1) NAMESPACE i18n hook

**Status**: ✅ The translation helper `.` is already imported at line 841:
```r
importFrom(jmvcore, .)
```

---

## 2) Wrap translatable strings (jamovi patterns)

### 2.1 Error & warning messages patch

```diff
# Line 86
- self$results$instructions$setContent("No data available for analysis.")
+ self$results$instructions$setContent(.("No data available for analysis."))

# Line 124  
- stop(paste("Missing variables in data:", paste(missing_vars, collapse = ", ")))
+ stop(paste(.("Missing variables in data:"), paste(missing_vars, collapse = ", ")))

# Line 129
- warning(paste("Found", sum(is.na(data[[x_var]])), "NA values in", x_var, "- these will be removed"))
+ warning(paste(.("Found"), sum(is.na(data[[x_var]])), .("NA values in"), x_var, .- .("these will be removed")))

# Line 134
- warning(paste("Found", sum(is.na(data[[fill_var]])), "NA values in", fill_var, "- these will be removed"))
+ warning(paste(.("Found"), sum(is.na(data[[fill_var]])), .("NA values in"), fill_var, .- .("these will be removed")))

# Line 140
- stop(paste(y_var, "must be a numeric variable"))
+ stop(paste(y_var, .("must be a numeric variable")))

# Line 149
- stop(paste(x_var, "must have at least one category"))
+ stop(paste(x_var, .("must have at least one category")))

# Line 153
- stop(paste(fill_var, "must have at least one segment"))
+ stop(paste(fill_var, .("must have at least one segment")))

# Line 158
- stop(paste("Facet variable", facet_var, "not found in data"))
+ stop(paste(.("Facet variable"), facet_var, .("not found in data")))

# Line 203
- stop("No valid data after processing. Please check your input variables.")
+ stop(.("No valid data after processing. Please check your input variables."))

# Line 213-215
- warning(paste("Categories with zero totals found:",
+ warning(paste(.("Categories with zero totals found:"),
                paste(zero_totals[[x_var]], collapse = ", "),
-               "- these may appear empty in the plot"))
+               .- .("these may appear empty in the plot")))

# Line 615
- warning("Plot creation returned NULL")
+ warning(.("Plot creation returned NULL"))

# Line 618
- warning("Plot creation failed: ", e$message)
+ warning(paste(.("Plot creation failed:"), e$message))

# Line 671
- stop("ggsegmentedtotalbar package not available")
+ stop(.("ggsegmentedtotalbar package not available"))
```

### 2.2 UI text & titles patch

```diff
# Line 42 - HTML Instructions (keep HTML structure, wrap text content)
- "<h3 style='color: #2e7d32; margin-top: 0;'>Segmented Total Bar Charts</h3>",
+ paste0("<h3 style='color: #2e7d32; margin-top: 0;'>", .("Segmented Total Bar Charts"), "</h3>"),

- "<p><strong>Create 100% stacked bar charts showing proportional composition:</strong></p>",
+ paste0("<p><strong>", .("Create 100% stacked bar charts showing proportional composition:"), "</strong></p>"),

- "<li><strong>Category Variable (X-axis):</strong> Main grouping variable</li>",
+ paste0("<li><strong>", .("Category Variable (X-axis):"), "</strong> ", .("Main grouping variable"), "</li>"),

- "<li><strong>Value Variable (Y-axis):</strong> Numeric values for segments</li>",
+ paste0("<li><strong>", .("Value Variable (Y-axis):"), "</strong> ", .("Numeric values for segments"), "</li>"),

- "<li><strong>Segment Variable (Fill):</strong> Variable defining bar segments</li>",
+ paste0("<li><strong>", .("Segment Variable (Fill):"), "</strong> ", .("Variable defining bar segments"), "</li>"),

- "<li><strong>Panel Variable:</strong> Optional faceting variable</li>",
+ paste0("<li><strong>", .("Panel Variable:"), "</strong> ", .("Optional faceting variable"), "</li>"),

- "<strong>Perfect for:</strong> Survey responses, patient demographics, market composition, treatment outcomes</p>",
+ paste0("<strong>", .("Perfect for:"), "</strong> ", .("Survey responses, patient demographics, market composition, treatment outcomes"), "</p>"),

# Line 422-425 - Plot titles
- plot_title <- if (self$options$plot_title != "") self$options$plot_title else "Segmented Total Bar Chart"
+ plot_title <- if (self$options$plot_title != "") self$options$plot_title else .("Segmented Total Bar Chart")

- y_title <- if (self$options$y_title != "") self$options$y_title else "Percentage"
+ y_title <- if (self$options$y_title != "") self$options$y_title else .("Percentage")

# Line 466 - Chart type
- chart_type = "Segmented Total Bar (100% Stacked)"
+ chart_type = .("Segmented Total Bar (100% Stacked)")

# Lines 573-579 - Statistics labels
- list(measure = "Total Categories", value = as.character(n_categories)),
+ list(measure = .("Total Categories"), value = as.character(n_categories)),

- list(measure = "Total Segments", value = as.character(n_segments)),
+ list(measure = .("Total Segments"), value = as.character(n_segments)),

- list(measure = "Total Observations", value = as.character(total_obs)),
+ list(measure = .("Total Observations"), value = as.character(total_obs)),

- list(measure = "Min Percentage", value = paste0(min_pct, "%")),
+ list(measure = .("Min Percentage"), value = paste0(min_pct, "%")),

- list(measure = "Max Percentage", value = paste0(max_pct, "%")),
+ list(measure = .("Max Percentage"), value = paste0(max_pct, "%")),

- list(measure = "Mean Percentage", value = paste0(mean_pct, "%")),
+ list(measure = .("Mean Percentage"), value = paste0(mean_pct, "%")),

- list(measure = "Most Variable Segment", value = as.character(segment_variation$segment[1]))
+ list(measure = .("Most Variable Segment"), value = as.character(segment_variation$segment[1]))

# Lines 522-536 - Chart interpretation HTML
- "<h4 style='color: #495057; margin-top: 0;'>Chart Interpretation</h4>",
+ paste0("<h4 style='color: #495057; margin-top: 0;'>", .("Chart Interpretation"), "</h4>"),

- "<p><strong>Data Overview:</strong></p>",
+ paste0("<p><strong>", .("Data Overview:"), "</strong></p>"),

- paste0("<li>", n_categories, " categories with ", n_segments, " segments each</li>"),
+ paste0("<li>", n_categories, " ", .("categories with"), " ", n_segments, " ", .("segments each"), "</li>"),

- paste0("<li>Total observations: ", sum(composition_data$count), "</li>"),
+ paste0("<li>", .("Total observations:"), " ", sum(composition_data$count), "</li>"),

- "<p><strong>Key Findings:</strong></p>",
+ paste0("<p><strong>", .("Key Findings:"), "</strong></p>"),

- paste0("<li><strong>Largest segment:</strong> ", largest_segment$segment[1],
+ paste0("<li><strong>", .("Largest segment:"), "</strong> ", largest_segment$segment[1],
         " in ", largest_segment$category[1],
-        " (", round(largest_segment$percentage[1], 1), "%)</li>"),
+        " (", round(largest_segment$percentage[1], 1), "%)</li>"),

- paste0("<li><strong>Most balanced category:</strong> ", balanced_category$category[1],
+ paste0("<li><strong>", .("Most balanced category:"), "</strong> ", balanced_category$category[1],
-        " shows the most even distribution across segments</li>"),
+        " ", .("shows the most even distribution across segments"), "</li>"),
```

---

## 3) Extraction & Update commands

### 3.1 Create or update English template (source language)

```r
# In R console
jmvtools::i18nCreate("en")
jmvtools::i18nUpdate("en")
```

This creates/updates: `jamovi/i18n/en.po`

### 3.2 Prepare Weblate template (POT)

```bash
cp jamovi/i18n/en.po jamovi/i18n/catalog.pot
# Edit header to ensure: Language: c\n
```

### 3.3 Create/Update Turkish catalog

```r
# In R console
jmvtools::i18nCreate("tr")
jmvtools::i18nUpdate("tr")
```

Outputs: `jamovi/i18n/tr.po`

---

## 4) Turkish translation table for missing/weak entries

| Status  | msgid                                                     | Suggested TR                                                      |
| ------- | -------------------------------------------------------- | ---------------------------------------------------------------- |
| missing | "No data available for analysis."                        | "Analiz için veri mevcut değil."                               |
| missing | "Missing variables in data:"                             | "Veride eksik değişkenler:"                                    |
| missing | "Found"                                                  | "Bulundu"                                                       |
| missing | "NA values in"                                           | "değişkeninde NA değeri"                                       |
| missing | "these will be removed"                                  | "bunlar kaldırılacak"                                          |
| missing | "must be a numeric variable"                             | "sayısal değişken olmalıdır"                                   |
| missing | "must have at least one category"                       | "en az bir kategori içermelidir"                               |
| missing | "must have at least one segment"                        | "en az bir segment içermelidir"                                |
| missing | "Facet variable"                                         | "Facet değişkeni"                                              |
| missing | "not found in data"                                      | "veride bulunamadı"                                            |
| missing | "No valid data after processing. Please check your input variables." | "İşlem sonrası geçerli veri yok. Girdi değişkenlerinizi kontrol edin." |
| missing | "Categories with zero totals found:"                     | "Sıfır toplamı olan kategoriler bulundu:"                      |
| missing | "these may appear empty in the plot"                    | "bunlar grafikte boş görünebilir"                              |
| missing | "Plot creation returned NULL"                            | "Grafik oluşturma NULL döndürdü"                               |
| missing | "Plot creation failed:"                                  | "Grafik oluşturma başarısız:"                                  |
| missing | "ggsegmentedtotalbar package not available"              | "ggsegmentedtotalbar paketi mevcut değil"                      |
| missing | "Segmented Total Bar Charts"                             | "Segmentli Toplam Çubuk Grafikler"                             |
| missing | "Create 100% stacked bar charts showing proportional composition:" | "Oransal bileşimi gösteren %100 yığılmış çubuk grafikleri oluşturun:" |
| missing | "Category Variable (X-axis):"                           | "Kategori Değişkeni (X-ekseni):"                               |
| missing | "Main grouping variable"                                 | "Ana gruplama değişkeni"                                        |
| missing | "Value Variable (Y-axis):"                              | "Değer Değişkeni (Y-ekseni):"                                  |
| missing | "Numeric values for segments"                            | "Segmentler için sayısal değerler"                             |
| missing | "Segment Variable (Fill):"                              | "Segment Değişkeni (Dolgu):"                                   |
| missing | "Variable defining bar segments"                         | "Çubuk segmentlerini tanımlayan değişken"                      |
| missing | "Panel Variable:"                                        | "Panel Değişkeni:"                                              |
| missing | "Optional faceting variable"                             | "İsteğe bağlı faceting değişkeni"                              |
| missing | "Perfect for:"                                           | "Şunlar için ideal:"                                           |
| missing | "Survey responses, patient demographics, market composition, treatment outcomes" | "Anket yanıtları, hasta demografileri, pazar bileşimi, tedavi sonuçları" |
| missing | "Segmented Total Bar Chart"                              | "Segmentli Toplam Çubuk Grafik"                                |
| missing | "Percentage"                                             | "Yüzde"                                                         |
| missing | "Segmented Total Bar (100% Stacked)"                    | "Segmentli Toplam Çubuk (%100 Yığılmış)"                       |
| missing | "Total Categories"                                       | "Toplam Kategori"                                               |
| missing | "Total Segments"                                         | "Toplam Segment"                                                |
| missing | "Total Observations"                                     | "Toplam Gözlem"                                                 |
| missing | "Min Percentage"                                         | "Minimum Yüzde"                                                 |
| missing | "Max Percentage"                                         | "Maksimum Yüzde"                                                |
| missing | "Mean Percentage"                                        | "Ortalama Yüzde"                                                |
| missing | "Most Variable Segment"                                  | "En Değişken Segment"                                           |
| missing | "Chart Interpretation"                                   | "Grafik Yorumlama"                                              |
| missing | "Data Overview:"                                         | "Veri Genel Bakış:"                                             |
| missing | "categories with"                                        | "kategorili"                                                    |
| missing | "segments each"                                          | "herbiri segment"                                               |
| missing | "Total observations:"                                    | "Toplam gözlem:"                                                |
| missing | "Key Findings:"                                          | "Ana Bulgular:"                                                 |
| missing | "Largest segment:"                                       | "En büyük segment:"                                             |
| missing | "Most balanced category:"                                | "En dengeli kategori:"                                          |
| missing | "shows the most even distribution across segments"       | "segmentler arasında en eşit dağılımı gösterir"                |

---

## 5) Consistency & glossary (TR)

```text
Segment → Segment
Chart → Grafik  
Category → Kategori
Variable → Değişken
Data → Veri
Analysis → Analiz
Plot/Chart → Grafik
Percentage → Yüzde
Observation → Gözlem
Total → Toplam
Distribution → Dağılım
Statistics → İstatistikler
Interpretation → Yorumlama
```

**Clinical Context Terms:**
```text
Patient demographics → Hasta demografileri
Treatment outcomes → Tedavi sonuçları
Survey responses → Anket yanıtları
Market composition → Pazar bileşimi
```

---

## 6) QA checklist

- ✅ Verify all user-visible strings in R backend files are wrapped with `` `.` ``
- ✅ Confirm the NAMESPACE imports the translation helper `.`
- ✅ Ensure all known YAML files exist (all present)
- ⚠️ Validate `.po` files for untranslated entries (need to run extraction commands)
- ⚠️ Review Turkish translations for clinical accuracy and tone (pending extraction)

---

## 7) Weblate integration (GitHub)

1. Create a dedicated repo: `ClinicoPathJamoviModule-i18n`
   - Add `catalog.pot`, `README.md`, license.
2. **Collaborators** → add Weblate bot.
3. **Webhooks** → add:\
   Payload URL: `https://hosted.weblate.org/hooks/github/`
4. Ask jamovi dev team to add your `ClinicoPathJamoviModule-i18n` project to Weblate.

---

## 8) Ready-to-run snippets (copy/paste)

**Create/Update catalogs**

```r
jmvtools::i18nCreate("en"); jmvtools::i18nUpdate("en")
jmvtools::i18nCreate("tr"); jmvtools::i18nUpdate("tr")
```

**Prepare POT**

```bash
cp jamovi/i18n/en.po jamovi/i18n/catalog.pot
# Edit header: Language: c\n
```

**Quick grep to find unwrapped strings in R (heuristic)**

```bash
# Lines with quoted strings not already wrapped (rough)
grep -nE '"[^"]+' R/jjsegmentedtotalbar.b.R | grep -v '\\.\('
```

---

## 9) Summary

### Files Status
- ✅ All required jamovi files present
- ✅ NAMESPACE already has translation import
- ⚠️ R backend needs string wrapping (42+ translatable strings identified)

### Next Steps
1. **Apply patches** to wrap translatable strings with `.()` in R/jjsegmentedtotalbar.b.R
2. **Run extraction commands** to generate .po files  
3. **Fill Turkish translations** using the provided table
4. **Set up Weblate integration** for collaborative translation
5. **Test translations** in jamovi interface

### Translation Priority
**High Priority (User-facing):**
- Error messages (data validation, processing failures)
- Chart titles and labels
- Statistics table headers
- HTML interpretation content

**Medium Priority:**
- Instruction text  
- Detailed descriptions

The function has rich user-facing content that would greatly benefit from Turkish localization, especially for clinical research contexts in Turkish-speaking medical institutions.