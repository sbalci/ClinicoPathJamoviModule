# Internationalization (i18n) Preparation & Translation Plan for oddsratio

## 0) Argument normalization (safety)

**SANITIZED_FN**: `oddsratio`

Target files (expected):
- `jamovi/oddsratio.a.yaml` ✅ (options)
- `jamovi/oddsratio.u.yaml` ✅ (UI)
- `jamovi/oddsratio.r.yaml` ✅ (results)
- `R/oddsratio.b.R` ✅ (backend)

All required files are present.

## 1) NAMESPACE i18n hook

**Status**: ✅ Translation helper is imported at line 854:
```r
importFrom(jmvcore, .)
```

## 2) Wrap translatable strings (jamovi patterns)

### 2.1 Error & warning messages - Patch suggestions

```r
# Line 149: Error message
- "Outcome variable contains no non-missing values."
+ .("Outcome variable contains no non-missing values.")

# Line 165: Error message  
- "Outcome variable must have at least 2 different values for logistic regression."
+ .("Outcome variable must have at least 2 different values for logistic regression.")

# Line 179: Warning message using paste
- paste("Outcome variable has very few observations in one category (", min_count, " out of ", total_count, "). Results may be unreliable.", sep="")
+ paste(.("Outcome variable has very few observations in one category ({min_count} out of {total_count}). Results may be unreliable."), collapse="")

# Line 182: Warning message using paste
- paste("Outcome variable is severely imbalanced (", round(min_proportion * 100, 1), "% in minority class). Consider using specialized methods for imbalanced data.", sep="")
+ paste(.("Outcome variable is severely imbalanced ({proportion}% in minority class). Consider using specialized methods for imbalanced data."), collapse="")

# Line 188: Error message
- "Please select the positive outcome level from the dropdown menu below the outcome variable."
+ .("Please select the positive outcome level from the dropdown menu below the outcome variable.")

# Lines 210-215: Error messages using paste
- paste("Explanatory variable '", var_name, "' contains no non-missing values.", sep="")
+ .("Explanatory variable '{var_name}' contains no non-missing values.")

- paste("Explanatory variable '", var_name, "' has no variation (all values are the same). It will not contribute to the model.", sep="")
+ .("Explanatory variable '{var_name}' has no variation (all values are the same). It will not contribute to the model.")

# Line 702-710: Warning message in stop function
- "⚠️ Likelihood Ratio Calculation Error:"
+ .("⚠️ Likelihood Ratio Calculation Error:")
```

### 2.2 User interface text & HTML content

```r
# Line 328-331: User instruction text in glue
- "Please select which level of your outcome variable represents the 'positive' case"
+ .("Please select which level of your outcome variable represents the 'positive' case")

# Lines 372-381: HTML error messages
- "<b>❌ Data Error:</b> No data available for analysis<br><br>"
+ paste0("<b>", .("❌ Data Error:"), "</b> ", .("No data available for analysis"), "<br><br>")

- "<b>💡 Possible reasons:</b><br>"
+ paste0("<b>", .("💡 Possible reasons:"), "</b><br>")

- "• Dataset has no rows<br>"
+ paste0("• ", .("Dataset has no rows"), "<br>")

# Lines 602-608: Status messages
- "<b>Positive outcome level:</b>"
+ paste0("<b>", .("Positive outcome level:"), "</b>")

- "<b>Positive predictor level:</b>"
+ paste0("<b>", .("Positive predictor level:"), "</b>")

- "<b>📊 Contingency Table:</b><br>"
+ paste0("<b>", .("📊 Contingency Table:"), "</b><br>")

# Line 623: Usage instruction
- "2. If incorrect, use the 'Positive Outcome Level' dropdown to specify the correct level<br>"
+ paste0("2. ", .("If incorrect, use the 'Positive Outcome Level' dropdown to specify the correct level"), "<br>")
```

### 2.3 Analysis summaries and explanations

```r
# Lines 1027-1029: Analysis overview
- '<h4 style="color: #2c5282; margin-top: 0;">📊 Odds Ratio Analysis Summary</h4>'
+ paste0('<h4 style="color: #2c5282; margin-top: 0;">📊 ', .("Odds Ratio Analysis Summary"), '</h4>')

- '<p style="margin: 10px 0;"><strong>Analysis Overview:</strong> Logistic regression was performed to examine the relationship between '
+ paste0('<p style="margin: 10px 0;"><strong>', .("Analysis Overview:"), '</strong> ', .("Logistic regression was performed to examine the relationship between"), ' ')

# Lines 1035-1037: Key findings
- '<p style="margin: 10px 0;"><strong>Key Findings:</strong></p>'
+ paste0('<p style="margin: 10px 0;"><strong>', .("Key Findings:"), '</strong></p>')

# Lines 1074-1078: Interpretation guide
- '<strong>💡 Interpretation Guide:</strong>'
+ paste0('<strong>💡 ', .("Interpretation Guide:"), '</strong>')

- '<li>OR > 1: Factor increases the odds of the outcome</li>'
+ paste0('<li>', .("OR > 1: Factor increases the odds of the outcome"), '</li>')

- '<li>OR < 1: Factor decreases the odds of the outcome</li>'
+ paste0('<li>', .("OR < 1: Factor decreases the odds of the outcome"), '</li>')
```

### 2.4 Nomogram analysis text

```r
# Lines 1100-1101: Nomogram summary
- '<h4 style="color: #856404; margin-top: 0;">📈 Nomogram Analysis Summary</h4>'
+ paste0('<h4 style="color: #856404; margin-top: 0;">📈 ', .("Nomogram Analysis Summary"), '</h4>')

# Lines 1104-1110: Usage instructions
- '<p style="margin: 10px 0;"><strong>How to Use the Nomogram:</strong></p>'
+ paste0('<p style="margin: 10px 0;"><strong>', .("How to Use the Nomogram:"), '</strong></p>')

- '<li>Find your patient\'s value for each predictor variable on its scale</li>'
+ paste0('<li>', .("Find your patient's value for each predictor variable on its scale"), '</li>')

- '<li>Draw a vertical line up to the "Points" axis to get the points for that predictor</li>'
+ paste0('<li>', .("Draw a vertical line up to the \"Points\" axis to get the points for that predictor"), '</li>')
```

## 3) Extraction & Update commands

### 3.1 Create or update English template (source language)

```r
# In R console
jmvtools::i18nCreate("en")
jmvtools::i18nUpdate("en")
```

### 3.2 Prepare Weblate template (POT)

```bash
cp jamovi/i18n/en.po jamovi/i18n/catalog.pot
# Ensure header contains exactly: Language: c\n
```

### 3.3 Create/Update Turkish catalog

```r
# In R console  
jmvtools::i18nCreate("tr")
jmvtools::i18nUpdate("tr")
```

## 4) Turkish translation table for key medical/statistical terms

| Status | msgid | Current msgstr | Suggested TR (Clinical/Statistical) |
|--------|-------|----------------|-------------------------------------|
| missing | "Odds Ratio Analysis Summary" | | "Odds Oranı Analizi Özeti" |
| missing | "Outcome variable contains no non-missing values." | | "Sonuç değişkeni hiç eksik olmayan değer içermiyor." |
| missing | "Outcome variable must have at least 2 different values for logistic regression." | | "Lojistik regresyon için sonuç değişkeni en az 2 farklı değere sahip olmalıdır." |
| missing | "Please select the positive outcome level from the dropdown menu below the outcome variable." | | "Lütfen sonuç değişkeni altındaki açılır menüden pozitif sonuç düzeyini seçiniz." |
| missing | "Explanatory variable '{var_name}' contains no non-missing values." | | "Açıklayıcı değişken '{var_name}' hiç eksik olmayan değer içermiyor." |
| missing | "Explanatory variable '{var_name}' has no variation (all values are the same). It will not contribute to the model." | | "Açıklayıcı değişken '{var_name}' değişkenlik göstermiyor (tüm değerler aynı). Modele katkı sağlamayacaktır." |
| missing | "Likelihood Ratio Calculation Error:" | | "Olabilirlik Oranı Hesaplama Hatası:" |
| missing | "Analysis Overview:" | | "Analiz Genel Bakış:" |
| missing | "Key Findings:" | | "Temel Bulgular:" |
| missing | "Interpretation Guide:" | | "Yorumlama Rehberi:" |
| missing | "OR > 1: Factor increases the odds of the outcome" | | "OO > 1: Faktör sonuç olasılığını artırır" |
| missing | "OR < 1: Factor decreases the odds of the outcome" | | "OO < 1: Faktör sonuç olasılığını azaltır" |
| missing | "OR = 1: No association between factor and outcome" | | "OO = 1: Faktör ile sonuç arasında ilişki yok" |
| missing | "95% CI not crossing 1.0 indicates statistical significance" | | "%95 GA'nın 1.0'ı geçmemesi istatistiksel anlamlılığı gösterir" |
| missing | "Nomogram Analysis Summary" | | "Nomogram Analizi Özeti" |
| missing | "How to Use the Nomogram:" | | "Nomogramın Nasıl Kullanılacağı:" |
| missing | "Find your patient's value for each predictor variable on its scale" | | "Hastanızın her öngörücü değişken için değerini o değişkenin ölçeğinde bulun" |
| missing | "Draw a vertical line up to the \"Points\" axis to get the points for that predictor" | | "O öngörücü için puanları almak üzere \"Puan\" eksenine dikey çizgi çizin" |
| missing | "Sum all points from all predictors to get the \"Total Points\"" | | "\"Toplam Puan\" elde etmek için tüm öngörücülerden gelen puanları toplayın" |
| missing | "Draw a vertical line down from the Total Points to find the predicted probability" | | "Öngörülen olasılığı bulmak için Toplam Puan'dan aşağı doğru dikey çizgi çizin" |
| missing | "Positive outcome level:" | | "Pozitif sonuç düzeyi:" |
| missing | "Positive predictor level:" | | "Pozitif öngörücü düzeyi:" |
| missing | "Contingency Table:" | | "Kontenjans Tablosu:" |
| missing | "Data Error:" | | "Veri Hatası:" |
| missing | "No data available for analysis" | | "Analiz için veri bulunmuyor" |
| missing | "Possible reasons:" | | "Olası nedenler:" |
| missing | "Dataset has no rows" | | "Veri seti hiç satır içermiyor" |
| missing | "All rows contain missing values" | | "Tüm satırlar eksik değer içeriyor" |

## 5) Consistency & glossary (TR) - Medical/Clinical Focus

```text
Statistical Terms:
Odds Ratio (OR) → Odds Oranı (OO)
Confidence Interval (CI) → Güven Aralığı (GA)
p-value → p-değeri
Statistical significance → İstatistiksel anlamlılık
Logistic regression → Lojistik regresyon
Predictor variable → Öngörücü değişken
Outcome variable → Sonuç değişkeni
Explanatory variable → Açıklayıcı değişken

Clinical Terms:
Positive outcome → Pozitif sonuç
Negative outcome → Negatif sonuç
Patient → Hasta
Clinical context → Klinik bağlam
Diagnostic test → Tanı testi
Sensitivity → Duyarlılık
Specificity → Özgüllük
Likelihood ratio → Olabilirlik oranı

Data Terms:
Missing values → Eksik değerler
Sample size → Örneklem büyüklüğü
Data quality → Veri kalitesi
Variable → Değişken
Level → Düzey
Category → Kategori
```

## 6) QA checklist

- [ ] **String extraction**: All user-visible strings in R backend files are wrapped with `` `.` ``.
- [x] **NAMESPACE import**: The NAMESPACE imports the translation helper `.`.
- [x] **File completeness**: All known YAML files exist.
- [ ] **Translation validation**: `.po` files validated for untranslated entries.
- [ ] **Clinical accuracy**: Turkish translations reviewed for medical terminology correctness.
- [ ] **Consistency check**: Medical/statistical terms used consistently throughout.

## 7) Weblate integration (GitHub)

1. Create a dedicated repo: `ClinicoPathJamoviModule-i18n`
   - Add `catalog.pot`, `README.md`, MIT license.
2. **Collaborators** → add Weblate bot.
3. **Webhooks** → add: `https://hosted.weblate.org/hooks/github/`
4. Contact jamovi dev team to add the `ClinicoPathJamoviModule-i18n` project to Weblate.

## 8) Ready-to-run snippets (copy/paste)

**Create/Update catalogs**

```r
jmvtools::i18nCreate("en"); jmvtools::i18nUpdate("en")
jmvtools::i18nCreate("tr"); jmvtools::i18nUpdate("tr")
```

**Prepare POT**

```bash
cp jamovi/i18n/en.po jamovi/i18n/catalog.pot
# Edit header to: Language: c\n
```

**Quick grep to find unwrapped strings in R (heuristic)**

```bash
# Lines with quoted strings not already wrapped (rough)
grep -nE '\"[^\"\n]+' R/oddsratio.b.R | grep -v '\\.\('
```

## 9) Deliverables Summary

### Files Analysis
- **Found**: All 4 core jamovi files present (✅)
- **NAMESPACE**: Translation helper imported (✅)
- **Status**: Ready for i18n implementation

### Translation Requirements  
- **String count**: ~50+ translatable strings identified
- **Priority areas**: Error messages, analysis summaries, clinical interpretations
- **Medical terminology**: Extensive use of statistical and clinical terms requiring specialized translation

### Implementation Steps
1. **Phase 1**: Wrap all identified strings with `.(...)` function
2. **Phase 2**: Generate and validate .po catalogs  
3. **Phase 3**: Implement Turkish translations with medical accuracy
4. **Phase 4**: Set up Weblate integration for ongoing translation management

### Clinical Translation Notes
- Maintain professional medical terminology
- Ensure consistency with Turkish medical literature
- Focus on pathologist/oncologist audience
- Preserve technical accuracy while improving readability

### Next Actions
1. Apply the string wrapping patches to `R/oddsratio.b.R`
2. Run catalog generation commands
3. Implement Turkish translations from the provided table
4. Set up Weblate integration for collaborative translation management