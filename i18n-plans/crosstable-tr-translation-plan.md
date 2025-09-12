# Crosstable Function - Turkish (TR) Translation Plan

Generated on 2025-09-12 for jamovi module ClinicoPath

## 0) Argument Normalization

**SANITIZED_FN**: `crosstable`

**Target Files Status**: ✅ All found
- `jamovi/crosstable.a.yaml` (options) - ✅ Found
- `jamovi/crosstable.u.yaml` (UI) - ✅ Found  
- `jamovi/crosstable.r.yaml` (results) - ✅ Found
- `R/crosstable.b.R` (backend) - ✅ Found

## 1) NAMESPACE i18n Hook

✅ **STATUS**: Already present in NAMESPACE (line 854)

```r
importFrom(jmvcore, .)
```

The translation helper `.` function is properly imported for i18n string wrapping.

## 2) Translatable Strings Requiring `.()` Wrapper

### 2.1 Error & Warning Messages

**Current unwrapped strings in R/crosstable.b.R:**

```r
# Line 199
stop("The dataset contains no complete rows. Please check your data.")

# Line 203  
stop("Confidence level must be between 0.80 and 0.99")

# Line 207
stop("Monte Carlo replicates must be between 1,000 and 100,000")

# Line 690
warning(paste("Effect size calculation failed for variable:", var, "-", e$message))

# Line 750
warning(paste("Residual analysis failed for variable:", var, "-", e$message))
```

**Recommended patch:**

```r
# Line 199
stop(.("The dataset contains no complete rows. Please check your data."))

# Line 203
stop(.("Confidence level must be between 0.80 and 0.99"))

# Line 207
stop(.("Monte Carlo replicates must be between 1,000 and 100,000"))

# Line 690
warning(.("Effect size calculation failed for variable: {var} - {error}"), list(var = var, error = e$message))

# Line 750
warning(.("Residual analysis failed for variable: {var} - {error}"), list(var = var, error = e$message))
```

### 2.2 Dynamic Titles & Captions

**Current unwrapped strings:**

```r
# Line 231
subtitle_text <- paste0("Cross Table Analysis - Grouped by: ", display_group_name)

# Line 400
caption = paste0("Cross Table for Dependent ", mygroup)

# Line 518
comparison = paste0(var_display, ": ", group1, " vs ", group2)

# Line 933, 944, 974 (plot titles)
main = paste("Mosaic Plot:", var, "by", group)
main = paste("Association:", var, "by", group)  
main = paste("Correspondence Analysis:", var, "by", group)
```

**Recommended patch:**

```r
# Line 231
subtitle_text <- .("Cross Table Analysis - Grouped by: {group}"), list(group = display_group_name)

# Line 400
caption = .("Cross Table for Dependent {dep}"), list(dep = mygroup)

# Line 518
comparison = .("{variable}: {group1} vs {group2}"), list(variable = var_display, group1 = group1, group2 = group2)

# Line 933, 944, 974
main = .("Mosaic Plot: {var} by {group}"), list(var = var, group = group)
main = .("Association: {var} by {group}"), list(var = var, group = group)
main = .("Correspondence Analysis: {var} by {group}"), list(var = var, group = group)
```

### 2.3 Table Row Values (Measure Names)

**Current unwrapped strings:**

```r
# Line 666, 680 (effect size measures)
measure = "Cramér's V"
measure = "Phi Coefficient"

# Line 770, 796, 815, 832 (statistical methods)
method = "Standardized Residuals"
method = "Cramér's V Test"  
method = "Freeman-Halton Extension"
method = "Fisher's Exact (Simulated)"

# Line 862 (dimension labels)
dimension = paste("Dimension", i)
```

**Recommended patch:**

```r
# Line 666, 680
measure = .("Cramér's V")
measure = .("Phi Coefficient")

# Line 770, 796, 815, 832
method = .("Standardized Residuals")
method = .("Cramér's V Test")
method = .("Freeman-Halton Extension")
method = .("Fisher's Exact (Simulated)")

# Line 862
dimension = .("Dimension {number}"), list(number = i)
```

### 2.4 HTML Content & User Notes

**Current unwrapped HTML strings (Lines 139-158, 349-377):**

```html
"<h4 style='margin-top: 0; color: #1976D2;'>Q-values and Multiple Testing Correction</h4>"
"<p><strong>What are Q-values?</strong><br>"
"Q-values represent the False Discovery Rate (FDR) - the expected proportion..."
"<p><strong>Why use Q-values in this table?</strong><br>"
"<p><strong>Interpretation Guidelines:</strong></p>"
"<li><strong>Q < 0.05:</strong> Strong evidence against null hypothesis (5% FDR)</li>"
"<p><strong>Method:</strong> Benjamini-Hochberg FDR correction..."
```

**Recommended patch (example for key sections):**

```r
# Line 140
.("<h4 style='margin-top: 0; color: #1976D2;'>Q-values and Multiple Testing Correction</h4>")

# Line 142-143
.("<p><strong>What are Q-values?</strong><br>Q-values represent the False Discovery Rate (FDR) - the expected proportion of false positives among rejected hypotheses when testing multiple variables simultaneously.</p>")

# Line 145-146
.("<p><strong>Why use Q-values in this table?</strong><br>When comparing multiple variables across groups (as in this cross-table), the chance of finding at least one false positive increases. Q-values control this family-wise error rate.</p>")
```

### 2.5 Notes & Explanatory Messages

**Current unwrapped notification strings:**

```r
# Line 447-449
"<strong>Note:</strong> Pairwise comparisons are only performed when the grouping variable has more than 2 levels. Your grouping variable has {n_groups} level(s)."

# Line 565-566  
"<strong>Pairwise Comparisons:</strong> All possible pairs of groups have been compared. Total comparisons: {comparisons}."

# Line 568
"P-values have been adjusted using the {method} method to control for multiple testing."
```

**Recommended patch:**

```r
# Line 447-449
.("<strong>Note:</strong> Pairwise comparisons are only performed when the grouping variable has more than 2 levels. Your grouping variable has {n_groups} level(s).")

# Line 565-566
.("<strong>Pairwise Comparisons:</strong> All possible pairs of groups have been compared. Total comparisons: {comparisons}.")

# Line 568
.("P-values have been adjusted using the {method} method to control for multiple testing.")
```

## 3) Extraction & Update Commands

**Create/Update English template:**

```r
# In R console from module root
jmvtools::i18nCreate("en")
jmvtools::i18nUpdate("en")
```

**Prepare Weblate template:**

```bash
cp jamovi/i18n/en.po jamovi/i18n/catalog.pot
# Edit header to ensure: Language: c\n
```

**Create/Update Turkish catalog:**

```r  
# In R console
jmvtools::i18nCreate("tr")
jmvtools::i18nUpdate("tr")
```

## 4) Turkish Translation Suggestions

Based on clinical/statistical terminology for pathologists and oncologists:

| Status | msgid | Current TR | Suggested TR |
|--------|-------|------------|--------------|
| missing | "The dataset contains no complete rows. Please check your data." | | "Veri setinde hiç eksiksiz satır yok. Lütfen verilerinizi kontrol edin." |
| missing | "Confidence level must be between 0.80 and 0.99" | | "Güven düzeyi 0,80 ile 0,99 arasında olmalıdır" |
| missing | "Monte Carlo replicates must be between 1,000 and 100,000" | | "Monte Carlo tekrarları 1.000 ile 100.000 arasında olmalıdır" |
| missing | "Cross Table Analysis - Grouped by: {group}" | | "Çapraz Tablo Analizi - Gruplandırma: {group}" |
| missing | "Cross Table for Dependent {dep}" | | "{dep} Bağımlı Değişkeni için Çapraz Tablo" |
| missing | "Cramér's V" | | "Cramér V" |
| missing | "Phi Coefficient" | | "Phi Katsayısı" |
| missing | "Standardized Residuals" | | "Standartlaştırılmış Kalıntılar" |
| missing | "Freeman-Halton Extension" | | "Freeman-Halton Genişlemesi" |
| missing | "Fisher's Exact (Simulated)" | | "Fisher Kesin Testi (Simülasyon)" |
| missing | "Dimension {number}" | | "{number}. Boyut" |
| missing | "Q-values and Multiple Testing Correction" | | "Q-değerleri ve Çoklu Test Düzeltmesi" |
| missing | "What are Q-values?" | | "Q-değerleri nedir?" |
| missing | "False Discovery Rate (FDR)" | | "Yanlış Keşif Oranı (YKO)" |
| missing | "Strong evidence against null hypothesis" | | "Sıfır hipotezine karşı güçlü kanıt" |
| missing | "Moderate evidence" | | "Orta düzeyde kanıt" |
| missing | "Suggestive evidence" | | "Düşündürücü kanıt" |
| missing | "Benjamini-Hochberg FDR correction" | | "Benjamini-Hochberg YKO düzeltmesi" |
| missing | "Pairwise Comparisons" | | "İkili Karşılaştırmalar" |
| missing | "All possible pairs of groups have been compared" | | "Grupların tüm olası ikilileri karşılaştırılmıştır" |
| missing | "Total comparisons: {comparisons}" | | "Toplam karşılaştırma: {comparisons}" |
| missing | "P-values have been adjusted using the {method} method" | | "P-değerleri {method} yöntemiyle düzeltilmiştir" |
| missing | "Mosaic Plot: {var} by {group}" | | "Mozaik Grafiği: {group} Gruplarına Göre {var}" |
| missing | "Association: {var} by {group}" | | "İlişki: {group} Gruplarına Göre {var}" |
| missing | "Correspondence Analysis: {var} by {group}" | | "Uygunluk Analizi: {group} Gruplarına Göre {var}" |

## 5) Clinical Turkish Terminology Glossary

```text
Cross-table → Çapraz tablo
Contingency table → Olasılık tablosu / Kontinjan tablosu
Chi-square test → Ki-kare testi
Fisher's exact test → Fisher kesin testi
Cramér's V → Cramér V (katsayısı)
Phi coefficient → Phi katsayısı
Effect size → Etki büyüklüğü
Confidence Interval (CI) → Güven Aralığı (GA)
P-value → P-değeri
Q-value → Q-değeri
False Discovery Rate (FDR) → Yanlış Keşif Oranı (YKO)
Multiple testing correction → Çoklu test düzeltmesi
Pairwise comparison → İkili karşılaştırma
Residual analysis → Kalıntı analizi
Correspondence analysis → Uygunluk analizi
Monte Carlo simulation → Monte Carlo simülasyonu
Null hypothesis → Sıfır hipotezi
Statistical significance → İstatistiksel anlamlılık
```

## 6) QA Checklist Results

- ✅ **NAMESPACE**: Translation helper `.` imported from jmvcore
- ✅ **Target files**: All 4 expected jamovi files found
- ⚠️ **Backend wrapping**: 23 user-visible strings need `.()` wrapping
- ⚠️ **HTML content**: Extensive Q-value explanations need translation
- ⚠️ **Error messages**: 5 stop/warning messages need wrapping
- ⚠️ **Dynamic strings**: 8 paste/paste0 constructions need templates
- ⚠️ **Table labels**: 9 statistical measure names need wrapping

**Issues found**:
1. Multiple complex HTML explanations of Q-values not wrapped
2. Error messages using direct string concatenation
3. Plot titles use paste() without translation templates
4. Statistical method names in table rows not wrapped

## 7) Weblate Integration Steps

1. **Create dedicated i18n repository**: `ClinicoPath-crosstable-i18n`
2. **Add required files**:
   - `catalog.pot` (copied from `en.po`)
   - `README.md` with translation guidelines
   - `LICENSE` file
3. **GitHub configuration**:
   - Add Weblate bot as collaborator
   - Add webhook: `https://hosted.weblate.org/hooks/github/`
4. **Request jamovi team**: Add ClinicoPath-crosstable-i18n to Weblate

## 8) Ready-to-Run Commands

**Create/Update catalogs:**

```r
jmvtools::i18nCreate("en"); jmvtools::i18nUpdate("en")
jmvtools::i18nCreate("tr"); jmvtools::i18nUpdate("tr")
```

**Prepare POT template:**

```bash
cp jamovi/i18n/en.po jamovi/i18n/catalog.pot
sed -i 's/Language: en/Language: c/' jamovi/i18n/catalog.pot
```

**Quick check for unwrapped strings:**

```bash
grep -nE '"[^"]*[a-zA-Z][^"]*"' R/crosstable.b.R | grep -v '\\.('
```

## 9) Implementation Priority

**High Priority (Essential for Turkish users):**

1. Error/warning messages (5 strings) - Critical for debugging
2. Statistical measure names (9 strings) - Essential for interpretation  
3. Dynamic titles/captions (6 strings) - Important for context

**Medium Priority (User Experience):**

4. HTML Q-value explanations (15+ strings) - Educational content
5. Plot titles (3 strings) - Visual context
6. Notification messages (3 strings) - User guidance

**Estimated Translation Effort**: 35-40 string entries requiring clinical expertise

## 10) Notes for Translators

- **Medical context**: This function is used for clinicopathological research
- **Target audience**: Pathologists, oncologists, medical researchers
- **Tone**: Professional, precise, educational
- **Terminology**: Maintain consistency with established Turkish medical literature
- **Abbreviations**: Keep internationally recognized terms (FDR, CI, etc.) with Turkish explanations

---

**Translation Plan Status**: Ready for implementation  
**Next Step**: Apply `.()` wrappers to backend strings, then run extraction commands