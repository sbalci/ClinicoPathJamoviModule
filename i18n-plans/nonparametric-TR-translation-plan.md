# Internationalization (i18n) Preparation & Translation Plan for `nonparametric`

## 0) Argument normalization (safety)

**SANITIZED_FN**: `nonparametric`

Target files checked:
- ✅ `jamovi/nonparametric.a.yaml` (exists)
- ✅ `jamovi/nonparametric.u.yaml` (exists) 
- ✅ `jamovi/nonparametric.r.yaml` (exists)
- ✅ `R/nonparametric.b.R` (exists)

All required files are present.

## 1) NAMESPACE i18n hook

✅ **Status**: Already present
The NAMESPACE file already contains the required import:
```r
importFrom(jmvcore,.)
```

## 2) Patch suggestions with .(...) wrapping

### 2.1 Error & warning messages

**File**: `R/nonparametric.b.R`

```diff
- self$results$descriptives$setNote("placeholder", 
-     "Please select dependent variable(s) and grouping variable to begin analysis.")
+ self$results$descriptives$setNote("placeholder", 
+     .("Please select dependent variable(s) and grouping variable to begin analysis."))

- self$results$tests$setNote("error",
-     paste("Insufficient data: At least", min_n, "complete observations required."))
+ self$results$tests$setNote("error",
+     .("Insufficient data: At least {min_n} complete observations required.", min_n = min_n))

- self$results$tests$setNote("warning",
-     "Some groups have fewer than 2 observations. Results may be unreliable.")
+ self$results$tests$setNote("warning",
+     .("Some groups have fewer than 2 observations. Results may be unreliable."))

- stop("Mann-Whitney U test requires exactly 2 groups")
+ stop(.("Mann-Whitney U test requires exactly 2 groups"))
```

### 2.2 Column titles & headers

```diff
- desc_table$getColumn('variable')$setTitle('Variable')
+ desc_table$getColumn('variable')$setTitle(.('Variable'))

- desc_table$getColumn('group')$setTitle('Group')  
+ desc_table$getColumn('group')$setTitle(.('Group'))

- test_table$getColumn('test')$setTitle('Test')
+ test_table$getColumn('test')$setTitle(.('Test'))

- effect_table$getColumn('measure')$setTitle('Effect Size Measure')
+ effect_table$getColumn('measure')$setTitle(.('Effect Size Measure'))

- posthoc_table$getColumn('comparison')$setTitle('Comparison')
+ posthoc_table$getColumn('comparison')$setTitle(.('Comparison'))
```

### 2.3 Test names & interpretations

```diff
- test_name <- "Shapiro-Wilk"
+ test_name <- .("Shapiro-Wilk")

- test_name <- "Anderson-Darling"
+ test_name <- .("Anderson-Darling")

- test_name <- "Kolmogorov-Smirnov"
+ test_name <- .("Kolmogorov-Smirnov")

- conclusion <- "Non-normal"
+ conclusion <- .("Non-normal")

- recommendation <- "Use non-parametric methods"
+ recommendation <- .("Use non-parametric methods")

- conclusion <- "Normal"
+ conclusion <- .("Normal")

- recommendation <- "Parametric methods acceptable"
+ recommendation <- .("Parametric methods acceptable")
```

### 2.4 Assumption testing labels

```diff
- assumption = "Independence",
+ assumption = .("Independence"),

- assessment = "Cannot be tested statistically",
+ assessment = .("Cannot be tested statistically"),

- recommendation = "Ensure random sampling and no clustering"
+ recommendation = .("Ensure random sampling and no clustering")

- assumption = "Homogeneity of Variance",
+ assumption = .("Homogeneity of Variance"),

- assessment <- "Equal variances assumption met"
+ assessment <- .("Equal variances assumption met")

- assessment <- "Unequal variances detected" 
+ assessment <- .("Unequal variances detected")

- recommendation <- "Consider robust methods or Welch's test"
+ recommendation <- .("Consider robust methods or Welch's test")

- assumption = "Sample Size Adequacy",
+ assumption = .("Sample Size Adequacy"),
```

### 2.5 Test method labels

```diff
- test_name = "Mann-Whitney U Test",
+ test_name = .("Mann-Whitney U Test"),

- test_name = "Kruskal-Wallis Test", 
+ test_name = .("Kruskal-Wallis Test"),

- test_name = "Wilcoxon Signed-Rank Test",
+ test_name = .("Wilcoxon Signed-Rank Test"),

- test_name = "Friedman Test",
+ test_name = .("Friedman Test"),

- measure = "Cliff's Delta",
+ measure = .("Cliff's Delta"),

- measure = "Eta-squared",
+ measure = .("Eta-squared"),

- measure = "Rank-biserial correlation",
+ measure = .("Rank-biserial correlation"),
```

## 3) Extraction & Update commands

```r
# Create or update English template (source language)
jmvtools::i18nCreate("en")
jmvtools::i18nUpdate("en")

# Create/Update Turkish catalog
jmvtools::i18nCreate("tr")
jmvtools::i18nUpdate("tr")
```

```bash
# Prepare Weblate template (POT)
cp jamovi/i18n/en.po jamovi/i18n/catalog.pot
# Ensure header contains exactly: Language: c\n
```

## 4) Turkish translation table for missing/weak entries

| Status | msgid | Suggested TR |
|--------|-------|--------------|
| missing | "Please select dependent variable(s) and grouping variable to begin analysis." | "Analize başlamak için bağımlı değişken(ler) ve gruplama değişkenini seçiniz." |
| missing | "Variable" | "Değişken" |
| missing | "Group" | "Grup" |
| missing | "Test" | "Test" |
| missing | "Effect Size Measure" | "Etki Büyüklüğü Ölçütü" |
| missing | "Comparison" | "Karşılaştırma" |
| missing | "Insufficient data: At least {min_n} complete observations required." | "Yetersiz veri: En az {min_n} eksiksiz gözlem gereklidir." |
| missing | "Some groups have fewer than 2 observations. Results may be unreliable." | "Bazı gruplarda 2'den az gözlem bulunmaktadır. Sonuçlar güvenilir olmayabilir." |
| missing | "Mann-Whitney U test requires exactly 2 groups" | "Mann-Whitney U testi tam olarak 2 grup gerektirir" |
| missing | "Independence" | "Bağımsızlık" |
| missing | "Cannot be tested statistically" | "İstatistiksel olarak test edilemez" |
| missing | "Ensure random sampling and no clustering" | "Rastgele örnekleme ve kümeleme olmadığından emin olunuz" |
| missing | "Homogeneity of Variance" | "Varyans Homojenliği" |
| missing | "Equal variances assumption met" | "Eşit varyans varsayımı karşılanmıştır" |
| missing | "Unequal variances detected" | "Eşit olmayan varyanslar tespit edildi" |
| missing | "Consider robust methods or Welch's test" | "Robust yöntemler veya Welch testi düşünülmelidir" |
| missing | "Sample Size Adequacy" | "Örnek Büyüklüğü Yeterliliği" |
| missing | "Adequate sample sizes" | "Yeterli örnek büyüklükleri" |
| missing | "Mann-Whitney U Test" | "Mann-Whitney U Testi" |
| missing | "Kruskal-Wallis Test" | "Kruskal-Wallis Testi" |
| missing | "Wilcoxon Signed-Rank Test" | "Wilcoxon İşaretli Sıra Testi" |
| missing | "Friedman Test" | "Friedman Testi" |
| missing | "Cliff's Delta" | "Cliff'in Delta'sı" |
| missing | "Eta-squared" | "Eta-kare" |
| missing | "Rank-biserial correlation" | "Sıra-biserial korelasyon" |
| missing | "Shapiro-Wilk" | "Shapiro-Wilk" |
| missing | "Anderson-Darling" | "Anderson-Darling" |
| missing | "Kolmogorov-Smirnov" | "Kolmogorov-Smirnov" |
| missing | "Non-normal" | "Normal dağılıma uymuyor" |
| missing | "Use non-parametric methods" | "Parametrik olmayan yöntemler kullanınız" |
| missing | "Normal" | "Normal dağılıma uyuyor" |
| missing | "Parametric methods acceptable" | "Parametrik yöntemler kabul edilebilir" |

## 5) Consistency & glossary (TR)

```text
Non-parametric → Parametrik olmayan
Mann–Whitney U Test → Mann–Whitney U Testi  
Kruskal-Wallis Test → Kruskal-Wallis Testi
Wilcoxon Test → Wilcoxon Testi
Friedman Test → Friedman Testi
Effect Size → Etki Büyüklüğü
Confidence Interval (CI) → Güven Aralığı (GA)
p-value → p-değeri
Post Hoc → Post Hoc
Descriptive Statistics → Betimleyici İstatistikler
Mean Difference → Ortalama Fark
Standard Deviation → Standart Sapma
Median → Medyan
Interquartile Range (IQR) → Çeyrekler Arası Aralık (ÇAA)
Sample Size → Örnek Büyüklüğü
Homogeneity of Variance → Varyans Homojenliği
Independence → Bağımsızlık
Normality → Normallik
Outliers → Aykırı Değerler
Bootstrap → Bootstrap
Robust Methods → Robust Yöntemler
```

**Clinical Context Notes for Turkish:**
- Keep statistical test names in original form (Mann-Whitney, Kruskal-Wallis) as they are universally recognized
- Use formal academic Turkish appropriate for medical/statistical contexts
- Prefer "test" over "sınama" as it's more commonly used in Turkish statistical literature
- Use "parametrik olmayan" rather than "non-parametrik" for better Turkish flow

## 6) QA checklist

- ✅ Verify all user-visible strings in R backend files are wrapped with `` `.` ``
- ✅ Confirm the NAMESPACE imports the translation helper `.`
- ✅ Ensure all known YAML files exist
- ✅ Target Turkish translations for clinical accuracy and tone
- ⚠️ Need to apply patch suggestions to wrap strings
- ⚠️ Need to create/update .po files

## 7) Weblate integration (GitHub)

1. Create a dedicated repo: `nonparametric-i18n`
   - Add `catalog.pot`, `README.md`, license
2. **Collaborators** → add Weblate bot
3. **Webhooks** → add:\
   Payload URL: `https://hosted.weblate.org/hooks/github/`
4. Ask jamovi dev team to add your `nonparametric-i18n` project to Weblate

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
grep -nE '\"[^\"\n]+' R/nonparametric.b.R | grep -v '\\.\('
```

## 9) Deliverables summary

### Files Status
- ✅ All 4 required jamovi files exist
- ✅ NAMESPACE already contains required `importFrom(jmvcore, .)`
- ⚠️ Need to apply string wrapping patches to `R/nonparametric.b.R`

### Priority Actions
1. **High**: Apply `.()` wrapping to 30+ user-visible strings
2. **High**: Create initial Turkish translations for medical/statistical terms
3. **Medium**: Set up i18n workflow with catalog creation
4. **Low**: Configure Weblate integration for ongoing translation

### Turkish Translation Notes
- Use formal medical/statistical Turkish
- Preserve statistical test names (Mann-Whitney, Kruskal-Wallis) 
- Translate descriptive content into clear, clinical Turkish
- Maintain consistency with established Turkish statistical terminology

This comprehensive plan ensures the `nonparametric` function will be fully prepared for Turkish localization with appropriate clinical terminology and user-friendly translations.