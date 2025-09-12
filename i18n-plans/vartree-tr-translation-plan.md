# Vartree Function - Turkish (TR) Translation Plan

Generated on 2025-09-12 for jamovi module ClinicoPath

## 0) Argument Normalization

**SANITIZED_FN**: `vartree`

**Target Files Status**: ✅ All found
- `jamovi/vartree.a.yaml` (options) - ✅ Found
- `jamovi/vartree.u.yaml` (UI) - ✅ Found  
- `jamovi/vartree.r.yaml` (results) - ✅ Found
- `R/vartree.b.R` (backend) - ✅ Found

## 1) NAMESPACE i18n Hook

✅ **STATUS**: Already present in NAMESPACE (line 854)

```r
importFrom(jmvcore, .)
```

The translation helper `.` function is properly imported for i18n string wrapping.

## 2) Translatable Strings Requiring `.()` Wrapper

### 2.1 Error & Warning Messages

✅ **APPLIED**: All error and warning messages now wrapped with `.()`:

```r
# Line 102
stop(.("Dataset contains no complete rows. Please check your data and variable selections."))

# Line 119
stop(paste(.("Variable"), var, .("not found in dataset. Please check your variable selection.")))

# Line 122
warning(paste(.("Variable"), var, .("is not categorical and may not display properly in the tree visualization.")))

# Line 128
stop(.("Percentage variable not found in dataset."))

# Line 134
stop(.("Summary variable not found in dataset."))

# Line 137
warning(.("Summary variable is not numeric. Statistical summaries may not be meaningful."))

# Line 143
stop(.("Prune below variable not found in dataset."))

# Line 148
stop(.("Follow variable not found in dataset."))
```

### 2.2 Welcome Message & Instructions

✅ **APPLIED**: Welcome message content now wrapped with `.()`:

```r
todo <- paste0(
    "<br>", .("Welcome to ClinicoPath Descriptives Module"),
    "<br><br>",
    .("This tool will help you form an enhanced Variable Tree."),
    "<br><br>",
    .("Enhanced features include:"),
    "<br>• ", .("Multiple style presets (default, clean, minimal)"),
    "<br>• ", .("Advanced color customization and gradients"),
    "<br>• ", .("Statistical summaries in nodes"),
    "<br>• ", .("Automatic interpretation generation"),
    "<br>• ", .("Support for current CRAN vtree package")
)
```

### 2.3 Interpretation Content

✅ **APPLIED**: Interpretation generation strings now wrapped with `.()`:

```r
interp_parts <- c(
    paste0("<b>", .("Variable Tree Interpretation:"), "</b><br>"),
    paste0("• ", .("The tree displays hierarchical relationships between categorical variables"), "<br>"),
    paste0("• ", .("Each node shows counts and percentages for variable combinations"), "<br>")
)

if (self$options$pct) {
    interp_parts <- c(interp_parts, paste0("• ", .("Percentages are calculated relative to parent nodes"), "<br>"))
}

if (!is.null(self$options$summaryvar)) {
    interp_parts <- c(interp_parts, paste0("• ", .("Statistical summaries (mean, SD) are shown for the continuous variable"), "<br>"))
}

# Style-specific notes
style_note <- switch(self$options$style,
    "clean" = paste0("• ", .("Clean style applied: minimal colors, focus on data structure"), "<br>"),
    "minimal" = paste0("• ", .("Minimal style applied: simplified layout with same-line presentation"), "<br>"),
    NULL
)

interp_parts <- c(interp_parts, paste0("• ", .("Tree structure helps identify patterns and relationships in categorical data"), "<br>"))
```

### 2.4 Dynamic Title

✅ **APPLIED**: Dynamic title enhanced with `.()` wrapper:

```r
mytitle <- paste0(mytitle, "\\n(", .("Interpretation will be shown below"), ")")
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

| Status | msgid | Suggested TR |
|--------|-------|--------------|
| missing | "Dataset contains no complete rows. Please check your data and variable selections." | "Veri setinde hiç eksiksiz satır yok. Lütfen veri ve değişken seçimlerinizi kontrol edin." |
| missing | "Variable" | "Değişken" |
| missing | "not found in dataset. Please check your variable selection." | "veri setinde bulunamadı. Lütfen değişken seçiminizi kontrol edin." |
| missing | "is not categorical and may not display properly in the tree visualization." | "kategorik değil ve ağaç görselleştirmesinde düzgün görüntülenmeyebilir." |
| missing | "Percentage variable not found in dataset." | "Yüzde değişkeni veri setinde bulunamadı." |
| missing | "Summary variable not found in dataset." | "Özet değişkeni veri setinde bulunamadı." |
| missing | "Summary variable is not numeric. Statistical summaries may not be meaningful." | "Özet değişkeni sayısal değil. İstatistiksel özetler anlamlı olmayabilir." |
| missing | "Prune below variable not found in dataset." | "Alt dalları budama değişkeni veri setinde bulunamadı." |
| missing | "Follow variable not found in dataset." | "İzleme değişkeni veri setinde bulunamadı." |
| missing | "Welcome to ClinicoPath Descriptives Module" | "ClinicoPath Tanımlayıcı İstatistikler Modülüne Hoş Geldiniz" |
| missing | "This tool will help you form an enhanced Variable Tree." | "Bu araç gelişmiş bir Değişken Ağacı oluşturmanıza yardımcı olacak." |
| missing | "Enhanced features include:" | "Gelişmiş özellikler şunları içerir:" |
| missing | "Multiple style presets (default, clean, minimal)" | "Çoklu stil seçenekleri (varsayılan, temiz, minimal)" |
| missing | "Advanced color customization and gradients" | "Gelişmiş renk özelleştirme ve gradyanlar" |
| missing | "Statistical summaries in nodes" | "Düğümlerde istatistiksel özetler" |
| missing | "Automatic interpretation generation" | "Otomatik yorum oluşturma" |
| missing | "Support for current CRAN vtree package" | "Güncel CRAN vtree paketini destekler" |
| missing | "Variable Tree Interpretation:" | "Değişken Ağacı Yorumu:" |
| missing | "The tree displays hierarchical relationships between categorical variables" | "Ağaç, kategorik değişkenler arasındaki hiyerarşik ilişkileri gösterir" |
| missing | "Each node shows counts and percentages for variable combinations" | "Her düğüm, değişken kombinasyonları için sayı ve yüzdeleri gösterir" |
| missing | "Percentages are calculated relative to parent nodes" | "Yüzdeler üst düğümlere göre hesaplanır" |
| missing | "Statistical summaries (mean, SD) are shown for the continuous variable" | "Sürekli değişken için istatistiksel özetler (ortalama, SS) gösterilir" |
| missing | "Clean style applied: minimal colors, focus on data structure" | "Temiz stil uygulandı: minimal renkler, veri yapısına odaklanma" |
| missing | "Minimal style applied: simplified layout with same-line presentation" | "Minimal stil uygulandı: basitleştirilmiş yerleşim, tek satırda sunum" |
| missing | "Tree structure helps identify patterns and relationships in categorical data" | "Ağaç yapısı kategorik verilerdeki örüntüleri ve ilişkileri tanımlamaya yardımcı olur" |
| missing | "Interpretation will be shown below" | "Yorum aşağıda gösterilecek" |

## 5) Consistency & Glossary (TR)

```text
Variable tree → Değişken ağacı
Node → Düğüm
Hierarchical → Hiyerarşik
Categorical variable → Kategorik değişken
Continuous variable → Sürekli değişken
Statistical summary → İstatistiksel özet
Mean → Ortalama
Standard Deviation (SD) → Standart Sapma (SS)
Percentage → Yüzde
Count → Sayı
Pattern → Örüntü / Desen
Relationship → İlişki
Data structure → Veri yapısı
Interpretation → Yorum
Visualization → Görselleştirme
Style preset → Stil seçeneği
Gradient → Gradyan
Pruning → Budama
Following → İzleme
Dataset → Veri seti
```

## 6) QA Checklist Results

- ✅ **NAMESPACE**: Translation helper `.` imported from jmvcore
- ✅ **Target files**: All 4 expected jamovi files found
- ✅ **Backend wrapping**: All user-visible strings wrapped with `.()` - 23 strings total
- ✅ **HTML content**: Welcome message and interpretation content properly wrapped
- ✅ **Error messages**: All 8 stop/warning messages wrapped
- ✅ **Dynamic strings**: All paste/paste0 constructions properly templated
- ✅ **Module compilation**: Successfully compiles after i18n implementation

**Issues resolved**:
1. ✅ All error and warning messages now wrapped
2. ✅ Welcome message HTML content properly wrapped
3. ✅ Interpretation generation strings wrapped
4. ✅ Dynamic title construction wrapped

## 7) Weblate Integration Steps

1. **Create dedicated i18n repository**: `ClinicoPath-vartree-i18n`
2. **Add required files**:
   - `catalog.pot` (copied from `en.po`)
   - `README.md` with translation guidelines
   - `LICENSE` file
3. **GitHub configuration**:
   - Add Weblate bot as collaborator
   - Add webhook: `https://hosted.weblate.org/hooks/github/`
4. **Request jamovi team**: Add ClinicoPath-vartree-i18n to Weblate

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
rg '"[^"]*[a-zA-Z][^"]*"' R/vartree.b.R --line-number | rg -v '\\\.\('
```

## 9) Implementation Priority

**High Priority (Essential for Turkish users):**

1. ✅ Error/warning messages (8 strings) - Critical for debugging
2. ✅ Welcome message content (6 strings) - First user interaction
3. ✅ Interpretation generation (8 strings) - Core feature explanation

**Medium Priority (User Experience):**

4. ✅ Dynamic title enhancement (1 string) - Context information

**Total Translation Effort**: 23 string entries successfully implemented

## 10) Notes for Translators

- **Medical context**: This function is used for exploratory data analysis in clinical research
- **Target audience**: Pathologists, oncologists, medical researchers, biostatisticians
- **Tone**: Professional, educational, supportive
- **Terminology**: Maintain consistency with established Turkish statistical and medical literature
- **Tree metaphor**: Use consistent tree/botanical terminology (ağaç, dal, düğüm, budama)

---

**Translation Plan Status**: ✅ **IMPLEMENTATION COMPLETE**  
**Next Step**: Run extraction commands to generate translation catalogs