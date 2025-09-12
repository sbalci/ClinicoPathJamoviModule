# Internationalization (i18n) Preparation & Turkish Translation Plan: `venn`

## 0) Argument normalization (safety)

**SANITIZED_FN**: `venn`

Target files analyzed:
- ✅ `jamovi/venn.a.yaml` (options) - **EXISTS**
- ✅ `jamovi/venn.u.yaml` (UI) - **EXISTS**  
- ✅ `jamovi/venn.r.yaml` (results) - **EXISTS**
- ✅ `R/venn.b.R` (backend) - **EXISTS**

All required files are present and accounted for.

---

## 1) NAMESPACE i18n hook

✅ **VERIFIED**: The NAMESPACE file already includes the translation helper import:

```r
importFrom(jmvcore,.)
```

No changes required.

---

## 2) Wrap translatable strings (jamovi patterns)

### Current Status
❌ **ISSUE IDENTIFIED**: The `venn` function contains **NO** internationalized strings. All user-visible strings need to be wrapped with `.()`.

### 2.1 User-facing strings requiring translation

**Error messages:**
```r
stop("Data contains no (complete) rows")  # Line 106, 188, 227
```

**Welcome/instruction messages:**
```r
todo <- "
    <br><strong>Welcome to ClinicoPath Venn Diagram Tool</strong>
    <br><br>
    This tool helps you visualize overlaps between categorical variables
    using Venn and Upset diagrams.
    <br>
    <em>Please select at least Variable 1 and Variable 2 to proceed.</em>
    <hr><br>
"
```

**Validation error messages:**
```r
"<div class='alert alert-warning'>
    <strong>Variable 1 Selected but True Level Missing</strong><br>
    Please select which level in Variable 1 represents the 'true' condition.
</div>"
```

**Plot titles:**
```r
"Venn Diagram of Selected Variables"  # Line 204
"ComplexUpset Diagram of Selected Variables"  # Line 285
"UpSetR Diagram of Selected Variables"  # Line 317
"Intersection Size"  # Line 264
```

### 2.2 Required patches

**Patch 1: R/venn.b.R - Error messages**

```r
# Line 106
- stop("Data contains no (complete) rows")
+ stop(.("Data contains no (complete) rows"))

# Line 188  
- stop('Data contains no (complete) rows')
+ stop(.('Data contains no (complete) rows'))

# Line 227
- stop('Data contains no (complete) rows') 
+ stop(.('Data contains no (complete) rows'))
```

**Patch 2: R/venn.b.R - Welcome message**

```r
# Lines 83-91
todo <- paste0(
    "<br><strong>", .("Welcome to ClinicoPath Venn Diagram Tool"), "</strong>",
    "<br><br>",
    .("This tool helps you visualize overlaps between categorical variables using Venn and Upset diagrams."),
    "<br>",
    "<em>", .("Please select at least Variable 1 and Variable 2 to proceed."), "</em>",
    "<hr><br>"
)
```

**Patch 3: R/venn.b.R - Validation errors**

```r
# Lines 328-330
return(paste0("<div class='alert alert-warning'>
    <strong>", .("Variable 1 Selected but True Level Missing"), "</strong><br>",
    .("Please select which level in Variable 1 represents the 'true' condition."),
    "</div>"))

# Lines 335-337  
return(paste0("<div class='alert alert-warning'>
    <strong>", .("Variable 2 Selected but True Level Missing"), "</strong><br>",
    .("Please select which level in Variable 2 represents the 'true' condition."),
    "</div>"))

# Lines 342-344
return(paste0("<div class='alert alert-warning'>
    <strong>", .("Variable 3 Selected but True Level Missing"), "</strong><br>",
    .("Please select which level in Variable 3 represents the 'true' condition."),
    "</div>"))

# Lines 349-351
return(paste0("<div class='alert alert-warning'>
    <strong>", .("Variable 4 Selected but True Level Missing"), "</strong><br>",
    .("Please select which level in Variable 4 represents the 'true' condition."),
    "</div>"))
```

**Patch 4: R/venn.b.R - Plot titles**

```r
# Line 204
- ggplot2::ggtitle("Venn Diagram of Selected Variables") +
+ ggplot2::ggtitle(.("Venn Diagram of Selected Variables")) +

# Line 264
- name = "Intersection Size",
+ name = .("Intersection Size"),

# Line 285
- ggplot2::ggtitle("ComplexUpset Diagram of Selected Variables") +
+ ggplot2::ggtitle(.("ComplexUpset Diagram of Selected Variables")) +

# Line 317
- grid::grid.text("UpSetR Diagram of Selected Variables", x = 0.5, y = 0.97,
+ grid::grid.text(.("UpSetR Diagram of Selected Variables"), x = 0.5, y = 0.97,
```

---

## 3) Extraction & Update commands

### 3.1 Create or update English template

```r
# In R console at module root
jmvtools::i18nCreate("en")
jmvtools::i18nUpdate("en")
```

### 3.2 Prepare Weblate template (POT)

```bash
cp jamovi/i18n/en.po jamovi/i18n/catalog.pot
# Edit header to set: Language: c\n
```

### 3.3 Create/Update Turkish catalog

```r
# In R console
jmvtools::i18nCreate("tr")
jmvtools::i18nUpdate("tr")
```

---

## 4) Turkish translations table

| Status | msgid | Current msgstr | Suggested Turkish Translation |
|--------|-------|----------------|------------------------------|
| missing | "Data contains no (complete) rows" | | "Veri hiç (tam) satır içermiyor" |
| missing | "Welcome to ClinicoPath Venn Diagram Tool" | | "ClinicoPath Venn Diyagramı Aracına Hoş Geldiniz" |
| missing | "This tool helps you visualize overlaps between categorical variables using Venn and Upset diagrams." | | "Bu araç, kategorik değişkenler arasındaki örtüşmeleri Venn ve Upset diyagramları kullanarak görselleştirmenize yardımcı olur." |
| missing | "Please select at least Variable 1 and Variable 2 to proceed." | | "Devam etmek için lütfen en az Değişken 1 ve Değişken 2'yi seçin." |
| missing | "Variable 1 Selected but True Level Missing" | | "Değişken 1 Seçili Ancak Doğru Düzey Eksik" |
| missing | "Variable 2 Selected but True Level Missing" | | "Değişken 2 Seçili Ancak Doğru Düzey Eksik" |
| missing | "Variable 3 Selected but True Level Missing" | | "Değişken 3 Seçili Ancak Doğru Düzey Eksik" |
| missing | "Variable 4 Selected but True Level Missing" | | "Değişken 4 Seçili Ancak Doğru Düzey Eksik" |
| missing | "Please select which level in Variable 1 represents the 'true' condition." | | "Lütfen Değişken 1'de hangi düzeyin 'doğru' durumu temsil ettiğini seçin." |
| missing | "Please select which level in Variable 2 represents the 'true' condition." | | "Lütfen Değişken 2'de hangi düzeyin 'doğru' durumu temsil ettiğini seçin." |
| missing | "Please select which level in Variable 3 represents the 'true' condition." | | "Lütfen Değişken 3'de hangi düzeyin 'doğru' durumu temsil ettiğini seçin." |
| missing | "Please select which level in Variable 4 represents the 'true' condition." | | "Lütfen Değişken 4'de hangi düzeyin 'doğru' durumu temsil ettiğini seçin." |
| missing | "Venn Diagram of Selected Variables" | | "Seçili Değişkenlerin Venn Diyagramı" |
| missing | "ComplexUpset Diagram of Selected Variables" | | "Seçili Değişkenlerin ComplexUpset Diyagramı" |
| missing | "UpSetR Diagram of Selected Variables" | | "Seçili Değişkenlerin UpSetR Diyagramı" |
| missing | "Intersection Size" | | "Kesişim Boyutu" |

---

## 5) Consistency & glossary (TR)

**Key Terms for Venn Function:**
- Venn Diagram → Venn Diyagramı
- Upset Diagram → Upset Diyagramı
- Variable → Değişken
- True Level → Doğru Düzey
- Intersection → Kesişim
- Overlap → Örtüşme
- Categorical Variable → Kategorik Değişken
- Selected Variables → Seçili Değişkenler

**Style Guidelines:**
- Use formal Turkish appropriate for clinical/academic context
- Maintain consistency with existing ClinicoPath translations
- Use accusative case (-i, -ı, -u, -ü) for direct objects
- Use dative case (-e, -a) for directions/destinations

---

## 6) QA checklist

- ❌ **CRITICAL**: User-visible strings in R/venn.b.R are NOT wrapped with `.()` 
- ✅ NAMESPACE imports translation helper `.`
- ✅ All required YAML files exist
- ❌ **MISSING**: Need to create/update .po files after applying patches
- ❌ **PENDING**: Turkish translations need to be added

---

## 7) Weblate integration (GitHub)

**Status**: ClinicoPath module already has Weblate integration established.

Existing integration:
- Repository: `ClinicoPathJamoviModule`
- Weblate project: Active
- Turkish translation: In progress

---

## 8) Ready-to-run snippets (copy/paste)

**Apply i18n patches (after manual edits):**
```r
# After applying all 4 patches to R/venn.b.R
jmvtools::i18nCreate("en"); jmvtools::i18nUpdate("en")
jmvtools::i18nCreate("tr"); jmvtools::i18nUpdate("tr")
```

**Prepare POT:**
```bash
cp jamovi/i18n/en.po jamovi/i18n/catalog.pot
# Edit header: Language: c\n
```

**Quick check for remaining unwrapped strings:**
```bash
# Find quoted strings not wrapped in .()
grep -nE '"[^"]+"|'"'"'[^'"'"']+'"'"'' R/venn.b.R | grep -v '\.\(' | grep -v '^[[:space:]]*#'
```

---

## 9) Implementation Priority

### Phase 1: Critical patches (HIGH PRIORITY)
1. **Error messages** - Apply Patch 1 (3 locations)
2. **Welcome message** - Apply Patch 2 (1 location)

### Phase 2: User experience (MEDIUM PRIORITY)  
3. **Validation errors** - Apply Patch 3 (4 locations)
4. **Plot titles** - Apply Patch 4 (4 locations)

### Phase 3: Finalization (LOW PRIORITY)
5. **Catalog generation** - Run i18n commands
6. **Turkish translations** - Add to .po files
7. **Testing** - Verify translations in jamovi

---

## Summary

**Current Status**: ❌ **NEEDS WORK** - Zero internationalization implemented

**Files to modify**:
- `R/venn.b.R` - Apply 4 patches wrapping 15 user-visible strings

**Translation workload**: 15 new strings requiring Turkish translation

**Estimated effort**: 2-3 hours (patches + translations + testing)

**Next step**: Apply Patch 1 (error messages) first as it affects core functionality.