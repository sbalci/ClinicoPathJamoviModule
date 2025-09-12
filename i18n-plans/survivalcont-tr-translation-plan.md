# survivalcont - Turkish (TR) Translation Plan

## 0) Argument normalization (safety)

**SANITIZED_FN**: `survivalcont`

### Target files found:
- ✅ `jamovi/survivalcont.a.yaml` (options)
- ✅ `jamovi/survivalcont.u.yaml` (UI)  
- ✅ `jamovi/survivalcont.r.yaml` (results)
- ✅ `R/survivalcont.b.R` (backend)

All required files are present and ready for i18n processing.

---

## 1) NAMESPACE i18n hook

✅ **Status**: Already configured correctly

The NAMESPACE file already includes the required import:
```r
importFrom(jmvcore, .)
```

**Line 854**: `importFrom(jmvcore,.)`

No changes needed - the translation helper `.` function is properly imported.

---

## 2) Wrap translatable strings (jamovi patterns)

### 2.1 Critical R Backend File Updates Required

The `R/survivalcont.b.R` file contains **numerous user-facing strings that need wrapping** with `.()` for translation extraction. Here are the key categories found:

### 2.2 Error & Warning Messages to Wrap

**Current unwrapped strings** (need `.()` wrapping):

```r
# Current code (lines 456, 461, 467):
stop(paste0("Date parsing error: ", e$message, ...))
stop(paste0("Unsupported time type: ", timetypedata, ...))
stop(paste0("Time difference cannot be calculated. Make sure that time type in variables are correct. Currently it is: ", self$options$timetypedata))

# Should become:
stop(.("Date parsing error: {error}"), error = e$message)
stop(.("Unsupported time type: {type}. Supported formats: {formats}"), 
     type = timetypedata, formats = paste(names(lubridate_functions), collapse = ", "))
stop(.("Time difference cannot be calculated. Make sure that time type in variables are correct. Currently it is: {type}"), 
     type = self$options$timetypedata)
```

**Additional warnings/messages to wrap**:

```r
# Line ~1915:
warning(paste("Variable", mycontexpl, "not found in data. Available columns:", paste(names(mydata), collapse = ", ")))
# Should become:
warning(.("Variable {var} not found in data. Available columns: {cols}"), 
        var = mycontexpl, cols = paste(names(mydata), collapse = ", "))

# Line ~1921:
warning(paste("Variable", mycontexpl, "is NULL"))
# Should become:
warning(.("Variable {var} is NULL"), var = mycontexpl)
```

### 2.3 Table Labels & User-visible Text

**HTML and Description Content** (lines 378, 1024, 1039):

```r
# Current glue() usage needs .() wrapping:
glue::glue("Cox regression analysis for continuous variable {contexpl}...")

# Should become:
glue::glue(.("Cox regression analysis for continuous variable {contexpl}..."))
```

### 2.4 Complete String Inventory Requiring Translation

**High Priority Strings**:

1. **Error Messages** (15+ instances)
2. **Warning Messages** (10+ instances) 
3. **Status Messages** (8+ instances)
4. **Table Descriptions** (5+ instances)
5. **Plot Titles** (embedded in glue() calls)
6. **Data Validation Messages** (10+ instances)

---

## 3) Extraction & Update commands

### 3.1 Create/Update English template:

```r
# In R console at module root
jmvtools::i18nCreate("en")
jmvtools::i18nUpdate("en")
```

### 3.2 Prepare Weblate template:

```bash
cp jamovi/i18n/en.po jamovi/i18n/catalog.pot
# Edit header to ensure: Language: c\n
```

### 3.3 Create/Update Turkish catalog:

```r
# In R console
jmvtools::i18nCreate("tr") 
jmvtools::i18nUpdate("tr")
```

---

## 4) Translation Table - Turkish Suggestions

Based on the current analysis, here are the **key translatable strings** with Turkish suggestions:

| Status | msgid (English) | Current msgstr | Suggested Turkish |
|--------|-----------------|----------------|-------------------|
| **missing** | "Date parsing error: {error}" | "" | "Tarih ayrıştırma hatası: {error}" |
| **missing** | "Unsupported time type: {type}" | "" | "Desteklenmeyen zaman türü: {type}" |
| **missing** | "Time difference cannot be calculated" | "" | "Zaman farkı hesaplanamıyor" |
| **missing** | "Variable {var} not found in data" | "" | "{var} değişkeni veride bulunamadı" |
| **missing** | "Variable {var} is NULL" | "" | "{var} değişkeni NULL" |
| **missing** | "Data contains no (complete) rows" | "" | "Veri hiçbir (tam) satır içermiyor" |
| **missing** | "Insufficient data for multiple cutoffs analysis" | "" | "Çoklu kesme noktası analizi için yetersiz veri" |
| **missing** | "Cox regression analysis for continuous variable" | "" | "Sürekli değişken için Cox regresyon analizi" |
| **missing** | "Survival Analysis for Continuous Variable" | "" | "Sürekli Değişken için Yaşam Analizi" |
| **missing** | "Cut-off Point Analysis" | "" | "Kesme Noktası Analizi" |
| **missing** | "Median Survival Summary" | "" | "Medyan Yaşam Özeti" |
| **missing** | "Person-Time Analysis" | "" | "Kişi-Zaman Analizi" |
| **missing** | "RMST Analysis" | "" | "RMST Analizi" |
| **missing** | "Multiple Cut-off Points" | "" | "Çoklu Kesme Noktaları" |
| **missing** | "Small sample size (n < 20). Results may be unreliable" | "" | "Küçük örneklem boyutu (n < 20). Sonuçlar güvenilmez olabilir" |

---

## 5) Consistency & Glossary (TR)

**Medical/Statistical Term Translations**:

```text
Survival Analysis → Yaşam Analizi / Sağkalım Analizi
Cox Regression → Cox Regresyon
Hazard Ratio (HR) → Tehlike Oranı (TO)
Confidence Interval (CI) → Güven Aralığı (GA)
Cut-off Point → Kesme Noktası
Median Survival → Medyan Yaşam / Medyan Sağkalım
Person-Time → Kişi-Zaman
RMST (Restricted Mean Survival Time) → KMYS (Kısıtlı Medyan Yaşam Süresi)
Log-rank test → Log-rank testi
Kaplan-Meier → Kaplan-Meier
Cumulative Events → Kümülatif Olaylar
Cumulative Hazard → Kümülatif Tehlike
Event Level → Olay Düzeyi
Time Elapsed → Geçen Zaman
Follow-up → Takip
Diagnosis → Tanı
```

---

## 6) QA Checklist

- ✅ Identified 4 target files (all exist)
- ✅ NAMESPACE correctly imports `.` function  
- ❌ **CRITICAL**: R backend file needs extensive `.()` wrapping (~40+ strings)
- ✅ Found existing i18n catalogs (`en.po`, `tr.po`) 
- ❌ **TR catalog is mostly empty** - needs population after string extraction
- ❌ **No strings currently wrapped** in survivalcont.b.R

**Priority Actions Needed**:
1. Wrap all user-visible strings in `R/survivalcont.b.R` with `.()` 
2. Update i18n catalogs after wrapping
3. Translate the ~40+ strings identified
4. Test string extraction process

---

## 7) Ready-to-run snippets

### Wrap critical error messages:

```r
# Replace in R/survivalcont.b.R around lines 456, 461, 467:

# OLD:
stop(paste0("Date parsing error: ", e$message, ...))
stop(paste0("Unsupported time type: ", timetypedata, ...))

# NEW: 
stop(.("Date parsing error: {error}"), error = e$message)
stop(.("Unsupported time type: {type}. Supported formats: {formats}"), 
     type = timetypedata, formats = paste(names(lubridate_functions), collapse = ", "))
```

### Update catalogs after wrapping:

```r
jmvtools::i18nUpdate("en")
jmvtools::i18nUpdate("tr")
```

### Find remaining unwrapped strings:

```bash
# Rough heuristic to find quoted strings not wrapped
grep -nE '"[^"\\n]+' R/survivalcont.b.R | grep -v '\\.\('
```

---

## 8) Weblate Integration

1. **Create i18n repository**: `survivalcont-i18n` or `clinicopath-i18n`
2. **Add files**: `catalog.pot`, `README.md`, license
3. **Add Weblate bot** as collaborator
4. **Configure webhook**: `https://hosted.weblate.org/hooks/github/`
5. **Contact jamovi dev team** to add project to Weblate

---

## 9) Implementation Priority

### Phase 1 (Critical): String Wrapping
- Wrap all error/warning/message strings in `R/survivalcont.b.R`
- Focus on lines: 456, 461, 467, 1915, 1921, and glue() calls

### Phase 2: Catalog Population
- Run extraction commands to populate catalogs
- Translate core survival analysis terminology

### Phase 3: Testing & Validation  
- Test string extraction with `jmvtools::prepare()`
- Validate Turkish translations with medical professionals
- Ensure clinical terminology consistency

### Critical File Changes Required

The **`R/survivalcont.b.R`** file requires significant modifications to wrap user-facing strings. The current file has **0 wrapped strings** but approximately **40+ translatable strings** that need `.()` wrapping.

**Next Steps**: 
1. Begin with high-impact error/warning messages
2. Process glue() template strings  
3. Handle table titles and descriptions
4. Update catalogs and test extraction process

**Estimated Effort**: ~4-6 hours for complete string wrapping + 2-3 hours for translation work.

---

## Summary

The `survivalcont` function is currently **not ready for translation** as it lacks proper string wrapping. However, the infrastructure (NAMESPACE, catalogs) is in place. The main task is systematically wrapping the ~40+ user-visible strings with the `.()` function, then extracting and translating them into Turkish using clinical/statistical terminology appropriate for pathologists and clinical researchers.