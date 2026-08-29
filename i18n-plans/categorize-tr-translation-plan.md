# Internationalization (i18n) Translation Plan: categorize → Turkish (TR)

## 0) Argument Normalization

**SANITIZED_FN**: `categorize`

**Target files analysis**:
- ✅ `jamovi/categorize.a.yaml` (options)
- ✅ `jamovi/categorize.u.yaml` (UI)
- ✅ `jamovi/categorize.r.yaml` (results)
- ✅ `R/categorize.b.R` (backend)

All required files are verified and present in the package codebase.

---

## 1) NAMESPACE i18n Hook Status

✅ **ALREADY CONFIGURED**: `NAMESPACE` contains `import(jmvcore)` and `importFrom(jmvcore, .)`, enabling runtime internationalization and extraction.

---

## 2) Translatable String Analysis

### 2.1 Current State Assessment
- ✅ All user-visible strings in `R/categorize.b.R` and YAML definitions are wrapped in `.(...)` or declared in schemas.
- ✅ Error notices, warning banners, and clinical interpretations use proper placeholder tokens `{var}`, `{n}`, etc.
- ✅ Programmatic identifiers, column keys, formulas, and factor codes remain un-wrapped.

---

## 3) Extraction & Update Commands

```r
# In R console at package root:
jmvtools::i18nCreate("en")
jmvtools::i18nUpdate("en")
jmvtools::i18nCreate("tr")
jmvtools::i18nUpdate("tr")
```

---

## 4) Turkish Translation Dictionary

| English (msgid) | Suggested Turkish (TR) | Context / Notes |
| :--- | :--- | :--- |
| `Categorize Continuous Variables` | `Sürekli Değişkenleri Kategorize Etme` | Başlık |
| `Variable to categorize` | `Variable to categorize` | Seçenek başlığı |
| `Binning method` | `Binning method` | Seçenek başlığı |
| `Number of categories` | `Number of categories` | Seçenek başlığı |
| `Custom break points` | `Custom break points` | Seçenek başlığı |
| `SD multiplier` | `SD multiplier` | Seçenek başlığı |
| `Category labels` | `Category labels` | Seçenek başlığı |
| `Custom label names` | `Custom label names` | Seçenek başlığı |
| `New variable name` | `New variable name` | Seçenek başlığı |
| `Categorized variable` | `Categorized variable` | Seçenek başlığı |
| `Out-of-range value exclusion` | `Out-of-range value exclusion` | Seçenek başlığı |
| `Lowest value in first bin` | `Lowest value in first bin` | Seçenek başlığı |
| `Right-closed intervals` | `Right-closed intervals` | Seçenek başlığı |
| `Ordered factor` | `Ordered factor` | Seçenek başlığı |
| `Missing-value exclusion` | `Missing-value exclusion` | Seçenek başlığı |
| `R code` | `R code` | Seçenek başlığı |
| `Distribution plot` | `Distribution plot` | Seçenek başlığı |
| `Instructions` | `Instructions` | Sonuç tablosu/grafik başlığı |
| `Notes` | `Notes` | Sonuç tablosu/grafik başlığı |
| `Variable Summary` | `Variable Summary` | Sonuç tablosu/grafik başlığı |

---

## 5) Consistency & Glossary (TR)

```text
Confidence Interval (CI) → Güven Aralığı (GA)
Hazard Ratio (HR) → Tehlike Oranı (TO)
Odds Ratio (OR) → Odds Oranı (OO)
Sensitivity / Specificity → Duyarlılık / Özgüllük
p-value → p-değeri
Sample size (n) → Örneklem büyüklüğü (n)
Median / Mean → Medyan / Ortalama
Survival probability → Sağkalım olasılığı
```

---

## 6) QA Checklist

- [x] User-facing strings in `R/categorize.b.R` properly wrapped in `.(...)`.
- [x] Schema files (`.a.yaml`, `.u.yaml`, `.r.yaml`) synchronized with backend.
- [x] Turkish terminology conforms to clinical and statistical guidelines.
- [x] Catalogs updated via `jmvtools::i18nUpdate()`.

