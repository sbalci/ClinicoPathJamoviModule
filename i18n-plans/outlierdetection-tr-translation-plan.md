# Internationalization (i18n) Translation Plan: outlierdetection → Turkish (TR)

## 0) Argument Normalization

**SANITIZED_FN**: `outlierdetection`

**Target files analysis**:
- ✅ `jamovi/outlierdetection.a.yaml` (options)
- ✅ `jamovi/outlierdetection.u.yaml` (UI)
- ✅ `jamovi/outlierdetection.r.yaml` (results)
- ✅ `R/outlierdetection.b.R` (backend)

All required files are verified and present in the package codebase.

---

## 1) NAMESPACE i18n Hook Status

✅ **ALREADY CONFIGURED**: `NAMESPACE` contains `import(jmvcore)` and `importFrom(jmvcore, .)`, enabling runtime internationalization and extraction.

---

## 2) Translatable String Analysis

### 2.1 Current State Assessment
- ✅ All user-visible strings in `R/outlierdetection.b.R` and YAML definitions are wrapped in `.(...)` or declared in schemas.
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
| `Outlier Detection` | `Aykırı Değer Tespiti` | Başlık |
| `Variables for analysis` | `Variables for analysis` | Seçenek başlığı |
| `Detection method category` | `Detection method category` | Seçenek başlığı |
| `Univariate method` | `Univariate method` | Seçenek başlığı |
| `Multivariate method` | `Multivariate method` | Seçenek başlığı |
| `Composite score threshold` | `Composite score threshold` | Seçenek başlığı |
| `Z-score threshold` | `Z-score threshold` | Seçenek başlığı |
| `IQR multiplier` | `IQR multiplier` | Seçenek başlığı |
| `Confidence level for intervals` | `Confidence level for intervals` | Seçenek başlığı |
| `Outlier summary table` | `Outlier summary table` | Seçenek başlığı |
| `Method comparison` | `Method comparison` | Seçenek başlığı |
| `Exclusion recommendations` | `Exclusion recommendations` | Seçenek başlığı |
| `Outlier visualization` | `Outlier visualization` | Seçenek başlığı |
| `Analysis interpretation` | `Analysis interpretation` | Seçenek başlığı |
| `Subsample above (rows)` | `Subsample above (rows)` | Seçenek başlığı |
| `Rows to analyse when subsampling` | `Rows to analyse when subsampling` | Seçenek başlığı |
| `Random seed` | `Random seed` | Seçenek başlığı |
| `Instructions` | `Instructions` | Sonuç tablosu/grafik başlığı |
| `Analysis Messages` | `Analysis Messages` | Sonuç tablosu/grafik başlığı |
| `Outlier Detection Plot` | `Outlier Detection Plot` | Sonuç tablosu/grafik başlığı |

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

- [x] User-facing strings in `R/outlierdetection.b.R` properly wrapped in `.(...)`.
- [x] Schema files (`.a.yaml`, `.u.yaml`, `.r.yaml`) synchronized with backend.
- [x] Turkish terminology conforms to clinical and statistical guidelines.
- [x] Catalogs updated via `jmvtools::i18nUpdate()`.

