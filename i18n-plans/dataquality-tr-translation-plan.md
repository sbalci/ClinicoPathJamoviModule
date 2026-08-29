# Internationalization (i18n) Translation Plan: dataquality → Turkish (TR)

## 0) Argument Normalization

**SANITIZED_FN**: `dataquality`

**Target files analysis**:
- ✅ `jamovi/dataquality.a.yaml` (options)
- ✅ `jamovi/dataquality.u.yaml` (UI)
- ✅ `jamovi/dataquality.r.yaml` (results)
- ✅ `R/dataquality.b.R` (backend)

All required files are verified and present in the package codebase.

---

## 1) NAMESPACE i18n Hook Status

✅ **ALREADY CONFIGURED**: `NAMESPACE` contains `import(jmvcore)` and `importFrom(jmvcore, .)`, enabling runtime internationalization and extraction.

---

## 2) Translatable String Analysis

### 2.1 Current State Assessment
- ✅ All user-visible strings in `R/dataquality.b.R` and YAML definitions are wrapped in `.(...)` or declared in schemas.
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
| `Multi-Variable Visual Quality` | `Çok Değişkenli Görsel Kalite` | Başlık |
| `Variables` | `Variables` | Seçenek başlığı |
| `Duplicate values` | `Duplicate values` | Seçenek başlığı |
| `Missing value analysis` | `Missing value analysis` | Seçenek başlığı |
| `Duplicate rows` | `Duplicate rows` | Seçenek başlığı |
| `Data overview plot (vis_dat)` | `Data overview plot (vis_dat)` | Seçenek başlığı |
| `Missing patterns plot (vis_miss)` | `Missing patterns plot (vis_miss)` | Seçenek başlığı |
| `Data types plot (vis_guess)` | `Data types plot (vis_guess)` | Seçenek başlığı |
| `Missing-data highlight threshold (percent)` | `Missing-data highlight threshold (percent)` | Seçenek başlığı |
| `Plain-language summary` | `Plain-language summary` | Seçenek başlığı |
| `Action recommendations` | `Action recommendations` | Seçenek başlığı |
| `Educational explanations` | `Educational explanations` | Seçenek başlığı |
| `Important Information` | `Important Information` | Sonuç tablosu/grafik başlığı |
| `Data Quality Summary` | `Data Quality Summary` | Sonuç tablosu/grafik başlığı |
| `Plain-Language Summary` | `Plain-Language Summary` | Sonuç tablosu/grafik başlığı |
| `Recommended Actions` | `Recommended Actions` | Sonuç tablosu/grafik başlığı |
| `Understanding Quality Metrics` | `Understanding Quality Metrics` | Sonuç tablosu/grafik başlığı |
| `Data Overview` | `Data Overview` | Sonuç tablosu/grafik başlığı |
| `Missing Patterns` | `Missing Patterns` | Sonuç tablosu/grafik başlığı |
| `Data Types` | `Data Types` | Sonuç tablosu/grafik başlığı |

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

- [x] User-facing strings in `R/dataquality.b.R` properly wrapped in `.(...)`.
- [x] Schema files (`.a.yaml`, `.u.yaml`, `.r.yaml`) synchronized with backend.
- [x] Turkish terminology conforms to clinical and statistical guidelines.
- [x] Catalogs updated via `jmvtools::i18nUpdate()`.

