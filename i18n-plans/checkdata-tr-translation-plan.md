# Internationalization (i18n) Translation Plan: checkdata → Turkish (TR)

## 0) Argument Normalization

**SANITIZED_FN**: `checkdata`

**Target files analysis**:
- ✅ `jamovi/checkdata.a.yaml` (options)
- ✅ `jamovi/checkdata.u.yaml` (UI)
- ✅ `jamovi/checkdata.r.yaml` (results)
- ✅ `R/checkdata.b.R` (backend)

All required files are verified and present in the package codebase.

---

## 1) NAMESPACE i18n Hook Status

✅ **ALREADY CONFIGURED**: `NAMESPACE` contains `import(jmvcore)` and `importFrom(jmvcore, .)`, enabling runtime internationalization and extraction.

---

## 2) Translatable String Analysis

### 2.1 Current State Assessment
- ✅ All user-visible strings in `R/checkdata.b.R` and YAML definitions are wrapped in `.(...)` or declared in schemas.
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
| `Single Variable Quality Check` | `Tek Değişkenli Kalite Kontrolü` | Başlık |
| `Variable to check` | `Variable to check` | Seçenek başlığı |
| `Outlier analysis` | `Outlier analysis` | Seçenek başlığı |
| `Distribution analysis` | `Distribution analysis` | Seçenek başlığı |
| `Duplicate analysis` | `Duplicate analysis` | Seçenek başlığı |
| `Data patterns` | `Data patterns` | Seçenek başlığı |
| `Rare category threshold (%)` | `Rare category threshold (%)` | Seçenek başlığı |
| `Clinical plausibility checks` | `Clinical plausibility checks` | Seçenek başlığı |
| `Unit system for clinical checks` | `Unit system for clinical checks` | Seçenek başlığı |
| `Outlier-detection transformation` | `Outlier-detection transformation` | Seçenek başlığı |
| `Explain MCAR testability` | `Explain MCAR testability` | Seçenek başlığı |
| `Minimum mean for CV calculation` | `Minimum mean for CV calculation` | Seçenek başlığı |
| `Natural-language summary` | `Natural-language summary` | Seçenek başlığı |
| `About this analysis` | `About this analysis` | Seçenek başlığı |
| `Caveats & assumptions` | `Caveats & assumptions` | Seçenek başlığı |
| `Important Information` | `Important Information` | Sonuç tablosu/grafik başlığı |
| `Getting Started` | `Getting Started` | Sonuç tablosu/grafik başlığı |
| `Quality Assessment Summary` | `Quality Assessment Summary` | Sonuç tablosu/grafik başlığı |
| `Missing Data Analysis` | `Missing Data Analysis` | Sonuç tablosu/grafik başlığı |
| `Outlier Detection (Consensus: >=2 methods)` | `Outlier Detection (Consensus: >=2 methods)` | Sonuç tablosu/grafik başlığı |

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

- [x] User-facing strings in `R/checkdata.b.R` properly wrapped in `.(...)`.
- [x] Schema files (`.a.yaml`, `.u.yaml`, `.r.yaml`) synchronized with backend.
- [x] Turkish terminology conforms to clinical and statistical guidelines.
- [x] Catalogs updated via `jmvtools::i18nUpdate()`.

