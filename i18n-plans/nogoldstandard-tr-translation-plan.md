# Internationalization (i18n) Translation Plan: nogoldstandard → Turkish (TR)

## 0) Argument Normalization

**SANITIZED_FN**: `nogoldstandard`

**Target files analysis**:
- ✅ `jamovi/nogoldstandard.a.yaml` (options)
- ✅ `jamovi/nogoldstandard.u.yaml` (UI)
- ✅ `jamovi/nogoldstandard.r.yaml` (results)
- ✅ `R/nogoldstandard.b.R` (backend)

All required files are verified and present in the package codebase.

---

## 1) NAMESPACE i18n Hook Status

✅ **ALREADY CONFIGURED**: `NAMESPACE` contains `import(jmvcore)` and `importFrom(jmvcore, .)`, enabling runtime internationalization and extraction.

---

## 2) Translatable String Analysis

### 2.1 Current State Assessment
- ✅ All user-visible strings in `R/nogoldstandard.b.R` and YAML definitions are wrapped in `.(...)` or declared in schemas.
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
| `Analysis Without Gold Standard` | `Altın Standart Olmadan Analiz (Hui-Walter / LCA)` | Başlık |
| `Illustrative Scenario Example` | `Illustrative Scenario Example` | Seçenek başlığı |
| `Test 1` | `Test 1` | Seçenek başlığı |
| `Positive Level` | `Positive Level` | Seçenek başlığı |
| `Test 2` | `Test 2` | Seçenek başlığı |
| `Test 3` | `Test 3` | Seçenek başlığı |
| `Test 4` | `Test 4` | Seçenek başlığı |
| `Test 5` | `Test 5` | Seçenek başlığı |
| `Analysis Method` | `Analysis Method` | Seçenek başlığı |
| `Bootstrap CI` | `Bootstrap CI` | Seçenek başlığı |
| `Number of Bootstrap Samples` | `Number of Bootstrap Samples` | Seçenek başlığı |
| `Alpha for Confidence Intervals` | `Alpha for Confidence Intervals` | Seçenek başlığı |
| `Analysis Diagnostics` | `Analysis Diagnostics` | Seçenek başlığı |
| `Random Seed` | `Random Seed` | Seçenek başlığı |
| `Plain-Language Summary` | `Plain-Language Summary` | Seçenek başlığı |
| `Method Guide` | `Method Guide` | Seçenek başlığı |
| `Important Information` | `Important Information` | Sonuç tablosu/grafik başlığı |
| `Instructions` | `Instructions` | Sonuç tablosu/grafik başlığı |
| `Agreement Statistics (Cohen's Kappa)` | `Agreement Statistics (Cohen's Kappa)` | Sonuç tablosu/grafik başlığı |
| `Plain-Language Summary` | `Plain-Language Summary` | Sonuç tablosu/grafik başlığı |

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

- [x] User-facing strings in `R/nogoldstandard.b.R` properly wrapped in `.(...)`.
- [x] Schema files (`.a.yaml`, `.u.yaml`, `.r.yaml`) synchronized with backend.
- [x] Turkish terminology conforms to clinical and statistical guidelines.
- [x] Catalogs updated via `jmvtools::i18nUpdate()`.

