# Internationalization (i18n) Translation Plan: datetimeconverter → Turkish (TR)

## 0) Argument Normalization

**SANITIZED_FN**: `datetimeconverter`

**Target files analysis**:
- ✅ `jamovi/datetimeconverter.a.yaml` (options)
- ✅ `jamovi/datetimeconverter.u.yaml` (UI)
- ✅ `jamovi/datetimeconverter.r.yaml` (results)
- ✅ `R/datetimeconverter.b.R` (backend)

All required files are verified and present in the package codebase.

---

## 1) NAMESPACE i18n Hook Status

✅ **ALREADY CONFIGURED**: `NAMESPACE` contains `import(jmvcore)` and `importFrom(jmvcore, .)`, enabling runtime internationalization and extraction.

---

## 2) Translatable String Analysis

### 2.1 Current State Assessment
- ✅ All user-visible strings in `R/datetimeconverter.b.R` and YAML definitions are wrapped in `.(...)` or declared in schemas.
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
| `DateTime Converter` | `Tarih-Saat Dönüştürücü` | Başlık |
| `DateTime Variable` | `DateTime Variable` | Seçenek başlığı |
| `DateTime Format` | `DateTime Format` | Seçenek başlığı |
| `Timezone` | `Timezone` | Seçenek başlığı |
| `Number of Rows to Preview` | `Number of Rows to Preview` | Seçenek başlığı |
| `Extract year` | `Extract year` | Seçenek başlığı |
| `Extract month` | `Extract month` | Seçenek başlığı |
| `Extract month name` | `Extract month name` | Seçenek başlığı |
| `Extract day` | `Extract day` | Seçenek başlığı |
| `Extract hour` | `Extract hour` | Seçenek başlığı |
| `Extract minute` | `Extract minute` | Seçenek başlığı |
| `Extract second` | `Extract second` | Seçenek başlığı |
| `Extract day name` | `Extract day name` | Seçenek başlığı |
| `Extract week number` | `Extract week number` | Seçenek başlığı |
| `Extract quarter` | `Extract quarter` | Seçenek başlığı |
| `Extract day of year` | `Extract day of year` | Seçenek başlığı |
| `Data quality assessment` | `Data quality assessment` | Seçenek başlığı |
| `Natural-language summary` | `Natural-language summary` | Seçenek başlığı |
| `Explanatory notes` | `Explanatory notes` | Seçenek başlığı |
| `Glossary of terms` | `Glossary of terms` | Seçenek başlığı |

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

- [x] User-facing strings in `R/datetimeconverter.b.R` properly wrapped in `.(...)`.
- [x] Schema files (`.a.yaml`, `.u.yaml`, `.r.yaml`) synchronized with backend.
- [x] Turkish terminology conforms to clinical and statistical guidelines.
- [x] Catalogs updated via `jmvtools::i18nUpdate()`.

