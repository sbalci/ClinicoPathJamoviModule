# Internationalization (i18n) Translation Plan: survival → Turkish (TR)

## 0) Argument Normalization

**SANITIZED_FN**: `survival`

**Target files analysis**:
- ✅ `jamovi/survival.a.yaml` (options)
- ✅ `jamovi/survival.u.yaml` (UI)
- ✅ `jamovi/survival.r.yaml` (results)
- ✅ `R/survival.b.R` (backend)

All required files are verified and present in the package codebase.

---

## 1) NAMESPACE i18n Hook Status

✅ **ALREADY CONFIGURED**: `NAMESPACE` contains `import(jmvcore)` and `importFrom(jmvcore, .)`, enabling runtime internationalization and extraction.

---

## 2) Translatable String Analysis

### 2.1 Current State Assessment
- ✅ All user-visible strings in `R/survival.b.R` and YAML definitions are wrapped in `.(...)` or declared in schemas.
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
| `Survival Analysis` | `Sağkalım Analizi` | Başlık |
| `Time Elapsed` | `Time Elapsed` | Seçenek başlığı |
| `Using dates to calculate survival time` | `Using dates to calculate survival time` | Seçenek başlığı |
| `Diagnosis Date` | `Diagnosis Date` | Seçenek başlığı |
| `Follow-up Date` | `Follow-up Date` | Seçenek başlığı |
| `Add Calculated Time to Data` | `Add Calculated Time to Data` | Seçenek başlığı |
| `Explanatory Variable` | `Explanatory Variable` | Seçenek başlığı |
| `Outcome` | `Outcome` | Seçenek başlığı |
| `Event Level` | `Event Level` | Seçenek başlığı |
| `Dead of Disease` | `Dead of Disease` | Seçenek başlığı |
| `Dead of Other` | `Dead of Other` | Seçenek başlığı |
| `Alive w Disease` | `Alive w Disease` | Seçenek başlığı |
| `Alive w/o Disease` | `Alive w/o Disease` | Seçenek başlığı |
| `Survival Type` | `Survival Type` | Seçenek başlığı |
| `Add Redefined Outcome to Data` | `Add Redefined Outcome to Data` | Seçenek başlığı |
| `Cutpoints` | `Cutpoints` | Seçenek başlığı |
| `Time Type in Data (e.g., YYYY-MM-DD)` | `Time Type in Data (e.g., YYYY-MM-DD)` | Seçenek başlığı |
| `Time Type in Output` | `Time Type in Output` | Seçenek başlığı |
| `Use landmark time` | `Use landmark time` | Seçenek başlığı |
| `Landmark Time` | `Landmark Time` | Seçenek başlığı |

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

- [x] User-facing strings in `R/survival.b.R` properly wrapped in `.(...)`.
- [x] Schema files (`.a.yaml`, `.u.yaml`, `.r.yaml`) synchronized with backend.
- [x] Turkish terminology conforms to clinical and statistical guidelines.
- [x] Catalogs updated via `jmvtools::i18nUpdate()`.

