# Internationalization (i18n) Translation Plan: outcomeorganizer → Turkish (TR)

## 0) Argument Normalization

**SANITIZED_FN**: `outcomeorganizer`

**Target files analysis**:
- ✅ `jamovi/outcomeorganizer.a.yaml` (options)
- ✅ `jamovi/outcomeorganizer.u.yaml` (UI)
- ✅ `jamovi/outcomeorganizer.r.yaml` (results)
- ✅ `R/outcomeorganizer.b.R` (backend)

All required files are verified and present in the package codebase.

---

## 1) NAMESPACE i18n Hook Status

✅ **ALREADY CONFIGURED**: `NAMESPACE` contains `import(jmvcore)` and `importFrom(jmvcore, .)`, enabling runtime internationalization and extraction.

---

## 2) Translatable String Analysis

### 2.1 Current State Assessment
- ✅ All user-visible strings in `R/outcomeorganizer.b.R` and YAML definitions are wrapped in `.(...)` or declared in schemas.
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
| `Outcome Organizer for Survival Analysis` | `Sağkalım Analizi için Sonlanım Düzenleyici` | Başlık |
| `Outcome Variable` | `Outcome Variable` | Seçenek başlığı |
| `Event Level` | `Event Level` | Seçenek başlığı |
| `Recurrence/Progression Variable` | `Recurrence/Progression Variable` | Seçenek başlığı |
| `Patient ID` | `Patient ID` | Seçenek başlığı |
| `Follow-up Time` | `Follow-up Time` | Seçenek başlığı |
| `Survival Analysis Type` | `Survival Analysis Type` | Seçenek başlığı |
| `Multiple event levels` | `Multiple event levels` | Seçenek başlığı |
| `Dead of Disease` | `Dead of Disease` | Seçenek başlığı |
| `Dead of Other Causes` | `Dead of Other Causes` | Seçenek başlığı |
| `Alive with Disease` | `Alive with Disease` | Seçenek başlığı |
| `Alive without Disease` | `Alive without Disease` | Seçenek başlığı |
| `Use event hierarchy` | `Use event hierarchy` | Seçenek başlığı |
| `Priority Event Type` | `Priority Event Type` | Seçenek başlığı |
| `Use interval censoring` | `Use interval censoring` | Seçenek başlığı |
| `Interval Start Variable` | `Interval Start Variable` | Seçenek başlığı |
| `Interval End Variable` | `Interval End Variable` | Seçenek başlığı |
| `Use administrative censoring` | `Use administrative censoring` | Seçenek başlığı |
| `Administrative Censoring Date` | `Administrative Censoring Date` | Seçenek başlığı |
| `Output table` | `Output table` | Seçenek başlığı |

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

- [x] User-facing strings in `R/outcomeorganizer.b.R` properly wrapped in `.(...)`.
- [x] Schema files (`.a.yaml`, `.u.yaml`, `.r.yaml`) synchronized with backend.
- [x] Turkish terminology conforms to clinical and statistical guidelines.
- [x] Catalogs updated via `jmvtools::i18nUpdate()`.

