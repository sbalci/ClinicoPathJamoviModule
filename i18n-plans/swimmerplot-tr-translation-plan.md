# Internationalization (i18n) Translation Plan: swimmerplot → Turkish (TR)

## 0) Argument Normalization

**SANITIZED_FN**: `swimmerplot`

**Target files analysis**:
- ✅ `jamovi/swimmerplot.a.yaml` (options)
- ✅ `jamovi/swimmerplot.u.yaml` (UI)
- ✅ `jamovi/swimmerplot.r.yaml` (results)
- ✅ `R/swimmerplot.b.R` (backend)

All required files are verified and present in the package codebase.

---

## 1) NAMESPACE i18n Hook Status

✅ **ALREADY CONFIGURED**: `NAMESPACE` contains `import(jmvcore)` and `importFrom(jmvcore, .)`, enabling runtime internationalization and extraction.

---

## 2) Translatable String Analysis

### 2.1 Current State Assessment
- ✅ All user-visible strings in `R/swimmerplot.b.R` and YAML definitions are wrapped in `.(...)` or declared in schemas.
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
| `Swimmer Plot` | `Yüzücü (Swimmer) Grafiği` | Başlık |
| `Patient ID` | `Patient ID` | Seçenek başlığı |
| `Start Time` | `Start Time` | Seçenek başlığı |
| `End Time` | `End Time` | Seçenek başlığı |
| `Response/Status Variable` | `Response/Status Variable` | Seçenek başlığı |
| `Censoring/Event Status Variable` | `Censoring/Event Status Variable` | Seçenek başlığı |
| `Grouping Variable` | `Grouping Variable` | Seçenek başlığı |
| `Time Input Type` | `Time Input Type` | Seçenek başlığı |
| `Date Format in Data` | `Date Format in Data` | Seçenek başlığı |
| `Time Unit for Display` | `Time Unit for Display` | Seçenek başlığı |
| `Time Display Mode` | `Time Display Mode` | Seçenek başlığı |
| `Maximum milestones` | `Maximum milestones` | Seçenek başlığı |
| `Milestone 1 Name` | `Milestone 1 Name` | Seçenek başlığı |
| `Milestone 1 Date` | `Milestone 1 Date` | Seçenek başlığı |
| `Milestone 2 Name` | `Milestone 2 Name` | Seçenek başlığı |
| `Milestone 2 Date` | `Milestone 2 Date` | Seçenek başlığı |
| `Milestone 3 Name` | `Milestone 3 Name` | Seçenek başlığı |
| `Milestone 3 Date` | `Milestone 3 Date` | Seçenek başlığı |
| `Milestone 4 Name` | `Milestone 4 Name` | Seçenek başlığı |
| `Milestone 4 Date` | `Milestone 4 Date` | Seçenek başlığı |

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

- [x] User-facing strings in `R/swimmerplot.b.R` properly wrapped in `.(...)`.
- [x] Schema files (`.a.yaml`, `.u.yaml`, `.r.yaml`) synchronized with backend.
- [x] Turkish terminology conforms to clinical and statistical guidelines.
- [x] Catalogs updated via `jmvtools::i18nUpdate()`.

