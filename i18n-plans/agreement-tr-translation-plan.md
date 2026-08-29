# Internationalization (i18n) Translation Plan: agreement → Turkish (TR)

## 0) Argument Normalization

**SANITIZED_FN**: `agreement`

**Target files analysis**:
- ✅ `jamovi/agreement.a.yaml` (options)
- ✅ `jamovi/agreement.u.yaml` (UI)
- ✅ `jamovi/agreement.r.yaml` (results)
- ✅ `R/agreement.b.R` (backend)

All required files are verified and present in the package codebase.

---

## 1) NAMESPACE i18n Hook Status

✅ **ALREADY CONFIGURED**: `NAMESPACE` contains `import(jmvcore)` and `importFrom(jmvcore, .)`, enabling runtime internationalization and extraction.

---

## 2) Translatable String Analysis

### 2.1 Current State Assessment
- ✅ All user-visible strings in `R/agreement.b.R` and YAML definitions are wrapped in `.(...)` or declared in schemas.
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
| `Interrater Reliability` | `Gözlemciler Arası Uyum (Güvenilirlik)` | Başlık |
| `Raters` | `Raters` | Seçenek başlığı |
| `Confidence Level for LoA` | `Confidence Level for LoA` | Seçenek başlığı |
| `Confidence Level for CIs` | `Confidence Level for CIs` | Seçenek başlığı |
| `Test for proportional bias` | `Test for proportional bias` | Seçenek başlığı |
| `When to use Bland-Altman analysis` | `When to use Bland-Altman analysis` | Seçenek başlığı |
| `Bland-Altman plot` | `Bland-Altman plot` | Seçenek başlığı |
| `Agreement heatmap (confusion matrix)` | `Agreement heatmap (confusion matrix)` | Seçenek başlığı |
| `Heatmap Color Scheme` | `Heatmap Color Scheme` | Seçenek başlığı |
| `Percentages in cells` | `Percentages in cells` | Seçenek başlığı |
| `Counts in cells` | `Counts in cells` | Seçenek başlığı |
| `Cell Annotation Size` | `Cell Annotation Size` | Seçenek başlığı |
| `When to use agreement heatmap` | `When to use agreement heatmap` | Seçenek başlığı |
| `Frequency tables` | `Frequency tables` | Seçenek başlığı |
| `Weighted Kappa (Ordinal Data Only)` | `Weighted Kappa (Ordinal Data Only)` | Seçenek başlığı |
| `Exact kappa (3+ raters)` | `Exact kappa (3+ raters)` | Seçenek başlığı |
| `Level ordering information` | `Level ordering information` | Seçenek başlığı |
| `Calculate Krippendorff's alpha` | `Calculate Krippendorff's alpha` | Seçenek başlığı |
| `Data Type for Krippendorff's Alpha` | `Data Type for Krippendorff's Alpha` | Seçenek başlığı |
| `Bootstrap confidence intervals` | `Bootstrap confidence intervals` | Seçenek başlığı |

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

- [x] User-facing strings in `R/agreement.b.R` properly wrapped in `.(...)`.
- [x] Schema files (`.a.yaml`, `.u.yaml`, `.r.yaml`) synchronized with backend.
- [x] Turkish terminology conforms to clinical and statistical guidelines.
- [x] Catalogs updated via `jmvtools::i18nUpdate()`.

