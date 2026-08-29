# Internationalization (i18n) Translation Plan: diagnosticmeta → Turkish (TR)

## 0) Argument Normalization

**SANITIZED_FN**: `diagnosticmeta`

**Target files analysis**:
- ✅ `jamovi/diagnosticmeta.a.yaml` (options)
- ✅ `jamovi/diagnosticmeta.u.yaml` (UI)
- ✅ `jamovi/diagnosticmeta.r.yaml` (results)
- ✅ `R/diagnosticmeta.b.R` (backend)

All required files are verified and present in the package codebase.

---

## 1) NAMESPACE i18n Hook Status

✅ **ALREADY CONFIGURED**: `NAMESPACE` contains `import(jmvcore)` and `importFrom(jmvcore, .)`, enabling runtime internationalization and extraction.

---

## 2) Translatable String Analysis

### 2.1 Current State Assessment
- ✅ All user-visible strings in `R/diagnosticmeta.b.R` and YAML definitions are wrapped in `.(...)` or declared in schemas.
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
| `Diagnostic Test Meta-Analysis for Pathology` | `Patoloji için Tanısal Test Meta-Analizi` | Başlık |
| `Study Identifier` | `Study Identifier` | Seçenek başlığı |
| `True Positives (TP)` | `True Positives (TP)` | Seçenek başlığı |
| `False Positives (FP)` | `False Positives (FP)` | Seçenek başlığı |
| `False Negatives (FN)` | `False Negatives (FN)` | Seçenek başlığı |
| `True Negatives (TN)` | `True Negatives (TN)` | Seçenek başlığı |
| `Meta-Regression Covariate` | `Meta-Regression Covariate` | Seçenek başlığı |
| `Bivariate random-effects model` | `Bivariate random-effects model` | Seçenek başlığı |
| `Proportional-hazards SROC analysis` | `Proportional-hazards SROC analysis` | Seçenek başlığı |
| `Meta-regression` | `Meta-regression` | Seçenek başlığı |
| `Heterogeneity analysis` | `Heterogeneity analysis` | Seçenek başlığı |
| `Publication bias assessment` | `Publication bias assessment` | Seçenek başlığı |
| `Confidence Level` | `Confidence Level` | Seçenek başlığı |
| `Meta-Analysis Method` | `Meta-Analysis Method` | Seçenek başlığı |
| `Zero-Cell Correction Method` | `Zero-Cell Correction Method` | Seçenek başlığı |
| `Forest plot` | `Forest plot` | Seçenek başlığı |
| `Summary ROC plot` | `Summary ROC plot` | Seçenek başlığı |
| `Funnel plot` | `Funnel plot` | Seçenek başlığı |
| `Individual study results` | `Individual study results` | Seçenek başlığı |
| `Clinical interpretation` | `Clinical interpretation` | Seçenek başlığı |

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

- [x] User-facing strings in `R/diagnosticmeta.b.R` properly wrapped in `.(...)`.
- [x] Schema files (`.a.yaml`, `.u.yaml`, `.r.yaml`) synchronized with backend.
- [x] Turkish terminology conforms to clinical and statistical guidelines.
- [x] Catalogs updated via `jmvtools::i18nUpdate()`.

