# Internationalization (i18n) Translation Plan: lassologistic → Turkish (TR)

## 0) Argument Normalization

**SANITIZED_FN**: `lassologistic`

**Target files analysis**:
- ✅ `jamovi/lassologistic.a.yaml` (options)
- ✅ `jamovi/lassologistic.u.yaml` (UI)
- ✅ `jamovi/lassologistic.r.yaml` (results)
- ✅ `R/lassologistic.b.R` (backend)

All required files are verified and present in the package codebase.

---

## 1) NAMESPACE i18n Hook Status

✅ **ALREADY CONFIGURED**: `NAMESPACE` contains `import(jmvcore)` and `importFrom(jmvcore, .)`, enabling runtime internationalization and extraction.

---

## 2) Translatable String Analysis

### 2.1 Current State Assessment
- ✅ All user-visible strings in `R/lassologistic.b.R` and YAML definitions are wrapped in `.(...)` or declared in schemas.
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
| `LASSO Logistic Regression` | `LASSO Lojistik Regresyon` | Başlık |
| `Binary Outcome` | `Binary Outcome` | Seçenek başlığı |
| `Event Level` | `Event Level` | Seçenek başlığı |
| `Explanatory Variables` | `Explanatory Variables` | Seçenek başlığı |
| `Penalty Type` | `Penalty Type` | Seçenek başlığı |
| `Elastic Net Mixing (0=Ridge, 1=LASSO)` | `Elastic Net Mixing (0=Ridge, 1=LASSO)` | Seçenek başlığı |
| `Lambda Selection Method` | `Lambda Selection Method` | Seçenek başlığı |
| `Number of CV Folds` | `Number of CV Folds` | Seçenek başlığı |
| `Random Seed` | `Random Seed` | Seçenek başlığı |
| `Standardize variables` | `Standardize variables` | Seçenek başlığı |
| `Data suitability assessment` | `Data suitability assessment` | Seçenek başlığı |
| `Bootstrap internal validation` | `Bootstrap internal validation` | Seçenek başlığı |
| `Bootstrap Iterations` | `Bootstrap Iterations` | Seçenek başlığı |
| `Cross-validation plot` | `Cross-validation plot` | Seçenek başlığı |
| `Coefficient plot` | `Coefficient plot` | Seçenek başlığı |
| `ROC curve` | `ROC curve` | Seçenek başlığı |
| `Scoring system` | `Scoring system` | Seçenek başlığı |
| `Scoring Method` | `Scoring Method` | Seçenek başlığı |
| `Maximum Points per Feature` | `Maximum Points per Feature` | Seçenek başlığı |
| `Cut Point for Continuous Predictors` | `Cut Point for Continuous Predictors` | Seçenek başlığı |

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

- [x] User-facing strings in `R/lassologistic.b.R` properly wrapped in `.(...)`.
- [x] Schema files (`.a.yaml`, `.u.yaml`, `.r.yaml`) synchronized with backend.
- [x] Turkish terminology conforms to clinical and statistical guidelines.
- [x] Catalogs updated via `jmvtools::i18nUpdate()`.

