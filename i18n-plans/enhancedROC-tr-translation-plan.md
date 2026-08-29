# Internationalization (i18n) Translation Plan: enhancedROC → Turkish (TR)

## 0) Argument Normalization

**SANITIZED_FN**: `enhancedROC`

**Target files analysis**:
- ✅ `jamovi/enhancedROC.a.yaml` (options)
- ✅ `jamovi/enhancedROC.u.yaml` (UI)
- ✅ `jamovi/enhancedROC.r.yaml` (results)
- ✅ `R/enhancedROC.b.R` (backend)

All required files are verified and present in the package codebase.

---

## 1) NAMESPACE i18n Hook Status

✅ **ALREADY CONFIGURED**: `NAMESPACE` contains `import(jmvcore)` and `importFrom(jmvcore, .)`, enabling runtime internationalization and extraction.

---

## 2) Translatable String Analysis

### 2.1 Current State Assessment
- ✅ All user-visible strings in `R/enhancedROC.b.R` and YAML definitions are wrapped in `.(...)` or declared in schemas.
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
| `Clinical ROC Analysis` | `Klinik ROC Analizi` | Başlık |
| `Outcome Variable` | `Outcome Variable` | Seçenek başlığı |
| `Positive Class` | `Positive Class` | Seçenek başlığı |
| `Predictor Variables` | `Predictor Variables` | Seçenek başlığı |
| `Analysis Type` | `Analysis Type` | Seçenek başlığı |
| `Direction` | `Direction` | Seçenek başlığı |
| `Youden index optimization` | `Youden index optimization` | Seçenek başlığı |
| `Custom Cutoffs` | `Custom Cutoffs` | Seçenek başlığı |
| `Minimum sensitivity` | `Minimum sensitivity` | Seçenek başlığı |
| `Minimum specificity` | `Minimum specificity` | Seçenek başlığı |
| `Confidence Level` | `Confidence Level` | Seçenek başlığı |
| `Bootstrap Samples` | `Bootstrap Samples` | Seçenek başlığı |
| `Use bootstrap` | `Use bootstrap` | Seçenek başlığı |
| `Bootstrap Method` | `Bootstrap Method` | Seçenek başlığı |
| `Bootstrap CI for optimal cutoff` | `Bootstrap CI for optimal cutoff` | Seçenek başlığı |
| `Bootstrap CI for partial AUC` | `Bootstrap CI for partial AUC` | Seçenek başlığı |
| `Stratified bootstrap` | `Stratified bootstrap` | Seçenek başlığı |
| `Random Seed` | `Random Seed` | Seçenek başlığı |
| `Pairwise comparisons` | `Pairwise comparisons` | Seçenek başlığı |
| `Comparison Method` | `Comparison Method` | Seçenek başlığı |

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

- [x] User-facing strings in `R/enhancedROC.b.R` properly wrapped in `.(...)`.
- [x] Schema files (`.a.yaml`, `.u.yaml`, `.r.yaml`) synchronized with backend.
- [x] Turkish terminology conforms to clinical and statistical guidelines.
- [x] Catalogs updated via `jmvtools::i18nUpdate()`.

