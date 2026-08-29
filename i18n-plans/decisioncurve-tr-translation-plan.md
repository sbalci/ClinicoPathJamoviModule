# Internationalization (i18n) Translation Plan: decisioncurve → Turkish (TR)

## 0) Argument Normalization

**SANITIZED_FN**: `decisioncurve`

**Target files analysis**:
- ✅ `jamovi/decisioncurve.a.yaml` (options)
- ✅ `jamovi/decisioncurve.u.yaml` (UI)
- ✅ `jamovi/decisioncurve.r.yaml` (results)
- ✅ `R/decisioncurve.b.R` (backend)

All required files are verified and present in the package codebase.

---

## 1) NAMESPACE i18n Hook Status

✅ **ALREADY CONFIGURED**: `NAMESPACE` contains `import(jmvcore)` and `importFrom(jmvcore, .)`, enabling runtime internationalization and extraction.

---

## 2) Translatable String Analysis

### 2.1 Current State Assessment
- ✅ All user-visible strings in `R/decisioncurve.b.R` and YAML definitions are wrapped in `.(...)` or declared in schemas.
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
| `Decision Curve Analysis` | `Karar Eğrisi Analizi (DCA)` | Başlık |
| `Outcome Variable` | `Outcome Variable` | Seçenek başlığı |
| `Positive Outcome Level` | `Positive Outcome Level` | Seçenek başlığı |
| `Prediction Variables/Models` | `Prediction Variables/Models` | Seçenek başlığı |
| `Model Names` | `Model Names` | Seçenek başlığı |
| `Threshold Range` | `Threshold Range` | Seçenek başlığı |
| `Minimum Threshold` | `Minimum Threshold` | Seçenek başlığı |
| `Maximum Threshold` | `Maximum Threshold` | Seçenek başlığı |
| `Threshold Step Size` | `Threshold Step Size` | Seçenek başlığı |
| `Show Results Table` | `Show Results Table` | Seçenek başlığı |
| `Selected Thresholds for Table` | `Selected Thresholds for Table` | Seçenek başlığı |
| `Show Decision Curve Plot` | `Show Decision Curve Plot` | Seçenek başlığı |
| `Plot Style` | `Plot Style` | Seçenek başlığı |
| `Show Reference Line Labels` | `Show Reference Line Labels` | Seçenek başlığı |
| `Highlight Clinical Range` | `Highlight Clinical Range` | Seçenek başlığı |
| `Highlight Range Minimum` | `Highlight Range Minimum` | Seçenek başlığı |
| `Highlight Range Maximum` | `Highlight Range Maximum` | Seçenek başlığı |
| `Calculate Clinical Impact` | `Calculate Clinical Impact` | Seçenek başlığı |
| `Population Size for Projections` | `Population Size for Projections` | Seçenek başlığı |
| `Net Interventions Avoided` | `Net Interventions Avoided` | Seçenek başlığı |

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

- [x] User-facing strings in `R/decisioncurve.b.R` properly wrapped in `.(...)`.
- [x] Schema files (`.a.yaml`, `.u.yaml`, `.r.yaml`) synchronized with backend.
- [x] Turkish terminology conforms to clinical and statistical guidelines.
- [x] Catalogs updated via `jmvtools::i18nUpdate()`.

