# Internationalization (i18n) Translation Plan: jjbetweenstats → Turkish (TR)

## 0) Argument Normalization

**SANITIZED_FN**: `jjbetweenstats`

**Target files analysis**:
- ✅ `jamovi/jjbetweenstats.a.yaml` (options)
- ✅ `jamovi/jjbetweenstats.u.yaml` (UI)
- ✅ `jamovi/jjbetweenstats.r.yaml` (results)
- ✅ `R/jjbetweenstats.b.R` (backend)

All required files are verified and present in the package codebase.

---

## 1) NAMESPACE i18n Hook Status

✅ **ALREADY CONFIGURED**: `NAMESPACE` contains `import(jmvcore)` and `importFrom(jmvcore, .)`, enabling runtime internationalization and extraction.

---

## 2) Translatable String Analysis

### 2.1 Current State Assessment
- ✅ All user-visible strings in `R/jjbetweenstats.b.R` and YAML definitions are wrapped in `.(...)` or declared in schemas.
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
| `Box-Violin Plots to Compare Between Groups` | `Gruplar Arası Karşılaştırma için Kutu-Keman Grafikleri` | Başlık |
| `Dependent Variables` | `Dependent Variables` | Seçenek başlığı |
| `Grouping Variable` | `Grouping Variable` | Seçenek başlığı |
| `Split By (Optional)` | `Split By (Optional)` | Seçenek başlığı |
| `Centrality` | `Centrality` | Seçenek başlığı |
| `Centrality Type` | `Centrality Type` | Seçenek başlığı |
| `Type of Statistic` | `Type of Statistic` | Seçenek başlığı |
| `Pairwise comparisons` | `Pairwise comparisons` | Seçenek başlığı |
| `Pairwise Display` | `Pairwise Display` | Seçenek başlığı |
| `Adjustment Method` | `Adjustment Method` | Seçenek başlığı |
| `Effect Size Needed for Parametric Tests` | `Effect Size Needed for Parametric Tests` | Seçenek başlığı |
| `Title` | `Title` | Seçenek başlığı |
| `X-Title` | `X-Title` | Seçenek başlığı |
| `Y-Title` | `Y-Title` | Seçenek başlığı |
| `Add GGStatsPlot layer` | `Add GGStatsPlot layer` | Seçenek başlığı |
| `Statistical results` | `Statistical results` | Seçenek başlığı |
| `Bayes factor message` | `Bayes factor message` | Seçenek başlığı |
| `Decimal Places` | `Decimal Places` | Seçenek başlığı |
| `Confidence Level` | `Confidence Level` | Seçenek başlığı |
| `Equal variances` | `Equal variances` | Seçenek başlığı |

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

- [x] User-facing strings in `R/jjbetweenstats.b.R` properly wrapped in `.(...)`.
- [x] Schema files (`.a.yaml`, `.u.yaml`, `.r.yaml`) synchronized with backend.
- [x] Turkish terminology conforms to clinical and statistical guidelines.
- [x] Catalogs updated via `jmvtools::i18nUpdate()`.

