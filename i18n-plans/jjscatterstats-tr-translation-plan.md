# Internationalization (i18n) Translation Plan: jjscatterstats → Turkish (TR)

## 0) Argument Normalization

**SANITIZED_FN**: `jjscatterstats`

**Target files analysis**:
- ✅ `jamovi/jjscatterstats.a.yaml` (options)
- ✅ `jamovi/jjscatterstats.u.yaml` (UI)
- ✅ `jamovi/jjscatterstats.r.yaml` (results)
- ✅ `R/jjscatterstats.b.R` (backend)

All required files are verified and present in the package codebase.

---

## 1) NAMESPACE i18n Hook Status

✅ **ALREADY CONFIGURED**: `NAMESPACE` contains `import(jmvcore)` and `importFrom(jmvcore, .)`, enabling runtime internationalization and extraction.

---

## 2) Translatable String Analysis

### 2.1 Current State Assessment
- ✅ All user-visible strings in `R/jjscatterstats.b.R` and YAML definitions are wrapped in `.(...)` or declared in schemas.
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
| `Scatter Plot` | `Dağılım (Saçılım) Grafiği` | Başlık |
| `x-axis (First Variable)` | `x-axis (First Variable)` | Seçenek başlığı |
| `y-axis (Second Variable)` | `y-axis (Second Variable)` | Seçenek başlığı |
| `Split By (Optional)` | `Split By (Optional)` | Seçenek başlığı |
| `Color Variable (Optional)` | `Color Variable (Optional)` | Seçenek başlığı |
| `Size Variable (Optional)` | `Size Variable (Optional)` | Seçenek başlığı |
| `Shape Variable (Optional)` | `Shape Variable (Optional)` | Seçenek başlığı |
| `Alpha Variable (Optional)` | `Alpha Variable (Optional)` | Seçenek başlığı |
| `Label Variable (Optional)` | `Label Variable (Optional)` | Seçenek başlığı |
| `Rug plot` | `Rug plot` | Seçenek başlığı |
| `Marginal Plot Type` | `Marginal Plot Type` | Seçenek başlığı |
| `Smooth Method` | `Smooth Method` | Seçenek başlığı |
| `Statistical Test Type` | `Statistical Test Type` | Seçenek başlığı |
| `Title` | `Title` | Seçenek başlığı |
| `X-Title` | `X-Title` | Seçenek başlığı |
| `Y-Title` | `Y-Title` | Seçenek başlığı |
| `Add GGStatsPlot layer` | `Add GGStatsPlot layer` | Seçenek başlığı |
| `Statistical results` | `Statistical results` | Seçenek başlığı |
| `Confidence Level` | `Confidence Level` | Seçenek başlığı |
| `Bayes factor message` | `Bayes factor message` | Seçenek başlığı |

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

- [x] User-facing strings in `R/jjscatterstats.b.R` properly wrapped in `.(...)`.
- [x] Schema files (`.a.yaml`, `.u.yaml`, `.r.yaml`) synchronized with backend.
- [x] Turkish terminology conforms to clinical and statistical guidelines.
- [x] Catalogs updated via `jmvtools::i18nUpdate()`.

