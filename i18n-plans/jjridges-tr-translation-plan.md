# Internationalization (i18n) Translation Plan: jjridges → Turkish (TR)

## 0) Argument Normalization

**SANITIZED_FN**: `jjridges`

**Target files analysis**:
- ✅ `jamovi/jjridges.a.yaml` (options)
- ✅ `jamovi/jjridges.u.yaml` (UI)
- ✅ `jamovi/jjridges.r.yaml` (results)
- ✅ `R/jjridges.b.R` (backend)

All required files are verified and present in the package codebase.

---

## 1) NAMESPACE i18n Hook Status

✅ **ALREADY CONFIGURED**: `NAMESPACE` contains `import(jmvcore)` and `importFrom(jmvcore, .)`, enabling runtime internationalization and extraction.

---

## 2) Translatable String Analysis

### 2.1 Current State Assessment
- ✅ All user-visible strings in `R/jjridges.b.R` and YAML definitions are wrapped in `.(...)` or declared in schemas.
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
| `Ridge Plot` | `Sırt (Ridge) Dağılım Grafiği` | Başlık |
| `X Variable (Distribution)` | `X Variable (Distribution)` | Seçenek başlığı |
| `Y Variable (Groups)` | `Y Variable (Groups)` | Seçenek başlığı |
| `Fill Variable (Optional)` | `Fill Variable (Optional)` | Seçenek başlığı |
| `Facet Variable (Optional)` | `Facet Variable (Optional)` | Seçenek başlığı |
| `Plot Type` | `Plot Type` | Seçenek başlığı |
| `Ridge Height Scale` | `Ridge Height Scale` | Seçenek başlığı |
| `Bandwidth Method` | `Bandwidth Method` | Seçenek başlığı |
| `Custom Bandwidth` | `Custom Bandwidth` | Seçenek başlığı |
| `Histogram Bin Width` | `Histogram Bin Width` | Seçenek başlığı |
| `Boxplot inside` | `Boxplot inside` | Seçenek başlığı |
| `Add data points` | `Add data points` | Seçenek başlığı |
| `Point Transparency` | `Point Transparency` | Seçenek başlığı |
| `Quantile lines` | `Quantile lines` | Seçenek başlığı |
| `Quantile Values` | `Quantile Values` | Seçenek başlığı |
| `Mean line` | `Mean line` | Seçenek başlığı |
| `Median line` | `Median line` | Seçenek başlığı |
| `Statistics` | `Statistics` | Seçenek başlığı |
| `Statistical Test` | `Statistical Test` | Seçenek başlığı |
| `P-value Adjustment` | `P-value Adjustment` | Seçenek başlığı |

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

- [x] User-facing strings in `R/jjridges.b.R` properly wrapped in `.(...)`.
- [x] Schema files (`.a.yaml`, `.u.yaml`, `.r.yaml`) synchronized with backend.
- [x] Turkish terminology conforms to clinical and statistical guidelines.
- [x] Catalogs updated via `jmvtools::i18nUpdate()`.

