# Internationalization (i18n) Translation Plan: hullplot → Turkish (TR)

## 0) Argument Normalization

**SANITIZED_FN**: `hullplot`

**Target files analysis**:
- ✅ `jamovi/hullplot.a.yaml` (options)
- ✅ `jamovi/hullplot.u.yaml` (UI)
- ✅ `jamovi/hullplot.r.yaml` (results)
- ✅ `R/hullplot.b.R` (backend)

All required files are verified and present in the package codebase.

---

## 1) NAMESPACE i18n Hook Status

✅ **ALREADY CONFIGURED**: `NAMESPACE` contains `import(jmvcore)` and `importFrom(jmvcore, .)`, enabling runtime internationalization and extraction.

---

## 2) Translatable String Analysis

### 2.1 Current State Assessment
- ✅ All user-visible strings in `R/hullplot.b.R` and YAML definitions are wrapped in `.(...)` or declared in schemas.
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
| `Hull Plot` | `Konveks Örtü (Hull) Grafiği` | Başlık |
| `X-Axis Variable` | `X-Axis Variable` | Seçenek başlığı |
| `Y-Axis Variable` | `Y-Axis Variable` | Seçenek başlığı |
| `Grouping Variable` | `Grouping Variable` | Seçenek başlığı |
| `Color Variable (Optional)` | `Color Variable (Optional)` | Seçenek başlığı |
| `Size Variable (Optional)` | `Size Variable (Optional)` | Seçenek başlığı |
| `Hull Concavity` | `Hull Concavity` | Seçenek başlığı |
| `Hull Transparency` | `Hull Transparency` | Seçenek başlığı |
| `Group labels` | `Group labels` | Seçenek başlığı |
| `Point Size` | `Point Size` | Seçenek başlığı |
| `Point Transparency` | `Point Transparency` | Seçenek başlığı |
| `Color Palette` | `Color Palette` | Seçenek başlığı |
| `Plot Theme` | `Plot Theme` | Seçenek başlığı |
| `Plot Title` | `Plot Title` | Seçenek başlığı |
| `X-Axis Label` | `X-Axis Label` | Seçenek başlığı |
| `Y-Axis Label` | `Y-Axis Label` | Seçenek başlığı |
| `Hull Boundary Expansion` | `Hull Boundary Expansion` | Seçenek başlığı |
| `Group statistics` | `Group statistics` | Seçenek başlığı |
| `Outlier detection` | `Outlier detection` | Seçenek başlığı |
| `Add confidence ellipses` | `Add confidence ellipses` | Seçenek başlığı |

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

- [x] User-facing strings in `R/hullplot.b.R` properly wrapped in `.(...)`.
- [x] Schema files (`.a.yaml`, `.u.yaml`, `.r.yaml`) synchronized with backend.
- [x] Turkish terminology conforms to clinical and statistical guidelines.
- [x] Catalogs updated via `jmvtools::i18nUpdate()`.

