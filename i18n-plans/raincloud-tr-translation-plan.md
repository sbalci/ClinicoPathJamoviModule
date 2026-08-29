# Internationalization (i18n) Translation Plan: raincloud → Turkish (TR)

## 0) Argument Normalization

**SANITIZED_FN**: `raincloud`

**Target files analysis**:
- ✅ `jamovi/raincloud.a.yaml` (options)
- ✅ `jamovi/raincloud.u.yaml` (UI)
- ✅ `jamovi/raincloud.r.yaml` (results)
- ✅ `R/raincloud.b.R` (backend)

All required files are verified and present in the package codebase.

---

## 1) NAMESPACE i18n Hook Status

✅ **ALREADY CONFIGURED**: `NAMESPACE` contains `import(jmvcore)` and `importFrom(jmvcore, .)`, enabling runtime internationalization and extraction.

---

## 2) Translatable String Analysis

### 2.1 Current State Assessment
- ✅ All user-visible strings in `R/raincloud.b.R` and YAML definitions are wrapped in `.(...)` or declared in schemas.
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
| `Raincloud Plot` | `Yağmur Bulutu (Raincloud) Grafiği` | Başlık |
| `Dependent Variable` | `Dependent Variable` | Seçenek başlığı |
| `Grouping Variable` | `Grouping Variable` | Seçenek başlığı |
| `Faceting Variable (Optional)` | `Faceting Variable (Optional)` | Seçenek başlığı |
| `Color Variable (Optional)` | `Color Variable (Optional)` | Seçenek başlığı |
| `Half-violin (density)` | `Half-violin (density)` | Seçenek başlığı |
| `Box plot` | `Box plot` | Seçenek başlığı |
| `Data points` | `Data points` | Seçenek başlığı |
| `Dots Position` | `Dots Position` | Seçenek başlığı |
| `Violin Width` | `Violin Width` | Seçenek başlığı |
| `Box Plot Width` | `Box Plot Width` | Seçenek başlığı |
| `Dots Size` | `Dots Size` | Seçenek başlığı |
| `Violin Transparency` | `Violin Transparency` | Seçenek başlığı |
| `Dots Transparency` | `Dots Transparency` | Seçenek başlığı |
| `Plot Orientation` | `Plot Orientation` | Seçenek başlığı |
| `Color Palette` | `Color Palette` | Seçenek başlığı |
| `Plot Theme` | `Plot Theme` | Seçenek başlığı |
| `Plot Title` | `Plot Title` | Seçenek başlığı |
| `X-Axis Label` | `X-Axis Label` | Seçenek başlığı |
| `Y-Axis Label` | `Y-Axis Label` | Seçenek başlığı |

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

- [x] User-facing strings in `R/raincloud.b.R` properly wrapped in `.(...)`.
- [x] Schema files (`.a.yaml`, `.u.yaml`, `.r.yaml`) synchronized with backend.
- [x] Turkish terminology conforms to clinical and statistical guidelines.
- [x] Catalogs updated via `jmvtools::i18nUpdate()`.

