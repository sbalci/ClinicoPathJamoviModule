# Internationalization (i18n) Translation Plan: jjdotchart → Turkish (TR)

## 0) Argument Normalization

**SANITIZED_FN**: `jjdotchart`

**Target files analysis**:
- ✅ `jamovi/jjdotchart.a.yaml` (options)
- ✅ `jamovi/jjdotchart.u.yaml` (UI)
- ✅ `jamovi/jjdotchart.r.yaml` (results)
- ✅ `R/jjdotchart.b.R` (backend)

All required files are verified and present in the package codebase.

---

## 1) NAMESPACE i18n Hook Status

✅ **ALREADY CONFIGURED**: `NAMESPACE` contains `import(jmvcore)` and `importFrom(jmvcore, .)`, enabling runtime internationalization and extraction.

---

## 2) Translatable String Analysis

### 2.1 Current State Assessment
- ✅ All user-visible strings in `R/jjdotchart.b.R` and YAML definitions are wrapped in `.(...)` or declared in schemas.
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
| `Dot Chart (Summary vs Reference Value)` | `Nokta Grafiği (Özet vs Referans Değer)` | Başlık |
| `Measurement` | `Measurement` | Seçenek başlığı |
| `Groups (one point each)` | `Groups (one point each)` | Seçenek başlığı |
| `Split By (Optional)` | `Split By (Optional)` | Seçenek başlığı |
| `Reference Value` | `Reference Value` | Seçenek başlığı |
| `Statistical Test` | `Statistical Test` | Seçenek başlığı |
| `Confidence Level` | `Confidence Level` | Seçenek başlığı |
| `Decimal Places` | `Decimal Places` | Seçenek başlığı |
| `Statistical results in plot` | `Statistical results in plot` | Seçenek başlığı |
| `Group summary table` | `Group summary table` | Seçenek başlığı |
| `Also mark the centre of the plotted points` | `Also mark the centre of the plotted points` | Seçenek başlığı |
| `Central Tendency Measure` | `Central Tendency Measure` | Seçenek başlığı |
| `Bayes factor interpretation` | `Bayes factor interpretation` | Seçenek başlığı |
| `Original ggstatsplot theme` | `Original ggstatsplot theme` | Seçenek başlığı |
| `Plot Title` | `Plot Title` | Seçenek başlığı |
| `X-axis Label (Measurement)` | `X-axis Label (Measurement)` | Seçenek başlığı |
| `Y-axis Label (Groups)` | `Y-axis Label (Groups)` | Seçenek başlığı |
| `Plot Width (pixels)` | `Plot Width (pixels)` | Seçenek başlığı |
| `Plot Height (pixels)` | `Plot Height (pixels)` | Seçenek başlığı |
| `To Do` | `To Do` | Sonuç tablosu/grafik başlığı |

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

- [x] User-facing strings in `R/jjdotchart.b.R` properly wrapped in `.(...)`.
- [x] Schema files (`.a.yaml`, `.u.yaml`, `.r.yaml`) synchronized with backend.
- [x] Turkish terminology conforms to clinical and statistical guidelines.
- [x] Catalogs updated via `jmvtools::i18nUpdate()`.

