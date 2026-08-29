# Internationalization (i18n) Translation Plan: statsplot2 → Turkish (TR)

## 0) Argument Normalization

**SANITIZED_FN**: `statsplot2`

**Target files analysis**:
- ✅ `jamovi/statsplot2.a.yaml` (options)
- ✅ `jamovi/statsplot2.u.yaml` (UI)
- ✅ `jamovi/statsplot2.r.yaml` (results)
- ✅ `R/statsplot2.b.R` (backend)

All required files are verified and present in the package codebase.

---

## 1) NAMESPACE i18n Hook Status

✅ **ALREADY CONFIGURED**: `NAMESPACE` contains `import(jmvcore)` and `importFrom(jmvcore, .)`, enabling runtime internationalization and extraction.

---

## 2) Translatable String Analysis

### 2.1 Current State Assessment
- ✅ All user-visible strings in `R/statsplot2.b.R` and YAML definitions are wrapped in `.(...)` or declared in schemas.
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
| `Automatic Plot Selection` | `Otomatik Grafik Seçimi` | Başlık |
| `Outcome - Dependent Variable (y-axis)` | `Outcome - Dependent Variable (y-axis)` | Seçenek başlığı |
| `Comparison Groups (x-axis)` | `Comparison Groups (x-axis)` | Seçenek başlığı |
| `Split By (Optional)` | `Split By (Optional)` | Seçenek başlığı |
| `Study Design` | `Study Design` | Seçenek başlığı |
| `Statistical Approach` | `Statistical Approach` | Seçenek başlığı |
| `Alluvial Plot Style` | `Alluvial Plot Style` | Seçenek başlığı |
| `Exclude missing values` | `Exclude missing values` | Seçenek başlığı |
| `Sample large datasets` | `Sample large datasets` | Seçenek başlığı |
| `Sample Above (rows)` | `Sample Above (rows)` | Seçenek başlığı |
| `Rows To Keep` | `Rows To Keep` | Seçenek başlığı |
| `Random Seed` | `Random Seed` | Seçenek başlığı |
| `Important Information` | `Important Information` | Sonuç tablosu/grafik başlığı |
| `To Do` | `To Do` | Sonuç tablosu/grafik başlığı |
| `Explanation` | `Explanation` | Sonuç tablosu/grafik başlığı |
| `Automatically Selected Plot` | `Automatically Selected Plot` | Sonuç tablosu/grafik başlığı |

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

- [x] User-facing strings in `R/statsplot2.b.R` properly wrapped in `.(...)`.
- [x] Schema files (`.a.yaml`, `.u.yaml`, `.r.yaml`) synchronized with backend.
- [x] Turkish terminology conforms to clinical and statistical guidelines.
- [x] Catalogs updated via `jmvtools::i18nUpdate()`.

