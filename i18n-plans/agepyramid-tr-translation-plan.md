# Internationalization (i18n) Translation Plan: agepyramid → Turkish (TR)

## 0) Argument Normalization

**SANITIZED_FN**: `agepyramid`

**Target files analysis**:
- ✅ `jamovi/agepyramid.a.yaml` (options)
- ✅ `jamovi/agepyramid.u.yaml` (UI)
- ✅ `jamovi/agepyramid.r.yaml` (results)
- ✅ `R/agepyramid.b.R` (backend)

All required files are verified and present in the package codebase.

---

## 1) NAMESPACE i18n Hook Status

✅ **ALREADY CONFIGURED**: `NAMESPACE` contains `import(jmvcore)` and `importFrom(jmvcore, .)`, enabling runtime internationalization and extraction.

---

## 2) Translatable String Analysis

### 2.1 Current State Assessment
- ✅ All user-visible strings in `R/agepyramid.b.R` and YAML definitions are wrapped in `.(...)` or declared in schemas.
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
| `Age Pyramid` | `Yaş Piramidi` | Başlık |
| `Age` | `Age` | Seçenek başlığı |
| `Gender` | `Gender` | Seçenek başlığı |
| `Female level` | `Female level` | Seçenek başlığı |
| `Male level` | `Male level` | Seçenek başlığı |
| `Age group preset` | `Age group preset` | Seçenek başlığı |
| `Age band boundaries` | `Age band boundaries` | Seçenek başlığı |
| `Bin width (years)` | `Bin width (years)` | Seçenek başlığı |
| `Custom age breaks` | `Custom age breaks` | Seçenek başlığı |
| `Plot title` | `Plot title` | Seçenek başlığı |
| `Color palette` | `Color palette` | Seçenek başlığı |
| `Female color` | `Female color` | Seçenek başlığı |
| `Male color` | `Male color` | Seçenek başlığı |
| `Original custom theme` | `Original custom theme` | Seçenek başlığı |
| `ggcharts pyramid` | `ggcharts pyramid` | Seçenek başlığı |
| `Bar order` | `Bar order` | Seçenek başlığı |
| `Bar colors` | `Bar colors` | Seçenek başlığı |
| `First group color` | `First group color` | Seçenek başlığı |
| `Second group color` | `Second group color` | Seçenek başlığı |
| `ggcharts plot title` | `ggcharts plot title` | Seçenek başlığı |

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

- [x] User-facing strings in `R/agepyramid.b.R` properly wrapped in `.(...)`.
- [x] Schema files (`.a.yaml`, `.u.yaml`, `.r.yaml`) synchronized with backend.
- [x] Turkish terminology conforms to clinical and statistical guidelines.
- [x] Catalogs updated via `jmvtools::i18nUpdate()`.

