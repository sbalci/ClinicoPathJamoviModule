# Internationalization (i18n) Translation Plan: jjwithinstats → Turkish (TR)

## 0) Argument Normalization

**SANITIZED_FN**: `jjwithinstats`

**Target files analysis**:
- ✅ `jamovi/jjwithinstats.a.yaml` (options)
- ✅ `jamovi/jjwithinstats.u.yaml` (UI)
- ✅ `jamovi/jjwithinstats.r.yaml` (results)
- ✅ `R/jjwithinstats.b.R` (backend)

All required files are verified and present in the package codebase.

---

## 1) NAMESPACE i18n Hook Status

✅ **ALREADY CONFIGURED**: `NAMESPACE` contains `import(jmvcore)` and `importFrom(jmvcore, .)`, enabling runtime internationalization and extraction.

---

## 2) Translatable String Analysis

### 2.1 Current State Assessment
- ✅ All user-visible strings in `R/jjwithinstats.b.R` and YAML definitions are wrapped in `.(...)` or declared in schemas.
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
| `Box-Violin Plots to Compare Within Groups` | `Grup İçi Karşılaştırma için Kutu-Keman Grafikleri` | Başlık |
| `First Measurement` | `First Measurement` | Seçenek başlığı |
| `Second Measurement` | `Second Measurement` | Seçenek başlığı |
| `Third Measurement (Optional)` | `Third Measurement (Optional)` | Seçenek başlığı |
| `Fourth Measurement (Optional)` | `Fourth Measurement (Optional)` | Seçenek başlığı |
| `Point path` | `Point path` | Seçenek başlığı |
| `Centrality path` | `Centrality path` | Seçenek başlığı |
| `Average values` | `Average values` | Seçenek başlığı |
| `Centrality Type` | `Centrality Type` | Seçenek başlığı |
| `Clinical Analysis Preset` | `Clinical Analysis Preset` | Seçenek başlığı |
| `Statistical Test Type` | `Statistical Test Type` | Seçenek başlığı |
| `Compare each time point pair` | `Compare each time point pair` | Seçenek başlığı |
| `Pairwise Display` | `Pairwise Display` | Seçenek başlığı |
| `Adjustment Method` | `Adjustment Method` | Seçenek başlığı |
| `Effect Size Needed for Parametric Tests` | `Effect Size Needed for Parametric Tests` | Seçenek başlığı |
| `Violin plot` | `Violin plot` | Seçenek başlığı |
| `Box plot` | `Box plot` | Seçenek başlığı |
| `Points` | `Points` | Seçenek başlığı |
| `Title` | `Title` | Seçenek başlığı |
| `X-Title` | `X-Title` | Seçenek başlığı |

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

- [x] User-facing strings in `R/jjwithinstats.b.R` properly wrapped in `.(...)`.
- [x] Schema files (`.a.yaml`, `.u.yaml`, `.r.yaml`) synchronized with backend.
- [x] Turkish terminology conforms to clinical and statistical guidelines.
- [x] Catalogs updated via `jmvtools::i18nUpdate()`.

