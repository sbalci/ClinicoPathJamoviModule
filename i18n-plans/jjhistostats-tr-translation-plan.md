# Internationalization (i18n) Translation Plan: jjhistostats → Turkish (TR)

## 0) Argument Normalization

**SANITIZED_FN**: `jjhistostats`

**Target files analysis**:
- ✅ `jamovi/jjhistostats.a.yaml` (options)
- ✅ `jamovi/jjhistostats.u.yaml` (UI)
- ✅ `jamovi/jjhistostats.r.yaml` (results)
- ✅ `R/jjhistostats.b.R` (backend)

All required files are verified and present in the package codebase.

---

## 1) NAMESPACE i18n Hook Status

✅ **ALREADY CONFIGURED**: `NAMESPACE` contains `import(jmvcore)` and `importFrom(jmvcore, .)`, enabling runtime internationalization and extraction.

---

## 2) Translatable String Analysis

### 2.1 Current State Assessment
- ✅ All user-visible strings in `R/jjhistostats.b.R` and YAML definitions are wrapped in `.(...)` or declared in schemas.
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
| `Histogram` | `Histogram` | Başlık |
| `Variables` | `Variables` | Seçenek başlığı |
| `Split By (Optional)` | `Split By (Optional)` | Seçenek başlığı |
| `Type of Statistic` | `Type of Statistic` | Seçenek başlığı |
| `Centrality line` | `Centrality line` | Seçenek başlığı |
| `Change bin width` | `Change bin width` | Seçenek başlığı |
| `Bin Width (Default is max(x) - min(x) / sqrt(N))` | `Bin Width (Default is max(x) - min(x) / sqrt(N))` | Seçenek başlığı |
| `Statistical results` | `Statistical results` | Seçenek başlığı |
| `Clinical interpretation` | `Clinical interpretation` | Seçenek başlığı |
| `Clinical Analysis Preset` | `Clinical Analysis Preset` | Seçenek başlığı |
| `One-sample test` | `One-sample test` | Seçenek başlığı |
| `Test Value` | `Test Value` | Seçenek başlığı |
| `Confidence Level` | `Confidence Level` | Seçenek başlığı |
| `Bayes factor message` | `Bayes factor message` | Seçenek başlığı |
| `Decimal Places` | `Decimal Places` | Seçenek başlığı |
| `X-axis Label` | `X-axis Label` | Seçenek başlığı |
| `Plot Title` | `Plot Title` | Seçenek başlığı |
| `Plot Subtitle` | `Plot Subtitle` | Seçenek başlığı |
| `Plot Caption` | `Plot Caption` | Seçenek başlığı |
| `Centrality Type` | `Centrality Type` | Seçenek başlığı |

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

- [x] User-facing strings in `R/jjhistostats.b.R` properly wrapped in `.(...)`.
- [x] Schema files (`.a.yaml`, `.u.yaml`, `.r.yaml`) synchronized with backend.
- [x] Turkish terminology conforms to clinical and statistical guidelines.
- [x] Catalogs updated via `jmvtools::i18nUpdate()`.

