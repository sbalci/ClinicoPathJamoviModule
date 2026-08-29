# Internationalization (i18n) Translation Plan: ihcheterogeneity → Turkish (TR)

## 0) Argument Normalization

**SANITIZED_FN**: `ihcheterogeneity`

**Target files analysis**:
- ✅ `jamovi/ihcheterogeneity.a.yaml` (options)
- ✅ `jamovi/ihcheterogeneity.u.yaml` (UI)
- ✅ `jamovi/ihcheterogeneity.r.yaml` (results)
- ✅ `R/ihcheterogeneity.b.R` (backend)

All required files are verified and present in the package codebase.

---

## 1) NAMESPACE i18n Hook Status

✅ **ALREADY CONFIGURED**: `NAMESPACE` contains `import(jmvcore)` and `importFrom(jmvcore, .)`, enabling runtime internationalization and extraction.

---

## 2) Translatable String Analysis

### 2.1 Current State Assessment
- ✅ All user-visible strings in `R/ihcheterogeneity.b.R` and YAML definitions are wrapped in `.(...)` or declared in schemas.
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
| `IHC Heterogeneity Analysis` | `İHK Heterojenite Analizi` | Başlık |
| `Overall / Whole Slide / HotSpot (Optional)` | `Overall / Whole Slide / HotSpot (Optional)` | Seçenek başlığı |
| `Regional Measurement 1 (Required)` | `Regional Measurement 1 (Required)` | Seçenek başlığı |
| `Regional Measurement 2 (Optional)` | `Regional Measurement 2 (Optional)` | Seçenek başlığı |
| `Regional Measurement 3 (Optional)` | `Regional Measurement 3 (Optional)` | Seçenek başlığı |
| `Regional Measurement 4 (Optional)` | `Regional Measurement 4 (Optional)` | Seçenek başlığı |
| `Additional Regional Measurements` | `Additional Regional Measurements` | Seçenek başlığı |
| `Spatial Region ID (Optional)` | `Spatial Region ID (Optional)` | Seçenek başlığı |
| `Spatial compartment comparison` | `Spatial compartment comparison` | Seçenek başlığı |
| `Compartment comparison tests` | `Compartment comparison tests` | Seçenek başlığı |
| `Analysis Focus` | `Analysis Focus` | Seçenek başlığı |
| `Sampling Strategy` | `Sampling Strategy` | Seçenek başlığı |
| `CV Threshold for Acceptable Variability` | `CV Threshold for Acceptable Variability` | Seçenek başlığı |
| `Minimum Acceptable Correlation` | `Minimum Acceptable Correlation` | Seçenek başlığı |
| `Variability plots` | `Variability plots` | Seçenek başlığı |
| `Variance component analysis` | `Variance component analysis` | Seçenek başlığı |
| `Power analysis` | `Power analysis` | Seçenek başlığı |
| `Clinical recommendations` | `Clinical recommendations` | Seçenek başlığı |
| `Plain-language summary` | `Plain-language summary` | Seçenek başlığı |
| `Statistical glossary` | `Statistical glossary` | Seçenek başlığı |

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

- [x] User-facing strings in `R/ihcheterogeneity.b.R` properly wrapped in `.(...)`.
- [x] Schema files (`.a.yaml`, `.u.yaml`, `.r.yaml`) synchronized with backend.
- [x] Turkish terminology conforms to clinical and statistical guidelines.
- [x] Catalogs updated via `jmvtools::i18nUpdate()`.

