# Internationalization (i18n) Translation Plan: waterfall → Turkish (TR)

## 0) Argument Normalization

**SANITIZED_FN**: `waterfall`

**Target files analysis**:
- ✅ `jamovi/waterfall.a.yaml` (options)
- ✅ `jamovi/waterfall.u.yaml` (UI)
- ✅ `jamovi/waterfall.r.yaml` (results)
- ✅ `R/waterfall.b.R` (backend)

All required files are verified and present in the package codebase.

---

## 1) NAMESPACE i18n Hook Status

✅ **ALREADY CONFIGURED**: `NAMESPACE` contains `import(jmvcore)` and `importFrom(jmvcore, .)`, enabling runtime internationalization and extraction.

---

## 2) Translatable String Analysis

### 2.1 Current State Assessment
- ✅ All user-visible strings in `R/waterfall.b.R` and YAML definitions are wrapped in `.(...)` or declared in schemas.
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
| `Treatment Response: Patient-Level Burden` | `Tedavi Yanıtı: Hasta Düzeyinde Yük (Waterfall Grafiği)` | Başlık |
| `Patient ID Variable` | `Patient ID Variable` | Seçenek başlığı |
| `Response Value (Raw or Percentage)` | `Response Value (Raw or Percentage)` | Seçenek başlığı |
| `Time Variable (Required for Spider Plot)` | `Time Variable (Required for Spider Plot)` | Seçenek başlığı |
| `Group Variable` | `Group Variable` | Seçenek başlığı |
| `Data Input Type` | `Data Input Type` | Seçenek başlığı |
| `Sort By` | `Sort By` | Seçenek başlığı |
| `Sort Direction` | `Sort Direction` | Seçenek başlığı |
| `Baseline (Y = 0) line` | `Baseline (Y = 0) line` | Seçenek başlığı |
| `Confirmation Status (optional)` | `Confirmation Status (optional)` | Seçenek başlığı |
| `On-Treatment / Ongoing (optional)` | `On-Treatment / Ongoing (optional)` | Seçenek başlığı |
| `Response Category Override (optional)` | `Response Category Override (optional)` | Seçenek başlığı |
| `Response category above each bar` | `Response category above each bar` | Seçenek başlığı |
| `Patient ID labels on spider lines` | `Patient ID labels on spider lines` | Seçenek başlığı |
| `Annotation Tracks (below the bars)` | `Annotation Tracks (below the bars)` | Seçenek başlığı |
| `RECIST thresholds` | `RECIST thresholds` | Seçenek başlığı |
| `Label large changes` | `Label large changes` | Seçenek başlığı |
| `Median response` | `Median response` | Seçenek başlığı |
| `Confidence interval` | `Confidence interval` | Seçenek başlığı |
| `Minimum Response for Labels ( percent)` | `Minimum Response for Labels ( percent)` | Seçenek başlığı |

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

- [x] User-facing strings in `R/waterfall.b.R` properly wrapped in `.(...)`.
- [x] Schema files (`.a.yaml`, `.u.yaml`, `.r.yaml`) synchronized with backend.
- [x] Turkish terminology conforms to clinical and statistical guidelines.
- [x] Catalogs updated via `jmvtools::i18nUpdate()`.

