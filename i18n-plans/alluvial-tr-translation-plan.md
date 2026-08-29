# Internationalization (i18n) Translation Plan: alluvial → Turkish (TR)

## 0) Argument Normalization

**SANITIZED_FN**: `alluvial`

**Target files analysis**:
- ✅ `jamovi/alluvial.a.yaml` (options)
- ✅ `jamovi/alluvial.u.yaml` (UI)
- ✅ `jamovi/alluvial.r.yaml` (results)
- ✅ `R/alluvial.b.R` (backend)

All required files are verified and present in the package codebase.

---

## 1) NAMESPACE i18n Hook Status

✅ **ALREADY CONFIGURED**: `NAMESPACE` contains `import(jmvcore)` and `importFrom(jmvcore, .)`, enabling runtime internationalization and extraction.

---

## 2) Translatable String Analysis

### 2.1 Current State Assessment
- ✅ All user-visible strings in `R/alluvial.b.R` and YAML definitions are wrapped in `.(...)` or declared in schemas.
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
| `Alluvial Diagrams` | `Alüviyal Diyagramlar` | Başlık |
| `Variables` | `Variables` | Seçenek başlığı |
| `Condensation variable` | `Condensation variable` | Seçenek başlığı |
| `Missing-value exclusion (NA)` | `Missing-value exclusion (NA)` | Seçenek başlığı |
| `Marginal plots` | `Marginal plots` | Seçenek başlığı |
| `Fill by` | `Fill by` | Seçenek başlığı |
| `Fill by (ggalluvial)` | `Fill by (ggalluvial)` | Seçenek başlığı |
| `Bin labels` | `Bin labels` | Seçenek başlığı |
| `Plot orientation` | `Plot orientation` | Seçenek başlığı |
| `Custom title` | `Custom title` | Seçenek başlığı |
| `Title` | `Title` | Seçenek başlığı |
| `Maximum variables` | `Maximum variables` | Seçenek başlığı |
| `Custom bin labels` | `Custom bin labels` | Seçenek başlığı |
| `Color palette` | `Color palette` | Seçenek başlığı |
| `Counts on nodes` | `Counts on nodes` | Seçenek başlığı |
| `Theme style` | `Theme style` | Seçenek başlığı |
| `Enhanced edge gradients` | `Enhanced edge gradients` | Seçenek başlığı |
| `Plot subtitle` | `Plot subtitle` | Seçenek başlığı |
| `Weight variable` | `Weight variable` | Seçenek başlığı |
| `Sankey styling` | `Sankey styling` | Seçenek başlığı |

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

- [x] User-facing strings in `R/alluvial.b.R` properly wrapped in `.(...)`.
- [x] Schema files (`.a.yaml`, `.u.yaml`, `.r.yaml`) synchronized with backend.
- [x] Turkish terminology conforms to clinical and statistical guidelines.
- [x] Catalogs updated via `jmvtools::i18nUpdate()`.

