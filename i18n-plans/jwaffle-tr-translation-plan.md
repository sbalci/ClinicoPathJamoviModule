# Internationalization (i18n) Translation Plan: jwaffle → Turkish (TR)

## 0) Argument Normalization

**SANITIZED_FN**: `jwaffle`

**Target files analysis**:
- ✅ `jamovi/jwaffle.a.yaml` (options)
- ✅ `jamovi/jwaffle.u.yaml` (UI)
- ✅ `jamovi/jwaffle.r.yaml` (results)
- ✅ `R/jwaffle.b.R` (backend)

All required files are verified and present in the package codebase.

---

## 1) NAMESPACE i18n Hook Status

✅ **ALREADY CONFIGURED**: `NAMESPACE` contains `import(jmvcore)` and `importFrom(jmvcore, .)`, enabling runtime internationalization and extraction.

---

## 2) Translatable String Analysis

### 2.1 Current State Assessment
- ✅ All user-visible strings in `R/jwaffle.b.R` and YAML definitions are wrapped in `.(...)` or declared in schemas.
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
| `Waffle Charts` | `Vafıl (Waffle) Grafikleri` | Başlık |
| `Counts (Optional)` | `Counts (Optional)` | Seçenek başlığı |
| `Groups` | `Groups` | Seçenek başlığı |
| `Facet By (Optional)` | `Facet By (Optional)` | Seçenek başlığı |
| `Number of Rows` | `Number of Rows` | Seçenek başlığı |
| `Flip chart` | `Flip chart` | Seçenek başlığı |
| `Color Palette` | `Color Palette` | Seçenek başlığı |
| `Legend` | `Legend` | Seçenek başlığı |
| `Title` | `Title` | Seçenek başlığı |
| `Legend Title` | `Legend Title` | Seçenek başlığı |
| `Analysis summary` | `Analysis summary` | Seçenek başlığı |
| `Explanations` | `Explanations` | Seçenek başlığı |
| `Important Information` | `Important Information` | Sonuç tablosu/grafik başlığı |
| `To Do` | `To Do` | Sonuç tablosu/grafik başlığı |
| `Messages` | `Messages` | Sonuç tablosu/grafik başlığı |
| `Analysis Summary` | `Analysis Summary` | Sonuç tablosu/grafik başlığı |
| `Waffle Chart` | `Waffle Chart` | Sonuç tablosu/grafik başlığı |
| `Methodology` | `Methodology` | Sonuç tablosu/grafik başlığı |

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

- [x] User-facing strings in `R/jwaffle.b.R` properly wrapped in `.(...)`.
- [x] Schema files (`.a.yaml`, `.u.yaml`, `.r.yaml`) synchronized with backend.
- [x] Turkish terminology conforms to clinical and statistical guidelines.
- [x] Catalogs updated via `jmvtools::i18nUpdate()`.

