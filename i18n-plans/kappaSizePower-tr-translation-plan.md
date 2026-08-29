# Internationalization (i18n) Translation Plan: kappaSizePower → Turkish (TR)

## 0) Argument Normalization

**SANITIZED_FN**: `kappaSizePower`

**Target files analysis**:
- ✅ `jamovi/kappaSizePower.a.yaml` (options)
- ✅ `jamovi/kappaSizePower.u.yaml` (UI)
- ✅ `jamovi/kappaSizePower.r.yaml` (results)
- ✅ `R/kappaSizePower.b.R` (backend)

All required files are verified and present in the package codebase.

---

## 1) NAMESPACE i18n Hook Status

✅ **ALREADY CONFIGURED**: `NAMESPACE` contains `import(jmvcore)` and `importFrom(jmvcore, .)`, enabling runtime internationalization and extraction.

---

## 2) Translatable String Analysis

### 2.1 Current State Assessment
- ✅ All user-visible strings in `R/kappaSizePower.b.R` and YAML definitions are wrapped in `.(...)` or declared in schemas.
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
| `Power Approach for the Number of Subjects Required` | `Gerekli Örneklem Boyutu için Güç Yaklaşımı` | Başlık |
| `Number of outcome levels` | `Number of outcome levels` | Seçenek başlığı |
| `kappa0` | `kappa0` | Seçenek başlığı |
| `kappa1` | `kappa1` | Seçenek başlığı |
| `Expected proportion of cases in each category` | `Expected proportion of cases in each category` | Seçenek başlığı |
| `raters` | `raters` | Seçenek başlığı |
| `alpha` | `alpha` | Seçenek başlığı |
| `power` | `power` | Seçenek başlığı |
| `Notes` | `Notes` | Sonuç tablosu/grafik başlığı |
| `Analysis result` | `Analysis result` | Sonuç tablosu/grafik başlığı |
| `Summary` | `Summary` | Sonuç tablosu/grafik başlığı |
| `Study Explanation` | `Study Explanation` | Sonuç tablosu/grafik başlığı |

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

- [x] User-facing strings in `R/kappaSizePower.b.R` properly wrapped in `.(...)`.
- [x] Schema files (`.a.yaml`, `.u.yaml`, `.r.yaml`) synchronized with backend.
- [x] Turkish terminology conforms to clinical and statistical guidelines.
- [x] Catalogs updated via `jmvtools::i18nUpdate()`.

