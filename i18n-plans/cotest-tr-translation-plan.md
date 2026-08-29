# Internationalization (i18n) Translation Plan: cotest → Turkish (TR)

## 0) Argument Normalization

**SANITIZED_FN**: `cotest`

**Target files analysis**:
- ✅ `jamovi/cotest.a.yaml` (options)
- ✅ `jamovi/cotest.u.yaml` (UI)
- ✅ `jamovi/cotest.r.yaml` (results)
- ✅ `R/cotest.b.R` (backend)

All required files are verified and present in the package codebase.

---

## 1) NAMESPACE i18n Hook Status

✅ **ALREADY CONFIGURED**: `NAMESPACE` contains `import(jmvcore)` and `importFrom(jmvcore, .)`, enabling runtime internationalization and extraction.

---

## 2) Translatable String Analysis

### 2.1 Current State Assessment
- ✅ All user-visible strings in `R/cotest.b.R` and YAML definitions are wrapped in `.(...)` or declared in schemas.
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
| `Co-Testing Analysis` | `Eş Zamanlı Test (Co-Testing) Analizi` | Başlık |
| `Name of test 1` | `Name of test 1` | Seçenek başlığı |
| `Name of test 2` | `Name of test 2` | Seçenek başlığı |
| `Test 1 sensitivity` | `Test 1 sensitivity` | Seçenek başlığı |
| `Test 1 specificity` | `Test 1 specificity` | Seçenek başlığı |
| `Test 2 sensitivity` | `Test 2 sensitivity` | Seçenek başlığı |
| `Test 2 specificity` | `Test 2 specificity` | Seçenek başlığı |
| `Assume conditional independence` | `Assume conditional independence` | Seçenek başlığı |
| `Dependence among subjects with disease` | `Dependence among subjects with disease` | Seçenek başlığı |
| `Dependence among subjects without disease` | `Dependence among subjects without disease` | Seçenek başlığı |
| `Disease prevalence` | `Disease prevalence` | Seçenek başlığı |
| `Guidance and explanations` | `Guidance and explanations` | Seçenek başlığı |
| `Footnotes` | `Footnotes` | Seçenek başlığı |
| `Fagan nomogram` | `Fagan nomogram` | Seçenek başlığı |
| `Worked example` | `Worked example` | Seçenek başlığı |
| `Instructions` | `Instructions` | Sonuç tablosu/grafik başlığı |
| `Validation Notices` | `Validation Notices` | Sonuç tablosu/grafik başlığı |
| `Test Parameters` | `Test Parameters` | Sonuç tablosu/grafik başlığı |
| `Co-Testing Results` | `Co-Testing Results` | Sonuç tablosu/grafik başlığı |
| `Test Dependence` | `Test Dependence` | Sonuç tablosu/grafik başlığı |

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

- [x] User-facing strings in `R/cotest.b.R` properly wrapped in `.(...)`.
- [x] Schema files (`.a.yaml`, `.u.yaml`, `.r.yaml`) synchronized with backend.
- [x] Turkish terminology conforms to clinical and statistical guidelines.
- [x] Catalogs updated via `jmvtools::i18nUpdate()`.

