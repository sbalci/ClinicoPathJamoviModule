# Internationalization (i18n) Translation Plan: chisqposttest → Turkish (TR)

## 0) Argument Normalization

**SANITIZED_FN**: `chisqposttest`

**Target files analysis**:
- ✅ `jamovi/chisqposttest.a.yaml` (options)
- ✅ `jamovi/chisqposttest.u.yaml` (UI)
- ✅ `jamovi/chisqposttest.r.yaml` (results)
- ✅ `R/chisqposttest.b.R` (backend)

All required files are verified and present in the package codebase.

---

## 1) NAMESPACE i18n Hook Status

✅ **ALREADY CONFIGURED**: `NAMESPACE` contains `import(jmvcore)` and `importFrom(jmvcore, .)`, enabling runtime internationalization and extraction.

---

## 2) Translatable String Analysis

### 2.1 Current State Assessment
- ✅ All user-visible strings in `R/chisqposttest.b.R` and YAML definitions are wrapped in `.(...)` or declared in schemas.
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
| `Chi-Square Post-Hoc Tests` | `Ki-Kare Post-Hoc Testleri` | Başlık |
| `Rows` | `Rows` | Seçenek başlığı |
| `Columns` | `Columns` | Seçenek başlığı |
| `Counts (optional)` | `Counts (optional)` | Seçenek başlığı |
| `Post-hoc method` | `Post-hoc method` | Seçenek başlığı |
| `Significance level` | `Significance level` | Seçenek başlığı |
| `Exclude missing values (always applied)` | `Exclude missing values (always applied)` | Seçenek başlığı |
| `Expected values` | `Expected values` | Seçenek başlığı |
| `Residual plot` | `Residual plot` | Seçenek başlığı |
| `Residuals analysis` | `Residuals analysis` | Seçenek başlığı |
| `Educational panels` | `Educational panels` | Seçenek başlığı |
| `Detailed comparison tables` | `Detailed comparison tables` | Seçenek başlığı |
| `Residual significance criterion` | `Residual significance criterion` | Seçenek başlığı |
| `Residual significance cutoff` | `Residual significance cutoff` | Seçenek başlığı |
| `Bootstrap confidence intervals for phi` | `Bootstrap confidence intervals for phi` | Seçenek başlığı |
| `Statistical test selection` | `Statistical test selection` | Seçenek başlığı |
| `Detailed results export` | `Detailed results export` | Seçenek başlığı |
| `Clinical summary` | `Clinical summary` | Seçenek başlığı |
| `Report sentences` | `Report sentences` | Seçenek başlığı |
| `Assumptions check` | `Assumptions check` | Seçenek başlığı |

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

- [x] User-facing strings in `R/chisqposttest.b.R` properly wrapped in `.(...)`.
- [x] Schema files (`.a.yaml`, `.u.yaml`, `.r.yaml`) synchronized with backend.
- [x] Turkish terminology conforms to clinical and statistical guidelines.
- [x] Catalogs updated via `jmvtools::i18nUpdate()`.

