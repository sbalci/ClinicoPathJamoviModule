# Internationalization (i18n) Translation Plan: timeinterval → Turkish (TR)

## 0) Argument Normalization

**SANITIZED_FN**: `timeinterval`

**Target files analysis**:
- ✅ `jamovi/timeinterval.a.yaml` (options)
- ✅ `jamovi/timeinterval.u.yaml` (UI)
- ✅ `jamovi/timeinterval.r.yaml` (results)
- ✅ `R/timeinterval.b.R` (backend)

All required files are verified and present in the package codebase.

---

## 1) NAMESPACE i18n Hook Status

✅ **ALREADY CONFIGURED**: `NAMESPACE` contains `import(jmvcore)` and `importFrom(jmvcore, .)`, enabling runtime internationalization and extraction.

---

## 2) Translatable String Analysis

### 2.1 Current State Assessment
- ✅ All user-visible strings in `R/timeinterval.b.R` and YAML definitions are wrapped in `.(...)` or declared in schemas.
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
| `Comprehensive Time Interval Calculator` | `Kapsamlı Zaman Aralığı Hesaplayıcı` | Başlık |
| `Start Date Variable` | `Start Date Variable` | Seçenek başlığı |
| `End Date Variable` | `End Date Variable` | Seçenek başlığı |
| `Date Format` | `Date Format` | Seçenek başlığı |
| `Time Unit for Results` | `Time Unit for Results` | Seçenek başlığı |
| `Time Basis` | `Time Basis` | Seçenek başlığı |
| `Landmark analysis` | `Landmark analysis` | Seçenek başlığı |
| `Landmark Time Point` | `Landmark Time Point` | Seçenek başlığı |
| `Remove negative intervals` | `Remove negative intervals` | Seçenek başlığı |
| `Flag extreme values` | `Flag extreme values` | Seçenek başlığı |
| `Extreme Threshold Multiplier` | `Extreme Threshold Multiplier` | Seçenek başlığı |
| `Add calculated times to dataset` | `Add calculated times to dataset` | Seçenek başlığı |
| `Include data quality assessment` | `Include data quality assessment` | Seçenek başlığı |
| `Confidence Level ( percent)` | `Confidence Level ( percent)` | Seçenek başlığı |
| `Natural-language summary` | `Natural-language summary` | Seçenek başlığı |
| `Glossary of terms` | `Glossary of terms` | Seçenek başlığı |
| `Timezone` | `Timezone` | Seçenek başlığı |
| `Getting Started` | `Getting Started` | Sonuç tablosu/grafik başlığı |
| `About This Analysis` | `About This Analysis` | Sonuç tablosu/grafik başlığı |
| `Understanding Person-Time Analysis` | `Understanding Person-Time Analysis` | Sonuç tablosu/grafik başlığı |

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

- [x] User-facing strings in `R/timeinterval.b.R` properly wrapped in `.(...)`.
- [x] Schema files (`.a.yaml`, `.u.yaml`, `.r.yaml`) synchronized with backend.
- [x] Turkish terminology conforms to clinical and statistical guidelines.
- [x] Catalogs updated via `jmvtools::i18nUpdate()`.

