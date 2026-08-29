# Internationalization (i18n) Translation Plan: psychopdaROC → Turkish (TR)

## 0) Argument Normalization

**SANITIZED_FN**: `psychopdaROC`

**Target files analysis**:
- ✅ `jamovi/psychopdaROC.a.yaml` (options)
- ✅ `jamovi/psychopdaROC.u.yaml` (UI)
- ✅ `jamovi/psychopdaROC.r.yaml` (results)
- ✅ `R/psychopdaROC.b.R` (backend)

All required files are verified and present in the package codebase.

---

## 1) NAMESPACE i18n Hook Status

✅ **ALREADY CONFIGURED**: `NAMESPACE` contains `import(jmvcore)` and `importFrom(jmvcore, .)`, enabling runtime internationalization and extraction.

---

## 2) Translatable String Analysis

### 2.1 Current State Assessment
- ✅ All user-visible strings in `R/psychopdaROC.b.R` and YAML definitions are wrapped in `.(...)` or declared in schemas.
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
| `Advanced ROC Analysis` | `İleri ROC Analizi` | Başlık |
| `Run manually` | `Run manually` | Seçenek başlığı |
| `Run` | `Run` | Seçenek başlığı |
| `Analysis Level` | `Analysis Level` | Seçenek başlığı |
| `Test Variables` | `Test Variables` | Seçenek başlığı |
| `Class Variable (Gold Standard)` | `Class Variable (Gold Standard)` | Seçenek başlığı |
| `Positive Class` | `Positive Class` | Seçenek başlığı |
| `Subgroup Variable (Optional)` | `Subgroup Variable (Optional)` | Seçenek başlığı |
| `Cutpoint Method` | `Cutpoint Method` | Seçenek başlığı |
| `Optimization Metric` | `Optimization Metric` | Seçenek başlığı |
| `Classification Direction` | `Classification Direction` | Seçenek başlığı |
| `Manual Cutpoint Value` | `Manual Cutpoint Value` | Seçenek başlığı |
| `Metric Tolerance` | `Metric Tolerance` | Seçenek başlığı |
| `Tie Breaking Method` | `Tie Breaking Method` | Seçenek başlığı |
| `All observed cutpoints` | `All observed cutpoints` | Seçenek başlığı |
| `Bootstrap Iterations` | `Bootstrap Iterations` | Seçenek başlığı |
| `Random Seed` | `Random Seed` | Seçenek başlığı |
| `Use prior prevalence` | `Use prior prevalence` | Seçenek başlığı |
| `Prior Prevalence Value` | `Prior Prevalence Value` | Seçenek başlığı |
| `Cost Ratio (FP:FN)` | `Cost Ratio (FP:FN)` | Seçenek başlığı |

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

- [x] User-facing strings in `R/psychopdaROC.b.R` properly wrapped in `.(...)`.
- [x] Schema files (`.a.yaml`, `.u.yaml`, `.r.yaml`) synchronized with backend.
- [x] Turkish terminology conforms to clinical and statistical guidelines.
- [x] Catalogs updated via `jmvtools::i18nUpdate()`.

