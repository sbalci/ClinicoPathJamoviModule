# Internationalization (i18n) Translation Plan: reportcat → Turkish (TR)

## 0) Argument Normalization

**SANITIZED_FN**: `reportcat`

**Target files analysis**:
- ✅ `jamovi/reportcat.a.yaml` (options) - EXISTS
- ✅ `jamovi/reportcat.u.yaml` (UI) - EXISTS
- ✅ `jamovi/reportcat.r.yaml` (results) - EXISTS
- ✅ `R/reportcat.b.R` (backend) - EXISTS

All required files are present and properly configured.

---

## 1) NAMESPACE i18n Hook Status

✅ **ALREADY CONFIGURED**: The NAMESPACE file contains the required `import(jmvcore)` and `importFrom(jmvcore, .)`.

---

## 2) Translatable String Analysis

### 2.1 Current State Assessment
- ✅ All user-facing error and advisory messages are wrapped with `.(...)`.
- ✅ HTML template strings use `glue::glue()` with localized tokens.
- ✅ Table titles and summaries in `gtExtras` fallback and main render are wrapped with `.(...)`.
- ✅ Pluralization and count strings handled with `.(...)`.

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
| `"Summary of Categorical Variables"` | `"Kategorik Değişkenlerin Özeti"` | Analysis title |
| `"Welcome to ClinicoPath"` | `"ClinicoPath'e Hoş Geldiniz"` | Welcome header |
| `"This tool generates a summary of your selected categorical variables."` | `"Bu araç, seçilen kategorik değişkenlerin özetini üretir."` | Description |
| `"Instructions"` | `"Talimatlar"` | Section header |
| `"Please select the Variables you wish to analyze."` | `"Lütfen analiz etmek istediğiniz Değişkenleri seçin."` | Guidance |
| `"Only Nominal, Ordinal, or Categorical variables (factors) are allowed."` | `"Yalnızca Nominal, Ordinal veya Kategorik değişkenlere (faktörler) izin verilir."` | Input restriction |
| `"The dataset has no rows. Check whether a row filter is excluding every case."` | `"Veri setinde satır bulunamadı. Bir satır filtresinin tüm olguları dışlayıp dışlamadığını kontrol edin."` | Validation error |
| `"No valid variables selected."` | `"Geçerli değişken seçilmedi."` | Validation error |
| `"Non-categorical variables detected"` | `"Kategorik olmayan değişkenler tespit edildi"` | Validation error |
| `"Please select only categorical (factor or character) variables."` | `"Lütfen yalnızca kategorik (faktör veya metin) değişkenleri seçin."` | Action guidance |
| `"Variables with no valid levels or all missing values"` | `"Geçerli düzeyi olmayan veya tümü eksik değerli değişkenler"` | Data warning |
| `"These will be excluded from analysis."` | `"Bunlar analizden hariç tutulacaktır."` | Data warning |
| `"Categorical Variables Summary"` | `"Kategorik Değişkenler Özeti"` | Table title |
| `"Distribution and missing value analysis"` | `"Dağılım ve eksik değer analizi"` | Table subtitle |
| `"{level}: n = {n}, {percent} of valid cases."` | `"{level}: n = {n}, geçerli olguların %{percent}'i."` | Level summary |
| `"<strong>{var}</strong> has {rows} and {levels}."` | `"<strong>{var}</strong> {rows} ve {levels} içermektedir."` | Summary sentence |
| `"Missing values: {count}. Percentages above are of {valid} valid cases."` | `"Eksik değerler: {count}. Yukarıdaki yüzdeler {valid} geçerli olguya aittir."` | Missing value summary |
| `"Clinical Interpretation"` | `"Klinik Yorum"` | Results section |

---

## 5) Consistency & Glossary (TR)

```text
Categorical variable → Kategorik değişken
Nominal variable → Nominal değişken
Ordinal variable → Ordinal değişken
Factor → Faktör
Level / Category → Düzey / Kategori
Frequency / Count → Sıklık / Sayı (n)
Valid cases → Geçerli olgular
Missing values → Eksik değerler
Distribution → Dağılım
```

---

## 6) QA Checklist

- [x] All user-visible strings in `R/reportcat.b.R` are wrapped in `.(...)`.
- [x] NAMESPACE imports `jmvcore`.
- [x] `.a.yaml`, `.u.yaml`, `.r.yaml` exist and are synchronized.
- [x] Turkish clinical terminology is reviewed for accuracy.
