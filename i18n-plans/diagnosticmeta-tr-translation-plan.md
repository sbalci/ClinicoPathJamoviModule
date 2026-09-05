# Internationalization (i18n) Translation Plan: diagnosticmeta → Turkish (TR)

## 0) Argument Normalization

**SANITIZED_FN**: `diagnosticmeta`

**Target files**:
- `jamovi/diagnosticmeta.a.yaml` (options)
- `jamovi/diagnosticmeta.u.yaml` (UI)
- `jamovi/diagnosticmeta.r.yaml` (results)
- `R/diagnosticmeta.b.R` (backend)

All required files are verified and present in both `OncoPath` and `ClinicoPathJamoviModule`.

---

## 1) NAMESPACE i18n Hook Status

`NAMESPACE` contains `import(jmvcore)` and `importFrom(jmvcore, .)`, enabling runtime internationalization and extraction.

---

## 2) Translatable String Analysis & Wrapping Status

- All user-visible strings in `R/diagnosticmeta.b.R` and YAML definitions are wrapped in `.(...)`.
- Spliced qualitative classification labels (`sens_class`, `spec_class`, `plr_class`, `nlr_class`) were refactored and wrapped in `.()` so they are fully extracted and translated.
- Error notices, warning banners, and clinical interpretations use proper placeholder tokens `{n}`, `%s`, `%.1f`, etc.
- Zero-cell continuity correction messages provide complete sentences for clear clinical interpretation.

---

## 3) Extraction & Update Commands

```r
# At package root:
jmvtools::i18nUpdate("tr")
```

---

## 4) Representative Turkish Translation Dictionary (diagnosticmeta)

| English (msgid) | Turkish (TR) Equivalent | Context / Notes |
| :--- | :--- | :--- |
| `Diagnostic Test Meta-Analysis for Pathology` | `Patoloji için Tanısal Test Meta-Analizi` | Analiz Başlığı |
| `Study identifier` | `Çalışma tanımlayıcısı` | Seçenek başlığı |
| `True Positives (TP)` | `Doğru Pozitifler (DP)` | Sayım değişkeni |
| `False Positives (FP)` | `Yanlış Pozitifler (YP)` | Sayım değişkeni |
| `False Negatives (FN)` | `Yanlış Negatifler (YN)` | Sayım değişkeni |
| `True Negatives (TN)` | `Doğru Negatifler (DN)` | Sayım değişkeni |
| `Bivariate random-effects model` | `İki değişkenli rastgele etkiler modeli` | Reitsma modeli |
| `Proportional-hazards SROC analysis` | `Orantılı tehlikeler SROC analizi` | Holling SROC modeli |
| `Meta-regression` | `Meta-regresyon` | Ortak değişken analizi |
| `Heterogeneity analysis` | `Heterojenlik analizi` | Çalışmalar arası varyans |
| `Publication bias assessment` | `Yayın yanlılığı değerlendirmesi` | Deeks huni grafiği testi |
| `Confidence Level` | `Güven Düzeyi` | GA düzeyi (%) |
| `Zero-cell correction method` | `Sıfır hücre düzeltme yöntemi` | Seyrek veri seçeneği |
| `Forest plot (sensitivity & specificity)` | `Orman grafiği (duyarlılık ve özgüllük)` | Görselleştirme |
| `Summary ROC plot` | `Özet ROC grafiği` | SROC eğrisi |
| `Funnel plot (publication bias)` | `Huni grafiği (yayın yanlılığı)` | Deeks asimetri grafiği |
| `Individual study results` | `Bireysel çalışma sonuçları` | Çalışma tablosu |
| `Clinical interpretation` | `Klinik yorum` | Klinik anlatım |
| `Pooled Sensitivity` | `Havuzlanmış Duyarlılık` | Özet metrik |
| `Pooled Specificity` | `Havuzlanmış Özgüllük` | Özet metrik |
| `Positive Likelihood Ratio` | `Pozitif Olabilirlik Oranı` | POO / LR+ |
| `Negative Likelihood Ratio` | `Negatif Olabilirlik Oranı` | NOO / LR- |
| `Diagnostic Odds Ratio` | `Tanısal Odds Oranı` | TOO / DOR |

---

## 5) Consistency & Glossary (TR)

```text
Confidence Interval (CI) → Güven Aralığı (GA) [%95 GA]
p-value → p-değeri
df → sd (serbestlik derecesi)
Sensitivity / Specificity → Duyarlılık / Özgüllük
Positive / Negative Likelihood Ratio → Pozitif / Negatif Olabilirlik Oranı
Diagnostic Odds Ratio (DOR) → Tanısal Odds Oranı (TOO)
Area Under Curve (AUC) → Eğri Altı Alan (EAA)
Summary ROC (SROC) → Özet ROC (SROC)
Continuity correction → Süreklilik düzeltmesi
```

---

## 6) QA Checklist

- [x] All 288 entries in `diagnosticmeta` translated in `tr.po`.
- [x] Qualitative performance labels wrapped in `.()`.
- [x] Verified `msgfmt -c -v` passes with 0 errors.
- [x] Verified 0 `msgctxt` bracket-tail traps.
- [x] Zero drift between `OncoPath` and `ClinicoPathJamoviModule`.
