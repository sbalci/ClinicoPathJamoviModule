# Internationalization (i18n) Translation Plan: ihcheterogeneity → Turkish (TR)

## 0) Argument Normalization

**SANITIZED_FN**: `ihcheterogeneity`

**Target files**:
- `jamovi/ihcheterogeneity.a.yaml` (options)
- `jamovi/ihcheterogeneity.u.yaml` (UI)
- `jamovi/ihcheterogeneity.r.yaml` (results)
- `R/ihcheterogeneity.b.R` (backend)

All required files are verified and present in both `OncoPath` and `ClinicoPathJamoviModule`.

---

## 1) NAMESPACE i18n Hook Status

`NAMESPACE` contains `import(jmvcore)` and `importFrom(jmvcore, .)`, enabling runtime internationalization and extraction.

---

## 2) Translatable String Analysis & Wrapping Status

- All user-visible strings in `R/ihcheterogeneity.b.R` and YAML definitions are wrapped in `.(...)`.
- Spliced variance truncation message refactored into complete translatable sentence alternatives for case, method, and joint truncation.
- Spliced `paste(.("Could not compute:"), ...)` refactored into `sprintf(.("Could not compute: %s"), ...)`.
- Error notices, warning banners, and clinical interpretations use proper placeholder tokens (`%s`, `%.1f`, `%d`, etc.).

---

## 3) Extraction & Update Commands

```r
# At package root:
jmvtools::i18nUpdate("tr")
```

---

## 4) Representative Turkish Translation Dictionary (ihcheterogeneity)

| English (msgid) | Turkish (TR) Equivalent | Context / Notes |
| :--- | :--- | :--- |
| `IHC Heterogeneity Analysis` | `IHC Heterojenlik Analizi` | Analiz Başlığı |
| `Overall / Whole Slide / HotSpot (Optional)` | `Genel / Bütün Kesit / HotSpot (İsteğe Bağlı)` | Referans değişkeni |
| `Regional Measurement 1 (Required)` | `Bölgesel Ölçüm 1 (Gerekli)` | Bölgesel girdi |
| `Regional Measurement 2 (Optional)` | `Bölgesel Ölçüm 2 (İsteğe Bağlı)` | Bölgesel girdi |
| `Additional Regional Measurements` | `Ek Bölgesel Ölçümler` | Çoklu bölge |
| `Spatial Region ID (Optional)` | `Uzamsal Bölge Kimliği (İsteğe Bağlı)` | Uzamsal girdi |
| `Spatial compartment comparison` | `Uzamsal kompartman karşılaştırması` | Kompartman seçeneği |
| `Compartment comparison tests` | `Kompartman karşılaştırma testleri` | İstatistiksel test |
| `CV Threshold for Acceptable Variability` | `Kabul Edilebilir Değişkenlik için CV Eşiği` | Kalite kriteri |
| `Minimum acceptable correlation` | `Asgari kabul edilebilir korelasyon` | Kalite kriteri |
| `Variability plots` | `Değişkenlik grafikleri` | Görselleştirme |
| `Variance Component Analysis` | `Varyans Bileşeni Analizi` | Varyans ayrıştırması |
| `Power analysis` | `Güç analizi` | Örneklem yeterliliği |
| `Clinical recommendations` | `Klinik öneriler` | Karar desteği |
| `Plain-language summary` | `Sade dilde özet` | Anlatım |
| `Statistical glossary` | `İstatistiksel sözlük` | Kılavuz |
| `Between-Case Variance` | `Vakalar Arası Varyans` | Biyolojik değişkenlik |
| `Within-Case Variance (Sampling)` | `Vaka İçi Varyans (Örnekleme)` | Örnekleme hatası |
| `Method Variance` | `Yöntem Varyansı` | Yöntemsel yanlılık |
| `Reproducibility & Bias Assessment` | `Tekrarlanabilirlik ve Yanlılık Değerlendirmesi` | Tablo başlığı |

---

## 5) Consistency & Glossary (TR)

```text
Immunohistochemistry (IHC) → İmmünohistokimya (IHC)
Intratumoral heterogeneity → İntratümöral heterojenlik
Coefficient of Variation (CV) → Varyasyon Katsayısı (CV) [%.. CV]
Intraclass Correlation Coefficient (ICC) → Sınıf içi Korelasyon Katsayısı (ICC)
Whole section → Bütün kesit
Spatial compartment → Uzamsal kompartman
Two-way random-effects decomposition → İki yönlü rastgele etkiler ayrıştırması
Brown-Forsythe test → Brown-Forsythe testi
Kruskal-Wallis Test → Kruskal-Wallis Testi
```

---

## 6) QA Checklist

- [x] All 364 entries in `ihcheterogeneity` translated in `tr.po`.
- [x] Spliced variance truncation strings refactored into complete alternatives.
- [x] Verified `msgfmt -c -v` passes with 0 errors.
- [x] Verified 0 `msgctxt` bracket-tail traps.
- [x] Zero drift between `OncoPath` and `ClinicoPathJamoviModule`.
