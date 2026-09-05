# Internationalization (i18n) Translation Plan: swimmerplot → Turkish (TR)

## 0) Argument Normalization

**SANITIZED_FN**: `swimmerplot`

**Target files**:
- `jamovi/swimmerplot.a.yaml` (options)
- `jamovi/swimmerplot.u.yaml` (UI)
- `jamovi/swimmerplot.r.yaml` (results)
- `R/swimmerplot.b.R` (backend)
- `R/swimmerplot_html.R` (rich HTML guides and tooltips)

All required files are verified and present in both `OncoPath` and `ClinicoPathJamoviModule`.

---

## 1) NAMESPACE i18n Hook Status

`NAMESPACE` contains `import(jmvcore)` and `importFrom(jmvcore, .)`, enabling runtime internationalization and extraction.

---

## 2) Translatable String Analysis & Wrapping Status

- All user-visible strings in `R/swimmerplot.b.R`, `R/swimmerplot_html.R`, and YAML definitions are wrapped in `.(...)`.
- Spliced Fisher's exact test concatenation was refactored into complete `sprintf(.("Fisher's exact test, OR = %.2f"), ...)` alternative.
- Error notices, warning banners, and clinical interpretations use proper placeholder tokens (`%s`, `%.1f`, `%d`, `{unit}`, `{response}`, etc.).

---

## 3) Extraction & Update Commands

```r
# At package root:
jmvtools::i18nUpdate("tr")
```

---

## 4) Representative Turkish Translation Dictionary (swimmerplot)

| English (msgid) | Turkish (TR) Equivalent | Context / Notes |
| :--- | :--- | :--- |
| `Swimmer Plot Analysis` | `Yüzücü Grafiği Analizi` | Analiz Başlığı |
| `Patient ID (Required)` | `Hasta Kimliği (Gerekli)` | Temel girdi |
| `Start Time (Required)` | `Başlangıç Zamanı (Gerekli)` | Temel girdi |
| `End Time (Required)` | `Bitiş Zamanı (Gerekli)` | Temel girdi |
| `Response/Status Variable (Optional)` | `Yanıt/Durum Değişkeni (İsteğe Bağlı)` | RECIST renklendirme |
| `Censoring/Event Status (Optional)` | `Sansürleme/Olay Durumu (İsteğe Bağlı)` | Devam eden tedavi oku |
| `Event Markers` | `Olay İşaretçileri` | Kilometre taşları |
| `Milestones` | `Kilometre Taşları` | Protokol olayları |
| `Time & Date Settings` | `Zaman ve Tarih Ayarları` | Format ayarları |
| `Follow-up Density` | `Takip Yoğunluğu` | Kişi-zamanı metriği |
| `Total Person-Time` | `Toplam Kişi-Zamanı` | Takip süresi toplamı |
| `Objective Response Rate (ORR)` | `Objektif Yanıt Oranı (ORR)` | CR + PR oranı |
| `Disease Control Rate (DCR)` | `Hastalık Kontrol Oranı (DCR)` | CR + PR + SD oranı |
| `Median Follow-up Time (reverse Kaplan-Meier)` | `Medyan Takip Süresi (ters Kaplan-Meier)` | Altın standart takip |
| `Fisher's exact test, OR = %.2f` | `Fisher kesin testi, OO = %.2f` | Grup karşılaştırması |
| `Ongoing treatment indicators` | `Devam eden tedavi göstergeleri` | Durum okları |

---

## 5) Consistency & Glossary (TR)

```text
Swimmer Plot → Yüzücü Grafiği (Swimmer Plot)
Swim lane → Yüzme kulvarı
Milestone → Kilometre Taşı
Event marker → Olay işaretçisi
Ongoing treatment → Devam eden tedavi
Person-time → Kişi-zamanı
Reverse Kaplan-Meier → Ters Kaplan-Meier
Objective Response Rate (ORR) → Objektif Yanıt Oranı (ORR)
Disease Control Rate (DCR) → Hastalık Kontrol Oranı (DCR)
Complete Response (CR) → Tam Yanıt (CR)
Partial Response (PR) → Kısmi Yanıt (PR)
Stable Disease (SD) → Kararlı Hastalık (SD)
Progressive Disease (PD) → İlerleyen Hastalık (PD)
```

---

## 6) QA Checklist

- [x] All 398 entries in `swimmerplot` translated in `tr.po`.
- [x] Spliced Fisher's exact test string refactored.
- [x] Verified `msgfmt -c -v` passes with 0 errors.
- [x] Verified 0 `msgctxt` bracket-tail traps.
- [x] Zero drift between `OncoPath` and `ClinicoPathJamoviModule`.
