# Internationalization (i18n) Translation Plan: waterfall → Turkish (TR)

## 0) Argument Normalization

**SANITIZED_FN**: `waterfall`

**Target files**:
- `jamovi/waterfall.a.yaml` (options)
- `jamovi/waterfall.u.yaml` (UI)
- `jamovi/waterfall.r.yaml` (results)
- `R/waterfall.b.R` (backend)

All required files are verified and present in both `OncoPath` and `ClinicoPathJamoviModule`.

---

## 1) NAMESPACE i18n Hook Status

`NAMESPACE` contains `import(jmvcore)` and `importFrom(jmvcore, .)`, enabling runtime internationalization and extraction.

---

## 2) Translatable String Analysis & Wrapping Status

- All user-visible strings in `R/waterfall.b.R` and YAML definitions are wrapped in `.(...)`.
- Fixed bracket-tail trap at line 2907 (`95%% CI (Median): [%.1f%%, %.1f%%]` -> `95%% CI (Median): (%.1f%%, %.1f%%)`).
- Spliced Fisher's exact test concatenation was refactored into complete `sprintf(.("Fisher's exact test, OR = %.2f"), ...)` alternative.
- Error notices, warning banners, and clinical interpretations use proper placeholder tokens (`%s`, `%.1f`, `%d`, etc.).

---

## 3) Extraction & Update Commands

```r
# At package root:
jmvtools::i18nUpdate("tr")
```

---

## 4) Representative Turkish Translation Dictionary (waterfall)

| English (msgid) | Turkish (TR) Equivalent | Context / Notes |
| :--- | :--- | :--- |
| `Waterfall Plot` | `Şelale Grafiği` | Analiz Başlığı |
| `Spider Plot` | `Örümcek Grafiği` | Boylamsal grafik |
| `Patient ID variable` | `Hasta Kimliği değişkeni` | Temel girdi |
| `Response value (raw or percentage)` | `Yanıt değeri (ham veya yüzde)` | Tümör yükü |
| `Time variable (required for spider plot)` | `Zaman değişkeni (örümcek grafiği için gerekli)` | Takip zamanı |
| `Group variable` | `Grup değişkeni` | Kol karşılaştırması |
| `Response Category Override (optional)` | `Yanıt Kategorisi Geçersiz Kılma (isteğe bağlı)` | RECIST geçersiz kılma |
| `Confirmation status (optional)` | `Doğrulama durumu (isteğe bağlı)` | Doğrulanmış yanıt |
| `Objective Response Rate (ORR)` | `Objektif Yanıt Oranı (ORR)` | CR + PR oranı |
| `Disease Control Rate (DCR)` | `Hastalık Kontrol Oranı (DCR)` | CR + PR + SD oranı |
| `Complete Response (CR)` | `Tam Yanıt (CR)` | Tüm lezyonların kaybolması |
| `Partial Response (PR)` | `Kısmi Yanıt (PR)` | >= %30 küçülme |
| `Stable Disease (SD)` | `Kararlı Hastalık (SD)` | Ne PR ne PD |
| `Progressive Disease (PD)` | `İlerleyen Hastalık (PD)` | >= %20 artış |
| `Nadir` | `Nadir (en düşük tümör yükü)` | Progresyon referansı |
| `Median Time to First Response` | `İlk Yanıta Kadar Geçen Medyan Süre` | TTR |
| `Median Duration of Response` | `Medyan Yanıt Süresi` | DoR (Kaplan-Meier) |
| `Time-to-Response & Duration of Response` | `Yanıta Kadar Geçen Süre ve Yanıt Süresi` | Tablo başlığı |

---

## 5) Consistency & Glossary (TR)

```text
Waterfall Plot → Şelale Grafiği (Waterfall Plot)
Spider Plot → Örümcek Grafiği (Spider Plot)
Tumor response → Tümör yanıtı
Percent change from baseline → Başlangıca göre yüzde değişim
Nadir → Nadir (en düşük tümör yükü)
Time to response (TTR) → Yanıta kadar geçen süre (TTR)
Duration of response (DoR) → Yanıt süresi (DoR)
Best Overall Response (BOR) → En İyi Genel Yanıt (BOR)
Target lesion → Hedef lezyon
Non-target lesion → Hedef dışı lezyon
Objective Response Rate (ORR) → Objektif Yanıt Oranı (ORR)
Disease Control Rate (DCR) → Hastalık Kontrol Oranı (DCR)
```

---

## 6) QA Checklist

- [x] All 443 entries in `waterfall` translated in `tr.po`.
- [x] Bracket-tail trap at line 2907 eliminated.
- [x] Spliced Fisher's exact test string refactored.
- [x] Verified `msgfmt -c -v` passes with 0 errors.
- [x] Verified 0 `msgctxt` bracket-tail traps.
- [x] Zero drift between `OncoPath` and `ClinicoPathJamoviModule`.
