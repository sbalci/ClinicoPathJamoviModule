# Internationalization (i18n) Translation Plan: OncoPath Module Functions → Turkish (TR)

## 0) Scope & Architecture

**Module**: `OncoPath` (standalone submodule and synchronized suite within `ClinicoPathJamoviModule`)
**Target Language**: Turkish (`tr`)
**Analyses Included**:
1. `diagnosticmeta` — Diagnostic Test Meta-Analysis for Pathology (288 entries in catalog)
2. `ihcheterogeneity` — Intratumoral & Regional IHC Heterogeneity Analysis (364 entries in catalog)
3. `swimmerplot` (including `R/swimmerplot_html.R`) — Patient Timeline & Swimmer Plot Visualization (398 entries in catalog)
4. `waterfall` — Treatment Response Analysis & Waterfall / Spider Plots (443 entries in catalog)
5. `package` level metadata — titles, descriptions, and example datasets (5 entries)

Total translatable strings: **1,498 entries**.
Coverage achieved: **100.0% (1,498 / 1,498 translated)**.

---

## 1) NAMESPACE & Runtime i18n Hook

- Both repositories import `.` from `jmvcore`:
  ```r
  import(jmvcore)
  importFrom(jmvcore, .)
  ```
- All R backend code uses `.("translatable text")`.
- Spliced string concatenations have been refactored into atomic sentences or `sprintf(.("... %s"), ...)` templates.
- Bracket-tail traps (e.g., `95%% CI: [%.1f%%, %.1f%%]`) have been refactored to parentheses `(%.1f%%, %.1f%%)` to prevent jamovi-compiler from stripping strings into invalid `msgctxt`.

---

## 2) Key Clinical and Statistical Glossary (TR)

| English Term | Turkish (TR) Equivalent | Notes / Context |
| :--- | :--- | :--- |
| `95% CI` | `%95 GA` | Güven Aralığı (önde % sembolü) |
| `p-value` | `p-değeri` | Küçük p harfi ile |
| `df` | `sd` | Serbestlik derecesi |
| `Sensitivity` / `Specificity` | `Duyarlılık` / `Özgüllük` | Tanısal test metrikleri |
| `Positive Likelihood Ratio (PLR)` | `Pozitif Olabilirlik Oranı (POO)` | Tanısal geçerlilik |
| `Negative Likelihood Ratio (NLR)` | `Negatif Olabilirlik Oranı (NOO)` | Tanısal dışlama |
| `Diagnostic Odds Ratio (DOR)` | `Tanısal Odds Oranı (TOO)` | Genel tanısal etkinlik |
| `Summary ROC (SROC)` / `AUC` | `Özet ROC (SROC)` / `EAA` | Eğri Altı Alan |
| `Bivariate random-effects model` | `İki değişkenli rastgele etkiler modeli` | Reitsma modeli |
| `Zero-cell continuity correction` | `Sıfır hücre süreklilik düzeltmesi` | Seyrek veri düzeltmesi |
| `Immunohistochemistry (IHC)` | `İmmünohistokimya (IHC)` | Patoloji boyama |
| `Intratumoral heterogeneity` | `İntratümöral heterojenlik` | Doku içi değişkenlik |
| `Coefficient of Variation (CV)` | `Varyasyon Katsayısı (CV)` | Göreli değişkenlik |
| `Intraclass Correlation (ICC)` | `Sınıf içi Korelasyon Katsayısı (ICC)` | Güvenilirlik analizi |
| `Whole section` | `Bütün kesit` | Referans doku alanı |
| `Swimmer Plot` | `Yüzücü Grafiği (Swimmer Plot)` | Hasta zaman çizelgesi |
| `Waterfall Plot` | `Şelale Grafiği (Waterfall Plot)` | Tümör yükü değişimi |
| `Spider Plot` | `Örümcek Grafiği (Spider Plot)` | Zaman içinde tümör gidişatı |
| `Objective Response Rate (ORR)` | `Objektif Yanıt Oranı (ORR)` | CR + PR oranı |
| `Disease Control Rate (DCR)` | `Hastalık Kontrol Oranı (DCR)` | CR + PR + SD oranı |
| `Complete Response (CR)` | `Tam Yanıt (CR)` | Tüm lezyonların kaybolması |
| `Partial Response (PR)` | `Kısmi Yanıt (PR)` | %30 veya daha fazla küçülme |
| `Stable Disease (SD)` | `Kararlı Hastalık (SD)` | Ne PR ne PD kriteri |
| `Progressive Disease (PD)` | `İlerleyen Hastalık (PD)` | %20 veya daha fazla artış |
| `Nadir` | `Nadir (en düşük tümör yükü)` | Progresyon referansı |
| `Person-time` | `Kişi-zamanı` | Takip süresi toplamı |
| `Reverse Kaplan-Meier` | `Ters Kaplan-Meier` | Medyan takip süresi hesabı |

---

## 3) Quality Assurance & Validation Gates

1. **Compilation Validation**:
   ```bash
   msgfmt -v -c -o /dev/null jamovi/i18n/tr.po
   # Result: 1498 translated messages. 0 errors, 0 warnings.
   ```
2. **Context & Bracket-Tail Trap Check**:
   ```bash
   grep -c "msgctxt" jamovi/i18n/tr.po
   # Result: 0 (No broken msgctxt or bracket-tail stripping)
   ```
3. **Format Token Parity**:
   All `%s`, `%d`, `%.1f`, `%%`, `{var}`, `{message}`, `{origin}`, `{unit}`, and HTML tags match identically between `msgid` and `msgstr`.
4. **Zero-Drift Invariant**:
   All 4 R backend files (`diagnosticmeta.b.R`, `ihcheterogeneity.b.R`, `swimmerplot.b.R`, `waterfall.b.R`), `R/swimmerplot_html.R`, and all 12 YAML schema files (`.a.yaml`, `.u.yaml`, `.r.yaml`) are byte-for-byte identical between `OncoPath` and `ClinicoPathJamoviModule`.
5. **Test Suite Verification**:
   `OncoPath` tests: `[ FAIL 0 | WARN 0 | SKIP 1 | PASS 59 ]`.
   `ClinicoPathJamoviModule` tests pass without regression.
