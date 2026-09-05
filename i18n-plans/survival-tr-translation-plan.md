# Internationalization (i18n) Translation Plan: survival → Turkish (TR)

## Analysis: Survival Analysis (Kaplan-Meier & Cox)

**Description**: Comprehensive Kaplan-Meier analysis, log-rank tests, univariate Cox regression, RMST, landmark analysis, and diagnostic residuals.

### 0) Argument Normalization

- **Sanitized Function**: `survival`
- **YAML Schema Files**:
  - `jamovi/survival.a.yaml` (Options schema)
  - `jamovi/survival.u.yaml` (UI layout schema)
  - `jamovi/survival.r.yaml` (Results presentation schema)
- **R Backend File**:
  - `R/survival.b.R` (Core implementation)

---

## 1) Translation Metrics

- **Total Function-Specific Strings**: 89
- **Translation Coverage**: 100% Complete
- **Validation Status**: 0 placeholder mismatches, 0 HTML tag mismatches, 0 context traps

---

## 2) Verification & Testing Commands

```bash
# Validate Turkish catalog syntax and compilation
msgfmt -v -c -o /dev/null jamovi/i18n/tr.po

# Run package unit tests
Rscript -e "devtools::test()"
```

---

## 3) Translation Sample Dictionary (First 35 Strings)

| English Source (`msgid`) | Turkish Translation (`msgstr`) |
| :--- | :--- |
| ` All times reported here are measured FROM the landmark, not from study entry...` | ` Burada bildirilen tüm süreler çalışmaya girişten değil, yer işaretinden İTİB...` |
| `{comparison}: hazard ratio {hr} (95% CI {lower} to {upper}, {stats}) - {direc...` | `{comparison}: tehlike oranı {hr} (%95 GA {lower} ila {upper}, {stats}) - refe...` |
| `{count} row(s) with missing outcome were excluded before model fitting.` | `Model uyarlamasından önce eksik sonucu olan {count} satır hariç tutuldu.` |
| ``Survival Plot with Multiple Cut-offs - ${contexpl}`` | ``Çoklu Kesim Noktaları ile Sağkalım Grafiği - ${contexpl}`` |
| `<br><b>Landmark analysis at %s %s.</b>` | `<br><b>%s %s zamanında yer işareti analizi.</b>` |
| `a higher hazard` | `daha yüksek bir tehlike` |
| `a lower hazard` | `daha düşük bir tehlike` |
| `A result table could not be populated completely: {message}` | `Bir sonuç tablosu tamamen doldurulamadı: {message}` |
| `Age interaction test failed: {message}` | `Yaş etkileşim testi başarısız oldu: {message}` |
| `Age standardization failed: {message}` | `Yaş standardizasyonu başarısız oldu: {message}` |
| `Age variable not found in data.` | `Yaş değişkeni verilerde bulunamadı.` |
| `Age-adjusted Cox regression failed: {message}` | `Yaşa göre düzeltilmiş Cox regresyonu başarısız oldu: {message}` |
| `Age-as-time-scale analysis failed: {message}` | `Zaman ölçeği olarak yaş analizi başarısız oldu: {message}` |
| `All times are measured FROM the landmark, not from study entry, and all estim...` | `Tüm süreler çalışmaya girişten değil, yer işaretinden İTİBAREN ölçülür ve tüm...` |
| `Assessment of Proportional Hazards Assumption` | `Orantılı Tehlikeler Varsayımının Değerlendirilmesi` |
| `Basic Kaplan-Meier and Cox analyses are available, but {count} events may be ...` | `Temel Kaplan-Meier ve Cox analizleri mevcuttur, ancak karmaşık kalibrasyon, e...` |
| `Bootstrap validation suppressed: fewer than 10 events.` | `Önyükleme doğrulaması baskılandı: 10'dan az olay.` |
| `Calibration curves suppressed: fewer than 10 events.` | `Kalibrasyon eğrileri baskılandı: 10'dan az olay.` |
| `Competing-risk mode is selected; standard Cox regression is skipped because c...` | `Yarışan risk modu seçildi; nedene özgü tehlikeler farklı bir model gerektirdi...` |
| `Cox model residual diagnostics` | `Cox modeli artık tanıları` |
| `Cox regression data could not be added to the clinical summary: {message}` | `Cox regresyon verileri klinik özete eklenemedi: {message}` |
| `Cox regression estimated {direction} for {comparison}, with a hazard ratio of...` | `Cox regresyonu, {comparison} için bir tehlike oranı {hr} (%95 GA: {lower} ila...` |
| `Cox regression estimated a hazard ratio of {hr} for {comparison} (95% CI: {lo...` | `Cox regresyonu {comparison} için {hr} tehlike oranı tahmin etti (%95 GA: {low...` |
| `Cox regression skipped (competing risks)` | `Cox regresyonu atlandı (yarışan riskler)` |
| `cox.zph p-values below 0.05 indicate potential violation of the proportional ...` | `0.05'in altındaki cox.zph p değerleri bir veya daha fazla terim için orantılı...` |
| `Cumulative events for {group}` | `{group} için kümülatif olaylar` |
| `Cumulative hazard for {group}` | `{group} için kümülatif tehlike` |
| `Descriptive results (Kaplan-Meier, median, counts) are shown, but with {count...` | `Tanımlayıcı sonuçlar (Kaplan-Meier, medyan, sayımlar) gösterilmektedir, ancak...` |
| `Diagnosis date and follow-up date must use the same format (both numeric or b...` | `Tanı tarihi ve takip tarihi aynı biçimi kullanmalıdır (her ikisi de sayısal v...` |
| `Error creating residuals plot: {message}` | `Artıklar grafiği oluşturulurken hata: {message}` |
| `Error: Data could not be cleaned for analysis.` | `Hata: Veriler analiz için temizlenemedi.` |
| `Explanatory variable is required. Please select a categorical variable to com...` | `Açıklayıcı değişken gereklidir. Lütfen gruplar arasındaki sağkalımı karşılaşt...` |
| `Gray's test for equality of cumulative incidence of the event of interest acr...` | `İlgilenilen olayın kümülatif insidansının gruplar arasında eşitliği için Gray...` |
| `Group(s) with zero events: {groups}. Their median is undefined and hazard rat...` | `Sıfır olaylı grup(lar): {groups}. Medyanları tanımsızdır ve bunları içeren te...` |
| `In the {group} group the median was not reached: fewer than half of the patie...` | `{group} grubunda medyana ulaşılamadı: gözlemlenen takip sırasında hastaların ...` |

