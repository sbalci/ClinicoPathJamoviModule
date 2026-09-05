# Internationalization (i18n) Translation Plan: singlearm → Turkish (TR)

## Analysis: Single-Arm Survival Analysis

**Description**: Benchmark comparison and survival analysis for single-arm clinical trials against historical controls or objective response thresholds.

### 0) Argument Normalization

- **Sanitized Function**: `singlearm`
- **YAML Schema Files**:
  - `jamovi/singlearm.a.yaml` (Options schema)
  - `jamovi/singlearm.u.yaml` (UI layout schema)
  - `jamovi/singlearm.r.yaml` (Results presentation schema)
- **R Backend File**:
  - `R/singlearm.b.R` (Core implementation)

---

## 1) Translation Metrics

- **Total Function-Specific Strings**: 163
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
| `(95%% CI: %.1f-%.1f %s)` | `(%%95 GA: %.1f-%.1f %s)` |
| `(not set)` | `(ayarlanmadı)` |
| `{censored} censored` | `{censored} sansürlenmiş` |
| `{censored} censored, {k} competing` | `{censored} sansürlenmiş, {k} yarışan` |
| `{n} time value(s) are negative (smallest {min}). Time values must be zero or ...` | `{n} zaman değeri negatif (en küçüğü {min}). Zaman değerleri sıfır veya poziti...` |
| `%d competing event(s) are present. Counts and the minimum-event summaries bel...` | `%d yarışan olay mevcut. Aşağıdaki sayımlar ve asgari olay özetleri yalnızca i...` |
| `%d observation(s) have follow-up time zero. They are retained: events at time...` | `%d gözlem sıfır takip süresine sahip. Bunlar korunur: sıfır zamanındaki olayl...` |
| `%d time value%s infinite (Inf). Follow-up time must be a finite number; an in...` | `%d zaman değeri%s sonsuzdur (Inf). Takip süresi sonlu bir sayı olmalıdır; son...` |
| `%s contain non-numeric value(s): %s ignored. Enter comma-separated numeric ti...` | `%s sayısal olmayan değer(ler) içeriyor: %s yok sayıldı. Virgülle ayrılmış say...` |
| `%s is not available for competing-risks analysis: it assumes a single event t...` | `%s yarışan riskler analizi için kullanılamaz: tek bir olay türü varsayar ve "...` |
| `%s is not available for competing-risks analysis: it assumes a single event t...` | `%s yarışan riskler analizi için kullanılamaz: tek bir olay türü varsayar ve y...` |
| `%s must be finite and zero or positive: %s ignored. Time is measured forward ...` | `%s sonlu ve sıfır veya pozitif olmalıdır: %s yok sayıldı. Zaman takibin başla...` |
| `%s were used exactly as entered (%s) and are read in %s, the selected time un...` | `%s tam olarak girildiği gibi kullanıldı (%s) ve seçilen zaman birimi olan %s ...` |
| `<b>95% CI</b> - pointwise interval for the incidence at that single time poin...` | `<b>%95 GA</b> - bu tek zaman noktasındaki insidans için noktasal güven aralığ...` |
| `<b>Cumulative incidence</b> - the proportion of the original cohort who have ...` | `<b>Kümülatif insidans</b> - o zamana kadar ilgilenilen olayı yaşayan orijinal...` |
| `<b>Do not compare it with a Kaplan-Meier median</b> from the same data. A Kap...` | `<b>Bunu aynı verilerden elde edilen bir Kaplan-Meier medyanı ile karşılaştırm...` |
| `<b>No confidence interval is reported.</b> A valid interval for this quantile...` | `<b>Güven aralığı bildirilmemiştir.</b> Bu kantil için geçerli bir aralık, küm...` |
| `<b>Not reached</b> is common and expected here: whenever competing events are...` | `<b>Ulaşılmadı</b> burada yaygındır ve beklenir: yarışan olaylar sık olduğunda...` |
| `<b>Number at Risk</b> - subjects still under follow-up and still free of both...` | `<b>Risk Altındaki Sayı</b> - o sırada hala takip altında olan ve her iki olay...` |
| `A smoothed continuous hazard was not estimated because one or more events occ...` | `Sıfır zamanında bir veya daha fazla olay meydana geldiği için düzeltilmiş sür...` |
| `A smoothed hazard was not estimated because no events were observed. A flat z...` | `Hiçbir olay gözlemlenmediği için düzeltilmiş bir tehlike tahmin edilmedi. Düz...` |
| `An error occurred during {context}.` | `{context} sırasında bir hata oluştu.` |
| `Analysis completed: %s. %s: %.1f %s. %s analysis using %s method.` | `Analiz tamamlandı: %s. %s: %.1f %s. %s yöntemi kullanılarak %s analizi.` |
| `Automated adequacy grades are not assigned. Interpret event counts, completen...` | `Otomatik yeterlilik dereceleri atanmaz. Olay sayılarını, eksiksizliği, takibi...` |
| `Bare numeric date columns were interpreted as %s since 1970-01-01. Each numer...` | `Yalın sayısal tarih sütunları 1970-01-01'den bu yana %s olarak yorumlandı. He...` |
| `Calculated follow-up from "%s" and "%s" is implausible: %d observation(s) exc...` | `"%s" ve "%s" üzerinden hesaplanan takip mantıksızdır: %d gözlem 150 yılı aşıy...` |
| `Calculated time from dates contains %d missing value%s. These observations wi...` | `Tarihlerden hesaplanan süre %d kayıp değer%s içeriyor. Bu gözlemler analizden...` |
| `Clinical Considerations` | `Klinik Hususlar` |
| `ClinicoPath Survival` | `ClinicoPath Sağkalım` |
| `Competing event` | `Yarışan olay` |
| `Competing risk analysis failed: %s. Please verify outcome is coded as 0 (cens...` | `Yarışan risk analizi başarısız oldu: %s. Lütfen sonucun 0 (sansürlenmiş), 1 (...` |
| `Competing risk analysis failed: the fitted model does not contain the event-o...` | `Yarışan risk analizi başarısız oldu: uyarlanan model ilgilenilen olay durumun...` |
| `Competing risk analysis: Median time represents cumulative incidence of event...` | `Yarışan risk analizi: Medyan süre, yarışan olayları uygun şekilde hesaba kata...` |
| `Confidence limits are left blank where the Kaplan-Meier event-free estimate i...` | `Kaplan-Meier olaysızlık tahmini tam olarak %0 veya %100 olduğunda güven sınır...` |
| `Copy-ready descriptive cohort summary` | `Kopyalamaya hazır tanımlayıcı kohort özeti` |

