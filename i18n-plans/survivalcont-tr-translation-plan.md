# Internationalization (i18n) Translation Plan: survivalcont → Turkish (TR)

## Analysis: Survival with Continuous Explanatory Variable

**Description**: Survival analysis with continuous biomarkers, cutpoint optimization (maximally selected rank statistics), and spline modeling.

### 0) Argument Normalization

- **Sanitized Function**: `survivalcont`
- **YAML Schema Files**:
  - `jamovi/survivalcont.a.yaml` (Options schema)
  - `jamovi/survivalcont.u.yaml` (UI layout schema)
  - `jamovi/survivalcont.r.yaml` (Results presentation schema)
- **R Backend File**:
  - `R/survivalcont.b.R` (Core implementation)

---

## 1) Translation Metrics

- **Total Function-Specific Strings**: 49
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
| `A recursive optimal split could not be estimated. Quantile-based cut-points w...` | `Özyinelemeli en uygun bölme tahmin edilemedi. Kalan gruplar için kantil taban...` |
| `Analysis completed for {variable}.` | `{variable} için analiz tamamlandı.` |
| `Analysis of {variable} could not determine hazard ratio.` | `{variable} analizi hazard oranını belirleyemedi.` |
| `Analysis warning` | `Analiz uyarısı` |
| `Approximate minimum p-value search` | `Yaklaşık minimum p-değeri araması` |
| `Clinical Assumptions Warning` | `Klinik varsayımlar uyarısı` |
| `Clinical Interpretation` | `Klinik Yorum` |
| `Copy-ready summary for clinical reports:` | `Klinik raporlar için kullanıma hazır özet:` |
| `Cox regression analysis` | `Cox regresyon analizi` |
| `Cox Regression Analysis` | `Cox regresyon analizi` |
| `Cox regression estimates the hazard ratio (HR) for a one-unit increase in the...` | `Cox regresyonu, sürekli yordayıcıdaki bir birimlik artış için hazard oranını ...` |
| `Cut-off & Univariate Survival Analysis` | `Kesim Noktası ve Tek Değişkenli Sağkalım Analizi` |
| `Dataset contains %d rows. Analysis may take longer than usual, especially wit...` | `Veri kümesi %d satır içeriyor. Analiz, özellikle veriye dayalı kesim noktası ...` |
| `Event counts include all-cause events as defined in the outcome mapping.` | `Olay sayıları, sonuç eşlemesinde tanımlandığı biçimde tüm nedenlere bağlı ola...` |
| `Event counts reflect the event of interest; competing events are treated as c...` | `Olay sayıları ilgilenilen olayı yansıtır; yarışan olaylar hız hesaplarında sa...` |
| `Event counts reflect the specified event of interest.` | `Olay sayıları belirtilen ilgilenilen olayı yansıtır.` |
| `HR = 1.05 means each one-unit increase multiplies the hazard by 1.05; it is n...` | `HR = 1,05, her bir birimlik artışın hazardı 1,05 ile çarptığı anlamına gelir;...` |
| `Large dataset detected` | `Büyük veri kümesi saptandı` |
| `Limited variability in continuous explanatory variable. Consider treating as ...` | `Sürekli açıklayıcı değişkende değişkenlik sınırlı. Kategorik olarak ele almay...` |
| `Low event rate ({rate}%). May need larger sample or longer follow-up for reli...` | `Olay hızı düşük (%{rate}). Güvenilir sağkalım analizi için daha büyük örnekle...` |
| `Marker groups` | `Belirteç grupları` |
| `Median survival time for {group} could not be determined.` | `{group} için medyan sağkalım süresi belirlenemedi.` |
| `Minimum p-value search found no admissible split` | `Minimum p-değeri araması kabul edilebilir bir bölme bulamadı` |
| `No Cox regression results are available to summarise. This usually means the ...` | `Özetlenecek Cox regresyonu sonucu yok. Bu durum genellikle modelin uyarlanama...` |
| `No evaluated combination both satisfied the minimum group-size requirement an...` | `Değerlendirilen hiçbir kombinasyon hem en küçük grup büyüklüğü koşulunu sağla...` |
| `only %d non-missing value(s) of '%s' are available; at least 10 are required` | `'%s' için yalnızca %d eksik olmayan değer mevcut; en az 10 gereklidir` |
| `Optimal cut-off point for {variable} could not be determined.` | `{variable} için en uygun kesim noktası belirlenemedi.` |
| `Optimal cutoff analysis` | `En uygun kesim noktası analizi` |
| `Recursive cut-off search incomplete` | `Özyinelemeli kesim noktası araması tamamlanamadı` |
| `RMST and its Greenwood-based standard error are calculated from the Kaplan-Me...` | `RMST ve Greenwood tabanlı standart hatası, ortak gözlenen zaman ufkunda survi...` |
| `Short median follow-up ({time} {units}). May be insufficient for meaningful s...` | `Medyan izlem kısa ({time} {units}). Anlamlı bir sağkalım analizi için yetersi...` |
| `Small sample size (n = {n}). Consider larger sample for more reliable cut-off...` | `Örneklem küçük (n = {n}). Daha güvenilir kesim noktası analizi için daha büyü...` |
| `Sparse strata detected: %s. Strata with fewer than 10 patients or 3 events pr...` | `Seyrek tabakalar saptandı: %s. 10'dan az hasta veya 3'ten az olay içeren taba...` |
| `Sparse strata in stratified Cox model` | `Tabakalı Cox modelinde seyrek tabakalar` |
| `Statistical Analysis Warning` | `İstatistiksel analiz uyarısı` |

