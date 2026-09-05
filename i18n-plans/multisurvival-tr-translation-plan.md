# Internationalization (i18n) Translation Plan: multisurvival → Turkish (TR)

## Analysis: Multivariate Survival Analysis

**Description**: Multivariate Cox proportional hazards modeling with automated model selection, assumption checks, and clinical reports.

### 0) Argument Normalization

- **Sanitized Function**: `multisurvival`
- **YAML Schema Files**:
  - `jamovi/multisurvival.a.yaml` (Options schema)
  - `jamovi/multisurvival.u.yaml` (UI layout schema)
  - `jamovi/multisurvival.r.yaml` (Results presentation schema)
- **R Backend File**:
  - `R/multisurvival.b.R` (Core implementation)

---

## 1) Translation Metrics

- **Total Function-Specific Strings**: 550
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
| `({count} of them events)` | `({count} tanesi olay)` |
| `(95% CI {lower} to {upper})` | `(%95 GA {lower} - {upper})` |
| `(Harrell's C = {cindex}): the probability that, for a random pair of subjects...` | `(Harrell C = {cindex}): rastgele bir hasta çiftinde, daha yüksek riskli olduğ...` |
| `(none specified)` | `(belirtilmedi)` |
| `{ipa} vs Kaplan-Meier ({reference})` | `Kaplan-Meier'e göre {ipa} ({reference})` |
| `{sig} out of {total} factors showed statistically significant associations wi...` | `{total} faktörden {sig} tanesi, {rows} katsayı düzeyi üzerinden, sonlanımla i...` |
| `{sig} out of {total} factors showed statistically significant associations wi...` | `{total} faktörden {sig} tanesi sonlanımla istatistiksel olarak anlamlı ilişki...` |
| `{variable} was associated with {effect} (hazard ratio = {hr}).` | `{variable}, {effect} ile ilişkiliydi (tehlike oranı = {hr}).` |
| `* Model did not converge for: {fits} (likely small-sample separation); interp...` | `* Model şu alt gruplarda yakınsamadı: {fits} (olası neden küçük örneklemde ay...` |
| `%d observation(s) have negative time values. To fix: (1) if using 'Elapsed Ti...` | `%d gözlemde negatif zaman değeri var. Düzeltmek için: (1) 'Geçen Süre' doğrud...` |
| `95% confidence interval (uncertainty in the estimate)` | `%95 güven aralığı (tahmindeki belirsizlik)` |
| `A further {rows} row(s){events} were excluded from the model because the foll...` | `İzlem süresi veya seçilen kovaryatlardan en az biri eksik olduğu için modelde...` |
| `A hazard ratio above 1 means a higher instantaneous event rate and below 1 a ...` | `1'in üzerindeki bir tehlike oranı daha yüksek, 1'in altındaki ise daha düşük ...` |
| `a higher fitted hazard` | `daha yüksek bir model tehlikesi` |
| `a lower fitted hazard` | `daha düşük bir model tehlikesi` |
| `A nomogram is a <b>graphical representation</b> of predictions from the fitte...` | `Nomogram, uydurulan regresyon modelinin öngörülerinin <b>grafiksel bir göster...` |
| `A non-significant test is <i>no evidence of a violation</i>, which is not the...` | `Anlamlı olmayan bir test <i>ihlal için kanıt yokluğudur</i>; bu, orantılı teh...` |
| `A non-significant test is <i>no evidence of a violation</i>, which is not the...` | `Anlamlı olmayan bir test <i>ihlal için kanıt yokluğudur</i>; bu, varsayımın s...` |
| `A note on sample size when comparing to a manual per-subgroup fit:` | `Elle yapılan alt grup uydurmalarıyla karşılaştırırken örneklem büyüklüğü hakk...` |
| `A per-patient risk score could not be produced for this fit (the competing-ri...` | `Bu uydurma için hasta başına risk skoru üretilemedi (yarışan riskler modeli, ...` |
| `A small p-value indicates the covariate significantly improves fit.` | `Küçük bir p-değeri kovaryatın model uyumunu anlamlı biçimde iyileştirdiğini g...` |
| `A subdistribution hazard ratio above 1 corresponds to greater cumulative inci...` | `1'in üzerindeki bir alt dağılım tehlike oranı, Fine-Gray modeli altında izlem...` |
| `A treatment-by-biomarker interaction supports a predictive-biomarker claim on...` | `Tedavi-biyobelirteç etkileşimi, öngördürücü biyobelirteç iddiasını yalnızca u...` |
| `Aalen-Johansen estimator` | `Aalen-Johansen tahmincisi` |
| `Add at least two covariates to compare their individual contributions.` | `Bireysel katkılarını karşılaştırmak için en az iki kovaryat ekleyin.` |
| `Add the points from all predictors to obtain the <i>Total Points</i>.` | `<i>Toplam Puan</i> için tüm öngördürücülerin puanlarını toplayın.` |
| `Add up all points to get the <b>Total Points</b>` | `<b>Toplam Puan</b> için tüm puanları toplayın` |
| `Add up total points from all variables` | `Tüm değişkenlerin puanlarını toplayın` |
| `Adequate Information:` | `Yeterli Bilgi:` |
| `Adjusted competing-risks curve unavailable` | `Düzeltilmiş yarışan riskler eğrisi elde edilemedi` |
| `adjusted cumulative incidence` | `düzeltilmiş kümülatif insidans` |
| `Adjusted Cumulative Incidence for {variable}` | `{variable} için Düzeltilmiş Kümülatif İnsidans` |
| `Adjusted curves are based on the Fine-Gray subdistribution model and display ...` | `Düzeltilmiş eğriler Fine-Gray alt dağılım modeline dayanır ve nedene özgü sağ...` |
| `Adjusted curves are model-based survival or cumulative-incidence predictions ...` | `Düzeltilmiş eğriler, seçilen standardizasyon altında modele dayalı sağkalım v...` |
| `Adjusted curves unavailable` | `Düzeltilmiş eğriler elde edilemedi` |

