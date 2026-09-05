# Internationalization (i18n) Translation Plan: oddsratio → Turkish (TR)

## Analysis: Odds Ratio Calculation

**Description**: Calculates odds ratios, relative risks, and cross-tabulations for binary and categorical clinical risk factors.

### 0) Argument Normalization

- **Sanitized Function**: `oddsratio`
- **YAML Schema Files**:
  - `jamovi/oddsratio.a.yaml` (Options schema)
  - `jamovi/oddsratio.u.yaml` (UI layout schema)
  - `jamovi/oddsratio.r.yaml` (Results presentation schema)
- **R Backend File**:
  - `R/oddsratio.b.R` (Core implementation)

---

## 1) Translation Metrics

- **Total Function-Specific Strings**: 108
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
| `After removing incomplete cases for the selected model, the outcome must reta...` | `Seçilen model için eksik olgular çıkarıldıktan sonra, sonuç değişkeni seçilen...` |
| `Automatic detection - first match ({language})` | `Otomatik algılama - ilk eşleşme ({language})` |
| `Automatic detection ({language})` | `Otomatik algılama ({language})` |
| `Automatic Predictor Level Detection` | `Otomatik Belirteç Düzeyi Algılama` |
| `Based on {n} observations with both the outcome and this predictor recorded; ...` | `Hem sonuç hem de bu belirtecin kaydedildiği {n} gözleme dayanmaktadır; yukarı...` |
| `Borderline events-per-variable (EPV ≈ {epv}). Interpret odds ratios with caut...` | `Sınırda değişken başına olay sayısı (EPV ≈ {epv}). Odds oranlarını dikkatle y...` |
| `Check that the positive outcome level is correct for your study` | `Pozitif sonuç düzeyinin çalışmanız için doğru olduğunu kontrol edin` |
| `Contingency Table:` | `Kontenjans Tablosu:` |
| `Could not map some explanatory variables after cleaning: {variables}.` | `Temizleme sonrasında bazı açıklayıcı değişkenler eşleştirilemedi: {variables}.` |
| `Critical validation errors detected: {errors}. Ensure the outcome variable ha...` | `Kritik doğrulama hataları tespit edildi: {errors}. Sonuç değişkeninin tam ola...` |
| `Default (second factor level)` | `Varsayılan (ikinci faktör düzeyi)` |
| `Diagnostic metrics for '{variable}' require paired complete observations with...` | `'{variable}' için tanısal metrikler, tam olarak iki gözlemlenen belirteç düze...` |
| `Diagnostic metrics were not calculated because a valid paired 2\u{00D7}2 tabl...` | `Geçerli bir eşleştirilmiş 2\u{00D7}2 tablo oluşturulamadığı için tanısal metr...` |
| `Diagnostic metrics were not calculated because no diagnostic predictor was av...` | `Tanısal belirteç mevcut olmadığı için tanısal metrikler hesaplanmadı.` |
| `Diagnostic Metrics:` | `Tanısal Metrikler:` |
| `Diagnostic predictor label matches multiple variables; using '{variable}'.` | `Tanısal belirteç etiketi birden fazla değişkenle eşleşiyor; '{variable}' kull...` |
| `Diagnostic test performance metrics` | `Tanı testi performans metrikleri` |
| `Different languages/coding may require manual specification` | `Farklı diller/kodlamalar manuel belirleme gerektirebilir` |
| `Entered as continuous: {variables}. Each is modelled as one odds ratio per on...` | `Sürekli olarak girildi: {variables}. Her biri bir birimlik artış başına bir o...` |
| `Error` | `Hata` |
| `Error fitting Firth model: {message}` | `Firth modeli uydurulurken hata oluştu: {message}` |
| `Explanatory variable '{variable}' contains infinite values.` | `'{variable}' açıklayıcı değişkeni sonsuz değerler içeriyor.` |
| `Explanatory variable '{variable}' contains no non-missing values.` | `'{variable}' açıklayıcı değişkeni eksik olmayan hiçbir değer içermiyor.` |
| `Explanatory variable '{variable}' has {n} categories with fewer than 5 observ...` | `'{variable}' açıklayıcı değişkeninde 5'ten az gözleme sahip {n} kategori var....` |
| `Explanatory variable '{variable}' has {n} levels. Consider grouping categorie...` | `'{variable}' açıklayıcı değişkeni {n} düzeye sahip. Kategorileri gruplandırma...` |
| `Explanatory variable '{variable}' has no variation (all values are the same)....` | `'{variable}' açıklayıcı değişkeninde varyasyon yok (tüm değerler aynı). Model...` |
| `Explanatory variable '{variable}' is entered as continuous and has {n} value(...` | `'{variable}' açıklayıcı değişkeni sürekli olarak girilmiştir ve 5'ten az gözl...` |
| `Explanatory variable '{variable}' may contain extreme outliers ({n} potential...` | `'{variable}' açıklayıcı değişkeni aşırı uç değerler içerebilir ({n} olası uç ...` |
| `Firth penalized likelihood logistic regression used to reduce bias and handle...` | `Yanlılığı azaltmak ve olası ayrılmayı (separation) ele almak için Firth cezal...` |
| `General Statistics` | `Genel İstatistikler` |
| `If incorrect, use the 'Positive Outcome Level' dropdown to specify the correc...` | `Yanlışsa, doğru düzeyi belirtmek için 'Pozitif Sonuç Düzeyi' açılır menüsünü ...` |
| `Important:` | `Önemli:` |
| `Important: Please Verify These Interpretations` | `Önemli: Lütfen Bu Yorumları Doğrulayın` |
| `infinite (zero false results in this cell)` | `sonsuz (bu hücrede sıfır yanlış sonuç)` |
| `Information` | `Bilgi` |

