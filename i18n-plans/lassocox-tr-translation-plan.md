# Internationalization (i18n) Translation Plan: lassocox → Turkish (TR)

## Analysis: LASSO Cox Regression

**Description**: High-dimensional variable selection and L1-penalized Cox proportional hazards regression for survival outcomes.

### 0) Argument Normalization

- **Sanitized Function**: `lassocox`
- **YAML Schema Files**:
  - `jamovi/lassocox.a.yaml` (Options schema)
  - `jamovi/lassocox.u.yaml` (UI layout schema)
  - `jamovi/lassocox.r.yaml` (Results presentation schema)
- **R Backend File**:
  - `R/lassocox.b.R` (Core implementation)

---

## 1) Translation Metrics

- **Total Function-Specific Strings**: 247
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
| `{n} rows excluded; {constants} constant predictors or columns removed` | `{n} satır dışlandı; {constants} sabit yordayıcı veya sütun çıkarıldı` |
| `Absolute event probabilities require a baseline survival estimate and evaluat...` | `Mutlak olay olasılıkları, başlangıç sağkalım tahminini ve klinik açıdan anlam...` |
| `Analysis Error` | `Analiz hatası` |
| `Analysis Notes` | `Analiz notları` |
| `Apparent C-index` | `Görünür C-indeksi` |
| `At least two non-constant encoded predictor columns are required by this LASS...` | `Bu LASSO motoru en az iki sabit olmayan kodlanmış yordayıcı sütunu gerektirir.` |
| `At most 30 paths are shown, ranked by summed absolute coefficients across the...` | `Yol boyunca mutlak katsayı toplamına göre sıralanan en fazla 30 yol gösterili...` |
| `Bar colors indicate higher or lower fitted hazard. Ordering uses \|coefficien...` | `Çubuk renkleri daha yüksek veya düşük tahmini hazardı gösterir. Sıralama \|ka...` |
| `Blue: lambda.min, Green: lambda.1se` | `Mavi: lambda.min, yeşil: lambda.1se` |
| `Both optional comparison rows are unpenalized development-data fits. Their AI...` | `İsteğe bağlı iki karşılaştırma satırı da geliştirme verisinde kurulan cezasız...` |
| `Both rows are unpenalized Cox fits on the same development data. The first is...` | `İki satır da aynı geliştirme verisinde kurulan cezasız Cox modelleridir. İlk ...` |
| `Cannot determine censored level: all observed levels equal the event level.` | `Sansür düzeyi belirlenemedi: gözlenen tüm düzeyler olay düzeyine eşit.` |
| `Cannot form stratified cross-validation folds: at least 3 folds require at le...` | `Tabakalı çapraz doğrulama katları oluşturulamıyor: en az 3 kat için en az 3 o...` |
| `Categorical predictors are expanded into indicator columns and those columns ...` | `Kategorik yordayıcılar gösterge sütunlarına açılır ve bu sütunlar ayrı ayrı s...` |
| `Categorical predictors are represented by indicator columns and LASSO selects...` | `Kategorik yordayıcılar gösterge sütunlarıyla temsil edilir. LASSO, faktörü gr...` |
| `Censored Level Used` | `Kullanılan sansür düzeyi` |
| `Censoring Rate` | `Sansür oranı` |
| `Check` | `Kontrol` |
| `Clinical interpretation and validation` | `Klinik yorumlama ve doğrulama` |
| `Coefficient and Hazard Ratio are from the penalized LASSO Cox fit at the sele...` | `Katsayı ve hazard oranı, seçilen lambda değerindeki cezalı LASSO Cox modeline...` |
| `Coefficients are on the original variable scale.` | `Katsayılar özgün değişken ölçeğindedir.` |
| `Column Selection Proportion` | `Sütun seçilme oranı` |
| `Complete data with no constant predictors.` | `Veriler tamdır ve sabit yordayıcı yoktur.` |
| `Complete-case analysis excluded {n} rows ({pct}%). Even a small excluded frac...` | `Tam olgu analizi {n} satırı (%{pct}) dışladı. Eksiklik bilgilendirici olduğun...` |
| `Consider an analysis that supports grouped or elastic-net penalties when corr...` | `İlişkili yordayıcıların birlikte korunması gerekiyorsa grup cezası veya elast...` |
| `Could not assess` | `Değerlendirilemedi` |
| `Could not compute` | `Hesaplanamadı` |
| `Could not create event/censor-stratified cross-validation folds. Reduce the r...` | `Olay ve sansüre göre tabakalı çapraz doğrulama katları oluşturulamadı. İstene...` |
| `Cox refit did not converge cleanly: {message}` | `Cox yeniden uyumlaması düzgün biçimde yakınsamadı: {message}` |
| `Cox refit returned a non-finite log-likelihood.` | `Cox yeniden uyumlaması sonlu olmayan bir log-olabilirlik döndürdü.` |
| `Cox refit returned non-finite coefficient estimates.` | `Cox yeniden uyumlaması sonlu olmayan katsayı tahminleri döndürdü.` |
| `Cross-validation failed. Check data quality and sample size.` | `Çapraz doğrulama başarısız oldu. Veri kalitesini ve örneklem büyüklüğünü kont...` |
| `Cross-validation folds are stratified by event status and use the recorded ra...` | `Çapraz doğrulama katları olay durumuna göre tabakalanır ve kaydedilen rastgel...` |
| `Cross-Validation Plot` | `Çapraz doğrulama grafiği` |
| `Dashed blue: lambda.min; dotted orange: lambda.1se` | `Kesikli mavi: lambda.min; noktalı turuncu: lambda.1se` |

