# Turkish Translation Suggestions for Agreement Function

## Statistical Method Names / İstatistiksel Yöntem Adları

| English | Turkish |
|---------|---------|
| Cohen's Kappa | Cohen Kappa |
| Fleiss' Kappa | Fleiss Kappa |
| Fleiss' Kappa (Exact) | Fleiss Kappa (Kesin) |
| Krippendorff's Alpha | Krippendorff Alfa |
| ICC Error | İKK Hatası |

## Error Messages / Hata Mesajları

| English | Turkish |
|---------|---------|
| ICC calculation failed: | İKK hesaplama hatası: |
| Krippendorff's alpha calculation failed | Krippendorff alfa hesaplaması başarısız |
| Krippendorff's alpha for non-nominal data requires full irr package implementation | Nominal olmayan veriler için Krippendorff alfa tam irr paketi gerektirir |
| Error calculating Krippendorff's alpha: | Krippendorff alfa hesaplama hatası: |
| Error calculating Gwet's coefficients: | Gwet katsayıları hesaplama hatası: |
| Error calculating PABAK: | PABAK hesaplama hatası: |
| Error in sample size planning: | Örneklem büyüklüğü planlaması hatası: |
| Error in bias analysis: | Yanlılık analizi hatası: |
| Error in trend analysis: | Trend analizi hatası: |
| Error in difficulty analysis: | Zorluk analizi hatası: |
| Error in stability analysis: | Kararlılık analizi hatası: |

## Agreement Levels / Uyum Düzeyleri

| English | Turkish |
|---------|---------|
| Cannot calculate | Hesaplanamıyor |
| Poor | Zayıf |
| Slight | Hafif |
| Fair | Orta |
| Moderate | İyi |
| Substantial | Önemli |
| Almost Perfect | Mükemmele Yakın |

## Consensus Analysis / Uzlaşma Analizi

| English | Turkish |
|---------|---------|
| Unanimous | Oybirliği |
| Majority | Çoğunluk |
| Super Majority | Üstün Çoğunluk |
| Tie | Beraberlik |
| No Consensus | Uzlaşma Yok |
| No consensus | Uzlaşma yok |
| Total Cases | Toplam Olgu |
| Consensus Achieved | Uzlaşma Sağlandı |
| Unanimous Agreement | Oybirliği Uyumu |
| Super Majority (≥2/3) | Üstün Çoğunluk (≥2/3) |
| Majority (≥50%) | Çoğunluk (≥%50) |
| Tied Cases | Berabere Olgular |
| Insufficient | Yetersiz |
| Multiple ties | Çoklu beraberlik |

## Diagnostic Style Analysis / Tanısal Stil Analizi

| English | Turkish |
|---------|---------|
| Not specified | Belirtilmemiş |
| Style | Stil |
| Mixed | Karışık |

## Notes and Warnings / Notlar ve Uyarılar

| English | Turkish |
|---------|---------|
| Note: Weighted kappa requires ordinal variables. Analysis performed with unweighted kappa instead. | Not: Ağırlıklı kappa sıralı değişkenler gerektirir. Analiz ağırlıksız kappa ile gerçekleştirildi. |

## Clinical Interpretations / Klinik Yorumlar

| English | Turkish |
|---------|---------|
| Could not calculate reliable agreement measure. Check your data for sufficient cases and category distribution. | Güvenilir uyum ölçütü hesaplanamadı. Yeterli olgu sayısı ve kategori dağılımı için verilerinizi kontrol edin. |
| Agreement is worse than chance - raters systematically disagree. Review rating criteria and consider additional training. | Uyum şanstan kötü - değerlendiriciler sistematik olarak anlaşmıyor. Derecelendirme kriterlerini gözden geçirin ve ek eğitim düşünün. |
| Agreement is <strong>poor</strong> - raters are essentially making independent judgments. Consider revising diagnostic criteria, providing additional training, or using consensus panels for critical diagnoses. | Uyum <strong>zayıf</strong> - değerlendiriciler temelde bağımsız kararlar veriyor. Tanı kriterlerini revize etmeyi, ek eğitim sağlamayı veya kritik tanılar için uzlaşma panelleri kullanmayı düşünün. |
| Agreement is <strong>fair</strong> - some consistency between raters but significant disagreement remains. Review cases with disagreement and consider standardizing evaluation procedures. | Uyum <strong>orta</strong> - değerlendiriciler arasında bir miktar tutarlılık var ancak önemli anlaşmazlık sürüyor. Anlaşmazlık olan olguları inceleyin ve değerlendirme prosedürlerini standartlaştırmayı düşünün. |
| Agreement is <strong>moderate</strong> - acceptable for many clinical applications but may need improvement for critical diagnoses. Consider additional training or clearer diagnostic guidelines. | Uyum <strong>iyi</strong> - birçok klinik uygulama için kabul edilebilir ancak kritik tanılar için iyileştirme gerekebilir. Ek eğitim veya daha net tanı kılavuzları düşünün. |
| Agreement is <strong>good</strong> - suitable for most clinical and research applications. This level indicates reliable inter-rater consistency. | Uyum <strong>önemli</strong> - çoğu klinik ve araştırma uygulaması için uygun. Bu düzey güvenilir değerlendiriciler arası tutarlılığı gösterir. |
| Agreement is <strong>excellent</strong> - raters show high consistency, suitable for all clinical applications including critical diagnoses and research studies. | Uyum <strong>mükemmel</strong> - değerlendiriciler yüksek tutarlılık gösteriyor, kritik tanılar ve araştırma çalışmaları dahil tüm klinik uygulamalar için uygun. |

## Usage Notes / Kullanım Notları

1. **Context-Sensitive Terms**: Some medical terms may need context-specific translations:
   - "Rater" can be "Değerlendirici", "Gözlemci", or "Uzman" depending on context
   - "Cases" can be "Olgular", "Vakalar", or "Örnekler"
   - "Categories" can be "Kategoriler", "Sınıflar", or "Gruplar"

2. **Statistical Terms**: Keep statistical terms consistent:
   - "Kappa" remains "Kappa" (internationally recognized)
   - "Alpha" becomes "Alfa" (Turkish spelling)
   - "ICC" becomes "İKK" (İntraklasse Korelasyon Katsayısı)

3. **HTML Tags**: When translating HTML content, preserve all HTML tags and only translate the text content.

4. **Percentage Symbols**: Use Turkish convention: %50 instead of 50%

5. **Mathematical Operators**: Keep mathematical symbols (≥, <, >, =) as they are universal.

## Implementation Priority

**High Priority (Core functionality):**
- Statistical method names
- Agreement levels
- Error messages
- Consensus terminology

**Medium Priority (User experience):**
- Clinical interpretations
- Notes and warnings

**Low Priority (Advanced features):**
- Diagnostic style analysis terms
- Specialized error messages

## Translation Validation

Before implementing, consider:
1. Medical terminology consistency with Turkish medical literature
2. Statistical terminology used in Turkish academic publications
3. User interface conventions in Turkish software applications
4. Clarity for both medical professionals and researchers