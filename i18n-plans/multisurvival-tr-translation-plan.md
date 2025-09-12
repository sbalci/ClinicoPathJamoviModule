# Turkish Translation Plan for Multisurvival Module

## Overview
This document outlines the Turkish translation plan for the multisurvival module in ClinicoPathJamoviModule. The module provides comprehensive survival analysis capabilities including Cox regression, machine learning methods, and advanced features.

## Translation Statistics
- **Estimated Total Strings**: ~140-160 translatable strings
- **Complexity Level**: High (medical/statistical terminology)
- **Priority Areas**: Error messages, UI labels, analysis summaries

## Key Translation Areas

### 1. Core Analysis Terms
| English | Turkish | Context |
|---------|---------|---------|
| Multivariable Cox Regression Summary | Çok Değişkenli Cox Regresyon Özeti | Analysis header |
| Analysis Overview | Analiz Genel Görünümü | Summary section |
| Key Findings | Temel Bulgular | Results summary |
| Interpretation Guide | Yorumlama Kılavuzu | Help section |
| Model Performance | Model Performansı | Statistics |

### 2. Survival Analysis Terminology
| English | Turkish | Context |
|---------|---------|---------|
| Time-to-event outcome | Zamana bağlı sonuç | Variable description |
| Hazard ratio | Hazard oranı | Statistical measure |
| Confidence interval | Güven aralığı | Statistics |
| Survival time | Sağkalım zamanı | Time variable |
| Censored observations | Sansürlenmiş gözlemler | Data type |

### 3. Machine Learning Terms
| English | Turkish | Context |
|---------|---------|---------|
| Random Survival Forest | Rastgele Sağkalım Ormanı | ML method |
| XGBoost Survival Analysis | XGBoost Sağkalım Analizi | ML method |
| Support Vector Machine | Destek Vektör Makinesi | ML method |
| Deep Survival Learning | Derin Sağkalım Öğrenmesi | ML method |
| Cross-validation | Çapraz doğrulama | Validation method |
| Variable importance | Değişken önemi | Feature analysis |
| SHAP Values | SHAP Değerleri | Interpretability |

### 4. Error Messages and Warnings
| English | Turkish | Context |
|---------|---------|---------|
| Package not available | Paket mevcut değil | Installation error |
| Please install it using | Şunu kullanarak yükleyin | Installation instruction |
| Analysis could not be completed | Analiz tamamlanamadı | General error |
| Data preparation error | Veri hazırlama hatası | Data error |
| Model fitting failed | Model uydurma başarısız | Statistical error |

### 5. Statistical Interpretations
| English | Turkish | Context |
|---------|---------|---------|
| Factor increases the hazard | Faktör hazardı artırır | HR interpretation |
| Factor decreases the hazard | Faktör hazardı azaltır | HR interpretation |
| No association between factor and event timing | Faktör ve olay zamanlaması arasında ilişki yok | HR interpretation |
| Statistically significant associations | İstatistiksel olarak anlamlı ilişkiler | Significance |
| Strongest association | En güçlü ilişki | Effect ranking |

### 6. Medical Decision Analysis
| English | Turkish | Context |
|---------|---------|---------|
| Nomogram Analysis | Nomogram Analizi | Prediction tool |
| Risk Prediction | Risk Tahmini | Clinical outcome |
| Individual risk stratification | Bireysel risk sınıflandırması | Personalized medicine |
| Calibration and validation procedures | Kalibrasyon ve doğrulama prosedürleri | Model validation |
| Decision tree analysis | Karar ağacı analizi | Clinical decisions |

### 7. Advanced Features
| English | Turkish | Context |
|---------|---------|---------|
| Time-dependent covariates | Zamana bağlı kovaryatlar | Advanced modeling |
| Proportional hazards assumption | Orantılı hazard varsayımı | Model assumption |
| Competing risks analysis | Rekabet eden riskler analizi | Complex outcomes |
| Frailty models | Kırılganlık modelleri | Random effects |
| Splines for non-proportional hazards | Orantısız hazardlar için splineler | Flexible modeling |

## Implementation Notes

### Clinical Context Considerations
- Turkish medical terminology should follow Turkish Medical Association standards
- Statistical terms should maintain consistency with Turkish statistical literature
- Time-related expressions need careful attention to Turkish temporal constructions

### Technical Requirements
- All strings are wrapped with .() function for jmvcore internationalization
- Long HTML content requires careful string concatenation in Turkish
- Medical abbreviations may need localization (HR vs HO for hazard ratio)

### Translation Validation
- Review by Turkish-speaking biostatistician recommended
- Clinical terminology validation with medical professionals
- Statistical accuracy verification for complex mathematical expressions

## File Structure
- **Source File**: `R/multisurvival.b.R` (5958 lines)
- **Translation Files**: Will be generated using jmvtools::i18nCreate()
- **Catalog Location**: `po/tr/LC_MESSAGES/`

## Priority Implementation Order
1. **High Priority**: Error messages and warnings (immediate user feedback)
2. **Medium Priority**: Analysis summaries and interpretations (clinical understanding)
3. **Lower Priority**: Advanced feature descriptions (specialized usage)

## Quality Assurance
- All translated strings should maintain medical accuracy
- Statistical terminology consistency across all modules
- User interface language should be accessible to Turkish medical professionals
- Technical accuracy validation for mathematical expressions

## Completion Status
- ✅ .() wrappers applied to all user-visible strings in multisurvival.b.R
- ⏳ Translation catalog generation pending
- ⏳ Turkish translation creation pending
- ⏳ Medical terminology review pending

---
*This plan serves as a comprehensive guide for Turkish localization of the multisurvival module, ensuring both linguistic accuracy and medical appropriateness.*