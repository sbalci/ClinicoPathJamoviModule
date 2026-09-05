# meddecide Module — Master Turkish (TR) Internationalization Plan

## 1. Overview & Scope

This document details the complete internationalization (i18n) and Turkish (TR) translation architecture for the **meddecide** jamovi module and its 15 production analyses:

1. `agreement`: Comprehensive inter-rater and intra-rater agreement, Fleiss/Cohen kappa, Gwet's AC1/AC2, ICC, Bland-Altman, TDI, and variance decomposition.
2. `cotest`: Dual testing and co-testing evaluation for screening protocols.
3. `decision`: Medical decision analysis, 2x2 diagnostic performance metrics, likelihood ratios, and predictive values.
4. `decisioncalculator`: Diagnostic metric calculator from summary counts and prevalence.
5. `decisioncombine`: Combination test strategies (parallel, serial, belief functions, belief logic).
6. `decisioncompare`: Paired and unpaired diagnostic test comparison.
7. `decisioncurve`: Decision curve analysis (DCA), net benefit calculation, and clinical utility.
8. `enhancedROC`: Advanced receiver operating characteristic (ROC) curves, optimal cutpoint discovery, boot-strapping, and cost-benefit curves.
9. `kappaSizeCI`: Sample size calculation for confidence interval estimation of Cohen's kappa.
10. `kappaSizeFixedN`: Power calculation for fixed sample size in kappa studies.
11. `kappaSizePower`: Sample size calculation for hypothesis testing of kappa.
12. `lassologistic`: Penalized LASSO logistic regression for diagnostic feature selection and risk modeling.
13. `nogoldstandard`: Diagnostic evaluation in the absence of a gold standard (latent class analysis, Hui-Walter models).
14. `psychopdaROC`: Psychometric diagnostic accuracy and ROC analysis.
15. `sequentialtests`: Multi-stage sequential testing strategies and stopping rules.

---

## 2. Extraction Hygiene & Bracket-Tail Trap Resolution

In accordance with the canonical `prepare-translation` playbook and jamovi compiler standards:
- **Bracket-Tail Traps Eliminated**:
  - `R/decision.b.R`: Fixed trailing brackets inside `.()` wrapper calls.
  - `jamovi/agreement.u.yaml`: Cleaned up formatting and trailing brackets.
- **Spliced Concatenations Refactored**:
  - `R/decisioncompare.b.R`: Eliminated string concatenation across translation boundaries, replacing them with unified format strings (`sprintf()`).
- **Zero msgctxt Traps**:
  - Validated that `jamovi/i18n/catalog.pot` contains 0 `msgctxt` entries with dangling punctuation or bracket-tails.
- **Zero Drift Enforced**:
  - All source fixes applied in `meddecide` were mirrored identically to the umbrella repository `ClinicoPathJamoviModule`.

---

## 3. Catalog Status & Quality Metrics

- **Catalog Total**: 3,996 translatable strings (plus PO header).
- **Translated Total**: 3,996 / 3,996 (100.0% completion).
- **Untranslated Count**: 0.
- **Fuzzy Messages**: 0.
- **Token Parity**: 100% verified across all format specifiers (`%s`, `%d`, `%.1f`, `%.3f`, `%.4g`, `%%`), brace tokens (`{var}`, `{n}`, unicode escapes `\u{2022}`, `\u{2192}`, `\u{00D7}`, `\u{03C1}`), and HTML tags (`<br>`, `<b>`, `</b>`).
- **Validation Engine**: Automated verification via `msgfmt -v -c -o /dev/null jamovi/i18n/tr.po` passing with 0 errors and 0 warnings.
- **Mirror Sync**: Synchronized 3,651 matching translation keys to `ClinicoPathJamoviModule/jamovi/i18n/tr.po`, bringing total translated keys in ClinicoPath to 7,178.

---

## 4. Standard Clinical & Statistical Turkish Terminology

| English Concept | Turkish (TR) Standard | Rationale & Context |
| :--- | :--- | :--- |
| `Interrater Reliability` | `Değerlendiriciler-Arası Güvenilirlik` | Standart epidemiyolojik ve biyofizik terimi |
| `Sensitivity` | `Duyarlılık` | Tıbbi tanısal test standardı |
| `Specificity` | `Özgüllük` | Tıbbi tanısal test standardı |
| `Positive Predictive Value (PPV)` | `Pozitif Öngörü Değeri (PÖD)` | Klinik epidemiyoloji |
| `Negative Predictive Value (NPV)` | `Negatif Öngörü Değeri (NÖD)` | Klinik epidemiyoloji |
| `Likelihood Ratio (+LR / -LR)` | `Olabilirlik Oranı (+OO / -OO)` | Tanısal test oranı |
| `Net Benefit` | `Net Fayda` | Karar eğrisi analizi (DCA) |
| `Area Under Curve (AUC)` | `Eğri Altındaki Alan (EAA)` | ROC eğrisi parametresi |
| `Cutoff / Threshold` | `Eşik Değer / Kesim Noktası` | Biyobelirteç ve test analizi |
| `Limits of Agreement (LoA)` | `Uyum Sınırları (LoA)` | Bland-Altman yöntem karşılaştırması |
| `Total Deviation Index (TDI)` | `Toplam Sapma İndeksi (TDI)` | Sürekli ölçüm uyumu |
| `Confidence Interval (CI)` | `Güven Aralığı (GA)` | İstatistiksel aralık tahmini |
| `Standard Error (SE)` | `Standart Hata (SH)` | Örnekleme hatası |
| `Disagreement Variance` | `Uyuşmazlık Varyansı` | Varyans ayrıştırması |
| `Prevalence` | `Prevalans / Yaygınlık` | Epidemiyolojik sıklık |
| `Gold Standard` | `Altın Standart` | Referans tanı yöntemi |
| `Latent Class Analysis` | `Gizil Sınıf Analizi` | Altın standart yokluğunda modelleme |

---

## 5. Verification & Test Suite Parity

- `devtools::test()` executed in `meddecide`:
  - 39 passed tests, 0 failures, 0 errors, 1 skipped.
- Catalog compilation:
  - `msgfmt -v -c -o /dev/null jamovi/i18n/tr.po` -> `3996 translated messages`.
