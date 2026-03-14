# Testing Lasso-Cox Regression Function

All test datasets are in `data-raw/` (CSV) or `data/` (RDA). Synthetic data can be generated via `data-raw/create_lassocox_test_data.R`. Real data: `inst/extdata/histopathology.rds`. Additional datasets: `lassocox_genomic` (n=80, 50 gene features), `lassocox_multicollinear` (n=180, 12 correlated predictors).

---

## 1. STANDARD CLINICAL (Green Suitability)

| # | Data | Variables | Options to Test |
|---|------|-----------|-----------------|
| 1 | `histopathology` (n=250) | elapsedtime: `OverallTime`, outcome: `Death`, outcomeLevel: `TRUE`, explanatory: Age, Sex, Grade, TStage, LVI, PNI, AntiX_intensity, AntiY_intensity | Default settings. `suitabilityCheck` (verify green banner), `lambda`: lambda.1se, `nfolds`: 10, `standardize`: TRUE, `cv_plot`, `coef_plot`, `survival_plot`. Toggle `showExplanations`, `showMethodologyNotes`, `includeClinicalGuidance`, `showVariableImportance`, `showModelComparison`. |
| 2 | `lassocox_lung_cancer` (n=200) | elapsedtime: `follow_up_months`, outcome: `progression`, outcomeLevel: `Yes`, explanatory: age, gender, smoking_status, histology, stage, tumor_size_cm, ecog_performance_status, hemoglobin_g_dl, wbc_count_k_ul, platelet_count_k_ul, creatinine_mg_dl, treatment_type | 12 mixed predictors (continuous + categorical). `lambda`: lambda.min vs lambda.1se comparison. `riskScore` output variable. |

**Options covered:** `elapsedtime`, `outcome`, `outcomeLevel`, `explanatory`, `lambda`, `nfolds`, `standardize`, `suitabilityCheck`, `cv_plot`, `coef_plot`, `survival_plot`, `riskScore`, `showExplanations`, `showMethodologyNotes`, `includeClinicalGuidance`, `showVariableImportance`, `showModelComparison`

---

## 2. HIGH-DIMENSIONAL (p >> n)

| # | Data | Variables | Options to Test |
|---|------|-----------|-----------------|
| 3 | `lassocox_breast_cancer` (n=120) | elapsedtime: `survival_months`, outcome: `death`, outcomeLevel: `Dead`, explanatory: age, stage, grade, er_status, her2_status, GENE_001–GENE_200 | 200+ predictors. `suitabilityCheck`: verify regularization strongly indicated (green). `lambda`: lambda.1se (should select sparse set). `nfolds`: 5 (small n). |
| 3b | `lassocox_genomic` (n=80) | elapsedtime: survival time variable, outcome: event variable, outcomeLevel: appropriate level, explanatory: all 50 gene features | 50 gene features, p >> n. `suitabilityCheck`: verify EPV and regularization checks. `nfolds`: 3 or 5. |

**Options covered:** High-dimensional scenario, automatic variable selection, sparse signal recovery

---

## 3. SMALL SAMPLE (Yellow/Red Suitability)

| # | Data | Variables | Options to Test |
|---|------|-----------|-----------------|
| 4 | `lassocox_small_cohort` (n=75) | elapsedtime: `time_months`, outcome: `event_occurred`, outcomeLevel: `Yes`, explanatory: age, gender, biomarker_a, biomarker_b, biomarker_c, treatment_group, severity_score | Small sample with high censoring. `suitabilityCheck`: verify sample size yellow, event rate warnings. `nfolds`: 3. |
| 5 | Synthetic (n=20, 10 vars) | Create inline: 6 events, 14 censored, 10 numeric predictors | `suitabilityCheck`: verify red EPV, red sample size. `lambda`: lambda.min. `nfolds`: 3. |

**Options covered:** Small sample handling, CV fold reduction, suitability red/yellow verdicts

---

## 4. MULTICOLLINEARITY

| # | Data | Variables | Options to Test |
|---|------|-----------|-----------------|
| 6 | `lassocox_cardiovascular` (n=150) | elapsedtime: `time_to_event_months`, outcome: `cv_event`, outcomeLevel: `Event`, explanatory: all clinical + lab + medication variables | Correlated predictors (systolic/diastolic BP, total/HDL/LDL cholesterol). `suitabilityCheck`: verify collinearity detection and top correlated pairs listed. |
| 6b | `lassocox_multicollinear` (n=180) | elapsedtime: time variable, outcome: event variable, explanatory: all 12 correlated predictors | Correlated predictors. `suitabilityCheck`: verify collinearity detection with specific pair names. |
| 7 | Synthetic (n=100, r>0.95 pair) | Create: var1 = rnorm, var2 = var1 + noise(0.1), var3, var4 | `suitabilityCheck`: verify yellow/red collinearity with specific pair names. Recommendation to use Elastic Net. |

**Options covered:** Multicollinearity detection, within-factor correlation exclusion, Elastic Net recommendation

---

## 5. ALL CATEGORICAL PREDICTORS

| # | Data | Variables | Options to Test |
|---|------|-----------|-----------------|
| 8 | Synthetic (n=100) | 3 factor variables: grade (Low/Med/High), stage (I/II/III), treatment (A/B) | `suitabilityCheck`: verify collinearity check handles dummy variables correctly (within-factor correlations excluded). Design matrix expansion. |

**Options covered:** Factor variable handling, model.matrix dummy creation, collinearity within-factor exclusion

---

## 6. SUITABILITY TOGGLE

| # | Data | Options to Test |
|---|------|-----------------|
| 9 | Any dataset | `suitabilityCheck = FALSE`: verify suitabilityReport is hidden (`visible: FALSE`), analysis still runs normally. |
| 10 | Any dataset | `suitabilityCheck = TRUE`: verify suitabilityReport appears before model results. |

---

## 7. LAMBDA SELECTION COMPARISON

| # | Data | Options to Test |
|---|------|-----------------|
| 11 | `histopathology` | `lambda = "lambda.min"`: more variables selected, lower bias, higher variance. |
| 12 | `histopathology` | `lambda = "lambda.1se"`: fewer variables, more parsimonious, better generalizability. Compare coefficient counts between the two. |

---

## 8. PLOT RENDERING (Protobuf Fix Verification)

| # | Data | Options to Test |
|---|------|-----------------|
| 13 | Any dataset with selected vars | `cv_plot = TRUE`: verify CV plot renders with error bars, lambda.min (blue) and lambda.1se (green) lines. |
| 14 | Any dataset with selected vars | `coef_plot = TRUE`: verify coefficient bar chart renders with red (risk) / blue (protective) colors. |
| 15 | Any dataset with selected vars | `survival_plot = TRUE`: verify KM survival curves by risk groups with risk table and p-value. |
| 16 | Any dataset | Toggle each plot off: `cv_plot = FALSE`, `coef_plot = FALSE`, `survival_plot = FALSE` — verify plots are hidden. |

**Key verification:** All 3 plots should render without protobuf serialization errors. Previous bug: `setState(results)` passed glmnet objects with function references.

---

## 9. EXPLANATORY OUTPUT OPTIONS

| # | Data | Options to Test |
|---|------|-----------------|
| 17 | Any dataset | `showExplanations = TRUE`: verify LASSO explanation HTML appears. When combined with `cv_plot`/`coef_plot`/`survival_plot`, additional plot-specific explanations appear. |
| 18 | Any dataset | `showMethodologyNotes = TRUE`: verify technical methodology HTML appears. |
| 19 | Any dataset | `includeClinicalGuidance = TRUE`: verify clinical interpretation guide with C-index table, HR interpretation, and reporting recommendations. |
| 20 | Any dataset with selected vars | `showVariableImportance = TRUE`: verify variable importance table with importance scores, selection frequency, stability rank. |
| 21 | Any dataset with selected vars | `showModelComparison = TRUE`: verify LASSO vs Standard Cox comparison table. |
| 21b | Any dataset with selected vars | `showSummary = TRUE`: verify natural-language summary paragraph appears with sample size, events, selected variables, C-index, and HR text. |
| 21c | `histopathology` | `random_seed = 42` vs `random_seed = 99999`: verify different seeds produce different variable selections. Same seed produces identical results. |

---

## 10. EDGE CASES

| # | Data | Options to Test |
|---|------|-----------------|
| 22 | Synthetic (n=50) with all censored | `status` all 0: verify error message about too few events (<5). |
| 23 | Synthetic with 1 explanatory | Single variable: verify error "at least 2 explanatory variables required." |
| 24 | Synthetic with constant variable | One variable all same value: verify error about constant variables. |
| 25 | Synthetic with zero times | Some `time = 0`: verify warning about zero values. |
| 26 | Large dataset (n=500, p=3) | LASSO selects no variables with lambda.1se → verify fallback to lambda.min, warning message shown. |
| 27 | Missing data in time | NAs in elapsedtime: verify error "Time variable contains missing values." |

---

## 11. RISK SCORE OUTPUT

| # | Data | Options to Test |
|---|------|-----------------|
| 28 | `histopathology` | `riskScore`: verify computed variable added to dataset. Check length matches original data (with NAs for excluded rows). |

---

## COMPLETE OPTION COVERAGE CHECKLIST

- [x] `elapsedtime` — all tests
- [x] `outcome` — all tests
- [x] `outcomeLevel` — all tests
- [x] `explanatory` — all tests
- [x] `lambda` (lambda.min / lambda.1se) — #11, #12
- [x] `nfolds` (3 / 5 / 10) — #1, #3, #4, #5
- [x] `standardize` (TRUE / FALSE) — #1
- [x] `suitabilityCheck` (TRUE / FALSE) — #9, #10, all scenarios
- [x] `cv_plot` — #13, #16
- [x] `coef_plot` — #14, #16
- [x] `survival_plot` — #15, #16
- [x] `riskScore` — #28
- [x] `showExplanations` — #17
- [x] `showMethodologyNotes` — #18
- [x] `includeClinicalGuidance` — #19
- [x] `showVariableImportance` — #20
- [x] `showSummary` — #21b
- [x] `showModelComparison` — #21
- [x] `random_seed` — #21c
