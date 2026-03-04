# Testing Agreement Function

All files are in `data-raw/non-rda/`. Open the `.omv` (or `.csv`) in jamovi.

---

## 1. CATEGORICAL (Default Kappa)

| # | File | Raters | Options to Test |
|---|------|--------|-----------------|
| 1 | `agreement_pathology` | Pathologist1, Pathologist2 | Default kappa, `sft` (Contingency Table), `pabak`, `gwet`, `specificAgreement` (positiveCategory: blank, allCategories: on), `raterBias`, `bhapkar`, `stuartMaxwell`, `agreementHeatmap`. Subgroup var: `specimen_type` |
| 2 | `agreement_binary` | PathologistX, PathologistY | Default kappa (binary), `specificAgreement` (positiveCategory: "Positive" for PSA/NSA), `pabak`, `specificConfidenceIntervals`. Subgroup var: `specimen_quality` |
| 3 | `agreement_ordinal` | PathologistA, PathologistB | `wght`: Linear then Quadratic, `kendallW`, `robinsonA`, `meanSpearman`, `showLevelInfo`. Subgroup var: `tumor_site` |

**Options covered:** `vars`, `wght`, `sft`, `pabak`, `gwet`, `gwetWeights`, `specificAgreement`, `specificPositiveCategory`, `specificAllCategories`, `specificConfidenceIntervals`, `raterBias`, `bhapkar`, `stuartMaxwell`, `kendallW`, `robinsonA`, `meanSpearman`, `agreementHeatmap`, `heatmapColorScheme`, `heatmapShowPercentages`, `heatmapShowCounts`, `heatmapAnnotationSize`, `showLevelInfo`, `agreementBySubgroup`, `subgroupVariable`

---

## 2. THREE+ RATERS (Fleiss / Light)

| # | File | Raters | Options to Test |
|---|------|--------|-----------------|
| 4 | `agreement_threeRater` | Rater1, Rater2, Rater3 | Fleiss kappa (default for 3+), `exct` (Exact kappa), `lightKappa`, `multiAnnotatorConcordance` (predictionColumn: 1), `finn` (finnLevels: 3, finnModel: oneway then twoway). Subgroup var: `tissue_site` |
| 5 | `agreement_multiRater` | SeniorPath1, SeniorPath2, MidLevelPath, JuniorPath1, JuniorPath2 | Fleiss kappa (5 raters), `lightKappa`, `raterClustering` (clusterMethod: hierarchical, showDendrogram, showClusterHeatmap), `caseClustering`, `raterProfiles` (raterProfileType: boxplot/violin/barplot, raterProfileShowPoints). Subgroup var: `difficulty` |

**Options covered:** `exct`, `lightKappa`, `finn`, `finnLevels`, `finnModel`, `multiAnnotatorConcordance`, `predictionColumn`, `raterClustering`, `clusterMethod`, `clusterDistance`, `clusterLinkage`, `nClusters`, `showDendrogram`, `showClusterHeatmap`, `caseClustering`, `caseClusterMethod`, `caseClusterDistance`, `caseClusterLinkage`, `nCaseClusters`, `showCaseDendrogram`, `showCaseClusterHeatmap`, `raterProfiles`, `raterProfileType`, `raterProfileShowPoints`, `subgroupForestPlot`, `subgroupMinCases`

---

## 3. CONTINUOUS (ICC / Bland-Altman / CCC)

| # | File | Raters | Options to Test |
|---|------|--------|-----------------|
| 6 | `agreement_continuous` | MeasurementA, MeasurementB | `icc` (cycle through all 6 `iccType` models), `linCCC`, `blandAltmanPlot` (`baConfidenceLevel`, `proportionalBias`), `tdi` (`tdiCoverage`, `tdiLimit`), `meanPearson`, `maxwellRE`, `iota` (`iotaStandardize`). Verify: kappa hidden, continuous guidance message shown. Subgroup var: `tumor_type` |
| 7 | `pathology_agreement_main` | Ki67_HALO, Ki67_Aiforia, Ki67_Manual | `icc` (3 continuous raters), `meanPearson`, `raterProfiles`, `raterClustering`, `kripp` (krippMethod: interval) |
| 8 | `pathology_agreement_multimethod` | Ki67_HALO, Ki67_Aiforia, Ki67_ImageJ, Ki67_Manual | `icc` (4 raters), `raterClustering`, `caseClustering`, `bootstrapCI` (`nBoot`: 500) |

**Options covered:** `icc`, `iccType` (icc11/icc21/icc31/icc1k/icc2k/icc3k), `linCCC`, `blandAltmanPlot`, `baConfidenceLevel`, `proportionalBias`, `tdi`, `tdiCoverage`, `tdiLimit`, `meanPearson`, `maxwellRE`, `iota`, `iotaStandardize`, `bootstrapCI`, `nBoot`

---

## 4. MISSING DATA

| # | File | Raters | Options to Test |
|---|------|--------|-----------------|
| 9 | `agreement_missing` | Rater1, Rater2, Rater3 | `kripp` (krippMethod: nominal — handles NAs natively), Fleiss kappa (verify missing data note), `bootstrapCI` |
| 10 | `pathology_agreement_missing` | HALO_Score, Aiforia_Score, ImageJ_Score | `kripp` (krippMethod: interval — continuous with NAs), `icc` (verify NA handling) |

**Options covered:** `kripp`, `krippMethod` (nominal/ordinal/interval/ratio), `bootstrap`

---

## 5. HIERARCHICAL / MULTI-CENTER

| # | File | Raters | Options to Test |
|---|------|--------|-----------------|
| 11 | `agreement_hierarchical` | HospitalA_Rater1, HospitalA_Rater2, HospitalB_Rater1, HospitalB_Rater2, HospitalC_Rater1, HospitalC_Rater2 | `hierarchicalKappa` (clusterVariable: `institution`), `clusterSpecificKappa`, `varianceDecomposition`, `testClusterHomogeneity`, `shrinkageEstimates`, `clusterRankings`, `iccHierarchical`, `multipleTestCorrection` (none/bonferroni/bh/holm) |
| 12 | `comprehensive_agreement_data` | Rater_1, Rater_2, Rater_3, Rater_4 | `hierarchicalKappa` (clusterVariable: `Institution`), `pairwiseKappa` (referenceRater: any rater, `rankRaters`). Subgroup var: `Difficulty` or `Specialty` |

**Options covered:** `hierarchicalKappa`, `clusterVariable`, `clusterSpecificKappa`, `varianceDecomposition`, `testClusterHomogeneity`, `shrinkageEstimates`, `clusterRankings`, `iccHierarchical`, `multipleTestCorrection`, `pairwiseKappa`, `referenceRater`, `rankRaters`

---

## 6. TEST-RETEST (Inter/Intra-Rater)

| # | File | Raters | Options to Test |
|---|------|--------|-----------------|
| 13 | `agreement_testRetest` | Rater1_T1, Rater1_T2, Rater2_T1, Rater2_T2, Rater3_T1, Rater3_T2 | `interIntraRater` (`interIntraSeparator`: "_"), `finn` (finnLevels: 3) |

**Options covered:** `interIntraRater`, `interIntraSeparator`

---

## 7. PAIRED AGREEMENT COMPARISON

| # | File | Raters (Condition A) | ConditionB Vars | Options to Test |
|---|------|----------------------|-----------------|-----------------|
| 14 | `agreement_paired_comparison` (NEW) | Manual_Rater1, Manual_Rater2 | AI_Rater1, AI_Rater2 | `pairedAgreementTest` (`conditionBVars`, `pairedBootN`: 1000). Subgroup var: `tumor_type` |

**Options covered:** `pairedAgreementTest`, `conditionBVars`, `pairedBootN`

---

## 8. MIXED-EFFECTS CONDITION COMPARISON

| # | File | Raters | Condition Variable | Options to Test |
|---|------|--------|--------------------|-----------------|
| 15 | `agreement_mixed_effects` (NEW) | Rater1, Rater2 | `condition` | `mixedEffectsComparison` (`conditionVariable`: `condition`), `icc`, `multipleTestCorrection` |
| 16 | `digital_pathology_validation` | Ki67_Manual, Ki67_AI_Assisted | `Pathologist_Experience` | `mixedEffectsComparison` (`conditionVariable`: `Pathologist_Experience`), `blandAltmanPlot`, `linCCC`. Subgroup var: `Institution` or `Tumor_Type` |

**Options covered:** `mixedEffectsComparison`, `conditionVariable`

---

## 9. AI VALIDATION

| # | File | Raters | Options to Test |
|---|------|--------|-----------------|
| 17 | `pathology_agreement_ai` | Ki67_AI, Ki67_Pathologist | `blandAltmanPlot`, `linCCC`, `icc`. Subgroup var: `TumorType` |

---

## 10. AGREEMENT HEATMAP (dedicated)

| # | File | Raters | Options to Test |
|---|------|--------|-----------------|
| 18 | `agreement_heatmap_test` (NEW) | Scorer_A, Scorer_B, Scorer_C | `agreementHeatmap` (`heatmapColorScheme`, `heatmapShowPercentages`, `heatmapShowCounts`, `heatmapAnnotationSize`), `sft`, ordinal `wght`: Linear |

---

## 11. EDGE CASES

| # | File | Raters | Options to Test |
|---|------|--------|-----------------|
| 19 | `agreement_perfect` | RaterA, RaterB | Perfect agreement (kappa=1 edge case) |
| 20 | `agreement_poor` | PathologistA, PathologistB | Poor agreement, `gwet` (stable with low kappa). Subgroup var: `difficulty_level` |
| 21 | `agreement_small` | Rater1, Rater2 | Small sample (n=30), `bootstrapCI` (nBoot: 500) |

---

## 12. COMPUTED VARIABLES & DISPLAY

| # | File | Options to Test |
|---|------|-----------------|
| 22 | `agreement_threeRater` (#4) | `consensusVar` (consensusName, consensusRule: majority/supermajority/unanimous, tieBreaker: exclude/first/lowest/highest) |
| 23 | `agreement_pathology` (#1) | `loaVariable` (detailLevel: simple/detailed, simpleThreshold, loaThresholds: custom/quartiles/tertiles, loaHighThreshold, loaLowThreshold, loaVariableName, showLoaTable) |
| 24 | Any file | `confLevel` (change from 0.95 to 0.90) |

**Options covered:** `consensusVar`, `consensusName`, `consensusRule`, `tieBreaker`, `loaVariable`, `detailLevel`, `simpleThreshold`, `loaThresholds`, `loaHighThreshold`, `loaLowThreshold`, `loaVariableName`, `showLoaTable`, `confLevel`

---

## 13. CONFUSION MATRIX

| # | File | Raters | Options to Test |
|---|------|--------|-----------------|
| 25 | `agreement_pathology` (#1) | Pathologist1, Pathologist2 | `confusionMatrix` (`confusionNormalize`: none/row/column) |

**Options covered:** `confusionMatrix`, `confusionNormalize`

---

## 14. SAMPLE SIZE CALCULATOR (data-independent)

| # | File | Options to Test |
|---|------|-----------------|
| 26 | Any file | `agreementSampleSize` (`ssMetric`: kappa/fleiss/icc, `ssKappaNull`, `ssKappaAlt`, `ssNRaters`, `ssNCategories`, `ssAlpha`, `ssPower`) |

**Options covered:** `agreementSampleSize`, `ssMetric`, `ssKappaNull`, `ssKappaAlt`, `ssNRaters`, `ssNCategories`, `ssAlpha`, `ssPower`

---

## 15. DISPLAY OPTIONS & GUIDES

| # | File | Options to Test |
|---|------|-----------------|
| 27 | Any file with analysis enabled | `showAbout` — verify all explanations appear when both the analysis and showAbout are checked |
| 28 | Any file | `showSummary` — verify plain-language interpretation appears |
| 29 | Any file (no analysis needed) | All 33 "When to use" guide checkboxes — tick each one and verify the explanation Html appears independently of `showAbout` |

**Guide checkboxes to test:**
- `showKrippGuide`, `showLightKappaGuide`, `showGwetGuide`, `showPABAKGuide`, `showFinnGuide`, `showKendallWGuide`
- `showICCGuide`, `showMeanPearsonGuide`, `showLinCCCGuide`, `showTDIGuide`, `showBlandAltmanGuide`, `showIotaGuide`
- `showRobinsonAGuide`, `showMeanSpearmanGuide`, `showMaxwellREGuide`, `showInterIntraRaterGuide`
- `showRaterBiasGuide`, `showBhapkarGuide`, `showStuartMaxwellGuide`, `showPairwiseKappaGuide`
- `showHierarchicalGuide`, `showMixedEffectsGuide`, `showConfusionMatrixGuide`, `showBootstrapCIGuide`
- `showConcordanceF1Guide`, `showSpecificAgreementGuide`, `showAgreementHeatmapGuide`
- `showRaterProfileGuide`, `showSubgroupGuide`, `showRaterClusterGuide`, `showCaseClusterGuide`
- `showPairedAgreementGuide`, `showSampleSizeGuide`

---

## NEW DATA FILES GENERATED

| File | Description | Columns |
|------|-------------|---------|
| `agreement_paired_comparison.csv` | 100 cases, 2 conditions (Manual vs AI), 2 raters each | case_id, tumor_type, Manual_Rater1, Manual_Rater2, AI_Rater1, AI_Rater2 |
| `agreement_mixed_effects.csv` | 80 cases x 2 conditions (Pre/Post Training) x 2 raters | case_id, condition, Rater1, Rater2 |
| `agreement_heatmap_test.csv` | 150 HER2 cases, 3 scorers, ordinal 4-level | case_id, specimen_type, Scorer_A, Scorer_B, Scorer_C |

---

## COMPLETE OPTION COVERAGE CHECKLIST

- [x] `vars` — all tests
- [x] `wght` (unweighted/equal/squared) — #3
- [x] `exct` — #4
- [x] `showLevelInfo` — #3
- [x] `kripp`, `krippMethod`, `bootstrap` — #9, #10
- [x] `lightKappa` — #4, #5
- [x] `gwet`, `gwetWeights` — #1, #20
- [x] `pabak` — #1, #2
- [x] `icc`, `iccType` — #6, #7, #8
- [x] `meanPearson` — #6, #7
- [x] `linCCC` — #6, #16, #17
- [x] `tdi`, `tdiCoverage`, `tdiLimit` — #6
- [x] `iota`, `iotaStandardize` — #6
- [x] `finn`, `finnLevels`, `finnModel` — #4, #13
- [x] `kendallW` — #3
- [x] `robinsonA` — #3
- [x] `meanSpearman` — #3
- [x] `raterBias` — #1
- [x] `bhapkar` — #1
- [x] `stuartMaxwell` — #1
- [x] `maxwellRE` — #6
- [x] `interIntraRater`, `interIntraSeparator` — #13
- [x] `blandAltmanPlot`, `baConfidenceLevel`, `proportionalBias` — #6, #16, #17
- [x] `agreementHeatmap`, `heatmapColorScheme`, `heatmapShowPercentages`, `heatmapShowCounts`, `heatmapAnnotationSize` — #1, #18
- [x] `pairwiseKappa`, `referenceRater`, `rankRaters` — #12
- [x] `hierarchicalKappa`, `clusterVariable`, `clusterSpecificKappa`, `varianceDecomposition`, `testClusterHomogeneity`, `shrinkageEstimates`, `clusterRankings`, `iccHierarchical` — #11
- [x] `mixedEffectsComparison`, `conditionVariable` — #15, #16
- [x] `multipleTestCorrection` — #11, #15
- [x] `confusionMatrix`, `confusionNormalize` — #25
- [x] `bootstrapCI`, `nBoot` — #8, #21
- [x] `multiAnnotatorConcordance`, `predictionColumn` — #4
- [x] `specificAgreement`, `specificPositiveCategory`, `specificAllCategories`, `specificConfidenceIntervals` — #1, #2
- [x] `raterClustering`, `clusterMethod`, `clusterDistance`, `clusterLinkage`, `nClusters`, `showDendrogram`, `showClusterHeatmap` — #5
- [x] `caseClustering`, `caseClusterMethod`, `caseClusterDistance`, `caseClusterLinkage`, `nCaseClusters`, `showCaseDendrogram`, `showCaseClusterHeatmap` — #5, #8
- [x] `raterProfiles`, `raterProfileType`, `raterProfileShowPoints` — #5, #7
- [x] `agreementBySubgroup`, `subgroupVariable`, `subgroupForestPlot`, `subgroupMinCases` — #1, #4, #5, #12
- [x] `pairedAgreementTest`, `conditionBVars`, `pairedBootN` — #14
- [x] `agreementSampleSize`, `ssMetric`, `ssKappaNull`, `ssKappaAlt`, `ssNRaters`, `ssNCategories`, `ssAlpha`, `ssPower` — #26
- [x] `consensusVar`, `consensusName`, `consensusRule`, `tieBreaker` — #22
- [x] `loaVariable`, `detailLevel`, `simpleThreshold`, `loaThresholds`, `loaHighThreshold`, `loaLowThreshold`, `loaVariableName`, `showLoaTable` — #23
- [x] `sft` — #1, #18
- [x] `showSummary` — #28
- [x] `showAbout` — #27
- [x] `confLevel` — #24
- [x] All 33 `show*Guide` checkboxes — #29
