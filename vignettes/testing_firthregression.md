# Testing Firth's Penalized Likelihood Regression

Test datasets:
- `.rda` files: `data/` (for R package loading)
- `.omv` and `.csv` files: `data-raw/non-rda/` (for jamovi and manual testing)
- Generation script: `data-raw/create_firthregression_test_data.R` (seeds: 42/123/99)

---

## 1. LOGISTIC MODE -- Standard Data

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 1 | `firth_standard` | outcome: `mortality`, outcomeLevel: `Dead`, predictors: `age, grade, tumor_size, lvi, marker` | `analysisType=logistic`, all defaults |
| 2 | `firth_standard` | Same | `ciLevel=0.99` |
| 3 | `firth_standard` | Same | `ciMethod=wald` |
| 4 | `firth_standard` | Same | `compareStandard=TRUE` (verify bias_reduction column + comparison table) |
| 5 | `firth_standard` | Same | `compareStandard=FALSE` |
| 6 | `firth_standard` | Same | `separationCheck=TRUE` |
| 7 | `firth_standard` | Same | `showModelFit=TRUE` (5 rows) |
| 8 | `firth_standard` | Same | `showModelFit=FALSE` (0 rows) |

**Options covered:** `analysisType`, `outcome`, `outcomeLevel`, `predictors`, `ciLevel`, `ciMethod`, `compareStandard`, `separationCheck`, `showModelFit`

---

## 2. COX MODE -- Survival Data

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 9 | `firth_standard` | time: `follow_up_time`, outcome: `status`, outcomeLevel: `Dead`, predictors: `age, grade, tumor_size, lvi, marker` | `analysisType=cox` |
| 10 | `firth_standard` | Same | `analysisType=cox`, `compareStandard=TRUE` |
| 11 | `firth_standard` | Same | `analysisType=cox`, `ciMethod=wald` (should show info notice about profile CIs) |

**Options covered:** `analysisType=cox`, `time`

---

## 3. SEPARATION & RARE EVENTS

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 12 | `firth_separation` | outcome: `outcome`, outcomeLevel: `Recurrence`, predictors: `age, bmi, grade, margin_positive` | `separationCheck=TRUE`, `compareStandard=TRUE` |
| 13 | `firth_separation` | Same but 3 predictors: `age, bmi, grade` | `suitabilityCheck=TRUE` (low EPV) |
| 14 | `firth_separation` | Only `age, margin_positive` | `separationCheck=TRUE` (margin_positive has separation) |

**Options covered:** Separation detection, rare events, suitability with low EPV

---

## 4. SMALL COX COHORT

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 15 | `firth_smallcox` | time: `time`, outcome: `status`, outcomeLevel: `Dead`, predictors: `age, treatment, biomarker` | `analysisType=cox`, `suitabilityCheck=TRUE` |

---

## 5. DISPLAY OPTIONS

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 16 | `firth_standard` | Same as #1 | `suitabilityCheck=TRUE` |
| 17 | `firth_standard` | Same | `suitabilityCheck=FALSE` |
| 18 | `firth_standard` | Same | `forestPlot=TRUE` |
| 19 | `firth_standard` | Same | `separationPlot=TRUE` |
| 20 | `firth_standard` | Same | `showSummary=TRUE` |
| 21 | `firth_standard` | Same | `showExplanations=TRUE` |
| 22 | `firth_standard` | Same | All outputs enabled |

**Options covered:** `suitabilityCheck`, `forestPlot`, `separationPlot`, `showSummary`, `showExplanations`

---

## 6. EDGE CASES

| # | File | Variables | Options to Test | Expected Behavior |
|---|------|-----------|-----------------|-------------------|
| 23 | -- | No data | -- | Welcome HTML |
| 24 | `firth_standard` | outcome only, no predictors | -- | Welcome HTML |
| 25 | `firth_standard` | Single predictor: `age` | Default | Should work |
| 26 | `firth_standard` | Missing data (~15% in marker) | Default | Listwise deletion |
| 27 | `firth_standard` | Cox without time variable | `analysisType=cox`, no time | Error notice |

---

## AVAILABLE TEST DATASETS

| File | N | Events | Mode | Key Features |
|------|---|--------|------|--------------|
| `firth_standard` | 120 | 58 mortality / 31 survival | Both | Balanced, no separation, mixed predictors |
| `firth_separation` | 80 | 11 (~14%) | Logistic | Rare events, `margin_positive` has separation |
| `firth_smallcox` | 50 | 23 | Cox | Small cohort |

**File locations:**
- RDA: `data/firth_standard.rda`, `data/firth_separation.rda`, `data/firth_smallcox.rda`
- CSV/OMV: `data-raw/non-rda/firth_*.csv` (`.omv`)

---

## COMPLETE OPTION COVERAGE CHECKLIST

- [x] `analysisType` (logistic) -- test #1
- [x] `analysisType` (cox) -- test #9
- [x] `time` -- tests #9-11, #15
- [x] `outcome` -- tests #1-27
- [x] `outcomeLevel` -- tests #1-27
- [x] `predictors` -- tests #1-27
- [x] `suitabilityCheck` -- tests #16, #17
- [x] `ciLevel` -- test #2
- [x] `ciMethod` (profile) -- test #1
- [x] `ciMethod` (wald) -- tests #3, #11
- [x] `separationCheck` -- tests #6, #12, #14
- [x] `compareStandard` -- tests #4, #5
- [x] `showModelFit` -- tests #7, #8
- [x] `forestPlot` -- test #18
- [x] `separationPlot` -- test #19
- [x] `showSummary` -- test #20
- [x] `showExplanations` -- test #21
