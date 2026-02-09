# Citation Review: Reproducibility of Ki67 Haralick Entropy as a Prognostic Marker in ER+/HER2- Breast Cancer

---

## ARTICLE SUMMARY

- **Title/Label**: Reproducibility of Ki67 Haralick entropy as a prognostic marker in estrogen receptor-positive HER2-negative breast cancer
- **Design & Cohort**: Retrospective cohort study; N = 254 patients with surgically resected primary invasive (stages I-III) ER+/HER2- BC from the National Cancer Institute (Vilnius, Lithuania, 2007-2014); 10-year follow-up; 34 BC-specific deaths. Ki-67 IHC (MIB-1, 1:200, Dako) analyzed independently on two DIA platforms: HALO (Indica Labs) and Aiforia (Helsinki, Finland). Hexagonal grid subsampling of WSIs for spatial heterogeneity analysis. Tumor tissue subsampled into halves, quadrants, and simulated core biopsies.
- **Key Analyses**:
  - Cross-platform reproducibility: Spearman correlation, ICC(3,1), Lin's CCC with bootstrap CIs, Bland-Altman analysis
  - Haralick texture entropy from hexagonal tile co-occurrence matrices as ITH indicator
  - Univariate and multivariable Cox regression for BCSS (CoxBoost with variable selection)
  - Harrell's C-index for model discrimination comparison
  - Optimal cutpoint determination (Cutoff Finder)
  - Kaplan-Meier with log-rank test for stratified survival analysis
  - Subsampling stability assessment (halves, quadrants, biopsy cores)

---

## ARTICLE CITATION

| Field | Value |
|-------|-------|
| Title | Reproducibility of Ki67 Haralick entropy as a prognostic marker in estrogen receptor-positive HER2-negative breast cancer |
| Journal | American Journal of Clinical Pathology |
| Year | 2025 |
| Volume | XX |
| Issue | XX |
| Pages | 1-14 |
| DOI | 10.1093/AJCP/AQAF081 |
| PMID | TODO |
| Publisher | Oxford University Press / American Society for Clinical Pathology |
| ISSN | TODO |
| First Author | Dovile Zilenaite-Petrulaitiene |
| Corresponding Author | Dovile Zilenaite-Petrulaitiene (dovile.zilenaite@vpc.lt) |
| Received / Accepted | March 27, 2025 / July 10, 2025 |
| License | CC BY 4.0 |

---

## Skipped Sources

*None -- PDF was read successfully (14 pages).*

---

## EXTRACTED STATISTICAL METHODS

| Method / Model | Role (primary/secondary) | Variants & Options | Assumptions/Diagnostics | References (sec/page) |
|---|---|---|---|---|
| Kolmogorov-Smirnov test | Preliminary -- normality assessment | Applied to Ki67% and Haralick entropy; all variables non-normal (P < .01) even after log transformation | Justifies use of nonparametric methods | Methods (p5) |
| Spearman rank correlation | Primary -- cross-platform concordance | r = 0.96 (Ki67%), r = 0.94 (Haralick entropy); P < .0001 | Nonparametric; no normality assumption required | Results (p6), Fig 2A,C |
| ICC(3,1) -- two-way mixed effects, single rater | Primary -- cross-platform reproducibility | ICC = 0.93 (95% CI, 0.91-0.95) for Ki67%; ICC = 0.94 (95% CI, 0.92-0.95) for Haralick entropy | Using psych R package; interpreted cautiously due to nonnormal data (per Koo & Li 2016 guidelines) | Methods (p5), Results (p6) |
| Lin's Concordance Correlation Coefficient (CCC) | Primary -- absolute agreement | CCC = 0.914 (95% CI, 0.878-0.944) for Ki67%; CCC = 0.923 (95% CI, 0.903-0.941) for entropy; 95% CI via nonparametric bootstrap (1000 replicates) | Measures precision and accuracy simultaneously; captures both bias and variability | Methods (p5), Results (p6), Fig 2A,C |
| Bland-Altman analysis | Primary -- limits of agreement | Ki67%: mean diff = -2.56, LoA = -12.57 to 7.45; Entropy: mean diff = 0.26, LoA = -0.69 to 1.21 | Visualizes systematic bias and spread of differences; paired measurements | Results (p6), Fig 2B,D |
| Wilcoxon signed-rank test | Secondary -- paired platform/region comparisons | Visual vs DIA Ki67% (P < .001); HALO vs Aiforia Ki67% (P > .05); region-to-region entropy (all P > .05) | Nonparametric paired test; appropriate for nonnormal data | Results (p5-6), Table 1 footnotes |
| Univariate Cox regression | Primary -- individual prognostic assessment | HR with 95% CI for BCSS; all Ki67% and Haralick entropy indicators tested individually; 34 events in 254 patients | Proportional hazards assumed; low event count limits model complexity | Results (p7), Table 2 |
| Multivariable Cox regression with CoxBoost | Primary -- independent predictor identification | Likelihood-based boosting for variable selection; optimCoxBoostPenalty for penalty tuning; 5-fold CV; LR test and C-index for model comparison; 21 models tested (Table 3) | CoxBoost handles high-dimensional variable selection; 5-fold CV mitigates overfitting | Methods (p5), Results (p7-8), Table 3 |
| Harrell's C-index | Primary -- model discrimination | C-index with 95% CI; visual Ki67%: 0.671 (0.623-0.726); HALO Ki67%: 0.687 (0.635-0.738); entropy models: 0.701-0.709 | Higher = better discrimination; 0.5 = chance; compared across nested models | Results (p7-8), Table 3 |
| Likelihood ratio (LR) test | Primary -- model fit comparison | LR values: 10.56 (model 1, visual) to 13.67 (model 4, HALO entropy); P values reported | Compares nested models; higher LR = better fit | Table 3 |
| Kaplan-Meier with log-rank test | Secondary -- stratified survival visualization | BCSS by optimal cutoff groups; HR and P values displayed on plots; risk tables included | Right-censored survival data | Results (p7), Fig 3 |
| Cutoff Finder (Budczies et al. 2012) | Secondary -- optimal dichotomization | Web tool applying log-rank test to find threshold maximizing BCSS separation; applied to each indicator | Data-driven cutpoint; risk of overfitting (cutoff from same data used for survival analysis) | Methods (p5) |
| Haralick texture entropy | Core methodological innovation | Co-occurrence matrix from hexagonal tile Ki67% (10 intervals: 0-10%, 10-20%, etc.); entropy = spatial heterogeneity measure | Hexagon side length = 262.5 um (1050 pixels); hexagons with <50 cells excluded; applied identically to HALO and Aiforia outputs | Methods (p4-5) |

---

## CLINICOPATH JAMOVI COVERAGE MATRIX

| Article Method | Jamovi Function(s) | Coverage | Notes / Workarounds |
|---|---|:---:|---|
| Spearman rank correlation | `pathologyagreement`, `jjscatterstats` | ✅ | `pathologyagreement` includes Spearman correlation for continuous measurements |
| ICC(3,1) -- single rater, two-way mixed | `agreement` (icc option, all 6 types), `icccoeff` | ✅ | `agreement` provides ICC(1,1) through ICC(3,k); `icccoeff` is dedicated ICC function |
| Lin's CCC with bootstrap CI | `agreement` (concordanceCCC), `pathologyagreement` | ✅ | `pathologyagreement` supports Lin's CCC; `agreement` includes it with BCa bootstrap CIs (implemented 2026-02-08) |
| Bland-Altman analysis | `agreement` (blandAltmanPlot), `pathologyagreement` | ✅ | Full Bland-Altman with LoA, mean difference, regression line |
| Wilcoxon signed-rank test | `jjhistostats` (ggwithinstats), base jamovi paired tests | ✅ | Available for paired nonparametric comparisons |
| Univariate Cox regression for BCSS | `survival` (Cox model with HR, 95% CI) | ✅ | Continuous and categorical covariates; cause-specific survival supported |
| Multivariable Cox regression | `survival` (multivariable Cox) | ✅ | Standard multivariable Cox with HR, CI, P values |
| CoxBoost (likelihood-based boosting for variable selection) | — | ❌ | Not available. `penalizedcox`, `lassocox` provide LASSO/elastic net but not boosting-based variable selection |
| Harrell's C-index | `concordanceindex`, `survival` | ✅ | C-index with 95% CI available; reported in survival model output |
| C-index comparison between models | — | 🟡 | C-indices reported per model but no formal statistical test comparing C-indices (e.g., Uno's test, DeLong-like test for survival) |
| Likelihood ratio test (model comparison) | `survival` (Cox model output) | ✅ | LR test is part of standard Cox regression output |
| Kaplan-Meier with log-rank | `survival` (sc, kmunicate) | ✅ | Full KM curves with risk tables, CI bands |
| Weighted log-rank tests | `survival` (weightedLogRank) | ✅ | Fleming-Harrington family implemented 2026-02-08 (just implemented!) |
| Optimal cutpoint (Cutoff Finder) | `optimalcutpoint` | ✅ | Survival-based optimal dichotomization with multiple methods |
| Kolmogorov-Smirnov normality test | base R / basic jamovi | ✅ | Available in descriptive statistics |
| Haralick texture entropy (spatial ITH from tile data) | `ihcheterogeneity` | 🟡 | `ihcheterogeneity` exists for tumor heterogeneity but does not compute Haralick texture features from co-occurrence matrices; the entropy calculation from region-level Ki67% data is a specialized image analysis preprocessing step |
| Cross-platform DIA reproducibility workflow | `pathologyagreement` | ✅ | `pathologyagreement` has presets for multi-platform validation (HALO, Aiforia, ImageJ, QuPath) |
| Subsampling stability analysis (region-level comparison) | `pathologyagreement` with stratification | 🟡 | Can compare measurements across regions using the same tools, but no automated "subsampling" workflow that splits data by spatial region |
| 5-fold cross-validation for Cox models | — | ❌ | No built-in cross-validation for survival model internal validation |

---

## CRITICAL EVALUATION

### Statistical Rigor Score: 🟢 Good (14/18)

| Criterion | Score | Notes |
|---|---|---|
| Sample size justification | 1/2 | No formal power analysis; 254 patients with 34 events is small for 21 multivariable models, but CoxBoost with CV partially mitigates |
| Multiple testing correction | 1/2 | 21 models explored in Table 3 without formal multiple comparison correction; but each model is presented as exploratory and variable selection is penalized via CoxBoost |
| Effect sizes with CIs | 2/2 | All HRs with 95% CIs; ICC and CCC with 95% CIs; bootstrap CIs for CCC (1000 replicates) |
| Assumption checking | 2/2 | Normality tested (KS test); nonparametric methods used throughout; ICC interpretation caveated per guidelines |
| Appropriate test selection | 2/2 | Excellent: nonparametric tests for nonnormal data; ICC + CCC + Bland-Altman triple assessment of agreement; CoxBoost for variable selection |
| Reproducibility | 2/2 | Two independent DIA platforms (HALO, Aiforia) analyzed blindly; same WSIs, same grid, same entropy pipeline |
| Handling of missing data | 1/2 | 10 patients excluded (neoadjuvant, distant mets, incomplete data, age <35); remaining 254 appear complete; no imputation needed |
| Model diagnostics | 2/2 | C-index reported for all models; LR test for model comparison; 5-fold CV for CoxBoost penalty optimization |
| Clinical significance assessment | 1/1 | HR = 2.64-2.76 for entropy (clinically meaningful); C-index improvement from 0.671 to 0.709 (meaningful discrimination gain) |
| External validity | 0/1 | No external validation cohort; single-institution study from Lithuania |
| Bias assessment | 0/1 | Cutoff values derived from the same cohort used for survival analysis (risk of optimistic bias); acknowledged in limitations |

### Strengths
1. **Triple agreement assessment** (Spearman + ICC + CCC + Bland-Altman) -- gold-standard reproducibility evaluation
2. **Two independent DIA platforms** analyzed blindly (HALO and Aiforia) -- strong cross-platform validation
3. **Systematic subsampling** (halves, quadrants, biopsy cores) -- clinically relevant tissue sampling simulation
4. **CoxBoost variable selection** -- penalized approach mitigates overfitting with 21 candidate models
5. **Haralick entropy as novel ITH metric** -- captures spatial distribution information beyond global Ki67%
6. **Consistent results across all subsampling conditions** -- entropy maintains prognostic value even on simulated biopsy cores
7. **Nonparametric methods throughout** -- appropriate for the nonnormal data distributions

### Weaknesses
1. **No external validation** -- single institution; generalizability to other populations/scanners unknown
2. **Low event rate** (34/254 = 13.4%) -- limits power for multivariable modeling; 21 models with 2-3 covariates each is exploratory
3. **Cutoff from same data** -- optimal thresholds derived from same cohort used for survival; risk of overfitting despite CoxBoost CV
4. **No therapy data** -- BCSS may be confounded by treatment differences; acknowledged as limitation
5. **Single IHC section per case** -- intra-tumor heterogeneity across blocks not assessed
6. **No comparison with manual hotspot counting** -- visual Ki67% was whole-tumor estimate, not hotspot-based

---

## GAP ANALYSIS

### Gap 1: Cross-Validation for Survival Model Internal Validation

**What the article used**: 5-fold cross-validation via CoxBoost to optimize the penalty parameter and perform variable selection. The cv.CoxBoost function splits data into training (190) and testing (64) subsets to prevent overfitting.

**Current ClinicoPath coverage**: The `survival` module provides standard Cox regression but no internal validation via cross-validation, bootstrap validation (optimism-corrected C-index), or calibration slope assessment.

**Gap**: A survival model internal validation option that computes:
- Optimism-corrected C-index via bootstrap (Harrell's approach: fit on bootstrap sample, evaluate on original data, compute optimism)
- Calibration slope from internal validation
- Optionally: k-fold CV for discrimination metrics

This is broadly important for any survival model -- every Cox regression result should ideally report internally validated discrimination.

**Priority**: Medium-High -- widely needed for any prognostic modeling study; would improve every survival analysis in the module.

### Gap 2: Formal C-index Comparison Between Models

**What the article used**: Compared C-indices between nested models (e.g., Model 1: lymph node + visual Ki67% → C = 0.671 vs Model 4: lymph node + Ki67% + entropy → C = 0.709) but without a formal statistical test for the difference.

**Current ClinicoPath coverage**: The `survival` and `concordanceindex` functions report C-indices per model but do not formally test whether the difference between two C-indices is statistically significant.

**Gap**: A paired C-index comparison test, analogous to DeLong's test for comparing AUCs but adapted for survival data. Options include:
- Uno's test for comparing time-dependent AUC/C-statistics
- Pencina's NRI/IDI (Net Reclassification Improvement / Integrated Discrimination Improvement)
- Bootstrap test for C-index difference

**Priority**: Medium -- useful for model comparison studies but somewhat specialized.

### Gap 3: Spatial Heterogeneity Metrics from Tabular Region-Level Data

**What the article used**: Haralick texture entropy computed from a co-occurrence matrix of Ki67% across hexagonal tiles. This quantifies spatial patterns in biomarker expression that global averages miss.

**Current ClinicoPath coverage**: `ihcheterogeneity` handles IHC heterogeneity but likely doesn't compute Haralick texture features from co-occurrence matrices of region-level data.

**Gap**: Given tabular data where each row represents a spatial region (tile, core, quadrant) with a biomarker percentage, compute:
- Shannon entropy (H = -sum(p_i * log(p_i))) of the distribution
- Simpson's diversity index
- Haralick texture entropy from co-occurrence matrix of binned values
- Coefficient of variation (CV) as simple heterogeneity measure
- Range and IQR as dispersion measures

**Priority**: Low-Medium -- this is quite specialized and requires users to have already performed hexagonal tiling/region-level quantification. The co-occurrence matrix computation for Haralick features is primarily an image analysis step.

---

## IMPLEMENTATION ROADMAP

### Recommended: Gap 1 -- Survival Model Internal Validation (Bootstrap)

**Where to add**: `R/survival.b.R`

**What to implement**:
Add a `bootstrapValidation` option to the survival module that computes an optimism-corrected C-index:

```r
# Harrell's bootstrap internal validation approach:
# 1. Fit model on original data → C_orig
# 2. For b in 1:B bootstrap samples:
#    a. Fit model on bootstrap sample → C_boot (apparent performance)
#    b. Evaluate bootstrap model on ORIGINAL data → C_test
#    c. optimism_b = C_boot - C_test
# 3. Optimism = mean(optimism_b)
# 4. C_corrected = C_orig - Optimism

# Using rms::validate() which implements this automatically
library(rms)
dd <- datadist(data)
options(datadist = "dd")
cph_model <- cph(Surv(time, status) ~ x1 + x2, data = data, x = TRUE, y = TRUE)
v <- validate(cph_model, B = 200)  # 200 bootstrap resamples
# v contains optimism-corrected Dxy (Somers' D), from which C = (Dxy + 1)/2
```

**Options to add**:
- `bootstrapValidation` (Bool, default FALSE)
- `bootstrapN` (Integer, default 200, min 50, max 1000)

**Results**:
- A table showing: apparent C-index, optimism, corrected C-index, calibration slope
- Note explaining interpretation

**Dependencies**: `rms` package (check if already imported; if not, use manual bootstrap with `survival` package)

**Complexity**: Medium (~100 lines)

### Not Recommended: Gap 2 (C-index comparison)

Requires specialized tests (Uno, NRI/IDI) that add complexity for a niche use case. Users needing this level of model comparison would typically use R directly.

### Not Recommended: Gap 3 (Spatial heterogeneity metrics)

This is an image analysis preprocessing step rather than a statistical analysis. The co-occurrence matrix computation requires specialized spatial data that most jamovi users wouldn't have in tabular form.

---

## TEST PLAN (Gap 1)

```r
# Check if rms package is available
library(rms)

# Create test data
set.seed(42)
n <- 200
test_data <- data.frame(
    time = rexp(n, 0.02),
    status = rbinom(n, 1, 0.15),
    age = rnorm(n, 60, 10),
    grade = factor(sample(1:3, n, replace = TRUE)),
    ki67 = runif(n, 0, 50)
)

# Manual bootstrap validation approach
dd <- datadist(test_data)
options(datadist = "dd")
fit <- cph(Surv(time, status) ~ age + grade + ki67,
           data = test_data, x = TRUE, y = TRUE)
v <- validate(fit, B = 200)
# Extract corrected Dxy → C-index
c_corrected <- (v["Dxy", "index.corrected"] + 1) / 2
cat("Corrected C-index:", round(c_corrected, 3), "\n")
```

### Dependencies

- `rms` package -- check DESCRIPTION; may need to add
- `survival` package -- already imported

---

## OVERALL ASSESSMENT

| Dimension | Rating |
|---|---|
| Statistical rigor | 🟢 Good (14/18) |
| Methodological novelty | High (Haralick texture entropy for ITH, cross-platform DIA validation) |
| Clinical relevance | High (entropy adds prognostic value beyond Ki67% in ER+/HER2- BC) |
| Coverage by ClinicoPath | Very Good (13 covered, 3 partial, 2 gaps) |
| Implementation priority | Medium-High (1 gap worth implementing: bootstrap internal validation) |

### Key Takeaway for ClinicoPath Development

This article exemplifies best-practice reproducibility assessment with the **triple agreement approach** (Spearman + ICC + CCC + Bland-Altman), which is already fully supported by `pathologyagreement` and `agreement`. The cross-platform DIA validation workflow (HALO vs Aiforia) maps directly to existing functionality.

The main implementation opportunity is **bootstrap internal validation for survival models** (optimism-corrected C-index), which addresses a fundamental need in every prognostic modeling study. This would complement the existing calibration curves and RCS splines by adding model validation capabilities. The `rms::validate()` function provides a clean implementation path.

The Haralick texture entropy concept is fascinating but outside the scope of a statistical analysis module -- it belongs in the image analysis pipeline (HALO/Aiforia/QuPath) rather than in downstream statistics.
