# Citation Review: Optimal Approaches to Grading Enteropancreatic Neuroendocrine Tumors Using Ki-67 Proliferation Index

---

## ARTICLE SUMMARY

- **Title/Label**: Optimal Approaches to Grading Enteropancreatic Neuroendocrine Tumors Using Ki-67 Proliferation Index: Hotspot and Whole-Slide Digital Quantitative Analysis
- **Design & Cohort**: Retrospective cohort study; primary cohort N = 325 patients (734 tissue sections) with surgically resected pancreatic and jejunoileal NETs from University of Iowa; external validation cohort N = 74 patients from University of Pittsburgh Medical Center; digital image analysis (DIA) for Ki-67 quantification using two methods: hotspot analysis (HSA) and whole-slide analysis (WSA).
- **Key Analyses**:
  - Kaplan-Meier survival analysis with log-rank test for trend (OS and PFS)
  - Cox proportional hazards regression with Ki-67 PI as continuous variable (Table 5)
  - Independent samples t-test comparing focal vs diffuse G2 Ki-67 PI values
  - Gehan-Breslow-Wilcoxon test for survival comparison (early event weighting)
  - Multi-site grade concordance analysis (primary vs regional mets vs distant mets)
  - Dual grading system (HSA-based grade vs WSA-based grade per patient)
  - External validation with independent image analysis software (QuPath vs QuantCenter)

---

## ARTICLE CITATION

| Field | Value |
|-------|-------|
| Title | Optimal Approaches to Grading Enteropancreatic Neuroendocrine Tumors Using Ki-67 Proliferation Index: Hotspot and Whole-Slide Digital Quantitative Analysis |
| Journal | Modern Pathology |
| Year | 2025 |
| Volume | 38 |
| Issue | — |
| Pages | 100780 |
| DOI | 10.1016/j.modpat.2025.100780 |
| PMID | TODO |
| Publisher | Elsevier (on behalf of USCAP) |
| ISSN | 0893-3952 |
| First Author | Ibrahim Abukhiran |
| Corresponding Author | Ibrahim Abukhiran (Abukhirani@upmc.edu) |
| Received / Accepted | 1 April 2024 / 3 April 2025 |
| License | CC BY-NC-ND 4.0 |

---

## Skipped Sources

*None -- PDF was read successfully (12 pages).*

---

## EXTRACTED STATISTICAL METHODS

| Method / Model | Role (primary/secondary) | Variants & Options | Assumptions/Diagnostics | References (sec/page) |
|---|---|---|---|---|
| Kaplan-Meier survival curves | Primary -- survival comparison across grade categories | 5 categories: Diffuse G1, Focal G2, Diffuse G2, Focal G3, Diffuse G3; log-rank test for trend (chi-square with P value) | Censoring assumed non-informative; right-censored OS data | Results (p5-6), Figs 5, 6, 8, 9, 10 |
| Log-rank test for trend | Primary -- test of ordered survival differences | Chi-square statistic reported (e.g., chi2 = 9.319, P = .0023 for overall grade; chi2 = 1.187, P = .2759 for primary-only grade) | Groups ordered by grade severity; independence assumption | Results (p5-6), Tables 4, 8 |
| Cox proportional hazards regression | Primary -- identify independent predictors of OS | Covariates: maximum WSA PI (continuous), location (pancreatic vs jejunoileal), pT, pN, pM stage; HR with 95% CI and P values reported | PH assumption implied but not explicitly tested; profile likelihood CIs | Results (p6-8), Table 5 |
| Gehan-Breslow-Wilcoxon test | Secondary -- early-event-weighted survival comparison | chi2 = 0.23, P = .6347 for focal G2 vs diffuse G1 in validation cohort | Gives greater weight to early events; alternative to standard log-rank | Results (p8) |
| Independent samples t-test | Secondary -- compare Ki-67 PI between focal and diffuse G2 | Two-sided; t = 9.238, df = 265, P < .0001; reports mean +/- SD, median, IQR | Assumes approximately normal distribution of Ki-67 PI; equal variances not stated | Results (p8), Table 7 |
| Descriptive statistics | Secondary -- characterize cohort and Ki-67 values | Mean +/- SD, median (IQR), range, counts/percentages | — | Tables 2, 3, 7 |
| Concordance assessment (multi-site grade) | Secondary -- assess grade agreement across anatomical sites | 70 patients with all 3 sites available; concordant = all same grade, discordant = at least 1 different; identifies which site drives highest grade | Not formal kappa/ICC; categorical matching only | Results (p7), Table 6 |
| External validation design | Design -- reproducibility across institutions/software | Independent cohort (N=74, Pittsburgh) with different DIA software (QuPath vs QuantCenter); same HSA/WSA methodology | Strengthens generalizability; tests platform independence | Methods (p5), Results (p8-9) |

---

## CLINICOPATH JAMOVI COVERAGE MATRIX

| Article Method | Jamovi Function(s) | Coverage | Notes / Workarounds |
|---|---|:---:|---|
| Kaplan-Meier survival curves | `survival` (sc option, kmunicate) | ✅ | Full KM curves with risk tables, CI bands, censoring marks |
| Log-rank test for trend | `survival` (log-rank with ordered groups) | ✅ | `survival` performs log-rank test; trend version available for ordered factor levels |
| Cox proportional hazards regression | `survival` (Cox model with HR, 95% CI) | ✅ | Continuous covariates (Ki-67 PI), categorical covariates (location, stage); multivariable model |
| Ki-67 PI as continuous predictor (non-linear) | `survival` (rcs_analysis, rcs_variable, rcs_knots) | ✅ | RCS splines (3-7 knots) for non-linear dose-response of Ki-67 PI; implemented 2026-02-08 |
| Calibration of survival predictions | `survival` (calibration_curves, calibration_timepoint) | ✅ | Predicted vs observed survival at fixed timepoints; risk quintile grouping; implemented 2026-02-08 |
| Gehan-Breslow-Wilcoxon test | `comparingsurvival` or `survival` | 🟡 | Standard log-rank available; Gehan-Breslow-Wilcoxon (early-event weighted) may not be explicitly offered as a named option |
| Independent samples t-test | `jjhistostats` (ggbetweenstats wrapper) | ✅ | Welch's t-test with effect size, CI; also available via base jamovi |
| Descriptive statistics (median, IQR, mean +/- SD) | `tableone`, `summarydata`, descriptive modules | ✅ | Comprehensive summary statistics by group |
| Multi-site grade concordance (3 sites per patient) | `agreement` (weighted kappa, Fleiss' kappa) | 🟡 | Can compute agreement across sites treated as "raters"; but the concept of "highest grade across sites" as clinical workflow is not automated |
| Percent concordance (simple matching) | `agreement` (percent agreement option) | ✅ | Overall percent agreement across raters/sites |
| Weighted kappa for ordinal grades (G1/G2/G3) | `agreement` (wght: linear or quadratic) | ✅ | Appropriate for ordinal WHO grade categories |
| Per-category specific agreement | `agreement` (specificAgreement) | ✅ | Positive/negative specific agreement per grade level |
| Intra-patient grade heterogeneity across sites | `agreement` with hierarchical/cluster analysis | 🟡 | Can model patient as cluster and sites as raters; but "which site has highest grade" logic is domain-specific |
| Grade distribution analysis (focal vs diffuse classification) | `ihcheterogeneity` | 🟡 | Function exists for IHC heterogeneity; but the specific focal/diffuse classification based on HSA-WSA discrepancy is not automated |
| Optimal cutpoint for Ki-67 (e.g., 10% for G2 subdivision) | `optimalcutpoint` | ✅ | Maximized sensitivity/specificity or Youden's J for survival-based cutoffs |
| Forest plot of subgroup effects | `survival` or `agreement` (subgroupForestPlot) | ✅ | Subgroup analysis by anatomical location, stage, etc. |
| External validation (independent cohort) | `survival` (calibration curves on validation data) | ✅ | Apply model from primary cohort; assess discrimination and calibration |
| Competing risks (metachronous progression) | `competingsurvival`, `finegray` | ✅ | Fine-Gray subdistribution hazard for competing events (death vs progression) |
| Parametric survival models | `survival` (use_parametric: Weibull, log-normal, etc.) | ✅ | Flexible parametric models for long-term extrapolation |

---

## CRITICAL EVALUATION

### Statistical Rigor Score: 🟡 Moderate-Good (12/18)

| Criterion | Score | Notes |
|---|---|---|
| Sample size justification | 0/2 | No formal power analysis or sample size calculation reported |
| Multiple testing correction | 0/2 | Multiple grade-level comparisons (5 categories, pairwise) without Bonferroni or FDR adjustment |
| Effect sizes with CIs | 1/2 | Cox HR with 95% CI reported (Table 5); but KM comparisons report only P values without hazard ratios or median survival CIs |
| Assumption checking | 0/2 | PH assumption for Cox regression not tested; normality for t-test not assessed; equal variance assumption not stated |
| Appropriate test selection | 2/2 | Log-rank for trend appropriate for ordered groups; Cox for multivariable analysis; t-test for continuous comparison; Gehan-Breslow-Wilcoxon as sensitivity analysis |
| Reproducibility | 2/2 | External validation with independent cohort, independent software (QuPath vs QuantCenter), and independent institution |
| Handling of missing data | 1/2 | TNM staging available for 178/325 patients (55%); missing staging data not formally addressed; analysis limited to subsets with complete data |
| Model diagnostics | 0/2 | No Schoenfeld residual plots, no Harrell's C-index, no model discrimination/calibration assessment |
| Clinical significance assessment | 2/2 | 4.3-year survival difference (diffuse G2 vs focal G2) is clinically meaningful; Ki-67 treated as continuous variable demonstrates dose-response |
| External validity | 2/2 | Independent validation cohort from different institution with different DIA software confirms key findings for PFS |
| Bias assessment | 1/1 | Retrospective design acknowledged; homogeneous treatment within institution noted as strength; selection bias from requiring surgical resection discussed |
| Censoring analysis | 1/1 | High censoring in validation cohort OS acknowledged as limitation (diffuse G1 OS undefined due to censoring) |

### Strengths
1. **Large, well-characterized cohort** (734 sections, 325 patients) — one of the largest NET Ki-67 studies
2. **Dual quantification methodology** (HSA + WSA) with digital image analysis — standardized and reproducible
3. **External validation** with independent institution and different DIA software (QuPath vs QuantCenter)
4. **Novel focal/diffuse G2 distinction** — clinically actionable refinement of WHO G2 category
5. **Multi-site analysis** — demonstrates that 66% of patients have discordant grades across anatomical sites
6. **Ki-67 as continuous variable** — Cox regression demonstrates this is more informative than categorical grades
7. **Standardized block selection criteria** — reduces sampling bias through prioritized selection algorithm

### Weaknesses
1. **No formal agreement metrics** — multi-site grade concordance is assessed as simple percent match (34% concordant) without kappa, ICC, or weighted agreement statistics
2. **No proportional hazards assumption testing** — Cox model relies on PH assumption that is not verified
3. **No C-index or AUC** — model discrimination not quantified; cannot compare predictive accuracy of HSA vs WSA vs combined approach
4. **Sample size for validation cohort is small** (N = 74) — OS analysis underpowered (non-significant differences likely reflect insufficient follow-up/events)
5. **No multivariate model in validation cohort** — only univariate KM/log-rank; Cox regression not replicated
6. **10% G2 cutoff not formally evaluated** — discussed as potential threshold but no optimal cutpoint analysis performed
7. **Missing TNM data** (45% of patients) — Cox model restricted to subset, potential selection bias
8. **No bootstrap or cross-validation** — model performance not internally validated

---

## GAP ANALYSIS

### Gap 1: Gehan-Breslow-Wilcoxon Test as Named Survival Comparison Option

**What the article used**: Gehan-Breslow-Wilcoxon test (chi2 = 0.23, P = .6347) as a sensitivity analysis for survival comparison, giving more weight to early events.

**Current ClinicoPath coverage**: The `survival` and `comparingsurvival` functions provide the standard log-rank test. The Fleming-Harrington family of weighted log-rank tests (which includes Gehan-Breslow-Wilcoxon as a special case with rho=1, gamma=0) may not be available as a named option.

**Gap**: A weighted log-rank test option (Gehan-Breslow-Wilcoxon, Tarone-Ware, Peto-Peto, or general Fleming-Harrington G^rho,gamma) would allow users to perform sensitivity analyses that weight different parts of the survival curve differently. This is important when:
- Early events are more clinically relevant (perioperative mortality)
- Late events are of interest (long-term recurrence)
- The PH assumption may be violated (crossing curves)

**Priority**: Medium — this is a well-known alternative but the standard log-rank test covers most use cases.

### Gap 2: Multi-Site Intra-Patient Grade Concordance Workflow

**What the article used**: For 70 patients with all 3 disease sites available, they computed: (a) percent with concordant grades across all sites (34%), (b) percent with discordant grades (66%), (c) which site drove the highest grade (primary 26.1%, regional 39.1%, distant 34.8%).

**Current ClinicoPath coverage**: The `agreement` function handles interrater agreement well, but the specific concept of "intra-patient concordance across anatomical sites" with identification of the highest-grade site is a domain-specific clinical workflow not directly supported.

**Gap**: A dedicated analysis or option within `agreement` or `pathologyagreement` that:
- Treats anatomical sites (primary, regional met, distant met) as "raters" of the same patient
- Computes concordance rates with weighted kappa for ordinal grades
- Identifies which site harbors the highest grade per patient
- Reports the distribution of "highest grade driver" sites
- Optionally assigns an "overall grade" (maximum across sites) as a new variable

**Priority**: Medium-Low — this can be approximated by the existing `agreement` function with some manual data reshaping. The unique workflow (maximum-grade assignment) is more of a data preparation step than a statistical method.

### Gap 3: Focal vs Diffuse Biomarker Distribution Classification

**What the article used**: A novel classification of G2 tumors as "focal" (Ki-67 PI concentrated in hotspot; HSA >> WSA) vs "diffuse" (Ki-67 PI distributed throughout tumor; HSA ≈ WSA) with significant survival implications (diffuse G2 median OS = 84 months vs focal G2 median OS = 136 months).

**Current ClinicoPath coverage**: The `ihcheterogeneity` function exists for tumor heterogeneity analysis, but the specific focal/diffuse classification based on the discrepancy between hotspot and whole-slide measurements is not automated.

**Gap**: A method to classify biomarker expression patterns as "focal" vs "diffuse" based on:
- Hotspot value vs whole-slide value comparison (ratio or difference)
- Threshold for classification (e.g., if WSA falls below the lower grade boundary while HSA is above, classify as focal)
- Survival stratification by focal/diffuse within the same grade
- Quantification of intra-tumoral heterogeneity (coefficient of variation, range, HSA-WSA ratio)

**Priority**: Medium — this is a novel and clinically impactful concept from the article, but implementing it requires domain-specific decisions about classification thresholds that may not generalize across biomarkers. Could be a useful addition to `ihcheterogeneity` or `pathologyagreement`.

---

## IMPLEMENTATION ROADMAP

### Gap 1: Weighted Log-Rank Tests (Fleming-Harrington Family)

**Where to add**: `R/survival.b.R` or `R/comparingsurvival.b.R`

**What to implement**:
- Add option `weightedLogrank` (List: standard, gehan_breslow, tarone_ware, peto_peto, fleming_harrington)
- For Fleming-Harrington: add `fh_rho` and `fh_gamma` parameters
- Use `survdiff()` with `rho` parameter (rho=0 = log-rank, rho=1 = Gehan-Breslow-Wilcoxon)
- Or use `survival::survdiff()` directly which supports rho parameter
- Report chi-square statistic, df, P value, and test name

**Dependencies**: `survival` package (already imported)

**Complexity**: Low (~50 lines)

### Gap 2: Multi-Site Grade Concordance (Not Recommended for Implementation)

This is better handled as a data preparation/reshaping step rather than a dedicated statistical method. Users can:
1. Reshape data so sites become "raters" in the `agreement` function
2. Use weighted kappa for ordinal grade concordance
3. Use R/Excel to compute maximum grade across sites

### Gap 3: Focal vs Diffuse Classification (Future Consideration)

This is a novel concept that needs further validation in the literature before implementing as a standardized tool. The classification criteria are not yet established as clinical guidelines. Could be revisited if WHO incorporates focal/diffuse distinction in future NET grading updates.

---

## RECOMMENDED IMPLEMENTATION: Gap 1 Only

**Gap 1 (Weighted Log-Rank Tests)** is the most broadly useful and well-established statistical method that is currently missing. It applies across all survival analyses, not just NET grading.

### Test Plan

```r
# Test weighted log-rank: Gehan-Breslow-Wilcoxon
# Create test data with crossing survival curves (where weighted tests differ from standard)
set.seed(42)
n <- 100
data <- data.frame(
    time = c(rexp(n/2, 0.02), rexp(n/2, 0.01)),  # Different hazard rates
    status = sample(0:1, n, replace = TRUE, prob = c(0.3, 0.7)),
    group = rep(c("A", "B"), each = n/2)
)

# Standard log-rank (rho=0) and Gehan-Breslow-Wilcoxon (rho=1) should give different P values
# when curves cross or PH assumption is violated
```

### Dependencies

- `survival::survdiff()` with `rho` parameter — already available in base `survival` package
- No new dependencies needed

---

## OVERALL ASSESSMENT

| Dimension | Rating |
|---|---|
| Statistical rigor | 🟡 Moderate-Good (12/18) |
| Methodological novelty | High (focal/diffuse G2 distinction, dual HSA+WSA grading) |
| Clinical relevance | High (4.3-year OS difference, applicable to NET management) |
| Coverage by ClinicoPath | Good (14 covered, 3 partial, 1 gap) |
| Implementation priority | Low (1 gap worth implementing: weighted log-rank tests) |

### Key Takeaway for ClinicoPath Development

This article demonstrates two important themes:
1. **Ki-67 PI as a continuous variable** (rather than categorical grade) provides more prognostic information — reinforcing the value of the `survival` function's RCS splines feature for non-linear dose-response modeling.
2. **Multi-site evaluation is essential** — the 66% discordance rate across disease sites challenges the common practice of grading from a single specimen. The existing `agreement` function with hierarchical kappa can support this type of multi-site concordance analysis when data is properly structured.

The one concrete implementation opportunity is adding **weighted log-rank tests** (particularly Gehan-Breslow-Wilcoxon) as a named option in the survival module, which has broad applicability beyond this specific use case.
