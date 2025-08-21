# ClinicoPath Development Roadmap


## Comprehensive Survival Analysis Roadmap (CRAN Task View Implementation)

This roadmap outlines the systematic implementation of advanced survival analysis capabilities based on the comprehensive CRAN Task View on Survival Analysis. The focus is on robust, validated methods specifically applicable to tabular clinical research data.

#### **Phase 1: Core Survival Distribution & Estimation** 🔄 IN PROGRESS

- **Non-Parametric Estimation:**
  - ✅ **Kaplan-Meier estimator with confidence intervals** (`survival`, `survminer`, `km.ci`) - **IMPLEMENTED**
  - ✅ **Nelson-Aalen cumulative hazard estimator** (`survival`) - **IMPLEMENTED**  
  - ✅ **Turnbull NPMLE for interval-censored data** (`Icens`, `MLEcens`, `interval`) - **IMPLEMENTED**
  - ✅ **Product-limit estimator variants** (`prodlim`) - **IMPLEMENTED**
  - ✅ **Smooth hazard estimation** (`muhaz`, `kerdiest`, `bshazard`) - **IMPLEMENTED** (`smoothhazard`)
  - ✅ **Survey-weighted survival estimates** (`survey`) - **IMPLEMENTED** (`surveysurvival`)

- **Parametric Distribution Modeling:**
  - ✅ **Weibull, Exponential, Log-normal, Log-logistic models** (`survival`, `flexsurv`) - **IMPLEMENTED**
  - ✅ **Generalized Gamma and F-distributions** (`flexsurv`, `eha`) - **IMPLEMENTED** (`flexparametric`)
  - ✅ **Spline-based hazard functions** (`flexsurv`, `splineSurv`) - **IMPLEMENTED** (`splinehazard`)
  - ✅ **Distribution selection and goodness-of-fit** (`fitdistrplus`, `AdequacyModel`) - **IMPLEMENTED** (`distributionfit`)
  - ✅ **Flexible baseline distributions** (`flexsurv`, `tram`) - **IMPLEMENTED** (`flexiblebaseline`)
  - ✅ **Stratified parametric models** (`rstpm2`) - **IMPLEMENTED** (`stratifiedparametric`)

- **Core Hypothesis Testing:**
  - ✅ **Log-rank test and variants** (`survival`) - **IMPLEMENTED**
  - ✅ **Fleming-Harrington G-rho family tests** (`survival`) - **IMPLEMENTED** (`flemingharrington`)
  - ✅ **Weighted log-rank tests** (`survMisc`, `coin`) - **IMPLEMENTED** (`weightedlogrank`)
  - ✅ **Restricted mean survival time tests** (`survRM2`, `SSRMST`) - **IMPLEMENTED** (`rmst`)
  - ✅ **Permutation tests for survival** (`coin`) - **IMPLEMENTED** (`permutationsurvival`)
  - ✅ **Median survival comparisons** (`survminer`) - **IMPLEMENTED** (`mediansurvival`)

#### **Phase 2: Cox Regression & Advanced Modeling** 🔄 IN PROGRESS

- **Cox Proportional Hazards Models:**
  - ✅ **Standard Cox PH with diagnostics** (`survival`, `survminer`, `rms`) - **IMPLEMENTED**
  - ✅ **Stratified Cox models** (`survival`) - **IMPLEMENTED**
  - ✅ **Penalized Cox regression** (`glmnet`, `penalized`) - **IMPLEMENTED** (`penalizedcox`)
  - ✅ **High-dimensional data Cox models** (`glmnet`, `CoxBoost`) - **IMPLEMENTED** (`highdimcox`)
  - ✅ **Robust Cox regression** (`coxrobust`) - **IMPLEMENTED** (`coxrobust`)
  - ✅ **Weighted Cox regression** (`coxphw`) - **IMPLEMENTED** (`coxphw`)
  - ✅ **Mixed-effects Cox models** (`coxme`) - **IMPLEMENTED** (`mixedcox`)

- **Time-Varying Effects & Non-Proportional Hazards:**
  - ✅ **Time-varying covariates in Cox models** (`survival`) - **IMPLEMENTED** (`timevarycox`)
  - ✅ **Aalen's additive hazard models** (`timereg`, `addhazard`) - **IMPLEMENTED** (`aalenhazard`)
  - ✅ **Flexible parametric models** (`rstpm2`) - **IMPLEMENTED** (`flexrstpm2`)
  - ✅ **Smoothly time-varying effects** (`smoothHR`, `timereg`) - **IMPLEMENTED** (`smoothtimevary`)
  - ✅ **Dynamic coefficient models** (`dynsurv`) - **IMPLEMENTED** (`dynamiccoeff`)
  - ✅ **Proportional hazards testing** (`PHeval`) - **IMPLEMENTED** (`pheval`)

- **Accelerated Failure Time Models:**
  - ✅ **Parametric AFT models** (`survival`, `flexsurv`) - **IMPLEMENTED**
  - ✅ **Rank-based AFT estimation** (`aftgee`) - **IMPLEMENTED** (`raftgee`)
  - ✅ **Robust AFT models** (`RobustAFT`) - **IMPLEMENTED** (`robustaft`)
  - ✅ **Transformation models** (`tram`) - **IMPLEMENTED** (`transformationmodels`)
  - ✅ **GEE-based AFT models** (`aftgee`) - **IMPLEMENTED** (`raftgee`)

- **Frailty Models:**
  - ✅ **Shared frailty models** (`frailtypack`) - **IMPLEMENTED**
  - ✅ **Mixed-effects survival models** (`coxme`) - **IMPLEMENTED**
  - ✅ **EM-algorithm frailty models** (`frailtyEM`) - **IMPLEMENTED**
  - ✅ **Parametric frailty models** (`frailtySurv`) - **IMPLEMENTED** (`parametricfrailty`)

#### **Phase 3: Competing Risks & Multi-State Analysis** 🔄 IN PROGRESS

- **Competing Risks Analysis:**
  - ✅ **Cumulative Incidence Functions** (`cmprsk`, `etm`) - **IMPLEMENTED**
  - ✅ **Fine-Gray subdistribution hazards** (`cmprsk`) - **IMPLEMENTED**
  - ✅ **Cause-specific hazards modeling** (`survival`) - **IMPLEMENTED** (`causespecifichazards`)
  - ✅ **Direct binomial regression** (`timereg`) - **IMPLEMENTED** (`directbinomial`)
  - ✅ **Competing risks power analysis** (`powerCompRisk`) - **IMPLEMENTED** (`powercomprisk`)
  - ✅ **Flexible competing risks models** (`riskRegression`) - **IMPLEMENTED** (`flexcomprisk`)

- **Multi-State Models:**
  - ✅ **Markov multi-state models** (`mstate`, `msm`) - **IMPLEMENTED** (`markovmultistate`)
  - ✅ **Semi-Markov models** (`SemiMarkov`) - **IMPLEMENTED** (`semimarkov`)
  - ✅ **Hidden Markov models for survival** (`msm`) - **IMPLEMENTED** (`hiddenmarkov`)
  - ✅ **Illness-death models** (`p3state.msm`) - **IMPLEMENTED** (`illnessdeath`)
  - ✅ **Flexible multi-state models** (`flexmsm`) - **IMPLEMENTED** (`flexmultistate`)
  - ✅ **Continuous-time Markov models** (`msm`) - **IMPLEMENTED** (`continuousmarkov`)

- **Recurrent Event Analysis:**
  - ✅ **Andersen-Gill and PWP models** (`survival`) - **IMPLEMENTED**
  - ✅ **Frailty models for recurrent events** (`frailtypack`) - **IMPLEMENTED**
  - **Marginal models for recurrent events** (`reReg`)
  - **Joint frailty models** (`frailtypack`)
  - **Conditional GEE for gap times** (`condGEE`)
  - **Recurrent event data analysis** (`reda`)

#### **Phase 4: Machine Learning & Advanced Prediction** ✅ LARGELY COMPLETED

- **Tree-Based Methods:**
  - ✅ **Survival trees** (`rpart`, `party`) - **IMPLEMENTED**
  - ✅ **Random survival forests** (`randomForestSRC`) - **IMPLEMENTED**
  - ✅ **Conditional inference trees** (`party`, `partykit`) - **IMPLEMENTED** (`conditionalinference`)
  - ✅ **Gradient boosting for survival** (`gbm`, `mboost`, `xgboost`) - **IMPLEMENTED** (`gradientboosting`)
  - ✅ **Extremely randomized trees** (`ranger`) - **IMPLEMENTED** (`extratrees`)
  - ✅ **Bayesian additive regression trees** (`BART`) - **IMPLEMENTED** (`survivalbart`)

- **Regularized Survival Models:**
  - ✅ **LASSO, Ridge, Elastic Net** (`glmnet`) - **IMPLEMENTED**
  - ✅ **Adaptive LASSO for Cox models** (`glmnet`) - **IMPLEMENTED** (`adaptivelasso`)
  - ✅ **Group LASSO for survival** (`grplasso`) - **IMPLEMENTED** (`grouplasso`)
  - ✅ **Sparse group LASSO** (`SGL`) - **IMPLEMENTED** (`sparsegrouplasso`)
  - **Penalized Cox regression** (`penalized`)
  - **Smoothly clipped absolute deviation** (`ncvreg`)

- **Bayesian Methods:**
  - ✅ **Bayesian survival models** (`rstanarm`) - **IMPLEMENTED** (`bayesiansurvival`)
  - ✅ **Bayesian model averaging** (`BMA`) - **IMPLEMENTED** (`bayesianma`)
  - **Spatial Bayesian survival** (`spBayesSurv`)
  - **Bayesian joint models** (`JMbayes`)

- **High-Dimensional Methods:**
  - **Principal component Cox models** (`superpc`)
  - **Partial least squares Cox** (`plsRcox`)
  - **Spike-and-slab priors** (`BoomSpikeSlab`)
  - **Variable selection for Cox models** (`c060`)

#### **Phase 5: Model Validation & Performance Assessment** ✅ COMPLETED

- **Prediction Performance Metrics:**
  - ✅ **Time-dependent ROC curves** (`timeROC`, `survivalROC`) - **IMPLEMENTED**
  - ✅ **Prediction error curves** (`pec`) - **IMPLEMENTED**
  - ✅ **Concordance index and C-statistics** (`survC1`, `compareC`) - **IMPLEMENTED**
  - ✅ **Integrated prediction error** (`pec`, `riskRegression`) - **IMPLEMENTED**
  - ✅ **Net reclassification improvement** (`nricens`) - **IMPLEMENTED** (`netreclassification`)
  - ✅ **Integrated discrimination improvement** (`survIDI`) - **IMPLEMENTED** (`idi`)

- **Model Calibration & Validation:**
  - ✅ **Calibration plots for survival models** (`rms`, `pec`) - **IMPLEMENTED**
  - ✅ **Bootstrap validation** (`rms`) - **IMPLEMENTED**
  - ✅ **Cross-validation for survival** (`pec`) - **IMPLEMENTED**
  - ✅ **External validation frameworks** (`riskRegression`) - **IMPLEMENTED**
  - **Optimism-corrected performance** (`rms`)
  - **Shrinkage factor estimation** (`rms`)

- **Power Analysis & Sample Size:**
  - **Log-rank test power** (`powerSurvEpi`)
  - **Cox regression power** (`powerSurvEpi`)
  - **Competing risks power** (`powerCompRisk`)
  - **Non-inferiority trial design** (`nphRCT`)
  - **Restricted mean survival time power** (`SSRMST`)
  - **SNP-based survival studies** (`survSNP`)

#### **Phase 6: Specialized Survival Methods** ✅ COMPLETED

- **Relative & Excess Mortality:**
  - ✅ **Relative survival analysis** (`relsurv`, `popEpi`) - **IMPLEMENTED**
  - **Excess mortality modeling** (`mexhaz`)
  - **Population-based survival** (`popEpi`)
  - **Cancer registry analysis** (`relsurv`)
  - **Flexible relative survival** (`flexrsurv`)

- **Joint & Longitudinal-Survival Models:**
  - ✅ **Joint longitudinal-survival modeling** (`JM`, `joineRML`) - **IMPLEMENTED**
  - **Shared parameter models** (`JMbayes`)
  - **Dynamic predictions** (`JMbayes2`)
  - **Landmark analysis** (`dynpred`)
  - **Joint modeling with competing risks** (`joineR`)

- **Interval-Censored & Complex Censoring:**
  - ✅ **Interval-censored survival** (`icenReg`, `Icens`) - **IMPLEMENTED**
  - **Doubly censored data** (`dblcens`)
  - **Partly interval-censored data** (`PWEALL`)
  - **Current status data** (`Icens`)
  - **Interval-censored cure models** (`ICGOR`)

- **Cure Models & Long-Term Survival:**
  - ✅ **Mixture cure models** (`smcure`) - **IMPLEMENTED** (`curemodels`)
  - **Promotion time cure models** (`miCoPTCM`)
  - ✅ **Flexible cure models** (`flexsurvcure`) - **IMPLEMENTED** (`curemodels`)
  - **Generalized odds rate cure models** (`GORCure`)
  - ✅ **Non-mixture cure models** (`NMCM`) - **IMPLEMENTED** (`curemodels`)

#### **Phase 7: Clinical Translation & Implementation** ✅ COMPLETED

- **Prognostic Model Development:**
  - ✅ **Nomogram construction** (`rms`, `nomogramFormula`) - **IMPLEMENTED**
  - ✅ **Risk score calculators** (`PredictABEL`) - **IMPLEMENTED**
  - ✅ **Decision curve analysis** (`rmda`, `DecisionCurve`) - **IMPLEMENTED**
  - ✅ **Net benefit analysis** (`stdca`) - **IMPLEMENTED**
  - **Dynamic prediction models** (`dynpred`)
  - **Personalized treatment selection** (`ITRSelect`)

- **Biomarker & Threshold Analysis:**
  - ✅ **Optimal cutpoint determination** (`survminer`, `maxstat`) - **IMPLEMENTED**
  - **Time-dependent biomarkers** (`timeROC`)
  - **Landmark analysis for biomarkers** (`dynpred`)
  - **Threshold regression** (`threg`)
  - **Biomarker evaluation** (`survcomp`)
  - **Prognostic index calculation** (`survcomp`)

- **Clinical Decision Support:**
  - ✅ **Interactive survival calculators** (Shiny integration) - **IMPLEMENTED**
  - ✅ **Patient-specific predictions** (`rms`) - **IMPLEMENTED**
  - **Treatment benefit calculators** (custom implementation)
  - **Prognosis communication tools** (custom implementation)
  - **Risk communication interfaces** (`DynNom`)
  - **Clinical prediction models** (`rms`)

#### **Phase 8: Advanced Survival Methodology** 🔄 IN PROGRESS

- **Pseudo-Observations & Direct Regression:**
  - ✅ **Pseudo-observations for survival** (`pseudo`) - **IMPLEMENTED**
  - **Direct regression on survival function** (`pseudo`)
  - **Generalized pseudo-observations** (`geepack`)
  - **Restricted mean survival time regression** (`survRM2`)

- **Conditional Survival & Temporal Methods:**
  - **Conditional survival estimation** (`condSURV`)
  - ✅ **Landmark analysis** (`landest`) - **IMPLEMENTED** (`landmarkanalysis`)
  - **Dynamic survival prediction** (`dynpred`)
  - **Time-updated survival estimates** (`timereg`)

- **Flexible Parametric & Transformation Models:**
  - **Flexible parametric survival models** (`flexsurv`)
  - **Royston-Parmar models** (`rstpm2`)
  - **Transformation models** (`tram`)
  - **Link-based survival models** (`rstpm2`)

#### **Phase 9: Specialized Clinical Applications** 📅 PLANNED

- **Cancer-Specific Survival Analysis:**
  - **Cure models for cancer data** (`smcure`, `flexsurvcure`)
  - **Tumor growth models** (`grofit`)
  - **Treatment switching analysis** (`rpsftm`)
  - **Progression-free survival** (specialized implementation)
  - **Cancer screening evaluation** (`HSROC`)

- **Epidemiological Survival Methods:**
  - **Cohort survival analysis** (`Epi`)
  - **Case-cohort designs** (`cchs`)
  - **Survey-weighted survival** (`survey`)
  - **Population attributable risk** (`Epi`)

- **Clinical Trial Applications:**
  - **Non-inferiority designs** (`nphRCT`)
  - **Adaptive trial methods** (`rpact`)
  - **Group sequential designs** (`gsDesign`)
  - **Futility analysis** (`interim`)

#### **Recent Major Achievements (Phase 4 Implementation):**

🎉 **Successfully Implemented 11 Advanced Survival Analysis Modules:**

**Machine Learning & Regularization Methods:**
- ✅ `adaptivelasso` - Adaptive LASSO for Cox models with optimal variable selection
- ✅ `bayesianma` - Bayesian model averaging with uncertainty quantification  
- ✅ `conditionalinference` - Conditional inference trees for non-parametric survival analysis
- ✅ `extratrees` - Extremely randomized trees for robust survival prediction
- ✅ `gradientboosting` - Gradient boosting machines for survival data
- ✅ `grouplasso` - Group LASSO for structured variable selection
- ✅ `survivalbart` - Bayesian Additive Regression Trees with uncertainty quantification
- ✅ `sparsegrouplasso` - Sparse group LASSO for hierarchical variable selection

**Model Validation & Performance Assessment:**
- ✅ `idi` - Integrated Discrimination Improvement for model comparison
- ✅ `netreclassification` - Net Reclassification Improvement for clinical utility

**Advanced Multi-State Modeling:**
- ✅ `illnessdeath` - Illness-death models for disease progression analysis

**Technical Achievements:**
- ✅ Complete YAML structure standardization across all modules
- ✅ Automated UI generation with jamovi compiler integration
- ✅ Proper type definitions and validation for all survival analysis parameters
- ✅ Comprehensive error handling and user guidance systems

#### **Latest Major Achievements (Advanced Survival Analysis - December 2024):**

🚀 **Recently Implemented 6 Critical Advanced Survival Modules:**

**Advanced Methodological Implementations:**
- ✅ `curemodels` - Comprehensive cure models for long-term survivor analysis
  - Mixture and non-mixture cure models using smcure and flexsurvcure
  - Bootstrap confidence intervals and sensitivity analysis
  - Multiple link functions (logit, probit, cloglog) and distributions
  - Cure fraction visualization and clinical interpretation

- ✅ `landmarkanalysis` - Landmark analysis for time-varying predictors
  - Addresses immortal time bias in dynamic biomarker studies
  - Multiple landmark time points with prediction windows
  - Dynamic risk prediction and super model approaches
  - Bootstrap validation with calibration and discrimination plots

- ✅ `flexmultistate` - Flexible parametric multi-state survival models
  - Illness-death, competing risks, and progressive disease models
  - Royston-Parmar splines and flexible hazard distributions
  - State transition and occupancy probability calculations
  - Microsimulation capabilities for complex predictions

- ✅ `continuousmarkov` - Continuous-time Markov models for longitudinal data
  - Handles irregularly spaced observation times in clinical studies
  - Estimates transition intensities between discrete health states
  - Multiple model structures (full, progressive, reversible)
  - Sojourn time calculations and prevalence estimation over time

- ✅ `pcacox` - Principal Component Cox models for high-dimensional data
  - PCA-based dimensionality reduction for survival analysis
  - Supervised, standard, and sparse PCA methods with genomic applications
  - Comprehensive validation with bootstrap and permutation testing
  - Risk stratification with scree plots, biplots, and survival curves

- ✅ `plscox` - Partial Least Squares Cox models for high-dimensional survival
  - PLS-based supervised dimensionality reduction optimized for survival outcomes
  - Cross-validation for optimal component selection with multiple algorithms
  - Variable importance analysis and component loadings interpretation
  - Advanced validation including bootstrap and permutation testing

**Implementation Excellence:**
- ✅ Each module follows complete jamovi architecture (4 YAML + 2 R + 1 JS files)
- ✅ Comprehensive error handling and clinical guidance systems
- ✅ Advanced visualization capabilities with state-of-the-art plots
- ✅ Full integration with existing ClinicoPath survival analysis framework

#### **Implementation Priority Matrix:**

🔥 **High Priority (Next 6 months):**

- **Phase 3**: ✅ COMPLETED - Multi-state models (Flexible multi-state ✅, Continuous-time Markov ✅, Recurrent events remaining)
- **Phase 4**: ✅ COMPLETED - High-dimensional methods (Principal component Cox ✅, Partial least squares Cox ✅)
- **Phase 8**: Begin advanced methodology (Pseudo-observations, Conditional survival, Flexible parametric models)
- **Phase 9**: Initiate specialized clinical applications (Cancer-specific methods, Clinical trial designs)

⚡ **Medium Priority (6-12 months):**

- **Complete Phase 8**: Advanced survival methodology (Direct regression, Time-updated estimates, Link-based models)
- **Expand Phase 9**: Full clinical applications suite (Epidemiological methods, Clinical trial designs, Regulatory compliance)
- **Cure models and long-term survival** analysis (mixture and non-mixture approaches)
- **Integration enhancements**: Cross-module workflows, automated reporting, clinical decision support

🔮 **Future Implementation (12+ months):**

- **Deep learning integration** for survival analysis
- **Real-time clinical decision support** systems
- **Integration with electronic health records**
- **Advanced biomarker discovery** platforms
- **Precision medicine applications**

#### **Comprehensive Package Integration Plan:**

**Core Foundation:** `survival`, `survminer`, `rms`, `flexsurv`, `prodlim`
**Competing Risks:** `cmprsk`, `etm`, `timereg`, `riskRegression`
**Multi-State:** `mstate`, `msm`, `flexmsm`, `SemiMarkov`
**Frailty/Mixed:** `frailtypack`, `coxme`, `frailtyEM`, `frailtySurv`
**Interval Censored:** `Icens`, `icenReg`, `interval`, `MLEcens`, `dblcens`
**Recurrent Events:** `frailtypack`, `reda`, `condGEE`, `reReg`
**Joint Modeling:** `JM`, `JMbayes`, `joineR`, `joineRML`
**Machine Learning:** `randomForestSRC`, `glmnet`, `gbm`, `ranger`, `mboost`
**Validation:** `timeROC`, `pec`, `riskRegression`, `survAUC`, `survC1`
**Relative Survival:** `relsurv`, `popEpi`, `mexhaz`, `flexrsurv`
**Cure Models:** `smcure`, `flexsurvcure`, `ICGOR`, `miCoPTCM`
**Bayesian:** `rstanarm`, `BMA`, `JMbayes`, `spBayesSurv`
**Power Analysis:** `powerSurvEpi`, `powerCompRisk`, `SSRMST`, `survSNP`
**Advanced Methods:** `pseudo`, `tram`, `rstpm2`, `condSURV`, `dynpred`
**Clinical Translation:** `nomogramFormula`, `DynNom`, `rmda`, `stdca`

---

## 🧪 Core Pathology Statistics Implementation Plan

Based on comprehensive analysis of statistical methods used in major pathology journals (Modern Pathology, AJSP, AJP, Histopathology, APLM), the following statistical features are essential for pathology research but currently missing from our module. **All implementations are designed specifically for jamovi's tabular data structure and GUI-based workflow.**

### **Phase A: Foundation Statistical Methods** 📅 HIGH PRIORITY

**Agreement & Reliability Statistics (Essential for Pathology):**
- **Cohen's kappa & weighted kappa:** Inter-rater reliability for pathologist agreement (used in ~40% of diagnostic studies)
  - *Jamovi Design:* Two-variable input (Rater1, Rater2), supports factor/ordinal variables
  - *Output:* Kappa table with confidence intervals, agreement visualization
  - *Packages:* `psych`, `irr`, `vcd` integration
- **Intraclass correlation coefficient (ICC):** For continuous measurement agreement and reproducibility  
  - *Jamovi Design:* Multi-variable input for multiple raters/measurements, numeric variables
  - *Output:* ICC table with 95% CI, reliability interpretation guide
  - *Packages:* `psych`, `irr`, `ICC` integration
- **Fleiss' kappa:** Multi-rater agreement for >2 pathologists
  - *Jamovi Design:* Matrix input format or multi-column rater variables
  - *Output:* Multi-rater agreement table, category-specific kappa values
  - *Packages:* `irr`, `psych` integration
- **Concordance correlation coefficient:** Alternative agreement measure
  - *Jamovi Design:* Two continuous variables input, scatterplot with CCC line
  - *Output:* CCC coefficient with precision/accuracy decomposition
  - *Packages:* `epiR`, `DescTools` integration

**Method Comparison & Laboratory Statistics:**
- **Bland-Altman plots:** Method comparison and agreement visualization (lab medicine standard)
  - *Jamovi Design:* Two continuous variables (Method1, Method2), optional grouping variable
  - *Output:* B-A plot with limits of agreement, bias assessment table, difference statistics
  - *Packages:* `BlandAltmanLeh`, `MethComp`, `blandr` integration
- **Passing-Bablok regression:** Non-parametric method comparison for lab tests
  - *Jamovi Design:* Two continuous variables (X=reference, Y=test method), handles outliers
  - *Output:* P-B regression line, slope/intercept with CI, systematic bias assessment
  - *Packages:* `mcr`, `MethComp` integration
- **Deming regression:** Method comparison accounting for errors in both methods  
  - *Jamovi Design:* Two continuous variables with optional error ratio specification
  - *Output:* Deming regression parameters, comparison with ordinary regression
  - *Packages:* `mcr`, `deming`, `MethComp` integration
- **Mountain plot:** Alternative visualization for method comparison
  - *Jamovi Design:* Two continuous variables, percentile-based difference visualization
  - *Output:* Mountain plot with quantile statistics, complement to B-A plots
  - *Packages:* Custom implementation with `ggplot2`

**Enhanced Diagnostic Accuracy Measures:**
- **Likelihood ratios:** Positive and negative likelihood ratios (clinical utility focus)
  - *Jamovi Design:* Test variable (continuous/factor) + Reference standard (binary factor)
  - *Output:* LR+ and LR- with confidence intervals, clinical interpretation guide
  - *Packages:* `epiR`, `DTComPair`, custom implementation
- **Diagnostic odds ratios:** Summary measure of diagnostic test performance
  - *Jamovi Design:* 2x2 contingency table from test/reference variables
  - *Output:* DOR with CI, comparison with other diagnostic measures
  - *Packages:* `epiR`, `DTComPair` integration  
- **Youden Index:** Optimal cutpoint determination for diagnostic tests
  - *Jamovi Design:* Continuous test variable + binary reference standard
  - *Output:* Optimal cutpoint with Youden J statistic, ROC curve with optimal point
  - *Packages:* `OptimalCutpoints`, `pROC`, `ROCR` integration
- **Predictive value curves:** PPV/NPV across different prevalence levels  
  - *Jamovi Design:* Test sensitivity/specificity inputs or raw diagnostic data
  - *Output:* PPV/NPV curves across prevalence range, prevalence threshold analysis
  - *Packages:* Custom implementation with `ggplot2`, `epiR` integration

### **Phase B: Advanced Non-Parametric Methods** 📅 HIGH PRIORITY

**Missing Non-Parametric Tests (Used in ~30% of pathology studies):**
- **McNemar's test:** Paired categorical data (biopsy vs resection diagnosis)
  - *Jamovi Design:* Two paired categorical variables (Before, After) or 2x2 paired table input
  - *Output:* McNemar chi-square, exact p-value, discordant pairs analysis
  - *Packages:* Base R `mcnemar.test`, `exact2x2` integration
- **Cochran's Q test:** Extension of McNemar for >2 paired groups  
  - *Jamovi Design:* Multiple paired binary variables or subject-by-condition matrix
  - *Output:* Cochran's Q statistic, post-hoc pairwise comparisons
  - *Packages:* `RVAideMemoire`, custom implementation
- **Friedman test:** Non-parametric repeated measures ANOVA
  - *Jamovi Design:* Dependent variable + subject ID + repeated factor (within-subjects design)
  - *Output:* Friedman chi-square, post-hoc pairwise comparisons with Bonferroni correction
  - *Packages:* Base R `friedman.test`, `PMCMRplus` for post-hocs
- **Page's trend test:** Ordered alternative to Friedman test
  - *Jamovi Design:* Same as Friedman but with ordered factor levels for trend analysis
  - *Output:* Page's L statistic, trend p-value, effect size measures
  - *Packages:* `PMCMRplus`, custom implementation
- **Exact tests for small samples:** Fisher's exact extensions, exact confidence intervals
  - *Jamovi Design:* Contingency table inputs with exact method options
  - *Output:* Exact p-values, exact confidence intervals for proportions/odds ratios
  - *Packages:* `exact2x2`, `exactci`, `fisher.test` extensions

**Enhanced Correlation Analysis:**
- **Spearman's rank correlation:** Non-parametric correlation (currently limited implementation)
  - *Jamovi Design:* Two or more continuous/ordinal variables, correlation matrix format
  - *Output:* Spearman correlation matrix with p-values, confidence intervals, scatterplot matrix
  - *Packages:* Base R `cor.test`, `psych` for matrix analysis
- **Kendall's tau:** Alternative non-parametric correlation with different properties
  - *Jamovi Design:* Two continuous/ordinal variables, tau-a vs tau-b options
  - *Output:* Kendall's tau with confidence intervals, comparison with Spearman
  - *Packages:* Base R `cor.test`, `Kendall` package
- **Partial correlation:** Correlation controlling for other variables
  - *Jamovi Design:* Two primary variables + multiple control variables
  - *Output:* Partial correlation coefficient, comparison with zero-order correlation
  - *Packages:* `psych`, `ppcor`, `ggm` integration
- **Polychoric/tetrachoric correlation:** For ordinal/binary variables
  - *Jamovi Design:* Ordinal factor variables, automatic detection of variable types
  - *Output:* Polychoric correlation matrix, comparison with Pearson correlations
  - *Packages:* `psych`, `polycor` integration

### **Phase C: Bayesian Statistics for Pathology** 📅 MEDIUM PRIORITY

**Bayesian Diagnostic Methods (Emerging in Digital Pathology):**
- **Bayesian diagnostic probability updates:** Post-test probability calculations
- **Bayesian confidence intervals (credible intervals):** Alternative to frequentist CI
- **Beta-binomial models:** For overdispersed diagnostic accuracy data
- **Hierarchical Bayesian models:** Multi-center diagnostic studies

**Prior Information Integration:**
- **Informative prior elicitation:** Methods for incorporating expert knowledge
- **Bayesian model averaging:** For diagnostic test combinations
- **Bayesian network models:** Complex diagnostic workflows and dependencies

### **Phase D: Spatial Statistics & Digital Pathology** 📅 MEDIUM PRIORITY

**Spatial Point Pattern Analysis (Growing Need):**
- **Ripley's K-function:** Spatial clustering analysis of cells/features
- **Nearest neighbor distance analysis:** Spatial relationship quantification
- **Marked point processes with MCMC:** Advanced spatial modeling
- **Spatial autocorrelation measures:** Moran's I, Geary's C for tissue patterns

**Image Analysis Statistics:**
- **Texture analysis statistics:** Haralick features statistical validation
- **Morphometric statistical validation:** Shape and size measure reliability
- **Color deconvolution validation:** Statistical assessment of stain separation

### **Phase E: Laboratory Quality Control Statistics** 📅 MEDIUM PRIORITY

**Quality Control Methods (Lab Medicine Focus):**
- **Control charts:** Shewhart, CUSUM, EWMA charts for lab monitoring
- **Sigma metrics:** Laboratory performance indicators and six sigma methodology
- **Method validation protocols:** Statistical frameworks for precision, accuracy, linearity
- **Reference interval establishment:** Robust methods for normal range determination
- **Measurement uncertainty estimation:** ISO 15189 compliant uncertainty calculations

**Proficiency Testing Statistics:**
- **Z-score calculations:** Performance assessment in external quality assurance
- **Robust statistics for PT:** Methods handling outliers in proficiency testing
- **Consensus value determination:** Statistical methods for reference value establishment

### **Phase F: Meta-Analysis & Evidence Synthesis** 📅 LOWER PRIORITY

**Meta-Analysis Methods (Systematic Reviews):**
- **Forest plots:** Graphical meta-analysis summaries with statistical validation
- **Heterogeneity testing:** I² statistics, Q-test, tau-squared estimation
- **Random-effects vs fixed-effects models:** Appropriate model selection methods
- **Publication bias assessment:** Funnel plots, Egger's test, trim-and-fill methods
- **Network meta-analysis:** Indirect treatment comparisons

**Diagnostic Test Accuracy Meta-Analysis:**
- **Bivariate meta-analysis:** Joint modeling of sensitivity and specificity
- **HSROC models:** Hierarchical summary ROC for diagnostic meta-analysis
- **Meta-regression for DTA:** Investigating sources of heterogeneity in diagnostic accuracy

### **Phase G: Advanced High-Dimensional Methods** 📅 LOWER PRIORITY

**Genomics/Proteomics Statistics (Molecular Pathology):**
- **Significance Analysis of Microarrays (SAM):** Specifically mentioned in pathology literature
- **False Discovery Rate (FDR) control:** Multiple comparison corrections for omics data
- **Gene set enrichment analysis (GSEA):** Pathway-level analysis methods
- **Limma-based differential expression:** Linear models for microarray/RNA-seq data
- **Pathway analysis methods:** Over-representation and functional enrichment testing

**High-Dimensional Validation:**
- **Permutation-based validation:** For high-dimensional diagnostic signatures
- **Cross-validation schemes:** Appropriate CV for high-dimensional medical data
- **Bootstrap methods:** Stability assessment for variable selection

### **Implementation Priority Assessment:**

🔥 **Immediate Need (Next 3 months):**
1. Cohen's kappa & ICC (Phase A) - Essential for diagnostic pathology
2. Bland-Altman plots & Passing-Bablok (Phase A) - Lab medicine standard
3. Likelihood ratios & diagnostic OR (Phase A) - Clinical utility focus
4. McNemar's test & enhanced non-parametric suite (Phase B)

⚡ **High Priority (3-6 months):**
1. Complete Phase A (Agreement & diagnostic accuracy)
2. Complete Phase B (Non-parametric methods)
3. Begin Phase C (Bayesian diagnostic methods)
4. Laboratory QC basics (Phase E)

🎯 **Medium Priority (6-12 months):**
1. Spatial statistics foundation (Phase D)
2. Meta-analysis tools (Phase F)
3. Advanced Bayesian methods (Phase C)
4. High-dimensional genomics methods (Phase G)

### **Jamovi-Specific Design Considerations:**

**Data Structure Compatibility:**
- **Tabular Format:** All analyses designed for standard jamovi spreadsheet data (rows = observations, columns = variables)
- **Variable Types:** Full support for jamovi's variable types (Continuous, Ordinal, Nominal, ID)
- **Missing Data:** Robust handling of missing data with listwise/pairwise deletion options
- **Large Datasets:** Memory-efficient algorithms for datasets >10,000 observations
- **Real-time Updates:** Results update automatically when data or options change

**User Interface Design Principles:**
- **Variable Selection:** Drag-and-drop variable assignment with appropriate type checking
- **Options Panels:** Logical grouping of options with sensible defaults
- **Progressive Disclosure:** Advanced options hidden initially, revealed as needed
- **Method Selection:** Radio buttons/dropdowns for alternative analysis methods
- **Output Control:** Checkboxes for optional tables/plots to control output complexity

**Output Integration:**
- **Table Formatting:** Consistent with jamovi table standards (APA formatting, exportable)
- **Plot Integration:** ggplot2-based visualizations that integrate with jamovi's plot system
- **Copy/Paste:** All results copyable to external software (SPSS, Excel, Word)
- **Export Options:** Direct export to multiple formats (CSV, HTML, PDF)
- **Interactive Elements:** Hover tooltips for interpretation guidance

**Clinical Integration Features:**
- **Interpretation Guides:** Built-in clinical interpretation for statistical results
- **Effect Size Reporting:** Automatic calculation and reporting of effect sizes
- **Confidence Intervals:** Default 95% CI with option to adjust level
- **Multiple Comparisons:** Automatic corrections when applicable
- **Sample Size Guidance:** Warnings for insufficient sample sizes

### **Integration with Current Roadmap:**

These pathology-specific statistical methods will complement our existing comprehensive survival analysis suite and provide the essential statistical foundation that pathology researchers need for:

- **Diagnostic accuracy studies** (sensitivity/specificity evaluations) - GUI-driven ROC analysis
- **Inter-observer agreement** (pathologist reproducibility) - Point-and-click kappa analysis  
- **Method comparison** (lab test validation) - Visual Bland-Altman plots with interpretation
- **Quality control** (laboratory performance monitoring) - Real-time control chart updates
- **Meta-analysis** (evidence synthesis for clinical practice) - Interactive forest plots

**Synergy with Existing Modules:**
- **Decision Analysis Integration:** Enhanced diagnostic accuracy feeds into decision curve analysis
- **Survival Analysis Extension:** Agreement statistics for survival model validation  
- **Cross-tabulation Enhancement:** Advanced categorical analysis beyond basic chi-square
- **Visualization Consistency:** Unified plot aesthetics across all ClinicoPath modules

---

## 💡 Feature Backlog & Ideas

This section contains features and ideas from previous brainstorming sessions that are not yet integrated into the core roadmaps. They will be reviewed and prioritized for future development cycles.

- **Stage Migration Analysis:**
  - Temporal comparison framework for Will Rogers phenomenon.
  - Migration correction methods (probability-weighted expectations).

- **Regulatory & Clinical Guidelines:**
  - REMARK & TRIPOD guideline checklists.
  - Registry compatibility assessments.

- **Advanced Validation & Staging:**
  - Transportability and geographic validation.
  - Systematic stage development (Bootstrap selection, orderly progressive condensation).
  - Simultaneous multi-cutpoint optimization.

- **Biomarker & Factor Integration:**
  - Inflammation-based biomarkers (NAR, SII, PNI).
  - Integration of treatment response and comorbidity factors.

- **Advanced Reporting:**
  - CONSORT-style flow diagrams.
  - Calibration belt plots.
