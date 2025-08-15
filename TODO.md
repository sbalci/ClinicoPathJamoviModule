# ClinicoPath Development Roadmap

This document outlines the development roadmap for the ClinicoPath Jamovi Module, consolidating various plans into a unified structure. It is organized by priority, analysis type, and long-term vision.

---


## 🎯 Immediate Priorities (Next 3-6 Months)

This is the high-priority queue of features planned for the upcoming development cycles.

---

## 🚀 Core Implementation Roadmaps

This section contains the detailed, multi-phase implementation plans for major functional areas of the module.

### 1. CRAN Task Views Integration Plan

This plan enhances ClinicoPath by systematically integrating capabilities from key CRAN Task Views.

-   **Phase 1: Core Clinical Research Tools ✅ COMPLETED**
    -   All Phase 1 components have been implemented and integrated.

-   **Phase 2: Advanced Analytics ✅ COMPLETED**
    -   **Machine Learning:** ✅ Clinical prediction models and biomarker discovery platforms with ML interpretability (SHAP, LIME) implemented (`clinicalprediction.b.R`, `biomarkerdiscovery.b.R`).
    -   **Psychometrics:** ✅ Patient-reported outcomes and Quality of Life analysis implemented (`patientreported.b.R`, `qualityoflife.b.R`).

-   **Phase 3: Specialized Clinical Methods** ✅ COMPLETED
    -   **Causal Inference:** ✅ Treatment effect estimation using propensity scores, IPTW, matching, and doubly robust estimation implemented (`treatmenteffects.b.R`).
    -   **Epidemiology:** ✅ Outbreak analysis and screening program evaluation implemented for tabular epidemiological data (`outbreakanalysis.b.R`, `screeningevaluation.b.R`).
    -   **Bayesian Methods:** ✅ Bayesian clinical analysis and adaptive trial design with comprehensive tabular data support implemented (`bayesianclinical.b.R`).

-   **Phase 4: Advanced Analytics Enhancement** ✅ COMPLETED
    -   **Enhanced Nonparametric Analysis:** ✅ Comprehensive nonparametric testing with modern methods implemented (`enhancednonparametric.b.R`).
    -   **Grafify Scientific Plots:** ✅ Scientific visualization with advanced statistical integration implemented (`grafify.b.R`).

-   **Phase 5: Future Specialized Applications** ✅ COMPLETED
    -   **Mixed Models:** ✅ Joint longitudinal-survival modeling implemented (`jointmodeling.b.R`).
    -   **Experimental Design:** ✅ Laboratory assay optimization implemented (`assayoptimization.b.R`).

### 2. Digital Pathology & AI/ML Integration Plan

A consolidated roadmap for digital pathology, AI, and machine learning features, emphasizing a full-pipeline approach from data QC to clinical translation.

-   **Phase 0: Foundational Quality Control** ✅ COMPLETED
    -   **Batch Effect Control:** ✅ PCA visualization and ComBat correction implemented (`batcheffect.b.R`).
    -   **Feature Quality Check:** ✅ Distribution analysis, outlier detection, and redundancy analysis implemented (`featurequality.b.R`).

-   **Phase 1: Core DP Statistics** ✅ COMPLETED
    -   **Enhanced Categorical Tests:** ✅ Effect sizes (Cramér's V) and residual analysis implemented (`categoricaladvanced.b.R`).
    -   **Advanced Agreement Statistics:** ✅ Enhanced Kappa, ICC, and Bland-Altman analysis implemented (`pathologyagreement.b.R`).

-   **Phase 2: Spatial & Hierarchical Analysis** ✅ COMPLETED
    -   **Spatial Statistics Framework:** ✅ Three-tier spatial analysis implemented (`spatialanalysis.b.R`).
    -   **Hierarchical Models:** ✅ Hierarchical pathology analysis implemented (`hierarchicalpathology.b.R`).

-   **Phase 3: AI/ML Model & Algorithm Evaluation** ✅ COMPLETED
    -   **Classification & Segmentation Metrics:** ✅ Comprehensive metrics implemented (`mlpathology.b.R`).
    -   **Model Interpretability:** ✅ SHAP and LIME integration implemented.

-   **Phase 4: Advanced Survival & Biomarker Analysis** ✅ COMPLETED
    -   **Optimal Cutpoint Determination:** ✅ Maximally selected rank statistics implemented (`optimalcutpoint.b.R`).

-   **Phase 5: Publication & Clinical Translation** ✅ COMPLETED
    -   **Natural Language Results:** ✅ Automatic generation of methods sections and plain-language summaries implemented (`nlresults.b.R`).
    -   **Visualization Excellence:** ✅ Publication-quality, colorblind-friendly plot templates implemented (`publicationplots.b.R`).
    -   **Clinical Calculators:** ✅ Risk score calculators and nomograms from image features implemented (`clinicalcalculators.a.yaml`).

### 3. Comprehensive Survival Analysis Roadmap (CRAN Task View Implementation)

This roadmap outlines the systematic implementation of advanced survival analysis capabilities based on the comprehensive CRAN Task View on Survival Analysis. The focus is on robust, validated methods specifically applicable to tabular clinical research data.

#### **Phase 1: Core Survival Distribution & Estimation** ⏳ IN PROGRESS
-   **Non-Parametric Estimation:**
    -   ✅ Kaplan-Meier estimator with confidence intervals (`survival`, `survminer`) - **IMPLEMENTED**
    -   ✅ Nelson-Aalen cumulative hazard estimator (`survival`) - **IMPLEMENTED**
    -   **Turnbull NPMLE for interval-censored data** (`Icens`, `MLEcens`, `interval`)
    -   **Product-limit estimator variants** (`prodlim`)
    -   **Smooth hazard estimation** (`muhaz`, `kerdiest`)

-   **Parametric Distribution Modeling:**
    -   ✅ **Weibull, Exponential, Log-normal, Log-logistic models** (`survival`, `flexsurv`) - **IMPLEMENTED**
    -   **Generalized Gamma and F-distributions** (`flexsurv`, `eha`)
    -   **Spline-based hazard functions** (`flexsurv`, `splineSurv`)
    -   **Distribution selection and goodness-of-fit** (`fitdistrplus`, `AdequacyModel`)

-   **Core Hypothesis Testing:**
    -   ✅ **Log-rank test** (`survival`) - **IMPLEMENTED**
    -   **Fleming-Harrington G-rho family tests** (`survival`)
    -   **Weighted log-rank tests** (`survMisc`)
    -   **Permutation tests for survival** (`coin`)

#### **Phase 2: Cox Regression & Advanced Modeling** ✅ PARTIALLY COMPLETED
-   **Cox Proportional Hazards Models:**
    -   ✅ **Standard Cox PH with diagnostics** (`survival`, `survminer`) - **IMPLEMENTED**
    -   ✅ **Stratified Cox models** (`survival`) - **IMPLEMENTED**
    -   **Penalized Cox regression** (`coxphf`, `penalized`)
    -   **High-dimensional data Cox models** (`glmnet`, `CoxBoost`)
    -   **Robust Cox regression** (`coxrobust`)

-   **Time-Varying Effects & Non-Proportional Hazards:**
    -   **Time-varying covariates in Cox models** (`survival`)
    -   **Aalen's additive hazard models** (`timereg`)
    -   **Flexible parametric models** (`rstpm2`)
    -   **Smoothly time-varying effects** (`smoothHR`, `timereg`)

-   **Accelerated Failure Time Models:**
    -   ✅ **Parametric AFT models** (`survival`, `flexsurv`) - **IMPLEMENTED**
    -   **Rank-based AFT estimation** (`aftgee`)
    -   **Robust AFT models** (`RobustAFT`)
    -   **Transformation models** (`tram`)

#### **Phase 3: Competing Risks & Multi-State Analysis** ✅ PARTIALLY COMPLETED
-   **Competing Risks Analysis:**
    -   ✅ **Cumulative Incidence Functions** (`cmprsk`, `etm`) - **IMPLEMENTED**
    -   ✅ **Fine-Gray subdistribution hazards** (`cmprsk`) - **IMPLEMENTED**
    -   **Cause-specific hazards modeling** (`survival`)
    -   **Direct binomial regression** (`timereg`)

-   **Multi-State Models:**
    -   **Markov multi-state models** (`mstate`, `msm`)
    -   **Semi-Markov models** (`SemiMarkov`)
    -   **Hidden Markov models for survival** (`msm`)
    -   **Illness-death models** (`p3state.msm`)

-   **Recurrent Event Analysis:**
    -   ✅ **Andersen-Gill and PWP models** (`survival`) - **IMPLEMENTED**
    -   ✅ **Frailty models for recurrent events** (`frailtypack`) - **IMPLEMENTED**
    -   **Marginal models for recurrent events** (`reReg`)
    -   **Joint frailty models** (`frailtypack`)

#### **Phase 4: Machine Learning & Advanced Prediction** 🔄 ENHANCED IMPLEMENTATION
-   **Tree-Based Methods:**
    -   ✅ **Survival trees** (`rpart`, `party`) - **IMPLEMENTED**
    -   ✅ **Random survival forests** (`randomForestSRC`) - **IMPLEMENTED**
    -   **Conditional inference trees** (`party`, `partykit`)
    -   **Gradient boosting for survival** (`gbm`, `mboost`)

-   **Regularized Survival Models:**
    -   ✅ **LASSO, Ridge, Elastic Net** (`glmnet`) - **IMPLEMENTED**
    -   **Adaptive LASSO for Cox models** (`glmnet`)
    -   **Group LASSO for survival** (`grplasso`)
    -   **Sparse group LASSO** (`SGL`)

-   **Neural Networks & Deep Learning:**
    -   **Neural networks for survival** (`nnet`, `neuralnet`)
    -   **Deep survival analysis** (`DeepSurv` integration)
    -   **Transformer models for survival** (custom implementation)

#### **Phase 5: Model Validation & Performance Assessment** 🔄 ENHANCED IMPLEMENTATION
-   **Prediction Performance Metrics:**
    -   ✅ **Time-dependent ROC curves** (`timeROC`, `survivalROC`) - **IMPLEMENTED**
    -   ✅ **Prediction error curves** (`pec`) - **IMPLEMENTED**
    -   **Concordance index and C-statistics** (`survC1`, `compareC`)
    -   **Integrated prediction error** (`pec`, `riskRegression`)
    -   **Net reclassification improvement** (`nricens`)

-   **Model Calibration & Validation:**
    -   **Calibration plots for survival models** (`rms`, `pec`)
    -   **Bootstrap validation** (`rms`)
    -   **Cross-validation for survival** (`pec`)
    -   **External validation frameworks** (`riskRegression`)

-   **Power Analysis & Sample Size:**
    -   **Log-rank test power** (`powerSurvEpi`)
    -   **Cox regression power** (`powerSurvEpi`)
    -   **Competing risks power** (`powerCompRisk`)
    -   **Non-inferiority trial design** (`nphRCT`)

#### **Phase 6: Specialized Survival Methods** 🆕 NEW IMPLEMENTATION
-   **Relative & Excess Mortality:**
    -   **Relative survival analysis** (`relsurv`, `popEpi`)
    -   **Excess mortality modeling** (`mexhaz`)
    -   **Population-based survival** (`popEpi`)
    -   **Cancer registry analysis** (`relsurv`)

-   **Joint & Longitudinal-Survival Models:**
    -   ✅ **Joint longitudinal-survival modeling** (`JM`, `joineRML`) - **IMPLEMENTED**
    -   **Shared parameter models** (`JMbayes`)
    -   **Dynamic predictions** (`JMbayes2`)
    -   **Landmark analysis** (`dynpred`)

-   **Interval-Censored & Complex Censoring:**
    -   ✅ **Interval-censored survival** (`icenReg`, `Icens`) - **IMPLEMENTED**
    -   **Doubly censored data** (`nada`)
    -   **Partly interval-censored data** (`PWEALL`)
    -   **Current status data** (`Icens`)

#### **Phase 7: Clinical Translation & Implementation** 🆕 NEW IMPLEMENTATION
-   **Prognostic Model Development:**
    -   **Nomogram construction** (`rms`, `nomogramFormula`)
    -   **Risk score calculators** (`PredictABEL`)
    -   **Decision curve analysis** (`rmda`, `DecisionCurve`)
    -   **Net benefit analysis** (`stdca`)

-   **Biomarker & Threshold Analysis:**
    -   ✅ **Optimal cutpoint determination** (`survminer`, `maxstat`) - **IMPLEMENTED**
    -   **Time-dependent biomarkers** (`timeROC`)
    -   **Landmark analysis for biomarkers** (`dynpred`)
    -   **Threshold regression** (`threg`)

-   **Clinical Decision Support:**
    -   **Interactive survival calculators** (Shiny integration)
    -   **Patient-specific predictions** (`rms`)
    -   **Treatment benefit calculators** (custom implementation)
    -   **Prognosis communication tools** (custom implementation)

#### **Phase 8: Specialized Clinical Applications** 🆕 NEW IMPLEMENTATION
-   **Cancer-Specific Survival Analysis:**
    -   **Cure models for cancer data** (`smcure`, `flexsurvcure`)
    -   **Tumor growth models** (`grofit`)
    -   **Treatment switching analysis** (`rpsftm`)
    -   **Progression-free survival** (specialized implementation)

-   **Cardiovascular & Chronic Disease:**
    -   **Heart failure progression models** (custom implementation)
    -   **Chronic kidney disease models** (custom implementation)
    -   **Multi-morbidity survival models** (custom implementation)

-   **Infectious Disease Survival:**
    -   **Epidemic survival analysis** (custom implementation)
    -   **Time-to-infection models** (custom implementation)
    -   **Vaccine efficacy survival models** (custom implementation)

#### **Implementation Priority Matrix:**
🔥 **High Priority (Next 6 months):**
- Phase 6: Relative survival and interval-censored enhancements
- Phase 5: Enhanced model validation and calibration
- Phase 7: Clinical nomograms and decision support tools

⚡ **Medium Priority (6-12 months):**
- Phase 4: Deep learning integration
- Phase 8: Disease-specific survival models
- Phase 2: Advanced time-varying effects

🔮 **Future Implementation (12+ months):**
- Specialized applications for emerging clinical needs
- Integration with electronic health records
- Real-time survival prediction systems

#### **Target Package Integration:**
**Core Packages:** `survival`, `survminer`, `flexsurv`, `cmprsk`, `mstate`, `frailtypack`
**ML Packages:** `randomForestSRC`, `glmnet`, `party`, `gbm`
**Validation Packages:** `timeROC`, `pec`, `riskRegression`, `rms`
**Specialized Packages:** `relsurv`, `icenReg`, `JMbayes2`, `smcure`


---

## 💡 Feature Backlog & Ideas

This section contains features and ideas from previous brainstorming sessions that are not yet integrated into the core roadmaps. They will be reviewed and prioritized for future development cycles.

-   **Stage Migration Analysis:**
    -   Temporal comparison framework for Will Rogers phenomenon.
    -   Cancer-specific templates (Pancreatic AJCC 8th, etc.).
    -   Migration correction methods (probability-weighted expectations).

-   **Regulatory & Clinical Guidelines:**
    -   REMARK & TRIPOD guideline checklists.
    -   Registry compatibility assessments.

-   **Advanced Validation & Staging:**
    -   Transportability and geographic validation.
    -   Systematic stage development (Bootstrap selection, orderly progressive condensation).
    -   Simultaneous multi-cutpoint optimization.

-   **Biomarker & Factor Integration:**
    -   Inflammation-based biomarkers (NAR, SII, PNI).
    -   Integration of treatment response and comorbidity factors.

-   **Advanced Reporting:**
    -   CONSORT-style flow diagrams.
    -   Calibration belt plots.
