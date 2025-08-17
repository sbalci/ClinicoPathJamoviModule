# ClinicoPath Development Roadmap









### 3. Comprehensive Survival Analysis Roadmap (CRAN Task View Implementation)

This roadmap outlines the systematic implementation of advanced survival analysis capabilities based on the comprehensive CRAN Task View on Survival Analysis. The focus is on robust, validated methods specifically applicable to tabular clinical research data.

#### **Phase 1: Core Survival Distribution & Estimation** 🔄 IN PROGRESS

- **Non-Parametric Estimation:**
  - ✅ **Kaplan-Meier estimator with confidence intervals** (`survival`, `survminer`, `km.ci`) - **IMPLEMENTED**
  - ✅ **Nelson-Aalen cumulative hazard estimator** (`survival`) - **IMPLEMENTED**  
  - ✅ **Turnbull NPMLE for interval-censored data** (`Icens`, `MLEcens`, `interval`) - **IMPLEMENTED**
  - ✅ **Product-limit estimator variants** (`prodlim`) - **IMPLEMENTED**
  - ✅ **Smooth hazard estimation** (`muhaz`, `kerdiest`, `bshazard`) - **IMPLEMENTED** (`smoothhazard`)
  - **Survey-weighted survival estimates** (`survey`)

- **Parametric Distribution Modeling:**
  - ✅ **Weibull, Exponential, Log-normal, Log-logistic models** (`survival`, `flexsurv`) - **IMPLEMENTED**
  - **Generalized Gamma and F-distributions** (`flexsurv`, `eha`)
  - **Spline-based hazard functions** (`flexsurv`, `splineSurv`)
  - **Distribution selection and goodness-of-fit** (`fitdistrplus`, `AdequacyModel`)
  - **Flexible baseline distributions** (`flexsurv`, `tram`)
  - **Stratified parametric models** (`rstpm2`)

- **Core Hypothesis Testing:**
  - ✅ **Log-rank test and variants** (`survival`) - **IMPLEMENTED**
  - **Fleming-Harrington G-rho family tests** (`survival`)
  - **Weighted log-rank tests** (`survMisc`, `coin`)
  - **Permutation tests for survival** (`coin`)
  - **Restricted mean survival time tests** (`survRM2`, `SSRMST`)
  - **Median survival comparisons** (`survminer`)

#### **Phase 2: Cox Regression & Advanced Modeling** 🔄 IN PROGRESS

- **Cox Proportional Hazards Models:**
  - ✅ **Standard Cox PH with diagnostics** (`survival`, `survminer`, `rms`) - **IMPLEMENTED**
  - ✅ **Stratified Cox models** (`survival`) - **IMPLEMENTED**
  - **Penalized Cox regression** (`coxphf`, `penalized`)
  - **High-dimensional data Cox models** (`glmnet`, `CoxBoost`)
  - **Robust Cox regression** (`coxrobust`)
  - **Weighted Cox regression** (`coxphw`)
  - **Mixed-effects Cox models** (`coxme`)

- **Time-Varying Effects & Non-Proportional Hazards:**
  - **Time-varying covariates in Cox models** (`survival`)
  - **Aalen's additive hazard models** (`timereg`, `addhazard`)
  - **Flexible parametric models** (`rstpm2`)
  - **Smoothly time-varying effects** (`smoothHR`, `timereg`)
  - **Dynamic coefficient models** (`dynsurv`)
  - **Proportional hazards testing** (`PHeval`)

- **Accelerated Failure Time Models:**
  - ✅ **Parametric AFT models** (`survival`, `flexsurv`) - **IMPLEMENTED**
  - **Rank-based AFT estimation** (`aftgee`)
  - **Robust AFT models** (`RobustAFT`)
  - **Transformation models** (`tram`)
  - **GEE-based AFT models** (`aftgee`)

- **Frailty Models:**
  - ✅ **Shared frailty models** (`frailtypack`) - **IMPLEMENTED**
  - **Mixed-effects survival models** (`coxme`)
  - **EM-algorithm frailty models** (`frailtyEM`)
  - **Parametric frailty models** (`frailtySurv`)

#### **Phase 3: Competing Risks & Multi-State Analysis** 🔄 IN PROGRESS

- **Competing Risks Analysis:**
  - ✅ **Cumulative Incidence Functions** (`cmprsk`, `etm`) - **IMPLEMENTED**
  - ✅ **Fine-Gray subdistribution hazards** (`cmprsk`) - **IMPLEMENTED**
  - **Cause-specific hazards modeling** (`survival`)
  - **Direct binomial regression** (`timereg`)
  - **Competing risks power analysis** (`powerCompRisk`)
  - **Flexible competing risks models** (`riskRegression`)

- **Multi-State Models:**
  - **Markov multi-state models** (`mstate`, `msm`)
  - **Semi-Markov models** (`SemiMarkov`)
  - **Hidden Markov models for survival** (`msm`)
  - **Illness-death models** (`p3state.msm`)
  - **Flexible multi-state models** (`flexmsm`)
  - **Continuous-time Markov models** (`msm`)

- **Recurrent Event Analysis:**
  - ✅ **Andersen-Gill and PWP models** (`survival`) - **IMPLEMENTED**
  - ✅ **Frailty models for recurrent events** (`frailtypack`) - **IMPLEMENTED**
  - **Marginal models for recurrent events** (`reReg`)
  - **Joint frailty models** (`frailtypack`)
  - **Conditional GEE for gap times** (`condGEE`)
  - **Recurrent event data analysis** (`reda`)

#### **Phase 4: Machine Learning & Advanced Prediction** 🔄 IN PROGRESS

- **Tree-Based Methods:**
  - ✅ **Survival trees** (`rpart`, `party`) - **IMPLEMENTED**
  - ✅ **Random survival forests** (`randomForestSRC`) - **IMPLEMENTED**
  - **Conditional inference trees** (`party`, `partykit`)
  - **Gradient boosting for survival** (`gbm`, `mboost`, `xgboost`)
  - **Extremely randomized trees** (`ranger`)
  - **Bayesian additive regression trees** (`BART`)

- **Regularized Survival Models:**
  - ✅ **LASSO, Ridge, Elastic Net** (`glmnet`) - **IMPLEMENTED**
  - **Adaptive LASSO for Cox models** (`glmnet`)
  - **Group LASSO for survival** (`grplasso`)
  - **Sparse group LASSO** (`SGL`)
  - **Penalized Cox regression** (`penalized`)
  - **Smoothly clipped absolute deviation** (`ncvreg`)

- **Bayesian Methods:**
  - **Bayesian survival models** (`rstanarm`)
  - **Bayesian model averaging** (`BMA`)
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
  - **Net reclassification improvement** (`nricens`)
  - **Integrated discrimination improvement** (`survIDI`)

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
  - **Mixture cure models** (`smcure`)
  - **Promotion time cure models** (`miCoPTCM`)
  - **Flexible cure models** (`flexsurvcure`)
  - **Generalized odds rate cure models** (`GORCure`)
  - **Non-mixture cure models** (`NMCM`)

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
  - **Landmark analysis** (`landest`)
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

#### **Implementation Priority Matrix:**

🔥 **High Priority (Next 6 months):**

- **Phase 1**: Complete core survival distribution methods (smooth hazard estimation, survey-weighted estimates)
- **Phase 2**: Time-varying effects and frailty models (penalized Cox, mixed-effects, time-varying covariates)
- **Phase 3**: Advanced competing risks and multi-state models (multi-state transitions, recurrent events)
- **Phase 4**: Machine Learning integration (conditional inference trees, gradient boosting, Bayesian methods)

⚡ **Medium Priority (6-12 months):**

- **Phase 8**: Advanced survival methodology (pseudo-observations, flexible parametric models, conditional survival)
- **Phase 9**: Specialized clinical applications (cancer-specific methods, epidemiological applications, clinical trials)
- **Cure models and long-term survival** analysis (mixture and non-mixture approaches)

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

## 💡 Feature Backlog & Ideas

This section contains features and ideas from previous brainstorming sessions that are not yet integrated into the core roadmaps. They will be reviewed and prioritized for future development cycles.

- **Stage Migration Analysis:**
  - Temporal comparison framework for Will Rogers phenomenon.
  - Cancer-specific templates (Pancreatic AJCC 8th, etc.).
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
