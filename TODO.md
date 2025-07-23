# TODO: Updating Stagemigration Feature

## **Phase 1: Foundational Features (Core Functionality)**

This phase focuses on the essential tools for comparing two existing staging systems.

### **1. Core Migration Analysis & Visualization**

*Quantify and visualize the movement of patients between staging systems.*

- **Migration Matrix:** Generate a contingency table showing patient counts moving from an `old_stage` to a `new_stage`.
- **Migration Heatmap:** Create a graphical heatmap from the migration matrix to provide an intuitive overview of re-staging patterns.
- **Net Migration Summary:** For each stage, calculate the net gain or loss of patients to show how stage distributions have shifted.
- **Transition Matrix & Migration Entropy:** (Advanced) Model the probabilities of stage-to-stage transitions and calculate migration entropy to quantify the "uncertainty" or "dispersion" of re-staging.

### **2. Staging System Performance Comparison**

*Evaluate the prognostic performance of the old vs. new staging system.*

- **Comparative Survival Plots:** Generate side-by-side Kaplan-Meier plots for both systems, with log-rank tests to assess statistical significance.
- **Automated Monotonicity Check:** Automatically check that survival decreases as stage increases for both systems and flag any "inversions" (e.g., Stage IIB having better survival than Stage IIA).
- **C-index Comparison:** Calculate the Concordance Index (C-index) for both systems.
- **Model Fit Comparison:** Calculate and compare AIC and BIC scores for the Cox models of both systems.

### **3. Will Rogers Phenomenon Assessment**

*Provide tools to investigate whether survival improvements are genuine or a statistical artifact.*

- **Automated Cohort Analysis:** Automatically identify and group patients into "stable" (e.g., T2→T2) and "migrated" (e.g., T2→T3) cohorts.
- **Intermediate Prognosis Check:** Perform survival analysis on the stable and migrated groups to check if upstaged patients have an intermediate prognosis, which is evidence against the Will Rogers phenomenon.
- **Confounder Analysis:** Analyze the distribution of a key prognostic factor (e.g., lymph node status) across the migration groups.

---

## **Phase 2: Advanced Validation & Clinical Utility**

This phase adds rigorous statistical validation and methods to assess the real-world clinical utility of the staging systems.

### **1. Advanced Performance Metrics**

*Implement a broader suite of modern statistical tests for a more robust comparison.*

- **Formal C-index Difference Test:** Implement a statistical test (e.g., DeLong's test) to determine if the difference between two C-indices is statistically significant.
- **Time-Dependent AUC Analysis:** Plot the Area Under the ROC Curve (AUC) at various clinically relevant time points (e.g., 1, 3, 5 years).
- **Calibration Analysis:**
  - **Calibration Plots:** Generate plots of predicted vs. observed survival to assess model accuracy.
  - **Brier Score:** Calculate the Brier Score to get a single metric combining discrimination and calibration.
- **Proportion of Variation Explained (Pseudo R²):** Calculate a pseudo R² value (e.g., Nagelkerke's R²) for the Cox models to quantify the variance in survival explained by each system.

### **2. Clinical Utility Assessment**

*Evaluate which staging system is more useful for making real-world treatment decisions.*

- **Decision Curve Analysis (DCA):** Generate Net Benefit Plots to assess which staging system offers more clinical utility across a range of risk thresholds.
- **Net Reclassification Improvement (NRI) & IDI:** (Optional/Legacy) Provide NRI and IDI calculations, but with a note recommending DCA as the preferred modern alternative.

### **3. Internal Validation Framework**

*Assess the robustness of the findings and correct for statistical bias.*

- **Bootstrap Validation:** Implement bootstrap resampling to calculate optimism-corrected estimates of performance metrics like the C-index.
- **K-fold Cross-Validation:** Provide a framework for performing k-fold cross-validation to get a more honest estimate of model performance on unseen data.

---

## **Phase 3: Cutting-Edge & Developmental Features**

This phase includes features for developing new staging systems and handling complex clinical scenarios.

### **1. New Stage Development Tools**

*Provide functionality to create and optimize new staging systems from data.*

- **Optimal Cut-point Determination:** For a continuous variable (e.g., tumor size), find the optimal cut-point that creates the most statistically significant separation in survival outcomes. Include corrections for multiple testing.
- **Systematic Stage Grouping Optimization:** Use bootstrap resampling to systematically test different ways of grouping TNM categories or other variables to find the combination that maximizes prognostic performance.

### **2. Advanced Survival Modeling**

*Handle more complex real-world data and clinical questions.*

- **Competing Risks Analysis:** Implement Fine-Gray models and Cumulative Incidence Function (CIF) plots to correctly handle scenarios with multiple event types (e.g., cancer death vs. other causes).
- **Multi-institutional Validation (Frailty Models):** Allow for the analysis of data from multiple centers by using frailty models (mixed-effects Cox models) to account for institutional clustering.
- **Covariate & Subgroup Analysis:** Facilitate the comparison of staging systems within specific clinical subgroups (e.g., only LN-positive patients) and after adjusting for other clinical covariates (Age, Sex, etc.).

### **3. Model Interpretability & Reporting**

*Make complex models understandable and generate actionable reports.*

- **Model Interpretability (SHAP):** Integrate Shapley Additive Explanations (SHAP) to generate plots that explain which factors are driving the predictions of a complex model.
- **Automated Summary Report:** Generate a final summary report that synthesizes the results from all analyses, assesses the evidence for or against the new system, and provides a concluding recommendation.

---

## **Phase 4: Enhanced Features from ChatGPT Stage Migration Analysis**

*Additional features identified from comprehensive stage migration analysis document.*

### **1. Advanced Will Rogers Phenomenon Analysis**

*Enhance current Will Rogers assessment with mathematical rigor and simulation capabilities.*

- **Mathematical Will Rogers Detection:** Implement rigorous mathematical checks for overlapping distributions (μB < x < μA) to identify stage migration artifacts
- **Simulation-Based Validation:** Add R simulation capabilities to demonstrate Will Rogers effects with synthetic data and validate findings
- **Intermediate Prognosis Analysis:** Enhanced detection of patients with intermediate survival (worse than origin stage average, better than destination stage average)
- **Stage Migration Entropy Calculation:** Quantify migration "uncertainty" using information theory metrics
- **Temporal Comparison Framework:** Compare stage-specific survival across different time periods to detect artifacts vs. real improvements

### **2. Advanced Statistical Comparisons**

*Implement sophisticated model comparison techniques.*

- **DeLong's Test for C-index Comparison:** Formal statistical testing for C-index differences with proper correlation handling
- **Likelihood Ratio Chi-square Comparison:** Enhanced LR χ² analysis for nested and non-nested model comparisons
- **Linear Trend Chi-square:** Test for ordinal trends across staging systems
- **Multiple Pseudo R² Metrics:** Implement Nagelkerke, McFadden, Cox-Snell, and Royston R² measures
- **Proportion of Variation Explained:** Calculate and compare variance explained by each staging system

### **3. Enhanced Reclassification Analysis**

*Improve current reclassification metrics with advanced methodologies.*

- **Net Reclassification Improvement (NRI) Suite:**
  - Category-Free NRI (continuous risk scores)
  - Clinical NRI (clinically relevant thresholds)
  - Directional NRI (separate upstaging/downstaging analysis)
  - Weighted NRI (emphasizing high-risk classifications)
- **Integrated Discrimination Improvement (IDI):** Both relative and absolute IDI calculations
- **Reclassification Table Analysis:** Enhanced cross-tabulation with proportion calculations and statistical testing
- **Migration Direction Analysis:** Detailed upstaging vs. downstaging pattern assessment

### **4. Real-World Clinical Examples Integration**

*Add cancer-type specific analysis templates and validation approaches.*

- **Cancer-Specific Templates:**
  - Pancreatic cancer AJCC 8th edition validation templates
  - Colorectal cancer lymph node examination effects
  - Thyroid cancer age threshold migration analysis
  - Lung cancer imaging advancement effects
- **Institutional Comparison Framework:** Multi-center analysis with institutional clustering effects
- **Era Comparison Tools:** Temporal analysis accounting for diagnostic advancement bias
- **Morphologic Feature Integration:** Tools for adding new prognostic factors (size, invasion patterns, etc.)

### **5. Advanced Visualization Enhancements**

*Improve current visualization with publication-quality plots.*

- **Side-by-Side KM Comparison Plots:** Enhanced comparative survival visualization
- **Migration Flow Diagrams:** Sankey-style patient flow visualization between staging systems
- **Before/After Survival Curve Overlays:** Visual demonstration of Will Rogers effects
- **Performance Metric Dashboard:** Comprehensive visual summary of all comparison metrics
- **Stage-Specific Hazard Ratio Forest Plots:** Visual representation of stage-specific risks

### **6. Methodological Validation Framework**

*Implement rigorous validation approaches from staging literature.*

- **Bootstrap Optimism Correction:** Enhanced bootstrap validation with optimism-corrected performance estimates
- **Cross-Validation Framework:** K-fold validation with proper handling of clustered data
- **External Validation Tools:** Framework for validating staging systems on independent cohorts
- **Sensitivity Analysis:** Robust analysis under different assumptions and scenarios
- **Assumption Testing:** Formal tests for proportional hazards and other model assumptions

### **7. Clinical Decision Support Enhancements**

*Improve current clinical integration with evidence-based recommendations.*

- **Evidence Strength Grading:** Systematic assessment of evidence quality (strong/moderate/weak/insufficient)
- **Implementation Readiness Assessment:** Clinical readiness evaluation for staging system adoption
- **Treatment Implication Analysis:** Assessment of how staging changes affect treatment decisions
- **Cost-Effectiveness Considerations:** Framework for evaluating staging system implementation costs
- **Guideline Integration:** Tools for aligning with national cancer staging guidelines

### **8. Advanced Quality Control**

*Ensure robust analysis with comprehensive quality checks.*

- **Data Quality Assessment:** Automated checks for missing data, outliers, and inconsistencies
- **Sample Size Adequacy:** Power analysis and sample size recommendations for staging studies
- **Event Rate Analysis:** Assessment of event rates across stages for statistical power
- **Censoring Pattern Analysis:** Evaluation of censoring mechanisms and their impact
- **Convergence Diagnostics:** Model fitting quality assessment and convergence checking

---

## **Phase 5: Advanced Features from Claude Stage Migration Analysis**

*Sophisticated features identified from comprehensive statistical methods document.*

### **1. Advanced Cox Modeling Enhancements**

*Implement sophisticated survival modeling techniques for staging validation.*

- **Schoenfeld Residuals Testing:** Comprehensive proportional hazards assumption verification with graphical diagnostics
- **Time-Varying Coefficients:** Handle violations of proportional hazards through time-dependent coefficient modeling
- **Stratified Cox Models:** Implement stratification for variables violating PH assumption while comparing staging systems
- **Frailty Models for Clustering:** Mixed-effects Cox models (coxme) for multi-institutional data with center-specific random effects
- **Martingale Residual Analysis:** Detect nonlinearity and outliers in Cox model assumptions

### **2. Enhanced Stage Migration Quantification**

*Sophisticated mathematical approaches to stage migration analysis.*

- **Stage Migration Effect Formula:** Implement mathematical quantification: SME = (S₁' - S₁) + (S₂' - S₂)
- **Transition Matrix Modeling:** Probability-weighted migration matrices with confidence intervals
- **Migration Correction Methods:** Apply probability-weighted expectations to adjust for migration effects
- **Nested Model Comparison:** Likelihood ratio tests for nested staging systems with proper df calculation
- **Stage-Specific Migration Impact:** Quantify survival impact by migration direction and magnitude

### **3. Advanced Calibration Assessment**

*Comprehensive calibration evaluation beyond basic plots.*

- **Calibration Slope Analysis:** Calculate and interpret calibration slopes (ideal = 1.0)
- **Flexible Spline-Based Calibration:** Implement spline curves for non-linear calibration assessment
- **Time-Dependent Calibration:** Evaluate calibration at multiple time points across follow-up
- **Calibration-in-the-Large:** Assess systematic over/under-prediction across risk spectrum
- **Integrated Brier Score:** Combine calibration and discrimination into single time-integrated metric

### **4. Regulatory and Clinical Guidelines Integration**

*Align with international staging standards and requirements.*

- **AJCC/UICC Compliance Checking:** Automated verification of staging criteria compatibility
- **REMARK Guidelines Adherence:** 20-item checklist for prognostic marker studies
- **TRIPOD Statement Compliance:** 27-item checklist for prediction model development
- **Registry Compatibility Assessment:** Ensure new staging aligns with cancer registry systems
- **Timeline Projection Tools:** Estimate development-validation-implementation timeline (5-7 years)

### **5. Advanced Validation Methodologies**

*State-of-the-art validation approaches from recent literature.*

- **Internal-External Cross-Validation:** Leave-one-center-out validation for multi-institutional data
- **Optimism-Corrected Performance:** Bootstrap-based optimism correction with confidence intervals
- **Transportability Assessment:** Evaluate performance across different populations and settings
- **Temporal Validation:** Assess performance on data from different time periods
- **Geographic Validation:** Test staging system across different geographic regions

### **6. Complex Survival Data Handling**

*Address advanced censoring and competing events scenarios.*

- **Interval Censoring Support:** icenReg implementation for events detected between visits
- **Informative Censoring Detection:** Tests and adjustments for non-random censoring patterns
- **Fine-Gray Models:** Competing risks regression with cmprsk package integration
- **Multi-State Models:** Complex transitions between disease states (msm package)
- **Cure Models:** Mixture models for populations with cured fraction

### **7. Enhanced Discrimination Metrics**

*Sophisticated discrimination assessment beyond basic C-index.*

- **Time-Dependent AUC Curves:** timeROC implementation with confidence bands
- **Integrated Discrimination Index:** Time-integrated area between survival curves
- **Concordance Probability Estimates:** Alternative concordance measures for heavily censored data
- **Restricted Mean Survival Time:** RMST-based discrimination for specific time horizons
- **Win Ratio Analysis:** Composite endpoint analysis for staging comparison

### **8. Clinical Decision Analysis Integration**

*Bridge statistical analysis with clinical decision-making.*

- **Net Benefit Curves:** Decision curve analysis across probability thresholds
- **Clinical Utility Index:** Combine sensitivity/specificity with disease prevalence
- **Treatment Benefit Modeling:** Assess staging impact on treatment selection
- **Number Needed to Treat:** Calculate NNT based on staging-guided interventions
- **Risk Stratification Tables:** Generate clinically actionable risk categories

### **9. Machine Learning Integration**

*Modern ML approaches for staging development and validation.*

- **Random Survival Forests:** Non-parametric alternative to Cox models
- **Deep Learning Survival Models:** Neural network approaches for complex interactions
- **Ensemble Methods:** Combine multiple models for robust predictions
- **Feature Importance Analysis:** SHAP values and permutation importance
- **Cross-Validation for ML:** Proper CV strategies for survival ML models

### **10. Reporting and Visualization Excellence**

*Publication-ready outputs aligned with journal requirements.*

- **CONSORT-Style Flow Diagrams:** Patient flow through staging analysis
- **Forest Plots with Subgroups:** Stage-specific and subgroup-specific hazard ratios
- **Calibration Belt Plots:** Advanced calibration visualization with confidence regions
- **Nomogram Generation:** Clinical prediction tools from staging models
- **Interactive Web Reports:** Shiny-based interactive staging comparison dashboards

### **11. Sample Size and Power Analysis**

*Rigorous planning tools for staging studies.*

- **Event-Per-Variable Calculations:** EPV requirements for reliable estimates (10-20 EPV)
- **Power for C-index Comparison:** Sample size for detecting meaningful C-index differences
- **Multi-Stage Power Analysis:** Power calculations for hierarchical staging comparisons
- **Simulation-Based Power:** Monte Carlo methods for complex staging scenarios
- **Adaptive Sample Size:** Interim analysis and sample size re-estimation

### **12. Biomarker Integration Framework**

*Tools for incorporating molecular markers into staging.*

- **Biomarker Cutpoint Optimization:** Optimal thresholds for continuous biomarkers
- **Multi-Marker Panels:** Combine multiple biomarkers with anatomic staging
- **Cost-Effectiveness Analysis:** Evaluate biomarker addition from economic perspective
- **Missing Biomarker Handling:** Multiple imputation for incomplete biomarker data
- **Validation of Biomarker Staging:** Specific validation approaches for integrated staging

---

## **Phase 6: Advanced Features from Gemini Stage Migration Analysis**

*Comprehensive features identified from detailed statistical methods and clinical implementation guide.*

### **1. Systematic Stage Development Framework**

*Advanced methodologies for creating optimal staging systems from data.*

- **Bootstrap Model/Cutpoint Selection Method:** Systematic search across candidate staging systems using bootstrap-estimated criteria
- **Orderly Progressive Condensation:** Define T and N category groupings with partial ordering constraints
- **Minimum Stage Size Constraints:** Ensure each stage contains ≥5% of sample for statistical stability
- **Eligible System Generation:** Algorithm to generate all valid candidate staging systems adhering to pre-specified rules
- **Maximization Bias Correction:** 10-fold cross-validation to address overly optimistic performance estimates

### **2. Advanced Optimal Cutpoint Determination**

*Sophisticated approaches for continuous variable categorization.*

- **Minimum p-value/Maximum Statistic Approach:** Conservative method for optimal cutpoint selection
- **Selection Interval Constraints:** Exclude outer 10-20% of distribution to maintain statistical power
- **Multiple Testing Corrections:** Adjust for multiple comparisons when testing various cutpoints
- **Outcome-Oriented Cutpoints:** Maximally selected rank statistics via maxstat package
- **Cutpoint Variability Assessment:** Bootstrap variability estimation for cutpoint stability

### **3. Model Interpretability Enhancement**

*Address the "black box" problem in staging predictions.*

- **SHAP Summary Plots:** Visualize direction and magnitude of feature impacts across dataset
- **SHAP Bar Plots:** Rank features by average absolute SHAP values for importance hierarchy
- **SHAP Dependence Plots:** Reveal non-linear relationships and feature interactions
- **Individual Force Plots:** Patient-specific prediction explanations showing feature contributions
- **Global vs Local Interpretability:** Framework for both population-level and individual-level explanations

### **4. Stage Migration Quantification Tools**

*Advanced methods for analyzing Will Rogers phenomenon.*

- **Migration Heatmap Visualization:** Matrix plot showing patient flow between old/new stages
- **Immune Response Hypothesis Testing:** Assess if lymph node count reflects immune response vs. understaging
- **Lymph Node Yield Analysis:** Correlate node count with survival independent of positivity
- **Temporal Migration Tracking:** Monitor stage distribution changes over diagnostic eras
- **Multi-Factor Migration Analysis:** Separate effects of diagnostic improvement vs. biological factors

### **5. Clinical Implementation Framework**

*Bridge research findings to practical clinical application.*

- **Online Platform Deployment:** Web calculator implementation for real-time risk assessment
- **Electronic Health Record Integration:** Embed staging tools within clinical workflows
- **Visual Decision Support Tools:** Interactive interfaces for personalized treatment planning
- **Clinical Workflow Optimization:** Streamline staging assessment at point of care
- **User-Friendly Interface Design:** Ensure accessibility for non-technical clinical users

### **6. Advanced Performance Metrics Implementation**

*Sophisticated measures beyond standard discrimination metrics.*

- **Landmark Analysis Framework:** Convert survival to binary outcomes at clinically relevant timepoints
- **Explained Variation (π̂):** Landmark-based measure of prognostic power
- **Concordance Probability (K̂):** Generalized C-index accounting for all observation pairs
- **Cure Rate Modeling:** Special handling for diseases with potential cure fraction
- **Non-Proportional Hazards Solutions:** Alternative metrics when PH assumption violated

### **7. Comprehensive Validation Strategies**

*Rigorous internal and external validation approaches.*

- **Bootstrap Confidence Intervals:** Robust inference for all candidate staging systems
- **Relative Performance Assessment:** Compare multiple candidate systems simultaneously
- **Cross-Validation Adjusted Staging:** Unbiased performance estimates via k-fold CV
- **Independent Cohort Requirements:** Minimum 200 patients basic, 500 for advanced metrics
- **Multi-Institutional Consistency:** Validate across diverse healthcare settings

### **8. R Package Integration Suite**

*Leverage specialized R packages for comprehensive analysis.*

- **SurvMetrics Integration:** Enhanced C-index calculations for tied survival data
- **calibmsm for Multistate Models:** Advanced calibration assessment for complex transitions
- **dcurves/DecisionCurve:** Comprehensive decision curve analysis implementation
- **maxstat via survminer:** Optimal cutpoint determination with survival outcomes
- **cutpointr Framework:** Tidy approach to cutpoint optimization with bootstrapping

### **9. Biomarker and Additional Factor Integration**

*Incorporate molecular and clinical factors beyond TNM.*

- **Inflammation-Based Biomarkers:** NAR, SII, PNI integration into staging
- **Pathology Grade Integration:** Incorporate tumor differentiation into staging
- **Molecular Alterations:** BRAF mutations, tumor markers (CEA) as staging factors
- **Treatment Response Factors:** Surgery status, adjuvant therapy considerations
- **Comorbidity Adjustment:** Account for patient factors affecting prognosis

### **10. Quality Assurance and Regulatory Compliance**

*Ensure staging systems meet clinical and regulatory standards.*

- **Minimum Sample Size Guidelines:** 100 events for basic, 200+ for robust validation
- **Staging Consistency Verification:** Automated checks for classification consistency
- **Missing Data Pattern Analysis:** Identify and handle systematic missingness
- **Event Definition Validation:** Ensure consistent outcome definitions across cohorts
- **Cancer-Specific Validation:** Tailored approaches for different malignancy types

### **11. Advanced Visualization Suite**

*Publication-quality graphics for staging analysis.*

- **Calibration Plot Enhancement:** Show agreement between predicted and observed risks
- **Stage-Specific Survival Curves:** Clear visualization of prognostic separation
- **Migration Flow Diagrams:** Sankey-style visualization of patient reclassification
- **Time-Dependent Performance Plots:** Show discrimination metrics across follow-up time
- **Interactive Dashboard Components:** Dynamic visualizations for exploring results

### **12. Clinical Translation Tools**

*Facilitate adoption of new staging systems in practice.*

- **Clinical Interpretation Guides:** Translate statistical findings to clinical language
- **Implementation Timelines:** Project 5-7 year development-validation-adoption cycle
- **Stakeholder Communication Tools:** Materials for oncologists, pathologists, registrars
- **Training Module Development:** Educational resources for new staging criteria
- **Performance Monitoring Framework:** Track real-world staging system performance
