#' Basic Decision Tree Test Data
#'
#' A dataset containing basic treatment comparison data for testing the decision tree graph module.
#' This dataset simulates a simple clinical decision between surgery and medical treatment.
#'
#' @format A data frame with 100 rows and 13 variables:
#' \describe{
#'   \item{patient_id}{Patient identifier (1-100)}
#'   \item{treatment}{Treatment type: Surgery or Medical Treatment}
#'   \item{hospital_type}{Hospital type: Academic or Community}
#'   \item{prob_success_surgery}{Probability of success for surgery (0.7-0.9)}
#'   \item{prob_success_medical}{Probability of success for medical treatment (0.5-0.7)}
#'   \item{prob_complications}{Probability of complications (0.1-0.3)}
#'   \item{prob_recurrence}{Probability of disease recurrence (0.2-0.4)}
#'   \item{cost_surgery}{Cost of surgery in USD (mean: 15,000)}
#'   \item{cost_medical}{Cost of medical treatment in USD (mean: 8,000)}
#'   \item{cost_complications}{Cost of managing complications in USD (mean: 5,000)}
#'   \item{cost_followup}{Follow-up costs in USD (mean: 2,000)}
#'   \item{cost_recurrence}{Cost of managing recurrence in USD (mean: 12,000)}
#'   \item{utility_success}{Utility value for successful treatment (0.8-0.95)}
#'   \item{utility_failure}{Utility value for treatment failure (0.4-0.6)}
#'   \item{utility_complications}{Utility value with complications (0.3-0.5)}
#'   \item{utility_recurrence}{Utility value with recurrence (0.2-0.4)}
#'   \item{clinical_outcome}{Clinical response: Complete/Partial/No Response}
#'   \item{adverse_events}{Adverse event severity: None/Mild/Moderate/Severe}
#' }
#' @source Simulated data for testing decision tree analysis
#' @examples
#' data(basic_decision_data)
#' head(basic_decision_data)
#' 
#' # Use in decision tree analysis
#' # In jamovi: meddecide > Decision > Decision Tree Graph
#' # Decisions: treatment
#' # Probabilities: prob_success_surgery, prob_success_medical
#' # Costs: cost_surgery, cost_medical
#' # Utilities: utility_success, utility_failure
"basic_decision_data"

#' Markov Model Decision Tree Test Data
#'
#' A dataset for testing complex Markov model decision trees with multi-state 
#' disease progression and time-dependent transitions.
#'
#' @format A data frame with 200 rows and 17 variables:
#' \describe{
#'   \item{state_id}{State identifier (1-200)}
#'   \item{treatment_strategy}{Treatment approach: Immediate/Watch and Wait/Combination}
#'   \item{patient_risk_group}{Risk stratification: Low/Intermediate/High Risk}
#'   \item{prob_healthy_to_sick}{Transition probability from healthy to sick state}
#'   \item{prob_sick_to_recovered}{Transition probability from sick to recovered}
#'   \item{prob_sick_to_dead}{Transition probability from sick to death}
#'   \item{prob_recovered_to_sick}{Transition probability from recovered to sick}
#'   \item{prob_progression}{Probability of disease progression}
#'   \item{cost_healthy_state}{Annual cost in healthy state (mean: 1,000)}
#'   \item{cost_sick_state}{Annual cost in sick state (mean: 8,000)}
#'   \item{cost_recovered_state}{Annual cost in recovered state (mean: 3,000)}
#'   \item{cost_treatment_annual}{Annual treatment cost (mean: 12,000)}
#'   \item{cost_monitoring}{Annual monitoring cost (mean: 2,000)}
#'   \item{utility_healthy}{Annual utility in healthy state (0.9-1.0)}
#'   \item{utility_sick}{Annual utility in sick state (0.4-0.7)}
#'   \item{utility_recovered}{Annual utility in recovered state (0.8-0.9)}
#'   \item{utility_treatment}{Annual utility during treatment (0.6-0.8)}
#'   \item{cycle_length}{Cycle length in years (typically 1)}
#'   \item{time_horizon}{Analysis time horizon in years (10-20)}
#' }
#' @source Simulated Markov model data
#' @examples
#' data(markov_decision_data)
#' head(markov_decision_data)
#' 
#' # Use for Markov model analysis
#' # Tree Type: Markov Model Tree
#' # Include transition probabilities and state-specific costs
"markov_decision_data"

#' Pharmaceutical Cost-Effectiveness Test Data
#'
#' A comprehensive dataset for pharmaceutical decision tree analysis, including
#' drug comparisons, dosing strategies, and health economic outcomes.
#'
#' @format A data frame with 150 rows and 20 variables:
#' \describe{
#'   \item{study_id}{Study identifier (1-150)}
#'   \item{drug_regimen}{Drug options: Drug A/B/C or Standard Care}
#'   \item{dosing_strategy}{Dosing approach: Standard/High/Personalized Dose}
#'   \item{administration}{Route: Oral/IV/Subcutaneous}
#'   \item{prob_response}{Probability of treatment response (varies by drug)}
#'   \item{prob_severe_ae}{Probability of severe adverse events}
#'   \item{prob_discontinuation}{Probability of treatment discontinuation}
#'   \item{cost_drug_per_cycle}{Cost per treatment cycle (varies by drug)}
#'   \item{cost_administration}{Administration cost (varies by route)}
#'   \item{cost_monitoring}{Monitoring costs (mean: 800)}
#'   \item{cost_ae_management}{Adverse event management cost (mean: 3,000)}
#'   \item{cost_progression}{Cost of disease progression (mean: 15,000)}
#'   \item{utility_response}{Utility value with treatment response (0.75-0.9)}
#'   \item{utility_stable}{Utility value with stable disease (0.6-0.75)}
#'   \item{utility_progression}{Utility value with progression (0.3-0.5)}
#'   \item{utility_severe_ae}{Utility value with severe AEs (0.4-0.6)}
#'   \item{progression_free_survival}{Progression-free survival in months}
#'   \item{overall_survival}{Overall survival in months}
#'   \item{quality_of_life_score}{Quality of life score (0-100)}
#' }
#' @source Simulated pharmaceutical trial data
#' @examples
#' data(pharma_decision_data)
#' head(pharma_decision_data)
#' 
#' # Complex drug comparison analysis
#' # Multiple decision variables and outcome measures
"pharma_decision_data"

#' Screening Program Cost-Effectiveness Test Data
#'
#' A dataset for analyzing cancer screening program cost-effectiveness with
#' test performance characteristics and population-specific parameters.
#'
#' @format A data frame with 120 rows and 21 variables:
#' \describe{
#'   \item{scenario_id}{Scenario identifier (1-120)}
#'   \item{screening_strategy}{Strategy: No/Annual/Biennial/Risk-Based Screening}
#'   \item{screening_method}{Test method: Method A/B/C}
#'   \item{target_population}{Population: General/High Risk/Genetic Risk}
#'   \item{sensitivity}{Test sensitivity (0.7-0.95)}
#'   \item{specificity}{Test specificity (0.8-0.98)}
#'   \item{positive_predictive_value}{PPV (0.1-0.5)}
#'   \item{negative_predictive_value}{NPV (0.95-0.999)}
#'   \item{disease_prevalence}{Disease prevalence (varies by population)}
#'   \item{prob_early_detection}{Probability of early detection (0.6-0.9)}
#'   \item{prob_cure_early}{Probability of cure if detected early (0.8-0.95)}
#'   \item{prob_cure_late}{Probability of cure if detected late (0.2-0.5)}
#'   \item{cost_screening_test}{Cost per screening test (varies by method)}
#'   \item{cost_diagnostic_workup}{Diagnostic workup cost (mean: 2,000)}
#'   \item{cost_treatment_early}{Early-stage treatment cost (mean: 25,000)}
#'   \item{cost_treatment_late}{Late-stage treatment cost (mean: 80,000)}
#'   \item{cost_false_positive}{False positive follow-up cost (mean: 1,500)}
#'   \item{utility_healthy}{Healthy state utility (0.95-1.0)}
#'   \item{utility_early_disease}{Early disease utility (0.8-0.9)}
#'   \item{utility_late_disease}{Late disease utility (0.4-0.6)}
#'   \item{utility_cured}{Cured state utility (0.9-0.95)}
#'   \item{utility_false_positive_anxiety}{False positive anxiety utility (0.85-0.95)}
#'   \item{life_years_gained}{Life years gained from screening}
#'   \item{quality_adjusted_life_years}{QALYs gained from screening}
#' }
#' @source Simulated screening program data
#' @examples
#' data(screening_decision_data)
#' head(screening_decision_data)
#' 
#' # Screening program cost-effectiveness analysis
#' # Test performance and population outcomes
"screening_decision_data"

#' Minimal Test Data for Decision Trees
#'
#' A simple dataset for basic functionality testing of the decision tree module.
#' Contains minimal variables needed to create a functional decision tree.
#'
#' @format A data frame with 10 rows and 9 variables:
#' \describe{
#'   \item{id}{Record identifier (1-10)}
#'   \item{treatment}{Treatment option: A or B}
#'   \item{prob1}{Probability of outcome 1 (0.5-0.9)}
#'   \item{prob2}{Probability of outcome 2 (complement of prob1)}
#'   \item{cost1}{Cost of outcome 1 (800-2200)}
#'   \item{cost2}{Cost of outcome 2 (1.5 × cost1)}
#'   \item{utility1}{Utility of outcome 1 (0.58-0.9)}
#'   \item{utility2}{Utility of outcome 2 (utility1 - 0.2)}
#'   \item{outcome}{Binary outcome: Success or Failure}
#' }
#' @source Minimal simulated data for testing
#' @examples
#' data(minimal_test_data)
#' minimal_test_data
#' 
#' # Basic decision tree example
#' # Perfect for learning the module interface
"minimal_test_data"

#' Edge Case Test Data for Decision Trees
#'
#' A dataset designed to test edge cases and error handling in the decision tree module.
#' Contains various problematic scenarios including missing values, extreme values,
#' and boundary conditions.
#'
#' @format A data frame with 20 rows and 11 variables:
#' \describe{
#'   \item{id}{Record identifier (1-20)}
#'   \item{treatment_missing}{Treatment with missing values (5 NAs)}
#'   \item{prob_zero}{Probabilities set to zero}
#'   \item{prob_one}{Probabilities set to one}
#'   \item{prob_negative}{Invalid negative probabilities}
#'   \item{prob_over_one}{Invalid probabilities > 1}
#'   \item{cost_zero}{Zero costs}
#'   \item{cost_negative}{Invalid negative costs}
#'   \item{cost_very_high}{Extremely high costs (1e8)}
#'   \item{utility_negative}{Invalid negative utilities}
#'   \item{utility_over_one}{Invalid utilities > 1}
#'   \item{single_treatment}{Single treatment category}
#'   \item{many_categories}{15 different categories}
#' }
#' @source Designed edge cases for robust testing
#' @examples
#' data(edge_case_data)
#' head(edge_case_data)
#' 
#' # Test error handling and data validation
#' # Check module robustness with problematic data
"edge_case_data"

#' Enhanced IHC and Molecular Pathology Comprehensive Dataset
#'
#' A comprehensive dataset representing modern molecular pathology practice, 
#' combining traditional immunohistochemistry with cutting-edge molecular diagnostics.
#' This dataset simulates real-world clinical laboratory scenarios including 
#' multi-platform validation, digital pathology integration, and precision oncology applications.
#' Educational focus: Training pathologists and clinicians in integrated morphologic 
#' and molecular diagnostics for personalized cancer care.
#'
#' @format A data frame with 400 rows and 50 variables:
#' \describe{
#'   \item{Patient_ID}{Character. Unique patient identifier (MP0001-MP0400)}
#'   \item{Age}{Numeric. Patient age in years (18-95)}
#'   \item{Gender}{Character. Patient gender (Male/Female)}
#'   \item{Institution}{Character. Institution type (Academic_Medical_Center/Community_Hospital/Cancer_Center/Regional_Hospital)}
#'   \item{Pathologist_Experience}{Character. Pathologist experience level (Trainee/Junior/Senior/Expert)}
#'   
#'   \item{Tumor_Type}{Character. Tumor classification including 16 major cancer types}
#'   \item{T_Stage}{Character. Primary tumor stage (T1-T4)}
#'   \item{N_Stage}{Character. Regional lymph node status (N0-N3)}
#'   \item{M_Stage}{Character. Distant metastasis status (M0/M1)}
#'   \item{Grade}{Numeric. Histologic grade (1-3)}
#'   
#'   \item{EGFR_Mutation}{Character. EGFR mutation status (Positive/Negative)}
#'   \item{KRAS_Mutation}{Character. KRAS mutation status (Positive/Negative)}
#'   \item{BRAF_Mutation}{Character. BRAF mutation status (Positive/Negative)}
#'   \item{PIK3CA_Mutation}{Character. PIK3CA mutation status (Positive/Negative)}
#'   \item{BRCA1_Mutation}{Character. BRCA1 mutation status (Positive/Negative)}
#'   \item{BRCA2_Mutation}{Character. BRCA2 mutation status (Positive/Negative)}
#'   
#'   \item{ALK_Fusion}{Character. ALK fusion gene status (Positive/Negative)}
#'   \item{ROS1_Fusion}{Character. ROS1 fusion gene status (Positive/Negative)}
#'   \item{RET_Fusion}{Character. RET fusion gene status (Positive/Negative)}
#'   
#'   \item{IDH1_Mutation}{Character. IDH1 mutation status (Positive/Negative)}
#'   \item{MGMT_Methylation}{Character. MGMT promoter methylation status (Positive/Negative)}
#'   \item{MSI_Status}{Character. Microsatellite instability status (MSI-High/MSI-Low/MSS)}
#'   \item{Tumor_Mutational_Burden}{Numeric. Mutations per megabase (1-50)}
#'   
#'   \item{HER2_IHC}{Character. HER2 immunohistochemistry score (0/1+/2+/3+)}
#'   \item{HER2_FISH_Ratio}{Numeric. HER2 FISH ratio (performed on 2+ and 3+ cases)}
#'   \item{HER2_FISH_Status}{Character. HER2 FISH amplification status (Amplified/Not_Amplified/Not_Performed)}
#'   \item{PD_L1_TPS}{Numeric. PD-L1 Tumor Proportion Score (0-100%)}
#'   \item{PD_L1_CPS}{Numeric. PD-L1 Combined Positive Score (0-100)}
#'   \item{PD_L1_IC}{Numeric. PD-L1 Immune Cell Score (0-100%)}
#'   \item{ER_Percentage}{Numeric. Estrogen receptor percentage (0-100%, breast cases only)}
#'   \item{PR_Percentage}{Numeric. Progesterone receptor percentage (0-100%, breast cases only)}
#'   
#'   \item{Ki67_Manual}{Numeric. Ki-67 proliferation index, manual count (1-90%)}
#'   \item{Ki67_AI_Assisted}{Numeric. Ki-67 proliferation index, AI-assisted count (0-100%)}
#'   \item{Ki67_Agreement}{Logical. Agreement between manual and AI scoring (within 5%)}
#'   \item{p53_Pattern}{Character. p53 staining pattern (Wild_type_pattern/Null_pattern/Overexpression)}
#'   \item{CD8_Count_per_mm2}{Numeric. CD8+ T-cell count per mm²}
#'   \item{TIL_Percentage}{Numeric. Tumor-infiltrating lymphocytes percentage (0-80%)}
#'   
#'   \item{Platform_A_PD_L1}{Numeric. PD-L1 score on platform A (0-100%)}
#'   \item{Platform_B_PD_L1}{Numeric. PD-L1 score on platform B (0-100%)}
#'   \item{Platform_Agreement}{Logical. Multi-platform agreement (within 10%)}
#'   
#'   \item{Specimen_Adequacy}{Character. Specimen quality assessment (Adequate/Suboptimal/Inadequate)}
#'   \item{Fixation_Time_Hours}{Numeric. Formalin fixation time in hours (2-24)}
#'   \item{Molecular_TAT_Days}{Numeric. Molecular testing turnaround time in days (3+)}
#'   \item{Diagnostic_Confidence}{Character. Pathologist confidence level (High/Moderate/Low)}
#'   \item{EQA_Score}{Numeric. External quality assessment score (60-100)}
#'   
#'   \item{Overall_Survival_Months}{Numeric. Overall survival in months (0.5-120)}
#'   \item{Death_Event}{Numeric. Death event indicator (0/1)}
#'   \item{PFS_Months}{Numeric. Progression-free survival in months}
#'   \item{Progression_Event}{Numeric. Progression event indicator (0/1)}
#'   \item{Treatment_Response}{Character. RECIST response (Complete_Response/Partial_Response/Stable_Disease/Progressive_Disease)}
#' }
#' @source Simulated comprehensive molecular pathology data
#' @examples
#' data(ihc_molecular_comprehensive)
#' head(ihc_molecular_comprehensive)
#' 
#' # Modern molecular pathology workflow example
#' # 1. Analyze mutation frequencies by tumor type
#' table(ihc_molecular_comprehensive$Tumor_Type, ihc_molecular_comprehensive$EGFR_Mutation)
#' 
#' # 2. Evaluate digital pathology agreement
#' mean(ihc_molecular_comprehensive$Ki67_Agreement)
#' 
#' # 3. Multi-platform biomarker validation
#' cor(ihc_molecular_comprehensive$Platform_A_PD_L1, 
#'     ihc_molecular_comprehensive$Platform_B_PD_L1, use = "complete.obs")
#' 
#' # 4. Quality metrics assessment
#' table(ihc_molecular_comprehensive$Specimen_Adequacy)
#' 
#' # Educational applications:
#' # - Precision oncology decision-making
#' # - Digital pathology validation studies  
#' # - Multi-platform biomarker harmonization
#' # - Quality assurance in molecular diagnostics
"ihc_molecular_comprehensive"

#' Biomarker Validation Study Dataset
#'
#' A focused dataset for multi-platform biomarker validation studies in pathology.
#' Represents real-world scenarios where the same biomarker is tested using different
#' platforms, antibodies, or scoring methods. Essential for understanding analytical
#' validation requirements in clinical molecular pathology.
#'
#' @format A data frame with 88 rows and 14 variables:
#' \describe{
#'   \item{Patient_ID}{Character. Patient identifier}
#'   \item{Institution}{Character. Testing institution type}
#'   \item{Pathologist_Experience}{Character. Scoring pathologist experience level}
#'   \item{Tumor_Type}{Character. Tumor type (Lung_Adenocarcinoma/Breast_Invasive_Ductal/Melanoma)}
#'   \item{PD_L1_TPS}{Numeric. PD-L1 Tumor Proportion Score, reference method}
#'   \item{Platform_A_PD_L1}{Numeric. PD-L1 score on validation platform A}
#'   \item{Platform_B_PD_L1}{Numeric. PD-L1 score on validation platform B}
#'   \item{Platform_Agreement}{Logical. Multi-platform agreement assessment}
#'   \item{Ki67_Manual}{Numeric. Ki-67 index by manual counting}
#'   \item{Ki67_AI_Assisted}{Numeric. Ki-67 index by AI-assisted counting}
#'   \item{Ki67_Agreement}{Logical. Manual vs AI agreement}
#'   \item{Specimen_Adequacy}{Character. Pre-analytical specimen quality}
#'   \item{Fixation_Time_Hours}{Numeric. Tissue fixation time impact}
#'   \item{EQA_Score}{Numeric. External quality assessment performance}
#' }
#' @source Subset of ihc_molecular_comprehensive focused on validation scenarios
#' @examples
#' data(biomarker_validation)
#' 
#' # Platform concordance analysis
#' plot(biomarker_validation$Platform_A_PD_L1, biomarker_validation$Platform_B_PD_L1)
#' 
#' # Digital pathology validation
#' plot(biomarker_validation$Ki67_Manual, biomarker_validation$Ki67_AI_Assisted)
#' 
#' # Quality factor impact assessment
#' boxplot(EQA_Score ~ Specimen_Adequacy, data = biomarker_validation)
"biomarker_validation"

#' Precision Oncology Dataset
#'
#' A targeted dataset focusing on actionable molecular alterations and treatment
#' outcomes in precision oncology. Includes clinically relevant biomarkers that
#' guide targeted therapy selection and treatment response assessment.
#'
#' @format A data frame with 116 rows and 17 variables:
#' \describe{
#'   \item{Patient_ID}{Character. Patient identifier}
#'   \item{Age}{Numeric. Patient age}
#'   \item{Gender}{Character. Patient gender}
#'   \item{Tumor_Type}{Character. Primary tumor type}
#'   \item{Grade}{Numeric. Histologic grade}
#'   \item{EGFR_Mutation}{Character. EGFR mutation for targeted therapy}
#'   \item{KRAS_Mutation}{Character. KRAS mutation status}
#'   \item{BRAF_Mutation}{Character. BRAF mutation for targeted therapy}
#'   \item{ALK_Fusion}{Character. ALK fusion for targeted therapy}
#'   \item{HER2_IHC}{Character. HER2 status for targeted therapy}
#'   \item{HER2_FISH_Status}{Character. HER2 amplification status}
#'   \item{PD_L1_TPS}{Numeric. PD-L1 for immunotherapy selection}
#'   \item{MSI_Status}{Character. Microsatellite instability for immunotherapy}
#'   \item{Treatment_Response}{Character. Treatment response outcome}
#'   \item{PFS_Months}{Numeric. Progression-free survival}
#'   \item{Progression_Event}{Numeric. Disease progression indicator}
#' }
#' @source Subset of ihc_molecular_comprehensive focused on precision oncology
#' @examples
#' data(precision_oncology)
#' 
#' # Biomarker-treatment response correlation
#' table(precision_oncology$EGFR_Mutation, precision_oncology$Treatment_Response)
#' 
#' # Immunotherapy biomarker analysis
#' boxplot(PFS_Months ~ MSI_Status, data = precision_oncology)
#' 
#' # Targeted therapy selection workflow
#' subset(precision_oncology, HER2_FISH_Status == "Amplified")
"precision_oncology"

#' Digital Pathology Validation Dataset
#'
#' A comprehensive dataset for validating AI-assisted pathology tools and digital
#' scoring systems. Includes traditional manual scoring alongside AI-assisted
#' measurements with agreement assessment and quality metrics.
#'
#' @format A data frame with 400 rows and 12 variables:
#' \describe{
#'   \item{Patient_ID}{Character. Patient identifier}
#'   \item{Institution}{Character. Institution implementing digital pathology}
#'   \item{Pathologist_Experience}{Character. Human scorer experience level}
#'   \item{Tumor_Type}{Character. Tumor type for context-specific validation}
#'   \item{Ki67_Manual}{Numeric. Manual Ki-67 proliferation index}
#'   \item{Ki67_AI_Assisted}{Numeric. AI-assisted Ki-67 proliferation index}
#'   \item{Ki67_Agreement}{Logical. Human-AI agreement assessment}
#'   \item{CD8_Count_per_mm2}{Numeric. CD8+ T-cell density}
#'   \item{TIL_Percentage}{Numeric. Tumor-infiltrating lymphocyte percentage}
#'   \item{p53_Pattern}{Character. p53 staining pattern classification}
#'   \item{Diagnostic_Confidence}{Character. Pathologist confidence in digital tools}
#'   \item{Specimen_Adequacy}{Character. Digital imaging quality assessment}
#' }
#' @source Subset of ihc_molecular_comprehensive focused on digital pathology
#' @examples
#' data(digital_pathology_validation)
#' 
#' # Human-AI agreement analysis
#' plot(digital_pathology_validation$Ki67_Manual, 
#'      digital_pathology_validation$Ki67_AI_Assisted)
#' abline(0, 1, col = "red")
#' 
#' # Agreement rate by pathologist experience
#' table(digital_pathology_validation$Pathologist_Experience, 
#'       digital_pathology_validation$Ki67_Agreement)
#' 
#' # Digital tool confidence assessment
#' table(digital_pathology_validation$Diagnostic_Confidence)
"digital_pathology_validation"