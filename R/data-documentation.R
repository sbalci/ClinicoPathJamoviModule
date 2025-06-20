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