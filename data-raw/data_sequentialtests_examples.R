#' Clinical Sequential Testing Examples
#'
#' @description
#' Comprehensive collection of realistic clinical scenarios demonstrating
#' different sequential testing strategies across medical specialties.
#' Shows how combining tests in series or parallel affects diagnostic
#' accuracy and clinical decision-making.
#'
#' @format A data frame with 15 rows and 25 variables:
#' \describe{
#'   \item{scenario}{Character. Clinical scenario category}
#'   \item{clinical_setting}{Character. Specific clinical context}
#'   \item{test1_name}{Character. Name of first/screening test}
#'   \item{test1_sens}{Numeric. Sensitivity of first test}
#'   \item{test1_spec}{Numeric. Specificity of first test}
#'   \item{test2_name}{Character. Name of second/confirmatory test}
#'   \item{test2_sens}{Numeric. Sensitivity of second test}
#'   \item{test2_spec}{Numeric. Specificity of second test}
#'   \item{prevalence}{Numeric. Disease prevalence in population}
#'   \item{strategy}{Character. Testing strategy (serial_positive, serial_negative, parallel)}
#'   \item{rationale}{Character. Clinical rationale for chosen strategy}
#'   \item{clinical_impact}{Character. Expected clinical impact}
#'   \item{combined_sens}{Numeric. Combined sensitivity of strategy}
#'   \item{combined_spec}{Numeric. Combined specificity of strategy}
#'   \item{test1_ppv}{Numeric. Positive predictive value of first test}
#'   \item{test1_npv}{Numeric. Negative predictive value of first test}
#'   \item{test2_ppv}{Numeric. Positive predictive value of second test}
#'   \item{test2_npv}{Numeric. Negative predictive value of second test}
#'   \item{combined_ppv}{Numeric. Combined positive predictive value}
#'   \item{combined_npv}{Numeric. Combined negative predictive value}
#'   \item{test1_plr}{Numeric. Positive likelihood ratio of first test}
#'   \item{test1_nlr}{Numeric. Negative likelihood ratio of first test}
#'   \item{test2_plr}{Numeric. Positive likelihood ratio of second test}
#'   \item{test2_nlr}{Numeric. Negative likelihood ratio of second test}
#'   \item{combined_plr}{Numeric. Combined positive likelihood ratio}
#'   \item{combined_nlr}{Numeric. Combined negative likelihood ratio}
#'   \item{strategy_benefit}{Character. Summary of strategy benefit}
#'   \item{ppv_improvement}{Character. PPV improvement description}
#' }
#'
#' @details
#' This dataset contains realistic clinical scenarios across different 
#' medical specialties demonstrating sequential testing strategies:
#' 
#' \strong{COVID-19 Testing:}
#' \itemize{
#'   \item Rapid antigen followed by RT-PCR confirmation
#'   \item Different prevalence settings (community, contacts, symptomatic)
#'   \item Serial positive strategy to reduce false positives
#' }
#' 
#' \strong{Cancer Screening:}
#' \itemize{
#'   \item Breast: Mammography followed by tissue biopsy
#'   \item Lung: Low-dose CT followed by PET-CT + biopsy
#'   \item Colorectal: FOBT followed by colonoscopy
#' }
#' 
#' \strong{Cardiovascular Disease:}
#' \itemize{
#'   \item Exercise stress test followed by cardiac catheterization
#'   \item Different risk populations
#'   \item Cost-effective non-invasive screening
#' }
#' 
#' \strong{Infectious Disease:}
#' \itemize{
#'   \item TB: Tuberculin skin test followed by chest X-ray + sputum
#'   \item Different risk populations and prevalence settings
#' }
#' 
#' \strong{Emergency Medicine:}
#' \itemize{
#'   \item MI rule-out: Troponin and ECG in parallel
#'   \item Time-sensitive diagnosis requiring high sensitivity
#' }
#' 
#' \strong{Rare Disease Screening:}
#' \itemize{
#'   \item Genetic disease: Basic panel followed by comprehensive testing
#'   \item Serial negative strategy to maximize sensitivity
#' }
#'
#' @examples
#' # Load the dataset
#' data(sequential_testing_examples)
#' 
#' # View COVID-19 testing scenarios
#' covid_scenarios <- sequential_testing_examples[
#'   sequential_testing_examples$scenario == "COVID-19 Testing", ]
#' print(covid_scenarios[, c("clinical_setting", "prevalence", "combined_ppv", "combined_npv")])
#' 
#' # Compare strategies across different scenarios
#' library(ggplot2)
#' ggplot(sequential_testing_examples, aes(x = combined_sens, y = combined_spec, color = scenario)) +
#'   geom_point(size = 3) +
#'   labs(title = "Sequential Testing Strategies: Sensitivity vs Specificity",
#'        x = "Combined Sensitivity", 
#'        y = "Combined Specificity") +
#'   theme_minimal()
#' 
#' # Analyze strategy benefits
#' strategy_summary <- sequential_testing_examples %>%
#'   group_by(strategy) %>%
#'   summarise(
#'     scenarios = n(),
#'     avg_sensitivity = mean(combined_sens),
#'     avg_specificity = mean(combined_spec),
#'     avg_ppv = mean(combined_ppv)
#'   )
#' print(strategy_summary)
#'
#' @seealso 
#' \code{\link{strategy_comparison}} for strategy comparison examples
#' \code{\link{cost_effectiveness_examples}} for economic analysis
#' \code{\link{teaching_examples}} for educational scenarios
#' \code{\link{common_test_combinations}} for reference test characteristics
#'
#' @source
#' Test characteristics compiled from published literature and clinical guidelines:
#' \itemize{
#'   \item COVID-19 testing: Cochrane reviews and WHO recommendations
#'   \item Cancer screening: Professional society guidelines and meta-analyses
#'   \item Cardiac testing: ACC/AHA guidelines and evidence-based protocols
#'   \item Infectious disease: CDC and WHO recommendations
#'   \item Emergency medicine: Society guidelines and clinical decision rules
#' }
#'
#' @keywords datasets sequential-testing clinical-scenarios diagnostic
# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

"sequential_testing_examples"


#' Strategy Comparison Examples
#'
#' @description
#' Demonstrates how different sequential testing strategies (serial positive,
#' serial negative, parallel) perform with identical test characteristics
#' across various prevalence settings. Educational dataset for understanding
#' strategy trade-offs.
#'
#' @format A data frame with 60 rows and 12 variables:
#' \describe{
#'   \item{test1_sens}{Numeric. Sensitivity of first test}
#'   \item{test1_spec}{Numeric. Specificity of first test}
#'   \item{test2_sens}{Numeric. Sensitivity of second test}
#'   \item{test2_spec}{Numeric. Specificity of second test}
#'   \item{prevalence}{Numeric. Disease prevalence}
#'   \item{scenario}{Character. "Strategy Comparison"}
#'   \item{test1_name}{Character. "Test A"}
#'   \item{test2_name}{Character. "Test B"}
#'   \item{strategy}{Character. Testing strategy}
#'   \item{combined_sens}{Numeric. Combined sensitivity}
#'   \item{combined_spec}{Numeric. Combined specificity}
#'   \item{combined_ppv}{Numeric. Combined positive predictive value}
#'   \item{combined_npv}{Numeric. Combined negative predictive value}
#' }
#'
#' @details
#' This dataset compares three sequential testing strategies:
#' 
#' \strong{Serial Positive (Confirmation):}
#' \itemize{
#'   \item Test 2 only performed if Test 1 positive
#'   \item Positive only if both tests positive
#'   \item Maximizes specificity, reduces sensitivity
#'   \item Best for avoiding false positives
#' }
#' 
#' \strong{Serial Negative (Exclusion):}
#' \itemize{
#'   \item Test 2 only performed if Test 1 negative
#'   \item Positive if either test positive
#'   \item Maximizes sensitivity, reduces specificity
#'   \item Best for avoiding false negatives
#' }
#' 
#' \strong{Parallel Testing:}
#' \itemize{
#'   \item Both tests performed on all subjects
#'   \item Positive if either test positive
#'   \item Improves sensitivity, reduces specificity
#'   \item Best for comprehensive evaluation
#' }
#'
#' @examples
#' # Load and analyze strategy comparisons
#' data(strategy_comparison)
#' 
#' # Compare strategies at different prevalence levels
#' library(dplyr)
#' library(ggplot2)
#' 
#' strategy_comparison %>%
#'   filter(prevalence == 0.10) %>%
#'   ggplot(aes(x = combined_sens, y = combined_spec, color = strategy)) +
#'   geom_point(alpha = 0.7, size = 3) +
#'   labs(title = "Strategy Comparison at 10% Prevalence",
#'        x = "Combined Sensitivity",
#'        y = "Combined Specificity") +
#'   theme_minimal()
#' 
#' # Analyze PPV across strategies
#' ppv_summary <- strategy_comparison %>%
#'   group_by(strategy, prevalence) %>%
#'   summarise(
#'     mean_ppv = mean(combined_ppv),
#'     mean_npv = mean(combined_npv)
#'   )
#' 
#' # Strategy performance heatmap
#' strategy_comparison %>%
#'   group_by(strategy, prevalence) %>%
#'   summarise(avg_ppv = mean(combined_ppv)) %>%
#'   ggplot(aes(x = factor(prevalence), y = strategy, fill = avg_ppv)) +
#'   geom_tile() +
#'   scale_fill_gradient(low = "red", high = "green", name = "Avg PPV") +
#'   labs(title = "Strategy Performance by Prevalence",
#'        x = "Disease Prevalence", y = "Testing Strategy") +
#'   theme_minimal()
#'
#' @seealso \code{\link{sequential_testing_examples}}, \code{\link{teaching_examples}}
#'
#' @keywords datasets strategy-comparison diagnostic-testing
"strategy_comparison"


#' Cost-Effectiveness Examples for Sequential Testing
#'
#' @description
#' Demonstrates economic considerations in sequential testing strategies,
#' showing how test costs, population size, and strategy choice affect
#' overall healthcare costs and diagnostic efficiency.
#'
#' @format A data frame with 3 rows and 15 variables:
#' \describe{
#'   \item{scenario}{Character. Cost scenario description}
#'   \item{test1_name}{Character. Name of first test}
#'   \item{test1_cost}{Numeric. Cost of first test}
#'   \item{test1_sens}{Numeric. Sensitivity of first test}
#'   \item{test1_spec}{Numeric. Specificity of first test}
#'   \item{test2_name}{Character. Name of second test}
#'   \item{test2_cost}{Numeric. Cost of second test}
#'   \item{test2_sens}{Numeric. Sensitivity of second test}
#'   \item{test2_spec}{Numeric. Specificity of second test}
#'   \item{prevalence}{Numeric. Disease prevalence}
#'   \item{strategy}{Character. Testing strategy}
#'   \item{population_size}{Numeric. Population size}
#'   \item{serial_total_cost}{Numeric. Total cost for serial strategy}
#'   \item{parallel_total_cost}{Numeric. Total cost for parallel strategy}
#'   \item{cost_savings}{Numeric. Cost savings from serial vs parallel}
#'   \item{cost_per_case_found}{Numeric. Cost per true positive identified}
#' }
#'
#' @details
#' This dataset illustrates economic factors in sequential testing:
#' 
#' \strong{High-Cost Confirmation:}
#' \itemize{
#'   \item Expensive second test (e.g., advanced imaging)
#'   \item Serial strategy reduces number needing expensive test
#'   \item Major cost savings compared to parallel testing
#' }
#' 
#' \strong{Rapid Screening:}
#' \itemize{
#'   \item Moderate-cost tests for rapid diagnosis
#'   \item Balance between speed and cost-effectiveness
#'   \item Laboratory confirmation strategy
#' }
#' 
#' \strong{Resource-Limited Setting:}
#' \itemize{
#'   \item Low-cost initial assessment
#'   \item Specialist referral for positives
#'   \item Maximizing diagnostic yield with limited resources
#' }
#'
#' @examples
#' # Load cost-effectiveness data
#' data(cost_effectiveness_examples)
#' 
#' # Visualize cost comparisons
#' library(ggplot2)
#' library(tidyr)
#' 
#' cost_data <- cost_effectiveness_examples %>%
#'   select(scenario, serial_total_cost, parallel_total_cost) %>%
#'   pivot_longer(cols = c(serial_total_cost, parallel_total_cost),
#'                names_to = "strategy", values_to = "total_cost") %>%
#'   mutate(strategy = ifelse(grepl("serial", strategy), "Serial", "Parallel"))
#' 
#' ggplot(cost_data, aes(x = scenario, y = total_cost, fill = strategy)) +
#'   geom_col(position = "dodge") +
#'   labs(title = "Cost Comparison: Serial vs Parallel Testing",
#'        x = "Clinical Scenario", y = "Total Cost ($)") +
#'   theme_minimal() +
#'   theme(axis.text.x = element_text(angle = 45, hjust = 1))
#' 
#' # Calculate cost-effectiveness metrics
#' cost_effectiveness_examples %>%
#'   mutate(
#'     savings_percent = (cost_savings / parallel_total_cost) * 100,
#'     efficiency_ratio = cost_per_case_found / test1_cost
#'   ) %>%
#'   select(scenario, savings_percent, cost_per_case_found, efficiency_ratio)
#'
#' @seealso \code{\link{sequential_testing_examples}}
#'
#' @keywords datasets cost-effectiveness healthcare-economics
"cost_effectiveness_examples"


#' Teaching Examples for Sequential Testing Concepts
#'
#' @description
#' Simplified examples designed for educational purposes to demonstrate
#' key concepts in sequential testing including perfect tests, complementary
#' testing, and sensitivity-specificity trade-offs.
#'
#' @format A data frame with 3 rows and 10 variables:
#' \describe{
#'   \item{scenario}{Character. Teaching scenario name}
#'   \item{test1_name}{Character. Name of first test}
#'   \item{test1_sens}{Numeric. Sensitivity of first test}
#'   \item{test1_spec}{Numeric. Specificity of first test}
#'   \item{test2_name}{Character. Name of second test}
#'   \item{test2_sens}{Numeric. Sensitivity of second test}
#'   \item{test2_spec}{Numeric. Specificity of second test}
#'   \item{prevalence}{Numeric. Disease prevalence}
#'   \item{strategy}{Character. Testing strategy}
#'   \item{teaching_point}{Character. Key learning objective}
#' }
#'
#' @details
#' Educational scenarios demonstrate:
#' 
#' \strong{Perfect Test Demonstration:}
#' \itemize{
#'   \item Shows theoretical maximum improvement
#'   \item Perfect specificity in confirmatory test
#'   \item Illustrates concept of diagnostic certainty
#' }
#' 
#' \strong{Complementary Tests:}
#' \itemize{
#'   \item Tests detect different disease subtypes
#'   \item Parallel strategy captures all variants
#'   \item Demonstrates additive diagnostic value
#' }
#' 
#' \strong{Trade-off Example:}
#' \itemize{
#'   \item High sensitivity vs high specificity tests
#'   \item Sequential strategy balances both needs
#'   \item Shows optimization for clinical context
#' }
#'
#' @examples
#' # Load teaching examples
#' data(teaching_examples)
#' 
#' # Display teaching points
#' teaching_examples %>%
#'   select(scenario, teaching_point) %>%
#'   knitr::kable()
#' 
#' # Calculate and compare outcomes
#' teaching_examples %>%
#'   mutate(
#'     combined_sens = case_when(
#'       strategy == "serial_positive" ~ test1_sens * test2_sens,
#'       strategy == "parallel" ~ test1_sens + test2_sens - (test1_sens * test2_sens)
#'     ),
#'     combined_spec = case_when(
#'       strategy == "serial_positive" ~ test1_spec + (1 - test1_spec) * test2_spec,
#'       strategy == "parallel" ~ test1_spec * test2_spec
#'     ),
#'     improvement_type = case_when(
#'       combined_sens > pmax(test1_sens, test2_sens) ~ "Sensitivity improved",
#'       combined_spec > pmax(test1_spec, test2_spec) ~ "Specificity improved",
#'       TRUE ~ "Balanced improvement"
#'     )
#'   ) %>%
#'   select(scenario, improvement_type, combined_sens, combined_spec)
#'
#' @seealso \code{\link{sequential_testing_examples}}
#'
#' @keywords datasets education teaching diagnostic-concepts
"teaching_examples"


#' Common Test Combinations Reference
#'
#' @description
#' Reference table of typical sensitivity and specificity ranges for 
#' commonly used clinical test combinations in sequential testing protocols.
#' Based on literature review and clinical guidelines.
#'
#' @format A data frame with 10 rows and 9 variables:
#' \describe{
#'   \item{clinical_area}{Character. Medical specialty}
#'   \item{condition}{Character. Clinical condition}
#'   \item{test1_name}{Character. First/screening test}
#'   \item{test1_sens_range}{Character. Literature sensitivity range}
#'   \item{test1_spec_range}{Character. Literature specificity range}
#'   \item{test2_name}{Character. Second/confirmatory test}
#'   \item{test2_sens_range}{Character. Literature sensitivity range}
#'   \item{test2_spec_range}{Character. Literature specificity range}
#'   \item{typical_prevalence}{Character. Typical prevalence range}
#'   \item{recommended_strategy}{Character. Recommended testing strategy}
#'   \item{clinical_rationale}{Character. Clinical reasoning}
#' }
#'
#' @details
#' Reference data for common clinical scenarios:
#' 
#' \strong{Infectious Disease:}
#' \itemize{
#'   \item HIV: ELISA followed by Western Blot
#'   \item TB: Tuberculin skin test followed by imaging/culture
#' }
#' 
#' \strong{Cardiology:}
#' \itemize{
#'   \item CAD: Exercise stress test followed by catheterization
#'   \item Heart failure: BNP followed by echocardiogram
#' }
#' 
#' \strong{Oncology:}
#' \itemize{
#'   \item Breast cancer: Mammography followed by biopsy
#'   \item Lung cancer: Chest X-ray followed by CT + biopsy
#' }
#' 
#' \strong{Emergency Medicine:}
#' \itemize{
#'   \item PE: D-dimer followed by CT angiogram
#'   \item MI: Troponin and ECG in parallel
#' }
#'
#' @examples
#' # Load reference data
#' data(common_test_combinations)
#' 
#' # View by clinical area
#' cardiology_tests <- common_test_combinations[
#'   common_test_combinations$clinical_area == "Cardiology", ]
#' print(cardiology_tests)
#' 
#' # Summary by recommended strategy
#' strategy_summary <- common_test_combinations %>%
#'   count(recommended_strategy) %>%
#'   arrange(desc(n))
#' print(strategy_summary)
#' 
#' # Display test combinations
#' common_test_combinations %>%
#'   mutate(test_combination = paste(test1_name, "→", test2_name)) %>%
#'   select(condition, test_combination, recommended_strategy, clinical_rationale) %>%
#'   knitr::kable()
#'
#' @source
#' Compiled from medical literature, clinical guidelines, and meta-analyses
#'
#' @keywords datasets reference medical-tests literature
"common_test_combinations"
