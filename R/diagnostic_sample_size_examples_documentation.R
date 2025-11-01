#' Diagnostic Sample Size Planning Examples
#'
#' Six clinical scenarios demonstrating sample size planning for diagnostic test
#' accuracy studies using Clopper-Pearson exact binomial confidence intervals.
#' Based on Bujang MA (2023) Diagnostics 13(8):1390.
#'
#' @format A data frame with 6 rows and 13 variables:
#' \describe{
#'   \item{scenario}{Name of the diagnostic test scenario}
#'   \item{population}{Target population description}
#'   \item{prevalence}{Disease prevalence in target population (0-1)}
#'   \item{target_sensitivity}{Target sensitivity value (0-1)}
#'   \item{target_specificity}{Target specificity value (0-1)}
#'   \item{ci_width}{Desired 95\% CI width}
#'   \item{study_purpose}{Study purpose: "diagnostic", "screening_sens", or "screening_spec"}
#'   \item{expected_n_sens}{Expected sample size for sensitivity estimation}
#'   \item{expected_n_spec}{Expected sample size for specificity estimation}
#'   \item{final_n}{Final required sample size (maximum of sensitivity/specificity)}
#'   \item{notes}{Clinical notes and justification}
#'   \item{nonresponse_rate}{Expected non-response rate (\%)}
#'   \item{final_n_adjusted}{Final sample size adjusted for non-response}
#' }
#'
#' @details
#' The dataset includes six diverse clinical scenarios:
#'
#' \strong{1. Colorectal Cancer Blood Test}
#' \itemize{
#'   \item Population: High-risk patients (age >50, family history)
#'   \item Prevalence: 10\%
#'   \item Purpose: Diagnostic (need excellent sensitivity AND specificity)
#'   \item Required N: 940 subjects
#' }
#'
#' \strong{2. COVID-19 Rapid Antigen Test}
#' \itemize{
#'   \item Population: General population (asymptomatic screening)
#'   \item Prevalence: 5\%
#'   \item Purpose: Screening (emphasize sensitivity)
#'   \item Required N: 1,880 subjects
#' }
#'
#' \strong{3. AI-Based Diabetic Retinopathy Detection}
#' \itemize{
#'   \item Population: Diabetic patients
#'   \item Prevalence: 30\%
#'   \item Purpose: Diagnostic with moderate precision
#'   \item Required N: 147 subjects
#' }
#'
#' \strong{4. Rare Disease Biomarker (Fabry Disease)}
#' \itemize{
#'   \item Population: Suspected patients referred to genetics clinic
#'   \item Prevalence: 2\% (very low!)
#'   \item Purpose: Diagnostic
#'   \item Required N: 7,900 subjects (large due to low prevalence)
#' }
#'
#' \strong{5. Lung Cancer LDCT Screening}
#' \itemize{
#'   \item Population: Heavy smokers (>30 pack-years)
#'   \item Prevalence: 15\%
#'   \item Purpose: Screening (emphasize specificity to reduce false positives)
#'   \item Required N: 4,020 subjects
#' }
#'
#' \strong{6. Digital Mammography Screening}
#' \itemize{
#'   \item Population: Women age 50-70 (recalled for further testing)
#'   \item Prevalence: 50\% (enriched population)
#'   \item Purpose: Diagnostic with moderate precision
#'   \item Required N: 88 subjects
#' }
#'
#' @source
#' Bujang MA (2023). An Elaboration on Sample Size Planning for Performing a
#' One-Sample Sensitivity and Specificity Analysis by Basing on Calculations on
#' a Specified 95\% Confidence Interval Width. Diagnostics 13(8):1390.
#' \doi{10.3390/diagnostics13081390}
#'
#' @examples
#' # Load the example scenarios
#' data(diagnostic_sample_size_examples)
#'
#' # View all scenarios
#' print(diagnostic_sample_size_examples[, c("scenario", "prevalence",
#'                                            "final_n", "final_n_adjusted")])
#'
#' # Scenario 1: Colorectal cancer screening
#' colorectal <- diagnostic_sample_size_examples[1, ]
#' cat("Scenario:", colorectal$scenario, "\n")
#' cat("Prevalence:", colorectal$prevalence * 100, "%\n")
#' cat("Required N:", colorectal$final_n, "\n")
#' cat("Adjusted N (20% non-response):", colorectal$final_n_adjusted, "\n")
#'
#' # Demonstrate impact of prevalence on sample size
#' prevalence_impact <- diagnostic_sample_size_examples[, c("scenario", "prevalence", "final_n")]
#' prevalence_impact <- prevalence_impact[order(prevalence_impact$prevalence), ]
#' print(prevalence_impact)
#'
#' @keywords datasets
"diagnostic_sample_size_examples"


#' Bujang 2023 Table 2 Validation Data
#'
#' Validation dataset containing excerpts from Bujang MA (2023) Table 2 for
#' diagnostic test sample size calculations. Used to validate Clopper-Pearson
#' exact binomial confidence interval implementation.
#'
#' @format A data frame with 8 rows and 7 variables:
#' \describe{
#'   \item{prevalence}{Disease prevalence (0.05 to 0.90)}
#'   \item{sensitivity}{Target sensitivity value}
#'   \item{specificity}{Target specificity value}
#'   \item{ci_width}{Desired 95\% CI width (0.10 or 0.20)}
#'   \item{n_sens_expected}{Expected sample size for sensitivity (from Bujang Table 2)}
#'   \item{n_spec_expected}{Expected sample size for specificity (from Bujang Table 2)}
#'   \item{n_total_expected}{Expected total sample size (maximum of sens/spec)}
#' }
#'
#' @details
#' This dataset provides benchmark values for validating the Clopper-Pearson
#' sample size calculation implementation. Each row represents a specific
#' combination of prevalence, target sensitivity/specificity, and desired
#' confidence interval width, with the expected sample sizes as published
#' in Bujang (2023).
#'
#' \strong{Validation Test Cases:}
#' \enumerate{
#'   \item Prevalence 5\%, Sens/Spec 95\%, CI width 0.10 → N = 940
#'   \item Prevalence 5\%, Sens/Spec 70\%, CI width 0.10 → N = 3,410
#'   \item Prevalence 10\%, Sens/Spec 95\%, CI width 0.10 → N = 940
#'   \item Prevalence 10\%, Sens/Spec 90\%, CI width 0.20 → N = 440
#'   \item Prevalence 20\%, Sens/Spec 90\%, CI width 0.20 → N = 220
#'   \item Prevalence 50\%, Sens/Spec 90\%, CI width 0.20 → N = 88
#'   \item Prevalence 50\%, Sens/Spec 80\%, CI width 0.20 → N = 140
#'   \item Prevalence 90\%, Sens/Spec 95\%, CI width 0.20 → N = 941
#' }
#'
#' @section Usage:
#' These values can be used to programmatically validate that the
#' \code{diagnosticsamplesize} module produces results consistent with
#' Bujang's published tables.
#'
#' @source
#' Bujang MA (2023). An Elaboration on Sample Size Planning for Performing a
#' One-Sample Sensitivity and Specificity Analysis by Basing on Calculations on
#' a Specified 95\% Confidence Interval Width. Diagnostics 13(8):1390.
#' Table 2 (pages 5-6).
#' \doi{10.3390/diagnostics13081390}
#'
#' @examples
#' # Load validation data
#' data(bujang_table2_validation)
#'
#' # View all test cases
#' print(bujang_table2_validation)
#'
#' # Test case: Low prevalence (5%), high targets (95%), narrow CI (0.10)
#' test1 <- bujang_table2_validation[1, ]
#' cat("Prevalence:", test1$prevalence * 100, "%\n")
#' cat("Targets: Sens =", test1$sensitivity, ", Spec =", test1$specificity, "\n")
#' cat("Expected total N:", test1$n_total_expected, "\n")
#'
#' # Compare low vs. high prevalence scenarios
#' low_prev <- bujang_table2_validation[bujang_table2_validation$prevalence == 0.05, ]
#' high_prev <- bujang_table2_validation[bujang_table2_validation$prevalence == 0.90, ]
#'
#' cat("\nLow prevalence (5%) scenarios:\n")
#' print(low_prev[, c("sensitivity", "specificity", "ci_width", "n_total_expected")])
#'
#' cat("\nHigh prevalence (90%) scenarios:\n")
#' print(high_prev[, c("sensitivity", "specificity", "ci_width", "n_total_expected")])
#'
#' @keywords datasets
"bujang_table2_validation"
