#' Clinical Screening Test Examples
#'
#' @description
#' Real-world clinical scenarios demonstrating how test characteristics 
#' (sensitivity, specificity) and disease prevalence affect screening
#' test performance in different medical contexts.
#'
#' @format A data frame with 15 rows and 12 variables:
#' \describe{
#'   \item{scenario}{Character. The clinical scenario name}
#'   \item{test_type}{Character. Type of diagnostic test (Antigen, Imaging, etc.)}
#'   \item{sensitivity}{Numeric. Test sensitivity (true positive rate)}
#'   \item{specificity}{Numeric. Test specificity (true negative rate)}  
#'   \item{setting}{Character. Clinical setting or population}
#'   \item{prevalence}{Numeric. Disease prevalence in the population}
#'   \item{clinical_context}{Character. Detailed clinical context}
#'   \item{ppv}{Numeric. Positive predictive value}
#'   \item{npv}{Numeric. Negative predictive value}
#'   \item{positive_lr}{Numeric. Positive likelihood ratio}
#'   \item{negative_lr}{Numeric. Negative likelihood ratio}
#'   \item{interpretation}{Character. Clinical interpretation of PPV}
#'   \item{clinical_action}{Character. Recommended clinical action}
#' }
#'
#' @details
#' This dataset contains realistic clinical scenarios across different 
#' medical specialties:
#' 
#' \strong{COVID-19 Testing:}
#' \itemize{
#'   \item Community screening (low prevalence)
#'   \item Outbreak testing (high prevalence)
#'   \item Symptomatic patient testing
#' }
#' 
#' \strong{Cancer Screening:}
#' \itemize{
#'   \item Mammography across age groups
#'   \item PSA screening scenarios
#' }
#' 
#' \strong{Cardiovascular Disease:}
#' \itemize{
#'   \item Stress testing in different risk populations
#' }
#' 
#' \strong{Infectious Disease:}
#' \itemize{
#'   \item HIV testing and confirmatory scenarios
#' }
#'
#' @examples
#' # Load the dataset
#' data(screening_examples)
#' 
#' # View COVID-19 testing scenarios
#' covid_tests <- screening_examples[screening_examples$scenario == "COVID-19 Rapid Test", ]
#' print(covid_tests[, c("setting", "prevalence", "ppv", "npv")])
#' 
#' # Compare PPV across different prevalence settings
#' library(ggplot2)
#' ggplot(screening_examples, aes(x = prevalence, y = ppv, color = scenario)) +
#'   geom_point(size = 3) +
#'   geom_line(aes(group = scenario)) +
#'   scale_x_log10() +
#'   labs(title = "Effect of Prevalence on Positive Predictive Value",
#'        x = "Disease Prevalence (log scale)", 
#'        y = "Positive Predictive Value") +
#'   theme_minimal()
#' 
#' # Find scenarios with low PPV requiring confirmatory testing
#' low_ppv <- screening_examples[screening_examples$ppv < 0.10, ]
#' print(low_ppv[, c("scenario", "setting", "ppv", "clinical_action")])
#'
#' @seealso 
#' \code{\link{prevalence_demo}} for prevalence effect demonstrations
#' \code{\link{performance_demo}} for test performance comparisons  
#' \code{\link{sequential_demo}} for sequential testing examples
#' \code{\link{common_tests}} for reference test characteristics
#'
#' @source
#' Test characteristics compiled from published literature and clinical guidelines:
#' \itemize{
#'   \item COVID-19 rapid tests: Cochrane reviews and FDA data
#'   \item Mammography: USPSTF recommendations and meta-analyses  
#'   \item Cardiac stress testing: ACC/AHA guidelines
#'   \item PSA screening: Urology association guidelines
#'   \item HIV testing: CDC recommendations
#' }
#'
#' @keywords datasets medical screening diagnostic
# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

"screening_examples"


#' Prevalence Effect Demonstration
#'
#' @description
#' Demonstrates how disease prevalence affects positive and negative 
#' predictive values while keeping test sensitivity and specificity constant.
#' Educational dataset for understanding Bayes' theorem in clinical practice.
#'
#' @format A data frame with 6 rows and 6 variables:
#' \describe{
#'   \item{sensitivity}{Numeric. Constant sensitivity (0.90)}
#'   \item{specificity}{Numeric. Constant specificity (0.90)}  
#'   \item{prevalence}{Numeric. Varying disease prevalence (0.001 to 0.50)}
#'   \item{scenario}{Character. "Prevalence Effect Demo"}
#'   \item{setting}{Character. Description of prevalence level}
#'   \item{ppv}{Numeric. Calculated positive predictive value}
#'   \item{npv}{Numeric. Calculated negative predictive value}
#' }
#'
#' @details
#' This dataset demonstrates the critical concept that identical test 
#' performance (sensitivity and specificity) can yield dramatically 
#' different predictive values depending on disease prevalence.
#' 
#' Key teaching points:
#' \itemize{
#'   \item Low prevalence → Low PPV (many false positives)
#'   \item High prevalence → High PPV (fewer false positives)
#'   \item NPV remains high across most prevalence ranges
#'   \item Critical for understanding screening vs. diagnostic testing
#' }
#'
#' @examples
#' # Load and visualize prevalence effect
#' data(prevalence_demo)
#' 
#' # Create prevalence effect plot
#' library(ggplot2)
#' library(tidyr)
#' 
#' # Reshape for plotting
#' prev_long <- prevalence_demo %>%
#'   select(prevalence, ppv, npv) %>%
#'   pivot_longer(cols = c(ppv, npv), names_to = "metric", values_to = "value")
#' 
#' ggplot(prev_long, aes(x = prevalence, y = value, color = metric)) +
#'   geom_line(size = 1.2) +
#'   geom_point(size = 3) +
#'   scale_x_log10() +
#'   scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
#'   labs(title = "How Prevalence Affects Predictive Values",
#'        subtitle = "Sensitivity = 90%, Specificity = 90%",
#'        x = "Disease Prevalence (log scale)",
#'        y = "Predictive Value",
#'        color = "Metric") +
#'   theme_minimal()
#'
#' @seealso \code{\link{screening_examples}}, \code{\link{performance_demo}}
#'
#' @keywords datasets education bayes-theorem prevalence
"prevalence_demo"


#' Test Performance Comparison
#'
#' @description
#' Demonstrates how different combinations of sensitivity and specificity 
#' affect predictive values at a fixed prevalence. Educational dataset 
#' for understanding test performance trade-offs.
#'
#' @format A data frame with 16 rows and 6 variables:
#' \describe{
#'   \item{sensitivity}{Numeric. Varying sensitivity (0.70 to 0.95)}
#'   \item{specificity}{Numeric. Varying specificity (0.70 to 0.95)}
#'   \item{prevalence}{Numeric. Fixed prevalence (0.10)}
#'   \item{scenario}{Character. "Test Performance Demo"}
#'   \item{setting}{Character. Description of sens/spec combination}
#'   \item{ppv}{Numeric. Calculated positive predictive value}
#'   \item{npv}{Numeric. Calculated negative predictive value}
#' }
#'
#' @details
#' This dataset explores all combinations of sensitivity and specificity
#' values (70%, 80%, 90%, 95%) at a fixed 10% prevalence to demonstrate:
#' 
#' \itemize{
#'   \item How sensitivity affects NPV
#'   \item How specificity affects PPV  
#'   \item Trade-offs in test optimization
#'   \item Importance of both metrics
#' }
#'
#' @examples
#' # Load and analyze test performance trade-offs
#' data(performance_demo)
#' 
#' # Create performance heatmap
#' library(ggplot2)
#' 
#' ggplot(performance_demo, aes(x = factor(specificity), y = factor(sensitivity), fill = ppv)) +
#'   geom_tile() +
#'   geom_text(aes(label = round(ppv, 2)), color = "white", size = 4) +
#'   scale_fill_gradient(low = "red", high = "green", name = "PPV") +
#'   labs(title = "Positive Predictive Value Heatmap",
#'        subtitle = "Prevalence = 10%",
#'        x = "Specificity", 
#'        y = "Sensitivity") +
#'   theme_minimal()
#'
#' @seealso \code{\link{screening_examples}}, \code{\link{prevalence_demo}}
#'
#' @keywords datasets education test-performance sensitivity specificity
"performance_demo"


#' Sequential Testing Demonstration
#'
#' @description
#' Demonstrates how sequential testing affects disease probability through
#' Bayesian updating. Shows probability evolution for different test 
#' sequence patterns.
#'
#' @format A data frame with 5 rows and 6 variables:
#' \describe{
#'   \item{scenario}{Character. "Sequential Testing Demo"}
#'   \item{test_sequence}{Character. Description of test sequence}
#'   \item{sensitivity}{Numeric. Test sensitivity (0.85)}
#'   \item{specificity}{Numeric. Test specificity (0.90)}
#'   \item{initial_prevalence}{Numeric. Starting prevalence (0.10)}
#'   \item{final_probability}{Numeric. Final disease probability}
#' }
#'
#' @details
#' Demonstrates five sequential testing scenarios:
#' \itemize{
#'   \item Single test (baseline)
#'   \item Two positive tests (highest probability)  
#'   \item Two negative tests (lowest probability)
#'   \item Positive then negative (intermediate)
#'   \item Negative then positive (intermediate)
#' }
#'
#' @examples
#' # Visualize sequential testing effects
#' data(sequential_demo)
#' 
#' library(ggplot2)
#' 
#' ggplot(sequential_demo, aes(x = test_sequence, y = final_probability)) +
#'   geom_col(fill = "steelblue", alpha = 0.7) +
#'   geom_text(aes(label = paste0(round(final_probability * 100, 1), "%")), 
#'             vjust = -0.5) +
#'   scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
#'   labs(title = "Disease Probability After Sequential Testing",
#'        subtitle = "Sensitivity = 85%, Specificity = 90%, Initial Prevalence = 10%",
#'        x = "Test Sequence",
#'        y = "Final Disease Probability") +
#'   theme_minimal() +
#'   theme(axis.text.x = element_text(angle = 45, hjust = 1))
#'
#' @seealso \code{\link{screening_examples}}
#'
#' @keywords datasets education sequential-testing bayes
"sequential_demo"


#' Common Test Characteristics Reference
#'
#' @description
#' Reference table of typical sensitivity and specificity ranges for 
#' commonly used clinical tests. Useful for educational purposes and 
#' realistic parameter selection.
#'
#' @format A data frame with 9 rows and 5 variables:
#' \describe{
#'   \item{test_name}{Character. Name of diagnostic test}
#'   \item{sensitivity_range}{Character. Typical sensitivity range}
#'   \item{specificity_range}{Character. Typical specificity range}
#'   \item{typical_prevalence}{Character. Typical disease prevalence}
#'   \item{clinical_use}{Character. Primary clinical application}
#' }
#'
#' @details
#' Includes reference data for:
#' \itemize{
#'   \item COVID-19 testing (Rapid antigen, RT-PCR)
#'   \item Cancer screening (Mammography, PSA, Pap smear, FOBT)
#'   \item Cardiovascular testing (Exercise stress test)
#'   \item Infectious disease (HIV ELISA, Tuberculin skin test)
#' }
#'
#' @examples
#' # View reference test characteristics
#' data(common_tests)
#' print(common_tests)
#' 
#' # Find tests with high sensitivity
#' high_sens <- common_tests[grepl("0.9", common_tests$sensitivity_range), ]
#' print(high_sens[, c("test_name", "sensitivity_range", "clinical_use")])
#'
#' @source
#' Compiled from medical literature, clinical guidelines, and meta-analyses
#'
#' @keywords datasets reference medical-tests
"common_tests"
