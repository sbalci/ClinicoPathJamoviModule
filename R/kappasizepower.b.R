#' @title Power Analysis for Inter-rater Agreement Studies
#' @description
#' Performs power analysis to determine the required sample size for detecting 
#' a specified improvement in inter-rater agreement (kappa coefficient). This function
#' helps researchers design adequately powered studies to validate training programs,
#' standardized protocols, or other interventions aimed at improving agreement between raters.
#' 
#' @details
#' The function calculates the sample size needed to detect a difference between
#' two kappa values (kappa0 vs kappa1) with specified power and significance level.
#' It supports 2-5 outcome categories and 2-5 raters, using the kappaSize package
#' implementation of power calculations for different scenarios.
#' 
#' Key requirements:
#' - kappa1 must be greater than kappa0 (alternative hypothesis should represent improvement)
#' - Proportions must sum to 1.0 and match the number of outcome categories
#' - Power should be at least 0.50, typically 0.80 or higher
#' 
#' @examples
#' \dontrun{
#' # Basic binary outcome power analysis
#' result <- kappaSizePower(
#'   outcome = "2",
#'   kappa0 = 0.40,           # Current agreement level
#'   kappa1 = 0.60,           # Target agreement level
#'   props = "0.30, 0.70",    # Expected proportions
#'   raters = "2",            # Number of raters
#'   alpha = 0.05,            # Significance level
#'   power = 0.80             # Desired power
#' )
#' 
#' # Medical diagnosis validation study
#' result <- kappaSizePower(
#'   outcome = "2",
#'   kappa0 = 0.50,           # Baseline fair agreement
#'   kappa1 = 0.75,           # Target good agreement
#'   props = "0.25, 0.75",    # 25% disease prevalence
#'   raters = "2",
#'   alpha = 0.05,
#'   power = 0.80
#' )
#' 
#' # Multi-category severity assessment
#' result <- kappaSizePower(
#'   outcome = "3",
#'   kappa0 = 0.55,           # Current moderate agreement
#'   kappa1 = 0.75,           # Target good agreement
#'   props = "0.20, 0.50, 0.30", # Mild, moderate, severe
#'   raters = "3",
#'   alpha = 0.05,
#'   power = 0.85
#' )
#' }
#' 
#' @importFrom R6 R6Class
#' @import jmvcore

kappaSizePowerClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "kappaSizePowerClass",
    inherit = kappaSizePowerBase,
    private = list(
        
        # Initialize results and validate dependencies
        .init = function() {
            # Check if kappaSize package is available
            if (!requireNamespace("kappaSize", quietly = TRUE)) {
                self$results$text1$setContent(
                    "Error: The 'kappaSize' package is required but not installed.\n\nPlease install it using: install.packages('kappaSize')"
                )
                self$results$text2$setContent(
                    "The kappaSize package provides the statistical functions needed for kappa power analysis."
                )
                return()
            }
        },
        
        .run = function() {
            # Early return if dependencies not available
            if (!requireNamespace("kappaSize", quietly = TRUE)) {
                return()
            }
            
            # Extract and validate parameters
            validation_result <- private$.validateParameters()
            if (!validation_result$valid) {
                error_msg <- paste("Input validation failed:", validation_result$message)
                jmvcore::reject(error_msg, code='validation_failed')
            }
            
            # Get validated parameters
            outcome <- as.numeric(self$options$outcome)
            kappa0 <- self$options$kappa0
            kappa1 <- self$options$kappa1
            props_parsed <- validation_result$props
            raters <- as.numeric(self$options$raters)
            alpha <- self$options$alpha
            power <- self$options$power
            
            # Perform calculation with error handling
            tryCatch({
                result <- private$.calculateSampleSize(
                    outcome = outcome,
                    kappa0 = kappa0,
                    kappa1 = kappa1,
                    props = props_parsed,
                    raters = raters,
                    alpha = alpha,
                    power = power
                )
                
                # Generate explanatory text
                explanation <- private$.generateExplanation(
                    outcome = outcome,
                    kappa0 = kappa0,
                    kappa1 = kappa1,
                    props = props_parsed,
                    raters = raters,
                    alpha = alpha,
                    power = power
                )
                
                # Format the result to string
                if (is.list(result)) {
                    if ("Total sample size" %in% names(result)) {
                        formatted_result <- paste0("Required sample size: ", result$`Total sample size`)
                    } else if ("n" %in% names(result)) {
                        formatted_result <- paste0("Required sample size: ", result$n)
                    } else {
                        formatted_result <- paste0("Required sample size: ", result[[1]])
                    }
                } else {
                    formatted_result <- paste0("Required sample size: ", result)
                }
                
                # Set results
                self$results$text1$setContent(formatted_result)
                self$results$text2$setContent(explanation)
                
            }, error = function(e) {
                error_msg <- paste("Calculation failed:", e$message)
                self$results$text1$setContent(error_msg)
                self$results$text2$setContent(
                    "Please check your input parameters and ensure they are within valid ranges."
                )
            })
        },
        
        # Comprehensive parameter validation
        .validateParameters = function() {
            outcome <- self$options$outcome
            kappa0 <- self$options$kappa0
            kappa1 <- self$options$kappa1
            props_str <- self$options$props
            raters <- self$options$raters
            alpha <- self$options$alpha
            power <- self$options$power
            
            # Validate outcome
            if (!(outcome %in% c("2", "3", "4", "5"))) {
                return(list(
                    valid = FALSE,
                    message = "Outcome must be 2, 3, 4, or 5 categories.",
                    explanation = "The number of outcome categories determines which statistical model is used for the analysis."
                ))
            }
            
            outcome_num <- as.numeric(outcome)
            
            # Validate kappa0
            if (is.null(kappa0) || is.na(kappa0) || kappa0 <= 0 || kappa0 >= 1) {
                return(list(
                    valid = FALSE,
                    message = "kappa0 must be between 0 and 1 (exclusive).",
                    explanation = "Kappa0 represents the null hypothesis value for agreement beyond chance."
                ))
            }
            
            # Validate kappa1
            if (is.null(kappa1) || is.na(kappa1) || kappa1 <= 0 || kappa1 >= 1) {
                return(list(
                    valid = FALSE,
                    message = "kappa1 must be between 0 and 1 (exclusive).",
                    explanation = "Kappa1 represents the alternative hypothesis value for agreement beyond chance."
                ))
            }
            
            # Validate kappa0 vs kappa1 relationship
            if (kappa1 <= kappa0) {
                return(list(
                    valid = FALSE,
                    message = "kappa1 must be greater than kappa0.",
                    explanation = "For power analysis, the alternative hypothesis (kappa1) should represent a better agreement level than the null hypothesis (kappa0)."
                ))
            }
            
            # Validate and parse proportions
            props_result <- private$.parseProportions(props_str, outcome_num)
            if (!props_result$valid) {
                return(props_result)
            }
            
            # Validate raters
            if (!(raters %in% c("2", "3", "4", "5"))) {
                return(list(
                    valid = FALSE,
                    message = "Number of raters must be 2, 3, 4, or 5.",
                    explanation = "The analysis supports 2 to 5 raters for inter-rater reliability assessment."
                ))
            }
            
            # Validate alpha
            if (is.null(alpha) || is.na(alpha) || alpha <= 0 || alpha >= 1) {
                return(list(
                    valid = FALSE,
                    message = "Alpha must be between 0 and 1 (exclusive).",
                    explanation = "Alpha represents the significance level, typically 0.05 (5%) or 0.01 (1%)."
                ))
            }
            
            # Validate power
            if (is.null(power) || is.na(power) || power <= 0 || power >= 1) {
                return(list(
                    valid = FALSE,
                    message = "Power must be between 0 and 1 (exclusive).",
                    explanation = "Power represents the probability of detecting a true effect, typically 0.80 (80%) or higher."
                ))
            }
            
            # Additional power validation
            if (power < 0.5) {
                return(list(
                    valid = FALSE,
                    message = "Power should be at least 0.5 (50%) for meaningful analysis.",
                    explanation = "Power values below 50% indicate very low ability to detect true effects."
                ))
            }
            
            return(list(
                valid = TRUE,
                props = props_result$props
            ))
        },
        
        # Enhanced proportion parsing with multiple delimiter support
        .parseProportions = function(props_str, expected_count) {
            if (is.null(props_str) || is.na(props_str) || nchar(trimws(props_str)) == 0) {
                return(list(
                    valid = FALSE,
                    message = "Proportions cannot be empty.",
                    explanation = "Please provide proportions for each outcome category, separated by commas."
                ))
            }
            
            # Clean and split proportions - handle multiple delimiters
            props_clean <- gsub("[,;|\\\\t]+", ",", props_str)
            props_split <- strsplit(props_clean, ",")[[1]]
            props_split <- trimws(props_split)
            props_split <- props_split[nchar(props_split) > 0]  # Remove empty strings
            
            # Handle space-separated format
            if (length(props_split) == 1 && grepl("\\s+", props_str)) {
                props_split <- trimws(strsplit(props_str, "\\s+")[[1]])
                props_split <- props_split[nchar(props_split) > 0]
            }
            
            # Convert to numeric
            props_numeric <- suppressWarnings(as.numeric(props_split))
            
            # Check for conversion errors
            if (any(is.na(props_numeric))) {
                return(list(
                    valid = FALSE,
                    message = "All proportions must be valid numbers.",
                    explanation = "Please ensure all proportions are numeric values (e.g., 0.25, 0.75)."
                ))
            }
            
            # Check count matches expected
            if (length(props_numeric) != expected_count) {
                return(list(
                    valid = FALSE,
                    message = paste0("Expected ", expected_count, " proportions but found ", length(props_numeric), "."),
                    explanation = paste0("For ", expected_count, " outcome categories, please provide exactly ", expected_count, " proportions.")
                ))
            }
            
            # Check all proportions are positive
            if (any(props_numeric <= 0)) {
                return(list(
                    valid = FALSE,
                    message = "All proportions must be positive (> 0).",
                    explanation = "Proportions represent the expected frequency of each category and must be greater than 0."
                ))
            }
            
            # Check all proportions are less than 1
            if (any(props_numeric >= 1)) {
                return(list(
                    valid = FALSE,
                    message = "All proportions must be less than 1.",
                    explanation = "Proportions represent frequencies and must be between 0 and 1."
                ))
            }
            
            # Check sum approximately equals 1
            prop_sum <- sum(props_numeric)
            if (abs(prop_sum - 1) > 0.01) {
                return(list(
                    valid = FALSE,
                    message = paste0("Proportions must sum to 1 (currently sum to ", round(prop_sum, 3), ")."),
                    explanation = "The proportions should represent the expected distribution of all categories and must sum to 1.0."
                ))
            }
            
            # Normalize to ensure exact sum of 1
            props_normalized <- props_numeric / prop_sum
            
            return(list(
                valid = TRUE,
                props = props_normalized
            ))
        },
        
        # Perform the actual calculation
        .calculateSampleSize = function(outcome, kappa0, kappa1, props, raters, alpha, power) {
            switch(as.character(outcome),
                "2" = kappaSize::PowerBinary(
                    kappa0 = kappa0,
                    kappa1 = kappa1,
                    props = props,
                    raters = raters,
                    alpha = alpha,
                    power = power
                ),
                "3" = kappaSize::Power3Cats(
                    kappa0 = kappa0,
                    kappa1 = kappa1,
                    props = props,
                    raters = raters,
                    alpha = alpha,
                    power = power
                ),
                "4" = kappaSize::Power4Cats(
                    kappa0 = kappa0,
                    kappa1 = kappa1,
                    props = props,
                    raters = raters,
                    alpha = alpha,
                    power = power
                ),
                "5" = kappaSize::Power5Cats(
                    kappa0 = kappa0,
                    kappa1 = kappa1,
                    props = props,
                    raters = raters,
                    alpha = alpha,
                    power = power
                ),
                stop("Unsupported outcome category count: ", outcome)
            )
        },
        
        # Generate comprehensive explanation text
        .generateExplanation = function(outcome, kappa0, kappa1, props, raters, alpha, power) {
            # Format proportions for display
            props_formatted <- paste0(sprintf("%.1f%%", props * 100), collapse = ", ")
            
            # Generate clinical context based on outcome categories
            context_examples <- switch(as.character(outcome),
                "2" = "This is commonly used for binary assessments such as:\n• Disease present/absent\n• Positive/negative test results\n• Malignant/benign classifications",
                "3" = "This is commonly used for three-category assessments such as:\n• Mild/Moderate/Severe classifications\n• Low/Medium/High risk categories\n• Well/Moderately/Poorly differentiated grades",
                "4" = "This is commonly used for four-category assessments such as:\n• Tumor grading systems (Grade 1-4)\n• Performance status scales\n• Quality ratings (Excellent/Good/Fair/Poor)",
                "5" = "This is commonly used for five-category assessments such as:\n• Likert scales (Strongly Disagree to Strongly Agree)\n• Pain intensity scales (None/Mild/Moderate/Severe/Extreme)\n• Complex grading systems with 5 levels"
            )
            
            # Calculate confidence level for display
            confidence_level <- (1 - alpha) * 100
            power_percent <- power * 100
            
            explanation <- paste0(
                "POWER ANALYSIS FOR SAMPLE SIZE DETERMINATION\n",
                "=============================================\n\n",
                "Research Objective:\n",
                "Determine the required sample size to test whether inter-rater agreement\n",
                "significantly differs from the null hypothesis level.\n\n",
                "Hypothesis Testing:\n",
                "• H₀: κ = ", kappa0, " (null hypothesis agreement level)\n",
                "• H₁: κ = ", kappa1, " (alternative hypothesis agreement level)\n\n",
                "Study Design Parameters:\n",
                "• Number of raters: ", raters, "\n",
                "• Outcome categories: ", outcome, "\n",
                "• Expected proportions: ", props_formatted, "\n",
                "• Significance level (α): ", alpha, " (", confidence_level, "% confidence)\n",
                "• Desired power: ", power, " (", power_percent, "% chance of detecting true effect)\n\n",
                "Clinical Context:\n",
                context_examples, "\n\n",
                "Interpretation:\n",
                "The calculated sample size ensures ", power_percent, "% power to detect a true difference\n",
                "between κ₀=", kappa0, " and κ₁=", kappa1, " at the ", alpha, " significance level.\n",
                "This means you have a ", power_percent, "% chance of correctly identifying improved\n",
                "agreement if it truly exists at the κ₁ level.\n\n",
                "Effect Size Considerations:\n",
                "• Difference (κ₁ - κ₀): ", round(kappa1 - kappa0, 3), "\n",
                "• This represents a ", 
                if (kappa1 - kappa0 < 0.2) "small" else if (kappa1 - kappa0 < 0.4) "moderate" else "large",
                " effect size for inter-rater agreement."
            )
            
            return(explanation)
        }
    )
)