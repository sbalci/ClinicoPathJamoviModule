#' @title Lowest Expected Value for a fixed sample size
#' @importFrom R6 R6Class
#' @import jmvcore

kappaSizeFixedNClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "kappaSizeFixedNClass",
    inherit = kappaSizeFixedNBase,
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
            props_parsed <- validation_result$props
            raters <- as.numeric(self$options$raters)
            alpha <- self$options$alpha
            n <- self$options$n
            
            # Perform calculation with error handling
            tryCatch({
                result <- private$.calculateKappaFixedN(
                    outcome = outcome,
                    kappa0 = kappa0,
                    props = props_parsed,
                    raters = raters,
                    alpha = alpha,
                    n = n
                )
                
                # Generate explanatory text
                explanation <- private$.generateExplanation(
                    outcome = outcome,
                    kappa0 = kappa0,
                    props = props_parsed,
                    raters = raters,
                    alpha = alpha,
                    n = n
                )
                
                # Format formatting the result to string
                if (is.list(result)) {
                    if ("Lowest expected value of kappa" %in% names(result)) {
                        formatted_result <- paste0("Lowest expected value of kappa: ", round(as.numeric(result[[1]]), 3))
                    } else {
                        formatted_result <- paste0("Lowest expected value of kappa: ", round(as.numeric(result[[1]]), 3))
                    }
                } else {
                    formatted_result <- paste0("Lowest expected value of kappa: ", round(as.numeric(result), 3))
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
            props_str <- self$options$props
            raters <- self$options$raters
            alpha <- self$options$alpha
            n <- self$options$n
            
            # Validate outcome
            if (is.null(switch(as.character(outcome), "2"=TRUE, "3"=TRUE, "4"=TRUE, "5"=TRUE, NULL))) {
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
                    explanation = "Kappa represents agreement beyond chance, with values from 0 (no agreement) to 1 (perfect agreement)."
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
            
            # Validate sample size
            if (is.null(n) || is.na(n) || n <= 0 || n != floor(n)) {
                return(list(
                    valid = FALSE,
                    message = "Sample size (n) must be a positive integer.",
                    explanation = "Sample size represents the number of subjects/observations in your study."
                ))
            }
            
            if (n < 10) {
                return(list(
                    valid = FALSE,
                    message = "Sample size (n) should be at least 10 for reliable results.",
                    explanation = "Very small sample sizes may not provide reliable kappa estimates."
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
            props_clean <- gsub("[,;|\\t]+", ",", props_str)
            props_split <- strsplit(props_clean, ",")[[1]]
            props_split <- trimws(props_split)
            props_split <- props_split[nchar(props_split) > 0]  # Remove empty strings
            
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
        .calculateKappaFixedN = function(outcome, kappa0, props, raters, alpha, n) {
            switch(as.character(outcome),
                "2" = kappaSize::FixedNBinary(
                    kappa0 = kappa0,
                    n = n,
                    props = props,
                    alpha = alpha,
                    raters = raters
                ),
                "3" = kappaSize::FixedN3Cats(
                    kappa0 = kappa0,
                    n = n,
                    props = props,
                    alpha = alpha,
                    raters = raters
                ),
                "4" = kappaSize::FixedN4Cats(
                    kappa0 = kappa0,
                    n = n,
                    props = props,
                    alpha = alpha,
                    raters = raters
                ),
                "5" = kappaSize::FixedN5Cats(
                    kappa0 = kappa0,
                    n = n,
                    props = props,
                    alpha = alpha,
                    raters = raters
                ),
                stop("Unsupported outcome category count: ", outcome)
            )
        },
        
        # Generate comprehensive explanation text
        .generateExplanation = function(outcome, kappa0, props, raters, alpha, n) {
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
            
            explanation <- paste0(
                "STUDY DESIGN ANALYSIS\n",
                "====================\n\n",
                "Research Question:\n",
                "Researchers want to determine the expected lower bound for kappa agreement\n",
                "given a fixed sample size of ", n, " subjects assessed by ", raters, " raters.\n\n",
                "Study Parameters:\n",
                "• Expected kappa value (κ₀): ", kappa0, "\n",
                "• Sample size: ", n, " subjects\n",
                "• Number of raters: ", raters, "\n",
                "• Outcome categories: ", outcome, "\n",
                "• Expected proportions: ", props_formatted, "\n",
                "• Significance level: ", alpha, " (", confidence_level, "% confidence)\n\n",
                "Clinical Context:\n",
                context_examples, "\n\n",
                "Interpretation:\n",
                "The analysis calculates the lowest expected value (lower confidence bound) for kappa\n",
                "that you can reliably detect with your available sample size. This helps determine\n",
                "whether your study has adequate power to detect meaningful agreement levels.\n\n",
                "Note: Values closer to 1.0 indicate better agreement, while values closer to 0\n",
                "indicate agreement no better than chance."
            )
            
            return(explanation)
        }
    )
)