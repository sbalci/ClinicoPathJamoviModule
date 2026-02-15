#' @title Confidence Interval Approach for the Number of Subjects Required
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom glue glue
#'
#' @description Calculate sample size for interobserver agreement studies using Cohen's kappa statistic.
#' This function provides confidence interval-based sample size determination for studies
#' evaluating agreement between raters across different numbers of outcome categories.
#'
#' @details The function uses the kappaSize package to calculate required sample sizes
#' for kappa coefficient confidence intervals. It supports 2-5 outcome categories
#' and 2-6 raters, with customizable precision requirements and significance levels.
#' Both two-sided and one-sided confidence intervals are supported.

kappaSizeCIClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "kappaSizeCIClass",
    inherit = kappaSizeCIBase,
    private = list(
        # Performance optimization: cache variables
        .prepared_params = NULL,
        .params_hash = NULL,
        .cached_result = NULL,
        .cached_summary = NULL,
        .cached_explanation = NULL,

        # Input validation methods
        .validateInputs = function() {
            errors <- c()

            is_one_sided <- (self$options$citype == "one_sided")

            # Validate outcome levels
            if (!self$options$outcome %in% c("2", "3", "4", "5")) {
                errors <- c(errors, "Outcome must be 2, 3, 4, or 5 categories")
            }

            # Validate kappa parameters
            if (self$options$kappa0 <= 0 || self$options$kappa0 >= 1) {
                errors <- c(errors, "kappa0 must be between 0 and 1")
            }

            if (self$options$kappaL <= 0 || self$options$kappaL >= 1) {
                errors <- c(errors, "kappaL must be between 0 and 1")
            }

            if (!is_one_sided) {
                if (self$options$kappaU <= 0 || self$options$kappaU >= 1) {
                    errors <- c(errors, "kappaU must be between 0 and 1")
                }

                if (self$options$kappaL >= self$options$kappaU) {
                    errors <- c(errors, "kappaL must be less than kappaU")
                }

                if (self$options$kappa0 < self$options$kappaL || self$options$kappa0 > self$options$kappaU) {
                    errors <- c(errors, "kappa0 should be within the confidence interval [kappaL, kappaU]")
                }
            } else {
                if (self$options$kappa0 < self$options$kappaL) {
                    errors <- c(errors, "kappa0 should be greater than or equal to kappaL")
                }
            }

            # Validate alpha
            if (self$options$alpha <= 0 || self$options$alpha >= 1) {
                errors <- c(errors, "alpha must be between 0 and 1")
            }

            # Validate raters
            if (!self$options$raters %in% c("2", "3", "4", "5", "6")) {
                errors <- c(errors, "Number of raters must be 2, 3, 4, 5, or 6")
            }

            # Validate proportions
            props_validation <- private$.validateProportions()
            if (!is.null(props_validation$error)) {
                errors <- c(errors, props_validation$error)
            }

            return(if (length(errors) > 0) errors else NULL)
        },

        .validateProportions = function() {
            tryCatch({
                props_str <- trimws(self$options$props)
                if (props_str == "") {
                    return(list(error = "Proportions cannot be empty"))
                }

                # Parse proportions with flexible delimiters
                props_clean <- gsub("[,;|\\t]+", ",", props_str)
                props_split <- strsplit(props_clean, ",")[[1]]
                props_numeric <- as.numeric(trimws(props_split))

                # Handle space-separated format
                if (length(props_numeric) == 1 && grepl("\\s+", props_str)) {
                    props_split <- trimws(strsplit(props_str, "\\s+")[[1]])
                    props_numeric <- as.numeric(props_split)
                }

                if (any(is.na(props_numeric))) {
                    return(list(error = "All proportions must be valid numbers"))
                }

                if (any(props_numeric <= 0) || any(props_numeric >= 1)) {
                    return(list(error = "All proportions must be between 0 and 1"))
                }

                expected_length <- as.numeric(self$options$outcome)
                # Binary models accept one prevalence value or two values summing to one
                if (expected_length == 2 && length(props_numeric) == 1) {
                    props_numeric <- c(props_numeric, 1 - props_numeric)
                }

                if (length(props_numeric) != expected_length) {
                    error_msg <- paste0("Expected ", expected_length, " proportions for ", expected_length, " outcome categories, got ", length(props_numeric))
                    return(list(error = error_msg))
                }

                prop_sum <- sum(props_numeric)
                if (abs(prop_sum - 1) > 0.01) {
                    error_msg <- paste0("Proportions should sum to 1.0, current sum is ", round(prop_sum, 3))
                    return(list(error = error_msg))
                }

                return(list(props = props_numeric, error = NULL))

            }, error = function(e) {
                return(list(error = paste("Error parsing proportions:", e$message)))
            })
        },

        .calculateParameterHash = function() {
            param_list <- list(
                outcome = self$options$outcome,
                citype = self$options$citype,
                kappa0 = self$options$kappa0,
                kappaL = self$options$kappaL,
                kappaU = self$options$kappaU,
                props = self$options$props,
                raters = self$options$raters,
                alpha = self$options$alpha
            )
            return(paste(param_list, collapse = "_"))
        },

        .canUseCache = function() {
            current_hash <- private$.calculateParameterHash()

            return(!is.null(private$.cached_result) &&
                   !is.null(private$.params_hash) &&
                   current_hash == private$.params_hash)
        },

        .prepareParameters = function() {
            current_hash <- private$.calculateParameterHash()

            if (is.null(private$.params_hash) || private$.params_hash != current_hash) {
                props_result <- private$.validateProportions()
                if (!is.null(props_result$error)) {
                    stop(props_result$error)
                }

                is_one_sided <- (self$options$citype == "one_sided")

                private$.prepared_params <- list(
                    outcome = as.numeric(self$options$outcome),
                    citype = self$options$citype,
                    kappa0 = self$options$kappa0,
                    kappaL = self$options$kappaL,
                    kappaU = if (is_one_sided) NA else self$options$kappaU,
                    props = props_result$props,
                    raters = as.numeric(self$options$raters),
                    alpha = self$options$alpha
                )

                private$.params_hash <- current_hash
                private$.cached_result <- NULL
                private$.cached_summary <- NULL
                private$.cached_explanation <- NULL
            }

            return(private$.prepared_params)
        },

        .calculateSampleSize = function(params) {
            if (!requireNamespace('kappaSize', quietly = TRUE)) {
                stop('The kappaSize package is required but not installed. Please install it using install.packages("kappaSize")')
            }

            kappa_function <- switch(
                as.character(params$outcome),
                "2" = kappaSize::CIBinary,
                "3" = kappaSize::CI3Cats,
                "4" = kappaSize::CI4Cats,
                "5" = kappaSize::CI5Cats,
                stop("Unsupported number of outcome categories")
            )

            tryCatch({
                result <- kappa_function(
                    kappa0 = params$kappa0,
                    kappaL = params$kappaL,
                    kappaU = params$kappaU,
                    props = params$props,
                    alpha = params$alpha,
                    raters = params$raters
                )

                return(result)

            }, error = function(e) {
                stop(paste("Error in sample size calculation:", e$message))
            })
        },

        .generateExplanation = function(params) {
            props_text <- if (params$outcome == 2) {
                paste("proportions of", paste(params$props, collapse = " and "))
            } else {
                prop_list <- paste(params$props[-length(params$props)], collapse = ", ")
                paste("proportions of", prop_list, ", and", params$props[length(params$props)])
            }

            is_one_sided <- (params$citype == "one_sided")

            if (is_one_sided) {
                ci_text <- paste0("• Lower confidence limit (κL): ", params$kappaL)
                ci_type_text <- "One-sided (lower bound only)"
                objective_text <- paste0(
                    "Determine the required sample size to estimate \u03ba\u2080 = ", params$kappa0,
                    " ensuring the lower confidence limit is at least ", params$kappaL,
                    " in an interobserver agreement study."
                )
            } else {
                ci_text <- paste0(
                    "• Confidence interval: [", params$kappaL, ", ", params$kappaU, "]\n",
                    "• Precision width: ", round(params$kappaU - params$kappaL, 3)
                )
                ci_type_text <- "Two-sided"
                objective_text <- paste0(
                    "Determine the required sample size to estimate \u03ba\u2080 = ", params$kappa0,
                    " with confidence limits [", params$kappaL, ", ", params$kappaU,
                    "] in an interobserver agreement study."
                )
            }

            explanation <- paste0(
                "Sample Size Calculation for Interobserver Agreement Study\n\n",
                "Study Design:\n",
                "• Number of outcome categories: ", params$outcome, "\n",
                "• Number of raters: ", params$raters, "\n",
                "• Significance level (\u03b1): ", params$alpha, "\n",
                "• CI type: ", ci_type_text, "\n\n",
                "Kappa Parameters:\n",
                "• Null hypothesis kappa (\u03ba\u2080): ", params$kappa0, "\n",
                ci_text, "\n\n",
                "Population Characteristics:\n",
                "• Expected category ", props_text, "\n\n",
                "Objective:\n",
                objective_text
            )

            return(explanation)
        },

        .formatSampleSizeOutput = function(result) {
            if (is.null(result) || length(result) == 0) {
                return("Sample size calculation failed")
            }

            required_n <- private$.extractRequiredN(result)
            if (is.na(required_n)) {
                return("Required sample size: unavailable")
            }

            is_one_sided <- (self$options$citype == "one_sided")

            if (is.list(result)) {
                sentence <- private$.buildExampleSentence(
                    required_n = required_n,
                    kappa0 = result$kappa0,
                    kappaL = result$kappaL,
                    kappaU = result$kappaU,
                    one_sided = is_one_sided
                )
            } else {
                sentence <- private$.buildExampleSentence(
                    required_n = required_n,
                    kappa0 = NA,
                    kappaL = NA,
                    kappaU = NA,
                    one_sided = is_one_sided
                )
            }

            return(paste0("Required sample size: ", required_n, "\n", sentence))
        },

        .extractRequiredN = function(result) {
            if (is.null(result) || length(result) == 0) {
                return(NA_real_)
            }

            if (is.list(result)) {
                if ("n" %in% names(result)) {
                    return(as.numeric(ceiling(result$n)))
                }
                if ("N" %in% names(result)) {
                    return(as.numeric(ceiling(result$N)))
                }
            }

            if (is.numeric(result) && length(result) == 1) {
                return(as.numeric(ceiling(result)))
            }

            return(NA_real_)
        },

        .buildExampleSentence = function(required_n, kappa0, kappaL, kappaU, one_sided = FALSE) {
            if (is.na(required_n)) {
                return("The required sample size could not be determined from the provided inputs.")
            }

            if (is.na(kappa0) || is.na(kappaL)) {
                return(paste0("At least ", required_n, " subjects are required for the requested confidence interval precision."))
            }

            if (one_sided || is.na(kappaU)) {
                return(paste0(
                    "At least ", required_n, " subjects are required to ensure the lower ",
                    "confidence limit for \u03ba\u2080 = ", kappa0, " is at least ", kappaL, "."
                ))
            }

            return(paste0(
                "At least ", required_n, " subjects are required to ensure the lower ",
                "confidence limit is at least ", kappaL, " and the upper confidence ",
                "limit does not exceed ", kappaU, "."
            ))
        },

        .run = function() {
            # Input validation
            validation_errors <- private$.validateInputs()
            if (!is.null(validation_errors)) {
                error_msg <- paste("Input validation failed:", paste(validation_errors, collapse = "; "))
                jmvcore::reject(error_msg, code='validation_failed')
            }

            # Check for cached results
            if (private$.canUseCache()) {
                self$results$text1$setContent(private$.cached_result)
                self$results$text_summary$setContent(private$.cached_summary)
                self$results$text2$setContent(private$.cached_explanation)
                return()
            }

            # Prepare parameters with caching
            tryCatch({
                params <- private$.prepareParameters()

                # Calculate sample size
                raw_result <- private$.calculateSampleSize(params)

                # Format the result
                formatted_result <- private$.formatSampleSizeOutput(raw_result)

                # Capture summary output from kappaSize
                summary_text <- paste(utils::capture.output(summary(raw_result)), collapse = "\n")

                # Generate explanation
                explanation <- private$.generateExplanation(params)

                # Cache results
                private$.cached_result <- formatted_result
                private$.cached_summary <- summary_text
                private$.cached_explanation <- explanation

                # Set results
                self$results$text1$setContent(formatted_result)
                self$results$text_summary$setContent(summary_text)
                self$results$text2$setContent(explanation)

            }, error = function(e) {
                error_msg <- paste("Error in kappa sample size calculation:", e$message)
                self$results$text1$setContent(error_msg)
                self$results$text2$setContent("Please check your input parameters and ensure the kappaSize package is installed.")
            })
        }
    )
)
