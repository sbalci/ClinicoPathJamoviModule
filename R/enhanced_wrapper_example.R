#' Enhanced Wrapper Example for ClinicoPath Functions
#'
#' Demonstrates how to integrate robust error handling into existing ClinicoPath analyses
#'
#' @name enhanced_wrapper_example
#' @import jmvcore
#' @importFrom stats t.test cor.test lm
NULL

#' Enhanced T-Test with Robust Error Handling
#' 
#' @description Enhanced t-test implementation with clinical context error handling
#' @param data Dataset for analysis
#' @param dependent Dependent variable name
#' @param group Grouping variable name (optional for one-sample test)
#' @param test_value Test value for one-sample test (default 0)
#' @param alternative Alternative hypothesis ("two.sided", "less", "greater")
#' @param conf_level Confidence level (default 0.95)
#' @param clinical_context Clinical research context
#' @export
enhanced_ttest <- function(data, dependent, group = NULL, test_value = 0, 
                          alternative = "two.sided", conf_level = 0.95,
                          clinical_context = "general") {
    
    # Initialize error handling
    clinicopath_init("enhanced_ttest", 
                     paste("Clinical context:", clinical_context))
    
    # Initialize result structure
    result <- list(
        success = FALSE,
        results = NULL,
        clinical_interpretation = NULL,
        warnings = NULL,
        errors = NULL
    )
    
    tryCatch(
        withCallingHandlers({
            
            # Validate input data
            validation <- validate_clinical_data(
                data = data,
                required_vars = c(dependent, group),
                min_observations = if(is.null(group)) 5 else 10,
                clinical_checks = TRUE
            )
            
            if (!validation$valid) {
                stop(paste("Data validation failed:", 
                          paste(validation$errors, collapse = "; ")))
            }
            
            # Add warnings if any
            if (length(validation$warnings) > 0) {
                for (warn in validation$warnings) {
                    warning(warn)
                }
            }
            
            # Prepare data with error handling
            analysis_result <- safe_execute(
                expr = {
                    if (is.null(group)) {
                        # One-sample t-test
                        values <- data[[dependent]]
                        values <- values[!is.na(values)]
                        
                        if (length(values) < 5) {
                            stop("Insufficient data for one-sample t-test (need at least 5 observations)")
                        }
                        
                        t.test(values, mu = test_value, alternative = alternative, conf.level = conf_level)
                        
                    } else {
                        # Two-sample t-test
                        group_var <- data[[group]]
                        dep_var <- data[[dependent]]
                        
                        # Remove missing values
                        complete_cases <- !is.na(group_var) & !is.na(dep_var)
                        group_var <- group_var[complete_cases]
                        dep_var <- dep_var[complete_cases]
                        
                        if (length(unique(group_var)) != 2) {
                            stop("Grouping variable must have exactly 2 levels for two-sample t-test")
                        }
                        
                        group_levels <- unique(group_var)
                        n1 <- sum(group_var == group_levels[1])
                        n2 <- sum(group_var == group_levels[2])
                        
                        if (n1 < 3 || n2 < 3) {
                            stop("Each group must have at least 3 observations")
                        }
                        
                        t.test(dep_var ~ group_var, alternative = alternative, conf.level = conf_level)
                    }
                },
                function_name = "enhanced_ttest",
                clinical_context = clinical_context,
                default_value = NULL
            )
            
            if (!analysis_result$success) {
                result$errors <- analysis_result$error_info
                return(result)
            }
            
            # Extract and format results
            ttest_result <- analysis_result$result
            
            formatted_results <- list(
                statistic = ttest_result$statistic,
                df = ttest_result$parameter,
                p_value = ttest_result$p.value,
                confidence_interval = ttest_result$conf.int,
                estimate = ttest_result$estimate,
                method = ttest_result$method,
                alternative = ttest_result$alternative,
                effect_size = calculate_effect_size(ttest_result, clinical_context),
                clinical_significance = assess_clinical_significance(ttest_result, clinical_context)
            )
            
            # Add clinical interpretation
            clinical_interp <- generate_clinical_interpretation(
                results = formatted_results,
                context = clinical_context,
                sample_size = validation$n_observations
            )
            
            result$success <- TRUE
            result$results <- formatted_results
            result$clinical_interpretation <- clinical_interp
            
        },
        warning = function(w) {
            warning_info <- clinicopath_warning_handler(w, "enhanced_ttest", clinical_context)
            result$warnings <- c(result$warnings, warning_info$warning_message)
            invokeRestart("muffleWarning")
        }),
        
        error = function(e) {
            error_info <- clinicopath_error_handler(e, "enhanced_ttest", clinical_context)
            result$errors <- error_info
            result$success <- FALSE
        }
    )
    
    # Clean up and finalize
    clinicopath_cleanup("enhanced_ttest")
    
    # Create enhanced result structure
    enhanced_result <- create_enhanced_result(
        results = result,
        function_name = "enhanced_ttest",
        success = result$success
    )
    
    return(enhanced_result)
}

#' Calculate Effect Size for T-Test
#' 
#' @description Calculate Cohen's d and other effect size measures
#' @param ttest_result T-test result object
#' @param clinical_context Clinical research context
calculate_effect_size <- function(ttest_result, clinical_context) {
    
    # Extract statistics
    t_stat <- as.numeric(ttest_result$statistic)
    df <- as.numeric(ttest_result$parameter)
    
    # Calculate Cohen's d
    if (length(ttest_result$estimate) == 1) {
        # One-sample test
        cohens_d <- t_stat / sqrt(df + 1)
    } else {
        # Two-sample test  
        cohens_d <- t_stat * sqrt(2 / df)
    }
    
    # Effect size interpretation
    magnitude <- if (abs(cohens_d) < 0.2) "Negligible"
                else if (abs(cohens_d) < 0.5) "Small"
                else if (abs(cohens_d) < 0.8) "Medium"
                else "Large"
    
    return(list(
        cohens_d = cohens_d,
        magnitude = magnitude,
        clinical_relevance = assess_clinical_relevance(cohens_d, clinical_context)
    ))
}

#' Assess Clinical Significance
#' 
#' @description Assess clinical significance beyond statistical significance
#' @param results Analysis results
#' @param clinical_context Clinical research context
assess_clinical_significance <- function(results, clinical_context) {
    
    p_value <- results$p_value
    effect_size <- results$effect_size$cohens_d
    confidence_interval <- results$confidence_interval
    
    # Clinical significance assessment
    clinically_significant <- FALSE
    interpretation <- "Not clinically significant"
    
    if (clinical_context == "diagnostic") {
        # For diagnostic tests, consider practical significance
        if (abs(effect_size) > 0.5 && p_value < 0.05) {
            clinically_significant <- TRUE
            interpretation <- "Clinically significant difference in diagnostic performance"
        }
    } else if (clinical_context == "treatment") {
        # For treatment comparisons, consider effect size and CI
        if (abs(effect_size) > 0.3 && 
            (confidence_interval[1] > 0.1 || confidence_interval[2] < -0.1)) {
            clinically_significant <- TRUE
            interpretation <- "Clinically meaningful treatment effect"
        }
    } else if (clinical_context == "biomarker") {
        # For biomarker studies, consider both statistical and practical significance
        if (abs(effect_size) > 0.4 && p_value < 0.01) {
            clinically_significant <- TRUE
            interpretation <- "Biomarker shows clinically relevant association"
        }
    }
    
    return(list(
        clinically_significant = clinically_significant,
        interpretation = interpretation,
        recommendations = get_clinical_recommendations(results, clinical_context)
    ))
}

#' Assess Clinical Relevance of Effect Size
#' 
#' @description Provide clinical relevance assessment
#' @param effect_size Calculated effect size
#' @param clinical_context Clinical research context
assess_clinical_relevance <- function(effect_size, clinical_context) {
    
    if (clinical_context == "survival") {
        if (abs(effect_size) > 0.5) {
            return("Potentially clinically meaningful survival difference")
        } else {
            return("Limited clinical significance for survival outcomes")
        }
    } else if (clinical_context == "quality_of_life") {
        if (abs(effect_size) > 0.3) {
            return("Potentially meaningful quality of life difference")
        } else {
            return("Below threshold for meaningful quality of life improvement")
        }
    } else {
        return("Standard effect size interpretation applies")
    }
}

#' Generate Clinical Interpretation
#' 
#' @description Generate comprehensive clinical interpretation
#' @param results Analysis results
#' @param context Clinical context
#' @param sample_size Sample size
generate_clinical_interpretation <- function(results, context, sample_size) {
    
    interpretation <- list(
        statistical_summary = generate_statistical_summary(results),
        clinical_summary = generate_clinical_summary(results, context),
        limitations = assess_limitations(results, sample_size),
        recommendations = get_analysis_recommendations(results, context, sample_size)
    )
    
    return(interpretation)
}

#' Generate Statistical Summary
#' 
#' @description Create statistical summary text
#' @param results Analysis results
generate_statistical_summary <- function(results) {
    p_val <- results$p_value
    t_stat <- results$statistic
    df <- results$df
    
    significance <- if (p_val < 0.001) "highly significant (p < 0.001)"
                   else if (p_val < 0.01) "very significant (p < 0.01)"
                   else if (p_val < 0.05) "significant (p < 0.05)"
                   else "not statistically significant"
    
    paste0("The t-test yielded t(", df, ") = ", round(t_stat, 3), 
           ", which is ", significance, ". Effect size (Cohen's d) = ", 
           round(results$effect_size$cohens_d, 3), " (", 
           tolower(results$effect_size$magnitude), " effect).")
}

#' Generate Clinical Summary
#' 
#' @description Create clinical context summary
#' @param results Analysis results
#' @param context Clinical context
generate_clinical_summary <- function(results, context) {
    
    base_text <- "Based on the analysis results, "
    
    if (context == "treatment") {
        if (results$clinical_significance$clinically_significant) {
            return(paste0(base_text, "the treatment shows clinically meaningful effects that warrant clinical consideration."))
        } else {
            return(paste0(base_text, "while statistically detectable, the treatment effect may not be clinically meaningful."))
        }
    } else if (context == "diagnostic") {
        if (results$clinical_significance$clinically_significant) {
            return(paste0(base_text, "the diagnostic difference is both statistically and clinically significant."))
        } else {
            return(paste0(base_text, "the diagnostic difference may not be clinically important despite statistical findings."))
        }
    } else {
        return(paste0(base_text, "interpret results in the context of clinical research objectives."))
    }
}

#' Assess Analysis Limitations
#' 
#' @description Identify potential limitations
#' @param results Analysis results
#' @param sample_size Sample size
assess_limitations <- function(results, sample_size) {
    
    limitations <- c()
    
    if (sample_size < 30) {
        limitations <- c(limitations, "Small sample size may limit generalizability")
    }
    
    if (results$p_value > 0.05 && results$p_value < 0.1) {
        limitations <- c(limitations, "Marginal statistical significance - interpret cautiously")
    }
    
    if (abs(results$effect_size$cohens_d) < 0.2) {
        limitations <- c(limitations, "Small effect size may limit practical significance")
    }
    
    return(limitations)
}

#' Get Clinical Recommendations
#' 
#' @description Provide clinical recommendations based on results
#' @param results Analysis results  
#' @param context Clinical context
get_clinical_recommendations <- function(results, context) {
    
    recommendations <- c()
    
    if (context == "treatment") {
        if (results$clinical_significance$clinically_significant) {
            recommendations <- c(recommendations, "Consider implementing treatment in clinical practice")
            recommendations <- c(recommendations, "Monitor for treatment effects in routine care")
        } else {
            recommendations <- c(recommendations, "Additional research may be needed to establish clinical utility")
        }
    }
    
    if (results$p_value > 0.05) {
        recommendations <- c(recommendations, "Consider power analysis for future studies")
    }
    
    return(recommendations)
}

#' Get Analysis Recommendations
#' 
#' @description Get comprehensive analysis recommendations
#' @param results Analysis results
#' @param context Clinical context
#' @param sample_size Sample size
get_analysis_recommendations <- function(results, context, sample_size) {
    
    recommendations <- c()
    
    # Sample size recommendations
    if (sample_size < 50) {
        recommendations <- c(recommendations, "Consider larger sample size for more robust results")
    }
    
    # Effect size recommendations
    if (abs(results$effect_size$cohens_d) > 0.8) {
        recommendations <- c(recommendations, "Large effect size suggests meaningful clinical difference")
    }
    
    # Context-specific recommendations
    if (context == "biomarker") {
        recommendations <- c(recommendations, "Validate findings in independent cohort")
        recommendations <- c(recommendations, "Consider biological plausibility of results")
    }
    
    return(recommendations)
}