#' @title Decision Curve Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon geom_vline geom_hline
#' @importFrom ggplot2 labs theme_minimal scale_color_brewer annotate xlim ylim
#' @importFrom ggplot2 scale_x_continuous geom_text geom_bar facet_wrap scale_fill_manual
#' @importFrom ggplot2 scale_x_discrete element_text
#' @importFrom dplyr filter mutate group_by summarise arrange
#' @importFrom tidyr gather
#' @importFrom stats quantile complete.cases

decisioncurveClass <- if (requireNamespace("jmvcore")) R6::R6Class(
    "decisioncurveClass",
    inherit = decisioncurveBase,
    private = list(

        # Store analysis results
        .dcaResults = NULL,
        .plotData = NULL,
        .clinicalImpactData = NULL,
        .analysisData = NULL,
        .analysisOutcomes = NULL,

        # Helper method to escape variable names for notice IDs
        .escapeVar = function(varName) {
            gsub("[^A-Za-z0-9]", "_", varName)
        },

        # HTML sanitization for security
        .safeHtmlOutput = function(text) {
          if (is.null(text) || length(text) == 0) return("")
          text <- as.character(text)
          # Sanitize potentially dangerous characters
          text <- gsub("&", "&amp;", text, fixed = TRUE)
          text <- gsub("<", "&lt;", text, fixed = TRUE)
          text <- gsub(">", "&gt;", text, fixed = TRUE)
          text <- gsub("\"", "&quot;", text, fixed = TRUE)
          text <- gsub("'", "&#x27;", text, fixed = TRUE)
          text <- gsub("/", "&#x2F;", text, fixed = TRUE)
          return(text)
        },

        # Initialize notice collection list
        .noticeList = list(),

        # Add a notice to the collection
        .addNotice = function(type, title, content) {
          private$.noticeList[[length(private$.noticeList) + 1]] <- list(
            type = type,
            title = title,
            content = content
          )
        },

        # Render collected notices as HTML
        .renderNotices = function() {
          if (length(private$.noticeList) == 0) {
            return()
          }

          # Map notice types to colors and icons
          typeStyles <- list(
            ERROR = list(color = "#dc2626", bgcolor = "#fef2f2", border = "#fca5a5", icon = "⛔"),
            STRONG_WARNING = list(color = "#ea580c", bgcolor = "#fff7ed", border = "#fdba74", icon = "⚠️"),
            WARNING = list(color = "#ca8a04", bgcolor = "#fefce8", border = "#fde047", icon = "⚡"),
            INFO = list(color = "#2563eb", bgcolor = "#eff6ff", border = "#93c5fd", icon = "ℹ️")
          )

          html <- "<div style='margin: 10px 0;'>"

          for (notice in private$.noticeList) {
            style <- typeStyles[[notice$type]] %||% typeStyles$INFO

            html <- paste0(html,
              "<div style='background-color: ", style$bgcolor, "; ",
              "border-left: 4px solid ", style$border, "; ",
              "padding: 12px; margin: 8px 0; border-radius: 4px;'>",
              "<strong style='color: ", style$color, ";'>",
              style$icon, " ", private$.safeHtmlOutput(notice$title), "</strong><br>",
              "<span style='color: #374151;'>", private$.safeHtmlOutput(notice$content), "</span>",
              "</div>"
            )
          }

          html <- paste0(html, "</div>")

          self$results$notices$setContent(html)
        },

        # Constants for default values and thresholds
        DECISIONCURVE_DEFAULTS = list(
            selected_thresholds = c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30),
            bootstrap_progress_threshold = 5000,
            performance_threshold_count = 1000,  # Threshold count for performance optimization
            bootstrap_chunk_size = 10000,       # Memory-efficient chunking threshold
            bootstrap_convergence_check = 500,  # Check convergence every N iterations
            convergence_tolerance = 0.001,      # CI stability tolerance
            max_models_full_plot = 10           # Plot optimization threshold
        ),

        # Calculate net benefit for a model at given threshold
        .calculateNetBenefit = function(predictions, outcomes, threshold, positive_outcome) {
            # Convert outcomes to binary (1 = positive, 0 = negative)
            binary_outcomes <- as.numeric(outcomes == positive_outcome)

            # Calculate predictions at threshold
            predicted_positive <- predictions >= threshold

            # Calculate confusion matrix elements
            tp <- sum(predicted_positive & binary_outcomes == 1)
            fp <- sum(predicted_positive & binary_outcomes == 0)
            tn <- sum(!predicted_positive & binary_outcomes == 0)
            fn <- sum(!predicted_positive & binary_outcomes == 1)

            n <- length(outcomes)
            prevalence <- sum(binary_outcomes) / n

            # Calculate net benefit
            if (tp + fn == 0) {
                sensitivity <- 0
            } else {
                sensitivity <- tp / (tp + fn)
            }

            if (fp + tn == 0) {
                specificity <- 1
            } else {
                specificity <- tn / (fp + tn)
            }

            # Net benefit formula
            nb <- (tp / n) - (fp / n) * (threshold / (1 - threshold))

            return(list(
                net_benefit = nb,
                sensitivity = sensitivity,
                specificity = specificity,
                tp = tp, fp = fp, tn = tn, fn = fn,
                prevalence = prevalence,
                interventions_per_100 = sum(predicted_positive) / n * 100,
                true_positives_per_100 = tp / n * 100,
                false_positives_per_100 = fp / n * 100
            ))
        },

        # Calculate net benefit for treat all strategy
        .calculateTreatAllNetBenefit = function(outcomes, threshold, positive_outcome) {
            binary_outcomes <- as.numeric(outcomes == positive_outcome)
            prevalence <- mean(binary_outcomes)

            # For treat all: sensitivity = 1, specificity = 0
            nb <- prevalence - (1 - prevalence) * (threshold / (1 - threshold))
            return(nb)
        },

        # Calculate net benefit for treat none strategy (always 0)
        .calculateTreatNoneNetBenefit = function() {
            return(0)
        },
        
        # Vectorized net benefit calculation for performance optimization
        .calculateNetBenefitsVectorized = function(predictions, outcomes, thresholds, positive_outcome) {
            # Convert outcomes to binary once
            binary_outcomes <- as.numeric(outcomes == positive_outcome)
            n <- length(outcomes)
            
            # Pre-allocate result vector
            net_benefits <- numeric(length(thresholds))
            
            # Calculate for each threshold (still a loop but optimized inner calculations)
            for (j in seq_along(thresholds)) {
                thresh <- thresholds[j]
                
                # Vectorized threshold comparison
                predicted_positive <- predictions >= thresh
                
                # Vectorized confusion matrix calculation
                tp <- sum(predicted_positive & binary_outcomes == 1)
                fp <- sum(predicted_positive & binary_outcomes == 0)
                
                # Net benefit formula
                net_benefits[j] <- (tp / n) - (fp / n) * (thresh / (1 - thresh))
            }
            
            return(net_benefits)
        },

        # Generate threshold sequence with enhanced validation
        .generateThresholds = function() {
            range_type <- self$options$thresholdRange
            step <- self$options$thresholdStep

            if (range_type == "auto") {
                thresholds <- seq(0.01, 0.99, by = step)
            } else if (range_type == "clinical") {
                thresholds <- seq(0.05, 0.50, by = step)
            } else { # custom
                min_thresh <- self$options$thresholdMin
                max_thresh <- self$options$thresholdMax
                
                # Enhanced threshold range validation with clinical guidance
                private$.validateThresholdRange(min_thresh, max_thresh)
                
                thresholds <- seq(min_thresh, max_thresh, by = step)
            }

            return(thresholds)
        },
        
        # Validate threshold ranges with clinical context and guidance
        .validateThresholdRange = function(min_thresh, max_thresh) {
            # Basic validation
            if (min_thresh >= max_thresh) {
                private$.addNotice(
                    type = "ERROR",
                    title = "Invalid Threshold Range",
                    content = sprintf(
                        'Minimum threshold (%.1f%%) must be less than maximum threshold (%.1f%%). Current settings: Min = %.1f%%, Max = %.1f%%. Please adjust threshold range in Analysis Options.',
                        min_thresh * 100,
                        max_thresh * 100,
                        min_thresh * 100,
                        max_thresh * 100
                    )
                )
                private$.renderNotices()
                stop("Validation failed", call. = FALSE)
            }

            if (min_thresh <= 0 || max_thresh >= 1) {
                private$.addNotice(
                    type = "ERROR",
                    title = "Threshold Out of Bounds",
                    content = sprintf(
                        'Threshold probabilities must be between 0 and 1 (exclusive). Current settings: Min = %.1f%%, Max = %.1f%%. Valid range: 0.1%% to 99.9%%. Please adjust threshold range.',
                        min_thresh * 100,
                        max_thresh * 100
                    )
                )
                private$.renderNotices()
                stop("Validation failed", call. = FALSE)
            }
            
            # Clinical guidance warnings for unusual ranges
            if (max_thresh > 0.8) {
                private$.addNotice(
                    type = "STRONG_WARNING",
                    title = "Very High Threshold",
                    content = sprintf(
                        'Very high maximum threshold (%.1f%%). Decision thresholds above 80%% are rarely clinically meaningful for most medical decisions. Consider whether this reflects your actual clinical decision context.',
                        max_thresh * 100
                    )
                )
            }

            if (min_thresh < 0.01) {
                private$.addNotice(
                    type = "WARNING",
                    title = "Very Low Threshold",
                    content = sprintf(
                        'Very low minimum threshold (%.1f%%). Thresholds below 1%% may not be clinically interpretable for most medical decisions. Ensure this aligns with your clinical context.',
                        min_thresh * 100
                    )
                )
            }

            # Range size warnings
            range_size <- max_thresh - min_thresh
            if (range_size > 0.7) {
                private$.addNotice(
                    type = "WARNING",
                    title = "Very Wide Range",
                    content = sprintf(
                        'Very wide threshold range (%.1f%% span). Consider focusing on clinically relevant range for your specific decision context (e.g., 5-50%% for most screening decisions).',
                        range_size * 100
                    )
                )
            }

            if (range_size < 0.05) {
                private$.addNotice(
                    type = "WARNING",
                    title = "Narrow Range",
                    content = sprintf(
                        'Narrow threshold range (%.1f%% span). Decision curve analysis is most informative across wider probability ranges (typically 10-30%% span or more).',
                        range_size * 100
                    )
                )
            }
            
            # Clinical context guidance
            private$.provideThresholdContextGuidance(min_thresh, max_thresh)
        },
        
        # Provide clinical context guidance for threshold selection
        .provideThresholdContextGuidance = function(min_thresh, max_thresh) {
            # Determine likely clinical contexts based on threshold range
            cancer_screening_range <- min_thresh <= 0.10 && max_thresh >= 0.15
            surgical_decision_range <- min_thresh <= 0.20 && max_thresh >= 0.40
            treatment_selection_range <- min_thresh <= 0.15 && max_thresh >= 0.35
            
            guidance_messages <- character(0)
            
            if (cancer_screening_range) {
                guidance_messages <- c(guidance_messages, 
                    "• Threshold range suitable for cancer screening decisions (typical range: 5-20%)")
            }
            
            if (surgical_decision_range) {
                guidance_messages <- c(guidance_messages, 
                    "• Threshold range suitable for surgical intervention decisions (typical range: 10-50%)")
            }
            
            if (treatment_selection_range) {
                guidance_messages <- c(guidance_messages, 
                    "• Threshold range suitable for treatment selection decisions (typical range: 15-40%)")
            }
            
            if (max_thresh <= 0.05) {
                guidance_messages <- c(guidance_messages, 
                    "• Very low threshold range - consider if this aligns with your clinical decision context")
            }
            
            if (min_thresh >= 0.60) {
                guidance_messages <- c(guidance_messages, 
                    "• Very high threshold range - ensure this reflects actual clinical decision thresholds")
            }
            
            if (length(guidance_messages) > 0) {
                message("Clinical threshold context guidance:")
                for (msg in guidance_messages) {
                    message(msg)
                }
            }
        },

        # Parse selected thresholds for table
        .parseSelectedThresholds = function() {
            threshold_str <- self$options$selectedThresholds
            if (threshold_str == "") {
                return(private$DECISIONCURVE_DEFAULTS$selected_thresholds)
            }

            # Parse comma-separated values
            thresholds <- as.numeric(unlist(strsplit(threshold_str, "[,;\\s]+")))
            thresholds <- thresholds[!is.na(thresholds)]
            thresholds <- thresholds[thresholds > 0 & thresholds < 1]

            if (length(thresholds) == 0) {
                return(private$DECISIONCURVE_DEFAULTS$selected_thresholds)
            }

            return(sort(thresholds))
        },

        # Parse model names
        .parseModelNames = function() {
            model_names_str <- self$options$modelNames
            model_vars <- self$options$models

            if (model_names_str == "" || is.null(model_names_str)) {
                return(model_vars)
            }

            # Parse comma-separated names
            names <- trimws(unlist(strsplit(model_names_str, ",")))

            # If number of names doesn't match variables, use variable names
            if (length(names) != length(model_vars)) {
                return(model_vars)
            }

            return(names)
        },

        # Check bootstrap convergence for early termination
        .checkBootstrapConvergence = function(ci_history_lower, ci_history_upper, tolerance = NULL) {
            if (is.null(tolerance)) tolerance <- private$DECISIONCURVE_DEFAULTS$convergence_tolerance
            
            # Need at least 100 iterations to assess convergence
            if (length(ci_history_lower) < 100) return(FALSE)
            
            # Check stability of recent CI estimates
            recent_lower <- tail(ci_history_lower, 20)
            recent_upper <- tail(ci_history_upper, 20)
            
            # Calculate moving range of recent estimates
            lower_changes <- abs(diff(recent_lower))
            upper_changes <- abs(diff(recent_upper))
            
            # Convergence achieved if recent changes are small
            lower_stable <- all(lower_changes < tolerance, na.rm = TRUE)
            upper_stable <- all(upper_changes < tolerance, na.rm = TRUE)
            
            return(lower_stable && upper_stable)
        },
        
        # Memory-efficient chunked bootstrap for very large n_boot
        .calculateBootstrapCIChunked = function(predictions, outcomes, thresholds, positive_outcome, n_boot = 1000) {
            chunk_size <- private$DECISIONCURVE_DEFAULTS$bootstrap_chunk_size
            
            if (n_boot <= chunk_size) {
                return(private$.calculateBootstrapCI(predictions, outcomes, thresholds, positive_outcome, n_boot))
            }
            
            message(sprintf("Large bootstrap (%d reps): Using memory-efficient chunked processing with %d reps per chunk", 
                           n_boot, chunk_size))
            
            n_chunks <- ceiling(n_boot / chunk_size)
            all_results <- list()
            
            for (chunk in 1:n_chunks) {
                chunk_start <- (chunk - 1) * chunk_size + 1
                chunk_end <- min(chunk * chunk_size, n_boot)
                chunk_n_boot <- chunk_end - chunk_start + 1
                
                message(sprintf("Processing chunk %d/%d (%d replications)...", chunk, n_chunks, chunk_n_boot))
                
                chunk_result <- private$.calculateBootstrapCI(
                    predictions, outcomes, thresholds, positive_outcome, chunk_n_boot
                )
                
                if (!is.null(chunk_result)) {
                    all_results[[chunk]] <- list(
                        lower = chunk_result$lower,
                        upper = chunk_result$upper,
                        n_boot = chunk_n_boot
                    )
                }
                
                # Memory cleanup
                gc(verbose = FALSE)
            }
            
            # Combine results from all chunks
            if (length(all_results) == 0) {
                return(list(lower = rep(NA, length(thresholds)), upper = rep(NA, length(thresholds))))
            }
            
            # Weight by chunk size and combine
            total_weight <- sum(sapply(all_results, function(x) x$n_boot))
            combined_lower <- numeric(length(thresholds))
            combined_upper <- numeric(length(thresholds))
            
            for (i in seq_along(all_results)) {
                weight <- all_results[[i]]$n_boot / total_weight
                combined_lower <- combined_lower + weight * all_results[[i]]$lower
                combined_upper <- combined_upper + weight * all_results[[i]]$upper
            }
            
            message("Chunked bootstrap processing completed successfully.")
            return(list(lower = combined_lower, upper = combined_upper))
        },

        # Bootstrap confidence intervals with enhanced error handling and progress reporting
        .calculateBootstrapCI = function(predictions, outcomes, thresholds, positive_outcome, n_boot = 1000) {

            # Validate inputs
            if (length(predictions) != length(outcomes)) {
                private$.addNotice(
                    type = "ERROR",
                    title = "Bootstrap CI Calculation Error",
                    content = sprintf(
                        'Bootstrap CI calculation error: Predictions and outcomes have different lengths (%d vs %d). This indicates a data processing error. Please report this issue.',
                        length(predictions),
                        length(outcomes)
                    )
                )
                private$.renderNotices()
                stop("Validation failed", call. = FALSE)
            }

            if (n_boot < 100) {
                private$.addNotice(
                    type = "WARNING",
                    title = "Low Bootstrap Replications",
                    content = sprintf(
                        'Low bootstrap replications (%d). Using fewer than 100 replications may give unreliable confidence intervals. Consider increasing to at least 1000 replications for stable estimates.',
                        n_boot
                    )
                )
            }
            
            # Use chunked bootstrap for very large n_boot to manage memory
            if (n_boot >= private$DECISIONCURVE_DEFAULTS$bootstrap_chunk_size) {
                return(private$.calculateBootstrapCIChunked(predictions, outcomes, thresholds, positive_outcome, n_boot))
            }
            
            # Progress reporting for large bootstrap runs
            if (n_boot >= private$DECISIONCURVE_DEFAULTS$bootstrap_progress_threshold) {
                message(sprintf("Bootstrap confidence intervals: Running %d replications (this may take several minutes)...", n_boot))
            }
            
            n <- length(outcomes)
            boot_results <- array(NA, dim = c(n_boot, length(thresholds)))
            
            # Convergence tracking for early termination
            convergence_check_interval <- private$DECISIONCURVE_DEFAULTS$bootstrap_convergence_check
            ci_history_lower <- list()
            ci_history_upper <- list()
            converged_early <- FALSE
            
            tryCatch({
                for (i in 1:n_boot) {
                    # Progress indicators for very large bootstrap runs
                    if (n_boot >= 10000 && i %% 2000 == 0) {
                        message(sprintf("Bootstrap progress: %d/%d replications completed (%.1f%%)", 
                                       i, n_boot, (i/n_boot)*100))
                    }
                    
                    # Check convergence periodically for early termination
                    if (i %% convergence_check_interval == 0 && i >= convergence_check_interval * 2) {
                        # Calculate interim CI estimates
                        interim_lower <- apply(boot_results[1:i, , drop = FALSE], 2, function(x) {
                            if (sum(!is.na(x)) < 10) return(NA)
                            quantile(x, probs = (1 - self$options$ciLevel) / 2, na.rm = TRUE)
                        })
                        interim_upper <- apply(boot_results[1:i, , drop = FALSE], 2, function(x) {
                            if (sum(!is.na(x)) < 10) return(NA)
                            quantile(x, probs = 1 - (1 - self$options$ciLevel) / 2, na.rm = TRUE)
                        })
                        
                        ci_history_lower[[length(ci_history_lower) + 1]] <- interim_lower
                        ci_history_upper[[length(ci_history_upper) + 1]] <- interim_upper
                        
                        # Check if converged
                        if (length(ci_history_lower) >= 3) {
                            last_lower <- sapply(ci_history_lower, function(x) mean(x, na.rm = TRUE))
                            last_upper <- sapply(ci_history_upper, function(x) mean(x, na.rm = TRUE))
                            
                            if (private$.checkBootstrapConvergence(last_lower, last_upper)) {
                                message(sprintf("Bootstrap confidence intervals converged early at iteration %d (%.1f%% of requested replications)", 
                                               i, (i/n_boot)*100))
                                converged_early <- TRUE
                                n_boot <- i  # Update effective n_boot
                                boot_results <- boot_results[1:i, , drop = FALSE]
                                break
                            }
                        }
                    }
                    
                    # Bootstrap sample with error checking
                    boot_idx <- sample(n, n, replace = TRUE)
                    boot_pred <- predictions[boot_idx]
                    boot_out <- outcomes[boot_idx]
                    
                    # Validate bootstrap sample has variation
                    if (length(unique(boot_out)) < 2) {
                        # Skip silently - this can happen randomly with very small samples or extreme prevalence
                        next
                    }

                    # Calculate net benefits for this bootstrap sample
                    for (j in seq_along(thresholds)) {
                        thresh <- thresholds[j]
                        nb_result <- private$.calculateNetBenefit(
                            boot_pred, boot_out, thresh, positive_outcome
                        )
                        boot_results[i, j] <- nb_result$net_benefit
                    }
                }

                # Calculate confidence intervals with error handling
                ci_lower <- apply(boot_results, 2, function(x) {
                    if (sum(!is.na(x)) < 10) return(NA)
                    quantile(x, probs = (1 - self$options$ciLevel) / 2, na.rm = TRUE)
                })
                
                ci_upper <- apply(boot_results, 2, function(x) {
                    if (sum(!is.na(x)) < 10) return(NA)
                    quantile(x, probs = 1 - (1 - self$options$ciLevel) / 2, na.rm = TRUE)
                })
                
                if (n_boot >= private$DECISIONCURVE_DEFAULTS$bootstrap_progress_threshold) {
                    message("Bootstrap confidence intervals completed successfully.")
                }

                return(list(lower = ci_lower, upper = ci_upper))
                
            }, error = function(e) {
                private$.addNotice(
                    type = "WARNING",
                    title = "Bootstrap CI Failed",
                    content = sprintf(
                        'Bootstrap confidence interval calculation failed: %s. Continuing analysis without confidence intervals. Results are still valid, but CI uncertainty estimates are unavailable.',
                        conditionMessage(e)
                    )
                )
                return(list(
                    lower = rep(NA, length(thresholds)),
                    upper = rep(NA, length(thresholds))
                ))
            })
        },

        # Find optimal threshold for a model
        .findOptimalThreshold = function(net_benefits, thresholds) {
            # Find threshold with maximum net benefit
            max_idx <- which.max(net_benefits)
            optimal_threshold <- thresholds[max_idx]
            max_net_benefit <- net_benefits[max_idx]

            # Find range where model is beneficial (net benefit > 0)
            beneficial <- net_benefits > 0
            if (any(beneficial)) {
                beneficial_thresholds <- thresholds[beneficial]
                range_start <- min(beneficial_thresholds)
                range_end <- max(beneficial_thresholds)
            } else {
                range_start <- NA
                range_end <- NA
            }

            return(list(
                optimal_threshold = optimal_threshold,
                max_net_benefit = max_net_benefit,
                range_start = range_start,
                range_end = range_end
            ))
        },

        # Calculate weighted AUC
        .calculateWeightedAUC = function(net_benefits, thresholds) {
            # Remove any missing values
            valid_idx <- !is.na(net_benefits) & !is.na(thresholds)
            nb_clean <- net_benefits[valid_idx]
            th_clean <- thresholds[valid_idx]

            if (length(nb_clean) < 2) {
                return(NA)
            }

            # Calculate AUC using trapezoidal rule
            # Sort by threshold
            ord <- order(th_clean)
            nb_sorted <- nb_clean[ord]
            th_sorted <- th_clean[ord]

            # Trapezoidal integration
            auc <- 0
            for (i in 2:length(th_sorted)) {
                width <- th_sorted[i] - th_sorted[i-1]
                height <- (nb_sorted[i] + nb_sorted[i-1]) / 2
                auc <- auc + width * height
            }

            # Normalize by range
            total_range <- max(th_sorted) - min(th_sorted)
            return(auc / total_range)
        },

        # Main analysis function
        .run = function() {

            # Check if required packages are available
            required_packages <- c("ggplot2", "dplyr", "tidyr")
            missing_packages <- character(0)

            for (pkg in required_packages) {
                if (!requireNamespace(pkg, quietly = TRUE)) {
                    missing_packages <- c(missing_packages, pkg)
                }
            }

            if (length(missing_packages) > 0) {
                private$.addNotice(
                    type = "ERROR",
                    title = "Missing Packages",
                    content = sprintf(
                        'Required packages missing: %s. Install with install.packages(c(%s)). These packages are essential for Decision Curve Analysis visualizations and data processing.',
                        paste(missing_packages, collapse = ", "),
                        paste0('"', paste(missing_packages, collapse = '", "'), '"')
                    )
                )
                private$.renderNotices()
                return()
            }

            # Show instructions if needed
            if (is.null(self$options$outcome) || is.null(self$options$models) ||
                length(self$options$models) == 0) {

                instructions <- "
                <html>
                <head></head>
                <body>
                <div class='instructions'>
                <p><b>Decision Curve Analysis</b></p>
                <p>Decision Curve Analysis evaluates the clinical utility of prediction models by calculating net benefit across different threshold probabilities.</p>
                <p>To get started:</p>
                <ol>
                <li>Select a binary <b>Outcome Variable</b> (the condition you want to predict)</li>
                <li>Specify which level represents the positive outcome</li>
                <li>Add one or more <b>Prediction Variables/Models</b> (predicted probabilities or risk scores)</li>
                <li>Configure the threshold range and other analysis options</li>
                </ol>
                <p>The analysis will show whether using your prediction model(s) provides more clinical benefit than treating all patients or treating no patients.</p>
                </div>
                </body>
                </html>
                "

                self$results$instructions$setContent(instructions)
                return()
            }

            # Hide instructions when analysis can proceed
            self$results$instructions$setVisible(FALSE)

            # Get data and variables
            data <- self$data
            outcome_var <- self$options$outcome
            outcome_positive <- self$options$outcomePositive
            model_vars <- self$options$models

            # Parse model names
            model_names <- private$.parseModelNames()

            # Clinical decision rule variable (optional)
            rule_var <- NULL
            rule_positive <- NULL
            if (self$options$clinicalDecisionRule && !is.null(self$options$decisionRuleVar)) {
                rule_var <- self$options$decisionRuleVar
                rule_positive <- self$options$decisionRulePositive
            }

            # Get complete cases
            complete_vars <- c(outcome_var, model_vars)
            if (!is.null(rule_var)) {
                complete_vars <- c(complete_vars, rule_var)
            }
            complete_cases <- complete.cases(data[complete_vars])

            if (sum(complete_cases) < 10) {
                private$.addNotice(
                    type = "ERROR",
                    title = "Insufficient Cases",
                    content = sprintf(
                        'Insufficient complete cases for analysis (%d cases available, minimum 10 required). Decision curve analysis requires adequate sample size for stable net benefit estimates. Remove missing data or collect additional cases.',
                        sum(complete_cases)
                    )
                )
                private$.renderNotices()
                return()
            }

            # Filter data to complete cases
            analysis_data <- data[complete_cases, ]
            outcomes <- analysis_data[[outcome_var]]
            private$.analysisData <- analysis_data
            private$.analysisOutcomes <- outcomes

            # Clinical Profile Notices: Sample Size Adequacy
            n_total <- sum(complete_cases)
            if (n_total < 100) {
                notice_type <- if (n_total < 50) "STRONG_WARNING" else "WARNING"
                severity <- if (n_total < 50) "very small" else "small"

                private$.addNotice(
                    type = notice_type,
                    title = "Small Sample Size",
                    content = sprintf(
                        '%s sample size (n=%d). Confidence intervals may be wide and net benefit estimates unstable. Minimum recommended: n=100-200 for adequate precision in decision curve analysis. Bootstrap confidence intervals strongly recommended to assess uncertainty. Consider collecting additional data for reliable assessment.',
                        tools::toTitleCase(severity),
                        n_total
                    )
                )
            }

            # Check outcome is binary
            unique_outcomes <- unique(outcomes)
            if (length(unique_outcomes) != 2) {
                private$.addNotice(
                    type = "ERROR",
                    title = "Outcome Not Binary",
                    content = sprintf(
                        'Outcome variable must be binary (exactly 2 levels). Current outcome "%s" has %d levels: %s. Decision curve analysis requires a binary outcome (diseased vs healthy, event vs no event). Please recode to binary or select different outcome variable.',
                        outcome_var,
                        length(unique_outcomes),
                        paste(unique_outcomes, collapse = ", ")
                    )
                )
                private$.renderNotices()
                return()
            }

            # Validate positive outcome level
            if (!outcome_positive %in% unique_outcomes) {
                private$.addNotice(
                    type = "WARNING",
                    title = "Positive Outcome Level Not Found",
                    content = sprintf(
                        'Selected positive outcome level not found. Using first level "%s" instead. Available levels: %s. Please verify this is the correct positive outcome level.',
                        unique_outcomes[1],
                        paste(unique_outcomes, collapse = ", ")
                    )
                )
                outcome_positive <- unique_outcomes[1]
            }

            # Clinical Profile Notices: Extreme Prevalence
            n_diseased <- sum(outcomes == outcome_positive)
            prevalence <- n_diseased / n_total

            if (prevalence < 0.05 || prevalence > 0.95) {
                private$.addNotice(
                    type = "STRONG_WARNING",
                    title = "Extreme Prevalence",
                    content = sprintf(
                        'Extreme outcome prevalence: %.1f%% (%d/%d cases). Decision curves may be less interpretable with very low or very high event rates. Net benefit calculations are sensitive to prevalence extremes. Consider whether sample represents target clinical population. Results may not generalize to populations with different event rates.',
                        prevalence * 100,
                        n_diseased,
                        n_total
                    )
                )
            }

            # Validate clinical decision rule variable if provided
            rule_data <- NULL
            if (!is.null(rule_var)) {
                rule_data <- analysis_data[[rule_var]]
                rule_levels <- unique(rule_data)
                if (length(rule_levels) != 2) {
                    private$.addNotice(
                        type = "ERROR",
                        title = "Rule Not Binary",
                        content = sprintf(
                            'Clinical decision rule variable must be binary (exactly 2 levels). Current rule variable "%s" has %d levels: %s. Please select a binary rule variable or disable "Clinical Decision Rule Integration".',
                            rule_var,
                            length(rule_levels),
                            paste(rule_levels, collapse = ", ")
                        )
                    )
                    private$.renderNotices()
                    return()
                }
                if (is.null(rule_positive) || !(rule_positive %in% rule_levels)) {
                    private$.addNotice(
                        type = "WARNING",
                        title = "Rule Positive Level Not Found",
                        content = sprintf(
                            'Selected positive rule level not found. Using first level "%s" instead. Available levels: %s.',
                            rule_levels[1],
                            paste(rule_levels, collapse = ", ")
                        )
                    )
                    rule_positive <- rule_levels[1]
                }
            }

            # Generate threshold sequence
            thresholds <- private$.generateThresholds()

            # Performance monitoring for large analyses
            n_calculations <- length(model_vars) * length(thresholds)
            if (n_calculations >= private$DECISIONCURVE_DEFAULTS$performance_threshold_count) {
                message(sprintf("Decision curve analysis: Processing %d models × %d thresholds (%d total calculations)...", 
                               length(model_vars), length(thresholds), n_calculations))
            }

            # Initialize results storage
            dca_results <- list()
            plot_data <- data.frame()

            # Calculate decision curves for each model
            for (i in seq_along(model_vars)) {
                model_var <- model_vars[i]
                model_name <- model_names[i]
                predictions <- analysis_data[[model_var]]

                # Progress reporting for multiple models
                if (length(model_vars) > 3) {
                    message(sprintf("Processing model %d/%d: %s", i, length(model_vars), model_name))
                }

                # CRITICAL: Validate predictions are CALIBRATED probabilities between 0 and 1
                # DO NOT auto-scale: linear transformation destroys probability interpretation
                pred_min <- min(predictions, na.rm = TRUE)
                pred_max <- max(predictions, na.rm = TRUE)

                if (pred_min < 0 || pred_max > 1) {
                    private$.addNotice(
                        type = "ERROR",
                        title = sprintf('Invalid Probabilities: %s', model_name),
                        content = sprintf(
                            'Model "%s" contains values outside [0,1] range (min=%.3f, max=%.3f). Decision curve analysis requires CALIBRATED PROBABILITIES, not raw scores. If using logistic regression: Use predicted probabilities (predict(model, type="response")), not logits. If using risk scores: Calibrate to probabilities first (e.g., via logistic calibration). Why this matters: Threshold probability must have clinical meaning. Min-max scaling would make thresholds uninterpretable.',
                            model_name,
                            pred_min,
                            pred_max
                        )
                    )
                    private$.renderNotices()
                    return()
                }

                # Warn if probabilities are suspiciously concentrated
                if (pred_max - pred_min < 0.05) {
                    private$.addNotice(
                        type = "STRONG_WARNING",
                        title = sprintf('Narrow Probability Range: %s', model_name),
                        content = sprintf(
                            'Model "%s" has very narrow probability range (%.3f to %.3f). Decision curve analysis may not be informative with such limited variation (range < 5%%). Consider checking model calibration or discrimination. Models with poor discrimination may not show clinical utility.',
                            model_name,
                            pred_min,
                            pred_max
                        )
                    )
                }

                # Optimized threshold calculations - vectorize when possible
                net_benefits <- private$.calculateNetBenefitsVectorized(
                    predictions, outcomes, thresholds, outcome_positive
                )
                
                # Detailed results for specific calculations (fallback to individual calculations)
                detailed_results <- list()
                for (j in seq_along(thresholds)) {
                    thresh <- thresholds[j]
                    detailed_results[[j]] <- private$.calculateNetBenefit(
                        predictions, outcomes, thresh, outcome_positive
                    )
                }

                # Store results
                dca_results[[model_name]] <- list(
                    net_benefits = net_benefits,
                    detailed_results = detailed_results,
                    thresholds = thresholds
                )

                # Add to plot data
                model_plot_data <- data.frame(
                    threshold = thresholds,
                    net_benefit = net_benefits,
                    model = model_name,
                    stringsAsFactors = FALSE
                )

                # Add confidence intervals if requested
                if (self$options$confidenceIntervals || self$options$showNetBenefitCI) {
                    ci_results <- private$.calculateBootstrapCI(
                        predictions, outcomes, thresholds, outcome_positive,
                        self$options$bootReps
                    )
                    model_plot_data$ci_lower <- ci_results$lower
                    model_plot_data$ci_upper <- ci_results$upper
                }

                plot_data <- rbind(plot_data, model_plot_data)
            }

            # Calculate net benefit for treat all strategy
            treat_all_nb <- numeric(length(thresholds))
            treat_none_nb <- numeric(length(thresholds))

            for (j in seq_along(thresholds)) {
                treat_all_nb[j] <- private$.calculateTreatAllNetBenefit(
                    outcomes, thresholds[j], outcome_positive
                )
                treat_none_nb[j] <- private$.calculateTreatNoneNetBenefit()
            }

            # Add reference strategies to plot data
            ref_data <- rbind(
                data.frame(
                    threshold = thresholds,
                    net_benefit = treat_all_nb,
                    model = "Treat All",
                    stringsAsFactors = FALSE
                ),
                data.frame(
                    threshold = thresholds,
                    net_benefit = treat_none_nb,
                    model = "Treat None",
                    stringsAsFactors = FALSE
                )
            )

            plot_data <- rbind(plot_data, ref_data)
            
            # Add clinical decision rule if requested and provided
            if (self$options$clinicalDecisionRule && !is.null(rule_data)) {
                rule_label <- self$options$decisionRuleLabel
                if (rule_label == "") {
                    rule_label <- paste0("Clinical Rule (", rule_positive, ")")
                }

                # Convert rule to numeric prediction (1 = intervene/recommend)
                rule_pred <- as.numeric(rule_data == rule_positive)

                # Net benefit across thresholds
                rule_net <- private$.calculateNetBenefitsVectorized(rule_pred, outcomes, thresholds, outcome_positive)

                rule_detailed <- lapply(thresholds, function(thresh) {
                    private$.calculateNetBenefit(rule_pred, outcomes, thresh, outcome_positive)
                })

                dca_results[[rule_label]] <- list(
                    net_benefits = rule_net,
                    detailed_results = rule_detailed,
                    thresholds = thresholds,
                    is_rule = TRUE
                )

                plot_data <- rbind(
                    plot_data,
                    data.frame(
                        threshold = thresholds,
                        net_benefit = rule_net,
                        model = rule_label,
                        stringsAsFactors = FALSE
                    )
                )
            }

            # Store results for plotting
            private$.dcaResults <- dca_results
            private$.plotData <- plot_data

            # Create procedure notes
            procedure_notes <- paste0(
                "<html><body>",
                "<h4>Decision Curve Analysis Summary</h4>",
                "<p><strong>Outcome Variable:</strong> ", outcome_var, " (", outcome_positive, " = positive)</p>",
                "<p><strong>Models Analyzed:</strong> ", paste(model_names, collapse = ", "), "</p>",
                "<p><strong>Sample Size:</strong> ", sum(complete_cases), " complete cases</p>",
                "<p><strong>Prevalence:</strong> ", round(mean(outcomes == outcome_positive) * 100, 1), "%</p>",
                "<p><strong>Threshold Range:</strong> ", round(min(thresholds) * 100, 1), "% to ",
                round(max(thresholds) * 100, 1), "%</p>",
                "</body></html>"
            )

            self$results$procedureNotes$setContent(procedure_notes)

            # Populate results table
            if (self$options$showTable) {
                private$.populateResultsTable(treat_all_nb, treat_none_nb)
            }

            # Populate optimal thresholds table
            if (self$options$showOptimalThreshold) {
                private$.populateOptimalTable()
            }

            # Calculate clinical impact if requested
            if (self$options$calculateClinicalImpact) {
                private$.calculateClinicalImpactMetrics(outcomes, outcome_positive)
            }

            # Calculate weighted AUC if requested
            if (self$options$weightedAUC) {
                private$.populateWeightedAUCTable()
            }

            # Model comparison if requested
            if (self$options$compareModels && length(model_vars) > 1) {
                private$.performModelComparison()
            }
            
            # Enhanced Analysis Options
            
            # Cost-Benefit Analysis
            if (self$options$costBenefitAnalysis) {
                private$.populateCostBenefitTable()
            }
            
            # Decision Consequences
            if (self$options$showDecisionConsequences) {
                private$.populateDecisionConsequencesTable()
            }
            
            # Resource Utilization
            if (self$options$resourceUtilization) {
                private$.populateResourceUtilizationTable()
            }
            
            # Enhanced Model Comparison
            if (self$options$multiModelComparison && length(model_vars) > 1) {
                private$.performEnhancedModelComparison()
            }

            # Generate clinical interpretation
            private$.generateClinicalInterpretation()

            # Success Completion Notice
            n_models <- length(model_names)
            n_cases <- sum(complete_cases)
            n_diseased_final <- sum(outcomes == outcome_positive)
            n_healthy_final <- n_cases - n_diseased_final
            threshold_min <- min(thresholds) * 100
            threshold_max <- max(thresholds) * 100

            private$.addNotice(
                type = "INFO",
                title = "Analysis Complete",
                content = sprintf(
                    'Decision curve analysis completed successfully. %d model(s) evaluated using %d complete cases. Outcome prevalence: %.1f%% (%d/%d). Threshold range: %.1f%% to %.1f%%. Review decision curves and optimal thresholds below.',
                    n_models,
                    n_cases,
                    prevalence * 100,
                    n_diseased_final,
                    n_cases,
                    threshold_min,
                    threshold_max
                )
            )

            # Render all collected notices as HTML
            private$.renderNotices()
        },

        .populateResultsTable = function(treat_all_nb, treat_none_nb) {
            selected_thresholds <- private$.parseSelectedThresholds()
            results_table <- self$results$resultsTable

            # Clear existing rows
            results_table$deleteRows()

            # Add columns for each model dynamically
            model_names <- names(private$.dcaResults)

            for (model_name in model_names) {
                results_table$addColumn(
                    name = paste0("model_", gsub("[^A-Za-z0-9]", "_", model_name)),
                    title = model_name,
                    type = "number",
                    format = "zto"
                )
            }

            # Populate table
            for (i in seq_along(selected_thresholds)) {
                thresh <- selected_thresholds[i]

                # Find closest threshold in our analysis
                closest_idx <- which.min(abs(private$.dcaResults[[1]]$thresholds - thresh))
                actual_thresh <- private$.dcaResults[[1]]$thresholds[closest_idx]

                # Create row values
                row_values <- list(
                    threshold = thresh,
                    treat_all = private$.calculateTreatAllNetBenefit(
                        private$.analysisOutcomes, thresh, self$options$outcomePositive
                    ),
                    treat_none = 0
                )
                
                # Add model values
                for (model_name in model_names) {
                    # Find net benefit for this model at this threshold
                    nb_idx <- which.min(abs(private$.dcaResults[[model_name]]$thresholds - thresh))
                    nb <- private$.dcaResults[[model_name]]$net_benefits[nb_idx]
                    
                    col_name <- paste0("model_", gsub("[^A-Za-z0-9]", "_", model_name))
                    row_values[[col_name]] <- nb
                }
                
                results_table$addRow(rowKey = paste0("thresh_", i), values = row_values)
            }
        },

        .populateCostBenefitTable = function() {
            table <- self$results$costBenefitTable
            selected_thresholds <- private$.parseSelectedThresholds()
            model_names <- names(private$.dcaResults)
            
            # Costs and benefits
            test_cost <- self$options$testCost
            treat_cost <- self$options$treatmentCost
            benefit_tp <- self$options$benefitCorrectTreatment
            harm_fp <- self$options$harmFalseTreatment
            analysis_data <- private$.analysisData
            outcomes <- private$.analysisOutcomes
            pop_size <- self$options$populationSize
            n_analysis <- length(outcomes)
            scale_factor <- if (!is.null(pop_size) && !is.na(pop_size) && pop_size > 0) pop_size / n_analysis else 1
            
            # Calculate for each model at each threshold
            for (model_name in model_names) {
                model_results <- private$.dcaResults[[model_name]]
                model_idx <- which(private$.parseModelNames() == model_name)
                if (length(model_idx) == 0) next
                model_var <- self$options$models[model_idx]
                
                for (i in seq_along(selected_thresholds)) {
                    thresh <- selected_thresholds[i]
                    
                    res <- private$.calculateNetBenefit(
                        analysis_data[[model_var]], 
                        outcomes, 
                        thresh, 
                        self$options$outcomePositive
                    )
                    
                    tp_scaled <- res$tp * scale_factor
                    fp_scaled <- res$fp * scale_factor
                    tn_scaled <- res$tn * scale_factor
                    fn_scaled <- res$fn * scale_factor
                    n_scaled <- tp_scaled + fp_scaled + tn_scaled + fn_scaled
                    
                    # Calculate costs and benefits
                    # Total Cost = (Tests * Test Cost) + (TP + FP) * Treatment Cost
                    # Everyone is assumed tested once.
                    total_cost <- (n_scaled * test_cost) + ((tp_scaled + fp_scaled) * treat_cost)
                    
                    # Total Benefit = (TP * Benefit) - (FP * Harm)
                    total_benefit <- (tp_scaled * benefit_tp) - (fp_scaled * harm_fp)
                    
                    # Net Monetary Benefit
                    nmb <- total_benefit - total_cost
                    
                    # Incremental values (vs Treat All)
                    # Treat All: Everyone treated, no test cost (if no test needed) or test cost?
                    # Usually "Treat All" means treat everyone without testing.
                    prevalence <- res$prevalence
                    tp_all <- prevalence * n_scaled
                    fp_all <- (1 - prevalence) * n_scaled
                    
                    cost_all <- n_scaled * treat_cost # Assuming no test cost for Treat All
                    benefit_all <- (tp_all * benefit_tp) - (fp_all * harm_fp)
                    nmb_all <- benefit_all - cost_all
                    
                    inc_cost <- total_cost - cost_all
                    inc_benefit <- total_benefit - benefit_all
                    
                    # ICER = Incremental Cost / Incremental Benefit (in units of outcome?)
                    # Here we have monetary benefit, so ICER might be different.
                    # Let's just output the values.
                    
                    table$addRow(rowKey = paste0(model_name, "_", i), values = list(
                        model = model_name,
                        threshold = thresh,
                        total_cost = total_cost,
                        total_benefit = total_benefit,
                        net_monetary_benefit = nmb,
                        incremental_cost = inc_cost,
                        incremental_benefit = inc_benefit,
                        icer = if (abs(inc_benefit) > 1e-6) inc_cost / inc_benefit else NA
                    ))
                }
            }
        },

        .populateDecisionConsequencesTable = function() {
            table <- self$results$decisionConsequencesTable
            selected_thresholds <- private$.parseSelectedThresholds()
            model_names <- names(private$.dcaResults)
            analysis_data <- private$.analysisData
            outcomes <- private$.analysisOutcomes
            
            for (model_name in model_names) {
                # Get the variable name corresponding to the model name
                # This logic assumes model_names and self$options$models are aligned
                model_idx <- which(private$.parseModelNames() == model_name)
                if (length(model_idx) == 0) next
                model_var <- self$options$models[model_idx]
                
                for (i in seq_along(selected_thresholds)) {
                    thresh <- selected_thresholds[i]
                    
                    res <- private$.calculateNetBenefit(
                        analysis_data[[model_var]], 
                        outcomes, 
                        thresh, 
                        self$options$outcomePositive
                    )
                    
                    # Calculate PPV/NPV
                    ppv <- if ((res$tp + res$fp) > 0) res$tp / (res$tp + res$fp) else NA
                    npv <- if ((res$tn + res$fn) > 0) res$tn / (res$tn + res$fn) else NA
                    
                    table$addRow(rowKey = paste0(model_name, "_", i), values = list(
                        model = model_name,
                        threshold = thresh,
                        true_positive = res$tp,
                        false_positive = res$fp,
                        true_negative = res$tn,
                        false_negative = res$fn,
                        sensitivity = res$sensitivity,
                        specificity = res$specificity,
                        ppv = ppv,
                        npv = npv
                    ))
                }
            }
        },

        .populateResourceUtilizationTable = function() {
            table <- self$results$resourceUtilizationTable
            selected_thresholds <- private$.parseSelectedThresholds()
            model_names <- names(private$.dcaResults)
            analysis_data <- private$.analysisData
            outcomes <- private$.analysisOutcomes
            
            for (model_name in model_names) {
                model_idx <- which(private$.parseModelNames() == model_name)
                if (length(model_idx) == 0) next
                model_var <- self$options$models[model_idx]
                
                for (i in seq_along(selected_thresholds)) {
                    thresh <- selected_thresholds[i]
                    
                    res <- private$.calculateNetBenefit(
                        analysis_data[[model_var]], 
                        outcomes, 
                        thresh, 
                        self$options$outcomePositive
                    )
                    
                    n_total <- res$tp + res$fp + res$tn + res$fn
                    
                    # Per 1000 patients
                    scale_factor <- 1000 / n_total
                    
                    tests_per_1000 <- n_total * scale_factor # Everyone tested
                    treatments_per_1000 <- (res$tp + res$fp) * scale_factor
                    unnecessary_treatments <- res$fp * scale_factor
                    missed_cases <- res$fn * scale_factor
                    
                    # Reduction vs Treat All
                    # Treat All: All treated
                    treatments_all <- n_total * scale_factor
                    reduction <- (treatments_all - treatments_per_1000) / treatments_all
                    
                    table$addRow(rowKey = paste0(model_name, "_", i), values = list(
                        model = model_name,
                        threshold = thresh,
                        tests_per_1000 = tests_per_1000,
                        treatments_per_1000 = treatments_per_1000,
                        unnecessary_treatments = unnecessary_treatments,
                        missed_cases = missed_cases,
                        reduction_vs_treat_all = reduction
                    ))
                }
            }
        },
        
        .performEnhancedModelComparison = function() {
            table <- self$results$modelComparisonEnhanced
            model_names <- names(private$.dcaResults)
            
            if (length(model_names) < 2) return()
            
            # Pairwise comparisons
            pairs <- combn(model_names, 2, simplify = FALSE)
            
            for (pair in pairs) {
                m1 <- pair[1]
                m2 <- pair[2]
                
                # Calculate differences in Net Benefit across range
                # Use the calculated thresholds from dcaResults
                # Assuming same thresholds for both (which is true as we generate one sequence)
                
                nb1 <- private$.dcaResults[[m1]]$net_benefits
                nb2 <- private$.dcaResults[[m2]]$net_benefits
                
                diff <- nb1 - nb2
                mean_diff <- mean(diff, na.rm = TRUE)
                median_diff <- median(diff, na.rm = TRUE)
                
                # Statistical test (Bootstrap)
                # This is computationally expensive, so we'll do a simple version
                # Resample data, calculate NB diff, get CI and p-value
                
                p_value <- NA
                conclusion <- "Inconclusive"
                
                if (self$options$comparisonMethod == "bootstrap") {
                    # Simple bootstrap for NB difference
                    # Note: This should ideally be done during the main bootstrap loop for efficiency
                    # But for now, we'll do a quick check if bootstrap results are available
                    
                    # If we didn't run bootstrap CIs, we can't easily get p-values without re-running
                    # For now, we'll just report the differences
                    conclusion <- "Bootstrap required"
                }
                
                table$addRow(rowKey = paste0(m1, "_vs_", m2), values = list(
                    model1 = m1,
                    model2 = m2,
                    nb_difference_mean = mean_diff,
                    nb_difference_median = median_diff,
                    test_statistic = NA,
                    p_value = NA,
                    conclusion = conclusion
                ))
            }
        },

        .populateOptimalTable = function() {
            optimal_table <- self$results$optimalTable
            optimal_table$deleteRows()

            model_names <- names(private$.dcaResults)

            for (i in seq_along(model_names)) {
                model_name <- model_names[i]
                model_results <- private$.dcaResults[[model_name]]

                optimal_info <- private$.findOptimalThreshold(
                    model_results$net_benefits,
                    model_results$thresholds
                )

                optimal_table$addRow(rowKey = i, values = list(
                    model = model_name,
                    optimal_threshold = optimal_info$optimal_threshold,
                    max_net_benefit = optimal_info$max_net_benefit,
                    threshold_range_start = optimal_info$range_start,
                    threshold_range_end = optimal_info$range_end
                ))
            }
        },

        .calculateClinicalImpactMetrics = function(outcomes, outcome_positive) {
            clinical_impact_table <- self$results$clinicalImpactTable
            clinical_impact_table$deleteRows()

            selected_thresholds <- private$.parseSelectedThresholds()
            model_names <- names(private$.dcaResults)
            pop_size <- self$options$populationSize

            # Calculate for each model at each selected threshold
            row_counter <- 1
            for (model_name in model_names) {
                model_results <- private$.dcaResults[[model_name]]

                for (thresh in selected_thresholds) {
                    # Find closest threshold
                    closest_idx <- which.min(abs(model_results$thresholds - thresh))
                    detailed_result <- model_results$detailed_results[[closest_idx]]

                    # Calculate interventions avoided compared to treat all
                    treat_all_interventions <- pop_size  # Treat all = 100% get intervention
                    model_interventions <- detailed_result$interventions_per_100 * pop_size / 100
                    interventions_avoided <- treat_all_interventions - model_interventions

                    # Number needed to screen (simplified calculation)
                    if (detailed_result$true_positives_per_100 > 0) {
                        nns <- 100 / detailed_result$true_positives_per_100
                    } else {
                        nns <- Inf
                    }

                    clinical_impact_table$addRow(rowKey = row_counter, values = list(
                        model = model_name,
                        threshold = thresh,
                        interventions_per_100 = detailed_result$interventions_per_100,
                        true_positives_per_100 = detailed_result$true_positives_per_100,
                        false_positives_per_100 = detailed_result$false_positives_per_100,
                        interventions_avoided = interventions_avoided,
                        number_needed_to_screen = if(is.finite(nns)) nns else NA
                    ))

                    row_counter <- row_counter + 1
                }
            }
        },

        .populateWeightedAUCTable = function() {
            weighted_auc_table <- self$results$weightedAUCTable
            weighted_auc_table$deleteRows()

            model_names <- names(private$.dcaResults)
            thresholds <- private$.dcaResults[[1]]$thresholds

            # CRITICAL FIX: Calculate treat all weighted AUC on SAME COHORT as models
            # Using self$data would include cases with missing predictors, creating biased baseline
            outcome_var <- self$options$outcome
            model_vars <- self$options$models
            complete_vars <- c(outcome_var, model_vars)
            complete_cases <- complete.cases(self$data[complete_vars])
            analysis_data <- self$data[complete_cases, ]

            outcomes <- analysis_data[[outcome_var]]
            outcome_positive <- self$options$outcomePositive

            treat_all_nb <- numeric(length(thresholds))
            for (j in seq_along(thresholds)) {
                treat_all_nb[j] <- private$.calculateTreatAllNetBenefit(
                    outcomes, thresholds[j], outcome_positive
                )
            }
            treat_all_wauc <- private$.calculateWeightedAUC(treat_all_nb, thresholds)

            for (i in seq_along(model_names)) {
                model_name <- model_names[i]
                model_results <- private$.dcaResults[[model_name]]

                # Calculate weighted AUC
                wauc <- private$.calculateWeightedAUC(
                    model_results$net_benefits,
                    model_results$thresholds
                )

                # Calculate relative benefit vs treat all
                if (!is.na(wauc) && !is.na(treat_all_wauc) && treat_all_wauc != 0) {
                    relative_benefit <- (wauc - treat_all_wauc) / abs(treat_all_wauc)
                } else {
                    relative_benefit <- NA
                }

                weighted_auc_table$addRow(rowKey = i, values = list(
                    model = model_name,
                    weighted_auc = wauc,
                    auc_range = paste0(round(min(thresholds) * 100, 1), "% - ",
                                       round(max(thresholds) * 100, 1), "%"),
                    relative_benefit = relative_benefit
                ))
            }
        },

        .performModelComparison = function() {
            # This would implement statistical tests for comparing models
            # For now, implement a simple comparison based on weighted AUC
            comparison_table <- self$results$comparisonTable
            comparison_table$deleteRows()

            model_names <- names(private$.dcaResults)

            # Compare each pair of models
            row_counter <- 1
            for (i in 1:(length(model_names) - 1)) {
                for (j in (i + 1):length(model_names)) {
                    model1 <- model_names[i]
                    model2 <- model_names[j]

                    # Calculate weighted AUC difference
                    wauc1 <- private$.calculateWeightedAUC(
                        private$.dcaResults[[model1]]$net_benefits,
                        private$.dcaResults[[model1]]$thresholds
                    )
                    wauc2 <- private$.calculateWeightedAUC(
                        private$.dcaResults[[model2]]$net_benefits,
                        private$.dcaResults[[model2]]$thresholds
                    )

                    wauc_diff <- wauc1 - wauc2

                    # For now, set placeholder values for CI and p-value
                    # In a full implementation, these would come from bootstrap testing
                    comparison_table$addRow(rowKey = row_counter, values = list(
                        comparison = paste(model1, "vs", model2),
                        weighted_auc_diff = wauc_diff,
                        ci_lower = NA,  # Would implement bootstrap CI
                        ci_upper = NA,  # Would implement bootstrap CI
                        p_value = NA    # Would implement statistical test
                    ))

                    row_counter <- row_counter + 1
                }
            }
        },

        .generateClinicalInterpretation = function() {
            model_names <- names(private$.dcaResults)

            # Find the best performing model (highest weighted AUC)
            best_wauc <- -Inf
            best_model <- NULL

            for (model_name in model_names) {
                wauc <- private$.calculateWeightedAUC(
                    private$.dcaResults[[model_name]]$net_benefits,
                    private$.dcaResults[[model_name]]$thresholds
                )
                if (!is.na(wauc) && wauc > best_wauc) {
                    best_wauc <- wauc
                    best_model <- model_name
                }
            }

            # Generate interpretation text
            interpretation <- paste0(
                "<html><body>",
                "<h4>Clinical Interpretation</h4>"
            )

            if (!is.null(best_model)) {
                interpretation <- paste0(
                    interpretation,
                    "<p><strong>Best Performing Model:</strong> ", best_model, "</p>"
                )

                # Get optimal threshold for best model
                best_results <- private$.dcaResults[[best_model]]
                optimal_info <- private$.findOptimalThreshold(
                    best_results$net_benefits,
                    best_results$thresholds
                )

                if (!is.na(optimal_info$optimal_threshold)) {
                    interpretation <- paste0(
                        interpretation,
                        "<p><strong>Optimal Threshold:</strong> ",
                        round(optimal_info$optimal_threshold * 100, 1),
                        "% (Net Benefit = ", round(optimal_info$max_net_benefit, 3), ")</p>"
                    )
                }

                if (!is.na(optimal_info$range_start) && !is.na(optimal_info$range_end)) {
                    interpretation <- paste0(
                        interpretation,
                        "<p><strong>Beneficial Range:</strong> ",
                        round(optimal_info$range_start * 100, 1), "% to ",
                        round(optimal_info$range_end * 100, 1), "%</p>"
                    )
                }
            }

            interpretation <- paste0(
                interpretation,
                "<p><strong>Interpretation Guidelines:</strong></p>",
                "<ul>",
                "<li>Models above both reference lines provide clinical benefit</li>",
                "<li>Higher net benefit indicates greater clinical utility</li>",
                "<li>Consider the threshold range relevant to your clinical context</li>",
                "<li>Net benefit can be interpreted as additional true positives per 100 patients screened</li>",
                "</ul>",
                private$.generateMethodologicalFootnotes(),
                "</body></html>"
            )

            self$results$summaryText$setContent(interpretation)
        },
        
        # Generate methodological footnotes for enhanced clinical understanding
        .generateMethodologicalFootnotes = function() {
            footnotes <- "<div style='margin-top: 20px; font-size: 0.9em; color: #666;'>"
            footnotes <- paste0(footnotes, "<p><strong>Methodological Notes:</strong></p>")
            footnotes <- paste0(footnotes, "<ul style='font-size: 0.85em;'>")
            
            # Net benefit formula explanation
            footnotes <- paste0(footnotes, 
                "<li><strong>Net Benefit Formula:</strong> NB = (TP/n) - (FP/n) × [pt/(1-pt)], where pt is threshold probability</li>")
            
            # Reference strategies explanation
            footnotes <- paste0(footnotes,
                "<li><strong>Reference Strategies:</strong> 'Treat All' assumes all patients receive intervention; 'Treat None' assumes no intervention</li>")
            
            # Threshold interpretation
            footnotes <- paste0(footnotes,
                "<li><strong>Threshold Probability:</strong> The minimum probability at which a patient would choose intervention over no intervention</li>")
            
            # Bootstrap CI note if applicable
            if (self$options$confidenceIntervals) {
                footnotes <- paste0(footnotes,
                    "<li><strong>Confidence Intervals:</strong> Bootstrap ", self$options$bootReps, 
                    " replications with ", (self$options$ciLevel * 100), "% confidence level</li>")
            }
            
            # Clinical impact note if applicable
            if (self$options$calculateClinicalImpact) {
                footnotes <- paste0(footnotes,
                    "<li><strong>Clinical Impact:</strong> Calculated for population size of ", 
                    self$options$populationSize, " patients</li>")
            }
            
            # Clinical decision rule note if applicable
            if (self$options$clinicalDecisionRule && !is.null(self$options$decisionRuleVar)) {
                footnotes <- paste0(footnotes,
                    "<li><strong>Clinical Decision Rule:</strong> Applied as provided in the data (", 
                    self$options$decisionRuleLabel, ")</li>")
            }
            
            footnotes <- paste0(footnotes, "</ul></div>")
            
            return(footnotes)
        },
        
        # Optimize plot data for many models to improve performance and readability
        .optimizePlotDataForManyModels = function(plot_data, n_models) {
            # Strategies for handling many models:
            # 1. Reduce line thickness
            # 2. Sample data points for smoother rendering
            # 3. Consider highlighting top-performing models
            
            # Sample data points if there are many thresholds
            n_thresholds_per_model <- nrow(plot_data) / n_models
            if (n_thresholds_per_model > 100) {
                # Sample every nth point to reduce rendering load
                sample_rate <- ceiling(n_thresholds_per_model / 50)  # Target ~50 points per model
                
                optimized_data <- data.frame()
                for (model in unique(plot_data$model)) {
                    model_data <- plot_data[plot_data$model == model, ]
                    model_data <- model_data[seq(1, nrow(model_data), by = sample_rate), ]
                    optimized_data <- rbind(optimized_data, model_data)
                }
                
                message(sprintf("Reduced data points from %d to %d for faster rendering", 
                               nrow(plot_data), nrow(optimized_data)))
                
                return(optimized_data)
            }
            
            return(plot_data)
        },
        
        # Optimized plotting functions with performance enhancements for many models
        .plotDCA = function(image, ggtheme, theme, ...) {
            if (is.null(private$.plotData) || nrow(private$.plotData) == 0) {
                return(FALSE)
            }

            plot_data <- private$.plotData
            
            # Performance optimization for many models
            n_models <- length(unique(plot_data$model))
            max_models_threshold <- private$DECISIONCURVE_DEFAULTS$max_models_full_plot
            
            if (n_models > max_models_threshold) {
                plot_data <- private$.optimizePlotDataForManyModels(plot_data, n_models)
                message(sprintf("Plot optimized for %d models: Using performance enhancements", n_models))
            }

            # Create base plot with optimized aesthetics
            p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = threshold, y = net_benefit, color = model)) +
                ggplot2::geom_line(size = if(n_models > max_models_threshold) 0.8 else 1) +
                ggplot2::labs(
                    title = "Decision Curve Analysis",
                    x = "Threshold Probability",
                    y = "Net Benefit",
                    color = "Strategy"
                ) +
                ggplot2::scale_x_continuous(labels = function(x) paste0(round(x * 100), "%")) +
                ggtheme

            # Add confidence intervals if calculated and display requested
            if (self$options$showNetBenefitCI && "ci_lower" %in% names(plot_data)) {
                model_data <- plot_data[!plot_data$model %in% c("Treat All", "Treat None"), ]
                if (nrow(model_data) > 0) {
                    p <- p + ggplot2::geom_ribbon(
                        data = model_data,
                        ggplot2::aes(ymin = ci_lower, ymax = ci_upper, fill = model),
                        alpha = 0.2, color = NA
                    )
                }
            }

            # Highlight clinical range if requested
            if (self$options$highlightRange) {
                p <- p + ggplot2::annotate(
                    "rect",
                    xmin = self$options$highlightMin,
                    xmax = self$options$highlightMax,
                    ymin = -Inf, ymax = Inf,
                    alpha = 0.1, fill = "yellow"
                )
            }

            # Optimize legend and colors for many models
            if (n_models > max_models_threshold) {
                # Use more efficient legend positioning and reduce legend size
                p <- p + ggplot2::theme(
                    legend.position = "bottom",
                    legend.text = ggplot2::element_text(size = 8),
                    legend.title = ggplot2::element_text(size = 9),
                    legend.key.size = ggplot2::unit(0.4, "cm")
                )
                
                # Consider using fewer distinct colors and rely more on line patterns
                if (n_models > 15) {
                    p <- p + ggplot2::guides(color = ggplot2::guide_legend(ncol = 3))
                }
            }

            # Style reference lines differently
            if (self$options$plotStyle == "standard" || self$options$plotStyle == "detailed") {
                # Make treat all/none lines dashed
                treat_lines <- plot_data[plot_data$model %in% c("Treat All", "Treat None"), ]
                if (nrow(treat_lines) > 0) {
                    p <- p + ggplot2::geom_line(
                        data = treat_lines,
                        linetype = "dashed", 
                        size = if(n_models > max_models_threshold) 0.6 else 0.8
                    )
                }
            }

            # Add annotations for detailed style
            if (self$options$plotStyle == "detailed") {
                # Add horizontal line at 0
                p <- p + ggplot2::geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.5)

                # Add labels if requested
                if (self$options$showReferenceLinesLabels) {
                    # This would add text annotations for reference lines
                }
            }

            print(p)
            return(TRUE)
        },

        .plotClinicalImpact = function(image, ggtheme, theme, ...) {
            if (is.null(private$.dcaResults) || (!self$options$calculateClinicalImpact && !self$options$showClinicalImpactPlot)) {
                return(FALSE)
            }

            # Get selected thresholds and models
            selected_thresholds <- private$.parseSelectedThresholds()
            model_names <- names(private$.dcaResults)
            pop_size <- self$options$populationSize

            # Prepare data for clinical impact plot
            impact_data <- data.frame()

            for (model_name in model_names) {
                model_results <- private$.dcaResults[[model_name]]

                for (thresh in selected_thresholds) {
                    # Find closest threshold
                    closest_idx <- which.min(abs(model_results$thresholds - thresh))
                    detailed_result <- model_results$detailed_results[[closest_idx]]

                    # Add to plot data
                    impact_data <- rbind(impact_data, data.frame(
                        threshold = thresh,
                        model = model_name,
                        interventions_per_100 = detailed_result$interventions_per_100,
                        true_positives_per_100 = detailed_result$true_positives_per_100,
                        false_positives_per_100 = detailed_result$false_positives_per_100,
                        stringsAsFactors = FALSE
                    ))
                }
            }

            if (nrow(impact_data) == 0) return(FALSE)

            # Reshape data for stacked bar chart
            library(tidyr)
            plot_data <- impact_data %>%
                tidyr::gather(key = "outcome_type", value = "count",
                              true_positives_per_100, false_positives_per_100) %>%
                dplyr::mutate(
                    outcome_type = factor(outcome_type,
                                          levels = c("true_positives_per_100", "false_positives_per_100"),
                                          labels = c("True Positives", "False Positives"))
                )

            # Create stacked bar chart showing clinical impact
            p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = factor(threshold), y = count, fill = outcome_type)) +
                ggplot2::geom_bar(stat = "identity", position = "stack") +
                ggplot2::facet_wrap(~ model, scales = "free_y") +
                ggplot2::labs(
                    title = "Clinical Impact: Interventions per 100 Patients",
                    x = "Threshold Probability",
                    y = "Patients per 100",
                    fill = "Outcome Type"
                ) +
                ggplot2::scale_x_discrete(labels = function(x) paste0(as.numeric(x) * 100, "%")) +
                ggplot2::scale_fill_manual(values = c("True Positives" = "darkgreen", "False Positives" = "darkred")) +
                ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
                ggtheme

            print(p)
            return(TRUE)
        },

        .plotInterventionsAvoided = function(image, ggtheme, theme, ...) {
            # Set state for plot rendering
            plotState <- list(
                dcaResults = private$.dcaResults,
                thresholds = if (!is.null(private$.dcaResults)) private$.dcaResults[[1]]$thresholds else NULL
            )
            image$setState(plotState)

            if (is.null(private$.dcaResults)) {
                return(FALSE)
            }

            # Calculate interventions avoided compared to "treat all" strategy
            thresholds <- private$.dcaResults[[1]]$thresholds
            model_names <- names(private$.dcaResults)

            # Prepare data
            avoided_data <- data.frame()

            for (model_name in model_names) {
                model_results <- private$.dcaResults[[model_name]]
                interventions_avoided <- numeric(length(thresholds))

                for (j in seq_along(thresholds)) {
                    detailed_result <- model_results$detailed_results[[j]]
                    # Treat all = 100% get intervention, model = actual intervention rate
                    interventions_avoided[j] <- 100 - detailed_result$interventions_per_100
                }

                avoided_data <- rbind(avoided_data, data.frame(
                    threshold = thresholds,
                    interventions_avoided = interventions_avoided,
                    model = model_name,
                    stringsAsFactors = FALSE
                ))
            }

            if (nrow(avoided_data) == 0) return(FALSE)

            # Create line plot showing interventions avoided
            p <- ggplot2::ggplot(avoided_data, ggplot2::aes(x = threshold, y = interventions_avoided, color = model)) +
                ggplot2::geom_line(size = 1) +
                ggplot2::geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
                ggplot2::labs(
                    title = "Interventions Avoided vs. Treat All Strategy",
                    subtitle = "Number of unnecessary interventions prevented per 100 patients",
                    x = "Threshold Probability",
                    y = "Interventions Avoided per 100 Patients",
                    color = "Model"
                ) +
                ggplot2::scale_x_continuous(labels = function(x) paste0(round(x * 100), "%")) +
                ggplot2::ylim(0, 100) +
                ggtheme

            # Add annotation explaining the interpretation
            p <- p + ggplot2::annotate(
                "text",
                x = max(thresholds) * 0.7,
                y = max(avoided_data$interventions_avoided, na.rm = TRUE) * 0.9,
                label = "Higher values = more\nunnecessary treatments avoided",
                hjust = 0.5,
                alpha = 0.7,
                size = 3
            )

            print(p)
            return(TRUE)
        },

        .plotRelativeUtility = function(image, ggtheme, theme, ...) {
            # Set state for plot rendering
            plotState <- list(
                plotData = private$.plotData,
                analysisOutcomes = private$.analysisOutcomes,
                outcomePositive = self$options$outcomePositive
            )
            image$setState(plotState)

            if (is.null(private$.dcaResults)) return(FALSE)

            plot_data <- private$.plotData

            # Calculate Relative Utility
            # RU = (NB_model - NB_all) / (NB_perfect - NB_all)
            
            prevalence <- mean(private$.analysisOutcomes == self$options$outcomePositive, na.rm=TRUE)
            
            plot_data$relative_utility <- NA
            
            for (i in 1:nrow(plot_data)) {
                thresh <- plot_data$threshold[i]
                nb <- plot_data$net_benefit[i]
                
                # NB_perfect (Sensitivity=1, Specificity=1)
                nb_perfect <- prevalence
                
                # NB_all
                nb_all <- prevalence - (1 - prevalence) * (thresh / (1 - thresh))
                
                denom <- nb_perfect - nb_all
                
                if (abs(denom) > 1e-6) {
                    ru <- (nb - nb_all) / denom
                } else {
                    ru <- 0
                }
                
                plot_data$relative_utility[i] <- ru
            }
            
            # Filter for reasonable range
            plot_data <- plot_data[plot_data$relative_utility > -0.5 & plot_data$relative_utility <= 1.1, ]
            
            plot <- ggplot(plot_data, aes(x = threshold, y = relative_utility, color = model)) +
                geom_line(size = 1) +
                scale_color_brewer(palette = "Set1") +
                labs(title = "Relative Utility Curve",
                     x = "Threshold Probability",
                     y = "Relative Utility (vs Treat All)",
                     color = "Model") +
                theme_minimal() +
                ggtheme +
                ylim(-0.1, 1.0)
            
            print(plot)
            return(TRUE)
        },
        
        .plotStandardizedNetBenefit = function(image, ggtheme, theme, ...) {
            # Set state for plot rendering
            plotState <- list(
                plotData = private$.plotData,
                analysisOutcomes = private$.analysisOutcomes,
                outcomePositive = self$options$outcomePositive
            )
            image$setState(plotState)

            if (is.null(private$.dcaResults)) return(FALSE)

            plot_data <- private$.plotData

            # Standardized Net Benefit (sNB) = NB / Prevalence
            prevalence <- mean(private$.analysisOutcomes == self$options$outcomePositive, na.rm=TRUE)
            
            plot_data$snb <- plot_data$net_benefit / prevalence
            
            plot <- ggplot(plot_data, aes(x = threshold, y = snb, color = model)) +
                geom_line(size = 1) +
                scale_color_brewer(palette = "Set1") +
                labs(title = "Standardized Net Benefit",
                     x = "Threshold Probability",
                     y = "Standardized Net Benefit (NB / Prevalence)",
                     color = "Model") +
                theme_minimal() +
                ggtheme
            
            print(plot)
            return(TRUE)
        }
    )
)
