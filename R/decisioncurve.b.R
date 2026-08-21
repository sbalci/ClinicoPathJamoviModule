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
#' @return An \code{R6} class generator object for the \code{decisioncurveClass} backend; used internally by the jamovi analysis wrapper and not called directly.

decisioncurveClass <- if (requireNamespace("jmvcore")) R6::R6Class(
    "decisioncurveClass",
    inherit = decisioncurveBase,
    private = list(

        # Store analysis results
        .dcaResults = NULL,
        .treatAllNB = NULL,
        .plotThinning = NULL,
        .bootConvergedAt = NULL,
        .plotData = NULL,
        .clinicalImpactData = NULL,
        .analysisData = NULL,
        .analysisOutcomes = NULL,
        .outcomePositive = NULL,

        # The positive outcome level actually used by the analysis. Falls back to the raw
        # option only before .run() has resolved it (e.g. an early return).
        .positiveLevel = function() {
            # The else branch used to read `private$.positiveLevel()`, i.e. this
            # method calling itself -- an unconditional infinite recursion that
            # ends in "C stack usage is too close to the limit". It is reachable
            # exactly on the path this comment describes: .run() sets
            # .outcomePositive to NULL before resolving it, so any early return
            # that still consults the positive level blows the stack.
            if (!is.null(private$.outcomePositive)) private$.outcomePositive
            else self$options$outcomePositive
        },

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
            ERROR = list(color = "#dc2626", bgcolor = "#fef2f2", border = "#fca5a5", icon = ""),
            STRONG_WARNING = list(color = "#ea580c", bgcolor = "#fff7ed", border = "#fdba74", icon = ""),
            WARNING = list(color = "#ca8a04", bgcolor = "#fefce8", border = "#fde047", icon = ""),
            INFO = list(color = "#2563eb", bgcolor = "#eff6ff", border = "#93c5fd", icon = "")
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
                    "\u{2022} Threshold range suitable for cancer screening decisions (typical range: 5-20%)")
            }
            
            if (surgical_decision_range) {
                guidance_messages <- c(guidance_messages, 
                    "\u{2022} Threshold range suitable for surgical intervention decisions (typical range: 10-50%)")
            }
            
            if (treatment_selection_range) {
                guidance_messages <- c(guidance_messages, 
                    "\u{2022} Threshold range suitable for treatment selection decisions (typical range: 15-40%)")
            }
            
            if (max_thresh <= 0.05) {
                guidance_messages <- c(guidance_messages, 
                    "\u{2022} Very low threshold range - consider if this aligns with your clinical decision context")
            }
            
            if (min_thresh >= 0.60) {
                guidance_messages <- c(guidance_messages, 
                    "\u{2022} Very high threshold range - ensure this reflects actual clinical decision thresholds")
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

            # Split on commas, semicolons or whitespace. This used to read "[,;\\s]+":
            # inside a POSIX bracket expression TRE treats \s as the literal characters
            # backslash and s, so space-separated entry produced one unparseable token, every
            # value became NA, and the analysis silently fell back to the default thresholds
            # while showing the user's own text in the box.
            raw <- unlist(strsplit(threshold_str, "[,;[:space:]]+"))
            raw <- raw[nzchar(raw)]
            parsed <- suppressWarnings(as.numeric(raw))

            unparsed <- raw[is.na(parsed)]
            kept <- parsed[!is.na(parsed)]
            out_of_range <- kept[kept <= 0 | kept >= 1]
            thresholds <- kept[kept > 0 & kept < 1]

            if (length(unparsed) > 0 || length(out_of_range) > 0) {
                private$.addNotice(
                    type = "WARNING",
                    title = "Some thresholds ignored",
                    content = sprintf(
                        "Ignored %s. Threshold probabilities must be numbers strictly between 0 and 1, separated by commas or spaces.",
                        paste(c(unparsed, base::format(out_of_range)), collapse = ", ")
                    )
                )
            }

            if (length(thresholds) == 0) {
                private$.addNotice(
                    type = "WARNING",
                    title = "Using default thresholds",
                    content = "No usable threshold probabilities were found in the list, so the default 5%, 10%, 15%, 20%, 25% and 30% are used."
                )
                return(private$DECISIONCURVE_DEFAULTS$selected_thresholds)
            }

            return(sort(unique(thresholds)))
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
        
        # NOTE: a .calculateBootstrapCIChunked() path used to live here and has been removed.
        # It was unreachable except as a crash: it delegated back to .calculateBootstrapCI()
        # when n_boot <= 10000 while .calculateBootstrapCI() delegated to it when
        # n_boot >= 10000, so bootReps at its own documented maximum of 10000 satisfied both
        # guards and the two recursed into each other until R aborted with "evaluation nested
        # too deeply". Above 10000 it was unreachable because the option caps there. It was
        # also statistically wrong where it did run: it averaged the per-chunk quantiles
        # rather than taking quantiles of the pooled replicates, which understates the
        # interval width.

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
                                private$.bootConvergedAt <- i
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
        # Range of threshold probabilities over which a model is the best available
        # strategy, i.e. its net benefit exceeds BOTH reference strategies.
        #
        # This deliberately replaces a "maximum net benefit / optimal threshold" summary.
        # Net benefit falls monotonically with the threshold probability, so the argmax is
        # always the lowest threshold on the grid and carries no information; ranking models
        # by their net benefit at that point can and does invert the true ordering. Threshold
        # probability in DCA is an expression of the clinician's relative weighting of a
        # missed case against an unnecessary treatment - it is elicited, not estimated - so
        # no "optimal" value exists to report. dcurves and rmda emit no such quantity either.
        #
        # Comparison against treat-none alone is not enough: at low thresholds nearly any
        # model clears treat-none while still being worse than simply treating everyone.
        .findBenefitRange = function(net_benefits, thresholds, treat_all_nb) {
            treat_none_nb <- 0

            if (is.null(treat_all_nb) || length(treat_all_nb) != length(net_benefits)) {
                reference <- rep(treat_none_nb, length(net_benefits))
            } else {
                reference <- pmax(treat_all_nb, treat_none_nb)
            }

            superior <- !is.na(net_benefits) & net_benefits > reference

            if (!any(superior)) {
                return(list(
                    range_start = NA_real_,
                    range_end = NA_real_,
                    width = NA_real_,
                    contiguous = NA
                ))
            }

            idx <- which(superior)
            range_start <- thresholds[min(idx)]
            range_end <- thresholds[max(idx)]

            # A model can beat both references over two separated stretches. Reporting only
            # the endpoints would then imply benefit across a gap where there is none.
            contiguous <- identical(as.integer(idx), as.integer(seq(min(idx), max(idx))))

            return(list(
                range_start = range_start,
                range_end = range_end,
                width = range_end - range_start,
                contiguous = contiguous
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

        # Bootstrap comparison for Weighted AUC difference AND Mean NB difference
        .calculateBootstrapComparison = function(pred1, pred2, outcomes, thresholds, positive_outcome, n_boot = 1000) {
            
            n <- length(outcomes)
            
            wauc_diff_results <- numeric(n_boot)
            nb_diff_results <- numeric(n_boot)
            
            valid_boot <- 0
            
            for (i in 1:n_boot) {
                # Bootstrap sample
                boot_idx <- sample(n, n, replace = TRUE)
                b_pred1 <- pred1[boot_idx]
                b_pred2 <- pred2[boot_idx]
                b_out <- outcomes[boot_idx]
                
                # Check variation
                if (length(unique(b_out)) < 2) {
                     next
                }
                
                # Calculate Net Benefits (using vectorized method)
                nb1_vals <- private$.calculateNetBenefitsVectorized(b_pred1, b_out, thresholds, positive_outcome)
                nb2_vals <- private$.calculateNetBenefitsVectorized(b_pred2, b_out, thresholds, positive_outcome)
                
                # Calculate wAUC
                wauc1 <- private$.calculateWeightedAUC(nb1_vals, thresholds)
                wauc2 <- private$.calculateWeightedAUC(nb2_vals, thresholds)
                
                # Calculate Mean NB Difference
                nb_diff_vals <- nb1_vals - nb2_vals
                mean_nb_diff <- mean(nb_diff_vals, na.rm = TRUE)
                
                if (!is.na(wauc1) && !is.na(wauc2) && !is.na(mean_nb_diff)) {
                    valid_boot <- valid_boot + 1
                    wauc_diff_results[valid_boot] <- wauc1 - wauc2
                    nb_diff_results[valid_boot] <- mean_nb_diff
                }
            }
            
            # Truncate to valid results
            if (valid_boot < 50) return(list(
                wauc = list(ci_lower=NA, ci_upper=NA, p_value=NA),
                nb = list(ci_lower=NA, ci_upper=NA, p_value=NA)
            ))
            
            wauc_diff_results <- wauc_diff_results[1:valid_boot]
            nb_diff_results <- nb_diff_results[1:valid_boot]
            
            alpha <- 1 - self$options$ciLevel
            
            # Helper for stats.
            # The two-sided bootstrap p-value uses the (b + 1) / (B + 1) convention of
            # Davison & Hinkley (1997, Sec. 4.2). Without the +1 the p-value is exactly 0
            # whenever every replicate falls on one side of the null, which is routine at
            # the default B and would be reported as an impossible p = 0.
            calc_stats <- function(vals) {
                ci_l <- quantile(vals, probs = alpha / 2, na.rm = TRUE)
                ci_u <- quantile(vals, probs = 1 - alpha / 2, na.rm = TRUE)
                n_valid <- sum(!is.na(vals))
                if (n_valid == 0) {
                    return(list(ci_lower = NA, ci_upper = NA, p_value = NA))
                }
                p_pos <- (sum(vals >= 0, na.rm = TRUE) + 1) / (n_valid + 1)
                p_neg <- (sum(vals <= 0, na.rm = TRUE) + 1) / (n_valid + 1)
                p_val <- min(1, 2 * min(p_pos, p_neg))
                return(list(ci_lower = ci_l, ci_upper = ci_u, p_value = p_val))
            }
            
            return(list(
                wauc = calc_stats(wauc_diff_results),
                nb = calc_stats(nb_diff_results)
            ))
        },

        # Main analysis function
        .run = function() {

            # jamovi reuses this R6 object across run cycles, so the notice list survives
            # from one run to the next. Without this reset every notice is re-appended on
            # each option change and the panel fills with duplicates.
            private$.noticeList <- list()
            private$.plotThinning <- NULL
            private$.bootConvergedAt <- NULL
            private$.outcomePositive <- NULL

            # Clear the previous run's analysis state. The five .plot* renderers read these
            # private fields directly, so without this an early return - a bad variable, a
            # non-probability column, an invalid threshold range - left the PREVIOUS run's
            # curves on screen beside the new error notice, and the clinician saw a decision
            # curve that did not come from the data they were looking at.
            private$.dcaResults <- NULL
            private$.plotData <- NULL
            private$.treatAllNB <- NULL
            private$.analysisData <- NULL
            private$.analysisOutcomes <- NULL
            private$.clinicalImpactData <- NULL

            # Fix the RNG for every bootstrap in this run. Unseeded, the same data and the
            # same options gave a different confidence interval and a different p-value on
            # each run: across eight identical reruns at the default 1000 replications the
            # comparison p-value moved between 0.030 and 0.060 and the 95% CI crossed zero
            # in two of them. A clinician who reruns an analysis must get the same numbers.
            # The caller's RNG state is restored on exit so an R-API user's stream is not
            # disturbed by running this analysis.
            seed_val <- self$options$seed
            if (is.null(seed_val) || is.na(seed_val)) seed_val <- 42
            if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
                .saved_seed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
                on.exit(assign(".Random.seed", .saved_seed, envir = globalenv()), add = TRUE)
            }
            set.seed(seed_val)

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

                self$results$instructions$setVisible(TRUE)
                self$results$instructions$setContent(instructions)
                return()
            }

            # Hide instructions when analysis can proceed. The matching setVisible(TRUE)
            # lives in the guard above: without it the panel stayed hidden for the rest of
            # the session once one analysis had succeeded, so a user who then cleared the
            # outcome variable was left with a blank pane and no guidance.
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

            # Persist the RESOLVED level. Table and plot methods used to re-read
            # private$.positiveLevel(), so whenever this fallback fired the curves were
            # computed against unique_outcomes[1] while every downstream table was computed
            # against a level that is not in the data - silently turning every count to zero
            # and every net benefit negative in those tables while the plot looked fine.
            private$.outcomePositive <- outcome_positive

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
                message(sprintf("Decision curve analysis: Processing %d models \u{00D7} %d thresholds (%d total calculations)...", 
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

                # The GUI restricts this box to numeric columns, but a programmatic caller
                # can still pass a factor or a character column. min() on a factor raises a
                # bare R error before any notice can be shown, so reject it explicitly.
                if (!is.numeric(predictions)) {
                    private$.addNotice(
                        type = "ERROR",
                        title = sprintf('Not a numeric column: %s', model_name),
                        content = sprintf(
                            'Model "%s" is a %s column. Decision curve analysis needs predicted probabilities in [0, 1] as a numeric column. Convert it before running the analysis - a categorical column cannot express a predicted risk.',
                            model_name,
                            paste(class(predictions), collapse = "/")
                        )
                    )
                    private$.renderNotices()
                    return()
                }

                if (all(is.na(predictions))) {
                    private$.addNotice(
                        type = "ERROR",
                        title = sprintf('No usable values: %s', model_name),
                        content = sprintf('Model "%s" is entirely missing after complete-case filtering.', model_name)
                    )
                    private$.renderNotices()
                    return()
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

            plot_data <- dplyr::bind_rows(plot_data, ref_data)
            
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

                plot_data <- dplyr::bind_rows(
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
            private$.treatAllNB <- treat_all_nb

            # Create procedure notes
            procedure_notes <- paste0(
                "<html><body>",
                "<h4>Decision Curve Analysis Summary</h4>",
                "<p><strong>Outcome Variable:</strong> ", private$.safeHtmlOutput(outcome_var),
                    " (", private$.safeHtmlOutput(outcome_positive), " = positive)</p>",
                "<p><strong>Models Analyzed:</strong> ",
                    paste(private$.safeHtmlOutput(model_names), collapse = ", "), "</p>",
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

            # Populate range-of-benefit table
            if (self$options$showBenefitRange) {
                private$.populateBenefitRangeTable()
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

            # An early-stopped bootstrap means the interval on screen rests on fewer
            # resamples than the user asked for. jamovi never surfaced the message() that
            # used to report this.
            if (!is.null(private$.bootConvergedAt)) {
                private$.addNotice(
                    type = "INFO",
                    title = "Bootstrap stopped early",
                    content = sprintf(
                        "The confidence intervals converged after %d of the %d requested replications and resampling stopped there. The intervals are based on %d resamples.",
                        private$.bootConvergedAt,
                        self$options$bootReps,
                        private$.bootConvergedAt
                    )
                )
            }

            # Apparent net benefit is not validated net benefit. The analysis is handed a
            # column of predicted risks and has no way to know whether they were fitted on
            # these same rows; for the common case of a marker developed on this dataset
            # every curve here is optimistically biased in the model's favour.
            private$.addNotice(
                type = "STRONG_WARNING",
                title = "Net benefit shown here is apparent, not validated",
                content = paste0(
                    "These curves are computed on the same rows that supplied the predicted risks. ",
                    "If those risks came from a model fitted on this dataset - including a cutpoint, ",
                    "a score, or a regression developed here - the net benefit is optimistically ",
                    "biased and the model can appear to beat treat-all when it does not. ",
                    "For a defensible clinical claim, supply predictions from an external dataset ",
                    "or from cross-validation, and report which was used."
                )
            )

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
                    'Decision curve analysis completed successfully. %d model(s) evaluated using %d complete cases. Outcome prevalence: %.1f%% (%d/%d). Threshold range: %.1f%% to %.1f%%. Review the decision curves and the range of benefit below.',
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
                        private$.analysisOutcomes, thresh, private$.positiveLevel()
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
            # Without this the rows appended below accumulate on every run cycle:
            # a three-model comparison becomes six rows, then nine.
            table$deleteRows()
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
                        private$.positiveLevel()
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
            # Without this the rows appended below accumulate on every run cycle:
            # a three-model comparison becomes six rows, then nine.
            table$deleteRows()
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
                        private$.positiveLevel()
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
            # Without this the rows appended below accumulate on every run cycle:
            # a three-model comparison becomes six rows, then nine.
            table$deleteRows()
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
                        private$.positiveLevel()
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
            # Without this the rows appended below accumulate on every run cycle:
            # a three-model comparison becomes six rows, then nine.
            table$deleteRows()
            model_names <- names(private$.dcaResults)
            analysis_data <- private$.analysisData
            outcomes <- private$.analysisOutcomes
            thresholds <- private$.dcaResults[[1]]$thresholds
            model_vars_map <- private$.parseModelNames()
            
            if (length(model_names) < 2) return()
            
            # Pairwise comparisons. Rows are collected first so the family of pairwise
            # tests can be Holm-adjusted before any "Significant Difference" verdict is
            # printed - this table used to declare significance from an unadjusted p while
            # the comparisonTable beside it adjusted the same family.
            pairs <- combn(model_names, 2, simplify = FALSE)
            rows <- list()
            capped <- FALSE

            for (pair in pairs) {
                m1 <- pair[1]
                m2 <- pair[2]

                idx1 <- which(model_vars_map == m1)
                idx2 <- which(model_vars_map == m2)

                if (length(idx1) == 0 || length(idx2) == 0) next
                var1 <- self$options$models[idx1]
                var2 <- self$options$models[idx2]
                pred1 <- analysis_data[[var1]]
                pred2 <- analysis_data[[var2]]

                nb1 <- private$.dcaResults[[m1]]$net_benefits
                nb2 <- private$.dcaResults[[m2]]$net_benefits

                diff <- nb1 - nb2
                mean_diff <- mean(diff, na.rm = TRUE)
                median_diff <- median(diff, na.rm = TRUE)

                n_boot <- self$options$bootReps
                n_boot_used <- min(n_boot, 1000)   # capped for performance
                if (n_boot_used < n_boot) capped <- TRUE

                private$.checkpoint()

                res_boot <- private$.calculateBootstrapComparison(
                    pred1, pred2, outcomes, thresholds, private$.positiveLevel(),
                    n_boot = n_boot_used
                )

                rows[[length(rows) + 1]] <- list(
                    key = paste0(m1, "_vs_", m2),
                    model1 = m1,
                    model2 = m2,
                    nb_difference_mean = mean_diff,
                    nb_difference_median = median_diff,
                    p_value = res_boot$nb$p_value
                )
            }

            if (length(rows) == 0) return()

            raw_p <- vapply(rows, function(r) as.numeric(r$p_value %||% NA_real_), numeric(1))
            adj_p <- stats::p.adjust(raw_p, method = "holm")

            for (k in seq_along(rows)) {
                r <- rows[[k]]
                conclusion <- if (is.na(adj_p[k])) {
                    "Not testable"
                } else if (adj_p[k] < 0.05) {
                    "Difference beyond chance"
                } else {
                    "No difference beyond chance"
                }
                table$addRow(rowKey = r$key, values = list(
                    model1 = r$model1,
                    model2 = r$model2,
                    nb_difference_mean = r$nb_difference_mean,
                    nb_difference_median = r$nb_difference_median,
                    test_statistic = r$nb_difference_mean,
                    p_value = adj_p[k],
                    conclusion = conclusion
                ))
            }

            table$setNote(
                "method",
                sprintf(
                    "Mean difference in net benefit across the threshold range, with a case-resampling bootstrap p-value Holm-adjusted across all %d pairwise comparisons (seed %d). The verdict column reads the adjusted p at the 0.05 level.",
                    length(rows),
                    if (is.null(self$options$seed) || is.na(self$options$seed)) 42 else self$options$seed
                )
            )
            if (capped) {
                table$setNote(
                    "cap",
                    sprintf("Bootstrap replications for this table are capped at 1000 for speed; the %d you requested are used elsewhere.",
                            self$options$bootReps)
                )
            }
        },

        .populateBenefitRangeTable = function() {
            range_table <- self$results$benefitRangeTable
            range_table$deleteRows()

            model_names <- names(private$.dcaResults)
            has_gap <- character(0)
            none_beneficial <- character(0)

            for (i in seq_along(model_names)) {
                model_name <- model_names[i]
                model_results <- private$.dcaResults[[model_name]]

                info <- private$.findBenefitRange(
                    model_results$net_benefits,
                    model_results$thresholds,
                    private$.treatAllNB
                )

                if (is.na(info$range_start)) {
                    none_beneficial <- c(none_beneficial, model_name)
                } else if (isFALSE(info$contiguous)) {
                    has_gap <- c(has_gap, model_name)
                }

                range_table$addRow(rowKey = i, values = list(
                    model = model_name,
                    range_start = info$range_start,
                    range_end = info$range_end,
                    range_width = info$width
                ))
            }


            range_table$setNote(
                "definition",
                "Range of threshold probabilities over which the model's net benefit exceeds both treat-all and treat-none. A model is only worth using within this range. Threshold probability is set by clinical judgement about the relative cost of a missed case versus an unnecessary treatment; it is not estimated from the data, so there is no optimal value to report."
            )

            if (length(none_beneficial) > 0) {
                range_table$setNote(
                    "none",
                    sprintf(
                        "%s never exceeds both reference strategies anywhere in the threshold range examined, so no range is shown.",
                        paste(none_beneficial, collapse = ", ")
                    )
                )
            }

            if (length(has_gap) > 0) {
                range_table$setNote(
                    "gap",
                    sprintf(
                        "%s is superior over more than one separate stretch of thresholds. The start and end shown span a gap where the model is not superior - read the curve rather than the endpoints.",
                        paste(has_gap, collapse = ", ")
                    )
                )
            }
        },

        .calculateClinicalImpactMetrics = function(outcomes, outcome_positive) {
            clinical_impact_table <- self$results$clinicalImpactTable
            clinical_impact_table$deleteRows()

            selected_thresholds <- private$.parseSelectedThresholds()
            model_names <- names(private$.dcaResults)

            # Calculate for each model at each selected threshold
            row_counter <- 1
            for (model_name in model_names) {
                model_results <- private$.dcaResults[[model_name]]

                for (thresh in selected_thresholds) {
                    # Find closest threshold
                    closest_idx <- which.min(abs(model_results$thresholds - thresh))
                    detailed_result <- model_results$detailed_results[[closest_idx]]

                    # Interventions avoided compared to treating everyone.
                    # Every other column in this table is per 100 patients; this one used to
                    # be scaled to populationSize (default 1000), so a single row carried two
                    # different denominators with nothing on screen to say so.
                    interventions_avoided <- 100 - detailed_result$interventions_per_100

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

            # Reuse the treat-all curve computed in .run() on the analysis cohort. This
            # method used to rebuild its own complete-case set from self$data using only
            # outcome + models, which excluded the clinical-decision-rule variable and so
            # produced a treat-all baseline drawn from MORE rows than the model curves it
            # was compared against. It also re-read the raw outcomePositive option rather
            # than the level the analysis actually resolved.
            treat_all_nb <- private$.treatAllNB
            if (is.null(treat_all_nb) || length(treat_all_nb) != length(thresholds)) {
                return()
            }

            # The comparator is the BEST DEFAULT STRATEGY at each threshold, not treat-all
            # alone. Treat-all net benefit goes sharply negative above the prevalence, so
            # measuring against it credited a model for beating a strategy no clinician
            # would ever adopt: on a 15.6%-prevalence cohort over the default 5-50% range
            # the gain came out at 0.248, which the table's own note reads as 24.8 extra
            # true positives per 100 patients when only 15.6 cases exist per 100. Against
            # pmax(treat-all, treat-none) the honest gain on that cohort is 0.030.
            reference_nb <- pmax(treat_all_nb, 0)
            reference_wauc <- private$.calculateWeightedAUC(reference_nb, thresholds)

            for (i in seq_along(model_names)) {
                model_name <- model_names[i]
                model_results <- private$.dcaResults[[model_name]]

                # Calculate weighted AUC
                wauc <- private$.calculateWeightedAUC(
                    model_results$net_benefits,
                    model_results$thresholds
                )

                # Gain over treating everyone, as a DIFFERENCE in weighted net benefit.
                # This used to be reported as the ratio (wauc - treat_all) / |treat_all|.
                # Treat-all net benefit crosses zero at a threshold equal to the outcome
                # prevalence, so whenever the threshold range brackets the prevalence the
                # denominator is near zero and the ratio explodes - percentages in the
                # hundreds were being displayed for ordinary differences.
                if (!is.na(wauc) && !is.na(reference_wauc)) {
                    benefit_gain <- wauc - reference_wauc
                } else {
                    benefit_gain <- NA
                }

                weighted_auc_table$addRow(rowKey = i, values = list(
                    model = model_name,
                    weighted_auc = wauc,
                    auc_range = paste0(round(min(thresholds) * 100, 1), "% - ",
                                       round(max(thresholds) * 100, 1), "%"),
                    benefit_gain = benefit_gain
                ))
            }

            weighted_auc_table$setNote(
                "wauc",
                sprintf(
                    "Average net benefit over the %.1f%% to %.1f%% threshold range: the area under the decision curve divided by the width of that range. Every threshold in the range counts equally, so the value depends on the range you chose - report the range with it. Gain vs Default is the difference against the better of treating everyone and treating no one at each threshold, on the net-benefit scale: 0.01 means one extra true positive per 100 patients at no extra cost in unnecessary treatment. A gain at or below zero means a default strategy serves these patients at least as well as the model.",
                    min(thresholds) * 100, max(thresholds) * 100
                )
            )
        },

        .performModelComparison = function() {
            comparison_table <- self$results$comparisonTable
            comparison_table$deleteRows()

            model_names <- names(private$.dcaResults)
            analysis_data <- private$.analysisData
            outcomes <- private$.analysisOutcomes
            thresholds <- private$.dcaResults[[1]]$thresholds
            
            # Map model names to variable names
            model_vars_map <- private$.parseModelNames()

            # Compare each pair of models.
            # Rows are collected first so that the family of k(k-1)/2 pairwise tests can be
            # Holm-adjusted together before anything is displayed. Reporting only nominal
            # p-values here invites reading a five-model screen as if it were one test.
            rows <- list()
            skipped <- character(0)

            for (i in 1:(length(model_names) - 1)) {
                for (j in (i + 1):length(model_names)) {
                    model1_name <- model_names[i]
                    model2_name <- model_names[j]

                    # Find corresponding variables
                    idx1 <- which(model_vars_map == model1_name)
                    idx2 <- which(model_vars_map == model2_name)

                    # Derived strategies (the clinical decision rule, treat-all/treat-none)
                    # have no input column to resample, so they cannot enter the bootstrap
                    # comparison. Record the omission rather than dropping it silently.
                    if (length(idx1) == 0 || length(idx2) == 0) {
                        skipped <- c(skipped, paste(model1_name, "vs", model2_name))
                        next
                    }

                    var1 <- self$options$models[idx1]
                    var2 <- self$options$models[idx2]

                    pred1 <- analysis_data[[var1]]
                    pred2 <- analysis_data[[var2]]

                    # Calculate weighted AUC difference (observed)
                    wauc1 <- private$.calculateWeightedAUC(
                        private$.dcaResults[[model1_name]]$net_benefits,
                        thresholds
                    )
                    wauc2 <- private$.calculateWeightedAUC(
                        private$.dcaResults[[model2_name]]$net_benefits,
                        thresholds
                    )
                    wauc_diff <- wauc1 - wauc2

                    # Reuse bootReps from options
                    n_boot <- self$options$bootReps

                    private$.checkpoint()

                    res_boot <- private$.calculateBootstrapComparison(
                        pred1, pred2, outcomes, thresholds, private$.positiveLevel(),
                        n_boot = n_boot
                    )

                    rows[[length(rows) + 1]] <- list(
                        comparison = paste(model1_name, "vs", model2_name),
                        weighted_auc_diff = wauc_diff,
                        ci_lower = res_boot$wauc$ci_lower,
                        ci_upper = res_boot$wauc$ci_upper,
                        p_value = res_boot$wauc$p_value
                    )
                }
            }

            if (length(rows) == 0) {
                return()
            }

            raw_p <- vapply(rows, function(r) {
                if (is.null(r$p_value)) NA_real_ else as.numeric(r$p_value)
            }, numeric(1))
            adj_p <- stats::p.adjust(raw_p, method = "holm")

            for (k in seq_along(rows)) {
                r <- rows[[k]]
                r$p_value_adj <- adj_p[k]
                comparison_table$addRow(rowKey = k, values = r)
            }

            if (length(rows) > 1) {
                comparison_table$setNote(
                    "boot",
                    sprintf(
                        "Bootstrap comparison of the average net benefit under each decision curve, %d resamples, seed %d. Intervals are %.0f%% percentile intervals. Re-running with the same seed reproduces these numbers exactly.",
                        self$options$bootReps,
                        if (is.null(self$options$seed) || is.na(self$options$seed)) 42 else self$options$seed,
                        self$options$ciLevel * 100
                    )
                )
                comparison_table$setNote(
                    "holm",
                    sprintf(
                        "p (Holm) controls the family-wise error rate across all %d pairwise comparisons. Interpret the unadjusted p only for a single comparison specified before the data were seen.",
                        length(rows)
                    )
                )
            }

            if (length(skipped) > 0) {
                private$.addNotice(
                    type = "INFO",
                    title = "Comparisons not tested",
                    content = sprintf(
                        "%s could not be bootstrap-tested because at least one side is a derived strategy rather than a predictor column, so it has no values to resample. Its curve is still shown in the plot.",
                        paste(skipped, collapse = "; ")
                    )
                )
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
                    "<p><strong>Best Performing Model:</strong> ", private$.safeHtmlOutput(best_model), "</p>"
                )

                # Range over which the leading model beats both reference strategies.
                best_results <- private$.dcaResults[[best_model]]
                range_info <- private$.findBenefitRange(
                    best_results$net_benefits,
                    best_results$thresholds,
                    private$.treatAllNB
                )

                if (!is.na(range_info$range_start)) {
                    interpretation <- paste0(
                        interpretation,
                        "<p><strong>Range of Benefit:</strong> ",
                        round(range_info$range_start * 100, 1), "% to ",
                        round(range_info$range_end * 100, 1),
                        "% threshold probability - the range over which this model beats both ",
                        "treating everyone and treating no one.",
                        if (isFALSE(range_info$contiguous))
                            " This range contains a gap where the model is not superior; read the curve."
                        else "",
                        "</p>"
                    )
                } else {
                    interpretation <- paste0(
                        interpretation,
                        "<p><strong>Range of Benefit:</strong> none. Across every threshold examined, ",
                        "treating everyone or treating no one does at least as well as this model.</p>"
                    )
                }
            }

            interpretation <- paste0(
                interpretation,
                "<p><strong>Interpretation Guidelines:</strong></p>",
                "<ul>",
                "<li>A model is useful only where its curve sits above BOTH reference lines</li>",
                "<li>Decide the threshold range from clinical judgement first, then read the curves there - not the other way round</li>",
                "<li>Net benefit is on the scale of true positives per patient; multiply by 100 to read it as true positives per 100 patients, at no additional cost in unnecessary treatment</li>",
                "<li>Differences of a few thousandths of net benefit are within bootstrap noise at typical sample sizes</li>",
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
                "<li><strong>Net Benefit Formula:</strong> NB = (TP/n) - (FP/n) \u{00D7} [pt/(1-pt)], where pt is threshold probability</li>")
            
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
                    private$.safeHtmlOutput(self$options$decisionRuleLabel), ")</li>")
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
                
                # jamovi never surfaces message(), so this used to be silent: the curve on
                # screen was not the curve that was computed. Record it for the plot caption.
                private$.plotThinning <- list(
                    from = nrow(plot_data),
                    to = nrow(optimized_data)
                )

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
                    color = "Strategy",
                    caption = if (!is.null(private$.plotThinning))
                        sprintf(
                            "Curve drawn from %d of %d computed points for rendering speed; tables and statistics use all of them.",
                            private$.plotThinning$to, private$.plotThinning$from
                        )
                    else NULL
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

            # Reshape data for stacked bar chart (tidyr::gather namespaced below)
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
                outcomePositive = private$.positiveLevel()
            )
            image$setState(plotState)

            if (is.null(private$.dcaResults)) return(FALSE)

            plot_data <- private$.plotData

            # Calculate Relative Utility
            # RU = (NB_model - NB_all) / (NB_perfect - NB_all)
            
            prevalence <- mean(private$.analysisOutcomes == private$.positiveLevel(), na.rm=TRUE)
            
            plot_data$relative_utility <- NA
            
            for (i in seq_len(nrow(plot_data))) {
                thresh <- plot_data$threshold[i]
                nb <- plot_data$net_benefit[i]
                
                # NB_perfect (Sensitivity=1, Specificity=1)
                nb_perfect <- prevalence

                # The baseline is the BEST default strategy at this threshold: treat
                # everyone below the prevalence, treat no one above it. Using raw treat-all
                # made the denominator explode above the prevalence, where treat-all net
                # benefit dives towards minus infinity - the do-nothing line then scored
                # 95-99% of "perfect" at high thresholds, which reads as an excellent
                # strategy when it is simply the absence of one.
                nb_all <- prevalence - (1 - prevalence) * (thresh / (1 - thresh))
                nb_baseline <- max(nb_all, 0)

                denom <- nb_perfect - nb_baseline

                if (abs(denom) > 1e-6) {
                    ru <- (nb - nb_baseline) / denom
                } else {
                    ru <- NA_real_
                }
                
                plot_data$relative_utility[i] <- ru
            }
            
            # The curve used to be truncated twice - rows outside (-0.5, 1.1] were dropped
            # and then ylim() dropped more, because ylim() sets a scale limit rather than a
            # viewport. A model performing badly therefore had its line simply stop, with no
            # indication that anything had been removed. Zoom with coord_cartesian instead,
            # which keeps every observation and only changes what is in view, and say so
            # when a model actually runs off the bottom.
            y_floor <- -0.2
            y_ceiling <- 1.05
            below_view <- plot_data[
                !is.na(plot_data$relative_utility) & plot_data$relative_utility < y_floor, ]
            off_view_models <- unique(below_view$model)

            plot_caption <- if (length(off_view_models) > 0) {
                sprintf(
                    "%s falls below the visible range at some thresholds: relative utility there is worse than shown.",
                    paste(off_view_models, collapse = ", ")
                )
            } else {
                NULL
            }

            plot <- ggplot(plot_data, aes(x = threshold, y = relative_utility, color = model)) +
                geom_line(linewidth = 1) +
                scale_color_brewer(palette = "Set1") +
                labs(title = "Relative Utility Curve",
                     x = "Threshold Probability",
                     y = "Relative Utility (vs best default strategy)",
                     color = "Model",
                     caption = plot_caption) +
                theme_minimal() +
                ggtheme +
                ggplot2::coord_cartesian(ylim = c(y_floor, y_ceiling))

            print(plot)
            return(TRUE)
        },
        
        .plotStandardizedNetBenefit = function(image, ggtheme, theme, ...) {
            # Set state for plot rendering
            plotState <- list(
                plotData = private$.plotData,
                analysisOutcomes = private$.analysisOutcomes,
                outcomePositive = private$.positiveLevel()
            )
            image$setState(plotState)

            if (is.null(private$.dcaResults)) return(FALSE)

            plot_data <- private$.plotData

            # Standardized Net Benefit (sNB) = NB / Prevalence
            prevalence <- mean(private$.analysisOutcomes == private$.positiveLevel(), na.rm=TRUE)
            
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
