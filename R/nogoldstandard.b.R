#' @title Analysis Without Gold Standard
#' @importFrom R6 R6Class
#' @import jmvcore

nogoldstandardClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "nogoldstandardClass",
    inherit = nogoldstandardBase,
    private = list(

        .init = function() {
            if (is.null(self$options$test1) || is.null(self$options$test2))
                return()

            table <- self$results$test_metrics
            tests <- private$.getTestVariables()
            for (test in tests) {
                table$addRow(rowKey=test, values=list(
                    test = test
                ))
            }
        },

        .getTestVariables = function() {
            vars <- c()
            for (i in 1:5) {
                var_name <- paste0("test", i)
                if (!is.null(self$options[[var_name]])) {
                    vars <- c(vars, self$options[[var_name]])
                }
            }
            return(vars)
        },

        .run = function() {
            if (is.null(self$options$test1) || is.null(self$options$test2)) {
                return()
            }

            if (nrow(self$data) == 0) {
                stop('Data contains no rows')
            }

            # Get test variables and their positive levels
            tests <- list()
            test_levels <- list()

            for (i in 1:5) {
                var_name <- paste0("test", i)
                level_name <- paste0("test", i, "Positive")

                if (!is.null(self$options[[var_name]])) {
                    test_var <- self$options[[var_name]]
                    pos_level <- self$options[[level_name]]

                    if (!is.null(pos_level)) {
                        tests[[length(tests) + 1]] <- test_var
                        test_levels[[length(test_levels) + 1]] <- pos_level
                    }
                }
            }

            # Data preparation
            data <- self$data
            test_data <- data[unlist(tests)]
            test_data <- jmvcore::naOmit(test_data)

            if (nrow(test_data) == 0) {
                stop('No complete cases available')
            }

            # Convert to binary format for analysis
            binary_data <- data.frame(matrix(nrow=nrow(test_data), ncol=length(tests)))
            names(binary_data) <- unlist(tests)

            for (i in seq_along(tests)) {
                test_name <- tests[[i]]
                pos_level <- test_levels[[i]]

                var <- test_data[[test_name]]
                binary_data[[test_name]] <- as.numeric(var == pos_level)
            }

            # Run analysis based on selected method
            results <- NULL
            if (self$options$method == "latent_class") {
                results <- private$.runLCA(binary_data, tests, test_levels)
            } else if (self$options$method == "composite") {
                results <- private$.runComposite(binary_data)
            } else if (self$options$method == "bayesian") {
                results <- private$.runBayesian(binary_data)
            } else if (self$options$method == "all_positive") {
                results <- private$.runAllPositive(binary_data)
            } else if (self$options$method == "any_positive") {
                results <- private$.runAnyPositive(binary_data)
            }

            # Update results
            if (!is.null(results)) {
                private$.populatePrevalence(results)
                private$.populateTestMetrics(results)
                if (self$options$method == "latent_class") {
                    private$.populateModelFit(results$model)
                }
            }

            # Prepare data for the plot
            agreement_matrix <- matrix(0, ncol=length(tests), nrow=length(tests))
            colnames(agreement_matrix) <- unlist(tests)
            rownames(agreement_matrix) <- unlist(tests)

            for (i in 1:length(tests)) {
                for (j in 1:length(tests)) {
                    test1_pos <- test_data[[tests[[i]]]] == test_levels[[i]]
                    test2_pos <- test_data[[tests[[j]]]] == test_levels[[j]]
                    agreement_matrix[i, j] <- mean(test1_pos == test2_pos, na.rm=TRUE)
                }
            }

            # Store agreement matrix for plotting
            self$results$agreement_plot$setVisible(TRUE)
            self$results$agreement_plot$setState(list(
                agreement_matrix = agreement_matrix,
                tests = unlist(tests)
            ))

            self$results$agreement_plot2$setVisible(TRUE)
            self$results$agreement_plot2$setState(list(
                agreement_matrix = agreement_matrix,
                tests = unlist(tests)
            ))




        },

        .populatePrevalence = function(results) {
            if(is.null(results))
                return()

            prevalence <- results$prevalence

            # Calculate confidence intervals if bootstrap is enabled
            if (self$options$bootstrap) {
                ci <- private$.calculateBootstrapCI(
                    data = results$data,
                    method = self$options$method,
                    nboot = self$options$nboot,
                    alpha = self$options$alpha,
                    type = "prevalence"
                )
                ci_lower <- ci$lower
                ci_upper <- ci$upper
            } else {
                # Simple normal approximation
                n <- nrow(results$data)
                se <- sqrt(prevalence * (1 - prevalence) / n)
                z <- qnorm(1 - self$options$alpha/2)
                ci_lower <- max(0, prevalence - z * se)
                ci_upper <- min(1, prevalence + z * se)
            }

            table <- self$results$prevalence
            table$setRow(rowNo=1, values=list(
                estimate = prevalence,
                ci_lower = ci_lower,
                ci_upper = ci_upper
            ))
        },

        .populateTestMetrics = function(results) {
            if(is.null(results))
                return()

            tests <- private$.getTestVariables()

            for (i in seq_along(tests)) {
                sensitivity <- results$sensitivities[i]
                specificity <- results$specificities[i]

                # Calculate confidence intervals
                if (self$options$bootstrap) {
                    sens_ci <- private$.calculateBootstrapCI(
                        data = results$data,
                        method = self$options$method,
                        nboot = self$options$nboot,
                        alpha = self$options$alpha,
                        type = "sensitivity",
                        test_index = i
                    )
                    spec_ci <- private$.calculateBootstrapCI(
                        data = results$data,
                        method = self$options$method,
                        nboot = self$options$nboot,
                        alpha = self$options$alpha,
                        type = "specificity",
                        test_index = i
                    )
                } else {
                    # Simple normal approximation
                    n <- nrow(results$data)
                    z <- qnorm(1 - self$options$alpha/2)

                    se_sens <- sqrt(sensitivity * (1 - sensitivity) / n)
                    sens_ci <- list(
                        lower = max(0, sensitivity - z * se_sens),
                        upper = min(1, sensitivity + z * se_sens)
                    )

                    se_spec <- sqrt(specificity * (1 - specificity) / n)
                    spec_ci <- list(
                        lower = max(0, specificity - z * se_spec),
                        upper = min(1, specificity + z * se_spec)
                    )
                }

                self$results$test_metrics$setRow(
                    rowKey=tests[i],
                    values=list(
                        test = tests[i],
                        sensitivity = sensitivity,
                        specificity = specificity,
                        sens_ci_lower = sens_ci$lower,
                        sens_ci_upper = sens_ci$upper,
                        spec_ci_lower = spec_ci$lower,
                        spec_ci_upper = spec_ci$upper
                    )
                )
            }
        },

        .populateModelFit = function(model) {
            if(is.null(model))
                return()

            table <- self$results$model_fit

            # Add basic fit statistics
            fit_stats <- list(
                BIC = model$bic,
                AIC = model$aic,
                "Log-Likelihood" = model$llik,
                "G-squared" = model$Gsq,
                "Chi-squared" = model$Chisq,
                "Degrees of Freedom" = model$resid.df
            )

            # Add each available statistic to table
            for (name in names(fit_stats)) {
                if (!is.null(fit_stats[[name]])) {
                    table$addRow(rowKey=name, values=list(
                        statistic = name,
                        value = fit_stats[[name]]
                    ))
                }
            }
        },

        .runLCA = function(binary_data, tests, test_levels) {
            if (!requireNamespace("poLCA", quietly = TRUE)) {
                stop("Package 'poLCA' is required for latent class analysis")
            }

            # Convert to LCA format (factors with "no"/"yes" levels)
            lca_data <- data.frame(matrix(nrow=nrow(binary_data), ncol=ncol(binary_data)))
            names(lca_data) <- names(binary_data)

            for (i in seq_along(names(binary_data))) {
                lca_data[[i]] <- factor(
                    binary_data[[i]],
                    levels = c(0, 1),
                    labels = c("no", "yes")
                )
            }

            # Create formula
            var_names <- names(lca_data)
            f <- stats::as.formula(paste("cbind(", paste(var_names, collapse=","), ")~1"))

            # Run LCA with multiple starts to ensure global optimum
            best_model <- NULL
            best_llik <- -Inf

            for (start in 1:10) {
                set.seed(start * 100)

                tryCatch({
                    model <- poLCA::poLCA(
                        formula = f,
                        data = lca_data,
                        nclass = 2,
                        maxiter = 1000,
                        graphs = FALSE,
                        verbose = FALSE,
                        nrep = 1
                    )

                    if (!is.null(model) && model$llik > best_llik) {
                        best_model <- model
                        best_llik <- model$llik
                    }

                }, error = function(e) {
                    # Continue to next start
                })
            }

            if (is.null(best_model)) {
                stop("LCA model fitting failed")
            }

            # Extract results
            # Ensure we identify which class represents disease presence
            # Usually the class with higher test positivity rates
            class_means <- sapply(best_model$probs, function(x) x[2,2])  # P(yes|class)
            disease_class <- which.max(colMeans(matrix(class_means, ncol=2)))
            healthy_class <- 3 - disease_class  # The other class

            # Disease prevalence is the probability of the disease class
            prevalence <- best_model$P[disease_class]

            # Extract sensitivities and specificities
            sensitivities <- numeric(length(tests))
            specificities <- numeric(length(tests))

            for (i in seq_along(tests)) {
                # Sensitivity: P(test positive | disease class)
                sensitivities[i] <- best_model$probs[[i]][2, disease_class]
                # Specificity: P(test negative | healthy class)
                specificities[i] <- best_model$probs[[i]][1, healthy_class]
            }

            return(list(
                prevalence = prevalence,
                sensitivities = sensitivities,
                specificities = specificities,
                model = best_model,
                data = binary_data,
                disease_class = disease_class
            ))
        },

        .runComposite = function(binary_data) {
            # Create composite reference from majority vote
            composite <- rowMeans(binary_data, na.rm = TRUE) >= 0.5

            # Calculate prevalence
            prevalence <- mean(composite, na.rm = TRUE)

            # Calculate metrics for each test
            sensitivities <- numeric(ncol(binary_data))
            specificities <- numeric(ncol(binary_data))

            for (i in seq_along(binary_data)) {
                test_result <- binary_data[[i]] == 1
                tp <- sum(test_result & composite, na.rm = TRUE)
                tn <- sum(!test_result & !composite, na.rm = TRUE)
                fp <- sum(test_result & !composite, na.rm = TRUE)
                fn <- sum(!test_result & composite, na.rm = TRUE)

                sensitivities[i] <- tp/(tp + fn)
                specificities[i] <- tn/(tn + fp)
            }

            return(list(
                prevalence = prevalence,
                sensitivities = sensitivities,
                specificities = specificities,
                data = binary_data
            ))
        },

        # FIXED: Bayesian analysis implementation with proper NA handling
        .runBayesian = function(binary_data) {
            # Simple Bayesian approach based on prior distributions and EM algorithm

            # Number of tests and patients
            num_tests <- ncol(binary_data)
            num_patients <- nrow(binary_data)

            # Prior parameters
            # Prior for prevalence (Beta distribution)
            alpha_prev <- 1  # uniform prior
            beta_prev <- 1   # uniform prior

            # Prior for sensitivity and specificity (Beta distribution)
            alpha_sens <- 2  # slightly informative prior favoring higher sensitivity
            beta_sens <- 1
            alpha_spec <- 2  # slightly informative prior favoring higher specificity
            beta_spec <- 1

            # Initialize parameters
            # Start with prevalence = 0.3 as initial guess
            prevalence <- 0.3

            # Initialize sensitivity and specificity for each test
            sensitivities <- rep(0.8, num_tests)  # initial guess
            specificities <- rep(0.9, num_tests)  # initial guess

            # EM algorithm for parameter estimation
            max_iter <- 100
            tol <- 1e-6
            converged <- FALSE

            for (iter in 1:max_iter) {
                # E-step: Calculate posterior probabilities of disease for each patient
                prob_disease <- numeric(num_patients)

                for (i in 1:num_patients) {
                    # Initialize log odds for this patient
                    log_odds <- log(prevalence / (1 - prevalence))

                    # Update log odds based on test results
                    for (j in 1:num_tests) {
                        # Skip if test result is NA
                        if (is.na(binary_data[i, j])) {
                            next
                        }

                        # Get test result (0 or 1)
                        test_result <- binary_data[i, j]

                        # Ensure sensitivity and specificity are valid probabilities
                        sens_j <- max(0.001, min(0.999, sensitivities[j]))
                        spec_j <- max(0.001, min(0.999, specificities[j]))

                        if (test_result == 1) {
                            # Test positive
                            log_odds <- log_odds + log(sens_j / (1 - spec_j))
                        } else {
                            # Test negative
                            log_odds <- log_odds + log((1 - sens_j) / spec_j)
                        }
                    }

                    # Convert log odds to probability
                    prob_disease[i] <- exp(log_odds) / (1 + exp(log_odds))

                    # Handle extreme values to avoid numerical issues
                    if (is.infinite(log_odds)) {
                        prob_disease[i] <- if (log_odds > 0) 0.999 else 0.001
                    }

                    # Handle NAs
                    if (is.na(prob_disease[i])) {
                        prob_disease[i] <- prevalence  # use current prevalence as a fallback
                    }
                }

                # M-step: Update parameters
                # Update prevalence
                new_prevalence <- (sum(prob_disease, na.rm=TRUE) + alpha_prev - 1) /
                    (num_patients + alpha_prev + beta_prev - 2)

                # Update sensitivities and specificities
                new_sensitivities <- numeric(num_tests)
                new_specificities <- numeric(num_tests)

                for (j in 1:num_tests) {
                    # For each test, get non-NA values
                    not_na <- !is.na(binary_data[, j])
                    if (sum(not_na) == 0) {
                        # If all values are NA, keep previous estimates
                        new_sensitivities[j] <- sensitivities[j]
                        new_specificities[j] <- specificities[j]
                        next
                    }

                    # Get test results and probabilities for non-NA values
                    test_results <- binary_data[not_na, j]
                    probs <- prob_disease[not_na]

                    # For sensitivity: P(T+|D+)
                    test_pos <- test_results == 1
                    if (sum(probs) > 0) {
                        new_sensitivities[j] <- (sum(probs[test_pos], na.rm=TRUE) + alpha_sens - 1) /
                            (sum(probs, na.rm=TRUE) + alpha_sens + beta_sens - 2)
                    } else {
                        # Fallback if denominator is zero
                        new_sensitivities[j] <- (alpha_sens - 1) / (alpha_sens + beta_sens - 2)
                    }

                    # For specificity: P(T-|D-)
                    test_neg <- test_results == 0
                    if (sum(1 - probs) > 0) {
                        new_specificities[j] <- (sum((1 - probs)[test_neg], na.rm=TRUE) + alpha_spec - 1) /
                            (sum(1 - probs, na.rm=TRUE) + alpha_spec + beta_spec - 2)
                    } else {
                        # Fallback if denominator is zero
                        new_specificities[j] <- (alpha_spec - 1) / (alpha_spec + beta_spec - 2)
                    }

                    # Ensure values are within valid range
                    new_sensitivities[j] <- max(0.001, min(0.999, new_sensitivities[j]))
                    new_specificities[j] <- max(0.001, min(0.999, new_specificities[j]))
                }

                # Check convergence - handle NAs properly
                # Maximum absolute difference across all parameters
                param_diffs <- c(
                    abs(new_prevalence - prevalence),
                    abs(new_sensitivities - sensitivities),
                    abs(new_specificities - specificities)
                )

                # Check if we've converged, ignoring NAs
                if (max(param_diffs, na.rm=TRUE) < tol) {
                    converged <- TRUE
                    break
                }

                # Update parameters for next iteration
                prevalence <- new_prevalence
                sensitivities <- new_sensitivities
                specificities <- new_specificities
            }

            return(list(
                prevalence = prevalence,
                sensitivities = sensitivities,
                specificities = specificities,
                data = binary_data,
                converged = converged,
                iterations = iter
            ))
        },

        # Analysis using "All Tests Positive" as reference
        .runAllPositive = function(binary_data) {
            # Create reference where disease is present only if ALL tests are positive
            reference <- apply(binary_data, 1, function(x) all(x == 1, na.rm=TRUE))

            # Calculate prevalence
            prevalence <- mean(reference, na.rm = TRUE)

            # Calculate metrics for each test
            sensitivities <- numeric(ncol(binary_data))
            specificities <- numeric(ncol(binary_data))

            for (i in seq_along(names(binary_data))) {
                test_result <- binary_data[[i]] == 1
                tp <- sum(test_result & reference, na.rm = TRUE)
                tn <- sum(!test_result & !reference, na.rm = TRUE)
                fp <- sum(test_result & !reference, na.rm = TRUE)
                fn <- sum(!test_result & reference, na.rm = TRUE)

                sensitivities[i] <- if ((tp + fn) > 0) tp/(tp + fn) else NA
                specificities[i] <- if ((tn + fp) > 0) tn/(tn + fp) else NA
            }

            return(list(
                prevalence = prevalence,
                sensitivities = sensitivities,
                specificities = specificities,
                data = binary_data
            ))
        },

        # Analysis using "Any Test Positive" as reference
        .runAnyPositive = function(binary_data) {
            # Create reference where disease is present if ANY test is positive
            reference <- apply(binary_data, 1, function(x) any(x == 1, na.rm=TRUE))

            # Calculate prevalence
            prevalence <- mean(reference, na.rm = TRUE)

            # Calculate metrics for each test
            sensitivities <- numeric(ncol(binary_data))
            specificities <- numeric(ncol(binary_data))

            for (i in seq_along(names(binary_data))) {
                test_result <- binary_data[[i]] == 1
                tp <- sum(test_result & reference, na.rm = TRUE)
                tn <- sum(!test_result & !reference, na.rm = TRUE)
                fp <- sum(test_result & !reference, na.rm = TRUE)
                fn <- sum(!test_result & reference, na.rm = TRUE)

                sensitivities[i] <- if ((tp + fn) > 0) tp/(tp + fn) else NA
                specificities[i] <- if ((tn + fp) > 0) tn/(tn + fp) else NA
            }

            return(list(
                prevalence = prevalence,
                sensitivities = sensitivities,
                specificities = specificities,
                data = binary_data
            ))
        },


        .calculateBootstrapCI = function(data, method, nboot, alpha, type, test_index = NULL) {
            # Simple bootstrap implementation with progress indicators
            n <- nrow(data)
            boot_results <- numeric(nboot)

            # Show starting message
            message("\n=== Bootstrap Analysis ===")
            message(sprintf("Starting bootstrap with %d iterations for %s method", nboot, method))
            message(sprintf("Estimating confidence intervals for %s", type))
            if (!is.null(test_index)) {
                message(sprintf("Test index: %d", test_index))
            }

            # Progress tracking variables
            start_time <- Sys.time()
            last_update <- start_time
            update_interval <- max(1, floor(nboot / 20))  # Update ~20 times during process
            success_count <- 0
            error_count <- 0

            for (b in 1:nboot) {
                # Resample data
                boot_indices <- sample(n, n, replace = TRUE)
                boot_data <- data[boot_indices, ]

                # Run analysis on bootstrap sample
                boot_result <- NULL
                tryCatch({
                    if (method == "latent_class") {
                        boot_result <- private$.runLCA(boot_data, names(data), NULL)
                    } else if (method == "composite") {
                        boot_result <- private$.runComposite(boot_data)
                    } else if (method == "all_positive") {
                        boot_result <- private$.runAllPositive(boot_data)
                    } else if (method == "any_positive") {
                        boot_result <- private$.runAnyPositive(boot_data)
                    } else if (method == "bayesian") {
                        boot_result <- private$.runBayesian(boot_data)
                    }
                    success_count <- success_count + 1
                }, error = function(e) {
                    # Count errors but continue bootstrap
                    error_count <- error_count + 1
                })

                # Extract relevant statistic
                if (!is.null(boot_result)) {
                    if (type == "prevalence") {
                        boot_results[b] <- boot_result$prevalence
                    } else if (type == "sensitivity" && !is.null(test_index)) {
                        boot_results[b] <- boot_result$sensitivities[test_index]
                    } else if (type == "specificity" && !is.null(test_index)) {
                        boot_results[b] <- boot_result$specificities[test_index]
                    }
                } else {
                    boot_results[b] <- NA
                }

                # Show progress updates
                current_time <- Sys.time()
                if (b %% update_interval == 0 || b == nboot ||
                    as.numeric(difftime(current_time, last_update, units = "secs")) > 10) {
                    elapsed <- as.numeric(difftime(current_time, start_time, units = "secs"))
                    percent_done <- b / nboot * 100
                    est_total <- elapsed / percent_done * 100
                    est_remaining <- est_total - elapsed

                    message(sprintf("  %d/%d (%.1f%%) - %d successful, %d errors - %.1f sec elapsed, ~%.1f sec remaining",
                                    b, nboot, percent_done, success_count, error_count,
                                    elapsed, est_remaining))

                    last_update <- current_time
                }
            }

            # Show final statistics
            total_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
            message("\n=== Bootstrap Complete ===")
            message(sprintf("Total time: %.1f seconds (%.2f iterations/sec)",
                            total_time, nboot/total_time))
            message(sprintf("Successful iterations: %d (%.1f%%)",
                            success_count, success_count/nboot*100))
            message(sprintf("Failed iterations: %d (%.1f%%)",
                            error_count, error_count/nboot*100))

            # Calculate percentile CI
            boot_results <- boot_results[!is.na(boot_results)]

            if (length(boot_results) > 0) {
                ci <- quantile(boot_results, c(alpha/2, 1-alpha/2), na.rm=TRUE)
                message(sprintf("Confidence interval (%.1f%%): [%.4f, %.4f]",
                                (1-alpha)*100, ci[1], ci[2]))
                return(list(lower = ci[1], upper = ci[2]))
            } else {
                message("WARNING: No valid bootstrap results obtained. Returning NA.")
                return(list(lower = NA, upper = NA))
            }
        },



        .plot = function(image, ggtheme, theme, ...) {
            # Get state
            state <- image$state
            if (is.null(state) || is.null(state$agreement_matrix) || is.null(state$tests)) {
                return(FALSE)
            }

            # Extract data
            agreement_matrix <- state$agreement_matrix
            tests <- state$tests

            # Safety check
            if (length(tests) < 2) {
                return(FALSE)
            }

            # Create the plot
            tryCatch({
                # Set up plotting parameters
                old_par <- par(no.readonly = TRUE)
                on.exit(par(old_par), add = TRUE)

                # Set margins to accommodate legend (right margin increased)
                par(mar = c(5, 5, 4, 8), xpd = TRUE)

                # Create better color palette - viridis-inspired
                # Using a green to blue color scheme for better differentiation
                colors <- colorRampPalette(c("#440154", "#414487", "#2a788e", "#22a884", "#7ad151", "#fde725"))(100)

                # Create the heatmap
                image(
                    1:nrow(agreement_matrix),
                    1:ncol(agreement_matrix),
                    agreement_matrix,
                    axes = FALSE,
                    xlab = "",
                    ylab = "",
                    main = "Test Agreement Matrix",
                    col = colors,
                    zlim = c(0, 1)
                )

                # Add test names with better formatting
                axis(1, at = 1:length(tests), labels = tests, las = 2, cex.axis = 1.2)
                axis(2, at = 1:length(tests), labels = tests, las = 2, cex.axis = 1.2)

                # Add agreement values with improved visibility
                for (i in 1:nrow(agreement_matrix)) {
                    for (j in 1:ncol(agreement_matrix)) {
                        # Determine text color based on background brightness
                        # Use white text on dark backgrounds, black text on light backgrounds
                        color_idx <- round(agreement_matrix[i, j] * 99) + 1
                        if (color_idx < 50) {
                            text_col <- "white"
                        } else {
                            text_col <- "black"
                        }

                        text(i, j, sprintf("%.2f", agreement_matrix[i, j]),
                             col = text_col, cex = 1.2, font = 2)
                    }
                }

                # Add a color bar legend outside the plot area
                legend_y_pos <- seq(1, length(tests), length.out = 6)
                legend_colors <- colors[seq(1, length(colors), length.out = 5)]
                legend_values <- seq(0, 1, length.out = 5)
                legend_labels <- sprintf("%.1f", legend_values)

                # Place legend to the right of the plot
                legend(length(tests) + 0.5, length(tests)/2,
                       legend = legend_labels,
                       fill = legend_colors,
                       title = "Agreement",
                       bty = "n",  # No box around legend
                       cex = 1.1,
                       y.intersp = 1.2,
                       title.cex = 1.2)

                # Add a subtle box around the plot area
                box(col = "gray50", lwd = 2)

                return(TRUE)
            }, error = function(e) {
                # In case of error, create a simpler plot
                message("Error in plot: ", e$message)

                # Simple fallback plot
                try({
                    plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1),
                         xlab = "", ylab = "", main = "Test Agreement")
                    text(0.5, 0.5, "Agreement data available but plotting failed",
                         cex = 1.2, col = "red")
                    return(TRUE)
                }, silent = TRUE)

                return(FALSE)
            })
        },


        .plot_ggplot = function(image, ggtheme, theme, ...) {
            # Get state
            state <- image$state
            if (is.null(state) || is.null(state$agreement_matrix) || is.null(state$tests)) {
                return(FALSE)
            }

            # Extract data
            agreement_matrix <- state$agreement_matrix
            tests <- state$tests

            # Safety check
            if (length(tests) < 2) {
                return(FALSE)
            }

            # Create the plot using ggplot2
            tryCatch({
                # Check if ggplot2 is available
                if (!requireNamespace("ggplot2", quietly = TRUE)) {
                    stop("Package 'ggplot2' is needed for this plot")
                }

                # Convert matrix to long format for ggplot
                plot_data <- data.frame()
                for (i in 1:nrow(agreement_matrix)) {
                    for (j in 1:ncol(agreement_matrix)) {
                        plot_data <- rbind(plot_data, data.frame(
                            Test1 = factor(tests[i], levels = tests),
                            Test2 = factor(tests[j], levels = tests),
                            Agreement = agreement_matrix[i, j]
                        ))
                    }
                }

                # Create plot with ggplot2
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Test1, y = Test2, fill = Agreement)) +
                    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
                    ggplot2::geom_text(
                        ggplot2::aes(label = sprintf("%.2f", Agreement),
                                     color = ifelse(Agreement > 0.5, "black", "white")),
                        size = 4, fontface = "bold"
                    ) +
                    ggplot2::scale_fill_viridis_c(
                        name = "Agreement",
                        option = "viridis",
                        begin = 0,
                        end = 1,
                        limits = c(0, 1),
                        breaks = seq(0, 1, by = 0.2)
                    ) +
                    ggplot2::scale_color_manual(values = c("white", "black"), guide = "none") +
                    ggplot2::labs(
                        title = "Test Agreement Matrix",
                        x = NULL,
                        y = NULL
                    ) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        axis.text = ggplot2::element_text(size = 11, face = "bold"),
                        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                        legend.position = "right",
                        legend.title = ggplot2::element_text(size = 12, face = "bold"),
                        legend.text = ggplot2::element_text(size = 10),
                        panel.grid = ggplot2::element_blank(),
                        plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
                        panel.border = ggplot2::element_rect(color = "grey70", fill = NA, linewidth = 1)
                    ) +
                    ggplot2::coord_fixed()  # Keep cells square

                print(p)
                return(TRUE)
            }, error = function(e) {
                # In case of error, create a simpler plot
                message("Error in ggplot: ", e$message)

                # Try base R fallback
                try({
                    # Set up plotting parameters
                    old_par <- par(no.readonly = TRUE)
                    on.exit(par(old_par), add = TRUE)

                    # Simple heatmap
                    par(mar = c(5, 5, 4, 5))
                    image(
                        1:nrow(agreement_matrix),
                        1:ncol(agreement_matrix),
                        agreement_matrix,
                        axes = FALSE,
                        xlab = "",
                        ylab = "",
                        main = "Test Agreement Matrix",
                        col = hcl.colors(50, "viridis"),
                        zlim = c(0, 1)
                    )

                    # Add labels
                    axis(1, at = 1:length(tests), labels = tests, las = 2)
                    axis(2, at = 1:length(tests), labels = tests, las = 2)
                    box()

                    return(TRUE)
                }, silent = TRUE)

                return(FALSE)
            })
        }




            )
)
