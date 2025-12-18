#' @title Analysis Without Gold Standard
#' @importFrom R6 R6Class
#' @import jmvcore

# Helper function to escape variable names with special characters for formulas
.escapeVariableNames <- function(var_names) {
    # Check if variable names contain special characters that need escaping
    need_escaping <- grepl("[^a-zA-Z0-9._]", var_names)
    var_names[need_escaping] <- paste0("`", var_names[need_escaping], "`")
    return(var_names)
}

nogoldstandardClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "nogoldstandardClass",
    inherit = nogoldstandardBase,
    private = list(
        .preset_info = NULL,
        .notices = list(),

        .addNotice = function(type, message, name = NULL) {
            if (is.null(name)) {
                name <- paste0('notice', length(private$.notices) + 1)
            }

            notice <- jmvcore::Notice$new(
                options = self$options,
                name = name,
                type = type
            )
            notice$setContent(message)

            # Store with priority for sorting
            priority <- switch(
                as.character(type),
                "1" = 1,  # ERROR
                "2" = 2,  # STRONG_WARNING
                "3" = 3,  # WARNING
                "4" = 4,  # INFO
                3         # Default to WARNING
            )

            private$.notices[[length(private$.notices) + 1]] <- list(
                notice = notice,
                priority = priority
            )
        },

        .insertNotices = function() {
            if (length(private$.notices) == 0) return()

            # Sort by priority (ERROR > STRONG_WARNING > WARNING > INFO)
            notices_sorted <- private$.notices[order(sapply(private$.notices, function(x) x$priority))]

            # Insert in order
            position <- 1
            for (n in notices_sorted) {
                self$results$insert(position, n$notice)
                position <- position + 1
            }
        },

        .resetNotices = function() {
            private$.notices <- list()
        },

        .init = function() {
            # Reset notices for new analysis
            private$.resetNotices()

            # Apply clinical preset if selected
            private$.applyPreset()

            # Show method selection guide
            private$.showMethodGuide()

            # Show welcome message initially
            private$.showWelcomeMessage()
        },

        .populateCrossTab = function(test_data, tests, test_levels) {
            # Create cross-tabulation table showing all possible test combinations
            n_tests <- length(tests)
            if (n_tests < 2) return()
            
            # Generate all possible combinations of test results
            # For each test, create binary result (positive/negative)
            binary_results <- data.frame(matrix(nrow=nrow(test_data), ncol=n_tests))
            names(binary_results) <- tests
            
            for (i in seq_along(tests)) {
                test_name <- tests[[i]]
                pos_level <- test_levels[[i]]
                binary_results[[test_name]] <- as.numeric(test_data[[test_name]] == pos_level)
            }
            
            # Generate all possible patterns (2^n_tests combinations)
            patterns <- expand.grid(replicate(n_tests, 0:1, simplify = FALSE))
            names(patterns) <- tests
            
            # Count occurrences of each pattern
            table_data <- data.frame()
            total_obs <- nrow(binary_results)
            
            for (i in 1:nrow(patterns)) {
                pattern <- patterns[i, ]
                
                # Check which rows match this pattern
                matches <- rep(TRUE, nrow(binary_results))
                for (j in 1:ncol(pattern)) {
                    matches <- matches & (binary_results[[j]] == pattern[[j]])
                }
                
                count <- sum(matches, na.rm = TRUE)
                percentage <- count / total_obs
                
                # Create descriptive label for the pattern
                pattern_labels <- character(ncol(pattern))
                for (j in 1:ncol(pattern)) {
                    test_name <- names(pattern)[j]
                    result <- ifelse(pattern[[j]] == 1, "+", "-")
                    pattern_labels[j] <- paste0(test_name, result)
                }
                combination_label <- paste(pattern_labels, collapse = ", ")
                
                # Add row to table
                table_data <- rbind(table_data, data.frame(
                    test_combination = combination_label,
                    count = count,
                    percentage = percentage,
                    stringsAsFactors = FALSE
                ))
            }
            
            # Sort by count (descending)
            table_data <- table_data[order(table_data$count, decreasing = TRUE), ]
            
            # Populate the results table
            crosstab_table <- self$results$crosstab
            for (i in 1:nrow(table_data)) {
                crosstab_table$addRow(rowKey=paste0("pattern_", i), values=list(
                    test_combination = table_data$test_combination[i],
                    count = table_data$count[i],
                    percentage = table_data$percentage[i]
                ))
            }
        },

        .showWelcomeMessage = function() {
            # Check if we should show instructions
            tests <- private$.getTestVariables()
            
            if (length(tests) < 2) {
                # Get method-specific content
                method_info <- private$.getMethodSpecificContent()
                
                # Show welcome/instruction message
                instructions <- paste0(
                    "<html><head></head><body>",
                    "<div class='instructions' style='background-color: #f8f9fa; padding: 20px; border-radius: 8px; margin: 20px 0;'>",
                    "<h3 style='color: #2e7d32; margin-top: 0;'>🔬 ", .("Analysis Without Gold Standard"), "</h3>",
                    "<p><strong>", .("Analyze diagnostic test performance when no perfect reference test (gold standard) is available."), "</strong></p>",
                    "<p>", .("This analysis uses advanced statistical methods to estimate test sensitivity, specificity, and disease prevalence from imperfect test results."), "</p>",
                    
                    "<h4 style='color: #2e7d32;'>", .("Required Steps:"), "</h4>",
                    "<ol>",
                    "<li><strong>", .("Select Test Variables:"), "</strong> ", .("Choose at least 2 diagnostic tests to analyze"), "</li>",
                    "<li><strong>", .("Define Positive Levels:"), "</strong> ", .("Specify which level represents a positive test result for each test"), "</li>",
                    "<li><strong>", .("Choose Analysis Method:"), "</strong> ", .("Select from available statistical approaches:"), 
                        "<ul>",
                        "<li><strong>", .("Latent Class Analysis:"), "</strong> ", .("Most robust method using mixture models (recommended)"), "</li>",
                        "<li><strong>", .("Bayesian Analysis:"), "</strong> ", .("Bayesian approach with prior distributions"), "</li>",
                        "<li><strong>", .("Composite Reference:"), "</strong> ", .("Uses majority vote as pseudo-gold standard"), "</li>",
                        "<li><strong>", .("All/Any Tests Positive:"), "</strong> ", .("Conservative/liberal reference standards"), "</li>",
                        "</ul>",
                    "</li>",
                    "<li><strong>", .("Optional: Bootstrap CI:"), "</strong> ", .("Enable bootstrap confidence intervals for robust estimates"), "</li>",
                    "</ol>",
                    
                    method_info$background,
                    method_info$references,
                    
                    # Add preset guidance if available
                    if (!is.null(private$.preset_info)) {
                        paste0(
                            "<div style='background: #e8f5e8; padding: 15px; border-radius: 8px; margin: 15px 0; border-left: 4px solid #4caf50;'>",
                            "<h4 style='color: #2e7d32; margin-top: 0;'>🎯 ", .("Active Clinical Preset"), "</h4>",
                            "<p><strong>", .("Scenario"), ":</strong> ", self$options$clinicalPreset, "</p>",
                            "<p><strong>", .("Description"), ":</strong> ", private$.preset_info$description, "</p>",
                            "<p><strong>", .("Guidance"), ":</strong> ", private$.preset_info$guidance, "</p>",
                            "<p><strong>", .("Recommended Method"), ":</strong> ", private$.preset_info$method, "</p>",
                            "</div>"
                        )
                    } else "",
                    
                    "</div></body></html>"
                )

                self$results$instructions$setContent(instructions)
                return(TRUE)  # Instructions shown
            } else {
                # Hide instructions when analysis can proceed
                self$results$instructions$setVisible(FALSE)
                return(FALSE)  # Analysis ready
            }
        },

        .getMethodSpecificContent = function() {
            method <- self$options$method
            
            if (method == "latent_class") {
                background <- paste0(
                    "<h4 style='color: #2e7d32;'>", "Statistical Background: Latent Class Analysis", "</h4>",
                    "<ul>",
                    "<li>", "<strong>Latent Class Analysis (LCA)</strong> assumes a latent (unobserved) disease status and estimates test parameters", "</li>",
                    "<li>", "<strong>Mixture model approach</strong> that identifies two classes: diseased and non-diseased", "</li>",
                    "<li>", "<strong>No identifiability issues</strong> when using 3+ tests or conditional independence assumptions", "</li>",
                    "<li>", "<strong>Most robust method</strong> for estimating sensitivity, specificity, PPV, NPV, and disease prevalence", "</li>",
                    "<li>", "<strong>Handles missing data</strong> and provides model fit statistics (AIC, BIC)", "</li>",
                    "</ul>"
                )
                references <- paste0(
                    "<h4 style='color: #2e7d32;'>", "References:", "</h4>",
                    "<ul>",
                    "<li>", "Hui SL, Walter SD. Estimating the error rates of diagnostic tests. <em>Biometrics</em>. 1980;36(1):167-71.", "</li>",
                    "<li>", "Collins LM, Lanza ST. Latent Class and Latent Transition Analysis. <em>Wiley</em>. 2010.", "</li>",
                    "<li>", "Dendukuri N, Joseph L. Bayesian approaches to modeling the conditional dependence between multiple diagnostic tests. <em>Biometrics</em>. 2001;57(1):158-67.", "</li>",
                    "</ul>"
                )
            } else if (method == "bayesian") {
                background <- paste0(
                    "<h4 style='color: #2e7d32;'>", "Statistical Background: Bayesian Analysis", "</h4>",
                    "<ul>",
                    "<li>", "<strong>Bayesian approach</strong> using prior distributions for sensitivity, specificity, and prevalence", "</li>",
                    "<li>", "<strong>Expectation-Maximization (EM) algorithm</strong> for parameter estimation", "</li>",
                    "<li>", "<strong>Beta priors</strong> for test parameters with informative or non-informative options", "</li>",
                    "<li>", "<strong>Incorporates prior knowledge</strong> about test performance or disease prevalence", "</li>",
                    "<li>", "<strong>Handles uncertainty</strong> through posterior distributions", "</li>",
                    "</ul>"
                )
                references <- paste0(
                    "<h4 style='color: #2e7d32;'>", "References:", "</h4>",
                    "<ul>",
                    "<li>", "Joseph L, Gyorkos TW, Coupal L. Bayesian estimation of disease prevalence and the parameters of diagnostic tests. <em>Am J Epidemiol</em>. 1995;141(3):263-72.", "</li>",
                    "<li>", "Spiegelhalter DJ, Best NG. Bayesian approaches to multiple sources of evidence and uncertainty in complex cost-effectiveness modelling. <em>Stat Med</em>. 2003;22(23):3687-709.", "</li>",
                    "</ul>"
                )
            } else if (method == "composite") {
                background <- paste0(
                    "<h4 style='color: #2e7d32;'>", "Statistical Background: Composite Reference Standard", "</h4>",
                    "<ul>",
                    "<li>", "<strong>Majority vote approach</strong> where consensus of tests serves as pseudo-gold standard", "</li>",
                    "<li>", "<strong>Simple and intuitive</strong> method requiring minimal assumptions", "</li>",
                    "<li>", "<strong>Creates binary reference</strong> from multiple imperfect tests", "</li>",
                    "<li>", "<strong>May underestimate</strong> true test performance due to imperfect reference", "</li>",
                    "<li>", "<strong>Useful as baseline</strong> comparison for other methods", "</li>",
                    "</ul>"
                )
                references <- paste0(
                    "<h4 style='color: #2e7d32;'>", "References:", "</h4>",
                    "<ul>",
                    "<li>", "Alonzo TA, Pepe MS. Using a combination of reference tests to assess the accuracy of a new diagnostic test. <em>Stat Med</em>. 1999;18(22):2987-3003.", "</li>",
                    "<li>", "Reitsma JB, et al. A review of solutions for diagnostic accuracy studies with an imperfect or missing reference standard. <em>J Clin Epidemiol</em>. 2009;62(8):797-806.", "</li>",
                    "</ul>"
                )
            } else if (method == "all_positive") {
                background <- paste0(
                    "<h4 style='color: #2e7d32;'>", "Statistical Background: All Tests Positive Reference", "</h4>",
                    "<ul>",
                    "<li>", "<strong>Conservative approach</strong> where disease is present only if ALL tests are positive", "</li>",
                    "<li>", "<strong>High specificity reference</strong> with potentially low sensitivity", "</li>",
                    "<li>", "<strong>Minimizes false positives</strong> in the reference standard", "</li>",
                    "<li>", "<strong>May underestimate prevalence</strong> and test sensitivity", "</li>",
                    "<li>", "<strong>Useful for highly specific</strong> disease definitions", "</li>",
                    "</ul>"
                )
                references <- paste0(
                    "<h4 style='color: #2e7d32;'>", "References:", "</h4>",
                    "<ul>",
                    "<li>", "Zhou XH, et al. Statistical Methods in Diagnostic Medicine. <em>Wiley</em>. 2011.", "</li>",
                    "<li>", "Pepe MS. The Statistical Evaluation of Medical Tests for Classification and Prediction. <em>Oxford University Press</em>. 2003.", "</li>",
                    "</ul>"
                )
            } else if (method == "any_positive") {
                background <- paste0(
                    "<h4 style='color: #2e7d32;'>", "Statistical Background: Any Test Positive Reference", "</h4>",
                    "<ul>",
                    "<li>", "<strong>Liberal approach</strong> where disease is present if ANY test is positive", "</li>",
                    "<li>", "<strong>High sensitivity reference</strong> with potentially low specificity", "</li>",
                    "<li>", "<strong>Minimizes false negatives</strong> in the reference standard", "</li>",
                    "<li>", "<strong>May overestimate prevalence</strong> and underestimate test specificity", "</li>",
                    "<li>", "<strong>Useful for screening scenarios</strong> where missing cases is costly", "</li>",
                    "</ul>"
                )
                references <- paste0(
                    "<h4 style='color: #2e7d32;'>", "References:", "</h4>",
                    "<ul>",
                    "<li>", "Zhou XH, et al. Statistical Methods in Diagnostic Medicine. <em>Wiley</em>. 2011.", "</li>",
                    "<li>", "Pepe MS. The Statistical Evaluation of Medical Tests for Classification and Prediction. <em>Oxford University Press</em>. 2003.", "</li>",
                    "</ul>"
                )
            } else {
                # Default content
                background <- paste0(
                    "<h4 style='color: #2e7d32;'>", "Statistical Background:", "</h4>",
                    "<ul>",
                    "<li>", "Multiple statistical approaches available for different scenarios", "</li>",
                    "<li>", "Each method has different assumptions and strengths", "</li>",
                    "<li>", "Select the method most appropriate for your research question", "</li>",
                    "</ul>"
                )
                references <- paste0(
                    "<h4 style='color: #2e7d32;'>", "General References:", "</h4>",
                    "<ul>",
                    "<li>", "Reitsma JB, et al. A review of solutions for diagnostic accuracy studies with an imperfect or missing reference standard. <em>J Clin Epidemiol</em>. 2009;62(8):797-806.", "</li>",
                    "</ul>"
                )
            }
            
            return(list(background = background, references = references))
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
            # Reset notices for new analysis run
            private$.resetNotices()

            # Show welcome message if needed and return early if instructions are displayed
            if (private$.showWelcomeMessage()) {
                return()
            }

            if (nrow(self$data) == 0) {
                stop(.('Data contains no rows'))
            }
            
            # Check for required packages early
            if (self$options$method == "latent_class" && !requireNamespace("poLCA", quietly = TRUE)) {
                stop(.("Package 'poLCA' is required for latent class analysis. Please install it with: install.packages('poLCA')"))
            }

            # Get test variables and their positive levels with validation
            tests <- list()
            test_levels <- list()

            for (i in 1:5) {
                var_name <- paste0("test", i)
                level_name <- paste0("test", i, "Positive")

                if (!is.null(self$options[[var_name]])) {
                    test_var <- self$options[[var_name]]
                    pos_level <- self$options[[level_name]]

                    if (!is.null(pos_level)) {
                        # Validate that the variable is a factor
                        if (!is.factor(self$data[[test_var]])) {
                            stop(sprintf(.("Variable '%s' must be a factor"), test_var))
                        }
                        
                        # Validate that the positive level exists in the data
                        if (!pos_level %in% levels(self$data[[test_var]])) {
                            stop(sprintf(.("Level '%s' not found in variable '%s'. Available levels: %s"),
                                       pos_level, test_var, 
                                       paste(levels(self$data[[test_var]]), collapse = ", ")))
                        }
                        
                        tests[[length(tests) + 1]] <- test_var
                        test_levels[[length(test_levels) + 1]] <- pos_level
                    }
                }
            }
            
            # Ensure at least 2 tests are provided
            if (length(tests) < 2) {
                stop(.("At least two tests with positive levels must be specified"))
            }

            # Enforce LCA constraint
            if (self$options$method == "latent_class" && length(tests) < 3) {
                 stop(.("Latent Class Analysis requires at least 3 tests to be statistically identifiable. Please add more tests or select a different method (e.g., Composite Reference)."))
            }

            # Data preparation
            data <- self$data
            test_data <- data[unlist(tests)]
            test_data <- jmvcore::naOmit(test_data)

            if (nrow(test_data) == 0) {
                stop(.('No complete cases available'))
            }

            private$.checkpoint()  # Before data conversion
            
            # Clinical assumption checking
            private$.validateClinicalAssumptions(test_data, tests, self$options$method)
            
            # Convert to binary format for analysis
            binary_data <- data.frame(matrix(nrow=nrow(test_data), ncol=length(tests)))
            names(binary_data) <- unlist(tests)

            for (i in seq_along(tests)) {
                test_name <- tests[[i]]
                pos_level <- test_levels[[i]]

                var <- test_data[[test_name]]
                binary_data[[test_name]] <- as.numeric(var == pos_level)
            }

            private$.checkpoint()  # Before main analysis
            
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

            private$.checkpoint()  # Before result population
            
            # Update results
            if (!is.null(results)) {
                private$.populatePrevalence(results)
                private$.populateTestMetrics(results)
                if (self$options$method == "latent_class") {
                    private$.populateModelFit(results$model)
                }
                # Add cross-tabulation if requested
                private$.populateCrossTab(test_data, tests, test_levels)
                
                # Add clinical summary
                clinical_summary <- private$.generateClinicalSummary(results, self$options$method, tests)
                self$results$clinical_summary$setContent(clinical_summary)
                self$results$clinical_summary$setVisible(TRUE)
            }

            private$.checkpoint()  # Before agreement matrix calculation
            
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
            
            # Populate Agreement Statistics Table
            private$.populateAgreementStats(test_data, tests, test_levels)

            # Insert all notices in priority order (ERROR > STRONG_WARNING > WARNING > INFO)
            private$.insertNotices()
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
                    type = "prevalence",
                    verbose = self$options$verbose
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
            
            # Clear any existing rows to prevent duplicates
            table <- self$results$test_metrics
            table$deleteRows()

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
                        test_index = i,
                        verbose = self$options$verbose
                    )
                    spec_ci <- private$.calculateBootstrapCI(
                        data = results$data,
                        method = self$options$method,
                        nboot = self$options$nboot,
                        alpha = self$options$alpha,
                        type = "specificity",
                        test_index = i,
                        verbose = self$options$verbose
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
                
                # Calculate PPV and NPV
                prevalence <- results$prevalence
                ppv_npv <- private$.calculatePPVNPV(sensitivity, specificity, prevalence)

                # Add row to cleared table
                table$addRow(rowKey=tests[i], values=list(
                    test = tests[i],
                    sensitivity = sensitivity,
                    specificity = specificity,
                    sens_ci_lower = sens_ci$lower,
                    sens_ci_upper = sens_ci$upper,
                    spec_ci_lower = spec_ci$lower,
                    spec_ci_upper = spec_ci$upper,
                    ppv = ppv_npv$ppv,
                    npv = ppv_npv$npv
                ))
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
                stop(.("Package 'poLCA' is required for latent class analysis"))
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

            # Create formula with escaped variable names
            var_names <- names(lca_data)
            escaped_var_names <- .escapeVariableNames(var_names)
            f <- stats::as.formula(paste("cbind(", paste(escaped_var_names, collapse=","), ")~1"))

            # Run LCA with more starts to ensure global optimum
            best_model <- NULL
            best_llik <- -Inf
            n_starts <- 30  # Increased from 10 for better convergence
            
            if (self$options$verbose) {
                message(sprintf(.("Running Latent Class Analysis with %d random starts..."), n_starts))
            }

            for (start in 1:n_starts) {
                # Checkpoint periodically during LCA iterations
                if (start %% 10 == 1) {  # Every 10 starts
                    private$.checkpoint(flush = FALSE)  # Poll for changes only
                }
                
                # Early termination for convergence
                if (start > 10 && best_llik > -Inf) {
                    # Check if we've had multiple recent starts with no improvement
                    if (start > 20 && (best_llik - (-Inf)) > 0.001) {
                        if (self$options$verbose) {
                            message(sprintf(.("Early termination: convergence achieved after %d starts"), start - 1))
                        }
                        break
                    }
                }
                
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
                        improvement <- model$llik - best_llik
                        best_model <- model
                        best_llik <- model$llik
                        
                        # Report significant improvement
                        if (improvement > 0.1 && self$options$verbose) {
                            message(sprintf(.("Start %d: Improvement found (LL = %.3f)"), start, best_llik))
                        }
                    }

                }, error = function(e) {
                    # Continue to next start
                })
            }

            if (is.null(best_model)) {
                stop(.("LCA model fitting failed after all attempts. Try a different method or check your data."))
            }
            
            # Add convergence warning if log-likelihood is suspiciously low
            if (best_llik < -1e10) {
                if (self$options$verbose) {
                    warning(.("LCA model may not have converged properly. Results should be interpreted with caution."))
                }
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
                # Checkpoint periodically during EM iterations
                if (iter %% 20 == 1) {  # Every 20 iterations
                    private$.checkpoint(flush = FALSE)  # Poll for changes only
                }
                
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


        .calculateBootstrapCI = function(data, method, nboot, alpha, type, test_index = NULL, verbose = FALSE) {
            # Simple bootstrap implementation with progress indicators
            n <- nrow(data)
            boot_results <- numeric(nboot)

            # Show starting message only if verbose
            if (verbose) {
                message(.("\n=== Bootstrap Analysis ==="))
                message(sprintf(.("Starting bootstrap with %d iterations for %s method"), nboot, method))
                message(sprintf(.("Estimating confidence intervals for %s"), type))
                if (!is.null(test_index)) {
                    message(sprintf(.("Test index: %d"), test_index))
                }
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

                # Checkpoint periodically during bootstrap
                if (b %% 50 == 1) {
                    private$.checkpoint(flush = FALSE)  # Periodic bootstrap checkpoint
                }
                
                # Show progress updates only if verbose
                if (verbose) {
                    current_time <- Sys.time()
                    if (b %% update_interval == 0 || b == nboot ||
                        as.numeric(difftime(current_time, last_update, units = "secs")) > 10) {
                        elapsed <- as.numeric(difftime(current_time, start_time, units = "secs"))
                        percent_done <- b / nboot * 100
                        est_total <- elapsed / percent_done * 100
                        est_remaining <- est_total - elapsed

                        message(sprintf(.("  %d/%d (%.1f%%) - %d successful, %d errors - %.1f sec elapsed, ~%.1f sec remaining"),
                                        b, nboot, percent_done, success_count, error_count,
                                        elapsed, est_remaining))

                        last_update <- current_time
                    }
                }
            }

            # Show final statistics only if verbose
            if (verbose) {
                total_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
                message(.("\n=== Bootstrap Complete ==="))
                message(sprintf(.("Total time: %.1f seconds (%.2f iterations/sec)"),
                                total_time, nboot/total_time))
                message(sprintf(.("Successful iterations: %d (%.1f%%)"),
                                success_count, success_count/nboot*100))
                message(sprintf(.("Failed iterations: %d (%.1f%%)"),
                                error_count, error_count/nboot*100))
            }
            
            if (error_count > 0) {
                 private$.addNotice(
                     jmvcore::NoticeType$WARNING,
                     sprintf(.("Bootstrap: %d sample(s) failed to converge or produced errors (%d%%). CIs may be affected."), error_count, round(error_count/nboot*100)),
                     'bootstrapConvergence'
                 )
            }

            # Calculate percentile CI
            boot_results <- boot_results[!is.na(boot_results)]

            if (length(boot_results) > 0) {
                ci <- quantile(boot_results, c(alpha/2, 1-alpha/2), na.rm=TRUE)
                if (verbose) {
                    message(sprintf(.("Confidence interval (%.1f%%): [%.4f, %.4f]"),
                                    (1-alpha)*100, ci[1], ci[2]))
                }
                return(list(lower = ci[1], upper = ci[2]))
            } else {
                if (verbose) {
                    message(.("WARNING: No valid bootstrap results obtained. Returning NA."))
                }
                return(list(lower = NA, upper = NA))
            }
        },
        

        
        .calculatePPVNPV = function(sensitivity, specificity, prevalence) {
            # Calculate Positive Predictive Value (PPV) and Negative Predictive Value (NPV)
            # Using Bayes' theorem
            
            # PPV = (sensitivity * prevalence) / ((sensitivity * prevalence) + ((1 - specificity) * (1 - prevalence)))
            ppv_numerator <- sensitivity * prevalence
            ppv_denominator <- ppv_numerator + ((1 - specificity) * (1 - prevalence))
            ppv <- if (ppv_denominator > 0) ppv_numerator / ppv_denominator else NA
            
            # NPV = (specificity * (1 - prevalence)) / (((1 - sensitivity) * prevalence) + (specificity * (1 - prevalence)))
            npv_numerator <- specificity * (1 - prevalence)
            npv_denominator <- ((1 - sensitivity) * prevalence) + npv_numerator
            npv <- if (npv_denominator > 0) npv_numerator / npv_denominator else NA
            
            return(list(ppv = ppv, npv = npv))
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
                message(.("Error in plot: "), e$message)

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
                    # Fallback to base R plot
                    return(private$.plot(image, ggtheme, theme, ...))
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
                message(.("Error in ggplot: "), e$message)

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
                        main = .("Test Agreement Matrix"),
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
        },

        .validateClinicalAssumptions = function(data, tests, method) {
            n_obs <- nrow(data)
            n_tests <- length(tests)

            # Sample size warnings based on method
            if (method == "latent_class" && n_obs < 100) {
                private$.addNotice(
                    jmvcore::NoticeType$STRONG_WARNING,
                    sprintf(.("LCA typically requires 100+ observations for stable results. Current N = %d. Consider using composite reference method for smaller samples."), n_obs),
                    'lcaSampleSize'
                )
            }

            if (method == "bayesian" && n_obs < 50) {
                private$.addNotice(
                    jmvcore::NoticeType$STRONG_WARNING,
                    sprintf(.("Bayesian analysis may be unstable with N < 50. Current N = %d. Consider collecting more data."), n_obs),
                    'bayesianSampleSize'
                )
            }
            
            # Check test result distributions
            for (i in seq_along(tests)) {
                test_name <- tests[[i]]
                test_values <- table(data[[test_name]])

                if (any(test_values < 5)) {
                    private$.addNotice(
                        jmvcore::NoticeType$WARNING,
                        sprintf(.("Test '%s' has categories with <5 observations. Results may be unstable. Consider combining categories if clinically appropriate."), test_name),
                        paste0('smallCategories_', i)
                    )
                }

                # Check for extreme imbalances
                min_prop <- min(test_values) / sum(test_values)
                if (min_prop < 0.05) {
                    private$.addNotice(
                        jmvcore::NoticeType$WARNING,
                        sprintf(.("Test '%s' shows extreme imbalance (minority category %.1f%%). This may affect parameter estimation."), test_name, min_prop * 100),
                        paste0('extremeImbalance_', i)
                    )
                }
            }
            
            # Method-specific warnings
            if (method == "latent_class" && n_tests < 3) {
                 # checking this earlier in .run now, but good to keep as message if we relax allow
                 private$.addNotice(
                     jmvcore::NoticeType$WARNING,
                     .("LCA with only 2 tests is under-identified."),
                     'lcaUnderIdentified'
                 )
            }

            if (method == "composite" && n_tests %% 2 == 0) {
                private$.addNotice(
                    jmvcore::NoticeType$WARNING,
                    .("Composite reference with even number of tests may result in ties. Consider using an odd number of tests or a different method."),
                    'compositeTies'
                )
            }

            # Clinical context message
            if (self$options$verbose) {
                private$.addNotice(
                    jmvcore::NoticeType$INFO,
                    sprintf(.("Clinical validation: %d tests analyzed with N=%d using %s method"), n_tests, n_obs, method),
                    'clinicalValidation'
                )
            }
        },

        .showMethodGuide = function() {
            # Create comprehensive method selection guide in HTML
            guide_html <- paste0(
                "<div style='background: #f8f9fa; padding: 20px; border-radius: 8px; margin: 15px 0; border-left: 4px solid #007bff;'>",
                "<h3 style='color: #007bff; margin-top: 0;'>📖 ", .("Method Selection Guide"), "</h3>",
                
                "<div style='margin: 15px 0; padding: 15px; background: #e8f5e8; border-radius: 5px;'>",
                "<h4 style='color: #2e7d32; margin-top: 0;'>🏆 ", .("Latent Class Analysis (Recommended)"), "</h4>",
                "<p><strong>", .("Description"), ":</strong> ", .("Most robust method using mixture models. Estimates disease prevalence and test parameters simultaneously."), "</p>",
                "<p><strong>", .("Best for"), ":</strong> ", .("Diagnostic validation studies with 3+ tests and N≥100"), "</p>",
                "<p><strong>", .("Strengths"), ":</strong> ", .("Handles conditional dependence, provides model fit statistics, most statistically rigorous"), "</p>",
                "</div>",
                
                "<div style='margin: 15px 0; padding: 15px; background: #e3f2fd; border-radius: 5px;'>",
                "<h4 style='color: #1565c0; margin-top: 0;'>📊 ", .("Bayesian Analysis"), "</h4>",
                "<p><strong>", .("Description"), ":</strong> ", .("Incorporates prior knowledge about test performance using Bayesian methods."), "</p>",
                "<p><strong>", .("Best for"), ":</strong> ", .("Studies where you have prior information about expected sensitivity/specificity"), "</p>",
                "<p><strong>", .("Strengths"), ":</strong> ", .("Uses prior knowledge, handles uncertainty well, good for smaller samples"), "</p>",
                "</div>",
                
                "<div style='margin: 15px 0; padding: 15px; background: #fff3e0; border-radius: 5px;'>",
                "<h4 style='color: #ef6c00; margin-top: 0;'>🗳️ ", .("Composite Reference"), "</h4>",
                "<p><strong>", .("Description"), ":</strong> ", .("Uses majority vote of available tests as pseudo-gold standard."), "</p>",
                "<p><strong>", .("Best for"), ":</strong> ", .("Inter-rater agreement studies with 3+ tests, exploratory analysis"), "</p>",
                "<p><strong>", .("Strengths"), ":</strong> ", .("Simple and intuitive, requires minimal assumptions, good starting point"), "</p>",
                "</div>",
                
                "<div style='margin: 15px 0; padding: 15px; background: #fce4ec; border-radius: 5px;'>",
                "<h4 style='color: #c2185b; margin-top: 0;'>🔒 ", .("All Tests Positive"), "</h4>",
                "<p><strong>", .("Description"), ":</strong> ", .("Conservative approach - disease present only if ALL tests are positive."), "</p>",
                "<p><strong>", .("Best for"), ":</strong> ", .("Highly specific diagnoses where false positives are very costly"), "</p>",
                "<p><strong>", .("Strengths"), ":</strong> ", .("High specificity reference, minimizes false positives"), "</p>",
                "</div>",
                
                "<div style='margin: 15px 0; padding: 15px; background: #e8f5e8; border-radius: 5px;'>",
                "<h4 style='color: #388e3c; margin-top: 0;'>🔓 ", .("Any Test Positive"), "</h4>",
                "<p><strong>", .("Description"), ":</strong> ", .("Liberal approach - disease present if ANY test is positive."), "</p>",
                "<p><strong>", .("Best for"), ":</strong> ", .("Population screening scenarios where missing cases is costly"), "</p>",
                "<p><strong>", .("Strengths"), ":</strong> ", .("High sensitivity reference, minimizes false negatives"), "</p>",
                "</div>",
                
                "<div style='margin: 15px 0; padding: 10px; background: #fff8e1; border-radius: 5px; border-left: 3px solid #ffb300;'>",
                "<h4 style='color: #e65100; margin-top: 0;'>💡 ", .("Selection Tips"), "</h4>",
                "<ul>",
                "<li>", .("Start with Latent Class Analysis for most diagnostic studies"), "</li>",
                "<li>", .("Use Composite Reference for quick exploratory analysis"), "</li>",
                "<li>", .("Choose All/Any Tests Positive based on clinical consequences of errors"), "</li>",
                "<li>", .("Consider Bayesian if you have strong prior information"), "</li>",
                "</ul>",
                "</div>",
                
                "</div>"
            )
            
            # Set the method guide content
            self$results$method_guide$setContent(guide_html)
        },

        .applyPreset = function() {
            preset <- self$options$clinicalPreset
            
            if (preset == "none") return()
            
            # Define preset configurations with descriptions
            presets <- list(
                diagnostic_validation = list(
                    method = "latent_class",
                    bootstrap = TRUE,
                    nboot = 1000,
                    alpha = 0.05,
                    verbose = FALSE,
                    description = .("Recommended for validating new diagnostic tests against existing standards"),
                    guidance = .("Use when evaluating new biomarkers or diagnostic technologies")
                ),
                pathology_agreement = list(
                    method = "composite",
                    bootstrap = FALSE,
                    nboot = 500,
                    alpha = 0.05,
                    verbose = FALSE,
                    description = .("Optimal for assessing agreement between pathologists or observers"),
                    guidance = .("Use for inter-rater reliability studies in pathology")
                ),
                tumor_markers = list(
                    method = "latent_class",
                    bootstrap = TRUE,
                    nboot = 1000,
                    alpha = 0.05,
                    verbose = TRUE,
                    description = .("Specialized for tumor marker validation studies"),
                    guidance = .("Use when validating cancer biomarkers or prognostic tests")
                ),
                screening_evaluation = list(
                    method = "any_positive",
                    bootstrap = TRUE,
                    nboot = 500,
                    alpha = 0.05,
                    verbose = FALSE,
                    description = .("Designed for population screening test evaluation"),
                    guidance = .("Use for evaluating screening programs with multiple tests")
                )
            )
            
            # Store preset info for welcome message integration
            if (preset %in% names(presets)) {
                preset_config <- presets[[preset]]
                
                # Store for use in welcome message
                private$.preset_info <- preset_config
                
                if (self$options$verbose) {
                    message(sprintf(.("Applied clinical preset: %s"), preset))
                    message(sprintf(.("Recommended method: %s"), preset_config$method))
                    message(preset_config$guidance)
                }
            }
        },



        .generateClinicalSummary = function(results, method, tests) {
            if (is.null(results)) return("")
            
            n_tests <- length(tests)
            prev_pct <- sprintf("%.1f%%", results$prevalence * 100)
            
            # Generate interpretation based on prevalence
            prev_interp <- if (results$prevalence < 0.10) {
                .("Low prevalence setting - high NPV expected, focus on ruling out disease")
            } else if (results$prevalence > 0.30) {
                .("High prevalence setting - high PPV expected, focus on confirming disease")
            } else {
                .("Moderate prevalence setting - balanced diagnostic performance")
            }
            
            # Sensitivity range
            sens_min <- sprintf("%.1f%%", min(results$sensitivities) * 100)
            sens_max <- sprintf("%.1f%%", max(results$sensitivities) * 100)
            
            summary_html <- paste0(
                "<div class='clinical-summary' style='background: #f0f8ff; padding: 15px; border-radius: 8px; margin: 10px 0;'>",
                "<h4 style='color: #1565c0; margin-top: 0;'>📋 ", .("Clinical Summary"), "</h4>",
                "<p><strong>", .("Analysis:"), "</strong> ", sprintf(.("No gold standard analysis using %s method"), method), "</p>",
                "<p><strong>", .("Tests analyzed:"), "</strong> ", paste(tests, collapse = ", "), " (N=", n_tests, ")</p>",
                "<p><strong>", .("Disease prevalence:"), "</strong> ", prev_pct, "</p>",
                "<p><strong>", .("Test sensitivities:"), "</strong> ", .("Range from"), " ", sens_min, " ", .("to"), " ", sens_max, "</p>",
                "<p><strong>", .("Clinical interpretation:"), "</strong> ", prev_interp, "</p>",
                "</div>"
            )
            
            return(summary_html)
        },

        .populateAgreementStats = function(test_data, tests, test_levels) {
            # Calculate pairwise Cohen's Kappa
            table <- self$results$agreement_stats
            n_tests <- length(tests)
            
            if (n_tests < 2) return()
            
            # Helper function for Cohen's Kappa
            calculate_kappa <- function(var1, var2) {
                # Create confusion matrix
                tbl <- table(var1, var2)
                
                # Check if tbl is valid (needs to be square if possible, but for Kappa we need matched levels)
                # Ensure we have 2x2 table even if some levels are missing
                levels_union <- union(levels(var1), levels(var2))
                tbl_full <- table(factor(var1, levels=levels_union), factor(var2, levels=levels_union))
                
                n <- sum(tbl_full)
                p_o <- sum(diag(tbl_full)) / n
                
                row_sums <- rowSums(tbl_full)
                col_sums <- colSums(tbl_full)
                p_e <- sum(row_sums * col_sums) / (n^2)
                
                kappa <- (p_o - p_e) / (1 - p_e)
                
                # Standard error and p-value
                se_kappa <- sqrt((p_o * (1 - p_o)) / (n * (1 - p_e)^2)) # Approximation
                z_score <- kappa / se_kappa
                p_value <- 2 * (1 - pnorm(abs(z_score)))
                
                return(list(kappa = kappa, p_value = p_value, agreement = p_o))
            }
            
            for (i in 1:(n_tests-1)) {
                for (j in (i+1):n_tests) {
                    test1 <- tests[[i]]
                    test2 <- tests[[j]]
                    
                    # Ensure binary/factor conversion matches what we used
                    # Original data is factors
                    res <- calculate_kappa(test_data[[test1]], test_data[[test2]])
                    
                    table$addRow(rowKey=paste0(test1, "_", test2), values=list(
                        test_pair = paste0(test1, " vs ", test2),
                        kappa = res$kappa,
                        p_value = res$p_value,
                        agreement = res$agreement
                    ))
                }
            }
        }




            )
)
