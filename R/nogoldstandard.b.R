#' @title Analysis Without Gold Standard
#' @importFrom R6 R6Class
#' @import jmvcore
#' @noRd
NULL

nogoldstandardClass <- if (requireNamespace("jmvcore")) {
    R6::R6Class(
        "nogoldstandardClass",
        inherit = nogoldstandardBase,
        private = list(
            .preset_info = NULL,

            # Notice collection helpers. A single Preformatted (plain-text) output item:
            # avoids BOTH the jmvcore::Notice serialization error from
            # self$results$insert(999, Notice) AND any HTML in notices (project convention:
            # notice content must be plain text). ====
            .noticeList = list(),
            # One bootstrap pass per run, shared by prevalence and every test metric.
            .boot_cache = NULL,

            # Lines collected for the Analysis Diagnostics panel. `verbose` previously did
            # almost nothing a jamovi user could see: 17 of its 18 effects were message()
            # calls, which go to the R console that jamovi never shows.
            .diagLines = character(0),

            # Set while bootstrap replicates run: they call the same estimators as the main
            # fit, and one diagnostic line per replicate would bury the useful ones.
            .diagSuppressed = FALSE,

            # A results item that may not exist in the compiled .h.R yet: jmvcore raises
            # rather than returning NULL, so a bare self$results$x would crash every run
            # between the .r.yaml edit and the next jmvtools::prepare().
            .resultsItem = function(name) tryCatch(self$results[[name]], error = function(e) NULL),

            .diag = function(...) {
                if (!isTRUE(self$options$verbose) || isTRUE(private$.diagSuppressed))
                    return(invisible(NULL))
                private$.diagLines <- c(private$.diagLines, paste0(...))
                invisible(NULL)
            },

            .renderDiagnostics = function() {
                item <- private$.resultsItem("diagnostics")
                if (is.null(item) || !isTRUE(self$options$verbose)) return()
                if (length(private$.diagLines) == 0) return()
                item$setContent(paste(private$.diagLines, collapse = "\n"))
            },

            .addNotice = function(type, title, content) {
                private$.noticeList[[length(private$.noticeList) + 1]] <- list(
                    type = type,
                    title = title,
                    content = content
                )
                # Render immediately so early-return validation aborts still display the notice
                private$.renderNotices()
            },
            .renderNotices = function() {
                if (length(private$.noticeList) == 0) {
                    self$results$notices$setContent("")
                    return()
                }

                # Plain text only - notices avoid HTML by project convention; the Preformatted
                # output item renders this literally (no markup, no injection surface).
                blocks <- vapply(private$.noticeList, function(notice) {
                    prefix <- switch(notice$type,
                        ERROR          = "ERROR: ",
                        STRONG_WARNING = "WARNING: ",
                        WARNING        = "WARNING: ",
                        ""
                    )
                    paste0(prefix, notice$title, "\n", notice$content)
                }, character(1))

                self$results$notices$setContent(paste(blocks, collapse = "\n\n"))
            },

            # TODO [meddecide audit 2026-05-14] - see docs/audit/MODULE_AUDIT_REPORT_20260514-1847.md
            #   [SECURITY/C1-HIGH] FIXED 2026-05-14 - formula now built with jmvcore::composeTerms +
            #     jmvcore::asFormula (allow-list parse-time guard) at .runLCA (~L642), replacing
            #     the former stats::as.formula(paste(...)) string-built formula. The old
            #     .escapeVariableNames helper has been removed; only the asFormula path remains.
            #   [CLINICAL-SAFETY] add STRONG_WARNING when LCA convergence < ~25% of n_starts in .runLCA
            #   [CLINICAL-SAFETY] add STRONG_WARNING when total cases < 100 (Hui-Walter assumption)
            #   [hygiene/notices] custom private$.addNotice parallels jmvcore::Notice - consolidate
            #   [hygiene/jmvcore] FIXED 2026-05-29 - 9 stop(.()) → jmvcore::reject(.()) migrations at L359/L364/L382/L387/L400/L405/L414/L638/L718
            #   [hygiene/jmvcore] FIXED - all na.omit() on jamovi-attributed frames migrated to jmvcore::naOmit (L411)
            #   [statistical-validation] /review-function nogoldstandard - Hui-Walter + Joseph-Gyorkos parity
            #   [i18n] 95 .() wraps but no .po catalog; bootstrap jamovi/i18n/
            #   [testing] no tests/testthat/test-nogoldstandard.R

            .init = function() {
                # Reset notices for new analysis
                private$.noticeList <- list()

                # NOTE: .applyPreset() is called from .run(), not here. .run() resets
                # private$.noticeList at its top, so a notice added during .init() is wiped
                # before anything is rendered -- which is why the preset advisory never
                # reached the user.

                # Show method selection guide
                private$.showMethodGuide()

                # Show welcome message initially
                private$.showWelcomeMessage()
            },
            .populateCrossTab = function(test_data, tests, test_levels) {
                # Create cross-tabulation table showing all possible test combinations
                n_tests <- length(tests)
                if (n_tests < 2) {
                    return()
                }

                # Generate all possible combinations of test results
                # For each test, create binary result (positive/negative)
                binary_results <- data.frame(matrix(nrow = nrow(test_data), ncol = n_tests))
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

                for (i in seq_len(nrow(patterns))) {
                    pattern <- patterns[i, ]

                    # Check which rows match this pattern
                    matches <- rep(TRUE, nrow(binary_results))
                    for (j in seq_len(ncol(pattern))) {
                        matches <- matches & (binary_results[[j]] == pattern[[j]])
                    }

                    count <- sum(matches, na.rm = TRUE)
                    percentage <- count / total_obs

                    # Create descriptive label for the pattern
                    pattern_labels <- character(ncol(pattern))
                    for (j in seq_len(ncol(pattern))) {
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
                crosstab_table$deleteRows() # Clear existing rows to prevent duplicates on re-run
                for (i in seq_len(nrow(table_data))) {
                    crosstab_table$addRow(rowKey = paste0("pattern_", i), values = list(
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
                        "<div class='instructions' style='background-color: rgba(138, 155, 172, 0.06); padding: 20px; border-radius: 8px; margin: 20px 0; color: inherit;'>",
                        "<h3 style='color: #2e7d32; margin-top: 0;'> ", .("Analysis Without Gold Standard"), "</h3>",
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
                                "<div style='background-color: rgba(33, 159, 33, 0.1); padding: 15px; border-radius: 8px; margin: 15px 0; border-left: 4px solid #4caf50; color: inherit;'>",
                                "<h4 style='color: #2e7d32; margin-top: 0;'> ", .("Active Clinical Preset"), "</h4>",
                                "<p><strong>", .("Scenario"), ":</strong> ", self$options$clinicalPreset, "</p>",
                                "<p><strong>", .("Description"), ":</strong> ", private$.preset_info$description, "</p>",
                                "<p><strong>", .("Guidance"), ":</strong> ", private$.preset_info$guidance, "</p>",
                                "<p><strong>", .("Recommended Method"), ":</strong> ", private$.preset_info$method, "</p>",
                                "</div>"
                            )
                        } else {
                            ""
                        },
                        "</div></body></html>"
                    )

                    self$results$instructions$setContent(instructions)
                    return(TRUE) # Instructions shown
                } else {
                    # Hide instructions when analysis can proceed
                    self$results$instructions$setVisible(FALSE)
                    return(FALSE) # Analysis ready
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
                        "<li>", "<strong>Requires 3 or more tests.</strong> With 2 tests the model has more parameters than degrees of freedom and is not identified; with exactly 3 it is just-identified, so the fit statistics cannot test it", "</li>",
                        "<li>", "<strong>Assumes conditional independence:</strong> that the tests make errors independently within the diseased and non-diseased groups. This model does NOT relax that assumption. Tests measuring the same biology (two stains for one antigen, two readers of one slide) violate it, which inflates the estimated accuracy", "</li>",
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
                private$.noticeList <- list()

                private$.diagLines <- character(0)

                # Preset advisory: after the reset above, so it survives to be rendered.
                private$.applyPreset()

                # Show welcome message if needed and return early if instructions are displayed
                if (private$.showWelcomeMessage()) {
                    return()
                }

                if (nrow(self$data) == 0) {
                    jmvcore::reject(.("Data contains no rows"))
                }

                # Check for required packages early
                if (self$options$method == "latent_class" && !requireNamespace("poLCA", quietly = TRUE)) {
                    jmvcore::reject(.("Package 'poLCA' is required for latent class analysis. Please install it with: install.packages('poLCA')"))
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
                                jmvcore::reject(.("Variable '{}' must be a factor"),
                                                code = NULL, test_var)
                            }

                            # Validate that the positive level exists in the data
                            if (!pos_level %in% levels(self$data[[test_var]])) {
                                jmvcore::reject(
                                    .("Level '{}' not found in variable '{}'. Available levels: {}"),
                                    code = NULL,
                                    pos_level, test_var,
                                    paste(levels(self$data[[test_var]]), collapse = ", ")
                                )
                            }

                            tests[[length(tests) + 1]] <- test_var
                            test_levels[[length(test_levels) + 1]] <- pos_level
                        }
                    }
                }

                # Ensure at least 2 tests are provided
                if (length(tests) < 2) {
                    jmvcore::reject(.("At least two tests with positive levels must be specified"))
                }

                # Enforce LCA constraint
                if (self$options$method == "latent_class" && length(tests) < 3) {
                    jmvcore::reject(.("Latent Class Analysis requires at least 3 tests to be statistically identifiable. Please add more tests or select a different method (e.g., Composite Reference)."))
                }

                # Data preparation
                data <- self$data
                test_data <- data[unlist(tests)]
                n_before <- nrow(test_data)
                test_data <- jmvcore::naOmit(test_data)
                n_after <- nrow(test_data)

                # Neither the analysed N nor the number excluded appeared anywhere in the
                # output: every estimate below was computed on the complete cases while the
                # user still saw the dataset's full size.
                if (n_after < n_before) {
                    private$.addNotice(
                        "WARNING",
                        sprintf("Excluded %d case(s) with missing test results", n_before - n_after),
                        sprintf("This analysis uses the %d of %d cases (%.1f%%) with a result recorded for every selected test. Latent class and composite estimates assume the excluded cases are missing at random; if a test is more often missing when it would have been positive, the estimates below are biased.",
                                n_after, n_before, 100 * n_after / n_before)
                    )
                } else {
                    private$.addNotice(
                        "INFO",
                        sprintf("Analysing %d cases", n_after),
                        sprintf("All %d cases have a result for every selected test.", n_after)
                    )
                }

                private$.diag("Analysis method : ", self$options$method)
                private$.diag("Tests           : ", paste(unlist(tests), collapse = ", "))
                private$.diag("Cases supplied  : ", n_before)
                private$.diag("Cases analysed  : ", n_after,
                              if (n_after < n_before)
                                  sprintf("  (%d excluded for missing test results)", n_before - n_after)
                              else "  (no exclusions)")

                if (nrow(test_data) == 0) {
                    jmvcore::reject(.("No complete cases available"))
                }

                private$.checkpoint() # Before data conversion

                # Clinical assumption checking
                private$.validateClinicalAssumptions(test_data, tests, self$options$method)

                # Convert to binary format for analysis
                binary_data <- data.frame(matrix(nrow = nrow(test_data), ncol = length(tests)))
                names(binary_data) <- unlist(tests)

                for (i in seq_along(tests)) {
                    test_name <- tests[[i]]
                    pos_level <- test_levels[[i]]

                    var <- test_data[[test_name]]
                    binary_data[[test_name]] <- as.numeric(var == pos_level)
                }

                private$.checkpoint() # Before main analysis

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

                # .runBayesian returns a `converged` flag and .runLCA warns only via
                # warning() behind the `verbose` option -- neither reached a jamovi user, who
                # never sees the R console. A non-converged fit is exactly the case where the
                # numbers should not be trusted, so say so where they will read it.
                if (isFALSE(results$converged)) {
                    private$.addNotice(
                        "STRONG_WARNING",
                        "The estimation did not converge",
                        sprintf("The EM algorithm reached its iteration limit (%s) without the parameter estimates settling. The sensitivities and specificities below are wherever the algorithm happened to stop, not a fitted solution, and should not be reported. Try a different method, or check whether the tests are nearly perfectly agreeing or nearly independent of one another.",
                                if (is.null(results$iterations)) "100" else as.character(results$iterations))
                    )
                }


                # One bootstrap pass, reused by every table below.
                private$.boot_cache <- NULL
                if (isTRUE(self$options$bootstrap) && !is.null(results)) {
                    private$.boot_cache <- private$.bootstrapAll(
                        data = results$data,
                        method = self$options$method,
                        nboot = self$options$nboot,
                        verbose = self$options$verbose,
                        warm_start = if (identical(self$options$method, "latent_class"))
                            results$model$probs else NULL
                    )
                }

                private$.checkpoint() # Before result population

                # Update results
                if (!is.null(results)) {
                    private$.populatePrevalence(results)
                    private$.populateTestMetrics(results)
                    if (self$options$method == "latent_class") {
                        private$.populateModelFit(results$model)
                        private$.populateConditionalDependence(results$model, tests)
                    }
                    # Add cross-tabulation if requested
                    private$.populateCrossTab(test_data, tests, test_levels)

                    # Add clinical summary
                    clinical_summary <- private$.generateClinicalSummary(results, self$options$method, tests)
                    self$results$clinical_summary$setContent(clinical_summary)
                    self$results$clinical_summary$setVisible(TRUE)
                }

                private$.checkpoint() # Before agreement matrix calculation

                # Prepare data for the plot
                agreement_matrix <- matrix(0, ncol = length(tests), nrow = length(tests))
                colnames(agreement_matrix) <- unlist(tests)
                rownames(agreement_matrix) <- unlist(tests)

                for (i in seq_along(tests)) {
                    for (j in seq_along(tests)) {
                        test1_pos <- test_data[[tests[[i]]]] == test_levels[[i]]
                        test2_pos <- test_data[[tests[[j]]]] == test_levels[[j]]
                        agreement_matrix[i, j] <- mean(test1_pos == test2_pos, na.rm = TRUE)
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

                # Render collected notices (plain-text Preformatted output)
                private$.renderDiagnostics()
                private$.renderNotices()
            },
            .populatePrevalence = function(results) {
                if (is.null(results)) {
                    return()
                }

                prevalence <- results$prevalence

                # Calculate confidence intervals if bootstrap is enabled
                if (self$options$bootstrap) {
                    ci <- private$.bootCI(private$.boot_cache$prevalence, self$options$alpha)
                    ci_lower <- ci$lower
                    ci_upper <- ci$upper
                } else {
                    # Simple normal approximation
                    n <- nrow(results$data)
                    se <- sqrt(prevalence * (1 - prevalence) / n)
                    z <- qnorm(1 - self$options$alpha / 2)
                    ci_lower <- max(0, prevalence - z * se)
                    ci_upper <- min(1, prevalence + z * se)
                }

                table <- self$results$prevalence
                table$setRow(rowNo = 1, values = list(
                    estimate = prevalence,
                    ci_lower = ci_lower,
                    ci_upper = ci_upper
                ))
            },
            .populateTestMetrics = function(results) {
                if (is.null(results)) {
                    return()
                }

                # Use the analyzed test names (aligned with results$sensitivities /
                # results$specificities). results$data columns are the filtered tests
                # that actually entered the analysis (those with BOTH a variable and a
                # positive level set). .getTestVariables() returns every selected test
                # variable even when its positive level is unset, which would misalign
                # labels with the metric vector and add spurious NA rows.
                tests <- names(results$data)

                # Clear any existing rows to prevent duplicates
                table <- self$results$test_metrics
                table$deleteRows()

                # The columns are headed "Lower CI"/"Upper CI" whatever produced them and at
                # whatever level, so an 80% Wald interval was indistinguishable from a 95%
                # bootstrap percentile interval. State both.
                conf_pct <- 100 * (1 - self$options$alpha)
                table$setNote(
                    "ci_provenance",
                    if (isTRUE(self$options$bootstrap))
                        sprintf(jmvcore::.("%.0f%% intervals are bootstrap percentile intervals from %d resamples of the cases, refitting the model on each. Seed: %s."),
                                conf_pct, self$options$nboot,
                                if (is.null(self$options$seed)) "0" else as.character(self$options$seed))
                    else
                        sprintf(jmvcore::.("%.0f%% intervals are normal-approximation (Wald) intervals, using the estimated number of diseased cases as the denominator for sensitivity and non-diseased for specificity. They treat the estimates as observed proportions and so understate the uncertainty of a latent-variable model; enable Bootstrap for intervals that account for the estimation itself."),
                                conf_pct)
                )

                # These two "methods" build the reference standard out of the very tests
                # being evaluated, so each test is scored against a standard that contains
                # its own result. That is incorporation bias, and for one metric it is total.
                meth <- self$options$method
                if (identical(meth, "composite") && length(tests) == 2) meth <- "any_positive"
                if (identical(meth, "all_positive") || identical(meth, "any_positive")) {
                    fixed_at_one <- if (identical(meth, "all_positive"))
                        "sensitivity and NPV" else "specificity and PPV"
                    rule <- if (identical(meth, "all_positive"))
                        "every test is positive" else "at least one test is positive"
                    private$.addNotice(
                        "STRONG_WARNING",
                        "This method cannot estimate accuracy",
                        sprintf("The reference standard is defined as \"%s\", so each test is being compared against a rule built from its own result. %s are therefore fixed at 100%% by construction on every dataset, whatever the tests actually do, and are left blank below rather than reported as findings. The remaining figures are inflated by the same circularity and describe agreement with the composite rule, not diagnostic accuracy. To estimate accuracy without a gold standard, use the latent class method with three or more conditionally independent tests.",
                                rule,
                                if (identical(meth, "all_positive")) "Sensitivity and NPV" else "Specificity and PPV")
                    )
                    table$setNote(
                        "incorporation",
                        jmvcore::.("Each test is scored against a reference standard built from the tests themselves, so these are measures of agreement with that rule, not estimates of diagnostic accuracy. The blank column is fixed at 100% by construction and carries no information.")
                    )
                }

                for (i in seq_along(tests)) {
                    sensitivity <- results$sensitivities[i]
                    specificity <- results$specificities[i]

                    # Calculate confidence intervals
                    if (self$options$bootstrap) {
                        sens_ci <- private$.bootCI(private$.boot_cache$sensitivities[, i], self$options$alpha)
                        spec_ci <- private$.bootCI(private$.boot_cache$specificities[, i], self$options$alpha)
                    } else {
                        # Normal approximation. The denominator for sensitivity is the number
                        # of DISEASED cases and for specificity the number of NON-diseased --
                        # not the total n. Using the total understated both standard errors
                        # (by a factor of about sqrt(1/prevalence) for sensitivity), so every
                        # interval was too narrow; at 30% prevalence the sensitivity SE was
                        # ~1.8x too small.
                        n_total <- nrow(results$data)
                        prev_for_se <- results$prevalence
                        if (!is.numeric(prev_for_se) || length(prev_for_se) != 1 ||
                            !is.finite(prev_for_se)) prev_for_se <- NA_real_

                        n_dis <- if (is.na(prev_for_se)) NA_real_ else n_total * prev_for_se
                        n_nondis <- if (is.na(prev_for_se)) NA_real_ else n_total * (1 - prev_for_se)
                        z <- qnorm(1 - self$options$alpha / 2)

                        wald <- function(p, n_eff) {
                            if (!is.finite(p) || !is.finite(n_eff) || n_eff < 1)
                                return(list(lower = NA_real_, upper = NA_real_))
                            se <- sqrt(p * (1 - p) / n_eff)
                            list(lower = max(0, p - z * se), upper = min(1, p + z * se))
                        }
                        sens_ci <- wald(sensitivity, n_dis)
                        spec_ci <- wald(specificity, n_nondis)
                    }

                    # Calculate PPV and NPV
                    prevalence <- results$prevalence
                    ppv_npv <- private$.calculatePPVNPV(sensitivity, specificity, prevalence)

                    # Some metrics are fixed at 1 by the CONSTRUCTION of the reference, not
                    # estimated from the data, and reporting them as results is misleading:
                    #   all_positive: the reference is TRUE only when every test is positive,
                    #     so a diseased case can never be test-negative -> FN = 0 ->
                    #     sensitivity == 1 and NPV == 1 for EVERY test, on EVERY dataset.
                    #   any_positive: the reference is FALSE only when every test is negative,
                    #     so FP = 0 -> specificity == 1 and PPV == 1 likewise.
                    # Confirmed on 25/25 random datasets. They are blanked rather than shown
                    # as "100% (95% CI 100-100%)", which reads as a perfect test.
                    degenerate_spec <- identical(self$options$method, "any_positive") ||
                        # composite with 2 tests IS any_positive (a 1-of-2 tie passes >= 0.5)
                        (identical(self$options$method, "composite") && length(tests) == 2)

                    if (identical(self$options$method, "all_positive")) {
                        sensitivity <- NA_real_
                        sens_ci <- list(lower = NA_real_, upper = NA_real_)
                        ppv_npv$npv <- NA_real_
                    } else if (degenerate_spec) {
                        specificity <- NA_real_
                        spec_ci <- list(lower = NA_real_, upper = NA_real_)
                        ppv_npv$ppv <- NA_real_
                    }

                    # Add row to cleared table
                    table$addRow(rowKey = tests[i], values = list(
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
            # Bivariate residuals: for each test pair, compare the observed two-way table
            # with the one the fitted latent class model implies. Latent class analysis
            # assumes the tests err INDEPENDENTLY given true status, and that assumption is
            # what makes the estimates identifiable -- when it fails (two stains for one
            # antigen, two readers of one slide) the estimated accuracy is inflated. The
            # analysis previously only stated the assumption; this measures it.
            #
            # Only meaningful when resid.df > 0. With three tests the model is
            # just-identified and reproduces every observed table exactly, so every residual
            # is structurally 0 and can reveal nothing. Verified: on data where two tests
            # share an artefact, 3 tests give all-zero residuals while 4 tests flag five of
            # six pairs (up to 436 against a 3.84 threshold).
            .populateConditionalDependence = function(model, tests) {
                # The table may not exist until jmvtools::prepare() compiles the .r.yaml,
                # but the dependence WARNING is clinically important either way -- so
                # compute regardless and guard only the table writes.
                table <- private$.resultsItem("conditional_dependence")
                if (is.null(model)) return()
                if (!is.null(table)) table$deleteRows()

                resid_df <- model$resid.df
                just_identified <- !is.numeric(resid_df) || length(resid_df) != 1 ||
                    !is.finite(resid_df) || resid_df <= 0

                if (just_identified) {
                    if (!is.null(table)) table$setNote("df0", jmvcore::.("Not computable with three tests: the model has no residual degrees of freedom, so it reproduces every observed table exactly and no residual can detect conditional dependence. Add a fourth test if you need to check this assumption."))
                    return()
                }

                pc <- model$predcell
                if (is.null(pc) || nrow(pc) == 0) return()
                var_cols <- intersect(names(pc), unlist(tests))
                if (length(var_cols) < 2) return()

                THRESHOLD <- stats::qchisq(0.95, df = 1)   # 3.841
                flagged <- character(0)

                for (i in seq_len(length(var_cols) - 1)) {
                    for (j in seq(i + 1, length(var_cols))) {
                        a <- var_cols[i]; b <- var_cols[j]
                        agg <- tryCatch(
                            stats::aggregate(cbind(observed, expected) ~ pc[[a]] + pc[[b]],
                                             data = pc, FUN = sum),
                            error = function(e) NULL)
                        if (is.null(agg) || nrow(agg) == 0) next
                        bvr <- sum((agg$observed - agg$expected)^2 /
                                       pmax(agg$expected, 1e-9))
                        pair <- paste(a, "vs", b)
                        if (bvr > THRESHOLD) flagged <- c(flagged, pair)
                        if (!is.null(table)) table$addRow(rowKey = pair, values = list(
                            pair = pair,
                            bvr = bvr,
                            verdict = if (bvr > THRESHOLD)
                                jmvcore::.("Evidence of shared error - estimates inflated")
                            else jmvcore::.("Consistent with independence")
                        ))
                    }
                }

                if (!is.null(table)) table$setNote("threshold", sprintf(
                    jmvcore::.("A residual above %.2f (the 5%% point of chi-squared on 1 degree of freedom) is evidence that the pair does not err independently. This is a descriptive check, not a formal test: the residuals are correlated with one another and no multiplicity adjustment is applied."),
                    THRESHOLD))

                if (length(flagged) > 0) {
                    private$.addNotice(
                        "STRONG_WARNING",
                        "Tests do not appear to err independently",
                        sprintf("%s show more agreement than the latent class model can explain. That model assumes the tests make mistakes independently given true disease status; when they do not -- because they measure the same biology, or share a reader, sample or platform -- the shared error is absorbed into the latent class and sensitivity and specificity come out too high. Treat the estimates above as an upper bound, and prefer tests that fail in genuinely different ways.",
                                paste(flagged, collapse = ", "))
                    )
                }
            },

            .populateModelFit = function(model) {
                if (is.null(model)) {
                    return()
                }

                table <- self$results$model_fit
                table$deleteRows() # Clear existing rows to prevent duplicates on re-run

                # A 2-class model over k binary tests has 2k+1 parameters and 2^k - 1
                # degrees of freedom, so k = 3 leaves resid.df = 0: the model is
                # just-identified and reproduces the observed table exactly. G-squared and
                # Chi-squared are then structurally ~0 with no df to test them against, and
                # printing them invites the reader to conclude the model "fits well". AIC and
                # BIC remain meaningful for comparing models.
                resid_df <- model$resid.df
                just_identified <- is.numeric(resid_df) && length(resid_df) == 1 &&
                    is.finite(resid_df) && resid_df <= 0

                fit_stats <- list(
                    BIC = model$bic,
                    AIC = model$aic,
                    "Log-Likelihood" = model$llik,
                    "G-squared" = if (just_identified) NULL else model$Gsq,
                    "Chi-squared" = if (just_identified) NULL else model$Chisq,
                    "Degrees of Freedom" = resid_df
                )

                if (just_identified) {
                    table$setNote(
                        "just_identified",
                        jmvcore::.("With three tests this model has as many parameters as the data can support (0 residual degrees of freedom), so it reproduces the observed table exactly. Goodness-of-fit statistics are therefore omitted: they cannot tell you whether the conditional-independence assumption holds. Use four or more tests if you need to test the model's fit.")
                    )
                }

                # Add each available statistic to table
                for (name in names(fit_stats)) {
                    if (!is.null(fit_stats[[name]])) {
                        table$addRow(rowKey = name, values = list(
                            statistic = name,
                            value = fit_stats[[name]]
                        ))
                    }
                }
            },
            .runLCA = function(binary_data, tests, test_levels, n_starts = 30L, probs_start = NULL) {
                if (!requireNamespace("poLCA", quietly = TRUE)) {
                    jmvcore::reject(.("Package 'poLCA' is required for latent class analysis"))
                }

                # Convert to LCA format (factors with "no"/"yes" levels)
                lca_data <- data.frame(matrix(nrow = nrow(binary_data), ncol = ncol(binary_data)))
                names(lca_data) <- names(binary_data)

                for (i in seq_along(names(binary_data))) {
                    lca_data[[i]] <- factor(
                        binary_data[[i]],
                        levels = c(0, 1),
                        labels = c("no", "yes")
                    )
                }

                # Create formula with allow-list parse-time guard.
                # jmvcore::composeTerms backtick-quotes names (handling internal backticks correctly);
                # jmvcore::asFormula enforces the parse-time allow-list and rejects code injection in RHS.
                # cbind is on the global allow-list (jamovi 2.7.27+).
                var_names <- names(lca_data)
                escaped_var_names <- jmvcore::composeTerms(as.list(var_names))
                f <- jmvcore::asFormula(paste0("cbind(", paste(escaped_var_names, collapse = ","), ") ~ 1"))

                # Run LCA with more starts to ensure global optimum
                best_model <- NULL
                best_llik <- -Inf
                # n_starts is a parameter: the main fit uses many random starts to find the
                # global optimum, bootstrap replicates use few because the cost is multiplied
                # by nboot.
                stalled_starts <- 0L # consecutive starts with no improvement
                prev_best <- -Inf


                for (start in 1:n_starts) {
                    # Checkpoint periodically during LCA iterations
                    if (start %% 10 == 1) { # Every 10 starts
                        private$.checkpoint(flush = FALSE) # Poll for changes only
                    }

                    # Stop once extra random starts stop finding a better optimum. The
                    # previous condition was `(best_llik - (-Inf)) > 0.001`, which is Inf >
                    # 0.001 -- always TRUE -- so it broke unconditionally at start 21 and
                    # never examined convergence at all, despite n_starts being 30.
                    if (start > 10 && stalled_starts >= 10) {
                        break
                    }

                    seed_val <- self$options$seed
                    if (is.null(seed_val)) seed_val <- 0
                    iter_seed <- seed_val + start * 100
                    set.seed(iter_seed)

                    tryCatch(
                        {
                            model <- poLCA::poLCA(
                                formula = f,
                                data = lca_data,
                                nclass = 2,
                                maxiter = 1000,
                                graphs = FALSE,
                                verbose = FALSE,
                                nrep = 1,
                                # Bootstrap replicates start from the main fit's solution
                                # rather than at random: a resample's optimum sits very close
                                # to the full-sample one, so EM converges in a few iterations
                                # and one start suffices. Random restarts per replicate were
                                # the whole cost of the latent-class bootstrap.
                                probs.start = if (start == 1L) probs_start else NULL
                            )

                            if (!is.null(model) && model$llik > best_llik) {
                                improvement <- model$llik - best_llik
                                best_model <- model
                                best_llik <- model$llik

                            }
                        },
                        error = function(e) {
                            # Continue to next start
                        }
                    )
                    if (best_llik > prev_best) stalled_starts <- 0L else stalled_starts <- stalled_starts + 1L
                    prev_best <- best_llik
                }

                private$.diag(sprintf("LCA             : %d random start(s) used of %d allowed; best log-likelihood %.4f",
                                      min(start, n_starts), n_starts, best_llik))

                if (is.null(best_model)) {
                    jmvcore::reject(.("LCA model fitting failed after all attempts. Try a different method or check your data."))
                }

                # Add convergence warning if log-likelihood is suspiciously low
                if (best_llik < -1e10) {
                    if (self$options$verbose) {
                        warning(.("LCA model may not have converged properly. Results should be interpreted with caution."))
                    }
                }

                # Extract results
                # Ensure we identify which class represents disease presence.
                # For each latent class, average P(test positive | class) across all tests;
                # the disease class is the one whose tests are, on average, most often
                # positive. poLCA stores probs[[i]] with rows = latent classes and columns =
                # outcomes (column 2 = "yes"/positive), so probs[[i]][c, 2] = P(test i
                # positive | class c). The previous code reshaped a single class's per-test
                # vector via matrix(..., ncol = 2), which produced an arbitrary disease class
                # and could invert sensitivity/specificity.
                # RUNTIME-CHECKED 2026-08-07 against poLCA: probs[[i]] is
                # [rows = latent class, cols = outcome], cols being c("no", "yes"). The
                # extraction below previously used [2, disease_class] / [1, healthy_class] --
                # i.e. [outcome, class] -- which is the TRANSPOSE of the layout the
                # identification code above correctly assumes. On simulated data with known
                # truth (sens .90/.80/.70, spec .95/.85/.75) the module returned
                # sens .936/.840/.737 and spec .903/.777/.720: sensitivity and specificity
                # exactly SWAPPED. poLCA itself recovers the truth.
                mean_pos_class1 <- mean(sapply(best_model$probs, function(x) x[1, 2]))
                mean_pos_class2 <- mean(sapply(best_model$probs, function(x) x[2, 2]))
                disease_class <- which.max(c(mean_pos_class1, mean_pos_class2))
                healthy_class <- 3 - disease_class # The other class

                # Disease prevalence is the probability of the disease class
                prevalence <- best_model$P[disease_class]

                # Extract sensitivities and specificities
                sensitivities <- numeric(length(tests))
                specificities <- numeric(length(tests))

                for (i in seq_along(tests)) {
                    # probs[[i]][class, outcome]; outcome 1 = "no", 2 = "yes"
                    # Sensitivity: P(test positive | disease class)
                    sensitivities[i] <- best_model$probs[[i]][disease_class, 2]
                    # Specificity: P(test negative | healthy class)
                    specificities[i] <- best_model$probs[[i]][healthy_class, 1]
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
                # "Majority vote", implemented as mean >= 0.5 -- so a TIE counts as diseased.
                # With exactly 2 tests the only attainable means are 0, 0.5 and 1, and 0.5
                # passes, which makes this rule literally identical to any_positive: the
                # composite is TRUE whenever either test is positive. FP is then identically
                # 0 and specificity/PPV come out 1.000 for every test, on every dataset.
                # Verified: at k=2 composite and any_positive agree on 100% of rows; at
                # k=3/4/5 they agree on 56%/66%/41%, so this is a k=2 degeneracy, not a
                # broken rule.
                if (ncol(binary_data) >= 3) {
                    # Even with a genuine majority, each test votes on the standard it is
                    # then scored against. Simulation puts the inflation at roughly +0.08 to
                    # +0.12 on sensitivity in the common regime; that was never disclosed.
                    private$.addNotice(
                        "WARNING",
                        "Composite reference inflates the estimates",
                        "The reference standard here is a majority vote of the same tests being evaluated, so each test helps decide the answer it is graded against. This inflates every figure below -- in simulation, sensitivity by roughly 8 to 12 percentage points -- and the inflation is largest for the test that agrees most often with the others. Treat these as agreement with the majority, not as diagnostic accuracy. The latent class method estimates accuracy without building the standard from the tests."
                    )
                }

                if (ncol(binary_data) == 2) {
                    private$.addNotice(
                        "STRONG_WARNING",
                        "Composite reference cannot be used with only two tests",
                        "With two tests a majority vote has no majority: one positive out of two is a tie, which this rule counts as diseased, making the composite reference identical to \"any test positive\". Every test then agrees perfectly with the reference whenever it is positive, so specificity and PPV are fixed at 100% by construction and are left blank below. Add a third test, or interpret only the sensitivity column -- and note that it too is inflated because each test helped build the standard it is being judged against."
                    )
                }
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

                    sensitivities[i] <- tp / (tp + fn)
                    specificities[i] <- tn / (tn + fp)
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

                # A two-class model over k binary tests has 2k + 1 free parameters and
                # 2^k - 1 degrees of freedom, so k = 2 gives 5 parameters for 3 df: the
                # model is NOT identified and the estimates are determined by the starting
                # values and the prior rather than by the data. latent_class already refuses
                # this case; the EM here accepted it silently and returned numbers.
                if (num_tests < 3) {
                    private$.addNotice(
                        "STRONG_WARNING",
                        "Two tests cannot identify this model",
                        "A two-class model over two binary tests has five unknown parameters but only three degrees of freedom, so sensitivity, specificity and prevalence are not separately estimable from the data. The values below reflect the starting values and the prior, not evidence from your sample, and must not be reported as accuracy estimates. Add a third conditionally independent test."
                    )
                }

                # Prior parameters
                # Prior for prevalence (Beta distribution)
                alpha_prev <- 1 # uniform prior
                beta_prev <- 1 # uniform prior

                # Prior for sensitivity and specificity (Beta distribution).
                # Beta(2,1) has density 2x on [0,1]: mean 2/3, monotonically increasing, so
                # it pulls BOTH estimates upward. In MAP terms it adds one pseudo-success and
                # no pseudo-failure to every test. That is a deliberate thumb on the scale
                # toward better-looking tests, and it was invisible to the user -- neither
                # the output nor the option description mentioned any prior at all.
                alpha_sens <- 2
                beta_sens <- 1
                alpha_spec <- 2
                beta_spec <- 1

                private$.addNotice(
                    "WARNING",
                    "Priors used by the Bayesian method",
                    sprintf("Prevalence uses a uniform Beta(%g, %g) prior. Sensitivity and specificity each use a Beta(%g, %g) prior, which has mean %.2f and increases toward 1, so it pulls both estimates upward -- in MAP terms it adds one pseudo-positive result to every test. With a small sample this prior, not your data, may be driving the numbers below. These are penalised-likelihood (MAP) point estimates from an EM algorithm, not draws from a posterior, so the intervals shown are not credible intervals.",
                            alpha_prev, beta_prev, alpha_sens, beta_sens,
                            alpha_sens / (alpha_sens + beta_sens))
                )

                # Initialize parameters
                # Start with prevalence = 0.3 as initial guess
                prevalence <- 0.3

                # Initialize sensitivity and specificity for each test
                sensitivities <- rep(0.8, num_tests) # initial guess
                specificities <- rep(0.9, num_tests) # initial guess

                # EM algorithm for parameter estimation
                max_iter <- 100
                tol <- 1e-6
                converged <- FALSE

                for (iter in 1:max_iter) {
                    # Checkpoint periodically during EM iterations
                    if (iter %% 20 == 1) { # Every 20 iterations
                        private$.checkpoint(flush = FALSE) # Poll for changes only
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
                            prob_disease[i] <- prevalence # use current prevalence as a fallback
                        }
                    }

                    # M-step: Update parameters
                    # Update prevalence
                    new_prevalence <- (sum(prob_disease, na.rm = TRUE) + alpha_prev - 1) /
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
                            new_sensitivities[j] <- (sum(probs[test_pos], na.rm = TRUE) + alpha_sens - 1) /
                                (sum(probs, na.rm = TRUE) + alpha_sens + beta_sens - 2)
                        } else {
                            # Fallback if denominator is zero
                            new_sensitivities[j] <- (alpha_sens - 1) / (alpha_sens + beta_sens - 2)
                        }

                        # For specificity: P(T-|D-)
                        test_neg <- test_results == 0
                        if (sum(1 - probs) > 0) {
                            new_specificities[j] <- (sum((1 - probs)[test_neg], na.rm = TRUE) + alpha_spec - 1) /
                                (sum(1 - probs, na.rm = TRUE) + alpha_spec + beta_spec - 2)
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
                    if (max(param_diffs, na.rm = TRUE) < tol) {
                        converged <- TRUE
                        break
                    }

                    # Update parameters for next iteration
                    prevalence <- new_prevalence
                    sensitivities <- new_sensitivities
                    specificities <- new_specificities
                }

                # When the estimated prevalence collapses toward 0 the sensitivity
                # denominator sum(prob_disease) collapses with it, the Beta(2,1) numerator
                # offset dominates, and the clamp below pins the answer at 0.999. A cohort in
                # which no test is ever positive was reported as 99.9% sensitive. A clamped
                # boundary value is not an estimate; blank it and say why.
                eff_diseased <- prevalence * num_patients
                eff_healthy <- (1 - prevalence) * num_patients
                unstable <- character(0)
                if (!is.finite(eff_diseased) || eff_diseased < 1) {
                    sensitivities <- rep(NA_real_, num_tests)
                    unstable <- c(unstable, "sensitivity")
                }
                if (!is.finite(eff_healthy) || eff_healthy < 1) {
                    specificities <- rep(NA_real_, num_tests)
                    unstable <- c(unstable, "specificity")
                }
                if (length(unstable) > 0) {
                    private$.addNotice(
                        "STRONG_WARNING",
                        "Parameters not estimable",
                        sprintf("The fitted prevalence is %.4f, so the model implies fewer than one %s case in this sample. %s cannot be estimated from it and %s left blank -- the algorithm would otherwise return a value pinned at its boundary by the prior rather than by your data.",
                                prevalence,
                                if ("sensitivity" %in% unstable) "diseased" else "non-diseased",
                                paste(tools::toTitleCase(unstable), collapse = " and "),
                                if (length(unstable) > 1) "have been" else "has been")
                    )
                }

                private$.diag(sprintf("Bayesian EM     : %s after %d iteration(s) (limit %d)",
                                      if (converged) "converged" else "DID NOT CONVERGE",
                                      iter, max_iter))

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
                # all(logical(0)) is TRUE, so a row whose every test is missing would be
                # counted as reference-POSITIVE. Complete-case filtering upstream makes this
                # unreachable today, but it is a live trap if that ever changes.
                reference <- apply(binary_data, 1, function(x)
                    any(!is.na(x)) && all(x == 1, na.rm = TRUE))

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

                    sensitivities[i] <- if ((tp + fn) > 0) tp / (tp + fn) else NA
                    specificities[i] <- if ((tn + fp) > 0) tn / (tn + fp) else NA
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
                reference <- apply(binary_data, 1, function(x) any(x == 1, na.rm = TRUE))

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

                    sensitivities[i] <- if ((tp + fn) > 0) tp / (tp + fn) else NA
                    specificities[i] <- if ((tn + fp) > 0) tn / (tn + fp) else NA
                }

                return(list(
                    prevalence = prevalence,
                    sensitivities = sensitivities,
                    specificities = specificities,
                    data = binary_data
                ))
            },
            # Run the bootstrap ONCE and cache every statistic from each replicate.
            #
            # Previously .calculateBootstrapCI was called separately for prevalence and for
            # the sensitivity AND specificity of every test -- 1 + 2*ntests independent
            # loops, each re-running the estimator from scratch. With method="latent_class"
            # each replicate also refits poLCA up to 21 times, so nboot=1000 with 3 tests
            # meant on the order of 10^5 model fits and the analysis never finished.
            # Resampling once per replicate and reading all the statistics off that single
            # fit is both far cheaper and statistically better: the intervals now come from
            # one coherent set of resamples rather than several unrelated ones.
            .bootstrapAll = function(data, method, nboot, verbose = FALSE, warm_start = NULL) {
                n <- nrow(data)
                n_tests <- ncol(data)

                # Seed ONCE, from the user's option, so a run is reproducible. Previously
                # the only set.seed() was inside .runLCA, which reset the stream on every
                # replicate and made the resamples cycle through a handful of distinct
                # bootstrap samples instead of nboot independent ones.
                seed_val <- self$options$seed
                if (is.null(seed_val) || !is.finite(seed_val)) seed_val <- 0
                set.seed(seed_val)
                # Draw every resample up front so nothing downstream can disturb the stream.
                idx <- matrix(sample.int(n, n * nboot, replace = TRUE), nrow = nboot, byrow = TRUE)

                # Replicates reuse the main estimators; keep their diagnostics out of the panel.
                private$.diagSuppressed <- TRUE
                on.exit(private$.diagSuppressed <- FALSE, add = TRUE)

                prevalence <- rep(NA_real_, nboot)
                sens <- matrix(NA_real_, nrow = nboot, ncol = n_tests)
                spec <- matrix(NA_real_, nrow = nboot, ncol = n_tests)
                error_count <- 0L

                for (b in seq_len(nboot)) {
                    if (b %% 25 == 1) private$.checkpoint(flush = FALSE)
                    boot_data <- data[idx[b, ], , drop = FALSE]

                    boot_result <- tryCatch({
                        if (method == "latent_class") {
                            private$.runLCA(boot_data, names(data), NULL,
                                            n_starts = if (is.null(warm_start)) 3L else 1L,
                                            probs_start = warm_start)
                        } else if (method == "composite") {
                            private$.runComposite(boot_data)
                        } else if (method == "all_positive") {
                            private$.runAllPositive(boot_data)
                        } else if (method == "any_positive") {
                            private$.runAnyPositive(boot_data)
                        } else if (method == "bayesian") {
                            private$.runBayesian(boot_data)
                        } else NULL
                    }, error = function(e) NULL)

                    if (is.null(boot_result)) {
                        error_count <- error_count + 1L
                        next
                    }
                    prevalence[b] <- boot_result$prevalence
                    k <- min(n_tests, length(boot_result$sensitivities))
                    if (k > 0) {
                        sens[b, seq_len(k)] <- boot_result$sensitivities[seq_len(k)]
                        spec[b, seq_len(k)] <- boot_result$specificities[seq_len(k)]
                    }
                }

                if (error_count > 0) {
                    private$.addNotice(
                        "WARNING",
                        "Some bootstrap replicates failed",
                        sprintf("%d of %d bootstrap resamples (%.0f%%) could not be fitted and were discarded. The intervals below are based on the remaining %d. A high failure rate usually means the resamples are too small or too sparse to support the model.",
                                error_count, nboot, 100 * error_count / nboot, nboot - error_count)
                    )
                }

                private$.diagSuppressed <- FALSE
                private$.diag(sprintf("Bootstrap       : %d resamples, seed %s, %d failed (%.1f%%)",
                                      nboot, base::format(seed_val), error_count,
                                      100 * error_count / nboot))
                if (!is.null(warm_start))
                    private$.diag("                  latent class replicates warm-started from the full-sample fit")

                list(prevalence = prevalence, sensitivities = sens, specificities = spec,
                     n_failed = error_count, nboot = nboot)
            },

            # Percentile interval from a cached bootstrap distribution.
            .bootCI = function(values, alpha) {
                values <- values[is.finite(values)]
                if (length(values) < 20) return(list(lower = NA_real_, upper = NA_real_))
                q <- stats::quantile(values, c(alpha / 2, 1 - alpha / 2), names = FALSE, na.rm = TRUE)
                list(lower = q[1], upper = q[2])
            },

            .calculatePPVNPV = function(sensitivity, specificity, prevalence) {
                # Calculate Positive Predictive Value (PPV) and Negative Predictive Value (NPV)
                # Using Bayes' theorem

                # PPV = (sensitivity * prevalence) / ((sensitivity * prevalence) + ((1 - specificity) * (1 - prevalence)))
                # A test with no diseased (or no healthy) cases yields NA sensitivity or
                # specificity upstream. `if (NA > 0)` then throws "missing value where
                # TRUE/FALSE needed" and aborts the ENTIRE analysis rather than leaving one
                # cell blank. isTRUE() makes the guard NA-safe.
                ppv_numerator <- sensitivity * prevalence
                ppv_denominator <- ppv_numerator + ((1 - specificity) * (1 - prevalence))
                ppv <- if (isTRUE(ppv_denominator > 0)) ppv_numerator / ppv_denominator else NA_real_

                # NPV = (specificity * (1 - prevalence)) / (((1 - sensitivity) * prevalence) + (specificity * (1 - prevalence)))
                npv_numerator <- specificity * (1 - prevalence)
                npv_denominator <- ((1 - sensitivity) * prevalence) + npv_numerator
                npv <- if (isTRUE(npv_denominator > 0)) npv_numerator / npv_denominator else NA_real_

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
                tryCatch(
                    {
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
                            seq_len(nrow(agreement_matrix)),
                            seq_len(ncol(agreement_matrix)),
                            agreement_matrix,
                            axes = FALSE,
                            xlab = "",
                            ylab = "",
                            main = "Test Agreement Matrix",
                            col = colors,
                            zlim = c(0, 1)
                        )

                        # Add test names with better formatting
                        axis(1, at = seq_along(tests), labels = tests, las = 2, cex.axis = 1.2)
                        axis(2, at = seq_along(tests), labels = tests, las = 2, cex.axis = 1.2)

                        # Add agreement values with improved visibility
                        for (i in seq_len(nrow(agreement_matrix))) {
                            for (j in seq_len(ncol(agreement_matrix))) {
                                # Determine text color based on background brightness
                                # Use white text on dark backgrounds, black text on light backgrounds
                                color_idx <- round(agreement_matrix[i, j] * 99) + 1
                                if (color_idx < 50) {
                                    text_col <- "white"
                                } else {
                                    text_col <- "black"
                                }

                                text(i, j, sprintf("%.2f", agreement_matrix[i, j]),
                                    col = text_col, cex = 1.2, font = 2
                                )
                            }
                        }

                        # Add a color bar legend outside the plot area
                        legend_y_pos <- seq(1, length(tests), length.out = 6)
                        legend_colors <- colors[seq(1, length(colors), length.out = 5)]
                        legend_values <- seq(0, 1, length.out = 5)
                        legend_labels <- sprintf("%.1f", legend_values)

                        # Place legend to the right of the plot
                        legend(length(tests) + 0.5, length(tests) / 2,
                            legend = legend_labels,
                            fill = legend_colors,
                            title = "Agreement",
                            bty = "n", # No box around legend
                            cex = 1.1,
                            y.intersp = 1.2,
                            title.cex = 1.2
                        )

                        # Add a subtle box around the plot area
                        box(col = "gray50", lwd = 2)

                        return(TRUE)
                    },
                    error = function(e) {
                        # In case of error, create a simpler plot
                        message(jmvcore::format(.("Error in plot: {msg}"), msg = e$message))

                        # Simple fallback plot
                        try(
                            {
                                plot(0, 0,
                                    type = "n", xlim = c(0, 1), ylim = c(0, 1),
                                    xlab = "", ylab = "", main = "Test Agreement"
                                )
                                text(0.5, 0.5, "Agreement data available but plotting failed",
                                    cex = 1.2, col = "red"
                                )
                                return(TRUE)
                            },
                            silent = TRUE
                        )

                        return(FALSE)
                    }
                )
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
                tryCatch(
                    {
                        # Check if ggplot2 is available
                        if (!requireNamespace("ggplot2", quietly = TRUE)) {
                            # Fallback to base R plot
                            return(private$.plot(image, ggtheme, theme, ...))
                        }

                        # Convert matrix to long format for ggplot
                        plot_data <- data.frame()
                        for (i in seq_len(nrow(agreement_matrix))) {
                            for (j in seq_len(ncol(agreement_matrix))) {
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
                                ggplot2::aes(
                                    label = sprintf("%.2f", Agreement),
                                    color = ifelse(Agreement > 0.5, "black", "white")
                                ),
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
                            ggplot2::coord_fixed() # Keep cells square

                        print(p)
                        return(TRUE)
                    },
                    error = function(e) {
                        # In case of error, create a simpler plot
                        message(jmvcore::format(.("Error in ggplot: {msg}"), msg = e$message))

                        # Try base R fallback
                        try(
                            {
                                # Set up plotting parameters
                                old_par <- par(no.readonly = TRUE)
                                on.exit(par(old_par), add = TRUE)

                                # Simple heatmap
                                par(mar = c(5, 5, 4, 5))
                                image(
                                    seq_len(nrow(agreement_matrix)),
                                    seq_len(ncol(agreement_matrix)),
                                    agreement_matrix,
                                    axes = FALSE,
                                    xlab = "",
                                    ylab = "",
                                    main = .("Test Agreement Matrix"),
                                    col = hcl.colors(50, "viridis"),
                                    zlim = c(0, 1)
                                )

                                # Add labels
                                axis(1, at = seq_along(tests), labels = tests, las = 2)
                                axis(2, at = seq_along(tests), labels = tests, las = 2)
                                box()

                                return(TRUE)
                            },
                            silent = TRUE
                        )

                        return(FALSE)
                    }
                )
            },
            .validateClinicalAssumptions = function(data, tests, method) {
                n_obs <- nrow(data)
                n_tests <- length(tests)

                # Sample size warnings based on method
                if (method == "latent_class" && n_obs < 100) {
                    private$.addNotice(
                        "STRONG_WARNING",
                        "LCA Sample Size",
                        sprintf(.("LCA typically requires 100+ observations for stable results. Current N = %d. Consider using composite reference method for smaller samples."), n_obs)
                    )
                }

                if (method == "bayesian" && n_obs < 50) {
                    private$.addNotice(
                        "STRONG_WARNING",
                        "Bayesian Sample Size",
                        sprintf(.("Bayesian analysis may be unstable with N < 50. Current N = %d. Consider collecting more data."), n_obs)
                    )
                }

                # Check test result distributions
                for (i in seq_along(tests)) {
                    test_name <- tests[[i]]
                    test_values <- table(data[[test_name]])

                    if (any(test_values < 5)) {
                        private$.addNotice(
                            "WARNING",
                            "Small Test Categories",
                            sprintf(.("Test '%s' has categories with <5 observations. Results may be unstable. Consider combining categories if clinically appropriate."), test_name)
                        )
                    }

                    # Check for extreme imbalances
                    min_prop <- min(test_values) / sum(test_values)
                    if (min_prop < 0.05) {
                        private$.addNotice(
                            "WARNING",
                            "Extreme Test Imbalance",
                            sprintf(.("Test '%s' shows extreme imbalance (minority category %.1f%%). This may affect parameter estimation."), test_name, min_prop * 100)
                        )
                    }
                }

                # Method-specific warnings
                if (method == "latent_class" && n_tests < 3) {
                    # checking this earlier in .run now, but good to keep as message if we relax allow
                    private$.addNotice(
                        "WARNING",
                        "LCA Under-Identified",
                        .("LCA with only 2 tests is under-identified.")
                    )
                }

                if (method == "composite" && n_tests %% 2 == 0) {
                    private$.addNotice(
                        "WARNING",
                        "Composite Ties",
                        .("Composite reference with even number of tests may result in ties. Consider using an odd number of tests or a different method.")
                    )
                }

                # Clinical context message
                if (self$options$verbose) {
                    private$.addNotice(
                        "INFO",
                        "Clinical Validation",
                        sprintf(.("Clinical validation: %d tests analyzed with N=%d using %s method"), n_tests, n_obs, method)
                    )
                }
            },
            .showMethodGuide = function() {
                # Create comprehensive method selection guide in HTML
                guide_html <- paste0(
                    "<div style='background-color: rgba(138, 155, 172, 0.06); padding: 20px; border-radius: 8px; margin: 15px 0; border-left: 4px solid #007bff; color: inherit;'>",
                    "<h3 style='color: #007bff; margin-top: 0;'> ", .("Method Selection Guide"), "</h3>",
                    "<div style='margin: 15px 0; padding: 15px; background-color: rgba(33, 159, 33, 0.1); border-radius: 5px; color: inherit;'>",
                    "<h4 style='color: #2e7d32; margin-top: 0;'> ", .("Latent Class Analysis (Recommended)"), "</h4>",
                    "<p><strong>", .("Description"), ":</strong> ", .("Most robust method using mixture models. Estimates disease prevalence and test parameters simultaneously."), "</p>",
                    "<p><strong>", .("Best for"), ":</strong> ", .("Diagnostic validation studies with 3+ tests and N>=100"), "</p>",
                    "<p><strong>", .("Strengths"), ":</strong> ", .("The only method here that estimates accuracy rather than agreement with a self-built reference; provides model fit statistics. Assumes the tests are conditionally independent given true status -- it does NOT model conditional dependence"), "</p>",
                    "</div>",
                    "<div style='margin: 15px 0; padding: 15px; background-color: rgba(33, 152, 239, 0.13); border-radius: 5px; color: inherit;'>",
                    "<h4 style='color: #1565c0; margin-top: 0;'> ", .("Bayesian Analysis"), "</h4>",
                    "<p><strong>", .("Description"), ":</strong> ", .("Incorporates prior knowledge about test performance using Bayesian methods."), "</p>",
                    "<p><strong>", .("Best for"), ":</strong> ", .("Studies where you have prior information about expected sensitivity/specificity"), "</p>",
                    "<p><strong>", .("Strengths"), ":</strong> ", .("Uses prior knowledge, handles uncertainty well, good for smaller samples"), "</p>",
                    "</div>",
                    "<div style='margin: 15px 0; padding: 15px; background-color: rgba(255, 169, 33, 0.14); border-radius: 5px; color: inherit;'>",
                    "<h4 style='color: #ef6c00; margin-top: 0;'> ", .("Composite Reference"), "</h4>",
                    "<p><strong>", .("Description"), ":</strong> ", .("Uses majority vote of available tests as pseudo-gold standard."), "</p>",
                    "<p><strong>", .("Best for"), ":</strong> ", .("Inter-rater agreement studies with 3+ tests, exploratory analysis"), "</p>",
                    "<p><strong>", .("Strengths"), ":</strong> ", .("Simple and intuitive. Not an accuracy estimate: each test helps build the standard it is judged against, which inflates its apparent performance. Needs 3+ tests -- with 2 a tie counts as diseased, making it identical to Any Test Positive"), "</p>",
                    "</div>",
                    "<div style='margin: 15px 0; padding: 15px; background-color: rgba(230, 33, 99, 0.12); border-radius: 5px; color: inherit;'>",
                    "<h4 style='color: #c2185b; margin-top: 0;'> ", .("All Tests Positive"), "</h4>",
                    "<p><strong>", .("Description"), ":</strong> ", .("Conservative approach - disease present only if ALL tests are positive."), "</p>",
                    "<p><strong>", .("Best for"), ":</strong> ", .("Highly specific diagnoses where false positives are very costly"), "</p>",
                    "<p><strong>", .("Strengths"), ":</strong> ", .("A deliberately strict reference. Sensitivity and NPV cannot be estimated under this rule -- they are fixed at 100% by construction -- so only specificity and PPV are shown, and both are inflated by the same circularity"), "</p>",
                    "</div>",
                    "<div style='margin: 15px 0; padding: 15px; background-color: rgba(33, 159, 33, 0.1); border-radius: 5px; color: inherit;'>",
                    "<h4 style='color: #388e3c; margin-top: 0;'> ", .("Any Test Positive"), "</h4>",
                    "<p><strong>", .("Description"), ":</strong> ", .("Liberal approach - disease present if ANY test is positive."), "</p>",
                    "<p><strong>", .("Best for"), ":</strong> ", .("Population screening scenarios where missing cases is costly"), "</p>",
                    "<p><strong>", .("Strengths"), ":</strong> ", .("A deliberately permissive reference. Specificity and PPV cannot be estimated under this rule -- they are fixed at 100% by construction -- so only sensitivity and NPV are shown, and both are inflated by the same circularity"), "</p>",
                    "</div>",
                    "<div style='margin: 15px 0; padding: 10px; background-color: rgba(255, 203, 33, 0.14); border-radius: 5px; border-left: 3px solid #ffb300; color: inherit;'>",
                    "<h4 style='color: #e65100; margin-top: 0;'> ", .("Selection Tips"), "</h4>",
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

                if (preset == "none") {
                    return()
                }

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

                # A jamovi backend cannot write self$options -- the GUI owns them -- so a
                # preset can only ADVISE. Previously it stored the config and printed it via
                # message(), which no jamovi user ever sees, so all five presets produced
                # byte-identical analyses: same method, same bootstrap, same nboot, same
                # numbers. The control looked like it did something and did not.
                # It now reports what it recommends and how that compares with the current
                # settings, so the user can act on it.
                if (preset %in% names(presets)) {
                    preset_config <- presets[[preset]]
                    private$.preset_info <- preset_config

                    cur <- list(method = self$options$method,
                                bootstrap = isTRUE(self$options$bootstrap),
                                nboot = self$options$nboot,
                                alpha = self$options$alpha)
                    diffs <- character(0)
                    if (!identical(cur$method, preset_config$method))
                        diffs <- c(diffs, sprintf("Analysis method: currently \"%s\", recommended \"%s\"",
                                                  cur$method, preset_config$method))
                    if (!identical(cur$bootstrap, isTRUE(preset_config$bootstrap)))
                        diffs <- c(diffs, sprintf("Bootstrap confidence intervals: currently %s, recommended %s",
                                                  if (cur$bootstrap) "on" else "off",
                                                  if (isTRUE(preset_config$bootstrap)) "on" else "off"))
                    if (!isTRUE(all.equal(cur$nboot, preset_config$nboot)))
                        diffs <- c(diffs, sprintf("Bootstrap samples: currently %g, recommended %g",
                                                  cur$nboot, preset_config$nboot))

                    private$.addNotice(
                        if (length(diffs) > 0) "WARNING" else "INFO",
                        sprintf("Clinical preset: %s", gsub("_", " ", preset)),
                        if (length(diffs) > 0)
                            sprintf("%s %s This preset does NOT change your settings automatically -- set them yourself in the options panel: %s.",
                                    preset_config$description, preset_config$guidance,
                                    paste(diffs, collapse = "; "))
                        else
                            sprintf("%s %s Your current settings already match this preset.",
                                    preset_config$description, preset_config$guidance)
                    )
                }
            },
            .generateClinicalSummary = function(results, method, tests) {
                if (is.null(results)) {
                    return("")
                }

                n_tests <- length(tests)
                prev_pct <- sprintf("%.1f%%", results$prevalence * 100)

                # For the reference-rule methods, `prevalence` is the proportion of cases
                # satisfying the rule (all tests agree positive / any test positive), NOT an
                # estimate of disease prevalence, and one of sensitivity/specificity is fixed
                # at 1 by construction. This panel read results$sensitivities directly and so
                # still announced "Range from 100.0% to 100.0%" after the table stopped doing
                # so.
                rule_based <- method %in% c("all_positive", "any_positive") ||
                    (identical(method, "composite") && n_tests == 2)
                prev_label <- if (rule_based)
                    .("Cases meeting the reference rule") else .("Disease prevalence:")
                degenerate_sens <- identical(method, "all_positive")

                # Generate interpretation based on prevalence
                prev_interp <- if (results$prevalence < 0.10) {
                    .("Low estimated prevalence: holding sensitivity and specificity fixed, a lower prevalence raises negative predictive value and lowers positive predictive value relative to a higher-prevalence setting.")
                } else if (results$prevalence > 0.30) {
                    .("High estimated prevalence: holding sensitivity and specificity fixed, a higher prevalence raises positive predictive value and lowers negative predictive value relative to a lower-prevalence setting.")
                } else {
                    .("Moderate estimated prevalence: positive and negative predictive values are more balanced at this prevalence.")
                }

                # Sensitivity range
                sens_min <- sprintf("%.1f%%", min(results$sensitivities) * 100)
                sens_max <- sprintf("%.1f%%", max(results$sensitivities) * 100)

                summary_html <- paste0(
                    "<div class='clinical-summary' style='background-color: rgba(33, 152, 255, 0.07); padding: 15px; border-radius: 8px; margin: 10px 0; color: inherit;'>",
                    "<h4 style='color: #1565c0; margin-top: 0;'> ", .("Clinical Summary"), "</h4>",
                    "<p><strong>", .("Analysis:"), "</strong> ", sprintf(.("No gold standard analysis using %s method"), method), "</p>",
                    "<p><strong>", .("Tests analyzed:"), "</strong> ", paste(htmltools::htmlEscape(unlist(tests)), collapse = ", "), " (N=", n_tests, ")</p>",
                    "<p><strong>", prev_label, "</strong> ", prev_pct,
                    if (rule_based) paste0(" <em>", .("(this is the share of cases satisfying the rule, not an estimate of disease prevalence)"), "</em>") else "",
                    "</p>",
                    if (degenerate_sens)
                        paste0("<p><strong>", .("Test sensitivities:"), "</strong> <em>",
                               .("not estimable - fixed at 100% by the construction of this reference rule"), "</em></p>")
                    else
                        paste0("<p><strong>", .("Test sensitivities:"), "</strong> ", .("Range from"), " ", sens_min, " ", .("to"), " ", sens_max, "</p>"),
                    if (rule_based) "" else paste0("<p><strong>", .("Clinical interpretation:"), "</strong> ", prev_interp, "</p>"),
                    if (rule_based)
                        paste0("<p><strong>", .("Caution:"), "</strong> ",
                               .("this method scores each test against a reference built from the tests themselves, so the figures describe agreement with that rule rather than diagnostic accuracy."), "</p>")
                    else "",
                    "</div>"
                )

                return(summary_html)
            },
            .populateAgreementStats = function(test_data, tests, test_levels) {
                # Calculate pairwise Cohen's Kappa
                table <- self$results$agreement_stats
                table$deleteRows() # Clear existing rows to prevent duplicates on re-run
                n_tests <- length(tests)

                if (n_tests < 2) {
                    return()
                }

                # Helper function for Cohen's Kappa
                calculate_kappa <- function(var1, var2) {
                    # Create confusion matrix
                    tbl <- table(var1, var2)

                    # Check if tbl is valid (needs to be square if possible, but for Kappa we need matched levels)
                    # Ensure we have 2x2 table even if some levels are missing
                    levels_union <- union(levels(var1), levels(var2))
                    tbl_full <- table(factor(var1, levels = levels_union), factor(var2, levels = levels_union))

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

                for (i in 1:(n_tests - 1)) {
                    for (j in (i + 1):n_tests) {
                        test1 <- tests[[i]]
                        test2 <- tests[[j]]

                        # Ensure binary/factor conversion matches what we used
                        # Original data is factors
                        res <- calculate_kappa(test_data[[test1]], test_data[[test2]])

                        table$addRow(rowKey = paste0(test1, "_", test2), values = list(
                            test_pair = paste0(test1, " vs ", test2),
                            kappa = res$kappa,
                            p_value = res$p_value,
                            agreement = res$agreement
                        ))
                    }
                }

                table$setNote(
                    "kappa_se",
                    .("Kappa standard errors and p-values use a large-sample normal approximation rather than the exact asymptotic SE (e.g. vcd::Kappa); interpret p-values cautiously, especially in small samples.")
                )
            }
        )
    )
}
