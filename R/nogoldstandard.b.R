#' @title Analysis Without Gold Standard
#' @importFrom R6 R6Class
#' @importFrom graphics axis box image
#' @importFrom grDevices colorRampPalette hcl.colors
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

            # Estimators are reused during bootstrap. Their main-fit warnings must not be
            # repeated once per resample; bootstrap failures are summarized separately.
            .noticeSuppressed = FALSE,

            # A results item that may not exist in the compiled .h.R yet: jmvcore raises
            # rather than returning NULL, so a bare self$results$x would crash every run
            # between the .r.yaml edit and the next jmvtools::prepare().
            .resultsItem = function(name) tryCatch(self$results[[name]], error = function(e) NULL),

            .seedValue = function(offset = 0) {
                seed <- self$options$seed
                if (is.null(seed) || !is.numeric(seed) || length(seed) != 1 ||
                    !is.finite(seed)) {
                    seed <- 0
                }
                as.integer((as.double(seed) + as.double(offset)) %% .Machine$integer.max)
            },

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
                if (isTRUE(private$.noticeSuppressed))
                    return(invisible(NULL))

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
                severity <- c(ERROR = 1L, STRONG_WARNING = 2L, WARNING = 3L, INFO = 4L)
                notice_types <- vapply(private$.noticeList, `[[`, character(1), "type")
                notice_order <- order(unname(severity[notice_types]), na.last = TRUE)

                blocks <- vapply(private$.noticeList[notice_order], function(notice) {
                    prefix <- switch(notice$type,
                        ERROR          = "ERROR: ",
                        STRONG_WARNING = "STRONG WARNING: ",
                        WARNING        = "WARNING: ",
                        INFO           = "INFO: ",
                        "INFO: "
                    )
                    paste0(prefix, notice$title, ": ", notice$content)
                }, character(1))

                self$results$notices$setContent(paste(blocks, collapse = "\n"))
            },

            # The Preformatted notice accumulator is deliberate: dynamic Notice objects are
            # not serializable in this result tree. LCA identifiability, small-sample,
            # conditional-independence and multi-start convergence cautions are regression-tested.

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
                # A test is configured only when both its variable and selected positive
                # level are present. Counting variables alone led to a validation error and
                # stale output when a positive-level selection was removed.
                configured <- vapply(seq_len(5), function(i) {
                    !is.null(self$options[[paste0("test", i)]]) &&
                        !is.null(self$options[[paste0("test", i, "Positive")]])
                }, logical(1))

                if (sum(configured) < 2) {
                    self$results$instructions$setVisible(TRUE)
                    self$results$clinical_summary$setContent("")
                    self$results$clinical_summary$setVisible(FALSE)

                    # Get method-specific content
                    method_info <- private$.getMethodSpecificContent()

                    # Show welcome/instruction message
                    instructions <- paste0(
                        "<html><head></head><body>",
                        "<div class='instructions' style='background-color: rgba(138, 155, 172, 0.06); padding: 20px; border-radius: 8px; margin: 20px 0; color: inherit;'>",
                        "<h3 style='color: #2e7d32; margin-top: 0;'> ", .("Analysis Without Gold Standard"), "</h3>",
                        "<p><strong>", .("Analyze diagnostic test performance when no perfect reference test (gold standard) is available."), "</strong></p>",
                        "<p>", .("This analysis estimates latent-class parameters or describes agreement with a reference rule built from the selected tests. Interpretation depends on the chosen method and its assumptions."), "</p>",
                        "<h4 style='color: #2e7d32;'>", .("Required Steps:"), "</h4>",
                        "<ol>",
                        "<li><strong>", .("Select Test Variables:"), "</strong> ", .("Choose at least 2 diagnostic tests to analyze"), "</li>",
                        "<li><strong>", .("Define Positive Levels:"), "</strong> ", .("Specify which level represents a positive test result for each test"), "</li>",
                        "<li><strong>", .("Choose Analysis Method:"), "</strong> ", .("Select from available statistical approaches:"),
                        "<ul>",
                        "<li><strong>", .("Latent Class Analysis:"), "</strong> ", .("Conditional-independence two-class mixture model"), "</li>",
                        "<li><strong>", .("Penalized EM (MAP-like):"), "</strong> ", .("Fixed-prior point estimation; not a full Bayesian posterior analysis"), "</li>",
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
                                "<h4 style='color: #2e7d32; margin-top: 0;'> ", .("Active Illustrative Example"), "</h4>",
                                "<p><strong>", .("Example only:"), "</strong> ", .("This scenario is illustrative, does not change settings, and is not a clinical guide or validated recommendation."), "</p>",
                                "<p><strong>", .("Scenario"), ":</strong> ", self$options$clinicalPreset, "</p>",
                                "<p><strong>", .("Description"), ":</strong> ", private$.preset_info$description, "</p>",
                                "<p><strong>", .("Example context"), ":</strong> ", private$.preset_info$guidance, "</p>",
                                "<p><strong>", .("Example method"), ":</strong> ", private$.preset_info$method, "</p>",
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
                    self$results$clinical_summary$setVisible(isTRUE(self$options$showSummary))
                    return(FALSE) # Analysis ready
                }
            },
            .getMethodSpecificContent = function() {
                method <- self$options$method
                method_content <- switch(method,
                    latent_class = list(
                        title = .("Statistical background: Latent class analysis"),
                        bullets = c(
                            .("This method fits two unobserved classes and estimates each test's selected-positive probability within each class."),
                            .("At least three tests are required. With exactly three tests the model is just-identified, so goodness-of-fit statistics cannot test its assumptions."),
                            .("The model assumes conditional independence within each latent class. A large bivariate residual is evidence against that assumption but does not determine the direction of bias."),
                            .("Class labels are unidentified. Calling the high-positive class disease requires a study-specific substantive justification.")
                        ),
                        citations = c(
                            "Hui SL, Walter SD. Estimating the error rates of diagnostic tests. <em>Biometrics</em>. 1980;36(1):167-71.",
                            "Dendukuri N, Joseph L. Bayesian approaches to modeling the conditional dependence between multiple diagnostic tests. <em>Biometrics</em>. 2001;57(1):158-67."
                        )
                    ),
                    bayesian = list(
                        title = .("Statistical background: Penalized EM"),
                        bullets = c(
                            .("This method fits the same conditional-independence two-class model using fixed Beta penalties and returns one MAP-like point estimate."),
                            .("At least three tests are required; the two-test model is not statistically identifiable."),
                            .("This is not posterior sampling, the priors are not configurable, and its intervals are bootstrap confidence intervals rather than Bayesian credible intervals."),
                            .("Class labels remain unidentified, so diagnostic-accuracy terminology requires a study-specific substantive justification.")
                        ),
                        citations = c(
                            "Joseph L, Gyorkos TW, Coupal L. Bayesian estimation of disease prevalence and the parameters of diagnostic tests. <em>Am J Epidemiol</em>. 1995;141(3):263-72."
                        )
                    ),
                    composite = list(
                        title = .("Statistical background: Composite reference rule"),
                        bullets = c(
                            .("This method defines rule-positive cases by majority vote of the selected tests."),
                            .("Each test helps construct the rule against which it is evaluated, creating incorporation bias."),
                            .("The results describe agreement with the majority-vote rule and are not estimates of diagnostic accuracy."),
                            .("A strict majority means more than half of the tests are positive. With an even number of tests, ties are classified as rule negative and must be interpreted as part of the chosen rule.")
                        ),
                        citations = c(
                            "Alonzo TA, Pepe MS. Using a combination of reference tests to assess the accuracy of a new diagnostic test. <em>Stat Med</em>. 1999;18(22):2987-3003.",
                            "Reitsma JB, et al. A review of solutions for diagnostic accuracy studies with an imperfect or missing reference standard. <em>J Clin Epidemiol</em>. 2009;62(8):797-806."
                        )
                    ),
                    all_positive = list(
                        title = .("Statistical background: All-tests-positive rule"),
                        bullets = c(
                            .("This method defines a case as rule positive only when every selected test is positive."),
                            .("Each test is part of the rule against which it is evaluated, so the results are affected by incorporation bias."),
                            .("The positive-response probability and NPV are fixed at 100% by construction and therefore are left blank."),
                            .("The remaining values describe agreement with the rule and are not estimates of diagnostic accuracy.")
                        ),
                        citations = character(0)
                    ),
                    any_positive = list(
                        title = .("Statistical background: Any-test-positive rule"),
                        bullets = c(
                            .("This method defines a case as rule positive when at least one selected test is positive."),
                            .("Each test is part of the rule against which it is evaluated, so the results are affected by incorporation bias."),
                            .("The negative-response probability and PPV are fixed at 100% by construction and therefore are left blank."),
                            .("The remaining values describe agreement with the rule and are not estimates of diagnostic accuracy.")
                        ),
                        citations = character(0)
                    ),
                    list(
                        title = .("Statistical background"),
                        bullets = .("Choose a method according to the estimand and assumptions in the study-specific analysis plan."),
                        citations = character(0)
                    )
                )

                background <- paste0(
                    "<h4 style='color: #2e7d32;'>", method_content$title, "</h4><ul>",
                    paste0("<li>", method_content$bullets, "</li>", collapse = ""),
                    "</ul>"
                )
                references <- if (length(method_content$citations) == 0) {
                    ""
                } else {
                    paste0(
                        "<h4 style='color: #2e7d32;'>", .("References"), "</h4><ul>",
                        paste0("<li>", method_content$citations, "</li>", collapse = ""),
                        "</ul>"
                    )
                }

                list(background = background, references = references)
            },
            .run = function() {
                # Reset notices for new analysis run
                private$.noticeList <- list()
                private$.diagLines <- character(0)
                private$.boot_cache <- NULL
                private$.preset_info <- NULL
                self$results$clinical_summary$setContent("")

                # .init() sees only the initial option values. Refresh this optional
                # panel on every run so switching it on after analysis creation does not
                # reveal an empty result item.
                private$.showMethodGuide()

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
                if (self$options$method == "bayesian" && length(tests) < 3) {
                    jmvcore::reject(.("Penalized EM requires at least 3 tests. A two-class model over two binary tests is not statistically identifiable, so its prevalence and response probabilities cannot be estimated from the data."))
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
                        sprintf("This analysis uses the %d of %d cases (%.1f%%) with a result recorded for every selected test. All displayed estimates use these complete cases. If the probability of a missing result is related to an unobserved test result or latent class, the estimates can be biased.",
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
                    results <- private$.runLCA(binary_data, tests)
                } else if (self$options$method == "composite") {
                    results <- private$.runComposite(binary_data)
                } else if (self$options$method == "bayesian") {
                    results <- private$.runBayesian(binary_data)
                } else if (self$options$method == "all_positive") {
                    results <- private$.runAllPositive(binary_data)
                } else if (self$options$method == "any_positive") {
                    results <- private$.runAnyPositive(binary_data)
                }

                if (self$options$method %in% c("latent_class", "bayesian")) {
                    private$.addNotice(
                        "STRONG_WARNING",
                        .("Latent classes are unlabeled"),
                        .("The model cannot determine which class is disease. Results are oriented so that the class with the higher average selected-positive probability is the high-positive class. Calling that class diseased, and the response probabilities sensitivity and specificity, requires the substantive assumption that selected-positive results are more common in diseased people for most tests. If tests perform below chance or positive levels are reversed, the clinical interpretation is inverted.")
                    )

                    if (is.finite(results$prevalence) &&
                        (results$prevalence < 0.05 || results$prevalence > 0.95)) {
                        private$.addNotice(
                            "STRONG_WARNING",
                            .("Extreme fitted class proportion"),
                            .fmt(
                                .("The fitted high-positive class contains {proportion}% of analyzed cases. This is a diagnostic warning, not a clinical threshold: class orientation is uncertain and predictive values and response-probability estimates can be unstable near the boundary."),
                                proportion = base::format(round(100 * results$prevalence, 1), nsmall = 1)
                            )
                        )
                    }
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

                    if (isTRUE(self$options$showSummary)) {
                        clinical_summary <- private$.generateClinicalSummary(
                            results,
                            self$options$method,
                            tests
                        )
                        self$results$clinical_summary$setContent(clinical_summary)
                    }
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

                # Populate Agreement Statistics Table
                private$.populateAgreementStats(test_data, tests, test_levels)

                # Render collected notices (plain-text Preformatted output)
                private$.renderDiagnostics()
                private$.renderNotices()
            },
            .wilsonCI = function(successes, total, alpha) {
                if (!is.finite(successes) || !is.finite(total) || total <= 0 ||
                    successes < 0 || successes > total) {
                    return(list(lower = NA_real_, upper = NA_real_))
                }

                z <- stats::qnorm(1 - alpha / 2)
                p <- successes / total
                z2 <- z^2
                denominator <- 1 + z2 / total
                centre <- (p + z2 / (2 * total)) / denominator
                half_width <- z * sqrt(p * (1 - p) / total + z2 / (4 * total^2)) /
                    denominator

                list(
                    lower = max(0, centre - half_width),
                    upper = min(1, centre + half_width)
                )
            },

            .populatePrevalence = function(results) {
                if (is.null(results)) {
                    return()
                }

                prevalence <- results$prevalence

                method <- self$options$method
                latent_method <- method %in% c("latent_class", "bayesian")

                if (isTRUE(self$options$bootstrap)) {
                    ci <- private$.bootCI(private$.boot_cache$prevalence, self$options$alpha)
                    ci_lower <- ci$lower
                    ci_upper <- ci$upper
                } else if (latent_method) {
                    # A fitted latent-class proportion is not an observed binomial
                    # proportion. A plug-in sqrt(p(1-p)/n) interval ignores uncertainty
                    # in class membership and the response probabilities, and can be much
                    # too narrow. Leave the interval blank unless the model is refitted in
                    # bootstrap resamples.
                    ci_lower <- NA_real_
                    ci_upper <- NA_real_
                } else {
                    n <- nrow(results$data)
                    ci <- private$.wilsonCI(round(prevalence * n), n, self$options$alpha)
                    ci_lower <- ci$lower
                    ci_upper <- ci$upper
                }

                table <- self$results$prevalence
                if (latent_method) {
                    table$setNote(
                        "meaning",
                        .("This is the fitted proportion of the high-positive latent class. It is not automatically disease prevalence because latent-class labels are unidentified.")
                    )
                } else {
                    table$setNote(
                        "meaning",
                        .("This is the proportion of analyzed cases meeting the selected reference rule, not an estimate of disease prevalence.")
                    )
                }
                table$setNote(
                    "ci_method",
                    if (isTRUE(self$options$bootstrap)) {
                        .("Confidence limits are case-resampling percentile bootstrap intervals obtained by refitting the selected method.")
                    } else if (latent_method) {
                        .("Confidence limits are not reported for latent-model parameters without bootstrap refitting because ordinary binomial intervals do not include latent-variable estimation uncertainty. Enable Bootstrap CI to obtain intervals.")
                    } else {
                        .("Confidence limits are Wilson score intervals for the observed proportion meeting the reference rule.")
                    }
                )
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

                # Use the analyzed test names aligned with the returned response-
                # probability vectors. results$data contains only tests with both a
                # variable and a selected positive level.
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
                    else if (self$options$method %in% c("latent_class", "bayesian"))
                        sprintf(jmvcore::.("%.0f%% confidence intervals are not reported for latent-model response probabilities without bootstrap refitting. Ordinary binomial intervals omit uncertainty from estimating the latent classes and can be substantially too narrow. Enable Bootstrap CI to obtain intervals."),
                                conf_pct)
                    else
                        sprintf(jmvcore::.("%.0f%% intervals are Wilson score intervals for agreement with the observed reference rule."),
                                conf_pct)
                )

                # These two "methods" build the reference standard out of the very tests
                # being evaluated, so each test is scored against a standard that contains
                # its own result. That is incorporation bias, and for one metric it is total.
                meth <- self$options$method
                if (identical(meth, "latent_class")) {
                    table$setNote(
                        "latent_label",
                        .("Response probabilities are shown using sensitivity/specificity terminology only under the assumption that the high-positive latent class represents disease. Latent-class labels are not identified by the data.")
                    )
                } else if (identical(meth, "bayesian")) {
                    table$setNote(
                        "penalized_em",
                        .("These are fixed-prior penalized-EM point estimates, not posterior summaries. Accuracy terminology also assumes that the high-positive latent class represents disease.")
                    )
                } else if (identical(meth, "composite")) {
                    table$setNote(
                        "composite_rule",
                        .("These values describe agreement with a majority-vote rule built from the same tests. They are not estimates of diagnostic accuracy.")
                    )
                }
                # A strict majority over two tests is positive only when both tests are
                # positive, so this special case is exactly the all-positive rule.
                composite_two <- identical(meth, "composite") && length(tests) == 2
                if (composite_two) meth <- "all_positive"
                if (identical(meth, "all_positive") || identical(meth, "any_positive")) {
                    rule <- if (identical(meth, "all_positive"))
                        "every test is positive" else "at least one test is positive"
                    private$.addNotice(
                        "STRONG_WARNING",
                        .("This method cannot estimate accuracy"),
                        sprintf(jmvcore::.("The reference rule is defined as \"%s\", so each test is compared against a rule built from its own result. %s are therefore fixed at 100%% by construction on every dataset and are left blank rather than reported as findings. The remaining values are also affected by this circularity and describe agreement with the rule, not diagnostic accuracy."),
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
                    } else if (self$options$method %in% c("latent_class", "bayesian")) {
                        sens_ci <- list(lower = NA_real_, upper = NA_real_)
                        spec_ci <- list(lower = NA_real_, upper = NA_real_)
                    } else {
                        n_total <- nrow(results$data)
                        n_rule_positive <- round(n_total * results$prevalence)
                        n_rule_negative <- n_total - n_rule_positive
                        sens_ci <- private$.wilsonCI(
                            round(sensitivity * n_rule_positive),
                            n_rule_positive,
                            self$options$alpha
                        )
                        spec_ci <- private$.wilsonCI(
                            round(specificity * n_rule_negative),
                            n_rule_negative,
                            self$options$alpha
                        )
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
                    degenerate_sens <- identical(self$options$method, "all_positive") ||
                        (identical(self$options$method, "composite") && length(tests) == 2)
                    degenerate_spec <- identical(self$options$method, "any_positive")

                    if (degenerate_sens) {
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

                y <- model$y
                if (is.null(y) || nrow(y) == 0) return()
                var_cols <- intersect(names(y), unlist(tests))
                if (length(var_cols) < 2) return()

                THRESHOLD <- stats::qchisq(0.95, df = 1)   # 3.841
                flagged <- character(0)

                for (i in seq_len(length(var_cols) - 1)) {
                    for (j in seq(i + 1, length(var_cols))) {
                        a <- var_cols[i]
                        b <- var_cols[j]
                        bvr <- private$.bivariateResidual(model, a, b)
                        if (!is.finite(bvr)) next
                        pair <- paste(a, "vs", b)
                        if (bvr > THRESHOLD) flagged <- c(flagged, pair)
                        if (!is.null(table)) table$addRow(rowKey = pair, values = list(
                            pair = pair,
                            bvr = bvr,
                            verdict = if (bvr > THRESHOLD)
                                jmvcore::.("Evidence against conditional independence")
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
                        .("Tests do not appear to err independently"),
                        sprintf(jmvcore::.("%s have bivariate residuals above the descriptive threshold. This is evidence that the conditional-independence model does not reproduce those pairwise tables. Because the residual is squared, it does not show the direction or cause of the dependence and cannot determine whether fitted response probabilities are biased upward or downward. Do not interpret the latent-class parameters as diagnostic accuracy without a dependence-sensitive model or a study-specific sensitivity analysis."),
                                paste(flagged, collapse = ", "))
                    )
                }
            },

            .bivariateResidual = function(model, a, b) {
                # poLCA$predcell contains only response patterns observed in the data.
                # Aggregating that table omits fitted expected counts for unobserved
                # higher-order patterns and can understate pairwise residuals. Construct
                # the complete 2 x 2 expected margin directly from the fitted model.
                if (is.null(model$y) || is.null(model$probs) || is.null(model$P) ||
                    !a %in% names(model$y) || !b %in% names(model$y) ||
                    !a %in% names(model$probs) || !b %in% names(model$probs)) {
                    return(NA_real_)
                }

                probs_a <- model$probs[[a]]
                probs_b <- model$probs[[b]]
                class_weights <- model$P
                n_classes <- min(nrow(probs_a), nrow(probs_b), length(class_weights))
                if (n_classes < 1 || ncol(probs_a) != 2 || ncol(probs_b) != 2 ||
                    any(!is.finite(class_weights[seq_len(n_classes)]))) {
                    return(NA_real_)
                }

                observed <- table(
                    factor(as.character(model$y[[a]]), levels = colnames(probs_a)),
                    factor(as.character(model$y[[b]]), levels = colnames(probs_b))
                )
                n_obs <- sum(observed)
                if (n_obs <= 0) return(NA_real_)

                expected_prob <- matrix(0, nrow = 2, ncol = 2)
                for (class in seq_len(n_classes)) {
                    expected_prob <- expected_prob + class_weights[class] *
                        outer(probs_a[class, ], probs_b[class, ])
                }
                expected <- n_obs * expected_prob
                if (any(!is.finite(expected)) || any(expected < 0)) return(NA_real_)

                sum((observed - expected)^2 / pmax(expected, 1e-9))
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
            .lcaConverged = function(model, maxiter = 1000L) {
                if (is.null(model)) {
                    return(FALSE)
                }

                iterations <- model$numiter
                likelihood <- model$llik
                is.numeric(iterations) && length(iterations) == 1L &&
                    is.finite(iterations) && iterations < maxiter &&
                    is.numeric(likelihood) && length(likelihood) == 1L &&
                    is.finite(likelihood) && likelihood > -1e10
            },
            .runLCA = function(binary_data, tests, n_starts = 30L, probs_start = NULL) {
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
                best_any_model <- NULL
                best_any_llik <- -Inf
                stalled_starts <- 0L # consecutive starts with no improvement
                prev_best <- -Inf
                failed_starts <- 0L
                nonconverged_starts <- 0L
                starts_used <- 0L

                for (start in 1:n_starts) {
                    starts_used <- start
                    # Checkpoint periodically during LCA iterations
                    if (start %% 10 == 1) { # Every 10 starts
                        private$.checkpoint(flush = FALSE) # Poll for changes only
                    }

                    # Stop once extra random starts stop finding a better optimum. The
                    # previous condition was `(best_llik - (-Inf)) > 0.001`, which is Inf >
                    # 0.001 -- always TRUE -- so it broke unconditionally at start 21 and
                    # never examined convergence at all, despite n_starts being 30.
                    if (start > 10 && is.finite(best_llik) && stalled_starts >= 10) {
                        break
                    }

                    iter_seed <- private$.seedValue(start * 100)

                    tryCatch(
                        {
                            model <- withr::with_seed(
                                iter_seed,
                                poLCA::poLCA(
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
                            )

                            if (!is.null(model) && is.finite(model$llik)) {
                                if (model$llik > best_any_llik) {
                                    best_any_model <- model
                                    best_any_llik <- model$llik
                                }
                                if (private$.lcaConverged(model, maxiter = 1000L)) {
                                    if (model$llik > best_llik) {
                                        best_model <- model
                                        best_llik <- model$llik
                                    }
                                } else {
                                    nonconverged_starts <- nonconverged_starts + 1L
                                }
                            } else {
                                failed_starts <- failed_starts + 1L
                            }
                        },
                        error = function(e) {
                            failed_starts <<- failed_starts + 1L
                        }
                    )
                    if (best_llik > prev_best) stalled_starts <- 0L else stalled_starts <- stalled_starts + 1L
                    prev_best <- best_llik
                }

                if (is.null(best_model)) {
                    best_model <- best_any_model
                    best_llik <- best_any_llik
                }
                if (is.null(best_model)) {
                    jmvcore::reject(.("LCA model fitting failed after all attempts. Try a different method or check your data."))
                }

                private$.diag(sprintf("LCA             : %d random start(s) used of %d allowed; %d failed; %d did not converge; best log-likelihood %.4f",
                                      starts_used, n_starts, failed_starts,
                                      nonconverged_starts, best_llik))

                unstable_starts <- failed_starts + nonconverged_starts
                if (starts_used > 0 && unstable_starts / starts_used >= 0.75) {
                    private$.addNotice(
                        "STRONG_WARNING",
                        .("Most latent-class starts did not converge"),
                        .fmt(
                            .("{unstable} of {starts} attempted starts failed, reached the iteration limit, or returned an invalid likelihood ({failed} fitting failures; {nonconverged} unfinished fits). The selected fit is the best converged solution when one was available, but this pattern indicates an unstable likelihood surface. Do not report the estimates without additional model checks."),
                            unstable = unstable_starts,
                            nonconverged = nonconverged_starts,
                            failed = failed_starts,
                            starts = starts_used
                        )
                    )
                }

                iterations <- if (is.null(best_model$numiter)) NA_integer_ else best_model$numiter
                # poLCA$eflag means that poLCA automatically restarted after a
                # numerical problem; the returned model is the final successful run.
                # It is not a nonconvergence flag. Convergence is instead determined
                # from the iteration limit and finite fitted output.
                restarted <- isTRUE(best_model$eflag)
                converged <- private$.lcaConverged(best_model, maxiter = 1000L)
                private$.diag("poLCA restart  : ",
                              if (restarted) "automatic numerical restart occurred" else "none")

                # Extract results
                # Orient the otherwise unlabeled classes by average selected-positive
                # probability. This is a naming convention, not evidence that the class is
                # disease; the main run displays that limitation as a strong warning.
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
                healthy_class <- 3 - disease_class # The lower-positive class

                # Fitted proportion of the high-positive class.
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
                    disease_class = disease_class,
                    converged = converged,
                    restarted = restarted,
                    iterations = iterations,
                    starts_used = starts_used,
                    failed_starts = failed_starts
                ))
            },
            .runComposite = function(binary_data) {
                # Strict majority means MORE than half positive. The previous >= 0.5 rule
                # silently classified ties as positive, contradicting the method label and
                # making two-test composite identical to any-positive. Ties are now rule
                # negative; the output explicitly discloses that convention for even k.
                if (ncol(binary_data) >= 3) {
                    # Even with a genuine majority, each test votes on the standard it is
                    # then scored against. The magnitude and direction of incorporation bias
                    # depend on the data-generating process, so do not attach a universal
                    # percentage to it.
                    private$.addNotice(
                        "WARNING",
                        .("Composite reference has incorporation bias"),
                        .("The reference rule is a majority vote of the same tests being evaluated, so each test helps decide the answer against which it is compared. This incorporation bias can materially distort the displayed agreement probabilities, but its magnitude and direction are study-specific. Treat these as agreement with the majority rule, not as diagnostic accuracy.")
                    )
                }

                if (ncol(binary_data) == 2) {
                    private$.addNotice(
                        "STRONG_WARNING",
                        .("Two-test composite reduces to the all-positive rule"),
                        .("With two tests, a strict majority requires both tests to be positive; a one-positive tie is rule negative. This is identical to the all-tests-positive rule, so the positive-response probability and NPV are fixed at 100% by construction and are left blank. Add a third test for a non-degenerate majority rule; all values from a self-built rule describe agreement, not diagnostic accuracy.")
                    )
                }
                composite <- rowMeans(binary_data, na.rm = TRUE) > 0.5

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

            # Fixed-prior, penalized latent-class EM. The public option name remains
            # `bayesian` for compatibility, but this is not posterior sampling.
            .runBayesian = function(binary_data) {
                # Number of tests and patients
                num_tests <- ncol(binary_data)
                num_patients <- nrow(binary_data)
                binary_matrix <- as.matrix(binary_data)

                # A two-class model over k binary tests has 2k + 1 free parameters and
                # 2^k - 1 degrees of freedom, so k = 2 gives 5 parameters for 3 df: the
                # model is NOT identified and the estimates are determined by the starting
                # values and the prior rather than by the data. latent_class already refuses
                # this case; the EM here accepted it silently and returned numbers.
                if (num_tests < 3) {
                    jmvcore::reject(.("Penalized EM requires at least 3 tests because the two-test latent-class model is not statistically identifiable."))
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
                    "Fixed priors used by penalized EM",
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

                    # E-step, vectorized over patients. Work on the log-odds scale to avoid
                    # underflow when several tests strongly favor one class.
                    prevalence <- max(0.001, min(0.999, prevalence))
                    log_odds <- rep(stats::qlogis(prevalence), num_patients)
                    sens_clamped <- pmax(0.001, pmin(0.999, sensitivities))
                    spec_clamped <- pmax(0.001, pmin(0.999, specificities))

                    for (j in seq_len(num_tests)) {
                        observed <- binary_matrix[, j]
                        contribution <- ifelse(
                            is.na(observed),
                            0,
                            ifelse(
                                observed == 1,
                                log(sens_clamped[j] / (1 - spec_clamped[j])),
                                log((1 - sens_clamped[j]) / spec_clamped[j])
                            )
                        )
                        log_odds <- log_odds + contribution
                    }
                    prob_disease <- stats::plogis(log_odds)
                    prob_disease[!is.finite(prob_disease)] <- prevalence

                    # M-step: Update parameters
                    # Update prevalence
                    new_prevalence <- (sum(prob_disease, na.rm = TRUE) + alpha_prev - 1) /
                        (num_patients + alpha_prev + beta_prev - 2)

                    # Update sensitivities and specificities
                    new_sensitivities <- numeric(num_tests)
                    new_specificities <- numeric(num_tests)

                    for (j in 1:num_tests) {
                        # For each test, get non-NA values
                        not_na <- !is.na(binary_matrix[, j])
                        if (sum(not_na) == 0) {
                            # If all values are NA, keep previous estimates
                            new_sensitivities[j] <- sensitivities[j]
                            new_specificities[j] <- specificities[j]
                            next
                        }

                        # Get test results and probabilities for non-NA values
                        test_results <- binary_matrix[not_na, j]
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
                        prevalence <- new_prevalence
                        sensitivities <- new_sensitivities
                        specificities <- new_specificities
                        converged <- TRUE
                        break
                    }

                    # Update parameters for next iteration
                    prevalence <- new_prevalence
                    sensitivities <- new_sensitivities
                    specificities <- new_specificities
                }

                # Make the reported class convention deterministic. If the fitted class
                # currently called positive has fewer selected-positive responses on average
                # than its complement, swap the class labels and transform the response
                # probabilities accordingly.
                mean_positive_current <- mean(sensitivities, na.rm = TRUE)
                mean_positive_complement <- mean(1 - specificities, na.rm = TRUE)
                if (is.finite(mean_positive_current) &&
                    is.finite(mean_positive_complement) &&
                    mean_positive_current < mean_positive_complement) {
                    old_sensitivities <- sensitivities
                    prevalence <- 1 - prevalence
                    sensitivities <- 1 - specificities
                    specificities <- 1 - old_sensitivities
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

                private$.diag(sprintf("Penalized EM    : %s after %d iteration(s) (limit %d)",
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
                reference <- apply(binary_data, 1, function(x) {
                    any(!is.na(x)) && all(x == 1, na.rm = TRUE)
                })

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
                seed_val <- private$.seedValue()
                # Scope the bootstrap RNG so running the analysis never changes the
                # caller's random-number stream.
                withr::local_seed(seed_val)
                # Draw every resample up front so nothing downstream can disturb the stream.
                idx <- matrix(sample.int(n, n * nboot, replace = TRUE), nrow = nboot, byrow = TRUE)

                # Replicates reuse the main estimators; keep their per-fit diagnostics and
                # warnings out of the output. Failures are summarized once below.
                previous_diag_suppression <- private$.diagSuppressed
                previous_notice_suppression <- private$.noticeSuppressed
                private$.diagSuppressed <- TRUE
                private$.noticeSuppressed <- TRUE
                on.exit({
                    private$.diagSuppressed <- previous_diag_suppression
                    private$.noticeSuppressed <- previous_notice_suppression
                }, add = TRUE)

                prevalence <- rep(NA_real_, nboot)
                sens <- matrix(NA_real_, nrow = nboot, ncol = n_tests)
                spec <- matrix(NA_real_, nrow = nboot, ncol = n_tests)
                error_count <- 0L

                for (b in seq_len(nboot)) {
                    if (b %% 25 == 1) private$.checkpoint(flush = FALSE)
                    boot_data <- data[idx[b, ], , drop = FALSE]

                    boot_result <- tryCatch({
                        if (method == "latent_class") {
                            private$.runLCA(boot_data, names(data),
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
                        } else {
                            NULL
                        }
                    }, error = function(e) NULL)

                    if (is.null(boot_result) || isFALSE(boot_result$converged)) {
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

                private$.diagSuppressed <- previous_diag_suppression
                private$.noticeSuppressed <- previous_notice_suppression

                if (error_count > 0) {
                    private$.addNotice(
                        "WARNING",
                        "Some bootstrap replicates failed",
                        sprintf("%d of %d bootstrap resamples (%.0f%%) could not be fitted and were discarded. The intervals below are based on the remaining %d. A high failure rate usually means the resamples are too small or too sparse to support the model.",
                                error_count, nboot, 100 * error_count / nboot, nboot - error_count)
                    )
                }

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
                        message(.fmt(.("Error in plot: {msg}"), msg = e$message))

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
                        message(.fmt(.("Error in ggplot: {msg}"), msg = e$message))

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
                        .fmt(
                            .("The latent-class analysis has N = {n}. N < 100 is a general stability warning, not a clinical adequacy threshold; precision also depends on class balance, response patterns, and the number of tests. Inspect bootstrap intervals and convergence, and consider collecting more observations."),
                            n = n_obs
                        )
                    )
                }

                if (method == "bayesian" && n_obs < 50) {
                    private$.addNotice(
                        "STRONG_WARNING",
                        "Penalized EM Sample Size",
                        .fmt(
                            .("The penalized-EM analysis has N = {n}. N < 50 is a general stability warning, not a clinical adequacy threshold; fixed priors may have substantial influence. Inspect bootstrap intervals and consider collecting more observations."),
                            n = n_obs
                        )
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
                            .fmt(
                                .("Test '{test}' has a category with fewer than 5 observations. Estimates may be unstable. Combine categories only when substantively defensible."),
                                test = test_name
                            )
                        )
                    }

                    # Check for extreme imbalances
                    min_prop <- min(test_values) / sum(test_values)
                    if (min_prop < 0.05) {
                        private$.addNotice(
                            "WARNING",
                            "Extreme Test Imbalance",
                            .fmt(
                                .("Test '{test}' is extremely imbalanced: the minority category is {percentage}% of observations. This may destabilize parameter estimation."),
                                test = test_name,
                                percentage = base::format(round(min_prop * 100, 1), nsmall = 1)
                            )
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
                        .("Composite ties are classified as rule negative"),
                        .("A strict majority requires more than half of the selected tests to be positive. With an even number of tests, tied cases are therefore rule negative. This is part of the rule definition, not evidence of disease absence; use an odd number of tests or a prespecified alternative if that convention is unsuitable.")
                    )
                }

                # Clinical context message
                if (self$options$verbose) {
                    private$.addNotice(
                        "INFO",
                        "Clinical Validation",
                        .fmt(
                            .("Analysis diagnostics: {tests} tests analyzed with N = {n} using method '{method}'."),
                            tests = n_tests,
                            n = n_obs,
                            method = method
                        )
                    )
                }
            },
            .showMethodGuide = function() {
                if (!isTRUE(self$options$showMethodGuide)) {
                    self$results$method_guide$setContent("")
                    return()
                }

                # Create comprehensive method selection guide in HTML
                guide_html <- paste0(
                    "<div style='background-color: rgba(138, 155, 172, 0.06); padding: 20px; border-radius: 8px; margin: 15px 0; border-left: 4px solid #007bff; color: inherit;'>",
                    "<h3 style='color: #007bff; margin-top: 0;'> ", .("Method Selection Guide"), "</h3>",
                    "<div style='margin: 15px 0; padding: 15px; background-color: rgba(33, 159, 33, 0.1); border-radius: 5px; color: inherit;'>",
                    "<p><strong>", .("Scope warning:"), "</strong> ", .("The scenarios and method examples below are illustrative examples only. They are not clinical guides, validated recommendations, or substitutes for a study-specific statistical analysis plan."), "</p>",
                    "<h4 style='color: #2e7d32; margin-top: 0;'> ", .("Latent Class Analysis"), "</h4>",
                    "<p><strong>", .("Description"), ":</strong> ", .("A two-class conditional-independence mixture model for three or more tests."), "</p>",
                    "<p><strong>", .("Illustrative context"), ":</strong> ", .("Studies with at least three tests whose errors are plausibly independent within each latent class."), "</p>",
                    "<p><strong>", .("Limitations"), ":</strong> ", .("Class labels are unidentified; the high-positive class is not automatically disease. The model assumes conditionally independent errors and does NOT model conditional dependence; with exactly three tests its fit cannot be tested."), "</p>",
                    "</div>",
                    "<div style='margin: 15px 0; padding: 15px; background-color: rgba(33, 152, 239, 0.13); border-radius: 5px; color: inherit;'>",
                    "<h4 style='color: #1565c0; margin-top: 0;'> ", .("Penalized EM (MAP-like; Fixed Priors)"), "</h4>",
                    "<p><strong>", .("Description"), ":</strong> ", .("A latent-class EM point estimator using fixed Beta priors."), "</p>",
                    "<p><strong>", .("Illustrative context"), ":</strong> ", .("Sensitivity analysis showing how the fixed penalties affect a conditional-independence fit."), "</p>",
                    "<p><strong>", .("Limitations"), ":</strong> ", .("This is not posterior sampling, priors are not configurable, intervals are not credible intervals, and latent-class labels remain unidentified."), "</p>",
                    "</div>",
                    "<div style='margin: 15px 0; padding: 15px; background-color: rgba(255, 169, 33, 0.14); border-radius: 5px; color: inherit;'>",
                    "<h4 style='color: #ef6c00; margin-top: 0;'> ", .("Composite Reference"), "</h4>",
                    "<p><strong>", .("Description"), ":</strong> ", .("Uses majority vote of available tests as pseudo-gold standard."), "</p>",
                    "<p><strong>", .("Illustrative context"), ":</strong> ", .("Exploratory description of agreement with a majority-vote rule using three or more tests."), "</p>",
                    "<p><strong>", .("Limitations"), ":</strong> ", .("This is not an accuracy estimate: each test helps build the rule against which it is evaluated, creating incorporation bias. Strict majority means more than half positive; with an even number of tests, tied cases are rule negative. With two tests this is identical to the all-tests-positive rule."), "</p>",
                    "</div>",
                    "<div style='margin: 15px 0; padding: 15px; background-color: rgba(230, 33, 99, 0.12); border-radius: 5px; color: inherit;'>",
                    "<h4 style='color: #c2185b; margin-top: 0;'> ", .("All Tests Positive"), "</h4>",
                    "<p><strong>", .("Description"), ":</strong> ", .("Defines a case as rule positive only when every selected test is positive."), "</p>",
                    "<p><strong>", .("Illustrative context"), ":</strong> ", .("Exploring agreement with the strict rule that every selected test is positive."), "</p>",
                    "<p><strong>", .("Limitations"), ":</strong> ", .("Each test is part of the rule against which it is evaluated, creating incorporation bias. The positive-response probability and NPV are fixed at 100% by construction and are left blank; the other values describe agreement with the rule, not diagnostic accuracy."), "</p>",
                    "</div>",
                    "<div style='margin: 15px 0; padding: 15px; background-color: rgba(33, 159, 33, 0.1); border-radius: 5px; color: inherit;'>",
                    "<h4 style='color: #388e3c; margin-top: 0;'> ", .("Any Test Positive"), "</h4>",
                    "<p><strong>", .("Description"), ":</strong> ", .("Defines a case as rule positive when at least one selected test is positive."), "</p>",
                    "<p><strong>", .("Illustrative context"), ":</strong> ", .("Exploring agreement with the permissive rule that at least one selected test is positive."), "</p>",
                    "<p><strong>", .("Limitations"), ":</strong> ", .("Each test is part of the rule against which it is evaluated, creating incorporation bias. The negative-response probability and PPV are fixed at 100% by construction and are left blank; the other values describe agreement with the rule, not diagnostic accuracy."), "</p>",
                    "</div>",
                    "<div style='margin: 15px 0; padding: 10px; background-color: rgba(255, 203, 33, 0.14); border-radius: 5px; border-left: 3px solid #ffb300; color: inherit;'>",
                    "<h4 style='color: #e65100; margin-top: 0;'> ", .("Interpretation Cautions"), "</h4>",
                    "<ul>",
                    "<li>", .("Match the method to the estimand: latent-class parameters and agreement with a self-built rule are different quantities."), "</li>",
                    "<li>", .("Assess conditional independence before interpreting latent-class response probabilities as accuracy."), "</li>",
                    "<li>", .("Treat all preset scenarios and examples as illustrative, not as clinical guidance."), "</li>",
                    "</ul>",
                    "</div>",
                    "</div>"
                )

                # Set the method guide content
                self$results$method_guide$setContent(guide_html)
            },
            .applyPreset = function() {
                preset <- self$options$clinicalPreset
                private$.preset_info <- NULL

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
                        description = .("Illustrative diagnostic-test validation scenario"),
                        guidance = .("Example context: comparing several imperfect biomarkers or diagnostic technologies")
                    ),
                    pathology_agreement = list(
                        method = "composite",
                        bootstrap = FALSE,
                        nboot = 500,
                        alpha = 0.05,
                        verbose = FALSE,
                        description = .("Illustrative observer-agreement scenario"),
                        guidance = .("Example context: describing agreement among pathologists or other observers")
                    ),
                    tumor_markers = list(
                        method = "latent_class",
                        bootstrap = TRUE,
                        nboot = 1000,
                        alpha = 0.05,
                        verbose = TRUE,
                        description = .("Illustrative tumor-marker scenario"),
                        guidance = .("Example context: exploring several cancer biomarkers or prognostic tests")
                    ),
                    screening_evaluation = list(
                        method = "any_positive",
                        bootstrap = TRUE,
                        nboot = 500,
                        alpha = 0.05,
                        verbose = FALSE,
                        description = .("Illustrative screening-rule scenario"),
                        guidance = .("Example context: describing agreement with an any-positive rule across several tests")
                    )
                )

                # A jamovi backend cannot write self$options -- the GUI owns them -- so a
                # preset can only illustrate. Previously it stored the config and printed it via
                # message(), which no jamovi user ever sees, so all five presets produced
                # byte-identical analyses: same method, same bootstrap, same nboot, same
                # numbers. The control looked like it did something and did not.
                # It now reports example settings and how they compare with the current ones.
                if (preset %in% names(presets)) {
                    preset_config <- presets[[preset]]
                    private$.preset_info <- preset_config

                    cur <- list(method = self$options$method,
                                bootstrap = isTRUE(self$options$bootstrap),
                                nboot = self$options$nboot,
                                alpha = self$options$alpha)
                    diffs <- character(0)
                    if (!identical(cur$method, preset_config$method))
                        diffs <- c(diffs, sprintf("Analysis method: currently \"%s\", example \"%s\"",
                                                  cur$method, preset_config$method))
                    if (!identical(cur$bootstrap, isTRUE(preset_config$bootstrap)))
                        diffs <- c(diffs, sprintf("Bootstrap confidence intervals: currently %s, example %s",
                                                  if (cur$bootstrap) "on" else "off",
                                                  if (isTRUE(preset_config$bootstrap)) "on" else "off"))
                    if (!isTRUE(all.equal(cur$nboot, preset_config$nboot)))
                        diffs <- c(diffs, sprintf("Bootstrap samples: currently %g, example %g",
                                                  cur$nboot, preset_config$nboot))

                    private$.addNotice(
                        if (length(diffs) > 0) "WARNING" else "INFO",
                        sprintf("Illustrative example: %s", gsub("_", " ", preset)),
                        if (length(diffs) > 0)
                            sprintf("Example only -- not clinical guidance. %s %s This example does not change settings. For comparison, its example settings differ as follows: %s.",
                                    preset_config$description, preset_config$guidance,
                                    paste(diffs, collapse = "; "))
                        else
                            sprintf("Example only -- not clinical guidance. %s %s Current settings happen to match this illustrative example.",
                                    preset_config$description, preset_config$guidance)
                    )
                }
            },
            .generateClinicalSummary = function(results, method, tests) {
                if (is.null(results)) {
                    return("")
                }

                n_tests <- length(tests)
                prev_pct <- if (is.finite(results$prevalence))
                    sprintf("%.1f%%", results$prevalence * 100) else .("not estimable")

                # For the reference-rule methods, `prevalence` is the proportion of cases
                # satisfying the rule (all tests agree positive / any test positive), NOT an
                # estimate of disease prevalence, and one of sensitivity/specificity is fixed
                # at 1 by construction. This panel read results$sensitivities directly and so
                # still announced "Range from 100.0% to 100.0%" after the table stopped doing
                # so.
                rule_based <- method %in% c("composite", "all_positive", "any_positive")
                prev_label <- if (rule_based) {
                    .("Cases meeting the reference rule")
                } else {
                    .("High-positive latent-class proportion")
                }
                degenerate_sens <- identical(method, "all_positive") ||
                    (identical(method, "composite") && n_tests == 2)
                method_label <- switch(method,
                    latent_class = .("Latent class analysis"),
                    bayesian = .("Penalized EM (MAP-like; fixed priors)"),
                    composite = .("Composite majority-vote reference rule"),
                    all_positive = .("All-tests-positive reference rule"),
                    any_positive = .("Any-test-positive reference rule"),
                    method
                )

                # Positive-response probability in the fitted high-positive class, or
                # positive agreement with the selected rule for rule-based methods.
                finite_sens <- results$sensitivities[is.finite(results$sensitivities)]
                sens_range <- if (length(finite_sens) == 0) {
                    .("not estimable")
                } else {
                    .fmt(
                        .("Range from {minimum}% to {maximum}%"),
                        minimum = base::format(round(min(finite_sens) * 100, 1), nsmall = 1),
                        maximum = base::format(round(max(finite_sens) * 100, 1), nsmall = 1)
                    )
                }

                response_label <- if (rule_based) {
                    .("Positive agreement with the rule:")
                } else {
                    .("Positive-response probabilities in the high-positive class:")
                }
                response_value <- if (degenerate_sens) {
                    .("not reported - fixed at 100% by construction")
                } else {
                    sens_range
                }

                summary_html <- paste0(
                    "<div class='clinical-summary' style='background-color: rgba(33, 152, 255, 0.07); padding: 15px; border-radius: 8px; margin: 10px 0; color: inherit;'>",
                    "<h4 style='color: #1565c0; margin-top: 0;'> ", .("Plain-Language Summary"), "</h4>",
                    "<p><strong>", .("Analysis:"), "</strong> ", method_label, "</p>",
                    "<p><strong>", .("Tests analyzed:"), "</strong> ", paste(htmltools::htmlEscape(unlist(tests)), collapse = ", "), " (N=", n_tests, ")</p>",
                    "<p><strong>", prev_label, "</strong> ", prev_pct,
                    if (rule_based) paste0(" <em>", .("(this is the share of cases satisfying the rule, not an estimate of disease prevalence)"), "</em>") else "",
                    "</p>",
                    "<p><strong>", response_label, "</strong> ",
                    if (degenerate_sens) paste0("<em>", response_value, "</em>") else response_value,
                    "</p>",
                    if (rule_based)
                        paste0("<p><strong>", .("Caution:"), "</strong> ",
                               .("this method scores each test against a reference built from the tests themselves, so the figures describe agreement with that rule rather than diagnostic accuracy."), "</p>")
                    else paste0("<p><strong>", .("Caution:"), "</strong> ",
                                .("latent classes are unlabeled. Interpreting the high-positive class as disease, and these response probabilities as accuracy, requires a study-specific substantive justification."), "</p>"),
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
                calculate_kappa <- function(var1, pos1, var2, pos2) {
                    # Agreement is about selected positive/negative meaning, not literal factor
                    # labels. Two semantically identical tests may use labels such as
                    # neg/pos and negative/positive; pooling those label sets makes every
                    # observation appear discordant.
                    binary1 <- ifelse(is.na(var1), NA, var1 == pos1)
                    binary2 <- ifelse(is.na(var2), NA, var2 == pos2)
                    tbl_full <- table(
                        factor(binary1, levels = c(FALSE, TRUE)),
                        factor(binary2, levels = c(FALSE, TRUE))
                    )

                    n <- sum(tbl_full)
                    if (n == 0)
                        return(list(kappa = NA_real_, p_value = NA_real_, agreement = NA_real_))

                    p_o <- sum(diag(tbl_full)) / n

                    row_sums <- rowSums(tbl_full)
                    col_sums <- colSums(tbl_full)
                    p_e <- sum(row_sums * col_sums) / (n^2)

                    chance_denominator <- 1 - p_e
                    if (!is.finite(chance_denominator) || chance_denominator <= .Machine$double.eps) {
                        return(list(kappa = NA_real_, p_value = NA_real_, agreement = p_o))
                    }
                    kappa <- (p_o - p_e) / chance_denominator

                    # The p-value tests H0: kappa = 0 and therefore needs the null
                    # variance. vcd::Kappa's ASE is the non-null variance intended for
                    # confidence intervals; dividing kappa by that ASE does not perform
                    # the stated null test. irr::kappa2 implements the standard null-
                    # variance Wald test for two raters.
                    kappa_test <- tryCatch(
                        irr::kappa2(
                            data.frame(
                                first = factor(binary1, levels = c(FALSE, TRUE)),
                                second = factor(binary2, levels = c(FALSE, TRUE))
                            ),
                            weight = "unweighted"
                        ),
                        error = function(e) NULL
                    )
                    p_value <- if (is.null(kappa_test) ||
                        !is.finite(kappa_test$p.value)) {
                        NA_real_
                    } else {
                        kappa_test$p.value
                    }

                    return(list(kappa = kappa, p_value = p_value, agreement = p_o))
                }

                for (i in 1:(n_tests - 1)) {
                    for (j in (i + 1):n_tests) {
                        test1 <- tests[[i]]
                        test2 <- tests[[j]]

                        res <- calculate_kappa(
                            test_data[[test1]], test_levels[[i]],
                            test_data[[test2]], test_levels[[j]]
                        )

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
                    .("Kappa compares the selected positive/negative meanings, even when tests use different factor labels. P-values test kappa = 0 using the null-variance Wald statistic from irr::kappa2. They are unadjusted across pairs and should be treated as exploratory; interpret them cautiously in small or sparse samples. Kappa is not estimable when both tests are constant.")
                )
            }
        )
    )
}
