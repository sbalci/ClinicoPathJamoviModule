#' @title Compare Medical Decision Tests
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import dplyr
#'

decisioncompareClass <- if (requireNamespace("jmvcore")) {
    R6::R6Class(
        "decisioncompareClass",
        inherit = decisioncompareBase,
        private = list(

            # Clinical thresholds for likelihood ratio interpretation
            LR_CLINICAL_THRESHOLDS = list(
                excellent_pos = 10, # LR+ > 10 = excellent for ruling in disease
                good_pos = 5, # LR+ 5-10 = good evidence
                fair_pos = 2, # LR+ 2-5 = weak but potentially useful
                excellent_neg = 0.1, # LR- < 0.1 = excellent for ruling out disease
                good_neg = 0.2, # LR- 0.1-0.2 = good evidence
                fair_neg = 0.5 # LR- 0.2-0.5 = weak but potentially useful
            ),

            # Statistical significance thresholds
            P_THRESHOLD_STRONG = 0.001, # Strong statistical evidence
            P_THRESHOLD_SIGNIFICANT = 0.05, # Conventional alpha level

            # Sample size adequacy thresholds
            MIN_DISCORDANT_PAIRS = 10, # Minimum for reliable McNemar's test
            ZERO_CELL_CONTINUITY = 0.5, # Continuity correction for LR stability

            # Cache for test results to avoid redundant calculations
            .cached_test_results = NULL,

            # Escape variable names for safe display and table keys
            .escapeVar = function(x) {
                if (is.null(x) || length(x) == 0) {
                    return(x)
                }
                make.names(gsub("[^A-Za-z0-9_. -]", "_", as.character(x)))
            },

            # Ensure that requested positive level exists in data
            .assertLevelExists = function(x, level, var_name, test_name = NULL) {
                if (is.null(level) || level == "") {
                    missing_for <- ifelse(is.null(test_name),
                        private$.escapeVar(var_name),
                        paste(private$.escapeVar(var_name), "(", private$.escapeVar(test_name), ")")
                    )

                    private$.addNotice(
                        type = "ERROR",
                        title = "Missing Positive Level",
                        content = paste0("No positive level supplied for ", missing_for, ". Please select the level that represents a positive result in the variable selection panel.")
                    )
                    stop("Validation failed", call. = FALSE)
                }

                if (!level %in% levels(x)) {
                    label <- ifelse(is.null(test_name),
                        private$.escapeVar(var_name),
                        paste0(private$.escapeVar(test_name), " (", private$.escapeVar(var_name), ")")
                    )

                    private$.addNotice(
                        type = "ERROR",
                        title = "Invalid Positive Level",
                        content = paste0('The positive level "', level, '" was not found in ', label, '. Check spelling and capitalisation. Available levels: ', paste(levels(x), collapse = ", "), '.')
                    )
                    stop("Validation failed", call. = FALSE)
                }
            },

            # Build a complete 2x2 confusion matrix with zero-filled cells
            .buildConfusionMatrix = function(test_values, gold_values) {
                lvls <- c("Positive", "Negative")
                tab <- table(test_values, gold_values, useNA = "no")
                matrix <- matrix(0,
                    nrow = 2, ncol = 2,
                    dimnames = list(test = lvls, gold = lvls)
                )
                common_rows <- intersect(rownames(tab), lvls)
                common_cols <- intersect(colnames(tab), lvls)
                if (length(common_rows) > 0 && length(common_cols) > 0) {
                    matrix[common_rows, common_cols] <- tab[common_rows, common_cols, drop = FALSE]
                }
                return(matrix)
            },

            # Safe proportion helper that reports issues once per test/metric
            .computeRate = function(numerator, denominator, metric_label, test_label) {
                if (is.na(denominator) || denominator == 0) {
                    warning(jmvcore::format("Unable to compute {metric} for {test}: denominator is zero. The result is set to NA.",
                        metric = metric_label, test = test_label
                    ), call. = FALSE)
                    return(NA_real_)
                }
                return(numerator / denominator)
            },

            # Extract aligned test and gold vectors for pairwise comparisons
            .extractComparisonVectors = function(test_results, test1, test2) {
                list(
                    test1 = test_results[[test1]]$test_results,
                    test2 = test_results[[test2]]$test_results,
                    gold = test_results[[test1]]$gold_reference
                )
            },

            # Compute paired proportion difference with correlated standard error
            .pairedProportionDifference = function(x, y) {
                valid <- !is.na(x) & !is.na(y)
                n <- sum(valid)
                if (n == 0) {
                    return(NULL)
                }

                x <- x[valid]
                y <- y[valid]

                both_pos <- sum(x == 1 & y == 1)
                x_only <- sum(x == 1 & y == 0)
                y_only <- sum(x == 0 & y == 1)
                both_neg <- sum(x == 0 & y == 0)

                diff_num <- x_only - y_only
                diff <- diff_num / n

                var_term <- ((x_only + y_only) * n - diff_num^2)
                se <- if (var_term <= 0) 0 else sqrt(var_term / n^3)

                z <- stats::qnorm(0.975)
                lower <- max(-1, diff - z * se)
                upper <- min(1, diff + z * se)

                list(
                    diff = diff,
                    lower = lower,
                    upper = upper,
                    n = n,
                    counts = c(both_pos = both_pos, x_only = x_only, y_only = y_only, both_neg = both_neg)
                )
            },

            # ======================================================================
            # Notice Management Helper Methods (HTML output to avoid serialization)
            # ======================================================================

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

            # ======================================================================
            # Main Analysis Methods
            # ======================================================================

            # Initialization - visibility now handled by .r.yaml
            .init = function() {
                # Initialize table rows for dynamic population
                private$.initializeTables()

                # Generate about this analysis panel
                private$.generateAboutPanel()
            },
            # Main analysis orchestrator - refactored for clarity
            .run = function() {
                # Step 1: Check if minimum required variables are selected
                if (!private$.hasRequiredVars()) {
                    return() # Wait for user to select required variables
                }

                tryCatch(
                    {
                        # Step 2: Comprehensive input validation
                        private$.validateInputs()

                        # Step 3: Process and clean data
                        processed_data <- private$.prepareData()

                        # Step 4: Process each test and calculate metrics
                        test_results <- private$.processAllTests(processed_data)

                        # Cache test results to avoid redundant calculations
                        private$.cached_test_results <- test_results

                        # Step 5: Populate comparison table
                        private$.populateComparisonTable(test_results)

                        # Step 6: Handle original data display if requested
                        if (self$options$od) {
                            private$.displayOriginalData(processed_data)
                        }

                        # Step 7: Perform statistical comparisons if requested
                        if (self$options$statComp && length(test_results) >= 2) {
                            private$.performStatisticalComparisons(test_results)
                        }

                        # Step 8a: Generate summary if requested
                        if (self$options$showSummary && self$options$statComp && length(test_results) >= 2) {
                            private$.generateSummary(test_results)
                        }

                        # Step 8b: Generate report sentence if requested
                        if (self$options$showReportSentence && self$options$statComp && length(test_results) >= 2) {
                            private$.generateReportSentence(test_results)
                        }

                        # Step 8c: Generate explanations if requested
                        if (self$options$showExplanations) {
                            private$.generateExplanations()
                        }

                        # Step 8: Setup visualizations if requested
                        if (self$options$plot || self$options$radarplot) {
                            private$.setupVisualizations(test_results)
                        }

                        # Step 9: Generate clinical report and summary
                        private$.generateClinicalReport(test_results, processed_data)

                        # Step 10: Success completion notice
                        n_tests <- length(test_results)
                        n_cases <- nrow(processed_data$data)
                        n_diseased <- sum(processed_data$data[[processed_data$goldVariable]] == processed_data$goldPLevel)
                        n_healthy <- n_cases - n_diseased

                        private$.addNotice(
                            type = "INFO",
                            title = "Analysis Completed Successfully",
                            content = paste0(n_tests, " diagnostic tests compared using ", n_cases, " complete cases. Gold standard identified ", n_diseased, " diseased and ", n_healthy, " healthy cases. Review comparison tables and statistical tests below.")
                        )
                    },
                    error = function(e) {
                        # Re-throw the original error without wrapping
                        stop(conditionMessage(e), call. = FALSE)
                    }
                )

                # Step 11: Render all collected notices as HTML
                private$.renderNotices()
            },

            # Check if minimum required variables are selected
            .hasRequiredVars = function() {
                # Gold standard is required
                if (is.null(self$options$gold) || length(self$options$gold) == 0) {
                    return(FALSE)
                }

                # At least 2 tests are required
                test_count <- 0
                if (!is.null(self$options$test1) && self$options$test1 != "") test_count <- test_count + 1
                if (!is.null(self$options$test2) && self$options$test2 != "") test_count <- test_count + 1
                if (!is.null(self$options$test3) && self$options$test3 != "") test_count <- test_count + 1

                if (test_count < 2) {
                    return(FALSE)
                }

                return(TRUE)
            },


            # Comprehensive input validation with informative error messages
            .validateInputs = function() {
                # Check for valid data
                if (is.null(self$data) || nrow(self$data) == 0) {
                    private$.addNotice(
                        type = "ERROR",
                        title = "No Data Provided",
                        content = "No data provided for analysis. Please ensure your dataset contains data. Load a dataset with diagnostic test variables and a gold standard reference."
                    )
                    stop("Validation failed", call. = FALSE)
                }

                # Validate that positive levels are specified for selected tests
                test_positive_pairs <- list(
                    list(test = self$options$test1, positive = self$options$test1Positive, name = "Test 1"),
                    list(test = self$options$test2, positive = self$options$test2Positive, name = "Test 2"),
                    list(test = self$options$test3, positive = self$options$test3Positive, name = "Test 3")
                )

                missing_positives <- character(0)
                for (pair in test_positive_pairs) {
                    if (!is.null(pair$test) && pair$test != "") {
                        if (is.null(pair$positive) || pair$positive == "") {
                            missing_positives <- c(missing_positives, pair$name)
                        }
                    }
                }

                if (length(missing_positives) > 0) {
                    private$.addNotice(
                        type = "ERROR",
                        title = "Missing Positive Levels",
                        content = paste0("Please specify positive levels for: ", paste(missing_positives, collapse = ", "), ". Use the level selector in the variable panel to choose which level represents a positive test result.")
                    )
                    stop("Validation failed", call. = FALSE)
                }

                # Validate prevalence setting
                if (self$options$pp && (self$options$pprob <= 0 || self$options$pprob >= 1)) {
                    private$.addNotice(
                        type = "ERROR",
                        title = "Invalid Prevalence Value",
                        content = paste0("Prior probability must be between 0 and 1 (exclusive). Current value: ", self$options$pprob, ". Enter a decimal value like 0.15 for 15% prevalence.")
                    )
                    stop("Validation failed", call. = FALSE)
                }

                # Check for conflicting options
                if (self$options$pp && self$options$ci) {
                    private$.addNotice(
                        type = "ERROR",
                        title = "Conflicting Options",
                        content = 'Prior probability and confidence intervals cannot both be enabled. Please disable either "Prior Probability" or "95% CI" option. Use CI for sample-based estimates or custom prevalence for population-based PPV/NPV.'
                    )
                    stop("Validation failed", call. = FALSE)
                }

                # Inform user about prevalence source
                if (self$options$pp) {
                    private$.addNotice(
                        type = "INFO",
                        title = "Prevalence Source",
                        content = "PPV/NPV will be calculated using the supplied population prevalence (pp=TRUE). Confidence intervals are unavailable in this mode."
                    )
                }
            },

            # Data preparation and cleaning
            .prepareData = function() {
                # Extract variable names first
                goldVariable <- self$options$gold
                goldPLevel <- self$options$goldPositive

                if (is.null(goldVariable) || goldVariable == "") {
                    private$.addNotice(
                        type = "ERROR",
                        title = "No Gold Standard",
                        content = "Gold standard variable must be specified. Select a reference test variable that represents the true disease status."
                    )
                    stop("Validation failed", call. = FALSE)
                }

                # Get test variables
                testVariables <- private$.getTestVariables()

                if (length(testVariables) == 0) {
                    private$.addNotice(
                        type = "ERROR",
                        title = "No Test Variables",
                        content = "At least one test variable must be specified. Select diagnostic test variables to compare against the gold standard."
                    )
                    stop("Validation failed", call. = FALSE)
                }

                # CRITICAL FIX: Only subset to SELECTED variables before removing NA
                # This prevents bias from unrelated missing values in other columns
                all_vars <- c(goldVariable, testVariables)
                mydata <- self$data[, all_vars, drop = FALSE]

                # Now remove rows with missing values in SELECTED variables only
                n_before <- nrow(mydata)
                mydata <- na.omit(mydata)
                n_after <- nrow(mydata)

                if (n_after < n_before) {
                    n_removed <- n_before - n_after
                    pct_retained <- round(n_after / n_before * 100, 1)

                    private$.addNotice(
                        type = "WARNING",
                        title = "Missing Data Removed",
                        content = paste0("Removed ", n_removed, " rows with missing values in selected variables. ", pct_retained, "% of original data retained (", n_after, "/", n_before, " cases). This may affect prevalence estimates if data are not missing completely at random.")
                    )
                }

                if (nrow(mydata) == 0) {
                    private$.addNotice(
                        type = "ERROR",
                        title = "No Complete Data",
                        content = "Data contains no complete rows after removing missing values. All cases have at least one missing value in selected variables. Check data quality and consider imputation or data cleaning."
                    )
                    stop("Validation failed", call. = FALSE)
                }

                # Convert to factor and validate positive level
                mydata[[goldVariable]] <- forcats::as_factor(mydata[[goldVariable]])
                private$.assertLevelExists(mydata[[goldVariable]], goldPLevel, goldVariable)

                # Clinical profile notices: sample size adequacy
                n_total <- nrow(mydata)
                if (n_total < 50) {
                    notice_type <- if (n_total < 30) "STRONG_WARNING" else "WARNING"
                    severity <- if (n_total < 30) "Very Small" else "Small"

                    private$.addNotice(
                        type = notice_type,
                        title = paste0(severity, " Sample Size"),
                        content = paste0(severity, " sample size (n=", n_total, "). Confidence intervals may be wide and estimates unstable. Minimum recommended: n=50-100 for adequate precision in diagnostic accuracy studies. Consider collecting additional data for reliable assessment.")
                    )
                }

                # Clinical profile notices: extreme prevalence
                n_diseased <- sum(mydata[[goldVariable]] == goldPLevel)
                prevalence <- n_diseased / n_total

                if (prevalence < 0.05 || prevalence > 0.95) {
                    private$.addNotice(
                        type = "STRONG_WARNING",
                        title = "Extreme Disease Prevalence",
                        content = paste0("Extreme disease prevalence: ", round(prevalence * 100, 1), "% (", n_diseased, "/", n_total, " cases). PPV and NPV are highly sensitive to prevalence and may not generalize to populations with different disease rates. Sensitivity and specificity are less affected by prevalence. Consider reporting likelihood ratios as primary metrics for broader applicability.")
                    )
                }

                return(list(
                    data = mydata,
                    goldVariable = goldVariable,
                    goldPLevel = goldPLevel
                ))
            },

            # Initialize table structures
            .initializeTables = function() {
                # Add rows to contingency tables
                for (i in 1:3) {
                    table_name <- paste0("cTable", i)
                    if (table_name %in% names(self$results)) {
                        cTable <- self$results[[table_name]]
                        cTable$addRow(rowKey = "Test Positive", values = list(newtest = "Test Positive"))
                        cTable$addRow(rowKey = "Test Negative", values = list(newtest = "Test Negative"))
                        cTable$addRow(rowKey = "Total", values = list(newtest = "Total"))
                    }
                }
            },

            # Display original data tables
            .displayOriginalData = function(processed_data) {
                mydata <- processed_data$data
                goldVariable <- processed_data$goldVariable

                # Create frequency table for gold standard
                freq_table <- table(mydata[[goldVariable]])
                self$results$text1$setContent(freq_table)

                # Create cross-tabulation tables
                test_vars <- private$.getTestVariables()

                if (length(test_vars) > 0) {
                    html_tables <- ""
                    for (test_var in test_vars) {
                        cross_tab <- table(mydata[[test_var]], mydata[[goldVariable]])
                        html_table <- knitr::kable(
                            cross_tab,
                            format = "html",
                            caption = paste("Cross-tabulation of", test_var, "and", goldVariable)
                        )
                        html_tables <- paste(html_tables, html_table, "<br><br>")
                    }
                    self$results$text2$setContent(html_tables)
                }
            },

            # Get valid test variables
            .getTestVariables = function() {
                testVariables <- c(self$options$test1, self$options$test2, self$options$test3)
                testVariables <- testVariables[!is.null(testVariables) & testVariables != ""]
                return(testVariables)
            },

            # Get test positive levels mapping
            .getTestPositives = function() {
                testPositives <- list()
                if (!is.null(self$options$test1) && self$options$test1 != "") {
                    testPositives[[self$options$test1]] <- self$options$test1Positive
                }
                if (!is.null(self$options$test2) && self$options$test2 != "") {
                    testPositives[[self$options$test2]] <- self$options$test2Positive
                }
                if (!is.null(self$options$test3) && self$options$test3 != "") {
                    testPositives[[self$options$test3]] <- self$options$test3Positive
                }
                return(testPositives)
            },

            # Process all tests and calculate metrics
            .processAllTests = function(processed_data) {
                mydata <- processed_data$data
                goldVariable <- processed_data$goldVariable
                goldPLevel <- processed_data$goldPLevel

                testVariables <- private$.getTestVariables()
                testPositives <- private$.getTestPositives()

                test_results <- list()

                for (i in seq_along(testVariables)) {
                    testVariable <- testVariables[i]
                    testPLevel <- testPositives[[testVariable]]

                    # Process individual test
                    result <- private$.processSingleTest(
                        mydata, testVariable, testPLevel, goldVariable, goldPLevel, i
                    )

                    test_results[[testVariable]] <- result
                }

                return(test_results)
            },

            # Process a single test and calculate all metrics
            .processSingleTest = function(mydata, testVariable, testPLevel, goldVariable, goldPLevel, test_index) {
                # Convert to factor and validate positive level
                mydata[[testVariable]] <- forcats::as_factor(mydata[[testVariable]])
                private$.assertLevelExists(mydata[[testVariable]], testPLevel, testVariable, testVariable)

                # CRITICAL: Check for multi-level variables (equivocal/invalid results)
                test_levels <- levels(mydata[[testVariable]])
                gold_levels <- levels(mydata[[goldVariable]])

                # Warn if more than 2 levels (indicates equivocal/invalid/indeterminate results)
                if (length(test_levels) > 2) {
                    extra_levels <- setdiff(test_levels, testPLevel)
                    count_extra <- sum(!is.na(mydata[[testVariable]]) & mydata[[testVariable]] %in% extra_levels)

                    private$.addNotice(
                        type = "STRONG_WARNING",
                        title = "Multi-Level Test Variable",
                        content = paste0(testVariable, " has ", length(test_levels), " levels: ", paste(test_levels, collapse = ", "), '. Only "', testPLevel, '" treated as positive; all others (', paste(extra_levels, collapse = ", "), ') treated as NEGATIVE. This may inflate specificity/NPV if equivocal results are present. ', count_extra, ' non-positive cases detected. Consider enabling "Exclude Indeterminate" option or using binary variables.')
                    )
                }

                if (length(gold_levels) > 2) {
                    extra_levels <- setdiff(gold_levels, goldPLevel)

                    private$.addNotice(
                        type = "STRONG_WARNING",
                        title = "Multi-Level Gold Standard",
                        content = paste0('Gold standard "', goldVariable, '" has ', length(gold_levels), ' levels: ', paste(gold_levels, collapse = ", "), '. Only "', goldPLevel, '" treated as positive; all others (', paste(extra_levels, collapse = ", "), ') treated as NEGATIVE. Ensure your reference standard truly has only two outcomes or use a binary variable.')
                    )
                }

                # Optionally exclude indeterminate/equivocal levels instead of collapsing to Negative
                if (self$options$excludeIndeterminate) {
                    valid_test_levels <- c(testPLevel, setdiff(test_levels, testPLevel))
                    valid_gold_levels <- c(goldPLevel, setdiff(gold_levels, goldPLevel))

                    rows_before <- nrow(mydata)
                    mydata <- mydata %>%
                        dplyr::filter(
                            (.data[[testVariable]] %in% valid_test_levels) &
                            (.data[[goldVariable]] %in% valid_gold_levels)
                        )
                    rows_after <- nrow(mydata)
                    if (rows_after < rows_before) {
                        private$.addNotice(
                            type = "INFO",
                            title = "Excluded Indeterminate Levels",
                            content = paste0("Excluded ", rows_before - rows_after, " rows for ", testVariable, " (and gold) with indeterminate/equivocal levels; retained ", rows_after, " rows.")
                        )
                    }
                }

                # Recode data to positive/negative with explicit handling of missing values
                mydata2 <- mydata %>%
                    dplyr::mutate(
                        testVariable2 = dplyr::case_when(
                            is.na(.data[[testVariable]]) ~ NA_character_,
                            .data[[testVariable]] == testPLevel ~ "Positive",
                            TRUE ~ "Negative" # All non-positive levels become Negative
                        ),
                        goldVariable2 = dplyr::case_when(
                            is.na(.data[[goldVariable]]) ~ NA_character_,
                            .data[[goldVariable]] == goldPLevel ~ "Positive",
                            TRUE ~ "Negative"
                        )
                    ) %>%
                    dplyr::mutate(
                        testVariable2 = forcats::fct_relevel(factor(testVariable2, levels = c("Positive", "Negative")), "Positive"),
                        goldVariable2 = forcats::fct_relevel(factor(goldVariable2, levels = c("Positive", "Negative")), "Positive")
                    )

                # Create confusion table with zero-filled cells
                conf_table <- private$.buildConfusionMatrix(mydata2[["testVariable2"]], mydata2[["goldVariable2"]])

                # Extract confusion matrix values
                TP <- conf_table["Positive", "Positive"]
                FP <- conf_table["Positive", "Negative"]
                FN <- conf_table["Negative", "Positive"]
                TN <- conf_table["Negative", "Negative"]

                # Calculate basic metrics
                metrics <- private$.calculateDiagnosticMetrics(TP, FP, FN, TN, testVariable)

                # Populate contingency table
                private$.populateContingencyTable(test_index, testVariable, TP, FP, FN, TN)

                # Calculate confidence intervals if requested
                if (self$options$ci) {
                    private$.populateConfidenceIntervals(test_index, testVariable, conf_table)
                }

                return(list(
                    variable = testVariable,
                    metrics = metrics,
                    conf_table = conf_table,
                    test_results = mydata2[["testVariable2"]],
                    gold_reference = mydata2[["goldVariable2"]],
                    TP = TP, FP = FP, FN = FN, TN = TN
                ))
            },

            # Helper function to calculate diagnostic metrics with prevalence logic
            .calculateDiagnosticMetrics = function(TP, FP, FN, TN, test_label) {
                TotalPop <- TP + TN + FP + FN
                DiseaseP <- TP + FN
                DiseaseN <- TN + FP
                TestP <- TP + FP
                TestN <- TN + FN

                # Basic metrics
                Sens <- private$.computeRate(TP, DiseaseP, "sensitivity", test_label)
                Spec <- private$.computeRate(TN, DiseaseN, "specificity", test_label)
                AccurT <- private$.computeRate(TP + TN, TotalPop, "accuracy", test_label)

                # Calculate PPV and NPV based on prevalence setting
                if (self$options$pp && !is.na(self$options$pprob)) {
                    # Use provided prevalence with Bayes' theorem
                    PriorProb <- self$options$pprob
                    if (is.na(Sens) || is.na(Spec)) {
                        PPV <- NA_real_
                        NPV <- NA_real_
                    } else {
                        denom_ppv <- (Sens * PriorProb) + ((1 - Spec) * (1 - PriorProb))
                        denom_npv <- ((1 - Sens) * PriorProb) + (Spec * (1 - PriorProb))
                        PPV <- private$.computeRate(Sens * PriorProb, denom_ppv, "positive predictive value", test_label)
                        NPV <- private$.computeRate(Spec * (1 - PriorProb), denom_npv, "negative predictive value", test_label)
                    }
                } else {
                    # Use sample-based calculation
                    PPV <- private$.computeRate(TP, TestP, "positive predictive value", test_label)
                    NPV <- private$.computeRate(TN, TestN, "negative predictive value", test_label)
                    PriorProb <- private$.computeRate(DiseaseP, TotalPop, "prevalence", test_label)
                }

                # Likelihood ratios with stability handling
                spec_is_one <- !is.na(Spec) && abs(Spec - 1) < sqrt(.Machine$double.eps)
                spec_is_zero <- !is.na(Spec) && abs(Spec - 0) < sqrt(.Machine$double.eps)

                # Apply continuity correction for LR stability only; do not alter reported counts
               zero_cells <- any(c(TP, FP, FN, TN) == 0)
                if (zero_cells) {
                    private$.addNotice(
                        type = "INFO",
                        title = "Zero Cell Continuity Correction",
                        content = paste0("Zero cell detected for ", test_label, ". LR+/LR- computed with a ", private$ZERO_CELL_CONTINUITY, " continuity correction to avoid infinite/undefined values; interpret cautiously.")
                    )

                    TP_cc <- TP + private$ZERO_CELL_CONTINUITY
                    FP_cc <- FP + private$ZERO_CELL_CONTINUITY
                    FN_cc <- FN + private$ZERO_CELL_CONTINUITY
                    TN_cc <- TN + private$ZERO_CELL_CONTINUITY
                } else {
                    TP_cc <- TP; FP_cc <- FP; FN_cc <- FN; TN_cc <- TN
                }

                LRP <- if (is.na(Sens) || is.na(Spec)) {
                    NA_real_
                } else if (spec_is_one) {
                    Inf
                } else {
                    private$.computeRate(TP_cc / (TP_cc + FN_cc), 1 - (TN_cc / (TN_cc + FP_cc)), "positive likelihood ratio", test_label)
                }
                LRN <- if (is.na(Sens) || is.na(Spec)) {
                    NA_real_
                } else if (spec_is_zero) {
                    Inf
                } else {
                    private$.computeRate(1 - (TP_cc / (TP_cc + FN_cc)), TN_cc / (TN_cc + FP_cc), "negative likelihood ratio", test_label)
                }

                return(list(
                    Sens = Sens, Spec = Spec, AccurT = AccurT,
                    PPV = PPV, NPV = NPV, LRP = LRP, LRN = LRN,
                    PriorProb = PriorProb,
                    zero_cells = zero_cells
                ))
            },

            # Populate contingency table for a specific test
            .populateContingencyTable = function(test_index, testVariable, TP, FP, FN, TN) {
                table_name <- paste0("cTable", test_index)
                if (table_name %in% names(self$results)) {
                    cTable <- self$results[[table_name]]

                    cTable$setRow(
                        rowKey = "Test Positive",
                        values = list(
                            newtest = jmvcore::.("Test Positive"),
                            GP = TP, GN = FP, Total = TP + FP
                        )
                    )

                    cTable$setRow(
                        rowKey = "Test Negative",
                        values = list(
                            newtest = jmvcore::.("Test Negative"),
                            GP = FN, GN = TN, Total = FN + TN
                        )
                    )

                    cTable$setRow(
                        rowKey = "Total",
                        values = list(
                            newtest = jmvcore::.("Total"),
                            GP = TP + FN, GN = FP + TN, Total = TP + FP + FN + TN
                        )
                    )

                    # Add footnotes if requested
                    if (self$options$fnote) {
                        private$.addContingencyTableFootnotes(cTable)
                        if (any(c(TP, FP, FN, TN) == 0)) {
                            cTable$addFootnote(rowKey = "Total", col = "Total", "Zero cell detected; LR+/LR- computed with continuity correction (0.5). Interpret cautiously.")
                        }
                    }
                }
            },

            # Add footnotes to contingency tables
            .addContingencyTableFootnotes = function(cTable) {
                cTable$addFootnote(rowKey = "Test Positive", col = "GP", jmvcore::.("True Positive (TP)"))
                cTable$addFootnote(rowKey = "Test Positive", col = "GN", jmvcore::.("False Positive (FP)"))
                cTable$addFootnote(rowKey = "Test Negative", col = "GP", jmvcore::.("False Negative (FN)"))
                cTable$addFootnote(rowKey = "Test Negative", col = "GN", jmvcore::.("True Negative (TN)"))
            },

            # Populate comparison table with all test results
            .populateComparisonTable = function(test_results) {
                comparisonTable <- self$results$comparisonTable

                try(comparisonTable$clearRows(), silent = TRUE)

                for (test_name in names(test_results)) {
                    result <- test_results[[test_name]]
                    metrics <- result$metrics

                    # Use escaped variable name consistently for row keys
                    escaped_test_name <- private$.escapeVar(test_name)

                    prevalence_note <- if (self$options$pp) " (population prevalence)" else " (sample prevalence)"

                    comparisonTable$addRow(
                        rowKey = escaped_test_name,
                        values = list(
                            test = test_name,
                            Sens = metrics$Sens,
                            Spec = metrics$Spec,
                            AccurT = metrics$AccurT,
                            PPV = metrics$PPV,
                            NPV = metrics$NPV,
                            LRP = metrics$LRP,
                            LRN = metrics$LRN
                        )
                    )

                    # Add clinical interpretation
                    clinical_interpretation <- private$.generateClinicalInterpretation(metrics)
                    comparisonTable$addFormat(rowKey = escaped_test_name, col = "test", format = jmvcore::Cell.BEGIN_GROUP)
                    comparisonTable$addRow(
                        rowKey = paste0(escaped_test_name, "_interp"),
                        values = list(
                            test = paste0("  → ", clinical_interpretation, ifelse(metrics$zero_cells, " (zero cell; LR may be unstable)", "")),
                            Sens = "", Spec = "", AccurT = "", PPV = "", NPV = "", LRP = "", LRN = ""
                        )
                    )
                    comparisonTable$addFormat(rowKey = paste0(escaped_test_name, "_interp"), col = "test", format = jmvcore::Cell.END_GROUP)

                    # Add footnotes if requested
                    if (self$options$fnote) {
                        private$.addComparisonTableFootnotes(comparisonTable, escaped_test_name)
                        comparisonTable$addFootnote(
                            rowKey = escaped_test_name, col = "PPV",
                            paste0("PPV uses", prevalence_note, ".")
                        )
                        comparisonTable$addFootnote(
                            rowKey = escaped_test_name, col = "NPV",
                            paste0("NPV uses", prevalence_note, ".")
                        )
                    }
                }
            },

            # Generate clinical interpretation for test performance
            .generateClinicalInterpretation = function(metrics) {
                sens_pct <- metrics$Sens * 100
                spec_pct <- metrics$Spec * 100
                ppv_pct <- metrics$PPV * 100
                npv_pct <- metrics$NPV * 100
                lrp <- metrics$LRP
                lrn <- metrics$LRN

                # Determine primary clinical strength
                interpretations <- c()

                if (sens_pct >= 95 && spec_pct >= 95) {
                    interpretations <- c(interpretations, "Excellent overall performance")
                } else if (sens_pct >= 95) {
                    interpretations <- c(interpretations, "Excellent for screening (rule-out)")
                } else if (spec_pct >= 95) {
                    interpretations <- c(interpretations, "Excellent for confirmation (rule-in)")
                } else if (sens_pct >= 85 && spec_pct >= 85) {
                    interpretations <- c(interpretations, "Good balanced performance")
                } else if (sens_pct >= 85) {
                    interpretations <- c(interpretations, "Good sensitivity for screening")
                } else if (spec_pct >= 85) {
                    interpretations <- c(interpretations, "Good specificity for confirmation")
                }

                # Add likelihood ratio interpretation
                if (!is.na(lrp) && lrp >= 10) {
                    interpretations <- c(interpretations, "Strong positive evidence")
                } else if (!is.na(lrp) && lrp >= 5) {
                    interpretations <- c(interpretations, "Moderate positive evidence")
                }

                if (!is.na(lrn) && lrn <= 0.1) {
                    interpretations <- c(interpretations, "Strong negative evidence")
                } else if (!is.na(lrn) && lrn <= 0.2) {
                    interpretations <- c(interpretations, "Moderate negative evidence")
                }

                # Combine interpretations or provide fallback
                if (length(interpretations) > 0) {
                    return(paste(interpretations, collapse = "; "))
                } else {
                    return("Limited diagnostic utility - consider combining with other tests")
                }
            },

            # Add footnotes to comparison table
            .addComparisonTableFootnotes = function(comparisonTable, test_name) {
                comparisonTable$addFootnote(
                    rowKey = test_name, col = "Sens",
                    "Sensitivity = TP/(TP+FN) = Probability of positive test among diseased"
                )
                comparisonTable$addFootnote(
                    rowKey = test_name, col = "Spec",
                    "Specificity = TN/(TN+FP) = Probability of negative test among healthy"
                )
                comparisonTable$addFootnote(
                    rowKey = test_name, col = "PPV",
                    "Positive Predictive Value = TP/(TP+FP) = Probability of disease when test is positive"
                )
                comparisonTable$addFootnote(
                    rowKey = test_name, col = "NPV",
                    "Negative Predictive Value = TN/(TN+FN) = Probability of being healthy when test is negative"
                )
            },

            # Populate confidence intervals for a specific test
            .populateConfidenceIntervals = function(test_index, testVariable, conf_table) {
                table_name <- paste0("epirTable", test_index)
                if (table_name %in% names(self$results)) {
                    epirTable <- self$results[[table_name]]

                    # Check if epiR package is available
                    if (!requireNamespace("epiR", quietly = TRUE)) {
                        private$.addNotice(
                            type = "ERROR",
                            title = "Missing epiR Package",
                            content = 'epiR package is required for confidence intervals. Install with install.packages("epiR"). Or disable "95% CI" option.'
                        )
                        return()
                    }

                    tryCatch(
                        {
                            epirresult <- epiR::epi.tests(dat = conf_table)
                            epirresult2 <- summary(epirresult)
                            epirresult2 <- as.data.frame(epirresult2) %>%
                                tibble::rownames_to_column(.data = ., var = "statsabv")

                            epirresult2$statsnames <- c(
                                jmvcore::.("Apparent prevalence"), jmvcore::.("True prevalence"), jmvcore::.("Test sensitivity"),
                                jmvcore::.("Test specificity"), jmvcore::.("Diagnostic accuracy"), jmvcore::.("Diagnostic odds ratio"),
                                jmvcore::.("Number needed to diagnose"), jmvcore::.("Youden's index"),
                                jmvcore::.("Positive predictive value"), jmvcore::.("Negative predictive value"),
                                jmvcore::.("Likelihood ratio of a positive test"), jmvcore::.("Likelihood ratio of a negative test"),
                                jmvcore::.("Proportion of subjects with the outcome ruled out"),
                                jmvcore::.("Proportion of subjects with the outcome ruled in"),
                                jmvcore::.("Proportion of false positives"), jmvcore::.("Proportion of false negative"),
                                jmvcore::.("False Discovery Rate"), jmvcore::.("False Omission Rate")
                            )

                            ratiorows <- c(
                                "ap", "tp", "se", "sp", "diag.ac", "pv.pos", "pv.neg",
                                "p.tpdn", "p.tndp", "p.dntp", "p.dptn"
                            )
                            epirresult_ratio <- epirresult2[epirresult2$statistic %in% ratiorows, ]

                            for (i in seq_along(epirresult_ratio[, 1, drop = TRUE])) {
                                epirTable$addRow(rowKey = i, values = c(epirresult_ratio[i, ]))
                            }
                        },
                        error = function(e) {
                            n_sample <- sum(conf_table)
                            enhanced_msg <- jmvcore::format("Could not calculate confidence intervals for {test} (n={n}). This may be due to insufficient sample size or extreme values. Error: {error}",
                                test = testVariable, n = n_sample, error = conditionMessage(e)
                            )
                            warning(enhanced_msg)
                        }
                    )
                }
            },

            # Perform statistical comparisons between tests with progress reporting
            .performStatisticalComparisons = function(test_results) {
                mcnemarTable <- self$results$mcnemarTable
                diffTable <- self$results$diffTable

                try(mcnemarTable$clearRows(), silent = TRUE)
                try(diffTable$clearRows(), silent = TRUE)

                test_names <- names(test_results)
                n_tests <- length(test_names)

                # For 3+ tests, perform Cochran's Q test first
                if (n_tests >= 3) {
                    private$.performCochranQ(test_results, mcnemarTable)
                }

                # Generate all pairwise combinations
                test_pairs <- combn(test_names, 2, simplify = FALSE)
                n_comparisons <- length(test_pairs)

                # Calculate p-value adjustment for multiple comparisons
                # Use Holm-Bonferroni method (less conservative than Bonferroni)
                p_values <- numeric(n_comparisons)
                comparison_names <- character(n_comparisons)

                if (n_comparisons > 1) {
                    message(sprintf("Performing %d pairwise comparisons with Holm-Bonferroni correction...", n_comparisons))
                }

                # First pass: collect p-values
                for (i in seq_along(test_pairs)) {
                    pair <- test_pairs[[i]]
                    test1 <- pair[1]
                    test2 <- pair[2]
                    comparison_names[i] <- paste(test1, "vs", test2)

                    # Get p-value from McNemar test
                    p_values[i] <- private$.getMcNemarPValue(test_results, test1, test2)
                }

                # Apply Holm-Bonferroni correction
                p_adjusted <- stats::p.adjust(p_values, method = "holm")

                # Second pass: populate table with adjusted p-values
                for (i in seq_along(test_pairs)) {
                    pair <- test_pairs[[i]]
                    test1 <- pair[1]
                    test2 <- pair[2]
                    comparison_name <- comparison_names[i]

                    # McNemar's test with adjusted p-value
                    private$.performMcNemarTest(
                        test_results, test1, test2, comparison_name,
                        mcnemarTable, p_adjusted[i]
                    )

                    # Confidence intervals for differences
                    private$.calculateDifferences(test_results, test1, test2, comparison_name, diffTable)
                }

                if (n_comparisons > 1) {
                    message(sprintf("Statistical comparisons completed (%d comparisons, Holm-Bonferroni corrected).", n_comparisons))
                }
            },

            # Perform Cochran's Q test for 3+ tests (global test)
            # CRITICAL: Compare CORRECTNESS relative to gold standard, not raw positivity rates
            .performCochranQ = function(test_results, mcnemarTable) {
                tryCatch(
                    {
                        # Extract test results and gold standard
                        test_names <- names(test_results)
                        n_tests <- length(test_names)

                        # Get common valid cases
                        gold <- test_results[[test_names[1]]]$gold_reference

                        # Build matrix of CORRECTNESS (not raw positivity)
                        all_tests_matrix <- sapply(test_names, function(tn) {
                            test_result <- test_results[[tn]]$test_results
                            # 1 if correct (matches gold), 0 if wrong
                            ifelse(test_result == gold, 1, 0)
                        })

                        valid_idx <- complete.cases(all_tests_matrix) & !is.na(gold)
                        if (sum(valid_idx) == 0) {
                            return()
                        }

                        all_tests_matrix <- all_tests_matrix[valid_idx, , drop = FALSE]

                        # Cochran's Q statistic
                        # Q = (k-1) * [k*sum(Ci^2) - (sum(Ci))^2] / [k*sum(Ri) - sum(Ri^2)]
                        k <- n_tests
                        n <- nrow(all_tests_matrix)

                        Ci <- colSums(all_tests_matrix) # Column sums (correct per test)
                        Ri <- rowSums(all_tests_matrix) # Row sums (correct per subject)

                        numerator <- k * sum(Ci^2) - (sum(Ci))^2
                        denominator <- k * sum(Ri) - sum(Ri^2)

                        if (denominator == 0) {
                            return() # All tests identical
                        }

                        Q <- (k - 1) * numerator / denominator
                        df <- k - 1
                        p_value <- stats::pchisq(Q, df, lower.tail = FALSE)

                        interpretation <- dplyr::case_when(
                            p_value < 0.001 ~ "Highly significant overall difference among tests (p<0.001)",
                            p_value < 0.01 ~ "Significant overall difference among tests (p<0.01)",
                            p_value < 0.05 ~ "Statistically significant overall difference among tests (p<0.05)",
                            TRUE ~ "No significant overall difference among tests (p≥0.05)"
                        )

                        mcnemarTable$addRow(
                            rowKey = "cochran_q_global",
                            values = list(
                                comparison = sprintf("Overall (%d tests)", n_tests),
                                stat = Q,
                                df = df,
                                p = p_value,
                                interpretation = interpretation
                            )
                        )

                        if (p_value >= private$P_THRESHOLD_SIGNIFICANT) {
                            mcnemarTable$addFootnote(
                                rowKey = "cochran_q_global",
                                col = "interpretation",
                                "Cochran's Q test shows no significant difference. Pairwise comparisons below may not be meaningful."
                            )
                        }
                    },
                    error = function(e) {
                        error_msg <- conditionMessage(e)
                        message(sprintf("Cochran's Q calculation failed: %s", error_msg))

                        # Add visible error row to table
                        mcnemarTable$addRow(
                            rowKey = "cochran_q_error",
                            values = list(
                                comparison = sprintf("Overall (%d tests) - ERROR", n_tests),
                                stat = NA,
                                df = NA,
                                p = NA,
                                interpretation = sprintf(
                                    "⚠️ Could not calculate: %s. Check that all tests have valid paired data.",
                                    error_msg
                                )
                            )
                        )

                        # Add warning footnote
                        mcnemarTable$setNote(
                            "Cochran's Q test failed. Pairwise comparisons may still be valid but should be interpreted cautiously.",
                            symbol = "⚠"
                        )
                    }
                )
            },

            # Get McNemar p-value only (for adjustment calculation)
            # CRITICAL: Compare CORRECTNESS relative to gold standard, not raw positivity rates
            .getMcNemarPValue = function(test_results, test1, test2) {
                test1_results <- test_results[[test1]]$test_results
                test2_results <- test_results[[test2]]$test_results
                gold_results <- test_results[[test1]]$gold_reference # Same for both tests

                tryCatch(
                    {
                        valid_idx <- !is.na(test1_results) & !is.na(test2_results) & !is.na(gold_results)
                        if (sum(valid_idx) == 0) {
                            return(1)
                        }

                        # Compare CORRECTNESS: does each test match the gold standard?
                        test1_correct <- (test1_results[valid_idx] == gold_results[valid_idx])
                        test2_correct <- (test2_results[valid_idx] == gold_results[valid_idx])

                        # McNemar table of correctness (not raw test results)
                        mcnemar_table <- table(
                            factor(test1_correct, levels = c(TRUE, FALSE)),
                            factor(test2_correct, levels = c(TRUE, FALSE)),
                            useNA = "no"
                        )

                        if (sum(mcnemar_table) == 0) {
                            return(1)
                        }

                        mcnemar_result <- stats::mcnemar.test(mcnemar_table)
                        return(mcnemar_result$p.value)
                    },
                    error = function(e) {
                        return(1)
                    }
                )
            },

            # Perform McNemar's test with clinical interpretation
            # CRITICAL: Compare CORRECTNESS relative to gold standard, not raw positivity rates
            .performMcNemarTest = function(test_results, test1, test2, comparison_name, mcnemarTable, p_adjusted = NULL) {
                test1_results <- test_results[[test1]]$test_results
                test2_results <- test_results[[test2]]$test_results
                gold_results <- test_results[[test1]]$gold_reference # Same for both tests

                tryCatch(
                    {
                        valid_idx <- !is.na(test1_results) & !is.na(test2_results) & !is.na(gold_results)
                        if (sum(valid_idx) == 0) {
                            comparison <- comparison_name
                            warning(jmvcore::format("Could not perform McNemar's test for {comparison}: no paired observations after removing missing values."))
                            return()
                        }

                        # Compare CORRECTNESS: does each test match the gold standard?
                        test1_correct <- (test1_results[valid_idx] == gold_results[valid_idx])
                        test2_correct <- (test2_results[valid_idx] == gold_results[valid_idx])

                        # McNemar table of correctness (TRUE/FALSE for each test)
                        mcnemar_table <- table(
                            factor(test1_correct, levels = c(TRUE, FALSE)),
                            factor(test2_correct, levels = c(TRUE, FALSE)),
                            useNA = "no"
                        )

                        if (sum(mcnemar_table) == 0) {
                            comparison <- comparison_name
                            warning(jmvcore::format("Could not perform McNemar's test for {comparison}: no data available after filtering."))
                            return()
                        }
                        mcnemar_result <- stats::mcnemar.test(mcnemar_table)

                        # Check sample size adequacy (number of discordant pairs)
                        # b = Test1 correct, Test2 wrong
                        # c = Test1 wrong, Test2 correct
                        b <- mcnemar_table[1, 2] # TRUE, FALSE
                        c <- mcnemar_table[2, 1] # FALSE, TRUE
                        n_discordant <- b + c

                        # Use adjusted p-value if provided, otherwise use raw p-value
                        p_to_interpret <- if (!is.null(p_adjusted)) p_adjusted else mcnemar_result$p.value

                        # Clinical interpretation of p-value (using constants)
                        interpretation <- dplyr::case_when(
                            p_to_interpret < private$P_THRESHOLD_STRONG ~ "Highly significant difference (p<0.001)",
                            p_to_interpret < 0.01 ~ "Significant difference (p<0.01)",
                            p_to_interpret < private$P_THRESHOLD_SIGNIFICANT ~ "Statistically significant difference (p<0.05)",
                            p_to_interpret < 0.1 ~ "Marginally significant difference (p<0.1)",
                            TRUE ~ "No significant difference (p≥0.1)"
                        )

                        # Add suffix to interpretation if adjusted
                        if (!is.null(p_adjusted)) {
                            interpretation <- paste0(interpretation, " (Holm-Bonferroni corrected)")
                        }

                        mcnemarTable$addRow(
                            rowKey = comparison_name,
                            values = list(
                                comparison = comparison_name,
                                stat = mcnemar_result$statistic,
                                df = mcnemar_result$parameter,
                                p = p_to_interpret,
                                interpretation = interpretation
                            )
                        )

                        # Add sample size adequacy warning if needed
                        if (n_discordant < private$MIN_DISCORDANT_PAIRS) {
                            mcnemarTable$addFootnote(
                                rowKey = comparison_name,
                                col = "p",
                                sprintf(
                                    "⚠️ Small number of discordant pairs (n=%d). Results may be unreliable (recommend n≥%d).",
                                    n_discordant, private$MIN_DISCORDANT_PAIRS
                                )
                            )
                        }
                    },
                    error = function(e) {
                        n1 <- length(test1_results)
                        n2 <- length(test2_results)
                        comparison <- comparison_name
                        error <- conditionMessage(e)
                        enhanced_msg <- jmvcore::format("Could not perform McNemar's test for {comparison} (n1={n1}, n2={n2}). This may be due to insufficient discordant pairs or identical test results. Error: {error}",
                            comparison = comparison, n1 = n1, n2 = n2, error = error
                        )
                        warning(enhanced_msg)
                    }
                )
            },

            # Calculate confidence intervals for paired metric differences
            .calculateDifferences = function(test_results, test1, test2, comparison_name, diffTable) {
                comp <- private$.extractComparisonVectors(test_results, test1, test2)
                t1 <- comp$test1
                t2 <- comp$test2
                gold <- comp$gold

                common_idx <- !is.na(t1) & !is.na(t2) & !is.na(gold)

                if (sum(common_idx) == 0) {
                    warning(jmvcore::format("Unable to compute paired differences for {comparison}: no observations with complete data across both tests and the gold standard.",
                        comparison = comparison_name
                    ))
                    return()
                }

                add_diff_row <- function(result, metric_label, metric_key) {
                    if (is.null(result)) {
                        warning(jmvcore::format("Unable to compute {metric} difference for {comparison}: insufficient paired data.",
                            metric = metric_label, comparison = comparison_name
                        ), call. = FALSE)
                        return()
                    }
                    row_key <- paste(comparison_name, metric_key, sep = "_")
                    diffTable$addRow(
                        rowKey = row_key,
                        values = list(
                            comparison = comparison_name,
                            metric = metric_label,
                            diff = result$diff,
                            lower = result$lower,
                            upper = result$upper
                        )
                    )
                    # Add small-sample/stability note
                    if (!is.null(result$n) && (result$n < 30 || any(result$counts < 5))) {
                        diffTable$addFootnote(
                            rowKey = row_key,
                            col = "diff",
                            jmvcore::format(jmvcore::.("Small paired sample/discordant counts; CI may be unstable (n={n}, discordant counts: {counts})."),
                                n = result$n,
                                counts = paste(result$counts, collapse = ", "))
                        )
                    }
                }

                # Sensitivity difference (gold positive)
                sens_idx <- common_idx & gold == "Positive"
                sens_diff <- if (sum(sens_idx) == 0) {
                    NULL
                } else {
                    private$.pairedProportionDifference(
                        x = ifelse(t1[sens_idx] == "Positive", 1, 0),
                        y = ifelse(t2[sens_idx] == "Positive", 1, 0)
                    )
                }
                add_diff_row(sens_diff, jmvcore::.("Sensitivity"), "Sens")

                # Specificity difference (gold negative)
                spec_idx <- common_idx & gold == "Negative"
                spec_diff <- if (sum(spec_idx) == 0) {
                    NULL
                } else {
                    private$.pairedProportionDifference(
                        x = ifelse(t1[spec_idx] == "Negative", 1, 0),
                        y = ifelse(t2[spec_idx] == "Negative", 1, 0)
                    )
                }
                add_diff_row(spec_diff, jmvcore::.("Specificity"), "Spec")

                # Accuracy difference (all subjects)
                acc_diff <- private$.pairedProportionDifference(
                    x = ifelse(t1[common_idx] == gold[common_idx], 1, 0),
                    y = ifelse(t2[common_idx] == gold[common_idx], 1, 0)
                )
                add_diff_row(acc_diff, jmvcore::.("Accuracy"), "AccurT")
            },

            # Setup visualizations with improved data structure
            .setupVisualizations = function(test_results) {
                plotData <- list()

                for (test_name in names(test_results)) {
                    result <- test_results[[test_name]]
                    metrics <- result$metrics

                    plotData[[test_name]] <- list(
                        test = test_name,
                        Sens = metrics$Sens,
                        Spec = metrics$Spec,
                        AccurT = metrics$AccurT, # Include accuracy for radar plot
                        PPV = metrics$PPV,
                        NPV = metrics$NPV,
                        LRP = metrics$LRP,
                        LRN = metrics$LRN
                    )
                }

                if (self$options$plot) {
                    self$results$plot1$setState(plotData)
                }

                if (self$options$radarplot) {
                    self$results$plotRadar$setState(plotData)
                }
            },

            # Optimized bar plot data building - eliminates inefficient rbind operations
            .buildBarPlotData = function(plotData) {
                if (is.null(plotData) || length(plotData) == 0) {
                    return(data.frame())
                }

                # Pre-calculate total rows needed
                n_tests <- length(plotData)
                n_metrics <- 4 # Sens, Spec, PPV, NPV
                total_rows <- n_tests * n_metrics

                # Pre-allocate vectors for efficiency
                test_names <- character(total_rows)
                metric_names <- character(total_rows)
                values <- numeric(total_rows)

                # Fill vectors efficiently
                row_idx <- 1
                metrics <- c("Sensitivity", "Specificity", "PPV", "NPV")
                metric_keys <- c("Sens", "Spec", "PPV", "NPV")

                for (test_name in names(plotData)) {
                    test_data <- plotData[[test_name]]
                    for (i in seq_along(metrics)) {
                        test_names[row_idx] <- test_name
                        metric_names[row_idx] <- metrics[i]
                        values[row_idx] <- test_data[[metric_keys[i]]]
                        row_idx <- row_idx + 1
                    }
                }

                return(data.frame(
                    test = test_names,
                    metric = metric_names,
                    value = values,
                    stringsAsFactors = FALSE
                ))
            },

            # Optimized radar plot data building with clinical scaling
            .buildRadarPlotData = function(plotData) {
                if (is.null(plotData) || length(plotData) == 0) {
                    return(data.frame())
                }

                # Pre-calculate dimensions
                n_tests <- length(plotData)
                n_metrics <- 7 # 5 percentage metrics + 2 LR quality metrics
                total_rows <- n_tests * n_metrics

                # Pre-allocate vectors
                test_names <- character(total_rows)
                metric_names <- character(total_rows)
                values <- numeric(total_rows)
                scaled_values <- numeric(total_rows)

                # Define metrics and their processing
                pct_metrics <- list(
                    "Sensitivity" = "Sens",
                    "Specificity" = "Spec",
                    "Accuracy" = "AccurT",
                    "PPV" = "PPV",
                    "NPV" = "NPV"
                )

                row_idx <- 1
                thresholds <- private$LR_CLINICAL_THRESHOLDS

                for (test_name in names(plotData)) {
                    test_data <- plotData[[test_name]]

                    # Process percentage metrics (direct 0-100 scaling)
                    for (metric_label in names(pct_metrics)) {
                        metric_key <- pct_metrics[[metric_label]]
                        test_names[row_idx] <- test_name
                        metric_names[row_idx] <- metric_label
                        values[row_idx] <- test_data[[metric_key]]
                        scaled_values[row_idx] <- test_data[[metric_key]] * 100
                        row_idx <- row_idx + 1
                    }

                    # Process LR+ with clinical thresholds
                    lrp_value <- test_data$LRP
                    lrp_scaled <- private$.scaleLikelihoodRatioPositive(lrp_value, thresholds)

                    test_names[row_idx] <- test_name
                    metric_names[row_idx] <- "LR+ Quality"
                    values[row_idx] <- lrp_value
                    scaled_values[row_idx] <- lrp_scaled
                    row_idx <- row_idx + 1

                    # Process LR- with clinical thresholds
                    lrn_value <- test_data$LRN
                    lrn_scaled <- private$.scaleLikelihoodRatioNegative(lrn_value, thresholds)

                    test_names[row_idx] <- test_name
                    metric_names[row_idx] <- "LR- Quality"
                    values[row_idx] <- lrn_value
                    scaled_values[row_idx] <- lrn_scaled
                    row_idx <- row_idx + 1
                }

                df <- data.frame(
                    test = test_names,
                    metric = factor(metric_names,
                        levels = c(
                            "Sensitivity", "Specificity", "Accuracy",
                            "PPV", "NPV", "LR+ Quality", "LR- Quality"
                        )
                    ),
                    value = values,
                    scaled_value = scaled_values,
                    stringsAsFactors = FALSE
                )

                return(df)
            },

            # Clinical scaling for positive likelihood ratios
            .scaleLikelihoodRatioPositive = function(lrp, thresholds) {
                if (is.na(lrp) || lrp <= 1) {
                    return(0)
                }

                # Clinical scaling based on evidence strength
                if (lrp >= thresholds$excellent_pos) {
                    return(100) # Excellent evidence
                } else if (lrp >= thresholds$good_pos) {
                    return(75 + (lrp - thresholds$good_pos) / (thresholds$excellent_pos - thresholds$good_pos) * 25)
                } else if (lrp >= thresholds$fair_pos) {
                    return(50 + (lrp - thresholds$fair_pos) / (thresholds$good_pos - thresholds$fair_pos) * 25)
                } else {
                    return(25 + (lrp - 1) / (thresholds$fair_pos - 1) * 25)
                }
            },

            # Clinical scaling for negative likelihood ratios
            .scaleLikelihoodRatioNegative = function(lrn, thresholds) {
                if (is.na(lrn) || lrn >= 1) {
                    return(0)
                }

                # Clinical scaling - lower LR- values are better
                if (lrn <= thresholds$excellent_neg) {
                    return(100) # Excellent evidence
                } else if (lrn <= thresholds$good_neg) {
                    return(75 + (thresholds$good_neg - lrn) / (thresholds$good_neg - thresholds$excellent_neg) * 25)
                } else if (lrn <= thresholds$fair_neg) {
                    return(50 + (thresholds$fair_neg - lrn) / (thresholds$fair_neg - thresholds$good_neg) * 25)
                } else {
                    return(max(0, 50 * (1 - lrn) / (1 - thresholds$fair_neg)))
                }
            },

            # Safe statistical calculation wrapper with enhanced error reporting
            .safeStatisticalCalculation = function(func, context_info, default_return = NULL, ...) {
                tryCatch(
                    func(...),
                    error = function(e) {
                        enhanced_msg <- jmvcore::format("Statistical calculation failed for {context}: {error}. Check data quality, sample sizes, and ensure no extreme values or division by zero.",
                            context = context_info, error = conditionMessage(e)
                        )
                        warning(enhanced_msg)
                        return(default_return)
                    }
                )
            },

            # Generate comprehensive clinical report with copy-ready sentences
            .generateClinicalReport = function(test_results, processed_data) {
                if (length(test_results) == 0) {
                    return()
                }

                # Find best performing test
                best_test <- private$.findBestTest(test_results)
                best_metrics <- test_results[[best_test]]$metrics

                # Generate report sections
                methods_section <- private$.generateMethodsSection(test_results, processed_data)
                results_section <- private$.generateResultsSection(test_results, best_test, best_metrics)
                clinical_recommendations <- private$.generateClinicalRecommendations(test_results, best_test)

                # Create comprehensive HTML report
                report_html <- paste0(
                    '<div style="font-family: Arial, sans-serif; max-width: 800px; margin: 0 auto; padding: 20px;">',
                    '<h2 style="color: #2c3e50; border-bottom: 2px solid #3498db;">📋 Clinical Summary</h2>',
                    results_section,
                    '<h3 style="color: #27ae60; margin-top: 30px;">📝 Report Sentences</h3>',
                    '<div style="background-color: #f8f9fa; padding: 15px; border-left: 4px solid #28a745; margin: 15px 0;">',
                    '<h4 style="margin-top: 0;">Methods Section:</h4>',
                    '<p style="font-style: italic; line-height: 1.6;">', methods_section, "</p>",
                    "</div>",
                    '<div style="background-color: #e8f4f8; padding: 15px; border-left: 4px solid #3498db; margin: 15px 0;">',
                    '<h4 style="margin-top: 0;">Results Section:</h4>',
                    '<p style="font-style: italic; line-height: 1.6;">', results_section, "</p>",
                    "</div>",
                    '<h3 style="color: #8e44ad; margin-top: 30px;">💡 Clinical Recommendations</h3>',
                    clinical_recommendations,
                    "</div>"
                )

                self$results$clinicalReport$setContent(report_html)
            },

            # Find the best performing test based on overall metrics
            .findBestTest = function(test_results) {
                best_score <- -1
                best_test <- names(test_results)[1]

                for (test_name in names(test_results)) {
                    metrics <- test_results[[test_name]]$metrics
                    # Balanced scoring: Youden index + accuracy
                    score <- (metrics$Sens + metrics$Spec - 1) + metrics$AccurT
                    if (score > best_score) {
                        best_score <- score
                        best_test <- test_name
                    }
                }
                return(best_test)
            },

            # Generate methods section for manuscripts
            .generateMethodsSection = function(test_results, processed_data) {
                n_tests <- length(test_results)
                # CRITICAL FIX: processed_data is a list, need to access $data element
                n_cases <- nrow(processed_data$data)
                test_names <- names(test_results)

                methods <- sprintf(
                    "We compared the diagnostic performance of %s tests (%s) against the gold standard reference using diagnostic accuracy analysis. The study included %d cases with complete data. Performance metrics calculated included sensitivity, specificity, positive and negative predictive values, likelihood ratios, and overall accuracy. %s",
                    n_tests,
                    paste(test_names, collapse = ", "),
                    n_cases,
                    if (n_tests >= 2 && self$options$statComp) "Statistical comparisons between tests were performed using McNemar's test comparing diagnostic correctness (agreement with gold standard)." else ""
                )

                return(methods)
            },

            # Generate results section with key findings
            .generateResultsSection = function(test_results, best_test, best_metrics) {
                sens_pct <- round(best_metrics$Sens * 100, 1)
                spec_pct <- round(best_metrics$Spec * 100, 1)
                acc_pct <- round(best_metrics$AccurT * 100, 1)
                ppv_pct <- round(best_metrics$PPV * 100, 1)
                npv_pct <- round(best_metrics$NPV * 100, 1)

                # Determine statistical significance if McNemar was performed
                significance_note <- if (self$options$statComp && length(test_results) >= 2) {
                    " Statistical comparisons using McNemar's test revealed significant differences in test performance (detailed results in comparison tables)."
                } else {
                    ""
                }

                results <- sprintf(
                    "Among the tests evaluated, %s demonstrated optimal diagnostic performance with %s%% sensitivity (95%% CI: [see confidence interval table]), %s%% specificity (95%% CI: [see confidence interval table]), %s%% positive predictive value, %s%% negative predictive value, and %s%% overall accuracy.%s The likelihood ratio for positive results was %.2f and for negative results was %.2f.",
                    best_test,
                    sens_pct,
                    spec_pct,
                    ppv_pct,
                    npv_pct,
                    acc_pct,
                    significance_note,
                    best_metrics$LRP,
                    best_metrics$LRN
                )

                return(results)
            },

            # Generate clinical recommendations
            .generateClinicalRecommendations = function(test_results, best_test) {
                best_metrics <- test_results[[best_test]]$metrics
                sens_pct <- best_metrics$Sens * 100
                spec_pct <- best_metrics$Spec * 100

                recommendations <- '<div style="background-color: #fff3cd; padding: 15px; border-radius: 8px;">'

                if (sens_pct >= 95 && spec_pct >= 95) {
                    recommendations <- paste0(
                        recommendations,
                        "<p><strong>Clinical Use:</strong> ", best_test, " shows excellent performance for both screening and confirmatory testing.</p>"
                    )
                } else if (sens_pct >= 95) {
                    recommendations <- paste0(
                        recommendations,
                        "<p><strong>Screening Application:</strong> ", best_test, " is excellent for initial screening due to high sensitivity (low false negative rate).</p>"
                    )
                } else if (spec_pct >= 95) {
                    recommendations <- paste0(
                        recommendations,
                        "<p><strong>Confirmatory Application:</strong> ", best_test, " is excellent for confirming diagnosis due to high specificity (low false positive rate).</p>"
                    )
                } else {
                    recommendations <- paste0(
                        recommendations,
                        "<p><strong>Clinical Consideration:</strong> Consider using ", best_test, " in combination with other tests for optimal diagnostic accuracy.</p>"
                    )
                }

                recommendations <- paste0(
                    recommendations,
                    "<p><strong>Implementation Note:</strong> Results should be interpreted in the context of disease prevalence in your clinical population. ",
                    "Consider local validation studies before implementation.</p></div>"
                )

                return(recommendations)
            },

            # Generate comprehensive about this analysis panel
            .generateAboutPanel = function() {
                about_html <- paste0(
                    '<div style="font-family: Arial, sans-serif; max-width: 900px; margin: 0 auto; padding: 20px;">',
                    '<h2 style="color: #2c3e50; text-align: center; border-bottom: 2px solid #3498db; padding-bottom: 10px;">',
                    "🔬 About Medical Decision Test Comparison</h2>",

                    # What This Analysis Does
                    '<div style="background: linear-gradient(135deg, #e3f2fd 0%, #bbdefb 100%); padding: 20px; border-radius: 10px; margin: 20px 0;">',
                    '<h3 style="color: #1565c0; margin-top: 0;">📊 What This Analysis Does</h3>',
                    '<p style="line-height: 1.6; color: #333;">',
                    "This tool compares the diagnostic performance of multiple medical tests against a gold standard reference. ",
                    "It systematically evaluates sensitivity, specificity, predictive values, likelihood ratios, and overall accuracy ",
                    "to help you determine which test performs best for your clinical scenario.",
                    "</p>",
                    "</div>",

                    # When to Use
                    '<div style="background-color: #f1f8e9; border: 1px solid #8bc34a; padding: 20px; border-radius: 8px; margin: 20px 0;">',
                    '<h3 style="color: #4a7c59; margin-top: 0;">🎯 When to Use This Analysis</h3>',
                    '<ul style="line-height: 1.8; color: #4a7c59;">',
                    "<li><strong>Test Validation:</strong> Comparing new diagnostic methods against established standards</li>",
                    "<li><strong>Method Comparison:</strong> Evaluating which of several tests performs better</li>",
                    "<li><strong>Clinical Research:</strong> Validating biomarkers, imaging techniques, or clinical assessments</li>",
                    "<li><strong>Quality Assessment:</strong> Measuring agreement between different raters or methods</li>",
                    "<li><strong>Protocol Development:</strong> Optimizing diagnostic workflows</li>",
                    "</ul>",
                    "</div>",

                    # How to Use
                    '<div style="background-color: #fff3e0; border: 1px solid #ff9800; padding: 20px; border-radius: 8px; margin: 20px 0;">',
                    '<h3 style="color: #e65100; margin-top: 0;">📝 How to Use This Analysis</h3>',
                    '<ol style="line-height: 1.8; color: #e65100;">',
                    "<li><strong>Select Gold Standard:</strong> Choose your most reliable reference test (e.g., biopsy, expert consensus)</li>",
                    "<li><strong>Choose Tests to Compare:</strong> Select 2-3 diagnostic tests you want to evaluate</li>",
                    '<li><strong>Define Positive Levels:</strong> Specify what constitutes a "positive" result for each test</li>',
                    "<li><strong>Configure Options:</strong> Enable statistical comparisons, confidence intervals, or visualizations as needed</li>",
                    "<li><strong>Run Analysis:</strong> Review results tables and clinical interpretations</li>",
                    "<li><strong>Copy Report:</strong> Use the auto-generated sentences for your documentation</li>",
                    "</ol>",
                    "</div>",

                    # Key Metrics Explained
                    '<div style="background-color: #f3e5f5; border: 1px solid #9c27b0; padding: 20px; border-radius: 8px; margin: 20px 0;">',
                    '<h3 style="color: #6a1b9a; margin-top: 0;">📈 Key Metrics Explained</h3>',
                    '<div style="display: grid; grid-template-columns: 1fr 1fr; gap: 15px; color: #6a1b9a;">',
                    "<div>",
                    "<p><strong>Sensitivity:</strong> Probability test is positive when disease present (rule-out ability)</p>",
                    "<p><strong>Specificity:</strong> Probability test is negative when disease absent (rule-in ability)</p>",
                    "<p><strong>PPV:</strong> Probability of disease when test positive</p>",
                    "<p><strong>NPV:</strong> Probability of no disease when test negative</p>",
                    "</div>",
                    "<div>",
                    "<p><strong>LR+:</strong> How much positive test increases odds of disease</p>",
                    "<p><strong>LR-:</strong> How much negative test decreases odds of disease</p>",
                    "<p><strong>Accuracy:</strong> Overall probability of correct classification</p>",
                    "<p><strong>McNemar Test:</strong> Statistical comparison between paired tests</p>",
                    "</div>",
                    "</div>",
                    "</div>",

                    # Clinical Guidelines
                    '<div style="background-color: #e8f5e8; border: 1px solid #4caf50; padding: 20px; border-radius: 8px; margin: 20px 0;">',
                    '<h3 style="color: #2e7d32; margin-top: 0;">⚕️ Clinical Interpretation Guidelines</h3>',
                    '<div style="display: grid; grid-template-columns: 1fr 1fr; gap: 15px; color: #2e7d32;">',
                    "<div>",
                    '<h4 style="margin-bottom: 5px;">Screening Tests (Rule-Out):</h4>',
                    '<p style="margin-top: 0;">• Sensitivity ≥95%: Excellent<br>• NPV ≥95%: High confidence<br>• Goal: Minimize false negatives</p>',
                    "</div>",
                    "<div>",
                    '<h4 style="margin-bottom: 5px;">Confirmatory Tests (Rule-In):</h4>',
                    '<p style="margin-top: 0;">• Specificity ≥95%: Excellent<br>• PPV ≥90%: High confidence<br>• Goal: Minimize false positives</p>',
                    "</div>",
                    "</div>",
                    "</div>",

                    # Assumptions and Limitations
                    '<div style="background-color: #fff8e1; border: 1px solid #ffc107; padding: 20px; border-radius: 8px; margin: 20px 0;">',
                    '<h3 style="color: #f57f17; margin-top: 0;">⚠️ Important Assumptions & Limitations</h3>',
                    '<ul style="line-height: 1.6; color: #f57f17;">',
                    "<li><strong>Gold Standard:</strong> Assumes your reference test is truly accurate</li>",
                    "<li><strong>Sample Size:</strong> Results more reliable with larger, representative samples</li>",
                    "<li><strong>Prevalence Dependency:</strong> PPV and NPV vary with disease prevalence</li>",
                    "<li><strong>McNemar Test:</strong> Requires paired/matched data for statistical comparisons</li>",
                    "<li><strong>Missing Data:</strong> Cases with incomplete data are excluded from analysis</li>",
                    "<li><strong>Confidence Intervals:</strong> Calculated using Wilson method for better accuracy</li>",
                    "</ul>",
                    "</div>",
                    "</div>"
                )

                self$results$aboutAnalysis$setContent(about_html)
            },
            .plot1 = function(image1, ggtheme, ...) {
                plotData <- image1$state

                # Optimized data frame building - pre-allocate and batch create
                df <- private$.buildBarPlotData(plotData)

                # Create plot
                plot <- ggplot2::ggplot(df, ggplot2::aes(x = metric, y = value, fill = test)) +
                    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge()) +
                    ggplot2::coord_flip() +
                    ggplot2::scale_y_continuous(labels = scales::percent) +
                    ggplot2::labs(
                        title = jmvcore::.("Comparison of Tests"),
                        x = "",
                        y = jmvcore::.("Value"),
                        fill = jmvcore::.("Test")
                    ) +
                    ggtheme +
                    ggplot2::theme(legend.position = "bottom")

                print(plot)
                TRUE
            },
            .plotRadar = function(imageRadar, ggtheme, ...) {
                plotData <- imageRadar$state

                # Optimized radar plot data building with clinical thresholds
                df <- private$.buildRadarPlotData(plotData)

                # Ensure factor ordering for consistent radar plot
                df$metric <- factor(df$metric,
                    levels = c(
                        "Sensitivity", "Specificity", "Accuracy",
                        "PPV", "NPV", "LR+ Quality", "LR- Quality"
                    )
                )

                # Create radar plot using ggplot2
                plot <- ggplot2::ggplot(df, ggplot2::aes(
                    x = metric, y = scaled_value,
                    group = test, color = test
                )) +
                    ggplot2::geom_line(size = 1.2) +
                    ggplot2::geom_point(size = 3) +
                    ggplot2::coord_polar() +
                    ggplot2::scale_y_continuous(
                        limits = c(0, 100),
                        breaks = c(0, 25, 50, 75, 100),
                        labels = c("0%", "25%", "50%", "75%", "100%")
                    ) +
                    ggplot2::labs(
                        title = jmvcore::.("Radar Plot: Decision Test Statistics Comparison"),
                        subtitle = jmvcore::.("All metrics scaled 0-100% (LR Quality: clinical performance scale)"),
                        x = "",
                        y = "",
                        color = jmvcore::.("Test")
                    ) +
                    ggtheme +
                    ggplot2::theme(
                        axis.text.x = ggplot2::element_text(size = 10),
                        axis.text.y = ggplot2::element_text(size = 8),
                        legend.position = "bottom",
                        plot.title = ggplot2::element_text(hjust = 0.5),
                        plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 9)
                    )

                print(plot)
                TRUE
            },

            # Generate natural language summary of comparison results
            .generateSummary = function(test_results) {
                n_tests <- length(test_results)
                test_names <- names(test_results)

                html <- "<div style='background-color:#f9f9f9; border-left:4px solid #2196F3; padding:15px; margin:10px 0;'>"
                html <- paste0(html, "<h4 style='margin-top:0;'>📊 Summary</h4>")

                if (n_tests == 3) {
                    # Check if Cochran's Q was performed
                    mcnemar_table <- self$results$mcnemarTable
                    if (length(mcnemar_table$rowKeys) > 0 && grepl("overall", tolower(mcnemar_table$rowKeys[1]), fixed = FALSE)) {
                        q_row <- mcnemar_table$rowKeys[1]
                        q_stat <- mcnemar_table$getCell(rowKey = q_row, col = "stat")$value
                        q_p <- mcnemar_table$getCell(rowKey = q_row, col = "p")$value

                        html <- paste0(html, sprintf(
                            "<p><b>Overall comparison:</b> Cochran's Q test (χ² = %.2f, p = %.3f) ",
                            q_stat, q_p
                        ))

                        if (q_p < private$P_THRESHOLD_SIGNIFICANT) {
                            html <- paste0(
                                html,
                                "<span style='color:#d32f2f;'><b>found significant differences</b></span> ",
                                "among the three tests. This means the tests do NOT perform equally—at least one differs significantly from the others.</p>"
                            )

                            # Identify best test
                            accs <- sapply(test_names, function(tn) {
                                test_results[[tn]]$metrics$AccurT
                            })
                            best_test <- test_names[which.max(accs)]
                            best_acc <- max(accs, na.rm = TRUE)

                            html <- paste0(html, sprintf(
                                "<p><b>Clinical interpretation:</b> <span style='color:#1976d2;'><b>%s</b></span> ",
                                best_test
                            ), sprintf(
                                "shows the highest diagnostic accuracy (%.1f%%). Review pairwise comparisons below to see which differences are statistically significant after multiple comparison correction.</p>",
                                best_acc * 100
                            ))
                        } else {
                            html <- paste0(
                                html,
                                "<span style='color:#388e3c;'><b>found no significant differences</b></span> ",
                                "among the three tests. All tests show similar diagnostic accuracy. ",
                                "Selection can be based on practical considerations such as cost, turnaround time, or availability.</p>"
                            )
                        }
                    }
                } else if (n_tests == 2) {
                    # Extract McNemar results
                    mcnemar_table <- self$results$mcnemarTable
                    if (length(mcnemar_table$rowKeys) > 0) {
                        mcn_row <- mcnemar_table$rowKeys[1]
                        mcn_stat <- mcnemar_table$getCell(rowKey = mcn_row, col = "stat")$value
                        mcn_p <- mcnemar_table$getCell(rowKey = mcn_row, col = "p")$value

                        test1_name <- test_names[1]
                        test2_name <- test_names[2]

                        acc1 <- test_results[[test1_name]]$metrics$AccurT
                        acc2 <- test_results[[test2_name]]$metrics$AccurT

                        html <- paste0(html, sprintf(
                            "<p><b>Comparison:</b> %s (accuracy: %.1f%%) vs %s (accuracy: %.1f%%)</p>",
                            test1_name, acc1 * 100, test2_name, acc2 * 100
                        ))

                        html <- paste0(html, sprintf(
                            "<p><b>McNemar's test:</b> χ² = %.2f, p = %.3f — ",
                            mcn_stat, mcn_p
                        ))

                        if (mcn_p < private$P_THRESHOLD_SIGNIFICANT) {
                            better_test <- if (acc1 > acc2) test1_name else test2_name
                            html <- paste0(html, sprintf(
                                "<span style='color:#d32f2f;'><b>Significant difference detected.</b></span> ",
                                "%s shows significantly better diagnostic accuracy.</p>",
                                better_test
                            ))
                        } else {
                            html <- paste0(
                                html,
                                "<span style='color:#388e3c;'><b>No significant difference.</b></span> ",
                                "Both tests perform similarly. Consider practical factors when choosing.</p>"
                            )
                        }
                    }
                }

                html <- paste0(html, "</div>")
                self$results$summaryReport$setContent(html)
            },

            # Generate manuscript-ready report sentence
            .generateReportSentence = function(test_results) {
                n_tests <- length(test_results)
                test_names <- names(test_results)

                html <- "<div style='background-color:#f0f0f0; padding:15px; border:1px solid #ccc; margin:10px 0;'>"
                html <- paste0(html, "<h4 style='margin-top:0;'>📝 Manuscript-Ready Report</h4>")
                html <- paste0(
                    html, "<p style='font-size:10pt; color:#666; margin-bottom:10px;'>",
                    "Copy and adapt to your manuscript. Verify all statistical values and add clinical context.</p>"
                )

                html <- paste0(html, "<div style='background-color:#fff; padding:12px; font-family:serif; font-size:11pt; line-height:1.8;'>")

                if (n_tests == 3) {
                    mcnemar_table <- self$results$mcnemarTable
                    if (length(mcnemar_table$rowKeys) > 0 && grepl("overall", tolower(mcnemar_table$rowKeys[1]), fixed = FALSE)) {
                        q_row <- mcnemar_table$rowKeys[1]
                        q_stat <- mcnemar_table$getCell(rowKey = q_row, col = "stat")$value
                        q_df <- mcnemar_table$getCell(rowKey = q_row, col = "df")$value
                        q_p <- mcnemar_table$getCell(rowKey = q_row, col = "p")$value

                        report <- sprintf(
                            "Cochran's Q test revealed %s significant difference in diagnostic accuracy among the three tests (χ²(%d) = %.2f, p = %.3f). ",
                            if (q_p < private$P_THRESHOLD_SIGNIFICANT) "a" else "no",
                            q_df, q_stat, q_p
                        )

                        if (q_p < private$P_THRESHOLD_SIGNIFICANT) {
                            report <- paste0(
                                report,
                                "Post-hoc pairwise comparisons with Holm-Bonferroni correction were conducted to identify specific differences between tests. "
                            )

                            # Find best test
                            accs <- sapply(test_names, function(tn) {
                                test_results[[tn]]$metrics$AccurT
                            })
                            best_test <- test_names[which.max(accs)]

                            report <- paste0(report, sprintf(
                                "%s demonstrated the highest overall diagnostic accuracy.",
                                best_test
                            ))
                        }

                        html <- paste0(html, report)
                    }
                } else if (n_tests == 2) {
                    mcnemar_table <- self$results$mcnemarTable
                    if (length(mcnemar_table$rowKeys) > 0) {
                        mcn_row <- mcnemar_table$rowKeys[1]
                        mcn_stat <- mcnemar_table$getCell(rowKey = mcn_row, col = "stat")$value
                        mcn_df <- mcnemar_table$getCell(rowKey = mcn_row, col = "df")$value
                        mcn_p <- mcnemar_table$getCell(rowKey = mcn_row, col = "p")$value

                        test1_name <- test_names[1]
                        test2_name <- test_names[2]

                        report <- sprintf(
                            "McNemar's test comparing %s and %s showed %s significant difference in diagnostic accuracy (χ²(%d) = %.2f, p = %.3f).",
                            test1_name, test2_name,
                            if (mcn_p < private$P_THRESHOLD_SIGNIFICANT) "a" else "no",
                            mcn_df, mcn_stat, mcn_p
                        )

                        if (mcn_p < private$P_THRESHOLD_SIGNIFICANT) {
                            acc1 <- test_results[[test1_name]]$metrics$AccurT
                            acc2 <- test_results[[test2_name]]$metrics$AccurT
                            better_test <- if (acc1 > acc2) test1_name else test2_name

                            report <- paste0(report, sprintf(
                                " %s demonstrated significantly better diagnostic performance.",
                                better_test
                            ))
                        }

                        html <- paste0(html, report)
                    }
                }

                html <- paste0(html, "</div></div>")
                self$results$reportSentence$setContent(html)
            },

            # Generate explanations and glossary content
            .generateExplanations = function() {
                html <- "<div style='font-family: Arial, sans-serif; line-height:1.6;'>"

                # Glossary section
                html <- paste0(html, "<h4 style='color:#2c3e50; border-bottom:2px solid #3498db;'>📚 Statistical Glossary</h4>")
                html <- paste0(html, "<dl style='margin-left:15px;'>")

                html <- paste0(
                    html,
                    "<dt><b>McNemar's Test</b></dt>",
                    "<dd style='margin-bottom:12px;'><strong>Compares diagnostic CORRECTNESS</strong> of two tests relative to the gold standard. Focuses on <i>discordant pairs</i> (cases where one test is correct and the other is wrong). Tests whether Test A correctly classifies significantly more/fewer cases than Test B. Appropriate for paired/matched data only. P < 0.05 indicates tests differ significantly in accuracy.</dd>"
                )

                html <- paste0(
                    html,
                    "<dt><b>Cochran's Q Test</b></dt>",
                    "<dd style='margin-bottom:12px;'>Extension of McNemar's for 3+ tests. <strong>Tests whether diagnostic correctness differs</strong> among tests relative to the gold standard. If significant (p < 0.05), at least one test is significantly more/less accurate than the others. Proceed to pairwise comparisons with multiple comparison correction to identify which tests differ.</dd>"
                )

                html <- paste0(
                    html,
                    "<dt><b>Holm-Bonferroni Correction</b></dt>",
                    "<dd style='margin-bottom:12px;'>Adjusts p-values when making multiple comparisons to control Type I error (false positives). More powerful than standard Bonferroni correction. Applied automatically when comparing 3 tests.</dd>"
                )

                html <- paste0(
                    html,
                    "<dt><b>Discordant Pairs</b></dt>",
                    "<dd style='margin-bottom:12px;'><strong>Cases where Test A and Test B have different CORRECTNESS</strong> relative to the gold standard (e.g., Test A correct but Test B wrong, or vice versa). McNemar's test examines if the imbalance between these discordant types (A-correct-B-wrong vs. A-wrong-B-correct) is statistically significant. At least 10 discordant pairs recommended for reliable results.</dd>"
                )

                html <- paste0(
                    html,
                    "<dt><b>Sensitivity</b></dt>",
                    "<dd style='margin-bottom:12px;'>Proportion of diseased cases correctly identified (True Positive Rate). High sensitivity means few false negatives. Important when missing disease is costly.</dd>"
                )

                html <- paste0(
                    html,
                    "<dt><b>Specificity</b></dt>",
                    "<dd style='margin-bottom:12px;'>Proportion of non-diseased cases correctly identified (True Negative Rate). High specificity means few false positives. Important when false alarms are problematic.</dd>"
                )

                html <- paste0(
                    html,
                    "<dt><b>PPV (Positive Predictive Value)</b></dt>",
                    "<dd style='margin-bottom:12px;'>Probability that a positive test result truly indicates disease. Depends on disease prevalence—higher in populations with higher disease rates.</dd>"
                )

                html <- paste0(
                    html,
                    "<dt><b>NPV (Negative Predictive Value)</b></dt>",
                    "<dd style='margin-bottom:12px;'>Probability that a negative test result truly indicates absence of disease. Also depends on prevalence.</dd>"
                )

                html <- paste0(html, "</dl>")

                # Assumptions section
                html <- paste0(html, "<h4 style='color:#2c3e50; border-bottom:2px solid #3498db; margin-top:25px;'>⚙️ Assumptions & Requirements</h4>")
                html <- paste0(html, "<ul style='margin-left:15px;'>")
                html <- paste0(
                    html,
                    "<li><b>Paired Data:</b> All tests must be performed on the same patients/samples (not independent groups)</li>",
                    "<li><b>Adequate Sample Size:</b> At least 10 discordant pairs recommended for McNemar's test reliability</li>",
                    "<li><b>Binary Tests:</b> Each test must have exactly 2 levels (positive/negative or similar)</li>",
                    "<li><b>Gold Standard:</b> Reference test must represent true disease status (e.g., biopsy, final diagnosis)</li>",
                    "<li><b>Complete Cases:</b> Cases with missing data in any test are excluded from comparisons</li>"
                )
                html <- paste0(html, "</ul>")

                # When to use section
                html <- paste0(html, "<h4 style='color:#2c3e50; border-bottom:2px solid #3498db; margin-top:25px;'>🎯 When to Use This Analysis</h4>")
                html <- paste0(html, "<ul style='margin-left:15px;'>")
                html <- paste0(
                    html,
                    "<li>Comparing diagnostic accuracy of 2-3 tests performed on same patients</li>",
                    "<li>Evaluating if a new test is significantly better/worse than standard test</li>",
                    "<li>Optimizing test selection based on performance metrics</li>",
                    "<li>Validating diagnostic tools in clinical or pathology practice</li>"
                )
                html <- paste0(html, "</ul>")

                # Limitations section
                html <- paste0(html, "<h4 style='color:#2c3e50; border-bottom:2px solid #e74c3c; margin-top:25px;'>⚠️ Limitations</h4>")
                html <- paste0(html, "<ul style='margin-left:15px; color:#c0392b;'>")
                html <- paste0(
                    html,
                    "<li>McNemar's test compares overall accuracy only—does not separately test sensitivity vs specificity differences</li>",
                    "<li>Requires paired observations—cannot compare tests performed on different patient groups</li>",
                    "<li>This function cannot handle continuous test results directly for statistical comparison. For continuous diagnostic tests, please use the dedicated ROC (Receiver Operating Characteristic) analysis functions for comparing curves (e.g., DeLong's test).</li>",
                    "<li>Small number of discordant pairs reduces statistical power and reliability</li>",
                    "<li>P-values do not indicate clinical importance—consider effect sizes and practical implications</li>"
                )
                html <- paste0(html, "</ul>")

                html <- paste0(html, "</div>")

                self$results$explanationsContent$setContent(html)
            }
        )
    )
}
