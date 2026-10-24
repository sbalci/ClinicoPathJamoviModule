#' @title Analysis Without Gold Standard
#' @importFrom R6 R6Class
#' @importFrom graphics axis box image
#' @importFrom grDevices hcl.colors
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
            # Set by .bootCI() when it blanks a zero-width interval.
            .degenerateCI = FALSE,

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

            # Syntax pane: jmvcore writes a Variable value unquoted (`test1 = HPV DNA` does not
            # parse) and a Level value without escaping `"` or `\`, so a crafted column name could
            # put live code into a snippet users copy into R. A NULL Level stays with super: the
            # wrapper needs `test3Positive = NULL` because Level arguments have no default.
            .sourcifyOption = function(option) {
                quoted <- c(paste0("test", 1:5), paste0("test", 1:5, "Positive"))
                value <- option$value
                if (option$name %in% quoted && is.character(value) &&
                    length(value) == 1L && nzchar(value))
                    return(paste0(option$name, " = ", encodeString(value, quote = '"')))
                super$.sourcifyOption(option)
            },

            # The .a.yaml option TITLES, so notices can name a choice the way the
            # drop-down does. Splicing the raw enum key showed the user internal
            # snake_case ("latent_class", "any_positive") in every locale.
            .methodLabel = function(m) {
                switch(as.character(m),
                    latent_class = .("Latent Class Analysis"),
                    composite    = .("Composite Reference (strict majority)"),
                    all_positive = .("All Tests Positive"),
                    any_positive = .("Any Test Positive"),
                    bayesian     = .("Penalized EM (MAP-like; fixed priors; 3+ tests)"),
                    as.character(m))
            },
            .presetLabel = function(p) {
                switch(as.character(p),
                    none                  = .("No example"),
                    diagnostic_validation = .("Diagnostic Test Validation (example)"),
                    pathology_agreement   = .("Pathologist Agreement (example)"),
                    tumor_markers         = .("Tumor Marker Evaluation (example)"),
                    screening_evaluation  = .("Screening Test Assessment (example)"),
                    as.character(p))
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

                # WRONG BEFORE: the severity word was a hard-coded English literal, so every
                # line of the Important Information panel opened in English even when its
                # body was translated -- and the severity marker is what a skimming reader
                # triages on. Built once here so `.()` is not called inside the vapply.
                labels <- c(
                    ERROR          = .("Error"),
                    STRONG_WARNING = .("Strong warning"),
                    WARNING        = .("Warning"),
                    INFO           = .("Info")
                )

                blocks <- vapply(private$.noticeList[notice_order], function(notice) {
                    prefix <- if (!is.null(notice$type) && notice$type %in% names(labels))
                        labels[[notice$type]] else labels[["INFO"]]
                    paste0(prefix, ": ", notice$title, ": ", notice$content)
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

                # Rows fixed by the options are built here and filled with setRow() in
                # .run(), so the tables do not appear empty and then restructure.
                tests <- private$.configuredTests()
                if (length(tests) >= 2 && !anyDuplicated(tests) &&
                    length(private$.pendingSlots()) == 0)
                    private$.initTables(tests)
            },
            # Variables that have both a test variable and a selected positive level.
            .configuredTests = function() {
                unlist(lapply(seq_len(5), function(i) {
                    v <- self$options[[paste0("test", i)]]
                    if (!is.null(v) && !is.null(self$options[[paste0("test", i, "Positive")]])) v
                }))
            },
            # Slots holding a variable whose positive level is still unselected (allowNone
            # makes that the state of every newly added variable). Such a test used to be
            # dropped in silence: rule methods ran on the rest, latent class asked for "more
            # tests" the user had already added.
            .pendingSlots = function() {
                Filter(function(i) !is.null(self$options[[paste0("test", i)]]) &&
                           is.null(self$options[[paste0("test", i, "Positive")]]),
                       seq_len(5))
            },
            .initTables = function(tests) {
                # Cells are HTML-filtered by the client: a raw "<LOD flag" renders blank and
                # "a<b" as "a". Break the tag at the display sink only; rowKeys stay raw.
                for (t in tests)
                    self$results$test_metrics$addRow(rowKey = t, values = list(test = .diagCellText(t)))

                latent <- identical(self$options$method, "latent_class")
                # Penalized EM gets the dependence check too (from its unpenalized fit).
                dependence <- self$options$method %in% c("latent_class", "bayesian")
                # Same "pair_i_j" keys and order as the i < j loops in .run().
                for (p in utils::combn(length(tests), 2, simplify = FALSE)) {
                    key <- paste0("pair_", p[1], "_", p[2])
                    label <- .diagCellText(.fmt(.("{a} vs {b}"), a = tests[p[1]], b = tests[p[2]]))
                    self$results$agreement_stats$addRow(rowKey = key, values = list(test_pair = label))
                    if (dependence)
                        self$results$conditional_dependence$addRow(rowKey = key, values = list(pair = label))
                }

                if (latent) {
                    # rowKey keeps the stable English key; only the label is translated.
                    labels <- list(
                        "BIC"                = .("BIC"),
                        "AIC"                = .("AIC"),
                        "Log-Likelihood"     = .("Log-Likelihood"),
                        "G-squared"          = .("G-squared"),
                        "Chi-squared"        = .("Chi-squared"),
                        "Degrees of Freedom" = .("Degrees of Freedom"))
                    for (name in names(labels))
                        self$results$model_fit$addRow(rowKey = name, values = list(statistic = labels[[name]]))
                }
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
                        pattern_labels[j] <- paste0(.diagCellText(test_name), result)
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
                .deleteTableRows(crosstab_table) # Clear existing rows to prevent duplicates on re-run
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
                if (length(private$.configuredTests()) < 2 ||
                    length(private$.pendingSlots()) > 0) {
                    self$results$instructions$setVisible(TRUE)
                    # clinical_summary carries `visible: (showSummary)` in the .r.yaml;
                    # calling setVisible() here re-implemented that binding imperatively and
                    # would silently override any future edit to the expression. Clearing the
                    # content is enough.
                    self$results$clinical_summary$setContent("")

                    # Get method-specific content
                    method_info <- private$.getMethodSpecificContent()

                    # Show welcome/instruction message
                    instructions <- paste0(
                        "<html><head></head><body>",
                        "<div class='instructions' style='background-color: rgba(138, 155, 172, 0.06); padding: 20px; border-radius: 8px; margin: 20px 0; color: inherit;'>",
                        "<h3 style='color: inherit; margin-top: 0;'> ", .("Analysis Without Gold Standard"), "</h3>",
                        "<p><strong>", .("Analyze diagnostic test performance when no perfect reference test (gold standard) is available."), "</strong></p>",
                        "<p>", .("This analysis estimates latent-class parameters or describes agreement with a reference rule built from the selected tests. Interpretation depends on the chosen method and its assumptions."), "</p>",
                        "<h4 style='color: inherit;'>", .("Required Steps:"), "</h4>",
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
                        "<li><strong>", .("Optional: Bootstrap CI:"), "</strong> ", .("Enable bootstrap for approximate confidence intervals (estimates do not change)"), "</li>",
                        "</ol>",
                        method_info$background,
                        method_info$references,

                        # Add preset guidance if available
                        if (!is.null(private$.preset_info)) {
                            paste0(
                                "<div style='background-color: rgba(33, 159, 33, 0.1); padding: 15px; border-radius: 8px; margin: 15px 0; border-left: 4px solid #4caf50; color: inherit;'>",
                                "<h4 style='color: inherit; margin-top: 0;'> ", .("Active Illustrative Example"), "</h4>",
                                "<p><strong>", .("Example only:"), "</strong> ", .("This scenario is illustrative, does not change settings, and is not a clinical guide or validated recommendation."), "</p>",
                                # was the raw enum key, e.g. "tumor_markers"
                                "<p><strong>", .("Scenario"), ":</strong> ", private$.presetLabel(self$options$clinicalPreset), "</p>",
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
                    # `visible: (showSummary)` in the .r.yaml already does this.
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
                            .("At least three informative tests are needed. Three tests give zero nominal residual degrees of freedom, but exact fit and parameter identifiability are not guaranteed. Fit discrepancies remain descriptive rather than formal chi-squared tests."),
                            .("The model assumes conditional independence within each latent class. A large bivariate residual is evidence against that assumption but does not determine the direction of bias."),
                            .("Class labels are unidentified. Calling the high-positive class disease requires a study-specific substantive justification.")
                        ),
                        citations = c(
                            "Linzer DA, Lewis JB. poLCA: An R Package for Polytomous Variable Latent Class Analysis. <em>Journal of Statistical Software</em>. 2011;42(10):1-29. doi:10.18637/jss.v042.i10.",
                            "Allman ES, et al. Maximum likelihood estimation of the Latent Class Model through model boundary decomposition. <em>arXiv</em>. 2018;1710.01696v2.",
                            "Hui SL, Walter SD. Estimating the error rates of diagnostic tests. <em>Biometrics</em>. 1980;36(1):167-71.",
                            "Dendukuri N, Joseph L. Bayesian approaches to modeling the conditional dependence between multiple diagnostic tests. <em>Biometrics</em>. 2001;57(1):158-67."
                        )
                    ),
                    bayesian = list(
                        title = .("Statistical background: Penalized EM"),
                        bullets = c(
                            .("This method maximizes the two-class conditional-independence log likelihood plus the sum of log sensitivities and log specificities. These fixed Beta(2,1) penalties favor higher accuracy; a uniform Beta(1,1) penalty is used for the class proportion. Multiple EM initializations and numerical refinement produce a MAP point estimate, not a posterior sample."),
                            .("At least three informative tests are needed; the number of tests alone does not establish identifiability. Estimates are suppressed when the data do not identify the latent parameters."),
                            .("The priors are fixed and its intervals are approximate case-bootstrap confidence intervals, not Bayesian credible intervals. The cited Bayesian studies are background; their posterior-sampling or dependence-adjusted methods are not implemented here."),
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
                    "<h4 style='color: inherit;'>", method_content$title, "</h4><ul>",
                    paste0("<li>", method_content$bullets, "</li>", collapse = ""),
                    "</ul>"
                )
                references <- if (length(method_content$citations) == 0) {
                    ""
                } else {
                    paste0(
                        "<h4 style='color: inherit;'>", .("References"), "</h4><ul>",
                        paste0("<li>", method_content$citations, "</li>", collapse = ""),
                        "</ul>"
                    )
                }

                list(background = background, references = references)
            },
            .run = function() {
                # Notices are rendered on EVERY exit path (completion, the welcome-state
                # return(), and each jmvcore::reject() below), so a validation abort never
                # leaves the previous run's "Important Information" panel on screen.
                private$.noticeList <- list()
                on.exit(private$.renderNotices(), add = TRUE)
                private$.diagLines <- character(0)
                private$.boot_cache <- NULL
                private$.degenerateCI <- FALSE
                private$.preset_info <- NULL
                self$results$clinical_summary$setContent("")

                # .init() sees only the initial option values. Refresh this optional panel's
                # CONTENT on every run so switching it on after analysis creation does not
                # reveal an empty result item. Its visibility is owned by the .r.yaml
                # `visible: (showMethodGuide)` binding, not by this call.
                private$.showMethodGuide()

                # Preset advisory: after the reset above, so it survives to be rendered.
                private$.applyPreset()

                # Show welcome message if needed and return early if instructions are displayed
                if (private$.showWelcomeMessage()) {
                    pending <- private$.pendingSlots()
                    if (length(pending) > 0)
                        private$.addNotice("INFO", .("Positive level not selected"),
                            .fmt(.("Choose the positive level for {tests}. A test enters the analysis only after its positive level is chosen, so no results are shown yet."),
                                 tests = paste(vapply(pending, function(i)
                                     .fmt(.("Test {number} ({variable})"), number = i,
                                          variable = self$options[[paste0("test", i)]]),
                                     character(1)), collapse = ", ")))
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

                # One variable in two slots left the second copy all-NA (columns are
                # assigned by name): composite aborted with an R error and latent class
                # fitted a column of missing values.
                duplicated_tests <- unlist(tests)[duplicated(unlist(tests))]
                if (length(duplicated_tests) > 0) {
                    jmvcore::reject(.("Each test must be a different variable; '{}' is selected more than once."),
                                    code = NULL, duplicated_tests[1])
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
                # Explicit NA factor levels are missing too; factor codes alone conceal them.
                for (nm in names(test_data)) {
                    missing <- is.na(as.character(test_data[[nm]]))
                    is.na(test_data[[nm]]) <- missing
                }
                test_data <- jmvcore::naOmit(test_data)
                n_after <- nrow(test_data)

                # Neither the analysed N nor the number excluded appeared anywhere in the
                # output: every estimate below was computed on the complete cases while the
                # user still saw the dataset's full size.
                if (n_after < n_before) {
                    private$.addNotice(
                        "WARNING",
                        # Was raw English: the msgid must be the literal, so wrap the
                        # sprintf FORMAT and interpolate outside it.
                        sprintf(.("Excluded %d case(s) with missing test results"), n_before - n_after),
                        sprintf(.("This analysis uses the %1$d of %2$d cases (%3$.1f%%) with a result recorded for every selected test. All displayed estimates use these complete cases. If the probability of a missing result is related to an unobserved test result or latent class, the estimates can be biased."),
                                n_after, n_before, 100 * n_after / n_before)
                    )
                } else {
                    private$.addNotice(
                        "INFO",
                        sprintf(.("Analyzing %d cases"), n_after),
                        sprintf(.("All %d cases have a result for every selected test."), n_after)
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
                private$.validateClinicalAssumptions(test_data, tests, self$options$method,
                                                    test_levels)

                # Convert to binary format for analysis
                binary_data <- data.frame(matrix(nrow = nrow(test_data), ncol = length(tests)))
                names(binary_data) <- unlist(tests)

                for (i in seq_along(tests)) {
                    test_name <- tests[[i]]
                    pos_level <- test_levels[[i]]

                    var <- test_data[[test_name]]
                    binary_data[[test_name]] <- as.numeric(var == pos_level)
                }

                # WRONG BEFORE: poLCA silently drops a constant test ("ALERT ... removed from
                # the analysis"), so .runLCA built 2(k-1)+1 starting values for a k-test
                # likelihood and nlminb stopped with "gradient function must return a numeric
                # vector of length 5". Penalized EM keeps the column and reports it honestly.
                if (self$options$method == "latent_class") {
                    constant <- names(binary_data)[vapply(binary_data,
                        function(x) length(unique(x)) < 2, logical(1))]
                    if (length(constant) > 0)
                        jmvcore::reject(.("Latent class analysis needs both positive and not-positive results for every test among the analyzed cases. Only one result was observed for: {}. Such a test carries no information about the latent classes. Check that the correct positive level is selected, or remove the test."),
                                        code = NULL, paste0("'", constant, "'", collapse = ", "))
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

                    # 2^3 - 1 = 7 cell frequencies, 2 * 3 + 1 = 7 parameters: the fit is
                    # saturated, so a dependent pair (sim: Se .8 read as .95) leaves no trace in
                    # G-squared or the residuals. The table footnotes alone were too easy to miss.
                    if (length(tests) == 3) {
                        private$.addNotice(
                            "STRONG_WARNING",
                            .("Conditional independence cannot be checked with three tests"),
                            .("With three tests the two-class model has as many parameters as the data have free cell frequencies, so it usually reproduces the observed table exactly and conditional independence cannot be checked from these data. If two of the tests share a mechanism (same target, same reader or same specimen), the estimates can be strongly biased in either direction with no sign of it in the fit statistics or the bivariate residuals. Detecting such dependence needs at least four tests.")
                        )
                    }

                    # Orientation is set by the tests on average, so one test whose positive
                    # level was picked the wrong way round comes out as sensitivity + specificity
                    # < 1 (sim .7/.75 reversed: .356/.266) and reads as a poor test. Name it.
                    below <- names(binary_data)[is.finite(results$sensitivities) &
                        is.finite(results$specificities) &
                        results$sensitivities + results$specificities < 1]
                    if (length(below)) {
                        private$.addNotice(
                            "STRONG_WARNING",
                            .("Test below chance"),
                            .fmt(.("Sensitivity plus specificity is below 1 for: {tests}. Such a test agrees with the fitted disease class less often than chance. Check that its positive level is not reversed before interpreting it as a poor test."),
                                 tests = paste(below, collapse = ", ")))
                    }

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
                } else if (!is.null(results) && is.finite(results$prevalence) &&
                           !is.null(results$data)) {
                    # Response probabilities condition on rule groups; predictive values
                    # instead condition on each test's positive or negative results.
                    n_rule <- nrow(results$data)
                    n_rule_pos <- round(n_rule * results$prevalence)
                    n_rule_neg <- n_rule - n_rule_pos
                    if (n_rule_pos < 10 || n_rule_neg < 10) {
                        private$.addNotice(
                            "STRONG_WARNING",
                            .("Reference rule splits the sample very unevenly"),
                            .fmt(
                                .("Of {n} analyzed cases, {npos} meet the selected reference rule and {nneg} do not. Positive- and negative-response probabilities use these rule-group counts. PPV and NPV instead use each test's positive and negative counts. With fewer than about 10 cases in a conditioning group, its estimate and interval can be unstable."),
                                n = n_rule, npos = n_rule_pos, nneg = n_rule_neg
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
                        # This is the notice that tells the user the estimates below are
                        # not a solution. It was English-only in every locale; wrapped now.
                        .("The estimation did not converge"),
                        sprintf(.("The latent-model solver did not meet its numerical convergence checks after %s refinement iterations. Point estimates and intervals have been left blank. Try a different method, or check whether the tests nearly perfectly agree or provide little information about separate classes."),
                                if (is.null(results$iterations)) "100" else as.character(results$iterations))
                    )
                }


                # One bootstrap pass, reused by every table below.
                if (isTRUE(self$options$bootstrap) && !is.null(results) &&
                    !isFALSE(results$estimable)) {
                    private$.boot_cache <- private$.bootstrapAll(
                        data = results$data,
                        method = self$options$method,
                        nboot = self$options$nboot,
                        warm_start = switch(self$options$method,
                            latent_class = results$model$probs,
                            bayesian = results$par)
                    )
                }

                if (isTRUE(self$options$bootstrap) && isFALSE(results$estimable)) {
                    private$.boot_cache <- list(prevalence = numeric(0),
                        sensitivities = matrix(numeric(0), 0, length(tests)),
                        specificities = matrix(numeric(0), 0, length(tests)))
                    private$.addNotice("WARNING", .("Bootstrap intervals not estimated"),
                        .("The full-sample latent fit is not reportable, so bootstrap refitting was skipped. Resolve the convergence, identification or class-orientation warning first."))
                }

                private$.checkpoint() # Before result population

                # Update results
                if (!is.null(results)) {
                    private$.populatePrevalence(results)
                    private$.populateTestMetrics(results)
                    if (isTRUE(private$.degenerateCI))
                        private$.addNotice("WARNING", .("Some bootstrap intervals left blank"),
                            .("For at least one estimate every bootstrap resample gave the same value, so its percentile interval has zero width. A zero-width interval does not describe the uncertainty and is left blank. This usually happens at 0% or 100%."))
                    if (self$options$method == "latent_class" && !isFALSE(results$converged)) {
                        private$.populateModelFit(results$model)
                        private$.populateConditionalDependence(results$model, tests)
                    }
                    # WRONG BEFORE: penalized EM never checked dependence, so a pair that
                    # latent_class flags (BVR 94.6) reported Se .95 for a .80 test silently.
                    if (self$options$method == "bayesian" && !isFALSE(results$converged)) {
                        if (is.null(results$model)) {
                            private$.addNotice("STRONG_WARNING", .("Conditional independence not checked"),
                                .("The unpenalized companion fit did not converge, so the bivariate residuals could not be computed. Penalized EM assumes the tests err independently within each class; run the latent class analysis and check its residuals before interpreting these estimates."))
                        } else {
                            private$.populateConditionalDependence(results$model, tests)
                            dep_table <- private$.resultsItem("conditional_dependence")
                            if (!is.null(dep_table)) dep_table$setNote("source",
                                .("Penalized EM: residuals come from the unpenalized (maximum-likelihood) fit of the same conditional-independence model, so the fixed priors do not enter them. They check the model assumption, not the prior-adjusted estimates."))
                        }
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
                # (agreement_plot has no `visible:` binding, so it is visible by default;
                # the setVisible(TRUE) that stood here was a no-op.)
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

            # Footnote for latent-model bootstrap replicates; NULL clears it otherwise.
            .bootWarmStartNote = function() {
                if (!isTRUE(self$options$bootstrap)) return(NULL)
                switch(self$options$method,
                    latent_class = .("Each latent-class resample uses the full-sample solution and two additional random starts, followed by likelihood refinement. The main fit uses a larger search, so local-optimum uncertainty may remain. Percentile intervals can be too narrow with weak identification, boundaries or small samples; nominal coverage is not guaranteed."),
                    bayesian = .("Each penalized-EM resample starts from the full-sample solution and two standard starting values, followed by penalized-likelihood refinement. The main fit uses a larger search, so local-optimum uncertainty may remain. Percentile intervals can be too narrow with weak identification, boundaries or small samples; nominal coverage is not guaranteed."),
                    NULL)
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

                # the fit statistics come from the same seeded multi-start search
                seed_note <- if (isTRUE(self$options$bootstrap) || identical(self$options$method, "latent_class"))
                    .fmt(.("Random seed: {seed}"), seed = private$.seedValue())
                for (nm in c("model_fit", "conditional_dependence")) {
                    t <- private$.resultsItem(nm)
                    if (!is.null(t)) t$setNote("seed", seed_note)
                }

                table <- self$results$prevalence
                # bootstrap resamples and the latent-class random starts both draw from the seed
                table$setNote("seed", if (isTRUE(self$options$bootstrap) || identical(self$options$method, "latent_class")) .fmt(.("Random seed: {seed}"), seed = private$.seedValue()))
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
                # Write NULL too, so a note from an earlier method does not persist.
                table$setNote(
                    "boot_warm_start", private$.bootWarmStartNote())
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

                # Rows are built in .init(), keyed by test name.
                table <- self$results$test_metrics

                # The columns are headed "Lower CI"/"Upper CI" whatever produced them and at
                # whatever level, so an 80% Wald interval was indistinguishable from a 95%
                # bootstrap percentile interval. State both.
                conf_pct <- 100 * (1 - self$options$alpha)
                # the bootstrap note below already names the seed
                table$setNote("seed", if (!isTRUE(self$options$bootstrap) && identical(self$options$method, "latent_class")) .fmt(.("Random seed: {seed}"), seed = private$.seedValue()))
                table$setNote(
                    "ci_provenance",
                    if (isTRUE(self$options$bootstrap))
                        sprintf(jmvcore::.("%1$.0f%% intervals are bootstrap percentile intervals from %2$d resamples of the cases, refitting the model on each. Seed: %3$s."),
                                # WRONG BEFORE: this printed the RAW option while the note
                                # directly above it on the same table printed
                                # private$.seedValue() -- the value withr::local_seed actually
                                # received. `seed` has no min:, so seed = -7 gave "Seed: -7"
                                # here and "Random seed: 2147483640" one line up, and the
                                # published seed did not reproduce the intervals.
                                conf_pct, self$options$nboot,
                                base::format(private$.seedValue()))
                    else if (self$options$method %in% c("latent_class", "bayesian"))
                        sprintf(jmvcore::.("%.0f%% confidence intervals are not reported for latent-model response probabilities and predictive values without bootstrap refitting. Ordinary binomial intervals omit uncertainty from estimating the latent classes and can be substantially too narrow. Enable Bootstrap CI to obtain intervals."),
                                conf_pct)
                    else
                        sprintf(jmvcore::.("%.0f%% intervals are Wilson score intervals for agreement with the observed reference rule."),
                                conf_pct)
                )
                table$setNote(
                    "boot_warm_start", private$.bootWarmStartNote())
                table$setNote(
                    "predictive",
                    .("PPV and NPV hold only at the class or rule proportion of these data. In a population with a different prevalence they differ, even when the response probabilities are the same."))

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
                    # WRONG BEFORE: the rule description ("every test is positive") and
                    # the metric-pair name ("Sensitivity and NPV") were hardcoded ENGLISH
                    # fragments spliced through %s into a translated format, so the clause
                    # naming which metrics are meaningless stayed English in every locale.
                    # The format also carried two unordered %s with no %n$ markers. Both
                    # branches are now complete, independently translatable sentences.
                    private$.addNotice(
                        "STRONG_WARNING",
                        .("This method cannot estimate accuracy"),
                        if (identical(meth, "all_positive"))
                            .("The reference rule is defined as \"every test is positive\", so each test is compared against a rule built from its own result. Sensitivity and NPV are therefore fixed at 100% by construction on every dataset and are left blank rather than reported as findings. The remaining values are also affected by this circularity and describe agreement with the rule, not diagnostic accuracy.")
                        else
                            .("The reference rule is defined as \"at least one test is positive\", so each test is compared against a rule built from its own result. Specificity and PPV are therefore fixed at 100% by construction on every dataset and are left blank rather than reported as findings. The remaining values are also affected by this circularity and describe agreement with the rule, not diagnostic accuracy.")
                    )
                    table$setNote(
                        "incorporation",
                        jmvcore::.("Each test is scored against a reference standard built from the tests themselves, so these are measures of agreement with that rule, not estimates of diagnostic accuracy. The blank column is fixed at 100% by construction and carries no information.")
                    )
                }

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
                no_ci <- list(lower = NA_real_, upper = NA_real_)

                for (i in seq_along(tests)) {
                    sensitivity <- results$sensitivities[i]
                    specificity <- results$specificities[i]

                    # Calculate confidence intervals
                    if (self$options$bootstrap) {
                        # WRONG BEFORE: the blanked column went through .bootCI too. Its
                        # resamples are all exactly 1, so it raised "Some bootstrap intervals
                        # left blank" on every all/any-positive run, blaming the bootstrap
                        # for a column that is blank by construction.
                        sens_ci <- if (degenerate_sens) no_ci else
                            private$.bootCI(private$.boot_cache$sensitivities[, i], self$options$alpha)
                        spec_ci <- if (degenerate_spec) no_ci else
                            private$.bootCI(private$.boot_cache$specificities[, i], self$options$alpha)
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
                    has_pos <- is.null(results$data) || any(results$data[[i]] == 1, na.rm = TRUE)
                    has_neg <- is.null(results$data) || any(results$data[[i]] == 0, na.rm = TRUE)
                    if (self$options$method %in% c("composite", "all_positive", "any_positive")) {
                        ref <- switch(self$options$method,
                            composite = rowMeans(results$data) > 0.5,
                            all_positive = rowSums(results$data) == ncol(results$data),
                            any_positive = rowSums(results$data) > 0)
                        positive <- results$data[[i]] == 1
                        ppv_npv <- list(
                            ppv = if (any(positive)) mean(ref[positive]) else NA_real_,
                            npv = if (any(!positive)) mean(!ref[!positive]) else NA_real_
                        )
                    } else {
                        ppv_npv <- private$.calculatePPVNPV(sensitivity, specificity, prevalence)
                        if (!has_pos) ppv_npv$ppv <- NA_real_
                        if (!has_neg) ppv_npv$npv <- NA_real_
                    }

                    # Predictive-value intervals use the same source as the other intervals.
                    # Each resample's PPV/NPV follow from its cached prevalence, sensitivity and
                    # specificity (exact on a 2x2 table), so no extra refitting is needed.
                    if (isTRUE(self$options$bootstrap)) {
                        boot <- private$.boot_cache
                        boot_pv <- private$.calculatePPVNPV(boot$sensitivities[, i],
                            boot$specificities[, i], boot$prevalence)
                        ppv_ci <- if (degenerate_spec || !has_pos) no_ci else
                            private$.bootCI(boot_pv$ppv, self$options$alpha)
                        npv_ci <- if (degenerate_sens || !has_neg) no_ci else
                            private$.bootCI(boot_pv$npv, self$options$alpha)
                    } else if (self$options$method %in% c("composite", "all_positive", "any_positive")) {
                        ppv_ci <- private$.wilsonCI(sum(ref & positive), sum(positive), self$options$alpha)
                        npv_ci <- private$.wilsonCI(sum(!ref & !positive), sum(!positive), self$options$alpha)
                    } else {
                        ppv_ci <- npv_ci <- no_ci
                    }

                    if (degenerate_sens) {
                        sensitivity <- NA_real_
                        sens_ci <- no_ci
                        ppv_npv$npv <- NA_real_
                        npv_ci <- no_ci
                    } else if (degenerate_spec) {
                        specificity <- NA_real_
                        spec_ci <- no_ci
                        ppv_npv$ppv <- NA_real_
                        ppv_ci <- no_ci
                    }

                    table$setRow(rowKey = tests[i], values = list(
                        sensitivity = sensitivity,
                        specificity = specificity,
                        sens_ci_lower = sens_ci$lower,
                        sens_ci_upper = sens_ci$upper,
                        spec_ci_lower = spec_ci$lower,
                        spec_ci_upper = spec_ci$upper,
                        ppv = ppv_npv$ppv,
                        ppv_ci_lower = ppv_ci$lower,
                        ppv_ci_upper = ppv_ci$upper,
                        npv = ppv_npv$npv,
                        npv_ci_lower = npv_ci$lower,
                        npv_ci_upper = npv_ci$upper
                    ))
                }
            },
            # Descriptive pairwise discrepancies remain useful at nominal df = 0:
            # the binary latent-class model imposes probability inequalities as well.
            .populateConditionalDependence = function(model, tests) {
                # The table may not exist until jmvtools::prepare() compiles the .r.yaml,
                # but the dependence WARNING is clinically important either way -- so
                # compute regardless and guard only the table writes.
                table <- private$.resultsItem("conditional_dependence")
                if (is.null(model)) return()

                resid_df <- model$resid.df
                just_identified <- !is.numeric(resid_df) || length(resid_df) != 1 ||
                    !is.finite(resid_df) || resid_df <= 0

                if (!is.null(table)) table$setNote("df0", if (just_identified)
                    .("With three tests the nominal residual degrees of freedom are zero, but the latent-class model need not reproduce the observed table. These residuals describe discrepancies; they are not chi-squared tests of conditional independence. A small residual does not establish the assumption."))

                y <- model$y
                if (is.null(y) || nrow(y) == 0) return()
                # WRONG BEFORE: matched on names(model$y). poLCA builds model$y with
                # as.data.frame() on the cbind response matrix, which runs make.names() on
                # the column names, while model$probs keeps the names the user selected.
                # Any test named non-syntactically ("HPV DNA", "p16+/p53", "Grade 2/3")
                # therefore never matched and the whole bivariate-residual panel stayed
                # silently empty. .bivariateResidual() matches on names(model$probs).
                tests <- unlist(tests)

                THRESHOLD <- stats::qchisq(0.95, df = 1)   # 3.841
                flagged <- character(0)

                for (i in seq_len(length(tests) - 1)) {
                    for (j in seq(i + 1, length(tests))) {
                        a <- tests[i]
                        b <- tests[j]
                        bvr <- private$.bivariateResidual(model, a, b)
                        # The .init() row stays blank when the residual is unavailable.
                        if (!is.finite(bvr)) next
                        # Test names can contain the display separator; key by position.
                        pair_key <- paste0("pair_", i, "_", j)
                        pair <- .fmt(.("{a} vs {b}"), a = a, b = b)
                        if (bvr > THRESHOLD) flagged <- c(flagged, pair)
                        if (!is.null(table)) table$setRow(rowKey = pair_key, values = list(
                            bvr = bvr,
                            # WRONG BEFORE: with three tests an interior fit reproduces every
                            # pairwise table exactly (residual ~1e-29 even when one test
                            # copies another), yet each row read "Consistent with independence".
                            verdict = if (just_identified && bvr < 1e-6)
                                jmvcore::.("Not assessable: the model reproduces this pair exactly")
                            else if (bvr > THRESHOLD)
                                jmvcore::.("Evidence against conditional independence")
                            else jmvcore::.("No evidence against conditional independence (descriptive)")
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
                        sprintf(jmvcore::.("%s have bivariate residuals above the descriptive threshold. This is evidence that the conditional-independence model does not reproduce those pairwise tables. The flagged pair is not necessarily the dependent pair: dependence between other tests can be absorbed into the latent classes and appear as misfit elsewhere. Because the residual is squared, it does not show the direction or cause of the dependence and cannot determine whether fitted response probabilities are biased upward or downward. Do not interpret the latent-class parameters as diagnostic accuracy without a dependence-sensitive model or a study-specific sensitivity analysis."),
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
                    !a %in% names(model$probs) || !b %in% names(model$probs)) {
                    return(NA_real_)
                }

                # model$y's names are make.names()-mangled copies of model$probs' names and
                # sit in the same order, so `model$y[[a]]` was NULL for any non-syntactic
                # test name. Index model$y positionally instead of by name.
                ia <- match(a, names(model$probs))
                ib <- match(b, names(model$probs))
                if (is.na(ia) || is.na(ib) ||
                    ia > ncol(model$y) || ib > ncol(model$y)) {
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
                    factor(as.character(model$y[[ia]]), levels = colnames(probs_a)),
                    factor(as.character(model$y[[ib]]), levels = colnames(probs_b))
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

                # Nominal dimension does not remove the model's inequality constraints.
                # Always show discrepancies, without a chi-squared p-value at zero df.
                resid_df <- model$resid.df
                just_identified <- is.numeric(resid_df) && length(resid_df) == 1 &&
                    is.finite(resid_df) && resid_df <= 0

                fit_stats <- list(
                    BIC = model$bic,
                    AIC = model$aic,
                    "Log-Likelihood" = model$llik,
                    "G-squared" = model$Gsq,
                    "Chi-squared" = model$Chisq,
                    "Degrees of Freedom" = resid_df
                )

                table$setNote("just_identified", if (just_identified)
                    .("With three tests there are 0 nominal residual degrees of freedom. This does not guarantee exact fit: the model also imposes constraints on the observed probabilities. G-squared and Chi-squared are descriptive discrepancies, without a chi-squared reference test. Small discrepancies do not verify conditional independence or parameter identifiability."))

                # Rows and their translated labels are built in .init().
                for (name in names(fit_stats)) {
                    if (!is.null(fit_stats[[name]]))
                        table$setRow(rowKey = name, values = list(value = fit_stats[[name]]))
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
                                    # Uncertainty is bootstrapped after likelihood refinement.
                                    calc.se = FALSE,
                                    nrep = 1,
                                    # Use the full-sample solution as the first candidate;
                                    # extra random starts check alternative likelihood modes.
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

                pattern_data <- nogoldstandard_pattern_data(binary_data)
                start_par <- c(best_model$P[1],
                    vapply(best_model$probs, function(x) x[1, 2], numeric(1)),
                    vapply(best_model$probs, function(x) x[2, 2], numeric(1)))
                refined <- nogoldstandard_refine(pattern_data, start_par,
                    checkpoint = function() private$.checkpoint(flush = FALSE))
                best_model <- nogoldstandard_update_lca(best_model, pattern_data, refined)
                identification <- nogoldstandard_identification(pattern_data, refined$par)
                private$.diag(sprintf("Likelihood score: %.3g; identification singular-value ratio %.3g",
                                      refined$score, identification$ratio))

                iterations <- refined$iterations
                # poLCA$eflag means that poLCA automatically restarted after a
                # numerical problem; the returned model is the final successful run.
                # It is not a nonconvergence flag. Convergence is instead determined
                # from the iteration limit and finite fitted output.
                restarted <- isTRUE(best_model$eflag)
                converged <- isTRUE(refined$converged)
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
                orientation <- nogoldstandard_orientation(
                    vapply(best_model$probs, function(x) x[1, 2], numeric(1)),
                    vapply(best_model$probs, function(x) x[2, 2], numeric(1)))
                disease_class <- if (isTRUE(orientation$contrast > 0)) 1L else 2L
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

                return(private$.checkLatentEstimate(list(
                    prevalence = prevalence,
                    sensitivities = sensitivities,
                    specificities = specificities,
                    model = best_model,
                    data = binary_data,
                    disease_class = disease_class,
                    identification = identification,
                    orientation = !orientation$tied,
                    orientation_tied = orientation$tied,
                    converged = converged,
                    restarted = restarted,
                    iterations = iterations,
                    starts_used = starts_used,
                    failed_starts = failed_starts
                )))
            },
            .checkLatentEstimate = function(results) {
                if (isTRUE(results$orientation_tied)) {
                    private$.addNotice("STRONG_WARNING", .("Class orientation is ambiguous"),
                        .("The fitted classes have equal average selected-positive probabilities within a numerical tolerance of 0.000001. There is no unique high-positive class under this naming rule. Class proportion, response probabilities, predictive values and their intervals are left blank. Observed agreement and available model-fit diagnostics are retained. This tolerance is a numerical safeguard, not a statistical test of class separation."))
                }
                identification <- results$identification
                if (!is.null(identification) && !isTRUE(identification$identified)) {
                    private$.addNotice("STRONG_WARNING", .("Latent parameters not estimable"),
                        .("Unique, stable latent estimates could not be established. The fitted probability map is singular, numerically close to singular, or represents a nonregular boundary case. This can indicate a likelihood ridge beyond class-label ambiguity; at a boundary the numerical check alone does not prove nonuniqueness. Latent estimates and intervals are conservatively left blank. Fixed priors do not establish identification by the data; use observed agreement or additional informative tests."))
                } else if (!is.null(identification) && isTRUE(identification$weak)) {
                    private$.addNotice("STRONG_WARNING", .("Weak identification of latent parameters"),
                        .("Small changes in the observed response frequencies can cause large changes in latent estimates. The numerical identification check is close to singular. Interpret estimates and bootstrap intervals cautiously and examine study-specific sensitivity analyses."))
                }
                if (isFALSE(results$converged) || isFALSE(results$orientation) ||
                    (!is.null(identification) && !isTRUE(identification$identified))) {
                    results$prevalence <- NA_real_
                    results$sensitivities[] <- NA_real_
                    results$specificities[] <- NA_real_
                    results$estimable <- FALSE
                } else {
                    results$estimable <- TRUE
                    # WRONG BEFORE: three identical tests passed every check above and were
                    # reported as 100% sensitivity and specificity with a 100-100% bootstrap
                    # interval. The data fit perfect tests and tests sharing one error
                    # equally well, so the identification check cannot catch it; say so.
                    est <- c(results$prevalence, results$sensitivities, results$specificities)
                    at_bound <- sum(est < 1e-6 | est > 1 - 1e-6, na.rm = TRUE)
                    results$boundary <- at_bound > 0
                    if (results$boundary)
                        private$.addNotice("STRONG_WARNING", .("Estimates at the parameter boundary"),
                            .fmt(.("{count} of the {total} latent estimates (class proportion and response probabilities) are at 0% or 100%. Boundary estimates are nonregular: their bootstrap intervals are unreliable and collapse to a single value when every resample stays at the boundary. Tests that agree perfectly fit both perfectly accurate tests and tests that share the same errors, so do not report 0% or 100% sensitivity or specificity from this fit without independent evidence."),
                                 count = at_bound, total = length(est)))
                }
                results
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

                # With no case on one side of the majority rule every metric on that side
                # has a zero denominator. .runAllPositive/.runAnyPositive already return NA
                # there; composite alone printed the literal NaN. Say why once, here.
                n_rule_pos <- sum(composite, na.rm = TRUE)
                n_rule_neg <- sum(!composite, na.rm = TRUE)
                if (n_rule_pos == 0 || n_rule_neg == 0) {
                    private$.addNotice(
                        "STRONG_WARNING",
                        .("Majority rule has an empty group"),
                        if (n_rule_pos == 0)
                            .("No case in the analyzed data meets the strict-majority rule, so there is no rule-positive group. The positive-response probability cannot be computed from an empty denominator and is left blank rather than reported.")
                        else
                            .("Every case in the analyzed data meets the strict-majority rule, so there is no rule-negative group. The negative-response probability cannot be computed from an empty denominator and is left blank rather than reported.")
                    )
                }

                # Calculate metrics for each test
                sensitivities <- numeric(ncol(binary_data))
                specificities <- numeric(ncol(binary_data))

                for (i in seq_along(binary_data)) {
                    test_result <- binary_data[[i]] == 1
                    tp <- sum(test_result & composite, na.rm = TRUE)
                    tn <- sum(!test_result & !composite, na.rm = TRUE)
                    fp <- sum(test_result & !composite, na.rm = TRUE)
                    fn <- sum(!test_result & composite, na.rm = TRUE)

                    # WRONG BEFORE: bare tp/(tp+fn). With an empty rule group this is
                    # 0/0 = NaN, which jamovi prints literally as "NaN". The two sibling
                    # rule estimators already guard the identical expression; match them
                    # and pass NA_real_ so the cell renders empty.
                    sensitivities[i] <- if ((tp + fn) > 0) tp / (tp + fn) else NA_real_
                    specificities[i] <- if ((tn + fp) > 0) tn / (tn + fp) else NA_real_
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
            # start_par: a bootstrap replicate starts from the full-sample solution plus
            # two standard starts instead of the full search (as .runLCA's probs_start).
            .runBayesian = function(binary_data, start_par = NULL) {
                num_tests <- ncol(binary_data)

                # A two-class model over k binary tests has 2k + 1 free parameters and
                # 2^k - 1 degrees of freedom, so k = 2 gives 5 parameters for 3 df: the
                # model is NOT identified and the estimates are determined by the starting
                # values and the prior rather than by the data. latent_class already refuses
                # this case; the EM here accepted it silently and returned numbers.
                if (num_tests < 3) {
                    jmvcore::reject(.("Penalized EM requires at least 3 tests because the two-test latent-class model is not statistically identifiable."))
                }

                # These constants only DESCRIBE the priors for the notice below. The penalty
                # itself is hard-coded in nogoldstandard_refine(penalized = TRUE) as
                # log(sens) + log(spec) per test, i.e. Beta(2,1) on both and a uniform
                # Beta(1,1) on prevalence (no term). Change both places together.
                # Beta(2,1) has density 2x on [0,1]: mean 2/3, monotonically increasing, so
                # it pulls BOTH estimates upward -- in MAP terms one pseudo true positive (class +)
                # and one pseudo true negative (class -) per test, NOT a pseudo-positive result.
                alpha_prev <- 1
                beta_prev <- 1
                alpha_sens <- 2
                beta_sens <- 1

                private$.addNotice(
                    "WARNING",
                    .("Fixed priors used by penalized EM"),
                    sprintf(.("Prevalence uses a uniform Beta(%1$g, %2$g) prior. Sensitivity and specificity each use a Beta(%3$g, %4$g) prior, which has mean %5$.2f and increases toward 1, so it pulls both estimates upward -- in MAP terms it adds one pseudo true-positive and one pseudo true-negative result to every test. With a small sample this prior, not your data, may be driving the numbers below. These are penalised-likelihood (MAP) point estimates from an EM algorithm, not draws from a posterior, so the intervals shown are not credible intervals."),
                            alpha_prev, beta_prev, alpha_sens, beta_sens,
                            alpha_sens / (alpha_sens + beta_sens))
                )

                pattern_data <- nogoldstandard_pattern_data(binary_data)
                checkpoint <- function() private$.checkpoint(flush = FALSE)
                starts <- list(
                    c(.3, rep(.8, num_tests), rep(.1, num_tests)),
                    c(.7, rep(.9, num_tests), rep(.2, num_tests)),
                    c(.5, rep(.6, num_tests), rep(.4, num_tests)),
                    c(.5, rep(.4, num_tests), rep(.6, num_tests))
                )
                # Exchangeable starts can miss modes where a test runs in the
                # opposite direction. Include each test-specific contrast and its mirror.
                for (j in seq_len(num_tests)) {
                    q1 <- rep(.2, num_tests)
                    q0 <- rep(.6, num_tests)
                    q1[j] <- .95
                    q0[j] <- .05
                    starts <- c(starts, list(c(.3, q1, q0), c(.7, q0, q1)))
                }
                # Each replicate ran all 4 + 2k starts (~2.5 min at nboot = 1000, n = 300).
                if (!is.null(start_par)) starts <- c(list(start_par), starts[1:2])
                fits <- lapply(starts, function(start) {
                    initial <- nogoldstandard_em_start(pattern_data, start, checkpoint)
                    nogoldstandard_refine(pattern_data, initial, penalized = TRUE,
                                          checkpoint = checkpoint)
                })
                finished <- which(vapply(fits, function(x) isTRUE(x$converged), logical(1)))
                candidates <- if (length(finished)) finished else seq_along(fits)
                best <- candidates[which.max(vapply(fits[candidates],
                                                    function(x) x$objective, numeric(1)))]
                fit <- fits[[best]]
                par <- fit$par
                sensitivities <- par[1 + seq_len(num_tests)]
                specificities <- 1 - par[1 + num_tests + seq_len(num_tests)]

                # Priors can pick a point on a likelihood ridge. Assess data identification
                # with an unpenalized companion fit, not the curvature of the penalty.
                unpenalized <- nogoldstandard_refine(pattern_data, par, checkpoint = checkpoint)
                identification <- nogoldstandard_identification(pattern_data, unpenalized$par)
                class_orientation <- nogoldstandard_orientation(sensitivities, 1 - specificities)
                orientation <- !class_orientation$tied && class_orientation$contrast > 0
                if (!orientation && !class_orientation$tied) {
                    private$.addNotice("STRONG_WARNING", .("Prior and class orientation conflict"),
                        .("The fixed-prior solution does not assign the higher average selected-positive probability to the positive class. Relabeling would change the asymmetric prior objective. Latent estimates and intervals are left blank; reconsider the selected positive levels or use the unpenalized model."))
                }
                private$.diag(sprintf(
                    "Penalized EM    : %d starts; numerical refinement %s; score %.3g; objective %.10f",
                    length(starts), if (fit$converged) "converged" else "DID NOT CONVERGE",
                    fit$score, fit$objective))
                private$.diag(sprintf("Identification : unpenalized singular-value ratio %.3g",
                                      identification$ratio))
                private$.checkLatentEstimate(list(prevalence = par[1], par = par, sensitivities = sensitivities,
                     specificities = specificities, data = binary_data,
                     converged = fit$converged, iterations = fit$iterations,
                     identification = identification, orientation = orientation,
                     orientation_tied = class_orientation$tied,
                     objective = fit$objective,
                     # Dependence is a property of the model, not the prior: check it on
                     # the unpenalized fit, as identification above does.
                     model = if (isTRUE(unpenalized$converged))
                         nogoldstandard_bvr_model(binary_data, unpenalized$par)))
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
            .bootstrapAll = function(data, method, nboot, warm_start = NULL) {
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
                # The resample indices used to be materialised up front as one n x nboot
                # integer matrix -- 190 MB at n = 5000, nboot = 10000, held for the whole
                # loop on top of the per-replicate data copies, which could exhaust memory
                # before the first fit. Drawing one resample per iteration from the same
                # seeded stream is bit-identical (every estimator scopes its own RNG with
                # withr, so nothing downstream consumes from this stream) and allocates n
                # integers instead of n * nboot. Verified identical for n = 7, nboot = 5.

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
                    boot_data <- data[sample.int(n, n, replace = TRUE), , drop = FALSE]

                    boot_result <- tryCatch({
                        if (method == "latent_class") {
                            # A rare test can be constant in a resample. poLCA would drop it and
                            # the replicate used to fail -- discarding exactly the resamples
                            # where its sensitivity is 0, which pulled the interval off the
                            # boundary. Its MLE is that value in both classes and leaves the other
                            # tests' likelihood unchanged: fit the rest, place it at the boundary.
                            keep <- vapply(boot_data, function(x) length(unique(x)) > 1, logical(1))
                            if (sum(keep) < 3L) {
                                NULL
                            } else {
                                fit <- private$.runLCA(boot_data[keep], names(data)[keep],
                                                       n_starts = 3L,
                                                       probs_start = warm_start[keep])
                                if (!all(keep) && isTRUE(fit$estimable)) {
                                    # (tryCatch evaluates here: never reuse the names sens/spec)
                                    value <- vapply(boot_data[!keep], `[`, numeric(1), 1L)
                                    full_sens <- full_spec <- numeric(n_tests)
                                    full_sens[keep] <- fit$sensitivities
                                    full_spec[keep] <- fit$specificities
                                    full_sens[!keep] <- value
                                    full_spec[!keep] <- 1 - value
                                    fit$sensitivities <- full_sens
                                    fit$specificities <- full_spec
                                }
                                fit
                            }
                        } else if (method == "composite") {
                            private$.runComposite(boot_data)
                        } else if (method == "all_positive") {
                            private$.runAllPositive(boot_data)
                        } else if (method == "any_positive") {
                            private$.runAnyPositive(boot_data)
                        } else if (method == "bayesian") {
                            private$.runBayesian(boot_data, start_par = warm_start)
                        } else {
                            NULL
                        }
                    }, error = function(e) {
                        # WRONG BEFORE: a catch-all handler. private$.checkpoint() signals a
                        # jamovi restart request as an error carrying code = "restart"
                        # (jmvcore::createError("restarting", "restart")), and BOTH .runLCA
                        # and .runBayesian checkpoint inside this region. Every cancelled run
                        # was therefore swallowed, counted as a fitting failure, and the loop
                        # ran on for up to nboot more replicates -- minutes for latent_class.
                        # Re-raise the restart; only genuine fit failures fall through.
                        if (identical(e$code, "restart")) stop(e)
                        NULL
                    })

                    if (is.null(boot_result) || isFALSE(boot_result$converged) ||
                        isFALSE(boot_result$estimable)) {
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
                        .("Some bootstrap replicates failed"),
                        sprintf(.("%1$d of %2$d bootstrap resamples (%3$.0f%%) did not yield a converged, identifiable fit with a unique class orientation and were discarded. At most %4$d fitted resamples remain for each interval. A high failure rate can reflect small or sparse samples, fitting problems, or ambiguous class orientation."),
                                error_count, nboot, 100 * error_count / nboot, nboot - error_count)
                    )
                }

                deg_sens <- identical(method, "all_positive") ||
                    (identical(method, "composite") && n_tests == 2L)
                deg_spec <- identical(method, "any_positive")
                finite_counts <- c(
                    sum(is.finite(prevalence)),
                    if (!deg_sens) colSums(is.finite(sens)) else integer(0),
                    if (!deg_spec) colSums(is.finite(spec)) else integer(0)
                )
                if (any(finite_counts < 20L)) {
                    private$.addNotice("STRONG_WARNING", .("Insufficient bootstrap estimates"),
                        .fmt(.("Each interval requires at least 20 finite parameter estimates. Across the class or rule proportion and test response probabilities, successful counts range from {minimum} to {maximum} of {requested}. Intervals with fewer than 20 are left blank; increase the sample information or resolve fitting failures before reporting uncertainty."),
                             minimum = min(finite_counts), maximum = max(finite_counts),
                             requested = nboot))
                }
                if (method %in% c("latent_class", "bayesian")) {
                    private$.addNotice("WARNING", .("Bootstrap coverage is approximate"),
                        .("Case-resampling percentile intervals assume independent cases. Small samples, boundary estimates and weak identification can give coverage below the stated confidence level. Fixed-prior estimates can also be biased; this bootstrap does not include uncertainty about the chosen priors. Check coverage in a study-specific simulation before confirmatory use."))
                }

                private$.diag(sprintf("Bootstrap       : %d resamples, seed %s, %d failed (%.1f%%)",
                                      nboot, base::format(seed_val), error_count,
                                      100 * error_count / nboot))
                if (!is.null(warm_start))
                    private$.diag("                  replicates start from the full-sample solution plus two additional starts")

                list(prevalence = prevalence, sensitivities = sens, specificities = spec,
                     n_failed = error_count, nboot = nboot)
            },

            # Percentile interval from a cached bootstrap distribution.
            .bootCI = function(values, alpha) {
                values <- values[is.finite(values)]
                if (length(values) < 20) return(list(lower = NA_real_, upper = NA_real_))
                q <- stats::quantile(values, c(alpha / 2, 1 - alpha / 2), names = FALSE, na.rm = TRUE)
                # Every resample gave the same value (in practice 0 or 1): a zero-width
                # percentile interval reads as certainty, so blank it and say why once.
                if (q[2] - q[1] < 1e-9) {
                    private$.degenerateCI <- TRUE
                    return(list(lower = NA_real_, upper = NA_real_))
                }
                list(lower = q[1], upper = q[2])
            },

            .calculatePPVNPV = function(sensitivity, specificity, prevalence) {
                # Calculate Positive Predictive Value (PPV) and Negative Predictive Value (NPV)
                # Using Bayes' theorem

                # PPV = (sensitivity * prevalence) / ((sensitivity * prevalence) + ((1 - specificity) * (1 - prevalence)))
                # A test with no diseased (or no healthy) cases yields NA sensitivity or
                # specificity upstream; ifelse() keeps that NA-safe (a bare `if (NA > 0)`
                # once aborted the whole analysis instead of blanking one cell).
                #
                # Vectorized for bootstrap replicates. On any 2x2 table this identity is exact,
                # so it also gives a rule method's empirical PPV/NPV -- provided an empty group
                # contributes 0: with prevalence 0 sensitivity is NA (no positive group) but
                # PPV is 0 and NPV 1, and NA * 0 would otherwise drop that resample.
                s <- ifelse(sensitivity <= 1e-9, 0, ifelse(sensitivity >= 1 - 1e-9, 1, sensitivity))
                sp <- ifelse(specificity <= 1e-9, 0, ifelse(specificity >= 1 - 1e-9, 1, specificity))
                pr <- ifelse(prevalence <= 1e-9, 0, ifelse(prevalence >= 1 - 1e-9, 1, prevalence))
                tp <- ifelse(pr == 0, 0, s * pr)
                fn <- ifelse(pr == 0, 0, (1 - s) * pr)
                fp <- ifelse(pr == 1, 0, (1 - sp) * (1 - pr))
                tn <- ifelse(pr == 1, 0, sp * (1 - pr))
                ppv <- ifelse(tp + fp > 1e-9, tp / (tp + fp), NA_real_)
                npv <- ifelse(tn + fn > 1e-9, tn / (tn + fn), NA_real_)

                return(list(ppv = ppv, npv = npv))
            },
            # ggtheme/theme default to NULL: the renderer guards on ggtheme, and forcing a
            # missing promise would error rather than fall back.
            .plot_ggplot = function(image, ggtheme = NULL, theme = NULL, ...) {
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

                        # Global themes also carry discrete palette scales. Keep the
                        # theme, but retain this heatmap's continuous fill and text colors.
                        plot_theme <- if (is.null(ggtheme)) ggplot2::theme_minimal() else ggtheme
                        if (is.list(plot_theme) && !inherits(plot_theme, "theme")) {
                            plot_theme <- Filter(function(x) {
                                !(inherits(x, "Scale") &&
                                  any(x$aesthetics %in% c("fill", "colour", "color")))
                            }, plot_theme)
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
                                name = .("Agreement"),
                                option = "viridis",
                                begin = 0,
                                end = 1,
                                limits = c(0, 1),
                                breaks = seq(0, 1, by = 0.2)
                            ) +
                            ggplot2::scale_color_manual(
                                values = c(white = "white", black = "black"), guide = "none") +
                            ggplot2::labs(
                                title = .("Test Agreement Matrix"),
                                x = NULL,
                                y = NULL
                            ) +
                            plot_theme +
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
            .validateClinicalAssumptions = function(data, tests, method, test_levels) {
                n_obs <- nrow(data)
                n_tests <- length(tests)

                # Sample size warnings based on method
                if (method == "latent_class" && n_obs < 100) {
                    private$.addNotice(
                        "STRONG_WARNING",
                        .("LCA Sample Size"),
                        .fmt(
                            .("The latent-class analysis has N = {n}. N < 100 is a general stability warning, not a clinical adequacy threshold; precision also depends on class balance, response patterns, and the number of tests. Inspect bootstrap intervals and convergence, and consider collecting more observations."),
                            n = n_obs
                        )
                    )
                }

                if (method == "bayesian" && n_obs < 50) {
                    private$.addNotice(
                        "STRONG_WARNING",
                        .("Penalized EM Sample Size"),
                        .fmt(
                            .("The penalized-EM analysis has N = {n}. N < 50 is a general stability warning, not a clinical adequacy threshold; fixed priors may have substantial influence. Inspect bootstrap intervals and consider collecting more observations."),
                            n = n_obs
                        )
                    )
                }

                # Check test result distributions.
                #
                # WRONG BEFORE: both warnings were computed from table(data[[test_name]]),
                # the RAW factor distribution. Nothing downstream ever sees those
                # categories: every estimate uses only the dichotomy `var == pos_level`.
                # A three-tier marker (negative / equivocal / positive) with 5 equivocal
                # cases therefore fired "extremely imbalanced: the minority category is
                # 1.3% of observations" on every run, naming a tier the analysis has
                # already merged into 'negative'. Worse, jmvcore::naOmit does not drop
                # unused levels, so a level emptied by complete-case filtering gave a zero
                # count and made both warnings fire unconditionally at "0.0%".
                # Judge the split the model actually sees: positive vs not-positive.
                for (i in seq_along(tests)) {
                    test_name <- tests[[i]]
                    pos_level <- test_levels[[i]]
                    observed_levels <- unique(as.character(data[[test_name]]))
                    observed_levels <- observed_levels[!is.na(observed_levels)]
                    if (length(observed_levels) > 2L) {
                        private$.addNotice("STRONG_WARNING", .("Test categories collapsed"),
                            .fmt(
                                .("Test '{test}' has more than two observed categories. Only '{positive}' is treated as positive; all other observed categories ({pooled}) are pooled as not-positive (negative). This includes equivocal or indeterminate results if present. Recode or exclude such results before analysis if this is not the intended comparison. All estimates and agreement statistics use this split on complete cases."),
                                test = test_name, positive = pos_level,
                                pooled = paste(setdiff(observed_levels, pos_level), collapse = ", ")))
                    }
                    is_pos <- data[[test_name]] == pos_level
                    n_pos <- sum(is_pos, na.rm = TRUE)
                    n_neg <- sum(!is_pos, na.rm = TRUE)
                    n_tot <- n_pos + n_neg
                    if (n_tot == 0) next
                    test_values <- c(n_pos, n_neg)

                    if (any(test_values < 5)) {
                        private$.addNotice(
                            "WARNING",
                            .("Small Test Categories"),
                            .fmt(
                                .("Test '{test}' has fewer than 5 cases on one side of the positive/not-positive split the analysis uses ({npos} positive, {nneg} not positive). Estimates may be unstable."),
                                test = test_name,
                                npos = n_pos,
                                nneg = n_neg
                            )
                        )
                    }

                    # Check for extreme imbalances
                    min_prop <- min(test_values) / n_tot
                    if (min_prop < 0.05) {
                        private$.addNotice(
                            "WARNING",
                            .("Extreme Test Imbalance"),
                            .fmt(
                                .("Test '{test}' is extremely imbalanced on the positive/not-positive split the analysis uses: the smaller side is {percentage}% of observations. This may destabilize parameter estimation."),
                                test = test_name,
                                percentage = base::format(round(min_prop * 100, 1), nsmall = 1)
                            )
                        )
                    }
                }

                # Method-specific warnings (LCA with < 3 tests is rejected in .run())
                if (method == "composite" && n_tests %% 2 == 0) {
                    private$.addNotice(
                        "WARNING",
                        .("Composite ties are classified as rule negative"),
                        .("A strict majority requires more than half of the selected tests to be positive. With an even number of tests, tied cases are therefore rule negative. This is part of the rule definition, not evidence of disease absence; use an odd number of tests or a prespecified alternative if that convention is unsuitable.")
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
                    "<h3 style='color: inherit; margin-top: 0;'> ", .("Method Selection Guide"), "</h3>",
                    "<div style='margin: 15px 0; padding: 15px; background-color: rgba(33, 159, 33, 0.1); border-radius: 5px; color: inherit;'>",
                    "<p><strong>", .("Scope warning:"), "</strong> ", .("The scenarios and method examples below are illustrative examples only. They are not clinical guides, validated recommendations, or substitutes for a study-specific statistical analysis plan."), "</p>",
                    "<h4 style='color: inherit; margin-top: 0;'> ", .("Latent Class Analysis"), "</h4>",
                    "<p><strong>", .("Description"), ":</strong> ", .("A two-class conditional-independence mixture model for three or more tests."), "</p>",
                    "<p><strong>", .("Illustrative context"), ":</strong> ", .("Studies with at least three tests whose errors are plausibly independent within each latent class."), "</p>",
                    "<p><strong>", .("Limitations"), ":</strong> ", .("Class labels are unidentified; the high-positive class is not automatically disease. The model assumes conditionally independent errors and does NOT model conditional dependence; with three tests the nominal residual degrees of freedom are zero, but probability constraints can still prevent an exact fit. Fit discrepancies remain informative; a conventional chi-squared reference distribution is not justified."), "</p>",
                    "</div>",
                    "<div style='margin: 15px 0; padding: 15px; background-color: rgba(33, 152, 239, 0.13); border-radius: 5px; color: inherit;'>",
                    "<h4 style='color: inherit; margin-top: 0;'> ", .("Penalized EM (MAP-like; Fixed Priors)"), "</h4>",
                    "<p><strong>", .("Description"), ":</strong> ", .("A latent-class EM point estimator using fixed Beta priors."), "</p>",
                    "<p><strong>", .("Illustrative context"), ":</strong> ", .("Sensitivity analysis showing how the fixed penalties affect a conditional-independence fit."), "</p>",
                    "<p><strong>", .("Limitations"), ":</strong> ", .("This is not posterior sampling, priors are not configurable, intervals are not credible intervals, and latent-class labels remain unidentified."), "</p>",
                    "</div>",
                    "<div style='margin: 15px 0; padding: 15px; background-color: rgba(255, 169, 33, 0.14); border-radius: 5px; color: inherit;'>",
                    "<h4 style='color: inherit; margin-top: 0;'> ", .("Composite Reference"), "</h4>",
                    "<p><strong>", .("Description"), ":</strong> ", .("Uses majority vote of available tests as pseudo-gold standard."), "</p>",
                    "<p><strong>", .("Illustrative context"), ":</strong> ", .("Exploratory description of agreement with a majority-vote rule using three or more tests."), "</p>",
                    "<p><strong>", .("Limitations"), ":</strong> ", .("This is not an accuracy estimate: each test helps build the rule against which it is evaluated, creating incorporation bias. Strict majority means more than half positive; with an even number of tests, tied cases are rule negative. With two tests this is identical to the all-tests-positive rule."), "</p>",
                    "</div>",
                    "<div style='margin: 15px 0; padding: 15px; background-color: rgba(230, 33, 99, 0.12); border-radius: 5px; color: inherit;'>",
                    "<h4 style='color: inherit; margin-top: 0;'> ", .("All Tests Positive"), "</h4>",
                    "<p><strong>", .("Description"), ":</strong> ", .("Defines a case as rule positive only when every selected test is positive."), "</p>",
                    "<p><strong>", .("Illustrative context"), ":</strong> ", .("Exploring agreement with the strict rule that every selected test is positive."), "</p>",
                    "<p><strong>", .("Limitations"), ":</strong> ", .("Each test is part of the rule against which it is evaluated, creating incorporation bias. The positive-response probability and NPV are fixed at 100% by construction and are left blank; the other values describe agreement with the rule, not diagnostic accuracy."), "</p>",
                    "</div>",
                    "<div style='margin: 15px 0; padding: 15px; background-color: rgba(33, 159, 33, 0.1); border-radius: 5px; color: inherit;'>",
                    "<h4 style='color: inherit; margin-top: 0;'> ", .("Any Test Positive"), "</h4>",
                    "<p><strong>", .("Description"), ":</strong> ", .("Defines a case as rule positive when at least one selected test is positive."), "</p>",
                    "<p><strong>", .("Illustrative context"), ":</strong> ", .("Exploring agreement with the permissive rule that at least one selected test is positive."), "</p>",
                    "<p><strong>", .("Limitations"), ":</strong> ", .("Each test is part of the rule against which it is evaluated, creating incorporation bias. The negative-response probability and PPV are fixed at 100% by construction and are left blank; the other values describe agreement with the rule, not diagnostic accuracy."), "</p>",
                    "</div>",
                    "<div style='margin: 15px 0; padding: 10px; background-color: rgba(255, 203, 33, 0.14); border-radius: 5px; border-left: 3px solid #ffb300; color: inherit;'>",
                    "<h4 style='color: inherit; margin-top: 0;'> ", .("Interpretation Cautions"), "</h4>",
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
                        # WRONG BEFORE: spliced the raw enum keys, and used two unordered
                        # %s that a translator could not reorder. Named placeholders can be
                        # reordered; the values are now the drop-down labels.
                        diffs <- c(diffs, .fmt(.("Analysis method: currently \"{current}\", example \"{example}\""),
                                               current = private$.methodLabel(cur$method),
                                               example = private$.methodLabel(preset_config$method)))
                    if (!identical(cur$bootstrap, isTRUE(preset_config$bootstrap)))
                        # The branch is only reached when the two differ, so two complete
                        # sentences replace the on/off fragments spliced through %s.
                        diffs <- c(diffs, if (cur$bootstrap)
                                       .("Bootstrap confidence intervals: currently on, example off")
                                   else
                                       .("Bootstrap confidence intervals: currently off, example on"))
                    if (!isTRUE(all.equal(cur$nboot, preset_config$nboot)))
                        # two unordered conversions -> named placeholders
                        diffs <- c(diffs, .fmt(.("Bootstrap samples: currently {current}, example {example}"),
                                               current = base::format(cur$nboot),
                                               example = base::format(preset_config$nboot)))

                    private$.addNotice(
                        if (length(diffs) > 0) "WARNING" else "INFO",
                        # was gsub("_", " ", preset), i.e. the internal key
                        .fmt(.("Illustrative example: {preset}"),
                             preset = private$.presetLabel(preset)),
                        if (length(diffs) > 0)
                            .fmt(.("Example only -- not clinical guidance. {description} {guidance} This example does not change settings. For comparison, its example settings differ as follows: {diffs}."),
                                 description = preset_config$description,
                                 guidance = preset_config$guidance,
                                 diffs = paste(diffs, collapse = "; "))
                        else
                            .fmt(.("Example only -- not clinical guidance. {description} {guidance} Current settings happen to match this illustrative example."),
                                 description = preset_config$description,
                                 guidance = preset_config$guidance)
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
                    "<h4 style='color: inherit; margin-top: 0;'> ", .("Plain-Language Summary"), "</h4>",
                    "<p><strong>", .("Analysis:"), "</strong> ", method_label, "</p>",
                    "<p><strong>", .("Tests analyzed:"), "</strong> ", paste(jmvcore::htmlEscape(unlist(tests)), collapse = ", "), "</p>",
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
                    if (isTRUE(results$boundary))
                        paste0("<p><strong>", .("Caution:"), "</strong> ",
                               .("at least one estimate is at 0% or 100%. Perfect agreement between tests does not show that the tests are perfectly accurate; read the boundary warning before reporting these values."), "</p>")
                    else "",
                    "</div>"
                )

                return(summary_html)
            },
            .populateAgreementStats = function(test_data, tests, test_levels) {
                # Calculate pairwise Cohen's Kappa; rows are built in .init()
                table <- self$results$agreement_stats
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
                    # One constant test: kappa is exactly 0 and the null variance is 0, so the
                    # z-test is undefined (irr::kappa2 returns 0/0 with "NaNs produced").
                    if (any(row_sums == 0) || any(col_sums == 0))
                        return(list(kappa = kappa, p_value = NA_real_, agreement = p_o))

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
                        !is.finite(kappa_test$statistic)) {
                        NA_real_
                    } else {
                        2 * stats::pnorm(-abs(kappa_test$statistic))
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

                        table$setRow(rowKey = paste0("pair_", i, "_", j), values = list(
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
                # Explains the blank p-value calculate_kappa() returns for a constant test.
                constant <- vapply(seq_len(n_tests), function(i)
                    length(unique(test_data[[tests[[i]]]] == test_levels[[i]])) < 2, logical(1))
                table$setNote("kappa_constant", if (any(constant))
                    .("A test with only one observed result has kappa 0 with every test that varies; the test of kappa = 0 is then undefined, so its p-value is left blank."))
            }
        )
    )
}
