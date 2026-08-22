#' @title Summary of Continuous Variables with Distribution Diagnostics
#' @return Text and an HTML summary table (with optional distribution diagnostics)
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom magrittr %>%
#' @importFrom gt gt tab_header fmt_number cols_label md cell_fill cells_column_labels cell_text tab_style opt_stylize tab_options
#' @importFrom gtExtras gt_plt_summary
#' @importFrom htmltools HTML
#' @importFrom moments kurtosis skewness
#' @importFrom utils packageVersion
#' @noRd
NULL

summarydataClass <- if (requireNamespace("jmvcore")) R6::R6Class("summarydataClass",
    inherit = summarydataBase, private = list(

        # Per-run cache of Shapiro-Wilk results keyed by variable name
        .shapiroCache = NULL,

        # Notice collection helpers. A single Preformatted (plain-text) output
        # item: avoids BOTH the jmvcore::Notice serialization error raised by
        # self$results$insert(999, Notice) AND any HTML in notices (project
        # convention: notice content is plain text). Data-quality problems such
        # as a dropped variable belong here, not in a box titled "To Do".
        .noticeList = list(),

        .addNotice = function(type, title, content) {
            duplicate <- vapply(private$.noticeList, function(notice) {
                identical(notice$type, type) &&
                    identical(notice$title, title) &&
                    identical(notice$content, content)
            }, logical(1))
            if (any(duplicate))
                return()

            private$.noticeList[[length(private$.noticeList) + 1]] <- list(
                type = type, title = title, content = content)
            # Render immediately so an early-return path still shows the notice.
            private$.renderNotices()
        },

        .renderNotices = function() {
            if (length(private$.noticeList) == 0) {
                self$results$notices$setContent("")
                return()
            }
            blocks <- vapply(private$.noticeList, function(notice) {
                prefix <- switch(notice$type,
                    ERROR          = "ERROR: ",
                    STRONG_WARNING = "WARNING: ",
                    WARNING        = "WARNING: ",
                    INFO           = "NOTE: ",
                    "")
                paste0(prefix, notice$title, "\n", notice$content)
            }, character(1))
            self$results$notices$setContent(paste(blocks, collapse = "\n\n"))
        },

        # Blank every result item whose content depends on the variable
        # selection. jamovi does not reset result items between runs, so without
        # this an early return leaves the previous selection's mean/SD line on
        # screen next to a variable the user has already deselected.
        .clearComputedOutputs = function() {
            self$results$text$setContent("")
            self$results$text1$setContent("")
            self$results$clinicalInterpretation$setContent("")
            self$results$outlierReport$setContent("")
            self$results$reportSentences$setContent("")
        },

        .run = function() {
        # Reset per-run state (the R6 instance persists across runs).
        private$.shapiroCache <- list()
        private$.noticeList <- list()
        private$.renderNotices()
        # Check if variables have been selected. If not, display a welcoming message with instructions.
        if (length(self$options$vars) == 0) {
            intro_msg <- "
          <h3>Welcome to ClinicoPath Descriptives!</h3>
          <p>This tool helps you generate descriptive statistics for your numeric variables.
          Please select one or more continuous variables from the options panel.</p>
          <p>If you want to inspect distribution characteristics, enable the 'Distribution Diagnostics' option.</p>"
            self$results$todo$setContent(intro_msg)
            # Both of these are `visible: true` in the .r.yaml, so before any
            # variable is chosen the user saw two empty boxes titled "About This
            # Analysis" and "Statistical Glossary". Their content is static, and
            # the welcome state is exactly when it is most useful.
            self$results$aboutAnalysis$setContent(private$.generateAboutContent())
            self$results$glossary$setContent(private$.generateGlossary())
            private$.clearComputedOutputs()
            return()
        } else {
            # Clear any introductory message if variables are selected.
            self$results$todo$setContent("")
            # The condition is zero ROWS, not zero complete rows - the two send
            # a user looking in completely different places, and the usual cause
            # of an empty frame here is an over-restrictive row filter.
            if (nrow(self$data) == 0) {
                jmvcore::reject(.("The dataset has no rows. If a row filter is active, check that it does not exclude every case."))
            }
            
            vars <- self$options$vars

            # Remove non-numeric variables and variables with all NAs
            vars_to_remove <- c()
            warning_msgs <- c()

            # Everything downstream of this loop assumes var_list holds numeric
            # columns with at least one non-missing value. Do not relax it.
            for (var in vars) {
                if (!is.numeric(self$data[[var]])) {
                    vars_to_remove <- c(vars_to_remove, var)
                    warning_msgs <- c(warning_msgs, paste0("Variable '", htmltools::htmlEscape(var), "' is not numeric"))
                    private$.addNotice("STRONG_WARNING", .("Variable excluded"),
                        jmvcore::format(
                            .("{variable} was excluded from the summary because it is not a numeric column. Change its measure type to continuous, or remove it from the Variables list."),
                            variable = var))
                } else if (all(is.na(self$data[[var]]))) {
                    vars_to_remove <- c(vars_to_remove, var)
                    warning_msgs <- c(warning_msgs, paste0("Variable '", htmltools::htmlEscape(var), "' contains only missing values"))
                    private$.addNotice("STRONG_WARNING", .("Variable excluded"),
                        jmvcore::format(
                            .("{variable} was excluded from the summary because every value in it is missing. Check the import and any active row filter for this column."),
                            variable = var))
                }
            }

            if (length(warning_msgs) > 0) {
                self$results$todo$setContent(paste0("<div style='color: inherit; background-color: rgba(255, 202, 33, 0.23); padding: 10px; border-radius: 4px;'>",
                    paste(warning_msgs, collapse="<br>"),
                    "</div>"))
            }

            vars <- setdiff(vars, vars_to_remove)

            if (length(vars) == 0) {
                # Same reason as the welcome branch: aboutAnalysis and glossary
                # are `visible: true`, so leaving them unset shows two empty
                # titled boxes, and the numbers from the previous selection would
                # otherwise stay on screen with nothing marking them stale.
                self$results$aboutAnalysis$setContent(private$.generateAboutContent())
                self$results$glossary$setContent(private$.generateGlossary())
                private$.clearComputedOutputs()
                return()
            }

            # Retrieve the data and construct the list of variables.
            dataset <- self$data
            # Use the filtered variable names directly as data[[]] keys. Round-tripping
            # through constructFormula/decomposeFormula can mangle names containing
            # spaces or special characters, breaking downstream dataset[[var]] lookups.
            var_list <- vars
            # mysummary function with optimized calculations
            mysummary <- function(myvar) {
                # Shapiro-Wilk runs per variable at up to n = 5000; yield here so
                # jamovi can process option changes and cancellations.
                private$.checkpoint()
                # var_list is numeric-only by construction (the is.numeric filter
                # above). jmvcore::toNumeric() is a no-op on character and factor
                # input, so it never was the guard it looked like; as.numeric()
                # matches what .gtExtras_style_fallback() and
                # .generateReportSentences() already do with the same columns.
                numeric_data <- as.numeric(dataset[[myvar]])

                # Sample size and missingness first, so the wording below can key
                # off the actual observation count rather than off a formatted NA.
                n_x <- sum(!is.na(numeric_data))
                missing_x <- sum(is.na(numeric_data))

                # Calculate all statistics at once with specified decimal places
                decimal_places <- self$options$decimal_places
                
                mean_x <- private$.fmtNum(mean(numeric_data, na.rm = TRUE), decimal_places)
                sd_x <- private$.fmtNum(sd(numeric_data, na.rm = TRUE), decimal_places)
                median_x <- private$.fmtNum(median(numeric_data, na.rm = TRUE), decimal_places)
                min_x <- private$.fmtNum(min(numeric_data, na.rm = TRUE), decimal_places)
                max_x <- private$.fmtNum(max(numeric_data, na.rm = TRUE), decimal_places)
                dist_text <- ""
                # If the distribution diagnostics option is enabled, add additional tests.
                if (self$options$distr) {
                    # Shapiro-Wilk test (only valid if 3 <= sample size <= 5000)
                    # Use already cached numeric_data
                    valid_data <- na.omit(numeric_data)

                    # Initialize variables
                    p_val <- NA
                    distribution_assessment <- NULL

                    # Check if data has variance (not all values identical)
                    n_unique <- length(unique(valid_data))

                    if (n_unique == 1) {
                        # All values are identical - no variance
                        distribution_assessment <- .("The data are constant and have no variance.")
                    } else {
                        # Shapiro-Wilk test (cached per run, shared with report sentences).
                        # Returns NULL when n is outside the valid 3-5000 range.
                        sw_test <- private$.shapiroResult(valid_data, key = myvar)
                        if (!is.null(sw_test)) {
                            # Keep the exact p for the verdict; round only to display.
                            p_val <- sw_test$p.value
                            verdict <- private$.normalityVerdict(p_val)
                            distribution_assessment <- verdict$long
                        }
                    }

                    if (is.null(distribution_assessment)) {
                        # .shapiroResult() returns NULL for two different reasons:
                        # n outside the valid 3-5000 range, or the test itself
                        # erroring. Blaming the sample size when n is in range
                        # sends the user hunting for the wrong problem, so name
                        # only the cause that actually applies.
                        n_valid <- length(valid_data)
                        distribution_assessment <- if (n_valid < 3 || n_valid > 5000)
                            jmvcore::format(
                                .("Normality was not assessed: the Shapiro-Wilk test needs between 3 and 5000 non-missing observations, and this variable has {n}. Inspect the distribution visually instead."),
                                n = n_valid)
                        else
                            .("Normality was not assessed: the Shapiro-Wilk test could not be computed for this variable. Inspect the distribution visually instead.")
                    }

                    # Skewness and kurtosis were hard-coded to 2 decimals, so a
                    # user who set "Decimal places" to 4 read "Mean 0.2574 +/-
                    # 2.1460 ... skewness = 1.24" - two precisions in one line.
                    skew_val <- private$.fmtNum(moments::skewness(numeric_data, na.rm = TRUE))
                    kurt_val <- private$.fmtNum(moments::kurtosis(numeric_data, na.rm = TRUE))

                    if (is.na(p_val)) {
                        # Normality was not assessed (constant data or n outside 3-5000);
                        # show only the assessment, not NA/NaN diagnostics.
                        dist_text <- private$.fmtVar(
                            .("<br><em>Distribution diagnostics for {variable}:</em> {assessment}"),
                            myvar,
                            assessment = distribution_assessment
                        )
                    } else if (p_val < 0.001) {
                        # .fmtP() carries the relation for a p below the reporting
                        # bound, so a template that also supplies "=" rendered
                        # "p-value = < 0.001". Below the bound the relation belongs
                        # in the sentence, not in the value.
                        dist_text <- private$.fmtVar(
                            .("<br><em>Distribution diagnostics for {variable}:</em> Shapiro-Wilk p-value &lt; 0.001; skewness = {skewness}; kurtosis = {kurtosis}. {assessment}"),
                            myvar,
                            skewness = skew_val,
                            kurtosis = kurt_val,
                            assessment = distribution_assessment
                        )
                    } else {
                        dist_text <- private$.fmtVar(
                            .("<br><em>Distribution diagnostics for {variable}:</em> Shapiro-Wilk p-value = {p}; skewness = {skewness}; kurtosis = {kurtosis}. {assessment}"),
                            myvar,
                            p = private$.fmtP(p_val),
                            skewness = skew_val,
                            kurtosis = kurt_val,
                            assessment = distribution_assessment
                        )
                    }
                }
                # A single observation has no standard deviation; printing
                # "Mean 5.00 \u{00B1} NA" reads as a broken calculation rather than
                # as an undefined quantity. Branch on the observation count, never
                # on the formatted SD: .fmtNum() maps every non-finite value to NA,
                # so an SD of NaN would be read back as "exactly one observation".
                # (jmvcore's OptionVariables rejects columns holding Inf before
                # .run() is reached, so today only n = 1 gets here - but the count
                # is the thing actually being asserted, so test the count.)
                if (n_x < 2) {
                    summary_text <- private$.fmtVar(
                        .("<strong>{variable}</strong> (N = {n}, missing = {missing}): Mean {mean} (SD not defined for a single observation). Median: {median} (minimum: {minimum}; maximum: {maximum})."),
                        myvar,
                        n = n_x, missing = missing_x, mean = mean_x,
                        median = median_x, minimum = min_x, maximum = max_x)
                    return(paste0(summary_text, dist_text, "<br><br>"))
                }
                # A bare "+/-" is ambiguous between SD and SEM, which differ by
                # sqrt(n); the single-observation branch above already names SD, so
                # the two branches also disagreed with each other.
                summary_text <- private$.fmtVar(
                    .("<strong>{variable}</strong> (N = {n}, missing = {missing}): Mean {mean} (SD {sd}). Median: {median} (minimum: {minimum}; maximum: {maximum})."),
                    myvar,
                    n = n_x,
                    missing = missing_x,
                    mean = mean_x,
                    sd = sd_x,
                    median = median_x,
                    minimum = min_x,
                    maximum = max_x
                )
                paste0(summary_text, dist_text, "<br><br>")
            }
            results <- purrr::map(.x = var_list, .f = mysummary)
            # Collapse the per-variable summary strings into a single HTML string.
            # setContent() on an Html result item writes to a non-repeated protobuf
            # field, so a length > 1 vector (multiple variables) triggers a
            # serialization error. Each element already ends with <br><br>.
            results <- paste(unlist(results), collapse = "")
            self$results$text$setContent(results)
            # CORRECT IMPLEMENTATION: Use gtExtras as intended by the package
            plot_dataset <- tryCatch({
                # var_list is already numeric-only and non-empty (filtered above),
                # so the old `numeric_vars <- var_list[sapply(..., is.numeric)]`
                # re-filter and its empty branch could never do anything.
                clean_data <- as.data.frame(dataset[var_list], stringsAsFactors = FALSE)

                # Ensure proper data types and remove any attributes that might interfere
                clean_data <- as.data.frame(lapply(clean_data, function(x) {
                    x <- if (is.factor(x)) as.numeric(as.character(x)) else as.numeric(x)
                    # Remove any attributes that might cause issues
                    attributes(x) <- NULL
                    x
                }), stringsAsFactors = FALSE)

                # Restore column names
                names(clean_data) <- var_list

                # Use gtExtras with default styling as intended
                private$.checkpoint()
                summary_table <- clean_data %>%
                    gtExtras::gt_plt_summary()

                # Convert to HTML with improved compatibility
                html_result <- tryCatch({
                    # Primary method: as_raw_html for clean HTML output
                    as.character(gt::as_raw_html(summary_table))
                }, error = function(e2) {
                    # Fallback: Direct table conversion
                    tryCatch({
                        as.character(summary_table)
                    }, error = function(e3) {
                        # Final fallback: use custom method
                        as.character(private$.gtExtras_style_fallback(dataset, var_list))
                    })
                })

                htmltools::HTML(html_result)
            }, error = function(e) {
                # If gtExtras fails, use the comprehensive fallback without error message
                # This is a design choice to avoid alarming users when the fallback works perfectly
                simple_table <- private$.gtExtras_style_fallback(dataset, var_list)
                htmltools::HTML(as.character(simple_table))
            })
            
            
            self$results$text1$setContent(plot_dataset)
            
            # Generate clinical interpretation content
            clinical_interpretation <- private$.generateClinicalInterpretation(var_list, dataset)
            self$results$clinicalInterpretation$setContent(clinical_interpretation)
            
            # Generate about analysis content
            about_content <- private$.generateAboutContent()
            self$results$aboutAnalysis$setContent(about_content)
            
            # Generate outlier detection report if enabled
            if (self$options$outliers) {
                outlier_report <- private$.generateOutlierReport(var_list, dataset)
                self$results$outlierReport$setContent(outlier_report)
            }
            
            # Generate report sentences if enabled
            if (self$options$report_sentences) {
                report_sentences <- private$.generateReportSentences(var_list, dataset)
                self$results$reportSentences$setContent(report_sentences)
            }
            
            # Generate statistical glossary
            glossary_content <- private$.generateGlossary()
            self$results$glossary$setContent(glossary_content)
        }
        },
        # Compute (and cache per run) the Shapiro-Wilk test for a numeric vector.
        # Returns the htest object, or NULL when the test is not applicable
        # (n outside 3-5000, constant data, or an error). Shared by the diagnostics
        # text and the report sentences so the test runs at most once per variable.
        # Format a statistic at the user's chosen precision. round() alone is not
        # enough for display: round(2.1460, 4) prints as "2.146", so a line could
        # read "Mean 0.2574 +/- 2.146" with two different precisions in it. Fixed
        # notation keeps every number on a line comparable.
        .fmtNum = function(x, dp = NULL) {
            if (is.null(dp)) dp <- self$options$decimal_places
            if (length(x) == 0) return(character(0))
            out <- formatC(as.numeric(x), format = "f", digits = dp)
            out[!is.finite(as.numeric(x))] <- NA_character_
            trimws(out)
        },

        # Substitute a column name into a translatable template.
        #
        # jmvcore::format() re-scans the string after EVERY substitution, so any
        # "{word}" inside a substituted value is picked up as a further
        # placeholder. Column names are user data: a column named "{n}" printed
        # the value of n in place of its own name, and "Ki67 {IHC}" printed
        # "Ki67 " plus an ellipsis (an unmatched placeholder). Passing a sentinel
        # through format() and putting the name in afterwards keeps the name
        # opaque. sub(fixed = TRUE) uses the replacement literally, so a name
        # containing "\\" or "&" survives intact.
        .fmtVar = function(template, var, ...) {
            out <- jmvcore::format(template, variable = "\001", ...)
            sub("\001", htmltools::htmlEscape(var), out, fixed = TRUE)
        },

        # A p-value is never zero. round(2.8e-12, 3) is 0, which printed as
        # "Shapiro-Wilk p-value = 0" - and that went into a copy-ready manuscript
        # sentence. Report the conventional bound instead.
        # Both call sites write into Html result items, so the "<" is emitted as an
        # entity. (A literal "< 0.001" does in fact render: an HTML tokenizer only
        # opens a tag when "<" is followed by a letter, "!", "/" or "?", so "< " is
        # kept as text - verified with an HTML parser. The entity is still the
        # correct thing to write, but it is robustness, not a rendering fix.)
        .fmtP = function(p) {
            if (length(p) != 1 || is.na(p)) return(NA_character_)
            if (p < 0.001) return("&lt; 0.001")
            formatC(p, format = "f", digits = 3)
        },

        # Single source of truth for the normality verdict.
        #
        # The diagnostics text used to test the ROUNDED p (round(p, 3) > 0.05) and
        # the copy-ready sentence the UNROUNDED one, so a variable with p = 0.0501
        # was declared "not consistent with a normal distribution" in one panel and
        # "showed normal distribution" in the other, both printing "p = 0.05".
        # Comparison is on the exact p; only the display is rounded.
        #
        # The wording is deliberately asymmetric: failing to reject H0 is not
        # evidence of normality, so the non-significant branch does not claim it.
        .normalityVerdict = function(p) {
            if (length(p) != 1 || is.na(p)) return(NULL)
            if (p > 0.05)
                list(normal = TRUE,
                     long = .("The data are consistent with a normal distribution (the test did not detect a departure, which is not the same as establishing normality)."),
                     short = .("no evidence of departure from normality"))
            else
                list(normal = FALSE,
                     long = .("The data are not consistent with a normal distribution; inspect the distribution visually and use appropriate tests."),
                     short = .("evidence of departure from normality"))
        },

        .shapiroResult = function(x, key = NULL) {
            if (is.null(private$.shapiroCache))
                private$.shapiroCache <- list()
            if (!is.null(key) && !is.null(private$.shapiroCache[[key]]))
                return(private$.shapiroCache[[key]]$value)
            valid <- x[!is.na(x)]
            result <- NULL
            if (length(valid) >= 3 && length(valid) <= 5000 && length(unique(valid)) > 1)
                result <- tryCatch(shapiro.test(valid), error = function(e) NULL)
            if (!is.null(key))
                private$.shapiroCache[[key]] <- list(value = result)
            result
        },
        # Fallback with gtExtras-style appearance
        .gtExtras_style_fallback = function(dataset, var_list) {
            # var_list arrives already filtered to numeric columns by .run(); the
            # single guard here asserts that invariant instead of silently
            # re-deriving it (the old re-filter and its empty branch never fired).
            if (length(var_list) == 0)
                return(htmltools::HTML(""))
            numeric_vars <- var_list

            # Match the text summary's precision (see decimal_places option)
            dp <- self$options$decimal_places

            # Calculate comprehensive summary statistics using vectorized operations
            calc_stats <- function(x) {
                x <- as.numeric(x)
                x_clean <- x[!is.na(x)]
                c(
                    n = length(x_clean),
                    missing = sum(is.na(x)),
                    mean = mean(x_clean, na.rm = TRUE),
                    sd = sd(x_clean, na.rm = TRUE),
                    min = min(x_clean, na.rm = TRUE),
                    q25 = unname(quantile(x_clean, 0.25, na.rm = TRUE)),
                    median = median(x_clean, na.rm = TRUE),
                    q75 = unname(quantile(x_clean, 0.75, na.rm = TRUE)),
                    max = max(x_clean, na.rm = TRUE)
                )
            }
            
            # vapply with FUN.VALUE = numeric(9) always returns a 9 x k matrix,
            # k >= 1, so the old `if (!is.matrix(stats_matrix))` reshape was
            # unreachable and only made this path look defensive.
            stats_matrix <- vapply(dataset[numeric_vars], calc_stats, numeric(9))

            summary_stats <- data.frame(
                Variable = numeric_vars,
                Type = rep("numeric", length(numeric_vars)),
                N = round(stats_matrix["n", ]),
                Missing = round(stats_matrix["missing", ]),
                Mean = round(stats_matrix["mean", ], dp),
                SD = round(stats_matrix["sd", ], dp),
                Min = round(stats_matrix["min", ], dp),
                Q25 = round(stats_matrix["q25", ], dp),
                Median = round(stats_matrix["median", ], dp),
                Q75 = round(stats_matrix["q75", ], dp),
                Max = round(stats_matrix["max", ], dp),
                stringsAsFactors = FALSE
            )
            
            # Create gtExtras-style table
            gt_table <- summary_stats %>%
                gt::gt() %>%
                gt::tab_header(
                    title = gt::md(paste0("**", .("Dataset Summary"), "**")),
                    subtitle = gt::md(paste0("*", .("Comprehensive statistics for numeric variables"), "*"))
                ) %>%
                gt::fmt_number(
                    columns = c("Mean", "SD", "Min", "Q25", "Median", "Q75", "Max"),
                    decimals = dp
                ) %>%
                gt::cols_label(
                    Variable = .("Variable"),
                    Type = .("Type"),
                    N = .("N"),
                    Missing = .("Missing"),
                    Mean = .("Mean"),
                    SD = .("SD"),
                    Min = .("Min"),
                    Q25 = .("Q25"),
                    Median = .("Median"),
                    Q75 = .("Q75"),
                    Max = .("Max")
                ) %>%
                gt::tab_style(
                    style = gt::cell_fill(color = "#f8f9fa"),
                    locations = gt::cells_column_labels()
                ) %>%
                gt::tab_style(
                    style = gt::cell_text(weight = "bold"),
                    locations = gt::cells_column_labels()
                ) %>%
                gt::opt_stylize(style = 6, color = "blue") %>%
                gt::tab_options(
                    table.font.size = 12,
                    heading.title.font.size = 16,
                    heading.subtitle.font.size = 12
                )
            
            # Convert to HTML
            return(htmltools::HTML(as.character(gt::as_raw_html(gt_table))))
        },

        # Generate clinical interpretation for continuous variables
        .generateClinicalInterpretation = function(variables, dataset) {
            if (length(variables) == 0) return("")
            
            # Calculate comprehensive clinical metrics
            n_vars <- length(variables)
            total_obs <- nrow(dataset)
            
            # Assess data completeness
            completeness_info <- sapply(variables, function(var) {
                var_data <- dataset[[var]]
                round(sum(is.na(var_data)) / length(var_data) * 100, 1)
            })

            avg_missing <- round(mean(completeness_info), 1)
            # The mean across variables hides the variable that matters: four
            # complete variables plus one biomarker at 24% missing averages to
            # 4.8%, which used to print "Data completeness is excellent." and
            # never warn. The verdict now follows the worst variable; the average
            # is still reported, but it no longer decides anything.
            worst_index <- which.max(completeness_info)
            worst_missing <- unname(completeness_info[worst_index])
            worst_var <- variables[[worst_index]]
            
            # Generate clinical context
            clinical_text <- paste0(
                "<div style='padding: 15px; background-color: rgba(33, 159, 33, 0.1); border-left: 4px solid #4caf50; margin: 10px 0; border-radius: 4px; color: inherit;'>",
                "<h4 style='margin-top: 0; color: inherit;'>", .("Clinical Interpretation Guide"), "</h4>",
                "<p><strong>", .("Dataset Overview"), ":</strong> ",
                # "patient records" was an unearned claim: nothing here establishes
                # that a row is a patient, and in pathology series a row is very
                # often a block, a core or a repeated measurement.
                jmvcore::format(
                    .("Analysis of {nvars} continuous variable(s) from {nobs} records"),
                    nvars = n_vars, nobs = total_obs), "</p>",

                "<p><strong>", .("Data Quality Assessment"), ":</strong></p>",
                "<ul style='margin: 5px 0 10px 20px;'>",
                "<li>", jmvcore::format(.("Average missing data: {pct}%"), pct = avg_missing), "</li>",
                if (n_vars > 1) paste0("<li>", private$.fmtVar(
                    .("Most incomplete variable: {variable}, {pct}% missing"),
                    worst_var, pct = worst_missing), "</li>") else "",
                # Fixed dark hex foregrounds (#d32f2f, #388e3c) fell below 4.5:1 on
                # jamovi's dark theme - and these are the data-quality lines. Weight
                # carries the emphasis instead, and the colour follows the theme.
                if (worst_missing > 20) paste0("<li style='color: inherit; font-weight: 600;'>", .("A high missing-data rate may affect interpretation."), "</li>") else "",
                if (worst_missing <= 5) paste0("<li style='color: inherit; font-weight: 600;'>", .("Data completeness is excellent."), "</li>") else "",
                "</ul>",
                
                "<p><strong>", .("Clinical Applications"), ":</strong></p>",
                "<ul style='margin: 5px 0 10px 20px;'>",
                "<li>", .("Biomarker distribution assessment"), "</li>",
                "<li>", .("Reference range validation"), "</li>",
                "<li>", .("Quality control and outlier detection"), "</li>",
                "<li>", .("Statistical assumption verification"), "</li>",
                "</ul>",
                
                # Backstop only: jmvcore's OptionVariables carries rejectInf = TRUE,
                # so a column holding Inf is rejected before .run() is reached.
                if (any(sapply(dataset[variables], function(x) any(is.infinite(x)))))
                    paste0("<p style='color: inherit; font-weight: 600;'><strong>",
                           .("Data quality alert: Some variables contain infinite or extreme values that may require investigation."),
                           "</strong></p>") else "",
                
                "</div>"
            )
            
            return(clinical_text)
        },
        
        # Generate explanatory content about the analysis
        .generateAboutContent = function() {
            about_text <- paste0(
                "<div style='padding: 15px; background-color: rgba(55, 72, 105, 0.06); border-left: 4px solid #6b7280; margin: 10px 0; border-radius: 4px; color: inherit;'>",
                "<h4 style='margin-top: 0; color: inherit;'>", .("About This Analysis"), "</h4>",
                
                "<p><strong>", .("What this analysis provides"), ":</strong></p>",
                "<ul style='margin: 5px 0 10px 20px;'>",
                "<li>", .("Descriptive statistics: mean, median, standard deviation, min/max"), "</li>",
                "<li>", .("Visual summary tables with distribution plots"), "</li>",
                "<li>", .("Optional normality testing and distribution diagnostics"), "</li>",
                "</ul>",
                
                "<p><strong>", .("When to use this analysis"), ":</strong></p>",
                "<ul style='margin: 5px 0 10px 20px;'>",
                "<li>", .("Initial data exploration and quality assessment"), "</li>",
                "<li>", .("Biomarker characterization studies"), "</li>",
                "<li>", .("Preparation for statistical modeling"), "</li>",
                "<li>", .("Laboratory reference range studies"), "</li>",
                "</ul>",
                
                "<p><strong>", .("Key considerations"), ":</strong></p>",
                "<ul style='margin: 5px 0 10px 20px;'>",
                "<li>", .("Enable 'Distribution Diagnostics' for normality assessment"), "</li>",
                "<li>", .("Consider data transformations if distributions are highly skewed"), "</li>",
                "<li>", .("Investigate outliers before proceeding with inferential statistics"), "</li>",
                "<li>", .("Every statistic here is computed per row. If the dataset holds several rows per patient (one per block, core or visit), aggregate to one row per patient before reading these numbers as patient-level results."), "</li>",
                "<li>", .("The decimal places option governs the text summary, the skewness and kurtosis values, the visual summary table, the outlier report and the copy-ready clinical summary. P-values are always shown to 3 decimal places."), "</li>",
                "</ul>",
                
                "</div>"
            )
            
            return(about_text)
        },
        
        # Outlier detection using IQR method
        .detectOutliers = function(data, variables) {
            outlier_results <- list()
            
            for (var in variables) {
                # quantile() per variable over a long column; yield so jamovi can
                # process option changes and cancellations during the loop.
                private$.checkpoint()
                var_data <- as.numeric(data[[var]])
                var_data_clean <- var_data[!is.na(var_data)]
                
                if (length(var_data_clean) < 4) {
                    outlier_results[[var]] <- list(outliers = integer(0), method = "insufficient_data")
                    next
                }
                
                # IQR method
                Q1 <- quantile(var_data_clean, 0.25, na.rm = TRUE)
                Q3 <- quantile(var_data_clean, 0.75, na.rm = TRUE)
                IQR_val <- Q3 - Q1
                
                lower_bound <- Q1 - 1.5 * IQR_val
                upper_bound <- Q3 + 1.5 * IQR_val
                
                outlier_indices <- which(var_data < lower_bound | var_data > upper_bound)
                outlier_values <- var_data[outlier_indices]
                
                # Bounds and values are formatted at the user's chosen precision;
                # they were fixed at 3 and 2 decimals respectively, which read as
                # more (or less) precision than the rest of the output.
                outlier_results[[var]] <- list(
                    outliers = outlier_indices,
                    values = outlier_values,
                    lower_bound = private$.fmtNum(lower_bound),
                    upper_bound = private$.fmtNum(upper_bound),
                    method = "iqr"
                )
            }
            
            return(outlier_results)
        },
        
        # Generate outlier detection report
        .generateOutlierReport = function(variables, dataset) {
            if (length(variables) == 0) return("")
            
            outlier_results <- private$.detectOutliers(dataset, variables)
            
            report_html <- paste0(
                "<div style='padding: 15px; background-color: rgba(255, 202, 33, 0.23); border-left: 4px solid #ffc107; margin: 10px 0; border-radius: 4px; color: inherit;'>",
                "<h4 style='margin-top: 0; color: inherit;'>", .("Outlier Detection Results"), "</h4>",
                "<p>", .("Outliers detected using IQR method (values beyond Q1-1.5\u{D7}IQR or Q3+1.5\u{D7}IQR):"), "</p>"
            )
            
            for (var in variables) {
                result <- outlier_results[[var]]
                safe_var <- htmltools::htmlEscape(var)

                # The bounds are IQR fences, not the observed range, so the old
                # "Range:" label was wrong; and " - " between them rendered as
                # "-5.00 - -1.00" for the negative values routine in clinical
                # chemistry. Each line is one translatable sentence.
                if (result$method == "insufficient_data") {
                    report_html <- paste0(report_html,
                        "<p><strong>", safe_var, ":</strong> ",
                        .("Fewer than 4 non-missing observations, so the IQR fences could not be computed."), "</p>")
                } else if (length(result$outliers) == 0) {
                    report_html <- paste0(report_html,
                        "<p><strong>", safe_var, ":</strong> ",
                        jmvcore::format(
                            .("No outliers detected (expected range {lower} to {upper})."),
                            lower = result$lower_bound, upper = result$upper_bound), "</p>")
                } else {
                    report_html <- paste0(report_html,
                        "<p><strong>", safe_var, ":</strong> ",
                        jmvcore::format(
                            .("{count} outlier(s) detected (values: {values})."),
                            count = length(result$outliers),
                            values = paste(private$.fmtNum(result$values), collapse = ", ")),
                        "<br><span style='color: inherit; font-size: 0.9em;'>",
                        jmvcore::format(
                            .("Expected range: {lower} to {upper}"),
                            lower = result$lower_bound, upper = result$upper_bound),
                        "</span></p>")
                }
            }
            
            report_html <- paste0(report_html,
                "<p style='margin-top: 15px; padding-top: 10px; border-top: 1px solid #ffc107; font-size: 0.9em;'>",
                "<strong>", .("Clinical Note"), ":</strong> ", 
                .("Outliers may indicate data entry errors, measurement issues, or genuine extreme values. Review outliers in clinical context before exclusion."),
                "</p></div>"
            )
            
            return(report_html)
        },
        
        # Generate copy-ready report sentences
        .generateReportSentences = function(variables, dataset) {
            if (length(variables) == 0) return("")
            
            sentences <- c()
            
            for (var in variables) {
                private$.checkpoint()
                var_data <- as.numeric(dataset[[var]])
                var_clean <- var_data[!is.na(var_data)]

                if (length(var_clean) == 0) next

                # Calculate statistics. These were hard-coded to 2 decimals, so a
                # user who set "Decimal places" to 4 got 4 in the summary and 2 in
                # the sentence they were told to paste into a manuscript.
                n <- length(var_clean)
                mean_val <- private$.fmtNum(mean(var_clean))
                sd_val <- private$.fmtNum(sd(var_clean))
                median_val <- private$.fmtNum(median(var_clean))
                min_val <- private$.fmtNum(min(var_clean))
                max_val <- private$.fmtNum(max(var_clean))

                # One translatable string per sentence: the old fragment
                # concatenation was untranslatable (a Turkish user got an English
                # sentence inside a translated frame) and could not be reordered.
                # "range {min}-{max}" also rendered as "range -5.00--1.00" for the
                # negative values routine in clinical chemistry.
                # Branch on the observation count, not on the formatted SD (same
                # reason as in mysummary()).
                # A bare "+/-" is ambiguous between SD and SEM, which differ by a
                # factor of sqrt(n); this is text the user is invited to paste into
                # a manuscript, so the dispersion measure is named.
                sentence <- if (n < 2) {
                    private$.fmtVar(
                        .("For {variable}, the single available observation was {value}."),
                        var, value = mean_val)
                } else {
                    private$.fmtVar(
                        .("For {variable}, analysis of {n} observations showed a mean of {mean} (SD {sd}); median {median} (range {min} to {max})."),
                        var, n = n,
                        mean = mean_val, sd = sd_val, median = median_val,
                        min = min_val, max = max_val)
                }
                
                # Add distribution information if enabled
                if (self$options$distr && n >= 3 && n <= 5000) {
                    # Reuse the cached Shapiro-Wilk result from the diagnostics text
                    sw_test <- private$.shapiroResult(var_clean, key = var)

                    # Same verdict function as the diagnostics panel, so the two
                    # cannot disagree - previously this compared the exact p while
                    # the diagnostics compared the rounded one, and a variable with
                    # p = 0.0501 was reported as normal here and non-normal there,
                    # both printing "p = 0.05".
                    #
                    # "Data showed normal distribution" is also not a claim a
                    # non-significant test supports, and this is the text the user
                    # is invited to paste into a manuscript.
                    verdict <- private$.normalityVerdict(
                        if (is.null(sw_test)) NA_real_ else sw_test$p.value)

                    if (!is.null(verdict)) {
                        # .fmtP() carries the relation below the reporting bound, so
                        # the template must not also supply "="; it rendered
                        # "(p = < 0.001)" in the one block labelled copy-ready.
                        sw_sentence <- if (sw_test$p.value < 0.001)
                            jmvcore::format(
                                .("The Shapiro-Wilk test showed {verdict} (p &lt; 0.001)."),
                                verdict = verdict$short)
                        else
                            jmvcore::format(
                                .("The Shapiro-Wilk test showed {verdict} (p = {p})."),
                                verdict = verdict$short,
                                p = private$.fmtP(sw_test$p.value))
                        sentence <- paste0(sentence, " ", sw_sentence)
                    }
                }
                
                sentences <- c(sentences, sentence)
            }
            
            report_html <- paste0(
                "<div style='padding: 15px; background-color: rgba(33, 159, 33, 0.1); border-left: 4px solid #4caf50; margin: 10px 0; border-radius: 4px; color: inherit;'>",
                "<h4 style='margin-top: 0; color: inherit;'>", .("Copy-Ready Clinical Summary"), "</h4>",
                # An opaque white background with no foreground colour rendered
                # this box - the very text the user is invited to read and copy -
                # as light-on-white under jamovi's dark theme. A neutral
                # translucent tint reads as a raised panel over either ground.
                "<div style='background-color: rgba(127, 127, 127, 0.12); color: inherit; padding: 10px; border-radius: 3px; border: 1px solid rgba(76, 175, 80, 0.45);'>",
                paste(sentences, collapse = "<br><br>"),
                "</div>",
                "<p style='margin-top: 10px; font-size: 0.9em; color: inherit;'>",
                "<strong>", .("Usage"), ":</strong> ",
                .("Copy the text above for use in clinical reports, research manuscripts, or medical documentation."),
                "</p></div>"
            )
            
            return(report_html)
        },
        
        # Generate statistical terminology glossary
        .generateGlossary = function() {
            glossary_html <- paste0(
                "<div style='padding: 15px; background-color: rgba(55, 72, 105, 0.06); border-left: 4px solid #6b7280; margin: 10px 0; border-radius: 4px; color: inherit;'>",
                "<h4 style='margin-top: 0; color: inherit;'>", .("Statistical Terminology"), "</h4>",
                
                "<div style='margin-bottom: 10px;'>",
                "<strong>", .("Mean"), ":</strong> ", .("Average value of all observations. Sensitive to outliers."), "<br>",
                "<strong>", .("Median"), ":</strong> ", .("Middle value when data is ordered. Less affected by outliers."), "<br>",
                "<strong>", .("Standard Deviation (SD)"), ":</strong> ", .("Measure of variability around the mean."), "<br>",
                "<strong>", .("Range"), ":</strong> ", .("Difference between maximum and minimum values."), "<br>",
                "</div>",
                
                if (self$options$distr) {
                    paste0(
                        "<div style='margin-bottom: 10px; padding-top: 10px; border-top: 1px solid #d1d5db;'>",
                        "<strong>", .("Distribution Diagnostics"), ":</strong><br>",
                        # "p > 0.05 suggests normality" accepts the null, and it
                        # contradicted .normalityVerdict() - which is the single
                        # source of truth for the per-variable wording - two panels
                        # further up. Phrased as a rule, the glossary won.
                        "<strong>", .("Shapiro-Wilk Test"), ":</strong> ", .("Tests whether the data depart from a normal distribution. A small p-value (p &lt;= 0.05) is evidence of a departure; a larger p-value does not establish normality, it only means no departure was detected. The test is sensitive to sample size, flagging trivial departures in large samples and missing real ones in small samples, so pair it with a visual check of the distribution."), "<br>",
                        "<strong>", .("Skewness"), ":</strong> ", .("Measures asymmetry. 0 = symmetric, &gt;0 = right-skewed, &lt;0 = left-skewed."), "<br>",
                        "<strong>", .("Kurtosis"), ":</strong> ", .("Measures tail heaviness. 3 = normal, &gt;3 = heavy tails, &lt;3 = light tails."), "<br>",
                        "</div>"
                    )
                } else "",
                
                if (self$options$outliers) {
                    paste0(
                        "<div style='margin-bottom: 10px; padding-top: 10px; border-top: 1px solid #d1d5db;'>",
                        "<strong>", .("Outlier Detection"), ":</strong><br>",
                        "<strong>", .("IQR Method"), ":</strong> ", .("Values beyond Q1-1.5\u{D7}IQR or Q3+1.5\u{D7}IQR are considered outliers."), "<br>",
                        "<strong>", .("Q1, Q3"), ":</strong> ", .("First and third quartiles (25th and 75th percentiles)."), "<br>",
                        "</div>"
                    )
                } else "",
                
                "<p style='margin-top: 15px; font-size: 0.9em; color: inherit;'>",
                .("These statistics help assess data distribution, identify unusual values, and guide appropriate statistical analyses."),
                "</p></div>"
            )
            
            return(glossary_html)
        }

        # NOTE: R code generation feature deferred to future release
        # See SUMMARYDATA_FIXES.md for details
        # Implementation can be restored from git history if needed
    ))
