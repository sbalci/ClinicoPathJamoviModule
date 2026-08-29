#' @title Multi-Variable Visual Quality
#' @return HTML summary of data quality issues including duplicates and missing values
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#
# NOTE: no other @importFrom tags here on purpose. Every call in this file is
# fully namespaced (visdat::, ggplot2::, htmltools::, stats::), and the tags that
# used to sit here named symbols the file never uses - magrittr's %>%,
# dplyr::n_distinct, htmltools::HTML, ggplot2::ggsave/theme_minimal - which only
# inflated NAMESPACE and fed the module-wide import-collision warnings.
dataqualityClass <- if (requireNamespace("jmvcore")) R6::R6Class("dataqualityClass",
    inherit = dataqualityBase, private = list(

    # Notice collection helpers. A single Preformatted (plain-text) output item:
    # avoids BOTH the jmvcore::Notice serialization error from
    # self$results$insert(999, Notice) AND any HTML in notices (project convention:
    # notice content must be plain text). ====
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

        # Plain text only notices avoid HTML by project convention; the Preformatted
        # output item renders this literally (no markup, no injection surface).
        blocks <- vapply(private$.noticeList, function(notice) {
            prefix <- switch(notice$type,
                ERROR          = "ERROR: ",
                STRONG_WARNING = "WARNING: ",
                WARNING        = "WARNING: ",
                "")
            paste0(prefix, notice$title, "\n", notice$content)
        }, character(1))

        self$results$notices$setContent(paste(blocks, collapse = "\n\n"))
    },

    # A plot renderer cannot populate a results element, so when a plot fails the
    # explanation has to travel inside the plot itself.
    .placeholderPlot = function(message) {
        plot <- ggplot2::ggplot() +
            ggplot2::geom_text(
                ggplot2::aes(
                    x = 0.5,
                    y = 0.5,
                    label = paste(strwrap(message, width = 70), collapse = "\n")
                ),
                size = 4
            ) +
            ggplot2::xlim(0, 1) +
            ggplot2::ylim(0, 1) +
            ggplot2::theme_void()

        print(plot)
        return(TRUE)
    },

    .mcarTestMessage = function(data) {
        numeric_data <- data[vapply(data, is.numeric, logical(1))]
        n_non_numeric <- ncol(data) - ncol(numeric_data)
        # The test runs on the numeric columns ONLY, but the per-variable
        # missingness printed immediately above it covers every selected
        # variable, so the scope has to be stated or the result reads as a
        # statement about the whole selection.
        scope_note <- if (n_non_numeric > 0) {
            sprintf(
                paste0(
                    " Computed on the %d numeric variable(s) (%s); missingness in the ",
                    "%d non-numeric variable(s) was not tested."
                ),
                ncol(numeric_data),
                paste(names(numeric_data), collapse = ", "),
                n_non_numeric
            )
        } else {
            ""
        }
        assumption_note <- paste0(
            " Little's test assumes multivariate normality within missing-data ",
            "patterns and has little power when patterns contain few cases."
        )
        if (ncol(numeric_data) < 2) {
            return(paste0(
                "Little's MCAR test was not run because it requires at least two ",
                "numeric variables; it is computed on numeric variables only, and ",
                sprintf("the selection contains %d numeric variable(s).", ncol(numeric_data))
            ))
        }
        if (!anyNA(numeric_data)) {
            return("Little's MCAR test was not run because the selected numeric variables have no missing values.")
        }

        if (!requireNamespace("naniar", quietly = TRUE)) {
            return("Little's MCAR test is unavailable because the optional naniar package is not installed.")
        }

        tryCatch({
            result <- as.data.frame(.quietly(naniar::mcar_test(numeric_data)))[1, , drop = FALSE]
            interpretation <- if (result$p.value < 0.05) {
                "The data provide evidence against the MCAR assumption."
            } else {
                paste0(
                    "The test does not reject the MCAR assumption, but this does not ",
                    "prove that the data are MCAR."
                )
            }
            paste0(
                sprintf(
                    paste0(
                        "Little's MCAR test (naniar): chi-square = %.2f, df = %s, ",
                        "p = %.4f, missing patterns = %s. %s"
                    ),
                    result$statistic,
                    result$df,
                    result$p.value,
                    result$missing.patterns,
                    interpretation
                ),
                scope_note,
                assumption_note
            )
        }, error = function(e) {
            paste0(
                "Little's MCAR test could not be computed for the selected variables. ",
                "Review variable types and missing-data patterns."
            )
        })
    },

    .run = function() {

        # Reset notices so the same message is not appended once per run cycle
        private$.noticeList <- list()
        private$.renderNotices()

        # Clear every HTML report block up front. .run() has three early-return
        # paths (no variables selected, empty dataset, variables missing from the
        # dataset) and without this the full report for the PREVIOUS variable set
        # stayed on screen underneath the new welcome/error panel - stale counts,
        # stale variable names and a stale overall verdict that a reader could
        # copy straight into a QC log.
        self$results$text$setContent("")
        self$results$summary$setContent("")
        self$results$recommendations$setContent("")
        self$results$explanations$setContent("")

        # TODO (forward-looking): no `.()` wrapping in this file (~1.1k LOC
        # of welcome HTML, recommendations, and explanations). Address in
        # a /prepare-translation pass.
        # TODO (forward-looking, perf): the visdat plot callbacks can be slow
        # on wide tables; add render-safe cancellation points when supported.

        # Check if variables have been selected. If not, display a welcoming message.
        if (length(self$options$vars) == 0) {
            intro_msg <- "
            <div style='background-color: rgba(33, 159, 33, 0.1); padding: 20px; border-radius: 8px; margin: 20px 0; color: inherit;'>
            <h3 style='color: #2e7d32; margin-top: 0;'>Welcome to Enhanced Data Quality Assessment!</h3>
            <p><strong>Comprehensive data quality analysis</strong> with visual exploration capabilities</p>
            <p>Enhanced with <strong>visdat integration</strong> based on autoEDA research (R Journal 2019)</p>

            <h4 style='color: #2e7d32;'>Quick Start:</h4>
            <ol>
            <li><strong>Select Variables:</strong> Choose specific variables or analyze entire dataset</li>
            <li><strong>Configure Analysis:</strong> Enable duplicate detection, missing value analysis</li>
            <li><strong>Visual Exploration:</strong> Use visdat for visual data quality assessment</li>
            <li><strong>Run Analysis:</strong> Get comprehensive data quality insights</li>
            </ol>

            <h4 style='color: #2e7d32;'>Analysis Features:</h4>
            <ul>
            <li><strong>Missing Value Analysis:</strong> Patterns and statistical summaries</li>
            <li><strong>Duplicate Detection:</strong> Row and value-level duplicate analysis</li>
            <li><strong>Data Completeness:</strong> Complete cases across variables</li>
            <li><strong>Visual Data Overview:</strong> visdat integration for visual exploration</li>
            <li><strong>Data Type Analysis:</strong> Automatic type detection and validation</li>
            </ul>

            <p style='font-size: 12px; color: inherit; opacity: 0.75; margin-top: 20px;'>
            <em>Enhanced with visdat package - unique visual data exploration (68,978+ downloads)</em>
            </p>
            </div>"
            self$results$todo$setContent(intro_msg)
            return()
        } else {
            self$results$todo$setContent("")
        }

        # Validate that the dataset contains complete rows.
        # Notices are rendered as HTML to avoid the jamovi protobuf serialization
        # error triggered by dynamically inserted jmvcore::Notice objects.
        if (nrow(self$data) == 0) {
            self$results$todo$setContent(
                "<div style='padding: 15px; background-color: rgba(216, 33, 50, 0.18); border-left: 4px solid #dc3545; color: inherit; border-radius: 5px;'><strong>Error:</strong> Dataset contains no rows. Please provide data with at least one observation.</div>"
            )
            return()
        }

        dataset <- self$data

        # Determine variables to analyze
        if (length(self$options$vars) > 0) {
            # Use raw variable names from self$options$vars for subsetting.
            # jmvcore::composeTerm is typically for constructing formulas, not for direct dataframe subsetting,
            # as it can add backticks that might prevent correct column selection.
            var_list <- self$options$vars

            # Validate that all requested variables exist in the dataset
            missing_vars <- var_list[!var_list %in% names(dataset)]
            if (length(missing_vars) > 0) {
                missing_safe <- paste(vapply(missing_vars, htmltools::htmlEscape, character(1)), collapse = ", ")
                self$results$todo$setContent(sprintf(
                    "<div style='padding: 15px; background-color: rgba(216, 33, 50, 0.18); border-left: 4px solid #dc3545; color: inherit; border-radius: 5px;'><strong>Error:</strong> Variables not found in dataset: %s. Please check variable names and try again.</div>",
                    missing_safe
                ))
                return()
            }

            # Safely extract columns
            analysis_data <- dataset[, var_list, drop = FALSE]
        }

        quality_results <- list()
        summary_rows <- list()

        # Helper to record per-variable summary for structured HTML
        add_summary_row <- function(var, data_vec) {
            n_total <- length(data_vec)
            n_missing <- sum(is.na(data_vec))
            missing_pct <- if (n_total > 0) round(n_missing / n_total * 100, 1) else NA
            n_nonmiss <- n_total - n_missing
            n_unique <- length(unique(jmvcore::naOmit(data_vec)))
            dup_pct <- if (n_nonmiss > 0) round((n_nonmiss - n_unique) / n_nonmiss * 100, 1) else NA
            vtype <- paste(class(data_vec), collapse = "/")

            near_zero_var <- FALSE
            high_card <- FALSE
            outlier_n <- NA
            if (is.numeric(data_vec)) {
                # This detects EXACTLY constant variables, not "near-zero
                # variance" in the caret::nearZeroVar sense: .Machine$double.eps
                # is ~2.2e-16, so a variable with sd = 1e-9 - genuinely degenerate
                # and quite capable of breaking a model - is not flagged. An
                # absolute SD cut-off is also scale-dependent (the same
                # measurement in metres and millimetres gives SDs 1000x apart), so
                # the label is what changed rather than the threshold. A proper
                # frequency-ratio / percent-unique screen would be a new feature.
                sdv <- stats::sd(data_vec, na.rm = TRUE)
                near_zero_var <- !is.na(sdv) && sdv < .Machine$double.eps
                # High cardinality is expected/normal for continuous numeric
                # variables (e.g. tumor size, lab values), so it is only flagged
                # for non-numeric (ID-like / categorical) variables below to
                # reduce false alarms.
                if (n_nonmiss > 10) {
                    q <- stats::quantile(data_vec, probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE)
                    iqr <- q[2] - q[1]
                    if (!is.na(iqr) && iqr > 0) {
                        lower <- q[1] - 1.5 * iqr
                        upper <- q[2] + 1.5 * iqr
                        outlier_n <- sum(data_vec < lower | data_vec > upper, na.rm = TRUE)
                    } else {
                        outlier_n <- 0
                    }
                }
            } else {
                high_card <- n_unique > 50 && n_unique > 0.5 * n_nonmiss
                # A categorical variable with a single observed level (every
                # patient "Stage IV", one hospital in Site, all-female Sex after a
                # filter) has exactly zero variance and breaks any model it enters
                # - the same defect the numeric branch flags via sd == 0. Without
                # this it stayed FALSE for every factor/character variable.
                near_zero_var <- n_nonmiss > 0 && n_unique <= 1
            }

            # Return the row; the caller appends it (avoids `<<-` into the
            # enclosing scope while keeping identical behaviour).
            return(data.frame(
                variable = var,
                type = vtype,
                n = n_total,
                missing = n_missing,
                missing_pct = missing_pct,
                unique = n_unique,
                dup_pct = dup_pct,
                near_zero_var = near_zero_var,
                high_card = high_card,
                outlier_n = outlier_n,
                # An outlier COUNT cannot be read without the sample size behind
                # it: Tukey's 1.5xIQR rule puts about 0.7% of normally
                # distributed observations outside the fences, so 7 outliers in
                # n=1000 is expected while 3 in n=15 is 20% of the data.
                outlier_pct = if (!is.na(outlier_n) && n_nonmiss > 0) {
                    round(outlier_n / n_nonmiss * 100, 1)
                } else {
                    NA
                },
                stringsAsFactors = FALSE
            ))
        }

        # Pre-compute per-variable summaries for downstream reporting.
        # .checkpoint() lets jamovi cancel a wide-table scan instead of freezing.
        for (nm in names(analysis_data)) {
            private$.checkpoint()
            summary_rows[[length(summary_rows) + 1]] <- add_summary_row(nm, analysis_data[[nm]])
        }

        # Check for high missingness (>50%)
        high_missing_vars <- vapply(summary_rows, function(r) {
            if (!is.na(r$missing_pct) && r$missing_pct > 50) r$variable else NA_character_
        }, character(1))
        high_missing_vars <- high_missing_vars[!is.na(high_missing_vars)]

        # Total sample size (used for small-sample checks and downstream reporting)
        n_total <- nrow(analysis_data)

        # Check for near-zero variance
        near_zero_vars <- vapply(summary_rows, function(r) {
            if (isTRUE(r$near_zero_var)) r$variable else NA_character_
        }, character(1))
        near_zero_vars <- near_zero_vars[!is.na(near_zero_vars)]

        # Surface critical warnings in the always-visible `text` output so they
        # persist even when the plain-language summary and recommendations
        # panels are both toggled off. (jmvcore::Notice objects are avoided
        # here because dynamically inserted notices break jamovi's protobuf
        # serialization.)
        critical_warnings <- character(0)
        if (length(high_missing_vars) > 0) {
            critical_warnings <- c(critical_warnings, sprintf(
                "<strong>High missingness (&gt;50%%):</strong> %s",
                paste(htmltools::htmlEscape(high_missing_vars), collapse = ", ")))
        }
        if (length(near_zero_vars) > 0) {
            critical_warnings <- c(critical_warnings, sprintf(
                "<strong>Constant (zero variance):</strong> %s",
                paste(htmltools::htmlEscape(near_zero_vars), collapse = ", ")))
        }
        if (n_total < 20) {
            critical_warnings <- c(critical_warnings, sprintf(
                "<strong>Very small sample size (n=%d):</strong> estimates may be unstable",
                n_total))
        }
        if (length(critical_warnings) > 0) {
            quality_results$critical_warnings <- paste0(
                "<div style='background-color: rgba(255, 202, 33, 0.23); padding: 15px; border-left: 4px solid #ffc107; border-radius: 5px; color: inherit;'>",
                "<h4 style='margin-top: 0; color: inherit;'>Critical Data Quality Warnings</h4>",
                "<ul style='margin-bottom: 0;'><li>",
                paste(critical_warnings, collapse = "</li><li>"),
                "</li></ul></div>"
            )
        }

        # Missing value analysis
        if (self$options$check_missing) {
            # OPTIMIZED: Extract directly from summary_rows instead of re-iterating
            missing_summary <- setNames(
                vapply(summary_rows, function(row) {
                    paste0("Missing: ", row$missing, "/", row$n, " (", row$missing_pct, "%)")
                }, character(1)),
                vapply(summary_rows, function(r) r$variable, character(1))
            )

            # Case-level missingness distribution
            case_missing <- rowSums(is.na(analysis_data))
            case_summary <- sprintf("Case-level missing: median %.1f, mean %.1f, max %d (of %d vars)",
                                    stats::median(case_missing),
                                    mean(case_missing),
                                    max(case_missing),
                                    ncol(analysis_data))

            private$.checkpoint()
            mcar_msg <- private$.mcarTestMessage(analysis_data)

            # Threshold flagging
            threshold <- self$options$missing_threshold_visual
            flags <- vapply(summary_rows, function(row) ifelse(!is.na(row$missing_pct) && row$missing_pct > threshold, row$variable, NA_character_), character(1))
            flags <- flags[!is.na(flags)]

            flag_html <- if (length(flags) > 0) {
                paste0("<p><strong>Variables exceeding ", threshold, "% missing:</strong> ", paste(htmltools::htmlEscape(flags), collapse = ", "), "</p>")
            } else {
                ""
            }

            quality_results$missing <- paste0(
                "<h4>Missing Value Analysis</h4>",
                paste(htmltools::htmlEscape(names(missing_summary)), missing_summary, sep = ": ", collapse = "<br>"),
                "<br>", case_summary,
                "<br>", htmltools::htmlEscape(mcar_msg),
                flag_html
            )
        }

        # Initialize duplicate_rows to NA (will be set if duplicate analysis runs)
        duplicate_rows <- NA

        # Duplicate analysis
        if (self$options$check_duplicates) {
            if (self$options$complete_cases_only && length(var_list) > 1) {
                # Check for duplicate rows across all selected variables
                total_rows <- nrow(analysis_data)
                unique_rows <- nrow(unique(analysis_data))
                duplicate_rows <- total_rows - unique_rows
                duplicate_pct <- round(duplicate_rows / total_rows * 100, 1)

                # Identify top duplicated row signatures
                dup_keys <- NA
                if (duplicate_rows > 0) {
                    # Pasting every row into a signature and tabulating it is the
                    # most expensive step here on a large registry export.
                    private$.checkpoint()
                    # unname()/as.list() is load bearing: c(analysis_data, sep = "||")
                    # keeps the column names, so a selected column called `sep`
                    # errors ("formal argument 'sep' matched by multiple actual
                    # arguments") and one called `collapse` is silently consumed
                    # as paste()'s collapse=, returning ONE string instead of a
                    # per-row vector - the duplicate count stayed right while the
                    # evidence list below rendered empty.
                    key_freq <- as.data.frame(table(
                        do.call(paste, c(unname(as.list(analysis_data)), list(sep = "||")))))
                    key_freq <- key_freq[key_freq$Freq > 1, ]
                    key_freq <- key_freq[order(-key_freq$Freq), ]
                    top_keys <- head(key_freq, 5)
                    dup_keys <- paste0("<br><em>Top duplicated patterns (first 5):</em><br>",
                                       paste(paste(htmltools::htmlEscape(top_keys$Var1), " (n=", top_keys$Freq, ")", sep = ""), collapse = "<br>"))
                }

                quality_results$duplicates <- paste0(
                    "<h4>Duplicate Row Analysis</h4>",
                    "Total rows: ", total_rows, "<br>",
                    "Unique rows: ", unique_rows, "<br>",
                    "Duplicate rows: ", duplicate_rows, " (", duplicate_pct, "%)",
                    if (!is.na(dup_keys)) dup_keys else ""
                )
            } else {
                # Ticking "Duplicate rows" with only ONE variable selected used to
                # fall through to value-level analysis silently, under a heading
                # ("Duplicate Value Analysis") that contradicted the box the user
                # had ticked. With a single variable a duplicate row and a
                # duplicate value are the same thing, so the numbers are right -
                # but say so rather than appearing to ignore the setting.
                if (isTRUE(self$options$complete_cases_only) && length(var_list) <= 1) {
                    quality_results$duplicate_mode_note <- paste0(
                        "<div style='background-color: rgba(33, 144, 246, 0.11); color: inherit; padding: 10px; ",
                        "border-left: 4px solid #2196f3; border-radius: 4px; margin-bottom: 10px;'>",
                        "Row-level duplicate analysis needs at least two variables to define a row ",
                        "signature. With one variable selected, a duplicate row and a duplicate value ",
                        "are the same thing, so the value-level result below answers the same question. ",
                        "Select more variables to compare full row signatures.",
                        "</div>")
                }
                # Check for duplicates within each variable
                # Accumulate the total number of duplicate values across all
                # selected variables so the plain-language summary and
                # recommendations report a non-zero count in value-level mode.
                # (Previously duplicate_rows stayed NA in this branch, so both
                # always reported 0 duplicates for value-level analysis.)
                duplicate_rows <- sum(vapply(analysis_data, function(x) {
                    non_missing <- sum(!is.na(x))
                    unique_vals <- length(unique(jmvcore::naOmit(x)))
                    non_missing - unique_vals
                }, numeric(1)))

                dup_summary <- sapply(analysis_data, function(x) {
                    total <- length(x)
                    non_missing <- sum(!is.na(x))
                    unique_vals <- length(unique(jmvcore::naOmit(x)))
                    duplicate_vals <- non_missing - unique_vals
                    dup_pct <- if (non_missing > 0) {
                        round(duplicate_vals / non_missing * 100, 1)
                    } else {
                        0
                    }

                    paste0("Unique: ", unique_vals, ", Duplicates: ", duplicate_vals,
                           " (", dup_pct, "% of non-missing)")
                })

                quality_results$duplicates <- paste0(
                    "<h4>Duplicate Value Analysis</h4>",
                    paste(htmltools::htmlEscape(names(dup_summary)), dup_summary, sep = ": ", collapse = "<br>"),
                    "<p style='margin-top: 10px; font-size: 0.9em; color: inherit; opacity: 0.75;'>",
                    "<em>Interpretation Note:</em> For categorical variables with few unique levels (e.g., 'Gender', 'Status'), ",
                    "a high number of 'Duplicates' often reflects data redundancy (many observations sharing the same valid value), ",
                    "not necessarily data errors. For identifier variables (e.g., 'Patient ID'), duplicates would typically indicate errors.",
                    "</p>"
                )
            }
        }

        # Complete cases analysis
        if (length(var_list) > 1) {
            complete_cases <- sum(complete.cases(analysis_data))
            total_cases <- nrow(analysis_data)
            complete_pct <- round(complete_cases / total_cases * 100, 1)

            quality_results$completeness <- paste0(
                "<h4>Data Completeness</h4>",
                "Complete cases: ", complete_cases, "/", total_cases, " (", complete_pct, "%)"
            )
        }

        # visdat Visual Analysis - Individual plot options
        if (self$options$plot_data_overview || self$options$plot_missing_patterns ||
            self$options$plot_data_types) {
            if (!requireNamespace("visdat", quietly = TRUE)) {
                private$.addNotice(
                    "WARNING",
                    "Visual plots unavailable",
                    paste0(
                        "The requested visual data quality plots cannot be drawn ",
                        "because the visdat package is not installed on this ",
                        "computer. All numeric checks above - missing values, ",
                        "duplicates, constant variables and outliers - are complete ",
                        "and unaffected. To get the plots, install the package with ",
                        "install.packages('visdat') and re-run the analysis; ",
                        "otherwise switch the plot options off to hide the empty ",
                        "plot areas."
                    )
                )
            }
            visdat_results <- private$.generate_visdat_analysis(analysis_data)
            quality_results$visual <- visdat_results
        }

        # Always provide structured summary table
        if (length(summary_rows) > 0) {
            df <- do.call(rbind, summary_rows)
            # Basic HTML table
            summary_table <- paste(
                apply(df, 1, function(r) paste0("<tr>", paste0("<td>", htmltools::htmlEscape(r), "</td>", collapse = ""), "</tr>")),
                collapse = "\n"
            )
            header <- paste0("<tr><th>Variable</th><th>Type</th><th>N</th><th>Missing</th><th>%Missing</th><th>Unique</th><th>%Duplicates</th><th>Constant</th><th>High card</th><th>Outliers</th><th>%Outliers</th></tr>")
            quality_results$summary_table <- paste0(
                "<h4>Variable Quality Summary</h4>",
                "<p><em>Flags:</em> constant (zero-variance) variables, high cardinality (many unique values), and IQR-based outlier counts for numeric variables. ",
                "The constant flag identifies variables with no variation at all; it does not screen for the broader <em>near-zero variance</em> case (very low but non-zero variation), for which caret::nearZeroVar() is the appropriate tool. ",
                "The high-cardinality flag is applied to categorical and text variables only - many distinct values are expected and normal for a continuous measurement, so numeric variables are deliberately never flagged. ",
                "A high %Duplicates is expected for categorical variables with few levels (a binary variable in n=500 reads about 99.6%) and only signals a problem for identifier variables. ",
                "Read %Outliers rather than the raw count: under the 1.5\u{D7}IQR rule about 0.7% of normally distributed observations fall outside the fences (the expected proportion does not grow with sample size, though it is estimated imprecisely in small samples, and the rule is not applied at all to variables with 10 or fewer non-missing values).</p>",
                "<table border='1' cellspacing='0' cellpadding='4'>",
                header,
                summary_table,
                "</table>"
            )
        }

        # Combine all results
        final_results <- paste(unlist(quality_results), collapse = "<br><br>")
        self$results$text$setContent(final_results)

        # Set plot states for individual visual analyses
        # Convert to base data.frame to avoid serialization issues
        plotData <- list(
            data = as.data.frame(analysis_data),
            threshold = self$options$missing_threshold_visual
        )

        if (self$options$plot_data_overview) {
            private$.checkpoint()
            self$results$plotDataOverview$setState(plotData)
        }

        if (self$options$plot_missing_patterns) {
            private$.checkpoint()
            self$results$plotMissingPatterns$setState(plotData)
        }

        if (self$options$plot_data_types) {
            private$.checkpoint()
            # visdat::vis_guess() type-guesses every individual CELL, so its cost
            # is proportional to rows x columns and it runs on the render thread
            # where nothing can cancel it. Above this size the plot is replaced by
            # an explanation rather than freezing jamovi.
            n_cells <- nrow(analysis_data) * ncol(analysis_data)
            if (n_cells > 2e5) {
                plotData$skip_message <- sprintf(
                    paste0(
                        "The data types plot was not drawn because the selection is too large: %d rows x %d ",
                        "variables is %s cells, and this plot guesses a type for every cell individually ",
                        "(the limit is 200,000 cells). Nothing else in the report is affected - the 'Type' ",
                        "column of the Variable Quality Summary above reports each variable's storage type ",
                        "for the full selection. To see the plot, select fewer variables or apply a row ",
                        "filter, then run again."
                    ),
                    nrow(analysis_data), ncol(analysis_data),
                    # base:: is REQUIRED here: `@import jmvcore` brings
                    # .fmt(str, ..., context) into this namespace, which
                    # masks base::format and silently swallows big.mark/scientific.
                    base::format(n_cells, big.mark = ",", scientific = FALSE)
                )
            }
            self$results$plotDataTypes$setState(plotData)
        }

        # Generate clinical summaries if requested
        if (self$options$showSummary) {
            private$.generateSummary(summary_rows, n_total, high_missing_vars, near_zero_vars, duplicate_rows)
        }

        if (self$options$showRecommendations) {
            private$.generateRecommendations(summary_rows, n_total, high_missing_vars, near_zero_vars, duplicate_rows)
        }

        if (self$options$showExplanations) {
            private$.generateExplanations()
        }

        # NOTE: Removed completion notice to avoid serialization errors
        # Dynamically inserted Notice objects contain function references that
        # cannot be serialized by jamovi's protobuf system

    },

    .generate_visdat_analysis = function(data) {
        # Generate visdat analysis based on individual plot selections

        # Safely require visdat
        # NOTE: Using HTML warning instead of Notice to avoid serialization errors
        if (!requireNamespace("visdat", quietly = TRUE)) {
            return(paste0(
                "<div style='background-color: rgba(255, 202, 33, 0.23); padding: 15px; border-left: 4px solid #ffc107; margin: 10px 0; color: inherit;'>",
                "<strong> Warning:</strong> visdat package not installed. Visual exploration disabled.<br>",
                "Install via: <code>install.packages('visdat')</code>",
                "</div>"
            ))
        }

        missing_threshold <- self$options$missing_threshold_visual

        # Determine which analyses are enabled
        enabled_analyses <- c()
        if (self$options$plot_data_overview) enabled_analyses <- c(enabled_analyses, "Data Overview")
        if (self$options$plot_missing_patterns) enabled_analyses <- c(enabled_analyses, "Missing Patterns")
        if (self$options$plot_data_types) enabled_analyses <- c(enabled_analyses, "Data Types")

        # Generate visual analysis summary
        header_html <- paste0(
            "<div style='background-color: rgba(33, 152, 239, 0.13); padding: 20px; border-radius: 8px; margin-bottom: 20px; color: inherit;'>",
            "<h3 style='color: #1976d2; margin-top: 0;'>Visual Data Exploration (visdat)</h3>",
            "<p>Advanced visual data quality assessment - Based on autoEDA research</p>",
            "<p><strong>Enabled Analyses:</strong> ", paste(enabled_analyses, collapse = ", "), "</p>",
            "</div>"
        )

        # Basic data overview for visual analysis
        n_vars <- ncol(data)
        n_obs <- nrow(data)
        missing_vars <- sum(sapply(data, function(x) any(is.na(x))))
        complete_vars <- n_vars - missing_vars

        overview_html <- paste0(
            "<div style='background-color: rgba(88, 88, 88, 0.06); padding: 15px; border-radius: 8px; margin-bottom: 15px; color: inherit;'>",
            "<h4 style='color: inherit; margin-top: 0;'>Visual Analysis Overview</h4>",
            "<table style='width: 100%; border-collapse: collapse;'>",
            "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Variables:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", n_vars, "</td></tr>",
            "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Observations:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", n_obs, "</td></tr>",
            "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Variables with Missing:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", missing_vars, " / ", n_vars, "</td></tr>",
            "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Complete Variables:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", complete_vars, " / ", n_vars, "</td></tr>",
            "</table>",
            "</div>"
        )

        # Analysis insights based on enabled plots
        insights_html <- private$.generate_visdat_insights(data, missing_threshold)

        return(paste0(header_html, overview_html, insights_html))
    },

    .generate_visdat_insights = function(data, threshold) {
        # Generate insights based on enabled plot types

        insights_html <- paste0(
            "<div style='background-color: rgba(255, 203, 33, 0.14); padding: 15px; border-radius: 8px; margin-bottom: 15px; color: inherit;'>",
            "<h4 style='color: #f57f17; margin-top: 0;'>Visual Analysis Insights</h4>"
        )

        if (self$options$plot_data_overview) {
            # Data overview insights
            # Collapse multiple classes into single string to avoid length mismatch errors
            var_types <- sapply(data, function(x) paste(class(x), collapse = "/"))
            type_summary <- table(var_types)

            insights_html <- paste0(insights_html,
                "<p><strong>Data Type Distribution:</strong></p>",
                "<ul>"
            )

            for (type in names(type_summary)) {
                insights_html <- paste0(insights_html,
                    "<li><strong>", type, ":</strong> ", type_summary[type], " variables</li>"
                )
            }
            insights_html <- paste0(insights_html, "</ul>")
        }

        if (self$options$plot_missing_patterns) {
            # Missing pattern insights
            missing_counts <- sapply(data, function(x) sum(is.na(x)))
            vars_above_threshold <- sum(missing_counts > (nrow(data) * threshold / 100))

            insights_html <- paste0(insights_html,
                "<p><strong>Missing Value Patterns:</strong></p>",
                "<ul>",
                "<li>Variables above ", threshold, "% missing threshold: ", vars_above_threshold, "</li>",
                "<li>Total missing values: ", sum(missing_counts), "</li>",
                "</ul>"
            )
        }

        if (self$options$plot_data_types) {
            # Type detection insights
            char_vars <- sum(sapply(data, is.character))
            numeric_vars <- sum(sapply(data, is.numeric))
            factor_vars <- sum(sapply(data, is.factor))

            insights_html <- paste0(insights_html,
                "<p><strong>Type Detection Results:</strong></p>",
                "<ul>",
                "<li>Character variables: ", char_vars, " (may need conversion)</li>",
                "<li>Numeric variables: ", numeric_vars, " (ready for analysis)</li>",
                "<li>Factor variables: ", factor_vars, " (categorical analysis ready)</li>",
                "</ul>"
            )
        }

        # Add recommendations
        insights_html <- paste0(insights_html,
            "<p><strong>Recommendations:</strong></p>",
            "<ul>",
            "<li>Review visual plots below for detailed patterns</li>",
            "<li>Address missing value issues before analysis</li>",
            "<li>Validate data types match analysis requirements</li>",
            "</ul>",
            "</div>"
        )

        return(insights_html)
    },

    .plotDataOverview = function(image, ggtheme, theme, ...) {
        # Get plot state
        plotData <- image$state

        if (is.null(plotData) || is.null(plotData$data) || nrow(plotData$data) == 0) {
            return(FALSE)
        }

        # Check if visdat package is available
        if (!requireNamespace("visdat", quietly = TRUE)) {
            return(FALSE)
        }

        tryCatch({
            # Create data overview plot
            plot <- .quietly(visdat::vis_dat(plotData$data)) +
                ggtheme +
                ggplot2::theme(
                    axis.text.x = ggplot2::element_text(
                        angle = 45,
                        hjust = 0,
                        vjust = 0.5,
                        margin = ggplot2::margin(t = 5)
                    ),
                    plot.margin = ggplot2::margin(t = 5, r = 5, b = 40, l = 5)
                )

            print(plot)
            return(TRUE)

        }, error = function(e) {
            private$.placeholderPlot(paste0(
                "The data overview plot could not be drawn for the ",
                ncol(plotData$data), " selected variable(s). ",
                "Every other data quality check above is unaffected - only this ",
                "picture is missing. This usually means one of the selected ",
                "variables has a type the overview cannot display, such as a wide ",
                "free-text field, a date/time column or a list column. ",
                "Try removing those variables from the selection, or turn off ",
                "'Data overview plot' and read the Variable Quality Summary table ",
                "instead. Technical detail: ", conditionMessage(e)
            ))
        })
    },

    .plotMissingPatterns = function(image, ggtheme, theme, ...) {
        # Get plot state
        plotData <- image$state

        if (is.null(plotData) || is.null(plotData$data) || nrow(plotData$data) == 0) {
            return(FALSE)
        }

        # Check if visdat package is available
        if (!requireNamespace("visdat", quietly = TRUE)) {
            return(FALSE)
        }

        tryCatch({
            # Create missing patterns plot
            # Note: visdat::vis_miss doesn't have threshold highlighting capability
            # It shows all missing values with sort_miss option
            plot <- .quietly(visdat::vis_miss(
                plotData$data,
                sort_miss = TRUE,  # Sort by missingness for clarity
                show_perc = TRUE,  # Show percentage missing
                show_perc_col = TRUE  # Show percentage by column
            )) +
                ggplot2::labs(
                    subtitle = paste0("Missing value patterns (threshold for warnings: ", plotData$threshold, "%)")
                ) +
                ggtheme +
                ggplot2::theme(
                    axis.text.x = ggplot2::element_text(
                        angle = 45,
                        hjust = 0,
                        vjust = 0.5,
                        margin = ggplot2::margin(t = 5)
                    ),
                    plot.margin = ggplot2::margin(t = 5, r = 5, b = 40, l = 5)
                )

            print(plot)
            return(TRUE)

        }, error = function(e) {
            private$.placeholderPlot(paste0(
                "The missing-values pattern plot could not be drawn for the ",
                ncol(plotData$data), " selected variable(s). ",
                "The missing-value counts and percentages in the summary table ",
                "above are unaffected - only this picture is missing, so you ",
                "cannot see how the gaps line up across variables. ",
                "This usually means a selected variable has a type the plot ",
                "cannot display, or that no value is missing at all. ",
                "Try removing wide free-text or date variables from the ",
                "selection and running again. Technical detail: ",
                conditionMessage(e)
            ))
        })
    },

    .plotDataTypes = function(image, ggtheme, theme, ...) {
        # Get plot state
        plotData <- image$state

        if (is.null(plotData) || is.null(plotData$data) || nrow(plotData$data) == 0) {
            return(FALSE)
        }

        # .run() refused this plot on size grounds; say so where the user is looking
        if (!is.null(plotData$skip_message)) {
            return(private$.placeholderPlot(plotData$skip_message))
        }

        # Check if visdat package is available
        if (!requireNamespace("visdat", quietly = TRUE)) {
            return(FALSE)
        }

        tryCatch({
            # Create data types plot
            plot <- .quietly(visdat::vis_guess(plotData$data)) +
                ggtheme +
                ggplot2::theme(
                    axis.text.x = ggplot2::element_text(
                        angle = 45,
                        hjust = 0,
                        vjust = 0.5,
                        margin = ggplot2::margin(t = 5)
                    ),
                    plot.margin = ggplot2::margin(t = 5, r = 5, b = 40, l = 5)
                )

            print(plot)
            return(TRUE)

        }, error = function(e) {
            private$.placeholderPlot(paste0(
                "The data types plot could not be drawn for the ",
                ncol(plotData$data), " selected variable(s). ",
                "The 'Type' column of the Variable Quality Summary table above ",
                "still reports each variable's storage type - only this ",
                "value-by-value guess is missing. Guessing types cell by cell is ",
                "also slow on large tables, so this can fail on very wide or very ",
                "long data. Try selecting fewer variables, or turn off ",
                "'Data types plot' and use the summary table instead. ",
                "Technical detail: ", conditionMessage(e)
            ))
        })
    },

    .generateSummary = function(summary_rows, n_total, high_missing_vars, near_zero_vars, duplicate_rows) {
        # Generate plain-language summary of data quality assessment

        n_vars_analyzed <- length(summary_rows)
        threshold <- self$options$missing_threshold_visual

        # Identify the single worst variable for missingness. The variable name
        # and the percentage MUST come from the same row: reading the name from
        # high_missing_vars[1] (first variable over 50% in selection order) while
        # reading the percentage from the overall maximum used to name one
        # variable and report another one's number.
        miss_pcts <- vapply(summary_rows, function(r) {
            if (is.na(r$missing_pct)) 0 else r$missing_pct
        }, numeric(1))
        worst_idx <- if (length(miss_pcts) > 0) which.max(miss_pcts) else integer(0)
        worst_missing_var <- if (length(worst_idx) > 0) summary_rows[[worst_idx]]$variable else NA_character_
        max_missing_pct <- if (length(worst_idx) > 0) miss_pcts[worst_idx] else 0

        # Get duplicate info
        dup_count <- if (!is.null(duplicate_rows) && !is.na(duplicate_rows)) duplicate_rows else 0
        # Mirror the branch that actually ran in .run(): the row-level branch
        # requires more than one variable, otherwise the value-level branch runs.
        row_level_dupes <- isTRUE(self$options$complete_cases_only) && length(summary_rows) > 1
        dup_type <- if (row_level_dupes) "rows" else "values"

        # Count variables exceeding the user's own stated missingness tolerance
        vars_above_threshold <- sum(vapply(summary_rows, function(r) {
            !is.na(r$missing_pct) && r$missing_pct > threshold
        }, logical(1)))

        # Shared "no flags" criterion, used for both the prose assessment and the
        # colored status box below so the two never disagree. It deliberately
        # covers everything the Recommended Actions panel treats as a problem:
        # previously a dataset where every variable was 49% missing, or where
        # every row was a duplicate, was still called "Good" in a green box while
        # the panel immediately below it raised both issues.
        # Only ROW-level duplicates count as a flag. In value-level mode
        # duplicate_rows is sum(non-missing - unique) over the selected variables,
        # which is large and entirely expected for any categorical variable
        # (histopathology / Age+Sex+Grade: 693 in n=250 with nothing wrong), so
        # including it made the "no flags" verdict unreachable and let merely
        # ticking the duplicate check downgrade the verdict on identical data.
        no_flags_raised <- length(high_missing_vars) == 0 &&
            vars_above_threshold == 0 &&
            (!row_level_dupes || dup_count == 0) &&
            length(near_zero_vars) == 0 &&
            n_total >= 30

        # Determine overall assessment. This describes what the checks found, not
        # whether the data are fit for any particular purpose.
        overall_assessment <- if (no_flags_raised) {
            "No quality flags raised by the checks that were run"
        } else if (n_total < 20 || length(high_missing_vars) > 0) {
            "Quality flags raised - see Recommended Actions below"
        } else {
            "Minor quality flags raised - see Recommended Actions below"
        }

        summary_html <- paste0(
            "<div style='background-color: rgba(33, 159, 43, 0.1); padding: 20px; border-radius: 8px; border-left: 5px solid #4caf50; color: inherit;'>",
            "<h3 style='color: #2e7d32; margin-top: 0;'> Plain-Language Summary</h3>",

            "<div style='background-color: rgba(127, 127, 127, 0.08); color: inherit; padding: 15px; border-radius: 5px; margin-bottom: 15px;'>",
            "<p style='font-size: 1.1em; line-height: 1.6;'>",
            sprintf("Analyzed <strong>%d variable%s</strong> from <strong>%d observation%s</strong>. ",
                    n_vars_analyzed, if (n_vars_analyzed == 1) "" else "s",
                    n_total, if (n_total == 1) "" else "s"),
            "</p>",

            "<h4 style='color: #2e7d32; margin-top: 15px;'>Key Findings:</h4>",
            "<ul style='line-height: 1.8;'>",

            # Missing data summary. Emitted as ONE self-contained <li>: splitting
            # the opening tag and the "(highest: ...)" tail across two independent
            # conditions used to leak a bare fragment with an unmatched </li> into
            # the list whenever check_missing was off but a variable was >50%
            # missing.
            if (self$options$check_missing) {
                paste0(
                    sprintf("<li><strong>Missing Data:</strong> %d variable%s exceed%s %g%% missing threshold",
                            vars_above_threshold,
                            if (vars_above_threshold == 1) "" else "s",
                            if (vars_above_threshold == 1) "s" else "",
                            threshold),
                    if (length(high_missing_vars) > 0 && !is.na(worst_missing_var)) {
                        sprintf(" (highest: <em>%s</em> at %.1f%% missing)",
                                htmltools::htmlEscape(worst_missing_var), max_missing_pct)
                    } else {
                        ""
                    },
                    "</li>"
                )
            } else if (length(high_missing_vars) > 0 && !is.na(worst_missing_var)) {
                sprintf("<li><strong>Missing Data:</strong> highest missingness is <em>%s</em> at %.1f%%</li>",
                        htmltools::htmlEscape(worst_missing_var), max_missing_pct)
            } else {
                ""
            },

            # Duplicate summary
            if (self$options$check_duplicates) {
                sprintf("<li><strong>Duplicates:</strong> %d duplicate %s detected%s</li>",
                        dup_count, dup_type,
                        if (dup_count == 0) {
                            ""
                        } else if (row_level_dupes) {
                            " - review for data entry errors or valid repetitions"
                        } else {
                            # Repeated VALUES are expected whenever a variable has
                            # few levels, so the row-level "data entry error"
                            # framing does not apply here.
                            " - expected for categorical variables; only diagnostic for identifier variables"
                        })
            } else {
                ""
            },

            # Data quality flags
            if (length(near_zero_vars) > 0) {
                sprintf("<li><strong>Constant Variables:</strong> %d variable%s ha%s no variation at all (<em>%s</em>)</li>",
                        length(near_zero_vars),
                        if (length(near_zero_vars) == 1) "" else "s",
                        if (length(near_zero_vars) == 1) "s" else "",
                        paste(htmltools::htmlEscape(near_zero_vars), collapse = ", "))
            } else {
                ""
            },

            # Sample size assessment
            sprintf("<li><strong>Sample Size:</strong> n=%d ", n_total),
            if (n_total < 20) {
                "- very small, estimates may be unstable"
            } else if (n_total < 30) {
                "- small, use caution with complex analyses"
            } else if (n_total < 100) {
                "- adequate for basic analyses"
            } else {
                "- good for most statistical analyses"
            },
            "</li>",

            "</ul>",
            "</div>",

            # Overall assessment box. Translucent tints (they composite to the
            # former #d1f2eb / #fff3cd over a white ground) plus an explicit
            # color: inherit, so the text stays readable in jamovi's dark theme.
            sprintf(
                "<div style='background-color: %s; color: inherit; padding: 15px; border-radius: 5px; border-left: 4px solid %s;'>",
                if (no_flags_raised) "rgba(25, 190, 155, 0.2)" else "rgba(255, 195, 5, 0.2)",
                if (no_flags_raised) "#00695c" else "#ff8f00"
            ),
            "<p style='margin: 0; font-weight: bold;'>Overall Assessment: ", overall_assessment, "</p>",
            "</div>",

            "<p style='margin-top: 15px; font-size: 0.9em; color: inherit; opacity: 0.75;'>",
            "<em> This summary is written in plain language for clinical documentation. ",
            "Copy this text for inclusion in study reports, quality control logs, or data management plans.</em>",
            "</p>",

            "</div>"
        )

        self$results$summary$setContent(summary_html)
    },

    .generateRecommendations = function(summary_rows, n_total, high_missing_vars, near_zero_vars, duplicate_rows) {
        # Generate actionable recommendations for addressing quality issues

        recs_html <- paste0(
            "<div style='background-color: rgba(255, 169, 33, 0.14); padding: 20px; border-radius: 8px; border-left: 5px solid #ff8f00; color: inherit;'>",
            "<h3 style='color: #e65100; margin-top: 0;'> Recommended Actions</h3>",

            "<p style='font-size: 1.05em; margin-bottom: 20px;'>",
            "Based on the quality assessment, here are specific actions to improve your data before analysis:",
            "</p>"
        )

        has_recommendations <- FALSE

        # High missingness recommendations
        if (length(high_missing_vars) > 0) {
            has_recommendations <- TRUE
            recs_html <- paste0(recs_html,
                "<div style='background-color: rgba(127, 127, 127, 0.08); color: inherit; padding: 15px; border-radius: 5px; margin-bottom: 15px;'>",
                "<h4 style='color: #e65100; margin-top: 0;'> High Missingness (>50%)</h4>",
                "<p><strong>Variables affected:</strong> ", paste(htmltools::htmlEscape(high_missing_vars), collapse = ", "), "</p>",
                "<p><strong>Actions:</strong></p>",
                "<ol style='line-height: 1.8;'>",
                "<li><strong>Investigate root cause:</strong> Why is data missing? (not collected, measurement failure, data entry error)</li>",
                "<li><strong>Consider exclusion:</strong> Variables with >50% missing often provide limited information</li>",
                "<li><strong>If retaining, use imputation:</strong>",
                "<ul>",
                "<li>Multiple imputation (mice package): <code>mice::mice(data, m=5, method='pmm')</code></li>",
                "<li>Review Little's MCAR test when available, but do not infer MAR solely from this test</li>",
                "<li>Report imputation method and sensitivity analysis in your manuscript</li>",
                "</ul></li>",
                "<li><strong>Alternative:</strong> Restrict to complete cases but report potential selection bias</li>",
                "</ol>",
                "<p style='background-color: rgba(255, 202, 33, 0.23); padding: 10px; border-radius: 4px; margin-top: 10px; color: inherit;'>",
                "<strong> Warning:</strong> Listwise deletion (complete-case analysis) with >50% missing can severely bias results. ",
                "Consult a statistician if you're uncertain about the best approach.",
                "</p>",
                "</div>"
            )
        }

        # Moderate missingness (10-50%)
        moderate_missing_vars <- vapply(summary_rows, function(r) {
            if (!is.na(r$missing_pct) && r$missing_pct > 10 && r$missing_pct <= 50) r$variable else NA_character_
        }, character(1))
        moderate_missing_vars <- moderate_missing_vars[!is.na(moderate_missing_vars)]

        if (length(moderate_missing_vars) > 0) {
            has_recommendations <- TRUE
            recs_html <- paste0(recs_html,
                "<div style='background-color: rgba(127, 127, 127, 0.08); color: inherit; padding: 15px; border-radius: 5px; margin-bottom: 15px;'>",
                "<h4 style='color: #ff8f00; margin-top: 0;'> Moderate Missingness (10-50%)</h4>",
                "<p><strong>Variables affected:</strong> ", paste(htmltools::htmlEscape(moderate_missing_vars), collapse = ", "), "</p>",
                "<p><strong>Recommended approach:</strong></p>",
                "<ul style='line-height: 1.8;'>",
                "<li><strong>Preferred:</strong> Multiple imputation with sensitivity analysis</li>",
                "<li><strong>Complete-case analysis:</strong> Consider only when MCAR is substantively plausible; a non-significant Little's test does not prove MCAR</li>",
                "<li><strong>Report:</strong> Compare baseline characteristics between complete vs. incomplete cases</li>",
                "<li><strong>Document:</strong> State missingness mechanism and handling method in Methods section</li>",
                "</ul>",
                "</div>"
            )
        }

        # Duplicate recommendations
        dup_count <- if (!is.null(duplicate_rows) && !is.na(duplicate_rows)) duplicate_rows else 0
        if (dup_count > 0) {
            # Mirror the branch that actually ran in .run(): the row-level branch
            # requires more than one variable, otherwise value-level mode runs.
            row_level_dupes <- isTRUE(self$options$complete_cases_only) && length(summary_rows) > 1
            # Only row-level duplicates count as an issue, matching the "no flags"
            # criterion in .generateSummary(). Repeated VALUES are expected for any
            # low-cardinality variable, so they must not suppress the
            # "No Critical Issues Detected" block; the block below is still shown
            # because it explains what the value-level count does and does not mean.
            if (row_level_dupes)
                has_recommendations <- TRUE
            dup_type <- if (row_level_dupes) "duplicate rows" else "duplicate values"
            recs_html <- paste0(recs_html,
                "<div style='background-color: rgba(127, 127, 127, 0.08); color: inherit; padding: 15px; border-radius: 5px; margin-bottom: 15px;'>",
                "<h4 style='color: #e65100; margin-top: 0;'> ", dup_count, " ", dup_type, " Detected</h4>",
                "<p><strong>Actions:</strong></p>",
                "<ol style='line-height: 1.8;'>",
                if (row_level_dupes) {
                    paste0(
                        "<li><strong>Review patient identifiers:</strong> Check if duplicates represent same patient (data entry error) or different patients</li>",
                        "<li><strong>If same patient:</strong> Merge records, keeping most complete/recent data</li>",
                        "<li><strong>If different patients:</strong> Check for ID assignment errors</li>",
                        "<li><strong>Remove true duplicates:</strong> Use <code>dplyr::distinct()</code> after verification</li>"
                    )
                } else {
                    paste0(
                        "<li><strong>For categorical variables:</strong> High duplicates are normal (e.g., many patients with 'Male' gender)</li>",
                        "<li><strong>For continuous variables:</strong> Investigate if duplicates are biologically plausible</li>",
                        "<li><strong>For ID variables:</strong> Duplicates likely indicate data errors - review source data</li>"
                    )
                },
                "</ol>",
                "</div>"
            )
        }

        # Near-zero variance recommendations
        if (length(near_zero_vars) > 0) {
            has_recommendations <- TRUE
            recs_html <- paste0(recs_html,
                "<div style='background-color: rgba(127, 127, 127, 0.08); color: inherit; padding: 15px; border-radius: 5px; margin-bottom: 15px;'>",
                "<h4 style='color: #ff8f00; margin-top: 0;'> Constant (Zero-Variance) Variables</h4>",
                "<p><strong>Variables affected:</strong> ", paste(htmltools::htmlEscape(near_zero_vars), collapse = ", "), "</p>",
                "<p><strong>Actions:</strong></p>",
                "<ul style='line-height: 1.8;'>",
                "<li><strong>Exclude from models:</strong> Variables with no variation cannot predict outcomes</li>",
                "<li><strong>Investigate:</strong> Is lack of variation a data quality issue or a true population characteristic?</li>",
                "<li><strong>Consider:</strong> May still be useful for descriptive statistics or subgroup identification</li>",
                "<li><strong>Wider screen:</strong> This flag catches only variables with <em>no</em> variation. For the broader near-zero-variance case (very low but non-zero variation, which can destabilise models just as badly), use <code>caret::nearZeroVar()</code></li>",
                "</ul>",
                "</div>"
            )
        }

        # Small sample recommendations
        if (n_total < 20) {
            has_recommendations <- TRUE
            recs_html <- paste0(recs_html,
                "<div style='background-color: rgba(127, 127, 127, 0.08); color: inherit; padding: 15px; border-radius: 5px; margin-bottom: 15px;'>",
                "<h4 style='color: #e65100; margin-top: 0;'> Very Small Sample Size (n=", n_total, ")</h4>",
                "<p><strong>Critical limitations:</strong></p>",
                "<ul style='line-height: 1.8;'>",
                "<li><strong>Statistical power:</strong> Severely underpowered for most analyses</li>",
                "<li><strong>Model stability:</strong> Regression models may not converge or produce unreliable estimates</li>",
                "<li><strong>Generalizability:</strong> Results may not generalize beyond this specific sample</li>",
                "</ul>",
                "<p><strong>Recommended actions:</strong></p>",
                "<ol style='line-height: 1.8;'>",
                "<li><strong>Primary recommendation:</strong> Increase sample size if possible (target n\u{2265}30 minimum)</li>",
                "<li><strong>If sample size fixed:</strong>",
                "<ul>",
                "<li>Limit to descriptive statistics only</li>",
                "<li>Use exact tests instead of asymptotic (e.g., Fisher's exact vs. chi-square)</li>",
                "<li>Avoid multivariable regression (rule of thumb: need \u{2265}10 events per predictor)</li>",
                "<li>Consider case series or qualitative analysis instead</li>",
                "</ul></li>",
                "<li><strong>Reporting:</strong> Clearly state sample size limitation in Discussion section</li>",
                "</ol>",
                "</div>"
            )
        } else if (n_total < 30) {
            has_recommendations <- TRUE
            recs_html <- paste0(recs_html,
                "<div style='background-color: rgba(127, 127, 127, 0.08); color: inherit; padding: 15px; border-radius: 5px; margin-bottom: 15px;'>",
                "<h4 style='color: #ff8f00; margin-top: 0;'> Small Sample Size (n=", n_total, ")</h4>",
                "<p><strong>Recommendations:</strong></p>",
                "<ul style='line-height: 1.8;'>",
                "<li><strong>Prefer exact tests:</strong> Use exact methods when possible (Fisher's exact, permutation tests)</li>",
                "<li><strong>Limit model complexity:</strong> Restrict to \u{2264}", floor(n_total/10), " predictor variables in regression</li>",
                "<li><strong>Use cross-validation:</strong> LOOCV (leave-one-out) for model validation instead of train/test split</li>",
                "<li><strong>Report uncertainty:</strong> Always include confidence intervals, not just p-values</li>",
                "<li><strong>Consider pilot study:</strong> Frame results as preliminary findings requiring validation</li>",
                "</ul>",
                "</div>"
            )
        }

        # If no issues detected
        if (!has_recommendations) {
            recs_html <- paste0(recs_html,
                "<div style='background-color: rgba(33, 192, 159, 0.21); padding: 15px; border-radius: 5px; color: inherit;'>",
                "<h4 style='color: inherit; margin-top: 0;'> No Critical Issues Detected</h4>",
                "<p style='line-height: 1.8;'>",
                "Your data quality appears acceptable for analysis. However, always:",
                "</p>",
                "<ul style='line-height: 1.8;'>",
                "<li>Check assumptions specific to your planned analysis (normality, homoscedasticity, etc.)</li>",
                "<li>Visualize distributions and relationships before modeling</li>",
                "<li>Screen for outliers that may influence results</li>",
                "<li>Document any data transformations or exclusions in your analysis plan</li>",
                "</ul>",
                "</div>"
            )
        }

        recs_html <- paste0(recs_html,
            "<p style='margin-top: 20px; font-size: 0.9em; color: inherit; opacity: 0.75;'>",
            "<em> These recommendations are based on general statistical best practices. ",
            "Consult with a biostatistician for guidance specific to your research question and study design.</em>",
            "</p>",
            "</div>"
        )

        self$results$recommendations$setContent(recs_html)
    },

    .generateExplanations = function() {
        # Generate educational explanations of quality metrics

        expl_html <- paste0(
            "<div style='background-color: rgba(33, 152, 239, 0.13); padding: 20px; border-radius: 8px; border-left: 5px solid #1976d2; color: inherit;'>",
            "<h3 style='color: inherit; margin-top: 0;'> Understanding Quality Metrics</h3>",

            "<p style='font-size: 1.05em; margin-bottom: 20px;'>",
            "This guide explains the quality metrics used in this analysis and how to interpret them.",
            "</p>",

            # Missing Data section
            "<div style='background-color: rgba(127, 127, 127, 0.08); color: inherit; padding: 15px; border-radius: 5px; margin-bottom: 15px;'>",
            "<h4 style='color: #1976d2; margin-top: 0;'>Missing Data Analysis</h4>",

            "<p><strong>What it measures:</strong> Percentage of observations with missing values for each variable.</p>",

            "<p><strong>Interpretation guidelines:</strong></p>",
            "<ul style='line-height: 1.8;'>",
            "<li><strong>&lt;5% missing:</strong> Excellent - minimal impact on analysis</li>",
            "<li><strong>5-10% missing:</strong> Acceptable - document and address appropriately</li>",
            "<li><strong>10-20% missing:</strong> Moderate concern - may require imputation</li>",
            "<li><strong>&gt;20% missing:</strong> Serious concern - results may be biased</li>",
            "<li><strong>&gt;50% missing:</strong> Critical - consider excluding variable</li>",
            "</ul>",

            "<p><strong>Little's MCAR Test:</strong></p>",
            "<ul style='line-height: 1.8;'>",
            "<li><strong>What it tests:</strong> Whether missing data is completely random (MCAR) vs. systematic (MAR/MNAR)</li>",
            "<li><strong>Scope:</strong> The test is computed on the selected <em>numeric</em> variables only; missingness in factor, text and date variables is reported in the table above but is not part of the test</li>",
            "<li><strong>Assumptions:</strong> Multivariate normality of those variables within each missing-data pattern; the test also has little power when patterns contain few cases, which is common in small clinical series</li>",
            "<li><strong>Interpretation:</strong>",
            "<ul>",
            "<li>p > 0.05: The test does not reject MCAR; it does not prove MCAR</li>",
            "<li>p \u{2264} 0.05: The data provide evidence against MCAR; use caution with complete-case analysis</li>",
            "</ul></li>",
            "<li><strong>Clinical relevance:</strong> Even when MCAR is plausible, deleting cases reduces power and should be reported</li>",
            "</ul>",
            "</div>",

            # Duplicate Detection section
            "<div style='background-color: rgba(127, 127, 127, 0.08); color: inherit; padding: 15px; border-radius: 5px; margin-bottom: 15px;'>",
            "<h4 style='color: #1976d2; margin-top: 0;'>Duplicate Detection</h4>",

            "<p><strong>Two types checked:</strong></p>",
            "<ul style='line-height: 1.8;'>",
            "<li><strong>Duplicate rows:</strong> Identical combinations across all selected variables (may indicate data entry errors or repeated measurements)</li>",
            "<li><strong>Duplicate values:</strong> Repeated values within each variable (normal for categorical data, unusual for IDs)</li>",
            "</ul>",

            "<p><strong>When duplicates are concerning:</strong></p>",
            "<ul style='line-height: 1.8;'>",
            "<li>Patient ID variables should have ~0% duplicates</li>",
            "<li>Exact matches across many variables may indicate copy-paste errors</li>",
            "<li>Unexpected patterns (e.g., same tumor size for multiple patients)</li>",
            "</ul>",

            "<p><strong>When duplicates are normal:</strong></p>",
            "<ul style='line-height: 1.8;'>",
            "<li>Categorical variables (Gender, Stage, etc.) expected to have many duplicates</li>",
            "<li>Rounded measurements (e.g., age in years)</li>",
            "<li>Binary outcomes (yes/no, positive/negative)</li>",
            "</ul>",
            "</div>",

            # Constant / zero-variance section
            "<div style='background-color: rgba(127, 127, 127, 0.08); color: inherit; padding: 15px; border-radius: 5px; margin-bottom: 15px;'>",
            "<h4 style='color: #1976d2; margin-top: 0;'>Constant (Zero-Variance) Variables</h4>",

            "<p><strong>What it means:</strong> Every observation of the variable has the same value, so its standard deviation is zero.</p>",

            "<p><strong>What this check does NOT cover:</strong> variables with very low but non-zero variation (\"near-zero variance\"). Those can destabilise a model just as badly but are not flagged here, because any absolute cut-off on the standard deviation depends on the measurement scale - the same quantity in metres and millimetres differs by a factor of 1000. Use caret::nearZeroVar(), which screens on the frequency ratio and percent-unique instead, if you need that wider check.</p>",

            "<p><strong>Why it matters:</strong></p>",
            "<ul style='line-height: 1.8;'>",
            "<li>Cannot predict outcomes if predictor doesn't vary</li>",
            "<li>May cause numerical instability in regression models</li>",
            "<li>Often indicates all patients have same value (e.g., all 'Stage IV')</li>",
            "</ul>",

            "<p><strong>Actions:</strong></p>",
            "<ul style='line-height: 1.8;'>",
            "<li><strong>For analysis:</strong> Exclude from regression models</li>",
            "<li><strong>For reporting:</strong> State as constant in descriptive statistics</li>",
            "<li><strong>For study design:</strong> May indicate homogeneous sample (affects generalizability)</li>",
            "</ul>",
            "</div>",

            # High Cardinality section
            "<div style='background-color: rgba(127, 127, 127, 0.08); color: inherit; padding: 15px; border-radius: 5px; margin-bottom: 15px;'>",
            "<h4 style='color: #1976d2; margin-top: 0;'>High Cardinality</h4>",

            "<p><strong>Definition:</strong> Variable has >50 unique values AND these represent >50% of observations. ",
            "This flag is applied to categorical and text variables only - many distinct values are expected and normal for a continuous measurement, ",
            "so numeric variables are deliberately never flagged and the High card column reads FALSE for all of them.</p>",

            "<p><strong>Examples that are flagged:</strong></p>",
            "<ul style='line-height: 1.8;'>",
            "<li>Patient ID stored as text (each patient unique) - very high cardinality</li>",
            "<li>Free-text fields such as a diagnosis comment or specimen description</li>",
            "<li>A site or surgeon code with almost as many levels as patients</li>",
            "</ul>",

            "<p><strong>Implications:</strong></p>",
            "<ul style='line-height: 1.8;'>",
            "<li><strong>For categorical variables:</strong> May need to collapse categories (e.g., group age into bands)</li>",
            "<li><strong>For continuous variables:</strong> Normal and expected - a tumour size in mm or an age in years has many distinct values by construction, which is why the flag is not raised for them</li>",
            "<li><strong>For factors in regression:</strong> High cardinality increases parameters and reduces power</li>",
            "</ul>",
            "</div>",

            # Outliers section
            "<div style='background-color: rgba(127, 127, 127, 0.08); color: inherit; padding: 15px; border-radius: 5px; margin-bottom: 15px;'>",
            "<h4 style='color: #1976d2; margin-top: 0;'>Outlier Detection (IQR Method)</h4>",

            "<p><strong>Method used:</strong> Tukey's IQR (Interquartile Range) rule</p>",

            "<p><strong>Formula:</strong></p>",
            "<ul style='line-height: 1.8;'>",
            "<li>Lower bound = Q1 - 1.5 \u{D7} IQR</li>",
            "<li>Upper bound = Q3 + 1.5 \u{D7} IQR</li>",
            "<li>Values outside these bounds flagged as outliers</li>",
            "</ul>",

            "<p><strong>Interpretation:</strong> judge the <em>proportion</em> (%Outliers column), not the raw count. ",
            "Under this rule roughly 0.7% of observations drawn from a normal distribution fall outside the fences at any sample size (the expected proportion is the same, but the observed percentage is estimated imprecisely when n is small), ",
            "so about 7 flagged values in n=1000 is exactly what a clean variable looks like, while 3 flagged values in n=15 is 20% of the data.</p>",
            "<ul style='line-height: 1.8;'>",
            "<li><strong>Up to about 1%:</strong> expected from a normal distribution; no signal</li>",
            "<li><strong>1-5%:</strong> suggests mild skew or heavier tails than normal</li>",
            "<li><strong>Above 5%:</strong> suggests marked skew, a mixture of subpopulations, or a coding problem (e.g. 999 entered as a missing indicator)</li>",
            "<li><strong>NA in both columns:</strong> the variable is non-numeric, or has 10 or fewer non-missing values, so the rule was not applied to it</li>",
            "</ul>",

            "<p><strong>Actions:</strong></p>",
            "<ul style='line-height: 1.8;'>",
            "<li><strong>Don't automatically delete outliers</strong> - may represent true biological variation</li>",
            "<li>Verify against source data for transcription errors</li>",
            "<li>Consider robust statistical methods (median-based, trimmed means)</li>",
            "<li>Run sensitivity analysis with/without outliers</li>",
            "</ul>",
            "</div>",

            # Sample Size section
            "<div style='background-color: rgba(127, 127, 127, 0.08); color: inherit; padding: 15px; border-radius: 5px; margin-bottom: 15px;'>",
            "<h4 style='color: #1976d2; margin-top: 0;'>Sample Size Guidelines</h4>",

            "<p><strong>General rules of thumb:</strong></p>",
            "<table border='1' cellspacing='0' cellpadding='8' style='width:100%; border-collapse: collapse;'>",
            "<tr style='background-color: rgba(88, 88, 88, 0.06); color: inherit;'>",
            "<th>Sample Size</th><th>Analysis Type</th><th>Recommendation</th>",
            "</tr>",
            "<tr><td>n &lt; 20</td><td>Any</td><td>Descriptive only; avoid inference</td></tr>",
            "<tr><td>n = 20-30</td><td>Basic</td><td>Simple comparisons; exact tests</td></tr>",
            "<tr><td>n = 30-100</td><td>Standard</td><td>Most analyses acceptable; limit predictors</td></tr>",
            "<tr><td>n = 100-500</td><td>Multivariable</td><td>Regression with multiple predictors OK</td></tr>",
            "<tr><td>n &gt; 500</td><td>Advanced</td><td>Machine learning, complex models feasible</td></tr>",
            "</table>",

            "<p style='margin-top: 15px;'><strong>Events per variable (EPV) rule:</strong></p>",
            "<ul style='line-height: 1.8;'>",
            "<li><strong>Minimum:</strong> 10 events per predictor variable in regression</li>",
            "<li><strong>Example:</strong> For binary outcome with 50 events, limit to 5 predictors</li>",
            "<li><strong>Survival analysis:</strong> Need 10 deaths/events per covariate in Cox model</li>",
            "</ul>",
            "</div>",

            # Visual exploration section
            if (self$options$plot_data_overview || self$options$plot_missing_patterns || self$options$plot_data_types) {
                paste0(
                    "<div style='background-color: rgba(127, 127, 127, 0.08); color: inherit; padding: 15px; border-radius: 5px; margin-bottom: 15px;'>",
                    "<h4 style='color: #1976d2; margin-top: 0;'>Visual Data Exploration (visdat)</h4>",

                    "<p><strong>Package background:</strong> visdat provides visual exploratory data analysis based on research published in the R Journal (2019).</p>",

                    "<p><strong>Plot interpretations:</strong></p>",
                    "<ul style='line-height: 1.8;'>",
                    if (self$options$plot_data_overview) {
                        "<li><strong>Data Overview (vis_dat):</strong> Shows data types and missing patterns in matrix format. Each row = observation, each column = variable. Colors indicate data type. Gray = missing.</li>"
                    } else {
                        ""
                    },
                    if (self$options$plot_missing_patterns) {
                        "<li><strong>Missing Patterns (vis_miss):</strong> Highlights missing data patterns. Variables sorted by missingness. Red bands indicate variables exceeding threshold. Look for systematic patterns (MAR) vs. random scatter (MCAR).</li>"
                    } else {
                        ""
                    },
                    if (self$options$plot_data_types) {
                        "<li><strong>Data Types (vis_guess):</strong> Shows R's guess at appropriate data type. Useful for validating that character variables should be factors, numeric variables aren't accidentally stored as text, etc.</li>"
                    } else {
                        ""
                    },
                    "</ul>",

                    "<p><strong>Clinical applications:</strong></p>",
                    "<ul style='line-height: 1.8;'>",
                    "<li>Quickly spot data collection issues (e.g., missing Stage for all patients after certain date)</li>",
                    "<li>Identify variables that should be recoded (e.g., '999' used as missing indicator)</li>",
                    "<li>Verify data types match intended analysis (factors for categorical, numeric for continuous)</li>",
                    "</ul>",
                    "</div>"
                )
            } else {
                ""
            },

            # Footer
            "<p style='margin-top: 20px; font-size: 0.9em; color: inherit; opacity: 0.75;'>",
            "<em> These explanations provide general guidance for clinical researchers. ",
            "For detailed statistical consultation, work with a biostatistician familiar with your research domain.</em>",
            "</p>",

            "</div>"
        )

        self$results$explanations$setContent(expl_html)
    }

    )
)
