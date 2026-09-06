#' @title Table One
#'
#' @description This function creates a "Table One" - a descriptive summary table commonly used in clinicopathological research.
#' It offers several output styles using different packages (tableone, gtsummary, arsenal, and janitor).
#'
#' @return A results object; see the Value section of the generated tableone() documentation.
#'
#' @importFrom R6 R6Class
#' @importFrom jmvcore . select naOmit constructFormula
#' @importFrom tableone CreateTableOne
#' @importFrom gtsummary tbl_summary as_kable_extra
#' @importFrom arsenal tableby
#' @importFrom janitor tabyl adorn_totals adorn_pct_formatting
#' @importFrom kableExtra kable kable_styling
#' @importFrom rlang sym
#' @importFrom magrittr %>%

#'
tableoneClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "tableoneClass",
    inherit = tableoneBase,
    private = list(
        .categoryLabels = function(data) {
            unlist(lapply(data, function(value) {
                if (is.factor(value)) levels(value)
                else if (is.character(value)) value
                else character()
            }), use.names = FALSE)
        },

        .uniqueSummaryLabel = function(label, existing) {
            candidate <- label
            suffix <- 1L
            while (candidate %in% existing) {
                candidate <- paste0(label, " (", suffix, ")")
                suffix <- suffix + 1L
            }
            candidate
        },

        .formatText = function(template, ...) {
            # Substitute only template tokens, never braces/backslashes in user
            # values: jmvcore::format recursively scans inserted values.
            values <- list(...)
            matches <- gregexpr("\\{[A-Za-z][A-Za-z0-9]*\\}", template)[[1]]
            if (matches[1] == -1L)
                return(template)
            widths <- attr(matches, "match.length")
            pieces <- character()
            start <- 1L
            for (i in seq_along(matches)) {
                end <- matches[i] + widths[i] - 1L
                key <- substr(template, matches[i] + 1L, end - 1L)
                # A translated msgstr that renames a placeholder ({N} for {n})
                # must not abort the whole analysis. The msgid is not available
                # here, so keep the unknown token literal: the sentence still
                # reads, and the stray {N} points at the catalogue line to fix.
                replacement <- if (key %in% names(values))
                    paste(as.character(values[[key]]), collapse = ", ")
                else
                    substr(template, matches[i], end)
                pieces <- c(pieces, substr(template, start, matches[i] - 1L),
                            replacement)
                start <- end + 1L
            }
            paste0(c(pieces, substring(template, start)), collapse = "")
        },

        .htmlSafeTableData = function(data) {
            escape <- function(value) {
                as.character(htmltools::htmlEscape(as.character(value)))
            }

            for (i in seq_along(data)) {
                value <- data[[i]]
                if (is.factor(value)) {
                    # Escaping must not change codes or missingness (levels<-
                    # removes actual NA levels, unlike attribute assignment).
                    attr(value, "levels") <- escape(levels(value))
                } else if (is.character(value)) {
                    value[] <- escape(value)
                }

                label <- attr(value, "label", exact = TRUE)
                if (is.null(label))
                    label <- names(data)[i]
                attr(value, "label") <- escape(label)

                units <- attr(value, "units", exact = TRUE)
                if (is.character(units))
                    attr(value, "units") <- escape(units)

                value_labels <- attr(value, "labels", exact = TRUE)
                if (!is.null(value_labels) && !is.null(names(value_labels))) {
                    names(value_labels) <- escape(names(value_labels))
                    attr(value, "labels") <- value_labels
                }

                data[[i]] <- value
            }

            data
        },

        # jmvcore::reject() routes its first argument through jmvcore's
        # {}-substitution formatter, which replaces any {token} it does not
        # recognise with a horizontal ellipsis. A column literally named
        # "Ki67 {%}", or a third-party error message containing braces, would
        # therefore reach the user as "..." with the content lost. Escaping is not
        # available - the formatter re-scans substituted values - so neutralise the
        # braces before handing the message over.
        .rejectPlain = function(message) {
            jmvcore::reject(gsub("}", ")", gsub("{", "(", message, fixed = TRUE), fixed = TRUE))
        },

        .run = function() {
            # NOTE: This function uses HTML outputs for messages instead of jmvcore::Notice
            # due to serialization constraints in jamovi's protobuf system.
            # When jamovi framework supports Notice serialization, migrate to:
            # - NoticeType$ERROR for missing data/variables
            # - NoticeType$STRONG_WARNING for data quality issues
            # - NoticeType$WARNING for recommendations
            # - NoticeType$INFO for confirmations
            # Restored Html content survives clearWith invalidation. Clear it
            # explicitly, including optional outputs, before any early return.
            private$.clearOutputs()
            private$.setAboutContent()

            # Check that the input data has at least one row.
            if (is.null(self$data) || nrow(self$data) == 0) {
                self$results$todo$setContent(paste0(
                    "<br><strong>", .("No Data Available"), "</strong><ul><li>",
                    .("Please load a dataset before using Table One."), "</li><li>",
                    .("Check that your data file is properly imported."), "</li></ul>"))
                private$.setAssumptionsSkipped(
                    .("No data quality check was performed because the dataset has no rows."))
                return(invisible(NULL))
            }

            # If no variables are selected, show a welcome/instructions message.
            if (length(self$options$vars) == 0L) {
                self$results$todo$setContent(private$.buildWelcomeMessage())
                return(invisible(NULL))  # Stop further processing until variables are selected.
            } else {
                # Clear the instructions message once variables are selected.
                self$results$todo$setContent("")
            }

            private$.checkpoint()
            selection <- private$.prepareVariables(self$options$vars)
            if (is.null(selection))
                return(invisible(NULL))
            cohort <- private$.prepareCohort(selection$vars, selection$todo_html)
            if (is.null(cohort))
                return(invisible(NULL))

            # Only publish reports after the selected engine renders successfully.
            if (private$.renderTable(cohort$data, selection$vars,
                                     selection$frequency_skipped)) {
                private$.populateReports(cohort, selection$vars)
            }
        },

        .prepareVariables = function(selected_vars) {
            # Normalize actual NA factor levels before any engine or exclusion.
            # levels<- removes the NA level and marks its entries as missing;
            # literal text levels "NA"/"Unknown" and unused levels are preserved.
            normalized_vars <- Filter(function(v) {
                value <- self$data[[v]]
                is.factor(value) && anyNA(levels(value))
            }, selected_vars)
            for (v in normalized_vars) {
                value <- self$data[[v]]
                levels(value) <- levels(value)
                private$.data[[v]] <- value
            }
            todo_html <- if (length(normalized_vars) > 0L) paste0(
                "<p>", private$.formatText(
                    .("Actual NA factor levels were treated as missing before exclusion and tabulation: {variables}. Literal text categories such as NA or Unknown are unchanged."),
                    variables = paste(htmltools::htmlEscape(normalized_vars), collapse = "; ")),
                "</p>") else ""
            self$results$todo$setContent(todo_html)
            # A variable that is entirely NA is dropped before it ever reaches the
            # table, and the only trace was an R console warning that a jamovi user
            # never sees: they selected 8 variables, got 7 rows, and nothing said
            # why. Table One row counts get transcribed into manuscripts, so an
            # absent row has to be accounted for. Check the source column directly
            # rather than diffing names against the post-select frame, because
            # jmvcore::select may also rename columns.
            all_na_vars <- Filter(function(v) {
                col <- self$data[[v]]
                !is.null(col) && length(col) > 0L && all(is.na(col))
            }, selected_vars)

            if (length(all_na_vars) > 0) {
                # Drop them here, not just report them. The claim made below - that
                # the variable does not appear in the table - was only true for the
                # tableone style: gtsummary rendered a row of NAs, arsenal rendered
                # "N-Miss" with a blank mean, and janitor had nothing to count and
                # aborted the whole analysis. Dropping first makes the sentence true
                # for every style, and stops one never-collected lab value from
                # wiping out every other row via listwise deletion below.
                selected_vars <- setdiff(selected_vars, all_na_vars)
                todo_html <- paste0(todo_html,
                    "<div style='background: rgba(255, 202, 33, 0.23); color: inherit;border-left:4px solid #ffc107;",
                    "padding:10px;margin:10px 0;'><b>",
                    .("Not included"), ":</b> ",
                    paste(vapply(all_na_vars, htmltools::htmlEscape, character(1)),
                          collapse = "; "),
                    ". ",
                    .("Every value of this variable is missing, so it cannot be summarised and does not appear in the table below."),
                    "</div>")
                self$results$todo$setContent(todo_html)
            }

            # Validate storage classes before exclusion. Dates/time durations and
            # custom storage classes have incompatible meanings across engines.
            supported_classes <- c("numeric", "integer", "factor", "ordered",
                                   "character", "logical")
            unsupported_vars <- Filter(function(v) {
                value <- self$data[[v]]
                !is.null(dim(value)) || !all(class(value) %in% supported_classes)
            }, selected_vars)
            if (length(unsupported_vars) > 0L) {
                selected_vars <- setdiff(selected_vars, unsupported_vars)
                details <- vapply(unsupported_vars, function(v) {
                    paste0(v, " (", paste(class(self$data[[v]]), collapse = "/"), ")")
                }, character(1))
                todo_html <- paste0(todo_html,
                    "<div style='background: rgba(255, 202, 33, 0.23); color: inherit;",
                    "border-left:4px solid #ffc107;padding:10px;margin:10px 0;'><b>",
                    .("Not included"), ":</b> ",
                    paste(htmltools::htmlEscape(details), collapse = "; "), ". ",
                    .("Unsupported storage type. Use numeric measurements, factors (including ordered factors), text or logical variables. Convert dates or time intervals to explicitly defined measurements before analysis. Omitted variables do not enter missing-value exclusion or supplementary summaries."),
                    "</div>")
                self$results$todo$setContent(todo_html)
            }

            if (length(selected_vars) == 0) {
                self$results$todo$setContent(paste0(
                    todo_html,
                    "<div style='background: rgba(255, 202, 33, 0.23); color: inherit;border-left:4px solid #ffc107;",
                    "padding:10px;margin:10px 0;'><b>",
                    .("Nothing to summarise"), ":</b> ",
                    .("No selected variable has recorded values in a supported storage type. Select at least one numeric, categorical, text or logical variable with recorded values."),
                    "</div>"))
                private$.setAssumptionsSkipped(
                    .("No data quality check was performed because no selected variable could be summarised; the Instructions panel above explains why."))
                return(invisible(NULL))
            }

            # Decide which variables can be tabulated BEFORE listwise deletion and
            # supplementary reporting. Missingness in an omitted measurement must
            # not remove cases from a categorical frequency table.
            frequency_skipped <- list()
            if (identical(self$options$sty, "t4")) {
                frequency_vars <- character()
                for (var in selected_vars) {
                    value <- self$data[[var]]
                    n_distinct <- length(unique(value[!is.na(value)]))
                    reason <- if (!is.factor(value) && !is.character(value) &&
                                  !is.logical(value)) {
                        .("not categorical; set category codes to Nominal or Ordinal, or use factor() in R")
                    } else if (n_distinct > 20) {
                        private$.formatText(.("{n} distinct values; maximum 20 categories"), n = n_distinct)
                    } else NULL
                    if (is.null(reason)) {
                        frequency_vars <- c(frequency_vars, var)
                    } else {
                        frequency_skipped[[length(frequency_skipped) + 1L]] <-
                            sprintf("%s (%s)", htmltools::htmlEscape(var),
                                    htmltools::htmlEscape(reason))
                    }
                }
                selected_vars <- frequency_vars
                if (length(selected_vars) == 0L) {
                    self$results$tablestyle4$setContent(
                        private$.frequencySkipHtml(frequency_skipped))
                    private$.setAssumptionsSkipped(
                        .("No data quality check was performed because no selected variable could be tabulated in the janitor style; the Frequency Tables panel below explains why."))
                    return(invisible(NULL))
                }
            }

            list(vars = selected_vars, todo_html = todo_html,
                 frequency_skipped = frequency_skipped)
        },

        .prepareCohort = function(selected_vars, todo_html) {
            # jmvcore::select() restores the original column names on the way out
            # (colnames(data) <- names(out)) and jamovi has already validated every
            # entry of `vars` against the dataset, so names(data) is exactly
            # selected_vars - no separate "actual" name vector is needed.
            data <- jmvcore::select(self$data, selected_vars)

            # Capture original data stats BEFORE naOmit so the summary reports
            # actual missingness rather than post-exclusion stats.
            original_data <- data
            original_n <- nrow(original_data)
            original_complete <- sum(complete.cases(original_data))

            # Optionally exclude rows with missing values.
            excluded_n <- 0
            if (isTRUE(self$options$excl)) {
                data <- jmvcore::naOmit(data)
                excluded_n <- original_n - nrow(data)
            }

            # Listwise deletion over several variables can empty the frame. Each
            # engine then dies with its own jargon - tableone "No valid variables",
            # gtsummary "Expecting `data` argument to have at least 1 row and 1
            # column", arsenal "No (non-missing) observations" - none of which names
            # the cause. Return rather than reject() so the About panel survives.
            if (nrow(data) == 0) {
                self$results$todo$setContent(paste0(
                    todo_html,
                    "<div style='background: rgba(255, 202, 33, 0.23); color: inherit;border-left:4px solid #ffc107;",
                    "padding:10px;margin:10px 0;'><b>",
                    .("No cases left"), ":</b> ",
                    private$.formatText(.("Excluding missing values left no cases: each of the {n} cases has at least one missing value among the selected variables. Untick Missing-value exclusion (NA), or select fewer variables."),
                                        n = original_n),
                    "</div>"))
                private$.setAssumptionsSkipped(
                    .("No data quality check was performed because missing-value exclusion left no cases; the Instructions panel above explains why."))
                return(invisible(NULL))
            }

            list(data = data, original_data = original_data,
                 original_complete = original_complete, excluded_n = excluded_n)
        },

        .populateReports = function(cohort, selected_vars) {
            data <- cohort$data
            original_data <- cohort$original_data
            original_complete <- cohort$original_complete
            excluded_n <- cohort$excluded_n
            # Visibility of summary / about / reportSentence is declared in
            # jamovi/tableone.r.yaml as visible: (showSummary) etc. Calling
            # setVisible() here would overwrite that expression with a literal
            # TRUE/FALSE for the lifetime of the analysis instance, so the panel
            # would stop following its own checkbox after any early return.

            # Generate clinical summaries and data quality checks.
            # Only populate if shown (saves computation).
            if (isTRUE(self$options$showSummary)) {
                private$.generateSummary(data, selected_vars, original_data, excluded_n, original_complete)
            }

            if (isTRUE(self$options$showReportSentence)) {
                private$.setReportSentence(data, selected_vars, original_data, excluded_n, original_complete)
            }

            private$.checkDataQuality(data, selected_vars, original_data, original_complete)
        },

        .clearOutputs = function() {
            for (name in c("todo", "tablestyle1", "tablestyle2", "tablestyle3",
                           "tablestyle4", "reportSentence", "summary", "about",
                           "assumptions")) {
                self$results[[name]]$setContent("")
            }
        },

        .renderTable = function(data, selected_vars, frequency_skipped) {
            table_style <- self$options$sty

            # Generate the table based on the chosen style.
            if (table_style == "t1") {
                # --- Using tableone package ---
                # Checkpoint before expensive statistical computation
                private$.checkpoint()

                mytable <- tryCatch({
                    tableone::CreateTableOne(data = data)
                }, error = function(e) {
                    if (grepl("insufficient", tolower(e$message))) {
                        private$.rejectPlain(.("Insufficient data for Table One analysis. Check for missing values. Try selecting different variables or disabling missing-value exclusion."))
                    } else {
                        private$.rejectPlain(private$.formatText(
                            .("Error creating Table One: {error}. Check that variables have valid data and appropriate types. Categorical variables should be factors. Numeric variables should contain valid numbers."),
                            error = sub("\\.+$", "", conditionMessage(e))))
                    }
                })

                # Checkpoint after expensive operation to allow UI update
                private$.checkpoint()

                # Render with missing = TRUE rather than handing the object over for
                # jamovi to print with defaults. Without it the table showed
                # "n 120" beside a mean computed on 112 values, with nothing to say
                # 8 were missing - the reader has no way to tell the denominator of
                # each row apart from the overall n. The gtsummary and arsenal
                # styles already disclose this ("Unknown", "N-Miss"); the DEFAULT
                # style was the one that did not.
                # print.TableOne defaults to mean (SD) for every continuous
                # variable. Ki-67 index, tumour size and CA-125 are right-skewed,
                # so offer the median [Q1, Q3] form the option description promises
                # (print.TableOne labels that row "median [IQR]" but the brackets
                # hold the two quartiles, verified on tableone 0.13.2).
                # Names that are not continuous are ignored by print.TableOne.
                nonnormal_vars <- if (isTRUE(self$options$nonnormal)) names(data) else NULL

                render_error <- NULL
                rendered <- tryCatch(
                    paste(utils::capture.output(
                        print(mytable, printToggle = TRUE, quote = FALSE,
                              noSpaces = TRUE, missing = TRUE,
                              nonnormal = nonnormal_vars)
                    ), collapse = "\n"),
                    error = function(e) { render_error <<- conditionMessage(e); NULL })

                # The old fallback handed the raw TableOne LIST to a Preformatted
                # output, which renders as garbage. Say what failed instead.
                if (is.null(rendered))
                    private$.rejectPlain(private$.formatText(
                        .("The Table One summary was computed but could not be formatted for display: {error}. Try another table style, or deselect variables with unusual storage types."),
                        error = sub("\\.+$", "", render_error)))

                # The "Missing" column that missing = TRUE adds holds PERCENTAGES,
                # not counts, and carries no unit on screen - readers transcribe
                # "0.4" into manuscripts as 0.4 cases. Preformatted has no
                # setNote(), so the legend has to be part of the content.
                legend <- paste0(
                    "\n\n",
                    .("Missing = percentage of cases with a missing value for that variable."),
                    "\n",
                    if (isTRUE(self$options$nonnormal))
                        # Square brackets are NOT usable here. jmvcore's
                        # Translator treats a trailing " [...]" as a gettext
                        # msgctxt marker and, on a catalogue miss, returns only
                        # what precedes it - this string lost 97 of its 137
                        # characters on screen. Parentheses round-trip intact,
                        # and match the wording of the option title.
                        .("Continuous variables are shown as median (Q1, Q3); categorical variables as N (percent of cases with a recorded value for that variable).")
                    else
                        .("Continuous variables are shown as mean (SD); categorical variables as N (percent of cases with a recorded value for that variable)."))

                self$results$tablestyle1$setContent(paste0(rendered, legend))

            } else if (table_style == "t2") {
                # --- Using gtsummary package ---
                # Checkpoint before expensive gtsummary computation
                private$.checkpoint()

                mytable <- tryCatch({
                    # No .htmlSafeTableData() here: as_kable_extra() routes through
                    # knitr::kable(escape = TRUE), which already escapes factor
                    # levels and variable labels (a level "<img src=x onerror=...>"
                    # comes out as "&lt;img ..."). Pre-escaping ran it twice, so the
                    # Ki-67 cut-off level "<20%" reached the reader as the literal
                    # text "&lt;20%". The arsenal path below still needs the call:
                    # arsenal's summary(text = "html") emits levels verbatim.
                    categories <- private$.categoryLabels(data)
                    unknown_label <- .("Unknown")
                    missing_label <- if (unknown_label %in% categories)
                        private$.uniqueSummaryLabel(.("Missing (NA)"), categories)
                    else unknown_label
                    tbl <- gtsummary::tbl_summary(data = data, missing_text = missing_label)
                    # Single-row dichotomous summaries must name the counted
                    # level, including TRUE when every observed value is FALSE.
                    counted <- tbl$inputs$value
                    tbl <- gtsummary::modify_table_body(tbl, function(body) {
                        rows <- which(body$var_type == "dichotomous" &
                                      body$row_type == "label")
                        for (i in rows) {
                            value <- counted[[body$variable[i]]]
                            body$label[i] <- paste0(body$label[i], " = ", as.character(value))
                        }
                        body
                    })
                    gtsummary::as_kable_extra(tbl)
                }, error = function(e) {
                    private$.rejectPlain(private$.formatText(
                        .("Error creating gtsummary table: {error}. Check that variables have valid data and appropriate types."),
                        error = conditionMessage(e)))
                })

                # Checkpoint after expensive operation to allow UI update
                private$.checkpoint()
                self$results$tablestyle2$setContent(mytable)

            } else if (table_style == "t3") {
                # --- Using arsenal package ---
                # Checkpoint before expensive arsenal computation
                private$.checkpoint()

                formula_str <- jmvcore::constructFormula(terms = selected_vars)
                formula_obj <- jmvcore::asFormula(paste('~', formula_str))
                mytable <- tryCatch({
                    arsenal_data <- private$.htmlSafeTableData(data)
                    if (!identical(is.na(arsenal_data), is.na(data))) {
                        private$.rejectPlain(.("Formatting changed missing values; no table or report was produced."))
                        return(invisible(NULL))
                    }
                    categories <- private$.categoryLabels(data)
                    nmiss_label <- .("N-Miss")
                    stats_labels <- list(Nmiss = if (nmiss_label %in% categories)
                        private$.uniqueSummaryLabel(.("Missing (NA)"), categories)
                    else nmiss_label)
                    tab <- arsenal::tableby(formula = formula_obj,
                                            data = arsenal_data,
                                            stats.labels = stats_labels,
                                            total = TRUE,
                                            digits = 1,
                                            digits.count = 0,
                                            digits.pct = 1)
                    tab_summary <- summary(
                        tab,
                        text = "html",
                        pfootnote = "html"
                    )
                    paste(capture.output(tab_summary), collapse = "\n")
                }, error = function(e) {
                    private$.rejectPlain(private$.formatText(
                        .("Error creating arsenal table: {error}. Check that categorical variables are factors and numeric variables contain valid numbers."),
                        error = conditionMessage(e)))
                })

                # Checkpoint after expensive operation to allow UI update
                private$.checkpoint()
                self$results$tablestyle3$setContent(private$.normalizeArsenalHtml(mytable))

            } else if (table_style == "t4") {
                # --- Using janitor package for frequency tables with improved spacing & styling ---
                # Checkpoint before starting the variable loop
                private$.checkpoint()

                # Wrap entire janitor operation in tryCatch for error handling
                frequency_failed <- FALSE
                result <- tryCatch({
                    # Variables too granular to tabulate; reported after the loop.
                    skipped_vars <- frequency_skipped

                    table_list <- lapply(seq_along(selected_vars), function(i) {
                    var <- selected_vars[i]

                    # Checkpoint for each variable processing (for incremental results)
                    private$.checkpoint(flush = FALSE)

                    freq_table <- tryCatch({
                        # Do NOT strip the NA rows before tabulating. Doing so was
                        # equivalent to show_na = FALSE: janitor::tabyl() normally
                        # emits an <NA> row AND a valid_percent column, and the
                        # pre-filter removed both, so a Ki-67 category with 30%
                        # missing rendered as "45 (75.0%)" with its denominator
                        # nowhere on screen. This was the only one of the four
                        # styles that hid it (t1 has a Missing column, t2 an
                        # "Unknown" row, t3 an "N-Miss" row). Passing the whole
                        # frame also retires the old drop = FALSE trap, since
                        # `data` is never subset down to a vector.
                        recorded <- !is.na(data[[var]])

                        # One useless column used to abort the ENTIRE analysis:
                        # this reject() was caught by the handler below, re-raised,
                        # caught by the outer tryCatch and re-raised again, so a
                        # perfectly good Sex table was destroyed by an empty
                        # neighbour. Skip the variable and say so instead.
                        if (!any(recorded)) {
                            skipped_vars[[length(skipped_vars) + 1]] <<-
                                sprintf("%s (%s)", htmltools::htmlEscape(var),
                                        .("no recorded values"))
                            return(NULL)
                        }

                        # Create tabyl table using actual column name
                        table <- janitor::tabyl(data, !!rlang::sym(var),
                                                show_missing_levels = FALSE)

                        # Display labels must not collide with real categories.
                        # Character conversion also prevents adorn_totals from
                        # adding a duplicate factor level named Total.
                        table[[1]] <- as.character(table[[1]])
                        if ("NA" %in% table[[1]]) {
                            missing_label <- private$.uniqueSummaryLabel(
                                .("Missing (NA)"), table[[1]])
                            table[[1]][is.na(table[[1]])] <- missing_label
                        }
                        total_default <- .("Total")
                        total_label <- if (total_default %in% table[[1]])
                            private$.uniqueSummaryLabel(.("Total (all cases)"), table[[1]])
                        else total_default
                        table <- janitor::adorn_totals(table, "row", name = total_label)
                        
                        # Do not label raw fractions as percentages if formatting
                        # fails; the visible failure panel below names the variable.
                        table <- janitor::adorn_pct_formatting(table)

                        # Get the actual column names to handle different janitor output formats
                        col_names <- names(table)
                        
                        # Rename columns for consistency - use more flexible approach
                        if (length(col_names) >= 2) {
                            # First column is typically the variable values, second is counts
                            names(table)[2] <- .("N")
                        }
                        if (length(col_names) >= 3) {
                            names(table)[3] <- .("Percent")
                        }
                        # janitor only emits valid_percent when the column has at
                        # least one NA, so a complete variable yields 3 columns and
                        # this rename is correctly skipped.
                        if (length(col_names) >= 4) {
                            names(table)[4] <- .("Valid Percent")
                        }
                        
                        table
                    }, error = function(e) {
                        frequency_failed <<- TRUE
                        # Record the failure and carry on with the other variables
                        # rather than aborting the whole analysis. The reason is
                        # reported in the "Not tabulated" panel below.
                        skipped_vars[[length(skipped_vars) + 1]] <<-
                            sprintf("%s (%s: %s)", htmltools::htmlEscape(var),
                                    class(data[[var]])[1],
                                    htmltools::htmlEscape(conditionMessage(e)))
                        NULL
                    })

                    # The error handler above returns from ITSELF, not from this
                    # lapply iteration, so the NULL has to be caught here before it
                    # reaches kableExtra::kable().
                    if (is.null(freq_table))
                        return(NULL)

                    # Add a header for clarity for each variable's table, plus a top margin.
                    # Use escaped variable name for safe HTML rendering
                    safe_var_name <- htmltools::htmlEscape(var)
                    header <- paste0("<h4 style='margin-top:20px;'>",
                        private$.formatText(.("Frequency Table for '{variable}'"),
                                            variable = safe_var_name), "</h4>")

                    # Convert to an HTML table with columns centered from the second column onward:
                    # The first column (variable level) is left-aligned, and columns 2-4 are centered.
                    styled_table <- kableExtra::kable(
                        freq_table,
                        format = "html",
                        digits = 1,
                        escape = TRUE,  # escape factor-level cell content (freq_table is plain data, not HTML)
                        align = c("l", "c", "c", "c")  # left, center, center, center
                    ) %>%
                        kableExtra::kable_styling(
                            bootstrap_options = c("striped", "hover"),
                            full_width = FALSE,
                            font_size = 14,
                            position = "center"
                        )

                        # Combine the header and the table with spacing (hr).
                        paste0(header, styled_table, "<br><hr style='margin:20px 0;'>")
                    })

                    # Join all the tables together. lapply() returns NULL for a
                    # skipped variable, so drop those before pasting, and say which
                    # were skipped - a silently absent variable reads as a bug.
                    rendered_tables <- Filter(Negate(is.null), table_list)
                    body <- paste(rendered_tables, collapse = "")

                    # The two percentage columns have different denominators, and
                    # "Valid Percent" only exists for variables that actually have
                    # missing values, so the columns have to be named on screen -
                    # this is the output most likely to be pasted into a manuscript.
                    if (length(rendered_tables) > 0)
                        body <- paste0(body,
                            "<p style='font-size:12px;'><em>",
                            .("N and Percent count all cases, including the missing (NA) row. Valid Percent, present only for variables that have missing values, counts only the cases with a recorded value."),
                            " ", .("Unused factor levels are not displayed."),
                            "</em></p>")

                    if (length(skipped_vars) > 0) {
                        body <- paste0(body, private$.frequencySkipHtml(skipped_vars))
                    }
                    if (!nzchar(trimws(gsub("<[^>]*>", "", body))))
                        body <- paste0(
                            "<div style='background: rgba(255, 202, 33, 0.23); color: inherit;border-left:4px solid #ffc107;",
                            "padding:10px;margin:10px 0;'>",
                            .("No selected categorical variable could be tabulated. Choose the tableone, gtsummary or arsenal style for numeric measurements."),
                            "</div>")
                    body
                }, error = function(e) {
                    private$.rejectPlain(private$.formatText(
                        .("Error creating frequency tables with janitor: {error}. Check that categorical variables have valid data."),
                        error = conditionMessage(e)))
                })

                # Checkpoint after expensive operation to allow UI update
                private$.checkpoint()
                self$results$tablestyle4$setContent(result)
                if (frequency_failed) {
                    self$results$todo$setContent(paste0(
                        self$results$todo$content,
                        "<p><strong>",
                        .("Some frequency tables could not be produced. Review the Not tabulated details below. Supplementary summaries and copy-ready text are withheld because the output is incomplete."),
                        "</strong></p>"))
                    private$.setAssumptionsSkipped(
                        .("The data quality check was withheld because some frequency tables could not be produced; the Frequency Tables panel below explains why."))
                    return(FALSE)
                }
            } else {
                private$.rejectPlain(.("Invalid table style selected. Choose tableone, gtsummary, arsenal or janitor."))
            }
            TRUE
        },

        .normalizeArsenalHtml = function(html) {
            # Replace only renderer-generated whitespace; never decode &lt; or
            # &amp;, which protect user-provided labels. Plain cell tags also
            # let jmvcore's text exporter recognise arsenal's table cells.
            # Assemble an input-only entity pattern; never emit a named entity.
            html <- gsub(paste0("&", "nbsp;"), "\u00a0", html, fixed = TRUE)
            gsub("<(td|th)\\s[^>]*>", "<\\1>", html, perl = TRUE)
        },

        # ========================================================================
        # HTML Builder Helper Functions
        # ========================================================================
        # These helpers extract HTML string building logic for maintainability.
        # When jamovi supports Notice serialization, these can be migrated to
        # Notice objects with appropriate NoticeType.

        .frequencySkipHtml = function(skipped_vars) {
            paste0(
                "<div style='background: rgba(255, 202, 33, 0.23); color: inherit;",
                "border-left:4px solid #ffc107;padding:10px;margin:10px 0;'><b>",
                .("Not tabulated"), ":</b> ",
                paste(unlist(skipped_vars), collapse = "; "), ". ",
                .("Janitor tabulates categorical, ordinal, text and logical variables with at most 20 recorded categories. Numeric measurements are not converted to categories automatically. Use another style for measurements; set numeric category codes to Nominal or Ordinal in jamovi, or convert them with factor() in R. Omitted variables do not enter missing-value exclusion or the analysis summary."),
                "</div>")
        },

        .buildWelcomeMessage = function() {
            paste0(
                "<br><strong>", .("Welcome to the ClinicoPath Table One Generator"),
                "</strong><br><br><strong>", .("Instructions"), "</strong><p>",
                .("This analysis describes the overall cohort only. It does not compare groups or compute p-values, confidence intervals or standardized mean differences."),
                "</p><ul><li>",
                .("Select numeric, ordinal, categorical, text or logical variables to include in Table One."),
                "</li><li>", .("Choose a table style for the output format."),
                "</li><li>", .("If needed, enable missing-value exclusion (NA). Exclusion may remove entire cases."),
                "</li></ul><p>", .("Please cite the packages and jamovi as referenced below."),
                "</p>")
        },

        .buildDataQualityHtml = function(warnings, recommendations) {
            # Build HTML for data quality warnings and recommendations
            # Returns empty string if no issues detected
            if (length(warnings) == 0 && length(recommendations) == 0) {
                return("")
            }

            html <- paste0("<section><h4>", .("Data Quality & Assumptions"), "</h4>")

            if (length(warnings) > 0) {
                html <- paste0(html,
                    "<div style='background:rgba(220,53,69,0.12);border-left:4px solid #dc3545;padding:12px;color:inherit;'>",
                    "<p><strong>", .("Warnings"), ":</strong></p><ul>",
                    paste0("<li>", warnings, "</li>", collapse = ""),
                    "</ul></div>"
                )
            }

            if (length(recommendations) > 0) {
                html <- paste0(html,
                    "<div style='background:rgba(255,193,7,0.12);border-left:4px solid #ffc107;padding:12px;color:inherit;'>",
                    "<p><strong>", .("Recommendations"), ":</strong></p><ul>",
                    paste0("<li>", recommendations, "</li>", collapse = ""),
                    "</ul></div>"
                )
            }

            paste0(html, "<p><em>",
                   .("These are descriptive screening heuristics, not validated clinical cutoffs or tests of statistical assumptions."),
                   "</em></p></section>")
        },

        .buildDataQualityOkHtml = function(n_final, missing_pct_original) {
            # Build HTML for successful data quality check
            paste0(
                "<div style='background-color: rgba(33, 159, 43, 0.1); padding: 15px; border-left: 4px solid #4caf50; margin: 10px 0; color: inherit;'>",
                "<h4>", .("Data Quality Check"), "</h4><p><strong>",
                .("Cases in the table"), ":</strong> ",
                private$.formatText(.("N = {n}"), n = n_final), "</p>",
                "<p><strong>", .("Complete cases in the source data"), ":</strong> ",
                round(100 - missing_pct_original, 1), "%</p>",
                "<p><em>",
                .("None of the sample-size, missing-data or case-loss thresholds this analysis checks was crossed. These are descriptive screening heuristics, not validated clinical cutoffs or tests of statistical assumptions."),
                "</em></p>",
                "</div>"
            )
        },
        
        # `assumptions` is visible whenever variables are selected, so every
        # path that returns before .checkDataQuality() must give it a body or
        # the heading sits there empty and reads as a glitch.
        .setAssumptionsSkipped = function(reason) {
            self$results$assumptions$setContent(paste0(
                "<div style='background: rgba(255, 202, 33, 0.23); color: inherit;",
                "border-left:4px solid #ffc107;padding:10px;margin:10px 0;'><b>",
                .("Data quality check not performed"), ":</b> ", reason, "</div>"))
        },

        .setAboutContent = function() {
            if (!isTRUE(self$options$showAbout))
                return(invisible(NULL))
            about_text <- paste0(
                "<div style='background-color: rgba(138, 155, 172, 0.06); padding:15px; color:inherit;'>",
                "<h4>", .("About Table One"), "</h4><p>",
                .("Table One summarizes characteristics of the overall cohort, such as demographics, tumor grade and laboratory measurements. It does not stratify by group or compute p-values, confidence intervals or standardized mean differences."),
                "</p><p>", .("A row is treated as one case. Repeated records are not deduplicated. Frequency weights, when supplied, represent replicated rows, not verified unique patients or a complex survey design."),
                "</p><h5>", .("Output styles"), "</h5><ul><li>",
                .("tableone: continuous variables as mean (SD), or median (Q1, Q3) when requested; categorical variables as N (percent). Missingness is shown as a percentage column. Only the second level of a binary factor is displayed, with that level named."),
                "</li><li>",
                .("gtsummary: continuous variables as median (Q1, Q3); categorical variables as N (percent); missing counts on an Unknown row. Dichotomous row labels name the counted level, such as TRUE, 1 or yes."),
                "</li><li>",
                .("arsenal: continuous variables as mean (SD) with the range; categorical variables as N (percent); missing counts on an N-Miss row."),
                "</li><li>",
                .("janitor: counts and percentages for categorical, ordinal, text and logical variables with at most 20 recorded categories. Unused factor levels are not displayed. Numeric measurements are skipped; convert numeric category codes to factors explicitly. Percent uses all cases; Valid Percent uses recorded cases."),
                "</li></ul><h5>", .("Example interpretation"), "</h5><p>",
                .("If 50 of 100 values are missing and 15 recorded values are category A, tableone, gtsummary and arsenal report A as 15 (30%). Janitor reports Percent as 15% and Valid Percent as 30%. Missingness is reported separately."),
                "</p><p>",
                .("A gtsummary row labeled flag = TRUE with 0 (0%) means no recorded value was TRUE; it does not mean there were no observations. Check which category the row label names before interpreting a percentage."),
                "</p><h5>", .("Input and missing-data policy"), "</h5><p>",
                .("Actual NA factor levels are treated as missing before exclusion and tabulation. Literal text categories such as NA and Unknown remain categories. Listwise deletion uses only included variables; no imputation is performed."),
                "</p><p>",
                .("When a summary label conflicts with a recorded category, the summary row uses Missing (NA) or Total (all cases), with a numeric suffix if needed. Recorded category labels and counts are unchanged."),
                "</p><p>",
                .("Date, date-time, duration and custom scalar storage classes are omitted before exclusion. Matrix, array and list columns are rejected before R data selection. Convert unsupported inputs to explicitly defined scalar measurements or categories first."),
                "</p><p>",
                .("Numeric variables with fewer than 10 distinct values may be reported as categories by gtsummary but as measurements by tableone and arsenal. Set category codes to Nominal or Ordinal in jamovi, or factor() in R."),
                "</p></div>")
            self$results$about$setContent(about_text)
        },
        
        .generateSummary = function(data, vars, original_data, excluded_n, n_complete = NULL) {
            # Report statistics from the ORIGINAL data so true missingness is shown.
            n_original <- nrow(original_data)
            n_final <- nrow(data)
            n_vars <- length(vars)

            # Calculate missing data from ORIGINAL dataset. The complete-case count is
            # computed once in .run() and passed in to avoid recomputing complete.cases().
            if (is.null(n_complete)) n_complete <- sum(complete.cases(original_data))
            n_complete_original <- n_complete
            # Base the missing-data branch on the raw count, not the rounded percent,
            # so tiny-but-nonzero missingness is not reported as complete data.
            has_missing <- n_complete_original < n_original
            missing_pct_original <- round(100 * (1 - n_complete_original / n_original), 1)
            missing_pct_label <- if (has_missing && missing_pct_original < 0.1)
                "&lt;0.1" else base::format(missing_pct_original)

            # Variable type analysis (on final data for consistency)
            var_types <- sapply(data, function(x) {
                if (is.numeric(x)) .("Numeric")
                else if (is.factor(x) || is.character(x)) .("Categorical")
                else if (is.logical(x)) .("Logical")
                else .("Other")
            })
            type_summary <- table(var_types)
            type_text <- paste(names(type_summary), ":", type_summary, collapse = "; ")

            # Per-variable missing counts (from ORIGINAL data)
            var_missing <- sapply(vars, function(v) sum(is.na(original_data[[v]])))
            high_missing_vars <- vars[var_missing > n_original * 0.2]  # >20% missing
            # Escape variable names for safe HTML display
            high_missing_vars_safe <- sapply(high_missing_vars, htmltools::htmlEscape)

            # Build summary text with transparent reporting
            summary_text <- paste0(
                "<div style='background-color: rgba(33, 149, 236, 0.1); padding: 15px; border-left: 4px solid #007bff; margin: 10px 0; color: inherit;'>",
                "<h4>", .("Analysis Summary"), "</h4>",

                # Original dataset info
                "<p><strong>", .("Original dataset"), ":</strong> ",
                private$.formatText(
                    if (n_vars == 1L) .("{n} cases with {variables} selected variable")
                    else .("{n} cases with {variables} selected variables"),
                    n = n_original, variables = n_vars), "</p>",
                "<p><strong>", .("Complete cases (original)"), ":</strong> ", n_complete_original, " (",
                round(100 * n_complete_original / n_original, 1), "%)</p>",

                # Missing data transparency
                if (has_missing) {
                    paste0("<p><strong>", .("Missing data (original)"), ":</strong> ",
                           private$.formatText(.("{percent}% of cases have at least one missing value"),
                                               percent = missing_pct_label),
                           if (length(high_missing_vars_safe) > 0) {
                               paste0(" <br><em>", private$.formatText(
                                   .("Variables with >20% missing: {variables}"),
                                   variables = paste(high_missing_vars_safe, collapse = ", ")),
                                   "</em>")
                           } else "",
                           "</p>")
                } else "",

                # Exclusion warning if applicable
                if (excluded_n > 0) {
                    paste0("<p><strong>", .("Case exclusion"), ":</strong> ",
                           private$.formatText(
                               .("{n} cases ({percent}%) excluded due to missing values. Final N = {retained}"),
                               n = excluded_n, percent = round(100 * excluded_n / n_original, 1),
                               retained = n_final), "</p>",
                           "<p style='color: inherit; background-color: rgba(255, 202, 33, 0.23); padding: 8px; border-radius: 4px;'>",
                           "<em>", private$.formatText(
                               .("Note: Listwise deletion was applied. The table below shows statistics for the {n} complete cases only. All displayed variables use the same complete-case denominator."),
                               n = n_final), "</em></p>")
                } else {
                    paste0("<p><strong>", .("Analysis sample"), ":</strong> ",
                           private$.formatText(.("{n} cases (no exclusions applied)"), n = n_final),
                           "</p>",
                           if (has_missing) {
                               paste0("<p><em>",
                                   .("Note: Missing values are present but NOT excluded. Different variables may have different sample sizes (denominators) in the table below. Consider enabling missing-value exclusion for consistent denominators."),
                                   "</em></p>")
                           } else "")
                },

                "<p><strong>", .("Variable types"), ":</strong> ", type_text, "</p>",
                "<p><em>", .("This Table One summarizes baseline characteristics commonly reported in clinical research manuscripts."), "</em></p>",
                "</div>"
            )
            self$results$summary$setContent(summary_text)
        },
        
        .checkDataQuality = function(data, vars, original_data, n_complete = NULL) {
            # Descriptive screening heuristics, not validated clinical cutoffs:
            # - STRONG_WARNING thresholds: N<10, missing>50%, exclusion>30%
            # - WARNING thresholds: N<30, missing>20%, exclusion>10%
            # These would map to NoticeType when Notice serialization is supported.

            # Check sample size on FINAL data (after exclusions)
            n_final <- nrow(data)
            n_original <- nrow(original_data)

            warnings <- c()
            recommendations <- c()

            # Check sample size (descriptive thresholds)
            if (n_final < 10) {
                # STRONG_WARNING: Very small sample
                warnings <- c(warnings, private$.formatText(.("Very small final sample size (N = {n}). With fewer than 10 cases, one case represents more than 10 percentage points of the total. Summaries can be sensitive to individual observations. Report counts and consider the risk of identifying individuals. This analysis is descriptive only; no test or confidence interval is computed."), n = n_final))
            } else if (n_final < 30) {
                # WARNING: Small sample
                recommendations <- c(recommendations, private$.formatText(.("Small final sample size (N = {n}). Each case represents more than 3 percentage points of the total. Percentages in sparse categories can be unstable and individual cases may be identifiable. Report counts alongside percentages."), n = n_final))
            }

            # Check missing data from ORIGINAL dataset. The complete-case count is
            # computed once in .run() and passed in to avoid recomputing complete.cases().
            if (is.null(n_complete)) n_complete <- sum(complete.cases(original_data))
            n_incomplete <- n_original - n_complete
            missing_pct_original <- 100 * n_incomplete / n_original
            missing_label <- sprintf("%.2f%% (%d/%d)", missing_pct_original,
                                     n_incomplete, n_original)
            if (n_incomplete > n_original * 0.5) {
                # STRONG_WARNING: High missing data
                warnings <- c(warnings, private$.formatText(.("High missing data rate in original dataset: {missing} cases have at least one missing value. More than half of cases are incomplete. Results may not represent the full sample. Report missing-data patterns and review the missing-data strategy; this analysis does not impute values."), missing = missing_label))
            } else if (n_incomplete > n_original * 0.2) {
                # WARNING: Moderate missing data
                recommendations <- c(recommendations, private$.formatText(.("Moderate missing data in original dataset: {missing} cases have at least one missing value. Report missing-data patterns and compare complete with incomplete cases. This analysis does not impute values."), missing = missing_label))
            }

            # Warn if large proportion excluded
            if (n_original > n_final) {
                n_excluded <- n_original - n_final
                excluded_pct <- 100 * n_excluded / n_original
                excluded_label <- sprintf("%.2f%% (%d/%d)", excluded_pct,
                                          n_excluded, n_original)
                if (n_excluded > n_original * 0.3) {
                    # STRONG_WARNING: Large exclusion
                    warnings <- c(warnings, private$.formatText(.("Large case loss due to missing data: {excluded} cases excluded; {n} retained. Results may not represent the full sample. Review the missing-data strategy and consider a sensitivity analysis."), excluded = excluded_label, n = n_final))
                } else if (n_excluded > n_original * 0.1) {
                    # WARNING: Notable exclusion
                    recommendations <- c(recommendations, private$.formatText(.("Notable case loss: {excluded} cases excluded; {n} retained. Compare excluded with included cases to assess potential bias."), excluded = excluded_label, n = n_final))
                }
            }

            # Check variable types and unusual patterns
            for (var in vars) {
                if (var %in% names(data)) {
                    var_data <- data[[var]]
                    n_unique <- length(unique(var_data[!is.na(var_data)]))
                    n_valid <- sum(!is.na(var_data))

                    # Threshold matched to gtsummary's own numeric/categorical
                    # cut-off, measured on gtsummary 2.5.1 by sweeping k: 9 distinct
                    # values -> n (%) per level, 10 -> median (Q1, Q3). At the old
                    # cut-off of 5 an ECOG 0-4 or a Gleason sum was never flagged,
                    # yet the styles still disagree about how to summarise it.
                    if (is.numeric(var_data) && n_unique < 10 && n_valid > 10) {
                        # INFO: Variable type recommendation
                        t1_summary <- if (isTRUE(self$options$nonnormal))
                            .("The tableone median option is enabled: numeric measurements are reported as median (Q1, Q3) in that style.")
                        else
                            .("The tableone median option is disabled: numeric measurements are reported as mean (SD) in that style.")
                        recommendations <- c(recommendations, paste(
                            private$.formatText(.("Variable '{variable}' is stored as a number but has only {n} distinct values."), variable = htmltools::htmlEscape(var), n = n_unique),
                            t1_summary,
                            .("Arsenal reports mean (SD); gtsummary may report N (percent) per level. Convert the variable to nominal or ordinal if the numbers are category codes rather than measurements.")))
                    }

                    if (is.character(var_data) && n_unique > n_valid * 0.8) {
                        # INFO: Variable type recommendation
                        recommendations <- c(recommendations, private$.formatText(.("Variable '{variable}' has many unique text values. Consider grouping categories."), variable = htmltools::htmlEscape(var)))
                    }
                }
            }

            # Build and set assumptions HTML output using helper
            if (length(warnings) > 0 || length(recommendations) > 0) {
                assumptions_html <- private$.buildDataQualityHtml(warnings, recommendations)
                self$results$assumptions$setContent(assumptions_html)
            } else {
                # Nothing crossed a warning threshold. Always say so - the old
                # narrower gate left the panel present in the results tree with an
                # empty body whenever n differed from the original but every
                # threshold still passed.
                self$results$assumptions$setContent(
                    private$.buildDataQualityOkHtml(n_final, missing_pct_original))
            }
        },

        # jmvcore's own sourcifier writes `vars = vars(Ki-67 (%))` - unquoted and
        # unbackticked - which is not parseable R for a non-syntactic column name.
        # Emit character literals produced by encodeString() instead, which escapes
        # embedded quotes and backslashes and never line-wraps. Overriding here (as
        # opposed to appending a second `vars = ...` in asSource) is what keeps the
        # option from being emitted twice by .asArgs().
        .sourcifyOption = function(option) {
            if (identical(option$name, "vars")) {
                vars <- option$value
                if (length(vars) == 0)
                    return("")
                return(paste0(
                    "vars = c(",
                    paste(encodeString(as.character(vars), quote = '"'), collapse = ", "),
                    ")"))
            }
            super$.sourcifyOption(option)
        },

        .setReportSentence = function(data, vars, original_data, excluded_n, n_complete = NULL) {
            n_final <- nrow(data)
            n_original <- nrow(original_data)
            n_vars <- length(vars)

            # Complete-case count computed once in .run(); recompute only as a fallback.
            if (is.null(n_complete)) n_complete <- sum(complete.cases(original_data))
            # Base the completeness branch on the raw count, not the rounded percent,
            # so tiny-but-nonzero missingness is not reported as complete data.
            has_missing <- n_complete < n_original
            missing_pct <- 100 * (n_original - n_complete) / n_original
            # Report values that round to 0.0% but are non-zero as "<0.1%".
            missing_pct_str <- if (missing_pct < 0.1) "<0.1%" else sprintf("%.1f%%", missing_pct)

            # Build variable list description
            var_list <- if (n_vars <= 3) {
                paste(vars, collapse = ", ")
            } else {
                private$.formatText(
                    if (n_vars == 4L) .("{variables}, and {n} other variable")
                    else .("{variables}, and {n} other variables"),
                    variables = paste(head(vars, 3), collapse = ", "), n = n_vars - 3L)
            }

            # Build missing data clause
            missing_clause <- if (!has_missing) {
                .("Complete data were available for all cases.")
            } else if (missing_pct < 5) {
                private$.formatText(.("Minimal missing data were detected ({percent} of cases with at least one missing value)."), percent = missing_pct_str)
            } else if (missing_pct < 20) {
                private$.formatText(.("Moderate missing data were observed ({percent} of cases incomplete)."), percent = missing_pct_str)
            } else {
                private$.formatText(.("Substantial missing data were present ({percent} of cases with at least one missing value)."), percent = missing_pct_str)
            }

            # This text is designed to be selected and pasted, and the leading
            # clause is a self-contained sentence a user can copy on its own. When
            # rows were dropped by listwise deletion it therefore has to name the
            # ANALYSED cohort, not the screened one, or it reports a number the
            # table below does not show.
            report_text <- if (excluded_n > 0) {
                private$.formatText(
                    if (n_final == 1L)
                        .("Table One summarizes baseline characteristics of the {n} case with complete data for all listed variables (of {screened} screened). Variables included {variables}. {missing}")
                    else
                        .("Table One summarizes baseline characteristics of the {n} cases with complete data for all listed variables (of {screened} screened). Variables included {variables}. {missing}"),
                    n = n_final, screened = n_original, variables = var_list, missing = missing_clause)
            } else {
                private$.formatText(
                    if (n_original == 1L)
                        .("Table One summarizes baseline characteristics of {n} case. Variables included {variables}. {missing}")
                    else
                        .("Table One summarizes baseline characteristics of {n} cases. Variables included {variables}. {missing}"),
                    n = n_original, variables = var_list, missing = missing_clause)
            }

            # Format with copy button styling
            html_output <- paste0(
                "<div style='background-color: rgba(33, 152, 255, 0.07); border: 2px solid #4682b4; border-radius: 5px; padding: 15px; margin: 10px 0; color: inherit;'>",
                "<h4 style='margin-top: 0;'>", .("Copy-Ready Report Sentence"), "</h4>",
                "<p style='font-family: Georgia, serif; font-size: 14px; line-height: 1.6;'>",
                htmltools::htmlEscape(report_text),
                "</p>",
                "<p style='margin-bottom: 0; font-size: 12px; opacity: 0.8;'>",
                "<em>",
                .("Select and copy the text above for your manuscript. Counts refer to rows, not verified unique patients; repeated records are not deduplicated. Edit as needed for your specific reporting requirements."),
                "</em>",
                "</p>",
                "</div>"
            )

            self$results$reportSentence$setContent(html_output)
        }
  ), # End of private list.
  public = list(
        #' @description Initialize the analysis, validating original R column shapes and empty selections.
        #' @param noThrow Whether initialization errors are stored in the results.
        init = function(noThrow = FALSE) {
            # Validate original columns before jmvcore::select() can flatten a
            # matrix/list and silently reinterpret its first component.
            if (is.data.frame(private$.data) && private$.status == "none") {
                selected <- intersect(self$options$vars, names(private$.data))
                shaped <- Filter(function(v) {
                    value <- private$.data[[v]]
                    !is.null(dim(value)) || is.list(value)
                }, selected)
                if (length(shaped) > 0L) {
                    private$.clearOutputs()
                    message <- private$.formatText(
                        .("Unsupported non-scalar columns: {variables}. Matrix, array and list columns cannot be summarized. Select scalar columns or explicitly extract the intended measurements first."),
                        variables = paste(shaped, collapse = "; "))
                    if (isTRUE(noThrow)) {
                        self$setError(message)
                        return(invisible(NULL))
                    }
                    private$.rejectPlain(message)
                }
            }
            # jmvcore 2.7 select(df, character()) assigns nonempty row names
            # to a zero-row frame. Only the no-selection R path needs this
            # workaround; preserve the source frame for onboarding afterwards.
            if (length(self$options$vars) == 0L && is.data.frame(private$.data)) {
                source_data <- private$.data
                private$.data <- source_data[FALSE, FALSE, drop = FALSE]
                on.exit(private$.data <- source_data, add = TRUE)
            }
            super$init(noThrow = noThrow)
        },
        #' @description
        #' Generate R source code for Table One analysis
        #' @return Character string with R syntax for reproducible analysis
        asSource = function() {
            if (length(self$options$vars) == 0)
                return('')

            # .asArgs() already walks every option (data and vars included) and
            # calls .sourcifyOption() on each, so nothing may be emitted by hand
            # here: doing so produced `vars = ...` twice and the pasted snippet
            # died with 'formal argument "vars" matched by multiple actual
            # arguments'.
            pkg_name <- utils::packageName()
            if (is.null(pkg_name)) pkg_name <- "ClinicoPath"  # fallback

            paste0(pkg_name, '::tableone(', private$.asArgs(incData = TRUE), ')')
        }
  ) # End of public list.
) # End of R6Class definition.
