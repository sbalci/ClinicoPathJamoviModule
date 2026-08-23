#' @title Table One
#'
#' @description This function creates a "Table One" - a descriptive summary table commonly used in clinicopathological research.
#' It offers several output styles using different packages (tableone, gtsummary, arsenal, and janitor).
#'
#' @return A results object; see the Value section of the generated tableone() documentation.
#'
#' @importFrom R6 R6Class
#' @importFrom jmvcore select naOmit constructFormula
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
        .htmlSafeTableData = function(data) {
            escape <- function(value) {
                as.character(htmltools::htmlEscape(as.character(value)))
            }

            for (i in seq_along(data)) {
                value <- data[[i]]
                if (is.factor(value)) {
                    levels(value) <- escape(levels(value))
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
            # TODO (forward-looking): no `.()` wrapping in this file - HTML
            # messages, table titles ("Data Quality Check", "Analysis
            # Summary"), and report-sentence text are English-only. The
            # function is otherwise architecturally clean (good checkpoint
            # coverage, htmlEscape usage, asSource method). Address in a
            # /prepare-translation pass.
            # Check that the input data has at least one complete row.
            if (is.null(self$data) || nrow(self$data) == 0) {
                self$results$todo$setContent("
                    <br><strong>No Data Available</strong>
                    <br><br>
                    <ul>
                        <li>Please load a dataset before using Table One.</li>
                        <li>Check that your data file is properly imported.</li>
                    </ul>
                ")
                private$.setAboutContent()
                return(invisible(NULL))
            }

            # If no variables are selected, show a welcome/instructions message.
            if (is.null(self$options$vars)) {
                self$results$todo$setContent(private$.buildWelcomeMessage())
                private$.setAboutContent()
                return(invisible(NULL))  # Stop further processing until variables are selected.
            } else {
                # Clear the instructions message once variables are selected.
                self$results$todo$setContent("")
            }

            # Prepare the data using user-selected variables.
            selected_vars <- self$options$vars  # Improved variable naming.

            # Checkpoint before data preparation (potentially expensive for large datasets)
            private$.checkpoint()

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

            todo_html <- ""
            if (length(all_na_vars) > 0) {
                # Drop them here, not just report them. The claim made below - that
                # the variable does not appear in the table - was only true for the
                # tableone style: gtsummary rendered a row of NAs, arsenal rendered
                # "N-Miss" with a blank mean, and janitor had nothing to count and
                # aborted the whole analysis. Dropping first makes the sentence true
                # for every style, and stops one never-collected lab value from
                # wiping out every other row via listwise deletion below.
                selected_vars <- setdiff(selected_vars, all_na_vars)
                todo_html <- paste0(
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

            if (length(selected_vars) == 0) {
                self$results$todo$setContent(paste0(
                    todo_html,
                    "<div style='background: rgba(255, 202, 33, 0.23); color: inherit;border-left:4px solid #ffc107;",
                    "padding:10px;margin:10px 0;'><b>",
                    .("Nothing to summarise"), ":</b> ",
                    .("Every selected variable is missing for all cases, so there is nothing to tabulate. Select at least one variable that has recorded values."),
                    "</div>"))
                private$.setAboutContent()
                return(invisible(NULL))
            }

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
                    sprintf(.("Excluding missing values left no cases: each of the %d cases has at least one missing value among the selected variables. Untick Missing-value exclusion (NA), or select fewer variables."),
                            original_n),
                    "</div>"))
                private$.setAboutContent()
                return(invisible(NULL))
            }

            # Retrieve the table style selected by the user.
            table_style <- self$options$sty

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

            if (isTRUE(self$options$showAbout)) {
                private$.setAboutContent()
            }

            if (isTRUE(self$options$showReportSentence)) {
                private$.setReportSentence(data, selected_vars, original_data, excluded_n, original_complete)
            }

            private$.checkDataQuality(data, selected_vars, original_data, original_complete)

            # Generate the table based on the chosen style.
            if (table_style == "t1") {
                # --- Using tableone package ---
                # Checkpoint before expensive statistical computation
                private$.checkpoint()

                mytable <- tryCatch({
                    tableone::CreateTableOne(data = data)
                }, error = function(e) {
                    if (grepl("insufficient", tolower(e$message))) {
                        private$.rejectPlain("Insufficient data for Table One analysis. Ensure you have at least 2 complete cases and check for missing values. Try selecting different variables or disabling 'Exclude Missing Values'.")
                    } else {
                        private$.rejectPlain(paste0("Error creating Table One: ", sub("\\.+$", "", e$message), ". Check that variables have valid data and appropriate types. Categorical variables should be factors. Numeric variables should contain valid numbers."))
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
                    private$.rejectPlain(paste0(
                        "The Table One summary was computed but could not be formatted for display: ",
                        sub("\\.+$", "", render_error),
                        ". Try another table style, or deselect variables with unusual storage types (dates, list columns)."))

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
                    tbl <- gtsummary::tbl_summary(data = data)
                    gtsummary::as_kable_extra(tbl)
                }, error = function(e) {
                    private$.rejectPlain(paste0("Error creating gtsummary table: ", e$message, ". Check that variables have valid data and appropriate types. gtsummary requires properly formatted variables for summarization."))
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
                    tab <- arsenal::tableby(formula = formula_obj,
                                            data = arsenal_data,
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
                    private$.rejectPlain(paste0("Error creating arsenal table: ", e$message, ". Arsenal requires properly formatted variables. Check that categorical variables are factors and numeric variables contain valid numbers."))
                })

                # Checkpoint after expensive operation to allow UI update
                private$.checkpoint()
                self$results$tablestyle3$setContent(mytable)

            } else if (table_style == "t4") {
                # --- Using janitor package for frequency tables with improved spacing & styling ---
                # Checkpoint before starting the variable loop
                private$.checkpoint()

                # Wrap entire janitor operation in tryCatch for error handling
                result <- tryCatch({
                    # Variables too granular to tabulate; reported after the loop.
                    skipped_vars <- list()

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

                        # A frequency table needs a manageable number of distinct
                        # values. On a continuous variable janitor tabulates EVERY
                        # observed value: 50 patients produced 50 rows labelled
                        # "41.9504137110896", which is unreadable and tells the
                        # reader nothing. The option's own description says this
                        # style is for categorical variables. Judge by distinct-value
                        # count rather than by type, so a numeric score with a few
                        # levels (a 1-5 grade) is still tabulated.
                        n_distinct <- length(unique(data[[var]][recorded]))
                        if (n_distinct > 20) {
                            skipped_vars[[length(skipped_vars) + 1]] <<-
                                sprintf("%s (%d distinct values)",
                                        htmltools::htmlEscape(var), n_distinct)
                            return(NULL)
                        }

                        # Create tabyl table using actual column name
                        table <- janitor::tabyl(data, !!rlang::sym(var))
                        
                        # Add totals
                        table <- janitor::adorn_totals(table, "row")
                        
                        # Add percentage formatting - but handle the case where it might fail
                        table <- tryCatch({
                            janitor::adorn_pct_formatting(table)
                        }, error = function(e) {
                            # If pct formatting fails, just return the table with totals
                            table
                        })

                        # Get the actual column names to handle different janitor output formats
                        col_names <- names(table)
                        
                        # Rename columns for consistency - use more flexible approach
                        if (length(col_names) >= 2) {
                            # First column is typically the variable values, second is counts
                            names(table)[2] <- "N"
                        }
                        if (length(col_names) >= 3) {
                            names(table)[3] <- "Percent"
                        }
                        # janitor only emits valid_percent when the column has at
                        # least one NA, so a complete variable yields 3 columns and
                        # this rename is correctly skipped.
                        if (length(col_names) >= 4) {
                            names(table)[4] <- "Valid Percent"
                        }
                        
                        table
                    }, error = function(e) {
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
                    header <- paste0("<h4 style='margin-top:20px;'>Frequency Table for '", safe_var_name, "'</h4>")

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
                            "</em></p>")

                    if (length(skipped_vars) > 0) {
                        body <- paste0(
                            body,
                            "<div style='background: rgba(255, 202, 33, 0.23); color: inherit;border-left:4px solid #ffc107;",
                            "padding:10px;margin:10px 0;'><b>",
                            .("Not tabulated"), ":</b> ",
                            paste(unlist(skipped_vars), collapse = "; "),
                            ". ", .("A frequency table needs a limited set of recorded categories: too many distinct values gives roughly one row per case, and a variable with no recorded values has nothing to count. Continuous variables are summarised by the tableone, gtsummary or arsenal style instead."),
                            "</div>")
                    }
                    if (!nzchar(trimws(gsub("<[^>]*>", "", body))))
                        body <- paste0(
                            "<div style='background: rgba(255, 202, 33, 0.23); color: inherit;border-left:4px solid #ffc107;",
                            "padding:10px;margin:10px 0;'>",
                            .("None of the selected variables has few enough distinct values for a frequency table. Choose the tableone, gtsummary or arsenal style instead."),
                            "</div>")
                    body
                }, error = function(e) {
                    private$.rejectPlain(paste0("Error creating frequency tables with janitor: ", e$message, ". Check that variables have valid data. Janitor works best with categorical or discrete variables."))
                })

                # Checkpoint after expensive operation to allow UI update
                private$.checkpoint()
                self$results$tablestyle4$setContent(result)
            } else {
                private$.rejectPlain("Invalid table style selected. Please choose a valid table style from the options (tableone, gtsummary, arsenal, janitor).")
            }
        }, # End of .run function.

        # ========================================================================
        # HTML Builder Helper Functions
        # ========================================================================
        # These helpers extract HTML string building logic for maintainability.
        # When jamovi supports Notice serialization, these can be migrated to
        # Notice objects with appropriate NoticeType.

        .buildWelcomeMessage = function() {
            "
            <br><strong>Welcome to the ClinicoPath Table One Generator</strong>
            <br><br>
            <strong>Instructions:</strong>
            <ul>
                <li>Select the <em>Variables</em> to include in the Table One. (Numeric, Ordinal, or Categorical)</li>
                <li>Choose a <em>Table Style</em> for the output format.</li>
                <li>If needed, check the option to <em>Exclude Missing Values</em> (NA). (Exclusion may remove entire cases.)</li>
            </ul>
            <br>
            Please ensure you cite the packages and jamovi as referenced below.
            "
        },

        .buildDataQualityHtml = function(warnings, recommendations) {
            # Build HTML for data quality warnings and recommendations
            # Returns empty string if no issues detected
            if (length(warnings) == 0 && length(recommendations) == 0) {
                return("")
            }

            html <- paste0(
                "<div style='background-color: rgba(255, 211, 33, 0.16); padding: 15px; border-left: 4px solid #ffa500; margin: 10px 0; color: inherit;'>",
                "<h4>Data Quality & Assumptions</h4>"
            )

            if (length(warnings) > 0) {
                html <- paste0(html,
                    "<p><strong>Warnings:</strong></p><ul>",
                    paste0("<li>", warnings, "</li>", collapse = ""),
                    "</ul>"
                )
            }

            if (length(recommendations) > 0) {
                html <- paste0(html,
                    "<p><strong>Recommendations:</strong></p><ul>",
                    paste0("<li>", recommendations, "</li>", collapse = ""),
                    "</ul>"
                )
            }

            paste0(html, "</div>")
        },

        .buildDataQualityOkHtml = function(n_final, missing_pct_original) {
            # Build HTML for successful data quality check
            paste0(
                "<div style='background-color: rgba(33, 159, 43, 0.1); padding: 15px; border-left: 4px solid #4caf50; margin: 10px 0; color: inherit;'>",
                "<h4>Data Quality Check</h4>",
                "<p><strong>Cases in the table:</strong> N = ", n_final, "</p>",
                "<p><strong>Complete cases in the source data:</strong> ", round(100 - missing_pct_original, 1), "%</p>",
                "<p><em>None of the sample-size, missing-data or case-loss thresholds this analysis checks was crossed.</em></p>",
                "</div>"
            )
        },
        
        .setAboutContent = function() {
            about_text <- "
            <div style='background-color: rgba(138, 155, 172, 0.06); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>
                <h4>About Table One</h4>
                <p><strong>Purpose:</strong> Table One is a standardized descriptive table used in medical research to summarize baseline characteristics and demographic information of study participants.</p>
                
                <p><strong>When to use:</strong></p>
                <ul>
                    <li>Describing patient demographics and clinical characteristics</li>
                    <li>Summarizing baseline features of your study population</li>
                    <li>Presenting lab values, vital signs, or biomarker data</li>
                    <li>Creating manuscript-ready descriptive summary tables</li>
                </ul>
                
                <p><strong>Variable types:</strong></p>
                <ul>
                    <li><em>Continuous:</em> Age, weight, lab values. How they are summarised depends on the style you pick - see below.</li>
                    <li><em>Categorical:</em> Sex, diagnosis, treatment groups (shown as N (%))</li>
                    <li><em>Ordinal:</em> Tumor grade, ECOG status (shown as N (%) by level)</li>
                </ul>
                
                <p><strong>Output styles:</strong></p>
                <ul>
                    <li><strong>tableone:</strong> continuous variables as mean (SD), or as median [Q1, Q3] if you tick <em>Report continuous variables as median (Q1, Q3)</em>; categorical variables as N (percent). Missingness is shown as a percentage column.</li>
                    <li><strong>gtsummary:</strong> continuous variables as median (Q1, Q3); categorical variables as N (percent); missing counts on an <em>Unknown</em> row.</li>
                    <li><strong>arsenal:</strong> continuous variables as mean (SD) with the range; categorical variables as N (percent); missing counts on an <em>N-Miss</em> row.</li>
                    <li><strong>janitor:</strong> counts and percentages only, one frequency table per variable, for variables with a limited number of categories. Missing values get their own row, with a Percent column computed over all cases and a Valid Percent column computed over the cases with a recorded value.</li>
                </ul>

                <p><strong>Reading the percentages:</strong> in the tableone, gtsummary and arsenal styles the percentages for a categorical variable are computed among the cases with a recorded value for that variable, not among all cases; the missing count is reported separately on its own column or row. A variable with 50 of 100 values missing and 15 cases in a level is therefore shown as 15 (30.0), not 15 (15.0).</p>

                <p><strong>Numbers used as category codes:</strong> a numeric variable with fewer than 10 distinct values is summarised as mean (SD) by the tableone and arsenal styles but as N (percent) per level by the gtsummary style, so the style you pick changes how such a variable is treated. Convert it to a nominal or ordinal variable in the data tab if the numbers are codes rather than measurements.</p>
            </div>"
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
                if (is.numeric(x)) "Numeric"
                else if (is.factor(x)) "Categorical"
                else if (is.logical(x)) "Logical"
                else "Other"
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
                "<h4>Analysis Summary</h4>",

                # Original dataset info
                "<p><strong>Original dataset:</strong> ", n_original, " cases with ", n_vars, " selected variables</p>",
                "<p><strong>Complete cases (original):</strong> ", n_complete_original, " (",
                round(100 * n_complete_original / n_original, 1), "%)</p>",

                # Missing data transparency
                if (has_missing) {
                    paste0("<p><strong>Missing data (original):</strong> ", missing_pct_label,
                           "% of cases have at least one missing value",
                           if (length(high_missing_vars_safe) > 0) {
                               paste0(" <br><em>Variables with >20% missing: ",
                                      paste(high_missing_vars_safe, collapse = ", "), "</em>")
                           } else "",
                           "</p>")
                } else "",

                # Exclusion warning if applicable
                if (excluded_n > 0) {
                    paste0("<p><strong>Case exclusion:</strong> ",
                           excluded_n, " cases (", round(100 * excluded_n / n_original, 1),
                           "%) excluded due to missing values. <strong>Final N = ", n_final,
                           "</strong></p>",
                           "<p style='color: inherit; background-color: rgba(255, 202, 33, 0.23); padding: 8px; border-radius: 4px;'>",
                           "<em>Note: Listwise deletion was applied. The table below shows statistics for the ",
                           n_final, " complete cases only. Per-variable denominators may differ if variables have different missing patterns.</em></p>")
                } else {
                    paste0("<p><strong>Analysis sample:</strong> ", n_final, " cases (no exclusions applied)</p>",
                           if (has_missing) {
                               "<p style='color: inherit; background-color: rgba(255, 202, 33, 0.23); padding: 8px; border-radius: 4px;'><em> Note: Missing values are present but NOT excluded. Different variables may have different sample sizes (denominators) in the table below. Consider enabling 'Exclude Missing Values' for consistent denominators.</em></p>"
                           } else "")
                },

                "<p><strong>Variable types:</strong> ", type_text, "</p>",
                "<p><em>This Table One summarizes baseline characteristics commonly reported in clinical research manuscripts.</em></p>",
                "</div>"
            )
            self$results$summary$setContent(summary_text)
        },
        
        .checkDataQuality = function(data, vars, original_data, n_complete = NULL) {
            # NOTE: Data quality thresholds align with clinical research standards:
            # - STRONG_WARNING thresholds: N<10, missing>50%, exclusion>30%
            # - WARNING thresholds: N<30, missing>20%, exclusion>10%
            # These would map to NoticeType when Notice serialization is supported.

            # Check sample size on FINAL data (after exclusions)
            n_final <- nrow(data)
            n_original <- nrow(original_data)

            warnings <- c()
            recommendations <- c()

            # Check sample size (clinical thresholds)
            if (n_final < 10) {
                # STRONG_WARNING: Very small sample
                warnings <- c(warnings, paste0("<strong>Very small final sample size (N = ", n_final, ").</strong> With fewer than 10 cases every observation shifts a category percentage by more than 10 points, and means and standard deviations are driven by single values. Listing the individual observations usually conveys more than a summary statistic. This analysis is descriptive only - no test or confidence interval is computed."))
            } else if (n_final < 30) {
                # WARNING: Small sample
                recommendations <- c(recommendations, paste0("<em>Small final sample size (N = ", n_final, ").</em> Each case moves a category percentage by more than 3 points, so percentages in sparse categories are unstable and individual cases may be identifiable. Consider reporting counts alongside percentages."))
            }

            # Check missing data from ORIGINAL dataset. The complete-case count is
            # computed once in .run() and passed in to avoid recomputing complete.cases().
            if (is.null(n_complete)) n_complete <- sum(complete.cases(original_data))
            missing_pct_original <- round(100 * (1 - n_complete / n_original), 1)
            if (missing_pct_original > 50) {
                # STRONG_WARNING: High missing data
                warnings <- c(warnings, paste0("<strong> High missing data rate in original dataset (", missing_pct_original, "%).</strong> More than half of cases have at least one missing value. Results may not be representative of the full population. Consider data cleaning, imputation, or reporting missing data patterns."))
            } else if (missing_pct_original > 20) {
                # WARNING: Moderate missing data
                recommendations <- c(recommendations, paste0("<em>Moderate missing data in original dataset (", missing_pct_original, "%).</em> Consider reporting missing data patterns or using multiple imputation. Compare characteristics of complete vs. incomplete cases."))
            }

            # Warn if large proportion excluded
            if (n_original > n_final) {
                excluded_pct <- round(100 * (n_original - n_final) / n_original, 1)
                if (excluded_pct > 30) {
                    # STRONG_WARNING: Large exclusion
                    warnings <- c(warnings, paste0("<strong> Large case loss due to missing data (", excluded_pct, "% excluded).</strong> Excluded: ", n_original - n_final, " cases | Retained: ", n_final, " cases. Results may not be representative of the full sample. Consider multiple imputation or sensitivity analyses."))
                } else if (excluded_pct > 10) {
                    # WARNING: Notable exclusion
                    recommendations <- c(recommendations, paste0("<em>Notable case loss (", excluded_pct, "% excluded).</em> Excluded: ", n_original - n_final, " cases | Retained: ", n_final, " cases. Compare characteristics of excluded vs. included cases to assess potential bias."))
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
                        recommendations <- c(recommendations, sprintf("<em>Variable '%s' is stored as a number but has only %d distinct values.</em> The tableone and arsenal styles report its mean (SD); the gtsummary style reports N (percent) per level. Convert it to a nominal or ordinal variable in the data tab if the numbers are category codes rather than measurements.", htmltools::htmlEscape(var), n_unique))
                    }

                    if (is.character(var_data) && n_unique > n_valid * 0.8) {
                        # INFO: Variable type recommendation
                        recommendations <- c(recommendations, sprintf("<em>Variable '%s' has many unique text values.</em> Consider grouping categories.", htmltools::htmlEscape(var)))
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
            missing_pct <- round(100 * (1 - n_complete / n_original), 1)
            # Report values that round to 0.0% but are non-zero as "<0.1%".
            missing_pct_str <- if (missing_pct < 0.1) "<0.1%" else sprintf("%.1f%%", missing_pct)

            # Build variable list description
            var_list <- if (n_vars <= 3) {
                paste(vars, collapse = ", ")
            } else {
                paste0(paste(head(vars, 3), collapse = ", "), ", and ", n_vars - 3, " other variable", if (n_vars - 3 > 1) "s" else "")
            }

            # Build missing data clause
            missing_clause <- if (!has_missing) {
                "Complete data were available for all cases."
            } else if (missing_pct < 5) {
                sprintf("Minimal missing data were detected (%s of cases with at least one missing value).", missing_pct_str)
            } else if (missing_pct < 20) {
                sprintf("Moderate missing data were observed (%s of cases incomplete).", missing_pct_str)
            } else {
                sprintf("Substantial missing data were present (%s of cases with at least one missing value).", missing_pct_str)
            }

            # This text is designed to be selected and pasted, and the leading
            # clause is a self-contained sentence a user can copy on its own. When
            # rows were dropped by listwise deletion it therefore has to name the
            # ANALYSED cohort, not the screened one, or it reports a number the
            # table below does not show.
            report_text <- if (excluded_n > 0) {
                sprintf(
                    "Table One summarizes baseline characteristics of the %d %s with complete data for all listed variables (of %d screened). Variables included %s. %s",
                    n_final,
                    if (n_final == 1) "patient" else "patients",
                    n_original,
                    var_list,
                    missing_clause
                )
            } else {
                sprintf(
                    "Table One summarizes baseline characteristics of %d %s. Variables included %s. %s",
                    n_original,
                    if (n_original == 1) "patient" else "patients",
                    var_list,
                    missing_clause
                )
            }

            # Format with copy button styling
            html_output <- paste0(
                "<div style='background-color: rgba(33, 152, 255, 0.07); border: 2px solid #4682b4; border-radius: 5px; padding: 15px; margin: 10px 0; color: inherit;'>",
                "<h4 style='margin-top: 0;'>Copy-Ready Report Sentence</h4>",
                "<p style='font-family: Georgia, serif; font-size: 14px; line-height: 1.6;'>",
                htmltools::htmlEscape(report_text),
                "</p>",
                "<p style='margin-bottom: 0; font-size: 12px; opacity: 0.8;'>",
                "<em>Select and copy the text above for your manuscript. Edit as needed for your specific reporting requirements.</em>",
                "</p>",
                "</div>"
            )

            self$results$reportSentence$setContent(html_output)
        }
  ), # End of private list.
  public = list(
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
