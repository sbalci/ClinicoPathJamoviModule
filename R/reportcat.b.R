#' @title Summary of Categorical Variables
#' @return A results object containing HTML summaries of the selected categorical variables.
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom magrittr %>%
#' @importFrom gtExtras gt_plt_summary
#'
# Improved version of reportcatClass with enhanced messages and formatting
reportcatClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "reportcatClass",
    inherit = reportcatBase,
    private = list(
        .run = function() {

            # aboutAnalysis is declared visible: true, so it must be populated
            # before any early return - otherwise the very first screen shows an
            # empty "About This Analysis" heading whose text tells the user to
            # select variables.
            self$results$aboutAnalysis$setContent(private$.generateAboutContent())

            # Clear everything written by the previous run. clearWith only reacts
            # to OPTIONS, and `vars` is the only option here, so a change in the
            # DATA (row filter, edited cells) does not clear anything: without
            # this, a validation early-return below would leave the previous
            # run's fully formed numbers on screen underneath an error message.
            private$.resetOutputs()

            # Check if any variables have been selected.
            # Enhanced welcome message with HTML formatting for a more user-friendly experience.
            if (length(self$options$vars) == 0) {
                todo <- glue::glue("
        <div style='font-family: Arial, sans-serif;'>
          <h2>{welcome_title}</h2>
          <p>{tool_description}</p>
          <p><strong>{instructions_label}:</strong> {instructions_text}
          {variable_types_note}</p>
          <hr>
        </div>",
        welcome_title = .("Welcome to ClinicoPath"),
        tool_description = .("This tool generates a summary of your selected categorical variables."),
        instructions_label = .("Instructions"),
        instructions_text = .("Please select the Variables you wish to analyze."),
        variable_types_note = .("Only Nominal, Ordinal, or Categorical variables (factors) are allowed.")
        )
                self$results$todo$setContent(todo)
                return()
            } else {

                # Enhanced input validation with proper error handling
                if (nrow(self$data) == 0) {
                    self$results$error$setContent(glue::glue("<div style='padding: 15px; background-color: rgba(216, 33, 50, 0.18); border: 1px solid #f5c6cb; border-radius: 4px; color: inherit;'><strong>{error_label}:</strong> {error_msg}</div>",
                        error_label = .("Error"),
                        error_msg = .("The dataset has no rows. Check whether a row filter is excluding every case.")))
                    self$results$error$setVisible(TRUE)
                    return()
                }

                mydata <- self$data

                # Raw, unquoted variable names: every downstream use is
                # mydata[[myvar]], which needs the raw name. (This used to
                # round-trip through constructFormula/decomposeFormula - an
                # identity transform here, but it reads as if the names came back
                # backtick-quoted, which would break the data[[ ]] lookups.)
                myvars <- unlist(self$options$vars)
                
                # Comprehensive validation of selected variables
                if (length(myvars) == 0) {
                    self$results$error$setContent(glue::glue("<div style='padding: 15px; background-color: rgba(216, 33, 50, 0.18); border: 1px solid #f5c6cb; border-radius: 4px; color: inherit;'><strong>{error_label}:</strong> {error_msg}</div>",
                        error_label = .("Error"),
                        error_msg = .("No valid variables selected.")))
                    self$results$error$setVisible(TRUE)
                    return()
                }
                
                # Check for variables that don't exist in the data
                missing_vars <- myvars[!myvars %in% names(mydata)]
                if (length(missing_vars) > 0) {
                    self$results$error$setContent(glue::glue("<div style='padding: 15px; background-color: rgba(216, 33, 50, 0.18); border: 1px solid #f5c6cb; border-radius: 4px; color: inherit;'><strong>{error_label}:</strong> {error_msg}: {vars}.</div>",
                        error_label = .("Error"),
                        error_msg = .("Variables not found in data"),
                        vars = paste(htmltools::htmlEscape(missing_vars), collapse = ", ")))
                    self$results$error$setVisible(TRUE)
                    return()
                }
                
                # Validate that selected variables are actually categorical
                non_categorical <- myvars[!sapply(mydata[myvars], function(x) is.factor(x) || is.character(x))]
                if (length(non_categorical) > 0) {
                    self$results$error$setContent(glue::glue("<div style='padding: 15px; background-color: rgba(216, 33, 50, 0.18); border: 1px solid #f5c6cb; border-radius: 4px; color: inherit;'><strong>{error_label}:</strong> {error_msg}: {vars}. {instruction}</div>",
                        error_label = .("Error"),
                        error_msg = .("Non-categorical variables detected"),
                        vars = paste(htmltools::htmlEscape(non_categorical), collapse = ", "),
                        instruction = .("Please select only categorical (factor or character) variables.")))
                    self$results$error$setVisible(TRUE)
                    return()
                }
                
                # Check for empty factor levels or all-NA variables
                empty_vars <- myvars[sapply(mydata[myvars], function(x) {
                    if (is.factor(x)) {
                        length(levels(x)) == 0 || all(is.na(x))
                    } else {
                        all(is.na(x)) || length(unique(x[!is.na(x)])) == 0
                    }
                })]
                
                if (length(empty_vars) > 0) {
                    # This is an advisory, not a failure: the analysis still runs
                    # on whatever is left. It goes to its own panel so it is not
                    # captioned "Error", and so that the terminal message below
                    # cannot overwrite the list of names it reports.
                    self$results$dataWarnings$setContent(glue::glue("<div style='padding: 15px; background-color: rgba(255, 202, 33, 0.23); border: 1px solid #ffeaa7; border-radius: 4px; color: inherit;'><strong>{warning_label}:</strong> {warning_msg}: {vars}. {action}</div>",
                        warning_label = .("Warning"),
                        warning_msg = .("Variables with no valid levels or all missing values"),
                        vars = paste(htmltools::htmlEscape(empty_vars), collapse = ", "),
                        action = .("These will be excluded from analysis.")))
                    self$results$dataWarnings$setVisible(TRUE)

                    # Remove empty variables from analysis
                    myvars <- myvars[!myvars %in% empty_vars]
                    if (length(myvars) == 0) {
                        self$results$error$setContent(glue::glue("<div style='padding: 15px; background-color: rgba(216, 33, 50, 0.18); border: 1px solid #f5c6cb; border-radius: 4px; color: inherit;'><strong>{error_label}:</strong> {error_msg} {action}</div>",
                            error_label = .("Error"),
                            error_msg = .("Every selected variable was empty or all-missing, so there is nothing to summarise."),
                            action = .("See the Data Warnings panel for the variable names, and select at least one variable that has observed categories.")))
                        self$results$error$setVisible(TRUE)
                        return()
                    }
                }

                # Function to generate a summary for a single categorical variable.
                catsummary <- function(myvar) {
                    # Calculate total observations, missing values, and valid (non-missing) count.
                    total_obs <- length(mydata[[myvar]])
                    missing_obs <- sum(is.na(mydata[[myvar]]))
                    valid_obs <- total_obs - missing_obs

                    # Count levels with table(useNA = "no"), which structurally
                    # cannot produce a missing-value row.
                    #
                    # This replaces summary(as.factor(x), maxsum = Inf) followed by
                    # dplyr::filter(level != "NA's"). summary.factor() names that
                    # row "NAs" - no apostrophe - so the filter never matched and
                    # the missing count was rendered as if it were a category:
                    #
                    #   g has 6 rows and 2 levels.
                    #   NAs: n = 3, 100% of valid cases.   <- missing, not a level
                    #   B:   n = 2, 67% of valid cases.
                    #   A:   n = 1, 33% of valid cases.
                    #
                    # The percentage is missing/valid, so it exceeds 100% whenever
                    # missing outnumbers observed. table() also keeps declared but
                    # unobserved levels, which summary() did, so a level with no
                    # cases is still shown.
                    tbl <- table(as.factor(mydata[[myvar]]), useNA = "no")
                    summar <- data.frame(
                        level = names(tbl),
                        n = as.numeric(tbl),
                        stringsAsFactors = FALSE
                    )
                    # Only NOMINAL variables are re-ordered by descending count.
                    # jamovi delivers an Ordinal column as an ordered factor
                    # (jmvcore::columnType maps `ordered` -> "ordinal"), and its
                    # level order is the clinical scale itself - grade, pT/pN,
                    # stage, ISUP. Sorting those by frequency printed
                    # G2, G1, G3, G4 for a G1<G2<G3<G4 factor.
                    if (!is.ordered(mydata[[myvar]]))
                        summar <- dplyr::arrange(summar, dplyr::desc(n))
                    summar$validtotal <- valid_obs

                    # Build a description for each level showing count and percentage.
                    description <- summar %>%
                        dplyr::mutate(
                            percent = n / validtotal,
                            level_description = glue::glue(
                                .("{level}: n = {n}, {percent} of valid cases."),
                                level = htmltools::htmlEscape(level),
                                n = n,
                                # accuracy is pinned so that this panel and the
                                # copy-ready sentences never print two different
                                # percentages for the same count. With the
                                # default accuracy = NULL, scales re-derives the
                                # precision from each variable's own spread: it
                                # printed 57% here, 39.76% for the next variable
                                # and 57.4% in the sentence below. The
                                # copy-ready panel now calls this same formatter.
                                percent = scales::percent(percent, accuracy = 0.1)
                            )
                        ) %>%
                        dplyr::pull(level_description)

                    # Create overall summary sentences with HTML tags for styling.
                    #
                    # The headline used to report only the OBSERVED level count while
                    # the list below it showed every declared level, so a factor with
                    # an unused level read "has 4 rows and 2 levels" and then listed
                    # three. Report the number listed, and say how many of them have
                    # no cases - a declared category with zero observations is
                    # clinically meaningful (no G3 tumours in this cohort), so it is
                    # worth naming rather than hiding.
                    n_listed <- nrow(summar)
                    n_empty <- sum(summar$n == 0)
                    # Singular/plural is branched in R rather than left to a
                    # single string: the source language is what users see, so
                    # "1 levels" cannot be fixed by a translator downstream.
                    rows_phrase <- if (total_obs == 1) .("1 row") else
                        glue::glue(.("{n} rows"), n = total_obs)
                    levels_phrase <- if (n_listed == 1) .("1 level") else
                        glue::glue(.("{n} levels"), n = n_listed)
                    sentence1 <- glue::glue(.("<strong>{var}</strong> has {rows} and {levels}."),
                        var = htmltools::htmlEscape(myvar),
                        rows = rows_phrase,
                        levels = levels_phrase)
                    if (n_empty > 0) {
                        empty_phrase <- if (n_empty == 1)
                            .("1 of these levels has no observations.") else
                            glue::glue(.("{count} of these levels have no observations."),
                                count = n_empty)
                        sentence1 <- paste0(sentence1, " ", empty_phrase)
                    }
                    sentence2 <- glue::glue(
                        .("Missing values: {count}. Percentages above are of {valid} valid cases."),
                        count = missing_obs,
                        valid = valid_obs
                    )
                    full_description <- paste(c(sentence1, description, sentence2), collapse = "<br>")
                    return(full_description)
                }

                # Generate summaries for all selected variables and combine them.
                summaries <- purrr::map(.x = myvars, .f = catsummary)
                summary_text <- paste(summaries, collapse = "<br><br>")
                self$results$text$setContent(summary_text)

                # RESTORED: Use gtExtras as intended - it works with categorical data too
                plot_dataset <- tryCatch({
                    # Primary approach: Use gtExtras with proper categorical data handling
                    cat_vars <- myvars[sapply(mydata[myvars], function(x) is.factor(x) || is.character(x))]

                    if (length(cat_vars) > 0) {
                        clean_data <- mydata[cat_vars]

                        # Convert character to factor for better handling
                        clean_data <- as.data.frame(lapply(clean_data, function(x) {
                            if (is.character(x)) as.factor(x) else x
                        }))

                        # Use gtExtras::gt_plt_summary with proper configuration for categorical data
                        gt_table <- clean_data %>%
                            gtExtras::gt_plt_summary() %>%
                            gt::tab_header(
                                title = gt::md(glue::glue("**{title}**", title = .("Categorical Variables Summary"))),
                                subtitle = .("Distribution and missing value analysis")
                            ) %>%
                            gt::tab_options(
                                table.font.size = 12,
                                heading.title.font.size = 14,
                                heading.subtitle.font.size = 11,
                                table.width = gt::pct(100)
                            )

                        # Convert to HTML using proper gt method
                        html_output <- gt::as_raw_html(gt_table)
                        htmltools::HTML(html_output)
                    } else {
                        htmltools::HTML(glue::glue("<div style='padding: 15px; background-color: rgba(138, 155, 172, 0.06); border: 1px solid #dee2e6; border-radius: 4px; color: inherit;'><p>{msg}</p></div>",
                            msg = .("No categorical variables found.")))
                    }
                }, error = function(e) {
                    # Enhanced fallback with better styling
                    tryCatch({
                        private$.gtExtras_style_fallback_cat(mydata, myvars)
                    }, error = function(e2) {
                        # Final fallback to simple table. It is wrapped too: this
                        # handler runs OUTSIDE the tryCatch above, so an error
                        # raised here would escape .run() as a raw R error.
                        tryCatch({
                            private$.create_simple_cat_summary_table(mydata, myvars)
                        }, error = function(e3) {
                            htmltools::HTML(glue::glue(
                                "<div style='padding: 15px; background-color: rgba(255, 202, 33, 0.23); border: 1px solid #ffeaa7; border-radius: 4px; color: inherit;'><strong>{label}:</strong> {msg} ({detail})</div>",
                                label = .("Summary table unavailable"),
                                msg = .("The summary table could not be produced. The variable summaries above are unaffected."),
                                detail = htmltools::htmlEscape(conditionMessage(e3))))
                        })
                    })
                })
                
                self$results$text1$setContent(plot_dataset)
                
                # Add clinical interpretation
                clinical_interpretation <- private$.generateClinicalInterpretation(myvars, mydata)
                self$results$clinicalSummary$setContent(clinical_interpretation)

                # Add copy-ready report sentences
                report_sentences <- private$.generateReportSentences(myvars, mydata)
                self$results$reportSentences$setContent(report_sentences)

                # (aboutAnalysis is populated at the top of .run(), before the
                # early-return branches, because it is declared visible: true.)

                # Add assumptions and data quality guidance
                assumptions_content <- private$.generateAssumptionsContent()
                
                # Add misuse detection
                misuse_warnings <- private$.detectMisusePatterns(mydata, myvars)
                if (length(misuse_warnings) > 0) {
                    warning_content <- glue::glue(
                        "<div style='padding: 15px; background-color: rgba(255, 202, 33, 0.23); border: 1px solid #ffeaa7; border-radius: 4px; color: inherit; margin-top: 10px;'>
                        <strong>{title}:</strong><br>{warnings}</div>",
                        title = .("Statistical Guidance"),
                        warnings = paste(misuse_warnings, collapse = "<br>")
                    )
                    assumptions_content <- paste(assumptions_content, warning_content, sep = "<br>")
                }
                
                self$results$assumptions$setContent(assumptions_content)
            }
        },

        # Clear every content item at the top of .run(). Result items are only
        # auto-cleared by clearWith, which watches OPTIONS - and `vars` is the
        # only option this analysis has. Without an explicit reset, editing the
        # data or applying a row filter leaves the previous run's summaries on
        # screen while a validation branch prints an error above them.
        .resetOutputs = function() {
            for (item in c("todo", "text", "text1", "clinicalSummary",
                           "reportSentences", "assumptions")) {
                self$results[[item]]$setContent("")
            }
            for (item in c("error", "dataWarnings")) {
                self$results[[item]]$setContent("")
                self$results[[item]]$setVisible(FALSE)
            }
        },

        # Simple categorical summary table without resource-intensive operations
        .create_simple_cat_summary_table = function(dataset, var_list) {
            # Filter to categorical/factor variables only
            cat_vars <- var_list[sapply(dataset[var_list], function(x) is.factor(x) || is.character(x))]
            
            if (length(cat_vars) == 0) {
                return(htmltools::HTML(glue::glue("<p>{msg}</p>", 
                    msg = .("No categorical variables available for summary table."))))
            }
            
            # Create simple HTML table. The heading says the same thing as the
            # gtExtras output, so say plainly that this is the reduced version.
            html <- glue::glue("<p style='margin: 0 0 8px 0;'><em>{msg}</em></p>",
                msg = .("The visual distribution summary could not be produced; counts only are shown below."))
            html <- paste0(html, "<table style='border-collapse: collapse; margin: 10px 0; width: 100%;'>")
            html <- paste0(html, "<tr style='background-color: rgba(138, 155, 172, 0.06); color: inherit;'>")
            html <- paste0(html, "<th style='border: 1px solid #ccc; padding: 8px;'>", .("Variable"), "</th>")
            html <- paste0(html, "<th style='border: 1px solid #ccc; padding: 8px;'>", .("Levels"), "</th>")
            html <- paste0(html, "<th style='border: 1px solid #ccc; padding: 8px;'>", .("N"), "</th>")
            html <- paste0(html, "<th style='border: 1px solid #ccc; padding: 8px;'>", .("Missing"), "</th>")
            html <- paste0(html, "</tr>")
            
            for (var in cat_vars) {
                data_col <- dataset[[var]]
                
                # Convert to factor if character
                if (is.character(data_col)) {
                    data_col <- factor(data_col)
                }
                
                levels_count <- length(levels(data_col))
                n_valid <- sum(!is.na(data_col))
                n_missing <- sum(is.na(data_col))
                
                html <- paste0(html, "<tr>")
                html <- paste0(html, "<td style='border: 1px solid #ccc; padding: 8px; font-weight: bold;'>", htmltools::htmlEscape(var), "</td>")
                html <- paste0(html, "<td style='border: 1px solid #ccc; padding: 8px; text-align: center;'>", levels_count, "</td>")
                html <- paste0(html, "<td style='border: 1px solid #ccc; padding: 8px; text-align: center;'>", n_valid, "</td>")
                html <- paste0(html, "<td style='border: 1px solid #ccc; padding: 8px; text-align: center;'>", n_missing, "</td>")
                html <- paste0(html, "</tr>")
            }
            
            html <- paste0(html, "</table>")
            return(htmltools::HTML(html))
        },
        
        # Format a percentage (already on the 0-100 scale) for display. A rate
        # that is non-zero but rounds to 0.0 is printed as "<0.1", never as "0":
        # printing "0%" beside a non-zero missing count is a contradiction.
        .fmtPercent = function(x) {
            if (is.finite(x) && x > 0 && round(x, 1) == 0)
                "<0.1"
            else
                as.character(round(x, 1))
        },

        # Generate clinical interpretation content
        .generateClinicalInterpretation = function(variables, data) {
            n_vars <- length(variables)
            n_patients <- nrow(data)
            
            # Data completeness. Report the WORST variable, not just the mean:
            # averaging hides a single 90%-missing column behind 20 complete ones,
            # and the panel below (Data Quality) would then contradict this one.
            missing_summary <- sapply(data[variables], function(x) sum(is.na(x)) / length(x) * 100)
            # Test the UNROUNDED maximum. round(0.033, 1) is 0, so rounding before
            # the zero test made 1 missing value in 3000 rows render "No missing
            # values in any of the selected variables" directly under a Variable
            # Summaries panel reading "Missing values: 1".
            raw_max <- max(missing_summary)
            avg_missing <- private$.fmtPercent(mean(missing_summary))
            min_missing <- private$.fmtPercent(min(missing_summary))
            max_missing <- private$.fmtPercent(raw_max)
            worst_var <- paste(htmltools::htmlEscape(
                names(missing_summary)[missing_summary == max(missing_summary)]),
                collapse = ", ")
            
            # High-level clinical interpretation
            interpretation <- glue::glue(
                "<div style='padding: 20px; background-color: rgba(33, 159, 33, 0.1); border-left: 4px solid #28a745; margin: 10px 0; color: inherit;'>
                <h4 style='margin-top: 0;'>{title}</h4>
                <p><strong>{summary}:</strong> {desc}</p>
                <p><strong>{quality}:</strong> {quality_desc}</p>
                <p><strong>{clinical_use}:</strong> {use_desc}</p>
                </div>",
                title = .("Clinical Summary"),
                summary = .("Dataset Overview"),
                desc = if (n_vars == 1) {
                    glue::glue(.("Analysis of 1 categorical variable from {patients} cases."),
                               patients = n_patients)
                } else {
                    glue::glue(.("Analysis of {n} categorical variables from {patients} cases."),
                               n = n_vars, patients = n_patients)
                },
                quality = .("Data Completeness"),
                # State what was measured instead of grading it. "Excellent" is an
                # unsupported judgement on someone's patient data, and it was
                # driven by the MEAN, so it stayed green while one variable was
                # 90% missing.
                quality_desc = if (raw_max == 0) {
                    .("No missing values in any of the selected variables.")
                } else if (n_vars == 1) {
                    glue::glue(.("Missing values: {rate}% of cases."), rate = max_missing)
                } else {
                    glue::glue(
                        .("Missing values range from {min}% to {max}% across the selected variables (mean {avg}%); highest: {var}."),
                        min = min_missing, max = max_missing, avg = avg_missing,
                        var = worst_var)
                },
                clinical_use = .("Clinical Applications"),
                use_desc = .("These summaries are suitable for baseline characteristics tables, data quality assessment, and descriptive analysis in clinical research.")
            )
            
            return(interpretation)
        },
        
        # Generate copy-ready clinical report sentences
        .generateReportSentences = function(variables, data) {
            sentences <- c()
            
            for (var in variables) {
                var_data <- data[[var]]
                n_total <- length(var_data)
                n_missing <- sum(is.na(var_data))
                n_valid <- n_total - n_missing
                
                if (n_valid > 0) {
                    # Get frequency distribution.
                    # which.max() silently returns the FIRST maximum, so a 50/50
                    # split used to be published as "the most common category was
                    # 'A'". These sentences are meant to be pasted into
                    # manuscripts, so a tie has to be named as a tie.
                    freq_table <- table(var_data, useNA = "no")
                    most_common_n <- max(freq_table)
                    most_common <- names(freq_table)[freq_table == most_common_n]
                    most_common_pct <- scales::percent(most_common_n / n_valid, accuracy = 0.1)

                    sentence <- if (length(most_common) > 1) {
                        glue::glue(
                            .("For {variable}, {categories} were equally the most frequent categories (n = {n} of {valid} valid cases each, {percent} each)."),
                            variable = htmltools::htmlEscape(var),
                            categories = paste0("'", htmltools::htmlEscape(most_common), "'", collapse = ", "),
                            n = most_common_n,
                            valid = n_valid,
                            percent = most_common_pct
                        )
                    } else {
                        glue::glue(
                            .("For {variable}, the most common category was '{category}' (n = {n} of {valid} valid cases, {percent})."),
                            variable = htmltools::htmlEscape(var),
                            category = htmltools::htmlEscape(most_common),
                            n = most_common_n,
                            valid = n_valid,
                            percent = most_common_pct
                        )
                    }
                    
                    if (n_missing > 0) {
                        # "{n} of {total} cases" also removes the "1 cases" plural
                        # bug without needing a singular/plural branch.
                        missing_pct <- private$.fmtPercent(n_missing / n_total * 100)
                        sentence <- paste(sentence,
                                        glue::glue(.("Missing data: {n} of {total} cases ({percent}%)."),
                                                 n = n_missing, total = n_total, percent = missing_pct))
                    }
                    
                    sentences <- c(sentences, sentence)
                }
            }
            
            if (length(sentences) > 0) {
                report_content <- glue::glue(
                    "<div style='padding: 15px; background-color: rgba(138, 155, 172, 0.06); border: 1px solid #dee2e6; border-radius: 4px; color: inherit;'>
                    <h5 style='margin-top: 0;'>{title}</h5>
                    <div style='font-family: Georgia, serif; line-height: 1.6;'>
                    {content}
                    </div>
                    <small style='margin-top: 10px; display: block; opacity: 0.8;'>{note}</small>
                    </div>",
                    title = .("Copy-Ready Clinical Summary"),
                    content = paste(sentences, collapse = "<br><br>"),
                    note = .("Copy these sentences directly into clinical reports or manuscripts.")
                )
            } else {
                report_content <- glue::glue(
                    "<div style='padding: 15px; background-color: rgba(138, 155, 172, 0.06); border: 1px solid #dee2e6; border-radius: 4px; color: inherit;'>
                    <p>{msg}</p>
                    </div>",
                    msg = .("No valid categorical data available for report generation.")
                )
            }
            
            return(report_content)
        },
        
        # Generate about content explaining the analysis
        .generateAboutContent = function() {
            about_content <- glue::glue(
                "<div style='padding: 20px; background-color: rgba(33, 152, 239, 0.13); border-left: 4px solid #2196f3; margin: 10px 0; color: inherit;'>
                <h4 style='margin-top: 0;'>{title}</h4>
                
                <h5>{what_title}</h5>
                <p>{what_desc}</p>
                
                <h5>{when_title}</h5>
                <ul>
                <li>{when_1}</li>
                <li>{when_2}</li>
                <li>{when_3}</li>
                <li>{when_4}</li>
                </ul>
                
                <h5>{how_title}</h5>
                <ol>
                <li>{how_1}</li>
                <li>{how_2}</li>
                <li>{how_3}</li>
                </ol>
                
                <h5>{output_title}</h5>
                <p>{output_desc}</p>
                </div>",
                title = .("About Categorical Variable Analysis"),
                what_title = .("What This Analysis Does"),
                what_desc = .("This tool generates comprehensive frequency distributions and descriptive statistics for categorical (nominal/ordinal) variables, including counts, percentages, missing value patterns, and data quality metrics."),
                when_title = .("When to Use This Analysis"),
                when_1 = .("Creating baseline characteristics tables for research papers"),
                when_2 = .("Assessing data quality and completeness before main analysis"),
                when_3 = .("Exploring categorical variable distributions in clinical datasets"),
                when_4 = .("Generating descriptive statistics for pathology or clinical reports"),
                how_title = .("How to Use"),
                how_1 = .("Select categorical variables (factors or text variables) from your dataset"),
                how_2 = .("Review the variable summaries and data quality metrics"),
                how_3 = .("Use the copy-ready sentences for clinical reports if needed"),
                output_title = .("Outputs Provided"),
                output_desc = .("Variable-by-variable summaries with counts and percentages, visual summary table, clinical interpretation, and copy-ready report sentences.")
            )
            
            return(about_content)
        },
        
        # Generate assumptions and data quality content
        # (static guidance; dynamic checks live in .detectMisusePatterns)
        .generateAssumptionsContent = function() {
            assumptions_content <- glue::glue(
                "<div style='padding: 20px; background-color: rgba(255, 169, 33, 0.14); border-left: 4px solid #ff9800; margin: 10px 0; color: inherit;'>
                <h4 style='margin-top: 0;'>{title}</h4>
                
                <h5>{data_title}</h5>
                <ul>
                <li>{data_1}</li>
                <li>{data_2}</li>
                <li>{data_3}</li>
                </ul>
                
                <h5>{consider_title}</h5>
                <ul>
                <li>{consider_1}</li>
                <li>{consider_2}</li>
                <li>{consider_3}</li>
                <li>{consider_4}</li>
                </ul>
                </div>",
                title = .("Data Quality & Statistical Considerations"),
                data_title = .("Data Requirements"),
                data_1 = .("Variables should be truly categorical (nominal or ordinal)"),
                data_2 = .("Each category should have sufficient sample size for reliable percentages"),
                data_3 = .("Missing data patterns should be examined for potential bias"),
                consider_title = .("Important Considerations"),
                consider_1 = .("Variables with more than 20 categories may need recoding for analysis"),
                consider_2 = .("Very sparse categories (fewer than 5 cases) may need combination"),
                consider_3 = .("High missing data rates (over 20%) require careful interpretation"),
                consider_4 = .("Ordinal variables should maintain their natural ordering")
            )
            
            return(assumptions_content)
        },
        
        # Detect common misuse patterns
        .detectMisusePatterns = function(data, variables) {
            warnings <- c()

            for (var in variables) {
                var_data <- data[[var]]

                # Check for too many levels
                n_levels <- length(unique(var_data[!is.na(var_data)]))
                if (n_levels > 20) {
                    warnings <- c(warnings, glue::glue(
                        .("Variable '{var}' has {n} observed categories. Variables with more than 20 categories are usually easier to interpret after recoding."),
                        var = htmltools::htmlEscape(var), n = n_levels
                    ))
                }
                
                # Check for sparse categories
                if (n_levels > 1) {
                    # Count only categories that were actually OBSERVED but rare.
                    # table() keeps declared-but-unused levels at 0, and 0 < 5, so a
                    # factor with five declared levels of which only two occur (30
                    # cases each - nothing rare at all) produced "3 categories with
                    # <5 cases. Consider combining rare categories." Empty categories
                    # cannot be combined, so the advice was unactionable.
                    #
                    # The count is reported against the number of observed
                    # categories. There used to be an additional
                    # sparse/observed > 0.3 gate that the message said nothing
                    # about, so 2 singletons among 7 categories - two cells that
                    # will break any chi-square or logistic model - produced no
                    # guidance at all, and the user could not tell that apart
                    # from having no sparse categories.
                    freq_table <- table(var_data, useNA = "no")
                    observed <- freq_table[freq_table > 0]
                    sparse_categories <- sum(observed < 5)
                    if (sparse_categories > 0) {
                        warnings <- c(warnings, glue::glue(
                            .("Variable '{var}' has {n} of {total} observed categories with fewer than 5 cases. Sparse cells make chi-square and regression estimates unstable; consider combining rare categories."),
                            var = htmltools::htmlEscape(var),
                            n = sparse_categories,
                            total = length(observed)
                        ))
                    }
                }
                
                # Check missing data rate
                missing_rate <- sum(is.na(var_data)) / length(var_data)
                if (missing_rate > 0.2) {
                    warnings <- c(warnings, glue::glue(
                        .("Variable '{var}' has {rate}% missing data. High missing rates may indicate data quality issues."),
                        var = htmltools::htmlEscape(var), rate = round(missing_rate * 100, 1)
                    ))
                }
            }
            
            return(warnings)
        },


        # Fallback with gtExtras-style appearance for categorical data
        .gtExtras_style_fallback_cat = function(dataset, var_list) {
            # Get categorical variables only
            cat_vars <- var_list[sapply(dataset[var_list], function(x) is.factor(x) || is.character(x))]
            
            if (length(cat_vars) == 0) {
                return(htmltools::HTML(glue::glue("<p>{msg}</p>", 
                    msg = .("No categorical variables available for summary table."))))
            }
            
            # Calculate comprehensive summary statistics for categorical data
            summary_stats <- data.frame(
                Variable = cat_vars,
                Type = rep(.("categorical"), length(cat_vars)),
                N = sapply(dataset[cat_vars], function(x) sum(!is.na(x))),
                Missing = sapply(dataset[cat_vars], function(x) sum(is.na(x))),
                # Declared levels, matching catsummary (nrow of table(as.factor(x)))
                # and .create_simple_cat_summary_table (length(levels(x))). This
                # column used to count OBSERVED levels only, so a factor with an
                # unused level read "3 levels" in the headline and "2" here.
                Levels = sapply(dataset[cat_vars], function(x) nlevels(as.factor(x))),
                Most_Common = sapply(dataset[cat_vars], function(x) {
                    tbl <- table(x, useNA = "no")
                    if (length(tbl) == 0) return("")
                    # Name every tied mode rather than letting which.max pick the
                    # first level in factor order and present it as the winner.
                    paste(names(tbl)[tbl == max(tbl)], collapse = ", ")
                }),
                Most_Common_N = sapply(dataset[cat_vars], function(x) {
                    tbl <- table(x, useNA = "no")
                    if (length(tbl) > 0) max(tbl) else 0
                }),
                stringsAsFactors = FALSE
            )
            
            # Create gtExtras-style table for categorical data
            gt_table <- summary_stats %>%
                gt::gt() %>%
                gt::tab_header(
                    title = gt::md(glue::glue("**{title}**", title = .("Categorical Variables Summary"))),
                    subtitle = gt::md(glue::glue("*{subtitle}*", subtitle = .("Comprehensive statistics for categorical variables")))
                ) %>%
                gt::cols_label(
                    Variable = .("Variable"),
                    Type = .("Type"),
                    N = .("N"),
                    Missing = .("Missing"),
                    Levels = .("Levels"),
                    Most_Common = .("Most Common"),
                    Most_Common_N = .("Count")
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
            
            # Convert to HTML using the documented gt API (same as primary path).
            # Prefixed with a plain statement that this is the fallback: it carries
            # the same title as the gtExtras output but different columns and no
            # distribution plots, so without this the substitution is invisible.
            notice <- glue::glue("<p style='margin: 0 0 8px 0;'><em>{msg}</em></p>",
                msg = .("The visual distribution summary could not be produced; counts only are shown below."))
            return(htmltools::HTML(paste0(notice, gt::as_raw_html(gt_table))))
        }
    )
)

