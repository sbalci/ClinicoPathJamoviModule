#' @title Date/DateTime Validation and Quality Assessment
#' @return Validated and quality-checked date/datetime fields using multiple packages
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom magrittr %>%
#' @importFrom datefixR fix_date_char fix_date_df
#' @importFrom anytime anytime anydate
#' @importFrom lubridate dmy mdy ymd dmy_hms mdy_hms ymd_hms parse_date_time
#' @importFrom dplyr mutate select bind_cols case_when
#' @importFrom htmltools HTML

datevalidatorClass <- if (requireNamespace("jmvcore")) R6::R6Class("datevalidatorClass",
    inherit = datevalidatorBase,
    private = list(
        .correction_results = NULL,
        .notices = list(),  # Track notices to avoid serialization issues

        #' @keywords internal
        .escapeVar = function(x) {
            # Escape variable names with spaces/special characters
            gsub("[^A-Za-z0-9_]+", "_", make.names(x))
        },

        #' @keywords internal
        .addNotice = function(name, type, content) {
            # Helper to safely add notices without serialization issues
            # Only create notice if it doesn't already exist for this run
            if (!(name %in% names(private$.notices))) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = name,
                    type = type
                )
                notice$setContent(content)
                self$results$insert(1, notice)
                private$.notices[[name]] <- TRUE  # Track that we added it
            }
        },

        .run = function() {
            # Clear notice tracking at start of each run
            private$.notices <- list()

            # Check if required variables have been selected
            if (is.null(self$options$date_vars) || length(self$options$date_vars) == 0) {
                intro_msg <- "
                <div style='background-color: #e8f5e8; padding: 20px; border-radius: 8px; margin: 20px 0;'>
                <h3 style='color: #2e7d32; margin-top: 0;'>🔍 Welcome to Date/DateTime Validator!</h3>
                <p><strong>Comprehensive date and datetime validation for clinical research databases</strong></p>
                <p>Validates and diagnoses messy date/datetime formats using multiple R packages (datefixR, anytime, lubridate)</p>

                <div style='background-color: #fff9c4; padding: 10px; border-radius: 5px; margin: 15px 0;'>
                <p style='margin: 0;'><strong>ℹ️ For datetime conversion with component extraction:</strong></p>
                <p style='margin: 5px 0 0 0;'>Use the <strong>DateTime Converter</strong> module to extract year, month, day, hour, minute, second components into separate columns.</p>
                </div>

                <h4 style='color: #2e7d32;'>Required Variables:</h4>
                <ol>
                <li><strong>Date/DateTime Variables:</strong> Select columns containing date or datetime information for validation</li>
                </ol>

                <h4 style='color: #2e7d32;'>Validation Methods Available:</h4>
                <ul>
                <li><strong>Automatic Detection (datefixR):</strong> Robust format detection and validation</li>
                <li><strong>Flexible Parsing (anytime):</strong> Minimal assumptions, maximum compatibility</li>
                <li><strong>Format-Specific (lubridate):</strong> When you know the expected format</li>
                <li><strong>Multi-Method Consensus:</strong> Combines methods for maximum reliability</li>
                </ul>

                <h4 style='color: #2e7d32;'>Common Date/DateTime Format Problems:</h4>
                <ul>
                <li><strong>Mixed Separators:</strong> 2021/03/15, 2021-03-15, 2021.03.15</li>
                <li><strong>Month Variations:</strong> March, Mar, 03, 3</li>
                <li><strong>Year Formats:</strong> 2021, 21</li>
                <li><strong>Time Components:</strong> With or without HMS (hours:minutes:seconds)</li>
                <li><strong>Missing Components:</strong> Missing day, month, or time values</li>
                <li><strong>Ambiguous Formats:</strong> 03/04/21 (could be Mar 4 or Apr 3)</li>
                <li><strong>Excel Dates:</strong> Numeric values representing days since 1900</li>
                </ul>

                <h4 style='color: #2e7d32;'>Perfect For:</h4>
                <ul>
                <li><strong>Data Quality Control:</strong> Validate dates/datetimes before analysis</li>
                <li><strong>Database Auditing:</strong> Identify problematic date entries</li>
                <li><strong>Multi-Source Data:</strong> Check consistency across different systems</li>
                <li><strong>Legacy Data Assessment:</strong> Evaluate dates from older clinical databases</li>
                <li><strong>Excel Imports:</strong> Diagnose Excel date formatting issues</li>
                <li><strong>International Data:</strong> Different regional date conventions</li>
                </ul>

                <h4 style='color: #2e7d32;'>Output Options:</h4>
                <ul>
                <li><strong>Validated Data Table:</strong> Auditable table with all validation results and errors</li>
                <li><strong>Quality Assessment:</strong> Success rates and problem identification</li>
                <li><strong>Format Analysis:</strong> Understand patterns in your data</li>
                <li><strong>Detailed Reports:</strong> Document validation procedures</li>
                </ul>

                <p style='font-size: 12px; color: #555; margin-top: 20px;'>
                💡 <em>Professional date/datetime validation for clinical research data quality and standardization</em>
                </p>
                </div>"

                self$results$todo$setContent(intro_msg)
                return()
            } else {
                self$results$todo$setContent("")
            }

            # Validate dataset
            if (nrow(self$data) == 0) {
                private$.addNotice(
                    'emptyDataset',
                    jmvcore::NoticeType$ERROR,
                    'Dataset contains no complete rows. • Please check your data for missing values. • Ensure at least one row has complete data before running date validation.'
                )
                return()
            }

            # Safely require packages
            required_packages <- c("datefixR", "anytime")
            missing_pkgs <- c()
            for (pkg in required_packages) {
                if (!requireNamespace(pkg, quietly = TRUE)) {
                    missing_pkgs <- c(missing_pkgs, pkg)
                }
            }

            if (length(missing_pkgs) > 0) {
                pkg_list <- paste(missing_pkgs, collapse = ", ")
                install_cmds <- paste0("install.packages(c(", paste0("'", missing_pkgs, "'", collapse = ", "), "))")
                private$.addNotice(
                    'missingPackages',
                    jmvcore::NoticeType$ERROR,
                    sprintf('Required package(s) not installed: %s • Install using: %s • Restart jamovi after installation. • These packages are required for date/datetime validation functionality.',
                            pkg_list, install_cmds)
                )
                return()
            }

            # Get data and variables
            dataset <- self$data
            date_vars <- self$options$date_vars

            if (length(date_vars) == 0) {
                private$.addNotice(
                    'missingDateVars',
                    jmvcore::NoticeType$ERROR,
                    'Date/DateTime variables are required. • Please select at least one variable containing date or datetime information. • Use the "Date/DateTime Variables to Validate" box in the left panel to add variables.'
                )
                return()
            }

            # Validate that all selected variables exist in the dataset
            available_vars <- names(dataset)
            missing_vars <- setdiff(date_vars, available_vars)

            if (length(missing_vars) > 0) {
                available_preview <- if (length(available_vars) > 10) {
                    paste(c(head(available_vars, 10), "..."), collapse = ", ")
                } else {
                    paste(available_vars, collapse = ", ")
                }
                private$.addNotice(
                    'variablesNotFound',
                    jmvcore::NoticeType$ERROR,
                    sprintf('Selected variables not found in dataset: %s • Available variables: %s • Check variable names for typos. • Ensure variables are present in the active dataset.',
                            paste(missing_vars, collapse = ', '),
                            available_preview)
                )
                return()
            }

            # Perform date/datetime validation
            correction_results <- private$.perform_date_correction(dataset, date_vars)

            # Populate validated data table (ALWAYS shown for auditability)
            private$.populate_corrected_data_table(correction_results)

            # Calculate overall success rate for quality notices
            total_observations <- 0
            successful_corrections <- 0
            for (var_name in names(correction_results)) {
                result <- correction_results[[var_name]]
                total_observations <- total_observations + length(result$original)
                successful_corrections <- successful_corrections + sum(result$success, na.rm = TRUE)
            }
            success_rate <- if (total_observations > 0) {
                round(successful_corrections / total_observations * 100, 2)
            } else {
                0
            }

            # Add quality notices based on success rate
            if (total_observations > 0 && success_rate < 85) {
                severity <- if (success_rate < 70) {
                    jmvcore::NoticeType$STRONG_WARNING
                } else {
                    jmvcore::NoticeType$WARNING
                }

                content <- if (success_rate < 70) {
                    sprintf('Low date/datetime validation success rate: %.1f%% • Only %d of %d dates/datetimes were successfully parsed. • Clinical analysis may be unreliable with <70%% success rate. • Consider: Using a different validation method (try "consensus"), Reviewing data source for systematic formatting issues, Manual review of failed validations in the audit table, Consulting with data management team.',
                            success_rate, successful_corrections, total_observations)
                } else {
                    sprintf('Moderate date/datetime validation success rate: %.1f%% • %d of %d dates/datetimes were successfully parsed. • Review failed validations in the "Validated Date/DateTime Data" table. • Consider: Trying the "consensus" method for better results, Checking common error patterns in quality assessment, Manual verification of critical dates/datetimes.',
                            success_rate, successful_corrections, total_observations)
                }

                private$.addNotice('lowSuccessRate', severity, content)
            }

            # Generate outputs
            if (self$options$show_correction_table) {
                table_html <- private$.generate_correction_table(correction_results)
                self$results$correction_table$setContent(table_html)
            }

            if (self$options$show_quality_assessment) {
                quality_html <- private$.generate_quality_assessment(correction_results)
                self$results$quality_assessment$setContent(quality_html)
            }

            if (self$options$show_format_analysis) {
                format_html <- private$.generate_format_analysis(correction_results)
                self$results$format_analysis$setContent(format_html)
            }

            if (self$options$show_correction_summary) {
                summary_html <- private$.generate_correction_summary(correction_results)
                self$results$correction_summary$setContent(summary_html)
            }

            if (self$options$show_interpretation) {
                interpretation_html <- private$.generate_interpretation_guide()
                self$results$interpretation$setContent(interpretation_html)
            }

            # Add completion summary INFO notice
            if (total_observations > 0) {
                private$.addNotice(
                    'analysisComplete',
                    jmvcore::NoticeType$INFO,
                    sprintf('Date/datetime validation completed successfully. • Processed %d observations across %d variable(s). • Successfully validated %d dates/datetimes (%.1f%%). • Full audit trail available in "Validated Date/DateTime Data" table. • Export table to CSV for documentation or downstream use.',
                            total_observations,
                            length(correction_results),
                            successful_corrections,
                            success_rate)
                )
            }

            # Store results for potential data export (future enhancement)
            private$.correction_results <- correction_results

        },

        .populate_corrected_data_table = function(results) {

            # Get table (columns already defined in .r.yaml)
            table <- self$results$corrected_data

            # Populate rows from results
            row_index <- 1
            for (var_name in names(results)) {
                result <- results[[var_name]]
                n_rows <- length(result$original)

                for (i in 1:n_rows) {
                    original_val <- if (is.na(result$original[i])) "NA" else as.character(result$original[i])
                    corrected_val <- if (is.na(result$corrected[i])) "NA" else as.character(result$corrected[i])
                    status_val <- if (result$success[i]) "Success" else "Failed"

                    # Get method (handle vector or scalar)
                    method_val <- if (length(result$method_used) == 1) {
                        result$method_used
                    } else {
                        result$method_used[i]
                    }

                    # Get error message
                    error_val <- if (length(result$errors) >= i) {
                        result$errors[i]
                    } else {
                        ""
                    }

                    table$addRow(rowKey = row_index, values = list(
                        row_num = i,
                        variable = var_name,
                        original = original_val,
                        corrected = corrected_val,
                        status = status_val,
                        method = method_val,
                        errors = error_val
                    ))

                    row_index <- row_index + 1
                }
            }
        },

        .perform_date_correction = function(data, date_vars) {

            # Defensive options access
            correction_method <- if (!is.null(self$options$correction_method)) {
                self$options$correction_method
            } else {
                "datefixr"  # Default fallback
            }
            results <- list()

            for (var in date_vars) {
                var_data <- data[[var]]

                # Skip if all NA
                if (all(is.na(var_data))) {
                    # Add warning notice for all-NA variable
                    private$.addNotice(
                        paste0('allNA_', var),
                        jmvcore::NoticeType$WARNING,
                        sprintf('Variable "%s" contains only missing values (NA). • No date/datetime validation possible for this variable. • %d row(s) affected. • Consider removing this variable or checking your data source.',
                                var, length(var_data))
                    )

                    results[[var]] <- list(
                        original = var_data,
                        corrected = rep(as.Date(NA), length(var_data)),
                        success = rep(FALSE, length(var_data)),
                        method_used = "none",
                        errors = rep("All values are NA", length(var_data))
                    )
                    next
                }

                # Convert to character for processing
                var_char <- as.character(var_data)

                if (correction_method == "datefixr") {
                    correction_result <- private$.correct_with_datefixr(var_char)
                } else if (correction_method == "anytime") {
                    correction_result <- private$.correct_with_anytime(var_char)
                } else if (correction_method == "lubridate") {
                    correction_result <- private$.correct_with_lubridate(var_char)
                } else if (correction_method == "consensus") {
                    correction_result <- private$.correct_with_consensus(var_char)
                }

                correction_result$original <- var_data
                # Plausibility flags appended to errors
                correction_result <- private$.apply_plausibility_checks(correction_result, var)
                results[[var]] <- correction_result
            }

            return(results)
        },

        .correct_with_datefixr = function(date_char) {

            tryCatch({
                fmt <- self$options$date_format

                # datefixR only supports date formats (dmy/mdy), not HMS
                # If HMS format requested, fall back to lubridate
                if (fmt %in% c("dmy_hms", "mdy_hms", "ymd_hms")) {
                    lres <- private$.correct_with_lubridate(date_char, force_format = fmt)
                    lres$method_used <- paste0("lubridate (", fmt, " - HMS not supported by datefixR)")
                    return(lres)
                }

                # datefixR supports dmy/mdy; handle auto/ymd explicitly
                if (fmt == "auto") {
                    res_dmy <- private$.attempt_datefixr(date_char, "dmy")
                    res_mdy <- private$.attempt_datefixr(date_char, "mdy")
                    # choose format with more successes; mark disagreements as ambiguous
                    success_dmy <- sum(res_dmy$success, na.rm = TRUE)
                    success_mdy <- sum(res_mdy$success, na.rm = TRUE)
                    use_dmy <- success_dmy >= success_mdy
                    chosen <- if (use_dmy) res_dmy else res_mdy
                    ambiguous <- (!is.na(res_dmy$corrected) & !is.na(res_mdy$corrected) &
                        as.Date(res_dmy$corrected) != as.Date(res_mdy$corrected))
                    chosen$ambiguous <- ambiguous
                    chosen$method_used[ambiguous] <- paste0(chosen$method_used[ambiguous], " (ambiguous dmy/mdy)")
                    if (any(ambiguous)) {
                        chosen$errors[ambiguous] <- "Ambiguous day/month; chose best-fit format"
                    }
                    return(chosen)
                }

                if (fmt == "ymd") {
                    # datefixR can't do ymd; fall back to lubridate ymd with imputation fallback
                    lres <- private$.correct_with_lubridate(date_char, force_format = "ymd")
                    lres$method_used <- "lubridate (ymd)"
                    return(lres)
                }

                return(private$.attempt_datefixr(date_char, fmt))

            }, error = function(e) {
                return(list(
                    corrected = rep(as.Date(NA), length(date_char)),
                    success = rep(FALSE, length(date_char)),
                    method_used = "datefixR",
                    errors = rep(paste("Error:", e$message), length(date_char)),
                    ambiguous = rep(FALSE, length(date_char)),
                    plausibility_flag = rep(FALSE, length(date_char))
                ))
            })
        },

        .attempt_datefixr = function(date_char, fmt) {
            corrected_dates <- datefixR::fix_date_char(
                date_char,
                day.impute = as.integer(self$options$day_impute),
                month.impute = as.integer(self$options$month_impute),
                format = fmt,
                excel = self$options$handle_excel
            )

            success <- !is.na(corrected_dates)
            errors <- ifelse(success, "", "Could not parse date")

            list(
                corrected = corrected_dates,
                success = success,
                method_used = paste0("datefixR (", fmt, ")"),
                errors = errors,
                ambiguous = rep(FALSE, length(date_char)),
                plausibility_flag = rep(FALSE, length(date_char))
            )
        },

        .correct_with_anytime = function(date_char) {

            tryCatch({
                corrected_dates <- anytime::anytime(date_char, tz = self$options$timezone)

                success <- !is.na(corrected_dates)
                errors <- ifelse(success, "", "Could not parse date/datetime")

                return(list(
                    corrected = corrected_dates,
                    success = success,
                    method_used = "anytime",
                    errors = errors,
                    ambiguous = rep(FALSE, length(date_char)),
                    plausibility_flag = rep(FALSE, length(date_char))
                ))

            }, error = function(e) {
                return(list(
                    corrected = rep(as.Date(NA), length(date_char)),
                    success = rep(FALSE, length(date_char)),
                    method_used = "anytime",
                    errors = rep(paste("Error:", e$message), length(date_char)),
                    ambiguous = rep(FALSE, length(date_char)),
                    plausibility_flag = rep(FALSE, length(date_char))
                ))
            })
        },

        .correct_with_lubridate = function(date_char, force_format = NULL) {

            tryCatch({
                date_format <- if (!is.null(force_format)) force_format else self$options$date_format
                tz <- self$options$timezone

                corrected_dates <- switch(date_format,
                    "dmy" = lubridate::dmy(date_char, tz = tz),
                    "mdy" = lubridate::mdy(date_char, tz = tz),
                    "ymd" = lubridate::ymd(date_char, tz = tz),
                    "dmy_hms" = lubridate::dmy_hms(date_char, tz = tz),
                    "mdy_hms" = lubridate::mdy_hms(date_char, tz = tz),
                    "ymd_hms" = lubridate::ymd_hms(date_char, tz = tz),
                    "auto" = {
                        # Try multiple formats including HMS
                        formats <- c("dmy", "mdy", "ymd", "dmy HMS", "mdy HMS", "ymd HMS")
                        lubridate::parse_date_time(date_char, formats, tz = tz)
                    }
                )

                success <- !is.na(corrected_dates)
                errors <- ifelse(success, "", "Could not parse date/datetime")

                return(list(
                    corrected = corrected_dates,
                    success = success,
                    method_used = paste0("lubridate (", date_format, ")"),
                    errors = errors,
                    ambiguous = rep(FALSE, length(date_char)),
                    plausibility_flag = rep(FALSE, length(date_char))
                ))

            }, error = function(e) {
                return(list(
                    corrected = rep(as.Date(NA), length(date_char)),
                    success = rep(FALSE, length(date_char)),
                    method_used = "lubridate",
                    errors = rep(paste("Error:", e$message), length(date_char)),
                    ambiguous = rep(FALSE, length(date_char)),
                    plausibility_flag = rep(FALSE, length(date_char))
                ))
            })
        },

        .correct_with_consensus = function(date_char) {

            # Try all methods and use consensus
            datefixr_result <- private$.correct_with_datefixr(date_char)
            anytime_result <- private$.correct_with_anytime(date_char)
            lubridate_result <- private$.correct_with_lubridate(date_char)

            # Track conflicts for notice
            conflict_count <- 0

            n <- length(date_char)
            consensus_dates <- rep(as.Date(NA), n)
            consensus_success <- rep(FALSE, n)
            consensus_errors <- rep("", n)
            method_used <- rep("", n)
            ambiguous_vec <- rep(FALSE, n)

            for (i in 1:n) {
                results <- list(
                    datefixr = if(datefixr_result$success[i]) datefixr_result$corrected[i] else NA,
                    anytime = if(anytime_result$success[i]) anytime_result$corrected[i] else NA,
                    lubridate = if(lubridate_result$success[i]) lubridate_result$corrected[i] else NA
                )

                # Remove NA results
                valid_results <- results[!is.na(results)]

                if (length(valid_results) == 0) {
                    consensus_errors[i] <- "No method could parse this date/datetime"
                    method_used[i] <- "none"
                } else if (length(valid_results) == 1) {
                    consensus_dates[i] <- valid_results[[1]]
                    consensus_success[i] <- TRUE
                    method_used[i] <- names(valid_results)[1]
                } else {
                    # Multiple methods succeeded - check for agreement
                    unique_dates <- unique(as.Date(unlist(valid_results)))
                    if (length(unique_dates) == 1) {
                        consensus_dates[i] <- unique_dates[1]
                        consensus_success[i] <- TRUE
                        method_used[i] <- paste(names(valid_results), collapse = "+")
                    } else {
                        # Methods disagree - flag ambiguity, prefer datefixR if available
                        conflict_count <- conflict_count + 1
                        ambiguous_vec[i] <- TRUE
                        if (!is.na(results$datefixr)) {
                            consensus_dates[i] <- results$datefixr
                            consensus_success[i] <- TRUE
                            method_used[i] <- "datefixR (ambiguous)"
                            consensus_errors[i] <- "Ambiguous format (d/m swap); defaulted to datefixR"
                        } else {
                            consensus_dates[i] <- valid_results[[1]]
                            consensus_success[i] <- TRUE
                            method_used[i] <- paste0(names(valid_results)[1], " (ambiguous)")
                            consensus_errors[i] <- "Ambiguous format; defaulted to first successful method"
                        }
                    }
                }
            }

            # Add notice if there were conflicts
            if (conflict_count > 0) {
                private$.addNotice(
                    'consensusConflicts',
                    jmvcore::NoticeType$INFO,
                    sprintf('Consensus method detected %d conflict(s) where date/datetime parsers disagreed. • Using datefixR as primary resolver when conflicts occur. • Review "Method" column in audit table to see affected rows. • Consider specifying exact date/datetime format if conflicts are widespread. • Conflicts often indicate ambiguous date formats (e.g., 03/04/2020 could be March 4 or April 3).',
                            conflict_count)
                )
            }

            return(list(
                corrected = consensus_dates,
                success = consensus_success,
                method_used = method_used,
                errors = consensus_errors,
                ambiguous = ambiguous_vec,
                plausibility_flag = rep(FALSE, length(date_char))
            ))
        },

        .generate_correction_table = function(results) {

            # Limit display to first 100 rows for performance
            max_display <- 100
            total_vars <- length(results)

            table_html <- paste0(
                "<div style='background-color: #f8f9fa; padding: 20px; border-radius: 8px; margin-bottom: 20px;'>",
                "<h3 style='color: #495057; margin-top: 0;'>🔍 Date/DateTime Validation Results</h3>",
                "<p><strong>Method:</strong> ", private$.get_method_description(), "</p>",
                "<p><strong>Variables Processed:</strong> ", total_vars, "</p>"
            )

            # Summary statistics
            total_success <- 0
            total_count <- 0
            for (var_name in names(results)) {
                result <- results[[var_name]]
                total_success <- total_success + sum(result$success, na.rm = TRUE)
                total_count <- total_count + length(result$success)
            }

            success_rate <- round(total_success / total_count * 100, 2)
            table_html <- paste0(table_html,
                "<p><strong>Overall Success Rate:</strong> ", total_success, "/", total_count, " (", success_rate, "%)</p>",
                "</div>"
            )

            # Detailed results for each variable
            for (var_name in names(results)) {
                result <- results[[var_name]]
                var_success <- sum(result$success, na.rm = TRUE)
                var_total <- length(result$success)
                var_rate <- round(var_success / var_total * 100, 2)

                table_html <- paste0(table_html,
                    "<div style='background-color: #ffffff; padding: 15px; border-radius: 8px; margin-top: 20px;'>",
                    "<h4>", var_name, " - Success Rate: ", var_success, "/", var_total, " (", var_rate, "%)</h4>"
                )

                # Show first few examples
                n_show <- min(max_display, length(result$original))
                if (n_show > 0) {
                    table_html <- paste0(table_html,
                        "<table style='width: 100%; border-collapse: collapse; font-size: 12px;'>",
                        "<thead><tr style='background-color: #6c757d; color: white;'>",
                        "<th style='padding: 6px; border: 1px solid #dee2e6;'>Row</th>",
                        "<th style='padding: 6px; border: 1px solid #dee2e6;'>Original</th>",
                        "<th style='padding: 6px; border: 1px solid #dee2e6;'>Validated</th>",
                        "<th style='padding: 6px; border: 1px solid #dee2e6;'>Status</th>",
                        "<th style='padding: 6px; border: 1px solid #dee2e6;'>Method</th>",
                        "</tr></thead><tbody>"
                    )

                    for (i in 1:n_show) {
                        row_bg <- if (i %% 2 == 0) "#ffffff" else "#f8f9fa"
                        if (!result$success[i]) row_bg <- "#ffebee"

                        original_val <- if (is.na(result$original[i])) "NA" else as.character(result$original[i])
                        corrected_val <- if (is.na(result$corrected[i])) "NA" else as.character(result$corrected[i])
                        status <- if (result$success[i]) "✓" else "✗"
                        method <- result$method_used[i] %||% "unknown"

                        table_html <- paste0(table_html,
                            "<tr style='background-color: ", row_bg, ";'>",
                            "<td style='padding: 6px; border: 1px solid #dee2e6;'>", i, "</td>",
                            "<td style='padding: 6px; border: 1px solid #dee2e6;'>", original_val, "</td>",
                            "<td style='padding: 6px; border: 1px solid #dee2e6;'>", corrected_val, "</td>",
                            "<td style='padding: 6px; border: 1px solid #dee2e6; text-align: center;'>", status, "</td>",
                            "<td style='padding: 6px; border: 1px solid #dee2e6;'>", method, "</td>",
                            "</tr>"
                        )
                    }

                    table_html <- paste0(table_html, "</tbody></table>")

                    if (length(result$original) > max_display) {
                        table_html <- paste0(table_html,
                            "<p style='font-size: 11px; color: #666; margin-top: 10px;'>",
                            "Showing first ", max_display, " of ", length(result$original), " observations</p>"
                        )
                    }
                }

                table_html <- paste0(table_html, "</div>")
            }

            return(table_html)
        },

        .generate_quality_assessment = function(results) {

            # Calculate quality metrics
            total_observations <- 0
            successful_corrections <- 0
            failed_corrections <- 0
            originally_na <- 0
            plausibility_flags <- 0
            ambiguous_flags <- 0

            common_errors <- list()
            method_performance <- list()

            for (var_name in names(results)) {
                result <- results[[var_name]]

                total_observations <- total_observations + length(result$original)
                successful_corrections <- successful_corrections + sum(result$success, na.rm = TRUE)
                failed_corrections <- failed_corrections + sum(!result$success, na.rm = TRUE)
                originally_na <- originally_na + sum(is.na(result$original))
                if (!is.null(result$plausibility_flag)) {
                    plausibility_flags <- plausibility_flags + sum(result$plausibility_flag, na.rm = TRUE)
                }
                if (!is.null(result$ambiguous)) {
                    ambiguous_flags <- ambiguous_flags + sum(result$ambiguous, na.rm = TRUE)
                }

                # Collect error patterns
                error_msgs <- result$errors[result$errors != ""]
                for (err in unique(error_msgs)) {
                    if (err != "") {
                        common_errors[[err]] <- (common_errors[[err]] %||% 0) + sum(error_msgs == err)
                    }
                }

                # Method usage
                methods <- result$method_used
                for (method in unique(methods)) {
                    if (method != "") {
                        method_performance[[method]] <- (method_performance[[method]] %||% 0) + sum(methods == method)
                    }
                }
            }

            success_rate <- round(successful_corrections / total_observations * 100, 2)

            quality_html <- paste0(
                "<div style='background-color: #e3f2fd; padding: 20px; border-radius: 8px;'>",
                "<h3 style='color: #1976d2; margin-top: 0;'>📊 Quality Assessment</h3>",

                "<h4 style='color: #1976d2;'>Overall Performance:</h4>",
                "<table style='width: 100%; border-collapse: collapse;'>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Total Observations:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", total_observations, "</td></tr>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Successful Validations:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", successful_corrections, " (", success_rate, "%)</td></tr>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Failed Validations:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", failed_corrections, "</td></tr>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Originally Missing:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", originally_na, "</td></tr>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Plausibility Flags (out of range/future):</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", plausibility_flags, "</td></tr>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Ambiguous Formats (d/m swap):</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", ambiguous_flags, "</td></tr>",
                "</table>"
            )

            # Display method performance
            if (length(method_performance) > 0) {
                quality_html <- paste0(quality_html,
                    "<h4 style='color: #1976d2;'>Method Performance:</h4>",
                    "<table style='width: 100%; border-collapse: collapse;'>"
                )
                for (method in names(method_performance)) {
                    count <- method_performance[[method]]
                    quality_html <- paste0(quality_html,
                        "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>", method, ":</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", count, " observations</td></tr>"
                    )
                }
                quality_html <- paste0(quality_html, "</table>")
            }

            # Display common errors
            if (length(common_errors) > 0) {
                quality_html <- paste0(quality_html,
                    "<h4 style='color: #1976d2;'>Common Errors:</h4>",
                    "<table style='width: 100%; border-collapse: collapse;'>"
                )
                # Sort errors by frequency
                error_counts <- unlist(common_errors)
                sorted_errors <- sort(error_counts, decreasing = TRUE)
                for (err_msg in names(sorted_errors)) {
                    if (err_msg != "") {
                        count <- sorted_errors[[err_msg]]
                        quality_html <- paste0(quality_html,
                            "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>", err_msg, "</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", count, " occurrences</td></tr>"
                        )
                    }
                }
                quality_html <- paste0(quality_html, "</table>")
            }

            # Quality recommendations
            quality_html <- paste0(quality_html,
                "<h4 style='color: #1976d2;'>Quality Recommendations:</h4>",
                "<ul>"
            )

            if (success_rate >= 95) {
                quality_html <- paste0(quality_html,
                    "<li>✅ Excellent validation rate (≥95%) - data is ready for analysis</li>"
                )
            } else if (success_rate >= 85) {
                quality_html <- paste0(quality_html,
                    "<li>⚠️ Good validation rate (85-94%) - review failed cases</li>"
                )
            } else {
                quality_html <- paste0(quality_html,
                    "<li>❌ Low validation rate (<85%) - consider different method or manual review</li>"
                )
            }

            if (failed_corrections > 0) {
                quality_html <- paste0(quality_html,
                    "<li>Consider manual review of ", failed_corrections, " failed validations</li>"
                )
            }

            quality_html <- paste0(quality_html, "</ul></div>")

            return(quality_html)
        },

        .generate_format_analysis = function(results) {

            # Analyze patterns in original data
            format_patterns <- list()

            for (var_name in names(results)) {
                result <- results[[var_name]]
                original_vals <- as.character(result$original)
                original_vals <- original_vals[!is.na(original_vals)]

                # Count ALL occurrences, not just unique values
                for (val in original_vals) {
                    if (val != "" && !is.na(val)) {
                        # Classify basic pattern
                        pattern <- private$.classify_date_pattern(val)
                        format_patterns[[pattern]] <- (format_patterns[[pattern]] %||% 0) + 1
                    }
                }
            }

            format_html <- paste0(
                "<div style='background-color: #fff3e0; padding: 20px; border-radius: 8px;'>",
                "<h3 style='color: #ef6c00; margin-top: 0;'>🔍 Format Analysis</h3>",

                "<h4 style='color: #ef6c00;'>Detected Patterns:</h4>",
                "<table style='width: 100%; border-collapse: collapse;'>"
            )

            for (pattern in names(format_patterns)) {
                count <- format_patterns[[pattern]]
                format_html <- paste0(format_html,
                    "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>", pattern, ":</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", count, " occurrences</td></tr>"
                )
            }

            format_html <- paste0(format_html,
                "</table>",

                "<h4 style='color: #ef6c00;'>Format Recommendations:</h4>",
                "<ul>",
                "<li>Standardize to ISO format (YYYY-MM-DD or YYYY-MM-DD HH:MM:SS) for consistency</li>",
                "<li>Document date/datetime formats used in your study protocol</li>",
                "<li>Consider validation rules for future data entry</li>",
                "<li>Use DateTime Converter module for component extraction if needed</li>",
                "</ul></div>"
            )

            return(format_html)
        },

        .classify_date_pattern = function(date_str) {
            # Simple pattern classification with datetime support
            if (grepl("^\\d{4}-\\d{2}-\\d{2}\\s+\\d{2}:\\d{2}:\\d{2}", date_str)) return("ISO datetime (YYYY-MM-DD HH:MM:SS)")
            if (grepl("^\\d{4}-\\d{2}-\\d{2}$", date_str)) return("ISO date (YYYY-MM-DD)")
            if (grepl("^\\d{1,2}/\\d{1,2}/\\d{4}\\s+\\d{1,2}:\\d{2}", date_str)) return("US datetime (MM/DD/YYYY HH:MM)")
            if (grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", date_str)) return("US format (MM/DD/YYYY)")
            if (grepl("^\\d{1,2}/\\d{1,2}/\\d{2}\\s+\\d{1,2}:\\d{2}", date_str)) return("Short US datetime (MM/DD/YY HH:MM)")
            if (grepl("^\\d{1,2}/\\d{1,2}/\\d{2}$", date_str)) return("Short US (MM/DD/YY)")
            if (grepl("^\\d{1,2}-\\d{1,2}-\\d{4}$", date_str)) return("Dash separated (DD-MM-YYYY)")
            if (grepl("^\\d{4}/\\d{1,2}/\\d{1,2}$", date_str)) return("ISO with slash (YYYY/MM/DD)")
            if (grepl("^\\d+$", date_str)) return("Numeric (Excel/Unix?)")
            if (grepl("[A-Za-z]", date_str)) return("Text month")
            return("Other/Complex")
        },

        .generate_correction_summary = function(results) {

            summary_html <- paste0(
                "<div style='background-color: #e8f5e8; padding: 20px; border-radius: 8px;'>",
                "<h3 style='color: #2e7d32; margin-top: 0;'>📋 Validation Summary</h3>",

                "<h4 style='color: #2e7d32;'>Processing Settings:</h4>",
                "<ul>",
                "<li><strong>Method:</strong> ", private$.get_method_description(), "</li>",
                "<li><strong>Date/DateTime Format:</strong> ", self$options$date_format, "</li>",
                "<li><strong>Missing Day Imputation:</strong> ", self$options$day_impute, "</li>",
                "<li><strong>Missing Month Imputation:</strong> ", self$options$month_impute, "</li>",
                "<li><strong>Excel Date Handling:</strong> ", if(self$options$handle_excel) "Enabled" else "Disabled", "</li>",
                "<li><strong>Output Timezone:</strong> ", self$options$timezone, "</li>",
                "</ul>",

                "<h4 style='color: #2e7d32;'>Next Steps:</h4>",
                "<ul>",
                "<li>Review validation results and quality assessment</li>",
                "<li>Manually verify failed validations if needed</li>",
                "<li>Use DateTime Converter module if you need to extract date/time components</li>",
                "<li>Document date/datetime handling procedures in your study protocol</li>",
                "</ul></div>"
            )

            return(summary_html)
        },

        .generate_interpretation_guide = function() {

            method <- self$options$correction_method

            interpretation_html <- paste0(
                "<div style='background-color: #f3e5f5; padding: 20px; border-radius: 8px;'>",
                "<h3 style='color: #7b1fa2; margin-top: 0;'>📚 Date/DateTime Validation Guide</h3>",

                "<div style='background-color: #fff9c4; padding: 12px; border-radius: 5px; margin-bottom: 15px;'>",
                "<p style='margin: 0;'><strong>ℹ️ Related Module:</strong></p>",
                "<p style='margin: 5px 0 0 0;'>For datetime conversion with component extraction (year, month, day, hour, minute, second), ",
                "use the <strong>DateTime Converter</strong> module. This validator focuses on quality assessment and format validation.</p>",
                "</div>",

                "<h4 style='color: #7b1fa2;'>Current Method: ", private$.get_method_description(), "</h4>"
            )

            if (method == "datefixr") {
                interpretation_html <- paste0(interpretation_html,
                    "<p><strong>datefixR</strong> uses intelligent format detection to handle messy date data. ",
                    "Note: datefixR only supports date formats (not datetime with HMS).</p>",
                    "<p><strong>Strengths:</strong> Robust format detection, handles missing components, good for mixed formats</p>",
                    "<p><strong>Best for:</strong> Database cleanup, legacy data, unknown date formats</p>",
                    "<p><strong>Limitation:</strong> Does not support HMS time components - use lubridate method for datetime</p>"
                )
            } else if (method == "anytime") {
                interpretation_html <- paste0(interpretation_html,
                    "<p><strong>anytime</strong> provides flexible parsing with minimal assumptions.</p>",
                    "<p><strong>Strengths:</strong> Fast processing, handles numeric dates, handles datetime formats, minimal configuration</p>",
                    "<p><strong>Best for:</strong> Standard formats, datetime with time components, performance-critical applications</p>"
                )
            } else if (method == "lubridate") {
                interpretation_html <- paste0(interpretation_html,
                    "<p><strong>lubridate</strong> allows format-specific parsing when you know the expected format.</p>",
                    "<p><strong>Strengths:</strong> Precise control, handles time zones, extensive format support including HMS</p>",
                    "<p><strong>Best for:</strong> Consistent formats, time-sensitive data, datetime with time components</p>",
                    "<p><strong>Datetime Support:</strong> Supports dmy_hms, mdy_hms, ymd_hms formats for validation</p>"
                )
            } else if (method == "consensus") {
                interpretation_html <- paste0(interpretation_html,
                    "<p><strong>Multi-Method Consensus</strong> combines all approaches for maximum reliability.</p>",
                    "<p><strong>Strengths:</strong> Highest success rate, identifies conflicts, robust across formats</p>",
                    "<p><strong>Best for:</strong> Critical data, final cleanup, quality control</p>",
                    "<p><strong>Note:</strong> Consensus uses lubridate for datetime HMS formats when needed</p>"
                )
            }

            interpretation_html <- paste0(interpretation_html,
                "<h4 style='color: #7b1fa2;'>Clinical Research Best Practices:</h4>",
                "<ul>",
                "<li><strong>Documentation:</strong> Record all date/datetime validation procedures</li>",
                "<li><strong>Validation:</strong> Sample check validated dates against source data</li>",
                "<li><strong>Standardization:</strong> Use ISO format (YYYY-MM-DD or YYYY-MM-DD HH:MM:SS) for analysis</li>",
                "<li><strong>Missing Data:</strong> Be transparent about imputation methods</li>",
                "<li><strong>Quality Control:</strong> Review low success rates carefully</li>",
                "<li><strong>Component Extraction:</strong> Use DateTime Converter module for extracting date/time components</li>",
                "</ul>",

                "<h4 style='color: #7b1fa2;'>Common Issues and Solutions:</h4>",
                "<ul>",
                "<li><strong>Ambiguous formats (03/04/21):</strong> Specify expected format or use regional settings</li>",
                "<li><strong>Missing components:</strong> Configure appropriate imputation values</li>",
                "<li><strong>Excel dates:</strong> Enable Excel date number handling</li>",
                "<li><strong>Multiple formats:</strong> Use consensus method or preprocess by format type</li>",
                "<li><strong>Datetime with time components:</strong> Use lubridate method with HMS formats or anytime method</li>",
                "</ul>",

                "<p style='font-size: 12px; color: #7b1fa2; margin-top: 15px;'>",
                "<em>🔍 Professional date/datetime validation for clinical research data quality assurance</em>",
                "</p></div>"
            )

            return(interpretation_html)
        },

        .apply_plausibility_checks = function(correction_result, var_name) {
            corrected <- correction_result$corrected
            flags <- correction_result$plausibility_flag
            errors <- correction_result$errors
            if (is.null(flags)) flags <- rep(FALSE, length(corrected))

            # Convert to Date for range checks while keeping POSIXct possible
            corrected_date <- corrected
            if (inherits(corrected_date, "POSIXct")) {
                corrected_date <- as.Date(corrected_date)
            }

            today <- Sys.Date()
            upper <- today + 365
            years <- suppressWarnings(as.integer(format(corrected_date, "%Y")))

            out_of_range <- !is.na(corrected_date) & (years < 1900 | corrected_date > upper)
            if (any(out_of_range, na.rm = TRUE)) {
                errors[out_of_range] <- paste0(errors[out_of_range], ifelse(errors[out_of_range] != "", "; ", ""), "Out of plausible range (pre-1900 or >1 year in future)")
            }
            flags <- flags | out_of_range

            correction_result$plausibility_flag <- flags
            correction_result$errors <- errors
            correction_result
        },

        .get_method_description = function() {
            method <- self$options$correction_method

            descriptions <- list(
                "datefixr" = "Automatic Detection (datefixR)",
                "anytime" = "Flexible Parsing (anytime)",
                "lubridate" = "Format-Specific (lubridate)",
                "consensus" = "Multi-Method Consensus"
            )

            return(descriptions[[method]] %||% method)
        }

    )
)

# Store results for potential future enhancements
.correction_results <- NULL

# Null-coalescing operator helper
`%||%` <- function(x, y) if (is.null(x)) y else x
