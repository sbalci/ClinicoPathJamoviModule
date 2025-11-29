#' @title Date Field Correction and Standardization
#' @return Corrected and standardized date fields using multiple packages
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom magrittr %>%
#' @importFrom datefixR fix_date_char fix_date_df
#' @importFrom anytime anytime anydate
#' @importFrom lubridate dmy mdy ymd parse_date_time
#' @importFrom dplyr mutate select bind_cols case_when
#' @importFrom htmltools HTML

datecorrectionClass <- if (requireNamespace("jmvcore")) R6::R6Class("datecorrectionClass",
    inherit = datecorrectionBase,
    private = list(
        .correction_results = NULL,

        #' @keywords internal
        .escapeVar = function(x) {
            # Escape variable names with spaces/special characters
            gsub("[^A-Za-z0-9_]+", "_", make.names(x))
        },

        .run = function() {

            # Check if required variables have been selected
            if (is.null(self$options$date_vars) || length(self$options$date_vars) == 0) {
                intro_msg <- "
                <div style='background-color: #e8f5e8; padding: 20px; border-radius: 8px; margin: 20px 0;'>
                <h3 style='color: #2e7d32; margin-top: 0;'>📅 Welcome to Date Field Correction!</h3>
                <p><strong>Comprehensive date standardization for clinical research databases</strong></p>
                <p>Handles messy date formats using multiple R packages (datefixR, anytime, lubridate)</p>
                
                <h4 style='color: #2e7d32;'>Required Variables:</h4>
                <ol>
                <li><strong>Date Variables:</strong> Select columns containing date information that need correction</li>
                </ol>
                
                <h4 style='color: #2e7d32;'>Correction Methods Available:</h4>
                <ul>
                <li><strong>Automatic Detection (datefixR):</strong> Robust format detection and correction</li>
                <li><strong>Flexible Parsing (anytime):</strong> Minimal assumptions, maximum compatibility</li>
                <li><strong>Format-Specific (lubridate):</strong> When you know the expected format</li>
                <li><strong>Multi-Method Consensus:</strong> Combines methods for maximum reliability</li>
                </ul>
                
                <h4 style='color: #2e7d32;'>Common Date Format Problems:</h4>
                <ul>
                <li><strong>Mixed Separators:</strong> 2021/03/15, 2021-03-15, 2021.03.15</li>
                <li><strong>Month Variations:</strong> March, Mar, 03, 3</li>
                <li><strong>Year Formats:</strong> 2021, 21</li>
                <li><strong>Missing Components:</strong> Missing day or month values</li>
                <li><strong>Ambiguous Formats:</strong> 03/04/21 (could be Mar 4 or Apr 3)</li>
                <li><strong>Excel Dates:</strong> Numeric values representing days since 1900</li>
                </ul>
                
                <h4 style='color: #2e7d32;'>Perfect For:</h4>
                <ul>
                <li><strong>Database Cleanup:</strong> Standardize dates before analysis</li>
                <li><strong>Multi-Source Data:</strong> Harmonize dates from different systems</li>
                <li><strong>Legacy Data:</strong> Fix dates from older clinical databases</li>
                <li><strong>Excel Imports:</strong> Handle Excel date formatting issues</li>
                <li><strong>International Data:</strong> Different regional date conventions</li>
                </ul>
                
                <h4 style='color: #2e7d32;'>Output Options:</h4>
                <ul>
                <li><strong>Corrected Data Table:</strong> Auditable table with all corrections and errors</li>
                <li><strong>Quality Assessment:</strong> Success rates and problem identification</li>
                <li><strong>Format Analysis:</strong> Understand patterns in your data</li>
                <li><strong>Detailed Reports:</strong> Document correction procedures</li>
                </ul>
                
                <p style='font-size: 12px; color: #555; margin-top: 20px;'>
                💡 <em>Professional date correction for clinical research data quality and standardization</em>
                </p>
                </div>"
                
                self$results$todo$setContent(intro_msg)
                return()
            } else {
                self$results$todo$setContent("")
            }

            # Validate dataset
            if (nrow(self$data) == 0) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'emptyDataset',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent('Dataset contains no complete rows.\n• Please check your data for missing values.\n• Ensure at least one row has complete data before running date correction.')
                self$results$insert(1, notice)
                return()
            }

            # Safely require packages
            required_packages <- c("datefixR", "anytime")
            for (pkg in required_packages) {
                if (!requireNamespace(pkg, quietly = TRUE)) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = paste0('missing_', pkg),
                        type = jmvcore::NoticeType$ERROR
                    )
                    notice$setContent(sprintf(
                        'Required package "%s" is not installed.\n• Install it using: install.packages("%s")\n• Restart jamovi after installation.\n• This package is required for date correction functionality.',
                        pkg, pkg
                    ))
                    self$results$insert(1, notice)
                    return()
                }
            }

            # Get data and variables
            dataset <- self$data
            date_vars <- self$options$date_vars

            if (length(date_vars) == 0) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'missingDateVars',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent('Date variables are required.\n• Please select at least one variable containing date information.\n• Use the "Date Variables to Correct" box in the left panel to add variables.')
                self$results$insert(1, notice)
                return()
            }

            # Validate that all selected variables exist in the dataset
            available_vars <- names(dataset)
            missing_vars <- setdiff(date_vars, available_vars)

            if (length(missing_vars) > 0) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'variablesNotFound',
                    type = jmvcore::NoticeType$ERROR
                )
                available_preview <- if (length(available_vars) > 10) {
                    paste(c(head(available_vars, 10), "..."), collapse = ", ")
                } else {
                    paste(available_vars, collapse = ", ")
                }
                notice$setContent(sprintf(
                    'Selected variables not found in dataset:\n• %s\n\nAvailable variables: %s\n\n• Check variable names for typos.\n• Ensure variables are present in the active dataset.',
                    paste(missing_vars, collapse = '\n• '),
                    available_preview
                ))
                self$results$insert(1, notice)
                return()
            }

            # Perform date correction
            correction_results <- private$.perform_date_correction(dataset, date_vars)

            # Populate corrected data table (ALWAYS shown for auditability)
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

                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'lowSuccessRate',
                    type = severity
                )

                if (success_rate < 70) {
                    notice$setContent(sprintf(
                        'Low date correction success rate: %.1f%%\n• Only %d of %d dates were successfully parsed.\n• Clinical analysis may be unreliable with <70%% success rate.\n\nConsider:\n• Using a different correction method (try "consensus")\n• Reviewing data source for systematic formatting issues\n• Manual review of failed corrections in the audit table\n• Consulting with data management team',
                        success_rate, successful_corrections, total_observations
                    ))
                } else {
                    notice$setContent(sprintf(
                        'Moderate date correction success rate: %.1f%%\n• %d of %d dates were successfully parsed.\n• Review failed corrections in the "Corrected Date Data" table.\n\nConsider:\n• Trying the "consensus" method for better results\n• Checking common error patterns in quality assessment\n• Manual verification of critical dates',
                        success_rate, successful_corrections, total_observations
                    ))
                }
                self$results$insert(2, notice)
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
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'analysisComplete',
                    type = jmvcore::NoticeType$INFO
                )
                notice$setContent(sprintf(
                    'Date correction completed successfully.\n• Processed %d observations across %d variable(s).\n• Successfully corrected %d dates (%.1f%%).\n• Full audit trail available in "Corrected Date Data" table.\n• Export table to CSV for documentation or downstream use.',
                    total_observations,
                    length(correction_results),
                    successful_corrections,
                    success_rate
                ))
                self$results$insert(999, notice)
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
                var_data <- data[[private$.escapeVar(var)]]

                # Skip if all NA
                if (all(is.na(var_data))) {
                    # Add warning notice for all-NA variable
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = paste0('allNA_', gsub("[^A-Za-z0-9]", "_", var)),
                        type = jmvcore::NoticeType$WARNING
                    )
                    notice$setContent(sprintf(
                        'Variable "%s" contains only missing values (NA).\n• No date corrections possible for this variable.\n• %d row(s) affected.\n• Consider removing this variable or checking your data source.',
                        var, length(var_data)
                    ))
                    self$results$insert(2, notice)

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
                results[[var]] <- correction_result
            }
            
            return(results)
        },

        .correct_with_datefixr = function(date_char) {
            
            tryCatch({
                # datefixR only accepts "dmy" or "mdy"
                # Map "auto" and "ymd" to default "dmy"
                fmt <- self$options$date_format
                if (fmt == "auto" || fmt == "ymd") {
                    fmt <- "dmy"
                }
                
                corrected_dates <- datefixR::fix_date_char(
                    date_char,
                    day.impute = as.integer(self$options$day_impute),
                    month.impute = as.integer(self$options$month_impute),
                    format = fmt,
                    excel = self$options$handle_excel
                )
                
                success <- !is.na(corrected_dates)
                errors <- ifelse(success, "", "Could not parse date")
                
                return(list(
                    corrected = corrected_dates,
                    success = success,
                    method_used = "datefixR",
                    errors = errors
                ))
                
            }, error = function(e) {
                return(list(
                    corrected = rep(as.Date(NA), length(date_char)),
                    success = rep(FALSE, length(date_char)),
                    method_used = "datefixR",
                    errors = rep(paste("Error:", e$message), length(date_char))
                ))
            })
        },

        .correct_with_anytime = function(date_char) {
            
            tryCatch({
                corrected_dates <- anytime::anydate(date_char, tz = self$options$timezone)
                
                success <- !is.na(corrected_dates)
                errors <- ifelse(success, "", "Could not parse date")
                
                return(list(
                    corrected = corrected_dates,
                    success = success,
                    method_used = "anytime",
                    errors = errors
                ))
                
            }, error = function(e) {
                return(list(
                    corrected = rep(as.Date(NA), length(date_char)),
                    success = rep(FALSE, length(date_char)),
                    method_used = "anytime",
                    errors = rep(paste("Error:", e$message), length(date_char))
                ))
            })
        },

        .correct_with_lubridate = function(date_char) {
            
            tryCatch({
                date_format <- self$options$date_format
                tz <- self$options$timezone
                
                corrected_dates <- switch(date_format,
                    "dmy" = lubridate::dmy(date_char, tz = tz),
                    "mdy" = lubridate::mdy(date_char, tz = tz),
                    "ymd" = lubridate::ymd(date_char, tz = tz),
                    "auto" = {
                        # Try multiple formats
                        formats <- c("dmy", "mdy", "ymd", "dmy HMS", "mdy HMS", "ymd HMS")
                        lubridate::parse_date_time(date_char, formats, tz = tz)
                    }
                )
                
                # Convert to Date class if it's POSIXct
                if (inherits(corrected_dates, "POSIXct")) {
                    corrected_dates <- as.Date(corrected_dates, tz = tz)
                }
                
                success <- !is.na(corrected_dates)
                errors <- ifelse(success, "", "Could not parse date")
                
                return(list(
                    corrected = corrected_dates,
                    success = success,
                    method_used = "lubridate",
                    errors = errors
                ))
                
            }, error = function(e) {
                return(list(
                    corrected = rep(as.Date(NA), length(date_char)),
                    success = rep(FALSE, length(date_char)),
                    method_used = "lubridate",
                    errors = rep(paste("Error:", e$message), length(date_char))
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
            
            for (i in 1:n) {
                results <- list(
                    datefixr = if(datefixr_result$success[i]) datefixr_result$corrected[i] else NA,
                    anytime = if(anytime_result$success[i]) anytime_result$corrected[i] else NA,
                    lubridate = if(lubridate_result$success[i]) lubridate_result$corrected[i] else NA
                )
                
                # Remove NA results
                valid_results <- results[!is.na(results)]
                
                if (length(valid_results) == 0) {
                    consensus_errors[i] <- "No method could parse this date"
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
                        # Methods disagree - use datefixR as primary
                        conflict_count <- conflict_count + 1
                        if (!is.na(results$datefixr)) {
                            consensus_dates[i] <- results$datefixr
                            consensus_success[i] <- TRUE
                            method_used[i] <- "datefixr (conflict)"
                            consensus_errors[i] <- "Methods disagreed, used datefixR"
                        } else {
                            consensus_dates[i] <- valid_results[[1]]
                            consensus_success[i] <- TRUE
                            method_used[i] <- paste0(names(valid_results)[1], " (conflict)")
                            consensus_errors[i] <- "Methods disagreed"
                        }
                    }
                }
            }

            # Add notice if there were conflicts
            if (conflict_count > 0) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'consensusConflicts',
                    type = jmvcore::NoticeType$INFO
                )
                notice$setContent(sprintf(
                    'Consensus method detected %d conflict(s) where date parsers disagreed.\n• Using datefixR as primary resolver when conflicts occur.\n• Review "Method" column in audit table to see affected rows.\n• Consider specifying exact date format if conflicts are widespread.\n• Conflicts often indicate ambiguous date formats (e.g., 03/04/2020 could be March 4 or April 3).',
                    conflict_count
                ))
                self$results$insert(3, notice)
            }
            
            return(list(
                corrected = consensus_dates,
                success = consensus_success,
                method_used = method_used,
                errors = consensus_errors
            ))
        },

        .generate_correction_table = function(results) {
            
            # Limit display to first 100 rows for performance
            max_display <- 100
            total_vars <- length(results)
            
            table_html <- paste0(
                "<div style='background-color: #f8f9fa; padding: 20px; border-radius: 8px; margin-bottom: 20px;'>",
                "<h3 style='color: #495057; margin-top: 0;'>📅 Date Correction Results</h3>",
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
                        "<th style='padding: 6px; border: 1px solid #dee2e6;'>Corrected</th>",
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
            
            common_errors <- list()
            method_performance <- list()
            
            for (var_name in names(results)) {
                result <- results[[var_name]]
                
                total_observations <- total_observations + length(result$original)
                successful_corrections <- successful_corrections + sum(result$success, na.rm = TRUE)
                failed_corrections <- failed_corrections + sum(!result$success, na.rm = TRUE)
                originally_na <- originally_na + sum(is.na(result$original))
                
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
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Successful Corrections:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", successful_corrections, " (", success_rate, "%)</td></tr>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Failed Corrections:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", failed_corrections, "</td></tr>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Originally Missing:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", originally_na, "</td></tr>",
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
                    "<li>✅ Excellent correction rate (≥95%) - data is ready for analysis</li>"
                )
            } else if (success_rate >= 85) {
                quality_html <- paste0(quality_html,
                    "<li>⚠️ Good correction rate (85-94%) - review failed cases</li>"
                )
            } else {
                quality_html <- paste0(quality_html,
                    "<li>❌ Low correction rate (<85%) - consider different method or manual review</li>"
                )
            }
            
            if (failed_corrections > 0) {
                quality_html <- paste0(quality_html,
                    "<li>Consider manual review of ", failed_corrections, " failed corrections</li>"
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
                "<li>Standardize to ISO format (YYYY-MM-DD) for consistency</li>",
                "<li>Document date formats used in your study protocol</li>",
                "<li>Consider validation rules for future data entry</li>",
                "</ul></div>"
            )
            
            return(format_html)
        },

        .classify_date_pattern = function(date_str) {
            # Simple pattern classification
            if (grepl("^\\d{4}-\\d{2}-\\d{2}$", date_str)) return("ISO (YYYY-MM-DD)")
            if (grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", date_str)) return("US format (MM/DD/YYYY)")
            if (grepl("^\\d{1,2}/\\d{1,2}/\\d{2}$", date_str)) return("Short US (MM/DD/YY)")
            if (grepl("^\\d{1,2}-\\d{1,2}-\\d{4}$", date_str)) return("Dash separated (DD-MM-YYYY)")
            if (grepl("^\\d{4}/\\d{1,2}/\\d{1,2}$", date_str)) return("ISO with slash (YYYY/MM/DD)")
            if (grepl("^\\d+$", date_str)) return("Numeric (Excel?)")
            if (grepl("[A-Za-z]", date_str)) return("Text month")
            return("Other/Complex")
        },

        .generate_correction_summary = function(results) {
            
            summary_html <- paste0(
                "<div style='background-color: #e8f5e8; padding: 20px; border-radius: 8px;'>",
                "<h3 style='color: #2e7d32; margin-top: 0;'>📋 Correction Summary</h3>",
                
                "<h4 style='color: #2e7d32;'>Processing Settings:</h4>",
                "<ul>",
                "<li><strong>Method:</strong> ", private$.get_method_description(), "</li>",
                "<li><strong>Date Format:</strong> ", self$options$date_format, "</li>",
                "<li><strong>Missing Day Imputation:</strong> ", self$options$day_impute, "</li>",
                "<li><strong>Missing Month Imputation:</strong> ", self$options$month_impute, "</li>",
                "<li><strong>Excel Date Handling:</strong> ", if(self$options$handle_excel) "Enabled" else "Disabled", "</li>",
                "<li><strong>Output Timezone:</strong> ", self$options$timezone, "</li>",
                "</ul>",
                
                "<h4 style='color: #2e7d32;'>Next Steps:</h4>",
                "<ul>",
                "<li>Review correction results and quality assessment</li>",
                "<li>Manually verify failed corrections if needed</li>",
                "<li>Document date handling procedures in your study protocol</li>",
                "<li>Consider using corrected dates for subsequent analyses</li>",
                "</ul></div>"
            )
            
            return(summary_html)
        },

        .generate_interpretation_guide = function() {
            
            method <- self$options$correction_method
            
            interpretation_html <- paste0(
                "<div style='background-color: #f3e5f5; padding: 20px; border-radius: 8px;'>",
                "<h3 style='color: #7b1fa2; margin-top: 0;'>📚 Date Correction Guide</h3>",
                
                "<h4 style='color: #7b1fa2;'>Current Method: ", private$.get_method_description(), "</h4>"
            )
            
            if (method == "datefixr") {
                interpretation_html <- paste0(interpretation_html,
                    "<p><strong>datefixR</strong> uses intelligent format detection to handle messy date data.</p>",
                    "<p><strong>Strengths:</strong> Robust format detection, handles missing components, good for mixed formats</p>",
                    "<p><strong>Best for:</strong> Database cleanup, legacy data, unknown formats</p>"
                )
            } else if (method == "anytime") {
                interpretation_html <- paste0(interpretation_html,
                    "<p><strong>anytime</strong> provides flexible parsing with minimal assumptions.</p>",
                    "<p><strong>Strengths:</strong> Fast processing, handles numeric dates, minimal configuration</p>",
                    "<p><strong>Best for:</strong> Standard formats, performance-critical applications</p>"
                )
            } else if (method == "lubridate") {
                interpretation_html <- paste0(interpretation_html,
                    "<p><strong>lubridate</strong> allows format-specific parsing when you know the expected format.</p>",
                    "<p><strong>Strengths:</strong> Precise control, handles time zones, extensive format support</p>",
                    "<p><strong>Best for:</strong> Consistent formats, time-sensitive data</p>"
                )
            } else if (method == "consensus") {
                interpretation_html <- paste0(interpretation_html,
                    "<p><strong>Multi-Method Consensus</strong> combines all approaches for maximum reliability.</p>",
                    "<p><strong>Strengths:</strong> Highest success rate, identifies conflicts, robust across formats</p>",
                    "<p><strong>Best for:</strong> Critical data, final cleanup, quality control</p>"
                )
            }
            
            interpretation_html <- paste0(interpretation_html,
                "<h4 style='color: #7b1fa2;'>Clinical Research Best Practices:</h4>",
                "<ul>",
                "<li><strong>Documentation:</strong> Record all date correction procedures</li>",
                "<li><strong>Validation:</strong> Sample check corrected dates against source data</li>",
                "<li><strong>Standardization:</strong> Use ISO format (YYYY-MM-DD) for analysis</li>",
                "<li><strong>Missing Data:</strong> Be transparent about imputation methods</li>",
                "<li><strong>Quality Control:</strong> Review low success rates carefully</li>",
                "</ul>",
                
                "<h4 style='color: #7b1fa2;'>Common Issues and Solutions:</h4>",
                "<ul>",
                "<li><strong>Ambiguous formats (03/04/21):</strong> Specify expected format or use regional settings</li>",
                "<li><strong>Missing components:</strong> Configure appropriate imputation values</li>",
                "<li><strong>Excel dates:</strong> Enable Excel date number handling</li>",
                "<li><strong>Multiple formats:</strong> Use consensus method or preprocess by format type</li>",
                "</ul>",
                
                "<p style='font-size: 12px; color: #7b1fa2; margin-top: 15px;'>",
                "<em>📅 Professional date correction for clinical research data quality assurance</em>",
                "</p></div>"
            )
            
            return(interpretation_html)
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