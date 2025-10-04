#' @title DateTime Converter and Component Extractor
#'
#' @description
#' Convert datetime variables to standardized format and extract datetime components
#' (year, month, day, hour, minute, day name, week number, quarter, etc.).
#' Features automatic format detection, quality assessment, and preview of converted data.
#'
#' @details
#' This function provides comprehensive datetime conversion capabilities including:
#' \itemize{
#'   \item Automatic datetime format detection
#'   \item Multiple datetime format parsing options
#'   \item Quality assessment with min/max/missing statistics
#'   \item Preview of converted data
#'   \item Component extraction (year, month, day, hour, minute, etc.)
#'   \item Day names and month names
#'   \item Week numbers, quarters, day of year
#' }
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import lubridate
#' @import dplyr
#'

datetimeconverterClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "datetimeconverterClass",
    inherit = datetimeconverterBase,
    private = list(

        # ===================================================================
        # DATETIME FORMAT DETECTION
        # ===================================================================

        # Detect datetime format from vector
        # Automatically detects the datetime format by testing common patterns against sample values
        # @param datetime_vector Character or numeric vector containing datetime values
        # @return Character string indicating detected format (e.g., "ymd", "dmy_hms")
        # @details Tests formats in order of likelihood. Falls back to "ymd" if no format achieves >80% success rate
        .detectDatetimeFormat = function(datetime_vector) {
            # Automatic datetime format detection

            # Remove missing values for format detection
            sample_dates <- datetime_vector[!is.na(datetime_vector)]
            if (length(sample_dates) == 0) {
                stop("No valid datetime values found for format detection")
            }

            # Take a sample for format detection
            sample_dates <- head(sample_dates, min(20, length(sample_dates)))

            # Test common formats in order of likelihood
            formats_to_try <- c(
                "ymd_hms", "dmy_hms", "mdy_hms",
                "ymd_hm", "dmy_hm", "mdy_hm",
                "ymd", "dmy", "mdy",
                "ydm", "myd", "dym"
            )

            for (fmt in formats_to_try) {
                parser <- private$.getParser(fmt)

                tryCatch({
                    parsed_dates <- parser(sample_dates)
                    # If most dates parse successfully, use this format
                    success_rate <- sum(!is.na(parsed_dates)) / length(sample_dates)
                    if (success_rate > 0.8) {
                        return(fmt)
                    }
                }, error = function(e) {
                    # Continue to next format
                })
            }

            # If no format works well, default to ymd
            warning("Could not reliably detect datetime format. Using YMD format. Please specify format manually if incorrect.")
            return("ymd")
        },

        # Get lubridate parser function for format
        # @param format Character string specifying datetime format
        # @return Lubridate parser function (e.g., lubridate::ymd, lubridate::dmy_hms)
        .getParser = function(format) {
            # Return appropriate lubridate parser for format
            parser <- switch(format,
                "ymd_hms" = lubridate::ymd_hms,
                "dmy_hms" = lubridate::dmy_hms,
                "mdy_hms" = lubridate::mdy_hms,
                "ymd_hm" = lubridate::ymd_hm,
                "dmy_hm" = lubridate::dmy_hm,
                "mdy_hm" = lubridate::mdy_hm,
                "dmyhms" = lubridate::dmy_hms,
                "mdyhms" = lubridate::mdy_hms,
                "ymdhms" = lubridate::ymd_hms,
                "dmyhm" = lubridate::dmy_hm,
                "mdyhm" = lubridate::mdy_hm,
                "ymdhm" = lubridate::ymd_hm,
                "ymd" = lubridate::ymd,
                "dmy" = lubridate::dmy,
                "mdy" = lubridate::mdy,
                "ydm" = lubridate::ydm,
                "myd" = lubridate::myd,
                "dym" = lubridate::dym,
                stop(paste("Unsupported datetime format:", format))
            )
            return(parser)
        },

        .prepareDatetimeInput = function(vector) {
            # Normalise input prior to parsing and keep user-friendly display copy

            notes <- character()
            format_hint <- NULL
            parsed_dates <- NULL
            already_parsed <- FALSE

            # Keep a copy for quality metrics before coercion
            quality_vector <- vector

            if (inherits(vector, c("POSIXct", "POSIXt"))) {
                parsed_dates <- lubridate::as_datetime(vector)
                original_display <- format(vector, usetz = TRUE)
                notes <- c(notes, "Detected POSIXct/POSIXt input; using supplied datetimes directly.")
                format_hint <- "posixct"
                return(list(
                    original_display = original_display,
                    parsing_vector = parsed_dates,
                    quality_vector = quality_vector,
                    parsed_dates = parsed_dates,
                    already_parsed = TRUE,
                    format_hint = format_hint,
                    notes = notes
                ))
            }

            if (inherits(vector, "Date")) {
                parsed_dates <- as.POSIXct(vector)
                original_display <- format(vector)
                notes <- c(notes, "Detected Date input; converted to POSIXct at midnight.")
                format_hint <- "date"
                return(list(
                    original_display = original_display,
                    parsing_vector = parsed_dates,
                    quality_vector = quality_vector,
                    parsed_dates = parsed_dates,
                    already_parsed = TRUE,
                    format_hint = format_hint,
                    notes = notes
                ))
            }

            if (is.factor(vector)) {
                vector <- as.character(vector)
                quality_vector <- vector
                notes <- c(notes, "Converted factor input to character.")
            }

            if (is.numeric(vector) && !inherits(vector, "Date")) {
                non_missing <- vector[!is.na(vector)]

                if (length(non_missing) == 0) {
                    char_vals <- rep(NA_character_, length(vector))
                    return(list(
                        original_display = char_vals,
                        parsing_vector = char_vals,
                        quality_vector = vector,
                        parsed_dates = NULL,
                        already_parsed = FALSE,
                        format_hint = NULL,
                        notes = c(notes, "Numeric column contained only missing values.")
                    ))
                }

                excel_like <- all(non_missing >= 0 & non_missing <= 600000)
                unix_like <- all(non_missing >= 1e9 & non_missing <= 4e9)

                if (excel_like) {
                    parsed_dates <- as.POSIXct(vector, origin = "1899-12-30", tz = "UTC")
                    original_display <- format(vector, trim = TRUE, scientific = FALSE)
                    original_display[is.na(vector)] <- NA_character_
                    notes <- c(notes, "Detected Excel serial numbers; converted using origin 1899-12-30 (UTC).")
                    format_hint <- "excel_serial"
                    return(list(
                        original_display = original_display,
                        parsing_vector = parsed_dates,
                        quality_vector = vector,
                        parsed_dates = parsed_dates,
                        already_parsed = TRUE,
                        format_hint = format_hint,
                        notes = notes
                    ))
                }

                if (unix_like) {
                    parsed_dates <- as.POSIXct(vector, origin = "1970-01-01", tz = "UTC")
                    original_display <- format(vector, trim = TRUE, scientific = FALSE)
                    original_display[is.na(vector)] <- NA_character_
                    notes <- c(notes, "Detected Unix epoch seconds; converted using origin 1970-01-01 (UTC).")
                    format_hint <- "unix_epoch"
                    return(list(
                        original_display = original_display,
                        parsing_vector = parsed_dates,
                        quality_vector = vector,
                        parsed_dates = parsed_dates,
                        already_parsed = TRUE,
                        format_hint = format_hint,
                        notes = notes
                    ))
                }

                char_vals <- format(vector, trim = TRUE, scientific = FALSE)
                char_vals <- trimws(char_vals)
                char_vals[char_vals == ""] <- NA_character_
                char_vals[is.na(vector)] <- NA_character_
                notes <- c(notes, "Treated numeric values as formatted strings for parsing.")
                return(list(
                    original_display = char_vals,
                    parsing_vector = char_vals,
                    quality_vector = char_vals,
                    parsed_dates = NULL,
                    already_parsed = FALSE,
                    format_hint = NULL,
                    notes = notes
                ))
            }

            char_vals <- as.character(vector)
            char_vals <- trimws(char_vals)
            blank_idx <- which(char_vals == "")
            if (length(blank_idx) > 0) {
                char_vals[blank_idx] <- NA_character_
                notes <- c(notes, paste0("Converted ", length(blank_idx), " blank entries to missing."))
            }

            return(list(
                original_display = char_vals,
                parsing_vector = char_vals,
                quality_vector = char_vals,
                parsed_dates = NULL,
                already_parsed = FALSE,
                format_hint = NULL,
                notes = notes
            ))
        },

        # ===================================================================
        # CHARACTER CONVERSION UTILITY
        # ===================================================================

        # Safe character conversion with NA protection
        # Three-layer NA protection for factor/datetime to character conversion
        # Prevents "0" or empty strings from appearing as NA values
        # @param source_vector Vector to convert (factor, datetime, or other)
        # @return Character vector with proper NA preservation
        .safeCharacterConversion = function(source_vector) {
            # Three-layer NA protection for factor/datetime to character conversion
            # Prevents "0" or empty strings from appearing as NA values

            char_vector <- rep(NA_character_, length(source_vector))
            valid_idx <- !is.na(source_vector)

            if (any(valid_idx)) {
                char_vector[valid_idx] <- as.character(source_vector[valid_idx])
            }

            # Safety filter: remove spurious values
            char_vector[char_vector == "0" | char_vector == "" | is.na(char_vector)] <- NA_character_

            return(char_vector)
        },

        # ===================================================================
        # DATETIME PARSING
        # ===================================================================

        # Parse datetime using detected or selected format
        # @param datetime_vector Character/numeric vector to parse
        # @param format Format string (e.g., "ymd", "dmy_hms")
        # @param tz Timezone (default: "" for system timezone)
        # @return POSIXct datetime vector
        .parseDatetime = function(datetime_vector, format, tz = "") {
            # Parse datetime using detected or selected format

            parser <- private$.getParser(format)

            tryCatch({
                # Apply timezone if parser supports it
                parsed_dates <- parser(datetime_vector, tz = tz)
                return(parsed_dates)
            }, error = function(e) {
                stop(paste("Error parsing datetimes with format", format, ":", e$message))
            })
        },

        # ===================================================================
        # QUALITY ASSESSMENT
        # ===================================================================

        # Assess quality of datetime parsing
        # @param original Original input vector (before parsing)
        # @param parsed Parsed POSIXct datetime vector
        # @return List with quality metrics (total_observations, success_rate, min/max datetime, etc.)
        .assessQuality = function(original, parsed) {
            # Comprehensive quality assessment

            total_obs <- length(original)
            original_na <- sum(is.na(original))
            parsed_na <- sum(is.na(parsed))

            successful <- sum(!is.na(parsed) & !is.na(original))
            failed_parsing <- sum(is.na(parsed) & !is.na(original))
            non_missing_original <- total_obs - original_na

            success_rate <- if (non_missing_original > 0) {
                round(successful / non_missing_original * 100, 2)
            } else {
                NA_real_
            }

            quality_metrics <- list(
                total_observations = total_obs,
                original_missing = original_na,
                successfully_parsed = successful,
                failed_parsing = failed_parsing,
                success_rate = success_rate,
                min_datetime = if (successful > 0) min(parsed, na.rm = TRUE) else NA,
                max_datetime = if (successful > 0) max(parsed, na.rm = TRUE) else NA
            )

            # Calculate date range
            if (successful > 0 && !is.na(quality_metrics$min_datetime) && !is.na(quality_metrics$max_datetime)) {
                range_days <- as.numeric(difftime(
                    quality_metrics$max_datetime,
                    quality_metrics$min_datetime,
                    units = "days"
                ))
                quality_metrics$range_days <- round(range_days, 1)
                quality_metrics$range_years <- round(range_days / 365.25, 2)
            } else {
                quality_metrics$range_days <- NA
                quality_metrics$range_years <- NA
            }

            # Get sample of failed values
            if (failed_parsing > 0) {
                failed_indices <- which(is.na(parsed) & !is.na(original))
                sample_size <- min(10, length(failed_indices))
                quality_metrics$failed_samples <- as.character(original[head(failed_indices, sample_size)])
            } else {
                quality_metrics$failed_samples <- character(0)
            }

            return(quality_metrics)
        },

        # ===================================================================
        # COMPONENT EXTRACTION
        # ===================================================================

        # Extract datetime components (year, month, day, etc.)
        # @param parsed_dates POSIXct datetime vector
        # @return List of extracted components based on user selections
        .extractComponents = function(parsed_dates) {
            # Extract all datetime components

            components <- list()

            # Check if EITHER preview extraction OR data output is requested
            if (self$options$extract_year || self$options$year_out) {
                components$year <- lubridate::year(parsed_dates)
            }

            if (self$options$extract_month || self$options$month_out) {
                components$month <- lubridate::month(parsed_dates)
            }

            if (self$options$extract_monthname || self$options$monthname_out) {
                monthname_factor <- lubridate::month(parsed_dates, label = TRUE, abbr = FALSE)
                # Ensure NA values are preserved, not converted to empty strings or zeros
                monthname_factor[is.na(parsed_dates)] <- NA
                components$monthname <- monthname_factor
            }

            if (self$options$extract_day || self$options$day_out) {
                components$day <- lubridate::day(parsed_dates)
            }

            if (self$options$extract_hour || self$options$hour_out) {
                components$hour <- lubridate::hour(parsed_dates)
            }

            if (self$options$extract_minute || self$options$minute_out) {
                components$minute <- lubridate::minute(parsed_dates)
            }

            if (self$options$extract_second || self$options$second_out) {
                components$second <- lubridate::second(parsed_dates)
            }

            if (self$options$extract_dayname || self$options$dayname_out) {
                dayname_factor <- lubridate::wday(parsed_dates, label = TRUE, abbr = FALSE)
                # Ensure NA values are preserved, not converted to empty strings or zeros
                dayname_factor[is.na(parsed_dates)] <- NA
                components$dayname <- dayname_factor
            }

            if (self$options$extract_weeknum || self$options$weeknum_out) {
                components$weeknum <- lubridate::week(parsed_dates)
            }

            if (self$options$extract_quarter || self$options$quarter_out) {
                components$quarter <- lubridate::quarter(parsed_dates)
            }

            if (self$options$extract_dayofyear || self$options$dayofyear_out) {
                components$dayofyear <- lubridate::yday(parsed_dates)
            }

            return(components)
        },

        # ===================================================================
        # HTML PREVIEW GENERATION
        # ===================================================================

        # Update output column titles with actual variable name
        # @param datetime_var Name of the datetime variable selected by user
        # @details Replaces template strings in varTitle/varDescription with actual variable name
        .updateOutputTitles = function(datetime_var) {
            if (is.null(datetime_var) || datetime_var == "")
                return()

            fmt <- function(template) {
                if (is.null(template))
                    return(NULL)
                gsub('\\$?\\{ *datetime_var *\\}', datetime_var, template)
            }

            update_output <- function(result, title_template = NULL, description_template = NULL) {
                if (is.null(result))
                    return()
                if (!is.null(title_template))
                    result$setTitle(fmt(title_template))
                if (!is.null(description_template))
                    result$setDescription(fmt(description_template))
            }

            update_output(self$results$corrected_datetime_char,
                "Corrected DateTime - from {datetime_var}",
                "DateTime variable {datetime_var} converted to standardized format (as character string)")
            update_output(self$results$corrected_datetime_numeric,
                "Corrected DateTime Numeric - from {datetime_var}",
                "DateTime variable {datetime_var} as Unix epoch seconds for calculations")
            update_output(self$results$year_out,
                "Year - from {datetime_var}",
                "Extracted year component from {datetime_var}")
            update_output(self$results$month_out,
                "Month - from {datetime_var}",
                "Extracted month component (1-12) from {datetime_var}")
            update_output(self$results$monthname_out,
                "Month Name - from {datetime_var}",
                "Extracted month name from {datetime_var}")
            update_output(self$results$day_out,
                "Day - from {datetime_var}",
                "Extracted day of month (1-31) from {datetime_var}")
            update_output(self$results$hour_out,
                "Hour - from {datetime_var}",
                "Extracted hour component (0-23) from {datetime_var}")
            update_output(self$results$minute_out,
                "Minute - from {datetime_var}",
                "Extracted minute component (0-59) from {datetime_var}")
            update_output(self$results$second_out,
                "Second - from {datetime_var}",
                "Extracted second component (0-59) from {datetime_var}")
            update_output(self$results$dayname_out,
                "Day Name - from {datetime_var}",
                "Extracted day of week name from {datetime_var}")
            update_output(self$results$weeknum_out,
                "Week Number - from {datetime_var}",
                "Extracted week number of year (1-53) from {datetime_var}")
            update_output(self$results$quarter_out,
                "Quarter - from {datetime_var}",
                "Extracted quarter (1-4) from {datetime_var}")
            update_output(self$results$dayofyear_out,
                "Day of Year - from {datetime_var}",
                "Extracted day of year (1-366) from {datetime_var}")
        },

        # Generate HTML preview table showing conversion results
        # @param original Original display values
        # @param parsed Parsed POSIXct datetime values
        # @param n Number of rows to display
        # @return HTML string containing formatted preview table
        .generatePreviewTable = function(original, parsed, n = 50) {
            # Generate HTML preview table

            n_show <- min(n, length(original))

            if (n_show == 0) {
                return("<p>No data to preview.</p>")
            }

            table_html <- paste0(
                "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 8px; overflow-x: auto;'>",
                "<p><strong>Showing first ", n_show, " of ", length(original), " observations</strong></p>",
                "<table style='width: 100%; border-collapse: collapse; font-size: 12px;'>",
                "<thead><tr style='background-color: #6c757d; color: white;'>",
                "<th style='padding: 6px; border: 1px solid #dee2e6;'>Row</th>",
                "<th style='padding: 6px; border: 1px solid #dee2e6;'>Original Value</th>",
                "<th style='padding: 6px; border: 1px solid #dee2e6;'>Converted DateTime</th>",
                "<th style='padding: 6px; border: 1px solid #dee2e6;'>Status</th>",
                "</tr></thead><tbody>"
            )

            for (i in 1:n_show) {
                row_bg <- if (i %% 2 == 0) "#ffffff" else "#f8f9fa"

                original_val <- if (is.na(original[i])) {
                    "<em>NA</em>"
                } else {
                    as.character(original[i])
                }

                parsed_val <- if (is.na(parsed[i])) {
                    "<em>NA</em>"
                } else {
                    as.character(parsed[i])
                }

                # Status: success if parsed is not NA (unless original was NA)
                if (is.na(original[i])) {
                    status <- "<span style='color: #6c757d;'>-</span>"
                } else if (is.na(parsed[i])) {
                    status <- "<span style='color: #dc3545;'>✗</span>"
                    row_bg <- "#ffebee"
                } else {
                    status <- "<span style='color: #28a745;'>✓</span>"
                }

                table_html <- paste0(table_html,
                    "<tr style='background-color: ", row_bg, ";'>",
                    "<td style='padding: 6px; border: 1px solid #dee2e6;'>", i, "</td>",
                    "<td style='padding: 6px; border: 1px solid #dee2e6;'>", original_val, "</td>",
                    "<td style='padding: 6px; border: 1px solid #dee2e6;'>", parsed_val, "</td>",
                    "<td style='padding: 6px; border: 1px solid #dee2e6; text-align: center;'>", status, "</td>",
                    "</tr>"
                )
            }

            table_html <- paste0(table_html, "</tbody></table></div>")

            return(table_html)
        },

        # Generate HTML preview of extracted datetime components
        # @param components List of extracted components (year, month, day, etc.)
        # @param n Number of rows to display
        # @return HTML string containing formatted component preview table
        .generateComponentPreview = function(components, n = 50) {
            # Generate HTML preview of extracted components

            if (length(components) == 0) {
                return("<p>No components selected for extraction.</p>")
            }

            n_show <- min(n, length(components[[1]]))

            table_html <- paste0(
                "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 8px; overflow-x: auto;'>",
                "<p><strong>Showing first ", n_show, " of ", length(components[[1]]), " observations</strong></p>",
                "<table style='width: 100%; border-collapse: collapse; font-size: 12px;'>",
                "<thead><tr style='background-color: #6c757d; color: white;'>",
                "<th style='padding: 6px; border: 1px solid #dee2e6;'>Row</th>"
            )

            # Add column headers for each component
            for (comp_name in names(components)) {
                display_name <- switch(comp_name,
                    "year" = "Year",
                    "month" = "Month",
                    "monthname" = "Month Name",
                    "day" = "Day",
                    "hour" = "Hour",
                    "minute" = "Minute",
                    "second" = "Second",
                    "dayname" = "Day Name",
                    "weeknum" = "Week #",
                    "quarter" = "Quarter",
                    "dayofyear" = "Day of Year",
                    comp_name
                )
                table_html <- paste0(table_html,
                    "<th style='padding: 6px; border: 1px solid #dee2e6;'>", display_name, "</th>"
                )
            }

            table_html <- paste0(table_html, "</tr></thead><tbody>")

            # Add data rows
            for (i in 1:n_show) {
                row_bg <- if (i %% 2 == 0) "#ffffff" else "#f8f9fa"

                table_html <- paste0(table_html,
                    "<tr style='background-color: ", row_bg, ";'>",
                    "<td style='padding: 6px; border: 1px solid #dee2e6;'>", i, "</td>"
                )

                for (comp_name in names(components)) {
                    comp_val <- if (is.na(components[[comp_name]][i])) {
                        "<em>NA</em>"
                    } else {
                        as.character(components[[comp_name]][i])
                    }

                    table_html <- paste0(table_html,
                        "<td style='padding: 6px; border: 1px solid #dee2e6;'>", comp_val, "</td>"
                    )
                }

                table_html <- paste0(table_html, "</tr>")
            }

            table_html <- paste0(table_html, "</tbody></table></div>")

            return(table_html)
        },

        # ===================================================================
        # CLINICIAN-FRIENDLY PANEL METHODS
        # ===================================================================

        .populateQualityAssessment = function(quality, parsed_dates) {
            # Populate quality assessment panel (controlled by checkbox)
            if (!self$options$show_quality_metrics) {
                return()
            }

            # Detect misuse warnings
            misuse_warnings <- private$.detectMisuse(parsed_dates)

            # Calculate percentages
            missing_pct <- if (quality$total_observations > 0) {
                round(quality$original_missing / quality$total_observations * 100, 1)
            } else {
                0
            }

            # Generate quality summary
            quality_summary <- if (quality$success_rate >= 95) {
                "Excellent (≥95% success)"
            } else if (quality$success_rate >= 80) {
                "Good (80-94% success)"
            } else if (quality$success_rate >= 50) {
                "Fair (50-79% success)"
            } else {
                "Poor (<50% success)"
            }

            # Conditional styling
            missing_bg <- if (quality$original_missing > 0) "'#fff3cd'" else "'white'"
            parsing_bg <- if (quality$failed_parsing > 0) "'#f8d7da'" else "'#d4edda'"

            quality_html <- glue::glue("
                <div style='background-color: #f0f7ff; padding: 15px; border-left: 4px solid #2196F3;'>
                    <h4 style='margin-top: 0; color: #1565c0;'>📊 Data Quality Assessment</h4>

                    <table style='width: 100%; border-collapse: collapse; margin-top: 10px;'>
                        <tr style='background-color: #e3f2fd;'>
                            <th style='padding: 8px; text-align: left; border: 1px solid #90caf9;'>Metric</th>
                            <th style='padding: 8px; text-align: right; border: 1px solid #90caf9;'>Count</th>
                            <th style='padding: 8px; text-align: right; border: 1px solid #90caf9;'>%</th>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border: 1px solid #90caf9;'>Total Observations</td>
                            <td style='padding: 8px; text-align: right; border: 1px solid #90caf9;'>{quality$total_observations}</td>
                            <td style='padding: 8px; text-align: right; border: 1px solid #90caf9;'>100%</td>
                        </tr>
                        <tr style='background-color: {missing_bg}'>
                            <td style='padding: 8px; border: 1px solid #90caf9;'>Missing Values</td>
                            <td style='padding: 8px; text-align: right; border: 1px solid #90caf9;'>{quality$original_missing}</td>
                            <td style='padding: 8px; text-align: right; border: 1px solid #90caf9;'>{missing_pct}%</td>
                        </tr>
                        <tr style='background-color: {parsing_bg}'>
                            <td style='padding: 8px; border: 1px solid #90caf9;'>Successful Parses</td>
                            <td style='padding: 8px; text-align: right; border: 1px solid #90caf9;'>{quality$successfully_parsed}</td>
                            <td style='padding: 8px; text-align: right; border: 1px solid #90caf9;'>{round(quality$success_rate, 1)}%</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border: 1px solid #90caf9;'>Failed Parsing</td>
                            <td style='padding: 8px; text-align: right; border: 1px solid #90caf9;'>{quality$failed_parsing}</td>
                            <td style='padding: 8px; text-align: right; border: 1px solid #90caf9;'>-</td>
                        </tr>
                    </table>

                    <p style='margin-top: 10px;'><strong>Quality Score:</strong> {quality_summary}</p>
                    {if (length(misuse_warnings) > 0) paste0('<div style=\"background-color: #fff3cd; padding: 10px; margin-top: 10px; border-left: 3px solid #ffc107;\"><strong>⚠️ Warnings:</strong><ul>', paste0('<li>', misuse_warnings, '</li>', collapse=''), '</ul></div>') else ''}
                </div>
            ")

            self$results$qualityAssessment$setContent(quality_html)
        },

        .populateNaturalLanguageSummary = function(datetime_var, detected_format, quality, components) {
            # Populate natural-language summary (controlled by checkbox)
            if (!self$options$show_summary) {
                return()
            }

            # Count extractions created
            extractions <- c()
            if (self$options$extract_year || self$options$year_out) extractions <- c(extractions, "Year")
            if (self$options$extract_month || self$options$month_out) extractions <- c(extractions, "Month")
            if (self$options$extract_monthname || self$options$monthname_out) extractions <- c(extractions, "Month Name")
            if (self$options$extract_day || self$options$day_out) extractions <- c(extractions, "Day")
            if (self$options$extract_hour || self$options$hour_out) extractions <- c(extractions, "Hour")
            if (self$options$extract_minute || self$options$minute_out) extractions <- c(extractions, "Minute")
            if (self$options$extract_second || self$options$second_out) extractions <- c(extractions, "Second")
            if (self$options$extract_dayname || self$options$dayname_out) extractions <- c(extractions, "Day Name")
            if (self$options$extract_weeknum || self$options$weeknum_out) extractions <- c(extractions, "Week Number")
            if (self$options$extract_quarter || self$options$quarter_out) extractions <- c(extractions, "Quarter")
            if (self$options$extract_dayofyear || self$options$dayofyear_out) extractions <- c(extractions, "Day of Year")

            extraction_text <- if (length(extractions) > 0) {
                paste(extractions, collapse = ", ")
            } else {
                "none (preview only)"
            }

            tz_display <- if (self$options$timezone == "utc") "UTC" else Sys.timezone()

            summary_html <- glue::glue("
                <div style='background-color: #f0f7ff; padding: 15px; border: 1px solid #b3d9ff; border-radius: 5px;'>
                    <h4 style='margin-top: 0;'>📄 Analysis Summary</h4>
                    <p><strong>Source column:</strong> {datetime_var}</p>
                    <p><strong>Format detected/used:</strong> {detected_format}</p>
                    <p><strong>Timezone:</strong> {tz_display}</p>
                    <p><strong>Successful conversions:</strong> {quality$successfully_parsed}/{quality$total_observations} ({round(quality$success_rate, 1)}%)</p>
                    <p><strong>Components extracted:</strong> {extraction_text}</p>

                    <div style='background-color: #e8f4f8; padding: 10px; margin-top: 15px; border-radius: 3px;'>
                        <p style='margin: 0;'><strong>📋 Copy-Ready Summary:</strong></p>
                        <p style='font-family: monospace; font-size: 0.9em; margin: 10px 0;'>
                        We extracted {extraction_text} from {quality$total_observations} datetime values in column '{datetime_var}' using {detected_format} format, with {round(quality$success_rate, 1)}% successful parsing.
                        </p>
                    </div>
                </div>
            ")

            self$results$nlSummary$setContent(summary_html)
        },

        .populateExplanatoryPanels = function() {
            # Populate about and caveats panels (both controlled by show_explanations checkbox)
            if (!self$options$show_explanations) {
                return()
            }

            # About Panel
            about_html <- "
            <div style='background-color: #f9f9f9; padding: 15px; border-radius: 5px;'>
                <h4 style='margin-top: 0;'>📖 What This Function Does</h4>
                <p>The DateTime Converter extracts components (year, month, day, hour, etc.)
                from datetime columns and creates new variables for downstream analysis.</p>

                <h4>When to Use</h4>
                <ul>
                    <li><strong>Cohort stratification:</strong> Extract year to group patients by diagnosis year</li>
                    <li><strong>Seasonal analysis:</strong> Extract month/quarter for temporal patterns</li>
                    <li><strong>Temporal patterns:</strong> Extract day-of-week for treatment schedules</li>
                    <li><strong>Data cleaning:</strong> Convert Excel dates to standardized format</li>
                    <li><strong>Time calculations:</strong> Extract components for survival time calculations</li>
                </ul>

                <h4>Typical Outputs</h4>
                <p>New columns containing numeric or text representations of datetime components.
                These can be used for grouping, filtering, or creating time-based variables.</p>

                <h4>Clinical Examples</h4>
                <ul>
                    <li>Extract year from diagnosis date to study temporal trends in cancer incidence</li>
                    <li>Extract month to analyze seasonal variation in disease presentation</li>
                    <li>Extract day-of-week to study weekend vs weekday treatment outcomes</li>
                </ul>
            </div>
            "
            self$results$aboutPanel$setContent(about_html)

            # Caveats Panel
            caveats_html <- "
            <div style='background-color: #fff3cd; padding: 15px; border-left: 4px solid #ffc107; border-radius: 3px;'>
                <h4 style='margin-top: 0;'>⚠️ Important Considerations</h4>
                <ul>
                    <li><strong>Format matching:</strong> Date format selection must match your data.
                    Incorrect format leads to parsing failures or wrong dates.</li>

                    <li><strong>Timezone consistency:</strong> UTC vs system timezone affects time extraction.
                    Use UTC for international studies, system timezone for local data.</li>

                    <li><strong>Numeric datetime (Unix epoch):</strong> The corrected datetime numeric output
                    represents seconds since 1970-01-01 00:00:00 UTC. This value is:
                        <ul>
                            <li>✅ OS-independent (same on Windows, Mac, Linux)</li>
                            <li>✅ Timezone-independent (same datetime = same number)</li>
                            <li>✅ Suitable for calculations and comparisons</li>
                            <li>⚠️ Very large numbers (billions) - use scientific notation if needed</li>
                        </ul>
                    </li>

                    <li><strong>Excel serial dates:</strong> Different epochs exist:
                        <ul>
                            <li>Windows Excel: 1900-01-01</li>
                            <li>Mac Excel: 1904-01-01</li>
                        </ul>
                    </li>

                    <li><strong>Missing values:</strong> Invalid dates become NA in output columns.
                    Check quality metrics to ensure acceptable parsing success rate.</li>

                    <li><strong>Daylight saving time:</strong> Can cause ambiguous or non-existent times.
                    Consider using UTC for medical studies to avoid DST complications.</li>

                    <li><strong>Leap years and leap seconds:</strong> Automatically handled by lubridate,
                    but be aware when working with precise time calculations.</li>
                </ul>

                <p style='margin-top: 15px;'><strong>💡 Best Practice:</strong> Always review the
                Quality Assessment and Preview tables before adding columns to your dataset.</p>
            </div>
            "
            self$results$caveatsPanel$setContent(caveats_html)
        },

        .populateGlossary = function() {
            # Populate glossary panel (controlled by checkbox)
            if (!self$options$show_glossary) {
                return()
            }

            glossary_html <- "
            <div style='background-color: #f3e5f5; padding: 15px; border-radius: 5px;'>
                <h4 style='margin-top: 0;'>📚 Key Terms & Concepts</h4>
                <dl style='line-height: 1.6;'>
                    <dt style='font-weight: bold; margin-top: 10px;'>Excel Serial Date</dt>
                    <dd style='margin-left: 20px;'>Number of days since January 1, 1900 (Windows) or
                    January 1, 1904 (Mac). Example: 45000 represents May 18, 2023.
                    Commonly used when exporting data from Excel to text files.</dd>

                    <dt style='font-weight: bold; margin-top: 10px;'>Unix Epoch</dt>
                    <dd style='margin-left: 20px;'>Seconds since January 1, 1970 00:00:00 UTC.
                    Example: 1609459200 represents January 1, 2021 00:00:00 UTC. This numeric representation
                    is <strong>OS-independent and timezone-independent</strong> - the same datetime always produces
                    the same number regardless of computer settings. Used in databases, programming systems,
                    and recommended for datetime calculations.</dd>

                    <dt style='font-weight: bold; margin-top: 10px;'>ISO 8601</dt>
                    <dd style='margin-left: 20px;'>International standard date format: YYYY-MM-DD or
                    YYYY-MM-DD HH:MM:SS. Recommended for data exchange as it's unambiguous across
                    cultures and time zones.</dd>

                    <dt style='font-weight: bold; margin-top: 10px;'>UTC (Coordinated Universal Time)</dt>
                    <dd style='margin-left: 20px;'>Global time standard with no daylight saving
                    adjustments. Recommended for multi-center studies and international collaborations
                    to avoid timezone confusion.</dd>

                    <dt style='font-weight: bold; margin-top: 10px;'>System Timezone</dt>
                    <dd style='margin-left: 20px;'>Your computer's local timezone setting, which may
                    include daylight saving time adjustments. Appropriate for local studies within
                    a single timezone.</dd>

                    <dt style='font-weight: bold; margin-top: 10px;'>POSIXct</dt>
                    <dd style='margin-left: 20px;'>R's internal datetime format storing time as seconds
                    since Unix epoch. Allows efficient date/time calculations.</dd>

                    <dt style='font-weight: bold; margin-top: 10px;'>Parsing</dt>
                    <dd style='margin-left: 20px;'>Converting text or numeric representations of dates
                    into standardized datetime objects that can be manipulated and analyzed.</dd>

                    <dt style='font-weight: bold; margin-top: 10px;'>Quarter</dt>
                    <dd style='margin-left: 20px;'>Three-month period used in financial and medical
                    reporting: Q1 (Jan-Mar), Q2 (Apr-Jun), Q3 (Jul-Sep), Q4 (Oct-Dec).</dd>
                </dl>
            </div>
            "
            self$results$glossaryPanel$setContent(glossary_html)
        },

        .detectMisuse = function(parsed_dates) {
            # Detect potential misuse patterns and return warnings
            warnings <- c()

            # Check if numeric column selected for text parsing
            datetime_var <- self$options$datetime_var
            original_vector <- self$data[[datetime_var]]

            if (self$options$datetime_format %in% c("ymd", "dmy", "mdy", "ymd_hms", "dmy_hms", "mdy_hms") &&
                is.numeric(original_vector) && !all(is.na(original_vector))) {
                warnings <- c(warnings,
                    "You selected a text date format (YMD/DMY/MDY) but your column appears to be numeric. Consider using 'Excel Serial Date' or 'Unix Epoch' instead.")
            }

            # Check for dates before 1900 (unusual in medical data)
            if (!is.null(parsed_dates) && any(!is.na(parsed_dates))) {
                years <- lubridate::year(parsed_dates)
                if (any(years < 1900, na.rm = TRUE)) {
                    count_old <- sum(years < 1900, na.rm = TRUE)
                    warnings <- c(warnings, glue::glue(
                        "{count_old} date(s) are before 1900. This may indicate incorrect format selection or data entry errors."
                    ))
                }

                # Check for future dates (may be acceptable depending on context)
                future_dates <- parsed_dates > Sys.time()
                if (any(future_dates, na.rm = TRUE)) {
                    count_future <- sum(future_dates, na.rm = TRUE)
                    warnings <- c(warnings, glue::glue(
                        "{count_future} date(s) are in the future. Verify this is intentional (e.g., planned follow-up dates)."
                    ))
                }

                # Check for very wide date range (may indicate mixed formats)
                date_range <- diff(range(parsed_dates, na.rm = TRUE))
                if (!is.na(date_range) && as.numeric(date_range, units = "days") > 36525) { # > 100 years
                    warnings <- c(warnings,
                        "Date range spans more than 100 years. This may indicate mixed date formats or data quality issues.")
                }
            }

            return(warnings)
        },

        # ===================================================================
        # MAIN RUN METHOD
        # ===================================================================

        .run = function() {

            # Show welcome message if no variable selected
            if (is.null(self$options$datetime_var) || self$options$datetime_var == "") {
                welcome_msg <- "
                <div style='background-color: #e3f2fd; padding: 20px; border-radius: 8px; margin: 20px 0;'>
                <h3 style='color: #1976d2; margin-top: 0;'>📅 Welcome to DateTime Converter!</h3>
                <p><strong>Convert datetime variables and extract components for analysis</strong></p>

                <h4 style='color: #1976d2;'>Quick Start:</h4>
                <ol>
                <li><strong>Select DateTime Variable:</strong> Choose a column containing datetime information</li>
                <li><strong>Choose Format:</strong> Auto-detect or manually specify the datetime format</li>
                <li><strong>Extract Components:</strong> Select which datetime components to extract</li>
                <li><strong>Review Preview:</strong> Check conversion quality before adding to dataset</li>
                </ol>

                <h4 style='color: #1976d2;'>Features:</h4>
                <ul>
                <li><strong>Automatic Format Detection:</strong> Intelligently identifies datetime format</li>
                <li><strong>Quality Assessment:</strong> Min/max values, success rate, missing data</li>
                <li><strong>Component Extraction:</strong> Year, month, day, hour, minute, day name, week number, quarter, etc.</li>
                <li><strong>Preview Before Adding:</strong> See converted data before adding to dataset</li>
                </ul>

                <h4 style='color: #1976d2;'>Supported Formats:</h4>
                <ul>
                <li>YYYY-MM-DD HH:MM:SS (ISO standard with time)</li>
                <li>YYYY-MM-DD (ISO date)</li>
                <li>DD-MM-YYYY or DD/MM/YYYY (European format)</li>
                <li>MM-DD-YYYY or MM/DD/YYYY (US format)</li>
                <li>And many more variations...</li>
                </ul>

                <p style='font-size: 12px; color: #555; margin-top: 20px;'>
                💡 <em>Perfect for preparing temporal data for survival analysis, time series, and longitudinal studies</em>
                </p>
                </div>"

                self$results$welcome$setContent(welcome_msg)
                return()
            }

            # Get data
            data <- self$data
            datetime_var <- self$options$datetime_var

            private$.updateOutputTitles(datetime_var)

            if (nrow(data) == 0) {
                stop("Dataset is empty")
            }

            # Prepare datetime values for parsing
            datetime_vector <- data[[datetime_var]]
            prepared <- private$.prepareDatetimeInput(datetime_vector)

            original_display <- prepared$original_display
            parsing_vector <- prepared$parsing_vector
            quality_vector <- prepared$quality_vector
            preprocessing_notes <- prepared$notes

            # Detect or use specified format
            # Determine timezone to use
            tz_to_use <- if (self$options$timezone == "utc") "UTC" else ""

            if (prepared$already_parsed) {
                parsed_dates <- prepared$parsed_dates
                detected_format <- if (!is.null(prepared$format_hint)) {
                    prepared$format_hint
                } else {
                    "preparsed"
                }
                if (self$options$datetime_format != "auto") {
                    preprocessing_notes <- c(preprocessing_notes,
                        paste0("Manual format selection (", self$options$datetime_format,
                               ") ignored because data were already stored as datetimes."))
                }
            } else {
                if (self$options$datetime_format == "auto") {
                    detected_format <- private$.detectDatetimeFormat(parsing_vector)
                } else {
                    detected_format <- self$options$datetime_format
                }
                parsed_dates <- private$.parseDatetime(parsing_vector, detected_format, tz = tz_to_use)
            }

            # Assess quality
            quality <- private$.assessQuality(quality_vector, parsed_dates)

            # Generate format info
            format_display <- switch(detected_format,
                "ymd_hms" = "YYYY-MM-DD HH:MM:SS",
                "dmy_hms" = "DD-MM-YYYY HH:MM:SS",
                "mdy_hms" = "MM-DD-YYYY HH:MM:SS",
                "ymd" = "YYYY-MM-DD",
                "dmy" = "DD-MM-YYYY",
                "mdy" = "MM-DD-YYYY",
                "excel_serial" = "Excel Serial (days since 1899-12-30)",
                "unix_epoch" = "Unix Epoch Seconds (since 1970-01-01)",
                "posixct" = "Already formatted POSIXct/POSIXt",
                "date" = "R Date class (converted to midnight)",
                "preparsed" = "Pre-parsed datetime values",
                detected_format
            )

            format_context <- if (prepared$already_parsed) {
                "<p><em>Input was already stored as datetime values; original encoding was preserved.</em></p>"
            } else if (self$options$datetime_format == "auto") {
                "<p><em>Format was automatically detected. If results look incorrect, try manually specifying the format.</em></p>"
            } else {
                paste0("<p><em>Parsing enforced manually using format <code>",
                    self$options$datetime_format, "</code>.</em></p>")
            }

            # Add time zone note
            tz_note <- ""
            if (detected_format %in% c("excel_serial", "unix_epoch")) {
                tz_note <- "<li>Conversions from numeric formats (Excel, Unix) always use UTC time zone.</li>"
            } else if (!prepared$already_parsed) {
                if (self$options$timezone == "utc") {
                    tz_note <- "<li>String-to-datetime conversions use UTC time zone.</li>"
                } else {
                    tz_note <- paste0("<li>String-to-datetime conversions use the system's local time zone (", Sys.timezone(), ").</li>")
                }
            }

            notes_html <- ""
            if (length(preprocessing_notes) > 0 || nzchar(tz_note)) {
                notes_html <- paste0(
                    "<ul style='margin-top: 10px;'>",
                    paste0("<li>", preprocessing_notes, "</li>", collapse = ""),
                    tz_note,
                    "</ul>"
                )
            }

            format_html <- paste0(
                "<div style='background-color: #fff3e0; padding: 15px; border-radius: 8px;'>",
                "<h4 style='color: #ef6c00; margin-top: 0;'>Format Detection</h4>",
                "<p><strong>Detected/Selected Format:</strong> ", format_display, " (", detected_format, ")</p>",
                format_context,
                notes_html,
                "</div>"
            )
            self$results$formatInfo$setContent(format_html)

            # Generate quality metrics
            success_rate_display <- if (is.na(quality$success_rate)) {
                "N/A"
            } else {
                paste0(quality$success_rate, "%")
            }

            quality_html <- paste0(
                "<div style='background-color: #e8f5e8; padding: 15px; border-radius: 8px;'>",
                "<h4 style='color: #2e7d32; margin-top: 0;'>Quality Assessment</h4>",
                "<table style='width: 100%; border-collapse: collapse;'>",
                "<tr><td style='padding: 6px; border: 1px solid #ddd;'><strong>Total Observations:</strong></td><td style='padding: 6px; border: 1px solid #ddd;'>", quality$total_observations, "</td></tr>",
                "<tr><td style='padding: 6px; border: 1px solid #ddd;'><strong>Originally Missing:</strong></td><td style='padding: 6px; border: 1px solid #ddd;'>", quality$original_missing, "</td></tr>",
                "<tr><td style='padding: 6px; border: 1px solid #ddd;'><strong>Successfully Parsed:</strong></td><td style='padding: 6px; border: 1px solid #ddd;'>", quality$successfully_parsed, " (", success_rate_display, ")</td></tr>",
                "<tr><td style='padding: 6px; border: 1px solid #ddd;'><strong>Failed Parsing:</strong></td><td style='padding: 6px; border: 1px solid #ddd;'>", quality$failed_parsing, "</td></tr>"
            )

            if (!is.na(quality$min_datetime)) {
                quality_html <- paste0(quality_html,
                    "<tr><td style='padding: 6px; border: 1px solid #ddd;'><strong>Minimum DateTime:</strong></td><td style='padding: 6px; border: 1px solid #ddd;'>", quality$min_datetime, "</td></tr>",
                    "<tr><td style='padding: 6px; border: 1px solid #ddd;'><strong>Maximum DateTime:</strong></td><td style='padding: 6px; border: 1px solid #ddd;'>", quality$max_datetime, "</td></tr>",
                    "<tr><td style='padding: 6px; border: 1px solid #ddd;'><strong>Date Range:</strong></td><td style='padding: 6px; border: 1px solid #ddd;'>", quality$range_days, " days (", quality$range_years, " years)</td></tr>"
                )
            }

            quality_html <- paste0(quality_html, "</table>")

            # Add sample of failed values if any
            if (length(quality$failed_samples) > 0) {
                quality_html <- paste0(quality_html,
                    "<h5 style='color: #d32f2f; margin-top: 15px;'>Sample of Failed Values:</h5>",
                    "<ul style='color: #d32f2f;'>"
                )
                for (failed_val in quality$failed_samples) {
                    quality_html <- paste0(quality_html, "<li>", failed_val, "</li>")
                }
                quality_html <- paste0(quality_html, "</ul>")
            }

            quality_html <- paste0(quality_html, "</div>")
            self$results$qualityMetrics$setContent(quality_html)

            # Generate preview table
            preview_html <- private$.generatePreviewTable(
                original_display,
                parsed_dates,
                self$options$preview_rows
            )
            self$results$previewTable$setContent(preview_html)

            # Extract components (always extract to show preview)
            components <- private$.extractComponents(parsed_dates)

            # Generate component preview
            if (length(components) > 0) {
                component_preview_html <- private$.generateComponentPreview(
                    components,
                    self$options$preview_rows
                )
                self$results$componentPreview$setContent(component_preview_html)
            } else {
                self$results$componentPreview$setContent(
                    "<p><em>No components selected for extraction. Use the preview checkboxes or 'Add to Data' toggles in the left panel to extract datetime components.</em></p>"
                )
            }

            # Add outputs to dataset when requested
            # Use isNotFilled() to prevent duplicate writes

            # Add corrected datetime as character
            if (self$options$corrected_datetime_char && self$results$corrected_datetime_char$isNotFilled()) {
                self$results$corrected_datetime_char$setRowNums(rownames(data))
                self$results$corrected_datetime_char$setValues(
                    private$.safeCharacterConversion(parsed_dates)
                )
            }

            # Add corrected datetime as numeric
            if (self$options$corrected_datetime_numeric && self$results$corrected_datetime_numeric$isNotFilled()) {
                self$results$corrected_datetime_numeric$setRowNums(rownames(data))
                corrected_numeric <- as.numeric(parsed_dates)
                self$results$corrected_datetime_numeric$setValues(corrected_numeric)
            }

            # Add component outputs
            if (self$options$year_out && self$results$year_out$isNotFilled()) {
                self$results$year_out$setRowNums(rownames(data))
                self$results$year_out$setValues(as.numeric(components$year))
            }

            if (self$options$month_out && self$results$month_out$isNotFilled()) {
                self$results$month_out$setRowNums(rownames(data))
                self$results$month_out$setValues(as.numeric(components$month))
            }


                # # Add Calculated Time to Data
                # if (self$options$tint && self$options$calculatedtime && 
                #     self$results$calculatedtime$isNotFilled()) {
                #     self$results$calculatedtime$setRowNums(results$cleanData$row_names)
                #     self$results$calculatedtime$setValues(results$cleanData$CalculatedTime)
                # }





            if (self$options$monthname_out && self$results$monthname_out$isNotFilled()) {
                self$results$monthname_out$setRowNums(rownames(data))
                self$results$monthname_out$setValues(
                    private$.safeCharacterConversion(components$monthname)
                )
            }

            if (self$options$day_out && self$results$day_out$isNotFilled()) {
                self$results$day_out$setRowNums(rownames(data))
                self$results$day_out$setValues(as.numeric(components$day))
            }

            if (self$options$hour_out && self$results$hour_out$isNotFilled()) {
                self$results$hour_out$setRowNums(rownames(data))
                self$results$hour_out$setValues(as.numeric(components$hour))
            }

            if (self$options$minute_out && self$results$minute_out$isNotFilled()) {
                self$results$minute_out$setRowNums(rownames(data))
                self$results$minute_out$setValues(as.numeric(components$minute))
            }

            if (self$options$second_out && self$results$second_out$isNotFilled()) {
                self$results$second_out$setRowNums(rownames(data))
                self$results$second_out$setValues(as.numeric(components$second))
            }

            if (self$options$dayname_out && self$results$dayname_out$isNotFilled()) {
                self$results$dayname_out$setRowNums(rownames(data))
                self$results$dayname_out$setValues(
                    private$.safeCharacterConversion(components$dayname)
                )
            }

            if (self$options$weeknum_out && self$results$weeknum_out$isNotFilled()) {
                self$results$weeknum_out$setRowNums(rownames(data))
                self$results$weeknum_out$setValues(as.numeric(components$weeknum))
            }

            if (self$options$quarter_out && self$results$quarter_out$isNotFilled()) {
                self$results$quarter_out$setRowNums(rownames(data))
                self$results$quarter_out$setValues(as.numeric(components$quarter))
            }

            if (self$options$dayofyear_out && self$results$dayofyear_out$isNotFilled()) {
                self$results$dayofyear_out$setRowNums(rownames(data))
                self$results$dayofyear_out$setValues(as.numeric(components$dayofyear))
            }

            # Populate new clinician-friendly panels
            private$.populateQualityAssessment(quality, parsed_dates)
            private$.populateNaturalLanguageSummary(datetime_var, detected_format,
                                                     quality, components)
            private$.populateExplanatoryPanels()
            private$.populateGlossary()
        }
    )
)
