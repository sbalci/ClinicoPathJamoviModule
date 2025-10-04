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

        .parseDatetime = function(datetime_vector, format) {
            # Parse datetime using detected or selected format

            parser <- private$.getParser(format)

            tryCatch({
                parsed_dates <- parser(datetime_vector)
                return(parsed_dates)
            }, error = function(e) {
                stop(paste("Error parsing datetimes with format", format, ":", e$message))
            })
        },

        # ===================================================================
        # QUALITY ASSESSMENT
        # ===================================================================

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
                parsed_dates <- private$.parseDatetime(parsing_vector, detected_format)
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
                tz_note <- "<li>Conversions from numeric formats (Excel, Unix) assume UTC time zone.</li>"
            } else if (!prepared$already_parsed) {
                tz_note <- paste0("<li>String-to-datetime conversions assume the system's local time zone (", Sys.timezone(), ").</li>")
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
        }
    )
)
