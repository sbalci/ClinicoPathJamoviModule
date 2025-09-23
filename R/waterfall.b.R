#' @title Treatment Response Analysis
#' @description Creates waterfall and spider plots to visualize tumor response data following RECIST criteria.
#' Supports both raw tumor measurements and pre-calculated percentage changes.
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import dplyr
#' @importFrom magrittr %>%
#' @import ggplot2
#' @import scales
#' @param data Data frame containing response data
#' @param patientID Column name for patient identifiers
#' @param responseVar Column name for response values (raw measurements or percent change)
#' @param timeVar Optional column name for time points (required for raw measurements and spider plots)
#' @param inputType Data format: 'raw' for actual measurements or 'percentage' for pre-calculated percentage changes
#' @return Waterfall and spider plots with response analysis tables
#' @details 
#' The function handles two data formats:
#' 
#' **Percentage Data**: Pre-calculated percentage changes from baseline
#' - Negative values indicate tumor shrinkage (improvement)
#' - Example: -30 means 30% decrease from baseline
#' 
#' **Raw Measurements**: Actual tumor measurements over time  
#' - Requires baseline measurement at time = 0
#' - Function calculates percentage changes automatically
#' - Uses ((current - baseline) / baseline) * 100 formula
#' 
#' **RECIST Categories**:
#' - Complete Response (CR): ≤ -100% (complete disappearance)
#' - Partial Response (PR): ≤ -30% decrease
#' - Stable Disease (SD): -30% to +20% change  
#' - Progressive Disease (PD): > +20% increase
#' 
#' @examples
#' # Percentage data example
#' data_pct <- data.frame(
#'   PatientID = paste0("PT", 1:5),
#'   Response = c(-60, -35, -10, 15, 45)
#' )
#' 
#' # Raw measurements example  
#' data_raw <- data.frame(
#'   PatientID = rep(paste0("PT", 1:3), each = 3),
#'   Time = rep(c(0, 2, 4), 3),
#'   Measurement = c(50, 30, 25, 60, 45, 40, 55, 50, 48)
#' )


#' @export waterfallClass
#'


waterfallClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "waterfallClass",
    inherit = waterfallBase,
    private = list(

        # RECIST v1.1 Constants ----
        RECIST_CR_THRESHOLD = -100,  # Complete Response threshold (≤-100%)
        RECIST_PR_THRESHOLD = -30,   # Partial Response threshold (≤-30%)
        RECIST_PD_THRESHOLD = 20,    # Progressive Disease threshold (>20%)
        RECIST_SD_MIN = -30,         # Stable Disease minimum (-30%)
        RECIST_SD_MAX = 20,          # Stable Disease maximum (20%)

        # Get validation messages for different scenarios
        .getValidationMessages = function() {
          list(
            no_data = .("Error: No data provided or data is empty."),
            missing_columns = .("Error: Missing required columns:"),
            available_columns = .("Available columns:"),
            few_patients_warning = .("Warning: Only %d patient found. Waterfall plots are more meaningful with multiple patients."),
            missing_responses = .("Warning: %d missing response values found. These will be excluded from analysis."),
            time_var_required = .("Time Variable Required for Raw Measurements:"),
            time_var_purpose = list(
              .("When using raw tumor measurements, a time variable is essential to:"),
              .("Identify baseline measurements (time = 0)"),
              .("Calculate accurate percentage changes"),
              .("Track response progression over time")
            ),
            data_format = list(
              .("Recommended Data Format:"),
              "PatientID  Time  Measurement",
              paste0("PT1        0     50          (", .("baseline"), ")"),
              paste0("PT1        2     25          (2 ", .("months"), ")"),
              paste0("PT1        4     10          (4 ", .("months"), ")")
            ),
            missing_baseline = .("Missing Baseline Measurements:"),
            baseline_explanation = list(
              .("The following patients lack baseline (time = 0) measurements:"),
              .("Why this matters:"),
              .("Baseline measurements are the reference point for calculating changes"),
              .("Without baseline values, percentage changes cannot be calculated accurately")
            ),
            recommended_actions = list(
              .("Recommended actions:"),
              .("Add time=0 measurements for each patient, OR"),
              .("Switch to 'Percentage Changes' input format if changes are pre-calculated, OR"),
              .("Remove patients without baseline from analysis")
            ),
            invalid_shrinkage = .("Invalid Tumor Shrinkage Values Detected:"),
            shrinkage_explanation = list(
              .("Tumor shrinkage cannot exceed 100% (complete disappearance)."),
              .("Values will be automatically capped at -100% for analysis"),
              .("Note: Values <-100% are mathematically impossible for tumor shrinkage.")
            ),
            verify_data = list(
              .("Verify data entry for calculation errors"),
              .("Check if baseline measurements are correct"),
              .("Confirm percentage calculation method: ((current - baseline) / baseline) × 100")
            ),
            large_growth = .("Unusually Large Growth Values Detected:"),
            growth_verification = list(
              .("While such large increases are possible, please verify:"),
              .("Measurement accuracy"),
              .("Calculation methods"),
              .("Any additional clinical factors"),
              .("These values will be included in the analysis but may affect scaling.")
            )
          )
        },

        # HTML sanitization for security
        .safeHtmlOutput = function(text) {
          if (is.null(text) || length(text) == 0) return("")
          text <- as.character(text)
          # Sanitize potentially dangerous characters
          text <- gsub("&", "&amp;", text, fixed = TRUE)
          text <- gsub("<", "&lt;", text, fixed = TRUE)
          text <- gsub(">", "&gt;", text, fixed = TRUE)
          text <- gsub("\"", "&quot;", text, fixed = TRUE)
          text <- gsub("'", "&#x27;", text, fixed = TRUE)
          text <- gsub("/", "&#x2F;", text, fixed = TRUE)
          return(text)
        },

        # Safely format patient IDs and other user inputs for HTML display
        .safeFormatPatientID = function(id_vector) {
          sapply(id_vector, function(id) {
            if (is.null(id) || is.na(id)) return("N/A")
            private$.safeHtmlOutput(as.character(id))
          }, USE.NAMES = FALSE)
        },

        # Calculate statistical power for response rates
        .calculateStatisticalPower = function(n, observed_rate, null_rate = 0.15) {
          # Post-hoc power calculation for response rate
          # null_rate: typical null hypothesis for phase II trials (15%)
          tryCatch({
            if (n < 5 || is.na(observed_rate) || observed_rate < 0 || observed_rate > 1) {
              return(list(
                power = NA,
                interpretation = .("Insufficient data for power calculation"),
                adequate = FALSE
              ))
            }

            # Convert percentages to proportions if needed
            if (observed_rate > 1) observed_rate <- observed_rate / 100
            if (null_rate > 1) null_rate <- null_rate / 100

            # Calculate effect size (Cohen's h)
            h <- 2 * (asin(sqrt(observed_rate)) - asin(sqrt(null_rate)))

            # Simple power calculation using normal approximation
            # This avoids dependency on external packages
            z_alpha <- qnorm(0.975)  # two-sided alpha = 0.05
            se <- sqrt((observed_rate * (1 - observed_rate)) / n)
            z_beta <- (abs(observed_rate - null_rate) - z_alpha * se) / se
            power <- pnorm(z_beta)

            # Interpretation
            interpretation <- if (power >= 0.80) {
              .("Adequate statistical power (≥80%)")
            } else if (power >= 0.60) {
              .("Moderate statistical power (60-80%)")
            } else if (power >= 0.40) {
              .("Low statistical power (40-60%)")
            } else {
              .("Very low statistical power (<40%)")
            }

            list(
              power = power,
              interpretation = interpretation,
              adequate = power >= 0.80,
              effect_size = abs(h)
            )
          }, error = function(e) {
            list(
              power = NA,
              interpretation = .("Power calculation failed"),
              adequate = FALSE
            )
          })
        },

        # Calculate time-to-event metrics
        .calculateTimeToEventMetrics = function(df, patientID, timeVar, responseVar) {
          if (is.null(timeVar) || !timeVar %in% names(df)) {
            return(NULL)
          }

          tryCatch({
            # Convert to numeric
            df[[timeVar]] <- jmvcore::toNumeric(df[[timeVar]])
            df[[responseVar]] <- jmvcore::toNumeric(df[[responseVar]])

            metrics <- df %>%
              dplyr::group_by(!!rlang::sym(patientID)) %>%
              dplyr::arrange(!!rlang::sym(timeVar)) %>%
              dplyr::summarise(
                # Time to first response (PR or better: ≤-30%)
                time_to_first_response = ifelse(
                  any(.data[[responseVar]] <= private$RECIST_PR_THRESHOLD, na.rm = TRUE),
                  min(.data[[timeVar]][.data[[responseVar]] <= private$RECIST_PR_THRESHOLD], na.rm = TRUE),
                  NA_real_
                ),
                # Duration of response (time from first response to progression/end)
                duration_of_response = ifelse(
                  any(.data[[responseVar]] <= private$RECIST_PR_THRESHOLD, na.rm = TRUE),
                  {
                    first_response_time <- min(.data[[timeVar]][.data[[responseVar]] <= private$RECIST_PR_THRESHOLD], na.rm = TRUE)
                    # Find progression after response (>20% increase)
                    progression_times <- .data[[timeVar]][.data[[responseVar]] > private$RECIST_PD_THRESHOLD & .data[[timeVar]] > first_response_time]
                    if (length(progression_times) > 0) {
                      min(progression_times) - first_response_time
                    } else {
                      max(.data[[timeVar]]) - first_response_time  # Censored at last follow-up
                    }
                  },
                  NA_real_
                ),
                # Best response achieved
                best_response = min(.data[[responseVar]], na.rm = TRUE),
                # Time to best response
                time_to_best_response = .data[[timeVar]][which.min(.data[[responseVar]])],
                .groups = "drop"
              ) %>%
              dplyr::filter(!is.na(time_to_first_response) | !is.na(duration_of_response))

            # Summary statistics
            summary_stats <- list(
              median_time_to_response = median(metrics$time_to_first_response, na.rm = TRUE),
              median_duration_of_response = median(metrics$duration_of_response, na.rm = TRUE),
              median_time_to_best_response = median(metrics$time_to_best_response, na.rm = TRUE),
              n_responders = sum(!is.na(metrics$time_to_first_response)),
              n_with_duration_data = sum(!is.na(metrics$duration_of_response))
            )

            list(
              by_patient = metrics,
              summary = summary_stats
            )
          }, error = function(e) {
            NULL
          })
        },

        # Check if dataset is large and needs optimization
        .shouldOptimizeForLargeDataset = function(df) {
          nrow(df) > 100 || length(unique(df[[1]])) > 50  # Assuming first column might be patient ID
        },

        # Progressive rendering for large datasets
        .prepareProgressiveData = function(df, chunk_size = 50) {
          n_rows <- nrow(df)
          if (n_rows <= chunk_size) {
            return(list(data = df, is_chunked = FALSE, total_chunks = 1))
          }

          # Create chunks for progressive processing
          chunk_indices <- split(seq_len(n_rows), ceiling(seq_len(n_rows) / chunk_size))

          list(
            data = df,
            chunk_indices = chunk_indices,
            is_chunked = TRUE,
            total_chunks = length(chunk_indices),
            chunk_size = chunk_size
          )
        },

        # Plot helper functions for better code organization ----

        # Prepare waterfall plot data and sort
        .prepareWaterfallPlotData = function(plotData) {
          df <- plotData$data$waterfall

          # Optimize for large datasets
          df <- private$.optimizeForLargeDatasets(df)

          # Sort data
          if (plotData$options$sortBy == "response") {
            df <- df[order(df$response, na.last = TRUE),]
          } else if (plotData$options$sortBy == "id") {
            df <- df[order(df[[plotData$options$patientID]], na.last = TRUE),]
          }

          return(df)
        },

        # Define color schemes for plots
        .getColorSchemes = function() {
          list(
            recist = c(
              "CR" = "#1b9e77",  # teal - colorblind safe
              "PR" = "#7570b3",  # purple - colorblind safe
              "SD" = "#e7298a",  # magenta - colorblind safe
              "PD" = "#e66101",  # orange - colorblind safe
              "NA" = "#666666"   # gray
            ),
            simple = c(
              "CR" = "#1b9e77",  # teal for positive response
              "PR" = "#1b9e77",  # same teal for positive response
              "SD" = "#666666",  # gray for stable
              "PD" = "#e66101",  # orange for progression
              "NA" = "#999999"   # lighter gray
            ),
            colorblind = c(
              "CR" = "#009E73",  # bluish green
              "PR" = "#56B4E9",  # sky blue
              "SD" = "#E69F00",  # orange
              "PD" = "#CC79A7",  # reddish purple
              "NA" = "#999999"   # gray
            )
          )
        },

        # Select appropriate colors for waterfall plot
        .selectWaterfallColors = function(plotData, df) {
          color_schemes <- private$.getColorSchemes()

          # Check if group-based coloring is requested and group variable exists
          useGroupColoring <- !is.null(plotData$options$colorBy) &&
                              plotData$options$colorBy == "group" &&
                              "patient_group" %in% names(df)

          if (useGroupColoring) {
            # Generate distinct colors for groups using reusable method
            group_levels <- unique(df$patient_group)
            colors <- private$.generateGroupColors(group_levels, plotData$options$colorScheme)
            fill_var <- "patient_group"
            legend_name <- .("Patient Group")
          } else {
            # Use RECIST coloring based on selected scheme
            colors <- switch(plotData$options$colorScheme,
              "simple" = color_schemes$simple,
              "colorblind" = color_schemes$colorblind,
              "jamovi" = color_schemes$recist,
              "recist" = color_schemes$recist,
              color_schemes$recist  # default fallback
            )
            fill_var <- "recist_category"
            legend_name <- .("RECIST Response")
          }

          list(colors = colors, fill_var = fill_var, legend_name = legend_name, useGroupColoring = useGroupColoring)
        },

        # Create base waterfall plot
        .createWaterfallBasePlot = function(df, color_info, plotData) {
          ggplot2::ggplot(df, ggplot2::aes(
            x = factor(seq_len(nrow(df))),
            y = response
          )) +
          ggplot2::geom_bar(
            stat = "identity",
            ggplot2::aes(fill = !!rlang::sym(color_info$fill_var)),
            width = plotData$options$barWidth,
            alpha = plotData$options$barAlpha
          ) +
          ggplot2::scale_fill_manual(
            name = color_info$legend_name,
            values = color_info$colors,
            na.value = "#808080",
            drop = FALSE
          ) +
          ggplot2::labs(
            x = .("Patients"),
            y = .("Change in Tumor Size (%)")
          )
        },

        # Add RECIST threshold lines to plot
        .addRecistThresholds = function(plot, show_thresholds) {
          if (show_thresholds) {
            plot +
              ggplot2::geom_hline(
                yintercept = c(private$RECIST_PR_THRESHOLD, private$RECIST_PD_THRESHOLD),
                linetype = "dashed",
                color = c("#4169E1", "#FF0000"),
                alpha = 0.5
              )
          } else {
            plot
          }
        },

        # Add response labels to plot
        .addResponseLabels = function(plot, df, plotData) {
          if (plotData$options$labelOutliers) {
            threshold <- plotData$options$minResponseForLabel
            labels <- ifelse(
              !is.na(df$response) & abs(df$response) > threshold,
              sprintf("%.1f%%", df$response),
              ""
            )

            if (any(labels != "")) {
              plot +
                ggplot2::geom_text(
                  data = df[labels != "",],
                  mapping = ggplot2::aes(
                    x = factor(which(labels != "")),
                    y = response,
                    label = labels[labels != ""]
                  ),
                  vjust = ifelse(df$response[labels != ""] > 0, -0.5, 1.2),
                  hjust = 0.5,
                  size = 3,
                  color = "black"
                )
            } else {
              plot
            }
          } else {
            plot
          }
        },

        # Add median and confidence interval lines
        .addMedianAndCI = function(plot, df, plotData) {
          if (plotData$options$showMedian) {
            median_response <- median(df$response, na.rm = TRUE)
            plot <- plot +
              ggplot2::geom_hline(
                yintercept = median_response,
                linetype = "solid",
                color = "blue",
                alpha = 0.7
              )
          }

          if (plotData$options$showCI) {
            # Calculate 95% confidence interval for median
            if (nrow(df) > 10) {
              ci_data <- tryCatch({
                t_test <- t.test(df$response, na.rm = TRUE)
                c(lower = t_test$conf.int[1], upper = t_test$conf.int[2])
              }, error = function(e) c(lower = NA, upper = NA))

              if (!is.na(ci_data[1])) {
                plot <- plot +
                  ggplot2::geom_ribbon(
                    data = data.frame(
                      x = c(0, nrow(df) + 1),
                      ymin = ci_data[1],
                      ymax = ci_data[2]
                    ),
                    mapping = ggplot2::aes(x = x, ymin = ymin, ymax = ymax),
                    alpha = 0.2,
                    fill = "blue",
                    inherit.aes = FALSE
                  )
              }
            }
          }

          return(plot)
        },

        # Memory-efficient processing for large datasets
        .processLargeDataset = function(df, patientID, inputType, responseVar, timeVar = NULL, groupVar = NULL) {
          tryCatch({
            # Work with references to avoid copying data
            df_copy <- df  # Minimal copy

            # Convert to numeric efficiently
            df_copy[[responseVar]] <- jmvcore::toNumeric(df_copy[[responseVar]])
            if (!is.null(timeVar)) {
              df_copy[[timeVar]] <- jmvcore::toNumeric(df_copy[[timeVar]])
            }

            if (inputType == "raw") {
              # For raw data, process in chunks if very large
              if (nrow(df_copy) > 1000) {
                result <- private$.processRawDataInChunks(df_copy, patientID, responseVar, timeVar, groupVar)
              } else {
                result <- private$.processRawDataStandard(df_copy, patientID, responseVar, timeVar, groupVar)
              }
            } else {
              # For percentage data, direct processing
              result <- private$.processPercentageDataEfficient(df_copy, patientID, responseVar, timeVar, groupVar)
            }

            return(result)
          }, error = function(e) {
            # Fall back to standard processing
            warning("Large dataset optimization failed, falling back to standard processing: ", e$message)
            return(private$.processDataStandard(df, patientID, inputType, responseVar, timeVar, groupVar))
          })
        },

        # Process raw data in chunks for very large datasets
        .processRawDataInChunks = function(df, patientID, responseVar, timeVar, groupVar) {
          # Get unique patients and process in batches
          unique_patients <- unique(df[[patientID]])
          chunk_size <- 100  # Process 100 patients at a time

          waterfall_results <- list()
          spider_results <- list()

          for (i in seq(1, length(unique_patients), by = chunk_size)) {
            end_idx <- min(i + chunk_size - 1, length(unique_patients))
            chunk_patients <- unique_patients[i:end_idx]

            # Filter data for this chunk
            chunk_df <- df[df[[patientID]] %in% chunk_patients, , drop = FALSE]

            # Process this chunk
            chunk_result <- private$.processRawDataStandard(chunk_df, patientID, responseVar, timeVar, groupVar)

            # Accumulate results
            if (i == 1) {
              waterfall_results <- chunk_result$waterfall
              spider_results <- chunk_result$spider
            } else {
              waterfall_results <- rbind(waterfall_results, chunk_result$waterfall)
              if (!is.null(chunk_result$spider)) {
                spider_results <- rbind(spider_results, chunk_result$spider)
              }
            }
          }

          return(list(waterfall = waterfall_results, spider = spider_results))
        },

        # Standard raw data processing (extracted for reuse)
        .processRawDataStandard = function(df, patientID, responseVar, timeVar, groupVar) {
          # Calculate percentage change from baseline
          baseline_df <- df %>%
            dplyr::filter(!!rlang::sym(timeVar) == 0) %>%
            dplyr::select(!!rlang::sym(patientID), baseline = !!rlang::sym(responseVar))

          processed_df <- df %>%
            dplyr::left_join(baseline_df, by = patientID) %>%
            dplyr::group_by(!!rlang::sym(patientID)) %>%
            dplyr::arrange(!!rlang::sym(timeVar)) %>%
            dplyr::mutate(
              baseline = jmvcore::toNumeric(baseline),
              response = ifelse(!is.na(baseline) & baseline != 0,
                              ((!!rlang::sym(responseVar) - baseline) / baseline) * 100,
                              NA_real_)
            ) %>%
            dplyr::ungroup()

          # Create waterfall data (best response per patient)
          waterfall_data <- processed_df %>%
            dplyr::group_by(!!rlang::sym(patientID)) %>%
            dplyr::summarise(
              response = min(response, na.rm = TRUE),
              .groups = "drop"
            )

          # Add group information if available
          if (!is.null(groupVar) && groupVar %in% names(df)) {
            group_info <- df %>%
              dplyr::select(!!rlang::sym(patientID), !!rlang::sym(groupVar)) %>%
              dplyr::distinct()
            waterfall_data <- waterfall_data %>%
              dplyr::left_join(group_info, by = patientID)
            names(waterfall_data)[names(waterfall_data) == groupVar] <- "patient_group"
          }

          # Add RECIST categories
          waterfall_data$recist_category <- private$.categorizeRECIST(waterfall_data$response)

          return(list(waterfall = waterfall_data, spider = processed_df))
        },

        # Efficient processing for percentage data
        .processPercentageDataEfficient = function(df, patientID, responseVar, timeVar, groupVar) {
          # Direct processing without copying
          processed_df <- df
          processed_df$response <- processed_df[[responseVar]]

          # Create waterfall data efficiently
          if (!is.null(timeVar) && timeVar %in% names(df)) {
            # For time-series percentage data, get best response per patient
            waterfall_data <- processed_df %>%
              dplyr::group_by(!!rlang::sym(patientID)) %>%
              dplyr::summarise(
                response = min(response, na.rm = TRUE),
                .groups = "drop"
              )
            spider_data <- processed_df
          } else {
            # For single-timepoint percentage data
            waterfall_data <- processed_df
            waterfall_data$response <- waterfall_data[[responseVar]]
            spider_data <- NULL
          }

          # Add group information efficiently if available
          if (!is.null(groupVar) && groupVar %in% names(df)) {
            waterfall_data$patient_group <- df[[groupVar]][match(waterfall_data[[patientID]], df[[patientID]])]
          }

          # Add RECIST categories
          waterfall_data$recist_category <- private$.categorizeRECIST(waterfall_data$response)

          return(list(waterfall = waterfall_data, spider = spider_data))
        },

        # Fallback to standard processing
        .processDataStandard = function(df, patientID, inputType, responseVar, timeVar, groupVar) {
          # This is the original processing logic as fallback
          if (inputType == "raw") {
            if (!is.null(timeVar)) {
              df[[responseVar]] <- jmvcore::toNumeric(df[[responseVar]])
              df[[timeVar]] <- jmvcore::toNumeric(df[[timeVar]])

              baseline_df <- df %>%
                dplyr::filter(!!rlang::sym(timeVar) == 0) %>%
                dplyr::select(!!rlang::sym(patientID), baseline = !!rlang::sym(responseVar))

              processed_df <- df %>%
                dplyr::left_join(baseline_df, by = patientID) %>%
                dplyr::group_by(!!rlang::sym(patientID)) %>%
                dplyr::arrange(!!rlang::sym(timeVar)) %>%
                dplyr::mutate(
                  baseline = jmvcore::toNumeric(baseline),
                  response = ifelse(!is.na(baseline) & baseline != 0,
                                  ((!!rlang::sym(responseVar) - baseline) / baseline) * 100,
                                  NA_real_)
                ) %>%
                dplyr::ungroup()
            } else {
              df[[responseVar]] <- jmvcore::toNumeric(df[[responseVar]])
              processed_df <- df %>%
                dplyr::group_by(!!rlang::sym(patientID)) %>%
                dplyr::arrange(!!rlang::sym(patientID)) %>%
                dplyr::mutate(
                  baseline = dplyr::first(!!rlang::sym(responseVar)),
                  response = ((!!rlang::sym(responseVar) - baseline) / baseline) * 100
                ) %>%
                dplyr::ungroup()
            }
          } else {
            processed_df <- df
            processed_df$response <- jmvcore::toNumeric(processed_df[[responseVar]])
          }

          # Create waterfall and spider data
          if (!is.null(timeVar) && timeVar %in% names(processed_df)) {
            waterfall_data <- processed_df %>%
              dplyr::group_by(!!rlang::sym(patientID)) %>%
              dplyr::summarise(response = min(response, na.rm = TRUE), .groups = "drop")
            spider_data <- processed_df
          } else {
            waterfall_data <- processed_df
            spider_data <- NULL
          }

          # Add group information
          if (!is.null(groupVar) && groupVar %in% names(df)) {
            if (!"patient_group" %in% names(waterfall_data)) {
              group_info <- df %>%
                dplyr::select(!!rlang::sym(patientID), !!rlang::sym(groupVar)) %>%
                dplyr::distinct()
              waterfall_data <- waterfall_data %>%
                dplyr::left_join(group_info, by = patientID)
              names(waterfall_data)[names(waterfall_data) == groupVar] <- "patient_group"
            }
          }

          # Add RECIST categories
          waterfall_data$recist_category <- private$.categorizeRECIST(waterfall_data$response)

          return(list(waterfall = waterfall_data, spider = spider_data))
        },

        # Basic data existence check
      .validateBasicData = function(df) {
        msgs <- private$.getValidationMessages()
        if (is.null(df) || nrow(df) == 0) {
          return(list(
            valid = FALSE,
            message = paste0("<br>", msgs$no_data)
          ))
        }
        return(list(valid = TRUE, message = ""))
      },
      
      # Column existence validation
      .validateColumns = function(df, patientID, responseVar, timeVar = NULL) {
        msgs <- private$.getValidationMessages()
        required_columns <- c(patientID, responseVar)
        if (!is.null(timeVar)) {
          required_columns <- c(required_columns, timeVar)
        }

        missing_columns <- required_columns[!required_columns %in% names(df)]
        if (length(missing_columns) > 0) {
          return(list(
            valid = FALSE,
            message = paste0(
              "<br>", msgs$missing_columns, " ", paste(missing_columns, collapse = ", "),
              "<br>", msgs$available_columns, " ", paste(names(df), collapse = ", ")
            )
          ))
        }
        return(list(valid = TRUE, message = ""))
      },
      
      # Main validation coordinator
      .validateData = function(df, patientID, inputType, responseVar, timeVar = NULL) {
        validation_messages <- character()
        data_valid <- TRUE

        # Basic data validation
        basic_check <- private$.validateBasicData(df)
        if (!basic_check$valid) {
          attr(df, "validation_messages") <- basic_check$message
          attr(df, "data_valid") <- FALSE
          return(df)
        }

        # Column validation
        column_check <- private$.validateColumns(df, patientID, responseVar, timeVar)
        if (!column_check$valid) {
          validation_messages <- c(validation_messages, column_check$message)
          data_valid <- FALSE
        }


        # Check minimum number of patients
        if (patientID %in% names(df)) {
          n_patients <- length(unique(df[[patientID]]))
          if (n_patients < 2) {
            validation_messages <- c(validation_messages, paste0(
              "<br>", .("Warning: Only"), " ", n_patients, " ", .("patient found."), " ",
              .("Waterfall plots are more meaningful with multiple patients.")
            ))
          }
        }

        # Check for missing response values
        if (responseVar %in% names(df)) {
          missing_responses <- sum(is.na(df[[responseVar]]))
          if (missing_responses > 0) {
            validation_messages <- c(validation_messages, paste0(
              "<br>", .("Warning:"), " ", missing_responses, " ", .("missing response values found."), " ",
              .("These will be excluded from analysis.")
            ))
          }
        }

        # For raw measurements validation
        if (inputType == "raw") {
          if (is.null(timeVar)) {
            validation_messages <- c(validation_messages, paste0(
              "<br>", .("Time Variable Required for Raw Measurements:"),
              "<br>", .("When using raw tumor measurements, a time variable is essential to:"),
              "<br>- ", .("Identify baseline measurements (time = 0)"),
              "<br>- ", .("Calculate accurate percentage changes"),
              "<br>- ", .("Track response progression over time"),
              "<br><br>", .("Recommended Data Format:"),
              "<br>PatientID  Time  Measurement",
              "<br>PT1        0     50          (", .("baseline"), ")",
              "<br>PT1        2     25          (2 ", .("months"), ")",
              "<br>PT1        4     10          (4 ", .("months"), ")"
            ))
            data_valid <- FALSE
          } else {
            # Check time variable exists
            if (!timeVar %in% names(df)) {
              validation_messages <- c(validation_messages, sprintf(
                "<br>%s '%s' %s %s",
                .("Time variable"), timeVar, .("not found in the data."), 
                .("Please ensure the time variable is correctly specified.")
              ))
              data_valid <- FALSE
            } else {
              # Convert and validate time values
              df[[timeVar]] <- jmvcore::toNumeric(df[[timeVar]])
              # Check for baseline measurements
              baseline_check <- df %>%
                dplyr::group_by(.data[[patientID]]) %>%
                dplyr::summarise(
                  has_baseline = any(.data[[timeVar]] == 0),
                  .groups = "drop"
                )
              patients_without_baseline <- baseline_check %>%
                dplyr::filter(!has_baseline) %>%
                dplyr::pull(!!patientID)
              if (length(patients_without_baseline) > 0) {
                validation_messages <- c(validation_messages, paste0(
                  "<br>", .("Missing Baseline Measurements:"),
                  sprintf("<br>%s %s",
                          .("The following patients lack baseline (time = 0) measurements:"),
                          paste(patients_without_baseline, collapse = ", ")),
                  "<br><br>", .("Why this matters:"),
                  "<br>- ", .("Baseline measurements are the reference point for calculating changes"),
                  "<br>- ", .("Without baseline values, percentage changes cannot be calculated accurately"),
                  "<br><br>", .("Recommended actions:"),
                  "<br>1. ", .("Add time=0 measurements for each patient, OR"),
                  "<br>2. ", .("Switch to 'Percentage Changes' input format if changes are pre-calculated, OR"),
                  "<br>3. ", .("Remove patients without baseline from analysis"),
                  "<br><br>", .("Example data format with baseline:"),
                  "<br>PatientID  Time  Measurement",
                  "<br>PT1        0     50          (", .("baseline required"), ")",
                  "<br>PT1        2     25          (", .("follow-up"), ")"
                ))
                data_valid <- FALSE
              }
            }
          }
        }

        # For percentage data, handle invalid shrinkage and large growth
        if (inputType == "percentage") {
          df[[responseVar]] <- jmvcore::toNumeric(df[[responseVar]])

          # Check for invalid shrinkage (< RECIST CR threshold)
          invalid_shrinkage <- df %>%
            dplyr::filter(.data[[responseVar]] < private$RECIST_CR_THRESHOLD) %>%
            dplyr::select(!!patientID, !!responseVar)

          if (nrow(invalid_shrinkage) > 0) {
            validation_messages <- c(validation_messages, paste0(
              "<br>", .("Invalid Tumor Shrinkage Values Detected:"),
              "<br>", .("Tumor shrinkage cannot exceed 100% (complete disappearance)."),
              sprintf("<br>%s %d%%:", .("The following measurements will be capped at"), private$RECIST_CR_THRESHOLD),
              paste(capture.output(print(invalid_shrinkage)), collapse = "<br>"),
              "<br><br>", .("Recommended actions:"),
              "<br>1. ", .("Verify data entry for calculation errors"),
              "<br>2. ", .("Check if baseline measurements are correct"),
              "<br>3. ", .("Confirm percentage calculation method: ((current - baseline) / baseline) × 100"),
              "<br>4. ", .("Values will be automatically capped at -100% for analysis"),
              "<br><br>", .("Note: Values <-100% are mathematically impossible for tumor shrinkage.")
            ))
            # Cap shrinkage values at RECIST CR threshold
            df[[responseVar]] <- pmax(df[[responseVar]], private$RECIST_CR_THRESHOLD)
          }

          # Check for unusually large growth (> 200%)
          large_growth <- df %>%
            dplyr::filter(.data[[responseVar]] > 200) %>%
            dplyr::select(!!patientID, !!responseVar)

          if (nrow(large_growth) > 0) {
          validation_messages <- c(validation_messages, paste0(
              "<br>", .("Unusually Large Growth Values Detected:"),
              "<br>", .("The following measurements show >200% growth:"),
              paste(capture.output(print(large_growth)), collapse = "<br>"),
              "<br><br>", .("While such large increases are possible, please verify:"),
              "<br>- ", .("Measurement accuracy"),
              "<br>- ", .("Calculation methods"),
              "<br>- ", .("Any additional clinical factors"),
              "<br><br>", .("These values will be included in the analysis but may affect scaling.")
            ))
          }
        }

        # Set attributes for validation results
        attr(df, "validation_messages") <- validation_messages
        attr(df, "data_valid") <- data_valid




        # Sample size validation warnings
        unique_patients <- length(unique(df[[patientID]]))
        if (unique_patients < 10) {
          validation_messages <- c(validation_messages,
            sprintf("<br>%s", sprintf(.("Warning: Very small sample size (n=%d). Results may not be reliable."), unique_patients)))
        } else if (unique_patients < 20) {
          validation_messages <- c(validation_messages,
            sprintf("<br>%s", sprintf(.("Note: Small sample size (n=%d). Interpret results with caution."), unique_patients)))
        }

        # Add checks for unrealistic values if response data is available
        if (responseVar %in% names(df)) {
          response_values <- df[[responseVar]][!is.na(df[[responseVar]])]
          if (length(response_values) > 0) {
            if (inputType == "percentage") {
              # For percentage data, check for extreme values
              if (any(response_values > 500 | response_values < private$RECIST_CR_THRESHOLD, na.rm = TRUE)) {
                validation_messages <- c(validation_messages,
                  sprintf("<br>%s", sprintf(.("Warning: Some percentage changes are outside typical range (%d%% to +500%%). Please verify data."), private$RECIST_CR_THRESHOLD)))
              }
            } else {
              # For raw measurements, check for negative values or zero
              if (any(response_values <= 0, na.rm = TRUE)) {
                validation_messages <- c(validation_messages,
                  paste0("<br>", .("Warning: Some measurements are zero or negative. Please verify these values.")))
              }
            }
          }
        }

        # Add check for time variable if provided
        if (!is.null(timeVar) && timeVar %in% names(df)) {
          time_values <- df[[timeVar]][!is.na(df[[timeVar]])]
          if (length(time_values) > 0) {
            # Check if baseline (time = 0) measurements exist for raw data
            if (inputType == "raw" && !any(time_values == 0)) {
              validation_messages <- c(validation_messages,
                paste0("<br>", .("Warning: No baseline measurements (time=0) found. Percentage changes may be incorrect.")))
            }
            # Check for negative time values
            if (any(time_values < 0, na.rm = TRUE)) {
              validation_messages <- c(validation_messages,
                paste0("<br>", .("Warning: Negative time values detected. Please verify time measurements.")))
            }
          }
        }




        # Re-set attributes for validation results (include late warnings)
        attr(df, "validation_messages") <- validation_messages
        attr(df, "data_valid") <- data_valid

        # Return modified dataframe with validation attributes
        return(df)
      },

      .generateGroupColors = function(group_levels, color_scheme) {
        # Generate colors for group-based coloring
        # @param group_levels: unique levels/groups to assign colors
        # @param color_scheme: "colorful", "jamovi", "classic", "colorblind", etc.
        # @return: named vector of colors

        n_groups <- length(group_levels)

        if (color_scheme == "colorful") {
          # Use rainbow colors for better distinction
          colors <- rainbow(n_groups)
        } else if (color_scheme == "colorblind") {
          # Use Okabe-Ito colorblind-safe palette
          okabe_ito <- c("#009E73", "#56B4E9", "#E69F00", "#CC79A7", "#F0E442", "#0072B2", "#D55E00", "#999999")
          if (n_groups <= length(okabe_ito)) {
            colors <- okabe_ito[1:n_groups]
          } else {
            # Fall back to colorblind-safe qualitative palette for more groups
            colors <- grDevices::hcl.colors(n_groups, palette = "Cividis")
          }
        } else if (color_scheme == "jamovi") {
          # Use jamovi-style colors (RColorBrewer Set2)
          if (n_groups <= 8) {
            colors <- RColorBrewer::brewer.pal(max(3, n_groups), "Set2")
          } else {
            colors <- rainbow(n_groups)
          }
        } else {
          # Classic/default style (RColorBrewer Dark2 or Set2)
          palette_name <- if (color_scheme == "classic") "Dark2" else "Set2"
          if (n_groups <= 8) {
            colors <- RColorBrewer::brewer.pal(max(3, n_groups), palette_name)
          } else {
            colors <- rainbow(n_groups)
          }
        }

        # Name the colors with group levels
        names(colors) <- group_levels
        return(colors)
      }



      ,
      # process validated data ----
      .processData = function(df, patientID, inputType, responseVar, timeVar = NULL, groupVar = NULL) {
        # Validate input parameters first
        if (is.null(patientID) || is.null(responseVar)) {
          return(list(
            error = TRUE,
            message = .("Patient ID and response variables are required")
          ))
        }

        # Optimize processing for large datasets
        use_efficient_processing <- private$.shouldOptimizeForLargeDataset(df)
        if (use_efficient_processing) {
          return(private$.processLargeDataset(df, patientID, inputType, responseVar, timeVar, groupVar))
        }

        # For raw measurements, calculate percentage change from baseline
        if (inputType == "raw") {

          # For raw data, we need time variable to identify baseline
          if (!is.null(timeVar)) {
            # Ensure numeric conversion for calculations
            df[[responseVar]] <- jmvcore::toNumeric(df[[responseVar]])
            df[[timeVar]] <- jmvcore::toNumeric(df[[timeVar]])
            
            # First, identify baseline values for each patient
            baseline_df <- df %>%
              dplyr::filter(!!rlang::sym(timeVar) == 0) %>%
              dplyr::select(!!rlang::sym(patientID), baseline = !!rlang::sym(responseVar))
            
            # Join baseline values and calculate response
            processed_df <- df %>%
              dplyr::left_join(baseline_df, by = patientID) %>%
              dplyr::group_by(!!rlang::sym(patientID)) %>%
              dplyr::arrange(!!rlang::sym(timeVar)) %>%
              dplyr::mutate(
                # Ensure baseline is numeric
                baseline = jmvcore::toNumeric(baseline),
                # Calculate percentage change from baseline
                response = ifelse(!is.na(baseline) & baseline != 0,
                                ((!!rlang::sym(responseVar) - baseline) / baseline) * 100,
                                NA_real_)
              ) %>%
              dplyr::ungroup()
          } else {
            # Without time variable, assume first measurement is baseline
            df[[responseVar]] <- jmvcore::toNumeric(df[[responseVar]])
            
            processed_df <- df %>%
              dplyr::group_by(!!rlang::sym(patientID)) %>%
              dplyr::arrange(!!rlang::sym(patientID)) %>%
              dplyr::mutate(
                baseline = dplyr::first(!!rlang::sym(responseVar)),
                response = ((!!rlang::sym(responseVar) - baseline) / baseline) * 100
              ) %>%
              dplyr::ungroup()
          }
          
          # Validate processed data
          if (nrow(processed_df) == 0) {
            return(list(
              error = TRUE,
              message = .("No data remaining after processing. Check baseline measurements and data format.")
            ))
          }
        } else {
          # Data is already in percentage format
          df[[responseVar]] <- jmvcore::toNumeric(df[[responseVar]])
          processed_df <- df %>%
            dplyr::mutate(
              response = !!rlang::sym(responseVar)
            )
        }

        # Calculate RECIST categories - select best response (most negative for tumor shrinkage)
        # Optimized for large datasets
        n_patients <- length(unique(processed_df[[patientID]]))

        if (n_patients > 1000) {
          # Use more efficient approach for large datasets
          df_waterfall <- processed_df %>%
            dplyr::filter(!is.na(response)) %>%  # Filter NA responses first
            dplyr::slice_min(response, by = !!rlang::sym(patientID), with_ties = FALSE) %>%
            dplyr::ungroup()
        } else {
          # Use standard approach for smaller datasets
          df_waterfall <- processed_df %>%
            dplyr::group_by(!!rlang::sym(patientID)) %>%
            dplyr::filter(!is.na(response)) %>%  # Filter out NA responses first
            dplyr::filter(response == min(response, na.rm = TRUE)) %>%
            dplyr::slice(1) %>%  # Take first row if there are ties
            dplyr::ungroup()
        }
        
        # Validate waterfall data
        if (nrow(df_waterfall) == 0) {
          return(list(
            error = TRUE,
            message = .("No patients with valid response data found.")
          ))
        }
        
        df_waterfall <- df_waterfall %>%
          dplyr::mutate(
            category = factor(
              cut(response,
                  breaks = c(-Inf, private$RECIST_PR_THRESHOLD, private$RECIST_PD_THRESHOLD, Inf),
                  labels = c(.("Response"), .("Stable"), .("Progression")),
                  right = FALSE),
              levels = c(.("Response"), .("Stable"), .("Progression"))
            ),
            # Also create detailed RECIST categories
            recist_category = factor(
              dplyr::case_when(
                response <= private$RECIST_CR_THRESHOLD ~ "CR",  # Complete Response
                response <= private$RECIST_PR_THRESHOLD ~ "PR",   # Partial Response
                response <= private$RECIST_PD_THRESHOLD ~ "SD",    # Stable Disease
                response > private$RECIST_PD_THRESHOLD ~ "PD",     # Progressive Disease
                TRUE ~ "Unknown"
              ),
              levels = c("CR", "PR", "SD", "PD", .("Unknown"))
            )
          )
        
        # Add group variable if specified
        if (!is.null(groupVar) && groupVar %in% names(processed_df)) {
          # Get group information for each patient (use first occurrence if multiple)
          group_info <- processed_df %>%
            dplyr::group_by(!!rlang::sym(patientID)) %>%
            dplyr::slice(1) %>%
            dplyr::ungroup() %>%
            dplyr::select(!!rlang::sym(patientID), patient_group = !!rlang::sym(groupVar))
          
          # Join group information to waterfall data
          df_waterfall <- df_waterfall %>%
            dplyr::left_join(group_info, by = patientID) %>%
            dplyr::mutate(
              patient_group = factor(patient_group)
            )
        }

        # Prepare spider plot data
        df_spider <- processed_df
        
        # Add group information to spider data if specified
        if (!is.null(groupVar) && groupVar %in% names(df_spider)) {
          df_spider <- df_spider %>%
            dplyr::mutate(
              patient_group = factor(!!rlang::sym(groupVar))
            )
        }

        # Add informative attributes about the processing
        attr(df_waterfall, "input_type") <- inputType
        attr(df_spider, "input_type") <- inputType

        if (!is.null(timeVar)) {
          attr(df_spider, "time_variable") <- timeVar
        }

        return(list(
          waterfall = df_waterfall,
          spider = df_spider
        ))
      }

      ,
      # calculate clinical metrics ----
      .calculateMetrics = function(df) {
        ## Calculate response rates using RECIST categories ----
        cats <- c("CR", "PR", "SD", "PD")

        # Use recist_category for detailed analysis
        summary_table <- data.frame(
          category = cats,
          n = sapply(cats, function(x) sum(df$recist_category == x, na.rm = TRUE)),
          stringsAsFactors = FALSE
        )

        total_n <- sum(summary_table$n)
        if (total_n > 0) {
          summary_table$percent <- summary_table$n / total_n
        } else {
          summary_table$percent <- rep(0, length(cats))
        }

        ## Calculate ORR and DCR ----
        if (total_n > 0) {
          ORR <- round(sum(summary_table$n[summary_table$category %in% c("CR", "PR")]) /
                         total_n * 100, 1)

          DCR <- round(sum(summary_table$n[summary_table$category %in% c("CR", "PR", "SD")]) /
                         total_n * 100, 1)
        } else {
          ORR <- NA_real_
          DCR <- NA_real_
        }

        return(list(
          summary = summary_table,
          ORR = ORR,
          DCR = DCR
        ))
      }


      ,
      # Add duration of response calculation
      .calculateDurationOfResponse = function(df) {
        df %>%
          dplyr::group_by(patientID) %>%
          dplyr::summarise(
            duration = max(time[response <= private$RECIST_PR_THRESHOLD], na.rm=TRUE) -
              min(time[response <= private$RECIST_PR_THRESHOLD], na.rm=TRUE),
            best_response = min(response, na.rm=TRUE)
          )
      }

      ,
      # Add time to response metric
      .calculateTimeToResponse = function(df) {
        df %>%
          dplyr::group_by(patientID) %>%
          dplyr::filter(response <= private$RECIST_PR_THRESHOLD) %>%
          dplyr::summarise(
            time_to_response = min(time, na.rm=TRUE)
          )
      }

      ,
      # Add subgroup analysis capability
      .createSubgroupAnalysis = function(df, subgroupVar) {
        df %>%
          dplyr::group_by(!!rlang::sym(subgroupVar)) %>%
          dplyr::summarise(
            ORR = mean(response <= private$RECIST_PR_THRESHOLD, na.rm=TRUE),
            DCR = mean(response <= private$RECIST_PD_THRESHOLD, na.rm=TRUE),
            median_response = median(response, na.rm=TRUE)
          )
      }

      ,
      # Calculate person-time metrics for enhanced analysis
      .calculatePersonTimeMetrics = function(df, patientID, timeVar, responseVar) {
        # Requires time variable to calculate person-time
        if (is.null(timeVar) || !timeVar %in% names(df))
          return(NULL)

        if (!patientID %in% names(df))
          return(NULL)

        if (nrow(df) == 0)
          return(NULL)

        df <- df %>%
          dplyr::filter(!is.na(.data[[patientID]]))

        if (nrow(df) == 0)
          return(NULL)

        # Convert time variable to numeric if needed
        df[[timeVar]] <- jmvcore::toNumeric(df[[timeVar]])

        response_col <- NULL
        if ("percentage_change" %in% names(df)) {
          response_col <- "percentage_change"
        } else if ("response" %in% names(df)) {
          response_col <- "response"
        } else {
          return(NULL)
        }

        df[[response_col]] <- jmvcore::toNumeric(df[[response_col]])

        safe_extreme <- function(x, fun) {
          x <- x[!is.na(x)]
          if (length(x) == 0)
            return(NA_real_)
          fun(x)
        }

        classify_response <- function(value) {
          if (is.na(value)) {
            return(NA_character_)
          }
          if (value <= private$RECIST_CR_THRESHOLD) {
            return("CR")
          }
          if (value <= private$RECIST_PR_THRESHOLD) {
            return("PR")
          }
          if (value <= private$RECIST_PD_THRESHOLD) {
            return("SD")
          }
          "PD"
        }

        pt_by_patient <- df %>%
          dplyr::group_by(!!rlang::sym(patientID)) %>%
          dplyr::summarise(
            follow_up_time = safe_extreme(.data[[timeVar]], max),
            best_response = safe_extreme(.data[[response_col]], min),
            time_to_best = {
              valid_idx <- which(!is.na(.data[[response_col]]))
              if (length(valid_idx) == 0) {
                NA_real_
              } else {
                best_idx <- valid_idx[which.min(.data[[response_col]][valid_idx])]
                .data[[timeVar]][best_idx]
              }
            },
            time_in_response = {
              responders <- which(!is.na(.data[[response_col]]) & .data[[response_col]] <= private$RECIST_PR_THRESHOLD)
              if (length(responders) == 0) {
                0
              } else {
                start_time <- min(.data[[timeVar]][responders], na.rm = TRUE)
                end_time <- max(.data[[timeVar]][responders], na.rm = TRUE)
                if (is.finite(start_time) && is.finite(end_time)) max(end_time - start_time, 0) else 0
              }
            },
            .groups = "drop"
          )

        if (!"best_response" %in% names(pt_by_patient))
          return(NULL)

        pt_by_patient <- pt_by_patient %>%
          dplyr::mutate(
            response_cat = vapply(best_response, classify_response, character(1), USE.NAMES = FALSE),
            response_cat = factor(response_cat, levels = c("CR", "PR", "SD", "PD"))
          )

        total_patients <- nrow(pt_by_patient)
        if (total_patients == 0)
          return(NULL)

        total_person_time <- sum(pt_by_patient$follow_up_time, na.rm = TRUE)
        total_response_time <- sum(pt_by_patient$time_in_response, na.rm = TRUE)

        pt_by_category <- pt_by_patient %>%
          dplyr::group_by(response_cat, .drop = FALSE) %>%
          dplyr::summarise(
            patients = dplyr::n(),
            person_time = sum(follow_up_time, na.rm = TRUE),
            median_time_to_response = safe_extreme(time_to_best, stats::median),
            median_duration = safe_extreme(time_in_response, stats::median),
            .groups = "drop"
          ) %>%
          dplyr::mutate(
            pct_patients = if (total_patients > 0) (patients / total_patients) * 100 else 0,
            pct_time = if (!is.na(total_person_time) && total_person_time > 0) (person_time / total_person_time) * 100 else 0
          )

        response_rate <- if (!is.na(total_person_time) && total_person_time > 0) {
          (total_response_time / total_person_time) * 100
        } else {
          NA_real_
        }

        summary_metrics <- list(
          total_patients = total_patients,
          total_person_time = total_person_time,
          total_response_time = total_response_time,
          response_rate_per_100 = response_rate
        )

        return(list(
          by_patient = pt_by_patient,
          by_category = pt_by_category,
          summary = summary_metrics
        ))
      }

      ,
      # Initialize analysis and show guidance ----
      .initializeAnalysis = function() {
        ## Show guided analysis first if enabled ----
        if (self$options$enableGuidedMode) {
          private$.generateGuidedAnalysis()
          return()
        }

        ## Show welcome text ----
        private$.generateWelcomeText()
      }

      ,
      # Generate welcome text for non-guided mode ----
      .generateWelcomeText = function() {
        todo <- paste0(
          "<br>", .("Welcome to ClinicoPath Treatment Response Analysis"),
          "<br><br>",
          .("This tool creates waterfall and spider plots for tumor response analysis following RECIST criteria."),
          "<br><br>",
          "<b>📊 ", .("Visualization Types:"), "</b>",
          "<br><br>",
          "<b>1. ", .("Waterfall Plot"), "</b>",
          "<br>- ", .("Shows best response for each patient as vertical bars"),
          "<br>- ", .("Requires one measurement per patient (for single timepoint data)"),
          "<br>- ", .("Colors bars by RECIST categories (CR/PR/SD/PD) or patient groups"),
          "<br><br>",
          "<b>2. ", .("Spider Plot"), "</b>",
          "<br>- ", .("Shows response trajectories over time as connected lines"),
          "<br>- ", .("Requires multiple measurements per patient with time variable"),
          "<br>- ", .("Best for longitudinal follow-up data"),
          "<br><br>",
          "<b>📝 ", .("Data Input Options:"), "</b>",
          "<br><br>",
          "<b>", .("Percentage Changes:"), "</b>",
          "<br>- ", .("Pre-calculated percent changes from baseline"),
          "<br>- ", .("Negative values = tumor shrinkage (improvement)"),
          "<br>- ", .("Example: -30 means 30% decrease from baseline"),
          "<br><br>",
          "<b>", .("Raw Measurements:"), "</b>",
          "<br>- ", .("Actual tumor measurements (mm, cm, sum of diameters)"),
          "<br>- ", .("Tool automatically calculates percent changes"),
          "<br>- ", .("Baseline assumed at Time = 0"),
          "<br><br>",
          "<b>🎯 ", .("RECIST v1.1 Categories:"), "</b>",
          sprintf("<br>- <b>%s</b> ≤ %d%% (%s)", .("Complete Response (CR):"), private$RECIST_CR_THRESHOLD, .("complete disappearance")),
          sprintf("<br>- <b>%s</b> ≤ %d%% %s", .("Partial Response (PR):"), private$RECIST_PR_THRESHOLD, .("decrease")),
          sprintf("<br>- <b>%s</b> %d%% %s +%d%% %s", .("Stable Disease (SD):"), private$RECIST_PR_THRESHOLD, .("to"), private$RECIST_PD_THRESHOLD, .("change")),
          sprintf("<br>- <b>%s</b> > +%d%% %s", .("Progressive Disease (PD):"), private$RECIST_PD_THRESHOLD, .("increase")),
          "<br><br>",
          "<b>", .("Required Variables:"), "</b>",
          "<br>- <b>", .("Patient ID:"), "</b> ", .("Unique identifier for each patient"),
          "<br>- <b>", .("Response Value:"), "</b> ", .("Either percentage change or raw measurements"),
          "<br>- <b>", .("Time Variable:"), "</b> ", .("Required only for Spider Plot (e.g., months from baseline)"),
          "<br><br>",
          "<b>", .("Data Format Examples:"), "</b>
        <pre>
        1. Using Percentage Changes:        2. Using Raw Measurements:
        PatientID Time Response            PatientID Time Measurement
        PT1      0     0                  PT1      0    50
        PT1      2    -45                 PT1      2    27.5
        PT1      4    -80                 PT1      4    10
        PT2      0     0                  PT2      0    40
        PT2      2    -20                 PT2      2    32
        </pre>
        <hr>
        "
        )

        self$results$todo$setContent(todo)
        return(todo)
      }

      ,
      # Validate inputs and data availability ----
      .validateInputsAndData = function() {
        # Get welcome text for potential error messages
        todo <- if (!self$options$enableGuidedMode) {
          private$.generateWelcomeText()
        } else {
          ""
        }

        ## Validate required inputs ----
        if (is.null(self$options$patientID) || is.null(self$options$responseVar)) {
          if (!self$options$enableGuidedMode) {
            todo <- paste0(todo,
                           paste0("<br><br>",
                           .("To start analysis select <b>Patient ID</b> and <b>Response Value</b>"))
            )
            self$results$todo$setContent(todo)
          }
          return(FALSE)
        }

        ## Validate data availability ----
        if (nrow(self$data) == 0) {
          if (!self$options$enableGuidedMode) {
            todo <- paste0(todo,
                           paste0("<br><br>",
                           .("Data contains no complete rows. Check the data.")))
            self$results$todo$setContent(todo)
          }
          return(FALSE)
        }

        # Clear welcome messages if validation passed
        if (!self$options$enableGuidedMode) {
          self$results$todo2$setVisible(FALSE)
          self$results$todo2$setContent("")
        }

        return(TRUE)
      }

      ,
      # Process and analyze data ----
      .processAndAnalyzeData = function() {
        ## Validate data ----
        private$.checkpoint()  # Checkpoint before data validation
        validated_data <- private$.validateData(
          self$data,
          self$options$patientID,
          self$options$inputType,
          self$options$responseVar,
          self$options$timeVar
        )

        ### Check for validation messages ----
        validation_messages <- attr(validated_data, "validation_messages")
        if (length(validation_messages) > 0 && !self$options$enableGuidedMode) {
          self$results$todo2$setVisible(TRUE)
          self$results$todo2$setContent(paste(validation_messages, collapse = ""))
          self$results$todo$setVisible(FALSE)
        }

        ## Continue with analysis if data is valid; abort only on critical errors ----
        if (!isTRUE(attr(validated_data, "data_valid"))) {
          plain_message <- "Data validation failed."
          if (length(validation_messages) > 0) {
            plain_message <- gsub("<[^>]+>", "", paste(validation_messages, collapse = " "))
            plain_message <- trimws(plain_message)
            if (plain_message == "") {
              plain_message <- "Data validation failed."
            }
          }

          stop(plain_message, call. = FALSE)
          return(NULL)
        }

        # Clear todo messages for successful validation
        if (!self$options$enableGuidedMode) {
          self$results$todo$setVisible(FALSE)
          self$results$todo2$setVisible(FALSE)
          self$results$todo2$setContent("")
        }

        # Process data
        private$.checkpoint()  # Checkpoint before data processing
        processed_data <- private$.processData(
          validated_data,
          self$options$patientID,
          self$options$inputType,
          self$options$responseVar,
          self$options$timeVar,
          self$options$groupVar
        )

        # Check for processing errors
        if (!is.null(processed_data$error) && processed_data$error) {
          error_message <- paste0(
            "<br><br>", .("Data Processing Error:"),
            "<br>", processed_data$message,
            "<br><br>", .("Please check your data and try again.")
          )

          if (!self$options$enableGuidedMode) {
            self$results$todo2$setVisible(TRUE)
            self$results$todo2$setContent(error_message)
            self$results$todo$setVisible(FALSE)
          }
          return(NULL)
        }

        return(processed_data)
      }

      ,
      # Generate tables and results ----
      .generateTablesAndResults = function(processed_data) {
        ## Calculate metrics ----
        private$.checkpoint()  # Checkpoint before metrics calculation
        metrics <- private$.calculateMetrics(processed_data$waterfall)

        ## Update results tables ----
        private$.checkpoint()  # Checkpoint before summary table population
        for(i in seq_len(nrow(metrics$summary))) {
          row_key <- sprintf("recist_%s", metrics$summary$category[i])
          percent_value <- metrics$summary$percent[i]
          percent_display <- if (!is.na(percent_value)) percent_value else NA_real_

          self$results$summaryTable$addRow(rowKey = row_key, values = list(
            category = metrics$summary$category[i],
            n = metrics$summary$n[i],
            percent = percent_display
          ))
        }



            self$results$summaryTable$addFootnote(
              rowNo = 1,
              col = "category",
              .("Complete Response (CR): Complete disappearance of all target lesions.")
            )

            self$results$summaryTable$addFootnote(
              rowNo = 2,
              col = "category",
              .("Partial Response (PR): At least 30% decrease in sum of target lesions.")
            )

            self$results$summaryTable$addFootnote(
              rowNo = 3,
              col = "category",
              .("Stable Disease (SD): Neither PR nor PD criteria met.")
            )

            self$results$summaryTable$addFootnote(
              rowNo = 4,
              col = "category",
              .("Progressive Disease (PD): At least 20% increase in sum of target lesions.")
            )


        # Add interpretations to clinical metrics
        orr_interpretation <- private$.interpretORR(metrics$ORR)
        dcr_interpretation <- private$.interpretDCR(metrics$DCR)

        metric_row_index <- 1
        add_metric_row <- function(values) {
          row_key <- sprintf("metric_%02d", metric_row_index)
          self$results$clinicalMetrics$addRow(rowKey = row_key, values = values)
          metric_row_index <<- metric_row_index + 1
        }

        if (!is.na(metrics$ORR)) {
          add_metric_row(list(
            metric = .("Objective Response Rate (CR+PR)"),
            value = sprintf("%.1f%% (%s)", metrics$ORR, orr_interpretation)
          ))
        }

        if (!is.na(metrics$DCR)) {
          add_metric_row(list(
            metric = .("Disease Control Rate (CR+PR+SD)"),
            value = sprintf("%.1f%% (%s)", metrics$DCR, dcr_interpretation)
          ))
        }

        # Add statistical power calculation
        total_patients <- sum(metrics$summary$n)
        orr_proportion <- if (!is.na(metrics$ORR)) metrics$ORR / 100 else NA_real_
        power_analysis <- private$.calculateStatisticalPower(total_patients, orr_proportion)

        if (!is.na(power_analysis$power)) {
          add_metric_row(list(
            metric = .("Statistical Power (ORR vs 15% null)"),
            value = sprintf("%.1f%% (%s)", power_analysis$power * 100, power_analysis$interpretation)
          ))
        }

        # Control visibility of personTimeTable based on conditions
        personTimeVisible <- !is.null(self$options$timeVar) && self$options$inputType == "raw"
        if (!is.null(self$results$personTimeTable)) {
          self$results$personTimeTable$setVisible(personTimeVisible)
        }
        
        # Calculate and add person-time metrics if time variable is available and input is raw
        person_time_metrics <- NULL
        if (personTimeVisible) {
          private$.checkpoint()  # Checkpoint before person-time calculations
          person_time_metrics <- tryCatch({
            private$.calculatePersonTimeMetrics(
              processed_data$spider,
              self$options$patientID,
              self$options$timeVar,
              self$options$responseVar
            )
          }, error = function(e) {
            warning("Person-time analysis failed: ", e$message)
            return(NULL)
          })
        }

        # Add time-to-event metrics if available
        tte_metrics <- NULL
        if (!is.null(self$options$timeVar) && self$options$timeVar %in% names(processed_data$spider)) {
          tte_metrics <- private$.calculateTimeToEventMetrics(
            processed_data$spider,
            self$options$patientID,
            self$options$timeVar,
            "response"
          )

          if (!is.null(tte_metrics)) {
            # Add median time to response
            if (!is.na(tte_metrics$summary$median_time_to_response)) {
              add_metric_row(list(
                metric = .("Median Time to First Response"),
                value = sprintf("%.1f time units (n=%d responders)",
                               tte_metrics$summary$median_time_to_response,
                               tte_metrics$summary$n_responders)
              ))
            }

            # Add median duration of response
            if (!is.na(tte_metrics$summary$median_duration_of_response)) {
              add_metric_row(list(
                metric = .("Median Duration of Response"),
                value = sprintf("%.1f time units (n=%d with duration data)",
                               tte_metrics$summary$median_duration_of_response,
                               tte_metrics$summary$n_with_duration_data)
              ))
            }
          }
        }

        # Add person-time metrics to the results if available
        if (!is.null(person_time_metrics) && personTimeVisible) {
          # Add response duration metrics to clinical metrics table
          # Get median time to response for interpretation
          median_ttr <- person_time_metrics$by_category$median_time_to_response[
            person_time_metrics$by_category$response_cat %in% c("CR", "PR")
          ][1]

          # Get median duration of response
          median_dor <- median(person_time_metrics$by_patient$time_in_response[
            person_time_metrics$by_patient$time_in_response > 0
          ], na.rm = TRUE)

          # Add interpretation for time to response
          ttr_interpretation <- if (!is.na(median_ttr)) {
            if (median_ttr <= 2) "rapid response" else if (median_ttr <= 6) "standard timeline" else "delayed response"
          } else "insufficient data"

          # Add interpretation for duration of response
          dor_interpretation <- if (!is.na(median_dor)) {
            if (median_dor >= 12) "durable response" else if (median_dor >= 6) "sustained response" else "short-term response"
          } else "insufficient data"

          add_metric_row(list(
            metric = "Median Time to Response",
            value = if (!is.na(median_ttr)) {
              sprintf("%.1f time units (%s)", median_ttr, ttr_interpretation)
            } else {
              sprintf("%s", ttr_interpretation)
            }
          ))

          add_metric_row(list(
            metric = "Median Duration of Response",
            value = if (!is.na(median_dor)) {
              sprintf("%.1f time units (%s)", median_dor, dor_interpretation)
            } else {
              sprintf("%s", dor_interpretation)
            }
          ))

          response_rate_value <- person_time_metrics$summary$response_rate_per_100
          response_rate_text <- if (!is.na(response_rate_value)) {
            sprintf("%.2f", response_rate_value)
          } else {
            .("Not estimable")
          }

          add_metric_row(list(
            metric = .("Response Time per 100 Person-Time Units"),
            value = response_rate_text
          ))

          # Add person-time table if it exists
          if (!is.null(self$results$personTimeTable)) {
            private$.checkpoint()  # Checkpoint before person-time table population
            for (i in seq_len(nrow(person_time_metrics$by_category))) {
              self$results$personTimeTable$addRow(rowKey = i, values = list(
                category = as.character(person_time_metrics$by_category$response_cat[i]),
                patients = person_time_metrics$by_category$patients[i],
                patient_pct = sprintf("%.1f%%", person_time_metrics$by_category$pct_patients[i]),
                person_time = sprintf("%.1f", person_time_metrics$by_category$person_time[i]),
                time_pct = sprintf("%.1f%%", person_time_metrics$by_category$pct_time[i]),
                median_time = sprintf("%.1f", person_time_metrics$by_category$median_time_to_response[i]),
                median_duration = sprintf("%.1f", person_time_metrics$by_category$median_duration[i])
              ))
            }

            # Add total row
            self$results$personTimeTable$addRow(rowKey = nrow(person_time_metrics$by_category) + 1, values = list(
              category = "Total",
              patients = person_time_metrics$summary$total_patients,
              patient_pct = "100.0%",
              person_time = sprintf("%.1f", person_time_metrics$summary$total_person_time),
              time_pct = "100.0%",
              median_time = "",
              median_duration = ""
            ))
          }
        }

        # Generate clinical summary ----
        private$.generateClinicalSummary(processed_data, metrics, person_time_metrics)

        # Generate group comparison analysis ----
        private$.generateGroupComparison(processed_data)

        # Generate about analysis panel ----
        private$.generateAboutAnalysis()

        return(list(metrics = metrics, person_time_metrics = person_time_metrics))
      }

      ,
      # Generate visualizations ----
      .generateVisualizations = function(processed_data, metrics) {
        # Prepare comprehensive plot data structure
        plotData <- list(
          "data" = processed_data,
          options = list(
            "patientID" = self$options$patientID,
            "response" = self$options$responseVar,
            "timeVar" = self$options$timeVar,
            "sortBy" = self$options$sortBy,
            "showThresholds" = self$options$showThresholds,
            "labelOutliers" = self$options$labelOutliers,
            "colorScheme" = self$options$colorScheme,
            "colorBy" = self$options$colorBy,
            "groupVar" = self$options$groupVar,
            "barWidth" = self$options$barWidth,
            "barAlpha" = self$options$barAlpha,
            "showMedian" = self$options$showMedian,
            "showCI" = self$options$showCI,
            "minResponseForLabel" = self$options$minResponseForLabel,
            "spiderColorBy" = self$options$spiderColorBy,
            "spiderColorScheme" = self$options$spiderColorScheme,
            "timeUnitLabel" = self$options$timeUnitLabel
          ),
          "metrics" = metrics
        )

        # Add checkpoint for performance monitoring
        private$.checkpoint()

        # Initialize waterfall plot if visible
        if (self$options$showWaterfallPlot) {
          self$results$waterfallplot$setState(plotData)
        }

        # Initialize spider plot with validation
        if (self$options$showSpiderPlot && !is.null(self$options$timeVar)) {
          # Validate spider plot requirements
          if (self$options$inputType == "percentage") {
            message("Note: Spider plot with percentage data shows static values over time. Consider using raw measurements for meaningful trajectories.")
          }

          plotData$timeVar <- self$options$timeVar
          self$results$spiderplot$setState(plotData)
        }
      }

      ,
      # Generate reports and supplementary content ----
      .generateReportsAndContent = function(processed_data, metrics, person_time_metrics) {
        # Apply clinical presets
        private$.applyClinicalPreset()

        # Generate enhanced clinical metrics with confidence intervals ----
        if (self$options$showConfidenceIntervals) {
          private$.generateEnhancedClinicalMetrics(processed_data, metrics)
        }

        # Generate copy-ready report ----
        if (self$options$generateCopyReadyReport) {
          private$.generateCopyReadyReport(processed_data, metrics, person_time_metrics)
        }

        # Show clinical significance assessment ----
        if (self$options$showClinicalSignificance) {
          private$.generateClinicalSignificance(metrics, nrow(processed_data$waterfall))
          private$.generateClinicalGlossary()
        }

        ## Add response category to data ----
        if (!is.null(self$options$addResponseCategory) && self$options$addResponseCategory) {
          if (is.null(self$options$timeVar) && self$results$addResponseCategory$isNotFilled()) {
            df <- processed_data$waterfall
            self$results$addResponseCategory$setRowNums(rownames(df))
            self$results$addResponseCategory$setValues(df$recist_category)
          }

          if (!is.null(self$options$timeVar) && self$results$addResponseCategory$isNotFilled()) {
            # Get waterfall data and extract unique patient categories
            df <- processed_data$waterfall %>%
              dplyr::select(!!rlang::sym(self$options$patientID), recist_category) %>%
              dplyr::distinct()

            # Join with original data
            df2 <- self$data %>%
              dplyr::left_join(df, by = self$options$patientID)

            # Update response category output
            self$results$addResponseCategory$setRowNums(rownames(df2))
            self$results$addResponseCategory$setValues(df2$recist_category)
          }
        }
      }

      ,
      # Refactored run method ----
      .run = function() {
        # Step 1: Initialize analysis and show guidance
        private$.initializeAnalysis()

        # Step 2: Validate inputs and data
        if (!private$.validateInputsAndData()) {
          return()
        }

        # Step 3: Process and analyze data
        processed_data <- private$.processAndAnalyzeData()
        if (is.null(processed_data)) {
          return()
        }

        # Step 4: Generate tables and calculate results
        results <- private$.generateTablesAndResults(processed_data)
        if (is.null(results)) {
          return()
        }

        # Step 5: Generate visualizations
        private$.generateVisualizations(processed_data, results$metrics)

        # Step 6: Generate reports and supplementary content
        private$.generateReportsAndContent(processed_data, results$metrics, results$person_time_metrics)
      }


      ,
      # Waterfall plot ----
      .waterfallplot = function(imageWaterfall, ggtheme, theme, ...) {
        if (!self$options$showWaterfallPlot) return()

        private$.checkpoint()  # Checkpoint before plot generation
        
        plotData <- imageWaterfall$state
        options <- plotData$options

        if (is.null(plotData) || is.null(plotData$data) || is.null(plotData$data$waterfall)) {
          warning("Plot data not properly initialized")
          return()
        }


        df <- plotData$data$waterfall

        # Optimize for large datasets
        df <- private$.optimizeForLargeDatasets(df)

        # Sort data
        if (plotData$options$sortBy == "response") {
          df <- df[order(df$response, na.last = TRUE),]
        } else if (plotData$options$sortBy == "id") {
          df <- df[order(df[[plotData$options$patientID]], na.last = TRUE),]
        }

        # Define colorblind-safe color schemes
        recistColors <- c(
          "CR" = "#1b9e77",  # teal - colorblind safe
          "PR" = "#7570b3",  # purple - colorblind safe
          "SD" = "#e7298a",  # magenta - colorblind safe
          "PD" = "#e66101",  # orange - colorblind safe
          "NA" = "#666666"   # gray
        )

        simpleColors <- c(
          "CR" = "#1b9e77",  # teal for positive response
          "PR" = "#1b9e77",  # same teal for positive response
          "SD" = "#666666",  # gray for stable
          "PD" = "#e66101",  # orange for progression
          "NA" = "#999999"   # lighter gray
        )

        # Okabe-Ito colorblind-safe palette
        colorblindColors <- c(
          "CR" = "#009E73",  # bluish green
          "PR" = "#56B4E9",  # sky blue
          "SD" = "#E69F00",  # orange
          "PD" = "#CC79A7",  # reddish purple
          "NA" = "#999999"   # gray
        )

        # Check if group-based coloring is requested and group variable exists
        useGroupColoring <- !is.null(plotData$options$colorBy) &&
                           plotData$options$colorBy == "group" &&
                           "patient_group" %in% names(df)

        # Preset-aware enhancements
        preset <- "custom" # self$options$clinicalPreset
        if (preset == "biomarker" && !useGroupColoring) {
          message("Biomarker preset: Consider enabling group-based coloring for biomarker analysis")
        }

        if (useGroupColoring) {
          # Generate distinct colors for groups using reusable method
          group_levels <- unique(df$patient_group)
          colors <- private$.generateGroupColors(group_levels, plotData$options$colorScheme)
          fill_var <- "patient_group"
          legend_name <- .("Patient Group")
        } else {
          # Use RECIST coloring based on selected scheme
          colors <- switch(plotData$options$colorScheme,
            "simple" = simpleColors,
            "colorblind" = colorblindColors,
            "jamovi" = recistColors,
            "recist" = recistColors,
            recistColors  # default fallback
          )
          fill_var <- "recist_category"
          legend_name <- .("RECIST Response")
        }

        # Create base plot
        p <- ggplot2::ggplot(df, ggplot2::aes(
          x = factor(seq_len(nrow(df))),
          y = response
        )) +
          ggplot2::geom_bar(
            stat = "identity",
            ggplot2::aes(fill = !!rlang::sym(fill_var)),
            width = plotData$options$barWidth,
            alpha = plotData$options$barAlpha
          ) +
          ggplot2::scale_fill_manual(
            name = legend_name,
            values = colors,
            na.value = "#808080",
            drop = FALSE
          ) +
          ggplot2::labs(
            x = .("Patients"),
            y = .("Change in Tumor Size (%)")
          )

        # Add RECIST thresholds
        if (plotData$options$showThresholds) {
          p <- p +
            ggplot2::geom_hline(
              yintercept = c(private$RECIST_PR_THRESHOLD, private$RECIST_PD_THRESHOLD),
              linetype = "dashed",
              color = c("#4169E1", "#FF0000"),
              alpha = 0.5
            )
        }

        # Add labels for large changes
        if (plotData$options$labelOutliers) {
          threshold <- plotData$options$minResponseForLabel
          labels <- ifelse(
            !is.na(df$response) & abs(df$response) > threshold,
            sprintf("%.1f%%", df$response),
            ""
          )

          if (any(labels != "")) {
            p <- p +
              ggplot2::geom_text(
                data = df[labels != "",],
                mapping = ggplot2::aes(
                  x = factor(which(labels != "")),
                  y = response
                ),
                label = labels[labels != ""],
                vjust = ifelse(
                  df$response[labels != ""] >= 0,
                  -0.5,
                  1.5
                ),
                size = 3
              )
          }
        }

        # Add median line
        if (plotData$options$showMedian) {
          med <- median(df$response, na.rm=TRUE)
          p <- p +
            ggplot2::geom_hline(
              yintercept = med,
              linetype = "dotted",
              color = "darkgray"
            ) +
            ggplot2::annotate(
              "text",
              x = nrow(df),
              y = med,
              label = sprintf("Median: %.1f%%", med),
              hjust = 1,
              vjust = -0.5,
              size = 3
            )
        }

        # Add confidence interval
        if (plotData$options$showCI && nrow(df) >= 10) {
          ci <- t.test(df$response)$conf.int
          p <- p +
            ggplot2::annotate(
              "text",
              x = 1,
              y = max(df$response, na.rm=TRUE),
              label = sprintf(
                "95%% CI: [%.1f%%, %.1f%%]",
                ci[1],
                ci[2]
              ),
              hjust = 0,
              vjust = -0.5,
              size = 3
            )
        }

        # Add theme
        if (plotData$options$colorScheme == "jamovi") {
          p <- p + ggtheme
        }

        p <- p +
          ggplot2::theme(
            axis.text.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.minor.x = ggplot2::element_blank(),
            legend.position = "right"
          )




        # Add waterfall plot faceting by subgroup
        # if (!is.null(plotData$options$subgroupVar)) {
        #   p <- p + ggplot2::facet_wrap(~subgroup)
        # }



        print(p)
        TRUE
      }




      ,
      # spider plot ----
      .spiderplot = function(imagespider, ggtheme, theme, ...) {
        private$.checkpoint()  # Checkpoint before spider plot generation

        # Check conditions for showing the information message
        if (is.null(self$options$timeVar) || !self$options$showSpiderPlot) {
          # Create an informative message with improved formatting for readability
          text_warning <- paste0(
            .("Spider Plot Requirements and Guidelines"),
            "\n\n",
            .("This visualization requires two key elements:"),
            "\n",
            .("1. A time variable to show response trajectories"),
            "\n",
            .("2. The 'Show Spider Plot' option to be enabled"),
            "\n\n",
            .("Understanding Spider Plots:"),
            "\n",
            .("A spider plot helps visualize how each patient's response changes over time. \n"),
            .("Each line represents one patient's treatment journey, making it easy to see \n"),
            .("patterns in response and identify different types of outcomes."),
            "\n\n",
            .("To Generate the Plot:"),
            "\n",
            .("- Add a time variable (such as months from baseline)"),
            "\n",
            .("- Enable 'Show Spider Plot' in the options panel"),
            "\n\n",
            .("The resulting visualization will help you track response patterns \n"),
            .("and compare outcomes across different patients over time.\n\n"),
            sep = ""
          )

          text_warning <- paste0(text_warning,
                                .("Time Variable Requirement:"),
                                .("\n\nA time variable is required to create visualizations when using raw measurements."),
                                .("\n\nWhy is this important?"),
                                .("\n- Baseline identification: Marks the starting point (time = 0)"),
                                .("\n- Response calculation: Computes changes from baseline"),
                                .("\n- Progression tracking: Shows how response changes over time"),
                                .("\n\nHow to proceed:"),
                                .("\n1. Add a time variable to your data"),
                                .("\n2. Time should start at 0 (baseline)"),
                                .("\n3. Use consistent time units (e.g., months or weeks)"),
                                .("\n\nExample time variable format:"),
                                .("\nPatientID.         Time          Measurement"),
                                .("\nPT1                0             50              (baseline)"),
                                .("\nPT1                2             25              (2 months)"),
                                .("\nPT1                4             10              (4 months)"),
                                sep = ""
          )



          # Create a new page
          grid::grid.newpage()

          # Create a viewport with margins for better readability
          vp <- grid::viewport(
            width = 0.9,    # Wider viewport for left-aligned text
            height = 0.9,   # Keep reasonable margins
            x = 0.5,        # Center the viewport
            y = 0.5         # Center the viewport
          )
          grid::pushViewport(vp)

          # Add the text with left alignment
          grid::grid.text(
            text_warning,
            x = 0.05,           # Move text to the left (5% margin)
            y = 0.95,           # Start from top (5% margin)
            just = c("left", "top"),  # Left align and top justify
            gp = grid::gpar(
              fontsize = 11,        # Maintain readable size
              fontface = "plain",   # Regular font
              lineheight = 1.3      # Slightly increased line spacing for readability
            )
          )

          # Reset viewport
          grid::popViewport()

          return(TRUE)
        }

        # Get plot data from state

        plotData <- imagespider$state

        # if (is.null(plotData) || is.null(plotData$data$spider)) {
        #   warning("Spider plot data not properly initialized")
        #   return()
        # }

        # Extract data and options
        df <- plotData$data$spider
        options <- plotData$options
        
        # Debug: Check if df exists and has data
        if (is.null(df) || nrow(df) == 0) {
          warning(.("Spider plot data is empty or null"))
          return()
        }

        # Validate required variables exist
        required_vars <- c(options$timeVar, options$patientID)
        missing_vars <- required_vars[!required_vars %in% names(df)]
        if (length(missing_vars) > 0) {
          warning(sprintf(.("Missing required variables: %s"),
                          paste(missing_vars, collapse = ", ")))
          return()
        }

        # Convert variables to numeric explicitly
        df$time <- jmvcore::toNumeric(df[[options$timeVar]])
        
        # Check if response column exists, if not create it from the response variable
        if ("response" %in% names(df)) {
          df$response <- jmvcore::toNumeric(df$response)
        } else if ("percentage_change" %in% names(df)) {
          df$response <- jmvcore::toNumeric(df$percentage_change)
        } else {
          # For raw data, we need to calculate percentage change from baseline
          df$response <- jmvcore::toNumeric(df[[options$responseVar]])
        }

        # Remove any rows with NA values in required columns
        if ("time" %in% names(df) && "response" %in% names(df)) {
          df <- df[complete.cases(df[c("time", "response")]), ]
        } else {
          warning(.("Required columns 'time' or 'response' not found in spider plot data"))
          return()
        }

        # Sort data by patient and time
        df <- df[order(df[[options$patientID]], df$time), ]
        
        # Determine coloring method (backward compatible, defaults to response)
        spiderColorBy <- options$spiderColorBy %||% "response"
        spiderColorScheme <- options$spiderColorScheme %||% "classic"
        useGroupColoring <- spiderColorBy == "group" && "patient_group" %in% names(df)

        # Preset-aware spider plot enhancements
        preset <- "custom" # self$options$clinicalPreset
        if (preset == "biomarker" && !useGroupColoring) {
          message("Biomarker preset: Group-based spider plot coloring recommended for biomarker studies")
        }
        if (preset == "phase1_2" && self$options$inputType == "percentage") {
          message("Phase I/II preset: Raw measurements with time points provide better safety monitoring")
        }
        
        # Set up color variables and schemes
        if (useGroupColoring) {
          # Group-based coloring using reusable method
          group_levels <- unique(df$patient_group)
          line_colors <- private$.generateGroupColors(group_levels, spiderColorScheme)
          point_colors <- line_colors  # Use same colors for lines and points
          
          # Create the spider plot with group coloring
          p <- ggplot2::ggplot(df) +
            # Add lines connecting points for each patient, colored by group
            ggplot2::geom_line(
              mapping = ggplot2::aes(
                x = time,
                y = response,
                group = .data[[options$patientID]],
                color = patient_group
              ),
              size = 1,
              alpha = 0.7
            ) +
            # Add points at each measurement, colored by group
            ggplot2::geom_point(
              mapping = ggplot2::aes(
                x = time,
                y = response,
                fill = patient_group
              ),
              size = 3,
              shape = 21,
              color = "black",
              alpha = 0.8
            ) +
            # Define colors
            ggplot2::scale_color_manual(
              name = .("Patient Group"),
              values = line_colors,
              na.value = "#808080"
            ) +
            ggplot2::scale_fill_manual(
              name = .("Patient Group"),
              values = point_colors,
              na.value = "#808080"
            )
        } else {
          # Response-based coloring (default for backward compatibility)
          # Create categorical responder variable with proper labels
          df$responder_status <- ifelse(df$response <= private$RECIST_PR_THRESHOLD,
                                       .("Responder"), .("Non-responder"))

          # Colorblind-safe responder colors
          responder_colors <- switch(spiderColorScheme,
            "classic" = c("Non-responder" = "#e66101", "Responder" = "#1b9e77"),  # orange vs teal
            "jamovi" = c("Non-responder" = "#d95f02", "Responder" = "#7570b3"),  # orange vs purple
            "colorblind" = c("Non-responder" = "#CC79A7", "Responder" = "#009E73"),  # Okabe-Ito reddish purple vs bluish green
            "colorful" = c("Non-responder" = "#e66101", "Responder" = "#1b9e77"),  # same as classic for responder status
            c("Non-responder" = "#e66101", "Responder" = "#1b9e77")  # default fallback
          )

          # Create the spider plot with response coloring
          p <- ggplot2::ggplot(df) +
            # Add lines connecting points for each patient
            ggplot2::geom_line(
              mapping = ggplot2::aes(
                x = time,
                y = response,
                group = .data[[options$patientID]]
              ),
              size = 1,
              color = "gray50"
            ) +
            # Add points at each measurement
            ggplot2::geom_point(
              mapping = ggplot2::aes(
                x = time,
                y = response,
                fill = responder_status
              ),
              size = 3,
              shape = 21,
              color = "black"
            ) +
            # Define colors for response categories
            ggplot2::scale_fill_manual(
              name = .("Response Status"),
              values = responder_colors
            )
        }
        
        # Add common plot elements
        p <- p +
          # Add RECIST threshold lines
          ggplot2::geom_hline(
            yintercept = c(private$RECIST_PR_THRESHOLD, private$RECIST_PD_THRESHOLD),
            linetype = "dashed",
            color = "gray50",
            alpha = 0.5
          ) +
          # Add labels
          # Configure x-axis label based on user-selected time unit label
          x_unit_label <- switch(options$timeUnitLabel,
            "days"   = .("Days from Baseline"),
            "weeks"  = .("Weeks from Baseline"),
            "months" = .("Months from Baseline"),
            "years"  = .("Years from Baseline"),
            .("Time from Baseline")
          )
          ggplot2::labs(
            x = x_unit_label,
            y = .("Change in Tumor Size (%)"),
            title = .("Spider Plot of Tumor Response")
          )

        # Add theme
        p <- p + ggtheme +
          ggplot2::theme(
            legend.position = "right",
            panel.grid.minor = ggplot2::element_blank(),
            axis.text = ggplot2::element_text(size = 10),
            axis.title = ggplot2::element_text(size = 12),
            plot.title = ggplot2::element_text(size = 14, face = "bold")
          )

        # Optional annotations
        if (options$showThresholds) {
          # Add threshold annotations
          p <- p +
            ggplot2::annotate(
              "text",
              x = min(df$time),
              y = c(private$RECIST_PR_THRESHOLD, private$RECIST_PD_THRESHOLD),
              label = c(sprintf(.("PR threshold (%d%%)"), private$RECIST_PR_THRESHOLD),
                       sprintf(.("PD threshold (+%d%%)"), private$RECIST_PD_THRESHOLD)),
              hjust = 0,
              vjust = c(1.5, -0.5),
              size = 3,
              color = "gray50"
            )
        }

        # Add summary statistics if requested
        if (options$showMedian) {
          # Calculate median response at each timepoint
          median_response <- stats::aggregate(
            response ~ time,
            data = df,
            FUN = median
          )

          # Add median line
          p <- p +
            ggplot2::geom_line(
              data = median_response,
              mapping = ggplot2::aes(
                x = time,
                y = response
              ),
              color = "black",
              linetype = "dotted",
              size = 1
            )
        }

        # Try to print the plot with error handling
        tryCatch({
          print(p)
          TRUE
        }, error = function(e) {
          warning(sprintf(.("Error creating spider plot: %s"), e$message))
          FALSE
        })
      }

      ,
      # Generate clinical summary ----
      .generateClinicalSummary = function(processed_data, metrics, person_time_metrics = NULL) {
        
        # Extract key metrics
        n_patients <- nrow(processed_data$waterfall)
        orr <- metrics$ORR
        dcr <- metrics$DCR
        
        # Count responses by category
        response_counts <- processed_data$waterfall %>%
          dplyr::count(recist_category) %>%
          dplyr::mutate(percent = round(n / sum(n) * 100, 1))
        
        # Get counts for each category (with safe extraction)
        cr_count <- ifelse(any(response_counts$recist_category == "CR"),
                          response_counts$n[response_counts$recist_category == "CR"], 0)
        pr_count <- ifelse(any(response_counts$recist_category == "PR"),
                          response_counts$n[response_counts$recist_category == "PR"], 0)
        sd_count <- ifelse(any(response_counts$recist_category == "SD"),
                          response_counts$n[response_counts$recist_category == "SD"], 0)
        pd_count <- ifelse(any(response_counts$recist_category == "PD"),
                          response_counts$n[response_counts$recist_category == "PD"], 0)
        
        # Generate natural language summary
        summary_text <- paste0(
          "<div style='background-color: #f8f9fa; padding: 15px; border-left: 4px solid #1b9e77; margin: 10px 0;'>",
          "<h4 style='color: #1b9e77; margin-top: 0;'>", .("Treatment Response Summary"), "</h4>",
          
          "<p><strong>", .("Analysis Overview:"), "</strong> ",
          sprintf(.("Response analysis of %d patients using RECIST v1.1 criteria."), n_patients), "</p>",
          
          "<p><strong>", .("Key Findings:"), "</strong></p>",
          "<ul>",
          "<li><strong>", .("Objective Response Rate (ORR):"), "</strong> ", orr, "% ",
          sprintf(.("(%d patients achieved complete or partial response)"), cr_count + pr_count), "</li>",
          "<li><strong>", .("Disease Control Rate (DCR):"), "</strong> ", dcr, "% ",
          sprintf(.("(%d patients achieved response or stable disease)"), cr_count + pr_count + sd_count), "</li>",
          "</ul>",
          
          "<p><strong>", .("Response Distribution:"), "</strong></p>",
          "<ul>",
          if (cr_count > 0) paste0("<li>", .("Complete Response:"), " ", cr_count, " ", .("patients"), " (", 
                                   round(cr_count/n_patients*100, 1), "%)</li>") else "",
          if (pr_count > 0) paste0("<li>", .("Partial Response:"), " ", pr_count, " ", .("patients"), " (", 
                                   round(pr_count/n_patients*100, 1), "%)</li>") else "",
          if (sd_count > 0) paste0("<li>", .("Stable Disease:"), " ", sd_count, " ", .("patients"), " (", 
                                   round(sd_count/n_patients*100, 1), "%)</li>") else "",
          if (pd_count > 0) paste0("<li>", .("Progressive Disease:"), " ", pd_count, " ", .("patients"), " (", 
                                   round(pd_count/n_patients*100, 1), "%)</li>") else "",
          "</ul>"
        )
        
        # Add clinical interpretation
        if (orr >= 30) {
          interpretation <- .("This represents a promising response rate for oncology studies.")
        } else if (orr >= 15) {
          interpretation <- .("This represents a moderate response rate.")
        } else {
          interpretation <- .("This represents a lower response rate that may require further investigation.")
        }
        
        # Optional methods note for person-time metrics
        if (!is.null(person_time_metrics)) {
          summary_text <- paste0(summary_text,
            "<p><em>", .("Methods note:"), " ",
            .("Person-time metrics summarize the total time patients spend in response relative to total follow-up, reported here as response time per 100 person-time units."),
            "</em></p>")
        }

        summary_text <- paste0(summary_text,
          "<p><strong>", .("Clinical Interpretation:"), "</strong> ", interpretation, "</p>",
          "</div>"
        )
        
        # Set the content
        self$results$clinicalSummary$setContent(summary_text)
      }

      ,
      # Generate about analysis panel ----
      .generateAboutAnalysis = function() {
        about_text <- paste0(
          "<div style='background-color: #f0f8ff; padding: 15px; border: 1px solid #d1ecf1; border-radius: 5px; margin: 10px 0;'>",
          "<h4 style='color: #0c5460; margin-top: 0;'>", .("What This Analysis Does"), "</h4>",
          
          "<p>", .("The Treatment Response Analysis creates waterfall and spider plots to visualize tumor response data according to RECIST v1.1 criteria."), "</p>",
          
          "<h5>", .("Visualization Types:"), "</h5>",
          "<ul>",
          "<li><strong>", .("Waterfall Plot:"), "</strong> ", .("Shows best response for each patient as vertical bars, ideal for single timepoint or best response data."), "</li>",
          "<li><strong>", .("Spider Plot:"), "</strong> ", .("Shows response trajectories over time as connected lines, requires time variable for longitudinal data."), "</li>",
          "</ul>",
          
          "<h5>", .("When to Use This Analysis:"), "</h5>",
          "<ul>",
          "<li>", .("Oncology clinical trials and treatment response studies"), "</li>",
          "<li>", .("Drug efficacy evaluation"), "</li>",
          "<li>", .("Tumor response monitoring"), "</li>",
          "<li>", .("Biomarker correlation studies"), "</li>",
          "</ul>",
          
          "<h5>", .("Data Requirements:"), "</h5>",
          "<ul>",
          "<li><strong>", .("Patient ID:"), "</strong> ", .("Unique identifier for each patient"), "</li>",
          "<li><strong>", .("Response Data:"), "</strong> ", .("Either percentage changes from baseline or raw tumor measurements"), "</li>",
          "<li><strong>", .("Time Variable:"), "</strong> ", .("Required for spider plots (e.g., months from baseline)"), "</li>",
          "</ul>",
          
          "<h5>", .("Key Assumptions & Limitations:"), "</h5>",
          "<ul>",
          sprintf("<li>%s CR ≤%d%%, PR ≤%d%%, PD >+%d%%</li>", .("RECIST v1.1 thresholds:"), private$RECIST_CR_THRESHOLD, private$RECIST_PR_THRESHOLD, private$RECIST_PD_THRESHOLD),
          "<li>", .("For raw measurements, baseline assumed at time = 0"), "</li>",
          "<li>", .("Waterfall plot shows best (most negative) response per patient"), "</li>",
          "<li>", .("Missing values are excluded from analysis"), "</li>",
          "</ul>",
          
          "<p><em>", .("Tip: Start with percentage data if available, or use raw measurements with proper time variables for automatic calculation."), "</em></p>",
          
          "</div>"
        )
        
        self$results$aboutAnalysis$setContent(about_text)
      }

      ,
      # Get effective option value (supports preset overrides in future)
      .getEffectiveOption = function(optionName) {
        # For now, return the option directly since JS events handle the preset changes
        # This method provides future extensibility for R-side preset logic if needed
        return(self$options[[optionName]])
      }

      ,
      # Apply clinical presets ----
      .applyClinicalPreset = function() {
        preset <- "custom" # self$options$clinicalPreset
        

        if (preset != "custom") {
          warning(
            .("Clinical presets are temporarily disabled; using custom settings."),
            call. = FALSE
          )
        }

        return()
      }

      ,
      # Preset-specific validation methods
      .validatePhase2Requirements = function() {
        # Phase II studies typically need confidence intervals and significance testing
        if (!self$options$showConfidenceIntervals) {
          message("Phase II preset: Confidence intervals recommended for regulatory submissions")
        }
        if (!self$options$generateCopyReadyReport) {
          message("Phase II preset: Copy-ready report enabled for regulatory documentation")
        }
      }

      ,
      .validatePhase12Requirements = function() {
        # Phase I/II studies need detailed monitoring and flexible analysis
        if (is.null(self$options$timeVar) && self$options$inputType == "raw") {
          message("Phase I/II preset: Time variable recommended for safety monitoring")
        }
      }

      ,
      .validateBiomarkerRequirements = function() {
        # Biomarker studies require group variables for meaningful analysis
        if (is.null(self$options$groupVar)) {
          message("Biomarker preset: Group variable required for biomarker correlation analysis")
        }
      }

      ,
      .validatePublicationRequirements = function() {
        # Publication quality requires specific statistical and visual standards
        if (self$options$barAlpha < 1.0) {
          message("Publication preset: Solid colors (alpha=1.0) recommended for print clarity")
        }
        if (!self$options$showConfidenceIntervals) {
          message("Publication preset: Confidence intervals essential for peer review")
        }
      }

      ,
      # Generate enhanced clinical metrics with confidence intervals ----
      .generateEnhancedClinicalMetrics = function(processed_data, metrics) {
        n_responders <- sum(processed_data$waterfall$recist_category %in% c("CR", "PR"), na.rm = TRUE)
        n_dcr <- sum(processed_data$waterfall$recist_category %in% c("CR", "PR", "SD"), na.rm = TRUE)
        n_total <- nrow(processed_data$waterfall)

        # Calculate exact binomial confidence intervals with edge case handling
        orr_ci <- tryCatch({
          if (n_total == 0) {
            c(0, 1)  # No data case
          } else if (n_responders == 0) {
            # Use exact method for 0 events
            binom.test(0, n_total, conf.level = 0.95)$conf.int
          } else if (n_responders == n_total) {
            # Use exact method for 100% response
            binom.test(n_total, n_total, conf.level = 0.95)$conf.int
          } else {
            binom.test(n_responders, n_total, conf.level = 0.95)$conf.int
          }
        }, error = function(e) {
          c(NA, NA)
        })

        dcr_ci <- tryCatch({
          if (n_total == 0) {
            c(0, 1)  # No data case
          } else if (n_dcr == 0) {
            # Use exact method for 0 events
            binom.test(0, n_total, conf.level = 0.95)$conf.int
          } else if (n_dcr == n_total) {
            # Use exact method for 100% disease control
            binom.test(n_total, n_total, conf.level = 0.95)$conf.int
          } else {
            binom.test(n_dcr, n_total, conf.level = 0.95)$conf.int
          }
        }, error = function(e) {
          c(NA, NA)
        })

        # Add ORR with CI
        self$results$enhancedClinicalMetrics$addRow(rowKey = 1, values = list(
          metric = .("Objective Response Rate (ORR)"),
          value = sprintf("%.1f%%", metrics$ORR),
          ci_lower = round(orr_ci[1] * 100, 1),
          ci_upper = round(orr_ci[2] * 100, 1),
          interpretation = private$.interpretORR(metrics$ORR)
        ))

        # Add DCR with CI
        self$results$enhancedClinicalMetrics$addRow(rowKey = 2, values = list(
          metric = .("Disease Control Rate (DCR)"),
          value = sprintf("%.1f%%", metrics$DCR),
          ci_lower = round(dcr_ci[1] * 100, 1),
          ci_upper = round(dcr_ci[2] * 100, 1),
          interpretation = private$.interpretDCR(metrics$DCR)
        ))
      }

      ,
      # Generate copy-ready report sentences ----
      .generateCopyReadyReport = function(processed_data, metrics, person_time_metrics = NULL) {
        n_patients <- nrow(processed_data$waterfall)

        # Count responses by category
        response_counts <- processed_data$waterfall %>%
          dplyr::count(recist_category) %>%
          dplyr::mutate(percent = round(n / sum(n) * 100, 1))

        cr_count <- response_counts$n[response_counts$recist_category == "CR"] %||% 0
        pr_count <- response_counts$n[response_counts$recist_category == "PR"] %||% 0

        # Calculate confidence intervals
        n_responders <- cr_count + pr_count
        orr_ci <- tryCatch({
          ci <- binom.test(n_responders, n_patients)$conf.int
          sprintf("95%% CI: %.1f-%.1f%%", ci[1] * 100, ci[2] * 100)
        }, error = function(e) {
          "95% CI: not calculable"
        })

        # Generate publication-ready sentences
        report_text <- paste0(
          "<div style='background-color: #f0f9ff; padding: 15px; border: 1px solid #0369a1; border-radius: 5px; margin: 10px 0;'>",
          "<h4 style='color: #0369a1; margin-top: 0;'>", .("Copy-Ready Report Sentences"), "</h4>",

          "<div style='background-color: white; padding: 10px; border-radius: 3px; margin: 10px 0;'>",
          "<h5>", .("Main Results:"), "</h5>",
          "<p style='font-family: monospace; background-color: #f8f9fa; padding: 8px; border-radius: 3px;'>",
          sprintf(.("Treatment response was evaluable in %d patients. The objective response rate (ORR) was %.1f%% (%s), with %d patients achieving complete response and %d achieving partial response. The disease control rate (DCR) was %.1f%%."),
                  n_patients, metrics$ORR, orr_ci, cr_count, pr_count, metrics$DCR),
          "</p>",
          "</div>",

          "<div style='background-color: white; padding: 10px; border-radius: 3px; margin: 10px 0;'>",
          "<h5>", .("Methods Description:"), "</h5>",
          "<p style='font-family: monospace; background-color: #f8f9fa; padding: 8px; border-radius: 3px;'>",
          .("Tumor response was assessed according to Response Evaluation Criteria in Solid Tumors (RECIST) version 1.1. Best overall response was determined for each patient, and response rates were calculated with exact binomial confidence intervals."),
          "</p>",
          "</div>",

          "<p><small>", .("Copy these sentences directly into your manuscript or clinical report. Modify as needed for your specific context."), "</small></p>",
          "</div>"
        )

        self$results$copyReadyReport$setContent(report_text)
      }

      ,
      # Optimize plotting performance for large datasets ----
      .optimizeForLargeDatasets = function(df) {
        n_patients <- nrow(df)

        # For very large datasets, consider sampling or aggregation strategies
        if (n_patients > 2000) {
          message(sprintf("Large dataset detected (%d patients). Consider performance optimizations.", n_patients))

          # Option 1: Warn user about potential performance impact
          if (n_patients > 5000) {
            warning(paste0(
              "Very large dataset (", n_patients, " patients). ",
              "Plotting may be slow. Consider filtering data or using summary statistics."
            ))
          }

          # Option 2: For extremely large datasets, suggest subsampling
          if (n_patients > 10000) {
            message(paste0(
              "Extremely large dataset detected. ",
              "Consider subsampling for visualization or using summary tables only."
            ))
          }
        }

        return(df)
      }

      ,
      # Generate group comparison analysis ----
      .generateGroupComparison = function(processed_data) {
        if (is.null(self$options$groupVar) || !"patient_group" %in% names(processed_data$waterfall)) {
          return()
        }

        df <- processed_data$waterfall

        # Calculate statistics by group
        group_stats <- df %>%
          dplyr::group_by(patient_group) %>%
          dplyr::summarise(
            n_patients = dplyr::n(),
            n_responders = sum(recist_category %in% c("CR", "PR"), na.rm = TRUE),
            n_dcr = sum(recist_category %in% c("CR", "PR", "SD"), na.rm = TRUE),
            orr = (n_responders / n_patients) * 100,
            dcr = (n_dcr / n_patients) * 100,
            .groups = "drop"
          )

        # Calculate confidence intervals for each group
        group_stats <- group_stats %>%
          dplyr::rowwise() %>%
          dplyr::mutate(
            orr_ci = if (n_patients > 0) {
              ci <- tryCatch({
                binom.test(n_responders, n_patients)$conf.int
              }, error = function(e) c(NA, NA))
              sprintf("%.1f-%.1f%%", ci[1] * 100, ci[2] * 100)
            } else "---",
            dcr_ci = if (n_patients > 0) {
              ci <- tryCatch({
                binom.test(n_dcr, n_patients)$conf.int
              }, error = function(e) c(NA, NA))
              sprintf("%.1f-%.1f%%", ci[1] * 100, ci[2] * 100)
            } else "---"
          ) %>%
          dplyr::ungroup()

        # Populate group comparison table
        for (i in seq_len(nrow(group_stats))) {
          self$results$groupComparisonTable$addRow(rowKey = i, values = list(
            group = as.character(group_stats$patient_group[i]),
            n_patients = group_stats$n_patients[i],
            orr = round(group_stats$orr[i], 1),
            orr_ci = group_stats$orr_ci[i],
            dcr = round(group_stats$dcr[i], 1),
            dcr_ci = group_stats$dcr_ci[i]
          ))
        }

        # Perform statistical tests if we have 2+ groups with sufficient data
        if (nrow(group_stats) >= 2 && all(group_stats$n_patients >= 1)) {

          # Fisher's exact test for ORR comparison
          orr_contingency <- df %>%
            dplyr::mutate(
              responder = recist_category %in% c("CR", "PR")
            ) %>%
            dplyr::select(patient_group, responder) %>%
            table()

          orr_test <- tryCatch({
            if (nrow(orr_contingency) >= 2 && ncol(orr_contingency) >= 2) {
              fisher.test(orr_contingency)
            } else {
              NULL
            }
          }, error = function(e) NULL)

          # Fisher's exact test for DCR comparison
          dcr_contingency <- df %>%
            dplyr::mutate(
              disease_control = recist_category %in% c("CR", "PR", "SD")
            ) %>%
            dplyr::select(patient_group, disease_control) %>%
            table()

          dcr_test <- tryCatch({
            if (nrow(dcr_contingency) >= 2 && ncol(dcr_contingency) >= 2) {
              fisher.test(dcr_contingency)
            } else {
              NULL
            }
          }, error = function(e) NULL)

          # Add test results to table
          row_count <- 1

          if (!is.null(orr_test)) {
            orr_interpretation <- if (orr_test$p.value < 0.05) {
              .("Statistically significant difference in response rates between groups")
            } else {
              .("No statistically significant difference in response rates between groups")
            }

            self$results$groupComparisonTest$addRow(rowKey = row_count, values = list(
              comparison = .("Objective Response Rate (ORR)"),
              test_statistic = sprintf("Fisher's exact test, OR = %.2f",
                                     if (!is.null(orr_test$estimate)) orr_test$estimate else NA),
              p_value = round(orr_test$p.value, 4),
              interpretation = orr_interpretation
            ))
            row_count <- row_count + 1
          }

          if (!is.null(dcr_test)) {
            dcr_interpretation <- if (dcr_test$p.value < 0.05) {
              .("Statistically significant difference in disease control rates between groups")
            } else {
              .("No statistically significant difference in disease control rates between groups")
            }

            self$results$groupComparisonTest$addRow(rowKey = row_count, values = list(
              comparison = .("Disease Control Rate (DCR)"),
              test_statistic = sprintf("Fisher's exact test, OR = %.2f",
                                     if (!is.null(dcr_test$estimate)) dcr_test$estimate else NA),
              p_value = round(dcr_test$p.value, 4),
              interpretation = dcr_interpretation
            ))
          }
        }
      }

      ,
      # Generate enhanced clinical glossary ----
      .generateClinicalGlossary = function() {
        glossary_text <- paste0(
          "<div style='background-color: #f8f9fa; padding: 15px; border: 1px solid #dee2e6; border-radius: 5px; margin: 10px 0;'>",
          "<h4 style='color: #495057; margin-top: 0;'>", .("Clinical Terms & Definitions"), "</h4>",

          "<div style='display: grid; grid-template-columns: 1fr 1fr; gap: 15px;'>",

          "<div>",
          "<h5 style='color: #6c757d; margin-bottom: 10px;'>", .("Response Metrics"), "</h5>",
          "<ul style='margin: 0; padding-left: 15px; line-height: 1.6;'>",
          "<li><strong>ORR (Objective Response Rate):</strong> ", .("Percentage of patients achieving complete or partial response (CR + PR)"), "</li>",
          "<li><strong>DCR (Disease Control Rate):</strong> ", .("Percentage achieving response or stable disease (CR + PR + SD)"), "</li>",
          "<li><strong>Best Response:</strong> ", .("Most favorable tumor response achieved during treatment period"), "</li>",
          "<li><strong>Person-Time:</strong> ", .("Total time patients are followed, accounting for different follow-up durations"), "</li>",
          "</ul>",
          "</div>",

          "<div>",
          "<h5 style='color: #6c757d; margin-bottom: 10px;'>", .("RECIST Categories"), "</h5>",
          "<ul style='margin: 0; padding-left: 15px; line-height: 1.6;'>",
          "<li><strong>CR (Complete Response):</strong> ", .("Complete disappearance of all target lesions (≤-100%)"), "</li>",
          "<li><strong>PR (Partial Response):</strong> ", .("≥30% decrease in sum of target lesions from baseline"), "</li>",
          "<li><strong>SD (Stable Disease):</strong> ", .("Neither PR nor PD criteria met (between -30% and +20%)"), "</li>",
          "<li><strong>PD (Progressive Disease):</strong> ", .("≥20% increase in sum of target lesions"), "</li>",
          "</ul>",
          "</div>",

          "</div>",

          "<div style='margin-top: 15px;'>",
          "<h5 style='color: #6c757d; margin-bottom: 10px;'>", .("Statistical Terms"), "</h5>",
          "<ul style='margin: 0; padding-left: 15px; line-height: 1.6;'>",
          "<li><strong>95% CI (Confidence Interval):</strong> ", .("Range of values likely to contain the true population parameter"), "</li>",
          "<li><strong>Fisher's Exact Test:</strong> ", .("Statistical test for comparing response rates between groups"), "</li>",
          "<li><strong>Binomial CI:</strong> ", .("Exact confidence interval for proportions (more accurate than normal approximation)"), "</li>",
          "<li><strong>Waterfall Plot:</strong> ", .("Bar chart showing best response for each patient, sorted by magnitude"), "</li>",
          "<li><strong>Spider Plot:</strong> ", .("Line graph showing individual patient response trajectories over time"), "</li>",
          "</ul>",
          "</div>",

          "<div style='margin-top: 15px; padding: 10px; background-color: #e3f2fd; border-radius: 3px;'>",
          "<small><strong>", .("Clinical Context:"), "</strong> ",
          .("These metrics follow international standards for oncology clinical trials and are commonly used in regulatory submissions and peer-reviewed publications."),
          "</small>",
          "</div>",

          "</div>"
        )

        self$results$clinicalGlossary$setContent(glossary_text)
      }

      ,
      # Generate clinical significance assessment ----
      .generateClinicalSignificance = function(metrics, n_patients) {
        orr_interpretation <- private$.interpretORR(metrics$ORR)
        dcr_interpretation <- private$.interpretDCR(metrics$DCR)

        # Sample size adequacy assessment
        sample_size_assessment <- if (n_patients < 20) {
          .("Very small sample size (n<20): Results should be interpreted with extreme caution. Confidence intervals will be very wide.")
        } else if (n_patients < 50) {
          .("Small sample size (n<50): Results provide preliminary evidence but should be confirmed in larger studies.")
        } else if (n_patients < 100) {
          .("Moderate sample size: Results provide reasonable evidence for preliminary conclusions.")
        } else {
          .("Adequate sample size: Results provide reliable evidence for clinical interpretation.")
        }

        significance_text <- paste0(
          "<div style='background-color: #fef3c7; padding: 15px; border-left: 4px solid #f59e0b; margin: 10px 0;'>",
          "<h4 style='color: #92400e; margin-top: 0;'>", .("Clinical Significance Assessment"), "</h4>",

          "<h5>", .("Response Rate Interpretation:"), "</h5>",
          "<ul>",
          "<li><strong>", .("ORR"), " (", metrics$ORR, "%): </strong>", orr_interpretation, "</li>",
          "<li><strong>", .("DCR"), " (", metrics$DCR, "%): </strong>", dcr_interpretation, "</li>",
          "</ul>",

          "<h5>", .("Sample Size Adequacy:"), "</h5>",
          "<p>", sample_size_assessment, "</p>",

          "<h5>", .("Clinical Context:"), "</h5>",
          "<ul>",
          "<li>", .("ORR <15%: May not warrant further development without compelling rationale"), "</li>",
          "<li>", .("ORR 15-30%: Moderate activity, may justify phase III evaluation"), "</li>",
          "<li>", .("ORR >30%: Promising activity, strong candidate for further development"), "</li>",
          "</ul>",

          "</div>"
        )

        self$results$clinicalSignificance$setContent(significance_text)
      }

      ,
      # Generate guided analysis steps ----
      .generateGuidedAnalysis = function() {
        # Check current state and provide guidance
        has_patient_id <- !is.null(self$options$patientID)
        has_response <- !is.null(self$options$responseVar)
        has_time <- !is.null(self$options$timeVar)
        input_type <- self$options$inputType

        guided_text <- paste0(
          "<div style='background-color: #f0fdf4; padding: 15px; border: 1px solid #16a34a; border-radius: 5px; margin: 10px 0;'>",
          "<h4 style='color: #15803d; margin-top: 0;'>", .("Guided Analysis"), "</h4>",

          "<div style='margin: 15px 0;'>",
          "<h5>", .("Step-by-Step Progress:"), "</h5>",
          "<ol style='margin-left: 20px;'>",

          # Step 1: Patient ID
          "<li style='margin: 5px 0;'>",
          if (has_patient_id) "[DONE]" else "[TODO]",
          " <strong>", .("Select Patient ID variable"), "</strong>",
          if (!has_patient_id) {
            paste0("<br><small style='color: #dc2626;'>", .("Required: Choose a variable that uniquely identifies each patient"), "</small>")
          } else {
            paste0("<br><small style='color: #16a34a;'>", .("Patient ID selected"), "</small>")
          },
          "</li>",

          # Step 2: Response Variable
          "<li style='margin: 5px 0;'>",
          if (has_response) "[DONE]" else "[TODO]",
          " <strong>", .("Select Response Variable"), "</strong>",
          if (!has_response) {
            paste0("<br><small style='color: #dc2626;'>", .("Required: Choose tumor measurements or percentage changes"), "</small>")
          } else {
            paste0("<br><small style='color: #16a34a;'>", .("Response variable selected"), "</small>")
          },
          "</li>",

          # Step 3: Input Type
          "<li style='margin: 5px 0;'>",
          "[INFO] <strong>", .("Choose Input Type"), "</strong>",
          "<br><small>",
          if (input_type == "percentage") {
            .("Percentage Changes selected - good for most analyses")
          } else {
            .("Raw Measurements selected - make sure you have a time variable")
          },
          "</small></li>",

          # Step 4: Time Variable (conditional)
          "<li style='margin: 5px 0;'>",
          if (input_type == "raw" || !is.null(self$options$timeVar)) {
            if (has_time) "[DONE]" else "[TODO]"
          } else "[OPTIONAL]",
          " <strong>", .("Time Variable (if needed)"), "</strong>",
          if (input_type == "raw" && !has_time) {
            paste0("<br><small style='color: #dc2626;'>", .("Required for raw measurements: Select time variable with baseline = 0"), "</small>")
          } else if (has_time) {
            paste0("<br><small style='color: #16a34a;'>", .("Time variable selected - enables spider plots"), "</small>")
          } else {
            paste0("<br><small style='color: #6b7280;'>", .("Optional for percentage data"), "</small>")
          },
          "</li>",

          # Step 5: Run Analysis
          "<li style='margin: 5px 0;'>",
          if (has_patient_id && has_response) "[READY]" else "[WAITING]",
          " <strong>", .("Run Analysis"), "</strong>",
          if (has_patient_id && has_response) {
            paste0("<br><small style='color: #16a34a;'>", .("Ready to run! Results will appear below."), "</small>")
          } else {
            paste0("<br><small style='color: #6b7280;'>", .("Complete required steps above"), "</small>")
          },
          "</li>",
          "</ol>",
          "</div>",

          "<div style='background-color: #dbeafe; padding: 10px; border-radius: 3px; margin: 10px 0;'>",
          "<h5 style='margin-top: 0;'>", .("Quick Tips & Preset Guide:"), "</h5>",
          "<ul style='margin: 5px 0; margin-left: 20px;'>",
          "<li>", .("Most studies use 'Percentage Changes' format"), "</li>",
          "<li>", .("Enable 'Show RECIST Thresholds' for clinical interpretation"), "</li>",
          "<li><strong>", .("Clinical Presets:"), "</strong></li>",
          "<ul style='margin-left: 15px;'>",
          "<li>", .("Phase II: Standard efficacy analysis with confidence intervals"), "</li>",
          "<li>", .("Phase I/II: Detailed monitoring with safety focus"), "</li>",
          "<li>", .("Biomarker: Group-based analysis (requires Group Variable)"), "</li>",
          "<li>", .("Publication: Clean, professional plots for manuscripts"), "</li>",
          "</ul>",
          "</ul>",

          # Add preset-specific guidance based on current selection
          # if (self$options$clinicalPreset != "custom") {
          #   paste0("<div style='background-color: #f0f9ff; padding: 8px; border-radius: 3px; margin: 5px 0;'>",
          #          "<strong>", .("Current Preset:"), " ", .("settings"),"</strong><br>",
          #          switch(self$options$clinicalPreset,
          #            "phase2" = .("Optimized for regulatory submissions with statistical rigor"),
          #            "phase1_2" = .("Configured for safety monitoring and exploratory analysis"),
          #            "biomarker" = .("Set up for group comparisons and biomarker correlations"),
          #            "publication" = .("Professional appearance suitable for peer-reviewed journals"),
          #            ""
          #          ),
          #          "</div>")
          # } else {
          #   ""
          # },

          "</div>",

          "</div>"
        )

        self$results$guidedAnalysis$setContent(guided_text)
      }

      ,
      # Helper functions for interpretation ----
      .interpretORR = function(orr) {
        if (orr >= 30) {
          return(.("Promising activity - warrants further investigation"))
        } else if (orr >= 15) {
          return(.("Moderate activity - may justify continued development"))
        } else {
          return(.("Limited activity - consider alternative approaches"))
        }
      }

      ,
      .interpretDCR = function(dcr) {
        if (dcr >= 70) {
          return(.("Excellent disease control"))
        } else if (dcr >= 50) {
          return(.("Good disease control"))
        } else {
          return(.("Limited disease control"))
        }
      }

    )
)
