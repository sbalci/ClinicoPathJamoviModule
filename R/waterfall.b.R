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


      .validateData = function(df, patientID, inputType, responseVar, timeVar = NULL) {
        validation_messages <- character()
        data_valid <- TRUE

        # Basic data validation
        if (is.null(df) || nrow(df) == 0) {
          validation_messages <- c(validation_messages, "<br>Error: No data provided or data is empty.")
          data_valid <- FALSE
          attr(df, "validation_messages") <- validation_messages
          attr(df, "data_valid") <- data_valid
          return(df)
        }

        # Check required columns exist
        required_columns <- c(patientID, responseVar)
        if (!is.null(timeVar)) {
          required_columns <- c(required_columns, timeVar)
        }
        
        missing_columns <- required_columns[!required_columns %in% names(df)]
        if (length(missing_columns) > 0) {
          validation_messages <- c(validation_messages, paste0(
            "<br>Error: Missing required columns: ", paste(missing_columns, collapse = ", "),
            "<br>Available columns: ", paste(names(df), collapse = ", ")
          ))
          data_valid <- FALSE
        }

        # Check minimum number of patients
        if (patientID %in% names(df)) {
          n_patients <- length(unique(df[[patientID]]))
          if (n_patients < 2) {
            validation_messages <- c(validation_messages, paste0(
              "<br>Warning: Only ", n_patients, " patient found. ",
              "Waterfall plots are more meaningful with multiple patients."
            ))
          }
        }

        # Check for missing response values
        if (responseVar %in% names(df)) {
          missing_responses <- sum(is.na(df[[responseVar]]))
          if (missing_responses > 0) {
            validation_messages <- c(validation_messages, paste0(
              "<br>Warning: ", missing_responses, " missing response values found. ",
              "These will be excluded from analysis."
            ))
          }
        }

        # For raw measurements validation
        if (inputType == "raw") {
          if (is.null(timeVar)) {
            validation_messages <- c(validation_messages, paste0(
              "<br>Time Variable Required for Raw Measurements:",
              "<br>When using raw tumor measurements, a time variable is essential to:",
              "<br>- Identify baseline measurements (time = 0)",
              "<br>- Calculate accurate percentage changes",
              "<br>- Track response progression over time",
              "<br><br>Recommended Data Format:",
              "<br>PatientID  Time  Measurement",
              "<br>PT1        0     50          (baseline)",
              "<br>PT1        2     25          (2 months)",
              "<br>PT1        4     10          (4 months)"
            ))
            data_valid <- FALSE
          } else {
            # Check time variable exists
            if (!timeVar %in% names(df)) {
              validation_messages <- c(validation_messages, sprintf(
                "<br>Time variable '%s' not found in the data. Please ensure the time variable is correctly specified.",
                timeVar
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
                  "<br>Missing Baseline Measurements:",
                  sprintf("<br>The following patients lack baseline (time = 0) measurements: %s",
                          paste(patients_without_baseline, collapse = ", ")),
                  "<br><br>Why this matters:",
                  "<br>- Baseline measurements are the reference point for calculating changes",
                  "<br>- Without baseline values, percentage changes cannot be calculated accurately",
                  "<br>- Please ensure each patient has a measurement at time = 0"
                ))
                data_valid <- FALSE
              }
            }
          }
        }

        # For percentage data, handle invalid shrinkage and large growth
        if (inputType == "percentage") {
          df[[responseVar]] <- jmvcore::toNumeric(df[[responseVar]])

          # Check for invalid shrinkage (< -100%)
          invalid_shrinkage <- df %>%
            dplyr::filter(.data[[responseVar]] < -100) %>%
            dplyr::select(!!patientID, !!responseVar)

          if (nrow(invalid_shrinkage) > 0) {
            validation_messages <- c(validation_messages, paste0(
              "<br>Invalid Tumor Shrinkage Values Detected:",
              "<br>Tumor shrinkage cannot exceed 100% (complete disappearance).",
              "<br>The following measurements will be capped at -100%:",
              paste(capture.output(print(invalid_shrinkage)), collapse = "<br>"),
              "<br><br>Please verify these measurements for data entry errors."
            ))
            # Cap shrinkage values at -100%
            df[[responseVar]] <- pmax(df[[responseVar]], -100)
          }

          # Check for unusually large growth (> 200%)
          large_growth <- df %>%
            dplyr::filter(.data[[responseVar]] > 200) %>%
            dplyr::select(!!patientID, !!responseVar)

          if (nrow(large_growth) > 0) {
            validation_messages <- c(validation_messages, paste0(
              "<br>Unusually Large Growth Values Detected:",
              "<br>The following measurements show >200% growth:",
              paste(capture.output(print(large_growth)), collapse = "<br>"),
              "<br><br>While such large increases are possible, please verify:",
              "<br>- Measurement accuracy",
              "<br>- Calculation methods",
              "<br>- Any additional clinical factors",
              "<br><br>These values will be included in the analysis but may affect scaling."
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
            sprintf("<br>Warning: Very small sample size (n=%d). Results may not be reliable.", unique_patients))
        } else if (unique_patients < 20) {
          validation_messages <- c(validation_messages,
            sprintf("<br>Note: Small sample size (n=%d). Interpret results with caution.", unique_patients))
        }

        # Add checks for unrealistic values if response data is available
        if (responseVar %in% names(df)) {
          response_values <- df[[responseVar]][!is.na(df[[responseVar]])]
          if (length(response_values) > 0) {
            if (inputType == "percentage") {
              # For percentage data, check for extreme values
              if (any(response_values > 500 | response_values < -100, na.rm = TRUE)) {
                validation_messages <- c(validation_messages,
                  "<br>Warning: Some percentage changes are outside typical range (-100% to +500%). Please verify data.")
              }
            } else {
              # For raw measurements, check for negative values or zero
              if (any(response_values <= 0, na.rm = TRUE)) {
                validation_messages <- c(validation_messages,
                  "<br>Warning: Some measurements are zero or negative. Please verify these values.")
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
                "<br>Warning: No baseline measurements (time=0) found. Percentage changes may be incorrect.")
            }
            # Check for negative time values
            if (any(time_values < 0, na.rm = TRUE)) {
              validation_messages <- c(validation_messages,
                "<br>Warning: Negative time values detected. Please verify time measurements.")
            }
          }
        }




        # Return modified dataframe with validation attributes
        return(df)
      },

      .generateGroupColors = function(group_levels, color_scheme) {
        # Generate colors for group-based coloring
        # @param group_levels: unique levels/groups to assign colors
        # @param color_scheme: "colorful", "jamovi", "classic", etc.
        # @return: named vector of colors
        
        n_groups <- length(group_levels)
        
        if (color_scheme == "colorful") {
          # Use rainbow colors for better distinction
          colors <- rainbow(n_groups)
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
          stop("Patient ID and response variables are required")
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
            stop("No data remaining after processing. Check baseline measurements and data format.")
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
        df_waterfall <- processed_df %>%
          dplyr::group_by(!!rlang::sym(patientID)) %>%
          dplyr::filter(!is.na(response)) %>%  # Filter out NA responses first
          dplyr::filter(response == min(response, na.rm = TRUE)) %>%
          dplyr::slice(1) %>%  # Take first row if there are ties
          dplyr::ungroup()
        
        # Validate waterfall data
        if (nrow(df_waterfall) == 0) {
          stop("No patients with valid response data found.")
        }
        
        df_waterfall <- df_waterfall %>%
          dplyr::mutate(
            category = factor(
              cut(response,
                  breaks = c(-Inf, -30, 20, Inf),
                  labels = c("Response", "Stable", "Progression"),
                  right = FALSE),
              levels = c("Response", "Stable", "Progression")
            ),
            # Also create detailed RECIST categories
            recist_category = factor(
              dplyr::case_when(
                response <= -100 ~ "CR",  # Complete Response 
                response <= -30 ~ "PR",   # Partial Response 
                response <= 20 ~ "SD",    # Stable Disease
                response > 20 ~ "PD",     # Progressive Disease
                TRUE ~ "Unknown"
              ),
              levels = c("CR", "PR", "SD", "PD", "Unknown")
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
        summary_table$percent <- summary_table$n / sum(summary_table$n) * 100

        ## Calculate ORR and DCR ----
        ORR <- round(sum(summary_table$n[summary_table$category %in% c("CR", "PR")]) /
                       sum(summary_table$n) * 100, 1)

        DCR <- round(sum(summary_table$n[summary_table$category %in% c("CR", "PR", "SD")]) /
                       sum(summary_table$n) * 100, 1)

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
            duration = max(time[response <= -30], na.rm=TRUE) -
              min(time[response <= -30], na.rm=TRUE),
            best_response = min(response, na.rm=TRUE)
          )
      }

      ,
      # Add time to response metric
      .calculateTimeToResponse = function(df) {
        df %>%
          dplyr::group_by(patientID) %>%
          dplyr::filter(response <= -30) %>%
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
            ORR = mean(response <= -30, na.rm=TRUE),
            DCR = mean(response <= 20, na.rm=TRUE),
            median_response = median(response, na.rm=TRUE)
          )
      }

      ,
      # Calculate person-time metrics for enhanced analysis
      .calculatePersonTimeMetrics = function(df, patientID, timeVar, responseVar) {
        # Requires time variable to calculate person-time
        if (is.null(timeVar)) {
          return(NULL)
        }

        # Ensure required columns exist and are properly formatted
        if (!all(c(patientID, timeVar) %in% names(df))) {
          warning("Required variables missing for person-time analysis")
          return(NULL)
        }

        # Convert time variable to numeric if needed
        df[[timeVar]] <- jmvcore::toNumeric(df[[timeVar]])

        # Debug: Check what columns are available
        # Determine which column to use for response calculations
        response_col <- NULL
        if ("percentage_change" %in% names(df)) {
          response_col <- "percentage_change"
        } else if ("response" %in% names(df)) {
          response_col <- "response" 
        } else {
          # For raw data, we can't do proper person-time analysis without percentage changes
          return(NULL)
        }

        # Calculate person-time metrics by patient
        pt_by_patient <- df %>%
          dplyr::group_by(!!rlang::sym(patientID)) %>%
          dplyr::summarise(
            # Calculate total follow-up time (max time - baseline)
            follow_up_time = max(.data[[timeVar]], na.rm = TRUE),
            # Calculate best response using the appropriate response column
            best_response = min(.data[[response_col]], na.rm = TRUE),
            # Determine response category
            response_cat = factor(
              cut(best_response,
                  breaks = c(-Inf, -100, -30, 20, Inf),
                  labels = c("CR", "PR", "SD", "PD"),
                  right = TRUE),
              levels = c("CR", "PR", "SD", "PD")
            ),
            # Calculate time to best response
            time_to_best = .data[[timeVar]][which.min(.data[[response_col]])],
            # Calculate time in response (for responders - PR or CR)
            time_in_response = ifelse(
              best_response <= -30,
              # For responders, calculate time from first response to last follow-up
              max(.data[[timeVar]][.data[[response_col]] <= -30], na.rm = TRUE) -
                min(.data[[timeVar]][.data[[response_col]] <= -30], na.rm = TRUE),
              0
            ),
            .groups = "drop"
          )

        # Calculate overall person-time metrics
        total_patients <- nrow(pt_by_patient)
        total_person_time <- sum(pt_by_patient$follow_up_time, na.rm = TRUE)
        total_response_time <- sum(pt_by_patient$time_in_response, na.rm = TRUE)

        # Calculate person-time by response category
        pt_by_category <- pt_by_patient %>%
          dplyr::group_by(response_cat, .drop = FALSE) %>%
          dplyr::summarise(
            patients = dplyr::n(),
            person_time = sum(follow_up_time, na.rm = TRUE),
            median_time_to_response = median(time_to_best, na.rm = TRUE),
            median_duration = median(time_in_response, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          dplyr::mutate(
            pct_patients = (patients / total_patients) * 100,
            pct_time = (person_time / total_person_time) * 100
          )

        # Calculate summary metrics
        summary_metrics <- list(
          total_patients = total_patients,
          total_person_time = total_person_time,
          total_response_time = total_response_time,
          response_rate_per_100 = (total_response_time / total_person_time) * 100
        )

        return(list(
          by_patient = pt_by_patient,
          by_category = pt_by_category,
          summary = summary_metrics
        ))
      }

      ,
      # run ----
      .run = function() {

        ## TODO text ----


        todo <- paste0(
          "<br>Welcome to ClinicoPath Treatment Response Analysis",
          "<br><br>",
          "
        <br><br>
        This tool creates two types of visualizations for tumor response data:
        <br><br>
        <b>1. Waterfall Plot</b>
        <br>- Shows best response for each patient
        <br>- Requires one measurement per patient (for single timepoint data)
        <br>- Shows percentage change from baseline
        <br><br>
        <b>2. Spider Plot</b>
        <br>- Shows response trajectories over time
        <br>- Requires multiple measurements per patient (one for each timepoint)
        <br>- Shows change over time
        <br><br>
        <b>Data Input Options:</b>
        <br>1. <b>Percentage Changes:</b>
        <br>- Values already calculated as percent change from baseline
        <br>- Negative values = decrease (improvement)
        <br>- Example: -30 means 30% decrease from baseline
        <br><br>
        2. <b>Raw Measurements:</b>
        <br>- Actual tumor measurements (e.g., mm, cm, sum of diameters)
        <br>- Tool will automatically calculate percent change from baseline
        <br>- Baseline is assumed to be first measurement (Time = 0)
        <br><br>
        <b>Required Variables:</b>
        <br>- <b>Patient ID:</b> Unique identifier for each patient
        <br>- <b>Response Value:</b> Either percentage change or raw measurements
        <br>- <b>Time Variable:</b> Required only for Spider Plot (e.g., months from baseline)
        <br><br>
        <b>RECIST Response Categories:</b>
        <br>- Complete Response (CR): -100% (complete disappearance)
        <br>- Partial Response (PR): ≥30% decrease
        <br>- Stable Disease (SD): Between -30% and +20% change
        <br>- Progressive Disease (PD): ≥20% increase
        <br><br>
        <b>Data Format Examples:</b>
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

        ## Validate inputs ----
        if (is.null(self$options$patientID) || is.null(self$options$responseVar)) {
         todo <- paste0(todo,
                        "<br><br>
                        To start analysis select <b>Patient ID</b> and <b>Response Value</b>"
         )

         self$results$todo$setContent(todo)

         return()

        }

        if (nrow(self$data) == 0) {
          todo <- paste0(todo,
                         "<br><br>
                         Data contains no complete rows. Check the data.")

          self$results$todo$setContent(todo)

          return()
        }

        self$results$todo2$setVisible(FALSE)
        self$results$todo2$setContent("")





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
        if (length(validation_messages) > 0) {
          # Display validation messages in an informative format
          message_text <- paste0(
            "Data Validation Results:",
            "<br>======================",
            paste(validation_messages, collapse = "<br>"),
            "<br><br>Please address these items to ensure accurate analysis.",
            sep = ""
          )

          self$results$todo2$setVisible(TRUE)
          self$results$todo2$setContent(validation_messages)
          self$results$todo$setVisible(FALSE)

          return()
        }

        ## Continue with analysis if data is valid ----
        if (attr(validated_data, "data_valid")) {

          self$results$todo$setVisible(FALSE)
          self$results$todo2$setVisible(FALSE)
          self$results$todo2$setContent("")

          # Process data and create visualizations
          private$.checkpoint()  # Checkpoint before data processing
          processed_data <- private$.processData(
            validated_data,
            self$options$patientID,
            self$options$inputType,
            self$options$responseVar,
            self$options$timeVar,
            self$options$groupVar
          )
        }



        ## Calculate metrics ----
        private$.checkpoint()  # Checkpoint before metrics calculation
        metrics <- private$.calculateMetrics(processed_data$waterfall)

        ## Update results tables ----
        private$.checkpoint()  # Checkpoint before summary table population
        for(i in seq_len(nrow(metrics$summary))) {
          self$results$summaryTable$addRow(rowKey = i, values = list(
            category = metrics$summary$category[i],
            n = metrics$summary$n[i],
            percent = paste0(round(metrics$summary$percent[i], digits = 1), "%")
          ))
        }



            self$results$summaryTable$addFootnote(
              rowNo = 1,
              col = "category",
              "Complete Response (CR): Complete disappearance of all target lesions."
            )

            self$results$summaryTable$addFootnote(
              rowNo = 2,
              col = "category",
              "Partial Response (PR): At least 30% decrease in sum of target lesions."
            )

            self$results$summaryTable$addFootnote(
              rowNo = 3,
              col = "category",
              "Stable Disease (SD): Neither PR nor PD criteria met."
            )

            self$results$summaryTable$addFootnote(
              rowNo = 4,
              col = "category",
              "Progressive Disease (PD): At least 20% increase in sum of target lesions."
            )


        self$results$clinicalMetrics$addRow(rowKey = 1, values = list(
          metric = "Objective Response Rate (CR+PR)",
          value = paste0(metrics$ORR, "%")
        ))

        self$results$clinicalMetrics$addRow(rowKey = 2, values = list(
          metric = "Disease Control Rate (CR+PR+SD)",
          value = paste0(metrics$DCR, "%")
        ))

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

        # Add person-time metrics to the results if available
        if (!is.null(person_time_metrics) && personTimeVisible) {
          # Add response duration metrics to clinical metrics table
          self$results$clinicalMetrics$addRow(rowKey = 3, values = list(
            metric = "Median Time to Response",
            value = sprintf("%.1f %s",
                            person_time_metrics$by_category$median_time_to_response[
                              person_time_metrics$by_category$response_cat %in% c("CR", "PR")
                            ][1],
                            "time units")
          ))

          self$results$clinicalMetrics$addRow(rowKey = 4, values = list(
            metric = "Median Duration of Response",
            value = sprintf("%.1f %s",
                            median(person_time_metrics$by_patient$time_in_response[
                              person_time_metrics$by_patient$time_in_response > 0
                            ], na.rm = TRUE),
                            "time units")
          ))

          self$results$clinicalMetrics$addRow(rowKey = 5, values = list(
            metric = "Response Rate per 100 Person-Time Units",
            value = sprintf("%.2f", person_time_metrics$summary$response_rate_per_100)
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

        # mydataview ----

        # self$results$mydataview$setContent(
        #   list(
        #     "data" = processed_data,
        #     "data_waterfall" = processed_data$waterfall,
        #     "data_spider" = processed_data$spider,
        #     options = list(
        #       "patientID" = self$options$patientID,
        #       "response" = self$options$responseVar,
        #       "timeVar" = self$options$timeVar,
        #       "sortBy" = self$options$sortBy,
        #       "showThresholds" = self$options$showThresholds,
        #       "labelOutliers" = self$options$labelOutliers,
        #       "colorScheme" = self$options$colorScheme,
        #       "barWidth" = self$options$barWidth,
        #       "barAlpha" = self$options$barAlpha,
        #       "showMedian" = self$options$showMedian,
        #       "showCI" = self$options$showCI,
        #       "minResponseForLabel" = self$options$minResponseForLabel
        #     ),
        #     "metrics" = metrics
        #   )
        # )


        ## Add response category to data ----

        if (is.null(self$options$timeVar) && self$options$addResponseCategory && self$results$addResponseCategory$isNotFilled()) {
          df <- processed_data$waterfall
          self$results$addResponseCategory$setRowNums(rownames(df))
          self$results$addResponseCategory$setValues(df$recist_category)
          }

        if (!is.null(self$options$timeVar) && self$options$addResponseCategory && self$results$addResponseCategory$isNotFilled()) {
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



        ## Prepare plot data ----
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
            "minResponseForLabel" = self$options$minResponseForLabel
          ),
          "metrics" = metrics
        )

        ### Update plots ----
        private$.checkpoint()  # Checkpoint before plot generation

        #### Waterfall plot ----
        if (self$options$showWaterfallPlot) {
          imageWaterfall <- self$results$waterfallplot
          imageWaterfall$setState(plotData)
        }


        #### Spider plot ----
        if (self$options$showSpiderPlot && !is.null(self$options$timeVar)) {
          plotData$timeVar <- self$options$timeVar
          # Add spider-specific color options
          plotData$options$spiderColorBy <- self$options$spiderColorBy
          plotData$options$spiderColorScheme <- self$options$spiderColorScheme
          imagespider <- self$results$spiderplot
          imagespider$setState(plotData)
        }

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

        # Sort data
        if (plotData$options$sortBy == "response") {
          df <- df[order(df$response, na.last = TRUE),]
        } else {
          df <- df[order(df[[plotData$options$patientID]], na.last = TRUE),]
        }

        # Define color schemes
        recistColors <- c(
          "CR" = "#0000FF",
          "PR" = "#4169E1",
          "SD" = "#FFA500",
          "PD" = "#FF0000",
          "NA" = "#808080"
        )

        simpleColors <- c(
          "CR" = "#00AA00",
          "PR" = "#00AA00",
          "SD" = "#808080",
          "PD" = "#FF0000",
          "NA" = "#808080"
        )

        # Check if group-based coloring is requested and group variable exists
        useGroupColoring <- !is.null(plotData$options$colorBy) && 
                           plotData$options$colorBy == "group" && 
                           "patient_group" %in% names(df)

        if (useGroupColoring) {
          # Generate distinct colors for groups using reusable method
          group_levels <- unique(df$patient_group)
          colors <- private$.generateGroupColors(group_levels, plotData$options$colorScheme)
          fill_var <- "patient_group"
          legend_name <- "Patient Group"
        } else {
          # Use RECIST coloring
          colors <- if (plotData$options$colorScheme == "simple")
            simpleColors else recistColors
          fill_var <- "recist_category"
          legend_name <- "RECIST Response"
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
            x = "Patients",
            y = "Change in Tumor Size (%)"
          )

        # Add RECIST thresholds
        if (plotData$options$showThresholds) {
          p <- p +
            ggplot2::geom_hline(
              yintercept = c(-30, 20),
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
            "Spider Plot Requirements and Guidelines",
            "\n\n",
            "This visualization requires two key elements:",
            "\n",
            "1. A time variable to show response trajectories",
            "\n",
            "2. The 'Show Spider Plot' option to be enabled",
            "\n\n",
            "Understanding Spider Plots:",
            "\n",
            "A spider plot helps visualize how each patient's response changes over time. \n",
            "Each line represents one patient's treatment journey, making it easy to see \n",
            "patterns in response and identify different types of outcomes.",
            "\n\n",
            "To Generate the Plot:",
            "\n",
            "- Add a time variable (such as months from baseline)",
            "\n",
            "- Enable 'Show Spider Plot' in the options panel",
            "\n\n",
            "The resulting visualization will help you track response patterns \n",
            "and compare outcomes across different patients over time.\n\n",
            sep = ""
          )

          text_warning <- paste0(text_warning,
                                "Time Variable Requirement:",
                                "\n\nA time variable is required to create visualizations when using raw measurements.",
                                "\n\nWhy is this important?",
                                "\n- Baseline identification: Marks the starting point (time = 0)",
                                "\n- Response calculation: Computes changes from baseline",
                                "\n- Progression tracking: Shows how response changes over time",
                                "\n\nHow to proceed:",
                                "\n1. Add a time variable to your data",
                                "\n2. Time should start at 0 (baseline)",
                                "\n3. Use consistent time units (e.g., months or weeks)",
                                "\n\nExample time variable format:",
                                "\nPatientID.         Time          Measurement",
                                "\nPT1                0             50              (baseline)",
                                "\nPT1                2             25              (2 months)",
                                "\nPT1                4             10              (4 months)",
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
          warning("Spider plot data is empty or null")
          return()
        }

        # Validate required variables exist
        required_vars <- c(options$timeVar, options$patientID)
        missing_vars <- required_vars[!required_vars %in% names(df)]
        if (length(missing_vars) > 0) {
          warning(sprintf("Missing required variables: %s",
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
          warning("Required columns 'time' or 'response' not found in spider plot data")
          return()
        }

        # Sort data by patient and time
        df <- df[order(df[[options$patientID]], df$time), ]
        
        # Determine coloring method (backward compatible, defaults to response)
        spiderColorBy <- options$spiderColorBy %||% "response"
        spiderColorScheme <- options$spiderColorScheme %||% "classic"
        useGroupColoring <- spiderColorBy == "group" && "patient_group" %in% names(df)
        
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
              name = "Patient Group",
              values = line_colors,
              na.value = "#808080"
            ) +
            ggplot2::scale_fill_manual(
              name = "Patient Group",
              values = point_colors,
              na.value = "#808080"
            )
        } else {
          # Response-based coloring (default for backward compatibility)
          responder_colors <- if (spiderColorScheme == "classic") {
            c("FALSE" = "#CC0000", "TRUE" = "#0066CC")
          } else if (spiderColorScheme == "jamovi") {
            c("FALSE" = "#E64B35", "TRUE" = "#4DBBD5")
          } else {
            c("FALSE" = "#FF6B6B", "TRUE" = "#4ECDC4")
          }
          
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
                fill = response <= -30
              ),
              size = 3,
              shape = 21,
              color = "black"
            ) +
            # Define colors for response categories
            ggplot2::scale_fill_manual(
              name = "Response",
              values = responder_colors,
              labels = c("Non-responder", "Responder")
            )
        }
        
        # Add common plot elements
        p <- p +
          # Add RECIST threshold lines
          ggplot2::geom_hline(
            yintercept = c(-30, 20),
            linetype = "dashed",
            color = "gray50",
            alpha = 0.5
          ) +
          # Add labels
          ggplot2::labs(
            x = paste("Time", "(", options$timeVar, ")"),
            y = "Change in Tumor Size (%)",
            title = "Spider Plot of Tumor Response"
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
              y = c(-30, 20),
              label = c("PR threshold (-30%)", "PD threshold (+20%)"),
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
          warning(sprintf("Error creating spider plot: %s", e$message))
          FALSE
        })
      }









    )
)
