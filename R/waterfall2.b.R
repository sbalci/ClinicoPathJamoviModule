#' @title Treatment Response Analysis
#' @description Creates waterfall and spider plots to visualize tumor response data following RECIST criteria
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import dplyr
#' @importFrom magrittr %>%
#' @import ggplot2
#' @import scales
#' @param data Data frame containing response data
#' @param patientID Column name for patient identifiers
#' @param response Column name for response values (raw measurements or percent change)
#' @param timeVar Optional column name for time points
#' @param inputType Whether values are raw measurements or pre-calculated percentages
#' @return A list containing plot object and summary statistics
#' @examples
#' data <- data.frame(
#'   PatientID = paste0("PT", 1:10),
#'   Response = c(-100, -45, -30, -20, -10, 0, 10, 20, 30, 40),
#'   Time = c(1,2,3,4,5,6,7,8,9,10)
#' )
#' waterfall(data, "PatientID", "Response", "Time")


#' @export waterfallClass
#'


waterfall2Class <- if (requireNamespace('jmvcore')) R6::R6Class(
    "waterfall2Class",
    inherit = waterfall2Base,
    private = list(


      .validateData = function(df, patientID, inputType, responseVar, timeVar = NULL) {
        validation_messages <- character()
        data_valid <- TRUE

        # For raw measurements validation
        if (inputType == "raw") {
          if (is.null(timeVar)) {
            validation_messages <- c(validation_messages, paste(
              "\nTime Variable Required for Raw Measurements:",
              "\nWhen using raw tumor measurements, a time variable is essential to:",
              "\n• Identify baseline measurements (time = 0)",
              "\n• Calculate accurate percentage changes",
              "\n• Track response progression over time",
              "\n\nRecommended Data Format:",
              "\nPatientID  Time  Measurement",
              "\nPT1        0     50          (baseline)",
              "\nPT1        2     25          (2 months)",
              "\nPT1        4     10          (4 months)"
            ))
            data_valid <- FALSE
          } else {
            # Check time variable exists
            if (!timeVar %in% names(df)) {
              validation_messages <- c(validation_messages, sprintf(
                "\nTime variable '%s' not found in the data. Please ensure the time variable is correctly specified.",
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
                validation_messages <- c(validation_messages, paste(
                  "\nMissing Baseline Measurements:",
                  sprintf("\nThe following patients lack baseline (time = 0) measurements: %s",
                          paste(patients_without_baseline, collapse = ", ")),
                  "\n\nWhy this matters:",
                  "\n• Baseline measurements are the reference point for calculating changes",
                  "\n• Without baseline values, percentage changes cannot be calculated accurately",
                  "\n• Please ensure each patient has a measurement at time = 0"
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
            validation_messages <- c(validation_messages, paste(
              "\nInvalid Tumor Shrinkage Values Detected:",
              "\nTumor shrinkage cannot exceed 100% (complete disappearance).",
              "\nThe following measurements will be capped at -100%:",
              paste(capture.output(print(invalid_shrinkage)), collapse = "\n"),
              "\n\nPlease verify these measurements for data entry errors."
            ))
            # Cap shrinkage values at -100%
            df[[responseVar]] <- pmax(df[[responseVar]], -100)
          }

          # Check for unusually large growth (> 200%)
          large_growth <- df %>%
            dplyr::filter(.data[[responseVar]] > 200) %>%
            dplyr::select(!!patientID, !!responseVar)

          if (nrow(large_growth) > 0) {
            validation_messages <- c(validation_messages, paste(
              "\nUnusually Large Growth Values Detected:",
              "\nThe following measurements show >200% growth:",
              paste(capture.output(print(large_growth)), collapse = "\n"),
              "\n\nWhile such large increases are possible, please verify:",
              "\n• Measurement accuracy",
              "\n• Calculation methods",
              "\n• Any additional clinical factors",
              "\n\nThese values will be included in the analysis but may affect scaling."
            ))
          }
        }

        # Set attributes for validation results
        attr(df, "validation_messages") <- validation_messages
        attr(df, "data_valid") <- data_valid

        # Return modified dataframe with validation attributes
        return(df)
      }


      ,
      # process validated data ----
      .processData = function(df, patientID, inputType, responseVar, timeVar = NULL) {
        # Validate input parameters first
        if (is.null(patientID) || is.null(responseVar)) {
          stop("Patient ID and response variables are required")
        }

        # For raw measurements, calculate percentage change from baseline
        if (inputType == "raw") {
          processed_df <- df %>%
            dplyr::group_by(!!rlang::sym(patientID)) %>%
            dplyr::arrange(!!rlang::sym(patientID))

          # Add time variable if present
          if (!is.null(timeVar)) {
            processed_df <- processed_df %>%
              dplyr::arrange(!!rlang::sym(timeVar))
          }

          processed_df <- processed_df %>%
            dplyr::mutate(
              baseline = dplyr::first(!!rlang::sym(responseVar)),
              response = ((!!rlang::sym(responseVar) - baseline) / baseline) * 100
            ) %>%
            dplyr::ungroup()
        } else {
          # Data is already in percentage format
          processed_df <- df %>%
            dplyr::mutate(
              response = !!rlang::sym(responseVar)
            )
        }

        # Calculate RECIST categories
        df_waterfall <- processed_df %>%
          dplyr::group_by(!!rlang::sym(patientID)) %>%
          dplyr::filter(abs(response) == max(abs(response), na.rm = TRUE)) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(
            category = factor(
              cut(response,
                  breaks = c(-Inf, -100, -30, 20, Inf),
                  labels = c("CR", "PR", "SD", "PD"),
                  right = TRUE),
              levels = c("CR", "PR", "SD", "PD", "NA")
            )
          )

        # Prepare spider plot data
        df_spider <- processed_df

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

        private$.checkpoint()

        ## Calculate response rates ----
        cats <- c("CR", "PR", "SD", "PD")

        summary_table <- data.frame(
          category = cats,
          n = sapply(cats, function(x) sum(df$category == x, na.rm = TRUE)),
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

        # Calculate person-time metrics by patient
        pt_by_patient <- df %>%
          dplyr::group_by(!!rlang::sym(patientID)) %>%
          dplyr::summarise(
            # Calculate total follow-up time (max time - baseline)
            follow_up_time = max(!!rlang::sym(timeVar), na.rm = TRUE),
            # Calculate best response
            best_response = min(response, na.rm = TRUE),
            # Determine response category
            response_cat = factor(
              cut(best_response,
                  breaks = c(-Inf, -100, -30, 20, Inf),
                  labels = c("CR", "PR", "SD", "PD"),
                  right = TRUE),
              levels = c("CR", "PR", "SD", "PD")
            ),
            # Calculate time to best response
            time_to_best = !!rlang::sym(timeVar)[which.min(response)],
            # Calculate time in response (for responders - PR or CR)
            time_in_response = ifelse(
              best_response <= -30,
              # For responders, calculate time from first response to last follow-up
              max(!!rlang::sym(timeVar)[response <= -30], na.rm = TRUE) -
                min(!!rlang::sym(timeVar)[response <= -30], na.rm = TRUE),
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
          dplyr::group_by(response_cat) %>%
          dplyr::summarise(
            patients = n(),
            person_time = sum(follow_up_time, na.rm = TRUE),
            pct_patients = patients / total_patients * 100,
            pct_time = person_time / total_person_time * 100,
            median_time_to_response = median(time_to_best[!is.infinite(time_to_best)], na.rm = TRUE),
            median_duration = median(time_in_response[time_in_response > 0], na.rm = TRUE),
            .groups = "drop"
          )

        # Calculate response rates per 100 person-time units
        response_rate <- sum(pt_by_patient$response_cat %in% c("CR", "PR")) / total_person_time * 100

        # Return comprehensive results
        return(list(
          by_patient = pt_by_patient,
          by_category = pt_by_category,
          summary = list(
            total_patients = total_patients,
            total_person_time = total_person_time,
            total_response_time = total_response_time,
            response_rate_per_100 = response_rate,
            pct_time_in_response = total_response_time / total_person_time * 100
          )
        ))
      }

      ,
      # run ----
      .run = function() {

        tryCatch({

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
        <b>3. Person-Time Analysis</b>
        <br>- Evaluates response duration and rates over time
        <br>- Requires time variable and multiple measurements
        <br>- Calculates metrics like response per 100 person-months
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

          processed_data <- private$.processData(
            validated_data,
            self$options$patientID,
            self$options$inputType,
            self$options$responseVar,
            self$options$timeVar
          )
        }



        ## Calculate metrics ----
        metrics <- private$.calculateMetrics(processed_data$waterfall)


        ## Calculate person-time metrics (if time variable available) ----
        if (!is.null(self$options$timeVar)) {
          person_time_metrics <- private$.calculatePersonTimeMetrics(
            processed_data$spider,
            self$options$patientID,
            self$options$timeVar,
            self$options$responseVar
          )

          # Add person-time metrics to the results if available
          if (!is.null(person_time_metrics)) {
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
              for (i in seq_len(nrow(person_time_metrics$by_category))) {
                self$results$personTimeTable$addRow(rowKey = i, values = list(
                  category = person_time_metrics$by_category$response_cat[i],
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
        }



        ## Update results tables ----
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
        if (self$options$addResponseCategory && self$results$addResponseCategory$isNotFilled()) {

          # Get waterfall data (contains best response per patient)
          df_waterfall <- processed_data$waterfall

          # Validate that we have the required data
          if (is.null(df_waterfall) || nrow(df_waterfall) == 0) {
            warning("No waterfall data available for response categorization")
            return()
          }

          # Extract unique patient-category pairs
          patient_categories <- df_waterfall %>%
            dplyr::select(!!rlang::sym(self$options$patientID), category) %>%
            dplyr::distinct()

          # Create a lookup table for joining
          join_by <- stats::setNames(self$options$patientID, self$options$patientID)

          # Join with original data to maintain all rows
          df_with_categories <- self$data %>%
            dplyr::left_join(patient_categories, by = join_by)

          # Handle cases where patients might not have categories (shouldn't happen, but defensive)
          if (any(is.na(df_with_categories$category))) {
            # Fill missing categories with "Not Evaluable"
            df_with_categories$category[is.na(df_with_categories$category)] <- "NE"
            warning("Some patients missing response categories - filled with 'NE' (Not Evaluable)")
          }

          # Convert factor to character for better handling
          category_values <- as.character(df_with_categories$category)

          # Set the response category output
          self$results$addResponseCategory$setRowNums(seq_len(nrow(df_with_categories)))
          self$results$addResponseCategory$setValues(category_values)

          # Set descriptive information
          description <- paste0(
            "RECIST v1.1 response categories: ",
            "CR (Complete Response ≤-100%), ",
            "PR (Partial Response -30% to -99%), ",
            "SD (Stable Disease -29% to +19%), ",
            "PD (Progressive Disease ≥+20%)"
          )

          # Add footnote or description if the method exists
          if (exists("setDescription", where = self$results$addResponseCategory)) {
            self$results$addResponseCategory$setDescription(description)
          }
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
            "barWidth" = self$options$barWidth,
            "barAlpha" = self$options$barAlpha,
            "showMedian" = self$options$showMedian,
            "showCI" = self$options$showCI,
            "minResponseForLabel" = self$options$minResponseForLabel
          ),
          "metrics" = metrics
        )

        # Add person-time metrics to plot data if available
        if (exists("person_time_metrics") && !is.null(person_time_metrics)) {
          plotData$person_time_metrics <- person_time_metrics
        }

        ### Update plots ----

        #### Waterfall plot ----
        if (self$options$showWaterfallPlot) {
          imageWaterfall <- self$results$waterfallplot
          imageWaterfall$setState(plotData)
        }


        #### Spider plot ----
        if (self$options$showSpiderPlot && !is.null(self$options$timeVar)) {
          plotData$timeVar <- self$options$timeVar
          imagespider <- self$results$spiderplot
          imagespider$setState(plotData)
        }

        }, error = function(e) {
          msg <- sprintf("Error in analysis: %s", e$message)
          self$results$todo$setContent(msg)
          return()
        }, warning = function(w) {
          msg <- sprintf("Warning in analysis: %s", w$message)
          self$results$todo2$setContent(msg)
        })
      }


      ,
      # Waterfall plot ----
      .waterfallplot = function(imageWaterfall, ggtheme, theme, ...) {
        if (!self$options$showWaterfallPlot) return()

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

        colors <- if (plotData$options$colorScheme == "simple")
          simpleColors else recistColors

        # Create base plot
        p <- ggplot2::ggplot(df, ggplot2::aes(
          x = factor(seq_len(nrow(df))),
          y = response
        )) +
          ggplot2::geom_bar(
            stat = "identity",
            ggplot2::aes(fill = category),
            width = plotData$options$barWidth,
            alpha = plotData$options$barAlpha
          ) +
          ggplot2::scale_fill_manual(
            name = "RECIST Response",
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



        # Add confidence bands around median line
        if (plotData$options$showCI && nrow(df) >= 10) {
          ci <- t.test(df$response)$conf.int
          med <- median(df$response, na.rm = TRUE)
          p <- p +
            ggplot2::geom_ribbon(
              ggplot2::aes(
                x = c(1, nrow(df)),
                ymin = ci[1],
                ymax = ci[2]
              ),
              alpha = 0.2,
              fill = "gray",
              inherit.aes = FALSE
            )
        }

        # Add optional patient annotations (controlled by labelOutliers option)
        if (plotData$options$labelOutliers) {
          threshold <- plotData$options$minResponseForLabel
          # Add patient ID labels for responses exceeding threshold
          df$patient_label <- ifelse(
            !is.na(df$response) & abs(df$response) > threshold,
            as.character(df[[plotData$options$patientID]]),
            ""
          )
          
          if (any(df$patient_label != "")) {
            p <- p +
              ggplot2::geom_text(
                data = df[df$patient_label != "",],
                ggplot2::aes(
                  x = factor(which(df$patient_label != "")),
                  y = response,
                  label = patient_label
                ),
                vjust = ifelse(df$response[df$patient_label != ""] >= 0, -0.5, 1.5),
                size = 2.5,
                angle = 45
              )
          }
        }

        # Finalize plot aesthetics
        p <- p +
          ggplot2::theme(
            axis.text.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.minor.x = ggplot2::element_blank(),
            legend.position = "right"
          )




        # Add waterfall plot faceting by subgroup
        if (!is.null(plotData$options$subgroupVar)) {
          p <- p + ggplot2::facet_wrap(~subgroup)
        }



        print(p)
        TRUE
      }




      ,
      # spider plot ----
      .spiderplot = function(imagespider, ggtheme, theme, ...) {


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
        df$response <- jmvcore::toNumeric(df$response)  # response should already be calculated

        # Remove any rows with NA values
        df <- df[complete.cases(df[c("time", "response")]), ]

        # Sort data by patient and time
        df <- df[order(df[[options$patientID]], df$time), ]

        # Create the spider plot
        p <- ggplot2::ggplot(df) +
          # Add lines connecting points for each patient
          ggplot2::geom_line(
            mapping = ggplot2::aes(
              x = time,                              # Use processed time variable
              y = response,                          # Use processed response
              group = .data[[options$patientID]],   # Group by patient ID
              color = response <= -30                # Color by response threshold
            ),
            size = 1
          ) +
          # Add points at each measurement
          ggplot2::geom_point(
            mapping = ggplot2::aes(
              x = time,
              y = response,
              color = response <= -30
            ),
            size = 2
          ) +
          # Define colors for response categories
          ggplot2::scale_color_manual(
            name = "Response",
            values = c("FALSE" = "#CC0000", "TRUE" = "#0066CC"),
            labels = c("Non-responder", "Responder")
          ) +
          # Add RECIST threshold lines
          ggplot2::geom_hline(
            yintercept = c(-30, 20),
            linetype = "dashed",
            color = "gray50",
            alpha = 0.5
          ) +
          # Add labels
          ggplot2::labs(
            x = paste("Time", "(", options$timeVar, ")"),  # Include time variable name
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
