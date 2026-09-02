#' @title Treatment Response Analysis (Waterfall) Class
#'
#' @description R6 class for performing treatment response analysis using waterfall plots.
#' @name waterfallClass
#' @importFrom R6 R6Class
#' @return An \code{R6} class generator object for the \code{waterfallClass} backend; used internally by the jamovi analysis wrapper and not called directly.
waterfallClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "waterfallClass",
    inherit = waterfallBase,
    private = list(

        # RECIST v1.1 Constants ----
        RECIST_CR_THRESHOLD = -100,  # Complete Response threshold (\u{2264}-100%)
        RECIST_PR_THRESHOLD = -30,   # Partial Response threshold (\u{2264}-30%)
        RECIST_PD_THRESHOLD = 20,    # Progressive Disease threshold (\u{2265}+20%, inclusive)
        RECIST_SD_MIN = -30,         # Stable Disease minimum (-30%)
        RECIST_SD_MAX = 20,          # Stable Disease maximum (20%)

        # Get validation messages for the basic-data / column checks. The
        # per-scenario messages that used to live here are built inline (with
        # sprintf + .()) at their single call sites in .validateData.
        .getValidationMessages = function() {
          list(
            no_data = .("Error: No data provided or data is empty."),
            missing_columns = .("Error: Missing required columns:"),
            available_columns = .("Available columns:")
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
          # Only the five named structural entities survive jamovi's renderer and
          # Word/PDF export; numeric references (&#x27;) and escaped slashes do not.
          text <- gsub("'", "&apos;", text, fixed = TRUE)
          return(text)
        },

        # Initialize notice collection list
        .noticeList = list(),

        # Add a notice to the collection
        .addNotice = function(type, title, content) {
          private$.noticeList[[length(private$.noticeList) + 1]] <- list(
            type = type,
            title = title,
            content = content
          )
        },

        # Render collected notices as HTML
        .renderNotices = function() {
          if (length(private$.noticeList) == 0) {
            # Clear a panel left over from a previous run (e.g. the welcome path).
            self$results$notices$setContent("")
            return()
          }

          # Map notice types to colors and icons. Backgrounds are translucent
          # rgba tints (house theme-safe pattern) so they composite over either
          # jamovi theme; title colors are saturated enough to read on both.
          typeStyles <- list(
            ERROR = list(color = "#dc2626", bgcolor = "rgba(220, 38, 38, 0.10)", border = "#fca5a5", icon = ""),
            STRONG_WARNING = list(color = "#ea580c", bgcolor = "rgba(234, 88, 12, 0.10)", border = "#fdba74", icon = ""),
            WARNING = list(color = "#ca8a04", bgcolor = "rgba(202, 138, 4, 0.12)", border = "#fde047", icon = ""),
            INFO = list(color = "#2563eb", bgcolor = "rgba(37, 99, 235, 0.08)", border = "#93c5fd", icon = "")
          )

          html <- "<div style='margin: 10px 0;'>"

          for (notice in private$.noticeList) {
            style <- typeStyles[[notice$type]] %||% typeStyles$INFO

            html <- paste0(html,
              "<div style='background-color: ", style$bgcolor, "; ",
              "border-left: 4px solid ", style$border, "; ",
              "padding: 12px; margin: 8px 0; border-radius: 4px;'>",
              "<strong style='color: ", style$color, ";'>",
              style$icon, " ", private$.safeHtmlOutput(notice$title), "</strong><br>",
              "<span style='color: inherit;'>", private$.safeHtmlOutput(notice$content), "</span>",
              "</div>"
            )
          }

          html <- paste0(html, "</div>")

          self$results$notices$setContent(html)
        },

        # Calculate statistical power for response rates
        .progressionTimes = function(times, values, after_time) {
          ok <- !is.na(times) & !is.na(values)
          times <- times[ok]
          values <- values[ok]
          if (length(times) == 0) return(numeric(0))

          ord <- order(times)
          times <- times[ord]
          values <- values[ord]

          burden <- 100 + values
          nadir_burden <- cummin(burden)
          # Guard the degenerate case of a nadir at complete disappearance.
          rel_increase <- ifelse(nadir_burden > 0,
                                 (burden - nadir_burden) / nadir_burden * 100,
                                 NA_real_)

          times[!is.na(rel_increase) &
                  rel_increase >= private$RECIST_PD_THRESHOLD &
                  times > after_time]
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
                # Time to first response (PR or better: <=-30%)
                time_to_first_response = ifelse(
                  any(.data[[responseVar]] <= private$RECIST_PR_THRESHOLD, na.rm = TRUE),
                  min(.data[[timeVar]][.data[[responseVar]] <= private$RECIST_PR_THRESHOLD], na.rm = TRUE),
                  NA_real_
                ),
                # Duration of response (time from first response to progression/end)
                #
                # Progression is referenced to the NADIR -- the smallest percent
                # change recorded so far -- not to baseline. RECIST v1.1 defines PD
                # as ">=20% increase taking as reference the smallest sum on study".
                # Testing `response > +20` against BASELINE instead means a patient
                # who shrinks and then regrows is never recorded as progressing
                # while their tumour is still smaller than at enrolment: a patient
                # going 100 -> 60 -> 78 mm is +30% over their nadir (RECIST
                # progression) yet sits at -22% from baseline, so they were counted
                # as censored and their duration of response ran to last follow-up.
                # That inflates every duration-of-response summary and the KM curve.
                duration_of_response = ifelse(
                  any(.data[[responseVar]] <= private$RECIST_PR_THRESHOLD, na.rm = TRUE),
                  {
                    first_response_time <- min(.data[[timeVar]][.data[[responseVar]] <= private$RECIST_PR_THRESHOLD], na.rm = TRUE)
                    progression_times <- private$.progressionTimes(
                      .data[[timeVar]], .data[[responseVar]], first_response_time)
                    if (length(progression_times) > 0) {
                      min(progression_times) - first_response_time  # Event observed
                    } else {
                      max(.data[[timeVar]]) - first_response_time  # Censored at last follow-up
                    }
                  },
                  NA_real_
                ),
                # ADDED: Censoring indicator (1 = event/progression observed, 0 = censored)
                duration_censored = ifelse(
                  any(.data[[responseVar]] <= private$RECIST_PR_THRESHOLD, na.rm = TRUE),
                  {
                    first_response_time <- min(.data[[timeVar]][.data[[responseVar]] <= private$RECIST_PR_THRESHOLD], na.rm = TRUE)
                    progression_times <- private$.progressionTimes(
                      .data[[timeVar]], .data[[responseVar]], first_response_time)
                    ifelse(length(progression_times) > 0, 1, 0)  # 1=event, 0=censored
                  },
                  NA_real_
                ),
                # Best response achieved. A patient whose every assessment is NA
                # (a baseline row plus missing measurements passes validation)
                # made min() return Inf and which.min() return integer(0);
                # summarise() then failed with "must return size 1" and the
                # whole TTR/DoR table vanished for the entire cohort.
                best_response = {
                  v <- .data[[responseVar]]
                  if (all(is.na(v))) NA_real_ else min(v, na.rm = TRUE)
                },
                # Time to best response
                time_to_best_response = {
                  valid_idx <- which(!is.na(.data[[responseVar]]))
                  if (length(valid_idx) == 0) NA_real_
                  else .data[[timeVar]][valid_idx[which.min(.data[[responseVar]][valid_idx])]]
                },
                .groups = "drop"
              ) %>%
              dplyr::filter(!is.na(time_to_first_response) | !is.na(duration_of_response))

            # Kaplan-Meier median duration of response (censoring-aware). The naive median
            # of duration_of_response ignores responders still in response at last
            # follow-up (duration_censored == 0) and so understates DoR.
            km_median_dor <- NA_real_
            n_dor_events <- NA_integer_
            dor_ok <- !is.na(metrics$duration_of_response) & !is.na(metrics$duration_censored)
            if (sum(dor_ok) >= 2 && requireNamespace("survival", quietly = TRUE)) {
              km_dor <- tryCatch({
                fit <- survival::survfit(
                  survival::Surv(metrics$duration_of_response[dor_ok],
                                 metrics$duration_censored[dor_ok]) ~ 1)
                unname(summary(fit)$table["median"])
              }, error = function(e) NA_real_)
              km_median_dor <- km_dor
              n_dor_events <- sum(metrics$duration_censored[dor_ok] == 1)
            }

            # Summary statistics
            summary_stats <- list(
              median_time_to_response = median(metrics$time_to_first_response, na.rm = TRUE),
              median_duration_of_response = median(metrics$duration_of_response, na.rm = TRUE),
              km_median_duration_of_response = km_median_dor,
              n_duration_events = n_dor_events,
              median_time_to_best_response = median(metrics$time_to_best_response, na.rm = TRUE),
              n_responders = sum(!is.na(metrics$time_to_first_response)),
              n_with_duration_data = sum(!is.na(metrics$duration_of_response))
            )

            list(
              by_patient = metrics,
              summary = summary_stats
            )
          }, error = function(e) {
            # Never fail silently: without this the TTR/DoR rows and the
            # response-duration table simply vanish with no explanation.
            private$.addNotice(
              type = "WARNING",
              title = .("TIME-TO-EVENT CALCULATION FAILED"),
              content = sprintf(
                .("Time to response and duration of response could not be computed (%s). The related rows and the Time-to-Response & Duration of Response table are omitted."),
                conditionMessage(e))
            )
            NULL
          })
        },

        # Check if dataset is large and needs optimization
        .shouldOptimizeForLargeDataset = function(df) {
          nrow(df) > 100 || length(unique(df[[1]])) > 50  # Assuming first column might be patient ID
        },

        # --- Issue #1 enhancements: baseline line + annotation markers ---

        # Vectorized isTRUE (NA -> FALSE)
        .isTrueVec = function(x) !is.na(x) & x,

        # Coerce an arbitrary vector to a logical "ongoing / on-treatment" flag
        .coerceOngoing = function(x) {
          if (is.logical(x)) return(ifelse(is.na(x), FALSE, x))
          if (is.numeric(x)) return(!is.na(x) & x != 0)
          xs <- tolower(trimws(as.character(x)))
          !is.na(xs) & xs %in% c("yes", "y", "true", "on", "ongoing", "1")
        },

        # Attach optional per-patient annotation columns to the waterfall data.
        # Matches by patient-ID VALUE, so it is robust to escaped column names.
        .attachAnnotations = function(wdf, source_df, pidCol, confVar, ongVar) {
          if (is.null(source_df) || is.null(pidCol) || !(pidCol %in% names(wdf)))
            return(wdf)
          src_pid_name <- self$options$patientID
          if (is.null(src_pid_name) || !(src_pid_name %in% names(source_df)))
            return(wdf)
          idx <- match(wdf[[pidCol]], source_df[[src_pid_name]])
          if (!is.null(confVar) && confVar %in% names(source_df))
            wdf$confirm_status <- as.character(source_df[[confVar]])[idx]
          if (!is.null(ongVar) && ongVar %in% names(source_df))
            wdf$ongoing_flag <- private$.coerceOngoing(source_df[[ongVar]][idx])
          wdf
        },

        # Override computed RECIST category with a user-supplied category variable.
        # Matches by patient-ID VALUE; only rows with a supplied value are changed.
        # Expected values: CR / PR / SD / PD (case-insensitive).
        .applyCategoryOverride = function(wdf, source_df, pidCol, categoryVar) {
          if (is.null(categoryVar) || is.null(source_df) ||
              !(categoryVar %in% names(source_df)) || !(pidCol %in% names(wdf)) ||
              !("recist_category" %in% names(wdf)))
            return(wdf)
          src_pid_name <- self$options$patientID
          if (is.null(src_pid_name) || !(src_pid_name %in% names(source_df)))
            return(wdf)
          idx <- match(wdf[[pidCol]], source_df[[src_pid_name]])
          user_cat <- toupper(trimws(as.character(source_df[[categoryVar]])[idx]))

          # recist_category is a factor with levels CR/PR/SD/PD/Unknown. Assigning
          # a label outside that set silently produced NA (with an "invalid factor
          # level" warning), and if EVERY row was overridden the whole column went
          # NA, after which downstream `if (orr > ...)` tests aborted the run with
          # "missing value where TRUE/FALSE needed". Accept only known labels and
          # say which ones were rejected.
          valid <- c("CR", "PR", "SD", "PD", "Unknown")
          recognised <- toupper(valid)
          ok <- !is.na(user_cat) & user_cat != "" & user_cat %in% recognised

          rejected <- unique(user_cat[!is.na(user_cat) & user_cat != "" &
                                        !(user_cat %in% recognised)])
          if (length(rejected) > 0) {
            private$.addNotice(
              type = "WARNING",
              title = .("RESPONSE CATEGORY OVERRIDE IGNORED"),
              content = sprintf(
                .("The response category override contained %d unrecognised label(s): %s. Only CR, PR, SD, PD and Unknown are accepted. Those patients keep their computed category; no patient was dropped."),
                length(rejected), paste(rejected, collapse = ", "))
            )
          }

          if (any(ok)) {
            # Match back to the canonical capitalisation of the factor levels.
            wdf$recist_category[ok] <- valid[match(user_cat[ok], recognised)]
          }
          wdf
        },

        # Annotation tracks drawn beneath the waterfall bars.
        #
        # Design credit: the idea of pairing the waterfall with aligned covariate
        # tiles underneath, combined via patchwork with a collected x axis, is taken
        # from the Jamovi-TrialPlots module by highwindmx (LGPL):
        #   https://github.com/highwindmx/Jamovi-TrialPlots
        # This is an independent implementation against our own data pipeline; only
        # the figure design is borrowed. LGPL is compatible with this package's GPL-2.
        #
        # `df` must already be in final bar order: the tiles use the same
        # factor(seq_len(nrow(df))) x positions, which is what keeps the two panels
        # aligned no matter how the user sorted the bars.
        .annotationTrack = function(df, plotData) {
            vars <- plotData$options$annotationVars
            if (is.null(vars) || length(vars) == 0) return(NULL)
            if (!requireNamespace("patchwork", quietly = TRUE)) return(NULL)

            pid <- plotData$options$patientID
            if (is.null(pid) || !pid %in% names(df)) return(NULL)

            src <- self$data
            vars <- vars[vars %in% names(src)]
            if (length(vars) == 0) return(NULL)

            # One row per patient, taken from the source data by ID.
            idx <- match(as.character(df[[pid]]), as.character(src[[pid]]))

            long <- do.call(rbind, lapply(vars, function(v) {
                data.frame(
                    bar   = seq_len(nrow(df)),
                    track = v,
                    value = as.character(src[[v]])[idx],
                    stringsAsFactors = FALSE
                )
            }))
            if (nrow(long) == 0 || all(is.na(long$value))) return(NULL)

            # Keep the user's variable order, top to bottom.
            long$track <- factor(long$track, levels = rev(vars))
            long$bar <- factor(long$bar, levels = seq_len(nrow(df)))

            ggplot2::ggplot(long, ggplot2::aes(x = .data$bar, y = .data$track,
                                               fill = .data$value)) +
                ggplot2::geom_tile(width = 0.9, height = 0.9, colour = "white",
                                   linewidth = 0.2) +
                ggplot2::scale_x_discrete(drop = FALSE) +
                ggplot2::labs(x = NULL, y = NULL, fill = NULL) +
                ggplot2::theme_minimal() +
                ggplot2::theme(
                    axis.text.x     = ggplot2::element_blank(),
                    axis.ticks.x    = ggplot2::element_blank(),
                    panel.grid      = ggplot2::element_blank(),
                    legend.position = "bottom"
                )
        },

        # Add a Y = 0 baseline reference line
        .addBaseline = function(plot, show_baseline) {
          if (isTRUE(show_baseline)) {
            plot + ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.5)
          } else {
            plot
          }
        },

        # Draw per-bar annotation markers: confirmation shapes + ongoing arrows.
        # x-positions match the bar index used in the base plot (seq_len(nrow(df))).
        .addAnnotationMarkers = function(plot, df, plotData) {
          n <- nrow(df)
          # Confirmation: a point at each bar tip, shape mapped by level
          if (!is.null(plotData$options$confirmationVar) && "confirm_status" %in% names(df)) {
            keep <- !is.na(df$confirm_status)
            if (any(keep)) {
              marker_df <- data.frame(
                xpos = which(keep),
                ypos = df$response[keep],
                confirm_status = df$confirm_status[keep],
                stringsAsFactors = FALSE
              )
              nlev <- length(unique(marker_df$confirm_status))
              # Guard against confirmation variables with many levels: a manual
              # scale must supply at least one shape per level or ggplot errors
              # ("Insufficient values in manual scale"). Recycle a pool of distinct
              # shapes to exactly nlev values; warn when levels exceed the pool.
              shape_pool <- c(16, 1, 17, 2, 15, 0)
              if (nlev > length(shape_pool)) {
                private$.addNotice("INFO", .("Marker shapes reused"), sprintf(
                  .("Confirmation variable has %d levels; markers reuse shapes beyond %d distinct symbols."),
                  nlev, length(shape_pool)))
              }
              shape_values <- rep(shape_pool, length.out = max(1, nlev))
              plot <- plot +
                ggplot2::geom_point(
                  data = marker_df,
                  mapping = ggplot2::aes(
                    x = factor(xpos, levels = seq_len(n)),
                    y = ypos,
                    shape = confirm_status
                  ),
                  size = 2.5, colour = "black", inherit.aes = FALSE
                ) +
                ggplot2::scale_shape_manual(
                  name = .("Confirmation"),
                  values = shape_values
                )
            }
          }
          # Ongoing: an arrow drawn outward from each ongoing bar tip
          if (!is.null(plotData$options$ongoingVar) && "ongoing_flag" %in% names(df)) {
            on_idx <- which(private$.isTrueVec(df$ongoing_flag))
            if (length(on_idx) > 0) {
              on_df <- data.frame(
                xpos = on_idx,
                ystart = df$response[on_idx],
                stringsAsFactors = FALSE
              )
              on_df$yend <- on_df$ystart + ifelse(on_df$ystart >= 0, 8, -8)
              plot <- plot +
                ggplot2::geom_segment(
                  data = on_df,
                  mapping = ggplot2::aes(
                    x = factor(xpos, levels = seq_len(n)),
                    xend = factor(xpos, levels = seq_len(n)),
                    y = ystart, yend = yend
                  ),
                  arrow = ggplot2::arrow(length = ggplot2::unit(0.15, "cm"), type = "closed"),
                  colour = "black", linewidth = 0.5, inherit.aes = FALSE
                )
            }
          }
          plot
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
            # Fall back to standard processing; tell the user rather than
            # leaving an R warning nobody sees.
            private$.addNotice("WARNING", .("Large-dataset optimisation unavailable"), sprintf(
              .("The optimised processing path failed (%s); the standard path was used instead, so results are unaffected."),
              e$message))
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

          # Create waterfall data (best response per patient).
          # Drop all-NA patients first so an empty group does not become
          # min(numeric(0)) = Inf (which .categorizeRECIST mis-labels as PD).
          waterfall_data <- processed_df %>%
            dplyr::filter(!is.na(response)) %>%
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

          # Spider data needs patient_group too, or "Spider Plot Color By:
          # Patient Groups" silently downgrades to responder coloring on the
          # large-dataset path while working on the standard path.
          if (!is.null(groupVar) && groupVar %in% names(processed_df))
            processed_df$patient_group <- factor(processed_df[[groupVar]])

          return(list(waterfall = waterfall_data, spider = processed_df))
        },

        # Efficient processing for percentage data
        .processPercentageDataEfficient = function(df, patientID, responseVar, timeVar, groupVar) {
          # Direct processing without copying
          processed_df <- df
          processed_df$response <- processed_df[[responseVar]]

          # Create waterfall data efficiently
          if (!is.null(timeVar) && timeVar %in% names(df)) {
            # For time-series percentage data, get best response per patient.
            # Drop all-NA patients first so an empty group does not become
            # min(numeric(0)) = Inf (which .categorizeRECIST mis-labels as PD).
            waterfall_data <- processed_df %>%
              dplyr::filter(!is.na(response)) %>%
              dplyr::group_by(!!rlang::sym(patientID)) %>%
              dplyr::summarise(
                response = min(response, na.rm = TRUE),
                .groups = "drop"
              )
            spider_data <- processed_df
          } else {
            # Percentage data with no time variable. This must still collapse to
            # one row per patient: without the reduction, a patient contributing
            # several assessment rows was counted once per ROW, so ORR/DCR were
            # computed over measurements rather than patients. Because this path
            # is only reached above the 100-row / 50-patient threshold, the same
            # dataset produced different rates on either side of that boundary
            # (verified: 30 patients x 3 rows -> ORR 100%; 60 patients x 3 rows
            # -> ORR 33.3%). The NA filter also matches the sibling branches, so
            # unevaluable rows no longer inflate the denominator.
            waterfall_data <- processed_df %>%
              dplyr::filter(!is.na(response)) %>%
              dplyr::group_by(!!rlang::sym(patientID)) %>%
              dplyr::summarise(
                response = min(response, na.rm = TRUE),
                .groups = "drop"
              )
            spider_data <- NULL
          }

          # Add group information efficiently if available
          if (!is.null(groupVar) && groupVar %in% names(df)) {
            waterfall_data$patient_group <- df[[groupVar]][match(waterfall_data[[patientID]], df[[patientID]])]
            if (!is.null(spider_data) && groupVar %in% names(spider_data))
              spider_data$patient_group <- factor(spider_data[[groupVar]])
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
            # Drop all-NA patients first so an empty group does not become
            # min(numeric(0)) = Inf (which .categorizeRECIST mis-labels as PD).
            waterfall_data <- processed_df %>%
              dplyr::filter(!is.na(response)) %>%
              dplyr::group_by(!!rlang::sym(patientID)) %>%
              dplyr::summarise(response = min(response, na.rm = TRUE), .groups = "drop")
            spider_data <- processed_df
          } else {
            # Collapse to one row per patient here too, so rates are computed over
            # patients rather than assessment rows regardless of which processing
            # path a dataset happens to take.
            waterfall_data <- processed_df %>%
              dplyr::filter(!is.na(response)) %>%
              dplyr::group_by(!!rlang::sym(patientID)) %>%
              dplyr::summarise(response = min(response, na.rm = TRUE), .groups = "drop")
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
            if (!is.null(spider_data) && groupVar %in% names(spider_data))
              spider_data$patient_group <- factor(spider_data[[groupVar]])
          }

          # Add RECIST categories
          waterfall_data$recist_category <- private$.categorizeRECIST(waterfall_data$response)

          return(list(waterfall = waterfall_data, spider = spider_data))
        },

        # Categorize responses into RECIST-style categories (CR/PR/SD/PD).
        # Shared by the large-dataset processing paths; mirrors the case_when
        # in .processData so both paths yield identical factors. Missing this
        # method previously caused "attempt to apply non-function" for any
        # dataset large enough to enter the optimized path (>100 rows or
        # >50 unique patients), e.g. the bundled histopathology example.
            # Enforce the two physical limits on tumour measurements, at the single
            # point where every processing path converges.
            #
            # 1. A tumour cannot shrink by more than 100%: -100% IS complete
            #    disappearance. The Response Value option text has always promised
            #    "values will be automatically capped at -100% for analysis" and the
            #    code never did it, so a data-entry slip of -150% flowed into the
            #    plot and the median unchanged.
            # 2. A raw measurement cannot be negative. A negative baseline flips the
            #    sign of ((current - baseline) / baseline), so a GROWING tumour is
            #    reported as a response. That is silent and inverted, so those
            #    patients are made unevaluable rather than guessed at.
            .enforceMeasurementLimits = function(waterfall_data, source_df, patientID,
                                                 responseVar, inputType) {
                if (is.null(waterfall_data) || !is.data.frame(waterfall_data) ||
                    nrow(waterfall_data) == 0 || !"response" %in% names(waterfall_data)) {
                    return(waterfall_data)
                }

                # --- negative raw measurements ------------------------------------
                if (identical(inputType, "raw") && !is.null(source_df) &&
                    !is.null(responseVar) && responseVar %in% names(source_df) &&
                    !is.null(patientID) && patientID %in% names(source_df) &&
                    patientID %in% names(waterfall_data)) {

                    vals <- jmvcore::toNumeric(source_df[[responseVar]])
                    bad <- !is.na(vals) & vals < 0
                    if (any(bad)) {
                        bad_ids <- unique(as.character(source_df[[patientID]][bad]))
                        idx <- which(as.character(waterfall_data[[patientID]]) %in% bad_ids)
                        if (length(idx) > 0) {
                            waterfall_data$response[idx] <- NA_real_
                            waterfall_data$recist_category <-
                                private$.categorizeRECIST(waterfall_data$response)
                        }
                        private$.addNotice(
                            "ERROR", .("NEGATIVE TUMOUR MEASUREMENTS"),
                            sprintf(
                                .("%d patient(s) have a negative raw measurement, which is not a possible tumour size: %s. A negative baseline inverts the sign of the percent change, so a growing tumour would be reported as a response. These patients are reported as \"Unknown\" rather than guessed at. Check the measurement column for data-entry errors."),
                                length(bad_ids),
                                paste(utils::head(bad_ids, 10), collapse = ", "))
                        )
                    }
                }

                # --- shrinkage beyond -100% ---------------------------------------
                # Read the SOURCE values, not the processed ones: .validateData
                # already caps at -100, so by this point there is nothing left to
                # detect. It records the capping in the validation panel, but that
                # panel is cleared and hidden whenever validation otherwise passes
                # (see "Clear todo messages for successful validation"), so the user
                # was never actually told. Reporting it here puts it in the
                # always-visible notices panel, which is what the option text
                # promising the cap implies.
                # self$data is the untouched dataset; source_df has already been
                # through .validateData, which caps at -100 before we ever see it.
                raw_df <- self$data
                src_vals <- if (!is.null(raw_df) && !is.null(responseVar) &&
                                identical(inputType, "percentage") &&
                                responseVar %in% names(raw_df))
                    jmvcore::toNumeric(raw_df[[responseVar]]) else numeric(0)
                src_bad <- !is.na(src_vals) & src_vals < -100

                too_small <- !is.na(waterfall_data$response) & waterfall_data$response < -100
                if (any(too_small) || any(src_bad)) {
                    ids <- if (any(src_bad) && !is.null(patientID) &&
                               patientID %in% names(raw_df))
                        unique(as.character(raw_df[[patientID]][src_bad])) else
                        as.character(waterfall_data[[patientID]][too_small])
                    worst <- min(c(src_vals[src_bad], waterfall_data$response[too_small]),
                                 na.rm = TRUE)
                    waterfall_data$response[too_small] <- -100
                    waterfall_data$recist_category <-
                        private$.categorizeRECIST(waterfall_data$response)
                    private$.addNotice(
                        "WARNING", .("IMPOSSIBLE SHRINKAGE CAPPED"),
                        sprintf(
                            .("%d patient(s) had a change below -100%%, which is not physically possible: -100%% already means the tumour has disappeared completely. The most extreme was %.1f%%. These values were capped at -100%% (complete response) for the analysis and the plot: %s. Check the response column for data-entry errors."),
                            length(ids), worst,
                            paste(utils::head(ids, 10), collapse = ", "))
                    )
                }

                waterfall_data
            },

        # Reconcile the patients that entered the analysis against those that made
        # it into the waterfall, and mark response-unevaluable patients as such.
        #
        # Two situations previously passed silently:
        #  1. A patient whose baseline is missing or zero yields response = NA and
        #     is filtered out. The cohort simply got smaller with no explanation.
        #  2. With a time variable, a patient having only a baseline scan produced
        #     ((baseline - baseline) / baseline) * 100 = 0, i.e. a 0% change, and
        #     was categorised SD. A patient with no post-baseline assessment is not
        #     response-evaluable and certainly not stable disease; counting them as
        #     SD inflates the disease control rate.
        .accountForUnevaluablePatients = function(waterfall_data, source_df,
                                                  patientID, timeVar) {
          if (is.null(waterfall_data) || !is.data.frame(waterfall_data) ||
              is.null(patientID) || is.null(source_df) ||
              !patientID %in% names(source_df)) {
            return(waterfall_data)
          }

          all_ids <- unique(source_df[[patientID]])
          all_ids <- all_ids[!is.na(all_ids)]
          # An empty frame still needs explaining, so handle it before requiring
          # the patient column to be present.
          kept_ids <- if (nrow(waterfall_data) > 0 &&
                          patientID %in% names(waterfall_data))
            unique(waterfall_data[[patientID]]) else character(0)
          dropped <- setdiff(all_ids, kept_ids)

          # A cohort where nothing is evaluable previously produced an empty
          # analysis with no explanation at all.
          if (length(kept_ids) == 0) {
            private$.addNotice(
              type = "ERROR",
              title = .("NO EVALUABLE PATIENTS"),
              content = sprintf(
                .("None of the %d patients supplied could be evaluated for response. Every response value was missing, non-numeric, or lacked a usable baseline, so no rates, plots or categories can be produced. Check that the response variable holds numeric values and, for raw measurements, that each patient has a time = 0 baseline."),
                length(all_ids))
            )
            return(waterfall_data)
          }

          if (length(dropped) > 0) {
            private$.addNotice(
              type = "WARNING",
              title = .("PATIENTS EXCLUDED"),
              content = sprintf(
                .("%d of %d patients were excluded from the response analysis because a usable baseline could not be established (baseline missing, zero, or non-numeric). Excluded: %s. All rates below are computed over the %d remaining patients, so they are NOT intention-to-treat."),
                length(dropped), length(all_ids),
                paste(utils::head(as.character(dropped), 10), collapse = ", "),
                length(kept_ids))
            )
          }

          # Patients with a baseline but no post-baseline assessment.
          if (!is.null(timeVar) && timeVar %in% names(source_df)) {
            tv <- jmvcore::toNumeric(source_df[[timeVar]])
            post <- stats::aggregate(
              list(n_post = tv), by = list(pid = source_df[[patientID]]),
              FUN = function(x) sum(!is.na(x) & x > 0))
            no_post <- post$pid[post$n_post == 0]
            idx <- which(waterfall_data[[patientID]] %in% no_post)
            if (length(idx) > 0) {
              waterfall_data$response[idx] <- NA_real_
              waterfall_data$recist_category <- private$.categorizeRECIST(
                waterfall_data$response)
              private$.addNotice(
                type = "WARNING",
                title = .("NOT RESPONSE-EVALUABLE"),
                content = sprintf(
                  .("%d patient(s) have a baseline measurement but no post-baseline assessment and are therefore not response-evaluable: %s. They are reported as \"Unknown\" rather than as stable disease, so they do not inflate the disease control rate."),
                  length(idx),
                  paste(utils::head(as.character(waterfall_data[[patientID]][idx]), 10),
                        collapse = ", "))
              )
            }
          }

          # (Small-cohort messaging lives in .processAndAnalyzeData, which runs
          # after the demotions above and counts evaluable patients - a second
          # notice here duplicated it with a different denominator.)

          waterfall_data
        },

        # Single source of truth for threshold-based response categories. Every
        # other code path routes here so the three former copies cannot drift.
        #
        # Boundary conventions follow RECIST v1.1 wording, which is inclusive on
        # BOTH sides:
        #   PR "at least a 30% decrease" -> exactly -30 is PR
        #   PD "at least a 20% increase" -> exactly +20 is PD
        # The PD boundary was previously exclusive (> 20), so a change of exactly
        # +20% was reported as SD. That is reachable whenever percentages are
        # pre-rounded, which is common when inputType = "percentage".
        .categorizeRECIST = function(response) {
          factor(
            dplyr::case_when(
              is.na(response) ~ "Unknown",
              response <= private$RECIST_CR_THRESHOLD ~ "CR",
              response > private$RECIST_CR_THRESHOLD &
                response <= private$RECIST_PR_THRESHOLD ~ "PR",
              response > private$RECIST_PR_THRESHOLD &
                response < private$RECIST_PD_THRESHOLD ~ "SD",
              response >= private$RECIST_PD_THRESHOLD ~ "PD",
              TRUE ~ "Unknown"
            ),
            levels = c("CR", "PR", "SD", "PD", "Unknown")
          )
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
              "<br>", msgs$missing_columns, " ", paste(htmltools::htmlEscape(missing_columns), collapse = ", "),
              "<br>", msgs$available_columns, " ", paste(htmltools::htmlEscape(names(df)), collapse = ", ")
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
              "<br>",
              sprintf(.("Warning: Only %d patient found. Waterfall plots are more meaningful with multiple patients."),
                      n_patients)
            ))
          }
        }

        # Check for missing response values
        if (responseVar %in% names(df)) {
          missing_responses <- sum(is.na(df[[responseVar]]))
          if (missing_responses > 0) {
            validation_messages <- c(validation_messages, paste0(
              "<br>",
              sprintf(.("Warning: %d missing response values found. These will be excluded from analysis."),
                      missing_responses)
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
              "<br>", .("PT1        0     50          (baseline)"),
              "<br>", .("PT1        2     25          (2 months)"),
              "<br>", .("PT1        4     10          (4 months)")
            ))
            data_valid <- FALSE
          } else {
            # Check time variable exists
            if (!timeVar %in% names(df)) {
              validation_messages <- c(validation_messages, paste0(
                "<br>",
                sprintf(.("Time variable '%s' not found in the data. Please ensure the time variable is correctly specified."),
                        htmltools::htmlEscape(timeVar))
              ))
              data_valid <- FALSE
            } else {
              # Convert and validate time values
              df[[timeVar]] <- jmvcore::toNumeric(df[[timeVar]])
              # A patient with MORE than one time = 0 row corrupts the analysis:
              # the baseline left-join duplicates every one of that patient's
              # visit rows (one copy per baseline value) with conflicting percent
              # changes. That is silent data corruption, so it blocks the run.
              dup_baseline <- df %>%
                dplyr::group_by(.data[[patientID]]) %>%
                dplyr::summarise(
                  n_baseline = sum(!is.na(.data[[timeVar]]) & .data[[timeVar]] == 0),
                  .groups = "drop"
                ) %>%
                dplyr::filter(n_baseline > 1) %>%
                dplyr::pull(!!patientID)
              if (length(dup_baseline) > 0) {
                validation_messages <- c(validation_messages, paste0(
                  "<br>",
                  sprintf(.("Error: %d patient(s) have more than one baseline (time = 0) row: %s. Each patient must have exactly one baseline; duplicate baselines multiply that patient's visit rows in the join and produce conflicting percent changes. Keep a single time = 0 measurement per patient."),
                          length(dup_baseline),
                          paste(htmltools::htmlEscape(as.character(utils::head(dup_baseline, 10))), collapse = ", "))
                ))
                data_valid <- FALSE
              }
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
                # Escape user-supplied patient IDs before HTML interpolation
                safe_missing_baseline_ids <- paste(
                  htmltools::htmlEscape(as.character(patients_without_baseline)),
                  collapse = ", "
                )
                validation_messages <- c(validation_messages, paste0(
                  "<br>", .("Missing Baseline Measurements:"),
                  sprintf("<br>%s %s",
                          .("The following patients lack baseline (time = 0) measurements:"),
                          safe_missing_baseline_ids),
                  "<br><br>", .("Why this matters:"),
                  "<br>- ", .("Baseline measurements are the reference point for calculating changes"),
                  "<br>- ", .("Without baseline values, percentage changes cannot be calculated accurately"),
                  "<br><br>", .("Recommended actions:"),
                  "<br>1. ", .("Add time=0 measurements for each patient, OR"),
                  "<br>2. ", .("Switch to 'Percentage Changes' input format if changes are pre-calculated, OR"),
                  "<br>3. ", .("Remove patients without baseline from analysis"),
                  "<br><br>", .("Example data format with baseline:"),
                  "<br>PatientID  Time  Measurement",
                  "<br>", .("PT1        0     50          (baseline required)"),
                  "<br>", .("PT1        2     25          (follow-up)")
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
            # Escape the printed data-frame rows (patient IDs + response values)
            # before HTML interpolation. `capture.output(print(df))` is plain text
            # but is being concatenated into HTML below, so any `<`/`>`/`&` in a
            # patient ID would render as markup.
            safe_invalid_shrinkage <- paste(
              htmltools::htmlEscape(capture.output(print(invalid_shrinkage))),
              collapse = "<br>"
            )
            validation_messages <- c(validation_messages, paste0(
              "<br>", .("Invalid Tumor Shrinkage Values Detected:"),
              "<br>", .("Tumor shrinkage cannot exceed 100% (complete disappearance)."),
              sprintf("<br>%s %d%%:", .("The following measurements will be capped at"), private$RECIST_CR_THRESHOLD),
              safe_invalid_shrinkage,
              "<br><br>", .("Recommended actions:"),
              "<br>1. ", .("Verify data entry for calculation errors"),
              "<br>2. ", .("Check if baseline measurements are correct"),
              "<br>3. ", .("Confirm percentage calculation method: ((current - baseline) / baseline) \u{00d7} 100"),
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
          # Escape printed data-frame rows before HTML interpolation
          safe_large_growth <- paste(
              htmltools::htmlEscape(capture.output(print(large_growth))),
              collapse = "<br>"
          )
          validation_messages <- c(validation_messages, paste0(
              "<br>", .("Unusually Large Growth Values Detected:"),
              "<br>", .("The following measurements show >200% growth:"),
              safe_large_growth,
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
        validated_df <- df # Assign df to validated_df to make the return syntactically correct
        return(validated_df)
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
        
        # Calculate SIMPLIFIED response categories (threshold-based, NOT full RECIST v1.1)
        # Best response = most negative percent change (minimum value for tumor shrinkage)
        # Optimized for large datasets
        n_patients <- length(unique(processed_df[[patientID]]))
        
        df_waterfall <- processed_df %>%
          dplyr::filter(!is.na(response)) %>%
          dplyr::group_by(!!rlang::sym(patientID)) %>%
          dplyr::slice_min(response, with_ties = FALSE, n = 1) %>%
          dplyr::ungroup()
        
        # Validate waterfall data
        if (nrow(df_waterfall) == 0) {
          return(list(
            error = TRUE,
            message = .("No patients with valid response data found.")
          ))
        }
        
        df_waterfall <- df_waterfall %>%
          dplyr::mutate(
            # Create simplified response categories (threshold-based, NOT RECIST v1.1 compliant)
            # NOTE: Variable name "recist_category" retained for backward compatibility
            # but represents SIMPLIFIED categories (no confirmation, no new lesions, no non-target)
            #
            # Delegates to .categorizeRECIST so this path cannot drift from the
            # other callers. The previous inline copy also declared its levels as
            # c(..., .("Unknown")) while case_when emitted the untranslated
            # "Unknown", so under any non-English locale every unevaluable
            # patient silently became NA instead of "Unknown".
            recist_category = private$.categorizeRECIST(response)
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
          DCR = DCR,
          n = total_n,
          # patients in the waterfall but not in any evaluable category
          n_unknown = sum(is.na(df$recist_category) | df$recist_category == "Unknown")
        ))

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

        # Delegates to .categorizeRECIST rather than repeating the thresholds, so
        # the person-time table cannot disagree with the summary table. NA maps to
        # "Unknown", which is absent from the factor levels applied below and so
        # becomes NA exactly as the previous local helper did.
        classify_response <- function(value) {
          as.character(private$.categorizeRECIST(value))
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
        if (isTRUE(self$options$enableGuidedMode)) {
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
          .("This tool creates waterfall and spider plots from ONE tumour burden value per patient (or per visit). Response categories use percent-change thresholds adapted from RECIST v1.1, but this is not a RECIST v1.1 implementation: it never sees individual lesions, so it cannot sum target lesions, detect new lesions, or judge non-target progression. If your data list each lesion separately, use the lesion-level RECIST v1.1 analysis. It will be available in upcoming releases."),
          "<br><br>",
          "<b> ", .("Visualization Types:"), "</b>",
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
          "<b> ", .("Data Input Options:"), "</b>",
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
          "<b> ", .("RECIST v1.1 Categories:"), "</b>",
          # Complete sentences per .() (no gluing single words), and the PD
          # boundary matches the classifier: exactly +20% IS PD (inclusive).
          "<br>- ", sprintf(.("Complete Response (CR): a change of %d%% (complete disappearance)"), private$RECIST_CR_THRESHOLD),
          "<br>- ", sprintf(.("Partial Response (PR): a decrease of %d%% or more"), private$RECIST_PR_THRESHOLD),
          "<br>- ", sprintf(.("Stable Disease (SD): a change between %d%% and +%d%%"), private$RECIST_PR_THRESHOLD, private$RECIST_PD_THRESHOLD),
          "<br>- ", sprintf(.("Progressive Disease (PD): an increase of +%d%% or more"), private$RECIST_PD_THRESHOLD),
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
        todo <- if (!isTRUE(self$options$enableGuidedMode)) {
          private$.generateWelcomeText()
        } else {
          ""
        }

        # Check if data is loaded
        if (is.null(self$data) || nrow(self$data) == 0) {
          if (!isTRUE(self$options$enableGuidedMode)) {
            self$results$todo$setContent(todo)
          }
          return(FALSE)
        }

        ## Validate required inputs ----
        if (is.null(self$options$patientID) || is.null(self$options$responseVar)) {
          private$.addNotice("ERROR", .("Variables required"),
            .("Select a Patient ID and a Response Value variable to run the analysis."))
          if (!isTRUE(self$options$enableGuidedMode)) {
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
          if (!isTRUE(self$options$enableGuidedMode)) {
            todo <- paste0(todo,
                           paste0("<br><br>",
                           .("Data contains no complete rows. Check the data.")))
            self$results$todo$setContent(todo)
          }
          return(FALSE)
        }

        # Clear welcome messages if validation passed
        if (!isTRUE(self$options$enableGuidedMode)) {
          self$results$todo2$setVisible(FALSE)
          self$results$todo2$setContent("")
        }

        return(TRUE)
      },


      # Process and analyze data ----
      .processAndAnalyzeData = function() {
        ## Validate data ----
        private$.checkpoint()  # Checkpoint        # Validate data structure and content

        # Use RAW variable names as data-frame keys. jamovi delivers self$data with
        # the ORIGINAL variable names, so make.names()-escaping (e.g. "Patient ID"
        # -> "Patient.ID") breaks column lookups and grouping. No R formula is
        # constructed anywhere here, so escaping provides no benefit.
        safe_patientID <- self$options$patientID
        safe_responseVar <- self$options$responseVar
        safe_timeVar <- self$options$timeVar
        safe_groupVar <- self$options$groupVar

        validated_data <- private$.validateData(
          self$data,
          safe_patientID,
          self$options$inputType,
          safe_responseVar,
          safe_timeVar
        )
        # Update self$data with validated version (includes type conversions)
        # self$data <- validated_data


        ### Check for validation messages ----
        validation_messages <- attr(validated_data, "validation_messages")
        data_valid_flag <- isTRUE(attr(validated_data, "data_valid"))

        # FATAL validation: keep the rich HTML guidance in todo2 (it survives
        # because the reject below aborts before anything hides it again).
        if (length(validation_messages) > 0 && !data_valid_flag &&
            !isTRUE(self$options$enableGuidedMode)) {
          self$results$todo2$setVisible(TRUE)
          self$results$todo2$setContent(paste(validation_messages, collapse = ""))
          self$results$todo$setVisible(FALSE)
        }

        ## Continue with analysis if data is valid; abort only on critical errors ----
        if (!data_valid_flag) {
          plain_message <- .("Data validation failed.")
          if (length(validation_messages) > 0) {
            plain_message <- gsub("<[^>]+>", "", paste(validation_messages, collapse = " "))
            plain_message <- trimws(plain_message)
            if (plain_message == "") {
              plain_message <- .("Data validation failed.")
            }
          }

          jmvcore::reject("{}", code = NULL, plain_message)
          return(NULL)
        }

        # NON-FATAL validation warnings go to the always-rendered notices panel.
        # They used to be written to todo2 and then wiped three statements later
        # whenever validation passed, so the user never saw them (missing
        # response counts, >200% growth, negative time values, ...).
        if (length(validation_messages) > 0) {
          plain <- gsub("<br>", " ", paste(validation_messages, collapse = " "), fixed = TRUE)
          plain <- gsub("<[^>]+>", " ", plain)
          # the messages were built HTML-escaped; undo that for the notice,
          # which escapes its own content on render
          plain <- gsub("&lt;", "<", plain, fixed = TRUE)
          plain <- gsub("&gt;", ">", plain, fixed = TRUE)
          plain <- gsub("&quot;", "\"", plain, fixed = TRUE)
          plain <- gsub("&apos;", "'", plain, fixed = TRUE)
          plain <- gsub("&#39;", "'", plain, fixed = TRUE)
          plain <- gsub("&amp;", "&", plain, fixed = TRUE)
          plain <- trimws(gsub("[[:space:]]+", " ", plain))
          if (nzchar(plain)) {
            private$.addNotice(
              type = "WARNING",
              title = .("DATA VALIDATION WARNINGS"),
              content = plain
            )
          }
        }

        # Clear todo messages for successful validation
        if (!isTRUE(self$options$enableGuidedMode)) {
          self$results$todo$setVisible(FALSE)
          self$results$todo2$setVisible(FALSE)
          self$results$todo2$setContent("")
        }

        # Process data
        # private$.checkpoint()  # Checkpoint before data processing

        processed_data <- private$.processData(

          validated_data,
          safe_patientID,
          self$options$inputType,
          safe_responseVar,
          safe_timeVar,
          safe_groupVar
        )

        # Account for every patient that entered the analysis but does not appear
        # in the waterfall, and demote patients with no post-baseline assessment
        # to "Unknown". Runs here because all three processing paths
        # (.processData, .processDataStandard, .processLargeDataset) converge on
        # this one return value.
        if (!is.null(processed_data) && !is.null(processed_data$waterfall)) {
          processed_data$waterfall <- private$.enforceMeasurementLimits(
            processed_data$waterfall, validated_data, safe_patientID,
            safe_responseVar, self$options$inputType)

          processed_data$waterfall <- private$.accountForUnevaluablePatients(
            processed_data$waterfall, validated_data, safe_patientID, safe_timeVar)
        }

        # Optional: override the computed RECIST category with a user-supplied one
        # (e.g., new-lesion PD despite target-lesion shrinkage). Applied before
        # metrics and plots so ORR/DCR and bar coloring all reflect it.
        if (!is.null(processed_data) && !is.null(processed_data$waterfall)) {
          processed_data$waterfall <- private$.applyCategoryOverride(
            processed_data$waterfall, self$data, safe_patientID,
            self$options$responseCategoryVar)
        }

        # ============================================================================
        # CRITICAL: REGULATORY USE BLOCKING
        # ============================================================================

        # ERROR BLOCKER: Regulatory/Clinical Trial Use
        # (Deficiency (2) previously claimed progression was measured from
        # BASELINE; .progressionTimes has referenced the NADIR since the DoR fix,
        # so the text now names only the limitations that actually remain.)
        # STRONG_WARNING, not ERROR: this is a permanent disclaimer on every
        # successful run, and a red ERROR banner every time teaches users to
        # ignore the real ones.
        private$.addNotice(
          type = "STRONG_WARNING",
          title = .("REGULATORY USE PROHIBITED"),
          content = .("This function is NOT validated for regulatory submissions, clinical trial endpoints, or companion diagnostic development. CRITICAL DEFICIENCIES: (1) Non-compliant RECIST v1.1 implementation (no target lesion summation, no new lesion detection, no confirmation requirement); (2) Progression for duration of response is referenced to the nadir, but the RECIST v1.1 requirement of an additional >=5 mm absolute increase is not applied, and new-lesion or non-target progression cannot be detected, so progression may still be under-detected; (3) Simplified best response = minimum value (may OVERCALL responses and MISS progressive disease). FDA/EMA GUIDANCE VIOLATION: This analysis does not meet requirements for biomarker companion diagnostic validation or pivotal trial endpoints. APPROVED USE ONLY: Exploratory visualization, pilot studies, hypothesis generation, educational demonstrations. For regulatory-grade RECIST assessment, use FDA-validated software (e.g., RECIST 1.1 certified platforms). Continuing with this analysis confirms understanding that results are EXPLORATORY ONLY and NOT for regulatory decision-making.")
        )

        # ============================================================================
        # CRITICAL: RECIST COMPLIANCE WARNINGS
        # ============================================================================
        # Add prominent warnings about analysis limitations before processing errors

        # Warning #1: Simplified Best Response Calculation (CRITICAL)
        private$.addNotice(
          type = "STRONG_WARNING",
          title = .("RECIST COMPLIANCE LIMITATION"),
          content = .("Best response is calculated as minimum percent change per patient. This analysis does NOT implement full RECIST v1.1 protocol. Missing: (1) Target lesion summation (assumes single lesion); (2) New lesion detection for PD; (3) Non-target lesion progression; (4) Response confirmation at 4+ weeks. Results may OVERCALL PR/CR and MISS progressive disease. For regulatory submissions or clinical trials, use RECIST-validated software. This is appropriate for exploratory visualization only.")
        )

        # Warning #2: Single-Lesion Assumption (HIGH)
        if (self$options$inputType == "raw") {
          private$.addNotice(
            type = "WARNING",
            title = .("SINGLE-LESION ASSUMPTION"),
            content = .("Raw measurements assume one target lesion per patient timepoint. If your data contains multiple lesions, percent changes will be INCORRECT. RECIST v1.1 requires summing diameters of up to 5 target lesions (max 2 per organ). Pre-process your data to sum target lesions before using this analysis, or switch to 'Percentage Changes' input with pre-calculated RECIST-compliant values.")
          )
        }

        # A spider plot is a trajectory over time; without a time variable there
        # is nothing to plot, and the checkbox previously just did nothing.
        if (isTRUE(self$options$showSpiderPlot) &&
            (is.null(self$options$timeVar) || identical(self$options$timeVar, ""))) {
          private$.addNotice(
            type = "WARNING",
            title = .("SPIDER PLOT NEEDS A TIME VARIABLE"),
            content = .("The spider plot draws each patient's tumour trajectory over time, so it requires a Time Variable. None is selected, so no spider plot can be produced. Assign the visit/assessment time column to \"Time Variable (Required for Spider Plot)\" to enable it.")
          )
        }

        # Same failure mode for the TTR/DoR table: previously it just sat empty.
        if (isTRUE(self$options$showResponseDuration) &&
            (is.null(self$options$timeVar) || identical(self$options$timeVar, ""))) {
          private$.addNotice(
            type = "WARNING",
            title = .("RESPONSE DURATION NEEDS A TIME VARIABLE"),
            content = .("Time to response and duration of response are computed from per-visit assessment times, so they require a Time Variable. None is selected, so the Time-to-Response & Duration of Response table cannot be populated.")
          )
        }

        # Warning #3: No Confirmation Requirement (MEDIUM)
        private$.addNotice(
          type = "WARNING",
          title = .("CONFIRMATION NOT REQUIRED"),
          content = .("RECIST v1.1 requires CR/PR confirmation at \u{2265}4 weeks. This analysis uses FIRST instance of response thresholds without confirmation. ORR and DCR may be INFLATED compared to confirmed RECIST responses. For clinical trials, unconfirmed responses should be clearly disclosed as exploratory endpoints.")
        )

        # Warning #4: Time-to-Event Methodology Limitations (MEDIUM)
        # (Progression IS nadir-referenced - .progressionTimes uses the running
        # minimum burden - so the former claim that it was baseline-referenced
        # was stale and mis-described the method.)
        if (!is.null(self$options$timeVar) && self$options$timeVar != "") {
          private$.addNotice(
            type = "WARNING",
            title = .("TIME-TO-EVENT LIMITATIONS"),
            content = .("Duration of response is reported both as a crude median (which ignores censoring and so understates DoR) and as a censoring-aware Kaplan-Meier median. Progression is detected as a >=20% increase over the NADIR (the smallest burden recorded so far), following RECIST v1.1. Two limitations remain. (1) The additional RECIST v1.1 requirement of a >=5 mm absolute increase cannot be applied to percent-change data, and new-lesion or non-target progression is invisible here, so progression may still be under-detected. (2) No log-rank test or Cox regression for covariates is provided. For formal progression-free survival (PFS) or duration of response analysis, use dedicated survival analysis functions. Current calculations are exploratory only.")
          )
        }

        # Warning #5: Baseline Validation (for raw measurements)
        if (self$options$inputType == "raw" && !is.null(self$options$timeVar) && self$options$timeVar != "") {
          private$.addNotice(
            type = "INFO",
            title = .("BASELINE ASSUMPTION"),
            content = .("Percent changes calculated assuming time=0 is baseline for each patient. Duplicate time=0 rows are now rejected during validation. Verify: (1) No measurements before time=0; (2) time=0 is the pre-treatment baseline (not post-treatment). Patients missing baseline measurements are excluded from waterfall analysis.")
          )
        }

        # ============================================================================
        # DATA QUALITY VALIDATION
        # ============================================================================

        # Check for small sample size (if processed_data is available)
        if (!is.null(processed_data) && !is.null(processed_data$waterfall)) {
          # Count EVALUABLE patients (non-missing response after the demotions
          # above), matching the ORR/DCR denominator - nrow() also counts
          # Unknown patients and so understated how small the usable cohort is.
          n_patients <- sum(!is.na(processed_data$waterfall$response))

          # Warning #6: Small Sample Size
          if (n_patients > 0 && n_patients < 10) {
            private$.addNotice(
              type = "STRONG_WARNING",
              title = .("VERY SMALL SAMPLE"),
              content = sprintf(
                .("Only n=%d evaluable patients. ORR and DCR confidence intervals will be EXTREMELY WIDE and unreliable, and a single patient changes the rate by %.0f percentage points. Phase II oncology trials typically require minimum n=20-40 for meaningful ORR estimation. With n<10, results are purely descriptive and should NOT be used for treatment decision-making or regulatory submissions. Consider this a pilot/feasibility analysis only."),
                n_patients, 100 / n_patients)
            )
          } else if (n_patients < 20) {
            private$.addNotice(
              type = "WARNING",
              title = .("SMALL SAMPLE"),
              content = sprintf(
                .("n=%d evaluable patients. Confidence intervals for ORR/DCR will be wide. Phase II single-arm trials typically enroll 30-50 patients for adequate precision. Results should be interpreted cautiously as exploratory."),
                n_patients)
            )
          }

          # Warning #7: Extreme Outlier Detection
          if ("response" %in% names(processed_data$waterfall)) {
            response_values <- processed_data$waterfall$response
            extreme_shrinkage <- any(response_values < -100, na.rm = TRUE)
            extreme_growth <- any(response_values > 200, na.rm = TRUE)

            if (extreme_shrinkage || extreme_growth) {
              parts <- character(0)
              if (extreme_shrinkage) {
                parts <- c(parts, sprintf(
                  .("%d patient(s) with tumor shrinkage >100%% (impossible with single lesion)."),
                  sum(response_values < -100, na.rm = TRUE)))
              }
              if (extreme_growth) {
                parts <- c(parts, sprintf(
                  .("%d patient(s) with tumor growth >200%% (verify data accuracy)."),
                  sum(response_values > 200, na.rm = TRUE)))
              }
              parts <- c(parts, .("Possible causes: (1) Data entry errors; (2) Multi-lesion summation issues; (3) New lesions added during followup; (4) Measurement variability. Verify raw data before interpreting results. Extreme values can distort plot scaling and statistical summaries."))

              private$.addNotice(
                type = "WARNING",
                title = .("EXTREME VALUES DETECTED"),
                content = paste(parts, collapse = " ")
              )
            }
          }
        }

        # Check for processing errors
        if (!is.null(processed_data$error) && processed_data$error) {
          error_message <- paste0(
            "<br><br>", .("Data Processing Error:"),
            "<br>", processed_data$message,
            "<br><br>", .("Please check your data and try again.")
          )

          if (!isTRUE(self$options$enableGuidedMode)) {
            self$results$todo2$setVisible(TRUE)
            self$results$todo2$setContent(error_message)
            self$results$todo$setVisible(FALSE)
          }
          return(NULL)
        }

        return(processed_data)
      },



      # Generate tables and results ----
      .generateTablesAndResults = function(processed_data) {
        # Use RAW variable names as data-frame keys (jamovi delivers self$data with
        # original names; make.names()-escaping breaks lookups for names with spaces).
        safe_patientID <- self$options$patientID
        safe_responseVar <- self$options$responseVar
        safe_timeVar <- self$options$timeVar
        safe_groupVar <- self$options$groupVar

        # Extract data
        df_waterfall <- processed_data$waterfall


        ## Calculate metrics ----
        private$.checkpoint()  # Checkpoint before metrics calculation
        metrics <- private$.calculateMetrics(processed_data$waterfall)
        

        
        # Calculate person-time metrics if applicable
        person_time_metrics <- NULL
        personTimeVisible <- !is.null(self$options$timeVar) && self$options$inputType == "raw"
        
        ## Populate tables ----
        # 1. Response Summary Table
        table <- self$results$summaryTable



        ## Update results tables ----
        private$.checkpoint()  # Checkpoint before summary table population
        # The four category rows are created in .init() (fixed row set); fill
        # them with setRow so the table does not restructure on every run. The
        # rowKeys fallback covers a run without a prior init cycle.
        for(i in seq_len(nrow(metrics$summary))) {
          row_key <- sprintf("recist_%s", metrics$summary$category[i])
          percent_value <- metrics$summary$percent[i]
          percent_display <- if (!is.na(percent_value)) percent_value else NA_real_

          values <- list(
            category = metrics$summary$category[i],
            n = metrics$summary$n[i],
            percent = percent_display
          )
          if (row_key %in% self$results$summaryTable$rowKeys)
            self$results$summaryTable$setRow(rowKey = row_key, values = values)
          else
            self$results$summaryTable$addRow(rowKey = row_key, values = values)
        }

        # Patients in the waterfall but not evaluable (all-missing responses,
        # negative measurements, baseline-only follow-up) used to vanish from
        # this table entirely, so its n never reconciled with the cohort.
        if (!is.null(metrics$n_unknown) && metrics$n_unknown > 0) {
          unknown_values <- list(
            category = .("Unknown / not evaluable"),
            n = metrics$n_unknown,
            percent = NA_real_
          )
          # This row is not part of the .init() skeleton; addRow() on a re-run
          # would append a second copy.
          if ("recist_Unknown" %in% self$results$summaryTable$rowKeys)
            self$results$summaryTable$setRow(rowKey = "recist_Unknown", values = unknown_values)
          else
            self$results$summaryTable$addRow(rowKey = "recist_Unknown", values = unknown_values)
          self$results$summaryTable$setNote("unknown",
            .("Percentages and the response rates below are computed over evaluable patients only (CR/PR/SD/PD); patients with an unknown category are excluded from every denominator. See the Important Information panel for who was excluded and why."))
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

        # Row counter held in an environment so the nested add_metric_row()
        # helper can advance it without `<<-` into the enclosing method scope.
        idx_env <- new.env(parent = emptyenv())
        idx_env$metric_row_index <- 1
        add_metric_row <- function(values) {
          row_key <- sprintf("metric_%02d", idx_env$metric_row_index)
          self$results$clinicalMetrics$addRow(rowKey = row_key, values = values)
          idx_env$metric_row_index <- idx_env$metric_row_index + 1
        }

        # Add Evaluable Patients (metrics$n excludes Unknown/NA-response patients,
        # so label it as evaluable-n to avoid confusion with the full cohort size
        # used elsewhere in CIs/summaries).
        add_metric_row(list(
          metric = .("Evaluable Patients"),
          value = as.character(metrics$n)
        ))


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

        # No post-hoc ("observed") power row here, deliberately. Power computed from
        # the OBSERVED response rate is a deterministic function of the p-value
        # (Hoenig & Heisey 2001, The Abuse of Power): it restates the test result
        # instead of informing it, and reporting it as an adequacy verdict is
        # circular - a trial that happens to succeed gets called well powered and
        # one that fails gets called underpowered, at identical sample size.
        # Power belongs in a DESIGN calculation before the trial; see the
        # Group-Sequential Design & Sample Size analysis.


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
              safe_patientID,
              safe_timeVar,
              safe_responseVar
            )
          }, error = function(e) {
            private$.addNotice(
              type = "WARNING",
              title = .("PERSON-TIME ANALYSIS FAILED"),
              content = sprintf(
                .("Person-time metrics could not be computed (%s). The Person-Time Analysis table is omitted."),
                e$message)
            )
            return(NULL)
          })
        }



        # Add time-to-event metrics if available
        tte_metrics <- NULL

        if (!is.null(self$options$timeVar) && !is.null(processed_data$spider) &&
            safe_timeVar %in% names(processed_data$spider)) {
          tte_metrics <- private$.calculateTimeToEventMetrics(
            processed_data$spider,
            safe_patientID,
            safe_timeVar,
            "response"
          )

          if (!is.null(tte_metrics)) {
            # Add median time to response
            if (!is.na(tte_metrics$summary$median_time_to_response)) {
              add_metric_row(list(
                metric = .("Median Time to First Response"),
                value = sprintf(.("%.1f time units (n=%d responders)"),
                               tte_metrics$summary$median_time_to_response,
                               tte_metrics$summary$n_responders)
              ))
            }

            # Add median duration of response
            if (!is.na(tte_metrics$summary$median_duration_of_response)) {
              add_metric_row(list(
                metric = .("Median Duration of Response"),
                value = sprintf(.("%.1f time units (n=%d with duration data)"),
                               tte_metrics$summary$median_duration_of_response,
                               tte_metrics$summary$n_with_duration_data)
              ))
            }

            # Dedicated TTR / DoR table with censoring-aware (Kaplan-Meier) DoR
            if (isTRUE(self$options$showResponseDuration) &&
                !is.null(self$results$responseDurationTable)) {
              rdt <- self$results$responseDurationTable
              s <- tte_metrics$summary
              if (!is.na(s$median_time_to_response))
                rdt$addRow(rowKey = "ttr", values = list(
                  metric = .("Median time to first response (TTR)"),
                  value = s$median_time_to_response,
                  detail = sprintf(.("RECIST PR or better; n=%d responders"), s$n_responders)))
              if (!is.na(s$median_duration_of_response))
                rdt$addRow(rowKey = "dor_naive", values = list(
                  metric = .("Median duration of response (naive)"),
                  value = s$median_duration_of_response,
                  detail = sprintf(.("Ignores censoring; n=%d with duration data"),
                                   s$n_with_duration_data)))
              if (!is.null(s$km_median_duration_of_response) &&
                  !is.na(s$km_median_duration_of_response)) {
                rdt$addRow(rowKey = "dor_km", values = list(
                  metric = .("Median duration of response (Kaplan-Meier)"),
                  value = s$km_median_duration_of_response,
                  detail = sprintf(.("Censoring-aware; %d progression events"), s$n_duration_events)))
              } else if (!is.null(s$n_duration_events) && !is.na(s$n_duration_events)) {
                # A silently missing KM row read as "not computed"; say why.
                rdt$addRow(rowKey = "dor_km", values = list(
                  metric = .("Median duration of response (Kaplan-Meier)"),
                  value = NA_real_,
                  detail = sprintf(.("Median not reached (only %d of %d responders progressed)"),
                                   s$n_duration_events, s$n_responders)))
              }
              rdt$setNote("dor",
                .("DoR is measured from first RECIST response to progression over the nadir; responders still in response at last follow-up are censored. The Kaplan-Meier median accounts for this censoring and is the preferred summary."))
            }
          }
        }
        

        # Add person-time metrics to the results if available
        if (!is.null(person_time_metrics) && personTimeVisible) {
          # The person-time TTR/DoR figures are computed differently from the
          # tte_metrics rows above (time to BEST response; first-to-last
          # response-visit span). Adding both put TWO rows named "Median
          # Duration of Response" with different numbers in one table, plus
          # unit-agnostic interpretations ("rapid" at <=2 of whatever the time
          # unit is). They are added only as an honestly-labelled FALLBACK when
          # the tte_metrics rows are unavailable; the per-category detail lives
          # in the Person-Time table either way.
          if (is.null(tte_metrics)) {
            median_tbr <- stats::median(person_time_metrics$by_patient$time_to_best, na.rm = TRUE)
            median_span <- median(person_time_metrics$by_patient$time_in_response[
              person_time_metrics$by_patient$time_in_response > 0
            ], na.rm = TRUE)

            if (!is.na(median_tbr))
              add_metric_row(list(
                metric = .("Median Time to Best Response"),
                value = sprintf("%.1f", median_tbr)
              ))
            if (!is.na(median_span))
              add_metric_row(list(
                metric = .("Median Time in Response (first to last response visit)"),
                value = sprintf("%.1f", median_span)
              ))
          }

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
              cat_i <- as.character(person_time_metrics$by_category$response_cat[i])
              # "Median Time to Response" is meaningless for SD/PD rows (it
              # would be time to the least-bad assessment); leave those blank.
              is_responder_cat <- cat_i %in% c("CR", "PR")
              self$results$personTimeTable$addRow(rowKey = i, values = list(
                category = cat_i,
                patients = person_time_metrics$by_category$patients[i],
                patient_pct = sprintf("%.1f%%", person_time_metrics$by_category$pct_patients[i]),
                person_time = sprintf("%.1f", person_time_metrics$by_category$person_time[i]),
                time_pct = sprintf("%.1f%%", person_time_metrics$by_category$pct_time[i]),
                median_time = if (is_responder_cat)
                  sprintf("%.1f", person_time_metrics$by_category$median_time_to_response[i]) else "",
                median_duration = if (is_responder_cat)
                  sprintf("%.1f", person_time_metrics$by_category$median_duration[i]) else ""
              ))
            }

            # Add total row
            self$results$personTimeTable$addRow(rowKey = nrow(person_time_metrics$by_category) + 1, values = list(
              category = .("Total"),
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
      },


      # Generate visualizations ----
      .generateVisualizations = function(processed_data, metrics) {
        # Use RAW variable names as data-frame keys (jamovi delivers self$data with
        # original names; make.names()-escaping breaks lookups for names with spaces).
        safe_patientID <- self$options$patientID
        safe_responseVar <- self$options$responseVar
        safe_timeVar <- self$options$timeVar
        safe_groupVar <- self$options$groupVar

        # Attach optional confirmation / ongoing annotations (issue #1 markers).
        # Baked into the waterfall data so the plot state carries them to render.
        if (!is.null(processed_data$waterfall)) {
          processed_data$waterfall <- private$.attachAnnotations(
            processed_data$waterfall, self$data, safe_patientID,
            self$options$confirmationVar, self$options$ongoingVar)
        }

        # Prepare comprehensive plot data structure
        plotData <- list(
          "data" = processed_data,
          options = list(
            "patientID" = safe_patientID,
            "response" = safe_responseVar,
            "timeVar" = safe_timeVar,
            "sortBy" = self$options$sortBy,
            "sortDirection" = self$options$sortDirection,
            "showThresholds" = self$options$showThresholds,
            "labelOutliers" = self$options$labelOutliers,
            "colorScheme" = self$options$colorScheme,
            "colorBy" = self$options$colorBy,
            "groupVar" = safe_groupVar,
            "barWidth" = self$options$barWidth,
            "barAlpha" = self$options$barAlpha,
            "showMedian" = self$options$showMedian,
            "showCI" = self$options$showCI,
            "seed" = self$options$seed,
            "showBaseline" = self$options$showBaseline,
            "confirmationVar" = self$options$confirmationVar,
            "ongoingVar" = self$options$ongoingVar,
            "minResponseForLabel" = self$options$minResponseForLabel,
            "spiderColorBy" = self$options$spiderColorBy,
            "spiderColorScheme" = self$options$spiderColorScheme,
            "timeUnitLabel" = self$options$timeUnitLabel,
            # tryCatch guards the window before jmvtools::prepare() regenerates the
            # header: jmvcore errors on an option the compiled .h.R does not carry.
            "annotationVars" = tryCatch(self$options$annotationVars,
                                       error = function(e) NULL),
            "showCategoryLabels" = tryCatch(self$options$showCategoryLabels,
                                            error = function(e) FALSE),
            "showSpiderLabels" = tryCatch(self$options$showSpiderLabels,
                                          error = function(e) FALSE)
          ),
          "metrics" = metrics
        )

        # Add checkpoint for performance monitoring
        private$.checkpoint()

        # The bootstrap median CI needs enough patients to be meaningful; the
        # renderer used to say so only via message(), which jamovi never shows.
        if (isTRUE(self$options$showCI) && !is.null(processed_data$waterfall)) {
          n_rows <- nrow(processed_data$waterfall)
          n_ci <- sum(!is.na(processed_data$waterfall$response))
          if (n_rows < 10) {
            private$.addNotice(
              type = "WARNING",
              title = .("MEDIAN CI NOT DRAWN"),
              content = sprintf(
                .("The bootstrap confidence interval for the median response requires at least 10 patients; only %d are available, so no CI annotation is drawn on the waterfall plot."),
                n_rows)
            )
          } else if (n_ci < 20) {
            private$.addNotice(
              type = "INFO",
              title = .("BOOTSTRAP CI STABILITY"),
              content = sprintf(
                .("The bootstrap confidence interval for the median response is computed from %d evaluable patients; below 20 it can be unstable, so interpret it with caution."),
                n_ci)
            )
          }
        }

        # Initialize waterfall plot if visible
        if (isTRUE(self$options$showWaterfallPlot)) {
          self$results$waterfallplot$setState(plotData)
        }

        # Initialize spider plot with validation
        if (isTRUE(self$options$showSpiderPlot) && !is.null(self$options$timeVar)) {
          # Validate spider plot requirements
          if (self$options$inputType == "percentage") {
            private$.addNotice(
              type = "INFO",
              title = .("SPIDER PLOT WITH PERCENTAGE DATA"),
              content = .("With percentage input the spider plot connects the pre-calculated percent changes at each visit. If you have raw per-visit measurements, the raw input type gives trajectories computed against the time = 0 baseline.")
            )
          }

          plotData$timeVar <- safe_timeVar
          self$results$spiderplot$setState(plotData)
        }
      },


      # Generate reports and supplementary content ----
      .generateReportsAndContent = function(processed_data, metrics, person_time_metrics) {
        # Use RAW variable names as data-frame keys (jamovi delivers self$data with
        # original names; make.names()-escaping breaks lookups for names with spaces).
        safe_patientID <- self$options$patientID
        safe_responseVar <- self$options$responseVar
        safe_timeVar <- self$options$timeVar
        safe_groupVar <- self$options$groupVar

        # Generate enhanced clinical metrics with confidence intervals ----
        if (isTRUE(self$options$showConfidenceIntervals)) {
          private$.generateEnhancedClinicalMetrics(processed_data, metrics)
        }

        # Generate copy-ready report ----
        if (isTRUE(self$options$generateCopyReadyReport)) {
          private$.generateCopyReadyReport(processed_data, metrics, person_time_metrics)
        }

        # Show clinical significance assessment ----
        if (isTRUE(self$options$showClinicalSignificance)) {
          # metrics$n is the evaluable count every rate on that panel uses;
          # nrow(waterfall) also counted Unknown patients.
          private$.generateClinicalSignificance(metrics, metrics$n)
          private$.generateClinicalGlossary()
        }

        ## Add response category to data ----
        if (isTRUE(self$options$addResponseCategory) &&
            self$results$addResponseCategory$isNotFilled() &&
            !is.null(processed_data$waterfall) &&
            safe_patientID %in% names(processed_data$waterfall)) {

          # Map each source row back to its patient's category BY PATIENT ID.
          #
          # The previous no-timeVar branch used rownames(processed_data$waterfall)
          # as dataset row numbers. That frame is a dplyr tibble (so its rownames
          # are always "1".."k", never the source row numbers) and it has been
          # collapsed to one row per patient and re-sorted into patient-ID order.
          # jmvcore ships those values to the literal dataset rows, so every
          # patient's exported category was written against the wrong patient --
          # silent, unflagged corruption of a column users then analyse further.
          cats <- processed_data$waterfall %>%
            dplyr::select(!!rlang::sym(safe_patientID), recist_category) %>%
            dplyr::distinct()

          idx <- match(self$data[[safe_patientID]], cats[[safe_patientID]])
          values <- cats$recist_category[idx]
          # Keep the full level set even when a category is absent, so the
          # exported column is a stable factor rather than one whose levels
          # depend on which categories happen to occur.
          values <- factor(as.character(values),
                           levels = levels(cats$recist_category))

          self$results$addResponseCategory$setRowNums(rownames(self$data))
          self$results$addResponseCategory$setValues(values)
        }
      },


      # Generate explanations and natural language summary ----
      .generateExplanations = function(processed_data, metrics) {
        # Generate Natural Language Summary
        if (!is.null(processed_data) && !is.null(metrics)) {
          # One denominator throughout: evaluable patients (metrics$n), the same
          # one ORR/DCR use. nrow() also counts Unknown patients and previously
          # made the distribution percentages disagree with the rates beside them.
          n_total <- nrow(processed_data$waterfall)
          n_eval <- metrics$n
          n_unknown <- if (!is.null(metrics$n_unknown)) metrics$n_unknown else (n_total - n_eval)

          # Extract counts from metrics$summary
          n_cr <- metrics$summary$n[metrics$summary$category == "CR"]
          n_pr <- metrics$summary$n[metrics$summary$category == "PR"]
          n_sd <- metrics$summary$n[metrics$summary$category == "SD"]
          n_pd <- metrics$summary$n[metrics$summary$category == "PD"]

          # Ensure we have numeric values (default to 0 if missing)
          if (length(n_cr) == 0) n_cr <- 0
          if (length(n_pr) == 0) n_pr <- 0
          if (length(n_sd) == 0) n_sd <- 0
          if (length(n_pd) == 0) n_pd <- 0

          # Use uppercase ORR and DCR, ensure they are numeric
          orr <- as.numeric(metrics$ORR)
          dcr <- as.numeric(metrics$DCR)

          pct_eval <- function(k) if (n_eval > 0) round(k / n_eval * 100) else 0

          # Create summary HTML (with NA handling)
          orr_text <- if (!is.na(orr)) {
            sprintf(.("%d%% (%d of %d evaluable patients achieved complete or partial response)"), round(orr), n_cr + n_pr, n_eval)
          } else {
            .("Not available (insufficient data)")
          }

          dcr_text <- if (!is.na(dcr)) {
            sprintf(.("%d%% (%d of %d evaluable patients achieved response or stable disease)"), round(dcr), n_cr + n_pr + n_sd, n_eval)
          } else {
            .("Not available (insufficient data)")
          }

          interpretation_text <- if (!is.na(orr)) {
            private$.interpretORR(orr)
          } else {
            .("Insufficient data for clinical interpretation")
          }

          summary_html <- paste0(
            "<div style='padding: 15px; background-color: rgba(138, 155, 172, 0.06); border-left: 4px solid #007bff; margin: 10px 0; color: inherit;'>",
            "<h3 style='color: #007bff; margin-top: 0;'>", .("Treatment Response Summary"), "</h3>",

            "<p><strong>", .("Analysis Overview:"), "</strong></p>",
            "<p>", sprintf(.("Response analysis of %d patients (%d evaluable) using threshold-based categories adapted from RECIST v1.1; this is not a full RECIST v1.1 assessment."), n_total, n_eval), "</p>",

            "<p><strong>", .("Key Findings:"), "</strong></p>",
            "<p><strong>", .("Objective Response Rate (ORR):"), "</strong><br>",
            orr_text, "</p>",

            "<p><strong>", .("Disease Control Rate (DCR):"), "</strong><br>",
            dcr_text, "</p>",

            "<p><strong>", .("Response Distribution:"), "</strong></p>",
            "<ul style='margin: 5px 0;'>",
            sprintf("<li><strong>%s:</strong> %d %s (%d%%)</li>", .("Complete Response"), n_cr, .("patients"), pct_eval(n_cr)),
            sprintf("<li><strong>%s:</strong> %d %s (%d%%)</li>", .("Partial Response"), n_pr, .("patients"), pct_eval(n_pr)),
            sprintf("<li><strong>%s:</strong> %d %s (%d%%)</li>", .("Stable Disease"), n_sd, .("patients"), pct_eval(n_sd)),
            sprintf("<li><strong>%s:</strong> %d %s (%d%%)</li>", .("Progressive Disease"), n_pd, .("patients"), pct_eval(n_pd)),
            if (n_unknown > 0) sprintf("<li><strong>%s:</strong> %d %s</li>", .("Unknown / not evaluable (excluded from percentages)"), n_unknown, .("patients")) else "",
            "</ul>",

            "<p><strong>", .("Clinical Interpretation:"), "</strong></p>",
            "<p>", interpretation_text, "</p>",

            "</div>"
          )

          self$results$naturalLanguageSummary$setContent(summary_html)
        }

        # Generate Analysis Explanations
        explanations_html <- paste0(
          "<div style='padding: 15px; background-color: rgba(33, 149, 188, 0.1); border-left: 4px solid #17a2b8; margin: 20px 0; color: inherit;'>",
          "<h3 style='color: #17a2b8; margin-top: 0;'>", .("What This Analysis Does"), "</h3>",
          "<p>", .("The Treatment Response Analysis creates waterfall and spider plots using threshold-based response categories adapted from RECIST v1.1 (not a full RECIST v1.1 assessment)."), "</p>",

          "<h4 style='color: #17a2b8; margin-top: 15px;'>", .("Visualization Types:"), "</h4>",
          "<ul style='margin: 5px 0;'>",
          "<li><strong>", .("Waterfall Plot:"), "</strong> ", .("Shows best response for each patient as vertical bars, ideal for single timepoint or best response data."), "</li>",
          "<li><strong>", .("Spider Plot:"), "</strong> ", .("Shows response trajectories over time as connected lines, requires time variable for longitudinal data."), "</li>",
          "</ul>",
          "</div>",

          "<div style='padding: 15px; background-color: rgba(255, 202, 33, 0.23); border-left: 4px solid #ffc107; margin: 20px 0; color: inherit;'>",
          "<h3 style='color: inherit; margin-top: 0;'>", .("When to Use This Analysis:"), "</h3>",
          "<ul style='margin: 5px 0;'>",
          "<li>", .("Oncology clinical trials and treatment response studies"), "</li>",
          "<li>", .("Drug efficacy evaluation"), "</li>",
          "<li>", .("Tumor response monitoring"), "</li>",
          "<li>", .("Biomarker correlation studies"), "</li>",
          "</ul>",
          "</div>",

          "<div style='padding: 15px; background-color: rgba(33, 163, 188, 0.21); border-left: 4px solid #0c5460; margin: 20px 0; color: inherit;'>",
          "<h3 style='color: inherit; margin-top: 0;'>", .("Data Requirements:"), "</h3>",
          "<ul style='margin: 5px 0;'>",
          "<li><strong>", .("Patient ID:"), "</strong> ", .("Unique identifier for each patient"), "</li>",
          "<li><strong>", .("Response Data:"), "</strong> ", .("Either percentage changes from baseline or raw tumor measurements"), "</li>",
          "<li><strong>", .("Time Variable:"), "</strong> ", .("Required for spider plots (e.g., months from baseline)"), "</li>",
          "</ul>",
          "</div>",

          "<div style='padding: 15px; background-color: rgba(216, 33, 50, 0.18); border-left: 4px solid #dc3545; margin: 20px 0; color: inherit;'>",
          "<h3 style='color: inherit; margin-top: 0;'>", .("Key Assumptions & Limitations:"), "</h3>",
          "<ul style='margin: 5px 0;'>",
          "<li>", sprintf(.("RECIST v1.1 thresholds: CR \u{2264}-100%%, PR \u{2264}-30%%, PD \u{2265}+20%%")), "</li>",
          "<li>", .("For raw measurements, baseline assumed at time = 0"), "</li>",
          "<li>", .("Waterfall plot shows best (most negative) response per patient"), "</li>",
          "<li>", .("Missing values are excluded from analysis"), "</li>",
          "</ul>",

          "<p style='margin-top: 15px; font-style: italic; color: inherit;'>",
          "<strong>", .("Tip:"), "</strong> ",
          .("Start with percentage data if available, or use raw measurements with proper time variables for automatic calculation."),
          "</p>",
          "</div>"
        )

        self$results$explanations$setContent(explanations_html)
      },


      # Populate fixed table structure. Rows whose set is known up front belong
      # here (library-review gate): the table skeleton is stable and .run()
      # fills values with setRow, so nothing visibly restructures each cycle.
      .init = function() {
        if (self$results$summaryTable$rowCount == 0) {
          for (cat in c("CR", "PR", "SD", "PD"))
            self$results$summaryTable$addRow(
              rowKey = sprintf("recist_%s", cat),
              values = list(category = cat))
        }
        if (self$results$enhancedClinicalMetrics$rowCount == 0) {
          self$results$enhancedClinicalMetrics$addRow(rowKey = 1, values = list())
          self$results$enhancedClinicalMetrics$addRow(rowKey = 2, values = list())
        }
      },

      # Refactored run method ----
      .run = function() {

        # Reset accumulated notices at the start of every run. jamovi reuses the
        # analysis R6 instance across re-runs within a session, so without this
        # reset .addNotice() would append to the prior run's list and .renderNotices()
        # would emit each notice 2x, 3x, ... on successive runs.
        private$.noticeList <- list()

        # Render whatever notices accumulated NO MATTER how this run ends.
        # .renderNotices() used to be the final statement of .run(), so every
        # early return (validation reject, processing error) and any crash
        # silently discarded the very notices that explain the failure.
        on.exit(private$.renderNotices(), add = TRUE)

        # Step 1: Initialize analysis and show guidance
        private$.initializeAnalysis()

        # jamovi reuses this instance, and Table$addRow() never checks for an
        # existing rowKey, so a re-run that does not trip clearWith doubled
        # every row in the addRow-populated tables. Clear them here, once;
        # summaryTable and enhancedClinicalMetrics keep their .init() skeleton
        # and are filled with setRow.
        for (tbl in c("clinicalMetrics", "responseDurationTable", "personTimeTable",
                      "groupComparisonTable", "groupComparisonTest")) {
          self$results[[tbl]]$deleteRows()
        }

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

        # Step 7: Generate explanations and natural language summary (if requested)
        if (isTRUE(self$options$showExplanations)) {
          private$.generateExplanations(processed_data, results$metrics)
        }

        # (Notices render via the on.exit() registered at the top of .run().)

      },



      # Waterfall plot ----
      .waterfallplot = function(imageWaterfall, ggtheme, theme, ...) {
        if (!self$options$showWaterfallPlot) return()

        private$.checkpoint()  # Checkpoint before plot generation

        # Renderers run on resize and on .omv reload before .run() has set any
        # state; guard BEFORE any read and return FALSE (clean empty panel).
        plotData <- imageWaterfall$state
        if (is.null(plotData) || is.null(plotData$data) || is.null(plotData$data$waterfall))
          return(FALSE)
        options <- plotData$options

        df <- plotData$data$waterfall

        # Sort data
        if (plotData$options$sortBy == "response") {
          # conventional oncology waterfall: worst (highest) on left, best (lowest) on right
          decreasing <- !identical(plotData$options$sortDirection, "reverse")
          df <- df[order(df$response, decreasing = decreasing, na.last = TRUE),]
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

        # Response category above each bar (PD / SD / PR / CR), the convention
        # in published waterfall figures: the reader gets the category without
        # having to map bar colour back to a legend.
        # Design credit: Jamovi-TrialPlots by highwindmx (LGPL),
        # https://github.com/highwindmx/Jamovi-TrialPlots
        if (isTRUE(plotData$options$showCategoryLabels) &&
            "recist_category" %in% names(df)) {
          cats <- as.character(df$recist_category)
          keep <- !is.na(cats) & cats != "Unknown"
          if (any(keep)) {
            p <- p +
              ggplot2::geom_text(
                data = data.frame(
                  .x = factor(which(keep), levels = levels(factor(seq_len(nrow(df))))),
                  .y = df$response[keep],
                  .lab = cats[keep],
                  stringsAsFactors = FALSE
                ),
                mapping = ggplot2::aes(x = .data$.x, y = .data$.y, label = .data$.lab),
                vjust = ifelse(df$response[keep] >= 0, -0.6, 1.4),
                size = 2.8,
                inherit.aes = FALSE
              )
          }
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
              label = sprintf(.("Median: %.1f%%"), med),
              hjust = 1,
              vjust = -0.5,
              size = 3
            )
        }

        # Add confidence interval - FIXED: Bootstrap percentile CI for median
        ci_state <- new.env(parent = emptyenv())
        if (plotData$options$showCI && sum(!is.na(df$response)) >= 10) {
          # REPLACED: t.test CI is inappropriate for skewed response data and computes CI for MEAN not MEDIAN
          # NEW: Bootstrap percentile CI for median (appropriate for skewed/ordinal data)
          tryCatch({
            # REPRODUCIBILITY: user-configurable seed for reproducible bootstrap
            # results (defaults to 123 when unset).
            seed_val <- plotData$options$seed
            if (is.null(seed_val)) seed_val <- 123
            set.seed(seed_val)

            # Resample the NON-MISSING responses only: drawing from the full
            # vector including NAs made each replicate's effective n random
            # (below n_data), subtly distorting the interval whenever Unknown
            # patients were present. (Stability guidance for small n is issued
            # as a notice at run time, where the user can actually see it.)
            boot_values <- df$response[!is.na(df$response)]
            n_data <- length(boot_values)

            # Use more iterations for small samples to improve stability
            n_boot <- ifelse(n_data < 30, 2000, 1000)

            boot_medians <- replicate(n_boot, {
              median(sample(boot_values, size = n_data, replace = TRUE))
            })
            # Percentile method: 2.5th and 97.5th percentiles
            ci <- quantile(boot_medians, probs = c(0.025, 0.975), na.rm = TRUE)

            p <- p +
              ggplot2::annotate(
                "text",
                x = 1,
                y = max(df$response, na.rm=TRUE),
                label = sprintf(
                  .("95%% CI (Median): [%.1f%%, %.1f%%]"),
                  ci[1],
                  ci[2]
                ),
                hjust = 0,
                vjust = -0.5,
                size = 3
              )
          }, error = function(e) {
            # No CI annotation; say so on the plot (notices are already rendered
            # by the time a renderer runs, and jamovi hides warning()).
            ci_state$failed <- TRUE
          })
        }
        if (isTRUE(ci_state$failed)) {
          p <- p + ggplot2::labs(caption = .("Bootstrap CI for the median could not be computed."))
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




        # Issue #1: baseline reference line + confirmation/ongoing markers
        p <- private$.addBaseline(p, isTRUE(plotData$options$showBaseline))
        p <- private$.addAnnotationMarkers(p, df, plotData)

        # Stack the annotation tracks under the bars, sharing the x axis. `df` is
        # already in bar order here, so the two panels line up patient for patient.
        track <- private$.annotationTrack(df, plotData)
        if (!is.null(track)) {
            p <- p + ggplot2::theme(axis.title.x = ggplot2::element_blank())
            n_tracks <- length(plotData$options$annotationVars)
            p <- patchwork::wrap_plots(
                p, track,
                ncol = 1,
                heights = c(1, min(0.5, 0.09 * n_tracks))
            )
        }

        print(p)
        TRUE
      },





      # spider plot ----
      .spiderplot = function(imagespider, ggtheme, theme, ...) {
        private$.checkpoint()  # Checkpoint before spider plot generation

        # Check conditions for showing the information message
        if (is.null(self$options$timeVar) || !self$options$showSpiderPlot) {
          # Informative placeholder. Each .() wraps one complete sentence; the
          # layout newlines live OUTSIDE the translatable strings.
          text_warning <- paste(
            .("Spider Plot Requirements and Guidelines"),
            "",
            .("This visualization requires a time variable (to draw response trajectories) and the 'Spider plot' option to be enabled."),
            "",
            .("A spider plot shows how each patient's response changes over time: each line is one patient's treatment journey, which makes response patterns and outcome types easy to see."),
            "",
            .("To generate the plot, add a time variable (such as months from baseline, starting at 0) and enable 'Spider plot' in the options panel."),
            "",
            .("Example data format:"),
            "PatientID   Time   Measurement",
            "PT1         0      50",
            "PT1         2      25",
            "PT1         4      10",
            sep = "\n"
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

        # Get plot data from state. Renderers run on resize and on .omv reload
        # before .run() has set any state; guard BEFORE any field read.
        plotData <- imagespider$state
        if (is.null(plotData) || is.null(plotData$data) || is.null(plotData$data$spider))
          return(FALSE)

        # Extract data and options
        df <- plotData$data$spider
        options <- plotData$options

        if (is.null(df) || nrow(df) == 0)
          return(FALSE)

        # Validate required variables exist
        required_vars <- c(options$timeVar, options$patientID)
        missing_vars <- required_vars[!required_vars %in% names(df)]
        if (length(missing_vars) > 0)
          return(FALSE)

        # Convert variables to numeric explicitly
        df$time <- jmvcore::toNumeric(df[[options$timeVar]])
        
        # Check if response column exists, if not create it from the response variable
        if ("response" %in% names(df)) {
          df$response <- jmvcore::toNumeric(df$response)
        } else if ("percentage_change" %in% names(df)) {
          df$response <- jmvcore::toNumeric(df$percentage_change)
        } else {
          # Fallback to the raw response column. The plotData options list packs
          # the response-variable NAME under the key "response" (there is no
          # "responseVar" key - reading it returned NULL and df[[NULL]] threw).
          df$response <- jmvcore::toNumeric(df[[options$response]])
        }

        # Remove any rows with NA values in required columns
        df <- df[complete.cases(df[c("time", "response")]), ]
        if (nrow(df) == 0)
          return(FALSE)

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
        
        # Configure x-axis label based on user-selected time unit label
        x_unit_label <- switch(options$timeUnitLabel,
          "days"   = .("Days from Baseline"),
          "weeks"  = .("Weeks from Baseline"),
          "months" = .("Months from Baseline"),
          "years"  = .("Years from Baseline"),
          .("Time from Baseline")
        )

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
          ggplot2::labs(
            x = x_unit_label,
            y = .("Change in Tumor Size (%)"),
            title = .("Spider Plot of Tumor Response")
          )

        # Label each trajectory at its last point, so an outlier line can be
        # traced back to a patient without reading a legend of 40 colours.
        # Design credit: Jamovi-TrialPlots by highwindmx (LGPL),
        # https://github.com/highwindmx/Jamovi-TrialPlots
        if (isTRUE(options$showSpiderLabels) && options$patientID %in% names(df)) {
          ends <- df[!is.na(df$time) & !is.na(df$response), , drop = FALSE]
          if (nrow(ends) > 0) {
            ends <- ends[order(ends[[options$patientID]], ends$time), ]
            last <- !duplicated(ends[[options$patientID]], fromLast = TRUE)
            ends <- ends[last, , drop = FALSE]
            lab_fn <- if (requireNamespace("ggrepel", quietly = TRUE))
              ggrepel::geom_text_repel else ggplot2::geom_text
            p <- p + lab_fn(
              data = ends,
              mapping = ggplot2::aes(x = .data$time, y = .data$response,
                                     label = .data[[options$patientID]]),
              size = 2.8, show.legend = FALSE, inherit.aes = FALSE
            )
          }
        }

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

        # Extract key metrics. n_eval (metrics$n) is the ONE denominator used
        # for every rate and percentage in this panel - it previously mixed
        # nrow() (including Unknown patients) for the distribution with the
        # evaluable-only ORR/DCR, so the same panel contradicted itself.
        n_total <- nrow(processed_data$waterfall)
        n_eval <- metrics$n
        n_unknown <- if (!is.null(metrics$n_unknown)) metrics$n_unknown else (n_total - n_eval)
        orr <- metrics$ORR
        dcr <- metrics$DCR

        # NA-safe display: with zero evaluable patients (all demoted to
        # Unknown) ORR/DCR are NA - the raw `orr >= 30` test here crashed the
        # whole run with "missing value where TRUE/FALSE needed".
        fmt_rate <- function(x) if (is.null(x) || is.na(x)) .("not estimable") else sprintf("%.1f%%", x)
        pct_of_eval <- function(k) if (n_eval > 0) sprintf(" (%.1f%%)", k / n_eval * 100) else ""

        count_of <- function(cat) {
          n <- metrics$summary$n[metrics$summary$category == cat]
          if (length(n) == 0 || is.na(n[1])) 0L else as.integer(n[1])
        }
        cr_count <- count_of("CR")
        pr_count <- count_of("PR")
        sd_count <- count_of("SD")
        pd_count <- count_of("PD")

        # Generate natural language summary
        summary_text <- paste0(
          "<div style='background-color: rgba(138, 155, 172, 0.06); padding: 15px; border-left: 4px solid #1b9e77; margin: 10px 0; color: inherit;'>",
          "<h4 style='color: #1b9e77; margin-top: 0;'>", .("Treatment Response Summary"), "</h4>",

          "<p><strong>", .("Analysis Overview:"), "</strong> ",
          sprintf(.("Response analysis of %d patients (%d evaluable) using threshold-based categories adapted from RECIST v1.1; this is not a full RECIST v1.1 assessment."),
                  n_total, n_eval), "</p>",

          "<p><strong>", .("Key Findings:"), "</strong></p>",
          "<ul>",
          "<li><strong>", .("Objective Response Rate (ORR):"), "</strong> ", fmt_rate(orr), " ",
          sprintf(.("(%d of %d evaluable patients achieved complete or partial response)"), cr_count + pr_count, n_eval), "</li>",
          "<li><strong>", .("Disease Control Rate (DCR):"), "</strong> ", fmt_rate(dcr), " ",
          sprintf(.("(%d of %d evaluable patients achieved response or stable disease)"), cr_count + pr_count + sd_count, n_eval), "</li>",
          "</ul>",

          "<p><strong>", .("Response Distribution:"), "</strong></p>",
          "<ul>",
          if (cr_count > 0) paste0("<li>", .("Complete Response:"), " ", cr_count, " ", .("patients"),
                                   pct_of_eval(cr_count), "</li>") else "",
          if (pr_count > 0) paste0("<li>", .("Partial Response:"), " ", pr_count, " ", .("patients"),
                                   pct_of_eval(pr_count), "</li>") else "",
          if (sd_count > 0) paste0("<li>", .("Stable Disease:"), " ", sd_count, " ", .("patients"),
                                   pct_of_eval(sd_count), "</li>") else "",
          if (pd_count > 0) paste0("<li>", .("Progressive Disease:"), " ", pd_count, " ", .("patients"),
                                   pct_of_eval(pd_count), "</li>") else "",
          if (n_unknown > 0) paste0("<li>", .("Unknown / not evaluable:"), " ", n_unknown, " ", .("patients"),
                                    " - ", .("excluded from all percentages above"), "</li>") else "",
          "</ul>"
        )

        # Add clinical interpretation (NA-safe, shared benchmark wording)
        interpretation <- private$.interpretORR(orr)

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
          "<div style='background-color: rgba(33, 152, 255, 0.07); padding: 15px; border: 1px solid #d1ecf1; border-radius: 5px; margin: 10px 0; color: inherit;'>",
          "<h4 style='color: inherit; margin-top: 0;'>", .("What This Analysis Does"), "</h4>",

          "<p>", .("The Treatment Response Analysis creates waterfall and spider plots using threshold-based response categories adapted from RECIST v1.1 (not a full RECIST v1.1 assessment)."), "</p>",
          
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
          sprintf("<li>%s CR \u{2264}%d%%, PR \u{2264}%d%%, PD \u{2265}+%d%%</li>", .("RECIST v1.1 thresholds:"), private$RECIST_CR_THRESHOLD, private$RECIST_PR_THRESHOLD, private$RECIST_PD_THRESHOLD),
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
      # Generate enhanced clinical metrics with confidence intervals ----
      .generateEnhancedClinicalMetrics = function(processed_data, metrics) {
        n_responders <- sum(processed_data$waterfall$recist_category %in% c("CR", "PR"), na.rm = TRUE)
        n_dcr <- sum(processed_data$waterfall$recist_category %in% c("CR", "PR", "SD"), na.rm = TRUE)
        # Must be the SAME denominator .calculateMetrics used for the point
        # estimate. It counts only CR/PR/SD/PD, so "Unknown"/unevaluable patients
        # are excluded; nrow() includes them. With the two out of step the printed
        # rate could fall outside its own confidence interval (e.g. ORR 50.0%
        # displayed with a 95% CI of 28.8-46.8%).
        n_total <- if (!is.null(metrics$n) && metrics$n > 0) metrics$n else
          sum(processed_data$waterfall$recist_category %in% c("CR", "PR", "SD", "PD"),
              na.rm = TRUE)

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

        # Fill the two rows created in .init() (setRow when they exist, addRow
        # as fallback for a cycle without a prior init).
        ecm <- self$results$enhancedClinicalMetrics
        orr_values <- list(
          metric = .("Objective Response Rate (ORR)"),
          value = if (is.na(metrics$ORR)) .("not estimable") else sprintf("%.1f%%", metrics$ORR),
          ci_lower = round(orr_ci[1] * 100, 1),
          ci_upper = round(orr_ci[2] * 100, 1),
          interpretation = private$.interpretORR(metrics$ORR)
        )
        dcr_values <- list(
          metric = .("Disease Control Rate (DCR)"),
          value = if (is.na(metrics$DCR)) .("not estimable") else sprintf("%.1f%%", metrics$DCR),
          ci_lower = round(dcr_ci[1] * 100, 1),
          ci_upper = round(dcr_ci[2] * 100, 1),
          interpretation = private$.interpretDCR(metrics$DCR)
        )
        if (1 %in% ecm$rowKeys) ecm$setRow(rowKey = 1, values = orr_values)
        else ecm$addRow(rowKey = 1, values = orr_values)
        if (2 %in% ecm$rowKeys) ecm$setRow(rowKey = 2, values = dcr_values)
        else ecm$addRow(rowKey = 2, values = dcr_values)

        # SAFETY CHECK: Warn if confidence intervals are extremely wide (>40 percentage points)
        # This indicates severe statistical uncertainty that makes results unreliable
        orr_ci_width <- (orr_ci[2] - orr_ci[1]) * 100
        dcr_ci_width <- (dcr_ci[2] - dcr_ci[1]) * 100

        if (!is.na(orr_ci_width) && orr_ci_width > 40) {
          private$.addNotice(
            type = "STRONG_WARNING",
            title = .("VERY WIDE CONFIDENCE INTERVAL"),
            content = sprintf(
              .("ORR 95%% CI spans %.1f percentage points (%.1f-%.1f%%). This indicates EXTREME STATISTICAL UNCERTAINTY due to small sample size. Results are NOT reliable for treatment decision-making, regulatory submissions, or publication without explicit acknowledgment of severe imprecision. The true ORR could be anywhere within this wide range. REQUIRED ACTION: Increase sample size substantially before drawing clinical conclusions. Minimum n=30-40 patients recommended for adequate precision in phase II trials. Current results should be considered preliminary screening data only."),
              round(orr_ci_width, 1), round(orr_ci[1] * 100, 1), round(orr_ci[2] * 100, 1))
          )
        }
      }

      ,
      # Generate copy-ready report sentences ----
      .generateCopyReadyReport = function(processed_data, metrics, person_time_metrics = NULL) {
        # With zero evaluable patients there is no rate to report; say so
        # instead of pasting "ORR was NA%" into someone's manuscript.
        if (is.null(metrics$n) || metrics$n == 0 || is.na(metrics$ORR)) {
          self$results$copyReadyReport$setContent(paste0(
            "<div style='background-color: rgba(33, 166, 255, 0.07); padding: 15px; border: 1px solid #0369a1; border-radius: 5px; margin: 10px 0; color: inherit;'>",
            "<h4 style='color: inherit; margin-top: 0;'>", .("Copy-Ready Report Sentences"), "</h4>",
            "<p>", .("No evaluable patients: response rates cannot be reported. See the Important Information panel for why patients were not evaluable."), "</p>",
            "</div>"))
          return()
        }

        # Same evaluable denominator the point estimates use, so the sentence a
        # user pastes into a manuscript cannot quote a rate and an interval that
        # were computed over different cohorts.
        n_patients <- metrics$n

        # Count responses by category
        response_counts <- processed_data$waterfall %>%
          dplyr::count(recist_category) %>%
          dplyr::mutate(percent = round(n / sum(n) * 100, 1))

        # dplyr::count() drops unobserved factor levels, so subsetting for an
        # absent category yields integer(0) -- not NULL, so `%||% 0` never fired.
        # sprintf() with a zero-length argument returns character(0), which paste0
        # silently collapses away: with no CR patients the entire "Main Results"
        # sentence rendered as an empty paragraph.
        count_for <- function(cat) {
          n <- response_counts$n[response_counts$recist_category == cat]
          if (length(n) == 0 || is.na(n[1])) 0L else as.integer(n[1])
        }
        cr_count <- count_for("CR")
        pr_count <- count_for("PR")

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
          "<div style='background-color: rgba(33, 166, 255, 0.07); padding: 15px; border: 1px solid #0369a1; border-radius: 5px; margin: 10px 0; color: inherit;'>",
          "<h4 style='color: inherit; margin-top: 0;'>", .("Copy-Ready Report Sentences"), "</h4>",

          "<div style='background-color: rgba(138, 155, 172, 0.08); padding: 10px; border-radius: 3px; margin: 10px 0; color: inherit;'>",
          "<h5>", .("Main Results:"), "</h5>",
          "<p style='font-family: monospace; background-color: rgba(138, 155, 172, 0.06); padding: 8px; border-radius: 3px; color: inherit;'>",
          sprintf(.("Treatment response was evaluable in %d patients. The objective response rate (ORR) was %.1f%% (%s), with %d patients achieving complete response and %d achieving partial response. The disease control rate (DCR) was %.1f%%."),
                  n_patients, metrics$ORR, orr_ci, cr_count, pr_count, metrics$DCR),
          "</p>",
          "</div>",

          "<div style='background-color: rgba(138, 155, 172, 0.08); padding: 10px; border-radius: 3px; margin: 10px 0; color: inherit;'>",
          "<h5>", .("Methods Description:"), "</h5>",
          "<p style='font-family: monospace; background-color: rgba(138, 155, 172, 0.06); padding: 8px; border-radius: 3px; color: inherit;'>",
          .("Tumor response was categorized using SIMPLIFIED threshold-based criteria adapted from RECIST v1.1 (NOT full RECIST-compliant). Categories based on percent change thresholds: CR \u{2264}-100%, PR \u{2264}-30%, SD >-30% to <+20%, PD \u{2265}+20%. This analysis does NOT include target lesion summation, new lesion detection, non-target assessment, or confirmation requirements mandated by RECIST v1.1. Response rates calculated with exact binomial confidence intervals."),
          "</p>",
          "</div>",

          "<p><small>", .("Copy these sentences directly into your manuscript or clinical report. Modify as needed for your specific context."), "</small></p>",
          "</div>"
        )

        self$results$copyReadyReport$setContent(report_text)
      }


      ,
      # Generate group comparison analysis ----
      .generateGroupComparison = function(processed_data) {
        if (is.null(self$options$groupVar) || !"patient_group" %in% names(processed_data$waterfall)) {
          return()
        }

        # EVALUABLE patients only, matching the overall ORR/DCR denominator.
        # Counting all rows put Unknown patients in the per-group denominators
        # (and the Fisher tests scored them as non-responders), so the group
        # rates could not reconcile with the headline rate.
        df <- processed_data$waterfall
        df <- df[!is.na(df$recist_category) &
                   df$recist_category %in% c("CR", "PR", "SD", "PD"), , drop = FALSE]
        if (nrow(df) == 0) return()

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
        self$results$groupComparisonTable$setNote("denom",
          .("N and all rates count evaluable patients only (CR/PR/SD/PD), the same denominator as the overall ORR/DCR; patients with an unknown category are excluded."))

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
              .("No statistically significant difference in response rates was detected; this does not establish that the rates are equal")
            }

            self$results$groupComparisonTest$addRow(rowKey = row_count, values = list(
              comparison = .("Objective Response Rate (ORR)"),
              # fisher.test() returns an odds ratio only for a 2x2 table; with
              # 3+ groups this printed "OR = NA".
              test_statistic = paste0(.("Fisher's exact test"),
                  if (!is.null(orr_test$estimate)) sprintf(", OR = %.2f", orr_test$estimate) else ""),
              p_value = round(orr_test$p.value, 4),
              interpretation = orr_interpretation
            ))
            row_count <- row_count + 1
          }

          if (!is.null(dcr_test)) {
            dcr_interpretation <- if (dcr_test$p.value < 0.05) {
              .("Statistically significant difference in disease control rates between groups")
            } else {
              .("No statistically significant difference in disease control rates was detected; this does not establish that the rates are equal")
            }

            self$results$groupComparisonTest$addRow(rowKey = row_count, values = list(
              comparison = .("Disease Control Rate (DCR)"),
              test_statistic = paste0(.("Fisher's exact test"),
                  if (!is.null(dcr_test$estimate)) sprintf(", OR = %.2f", dcr_test$estimate) else ""),
              p_value = round(dcr_test$p.value, 4),
              interpretation = dcr_interpretation
            ))
          }

          if (self$results$groupComparisonTest$rowCount > 1) {
            self$results$groupComparisonTest$setNote("multiplicity",
              .("Two Fisher's exact tests (ORR and DCR) are reported with unadjusted p-values; interpret them jointly rather than as independent evidence."))
          }

          # A skipped test used to leave a silent gap in the table.
          if (is.null(orr_test) || is.null(dcr_test)) {
            self$results$groupComparisonTest$setNote("skipped",
              .("A Fisher's exact test was not run where every patient fell into the same class (e.g. no responders in any group), so there is no contrast to test."))
          }
        } else {
          self$results$groupComparisonTest$setNote("skipped",
            .("Group comparison tests need at least two groups, each with at least one evaluable patient."))
        }
      }

      ,
      # Generate enhanced clinical glossary ----
      .generateClinicalGlossary = function() {
        glossary_text <- paste0(
          "<div style='background-color: rgba(138, 155, 172, 0.06); padding: 15px; border: 1px solid #dee2e6; border-radius: 5px; margin: 10px 0; color: inherit;'>",
          "<h4 style='color: inherit; margin-top: 0;'>", .("Clinical Terms & Definitions"), "</h4>",

          "<div style='display: grid; grid-template-columns: 1fr 1fr; gap: 15px;'>",

          "<div>",
          "<h5 style='color: #6c757d; margin-bottom: 10px;'>", .("Response Metrics"), "</h5>",
          "<ul style='margin: 0; padding-left: 15px; line-height: 1.6;'>",
          "<li><strong>ORR (Objective Response Rate - Unconfirmed):</strong> ", .("Percentage of patients achieving threshold-based CR (\u{2264}-100%) or PR (\u{2264}-30%) without RECIST v1.1 confirmation requirement. May overestimate true confirmed ORR."), "</li>",
          "<li><strong>DCR (Disease Control Rate - Unconfirmed):</strong> ", .("Percentage achieving threshold-based response or stable disease (CR + PR + SD) without confirmation. Exploratory endpoint only."), "</li>",
          "<li><strong>Best Response (Simplified):</strong> ", .("Most favorable (most negative) percent change from baseline. NOT equivalent to RECIST v1.1 'Best Overall Response' which requires confirmation."), "</li>",
          "<li><strong>Person-Time:</strong> ", .("Total time patients are followed, accounting for different follow-up durations"), "</li>",
          "</ul>",
          "</div>",

          "<div>",
          "<h5 style='color: #6c757d; margin-bottom: 10px;'>", .("Response Categories (Simplified Threshold-Based)"), "</h5>",
          "<ul style='margin: 0; padding-left: 15px; line-height: 1.6;'>",
          "<li><strong>CR (Complete Response - Threshold):</strong> ", .("\u{2264}-100% change from baseline (simplified criterion, NOT full RECIST CR which requires disappearance of ALL lesions including non-target)"), "</li>",
          "<li><strong>PR (Partial Response - Threshold):</strong> ", .("\u{2264}-30% change from baseline (simplified criterion, NOT full RECIST PR which requires target lesion sum calculation and no new lesions)"), "</li>",
          "<li><strong>SD (Stable Disease - Threshold):</strong> ", .("Between -30% and +20% change (simplified criterion)"), "</li>",
          "<li><strong>PD (Progressive Disease - Threshold):</strong> ", .("\u{2265}+20% change from baseline (simplified criterion, NOT full RECIST PD which includes new lesion detection and non-target progression)"), "</li>",
          "</ul>",
          "</div>",

          "</div>",

          "<div style='margin-top: 15px;'>",
          "<h5 style='color: #6c757d; margin-bottom: 10px;'>", .("Statistical Terms"), "</h5>",
          "<ul style='margin: 0; padding-left: 15px; line-height: 1.6;'>",
          "<li><strong>95% CI (Confidence Interval):</strong> ", .("Range of values compatible with the observed data; over repeated studies, 95% of such intervals contain the true population parameter"), "</li>",
          "<li><strong>Fisher's Exact Test:</strong> ", .("Statistical test for comparing response rates between groups"), "</li>",
          "<li><strong>Binomial CI:</strong> ", .("Exact confidence interval for proportions (more accurate than normal approximation)"), "</li>",
          "<li><strong>Waterfall Plot:</strong> ", .("Bar chart showing best response for each patient, sorted by magnitude"), "</li>",
          "<li><strong>Spider Plot:</strong> ", .("Line graph showing individual patient response trajectories over time"), "</li>",
          "</ul>",
          "</div>",

          "<div style='margin-top: 15px; padding: 10px; background-color: rgba(33, 152, 239, 0.13); border-radius: 3px; color: inherit;'>",
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
        # NA-safe display (all patients unevaluable -> ORR/DCR are NA)
        orr_display <- if (is.na(metrics$ORR)) .("not estimable") else paste0(metrics$ORR, "%")
        dcr_display <- if (is.na(metrics$DCR)) .("not estimable") else paste0(metrics$DCR, "%")

        # Sample size adequacy assessment
        sample_size_assessment <- if (n_patients < 20) {
          .("Very small sample size (n<20): Results should be interpreted with extreme caution. Confidence intervals will be very wide.")
        } else if (n_patients < 50) {
          .("Small sample size (n<50): Results provide preliminary evidence but should be confirmed in larger studies.")
        } else if (n_patients < 100) {
          .("Moderate sample size: Results provide reasonable evidence for preliminary conclusions.")
        } else {
          .("Adequate sample size (n>=100): confidence intervals for ORR and DCR will be comparatively narrow.")
        }

        significance_text <- paste0(
          "<div style='background-color: rgba(251, 207, 33, 0.25); padding: 15px; border-left: 4px solid #f59e0b; margin: 10px 0; color: inherit;'>",
          "<h4 style='color: inherit; margin-top: 0;'>", .("Clinical Significance Assessment"), "</h4>",

          "<h5>", .("Response Rate Interpretation:"), "</h5>",
          "<ul>",
          "<li><strong>", .("ORR"), " (", orr_display, "): </strong>", orr_interpretation, "</li>",
          "<li><strong>", .("DCR"), " (", dcr_display, "): </strong>", dcr_interpretation, "</li>",
          "</ul>",

          "<h5>", .("Sample Size Adequacy:"), "</h5>",
          "<p>", sample_size_assessment, "</p>",

          "<h5>", .("Clinical Context:"), "</h5>",
          "<ul>",
          "<li>", .("ORR <15%: below the activity range conventionally cited for phase II single-agent studies"), "</li>",
          "<li>", .("ORR 15-30%: within the moderate-activity range conventionally cited for phase II single-agent studies"), "</li>",
          "<li>", .("ORR >30%: above the activity range conventionally cited for phase II single-agent studies"), "</li>",
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
          "<div style='background-color: rgba(33, 225, 92, 0.07); padding: 15px; border: 1px solid #16a34a; border-radius: 5px; margin: 10px 0; color: inherit;'>",
          "<h4 style='color: inherit; margin-top: 0;'>", .("Guided Analysis"), "</h4>",

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

          "<div style='background-color: rgba(33, 126, 249, 0.16); padding: 10px; border-radius: 3px; margin: 10px 0; color: inherit;'>",
          "<h5 style='margin-top: 0;'>", .("Quick Tips:"), "</h5>",
          "<ul style='margin: 5px 0; margin-left: 20px;'>",
          "<li>", .("Most studies use 'Percentage Changes' format"), "</li>",
          "<li>", .("Enable 'Show RECIST Thresholds' for clinical interpretation"), "</li>",
          "<li>", .("Use a group variable to compare biomarker-defined cohorts"), "</li>",
          "<li>", .("Enable confidence intervals for reporting response rates"), "</li>",
          "</ul>",

          "</div>",

          "</div>"
        )

        self$results$guidedAnalysis$setContent(guided_text)
      }

      ,
      # Helper functions for interpretation ----
      # NOTE: These benchmarks are GENERAL guidelines for phase II oncology trials
      # Actual thresholds vary by tumor type, line of therapy, and standard of care
      .interpretORR = function(orr) {
        if (is.null(orr) || is.na(orr)) return(.("Not available"))
        if (orr >= 30) {
          return(.("Promising activity for single-agent therapy (general benchmark; verify against tumor-specific thresholds)"))
        } else if (orr >= 15) {
          return(.("Moderate activity by general phase II benchmarks (context-dependent)"))
        } else {
          return(.("Limited activity by general phase II standards (may still be clinically meaningful in refractory settings)"))
        }
      },

      .interpretDCR = function(dcr) {
        if (is.null(dcr) || is.na(dcr)) return(.("Not available"))
        if (dcr >= 70) {
          return(.("Excellent disease control (general benchmark; varies by tumor type and treatment setting)"))
        } else if (dcr >= 50) {
          return(.("Good disease control for exploratory cohort"))
        } else {
          return(.("Limited disease control by general benchmarks"))
        }
      }


    ), # End of private list
    public = list(
      #' @description
      #' Generate R source code for Waterfall Plot analysis
      #' @return Character string with R syntax for reproducible analysis
      asSource = function() {
          responseVar <- self$options$responseVar

          if (is.null(responseVar))
              return('')

          # Build the argument list in option-declaration order.
          #
          # Every variable-name option (single OptionVariable or multi-variable
          # OptionVariables) is emitted as a deparse()'d string literal. deparse()
          # produces valid, fully-escaped R for names containing spaces, quotes or
          # backslashes (e.g. `Tumor Grade`); jmvcore's default sourcify would emit
          # some of these as bare, unquoted symbols and yield invalid syntax.
          # Detecting the option by CLASS (not by name) means any variable option
          # added later is escaped automatically.
          #
          # Variables are NOT re-emitted through private$.asArgs() - doing so
          # previously duplicated them in the generated syntax (the "double
          # variables" bug). All non-variable options keep jmvcore's per-option
          # sourcify so formatting stays consistent with jamovi.
          args <- character(0)
          for (option in private$.options$options) {
              if (option$name == 'data')
                  next
              if (inherits(option, 'OptionVariable') || inherits(option, 'OptionVariables')) {
                  val <- option$value
                  if (!is.null(val) && length(val) > 0)
                      args <- c(args, paste0(option$name, ' = ',
                                             paste0(deparse(val), collapse = '')))
              } else {
                  as <- private$.sourcifyOption(option)
                  if (!identical(as, ''))
                      args <- c(args, as)
              }
          }

          # Get package name dynamically
          pkg_name <- utils::packageName()
          if (is.null(pkg_name)) pkg_name <- "ClinicoPath"  # fallback

          # Build complete function call
          paste0(pkg_name, '::waterfall(\n    data = data,\n    ',
                 paste(args, collapse = ',\n    '), ')')
      }
    ) # End of public list
)
