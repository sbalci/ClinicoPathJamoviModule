#' @title Treatment Response Analysis (Waterfall) Class
#'
#' @description R6 class for performing treatment response analysis using waterfall plots.
#' @name waterfallClass
#' @importFrom R6 R6Class
#' @importFrom withr local_seed
#' @import jmvcore
#' @return An \code{R6} class generator object for the \code{waterfallClass} backend; used internally by the jamovi analysis wrapper and not called directly.
waterfallClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "waterfallClass",
    inherit = waterfallBase,
    private = list(

        # RECIST v1.1 Constants ----
        RECIST_CR_THRESHOLD = -100,  # Complete Response threshold (\u2264-100%)
        RECIST_PR_THRESHOLD = -30,   # Partial Response threshold (\u2264-30%)
        RECIST_PD_THRESHOLD = 20,    # Progressive Disease threshold (\u2265+20%, inclusive)
        RECIST_SD_MIN = -30,         # Stable Disease minimum (-30%)
        RECIST_SD_MAX = 20,          # Stable Disease maximum (20%)
        # Tolerance for the inclusive thresholds. Percent changes computed from raw
        # sums carry rounding error (a sum of exactly 1.2 x the nadir gave a +19.99999999999999%
        # increase), which silently dropped ~40% of exact-boundary progressions.
        RECIST_TOL = 1e-8,

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
        # Patients whose change below -100% was capped this run (for the report text)
        .nCapped = 0L,

        # Add a notice to the collection
        # library-audit 2026-09-16 OncoPath [INFO] REJECTED: no native notice element - type: Notice fails the
        #   .r.yaml schema, type: Notification builds no results object (guide section 13)
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

          # library-audit 2026-09-16 OncoPath [INFO] DONE: titles inherit the pane colour - fixed hues fell to
          #   2.7-2.9:1 on the dark theme; the translucent tint and the border carry the severity
          typeStyles <- list(
            ERROR = list(bgcolor = "rgba(220, 38, 38, 0.10)", border = "#fca5a5", icon = ""),
            STRONG_WARNING = list(bgcolor = "rgba(234, 88, 12, 0.10)", border = "#fdba74", icon = ""),
            WARNING = list(bgcolor = "rgba(202, 138, 4, 0.12)", border = "#fde047", icon = ""),
            INFO = list(bgcolor = "rgba(37, 99, 235, 0.08)", border = "#93c5fd", icon = "")
          )

          html <- "<div style='margin: 10px 0;'>"

          for (notice in private$.noticeList) {
            style <- typeStyles[[notice$type]] %||% typeStyles$INFO

            html <- paste0(html,
              "<div style='background-color: ", style$bgcolor, "; ",
              "border-left: 4px solid ", style$border, "; ",
              "padding: 12px; margin: 8px 0; border-radius: 4px;'>",
              "<strong style='color: inherit;'>",
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
          # After a complete response the nadir is zero, so a relative increase is
          # undefined; any burden above zero is then the lesion reappearing, which
          # RECIST v1.1 counts as progression. (Returning NA here censored every
          # relapsing CR and pushed the KM median to "not reached".)
          rel_increase <- ifelse(nadir_burden > 0,
                                 (burden - nadir_burden) / nadir_burden * 100,
                                 ifelse(burden > 0, Inf, NA_real_))

          times[!is.na(rel_increase) &
                  rel_increase >= private$RECIST_PD_THRESHOLD - private$RECIST_TOL &
                  times > after_time]
        },

        # Time of the first progression over the nadir, the baseline included (an
        # implicit 0% at time 0 when the patient has no time-0 row, as percent-change
        # data often lack one). Inf when the patient never progressed. RECIST v1.1:
        # a progression precludes a later CR, PR or SD, so assessments after this
        # time do not count towards the best response.
        .firstProgression = function(times, values) {
          ok <- !is.na(times) & !is.na(values)
          t <- times[ok]; v <- values[ok]
          if (!any(t == 0)) { t <- c(0, t); v <- c(0, v) }
          pt <- private$.progressionTimes(t, v, 0)
          if (length(pt) > 0) min(pt) else Inf
        },

        # Calculate time-to-event metrics
        #
        # One record per patient from their measured assessments:
        #   - time to first response: first post-baseline assessment at PR or better
        #   - duration of response: from first response to progression over the
        #     NADIR (RECIST v1.1: ">=20% increase taking as reference the smallest
        #     sum on study"; reappearance after CR is progression), or censored at
        #     the last MEASURED assessment - a scheduled visit without a measurement
        #     is not follow-up in response.
        # Responders are the patients whose FINAL category (after demotions and the
        # category override) is CR/PR, so TTR/DoR describe the same patients as the
        # ORR numerator. `final_categories` is data.frame(<patientID>, category).
        .calculateTimeToEventMetrics = function(df, patientID, timeVar, responseVar,
                                                final_categories = NULL) {
          if (is.null(timeVar) || !timeVar %in% names(df)) {
            return(NULL)
          }

          tryCatch({
            times_all <- jmvcore::toNumeric(df[[timeVar]])
            values_all <- jmvcore::toNumeric(df[[responseVar]])
            pr_thr <- private$RECIST_PR_THRESHOLD + private$RECIST_TOL

            one_patient <- function(t, v) {
              ok <- !is.na(t) & !is.na(v)
              t <- t[ok]; v <- v[ok]
              o <- order(t); t <- t[o]; v <- v[o]
              post <- t > 0 & t <= private$.firstProgression(t, v)
              best <- if (any(post)) min(v[post]) else NA_real_
              ttb <- if (any(post)) t[post][which.min(v[post])] else NA_real_
              last <- if (length(t)) max(t) else NA_real_
              resp <- post & v <= pr_thr
              if (!any(resp)) {
                return(c(ttr = NA_real_, dor = NA_real_, event = NA_real_,
                         best = best, ttb = ttb, last = last))
              }
              first <- min(t[resp])
              prog <- private$.progressionTimes(t, v, first)
              if (length(prog) > 0)
                c(ttr = first, dor = min(prog) - first, event = 1, best = best, ttb = ttb, last = last)
              else
                c(ttr = first, dor = last - first, event = 0, best = best, ttb = ttb, last = last)
            }

            ids <- df[[patientID]]
            keep_id <- !is.na(ids)
            per <- lapply(split(seq_along(ids)[keep_id], as.character(ids[keep_id])),
                          function(i) one_patient(times_all[i], values_all[i]))
            all_patients <- data.frame(
              pid = names(per),
              time_to_first_response = vapply(per, `[[`, numeric(1), "ttr"),
              duration_of_response = vapply(per, `[[`, numeric(1), "dor"),
              duration_censored = vapply(per, `[[`, numeric(1), "event"),
              best_response = vapply(per, `[[`, numeric(1), "best"),
              time_to_best_response = vapply(per, `[[`, numeric(1), "ttb"),
              last_assessment = vapply(per, `[[`, numeric(1), "last"),
              stringsAsFactors = FALSE, row.names = NULL
            )
            names(all_patients)[1] <- patientID

            # Responders: final category CR/PR and a measured response time.
            final_responder <- if (!is.null(final_categories)) {
              resp_ids <- as.character(final_categories[[1]][
                final_categories$category %in% c("CR", "PR")])
              all_patients[[patientID]] %in% resp_ids
            } else {
              rep(TRUE, nrow(all_patients))
            }
            metrics <- all_patients[final_responder &
                                      !is.na(all_patients$time_to_first_response), , drop = FALSE]
            n_without_time <- if (!is.null(final_categories))
              sum(final_categories$category %in% c("CR", "PR")) - nrow(metrics) else 0L

            # Kaplan-Meier median duration of response (censoring-aware), with its
            # 95% CI. The crude median ignores responders still in response.
            km_median_dor <- NA_real_; km_lcl <- NA_real_; km_ucl <- NA_real_
            n_dor_events <- if (nrow(metrics) > 0) sum(metrics$duration_censored == 1) else 0L
            if (nrow(metrics) >= 2 && requireNamespace("survival", quietly = TRUE)) {
              km <- tryCatch({
                fit <- survival::survfit(
                  survival::Surv(metrics$duration_of_response, metrics$duration_censored) ~ 1)
                summary(fit)$table[c("median", "0.95LCL", "0.95UCL")]
              }, error = function(e) c(NA_real_, NA_real_, NA_real_))
              km_median_dor <- unname(km[1]); km_lcl <- unname(km[2]); km_ucl <- unname(km[3])
            }

            summary_stats <- list(
              median_time_to_response = if (nrow(metrics)) stats::median(metrics$time_to_first_response) else NA_real_,
              median_duration_of_response = if (nrow(metrics)) stats::median(metrics$duration_of_response) else NA_real_,
              km_median_duration_of_response = km_median_dor,
              km_median_lcl = km_lcl,
              km_median_ucl = km_ucl,
              n_duration_events = n_dor_events,
              median_time_to_best_response = if (nrow(metrics)) stats::median(metrics$time_to_best_response, na.rm = TRUE) else NA_real_,
              n_responders = nrow(metrics),
              n_with_duration_data = sum(!is.na(metrics$duration_of_response)),
              n_responders_without_time = max(0L, as.integer(n_without_time)),
              any_followup_after_response = nrow(metrics) > 0 &&
                any(metrics$duration_of_response > 0, na.rm = TRUE)
            )

            list(
              by_patient = metrics,
              all_patients = all_patients,
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
          # First non-missing value per patient (in time order), not the first row.
          timeVar <- self$options$timeVar
          patient_value <- function(var, which) {
            f <- private$.resolvePatientField(source_df, src_pid_name, var, timeVar, which = which)
            attr(f$value, "conflicts") <- f$conflicts
            f$value[match(as.character(wdf[[pidCol]]), as.character(f$ids))]
          }
          if (!is.null(confVar) && confVar %in% names(source_df)) {
            f <- private$.resolvePatientField(source_df, src_pid_name, confVar, timeVar)
            wdf$confirm_status <- as.character(
              f$value[match(as.character(wdf[[pidCol]]), as.character(f$ids))])
            if (length(f$conflicts) > 0) {
              private$.addNotice("WARNING", .("CONFLICTING CONFIRMATION VALUES"), sprintf(
                .("%d patient(s) have more than one confirmation value across their rows: %s. The first recorded value is used for the marker."),
                length(f$conflicts), paste(utils::head(f$conflicts, 10), collapse = ", ")))
            }
          }
          # On-treatment / ongoing status is a status AT the last assessment, so the
          # latest recorded value is used (a per-visit "Yes ... No" means stopped).
          if (!is.null(ongVar) && ongVar %in% names(source_df))
            wdf$ongoing_flag <- private$.coerceOngoing(patient_value(ongVar, "last"))
          wdf
        },

        # Override computed RECIST category with a user-supplied category variable.
        # Matches by patient-ID VALUE; only rows with a supplied value are changed.
        # Expected values: CR / PR / SD / PD (case-insensitive).
        .applyCategoryOverride = function(wdf, source_df, pidCol, categoryVar, timeVar = NULL) {
          if (is.null(categoryVar) || is.null(source_df) ||
              !(categoryVar %in% names(source_df)) || !(pidCol %in% names(wdf)) ||
              !("recist_category" %in% names(wdf)))
            return(wdf)
          src_pid_name <- self$options$patientID
          if (is.null(src_pid_name) || !(src_pid_name %in% names(source_df)))
            return(wdf)
          # Patient-level value: the first non-missing one in time order. match()
          # on the patient ID read only the patient's FIRST row, so an override
          # recorded on a follow-up row (e.g. a new lesion at week 12) was ignored.
          field <- private$.resolvePatientField(source_df, src_pid_name, categoryVar, timeVar,
                                                normalize = function(x) toupper(trimws(x)))
          if (length(field$conflicts) > 0) {
            private$.addNotice(
              type = "WARNING",
              title = .("CONFLICTING CATEGORY OVERRIDES"),
              content = sprintf(
                .("%d patient(s) have more than one response category override across their rows: %s. The first recorded value is used. Enter one category per patient."),
                length(field$conflicts), paste(utils::head(field$conflicts, 10), collapse = ", "))
            )
          }
          idx <- match(as.character(wdf[[pidCol]]), as.character(field$ids))
          user_cat <- toupper(trimws(as.character(field$value)[idx]))

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

            # One value per patient: the first non-missing one in time order (a
            # value on a follow-up row was lost when only the first row was read).
            long <- do.call(rbind, lapply(vars, function(v) {
                f <- private$.resolvePatientField(src, pid, v, plotData$options$timeVar)
                data.frame(
                    bar   = seq_len(nrow(df)),
                    track = v,
                    value = as.character(f$value)[match(as.character(df[[pid]]),
                                                        as.character(f$ids))],
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
              shape_pool <- c(16, 1, 17, 2, 15, 0)   # six distinct shapes (see .generateVisualizations)
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
                    private$.nCapped <- length(ids)
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
                                                  patientID, timeVar,
                                                  responseVar = NULL,
                                                  inputType = "percentage") {
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

          no_evaluable_notice <- function() {
            private$.addNotice(
              type = "ERROR",
              title = .("NO EVALUABLE PATIENTS"),
              content = sprintf(
                .("None of the %d patients supplied could be evaluated for response. Every response value was missing, non-numeric, lacked a usable baseline, or had no post-baseline assessment, so no rates or categories can be produced. Check that the response variable holds numeric values and, for raw measurements, that each patient has a time = 0 baseline and at least one later measurement."),
                length(all_ids))
            )
          }

          if (length(kept_ids) == 0) {
            no_evaluable_notice()
            return(waterfall_data)
          }

          # Patients with no post-baseline ASSESSMENT: a later row counts only when
          # its measurement is present. Counting rows by time alone let a scheduled
          # visit with a missing measurement pass, and the patient became SD at 0%.
          if (!is.null(timeVar) && timeVar %in% names(source_df)) {
            tv <- jmvcore::toNumeric(source_df[[timeVar]])
            measured <- if (!is.null(responseVar) && responseVar %in% names(source_df))
              !is.na(jmvcore::toNumeric(source_df[[responseVar]])) else rep(TRUE, length(tv))
            post <- stats::aggregate(
              list(n_post = !is.na(tv) & tv > 0 & measured),
              by = list(pid = source_df[[patientID]]), FUN = sum)
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
                  .("%d patient(s) have a baseline but no post-baseline measurement (no later row, or the later measurements are missing) and are therefore not response-evaluable: %s. They are reported as \"Unknown\" rather than as stable disease, so they do not inflate the disease control rate."),
                  length(idx),
                  paste(utils::head(as.character(waterfall_data[[patientID]][idx]), 10),
                        collapse = ", "))
              )
            }
          }

          # Reported after the demotions so the stated denominator is the one the
          # rates use (patients shown as Unknown are not in it).
          if (length(dropped) > 0) {
            # The reason depends on the input type: raw measurements need a usable
            # time = 0 baseline, while a percentage patient is dropped only when
            # every response value is missing.
            template <- if (identical(inputType, "raw"))
              .("%d of %d patients were excluded from the response analysis because a usable baseline could not be established (baseline missing, zero, or non-numeric). Excluded: %s. All rates below are computed over the %d evaluable patients, so they are NOT intention-to-treat.")
            else
              .("%d of %d patients were excluded from the response analysis because they have no non-missing response value. Excluded: %s. All rates below are computed over the %d evaluable patients, so they are NOT intention-to-treat.")
            private$.addNotice(
              type = "WARNING",
              title = .("PATIENTS EXCLUDED"),
              content = sprintf(template,
                length(dropped), length(all_ids),
                paste(utils::head(as.character(dropped), 10), collapse = ", "),
                sum(!is.na(waterfall_data$response)))
            )
          }

          # Checked after the demotions: a cohort can have rows for every patient
          # and still leave nobody evaluable.
          if (all(is.na(waterfall_data$response))) {
            no_evaluable_notice()
          }

          # (Small-cohort messaging lives in .processAndAnalyzeData, which runs
          # after the demotions above and counts evaluable patients.)

          waterfall_data
        },

        # Scale diagnostics for percentage input. Each mistake below runs silently and
        # produces confident but wrong categories:
        #   - proportions (Excel percent format): -0.35 for -35%, so every patient is SD
        #   - values multiplied by 100: -3500 for -35%, capped to -100% = CR
        #   - raw measurements left under the default "Percentage Changes" input
        .checkPercentageScale = function(df, patientID, responseVar, timeVar = NULL) {
          v <- jmvcore::toNumeric(df[[responseVar]])
          x <- v[!is.na(v)]
          if (length(x) < 3) return(invisible(NULL))
          nz <- x[x != 0]

          small <- abs(nz) <= 1
          # A partial mix counts only strictly fractional values: rounded percent
          # data legitimately contain integer changes of -1 and +1.
          fractional <- abs(nz) < 1
          if (length(nz) >= 3 && all(small)) {
            private$.addNotice("STRONG_WARNING", .("PERCENT CHANGE LOOKS LIKE PROPORTIONS"),
              .("Every response value lies between -1 and 1. Percent change must be entered as -35 for a 35% decrease; these look like proportions (for example from a spreadsheet percent format), which makes every patient stable disease. Multiply the values by 100."))
          } else if (sum(fractional) >= 3 && mean(fractional) >= 0.25) {
            private$.addNotice("WARNING", .("SOME VALUES LOOK LIKE PROPORTIONS"),
              sprintf(.("%d of %d non-zero response values lie strictly between -1 and 1, while the others look like percentages. If some rows were entered as proportions (-0.35 for -35%%), multiply those rows by 100."),
                      sum(fractional), length(nz)))
          }

          # Needs a value below -100 (impossible shrinkage) as well as a large typical
          # magnitude; progression-heavy data alone (+110 .. +240) are not "x100".
          if (length(nz) >= 3 &&
              ((any(x < -100) && stats::median(abs(nz)) > 100) || mean(x < -100 | x > 500) >= 0.25)) {
            private$.addNotice("STRONG_WARNING", .("PERCENT CHANGE LOOKS MULTIPLIED BY 100"),
              sprintf(.("The typical absolute response value is %.0f, and a tumour cannot shrink by more than 100%%. The values look multiplied by 100 (for example -3500 for -35%%); values below -100 are capped to -100%% and counted as complete responses. Check the scale of the response column."),
                      stats::median(abs(nz))))
          }

          # Baseline rows coded at the first visit (study day 1, visit 1) instead of
          # time 0 are read as assessments, so their 0% takes part in the best response.
          if (!is.null(timeVar) && timeVar %in% names(df)) {
            tvv <- jmvcore::toNumeric(df[[timeVar]])
            okr <- !is.na(tvv) & !is.na(v) & !is.na(df[[patientID]])
            if (any(okr)) {
              ord <- order(tvv[okr])
              pid_o <- as.character(df[[patientID]][okr])[ord]
              first_row <- !duplicated(pid_o)
              t_first <- tvv[okr][ord][first_row]
              v_first <- v[okr][ord][first_row]
              if (length(t_first) >= 2 && all(v_first == 0) && all(t_first != 0)) {
                private$.addNotice("STRONG_WARNING", .("BASELINE NOT AT TIME 0"),
                  sprintf(.("Every patient's earliest row is 0%% at time %s rather than time 0. If these rows are baselines, recode the baseline time as 0: rows after time 0 are treated as assessments, so a 0%% baseline row takes part in the best response and hides progression."),
                          paste(unique(t_first), collapse = ", ")))
              }
            }
          }

          raw_like <- FALSE
          if (!is.null(timeVar) && timeVar %in% names(df)) {
            tv <- jmvcore::toNumeric(df[[timeVar]])
            at0 <- !is.na(tv) & tv == 0 & !is.na(v)
            # A percent change at the time = 0 baseline is 0 by definition.
            raw_like <- sum(at0) >= 2 && mean(v[at0] != 0) >= 0.5
          }
          if (raw_like) {
            private$.addNotice("STRONG_WARNING", .("VALUES LOOK LIKE RAW MEASUREMENTS"),
              .("The data input type is Percentage Changes, but the time = 0 values are not 0%: these look like raw tumour measurements. Set Data Input Type to Raw Measurements so percent changes are computed from the baseline."))
          } else if (is.null(timeVar) && all(x >= 0) && any(x > 0)) {
            # (>= 0: raw sizes include 0 mm for a complete response)
            private$.addNotice("WARNING", .("NO NEGATIVE CHANGES"),
              .("Every response value is positive, so no patient shrank at all. If these are raw tumour sizes rather than percent changes, set Data Input Type to Raw Measurements and add a time variable."))
          }
          invisible(NULL)
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
          tol <- private$RECIST_TOL
          cr <- private$RECIST_CR_THRESHOLD + tol
          pr <- private$RECIST_PR_THRESHOLD + tol
          pd <- private$RECIST_PD_THRESHOLD - tol
          factor(
            dplyr::case_when(
              is.na(response) ~ "Unknown",
              response <= cr ~ "CR",
              response <= pr ~ "PR",
              response < pd ~ "SD",
              response >= pd ~ "PD",
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
              "<br>", sprintf(.("The following measurements will be capped at %d%%:"), private$RECIST_CR_THRESHOLD),
              safe_invalid_shrinkage,
              "<br><br>", .("Recommended actions:"),
              "<br>1. ", .("Verify data entry for calculation errors"),
              "<br>2. ", .("Check if baseline measurements are correct"),
              "<br>3. ", .("Confirm percentage calculation method: ((current - baseline) / baseline) \u00d7 100"),
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




        # (Sample size is reported once, on the EVALUABLE count, by the SMALL SAMPLE
        # notices in .processAndAnalyzeData; a second message here counted rows'
        # patients before exclusions and repeated it.)

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
              # A measurement of 0 is a legitimate complete disappearance; only a
              # negative size is impossible (a zero BASELINE is handled as an
              # exclusion with its own notice).
              if (any(response_values < 0, na.rm = TRUE)) {
                validation_messages <- c(validation_messages,
                  paste0("<br>", .("Warning: Some measurements are negative, which is not a possible tumour size. Please verify these values.")))
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
      # One value per patient for a patient-level field (group, category override,
      # confirmation, ongoing flag). The FIRST non-missing value in time order is used,
      # so a value recorded on a follow-up row is not lost behind an empty baseline
      # row; patients whose non-missing values disagree are returned in $conflicts.
      .resolvePatientField = function(df, patientID, var, timeVar = NULL,
                                      which = c("first", "last"), normalize = NULL) {
        which <- match.arg(which)
        ids <- df[[patientID]]
        v <- df[[var]]
        keep <- !is.na(ids) & !is.na(v) & trimws(as.character(v)) != ""
        o <- if (!is.null(timeVar) && timeVar %in% names(df))
          order(jmvcore::toNumeric(df[[timeVar]]), na.last = TRUE) else seq_along(ids)
        o <- o[keep[o]]
        pick <- !duplicated(ids[o], fromLast = identical(which, "last"))
        # Conflicts are judged on the normalised value (e.g. "PD" and "pd " agree).
        key <- as.character(v[keep])
        if (!is.null(normalize)) key <- normalize(key)
        n_distinct <- tapply(key, as.character(ids[keep]), function(x) length(unique(x)))
        list(
          ids = ids[o][pick],
          value = v[o][pick],
          conflicts = names(n_distinct)[n_distinct > 1]
        )
      },

      # process validated data ----
      #
      # One processing path for every cohort size. There used to be a separate
      # "large dataset" path above 100 rows with its own copy of the best-response
      # and group logic; the copies drifted (patients duplicated when their group
      # changed between rows, a numeric group crashing the plot), and the vectorised
      # dplyr code below handles tens of thousands of rows in well under a second.
      .processData = function(df, patientID, inputType, responseVar, timeVar = NULL, groupVar = NULL) {

        if (is.null(patientID) || is.null(responseVar)) {
          return(list(
            error = TRUE,
            message = .("Patient ID and response variables are required")
          ))
        }

        has_time <- !is.null(timeVar) && timeVar %in% names(df)
        df[[responseVar]] <- jmvcore::toNumeric(df[[responseVar]])
        if (has_time) df[[timeVar]] <- jmvcore::toNumeric(df[[timeVar]])

        if (inputType == "raw") {
          if (has_time) {
            # Percent change from each patient's time = 0 measurement.
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
            # Validation requires a time variable for raw input; kept for direct calls.
            processed_df <- df %>%
              dplyr::group_by(!!rlang::sym(patientID)) %>%
              dplyr::mutate(
                baseline = dplyr::first(!!rlang::sym(responseVar)),
                response = ((!!rlang::sym(responseVar) - baseline) / baseline) * 100
              ) %>%
              dplyr::ungroup()
          }

          if (nrow(processed_df) == 0) {
            return(list(
              error = TRUE,
              message = .("No data remaining after processing. Check baseline measurements and data format.")
            ))
          }
        } else {
          processed_df <- df
          processed_df$response <- df[[responseVar]]
        }

        # Best response = the most negative change over POST-BASELINE assessments.
        # With a time variable the time = 0 row is the baseline itself (0% by
        # definition for raw input, and the format the welcome text suggests for
        # percentage input). Letting it compete in the minimum capped every
        # patient's best response at 0%, so a tumour that only grew was reported as
        # SD, PD could not occur and the disease control rate was inflated.
        post <- !is.na(processed_df$response)
        if (has_time) {
          tv <- processed_df[[timeVar]]
          post <- post & !is.na(tv) & tv > 0
          # ...and only up to the first progression (RECIST v1.1: a progression
          # precludes a later CR, PR or SD). A patient who progressed at the first
          # scan and shrank later was scored SD or PR.
          ids_chr <- as.character(processed_df[[patientID]])
          first_pd <- vapply(split(seq_len(nrow(processed_df)), ids_chr), function(i)
            private$.firstProgression(tv[i], processed_df$response[i]), numeric(1))
          post <- post & tv <= first_pd[ids_chr]
        }
        df_waterfall <- processed_df[post, , drop = FALSE] %>%
          dplyr::group_by(!!rlang::sym(patientID)) %>%
          dplyr::slice_min(response, with_ties = FALSE, n = 1) %>%
          dplyr::ungroup()

        # A patient with a usable value but no post-baseline one (a baseline row
        # only, or follow-up rows whose measurement is missing) is not
        # response-evaluable: keep them as "Unknown" instead of dropping them.
        # Patients with no usable value at all are reported as excluded later.
        with_value <- unique(processed_df[[patientID]][!is.na(processed_df$response)])
        no_post <- setdiff(with_value, df_waterfall[[patientID]])
        if (length(no_post) > 0) {
          unevaluable <- data.frame(no_post, response = NA_real_, stringsAsFactors = FALSE)
          names(unevaluable)[1] <- patientID
          df_waterfall <- dplyr::bind_rows(df_waterfall, unevaluable)
        }

        if (nrow(df_waterfall) == 0) {
          return(list(
            error = TRUE,
            message = .("No patients with valid response data found.")
          ))
        }

        df_waterfall$recist_category <- private$.categorizeRECIST(df_waterfall$response)

        # Several rows per patient are reduced to one value; say so when the data
        # give no time order to explain it (or repeat a visit time).
        valued <- processed_df[!is.na(processed_df$response), , drop = FALSE]
        if (!has_time) {
          n_rows <- table(valued[[patientID]])
          multi <- names(n_rows)[n_rows > 1]
          if (length(multi) > 0) {
            # Without a time order a 0% baseline row cannot be told apart from a
            # follow-up, so it competes in the minimum and hides progression.
            private$.addNotice(
              type = "STRONG_WARNING",
              title = .("SEVERAL ROWS PER PATIENT"),
              content = sprintf(
                .("%d patient(s) have more than one row but no time variable is selected: %s. The smallest value was used as each patient's best response. If the rows are visits, a 0%% baseline row takes part in that minimum, so a tumour that only grew is scored SD instead of PD. Select the visit time as the Time Variable."),
                length(multi), paste(utils::head(multi, 10), collapse = ", "))
            )
          }
        } else {
          dup_visit <- duplicated(valued[, c(patientID, timeVar)])
          if (any(dup_visit)) {
            dup_ids <- unique(as.character(valued[[patientID]][dup_visit]))
            private$.addNotice(
              type = "WARNING",
              title = .("REPEATED VISIT TIMES"),
              content = sprintf(
                .("%d patient(s) have more than one assessment at the same time point: %s. All of them are used, and the best is taken as the best response. Check for duplicated rows."),
                length(dup_ids), paste(utils::head(dup_ids, 10), collapse = ", "))
            )
          }
        }

        # One group per patient: the first non-missing value in time order. Joining
        # on distinct (patient, group) pairs made a patient whose group changed
        # between rows appear once per group.
        df_spider <- processed_df
        if (!is.null(groupVar) && groupVar %in% names(processed_df)) {
          g <- private$.resolvePatientField(processed_df, patientID, groupVar,
                                            if (has_time) timeVar)
          group_levels <- levels(factor(g$value))
          df_waterfall$patient_group <- factor(
            as.character(g$value)[match(df_waterfall[[patientID]], g$ids)],
            levels = group_levels)
          df_spider$patient_group <- factor(
            as.character(g$value)[match(df_spider[[patientID]], g$ids)],
            levels = group_levels)
          if (length(g$conflicts) > 0) {
            private$.addNotice(
              type = "WARNING",
              title = .("GROUP CHANGES WITHIN PATIENT"),
              content = sprintf(
                .("%d patient(s) have more than one value of the group variable across their rows: %s. Each patient is counted once, in the group of their first recorded value. Check the group column."),
                length(g$conflicts), paste(utils::head(g$conflicts, 10), collapse = ", "))
            )
          }
        }

        attr(df_waterfall, "input_type") <- inputType
        attr(df_spider, "input_type") <- inputType
        if (has_time) {
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
          summary_table$percent <- rep(NA_real_, length(cats))   # 0/0 is not 0%
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
      #
      # Per patient: follow-up = last measured assessment; time in response = the
      # duration of response from .calculateTimeToEventMetrics (first response to
      # nadir-referenced progression, else to the last measured assessment), so it
      # stops at progression. Each patient's category is their FINAL category from
      # the waterfall (after demotions and the override), so this table cannot
      # disagree with the Response Categories table; patients who are not
      # evaluable are left out and counted in `n_excluded`.
      .calculatePersonTimeMetrics = function(df, patientID, timeVar, responseVar,
                                             final_categories = NULL, tte = NULL) {
        if (is.null(timeVar) || !timeVar %in% names(df))
          return(NULL)
        if (!patientID %in% names(df) || nrow(df) == 0)
          return(NULL)
        if (is.null(final_categories) || is.null(tte) || is.null(tte$all_patients))
          return(NULL)

        ap <- tte$all_patients
        cats <- data.frame(pid = as.character(final_categories[[1]]),
                           category = as.character(final_categories$category),
                           stringsAsFactors = FALSE)
        evaluable <- cats$pid[cats$category %in% c("CR", "PR", "SD", "PD")]
        n_excluded <- length(setdiff(cats$pid, evaluable))

        pt_by_patient <- ap[as.character(ap[[patientID]]) %in% evaluable, , drop = FALSE]
        if (nrow(pt_by_patient) == 0)
          return(NULL)
        pt_by_patient$response_cat <- factor(
          cats$category[match(as.character(pt_by_patient[[patientID]]), cats$pid)],
          levels = c("CR", "PR", "SD", "PD"))
        pt_by_patient$follow_up_time <- pt_by_patient$last_assessment
        pt_by_patient$time_to_best <- pt_by_patient$time_to_best_response
        # Only final responders have a duration of response; everyone else has
        # spent no time in response.
        is_resp <- pt_by_patient$response_cat %in% c("CR", "PR") &
          !is.na(pt_by_patient$duration_of_response)
        pt_by_patient$time_in_response <- ifelse(is_resp, pt_by_patient$duration_of_response, 0)
        pt_by_patient$dor_event <- ifelse(is_resp, pt_by_patient$duration_censored, NA_real_)

        total_patients <- nrow(pt_by_patient)
        total_person_time <- sum(pt_by_patient$follow_up_time, na.rm = TRUE)
        total_response_time <- sum(pt_by_patient$time_in_response, na.rm = TRUE)

        km_median <- function(time, event) {
          ok <- !is.na(time) & !is.na(event)
          if (sum(ok) < 2 || !requireNamespace("survival", quietly = TRUE)) return(NA_real_)
          tryCatch(unname(summary(survival::survfit(
            survival::Surv(time[ok], event[ok]) ~ 1))$table["median"]),
            error = function(e) NA_real_)
        }
        safe_median <- function(x) if (all(is.na(x))) NA_real_ else stats::median(x, na.rm = TRUE)

        pt_by_category <- do.call(rbind, lapply(levels(pt_by_patient$response_cat), function(k) {
          sub <- pt_by_patient[pt_by_patient$response_cat == k, , drop = FALSE]
          responder <- k %in% c("CR", "PR")
          # Only responders with a MEASURED response have a time to response and a
          # duration of response (a responder by category override has neither).
          measured <- responder & !is.na(sub$dor_event)
          data.frame(
            response_cat = k,
            patients = nrow(sub),
            person_time = sum(sub$follow_up_time, na.rm = TRUE),
            median_time_to_response = if (any(measured)) safe_median(sub$time_to_best[measured]) else NA_real_,
            median_duration = if (responder) km_median(sub$time_in_response, sub$dor_event) else NA_real_,
            n_dor = if (any(measured) && any(sub$time_in_response[measured] > 0)) sum(measured) else 0L,
            n_dor_events = if (responder) sum(sub$dor_event == 1, na.rm = TRUE) else NA_integer_,
            stringsAsFactors = FALSE
          )
        }))
        pt_by_category$response_cat <- factor(pt_by_category$response_cat, levels = c("CR", "PR", "SD", "PD"))
        pt_by_category$pct_patients <- pt_by_category$patients / total_patients * 100
        pt_by_category$pct_time <- if (total_person_time > 0)
          pt_by_category$person_time / total_person_time * 100 else 0

        response_rate <- if (total_person_time > 0) total_response_time / total_person_time * 100 else NA_real_

        list(
          by_patient = pt_by_patient,
          by_category = pt_by_category,
          summary = list(
            total_patients = total_patients,
            total_person_time = total_person_time,
            total_response_time = total_response_time,
            response_rate_per_100 = response_rate,
            n_excluded = n_excluded
          )
        )
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
          "<br>", .("Welcome to Treatment Response Analysis"),
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
          "<br>- <b>", .("Time Variable:"), "</b> ", .("Required for raw measurements, for several rows per patient (visits), and for the spider plot (e.g., months from baseline)"),
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

        # Rows without a patient ID cannot belong to a patient. Grouped by an NA
        # key they became one phantom "NA" patient counted in ORR/DCR (and named
        # "NA" by the duplicate-baseline check), so drop and report them first.
        analysis_data <- self$data
        if (!is.null(safe_patientID) && safe_patientID %in% names(analysis_data)) {
          ids <- analysis_data[[safe_patientID]]
          no_id <- is.na(ids) | trimws(as.character(ids)) == ""
          if (any(no_id)) {
            analysis_data <- analysis_data[!no_id, , drop = FALSE]
            private$.addNotice(
              type = "WARNING",
              title = .("ROWS WITHOUT PATIENT ID"),
              content = sprintf(
                .("%d row(s) with no patient ID were excluded from the analysis; they cannot be assigned to a patient. Fill in the patient ID if they belong to a patient."),
                sum(no_id))
            )
          }
        }

        validated_data <- private$.validateData(
          analysis_data,
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
            plain_message <- gsub("</?(br|b|strong|i|em|p|div|span|ul|ol|li|hr|pre|small|h[1-6])\\b[^>]*>", " ", paste(validation_messages, collapse = " "), ignore.case = TRUE)
            plain_message <- trimws(gsub("[[:space:]]+", " ", plain_message))
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
          # Strip known tags only: messages quote "<-100%" and ">200%" as text, and
          # "<[^>]+>" swallowed everything between them.
          plain <- gsub("</?(br|b|strong|i|em|p|div|span|ul|ol|li|hr|pre|small|h[1-6])\\b[^>]*>", " ", plain, ignore.case = TRUE)
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

        # Scale diagnostics for percentage input, on the UNCAPPED values (validation
        # has already capped anything below -100).
        if (identical(self$options$inputType, "percentage")) {
          private$.checkPercentageScale(analysis_data, safe_patientID, safe_responseVar,
                                        safe_timeVar)
        }

        # Patients supplied = distinct IDs after dropping rows without an ID. Every
        # panel states the cohort from this one number.
        n_supplied <- length(unique(analysis_data[[safe_patientID]]))

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
        # to "Unknown".
        if (!is.null(processed_data) && !is.null(processed_data$waterfall)) {
          processed_data$waterfall <- private$.enforceMeasurementLimits(
            processed_data$waterfall, validated_data, safe_patientID,
            safe_responseVar, self$options$inputType)

          processed_data$waterfall <- private$.accountForUnevaluablePatients(
            processed_data$waterfall, validated_data, safe_patientID, safe_timeVar,
            safe_responseVar, self$options$inputType)
        }

        # Optional: override the computed RECIST category with a user-supplied one
        # (e.g., new-lesion PD despite target-lesion shrinkage). Applied before
        # metrics and plots so ORR/DCR and bar coloring all reflect it.
        if (!is.null(processed_data) && !is.null(processed_data$waterfall)) {
          processed_data$waterfall <- private$.applyCategoryOverride(
            processed_data$waterfall, analysis_data, safe_patientID,
            self$options$responseCategoryVar, safe_timeVar)
        }

        # A processing error ends the run here, before the method disclaimers (they
        # describe results that were not produced). One ERROR notice; the guided
        # panel is updated so it no longer says results will appear.
        if (!is.null(processed_data$error) && processed_data$error) {
          private$.addNotice("ERROR", .("DATA PROCESSING ERROR"), processed_data$message)
          if (isTRUE(self$options$enableGuidedMode)) {
            private$.generateGuidedAnalysis(problem = processed_data$message)
          }
          return(NULL)
        }

        # One factual statement of what the categories are and are not. It replaces
        # three overlapping always-on notices ("REGULATORY USE PROHIBITED", "RECIST
        # COMPLIANCE LIMITATION", "CONFIRMATION NOT REQUIRED") that repeated each
        # other, named regulators and "certified platforms", and contradicted the
        # glossary. WARNING rather than STRONG_WARNING: it is shown on every run.
        private$.addNotice(
          type = "WARNING",
          title = .("EXPLORATORY RESPONSE CATEGORIES"),
          content = .("Categories are thresholds applied to one tumour burden value per patient and visit (CR -100%, PR at least -30%, PD at least +20%), and best response is the smallest post-baseline change. This is not a RECIST v1.1 assessment: there is no target-lesion summation, no new-lesion or non-target assessment, no confirmation of response and no minimum duration for stable disease, so ORR and DCR are unconfirmed and progression may be missed. Use the results for exploratory description; trial endpoints need a lesion-level RECIST v1.1 assessment.")
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
            content = .("The spider plot draws each patient's tumour trajectory over time, so it requires a Time Variable. None is selected, so no spider plot can be produced. Assign the visit/assessment time column to \"Time Variable\" to enable it.")
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

        # Warning #4: Time-to-Event Methodology Limitations (MEDIUM)
        # (Progression IS nadir-referenced - .progressionTimes uses the running
        # minimum burden - so the former claim that it was baseline-referenced
        # was stale and mis-described the method.)
        if (!is.null(self$options$timeVar) && self$options$timeVar != "") {
          private$.addNotice(
            type = "WARNING",
            title = .("TIME-TO-EVENT LIMITATIONS"),
            content = .("The headline duration of response is the censoring-aware Kaplan-Meier median; the crude median in the Time-to-Response table ignores censoring and understates DoR. Progression is detected as a >=20% increase over the NADIR (the smallest burden recorded so far), or any reappearance after a complete response, following RECIST v1.1. Two limitations remain. (1) The additional RECIST v1.1 requirement of a >=5 mm absolute increase cannot be applied to percent-change data, and new-lesion or non-target progression is invisible here, so progression may still be under-detected. (2) No log-rank test or Cox regression for covariates is provided. For formal progression-free survival (PFS) or duration of response analysis, use dedicated survival analysis functions. Current calculations are exploratory only.")
          )
        }

        # Warning #5: Baseline Validation (for raw measurements)
        if (self$options$inputType == "raw" && !is.null(self$options$timeVar) && self$options$timeVar != "") {
          private$.addNotice(
            type = "INFO",
            title = .("BASELINE ASSUMPTION"),
            content = .("Percent changes calculated assuming time=0 is baseline for each patient. Duplicate time=0 rows are rejected during validation. Verify: (1) No measurements before time=0; (2) time=0 is the pre-treatment baseline (not post-treatment). Patients missing baseline measurements are excluded from waterfall analysis.")
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
                .("Only n=%d evaluable patients: a single patient changes each rate by %.0f percentage points and the confidence intervals are very wide. Treat the rates as descriptive."),
                n_patients, 100 / n_patients)
            )
          } else if (n_patients > 0 && n_patients < 20) {
            # (n = 0 is reported once, as NO EVALUABLE PATIENTS.)
            private$.addNotice(
              type = "WARNING",
              title = .("SMALL SAMPLE"),
              content = sprintf(
                .("n=%d evaluable patients: the confidence intervals for ORR and DCR are wide. Interpret the rates with their intervals."),
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

        processed_data$n_supplied <- n_supplied
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
              .("CR: best change of -100% (the measured burden disappeared). Threshold-based; not a RECIST v1.1 complete response.")
            )

            self$results$summaryTable$addFootnote(
              rowNo = 2,
              col = "category",
              .("PR: best change of -30% or less, above -100%.")
            )

            self$results$summaryTable$addFootnote(
              rowNo = 3,
              col = "category",
              .("SD: best change above -30% and below +20%; no minimum duration is required.")
            )

            self$results$summaryTable$addFootnote(
              rowNo = 4,
              col = "category",
              .("PD: best change of +20% or more from baseline (no new-lesion assessment).")
            )


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

        # Rates with their exact (Clopper-Pearson) 95% CI. The value cells used to
        # carry a verdict ("Excellent disease control", "Promising activity")
        # graded on the point estimate alone; the interval is what a reader needs
        # to judge a rate, and the benchmark depends on the tumour and setting.
        rate_with_ci <- function(k, n, rate) {
          ci <- tryCatch(stats::binom.test(k, n)$conf.int * 100, error = function(e) c(NA, NA))
          if (any(is.na(ci))) sprintf("%.1f%%", rate)
          else sprintf(.("%.1f%% (95%% CI %.1f-%.1f%%)"), rate, ci[1], ci[2])
        }
        n_resp <- sum(metrics$summary$n[metrics$summary$category %in% c("CR", "PR")])
        n_ctrl <- sum(metrics$summary$n[metrics$summary$category %in% c("CR", "PR", "SD")])

        if (!is.na(metrics$ORR)) {
          add_metric_row(list(
            metric = .("Objective Response Rate (CR+PR)"),
            value = rate_with_ci(n_resp, metrics$n, metrics$ORR)
          ))
        }

        if (!is.na(metrics$DCR)) {
          add_metric_row(list(
            metric = .("Disease Control Rate (CR+PR+SD)"),
            value = rate_with_ci(n_ctrl, metrics$n, metrics$DCR)
          ))
        }

        # RECIST reporting puts every patient in the denominator and counts a patient
        # who could not be assessed as a non-responder; the rates above are over
        # evaluable patients only. Shown when the two differ.
        n_supplied <- processed_data$n_supplied %||% metrics$n
        if (!is.na(metrics$ORR) && n_supplied > metrics$n) {
          add_metric_row(list(
            metric = sprintf(.("Objective Response Rate, all %d patients (not evaluable counted as non-responders)"),
                             n_supplied),
            value = rate_with_ci(n_resp, n_supplied, n_resp / n_supplied * 100)
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

        # Each patient's FINAL category (after demotions and the override): the
        # time-to-event and person-time figures describe exactly these patients.
        final_categories <- data.frame(
          pid = as.character(processed_data$waterfall[[safe_patientID]]),
          category = as.character(processed_data$waterfall$recist_category),
          stringsAsFactors = FALSE)

        # Time to response and duration of response ----
        tte_metrics <- NULL
        if (!is.null(self$options$timeVar) && !is.null(processed_data$spider) &&
            safe_timeVar %in% names(processed_data$spider)) {
          tte_metrics <- private$.calculateTimeToEventMetrics(
            processed_data$spider, safe_patientID, safe_timeVar, "response", final_categories)
        }

        if (!is.null(tte_metrics)) {
          s <- tte_metrics$summary
          unit <- switch(self$options$timeUnitLabel %||% "generic",
                         days = .("days"), weeks = .("weeks"), months = .("months"),
                         years = .("years"), .("time units"))
          # A missing Kaplan-Meier limit is spelled out (it was an undefined,
          # untranslated "NR"); a not-reached median keeps its finite lower limit.
          fmt_lim <- function(x) if (is.na(x)) .("not estimable") else sprintf("%.1f", x)
          ci_text <- sprintf(.("95%% CI %s to %s"), fmt_lim(s$km_median_lcl), fmt_lim(s$km_median_ucl))
          no_followup <- s$n_responders >= 2 && !isTRUE(s$any_followup_after_response)
          km_text <- if (no_followup) {
            sprintf(.("not estimable (no assessment after the first response for any of the %d responders)"),
                    s$n_responders)
          } else if (!is.na(s$km_median_duration_of_response)) {
            sprintf(.("%.1f %s (%s; %d of %d responders progressed)"),
                    s$km_median_duration_of_response, unit, ci_text,
                    s$n_duration_events, s$n_responders)
          } else if (s$n_responders >= 2 && !is.na(s$km_median_lcl)) {
            sprintf(.("not reached (95%% CI lower limit %.1f %s; %d of %d responders progressed)"),
                    s$km_median_lcl, unit, s$n_duration_events, s$n_responders)
          } else if (s$n_responders >= 2) {
            sprintf(.("not reached (%d of %d responders progressed)"),
                    s$n_duration_events, s$n_responders)
          } else NULL

          if (s$n_responders > 0 && !is.na(s$median_time_to_response)) {
            add_metric_row(list(
              metric = .("Median Time to First Response"),
              value = sprintf(.("%.1f %s (n=%d responders)"),
                              s$median_time_to_response, unit, s$n_responders)
            ))
          }
          # The headline duration of response is the Kaplan-Meier median: the crude
          # median ignores responders still in response and understates DoR.
          if (!is.null(km_text)) {
            add_metric_row(list(
              metric = .("Median Duration of Response (Kaplan-Meier)"),
              value = km_text
            ))
          }

          # Dedicated TTR / DoR table
          if (isTRUE(self$options$showResponseDuration) &&
              !is.null(self$results$responseDurationTable)) {
            rdt <- self$results$responseDurationTable
            if (s$n_responders == 0) {
              rdt$setNote("none", .("No patient reached a response (PR or better, <= -30%) after baseline, so time to response and duration of response are not estimable."))
            } else {
              rdt$addRow(rowKey = "ttr", values = list(
                metric = .("Median time to first response (TTR)"),
                value = s$median_time_to_response,
                detail = sprintf(.("RECIST PR or better; n=%d responders"), s$n_responders)))
              rdt$addRow(rowKey = "dor_naive", values = list(
                metric = .("Median duration of response (naive)"),
                value = s$median_duration_of_response,
                detail = sprintf(.("Ignores censoring; n=%d with duration data"),
                                 s$n_with_duration_data)))
              rdt$addRow(rowKey = "dor_km", values = list(
                metric = .("Median duration of response (Kaplan-Meier)"),
                value = s$km_median_duration_of_response,
                detail = if (s$n_responders < 2)
                  .("Not estimable with fewer than 2 responders")
                else if (no_followup)
                  .("Not estimable: no responder has an assessment after the first response")
                else if (is.na(s$km_median_duration_of_response))
                  sprintf(.("Median not reached (only %d of %d responders progressed); %s"),
                          s$n_duration_events, s$n_responders, ci_text)
                else
                  sprintf(.("Censoring-aware; %d progression events; %s"),
                          s$n_duration_events, ci_text)))
              rdt$setNote("dor",
                .("DoR is measured from first response to progression over the nadir (reappearance after a complete response counts as progression); responders still in response are censored at their last measured assessment. The Kaplan-Meier median accounts for this censoring and is the preferred summary."))
            }
            if (s$n_responders_without_time > 0) {
              rdt$setNote("override",
                sprintf(.("%d responder(s) by category override have no measured response time and are not included here."),
                        s$n_responders_without_time))
            }
          }
        }

        # Person-time ----
        personTimeVisible <- !is.null(self$options$timeVar) && self$options$inputType == "raw"
        if (!is.null(self$results$personTimeTable)) {
          self$results$personTimeTable$setVisible(personTimeVisible)
        }
        person_time_metrics <- NULL
        if (personTimeVisible) {
          private$.checkpoint()  # Checkpoint before person-time calculations
          person_time_metrics <- tryCatch({
            private$.calculatePersonTimeMetrics(
              processed_data$spider, safe_patientID, safe_timeVar, safe_responseVar,
              final_categories, tte_metrics)
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

        if (!is.null(person_time_metrics) && personTimeVisible) {
          response_rate_value <- person_time_metrics$summary$response_rate_per_100
          add_metric_row(list(
            metric = .("Time in response per 100 person-time units (DoR-based, exploratory)"),
            value = if (!is.na(response_rate_value)) sprintf("%.2f", response_rate_value) else .("Not estimable")
          ))

          if (!is.null(self$results$personTimeTable)) {
            private$.checkpoint()  # Checkpoint before person-time table population
            pt <- self$results$personTimeTable
            by_cat <- person_time_metrics$by_category
            blank_na <- function(x) if (is.na(x)) "" else sprintf("%.1f", x)
            for (i in seq_len(nrow(by_cat))) {
              cat_i <- as.character(by_cat$response_cat[i])
              # "Median time to best response" and the DoR are meaningless for
              # SD/PD rows; leave those blank.
              is_responder_cat <- cat_i %in% c("CR", "PR")
              pt$addRow(rowKey = i, values = list(
                category = cat_i,
                patients = by_cat$patients[i],
                patient_pct = sprintf("%.1f%%", by_cat$pct_patients[i]),
                person_time = sprintf("%.1f", by_cat$person_time[i]),
                time_pct = sprintf("%.1f%%", by_cat$pct_time[i]),
                median_time = if (is_responder_cat) blank_na(by_cat$median_time_to_response[i]) else "",
                median_duration = if (is_responder_cat) {
                  # "not reached" only when it is estimable (>= 2 measured responders)
                  if (by_cat$n_dor[i] < 2) ""
                  else if (is.na(by_cat$median_duration[i])) .("not reached")
                  else sprintf("%.1f", by_cat$median_duration[i])
                } else ""
              ))
            }
            pt$addRow(rowKey = nrow(by_cat) + 1, values = list(
              category = .("Total"),
              patients = person_time_metrics$summary$total_patients,
              patient_pct = "100.0%",
              person_time = sprintf("%.1f", person_time_metrics$summary$total_person_time),
              time_pct = "100.0%",
              median_time = "",
              median_duration = ""
            ))
            pt$setNote("dor",
              .("Categories are the final response categories (same as the Response Categories table). Person-time is follow-up to the last measured assessment. The DoR column is the Kaplan-Meier median duration of response within the category."))
            if (person_time_metrics$summary$n_excluded > 0) {
              pt$setNote("excluded",
                sprintf(.("%d patient(s) not evaluable for response are not included."),
                        person_time_metrics$summary$n_excluded))
            }
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

        # Notices cannot be added from a renderer (they are rendered by then), so the
        # marker-shape recycling note is decided here from the attached levels.
        if (isTRUE(self$options$showWaterfallPlot) &&
            "confirm_status" %in% names(processed_data$waterfall)) {
          nlev <- length(unique(stats::na.omit(processed_data$waterfall$confirm_status)))
          if (nlev > 6) {
            private$.addNotice("INFO", .("MARKER SHAPES REUSED"), sprintf(
              .("Confirmation variable has %d levels; markers reuse shapes beyond %d distinct symbols."),
              nlev, 6L))
          }
        }

        # Annotation tracks show one value per patient (the first non-missing one);
        # the renderer cannot add notices, so conflicting per-visit values are
        # reported here.
        ann_vars <- tryCatch(self$options$annotationVars, error = function(e) NULL)
        if (isTRUE(self$options$showWaterfallPlot) && length(ann_vars) > 0) {
          conflicts <- unlist(lapply(ann_vars[ann_vars %in% names(self$data)], function(v) {
            f <- private$.resolvePatientField(self$data, safe_patientID, v, safe_timeVar)
            if (length(f$conflicts)) sprintf("%s (%s)", v, paste(utils::head(f$conflicts, 5), collapse = ", "))
          }))
          if (length(conflicts) > 0) {
            private$.addNotice("INFO", .("CONFLICTING ANNOTATION VALUES"), sprintf(
              .("Some annotation variables have more than one value for a patient across rows: %s. The track shows the first recorded value."),
              paste(conflicts, collapse = "; ")))
          }
        }

        # Plot states carry small plot-ready frames only (they are saved in the
        # .omv): the waterfall needs one row per patient, the spider plot the
        # per-visit trajectory columns. Metrics are not read by either renderer.
        w_cols <- intersect(c(safe_patientID, "response", "recist_category", "patient_group",
                              "confirm_status", "ongoing_flag"), names(processed_data$waterfall))
        waterfall_frame <- as.data.frame(processed_data$waterfall)[, w_cols, drop = FALSE]
        spider_frame <- NULL
        if (!is.null(processed_data$spider)) {
          s_cols <- intersect(c(safe_patientID, safe_timeVar, "response", "patient_group"),
                              names(processed_data$spider))
          spider_frame <- as.data.frame(processed_data$spider)[, s_cols, drop = FALSE]
        }

        plotData <- list(
          "data" = list(waterfall = waterfall_frame),
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
          )
        )

        # Add checkpoint for performance monitoring
        private$.checkpoint()

        # The bootstrap median CI needs enough patients to be meaningful; the
        # renderer used to say so only via message(), which jamovi never shows.
        if (isTRUE(self$options$showCI) && isTRUE(self$options$showWaterfallPlot) &&
            !is.null(processed_data$waterfall)) {
          n_ci <- sum(!is.na(processed_data$waterfall$response))
          if (n_ci < 10) {
            # Same quantity the renderer tests (evaluable patients), so the notice
            # cannot promise an interval the plot then leaves out.
            private$.addNotice(
              type = "WARNING",
              title = .("MEDIAN CI NOT DRAWN"),
              content = sprintf(
                .("The bootstrap confidence interval for the median response requires at least 10 evaluable patients; only %d are available, so no CI annotation is drawn on the waterfall plot."),
                n_ci)
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
          plotData$data <- list(spider = spider_frame)
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
          n_total <- processed_data$n_supplied %||% nrow(processed_data$waterfall)
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

          pct_eval <- function(k) if (n_eval > 0) k / n_eval * 100 else NA_real_

          # Create summary HTML (with NA handling)
          orr_text <- if (!is.na(orr)) {
            sprintf(.("%.1f%% (%d of %d evaluable patients achieved complete or partial response)"), orr, n_cr + n_pr, n_eval)
          } else {
            .("Not available (insufficient data)")
          }

          dcr_text <- if (!is.na(dcr)) {
            sprintf(.("%.1f%% (%d of %d evaluable patients achieved response or stable disease)"), dcr, n_cr + n_pr + n_sd, n_eval)
          } else {
            .("Not available (insufficient data)")
          }

          interpretation_text <- if (!is.na(orr)) {
            private$.interpretORR(orr, n_cr + n_pr, n_eval)
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
            # One whole translatable sentence per line: "%d %s" with a separately
            # translated "patients" cannot be inflected in other languages.
            "<li>", sprintf(.("Complete response: n = %d (%.1f%%)"), n_cr, pct_eval(n_cr)), "</li>",
            "<li>", sprintf(.("Partial response: n = %d (%.1f%%)"), n_pr, pct_eval(n_pr)), "</li>",
            "<li>", sprintf(.("Stable disease: n = %d (%.1f%%)"), n_sd, pct_eval(n_sd)), "</li>",
            "<li>", sprintf(.("Progressive disease: n = %d (%.1f%%)"), n_pd, pct_eval(n_pd)), "</li>",
            if (n_unknown > 0) paste0("<li>", sprintf(.("Unknown / not evaluable: n = %d (excluded from percentages)"), n_unknown), "</li>") else "",
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
          "<h3 style='color: inherit; margin-top: 0;'>", .("What This Analysis Does"), "</h3>",
          "<p>", .("The Treatment Response Analysis creates waterfall and spider plots using threshold-based response categories adapted from RECIST v1.1 (not a full RECIST v1.1 assessment)."), "</p>",

          "<h4 style='color: inherit; margin-top: 15px;'>", .("Visualization Types:"), "</h4>",
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
          "<li><strong>", .("Time Variable:"), "</strong> ", .("Required for raw measurements, for several rows per patient (visits), and for the spider plot (e.g., months from baseline)"), "</li>",
          "</ul>",
          "</div>",

          "<div style='padding: 15px; background-color: rgba(216, 33, 50, 0.18); border-left: 4px solid #dc3545; margin: 20px 0; color: inherit;'>",
          "<h3 style='color: inherit; margin-top: 0;'>", .("Key Assumptions & Limitations:"), "</h3>",
          "<ul style='margin: 5px 0;'>",
          "<li>", sprintf(.("RECIST v1.1 thresholds: CR \u2264-100%%, PR \u2264-30%%, PD \u2265+20%%")), "</li>",
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
        private$.nCapped <- 0L

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
          legend_name <- .("Response category")
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
            y = .("Best change from baseline (%)")
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
            # library-audit 2026-09-16 meddecide [LOW] DONE (same class): local_seed() restores the
            #   session's RNG stream when this renderer returns; set.seed() left it fixed for the next analysis
            seed_val <- plotData$options$seed
            if (is.null(seed_val)) seed_val <- 123
            withr::local_seed(seed_val)

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
                  .("95%% CI (Median): (%.1f%%, %.1f%%)"),
                  ci[1],
                  ci[2]
                ),
                hjust = 0,
                vjust = -0.5,
                size = 3
              )
            # the interval is a bootstrap: name the seed that drew it
            p <- p + ggplot2::labs(caption = jmvcore::format(.("Random seed: {seed}"), seed = seed_val))
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
              linewidth = 1,
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
          # Map the fill to an UNTRANSLATED key: the colour vector below is named
          # "Responder"/"Non-responder", so a translated value matched no colour and
          # every point turned grey with an empty legend in any other language.
          df$responder_status <- private$.spiderResponderKey(df$response)

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
              linewidth = 1,
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
              values = responder_colors,
              breaks = c("Responder", "Non-responder"),
              labels = c(.("Responder"), .("Non-responder"))
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

        # RECIST threshold lines, like their labels below, follow "Show RECIST
        # thresholds" (they were drawn unconditionally).
        if (isTRUE(options$showThresholds)) {
          p <- p +
            ggplot2::geom_hline(
              yintercept = c(private$RECIST_PR_THRESHOLD, private$RECIST_PD_THRESHOLD),
              linetype = "dashed",
              color = "gray50",
              alpha = 0.5
            )
        }
        p <- p +
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
              linewidth = 1
            )
        }

        # Try to print the plot with error handling
        tryCatch({
          print(p)
          TRUE
        }, error = function(e) {
          # A renderer cannot add a notice, and warning() only reaches the generic
          # Analysis Notes; say it on the plot area instead.
          grid::grid.newpage()
          grid::grid.text(sprintf(.("The spider plot could not be drawn: %s"), conditionMessage(e)),
                          gp = grid::gpar(fontsize = 11))
          TRUE
        })
      }

      ,
      # Responder key for the spider plot, in untranslated labels.
      .spiderResponderKey = function(response) {
        factor(ifelse(response <= private$RECIST_PR_THRESHOLD + private$RECIST_TOL,
                      "Responder", "Non-responder"),
               levels = c("Responder", "Non-responder"))
      },

      # Generate clinical summary ----
      .generateClinicalSummary = function(processed_data, metrics, person_time_metrics = NULL) {

        # Extract key metrics. n_eval (metrics$n) is the ONE denominator used
        # for every rate and percentage in this panel - it previously mixed
        # nrow() (including Unknown patients) for the distribution with the
        # evaluable-only ORR/DCR, so the same panel contradicted itself.
        n_total <- processed_data$n_supplied %||% nrow(processed_data$waterfall)
        n_eval <- metrics$n
        n_unknown <- if (!is.null(metrics$n_unknown)) metrics$n_unknown else (n_total - n_eval)
        orr <- metrics$ORR
        dcr <- metrics$DCR

        # NA-safe display: with zero evaluable patients (all demoted to
        # Unknown) ORR/DCR are NA - the raw `orr >= 30` test here crashed the
        # whole run with "missing value where TRUE/FALSE needed".
        fmt_rate <- function(x) if (is.null(x) || is.na(x)) .("not estimable") else sprintf("%.1f%%", x)
        pct_of_eval <- function(k) if (n_eval > 0) k / n_eval * 100 else 0

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
          "<h4 style='color: inherit; margin-top: 0;'>", .("Treatment Response Summary"), "</h4>",

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
          if (cr_count > 0) paste0("<li>", sprintf(.("Complete response: n = %d (%.1f%%)"), cr_count, pct_of_eval(cr_count)), "</li>") else "",
          if (pr_count > 0) paste0("<li>", sprintf(.("Partial response: n = %d (%.1f%%)"), pr_count, pct_of_eval(pr_count)), "</li>") else "",
          if (sd_count > 0) paste0("<li>", sprintf(.("Stable disease: n = %d (%.1f%%)"), sd_count, pct_of_eval(sd_count)), "</li>") else "",
          if (pd_count > 0) paste0("<li>", sprintf(.("Progressive disease: n = %d (%.1f%%)"), pd_count, pct_of_eval(pd_count)), "</li>") else "",
          if (n_unknown > 0) paste0("<li>", sprintf(.("Unknown / not evaluable: n = %d (excluded from percentages)"), n_unknown), "</li>") else "",
          "</ul>"
        )

        # Add clinical interpretation (NA-safe, shared benchmark wording)
        interpretation <- private$.interpretORR(orr, cr_count + pr_count, n_eval)

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
          "<li><strong>", .("Time Variable:"), "</strong> ", .("Required for raw measurements, for several rows per patient (visits), and for the spider plot (e.g., months from baseline)"), "</li>",
          "</ul>",
          
          "<h5>", .("Key Assumptions & Limitations:"), "</h5>",
          "<ul>",
          sprintf("<li>%s CR \u2264%d%%, PR \u2264%d%%, PD \u2265+%d%%</li>", .("RECIST v1.1 thresholds:"), private$RECIST_CR_THRESHOLD, private$RECIST_PR_THRESHOLD, private$RECIST_PD_THRESHOLD),
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
            c(NA_real_, NA_real_)  # no evaluable patient: no interval (0-100% read as a real CI)
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
            c(NA_real_, NA_real_)  # no evaluable patient: no interval (0-100% read as a real CI)
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
          interpretation = private$.interpretORR(metrics$ORR, n_responders, n_total)
        )
        dcr_values <- list(
          metric = .("Disease Control Rate (DCR)"),
          value = if (is.na(metrics$DCR)) .("not estimable") else sprintf("%.1f%%", metrics$DCR),
          ci_lower = round(dcr_ci[1] * 100, 1),
          ci_upper = round(dcr_ci[2] * 100, 1),
          interpretation = private$.interpretDCR(metrics$DCR, n_dcr, n_total)
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
              .("The ORR 95%% CI spans %.1f percentage points (%.1f-%.1f%%): the data are compatible with rates anywhere in that range. Report the interval with the rate."),
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

        n_responders <- cr_count + pr_count
        n_control <- n_responders + count_for("SD")
        exact_ci <- function(k) tryCatch(stats::binom.test(k, n_patients)$conf.int * 100,
                                         error = function(e) c(NA_real_, NA_real_))
        orr_ci <- exact_ci(n_responders)
        dcr_ci <- exact_ci(n_control)
        n_supplied <- processed_data$n_supplied %||% n_patients

        # Rates are unconfirmed, over evaluable patients only, with exact CIs for
        # both; "n = k" reads correctly for one patient and for many.
        main_results <- sprintf(
          .("Of %d patients, %d were evaluable for response (%d not evaluable; reasons in the notices). The unconfirmed objective response rate (ORR) was %.1f%% (95%% CI %.1f-%.1f%%; complete response n = %d, partial response n = %d), and the disease control rate (DCR) was %.1f%% (95%% CI %.1f-%.1f%%)."),
          n_supplied, n_patients, n_supplied - n_patients, metrics$ORR, orr_ci[1], orr_ci[2],
          cr_count, pr_count, metrics$DCR, dcr_ci[1], dcr_ci[2])
        # Capped values are counted as complete responses; the pasted text must say so.
        if (private$.nCapped > 0) {
          main_results <- paste(main_results, sprintf(
            .("For %d patient(s) a change below -100%%, which is not possible, was capped at -100%% and counted as a complete response; check the data before reporting."),
            private$.nCapped))
        }

        # Generate publication-ready sentences
        report_text <- paste0(
          "<div style='background-color: rgba(33, 166, 255, 0.07); padding: 15px; border: 1px solid #0369a1; border-radius: 5px; margin: 10px 0; color: inherit;'>",
          "<h4 style='color: inherit; margin-top: 0;'>", .("Copy-Ready Report Sentences"), "</h4>",

          "<div style='background-color: rgba(138, 155, 172, 0.08); padding: 10px; border-radius: 3px; margin: 10px 0; color: inherit;'>",
          "<h5>", .("Main Results:"), "</h5>",
          "<p style='font-family: monospace; background-color: rgba(138, 155, 172, 0.06); padding: 8px; border-radius: 3px; color: inherit;'>",
          main_results,
          "</p>",
          "</div>",

          "<div style='background-color: rgba(138, 155, 172, 0.08); padding: 10px; border-radius: 3px; margin: 10px 0; color: inherit;'>",
          "<h5>", .("Methods Description:"), "</h5>",
          "<p style='font-family: monospace; background-color: rgba(138, 155, 172, 0.06); padding: 8px; border-radius: 3px; color: inherit;'>",
          .("Best response was the smallest percent change in tumour burden from baseline over the post-baseline assessments, categorised with thresholds adapted from RECIST v1.1 (complete response -100%, partial response at least -30%, progressive disease at least +20%). This is not a RECIST v1.1 assessment: there was no target-lesion summation, no new-lesion or non-target assessment and no confirmation of response. Response rates were computed over evaluable patients with exact (Clopper-Pearson) 95% confidence intervals."),
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
        # Patients without a group value cannot be compared. They showed up as an
        # "NA" group row while fisher.test() silently dropped them.
        n_missing_group <- sum(is.na(df$patient_group))
        df <- df[!is.na(df$patient_group), , drop = FALSE]
        if (nrow(df) == 0) return()
        df$patient_group <- droplevels(factor(df$patient_group))
        if (n_missing_group > 0) {
          missing_note <- sprintf(
            .("%d patient(s) with a missing group value are excluded from the group comparison."),
            n_missing_group)
          self$results$groupComparisonTable$setNote("missing_group", missing_note)
          self$results$groupComparisonTest$setNote("missing_group", missing_note)
        }

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

          # The odds ratio of a 2x2 table (rows = groups, columns = no/yes) is the
          # odds of the outcome in the SECOND group over the FIRST. Printed bare
          # ("OR = 0.06") it read as a large effect in either direction.
          group_levels <- levels(df$patient_group)
          or_label <- function(test) {
            if (is.null(test$estimate) || length(group_levels) != 2)
              return(.("Fisher's exact test"))
            sprintf(.("Fisher's exact test; OR (%s vs %s) = %.2f (95%% CI %.2f-%.2f)"),
                    group_levels[2], group_levels[1], test$estimate,
                    test$conf.int[1], test$conf.int[2])
          }

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
              test_statistic = or_label(orr_test),
              p_value = orr_test$p.value,
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
              test_statistic = or_label(dcr_test),
              p_value = dcr_test$p.value,
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
          "<h5 style='color: inherit; margin-bottom: 10px;'>", .("Response Metrics"), "</h5>",
          "<ul style='margin: 0; padding-left: 15px; line-height: 1.6;'>",
          "<li><strong>", .("ORR (Objective Response Rate - Unconfirmed):"), "</strong> ", .("Percentage of patients achieving threshold-based CR (\u2264-100%) or PR (\u2264-30%) without RECIST v1.1 confirmation requirement. May overestimate true confirmed ORR."), "</li>",
          "<li><strong>", .("DCR (Disease Control Rate - Unconfirmed):"), "</strong> ", .("Percentage achieving threshold-based response or stable disease (CR + PR + SD) without confirmation. Exploratory endpoint only."), "</li>",
          "<li><strong>", .("Best Response (Simplified):"), "</strong> ", .("Most favourable (most negative) percent change from baseline over the post-baseline assessments. NOT equivalent to RECIST v1.1 'Best Overall Response', which requires confirmation."), "</li>",
          "<li><strong>", .("Person-Time:"), "</strong> ", .("Total time patients are followed, accounting for different follow-up durations"), "</li>",
          "</ul>",
          "</div>",

          "<div>",
          "<h5 style='color: inherit; margin-bottom: 10px;'>", .("Response Categories (Simplified Threshold-Based)"), "</h5>",
          "<ul style='margin: 0; padding-left: 15px; line-height: 1.6;'>",
          "<li><strong>", .("CR (Complete Response - Threshold):"), "</strong> ", .("\u2264-100% change from baseline (simplified criterion, NOT full RECIST CR which requires disappearance of ALL lesions including non-target)"), "</li>",
          "<li><strong>", .("PR (Partial Response - Threshold):"), "</strong> ", .("\u2264-30% change from baseline (simplified criterion, NOT full RECIST PR which requires target lesion sum calculation and no new lesions)"), "</li>",
          "<li><strong>", .("SD (Stable Disease - Threshold):"), "</strong> ", .("Between -30% and +20% change (simplified criterion)"), "</li>",
          "<li><strong>", .("PD (Progressive Disease - Threshold):"), "</strong> ", .("\u2265+20% change from baseline (simplified criterion, NOT full RECIST PD which includes new lesion detection and non-target progression)"), "</li>",
          "</ul>",
          "</div>",

          "</div>",

          "<div style='margin-top: 15px;'>",
          "<h5 style='color: inherit; margin-bottom: 10px;'>", .("Statistical Terms"), "</h5>",
          "<ul style='margin: 0; padding-left: 15px; line-height: 1.6;'>",
          "<li><strong>", .("95% CI (Confidence Interval):"), "</strong> ", .("Range of values compatible with the observed data; over repeated studies, 95% of such intervals contain the true population parameter"), "</li>",
          "<li><strong>", .("Fisher's Exact Test:"), "</strong> ", .("Statistical test for comparing response rates between groups"), "</li>",
          "<li><strong>", .("Binomial CI:"), "</strong> ", .("Exact confidence interval for proportions (more accurate than normal approximation)"), "</li>",
          "<li><strong>", .("Waterfall Plot:"), "</strong> ", .("Bar chart showing best response for each patient, sorted by magnitude"), "</li>",
          "<li><strong>", .("Spider Plot:"), "</strong> ", .("Line graph showing individual patient response trajectories over time"), "</li>",
          "</ul>",
          "</div>",

          "<div style='margin-top: 15px; padding: 10px; background-color: rgba(33, 152, 239, 0.13); border-radius: 3px; color: inherit;'>",
          "<small><strong>", .("Clinical Context:"), "</strong> ",
          .("ORR and DCR are standard summaries in oncology studies. The threshold-based, unconfirmed versions computed here are for exploratory description, not for trial endpoints."),
          "</small>",
          "</div>",

          "</div>"
        )

        self$results$clinicalGlossary$setContent(glossary_text)
      }

      ,
      # Generate clinical significance assessment ----
      .generateClinicalSignificance = function(metrics, n_patients) {
        n_of <- function(cats) sum(metrics$summary$n[metrics$summary$category %in% cats])
        orr_interpretation <- private$.interpretORR(metrics$ORR, n_of(c("CR", "PR")), metrics$n)
        dcr_interpretation <- private$.interpretDCR(metrics$DCR, n_of(c("CR", "PR", "SD")), metrics$n)
        # NA-safe display (all patients unevaluable -> ORR/DCR are NA)
        orr_display <- if (is.na(metrics$ORR)) .("not estimable") else paste0(metrics$ORR, "%")
        dcr_display <- if (is.na(metrics$DCR)) .("not estimable") else paste0(metrics$DCR, "%")

        # Sample size adequacy assessment
        # Same bands as the SMALL SAMPLE notices (n < 10 very small, n < 20 small).
        sample_size_assessment <- if (n_patients < 10) {
          .("Very small sample (n<10): a single patient changes each rate substantially; the confidence intervals are very wide.")
        } else if (n_patients < 20) {
          .("Small sample (n<20): the confidence intervals are wide.")
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
          "<li>", .("Judge the response rate against the rate expected without benefit in your tumour type and line of therapy (the null rate of your study design), using the confidence interval above, rather than against generic cut-offs."), "</li>",
          "<li>", .("ORR and DCR here are unconfirmed, threshold-based rates over evaluable patients."), "</li>",
          "</ul>",

          "</div>"
        )

        self$results$clinicalSignificance$setContent(significance_text)
      }

      ,
      # Generate guided analysis steps ----
      .generateGuidedAnalysis = function(problem = NULL) {
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
            paste0("<br><small style='color: inherit;'>", .("Patient ID selected"), "</small>")
          },
          "</li>",

          # Step 2: Response Variable
          "<li style='margin: 5px 0;'>",
          if (has_response) "[DONE]" else "[TODO]",
          " <strong>", .("Select Response Variable"), "</strong>",
          if (!has_response) {
            paste0("<br><small style='color: #dc2626;'>", .("Required: Choose tumor measurements or percentage changes"), "</small>")
          } else {
            paste0("<br><small style='color: inherit;'>", .("Response variable selected"), "</small>")
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
            paste0("<br><small style='color: inherit;'>", .("Time variable selected - enables spider plots"), "</small>")
          } else {
            paste0("<br><small style='color: inherit;'>", .("Optional for percentage data"), "</small>")
          },
          "</li>",

          # Step 5: Run Analysis
          "<li style='margin: 5px 0;'>",
          if (!is.null(problem)) "[STOPPED]" else if (has_patient_id && has_response) "[READY]" else "[WAITING]",
          " <strong>", .("Run Analysis"), "</strong>",
          if (!is.null(problem)) {
            paste0("<br><small style='color: inherit;'>", .("The analysis stopped; see the notices for the reason."), "</small>")
          } else if (has_patient_id && has_response) {
            paste0("<br><small style='color: inherit;'>", .("Ready to run! Results will appear below."), "</small>")
          } else {
            paste0("<br><small style='color: inherit;'>", .("Complete required steps above"), "</small>")
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
      .interpretORR = function(orr, k = NULL, n = NULL) {
        private$.interpretRate(orr, k, n,
          .("Response rates from %.1f%% to %.1f%% are compatible with these data (exact 95%% CI). There is no universal benchmark: compare them with the response rate expected in your tumour type and treatment setting."))
      },

      .interpretDCR = function(dcr, k = NULL, n = NULL) {
        private$.interpretRate(dcr, k, n,
          .("Disease control rates from %.1f%% to %.1f%% are compatible with these data (exact 95%% CI). Disease control has no conventional benchmark; interpret it against the expected course of the disease."))
      },

      # The interpretation is the confidence interval, stated in words. Grading the
      # point estimate ("Promising activity", "Excellent disease control") told
      # users the analysis had judged efficacy, which a single rate cannot do.
      .interpretRate = function(rate, k, n, template) {
        if (is.null(rate) || is.na(rate)) return(.("Not available"))
        ci <- if (!is.null(k) && !is.null(n) && isTRUE(n > 0))
          tryCatch(stats::binom.test(k, n)$conf.int * 100, error = function(e) NULL)
        if (is.null(ci)) return(.("No universal benchmark: compare the rate with the rate expected in your tumour type and treatment setting."))
        sprintf(template, ci[1], ci[2])
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
