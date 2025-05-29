timerocClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "timerocClass",
    inherit = timerocBase,
    private = list(
        .init = function() {
            # Show welcome/instruction message if variables not selected
            if (is.null(self$options$marker) ||
                is.null(self$options$outcome) ||
                is.null(self$options$elapsedtime)) {

                welcome <- glue::glue("
                    <br>Welcome to Time-Dependent ROC Analysis
                    <br><br>
                    This tool evaluates how well a continuous marker predicts survival outcomes at different timepoints.
                    <br><br>
                    Please select:
                    <br>- Time variable (follow-up duration)
                    <br>- Outcome variable (event status)
                    <br>- Marker variable (continuous predictor to evaluate)
                    <br><br>
                    The analysis will provide:
                    <br>- Time-specific ROC curves
                    <br>- AUC values at specified timepoints
                    <br>- Confidence intervals via bootstrap (optional)
                ")

                self$results$text$setContent(welcome)
                return()
            }

            private$.cleanData()
        },

        .cleanData = function() {
            # Get data
            data <- self$data

            # Get variable names
            time_var <- self$options$elapsedtime
            outcome_var <- self$options$outcome
            marker_var <- self$options$marker
            outcome_level <- self$options$outcomeLevel

            # Input validation
            if (!is.numeric(data[[time_var]])) {
                stop("Time variable must be numeric")
            }

            if (!is.numeric(data[[marker_var]])) {
                stop("Marker variable must be numeric")
            }

            # Convert outcome to 0/1
            if (is.factor(data[[outcome_var]])) {
                data$status <- ifelse(data[[outcome_var]] == outcome_level, 1, 0)
            } else {
                if (!all(data[[outcome_var]] %in% c(0,1,NA))) {
                    stop("Numeric outcome must contain only 0s and 1s")
                }
                data$status <- data[[outcome_var]]
            }

            # Clean time and marker
            data$time <- jmvcore::toNumeric(data[[time_var]])
            data$marker <- jmvcore::toNumeric(data[[marker_var]])

            # Remove missing values
            data <- na.omit(data[c("time", "status", "marker")])

            # Validate final dataset
            if (nrow(data) == 0) {
                stop("No complete cases remaining after removing missing values")
            }

            # Store cleaned data
            private$.data <- data
        },

        .run = function() {
            if (is.null(private$.data))
                return()

            # Parse and validate timepoints
            timepoints <- tryCatch({
                pts <- as.numeric(trimws(unlist(strsplit(self$options$timepoints, ","))))
                pts <- sort(unique(pts[!is.na(pts)]))
                if (length(pts) == 0) c(12, 36, 60) else pts
            }, error = function(e) {
                warning("Invalid timepoints specified, using defaults")
                c(12, 36, 60)
            })

            # Compute time-dependent ROC
            tryCatch({
                fit <- timeROC::timeROC(
                    T = private$.data$time,
                    delta = private$.data$status,
                    marker = private$.data$marker,
                    cause = 1,
                    times = timepoints,
                    iid = self$options$bootstrapCI,
                    n.bootstrap = self$options$nboot,
                    method = self$options$method
                )

                # Store results
                private$.fit <- fit

                # Fill AUC table
                table <- self$results$aucTable
                table$setRow(NULL) # Clear existing rows

                for (i in seq_along(timepoints)) {
                    row <- list(
                        timepoint = timepoints[i],
                        auc = round(fit$AUC[i], 3),
                        se = round(sqrt(fit$var.AUC[i]), 3),
                        ci_lower = round(fit$AUC[i] - 1.96*sqrt(fit$var.AUC[i]), 3),
                        ci_upper = round(fit$AUC[i] + 1.96*sqrt(fit$var.AUC[i]), 3)
                    )
                    table$addRow(rowKey=i, values=row)
                }

                # Create interpretation text
                text <- sprintf(
                    "<b>Time-dependent ROC Analysis Results for %s</b><br><br>",
                    self$options$marker
                )

                text <- paste0(text, "Method: ", self$options$method, "<br>")
                if (self$options$bootstrapCI) {
                    text <- paste0(text,
                                   sprintf("Bootstrap samples: %d<br><br>",
                                           self$options$nboot))
                }

                # Add interpretation for each timepoint
                for (i in seq_along(timepoints)) {
                    auc <- fit$AUC[i]
                    ci_lower <- fit$AUC[i] - 1.96*sqrt(fit$var.AUC[i])
                    ci_upper <- fit$AUC[i] + 1.96*sqrt(fit$var.AUC[i])

                    interp <- sprintf(
                        "At %d months, AUC = %.3f [%.3f - %.3f]<br>",
                        timepoints[i], auc, ci_lower, ci_upper
                    )

                    # Add interpretation of AUC value
                    strength <- dplyr::case_when(
                        auc >= 0.9 ~ "excellent",
                        auc >= 0.8 ~ "good",
                        auc >= 0.7 ~ "fair",
                        auc >= 0.6 ~ "poor",
                        TRUE ~ "failed"
                    )

                    interp <- paste0(interp,
                                     sprintf("Discriminative ability at this timepoint is %s<br><br>",
                                             strength))

                    text <- paste0(text, interp)
                }

                self$results$text$setContent(text)

            }, error = function(e) {
                self$results$text$setContent(
                    sprintf("<br>Error in analysis: %s<br>Please check your input data and settings.",
                            e$message))
            })
        },

        .plotROC = function(image, ...) {
            if (!self$options$plotROC || is.null(private$.fit))
                return()

            # Extract timepoints
            timepoints <- as.numeric(unlist(strsplit(self$options$timepoints, ",")))

            # Create enhanced ROC plot
            plot <- timeROC::plot.timeROC(
                private$.fit,
                time = timepoints,
                title = sprintf("Time-dependent ROC curves for %s",
                                self$options$marker),
                col = c("blue", "red", "green"),
                lwd = 2,
                legend = TRUE,
                legend.pos = "bottomright",
                conf.int = self$options$bootstrapCI
            )

            # Add reference line
            graphics::abline(0, 1, lty = 2, col = "gray")

            # Add annotations
            for (i in seq_along(timepoints)) {
                auc <- round(private$.fit$AUC[i], 3)
                graphics::legend(
                    "bottomright",
                    legend = sprintf("AUC at %d months = %.3f",
                                     timepoints[i], auc),
                    bty = "n"
                )
            }

            print(plot)
            TRUE
        },

        .plotAUC = function(image, ...) {
            if (!self$options$plotAUC || is.null(private$.fit))
                return()

            timepoints <- as.numeric(unlist(strsplit(self$options$timepoints, ",")))
            auc <- private$.fit$AUC
            se <- sqrt(private$.fit$var.AUC)

            # Create data frame for plotting
            plot_data <- data.frame(
                time = timepoints,
                auc = auc,
                lower = auc - 1.96*se,
                upper = auc + 1.96*se
            )

            # Create enhanced AUC plot
            plot <- ggplot2::ggplot(
                data = plot_data,
                ggplot2::aes(x = time, y = auc)
            ) +
                ggplot2::geom_line(color = "blue", size = 1) +
                ggplot2::geom_point(color = "blue", size = 3) +
                ggplot2::geom_ribbon(
                    ggplot2::aes(ymin = lower, ymax = upper),
                    alpha = 0.2,
                    fill = "blue"
                ) +
                ggplot2::geom_hline(
                    yintercept = 0.5,
                    linetype = "dashed",
                    color = "red"
                ) +
                ggplot2::scale_y_continuous(
                    limits = c(0.4, 1),
                    breaks = seq(0.4, 1, by = 0.1)
                ) +
                ggplot2::xlab(sprintf("Time (%s)",
                                      self$options$timetypeoutput)) +
                ggplot2::ylab("AUC") +
                ggplot2::ggtitle(
                    sprintf("AUC over time for %s",
                            self$options$marker),
                    subtitle = sprintf("Method: %s",
                                       self$options$method)
                ) +
                ggplot2::theme_bw() +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(size = 14, face = "bold"),
                    plot.subtitle = ggplot2::element_text(size = 12),
                    axis.title = ggplot2::element_text(size = 12),
                    axis.text = ggplot2::element_text(size = 10)
                )

            print(plot)
            TRUE
        }
    )
)
