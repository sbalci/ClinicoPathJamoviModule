
timerocClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "timerocClass",
    inherit = timerocBase,
    private = list(
        .init = function() {
            if (is.null(self$options$marker) ||
                is.null(self$options$outcome) ||
                is.null(self$options$elapsedtime)) {
                self$results$text$setContent(
                    "Please select time, outcome and marker variables")
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

            # Convert outcome to 0/1
            if (is.factor(data[[outcome_var]])) {
                data$status <- ifelse(data[[outcome_var]] == outcome_level, 1, 0)
            } else {
                data$status <- data[[outcome_var]]
            }

            # Clean time and marker
            data$time <- jmvcore::toNumeric(data[[time_var]])
            data$marker <- jmvcore::toNumeric(data[[marker_var]])

            # Remove missing values
            data <- na.omit(data[c("time", "status", "marker")])

            private$.data <- data
        },

        .run = function() {
            if (is.null(private$.data))
                return()

            # Get timepoints
            timepoints <- as.numeric(unlist(strsplit(self$options$timepoints, ",")))

            # Compute time-dependent ROC using timeROC package
            tryCatch({
                fit <- timeROC::timeROC(
                    T = private$.data$time,
                    delta = private$.data$status,
                    marker = private$.data$marker,
                    cause = 1,
                    times = timepoints,
                    iid = self$options$bootstrapCI,
                    n.bootstrap = self$options$nboot
                )

                # Store results
                private$.fit <- fit

                # Fill AUC table
                table <- self$results$aucTable
                for (i in seq_along(timepoints)) {
                    row <- list(
                        timepoint = timepoints[i],
                        auc = fit$AUC[i],
                        se = sqrt(fit$var.AUC[i]),
                        ci_lower = fit$AUC[i] - 1.96*sqrt(fit$var.AUC[i]),
                        ci_upper = fit$AUC[i] + 1.96*sqrt(fit$var.AUC[i])
                    )
                    table$addRow(rowKey=i, values=row)
                }

                # Create summary text
                text <- sprintf(
                    "Time-dependent ROC analysis for marker: %s\n\n",
                    self$options$marker
                )
                text <- paste0(text, "Method: ", self$options$method, "\n")
                if (self$options$bootstrapCI)
                    text <- paste0(text,
                                   sprintf("Bootstrap samples: %d\n",
                                           self$options$nboot))

                self$results$text$setContent(text)

            }, error = function(e) {
                self$results$text$setContent(
                    sprintf("Error in analysis: %s", e$message))
            })
        },

        .plotROC = function(image, ...) {
            if (!self$options$plotROC || is.null(private$.fit))
                return()

            plot <- plot(private$.fit,
                         time = as.numeric(unlist(strsplit(
                             self$options$timepoints, ","))),
                         title = sprintf("Time-dependent ROC curves for %s",
                                         self$options$marker))

            print(plot)
            TRUE
        },

        .plotAUC = function(image, ...) {
            if (!self$options$plotAUC || is.null(private$.fit))
                return()

            timepoints <- as.numeric(unlist(strsplit(
                self$options$timepoints, ",")))
            auc <- private$.fit$AUC
            se <- sqrt(private$.fit$var.AUC)

            plot <- ggplot2::ggplot(
                data = data.frame(
                    time = timepoints,
                    auc = auc,
                    lower = auc - 1.96*se,
                    upper = auc + 1.96*se
                ),
                ggplot2::aes(x = time, y = auc)
            ) +
                ggplot2::geom_line() +
                ggplot2::geom_ribbon(
                    ggplot2::aes(ymin = lower, ymax = upper),
                    alpha = 0.2
                ) +
                ggplot2::ylim(0.5, 1) +
                ggplot2::xlab("Time") +
                ggplot2::ylab("AUC") +
                ggplot2::ggtitle(
                    sprintf("AUC over time for %s",
                            self$options$marker)
                )

            print(plot)
            TRUE
        }
        )
)
