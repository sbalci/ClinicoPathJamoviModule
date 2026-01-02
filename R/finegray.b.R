#' @title Fine-Gray Competing Risks Regression
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom survival Surv survfit coxph
#' @importFrom cmprsk crr cuminc
#' @importFrom ggplot2 ggplot aes geom_step geom_ribbon geom_line labs theme_minimal
#' @importFrom ggplot2 scale_color_manual scale_fill_manual theme element_text
#' @importFrom dplyr mutate group_by summarise arrange filter
#' @importFrom stats confint pchisq qnorm
#' @export

finegrayClass <- if (requireNamespace("jmvcore")) R6::R6Class(
    "finegrayClass",
    inherit = finegrayBase,
    private = list(

        # Store model results
        .fgModel = NULL,
        .cifData = NULL,
        .grayTestResults = NULL,
        .competingEventMap = NULL,  # Stores mapping of competing event names to codes

        # Variable name escaping utility
        .escapeVar = function(x) {
            if (is.character(x)) {
                x <- gsub("[^A-Za-z0-9_]", "_", make.names(x))
            }
            return(x)
        },

        # Initialize analysis
        .init = function() {

            # Add instructions
            html <- private$.getInstructions()
            self$results$instructions$setContent(html)

            # Prepare coefficient table if covariates specified
            if (!is.null(self$options$covariates) && length(self$options$covariates) > 0) {
                shrTable <- self$results$shrTable
                shrTable$setNote("info", "Sub-hazard ratios from Fine-Gray model")
            }

        },

        # Main analysis
        .run = function() {

            # Check if data is ready
            if (is.null(self$options$survivalTime) ||
                is.null(self$options$status) ||
                is.null(self$options$eventOfInterest) ||
                is.null(self$options$censorLevel)) {
                return()
            }

            # Get data
            data <- private$.prepareData()
            if (is.null(data)) return()

            # Add procedure notes
            private$.addProcedureNotes(data)

            # Fit Fine-Gray model if covariates provided
            if (!is.null(self$options$covariates) && length(self$options$covariates) > 0) {
                private$.fitFineGrayModel(data)

                if (self$options$showCoefficientTable) {
                    private$.populateSHRTable()
                }

                if (self$options$showModelFit) {
                    private$.populateModelFitTable()
                }

                if (self$options$causeSpecificComparison) {
                    private$.compareToCauseSpecific(data)
                }

                if (self$options$showPredictionTable) {
                    private$.makePredictions(data)
                }
            }

            # Calculate cumulative incidence
            private$.calculateCIF(data)

            # Gray's test if grouping variable provided
            if (self$options$showGrayTest && !is.null(self$options$groupVar)) {
                private$.performGrayTest(data)
            }

            # Clinical interpretation
            if (self$options$showInterpretation) {
                private$.addInterpretation()
            }

        },

        # Prepare data for analysis
        .prepareData = function() {

            tryCatch({

                # Get variables (with escaping for special characters)
                timeVar <- private$.escapeVar(self$options$survivalTime)
                statusVar <- private$.escapeVar(self$options$status)

                # Extract data
                time <- jmvcore::toNumeric(self$data[[timeVar]])
                status <- self$data[[statusVar]]

                # Check for missing data
                if (any(is.na(time)) || any(is.na(status))) {
                    stop("Missing data in time or status variables. Please remove or impute.")
                }

                # Check for negative times
                if (any(time < 0, na.rm = TRUE)) {
                    stop("Survival time contains negative values.")
                }

                # Convert status to factor if needed
                if (!is.factor(status)) {
                    status <- as.factor(status)
                }

                # Get event levels
                eventLevel <- self$options$eventOfInterest
                censorLevel <- self$options$censorLevel

                # CRITICAL FIX: Create numeric status for Fine-Gray with DISTINCT competing causes
                # 0 = censored, 1 = event of interest, 2, 3, 4... = distinct competing events
                # Previous code collapsed all competing events into 2, which is mathematically invalid
                status_numeric <- rep(NA, length(status))
                status_numeric[status == censorLevel] <- 0
                status_numeric[status == eventLevel] <- 1

                # Assign DISTINCT codes to each competing event (2, 3, 4, ...)
                other_levels <- setdiff(levels(status), c(censorLevel, eventLevel))
                for (i in seq_along(other_levels)) {
                    lev <- other_levels[i]
                    status_numeric[status == lev] <- i + 1  # 2, 3, 4, ... for each competing cause
                }

                # Store competing event mapping for later reference
                competing_event_map <- setNames(2:(length(other_levels) + 1), other_levels)
                private$.competingEventMap <- competing_event_map

                # Create data frame
                data <- data.frame(
                    time = time,
                    status = status,
                    status_numeric = status_numeric,
                    status_label = status  # Keep original labels for plotting
                )

                # Add covariates if specified (with escaping)
                if (!is.null(self$options$covariates) && length(self$options$covariates) > 0) {
                    for (cov in self$options$covariates) {
                        cov_escaped <- private$.escapeVar(cov)
                        data[[cov]] <- self$data[[cov_escaped]]
                    }
                }

                # Add grouping variable if specified (with escaping)
                if (!is.null(self$options$groupVar)) {
                    groupVar_escaped <- private$.escapeVar(self$options$groupVar)
                    data$group <- self$data[[groupVar_escaped]]
                }

                # CRITICAL FIX: Strata option removed - cmprsk::crr() does not support stratification
                # Warn if user has strata option set (backward compatibility)
                if (!is.null(self$options$strata)) {
                    jmvcore::warning(paste(
                        "Stratification is not supported by Fine-Gray models (cmprsk::crr).",
                        "Consider using the stratification variable as a covariate instead,",
                        "or fitting separate models for each stratum."
                    ))
                }

                # Remove rows with NA
                data <- data[complete.cases(data), ]

                # Check sample size
                if (nrow(data) < 10) {
                    stop("Insufficient data. At least 10 complete observations required.")
                }

                # Check event counts
                event_counts <- table(data$status_numeric)
                if (sum(data$status_numeric == 1) < 5) {
                    stop("Insufficient events of interest (< 5). Cannot fit Fine-Gray model.")
                }

                return(data)

            }, error = function(e) {
                stop(paste("Error preparing data:", e$message))
            })

        },

        # Fit Fine-Gray subdistribution hazard model
        .fitFineGrayModel = function(data) {

            tryCatch({

                # Build formula
                covars <- self$options$covariates
                if (length(covars) == 0) {
                    return()
                }

                # Create covariate matrix
                cov_data <- data[, covars, drop = FALSE]

                # Convert factors to dummy variables if needed
                cov_matrix <- model.matrix(~ ., data = cov_data)[, -1, drop = FALSE]

                # Fit Fine-Gray model using cmprsk::crr
                private$.checkpoint()

                fgModel <- cmprsk::crr(
                    ftime = data$time,
                    fstatus = data$status_numeric,
                    cov1 = cov_matrix,
                    failcode = 1,  # Event of interest
                    cencode = 0     # Censoring code
                )

                private$.fgModel <- fgModel

            }, error = function(e) {
                stop(paste("Error fitting Fine-Gray model:", e$message))
            })

        },

        # Populate sub-hazard ratio table
        .populateSHRTable = function() {

            if (is.null(private$.fgModel)) return()

            tryCatch({

                shrTable <- self$results$shrTable

                model <- private$.fgModel

                # Get coefficients
                coef <- model$coef
                se <- sqrt(diag(model$var))
                z <- coef / se
                p <- 2 * (1 - pnorm(abs(z)))

                # Calculate confidence intervals
                # CRITICAL FIX: Convert percentage to proportion before qnorm
                # confLevel is stored as percentage (e.g., 95), must convert to 0.95
                conf_level <- self$options$confLevel / 100
                z_crit <- qnorm((1 + conf_level) / 2)

                ci_lower <- coef - z_crit * se
                ci_upper <- coef + z_crit * se

                # Exponentiate if requested
                if (self$options$exponentiate) {
                    estimate <- exp(coef)
                    ci_lower <- exp(ci_lower)
                    ci_upper <- exp(ci_upper)
                } else {
                    estimate <- coef
                }

                # Get covariate names
                cov_names <- names(coef)
                if (is.null(cov_names)) {
                    cov_names <- paste0("Covariate_", 1:length(coef))
                }

                # Populate table
                for (i in 1:length(estimate)) {

                    row <- list(
                        term = cov_names[i],
                        estimate = estimate[i],
                        se = se[i],
                        ci_lower = ci_lower[i],
                        ci_upper = ci_upper[i],
                        z_value = z[i],
                        p_value = p[i]
                    )

                    shrTable$addRow(rowKey = i, values = row)
                }

                # Add note
                if (self$options$exponentiate) {
                    shrTable$setNote("shr", "Sub-hazard ratios (sHR) represent the effect on cumulative incidence")
                } else {
                    shrTable$setNote("log_shr", "Log sub-hazard ratios. Exponentiate for sHR.")
                }

            }, error = function(e) {
                jmvcore::warning(paste("Error creating coefficient table:", e$message))
            })

        },

        # Populate model fit table
        .populateModelFitTable = function() {

            if (is.null(private$.fgModel)) return()

            tryCatch({

                fitTable <- self$results$modelFitTable

                model <- private$.fgModel

                # Calculate pseudo-R²
                # Using Gray's modification of Kent and O'Quigley's R²
                loglik <- model$loglik
                loglik_null <- model$loglik.null

                if (!is.null(loglik) && !is.null(loglik_null)) {
                    pseudo_r2 <- 1 - exp(-2 * (loglik - loglik_null) / length(model$ftime))

                    fitTable$addRow(rowKey = "r2", values = list(
                        statistic = "Pseudo R²",
                        value = pseudo_r2
                    ))
                }

                # Number of observations
                fitTable$addRow(rowKey = "n", values = list(
                    statistic = "Observations",
                    value = length(model$ftime)
                ))

                # Number of events
                n_events <- sum(model$fstatus == 1)
                fitTable$addRow(rowKey = "events", values = list(
                    statistic = "Events of Interest",
                    value = n_events
                ))

                # CRITICAL FIX: Report each competing event separately
                if (!is.null(private$.competingEventMap) && length(private$.competingEventMap) > 0) {
                    for (event_name in names(private$.competingEventMap)) {
                        event_code <- private$.competingEventMap[event_name]
                        n_competing <- sum(model$fstatus == event_code)
                        fitTable$addRow(rowKey = paste0("competing_", event_code), values = list(
                            statistic = paste0("Competing: ", event_name),
                            value = n_competing
                        ))
                    }
                } else {
                    # Fallback for backward compatibility (if no mapping stored)
                    n_competing_total <- sum(model$fstatus >= 2)
                    fitTable$addRow(rowKey = "competing", values = list(
                        statistic = "Competing Events (total)",
                        value = n_competing_total
                    ))
                }

            }, error = function(e) {
                jmvcore::warning(paste("Error creating model fit table:", e$message))
            })

        },

        # Calculate cumulative incidence functions
        .calculateCIF = function(data) {

            tryCatch({

                # Calculate CIF using cmprsk::cuminc
                if (!is.null(self$options$groupVar)) {
                    # Grouped analysis
                    ci_result <- cmprsk::cuminc(
                        ftime = data$time,
                        fstatus = data$status_numeric,
                        group = data$group
                    )
                } else {
                    # Overall analysis
                    ci_result <- cmprsk::cuminc(
                        ftime = data$time,
                        fstatus = data$status_numeric
                    )
                }

                private$.cifData <- ci_result

            }, error = function(e) {
                jmvcore::warning(paste("Error calculating CIF:", e$message))
            })

        },

        # Perform Gray's test
        .performGrayTest = function(data) {

            if (is.null(private$.cifData)) return()

            tryCatch({

                grayTable <- self$results$grayTestTable

                # Extract test results from cuminc object
                ci_result <- private$.cifData

                # Gray's test is automatically performed by cuminc when groups specified
                tests <- ci_result$Tests

                if (!is.null(tests)) {

                    # Event of interest
                    if ("1" %in% rownames(tests)) {
                        test_result <- tests["1", ]

                        grayTable$addRow(rowKey = "event", values = list(
                            event_type = "Event of Interest",
                            chisq = test_result["stat"],
                            df = test_result["df"],
                            p_value = test_result["pv"]
                        ))
                    }

                    # CRITICAL FIX: Report Gray's test for EACH distinct competing event
                    if (!is.null(private$.competingEventMap) && length(private$.competingEventMap) > 0) {
                        for (event_name in names(private$.competingEventMap)) {
                            event_code <- as.character(private$.competingEventMap[event_name])
                            if (event_code %in% rownames(tests)) {
                                test_result <- tests[event_code, ]

                                grayTable$addRow(rowKey = paste0("competing_", event_code), values = list(
                                    event_type = paste0("Competing: ", event_name),
                                    chisq = test_result["stat"],
                                    df = test_result["df"],
                                    p_value = test_result["pv"]
                                ))
                            }
                        }
                    } else {
                        # Fallback: check for any competing events in tests
                        for (row_name in rownames(tests)) {
                            if (row_name != "1" && row_name != "0") {
                                test_result <- tests[row_name, ]
                                grayTable$addRow(rowKey = paste0("competing_", row_name), values = list(
                                    event_type = paste0("Competing Event ", row_name),
                                    chisq = test_result["stat"],
                                    df = test_result["df"],
                                    p_value = test_result["pv"]
                                ))
                            }
                        }
                    }

                    grayTable$setNote("gray", "Gray's test compares cumulative incidence curves between groups for each event type")
                }

            }, error = function(e) {
                jmvcore::warning(paste("Error performing Gray's test:", e$message))
            })

        },

        # Plot cumulative incidence function
        .plotCIF = function(image, ...) {

            if (is.null(private$.cifData)) return()

            tryCatch({

                cifData <- private$.cifData

                # Extract plot data from cuminc object
                plot_data <- NULL

                if (!is.null(self$options$groupVar)) {
                    # Grouped plots
                    for (i in 1:length(cifData)) {
                        if (is.list(cifData[[i]])) {
                            group_name <- names(cifData)[i]
                            event_type <- gsub(".*\\s", "", group_name)
                            group_label <- gsub("\\s.*", "", group_name)

                            # CRITICAL FIX: Extract confidence bounds from cuminc object
                            df <- data.frame(
                                time = cifData[[i]]$time,
                                cif = cifData[[i]]$est,
                                ci_lower = if (!is.null(cifData[[i]]$var)) {
                                    pmax(0, cifData[[i]]$est - 1.96 * sqrt(cifData[[i]]$var))
                                } else {
                                    NA
                                },
                                ci_upper = if (!is.null(cifData[[i]]$var)) {
                                    pmin(1, cifData[[i]]$est + 1.96 * sqrt(cifData[[i]]$var))
                                } else {
                                    NA
                                },
                                group = group_label,
                                event = event_type
                            )

                            plot_data <- rbind(plot_data, df)
                        }
                    }
                } else {
                    # Overall plot
                    for (i in 1:length(cifData)) {
                        if (is.list(cifData[[i]])) {
                            event_type <- names(cifData)[i]

                            # CRITICAL FIX: Extract confidence bounds from cuminc object
                            df <- data.frame(
                                time = cifData[[i]]$time,
                                cif = cifData[[i]]$est,
                                ci_lower = if (!is.null(cifData[[i]]$var)) {
                                    pmax(0, cifData[[i]]$est - 1.96 * sqrt(cifData[[i]]$var))
                                } else {
                                    NA
                                },
                                ci_upper = if (!is.null(cifData[[i]]$var)) {
                                    pmin(1, cifData[[i]]$est + 1.96 * sqrt(cifData[[i]]$var))
                                } else {
                                    NA
                                },
                                event = event_type
                            )

                            plot_data <- rbind(plot_data, df)
                        }
                    }
                }

                if (is.null(plot_data)) return()

                # CRITICAL FIX: Show ALL competing event CIFs, not just event of interest
                # Old code filtered to event == "1", hiding all competing risks
                # Now we show all events and distinguish them by color/linetype

                # Add readable event labels using the mapping
                plot_data$event_label <- plot_data$event
                if (!is.null(private$.competingEventMap)) {
                    plot_data$event_label[plot_data$event == "1"] <- "Event of Interest"
                    for (event_name in names(private$.competingEventMap)) {
                        event_code <- as.character(private$.competingEventMap[event_name])
                        plot_data$event_label[plot_data$event == event_code] <- paste0("Competing: ", event_name)
                    }
                } else {
                    # Fallback labels
                    plot_data$event_label[plot_data$event == "1"] <- "Event of Interest"
                    plot_data$event_label[plot_data$event != "1"] <- "Competing Event"
                }

                # Create plot with event type distinguished by color and linetype
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = time, y = cif,
                                                             color = event_label,
                                                             linetype = event_label)) +
                    ggplot2::geom_step(size = 1.2) +
                    ggplot2::labs(
                        x = "Time",
                        y = "Cumulative Incidence",
                        title = "Cumulative Incidence Functions",
                        color = "Event Type",
                        linetype = "Event Type"
                    ) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(face = "bold", size = 14),
                        axis.title = ggplot2::element_text(size = 12),
                        legend.position = "right"
                    ) +
                    ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent)

                # Add grouping if specified (facet by group)
                if (!is.null(self$options$groupVar)) {
                    p <- p + ggplot2::facet_wrap(~ group)
                }

                # Apply color scheme
                p <- p + private$.getColorScale()

                # Add confidence intervals if requested
                if (self$options$cifConfInt && !is.null(plot_data$ci_lower)) {
                    p <- p + ggplot2::geom_ribbon(
                        ggplot2::aes(ymin = ci_lower, ymax = ci_upper, fill = group),
                        alpha = 0.2
                    )
                }

                print(p)
                TRUE

            }, error = function(e) {
                jmvcore::warning(paste("Error creating CIF plot:", e$message))
                FALSE
            })

        },

        # Plot stacked CIF
        .plotStackedCIF = function(image, ...) {

            if (is.null(private$.cifData)) return()

            tryCatch({

                # Implementation for stacked CIF plot
                # Shows event of interest + competing events + survival stacked to 100%
                jmvcore::warning("Stacked CIF plot not yet implemented")

                TRUE

            }, error = function(e) {
                jmvcore::warning(paste("Error creating stacked CIF plot:", e$message))
                FALSE
            })

        },

        # Plot 1-KM vs CIF comparison
        .plotKMvsCIF = function(image, ...) {

            tryCatch({

                # Implementation showing why 1-KM overestimates cumulative incidence
                jmvcore::warning("1-KM vs CIF comparison not yet implemented")

                TRUE

            }, error = function(e) {
                jmvcore::warning(paste("Error creating comparison plot:", e$message))
                FALSE
            })

        },

        # Plot cause-specific hazards
        .plotCauseSpecific = function(image, ...) {

            tryCatch({

                # Implementation for cause-specific hazard plots
                jmvcore::warning("Cause-specific hazard plot not yet implemented")

                TRUE

            }, error = function(e) {
                jmvcore::warning(paste("Error creating cause-specific plot:", e$message))
                FALSE
            })

        },

        # Plot diagnostics
        .plotDiagnostics = function(image, ...) {

            if (is.null(private$.fgModel)) return()

            tryCatch({

                # Implementation for diagnostic plots
                jmvcore::warning("Diagnostic plots not yet implemented")

                TRUE

            }, error = function(e) {
                jmvcore::warning(paste("Error creating diagnostic plots:", e$message))
                FALSE
            })

        },

        # Compare to cause-specific hazards
        .compareToCauseSpecific = function(data) {

            tryCatch({

                # Fit cause-specific Cox model for comparison
                # Implementation pending
                jmvcore::warning("Cause-specific comparison not yet fully implemented")

            }, error = function(e) {
                jmvcore::warning(paste("Error comparing to cause-specific:", e$message))
            })

        },

        # Make predictions
        .makePredictions = function(data) {

            if (is.null(private$.fgModel)) return()

            tryCatch({

                # Parse time points
                time_points <- private$.parseTimePoints(self$options$predictAt)
                if (is.null(time_points)) return()

                # Get covariate pattern
                cov_pattern <- private$.getCovariatePattern(data)
                if (is.null(cov_pattern)) return()

                # Make predictions
                # Implementation using predict.crr or manual calculation
                jmvcore::warning("Predictions not yet fully implemented")

            }, error = function(e) {
                jmvcore::warning(paste("Error making predictions:", e$message))
            })

        },

        # Get covariate pattern for predictions
        .getCovariatePattern = function(data) {

            pattern_type <- self$options$predictCovariatePattern

            covars <- self$options$covariates
            if (length(covars) == 0) return(NULL)

            cov_data <- data[, covars, drop = FALSE]

            if (pattern_type == "mean") {
                # Mean for numeric, mode for factors
                pattern <- lapply(cov_data, function(x) {
                    if (is.numeric(x)) {
                        mean(x, na.rm = TRUE)
                    } else {
                        names(sort(table(x), decreasing = TRUE)[1])
                    }
                })
            } else if (pattern_type == "median") {
                # Median for numeric, mode for factors
                pattern <- lapply(cov_data, function(x) {
                    if (is.numeric(x)) {
                        median(x, na.rm = TRUE)
                    } else {
                        names(sort(table(x), decreasing = TRUE)[1])
                    }
                })
            } else if (pattern_type == "reference") {
                # Reference levels
                pattern <- lapply(cov_data, function(x) {
                    if (is.factor(x)) {
                        levels(x)[1]
                    } else {
                        0
                    }
                })
            } else if (pattern_type == "custom") {
                # Parse custom values
                pattern <- private$.parseCustomCovariates()
            }

            return(pattern)

        },

        # Parse time points
        .parseTimePoints = function(time_str) {

            if (is.null(time_str) || time_str == "") return(NULL)

            times <- tryCatch({
                as.numeric(unlist(strsplit(time_str, ",")))
            }, error = function(e) {
                NULL
            })

            return(times)

        },

        # Parse custom covariate values
        .parseCustomCovariates = function() {

            cov_str <- self$options$customCovariateValues
            if (is.null(cov_str) || cov_str == "") return(NULL)

            # Parse format like "age=65,stage=III,treatment=chemo"
            # Implementation pending

            return(NULL)

        },

        # Get color scale based on scheme
        .getColorScale = function() {

            scheme <- self$options$colorScheme

            if (scheme == "colorblind") {
                return(ggplot2::scale_color_brewer(palette = "Set2"))
            } else if (scheme == "grayscale") {
                return(ggplot2::scale_color_grey())
            } else if (scheme == "nejm") {
                return(ggplot2::scale_color_manual(values = c("#BC3C29", "#0072B5", "#E18727", "#20854E")))
            } else if (scheme == "lancet") {
                return(ggplot2::scale_color_manual(values = c("#00468B", "#ED0000", "#42B540", "#0099B4")))
            } else {
                return(ggplot2::scale_color_brewer(palette = "Set1"))
            }

        },

        # Add procedure notes
        .addProcedureNotes = function(data) {

            html <- "<div style='font-family: Arial, sans-serif; line-height: 1.6;'>"
            html <- paste0(html, "<h3 style='color: #333;'>Analysis Summary</h3>")

            # Sample size
            html <- paste0(html, "<p><b>Sample size:</b> ", nrow(data), " observations</p>")

            # Event counts
            event_counts <- table(data$status_numeric)
            html <- paste0(html, "<p><b>Events of interest:</b> ", event_counts["1"], "</p>")

            # CRITICAL FIX: Report each competing event separately
            if (!is.null(private$.competingEventMap) && length(private$.competingEventMap) > 0) {
                for (event_name in names(private$.competingEventMap)) {
                    event_code <- as.character(private$.competingEventMap[event_name])
                    if (event_code %in% names(event_counts)) {
                        html <- paste0(html, "<p><b>Competing (", event_name, "):</b> ",
                                     event_counts[event_code], "</p>")
                    }
                }
            } else {
                # Fallback: report total competing events
                if ("2" %in% names(event_counts)) {
                    competing_total <- sum(event_counts[names(event_counts) >= "2"])
                    html <- paste0(html, "<p><b>Competing events (total):</b> ", competing_total, "</p>")
                }
            }

            html <- paste0(html, "<p><b>Censored:</b> ", event_counts["0"], "</p>")

            # Model type
            if (!is.null(self$options$covariates) && length(self$options$covariates) > 0) {
                html <- paste0(html, "<p><b>Model:</b> Fine-Gray subdistribution hazard regression")
                html <- paste0(html, " with ", length(self$options$covariates), " covariate(s)</p>")
            }

            html <- paste0(html, "</div>")

            self$results$procedureNotes$setContent(html)

        },

        # Add clinical interpretation
        .addInterpretation = function() {

            html <- "<div style='font-family: Arial, sans-serif; line-height: 1.6;'>"
            html <- paste0(html, "<h3 style='color: #333; border-bottom: 2px solid #ccc; padding-bottom: 5px;'>")
            html <- paste0(html, "Clinical Interpretation</h3>")

            html <- paste0(html, "<h4>Understanding Fine-Gray Regression</h4>")
            html <- paste0(html, "<p>The Fine-Gray model estimates <b>sub-hazard ratios (sHR)</b> ")
            html <- paste0(html, "which represent the effect of covariates on the <b>cumulative incidence</b> ")
            html <- paste0(html, "of the event of interest, accounting for competing events.</p>")

            html <- paste0(html, "<h4>Interpreting Sub-Hazard Ratios</h4>")
            html <- paste0(html, "<ul>")
            html <- paste0(html, "<li>sHR > 1: Covariate increases cumulative incidence of the event</li>")
            html <- paste0(html, "<li>sHR < 1: Covariate decreases cumulative incidence of the event</li>")
            html <- paste0(html, "<li>sHR = 1: Covariate has no effect on cumulative incidence</li>")
            html <- paste0(html, "</ul>")

            html <- paste0(html, "<h4>Key Differences from Cox Regression</h4>")
            html <- paste0(html, "<ul>")
            html <- paste0(html, "<li><b>Subdistribution hazard</b> models cumulative incidence (probability)</li>")
            html <- paste0(html, "<li><b>Cause-specific hazard</b> models instantaneous risk conditional on not experiencing any event</li>")
            html <- paste0(html, "<li>Use Fine-Gray when interest is in <b>predicting absolute risk</b></li>")
            html <- paste0(html, "<li>Use cause-specific when interest is in <b>etiologic effects</b></li>")
            html <- paste0(html, "</ul>")

            html <- paste0(html, "<h4>Gray's Test</h4>")
            html <- paste0(html, "<p>Gray's test is analogous to the log-rank test but appropriate for ")
            html <- paste0(html, "competing risks. It tests whether cumulative incidence curves differ ")
            html <- paste0(html, "between groups.</p>")

            html <- paste0(html, "<h4>References</h4>")
            html <- paste0(html, "<ul style='font-size: 0.9em;'>")
            html <- paste0(html, "<li>Fine JP, Gray RJ (1999). A proportional hazards model for the ")
            html <- paste0(html, "subdistribution of a competing risk. <i>JASA</i> 94:496-509.</li>")
            html <- paste0(html, "<li>Lau B et al. (2009). Competing risk regression models for ")
            html <- paste0(html, "epidemiologic data. <i>Am J Epidemiol</i> 170:244-256.</li>")
            html <- paste0(html, "</ul>")

            html <- paste0(html, "</div>")

            self$results$interpretation$setContent(html)

        },

        # Get instructions
        .getInstructions = function() {

            html <- "<div style='font-family: Arial, sans-serif; line-height: 1.6;'>"
            html <- paste0(html, "<h3 style='color: #333;'>Fine-Gray Competing Risks Regression</h3>")

            html <- paste0(html, "<h4>Data Requirements</h4>")
            html <- paste0(html, "<ol>")
            html <- paste0(html, "<li><b>Survival Time:</b> Numeric variable with time to event or censoring</li>")
            html <- paste0(html, "<li><b>Event Status:</b> Factor variable with multiple levels:")
            html <- paste0(html, "<ul>")
            html <- paste0(html, "<li>One level for censoring (e.g., 0, 'censored')</li>")
            html <- paste0(html, "<li>One level for event of interest (e.g., 1, 'disease death')</li>")
            html <- paste0(html, "<li>One or more levels for competing events (e.g., 2, 'other death')</li>")
            html <- paste0(html, "</ul></li>")
            html <- paste0(html, "<li><b>Covariates:</b> Optional predictors (continuous or categorical)</li>")
            html <- paste0(html, "<li><b>Grouping Variable:</b> Optional for comparing groups</li>")
            html <- paste0(html, "</ol>")

            html <- paste0(html, "<h4>Analysis Steps</h4>")
            html <- paste0(html, "<ol>")
            html <- paste0(html, "<li>Select survival time and event status variables</li>")
            html <- paste0(html, "<li>Specify which level represents event of interest</li>")
            html <- paste0(html, "<li>Specify which level represents censoring</li>")
            html <- paste0(html, "<li>Add covariates to model their effect on cumulative incidence</li>")
            html <- paste0(html, "<li>Optionally add grouping variable for Gray's test</li>")
            html <- paste0(html, "</ol>")

            html <- paste0(html, "<p style='color: #666; font-size: 0.9em;'>")
            html <- paste0(html, "<i>Note: Fine-Gray regression models the cumulative incidence function ")
            html <- paste0(html, "(subdistribution hazard), which is different from cause-specific hazards.</i>")
            html <- paste0(html, "</p>")

            html <- paste0(html, "</div>")

            return(html)

        }
    ), # End of private list
    public = list(
        #' @description
        #' Generate R source code for finegray analysis
        #' @return Character string with R syntax for reproducible analysis
        asSource = function() {
            survivalTime <- self$options$survivalTime
            status <- self$options$status
            covariates <- self$options$covariates
            groupVar <- self$options$groupVar

            if (is.null(survivalTime) || is.null(status))
                return('')

            # Escape survivalTime variable
            survivalTime_escaped <- if (!is.null(survivalTime) && !identical(make.names(survivalTime), survivalTime)) {
                paste0('`', survivalTime, '`')
            } else {
                survivalTime
            }

            # Escape status variable
            status_escaped <- if (!is.null(status) && !identical(make.names(status), status)) {
                paste0('`', status, '`')
            } else {
                status
            }

            # Build required arguments
            survivalTime_arg <- paste0('survivalTime = "', survivalTime_escaped, '"')
            status_arg <- paste0('status = "', status_escaped, '"')

            # Build optional covariates argument
            covariates_arg <- ''
            if (!is.null(covariates) && length(covariates) > 0) {
                covariates_escaped <- sapply(covariates, function(v) {
                    if (!is.null(v) && !identical(make.names(v), v))
                        paste0('`', v, '`')
                    else
                        v
                })
                covariates_arg <- paste0(',\n    covariates = c(',
                                       paste(sapply(covariates_escaped, function(v) paste0('"', v, '"')), collapse = ', '),
                                       ')')
            }

            # Build optional groupVar argument
            groupVar_arg <- ''
            if (!is.null(groupVar)) {
                groupVar_escaped <- if (!identical(make.names(groupVar), groupVar)) {
                    paste0('`', groupVar, '`')
                } else {
                    groupVar
                }
                groupVar_arg <- paste0(',\n    groupVar = "', groupVar_escaped, '"')
            }

            # Get other arguments using base helper (if available)
            args <- ''
            if (!is.null(private$.asArgs)) {
                args <- private$.asArgs(incData = FALSE)
            }
            if (args != '')
                args <- paste0(',\n    ', args)

            # Get package name dynamically
            pkg_name <- utils::packageName()
            if (is.null(pkg_name)) pkg_name <- "ClinicoPath"  # fallback

            # Build complete function call
            paste0(pkg_name, '::finegray(\n    data = data,\n    ',
                   survivalTime_arg, ',\n    ', status_arg, covariates_arg, groupVar_arg, args, ')')
        }
    ) # End of public list
)
