#' @title Bayesian Decision Curve Analysis
#' @return Tables and plots for decision curve analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @importFrom stats rbeta quantile
#' @importFrom RColorBrewer brewer.pal
#' @importFrom scales percent

bayesdcaClass <- if (requireNamespace("jmvcore"))
    R6::R6Class(
        "bayesdcaClass",
        inherit = bayesdcaBase,
        private = list(
            # Storage for analysis results
            .thresholds = NULL,
            .dcaResults = NULL,
            .posteriorDraws = NULL,

            .init = function() {
                # Initialize the analysis
                if (is.null(self$options$outcomes) ||
                    is.null(self$options$predictors)) {
                    html <- self$results$instructions
                    html$setContent(
                        "<html>
                        <head>
                        </head>
                        <body>
                        <div class='instructions'>
                        <p><b>Bayesian Decision Curve Analysis</b></p>
                        <p>This analysis evaluates the clinical utility of prediction models and diagnostic tests across different decision thresholds.</p>
                        <p>To get started:</p>
                        <ol>
                        <li>Select a binary outcome variable (0/1) in the 'Outcome' field</li>
                        <li>Select one or more variables in the 'Models or Tests' field:
                          <ul>
                            <li>For prediction models: select continuous variables with probability predictions</li>
                            <li>For diagnostic tests: select binary variables (0/1) with test results</li>
                          </ul>
                        </li>
                        <li>Adjust threshold settings and analysis options as needed</li>
                        </ol>
                        </div>
                        </body>
                        </html>"
                    )
                }
            },

            .run = function() {
                # Check if required inputs are available
                if (is.null(self$options$outcomes) ||
                    is.null(self$options$predictors)) {
                    return()
                }

                # Hide instructions
                self$results$instructions$setVisible(FALSE)

                # Get data and options
                data <- self$data
                outcomeVar <- self$options$outcomes
                predictorVars <- self$options$predictors
                thresholdMin <- self$options$thresholdMin
                thresholdMax <- self$options$thresholdMax
                thresholdPoints <- self$options$thresholdPoints

                # Validate threshold settings
                if (thresholdMin >= thresholdMax) {
                    jmvcore::reject("Minimum threshold must be less than maximum threshold")
                }
                if (thresholdMin < 0.001 || thresholdMax > 0.99) {
                    jmvcore::reject("Thresholds must be between 0.001 and 0.99")
                }
                
                # Generate threshold sequence
                thresholds <- seq(thresholdMin, thresholdMax, length.out = thresholdPoints)
                private$.thresholds <- thresholds

                # Prepare outcome variable
                if (!is.null(self$options$outcomePos)) {
                    outcomePosLevel <- self$options$outcomePos
                    outcomes <- as.integer(data[[outcomeVar]] == outcomePosLevel)
                } else {
                    outcomes <- as.integer(data[[outcomeVar]])
                    if (!all(outcomes %in% c(0, 1, NA))) {
                        jmvcore::reject("Outcome variable must be binary (0/1)")
                    }
                }

                # Validate and handle missing values
                if (sum(is.na(outcomes)) > 0) {
                    warning(paste("Removing", sum(is.na(outcomes)), "cases with missing outcome values"))
                }
                
                complete_cases <- complete.cases(outcomes)
                if (sum(complete_cases) < 10) {
                    jmvcore::reject("Insufficient data: Need at least 10 complete cases for analysis")
                }
                
                outcomes <- outcomes[complete_cases]
                data <- data[complete_cases, , drop = FALSE]

                # Count cases for prevalence
                if (self$options$useExternalPrevalence) {
                    cases <- self$options$externalCases
                    total <- self$options$externalTotal

                    if (cases > total) {
                        jmvcore::reject("Number of cases cannot exceed total sample size")
                    }
                } else {
                    cases <- sum(outcomes == 1)
                    total <- length(outcomes)
                }

                prevalence <- cases / total
                
                # Validate prevalence
                if (prevalence < 0.01 || prevalence > 0.99) {
                    warning("Extreme prevalence detected. Results may be unstable.")
                }
                
                # Validate predictor variables
                for (pred in predictorVars) {
                    if (!pred %in% names(data)) {
                        jmvcore::reject(paste("Predictor variable", pred, "not found in data"))
                    }
                    
                    pred_values <- data[[pred]]
                    if (all(is.na(pred_values))) {
                        jmvcore::reject(paste("Predictor variable", pred, "contains only missing values"))
                    }
                    
                    # Check if continuous predictor is in valid probability range
                    if (!all(pred_values %in% c(0, 1, NA), na.rm = TRUE)) {
                        if (any(pred_values < 0 | pred_values > 1, na.rm = TRUE)) {
                            warning(paste("Predictor", pred, "contains values outside [0,1] range. Consider probability calibration."))
                        }
                    }
                }

                # Create summary
                summary_html <- paste0(
                    "<p><b>Data Summary:</b></p>",
                    "<ul>",
                    "<li>Total sample size: ", total, "</li>",
                    "<li>Number of cases: ", cases, "</li>",
                    "<li>Prevalence: ", round(prevalence * 100, 1), "%</li>",
                    "</ul>",
                    "<p><b>Analysis Settings:</b></p>",
                    "<ul>",
                    "<li>Analysis type: ",
                    ifelse(self$options$bayesianAnalysis, "Bayesian", "Frequentist"),
                    "</li>",
                    "<li>Thresholds: ", round(thresholdMin, 3), " - ", round(thresholdMax, 3),
                    " (", thresholdPoints, " points)</li>",
                    "</ul>"
                )

                self$results$summary$setContent(summary_html)

                # Initialize results storage
                private$.dcaResults <- list(
                    thresholds = thresholds,
                    prevalence = prevalence,
                    strategies = list()
                )

                # Run analysis - FIXED: Use private$ instead of self$
                if (self$options$bayesianAnalysis) {
                    private$.runBayesianDCA(outcomes, data, predictorVars, thresholds, prevalence)
                } else {
                    private$.runFrequentistDCA(outcomes, data, predictorVars, thresholds, prevalence)
                }

                # Calculate comparisons - FIXED: Use private$ instead of self$
                private$.calculateComparisons()

                # Calculate EVPI if requested - FIXED: Use private$ instead of self$
                if (self$options$calculateEVPI && self$options$bayesianAnalysis) {
                    private$.calculateEVPI()
                }

                # Populate plot states - FIXED: Use private$ instead of self$
                private$.preparePlotData()
            },

            .runBayesianDCA = function(outcomes, data, predictorVars, thresholds, prevalence) {
                # Bayesian DCA implementation
                n_draws <- self$options$nDraws
                prior_strength <- self$options$priorStrength

                # Prior parameters (Beta distribution)
                prior_alpha <- prior_strength / 2
                prior_beta <- prior_strength / 2

                # Calculate treat all/none
                treatAllNB <- prevalence - (1 - prevalence) * thresholds / (1 - thresholds)
                treatNoneNB <- rep(0, length(thresholds))

                # Store posterior draws
                private$.posteriorDraws <- list(
                    "Treat all" = matrix(rep(treatAllNB, n_draws), nrow = n_draws, byrow = TRUE),
                    "Treat none" = matrix(0, nrow = n_draws, ncol = length(thresholds))
                )

                # Populate net benefit table
                nbTable <- self$results$netBenefitTable
                for (i in seq_along(thresholds)) {
                    nbTable$addRow(rowKey = i, values = list(
                        threshold = thresholds[i],
                        treatAll = treatAllNB[i],
                        treatNone = treatNoneNB[i]
                    ))
                }

                # Store results
                private$.dcaResults$strategies[["Treat all"]] <- list(
                    netBenefit = treatAllNB,
                    lowerCI = treatAllNB,
                    upperCI = treatAllNB
                )

                private$.dcaResults$strategies[["Treat none"]] <- list(
                    netBenefit = treatNoneNB,
                    lowerCI = treatNoneNB,
                    upperCI = treatNoneNB
                )

                # Process each predictor
                for (predictor in predictorVars) {
                    # Create table item
                    if (!predictor %in% self$results$modelResults$itemKeys) {
                        self$results$modelResults$addItem(key = predictor)
                    }

                    table <- self$results$modelResults$get(key = predictor)
                    pred_values <- data[[predictor]]

                    # Storage for this predictor
                    nb_values <- numeric(length(thresholds))
                    lowerCI <- numeric(length(thresholds))
                    upperCI <- numeric(length(thresholds))
                    sensitivity <- numeric(length(thresholds))
                    specificity <- numeric(length(thresholds))

                    # Matrix for posterior draws
                    nb_draws <- matrix(NA, nrow = n_draws, ncol = length(thresholds))

                    for (i in seq_along(thresholds)) {
                        thresh <- thresholds[i]

                        # Determine predictions
                        if (all(pred_values %in% c(0, 1, NA), na.rm = TRUE)) {
                            # Binary test
                            test_results <- pred_values
                        } else {
                            # Continuous predictor
                            if (self$options$directionIndicator == ">=") {
                                test_results <- ifelse(pred_values >= thresh, 1, 0)
                            } else {
                                test_results <- ifelse(pred_values <= thresh, 1, 0)
                            }
                        }

                        # Calculate 2x2 table
                        tp <- sum(outcomes == 1 & test_results == 1, na.rm = TRUE)
                        fp <- sum(outcomes == 0 & test_results == 1, na.rm = TRUE)
                        tn <- sum(outcomes == 0 & test_results == 0, na.rm = TRUE)
                        fn <- sum(outcomes == 1 & test_results == 0, na.rm = TRUE)

                        # Point estimates with better error handling
                        sens <- if ((tp + fn) > 0) tp / (tp + fn) else 0
                        spec <- if ((tn + fp) > 0) tn / (tn + fp) else 0
                        
                        # Calculate net benefit with safeguards
                        if (thresh >= 1.0) {
                            nb <- 0  # At threshold = 1, no intervention is worthwhile
                        } else {
                            nb <- sens * prevalence - (1 - spec) * (1 - prevalence) * thresh / (1 - thresh)
                        }
                        
                        # Ensure net benefit is finite
                        if (!is.finite(nb)) {
                            nb <- 0
                        }

                        sensitivity[i] <- sens
                        specificity[i] <- spec
                        nb_values[i] <- nb

                        # Bayesian posterior
                        post_sens_alpha <- tp + prior_alpha
                        post_sens_beta <- fn + prior_beta
                        post_spec_alpha <- tn + prior_alpha
                        post_spec_beta <- fp + prior_beta

                        # Draw from posterior
                        sens_draws <- stats::rbeta(n_draws, post_sens_alpha, post_sens_beta)
                        spec_draws <- stats::rbeta(n_draws, post_spec_alpha, post_spec_beta)

                        # Calculate net benefit for each draw
                        nb_draws[, i] <- sens_draws * prevalence -
                            (1 - spec_draws) * (1 - prevalence) * thresh / (1 - thresh)

                        # Get credible intervals
                        nb_ci <- stats::quantile(nb_draws[, i], c(0.025, 0.975))
                        lowerCI[i] <- nb_ci[1]
                        upperCI[i] <- nb_ci[2]

                        # Add row to table
                        table$addRow(rowKey = i, values = list(
                            threshold = thresh,
                            netBenefit = nb,
                            lowerCI = lowerCI[i],
                            upperCI = upperCI[i],
                            sensitivity = sens,
                            specificity = spec
                        ))
                    }

                    # Store results
                    private$.dcaResults$strategies[[predictor]] <- list(
                        netBenefit = nb_values,
                        lowerCI = lowerCI,
                        upperCI = upperCI,
                        sensitivity = sensitivity,
                        specificity = specificity
                    )

                    # Store posterior draws
                    private$.posteriorDraws[[predictor]] <- nb_draws
                }
            },

            .runFrequentistDCA = function(outcomes, data, predictorVars, thresholds, prevalence) {
                # Frequentist DCA implementation

                # Calculate treat all/none
                treatAllNB <- prevalence - (1 - prevalence) * thresholds / (1 - thresholds)
                treatNoneNB <- rep(0, length(thresholds))

                # Populate net benefit table
                nbTable <- self$results$netBenefitTable
                for (i in seq_along(thresholds)) {
                    nbTable$addRow(rowKey = i, values = list(
                        threshold = thresholds[i],
                        treatAll = treatAllNB[i],
                        treatNone = treatNoneNB[i]
                    ))
                }

                # Store results
                private$.dcaResults$strategies[["Treat all"]] <- list(
                    netBenefit = treatAllNB,
                    lowerCI = treatAllNB,
                    upperCI = treatAllNB
                )

                private$.dcaResults$strategies[["Treat none"]] <- list(
                    netBenefit = treatNoneNB,
                    lowerCI = treatNoneNB,
                    upperCI = treatNoneNB
                )

                # Process each predictor
                for (predictor in predictorVars) {
                    # Create table item
                    if (!predictor %in% self$results$modelResults$itemKeys) {
                        self$results$modelResults$addItem(key = predictor)
                    }

                    table <- self$results$modelResults$get(key = predictor)
                    pred_values <- data[[predictor]]

                    # Storage
                    nb_values <- numeric(length(thresholds))
                    lowerCI <- numeric(length(thresholds))
                    upperCI <- numeric(length(thresholds))
                    sensitivity <- numeric(length(thresholds))
                    specificity <- numeric(length(thresholds))

                    for (i in seq_along(thresholds)) {
                        thresh <- thresholds[i]

                        # Determine predictions
                        if (all(pred_values %in% c(0, 1, NA), na.rm = TRUE)) {
                            # Binary test
                            test_results <- pred_values
                        } else {
                            # Continuous predictor
                            if (self$options$directionIndicator == ">=") {
                                test_results <- ifelse(pred_values >= thresh, 1, 0)
                            } else {
                                test_results <- ifelse(pred_values <= thresh, 1, 0)
                            }
                        }

                        # Calculate 2x2 table
                        tp <- sum(outcomes == 1 & test_results == 1, na.rm = TRUE)
                        fp <- sum(outcomes == 0 & test_results == 1, na.rm = TRUE)
                        tn <- sum(outcomes == 0 & test_results == 0, na.rm = TRUE)
                        fn <- sum(outcomes == 1 & test_results == 0, na.rm = TRUE)

                        # Calculate metrics with better error handling
                        sens <- if ((tp + fn) > 0) tp / (tp + fn) else 0
                        spec <- if ((tn + fp) > 0) tn / (tn + fp) else 0
                        
                        # Calculate net benefit with safeguards
                        if (thresh >= 1.0) {
                            nb <- 0  # At threshold = 1, no intervention is worthwhile
                        } else {
                            nb <- sens * prevalence - (1 - spec) * (1 - prevalence) * thresh / (1 - thresh)
                        }
                        
                        # Ensure net benefit is finite
                        if (!is.finite(nb)) {
                            nb <- 0
                        }

                        sensitivity[i] <- sens
                        specificity[i] <- spec
                        nb_values[i] <- nb

                        # Bootstrap CI if requested
                        if (self$options$bootstrapCI) {
                            # Simple bootstrap
                            n_boot <- self$options$bootstrapReps
                            boot_nb <- numeric(n_boot)

                            for (b in 1:n_boot) {
                                # Resample indices
                                boot_idx <- sample(length(outcomes), replace = TRUE)
                                boot_outcomes <- outcomes[boot_idx]
                                boot_pred <- pred_values[boot_idx]

                                # Apply threshold
                                if (all(boot_pred %in% c(0, 1, NA), na.rm = TRUE)) {
                                    boot_test <- boot_pred
                                } else {
                                    if (self$options$directionIndicator == ">=") {
                                        boot_test <- ifelse(boot_pred >= thresh, 1, 0)
                                    } else {
                                        boot_test <- ifelse(boot_pred <= thresh, 1, 0)
                                    }
                                }

                                # Calculate bootstrap 2x2 table
                                boot_tp <- sum(boot_outcomes == 1 & boot_test == 1, na.rm = TRUE)
                                boot_fp <- sum(boot_outcomes == 0 & boot_test == 1, na.rm = TRUE)
                                boot_tn <- sum(boot_outcomes == 0 & boot_test == 0, na.rm = TRUE)
                                boot_fn <- sum(boot_outcomes == 1 & boot_test == 0, na.rm = TRUE)

                                # Calculate bootstrap metrics with safeguards
                                boot_sens <- if ((boot_tp + boot_fn) > 0) boot_tp / (boot_tp + boot_fn) else 0
                                boot_spec <- if ((boot_tn + boot_fp) > 0) boot_tn / (boot_tn + boot_fp) else 0
                                
                                # Calculate bootstrap net benefit with safeguards
                                if (thresh >= 1.0) {
                                    boot_nb[b] <- 0
                                } else {
                                    boot_nb[b] <- boot_sens * prevalence -
                                        (1 - boot_spec) * (1 - prevalence) * thresh / (1 - thresh)
                                }
                                
                                # Ensure bootstrap net benefit is finite
                                if (!is.finite(boot_nb[b])) {
                                    boot_nb[b] <- 0
                                }
                            }

                            # Get CI
                            boot_ci <- stats::quantile(boot_nb, c(0.025, 0.975))
                            lowerCI[i] <- boot_ci[1]
                            upperCI[i] <- boot_ci[2]
                        } else {
                            lowerCI[i] <- NA
                            upperCI[i] <- NA
                        }

                        # Add row to table
                        table$addRow(rowKey = i, values = list(
                            threshold = thresh,
                            netBenefit = nb,
                            lowerCI = lowerCI[i],
                            upperCI = upperCI[i],
                            sensitivity = sens,
                            specificity = spec
                        ))
                    }

                    # Store results
                    private$.dcaResults$strategies[[predictor]] <- list(
                        netBenefit = nb_values,
                        lowerCI = lowerCI,
                        upperCI = upperCI,
                        sensitivity = sensitivity,
                        specificity = specificity
                    )
                }
            },

            .calculateComparisons = function() {
                # Calculate best strategy comparisons
                thresholds <- private$.thresholds
                strategies <- names(private$.dcaResults$strategies)
                compTable <- self$results$comparisonTable

                for (i in seq_along(thresholds)) {
                    # Get all net benefits at this threshold
                    all_nb <- sapply(strategies, function(s) {
                        private$.dcaResults$strategies[[s]]$netBenefit[i]
                    })

                    # Find best strategy
                    best_idx <- which.max(all_nb)
                    best_strategy <- strategies[best_idx]
                    best_nb <- all_nb[best_idx]

                    # Find second best
                    if (length(all_nb) > 1) {
                        all_nb_except_best <- all_nb[-best_idx]
                        second_best_nb <- max(all_nb_except_best)
                        diff_from_next <- best_nb - second_best_nb
                    } else {
                        diff_from_next <- 0
                    }

                    # For Bayesian, calculate probability of being best
                    prob_best <- NA
                    if (self$options$bayesianAnalysis && !is.null(private$.posteriorDraws)) {
                        # Count how often each strategy is best
                        n_draws <- nrow(private$.posteriorDraws[[1]])
                        best_count <- numeric(length(strategies))

                        for (draw in 1:n_draws) {
                            draw_nb <- sapply(strategies, function(s) {
                                if (s %in% names(private$.posteriorDraws)) {
                                    private$.posteriorDraws[[s]][draw, i]
                                } else {
                                    private$.dcaResults$strategies[[s]]$netBenefit[i]
                                }
                            })
                            best_count[which.max(draw_nb)] <- best_count[which.max(draw_nb)] + 1
                        }

                        prob_best <- best_count[best_idx] / n_draws
                    }

                    compTable$addRow(rowKey = i, values = list(
                        threshold = thresholds[i],
                        bestStrategy = best_strategy,
                        diffFromNext = diff_from_next,
                        probBest = prob_best
                    ))
                }
            },

            .calculateEVPI = function() {
                # Calculate Expected Value of Perfect Information
                if (!self$options$bayesianAnalysis || is.null(private$.posteriorDraws)) {
                    return()
                }

                evpiTable <- self$results$evpiTable
                thresholds <- private$.thresholds
                strategies <- names(private$.posteriorDraws)

                if (length(strategies) == 0) {
                    return()
                }

                for (i in seq_along(thresholds)) {
                    # Get draws for all strategies at this threshold
                    all_draws <- list()

                    for (s in strategies) {
                        if (s %in% names(private$.posteriorDraws)) {
                            all_draws[[s]] <- private$.posteriorDraws[[s]][, i]
                        }
                    }

                    # Also include treat all/none if they exist but aren't in posterior draws
                    all_strategies <- names(private$.dcaResults$strategies)
                    for (s in all_strategies) {
                        if (!s %in% names(all_draws)) {
                            # For strategies without posterior draws (treat all/none),
                            # create constant draws
                            n_draws <- nrow(private$.posteriorDraws[[1]])
                            all_draws[[s]] <- rep(private$.dcaResults$strategies[[s]]$netBenefit[i], n_draws)
                        }
                    }

                    if (length(all_draws) == 0) {
                        evpiTable$addRow(rowKey = i, values = list(
                            threshold = thresholds[i],
                            evpi = 0
                        ))
                        next
                    }

                    # Calculate EVPI: E[max(NB)] - max(E[NB])
                    n_draws <- length(all_draws[[1]])
                    max_nb_draws <- numeric(n_draws)

                    for (draw in 1:n_draws) {
                        draw_values <- sapply(all_draws, function(d) d[draw])
                        max_nb_draws[draw] <- max(draw_values, na.rm = TRUE)
                    }

                    expected_max_nb <- mean(max_nb_draws, na.rm = TRUE)
                    max_expected_nb <- max(sapply(all_draws, mean, na.rm = TRUE), na.rm = TRUE)

                    evpi <- expected_max_nb - max_expected_nb

                    # Ensure EVPI is non-negative (should be by theory)
                    evpi <- max(0, evpi)

                    evpiTable$addRow(rowKey = i, values = list(
                        threshold = thresholds[i],
                        evpi = evpi
                    ))
                }
            },

            .preparePlotData = function() {
                # Prepare data for all plots
                if (is.null(private$.dcaResults)) return()

                thresholds <- private$.thresholds
                strategies <- names(private$.dcaResults$strategies)

                # Main DCA plot data
                mainPlotData <- list(
                    thresholds = thresholds,
                    strategies = strategies,
                    dcaResults = private$.dcaResults$strategies
                )

                # Set plot states
                self$results$mainPlot$setState(mainPlotData)
                self$results$deltaPlot$setState(mainPlotData)

                if (self$options$bayesianAnalysis) {
                    self$results$probPlot$setState(mainPlotData)
                }

                if (self$options$calculateEVPI && self$options$bayesianAnalysis) {
                    self$results$evpiPlot$setState(mainPlotData)
                }
            },

            .plotDCA = function(image, ggtheme, theme, ...) {
                # Main DCA plot
                plotData <- image$state
                if (is.null(plotData)) return(FALSE)

                thresholds <- plotData$thresholds
                strategies <- plotData$strategies
                dcaResults <- plotData$dcaResults

                # Check if required packages are available
                if (!requireNamespace("RColorBrewer", quietly = TRUE) ||
                    !requireNamespace("scales", quietly = TRUE)) {
                    return(FALSE)
                }

                # Prepare data for plotting
                plotDf <- data.frame()

                for (strategy in strategies) {
                    strat_data <- dcaResults[[strategy]]
                    df <- data.frame(
                        threshold = thresholds,
                        strategy = strategy,
                        netBenefit = strat_data$netBenefit,
                        lower = strat_data$lowerCI,
                        upper = strat_data$upperCI
                    )
                    plotDf <- rbind(plotDf, df)
                }

                # Create color palette
                n_strategies <- length(strategies)
                if (n_strategies <= 8) {
                    colors <- RColorBrewer::brewer.pal(max(3, n_strategies), "Dark2")
                } else {
                    colors <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(n_strategies)
                }

                # Override colors for default strategies
                color_values <- setNames(colors, strategies)
                if ("Treat all" %in% strategies) color_values["Treat all"] <- "black"
                if ("Treat none" %in% strategies) color_values["Treat none"] <- "gray40"

                # Create plot
                plot <- ggplot2::ggplot(plotDf, ggplot2::aes(
                    x = threshold,
                    y = netBenefit,
                    color = strategy,
                    group = strategy
                )) +
                    ggplot2::geom_line(size = 1) +
                    ggplot2::scale_color_manual(values = color_values) +
                    ggplot2::theme_bw(base_size = 14) +
                    ggplot2::labs(
                        x = "Decision Threshold",
                        y = "Net Benefit",
                        color = NULL
                    ) +
                    ggplot2::scale_x_continuous(labels = scales::percent) +
                    ggplot2::coord_cartesian(
                        ylim = c(min(plotDf$netBenefit) * 1.1, max(plotDf$netBenefit) * 1.1)
                    )

                # Add confidence bands if Bayesian
                if (self$options$bayesianAnalysis) {
                    plot <- plot + ggplot2::geom_ribbon(
                        ggplot2::aes(ymin = lower, ymax = upper, fill = strategy),
                        alpha = 0.2,
                        color = NA
                    ) +
                        ggplot2::scale_fill_manual(values = color_values) +
                        ggplot2::guides(fill = "none")
                }

                print(plot)
                return(TRUE)
            },

            .plotDeltaNB = function(image, ggtheme, theme, ...) {
                # Delta net benefit plot
                plotData <- image$state
                if (is.null(plotData)) return(FALSE)

                thresholds <- plotData$thresholds
                strategies <- plotData$strategies
                dcaResults <- plotData$dcaResults

                # Get non-default strategies
                strategies <- strategies[!strategies %in% c("Treat all", "Treat none")]
                if (length(strategies) == 0) return(FALSE)

                # Check if required packages are available
                if (!requireNamespace("scales", quietly = TRUE)) {
                    return(FALSE)
                }

                # Calculate deltas
                plotDf <- data.frame()

                for (strategy in strategies) {
                    strat_nb <- dcaResults[[strategy]]$netBenefit

                    # Compare to best of treat all/none
                    treat_all_nb <- dcaResults[["Treat all"]]$netBenefit
                    treat_none_nb <- dcaResults[["Treat none"]]$netBenefit
                    best_default <- pmax(treat_all_nb, treat_none_nb)

                    delta <- strat_nb - best_default

                    df <- data.frame(
                        threshold = thresholds,
                        strategy = strategy,
                        delta = delta
                    )
                    plotDf <- rbind(plotDf, df)
                }

                # Create plot
                plot <- ggplot2::ggplot(plotDf, ggplot2::aes(
                    x = threshold,
                    y = delta,
                    color = strategy,
                    group = strategy
                )) +
                    ggplot2::geom_line(size = 1) +
                    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
                    ggplot2::theme_bw(base_size = 14) +
                    ggplot2::labs(
                        x = "Decision Threshold",
                        y = expression(Delta[NB]),
                        color = NULL,
                        subtitle = "Difference from treat all/none"
                    ) +
                    ggplot2::scale_x_continuous(labels = scales::percent)

                print(plot)
                return(TRUE)
            },

            .plotProbability = function(image, ggtheme, theme, ...) {
                # Probability plot - only for Bayesian
                if (!self$options$bayesianAnalysis || is.null(private$.posteriorDraws)) {
                    return(FALSE)
                }

                plotData <- image$state
                if (is.null(plotData)) return(FALSE)

                # Check if required packages are available
                if (!requireNamespace("scales", quietly = TRUE)) {
                    return(FALSE)
                }

                # Calculate probability each strategy is best
                strategies <- names(private$.posteriorDraws)
                thresholds <- private$.thresholds

                prob_matrix <- matrix(0, nrow = length(thresholds), ncol = length(strategies))
                colnames(prob_matrix) <- strategies

                for (i in seq_along(thresholds)) {
                    n_draws <- nrow(private$.posteriorDraws[[1]])
                    best_count <- numeric(length(strategies))

                    for (draw in 1:n_draws) {
                        draw_nb <- sapply(strategies, function(s) {
                            private$.posteriorDraws[[s]][draw, i]
                        })
                        best_count[which.max(draw_nb)] <- best_count[which.max(draw_nb)] + 1
                    }

                    prob_matrix[i, ] <- best_count / n_draws
                }

                # Create plot data
                plotDf <- data.frame()
                for (strategy in strategies) {
                    df <- data.frame(
                        threshold = thresholds,
                        strategy = strategy,
                        probability = prob_matrix[, strategy]
                    )
                    plotDf <- rbind(plotDf, df)
                }

                # Create plot
                plot <- ggplot2::ggplot(plotDf, ggplot2::aes(
                    x = threshold,
                    y = probability,
                    color = strategy,
                    group = strategy
                )) +
                    ggplot2::geom_line(size = 1) +
                    ggplot2::theme_bw(base_size = 14) +
                    ggplot2::labs(
                        x = "Decision Threshold",
                        y = "Probability of Being Optimal",
                        color = NULL
                    ) +
                    ggplot2::scale_x_continuous(labels = scales::percent) +
                    ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, 1))

                print(plot)
                return(TRUE)
            },

            .plotEVPI = function(image, ggtheme, theme, ...) {
                # EVPI plot
                if (!self$options$calculateEVPI || !self$options$bayesianAnalysis) {
                    return(FALSE)
                }

                # Check if required packages are available
                if (!requireNamespace("scales", quietly = TRUE)) {
                    return(FALSE)
                }

                # Alternative approach: calculate EVPI directly from stored posterior draws
                if (is.null(private$.posteriorDraws) || is.null(private$.thresholds)) {
                    return(FALSE)
                }

                thresholds <- private$.thresholds
                strategies <- names(private$.posteriorDraws)

                if (length(strategies) == 0) {
                    return(FALSE)
                }

                # Calculate EVPI directly
                evpi_values <- numeric(length(thresholds))

                for (i in seq_along(thresholds)) {
                    # Get draws for all strategies at this threshold
                    all_draws <- lapply(strategies, function(s) {
                        if (s %in% names(private$.posteriorDraws)) {
                            private$.posteriorDraws[[s]][, i]
                        } else {
                            # For treat all/none strategies, use the fixed values
                            rep(private$.dcaResults$strategies[[s]]$netBenefit[i],
                                nrow(private$.posteriorDraws[[1]]))
                        }
                    })

                    # Skip if no valid draws
                    if (length(all_draws) == 0 || any(sapply(all_draws, function(x) length(x) == 0))) {
                        evpi_values[i] <- 0
                        next
                    }

                    # Calculate EVPI: E[max(NB)] - max(E[NB])
                    n_draws <- length(all_draws[[1]])
                    max_nb_draws <- numeric(n_draws)

                    for (draw in 1:n_draws) {
                        draw_values <- sapply(all_draws, function(d) d[draw])
                        max_nb_draws[draw] <- max(draw_values, na.rm = TRUE)
                    }

                    expected_max_nb <- mean(max_nb_draws, na.rm = TRUE)
                    max_expected_nb <- max(sapply(all_draws, mean, na.rm = TRUE), na.rm = TRUE)

                    evpi_values[i] <- expected_max_nb - max_expected_nb
                }

                # Create plot data
                plotDf <- data.frame(
                    threshold = thresholds,
                    evpi = evpi_values
                )

                # Remove any rows with NA or infinite values
                plotDf <- plotDf[is.finite(plotDf$evpi), ]

                if (nrow(plotDf) == 0) {
                    return(FALSE)
                }

                # Create plot
                plot <- ggplot2::ggplot(plotDf, ggplot2::aes(x = threshold, y = evpi)) +
                    ggplot2::geom_line(size = 1, color = "blue") +
                    ggplot2::geom_point(size = 1, color = "blue", alpha = 0.6) +
                    ggplot2::theme_bw(base_size = 14) +
                    ggplot2::labs(
                        x = "Decision Threshold",
                        y = "Expected Value of Perfect Information",
                        subtitle = "EVPI: Value of Reducing Uncertainty"
                    ) +
                    ggplot2::scale_x_continuous(labels = scales::percent) +
                    ggplot2::scale_y_continuous(labels = function(x) format(x, digits = 4))

                # Add horizontal line at zero for reference
                plot <- plot + ggplot2::geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5)

                print(plot)
                return(TRUE)
            }
        )
    )
