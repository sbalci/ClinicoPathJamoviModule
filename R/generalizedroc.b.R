
#' @title Generalized ROC Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom stats qnorm quantile var sd shapiro.test qqnorm qqline
#' @importFrom graphics plot lines legend abline hist par
#' @export


generalizedrocClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "generalizedrocClass",
    inherit = generalizedrocBase,
    private = list(

        # Data storage
        .data_prepared = NULL,
        .roc_result = NULL,
        .group_params = NULL,

        #---------------------------------------------
        # INIT
        #---------------------------------------------
        .init = function() {

            # Instructions
            html <- "<h3>Generalized ROC Analysis</h3>
            <p>Generalized ROC extends traditional binary ROC to handle continuous outcomes with potentially unequal variances.</p>
            <h4>When to Use:</h4>
            <ul>
            <li><b>Continuous biomarkers:</b> PD-L1 CPS, Ki67 percentage, quantitative imaging</li>
            <li><b>Unequal variances:</b> When outcome groups have different spreads</li>
            <li><b>Non-normal distributions:</b> With appropriate transformations or non-parametric methods</li>
            </ul>
            <h4>Key Differences from Standard ROC:</h4>
            <ul>
            <li><b>Standard ROC:</b> Assumes equal variances (or uses rank-based methods)</li>
            <li><b>Generalized ROC:</b> Models full distribution with unequal variances</li>
            <li><b>Interpretation:</b> Same AUC scale (0.5-1.0), but accounts for heteroscedasticity</li>
            </ul>"

            self$results$instructionsText$setContent(html)

            # Interpretation guide
            interp_html <- "<h3>Interpretation Guide</h3>
            <h4>AUC Interpretation:</h4>
            <ul>
            <li><b>0.90-1.00:</b> Excellent discrimination</li>
            <li><b>0.80-0.90:</b> Good discrimination</li>
            <li><b>0.70-0.80:</b> Fair discrimination</li>
            <li><b>0.60-0.70:</b> Poor discrimination</li>
            <li><b>0.50-0.60:</b> Fail (barely better than chance)</li>
            </ul>
            <h4>Variance Equality Tests:</h4>
            <ul>
            <li><b>p < 0.05:</b> Reject equal variance assumption (use generalized ROC)</li>
            <li><b>p ≥ 0.05:</b> Equal variance plausible (standard ROC acceptable)</li>
            </ul>
            <h4>Transformation Guide:</h4>
            <ul>
            <li><b>Log:</b> Right-skewed data (e.g., biomarker concentrations)</li>
            <li><b>Square root:</b> Count data or mild skew</li>
            <li><b>Box-Cox:</b> Automatic optimal transformation</li>
            </ul>"

            self$results$interpretationText$setContent(interp_html)
        },

        #---------------------------------------------
        # RUN
        #---------------------------------------------
        .run = function() {

            # Check requirements
            if (is.null(self$options$outcome) || self$options$outcome == "") {
                return()
            }

            if (is.null(self$options$predictor) || self$options$predictor == "") {
                return()
            }

            # Prepare data
            tryCatch({
                private$.prepareData()
            }, error = function(e) {
                stop(paste("Data preparation error:", e$message))
            })

            if (is.null(private$.data_prepared)) {
                return()
            }

            # Calculate group parameters
            private$.calculateGroupParameters()

            # Show diagnostics
            if (self$options$show_diagnostics) {
                private$.populateDiagnostics()
            }

            # Calculate generalized ROC
            tryCatch({
                private$.calculateGeneralizedROC()
            }, error = function(e) {
                stop(paste("ROC calculation error:", e$message))
            })

            # Populate results
            if (self$options$calculate_auc) {
                private$.populateAUC()
            }

            private$.populateParameters()

            if (self$options$optimal_threshold) {
                private$.populateThreshold()
            }
        },

        #---------------------------------------------
        # PREPARE DATA
        #---------------------------------------------
        .prepareData = function() {

            data <- self$data
            outcome_var <- self$options$outcome
            predictor_var <- self$options$predictor

            # Extract data
            y <- as.factor(data[[outcome_var]])
            x <- as.numeric(data[[predictor_var]])

            # Check binary outcome
            if (length(levels(y)) != 2) {
                stop("Outcome variable must have exactly 2 levels for generalized ROC")
            }

            # Check for missing values
            if (any(is.na(y)) || any(is.na(x))) {
                complete_cases <- !is.na(y) & !is.na(x)
                y <- y[complete_cases]
                x <- x[complete_cases]
                warning(paste("Removed", sum(!complete_cases), "cases with missing values"))
            }

            # Apply transformation
            transformation <- self$options$transformation
            if (transformation != "none") {
                x_original <- x

                if (transformation == "log") {
                    if (any(x <= 0)) {
                        stop("Log transformation requires all values > 0. Consider adding a constant.")
                    }
                    x <- log(x)
                } else if (transformation == "sqrt") {
                    if (any(x < 0)) {
                        stop("Square root transformation requires all values >= 0")
                    }
                    x <- sqrt(x)
                } else if (transformation == "boxcox") {
                    # Simple Box-Cox (lambda = 0 is log, lambda = 0.5 is sqrt)
                    # Find optimal lambda (simplified)
                    lambda <- private$.findBoxCoxLambda(x)
                    if (abs(lambda) < 0.01) {
                        x <- log(x + 1)
                    } else {
                        x <- ((x + 1)^lambda - 1) / lambda
                    }
                }
            }

            private$.data_prepared <- list(
                y = y,
                x = x,
                levels = levels(y),
                n = length(y)
            )
        },

        #---------------------------------------------
        # FIND BOX-COX LAMBDA
        #---------------------------------------------
        .findBoxCoxLambda = function(x) {
            # Simplified Box-Cox lambda estimation
            # Test common values and choose best based on normality
            lambdas <- c(-1, -0.5, 0, 0.5, 1, 2)
            best_p <- 0
            best_lambda <- 0

            for (lambda in lambdas) {
                if (abs(lambda) < 0.01) {
                    x_trans <- log(x + 1)
                } else {
                    x_trans <- ((x + 1)^lambda - 1) / lambda
                }

                # Test normality
                if (length(x_trans) >= 3 && length(x_trans) <= 5000) {
                    suppressWarnings({
                        test <- shapiro.test(x_trans)
                        if (test$p.value > best_p) {
                            best_p <- test$p.value
                            best_lambda <- lambda
                        }
                    })
                }
            }

            return(best_lambda)
        },

        #---------------------------------------------
        # CALCULATE GROUP PARAMETERS
        #---------------------------------------------
        .calculateGroupParameters = function() {

            y <- private$.data_prepared$y
            x <- private$.data_prepared$x
            levels <- private$.data_prepared$levels

            params <- list()

            for (lev in levels) {
                x_group <- x[y == lev]

                # Calculate moments
                params[[lev]] <- list(
                    n = length(x_group),
                    mean = mean(x_group),
                    sd = sd(x_group),
                    variance = var(x_group),
                    skewness = private$.calculateSkewness(x_group),
                    kurtosis = private$.calculateKurtosis(x_group)
                )
            }

            private$.group_params <- params
        },

        #---------------------------------------------
        # CALCULATE SKEWNESS
        #---------------------------------------------
        .calculateSkewness = function(x) {
            n <- length(x)
            m <- mean(x)
            s <- sd(x)
            skew <- (1 / n) * sum(((x - m) / s)^3)
            return(skew)
        },

        #---------------------------------------------
        # CALCULATE KURTOSIS
        #---------------------------------------------
        .calculateKurtosis = function(x) {
            n <- length(x)
            m <- mean(x)
            s <- sd(x)
            kurt <- (1 / n) * sum(((x - m) / s)^4) - 3  # Excess kurtosis
            return(kurt)
        },

        #---------------------------------------------
        # POPULATE DIAGNOSTICS
        #---------------------------------------------
        .populateDiagnostics = function() {

            y <- private$.data_prepared$y
            x <- private$.data_prepared$x
            levels <- private$.data_prepared$levels

            table <- self$results$diagnosticsTable

            # Normality tests for each group
            for (lev in levels) {
                x_group <- x[y == lev]

                if (length(x_group) >= 3 && length(x_group) <= 5000) {
                    suppressWarnings({
                        shapiro <- shapiro.test(x_group)
                    })

                    result_text <- if (shapiro$p.value < 0.05) {
                        "Non-normal (p < 0.05)"
                    } else {
                        "Normal (p ≥ 0.05)"
                    }

                    table$addRow(rowKey = paste0("shapiro_", lev), values = list(
                        test = paste("Shapiro-Wilk (", lev, ")", sep = ""),
                        statistic = shapiro$statistic,
                        p_value = shapiro$p.value,
                        result = result_text
                    ))
                }
            }

            # Variance equality test
            variance_test <- self$options$variance_test

            if (variance_test == "levene") {
                # Levene's test (simplified - median-based)
                result <- private$.leveneTest(x, y)
                test_name <- "Levene's Test"
            } else if (variance_test == "fligner") {
                # Fligner-Killeen test
                result <- fligner.test(x ~ y)
                test_name <- "Fligner-Killeen Test"
            } else {  # bartlett
                # Bartlett's test
                result <- bartlett.test(x ~ y)
                test_name <- "Bartlett's Test"
            }

            result_text <- if (result$p.value < 0.05) {
                "Unequal variances (p < 0.05)"
            } else {
                "Equal variances (p ≥ 0.05)"
            }

            table$addRow(rowKey = "variance", values = list(
                test = test_name,
                statistic = result$statistic,
                p_value = result$p.value,
                result = result_text
            ))
        },

        #---------------------------------------------
        # LEVENE'S TEST (SIMPLIFIED)
        #---------------------------------------------
        .leveneTest = function(x, groups) {
            # Median-based Levene's test
            group_levels <- levels(groups)

            # Calculate absolute deviations from group medians
            z <- numeric(length(x))
            for (lev in group_levels) {
                idx <- groups == lev
                z[idx] <- abs(x[idx] - median(x[idx]))
            }

            # ANOVA on absolute deviations
            result <- summary(aov(z ~ groups))
            f_stat <- result[[1]]$`F value`[1]
            p_val <- result[[1]]$`Pr(>F)`[1]

            return(list(statistic = f_stat, p.value = p_val))
        },

        #---------------------------------------------
        # CALCULATE GENERALIZED ROC
        #---------------------------------------------
        .calculateGeneralizedROC = function() {

            y <- private$.data_prepared$y
            x <- private$.data_prepared$x
            levels <- private$.data_prepared$levels
            assume_equal_var <- self$options$assume_equal_variance

            set.seed(self$options$random_seed)

            # Get parameters for each group
            params0 <- private$.group_params[[levels[1]]]
            params1 <- private$.group_params[[levels[2]]]

            # Calculate AUC based on distribution model
            distribution <- self$options$distribution_model

            if (distribution == "normal") {
                # Parametric approach assuming normal distributions
                mu0 <- params0$mean
                mu1 <- params1$mean
                sigma0 <- params0$sd
                sigma1 <- params1$sd

                if (assume_equal_var) {
                    # Standard ROC with equal variance
                    pooled_sd <- sqrt((params0$variance * (params0$n - 1) +
                                       params1$variance * (params1$n - 1)) /
                                      (params0$n + params1$n - 2))
                    delta <- abs(mu1 - mu0) / pooled_sd
                    auc <- pnorm(delta / sqrt(2))
                } else {
                    # Generalized ROC with unequal variance
                    # Use bi-normal model
                    a <- sigma0 / sigma1
                    b <- (mu1 - mu0) / sigma1

                    # AUC for bi-normal ROC
                    auc <- pnorm(b / sqrt(1 + a^2))
                }

                # Generate ROC curve
                fpr_seq <- seq(0, 1, length.out = 100)
                tpr_seq <- numeric(100)

                for (i in seq_along(fpr_seq)) {
                    # Convert FPR to threshold
                    thresh <- qnorm(1 - fpr_seq[i], mean = mu0, sd = sigma0)
                    # Calculate TPR at this threshold
                    tpr_seq[i] <- 1 - pnorm(thresh, mean = mu1, sd = sigma1)
                }

            } else {
                # Non-parametric approach (empirical)
                auc <- private$.mannWhitneyU(x[y == levels[1]], x[y == levels[2]])

                # Generate empirical ROC curve
                thresholds <- sort(unique(x), decreasing = TRUE)
                tpr_seq <- numeric(length(thresholds) + 2)
                fpr_seq <- numeric(length(thresholds) + 2)

                tpr_seq[1] <- 0
                fpr_seq[1] <- 0

                n_pos <- sum(y == levels[2])
                n_neg <- sum(y == levels[1])

                for (i in seq_along(thresholds)) {
                    y_pred <- x >= thresholds[i]
                    tp <- sum(y_pred & y == levels[2])
                    fp <- sum(y_pred & y == levels[1])

                    tpr_seq[i + 1] <- tp / n_pos
                    fpr_seq[i + 1] <- fp / n_neg
                }

                tpr_seq[length(tpr_seq)] <- 1
                fpr_seq[length(fpr_seq)] <- 1
            }

            # Calculate confidence intervals if requested
            if (self$options$confidence_intervals) {
                ci <- private$.calculateAUC_CI(y, x, auc)
                ci_lower <- ci$lower
                ci_upper <- ci$upper
            } else {
                ci_lower <- NA
                ci_upper <- NA
            }

            # Calculate standard error
            n0 <- params0$n
            n1 <- params1$n
            se <- sqrt((auc * (1 - auc) + (n1 - 1) * (auc / (2 - auc) - auc^2) +
                        (n0 - 1) * (2 * auc^2 / (1 + auc) - auc^2)) / (n0 * n1))

            private$.roc_result <- list(
                auc = auc,
                se = se,
                ci_lower = ci_lower,
                ci_upper = ci_upper,
                tpr = tpr_seq,
                fpr = fpr_seq
            )
        },

        #---------------------------------------------
        # MANN-WHITNEY U (FOR AUC)
        #---------------------------------------------
        .mannWhitneyU = function(x0, x1) {
            # Non-parametric AUC estimation
            n0 <- length(x0)
            n1 <- length(x1)

            # Count concordant pairs
            u <- 0
            for (i in 1:n0) {
                u <- u + sum(x1 > x0[i])
            }

            auc <- u / (n0 * n1)
            return(auc)
        },

        #---------------------------------------------
        # AUC CONFIDENCE INTERVALS
        #---------------------------------------------
        .calculateAUC_CI = function(y, x, auc_observed) {

            method <- self$options$ci_method
            conf_level <- self$options$confidence_level
            levels <- private$.data_prepared$levels

            if (method == "asymptotic") {
                # Normal approximation
                se <- private$.roc_result$se
                z <- qnorm(1 - (1 - conf_level) / 2)

                lower <- max(0, auc_observed - z * se)
                upper <- min(1, auc_observed + z * se)

            } else {  # bootstrap
                n_boot <- self$options$bootstrap_samples
                boot_aucs <- numeric(n_boot)
                n <- length(y)

                for (b in 1:n_boot) {
                    idx <- sample(1:n, n, replace = TRUE)
                    y_boot <- y[idx]
                    x_boot <- x[idx]

                    if (length(unique(y_boot)) < 2) {
                        boot_aucs[b] <- 0.5
                    } else {
                        x0 <- x_boot[y_boot == levels[1]]
                        x1 <- x_boot[y_boot == levels[2]]
                        boot_aucs[b] <- private$.mannWhitneyU(x0, x1)
                    }
                }

                lower <- quantile(boot_aucs, (1 - conf_level) / 2)
                upper <- quantile(boot_aucs, 1 - (1 - conf_level) / 2)
            }

            return(list(lower = lower, upper = upper))
        },

        #---------------------------------------------
        # POPULATE AUC TABLE
        #---------------------------------------------
        .populateAUC = function() {

            if (is.null(private$.roc_result)) return()

            auc <- private$.roc_result$auc
            se <- private$.roc_result$se
            ci_lower <- private$.roc_result$ci_lower
            ci_upper <- private$.roc_result$ci_upper

            # Interpret AUC
            if (auc >= 0.90) {
                interp <- "Excellent"
            } else if (auc >= 0.80) {
                interp <- "Good"
            } else if (auc >= 0.70) {
                interp <- "Fair"
            } else if (auc >= 0.60) {
                interp <- "Poor"
            } else {
                interp <- "Fail"
            }

            row <- list(
                auc = auc,
                ci_lower = ci_lower,
                ci_upper = ci_upper,
                se = se,
                interpretation = interp
            )

            self$results$aucTable$setRow(rowNo = 1, values = row)
        },

        #---------------------------------------------
        # POPULATE PARAMETERS TABLE
        #---------------------------------------------
        .populateParameters = function() {

            if (is.null(private$.group_params)) return()

            table <- self$results$parametersTable

            for (group in names(private$.group_params)) {
                params <- private$.group_params[[group]]

                row <- list(
                    group = group,
                    n = params$n,
                    mean = params$mean,
                    sd = params$sd,
                    variance = params$variance,
                    skewness = params$skewness,
                    kurtosis = params$kurtosis
                )

                table$addRow(rowKey = group, values = row)
            }
        },

        #---------------------------------------------
        # POPULATE THRESHOLD TABLE
        #---------------------------------------------
        .populateThreshold = function() {

            y <- private$.data_prepared$y
            x <- private$.data_prepared$x
            levels <- private$.data_prepared$levels

            # Find optimal threshold using Youden's index
            thresholds <- sort(unique(x))
            best_youden <- -Inf
            best_threshold <- thresholds[1]
            best_sens <- 0
            best_spec <- 0

            for (thresh in thresholds) {
                y_pred <- x >= thresh

                tp <- sum(y_pred & y == levels[2])
                fp <- sum(y_pred & y == levels[1])
                tn <- sum(!y_pred & y == levels[1])
                fn <- sum(!y_pred & y == levels[2])

                sens <- if (tp + fn > 0) tp / (tp + fn) else 0
                spec <- if (tn + fp > 0) tn / (tn + fp) else 0
                youden <- sens + spec - 1

                if (youden > best_youden) {
                    best_youden <- youden
                    best_threshold <- thresh
                    best_sens <- sens
                    best_spec <- spec
                }
            }

            # Calculate PPV and NPV at optimal threshold
            y_pred <- x >= best_threshold
            tp <- sum(y_pred & y == levels[2])
            fp <- sum(y_pred & y == levels[1])
            tn <- sum(!y_pred & y == levels[1])
            fn <- sum(!y_pred & y == levels[2])

            ppv <- if (tp + fp > 0) tp / (tp + fp) else 0
            npv <- if (tn + fn > 0) tn / (tn + fn) else 0

            row <- list(
                threshold = best_threshold,
                sensitivity = best_sens,
                specificity = best_spec,
                youden = best_youden,
                ppv = ppv,
                npv = npv
            )

            self$results$thresholdTable$setRow(rowNo = 1, values = row)
        },

        #---------------------------------------------
        # PLOT ROC CURVE
        #---------------------------------------------
        .rocPlot = function(image, ...) {

            if (is.null(private$.roc_result)) return()

            fpr <- private$.roc_result$fpr
            tpr <- private$.roc_result$tpr
            auc <- private$.roc_result$auc

            plot(fpr, tpr, type = "l", lwd = 2, col = "blue",
                 xlim = c(0, 1), ylim = c(0, 1),
                 xlab = "False Positive Rate (1 - Specificity)",
                 ylab = "True Positive Rate (Sensitivity)",
                 main = sprintf("Generalized ROC Curve (AUC = %.3f)", auc))

            abline(0, 1, lty = 2, col = "gray")
            grid()

            TRUE
        },

        #---------------------------------------------
        # PLOT DISTRIBUTIONS
        #---------------------------------------------
        .distributionPlot = function(image, ...) {

            if (is.null(private$.data_prepared)) return()

            y <- private$.data_prepared$y
            x <- private$.data_prepared$x
            levels <- private$.data_prepared$levels

            x0 <- x[y == levels[1]]
            x1 <- x[y == levels[2]]

            # Determine x-axis range
            x_range <- range(x)

            # Plot densities
            plot(density(x0), xlim = x_range, col = "red", lwd = 2,
                 main = "Group Distributions",
                 xlab = "Predictor Value", ylab = "Density")
            lines(density(x1), col = "blue", lwd = 2)

            legend("topright", legend = levels, col = c("red", "blue"), lwd = 2)

            TRUE
        },

        #---------------------------------------------
        # PLOT Q-Q PLOTS
        #---------------------------------------------
        .qqPlot = function(image, ...) {

            if (is.null(private$.data_prepared)) return()

            y <- private$.data_prepared$y
            x <- private$.data_prepared$x
            levels <- private$.data_prepared$levels

            par(mfrow = c(1, 2))

            for (i in 1:2) {
                lev <- levels[i]
                x_group <- x[y == lev]

                qqnorm(x_group, main = paste("Q-Q Plot:", lev))
                qqline(x_group, col = "red", lwd = 2)
            }

            par(mfrow = c(1, 1))

            TRUE
        }
    )
)
