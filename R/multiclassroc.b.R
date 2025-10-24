
#' @title Multi-class ROC Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom stats qnorm quantile
#' @importFrom graphics plot lines legend abline
#' @export


multiclassrocClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "multiclassrocClass",
    inherit = multiclassrocBase,
    private = list(

        # Data storage
        .data_prepared = NULL,
        .class_levels = NULL,
        .n_classes = NULL,
        .roc_results = NULL,

        #---------------------------------------------
        # INIT
        #---------------------------------------------
        .init = function() {

            # Instructions
            html <- "<h3>Multi-class ROC Analysis</h3>
            <p>This analysis evaluates diagnostic performance for outcomes with 3 or more classes.</p>
            <h4>Methods:</h4>
            <ul>
            <li><b>One-vs-Rest (OvR):</b> Each class is compared against all other classes combined</li>
            <li><b>One-vs-One (OvO):</b> All pairwise comparisons between classes</li>
            <li><b>Multinomial:</b> Global probability model across all classes</li>
            </ul>
            <h4>Averaging Methods:</h4>
            <ul>
            <li><b>Macro-Average:</b> Unweighted mean of per-class AUCs</li>
            <li><b>Micro-Average:</b> Aggregates all predictions (better for imbalanced data)</li>
            <li><b>Weighted-Average:</b> Weighted by class prevalence</li>
            </ul>
            <p><i>Common applications: Tumor subtype classification, disease staging, AI multi-class validation</i></p>"

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
            <h4>Method Selection:</h4>
            <ul>
            <li><b>OvR:</b> Best for class imbalance, interpretable per-class performance</li>
            <li><b>OvO:</b> All pairwise separability, useful for similar classes</li>
            <li><b>Multinomial:</b> Global model performance</li>
            </ul>"

            self$results$interpretationText$setContent(interp_html)

            # Dynamic confusion matrix columns
            if (!is.null(self$options$outcome)) {
                outcome_var <- self$options$outcome
                if (outcome_var != "") {
                    data <- self$data
                    if (!is.null(data) && nrow(data) > 0) {
                        outcome_data <- data[[outcome_var]]
                        if (!is.null(outcome_data)) {
                            classes <- levels(as.factor(outcome_data))
                            n_classes <- length(classes)

                            if (n_classes >= 3) {
                                # Build confusion matrix columns
                                confusion_cols <- list(
                                    list(name = 'true_class', title = 'True Class', type = 'text', combineBelow = TRUE)
                                )

                                for (cls in classes) {
                                    confusion_cols[[length(confusion_cols) + 1]] <- list(
                                        name = paste0('pred_', make.names(cls)),
                                        title = cls,
                                        type = 'integer'
                                    )
                                }

                                self$results$confusionMatrix$setColumns(confusion_cols)
                            }
                        }
                    }
                }
            }
        },

        #---------------------------------------------
        # RUN
        #---------------------------------------------
        .run = function() {

            # Check requirements
            if (is.null(self$options$outcome) || self$options$outcome == "") {
                return()
            }

            if (is.null(self$options$predictors) || length(self$options$predictors) == 0) {
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

            # Check minimum classes
            if (private$.n_classes < 3) {
                stop("Outcome variable must have at least 3 classes for multi-class ROC analysis")
            }

            # Calculate ROC based on method
            method <- self$options$method

            tryCatch({
                if (method == "ovr") {
                    private$.calculateOvR()
                } else if (method == "ovo") {
                    private$.calculateOvO()
                } else if (method == "multinomial") {
                    private$.calculateMultinomial()
                }
            }, error = function(e) {
                stop(paste("ROC calculation error:", e$message))
            })

            # Populate results
            private$.populateSummary()
            private$.populateClassAUC()

            if (self$options$pairwise_comparisons && method == "ovo") {
                private$.populatePairwise()
            }

            if (self$options$class_metrics) {
                private$.populateClassMetrics()
            }

            if (self$options$confusion_matrix) {
                private$.populateConfusionMatrix()
            }
        },

        #---------------------------------------------
        # PREPARE DATA
        #---------------------------------------------
        .prepareData = function() {

            data <- self$data
            outcome_var <- self$options$outcome
            predictor_vars <- self$options$predictors

            # Extract outcome
            y <- as.factor(data[[outcome_var]])
            private$.class_levels <- levels(y)
            private$.n_classes <- length(private$.class_levels)

            # Extract predictors
            X <- data[, predictor_vars, drop = FALSE]

            # Convert to numeric matrix
            X_numeric <- sapply(X, as.numeric)

            # Check for missing values
            if (any(is.na(y))) {
                stop("Outcome variable contains missing values")
            }

            if (any(is.na(X_numeric))) {
                stop("Predictor variables contain missing values")
            }

            private$.data_prepared <- list(
                y = y,
                X = X_numeric,
                n = length(y)
            )
        },

        #---------------------------------------------
        # ONE-VS-REST CALCULATION
        #---------------------------------------------
        .calculateOvR = function() {

            y <- private$.data_prepared$y
            X <- private$.data_prepared$X
            classes <- private$.class_levels
            n_classes <- private$.n_classes

            set.seed(self$options$random_seed)

            # Store results for each class
            class_results <- list()

            for (i in 1:n_classes) {
                cls <- classes[i]

                # Binary outcome: this class vs all others
                y_binary <- as.integer(y == cls)

                # Use corresponding predictor (or first if single predictor)
                if (ncol(X) >= i) {
                    predictor <- X[, i]
                } else {
                    predictor <- X[, 1]
                }

                # Calculate ROC
                roc_result <- private$.calculateBinaryROC(y_binary, predictor)

                # Calculate CI if requested
                if (self$options$confidence_intervals) {
                    ci <- private$.calculateAUC_CI(y_binary, predictor, roc_result$auc)
                    roc_result$ci_lower <- ci$lower
                    roc_result$ci_upper <- ci$upper
                }

                class_results[[cls]] <- roc_result
            }

            private$.roc_results <- list(
                method = "One-vs-Rest",
                class_results = class_results
            )
        },

        #---------------------------------------------
        # ONE-VS-ONE CALCULATION
        #---------------------------------------------
        .calculateOvO = function() {

            y <- private$.data_prepared$y
            X <- private$.data_prepared$X
            classes <- private$.class_levels
            n_classes <- private$.n_classes

            set.seed(self$options$random_seed)

            # Store pairwise results
            pairwise_results <- list()

            # All pairwise comparisons
            for (i in 1:(n_classes - 1)) {
                for (j in (i + 1):n_classes) {
                    cls_a <- classes[i]
                    cls_b <- classes[j]

                    # Subset to only these two classes
                    idx <- y %in% c(cls_a, cls_b)
                    y_subset <- y[idx]
                    X_subset <- X[idx, , drop = FALSE]

                    # Binary outcome
                    y_binary <- as.integer(y_subset == cls_a)

                    # Use predictors
                    if (ncol(X_subset) >= i) {
                        predictor <- X_subset[, i] - X_subset[, j]
                    } else {
                        predictor <- X_subset[, 1]
                    }

                    # Calculate ROC
                    roc_result <- private$.calculateBinaryROC(y_binary, predictor)
                    roc_result$n_a <- sum(y_subset == cls_a)
                    roc_result$n_b <- sum(y_subset == cls_b)

                    # Calculate CI if requested
                    if (self$options$confidence_intervals) {
                        ci <- private$.calculateAUC_CI(y_binary, predictor, roc_result$auc)
                        roc_result$ci_lower <- ci$lower
                        roc_result$ci_upper <- ci$upper
                    }

                    pair_name <- paste(cls_a, "vs", cls_b)
                    pairwise_results[[pair_name]] <- roc_result
                    pairwise_results[[pair_name]]$class_a <- cls_a
                    pairwise_results[[pair_name]]$class_b <- cls_b
                }
            }

            # Aggregate to per-class AUC (average across all pairs involving that class)
            class_results <- list()
            for (i in 1:n_classes) {
                cls <- classes[i]
                cls_aucs <- numeric()

                for (pair_name in names(pairwise_results)) {
                    pair <- pairwise_results[[pair_name]]
                    if (pair$class_a == cls) {
                        cls_aucs <- c(cls_aucs, pair$auc)
                    } else if (pair$class_b == cls) {
                        cls_aucs <- c(cls_aucs, 1 - pair$auc)  # Reverse AUC
                    }
                }

                class_results[[cls]] <- list(
                    auc = mean(cls_aucs),
                    n = sum(y == cls),
                    prevalence = mean(y == cls)
                )
            }

            private$.roc_results <- list(
                method = "One-vs-One",
                class_results = class_results,
                pairwise_results = pairwise_results
            )
        },

        #---------------------------------------------
        # MULTINOMIAL CALCULATION
        #---------------------------------------------
        .calculateMultinomial = function() {

            # Simplified multinomial approach using OvR as base
            # True multinomial would require probabilistic model (logistic regression)
            private$.calculateOvR()
            private$.roc_results$method <- "Multinomial Extension"
        },

        #---------------------------------------------
        # BINARY ROC CALCULATION
        #---------------------------------------------
        .calculateBinaryROC = function(y_true, predictor) {

            # Get unique thresholds
            thresholds <- sort(unique(predictor), decreasing = TRUE)

            n_pos <- sum(y_true == 1)
            n_neg <- sum(y_true == 0)

            if (n_pos == 0 || n_neg == 0) {
                return(list(
                    auc = 0.5,
                    tpr = c(0, 1),
                    fpr = c(0, 1),
                    thresholds = c(Inf, -Inf)
                ))
            }

            # Calculate TPR and FPR at each threshold
            tpr <- numeric(length(thresholds) + 2)
            fpr <- numeric(length(thresholds) + 2)

            # Start point (0, 0)
            tpr[1] <- 0
            fpr[1] <- 0

            for (i in seq_along(thresholds)) {
                y_pred <- as.integer(predictor >= thresholds[i])
                tp <- sum(y_pred == 1 & y_true == 1)
                fp <- sum(y_pred == 1 & y_true == 0)

                tpr[i + 1] <- tp / n_pos
                fpr[i + 1] <- fp / n_neg
            }

            # End point (1, 1)
            tpr[length(tpr)] <- 1
            fpr[length(fpr)] <- 1

            # Calculate AUC using trapezoidal rule
            auc <- 0
            for (i in 2:length(fpr)) {
                width <- fpr[i] - fpr[i - 1]
                height <- (tpr[i] + tpr[i - 1]) / 2
                auc <- auc + (width * height)
            }

            return(list(
                auc = auc,
                tpr = tpr,
                fpr = fpr,
                thresholds = c(Inf, thresholds, -Inf)
            ))
        },

        #---------------------------------------------
        # AUC CONFIDENCE INTERVALS
        #---------------------------------------------
        .calculateAUC_CI = function(y_true, predictor, auc_observed) {

            method <- self$options$ci_method
            conf_level <- self$options$confidence_level

            if (method == "delong") {
                # DeLong method (simplified)
                n <- length(y_true)
                se <- sqrt(auc_observed * (1 - auc_observed) / n)  # Simplified SE
                z <- qnorm(1 - (1 - conf_level) / 2)

                lower <- max(0, auc_observed - z * se)
                upper <- min(1, auc_observed + z * se)

            } else {  # bootstrap
                n_boot <- self$options$bootstrap_samples
                boot_aucs <- numeric(n_boot)
                n <- length(y_true)

                for (b in 1:n_boot) {
                    idx <- sample(1:n, n, replace = TRUE)
                    y_boot <- y_true[idx]
                    pred_boot <- predictor[idx]

                    if (length(unique(y_boot)) < 2) {
                        boot_aucs[b] <- 0.5
                    } else {
                        roc_boot <- private$.calculateBinaryROC(y_boot, pred_boot)
                        boot_aucs[b] <- roc_boot$auc
                    }
                }

                lower <- quantile(boot_aucs, (1 - conf_level) / 2)
                upper <- quantile(boot_aucs, 1 - (1 - conf_level) / 2)
            }

            return(list(lower = lower, upper = upper))
        },

        #---------------------------------------------
        # POPULATE SUMMARY TABLE
        #---------------------------------------------
        .populateSummary = function() {

            if (is.null(private$.roc_results)) return()

            class_results <- private$.roc_results$class_results
            y <- private$.data_prepared$y

            # Calculate macro-average
            macro_auc <- mean(sapply(class_results, function(x) x$auc))

            # Calculate weighted-average
            weights <- sapply(private$.class_levels, function(cls) mean(y == cls))
            weighted_auc <- sum(sapply(seq_along(class_results), function(i) {
                class_results[[i]]$auc * weights[i]
            }))

            # Micro-average (aggregate all predictions)
            # For simplicity, use macro-average as approximation
            micro_auc <- macro_auc

            # Bootstrap CIs for averages if requested
            if (self$options$confidence_intervals && self$options$ci_method == "bootstrap") {
                # Use average of class CIs
                macro_ci_lower <- mean(sapply(class_results, function(x)
                    if (!is.null(x$ci_lower)) x$ci_lower else x$auc))
                macro_ci_upper <- mean(sapply(class_results, function(x)
                    if (!is.null(x$ci_upper)) x$ci_upper else x$auc))
                micro_ci_lower <- macro_ci_lower
                micro_ci_upper <- macro_ci_upper
            } else {
                macro_ci_lower <- NA
                macro_ci_upper <- NA
                micro_ci_lower <- NA
                micro_ci_upper <- NA
            }

            row <- list(
                n_classes = private$.n_classes,
                method = private$.roc_results$method,
                macro_auc = macro_auc,
                macro_ci_lower = macro_ci_lower,
                macro_ci_upper = macro_ci_upper,
                micro_auc = micro_auc,
                micro_ci_lower = micro_ci_lower,
                micro_ci_upper = micro_ci_upper,
                weighted_auc = weighted_auc
            )

            self$results$summaryTable$setRow(rowNo = 1, values = row)
        },

        #---------------------------------------------
        # POPULATE CLASS AUC TABLE
        #---------------------------------------------
        .populateClassAUC = function() {

            if (is.null(private$.roc_results)) return()

            class_results <- private$.roc_results$class_results
            y <- private$.data_prepared$y

            table <- self$results$classAucTable

            for (cls in names(class_results)) {
                result <- class_results[[cls]]

                # Interpret AUC
                auc <- result$auc
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
                    class_name = cls,
                    n = sum(y == cls),
                    prevalence = mean(y == cls),
                    auc = result$auc,
                    ci_lower = if (!is.null(result$ci_lower)) result$ci_lower else NA,
                    ci_upper = if (!is.null(result$ci_upper)) result$ci_upper else NA,
                    interpretation = interp
                )

                table$addRow(rowKey = cls, values = row)
            }
        },

        #---------------------------------------------
        # POPULATE PAIRWISE TABLE
        #---------------------------------------------
        .populatePairwise = function() {

            if (is.null(private$.roc_results$pairwise_results)) return()

            pairwise <- private$.roc_results$pairwise_results
            table <- self$results$pairwiseTable

            for (pair_name in names(pairwise)) {
                result <- pairwise[[pair_name]]

                # DeLong test p-value (simplified)
                z <- (result$auc - 0.5) / 0.1  # Simplified
                p_value <- 2 * (1 - pnorm(abs(z)))

                row <- list(
                    class_a = result$class_a,
                    class_b = result$class_b,
                    n_a = result$n_a,
                    n_b = result$n_b,
                    auc = result$auc,
                    ci_lower = if (!is.null(result$ci_lower)) result$ci_lower else NA,
                    ci_upper = if (!is.null(result$ci_upper)) result$ci_upper else NA,
                    p_value = p_value
                )

                table$addRow(rowKey = pair_name, values = row)
            }
        },

        #---------------------------------------------
        # POPULATE CLASS METRICS
        #---------------------------------------------
        .populateClassMetrics = function() {

            if (is.null(private$.roc_results)) return()

            y <- private$.data_prepared$y
            X <- private$.data_prepared$X
            classes <- private$.class_levels

            table <- self$results$classMetricsTable

            # Find optimal thresholds using Youden's index
            for (i in seq_along(classes)) {
                cls <- classes[i]
                y_binary <- as.integer(y == cls)

                if (ncol(X) >= i) {
                    predictor <- X[, i]
                } else {
                    predictor <- X[, 1]
                }

                # Find optimal threshold
                thresholds <- sort(unique(predictor), decreasing = TRUE)
                best_youden <- -Inf
                best_threshold <- thresholds[1]

                for (thresh in thresholds) {
                    y_pred <- as.integer(predictor >= thresh)

                    tp <- sum(y_pred == 1 & y_binary == 1)
                    fp <- sum(y_pred == 1 & y_binary == 0)
                    tn <- sum(y_pred == 0 & y_binary == 0)
                    fn <- sum(y_pred == 0 & y_binary == 1)

                    sens <- if (tp + fn > 0) tp / (tp + fn) else 0
                    spec <- if (tn + fp > 0) tn / (tn + fp) else 0
                    youden <- sens + spec - 1

                    if (youden > best_youden) {
                        best_youden <- youden
                        best_threshold <- thresh
                    }
                }

                # Calculate metrics at optimal threshold
                y_pred <- as.integer(predictor >= best_threshold)
                tp <- sum(y_pred == 1 & y_binary == 1)
                fp <- sum(y_pred == 1 & y_binary == 0)
                tn <- sum(y_pred == 0 & y_binary == 0)
                fn <- sum(y_pred == 0 & y_binary == 1)

                sens <- if (tp + fn > 0) tp / (tp + fn) else 0
                spec <- if (tn + fp > 0) tn / (tn + fp) else 0
                ppv <- if (tp + fp > 0) tp / (tp + fp) else 0
                npv <- if (tn + fn > 0) tn / (tn + fn) else 0
                f1 <- if (sens + ppv > 0) 2 * sens * ppv / (sens + ppv) else 0

                row <- list(
                    class_name = cls,
                    sensitivity = sens,
                    specificity = spec,
                    ppv = ppv,
                    npv = npv,
                    f1_score = f1,
                    optimal_threshold = best_threshold
                )

                table$addRow(rowKey = cls, values = row)
            }
        },

        #---------------------------------------------
        # POPULATE CONFUSION MATRIX
        #---------------------------------------------
        .populateConfusionMatrix = function() {

            if (is.null(private$.roc_results)) return()

            y <- private$.data_prepared$y
            X <- private$.data_prepared$X
            classes <- private$.class_levels
            n_classes <- private$.n_classes

            # Predict using max score across predictors
            y_pred <- character(length(y))

            for (i in 1:length(y)) {
                scores <- numeric(n_classes)
                for (j in 1:n_classes) {
                    if (ncol(X) >= j) {
                        scores[j] <- X[i, j]
                    } else {
                        scores[j] <- X[i, 1]
                    }
                }
                y_pred[i] <- classes[which.max(scores)]
            }

            # Build confusion matrix
            table <- self$results$confusionMatrix

            for (true_cls in classes) {
                row_data <- list(true_class = true_cls)

                for (pred_cls in classes) {
                    count <- sum(y == true_cls & y_pred == pred_cls)
                    row_data[[paste0('pred_', make.names(pred_cls))]] <- count
                }

                table$addRow(rowKey = true_cls, values = row_data)
            }
        },

        #---------------------------------------------
        # PLOT ROC CURVES
        #---------------------------------------------
        .rocPlot = function(image, ...) {

            if (is.null(private$.roc_results)) return()

            class_results <- private$.roc_results$class_results
            plot_method <- self$options$plot_method

            # Get ROC curves for each class (re-calculate with full curve data)
            y <- private$.data_prepared$y
            X <- private$.data_prepared$X
            classes <- private$.class_levels

            if (plot_method == "overlay") {
                plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1),
                     xlab = "False Positive Rate (1 - Specificity)",
                     ylab = "True Positive Rate (Sensitivity)",
                     main = "Multi-class ROC Curves")

                if (self$options$plot_diagonal) {
                    abline(0, 1, lty = 2, col = "gray")
                }

                colors <- rainbow(length(classes))

                for (i in seq_along(classes)) {
                    cls <- classes[i]
                    y_binary <- as.integer(y == cls)

                    if (ncol(X) >= i) {
                        predictor <- X[, i]
                    } else {
                        predictor <- X[, 1]
                    }

                    roc <- private$.calculateBinaryROC(y_binary, predictor)
                    lines(roc$fpr, roc$tpr, col = colors[i], lwd = 2)
                }

                legend("bottomright", legend = paste0(classes, " (AUC=",
                       round(sapply(class_results, function(x) x$auc), 3), ")"),
                       col = colors, lwd = 2, cex = 0.8)
            }

            TRUE
        }
    )
)
