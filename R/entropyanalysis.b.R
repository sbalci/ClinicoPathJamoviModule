
#' @title Entropy and Mutual Information Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom stats quantile
#' @importFrom graphics hist barplot par
#' @export


entropyanalysisClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "entropyanalysisClass",
    inherit = entropyanalysisBase,
    private = list(

        # Data storage
        .data_prepared = NULL,
        .entropy_results = NULL,
        .mi_results = NULL,

        #---------------------------------------------
        # INIT
        #---------------------------------------------
        .init = function() {

            # Instructions
            html <- "<h3>Entropy and Mutual Information Analysis</h3>
            <p>Quantifies uncertainty and information content in predictions and features.</p>
            <h4>Shannon Entropy:</h4>
            <p style='font-family: monospace;'>H(X) = -Σ p(x) × log₂(p(x))</p>
            <p>Where p(x) is the probability of outcome x. Higher entropy = more uncertainty.</p>
            <h4>Mutual Information:</h4>
            <p style='font-family: monospace;'>I(X;Y) = H(X) + H(Y) - H(X,Y)</p>
            <p>Measures how much knowing X reduces uncertainty about Y.</p>
            <h4>Applications:</h4>
            <ul>
            <li><b>AI Triage:</b> Flag uncertain predictions for human review</li>
            <li><b>Feature Selection:</b> Select features with high MI to outcome</li>
            <li><b>Test Ordering:</b> Prioritize tests that maximize information gain</li>
            </ul>"

            self$results$instructionsText$setContent(html)

            # Interpretation guide
            interp_html <- "<h3>Interpretation Guide</h3>
            <h4>Entropy Values (normalized):</h4>
            <ul>
            <li><b>0.0:</b> Perfect certainty (100% confident in one class)</li>
            <li><b>0.0-0.3:</b> Low uncertainty (confident prediction)</li>
            <li><b>0.3-0.7:</b> Moderate uncertainty (consider human review)</li>
            <li><b>0.7-1.0:</b> High uncertainty (defer to expert)</li>
            <li><b>1.0:</b> Maximum uncertainty (uniform distribution)</li>
            </ul>
            <h4>Mutual Information:</h4>
            <ul>
            <li><b>I(X;Y) = 0:</b> X provides no information about Y (independent)</li>
            <li><b>I(X;Y) > 0:</b> X reduces uncertainty about Y</li>
            <li><b>I(X;Y) = H(Y):</b> X completely determines Y</li>
            <li><b>Normalized MI ∈ [0,1]:</b> I(X;Y) / min(H(X), H(Y))</li>
            </ul>
            <h4>Clinical Decision Rules:</h4>
            <ul>
            <li><b>High entropy + correct:</b> Lucky guess, review case</li>
            <li><b>High entropy + incorrect:</b> Expected error, acceptable</li>
            <li><b>Low entropy + incorrect:</b> Systematic error, investigate</li>
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

            if (is.null(self$options$probability_vars) || length(self$options$probability_vars) == 0) {
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

            # Calculate entropy
            if (self$options$calculate_entropy) {
                private$.calculateEntropy()
            }

            # Calculate mutual information
            if (self$options$calculate_mutual_information) {
                private$.calculateMutualInformation()
            }

            # Populate results
            private$.populateSummary()
            private$.populateEntropyByClass()

            if (self$options$calculate_mutual_information) {
                private$.populateMutualInfo()
            }

            if (self$options$show_case_level) {
                private$.populateCaseLevel()
            }

            if (self$options$calculate_kl_divergence) {
                private$.populateKLDivergence()
            }
        },

        #---------------------------------------------
        # PREPARE DATA
        #---------------------------------------------
        .prepareData = function() {

            data <- self$data
            outcome_var <- self$options$outcome
            prob_vars <- self$options$probability_vars

            # Extract outcome
            y <- as.factor(data[[outcome_var]])
            classes <- levels(y)
            n_classes <- length(classes)

            # Extract probabilities
            probs_matrix <- matrix(NA, nrow = nrow(data), ncol = length(prob_vars))

            for (i in seq_along(prob_vars)) {
                probs_matrix[, i] <- as.numeric(data[[prob_vars[i]]])
            }

            # Check dimensions
            if (ncol(probs_matrix) != n_classes) {
                warning(paste("Number of probability variables (", ncol(probs_matrix),
                              ") does not match number of outcome classes (", n_classes, ")",
                              sep = ""))
            }

            # Normalize probabilities to sum to 1 for each case
            row_sums <- rowSums(probs_matrix, na.rm = TRUE)
            probs_matrix_norm <- probs_matrix / row_sums

            # Handle any remaining NA or invalid probabilities
            probs_matrix_norm[is.na(probs_matrix_norm)] <- 1 / n_classes
            probs_matrix_norm[probs_matrix_norm < 0] <- 0
            probs_matrix_norm[probs_matrix_norm > 1] <- 1

            private$.data_prepared <- list(
                y = y,
                classes = classes,
                n_classes = n_classes,
                probs = probs_matrix_norm,
                n = length(y)
            )
        },

        #---------------------------------------------
        # CALCULATE ENTROPY
        #---------------------------------------------
        .calculateEntropy = function() {

            probs <- private$.data_prepared$probs
            n <- private$.data_prepared$n
            n_classes <- private$.data_prepared$n_classes

            # Calculate Shannon entropy for each case
            entropy <- numeric(n)

            for (i in 1:n) {
                p <- probs[i, ]
                p <- p[p > 0]  # Remove zero probabilities (log(0) undefined)

                if (length(p) > 0) {
                    entropy[i] <- -sum(p * log2(p))
                } else {
                    entropy[i] <- 0
                }
            }

            # Normalize entropy if requested
            if (self$options$normalize_entropy) {
                max_entropy <- log2(n_classes)
                entropy <- entropy / max_entropy
            }

            # Identify predicted class (highest probability)
            predicted_class_idx <- apply(probs, 1, which.max)
            predicted_class <- private$.data_prepared$classes[predicted_class_idx]

            # Max probability
            max_prob <- apply(probs, 1, max)

            # Flag high uncertainty
            threshold <- self$options$uncertainty_threshold
            high_uncertainty <- entropy > threshold

            private$.entropy_results <- list(
                entropy = entropy,
                predicted_class = predicted_class,
                max_prob = max_prob,
                high_uncertainty = high_uncertainty
            )
        },

        #---------------------------------------------
        # CALCULATE MUTUAL INFORMATION
        #---------------------------------------------
        .calculateMutualInformation = function() {

            y <- private$.data_prepared$y
            predictor_var <- self$options$predictor_var

            if (is.null(predictor_var) || predictor_var == "") {
                # Calculate MI between outcome and predicted probabilities
                # Use predicted class as proxy
                if (!is.null(private$.entropy_results)) {
                    x <- factor(private$.entropy_results$predicted_class)
                    mi <- private$.mutualInformationDiscrete(x, y)

                    private$.mi_results <- list(
                        variable_pair = "Predicted vs True",
                        mi = mi$mi,
                        normalized_mi = mi$normalized_mi
                    )
                }
            } else {
                # Calculate MI between predictor and outcome
                data <- self$data
                x <- data[[predictor_var]]

                if (is.numeric(x)) {
                    # Discretize continuous predictor
                    x_discrete <- private$.discretizeVariable(x)
                    mi <- private$.mutualInformationDiscrete(x_discrete, y)
                } else {
                    x <- as.factor(x)
                    mi <- private$.mutualInformationDiscrete(x, y)
                }

                private$.mi_results <- list(
                    variable_pair = paste(predictor_var, "vs", self$options$outcome),
                    mi = mi$mi,
                    normalized_mi = mi$normalized_mi
                )
            }
        },

        #---------------------------------------------
        # MUTUAL INFORMATION (DISCRETE)
        #---------------------------------------------
        .mutualInformationDiscrete = function(x, y) {

            # Calculate marginal entropies
            h_x <- private$.entropyDiscrete(x)
            h_y <- private$.entropyDiscrete(y)

            # Calculate joint entropy
            joint_counts <- table(x, y)
            joint_probs <- joint_counts / sum(joint_counts)
            joint_probs <- joint_probs[joint_probs > 0]

            h_xy <- -sum(joint_probs * log2(joint_probs))

            # Mutual information
            mi <- h_x + h_y - h_xy

            # Normalized MI
            normalized_mi <- mi / min(h_x, h_y)

            return(list(
                mi = mi,
                normalized_mi = normalized_mi,
                h_x = h_x,
                h_y = h_y,
                h_xy = h_xy
            ))
        },

        #---------------------------------------------
        # ENTROPY (DISCRETE)
        #---------------------------------------------
        .entropyDiscrete = function(x) {
            counts <- table(x)
            probs <- counts / sum(counts)
            probs <- probs[probs > 0]

            entropy <- -sum(probs * log2(probs))
            return(entropy)
        },

        #---------------------------------------------
        # DISCRETIZE VARIABLE
        #---------------------------------------------
        .discretizeVariable = function(x) {

            method <- self$options$binning_method
            n_bins <- self$options$n_bins

            if (method == "equal_width") {
                breaks <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = n_bins + 1)
                x_discrete <- cut(x, breaks = breaks, include.lowest = TRUE)
            } else if (method == "equal_freq") {
                breaks <- quantile(x, probs = seq(0, 1, length.out = n_bins + 1), na.rm = TRUE)
                x_discrete <- cut(x, breaks = breaks, include.lowest = TRUE)
            } else {  # sturges
                x_discrete <- cut(x, breaks = "Sturges")
            }

            return(x_discrete)
        },

        #---------------------------------------------
        # POPULATE SUMMARY TABLE
        #---------------------------------------------
        .populateSummary = function() {

            if (is.null(private$.entropy_results)) return()

            entropy <- private$.entropy_results$entropy
            high_unc <- private$.entropy_results$high_uncertainty

            row <- list(
                n = length(entropy),
                mean_entropy = mean(entropy, na.rm = TRUE),
                median_entropy = median(entropy, na.rm = TRUE),
                sd_entropy = sd(entropy, na.rm = TRUE),
                high_uncertainty_n = sum(high_unc, na.rm = TRUE),
                high_uncertainty_pct = mean(high_unc, na.rm = TRUE)
            )

            self$results$summaryTable$setRow(rowNo = 1, values = row)
        },

        #---------------------------------------------
        # POPULATE ENTROPY BY CLASS
        #---------------------------------------------
        .populateEntropyByClass = function() {

            if (is.null(private$.entropy_results)) return()

            y <- private$.data_prepared$y
            classes <- private$.data_prepared$classes
            entropy <- private$.entropy_results$entropy
            high_unc <- private$.entropy_results$high_uncertainty

            table <- self$results$entropyTable

            for (cls in classes) {
                idx <- y == cls
                entropy_class <- entropy[idx]
                high_unc_class <- high_unc[idx]

                row <- list(
                    class = cls,
                    n = sum(idx),
                    mean_entropy = mean(entropy_class, na.rm = TRUE),
                    sd_entropy = sd(entropy_class, na.rm = TRUE),
                    pct_high_uncertainty = mean(high_unc_class, na.rm = TRUE)
                )

                table$addRow(rowKey = cls, values = row)
            }
        },

        #---------------------------------------------
        # POPULATE MUTUAL INFORMATION TABLE
        #---------------------------------------------
        .populateMutualInfo = function() {

            if (is.null(private$.mi_results)) return()

            table <- self$results$mutualInfoTable

            mi <- private$.mi_results$mi
            nmi <- private$.mi_results$normalized_mi

            # Interpret
            if (nmi < 0.1) {
                interp <- "Very weak dependence"
            } else if (nmi < 0.3) {
                interp <- "Weak dependence"
            } else if (nmi < 0.6) {
                interp <- "Moderate dependence"
            } else if (nmi < 0.9) {
                interp <- "Strong dependence"
            } else {
                interp <- "Very strong dependence"
            }

            row <- list(
                variable_pair = private$.mi_results$variable_pair,
                mutual_information = mi,
                normalized_mi = nmi,
                interpretation = interp
            )

            table$addRow(rowKey = "mi1", values = row)
        },

        #---------------------------------------------
        # POPULATE CASE LEVEL TABLE
        #---------------------------------------------
        .populateCaseLevel = function() {

            if (is.null(private$.entropy_results)) return()

            y <- private$.data_prepared$y
            entropy <- private$.entropy_results$entropy
            predicted <- private$.entropy_results$predicted_class
            max_prob <- private$.entropy_results$max_prob
            high_unc <- private$.entropy_results$high_uncertainty

            table <- self$results$caseLevelTable

            # Show first 100 cases or all if less
            n_show <- min(100, length(y))

            for (i in 1:n_show) {
                row <- list(
                    case_id = i,
                    true_class = as.character(y[i]),
                    predicted_class = as.character(predicted[i]),
                    entropy = entropy[i],
                    max_prob = max_prob[i],
                    high_uncertainty = if (high_unc[i]) "Yes" else "No"
                )

                table$addRow(rowKey = i, values = row)
            }

            if (length(y) > 100) {
                message("Showing first 100 cases only")
            }
        },

        #---------------------------------------------
        # POPULATE KL DIVERGENCE TABLE
        #---------------------------------------------
        .populateKLDivergence = function() {

            probs <- private$.data_prepared$probs
            n_classes <- private$.data_prepared$n_classes

            table <- self$results$klDivergenceTable

            # Uniform distribution (reference)
            uniform_prob <- 1 / n_classes

            # Show first 50 cases
            n_show <- min(50, nrow(probs))

            for (i in 1:n_show) {
                p <- probs[i, ]
                p <- p[p > 0]

                # KL divergence from uniform
                kl <- sum(p * log2(p / uniform_prob))

                # Interpret
                if (kl < 0.1) {
                    interp <- "Near uniform (high uncertainty)"
                } else if (kl < 0.5) {
                    interp <- "Moderate certainty"
                } else {
                    interp <- "High certainty"
                }

                row <- list(
                    case_id = i,
                    kl_divergence = kl,
                    interpretation = interp
                )

                table$addRow(rowKey = i, values = row)
            }
        },

        #---------------------------------------------
        # PLOT ENTROPY DISTRIBUTION
        #---------------------------------------------
        .entropyDistPlot = function(image, ...) {

            if (is.null(private$.entropy_results)) return()

            entropy <- private$.entropy_results$entropy
            threshold <- self$options$uncertainty_threshold

            hist(entropy, breaks = 30, col = "lightblue", border = "white",
                 main = "Entropy Distribution",
                 xlab = "Entropy (normalized)",
                 ylab = "Frequency")

            abline(v = threshold, col = "red", lwd = 2, lty = 2)
            legend("topright", legend = paste("Threshold =", threshold),
                   col = "red", lwd = 2, lty = 2)

            TRUE
        },

        #---------------------------------------------
        # PLOT UNCERTAINTY BY CLASS
        #---------------------------------------------
        .uncertaintyByClassPlot = function(image, ...) {

            if (is.null(private$.entropy_results)) return()

            y <- private$.data_prepared$y
            classes <- private$.data_prepared$classes
            entropy <- private$.entropy_results$entropy

            # Create boxplot
            boxplot(entropy ~ y, col = "lightblue",
                    main = "Entropy Distribution by True Class",
                    xlab = "True Class",
                    ylab = "Entropy (normalized)")

            abline(h = self$options$uncertainty_threshold, col = "red", lwd = 2, lty = 2)

            TRUE
        }
    )
)
