
#' @title Generalized Estimating Equations
#'
#' @description
#' Fits Generalized Estimating Equation (GEE) models for analyzing correlated/clustered data.
#' GEE provides population-averaged (marginal) estimates for studies with repeated measures,
#' longitudinal data, multi-site studies, or clustered observations such as multiple samples
#' per subject in pathology studies.
#'
#' @details
#' GEE is essential for pathology studies where observations are correlated within clusters:
#' - Multiple biopsies from the same patient
#' - Bilateral organs (paired kidneys, eyes)
#' - Repeated measures over time
#' - Multi-site or multi-center studies
#' - Hierarchical/nested data structures
#'
#' **Key Advantages of GEE:**
#' - Provides valid inference even if correlation structure is misspecified (with robust SE)
#' - Does not require distributional assumptions for within-cluster correlation
#' - Population-averaged interpretation (vs. subject-specific in mixed models)
#' - Handles unbalanced designs (different cluster sizes)
#'
#' **Working Correlation Structures:**
#' - **Exchangeable**: Constant correlation between all pairs within cluster (most common)
#' - **AR(1)**: Autoregressive - correlation decays with time lag (for longitudinal data)
#' - **Unstructured**: Estimates all pairwise correlations (requires many observations)
#' - **Independence**: No correlation (equivalent to GLM)
#'
#' **Model Selection:**
#' QIC (Quasi-likelihood Information Criterion) is used for comparing models with different
#' correlation structures or predictor sets. Lower QIC indicates better model fit.
#'
#' @importFrom geepack geeglm
#' @importFrom stats coef vcov confint pnorm qnorm pchisq
#' @importFrom jmvcore toNumeric
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_gradient2 labs theme_minimal theme element_text
#' @importFrom R6 R6Class
#'
#' @export
geemodelClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "geemodelClass",
    inherit = geemodelBase,
    private = list(
        .model = NULL,

        .init = function() {

            # Initialize instructions
            if (is.null(self$options$outcome) || is.null(self$options$cluster_id)) {
                self$results$instructions$setContent(
                    "<html>
                    <head>
                    <style>
                    .instructions { font-family: sans-serif; padding: 20px; }
                    .instructions h3 { color: #2c3e50; }
                    .instructions ul { line-height: 1.6; }
                    .instructions code { background: #f4f4f4; padding: 2px 6px; border-radius: 3px; }
                    </style>
                    </head>
                    <body>
                    <div class='instructions'>
                    <h3>How to Use GEE Analysis</h3>
                    <ol>
                    <li><strong>Outcome Variable</strong>: Select your dependent variable (continuous, binary, or count)</li>
                    <li><strong>Predictor Variables</strong>: Select one or more independent variables</li>
                    <li><strong>Cluster/Subject ID</strong>: REQUIRED - Variable identifying clusters (e.g., patient_id, dog_id)</li>
                    <li><strong>Time Variable</strong>: Optional - For longitudinal data with AR(1) correlation</li>
                    </ol>

                    <h3>Choosing Distribution Family:</h3>
                    <ul>
                    <li><strong>Gaussian</strong>: Continuous numeric outcomes (e.g., tumor size, biomarker levels)
                        <br><em>Outcome must be: Numeric variable</em></li>
                    <li><strong>Binomial</strong>: Binary outcomes (e.g., diagnosis positive/negative)
                        <br><em>Outcome must be: Factor with 2 levels OR numeric 0/1</em></li>
                    <li><strong>Poisson</strong>: Count outcomes (e.g., number of metastases)
                        <br><em>Outcome must be: Non-negative integer counts</em></li>
                    <li><strong>Gamma</strong>: Positive continuous with right skew (e.g., survival time)
                        <br><em>Outcome must be: Positive numeric values (no zeros)</em></li>
                    </ul>

                    <p style='color: #d9534f;'><strong>Important:</strong> Make sure your outcome variable type matches
                    the selected family. The analysis will display an error if there's a mismatch.</p>

                    <h3>Choosing Correlation Structure:</h3>
                    <ul>
                    <li><strong>Exchangeable</strong>: Use when observations within cluster are equally correlated (most common)</li>
                    <li><strong>AR(1)</strong>: Use for longitudinal data where closer time points are more correlated</li>
                    <li><strong>Unstructured</strong>: Estimates all correlations (requires large sample size)</li>
                    <li><strong>Independence</strong>: No correlation (use for comparison with exchangeable)</li>
                    </ul>

                    <p><strong>Tip:</strong> Always use <code>Robust Standard Errors</code> (default) for valid inference
                    even if correlation structure is misspecified.</p>
                    </div>
                    </body>
                    </html>"
                )
                self$results$instructions$setVisible(TRUE)
                return()
            }

            # Initialize tables
            self$results$modelInfo$setVisible(TRUE)
            self$results$coefficientsTable$setVisible(TRUE)

            # Add methodology note
            private$.addMethodologyNote()

        },

        .run = function() {

            # Check required inputs
            if (is.null(self$options$outcome) ||
                length(self$options$predictors) == 0 ||
                is.null(self$options$cluster_id)) {
                return()
            }

            private$.checkpoint()

            # Get data
            outcome_var <- self$options$outcome
            predictor_vars <- self$options$predictors
            cluster_var <- self$options$cluster_id
            time_var <- self$options$time_var

            # Prepare data - get all needed variables
            all_vars <- c(outcome_var, predictor_vars, cluster_var)
            if (!is.null(time_var)) all_vars <- c(all_vars, time_var)

            # Extract data and convert to regular data frame
            raw_data <- self$data
            data_list <- lapply(all_vars, function(v) raw_data[[v]])
            names(data_list) <- all_vars
            data <- as.data.frame(data_list)

            # Remove missing values
            missing_count <- sum(!complete.cases(data))
            if (missing_count > 0) {
                self$results$instructions$setContent(
                    paste0("<html><body><p style='color: red;'>Warning: ", missing_count,
                           " observations removed due to missing values.</p></body></html>")
                )
                self$results$instructions$setVisible(TRUE)
            }

            data <- data[complete.cases(data), ]

            if (nrow(data) < 10) {
                self$setError("Insufficient data: Need at least 10 complete observations for GEE analysis")
                return()
            }

            private$.checkpoint()

            # Prepare outcome variable based on family
            family_type <- self$options$family
            outcome_data <- data[[outcome_var]]

            # Validate and prepare outcome based on family
            if (family_type == "binomial") {
                # For binomial, outcome should be factor with 2 levels or numeric 0/1
                if (is.factor(outcome_data)) {
                    if (nlevels(outcome_data) != 2) {
                        self$setError(paste0("Binomial family requires binary outcome. The variable '",
                                           outcome_var, "' has ", nlevels(outcome_data),
                                           " levels. Please use a binary variable."))
                        return()
                    }
                    # Convert factor to numeric 0/1 (second level = 1)
                    # Use droplevels to ensure only 2 levels, then convert
                    outcome_data <- droplevels(outcome_data)
                    numeric_outcome <- as.numeric(outcome_data) - 1

                    # Check for NA values created during conversion
                    if (any(is.na(numeric_outcome) & !is.na(outcome_data))) {
                        self$setError(paste0("Error converting factor '", outcome_var,
                                           "' to binary numeric. Please check the data."))
                        return()
                    }

                    # Replace in data frame
                    data[[outcome_var]] <- numeric_outcome

                } else if (is.numeric(outcome_data)) {
                    # Check if it's 0/1
                    unique_vals <- unique(outcome_data[!is.na(outcome_data)])
                    if (!all(unique_vals %in% c(0, 1))) {
                        self$setError(paste0("Binomial family requires binary 0/1 outcome. The variable '",
                                           outcome_var, "' contains values other than 0 or 1: ",
                                           paste(setdiff(unique_vals, c(0, 1)), collapse = ", ")))
                        return()
                    }
                } else {
                    self$setError(paste0("Binomial family requires binary outcome. The variable '",
                                       outcome_var, "' is not factor or numeric."))
                    return()
                }
            } else if (family_type == "gaussian") {
                # For gaussian, outcome should be numeric
                if (!is.numeric(outcome_data)) {
                    self$setError(paste0("Gaussian family requires continuous numeric outcome. The variable '",
                                       outcome_var, "' is not numeric. For binary outcomes, use Binomial family."))
                    return()
                }
            } else if (family_type == "poisson") {
                # For poisson, outcome should be non-negative integer
                if (!is.numeric(outcome_data)) {
                    self$setError(paste0("Poisson family requires count (non-negative integer) outcome. The variable '",
                                       outcome_var, "' is not numeric."))
                    return()
                }
                if (any(outcome_data < 0, na.rm = TRUE)) {
                    self$setError(paste0("Poisson family requires non-negative counts. The variable '",
                                       outcome_var, "' contains negative values."))
                    return()
                }
            } else if (family_type == "gamma") {
                # For gamma, outcome should be positive numeric
                if (!is.numeric(outcome_data)) {
                    self$setError(paste0("Gamma family requires positive continuous outcome. The variable '",
                                       outcome_var, "' is not numeric."))
                    return()
                }
                if (any(outcome_data <= 0, na.rm = TRUE)) {
                    self$setError(paste0("Gamma family requires positive values. The variable '",
                                       outcome_var, "' contains zero or negative values."))
                    return()
                }
            }

            # Build formula
            formula_str <- paste(outcome_var, "~", paste(predictor_vars, collapse = " + "))
            formula_obj <- as.formula(formula_str)

            # Get family
            family_choice <- switch(self$options$family,
                "gaussian" = gaussian(),
                "binomial" = binomial(),
                "poisson" = poisson(),
                "gamma" = Gamma(),
                gaussian()
            )

            # Get correlation structure
            corstr <- switch(self$options$corstr,
                "independence" = "independence",
                "exchangeable" = "exchangeable",
                "ar1" = "ar1",
                "unstructured" = "unstructured",
                "exchangeable"
            )

            private$.checkpoint()

            # Validate outcome variable one more time before fitting
            final_outcome <- data[[outcome_var]]
            if (any(is.na(final_outcome))) {
                n_missing <- sum(is.na(final_outcome))
                self$setError(paste0("Outcome variable '", outcome_var,
                                   "' contains ", n_missing, " NA values after preparation. ",
                                   "Please check your data."))
                return()
            }
            if (any(is.infinite(final_outcome))) {
                self$setError(paste0("Outcome variable '", outcome_var,
                                   "' contains infinite values. Please check your data."))
                return()
            }
            if (!is.numeric(final_outcome)) {
                self$setError(paste0("Outcome variable '", outcome_var,
                                   "' is not numeric after conversion (type: ",
                                   class(final_outcome), "). This is an internal error."))
                return()
            }

            private$.checkpoint()

            # Fit GEE model
            tryCatch({
                if (!requireNamespace("geepack", quietly = TRUE)) {
                    self$setError("The 'geepack' package is required but not installed. Please install it using: install.packages('geepack')")
                    return()
                }

                # Fit model
                private$.model <- geepack::geeglm(
                    formula = formula_obj,
                    data = data,
                    id = data[[cluster_var]],
                    family = family_choice,
                    corstr = corstr,
                    std.err = if (self$options$robust_se) "san.se" else "jack"
                )

                private$.checkpoint()

                # Populate model info
                private$.populateModelInfo(data, cluster_var)

                # Populate coefficients
                private$.populateCoefficients()

                # Calculate QIC if requested
                if (self$options$qic) {
                    private$.calculateQIC()
                }

                # Post-hoc comparisons if requested
                if (self$options$posthoc && any(sapply(data[predictor_vars], is.factor))) {
                    private$.performPosthoc()
                }

                # Diagnostics
                if (self$options$diagnostics) {
                    private$.populateDiagnostics(data, cluster_var)
                }

                # Add interpretation note
                private$.addInterpretationNote()

            }, error = function(e) {
                self$setError(paste("GEE model fitting failed:", e$message))
            })

        },

        .populateModelInfo = function(data, cluster_var) {

            table <- self$results$modelInfo

            n_obs <- nrow(data)
            n_clusters <- length(unique(data[[cluster_var]]))
            avg_cluster_size <- round(n_obs / n_clusters, 2)

            family_name <- switch(self$options$family,
                "gaussian" = "Gaussian (Identity link)",
                "binomial" = "Binomial (Logit link)",
                "poisson" = "Poisson (Log link)",
                "gamma" = "Gamma (Log link)"
            )

            corstr_name <- switch(self$options$corstr,
                "independence" = "Independence",
                "exchangeable" = "Exchangeable",
                "ar1" = "AR(1)",
                "unstructured" = "Unstructured"
            )

            table$setRow(rowNo=1, values=list(
                info=as.character("Number of Observations"),
                value=as.character(n_obs)))
            table$setRow(rowNo=2, values=list(
                info=as.character("Number of Clusters"),
                value=as.character(n_clusters)))
            table$setRow(rowNo=3, values=list(
                info=as.character("Average Cluster Size"),
                value=as.character(avg_cluster_size)))
            table$setRow(rowNo=4, values=list(
                info=as.character("Family"),
                value=as.character(family_name)))
            table$setRow(rowNo=5, values=list(
                info=as.character("Correlation Structure"),
                value=as.character(corstr_name)))
            table$setRow(rowNo=6, values=list(
                info=as.character("Standard Errors"),
                value=as.character(if(self$options$robust_se) "Robust (Sandwich)" else "Model-Based")))

        },

        .populateCoefficients = function() {

            if (is.null(private$.model)) return()

            tryCatch({
                table <- self$results$coefficientsTable

                # Get coefficients summary
                coef_summary <- summary(private$.model)$coefficients

                # Calculate confidence intervals
                alpha <- 1 - self$options$conf_level
                z_crit <- qnorm(1 - alpha/2)

                for (i in 1:nrow(coef_summary)) {

                    term <- as.character(rownames(coef_summary)[i])
                    estimate <- as.numeric(coef_summary[i, "Estimate"])
                    se <- as.numeric(coef_summary[i, "Std.err"])
                    wald <- as.numeric(coef_summary[i, "Wald"])
                    p_value <- as.numeric(coef_summary[i, "Pr(>|W|)"])

                    lower_ci <- as.numeric(estimate - z_crit * se)
                    upper_ci <- as.numeric(estimate + z_crit * se)

                    # Get degrees of freedom (use number of clusters - number of parameters)
                    # For GEE, df should be based on clusters, not observations
                    n_clusters <- length(unique(private$.model$id))
                    n_params <- length(coef(private$.model))
                    df <- as.integer(max(1, n_clusters - n_params))  # Ensure at least 1

                    # Debug: Check all values are atomic
                    if (!is.atomic(term) || length(term) != 1) {
                        stop(paste("term is not atomic:", class(term), "length:", length(term)))
                    }
                    if (!is.atomic(estimate) || length(estimate) != 1) {
                        stop(paste("estimate is not atomic:", class(estimate), "length:", length(estimate)))
                    }
                    if (!is.atomic(se) || length(se) != 1) {
                        stop(paste("se is not atomic:", class(se), "length:", length(se)))
                    }
                    if (!is.atomic(wald) || length(wald) != 1) {
                        stop(paste("wald is not atomic:", class(wald), "length:", length(wald)))
                    }
                    if (!is.atomic(df) || length(df) != 1) {
                        stop(paste("df is not atomic:", class(df), "length:", length(df)))
                    }
                    if (!is.atomic(p_value) || length(p_value) != 1) {
                        stop(paste("p_value is not atomic:", class(p_value), "length:", length(p_value)))
                    }
                    if (!is.atomic(lower_ci) || length(lower_ci) != 1) {
                        stop(paste("lower_ci is not atomic:", class(lower_ci), "length:", length(lower_ci)))
                    }
                    if (!is.atomic(upper_ci) || length(upper_ci) != 1) {
                        stop(paste("upper_ci is not atomic:", class(upper_ci), "length:", length(upper_ci)))
                    }

                    table$addRow(rowKey=i, values=list(
                        term = term,
                        estimate = estimate,
                        se = se,
                        wald = wald,
                        df = df,
                        p = p_value,
                        lower = lower_ci,
                        upper = upper_ci
                    ))
                }

            }, error = function(e) {
                self$setError(paste("Error populating coefficients:", e$message))
            })

        },

        .calculateQIC = function() {

            if (is.null(private$.model)) return()

            tryCatch({

                # Calculate QIC manually since not all packages provide it
                # QIC = -2 * quasi-likelihood + 2 * trace(Omega_I * V_R)

                # Get model components
                mu <- fitted(private$.model)
                y <- private$.model$y
                family <- private$.model$family

                # Calculate quasi-likelihood
                quasi_ll <- sum(family$dev.resids(y, mu, wt = rep(1, length(y))))

                # Number of parameters
                p <- length(coef(private$.model))

                # Simple QIC approximation
                qic_value <- as.numeric(quasi_ll + 2 * p)
                qicu_value <- as.numeric(quasi_ll + 2 * p * (private$.model$nobs / (private$.model$nobs - p)))
                aic_value <- as.numeric(quasi_ll + 2 * p)

                table <- self$results$qicTable
                table$setRow(rowNo=1, values=list(
                    qic = qic_value,
                    qicu = qicu_value,
                    aic = aic_value
                ))

            }, error = function(e) {
                # QIC calculation failed - not critical
            })

        },

        .performPosthoc = function() {

            if (is.null(private$.model)) return()

            tryCatch({

                if (!requireNamespace("emmeans", quietly = TRUE)) {
                    return()  # Skip if emmeans not available
                }

                # Get factor variables
                predictor_vars <- self$options$predictors
                data <- self$data
                factor_vars <- predictor_vars[sapply(data[predictor_vars], is.factor)]

                if (length(factor_vars) == 0) return()

                table <- self$results$posthocTable

                # Get adjustment method
                adjust_method <- switch(self$options$posthoc_adjust,
                    "bonferroni" = "bonferroni",
                    "holm" = "holm",
                    "bh" = "BH",
                    "none" = "none",
                    "holm"
                )

                # Perform pairwise comparisons for first factor variable
                factor_var <- factor_vars[1]

                emm <- emmeans::emmeans(private$.model, specs = factor_var)
                pairs <- emmeans::pairs(emm, adjust = adjust_method)
                pairs_summary <- summary(pairs)

                for (i in 1:nrow(pairs_summary)) {
                    table$addRow(rowKey=i, values=list(
                        contrast = as.character(pairs_summary[i, "contrast"]),
                        estimate = as.numeric(pairs_summary[i, "estimate"]),
                        se = as.numeric(pairs_summary[i, "SE"]),
                        z = as.numeric(pairs_summary[i, "z.ratio"]),
                        p = as.numeric(pairs_summary[i, "p.value"])
                    ))
                }

            }, error = function(e) {
                # Post-hoc failed - not critical
            })

        },

        .populateDiagnostics = function(data, cluster_var) {

            if (is.null(private$.model)) return()

            table <- self$results$diagnosticsTable

            # Calculate diagnostics
            cluster_sizes <- table(data[[cluster_var]])
            min_cluster <- as.numeric(min(cluster_sizes))
            max_cluster <- as.numeric(max(cluster_sizes))
            median_cluster <- as.numeric(median(cluster_sizes))

            # Get working correlation
            working_corr <- NA
            if (!is.null(private$.model$geese)) {
                if (!is.null(private$.model$geese$alpha)) {
                    working_corr <- as.numeric(private$.model$geese$alpha)
                }
            }

            table$setRow(rowNo=1, values=list(
                diagnostic = as.character("Min Cluster Size"),
                value = as.character(min_cluster)
            ))
            table$setRow(rowNo=2, values=list(
                diagnostic = as.character("Max Cluster Size"),
                value = as.character(max_cluster)
            ))
            table$setRow(rowNo=3, values=list(
                diagnostic = as.character("Median Cluster Size"),
                value = as.character(median_cluster)
            ))
            table$setRow(rowNo=4, values=list(
                diagnostic = as.character("Working Correlation (α)"),
                value = as.character(if (!is.na(working_corr)) sprintf("%.3f", working_corr) else "N/A")
            ))
            # Check convergence (geeglm models may not have a 'converged' field)
            converged <- "Unknown"
            if (!is.null(private$.model$converged)) {
                converged <- if (private$.model$converged) "Yes" else "No"
            } else if (!is.null(private$.model$geese)) {
                # Try to check geese convergence info
                if (!is.null(private$.model$geese$error)) {
                    converged <- if (private$.model$geese$error == 0) "Yes" else "No"
                }
            }

            table$setRow(rowNo=5, values=list(
                diagnostic = as.character("Model Convergence"),
                value = as.character(converged)
            ))

        },

        .plotCorrelation = function(image, ggtheme, theme, ...) {

            if (is.null(private$.model)) return(FALSE)

            tryCatch({

                # Get working correlation structure and alpha
                corstr <- self$options$corstr
                alpha <- NA

                if (!is.null(private$.model$geese) && !is.null(private$.model$geese$alpha)) {
                    alpha <- as.numeric(private$.model$geese$alpha)
                }

                if (is.na(alpha)) {
                    return(FALSE)
                }

                # Create a simple example correlation matrix based on structure
                # Show a 5x5 example matrix for visualization
                n <- 5
                corr_matrix <- matrix(0, n, n)

                if (corstr == "exchangeable") {
                    # All off-diagonal elements = alpha
                    corr_matrix <- matrix(alpha, n, n)
                    diag(corr_matrix) <- 1
                } else if (corstr == "ar1") {
                    # AR(1): corr(i,j) = alpha^|i-j|
                    for (i in 1:n) {
                        for (j in 1:n) {
                            corr_matrix[i, j] <- alpha^abs(i - j)
                        }
                    }
                } else if (corstr == "independence") {
                    # Independence: only diagonal = 1
                    diag(corr_matrix) <- 1
                } else {
                    # For unstructured, just show exchangeable as example
                    corr_matrix <- matrix(alpha, n, n)
                    diag(corr_matrix) <- 1
                }

                # Convert to data frame for plotting
                corr_df <- expand.grid(Var1 = 1:n, Var2 = 1:n)
                corr_df$value <- as.vector(corr_matrix)

                # Create heatmap
                corstr_label <- switch(corstr,
                    "exchangeable" = "Exchangeable",
                    "ar1" = "AR(1)",
                    "independence" = "Independence",
                    "unstructured" = "Unstructured",
                    corstr
                )

                title_text <- sprintf("Working Correlation Structure: %s (α = %.3f)", corstr_label, alpha)

                p <- ggplot2::ggplot(corr_df, ggplot2::aes(x=Var1, y=Var2, fill=value)) +
                    ggplot2::geom_tile(color = "white", linewidth = 1) +
                    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", value)),
                                     color = "black", size = 3.5) +
                    ggplot2::scale_fill_gradient2(
                        low = "#3498db", mid = "white", high = "#e74c3c",
                        midpoint = 0, limits = c(-1, 1),
                        name = "Correlation"
                    ) +
                    ggplot2::labs(
                        title = title_text,
                        subtitle = "(5×5 example matrix showing the correlation structure)",
                        x = "Observation within cluster",
                        y = "Observation within cluster"
                    ) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(hjust = 0.5, size = 12, face = "bold"),
                        plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 9, color = "gray40"),
                        axis.text = ggplot2::element_text(size = 9)
                    ) +
                    ggplot2::scale_x_continuous(breaks = 1:n) +
                    ggplot2::scale_y_continuous(breaks = 1:n)

                print(p)
                TRUE

            }, error = function(e) {
                message("Correlation plot error: ", e$message)
                FALSE
            })

        },

        .addMethodologyNote = function() {

            html <- "<html>
            <head>
            <style>
            .methodology { font-family: sans-serif; padding: 15px; }
            .methodology h4 { color: #2c3e50; margin-top: 10px; }
            .methodology p { line-height: 1.6; }
            .methodology ul { line-height: 1.6; }
            </style>
            </head>
            <body>
            <div class='methodology'>
            <h4>Statistical Methodology</h4>
            <p>Generalized Estimating Equations (GEE) provide population-averaged estimates
            for correlated/clustered data using a working correlation structure.</p>

            <h4>Key Statistical Concepts:</h4>
            <ul>
            <li><strong>Working Correlation</strong>: Assumed structure for within-cluster correlation</li>
            <li><strong>Robust Standard Errors</strong>: Sandwich estimator provides valid inference
            even if correlation structure is misspecified</li>
            <li><strong>Quasi-likelihood</strong>: Uses first two moments without full distributional assumptions</li>
            <li><strong>Population-averaged interpretation</strong>: Coefficients represent marginal effects
            averaged across the population</li>
            </ul>

            <h4>References:</h4>
            <p>Liang, K. Y., & Zeger, S. L. (1986). Longitudinal data analysis using generalized linear models.
            <em>Biometrika</em>, 73(1), 13-22.</p>
            <p>Zeger, S. L., & Liang, K. Y. (1986). Longitudinal data analysis for discrete and continuous outcomes.
            <em>Biometrics</em>, 121-130.</p>
            </div>
            </body>
            </html>"

            self$results$methodologyNote$setContent(html)

        },

        .addInterpretationNote = function() {

            if (is.null(private$.model)) return()

            # Get family for interpretation context
            family_type <- self$options$family

            interp <- switch(family_type,
                "gaussian" = "For continuous outcomes, coefficients represent the average change in the outcome for a one-unit change in the predictor.",
                "binomial" = "For binary outcomes, coefficients are log-odds ratios. Exponentiate (exp(β)) to get odds ratios.",
                "poisson" = "For count outcomes, coefficients are log rate ratios. Exponentiate (exp(β)) to get rate ratios.",
                "gamma" = "For positive continuous outcomes, coefficients represent multiplicative effects on the mean."
            )

            html <- paste0("<html>
            <head>
            <style>
            .interpretation { font-family: sans-serif; padding: 15px; background: #f8f9fa; }
            .interpretation h4 { color: #2c3e50; }
            .interpretation p { line-height: 1.6; }
            .interpretation .important { color: #e74c3c; font-weight: bold; }
            </style>
            </head>
            <body>
            <div class='interpretation'>
            <h4>Interpreting Results</h4>
            <p>", interp, "</p>

            <h4>Coefficient Interpretation:</h4>
            <ul>
            <li><strong>Estimate</strong>: The estimated effect size (scale depends on link function)</li>
            <li><strong>SE</strong>: Standard error (",
            if(self$options$robust_se) "robust sandwich estimator" else "model-based",
            ")</li>
            <li><strong>Wald</strong>: Wald test statistic for testing H₀: β = 0</li>
            <li><strong>p-value</strong>: Probability of observing this effect by chance</li>
            <li><strong>CI</strong>: ", 100*self$options$conf_level, "% confidence interval for the coefficient</li>
            </ul>

            <p class='important'>Important: GEE provides population-averaged (marginal) effects,
            not subject-specific effects. Results describe average effects across the population.</p>

            <h4>Model Selection:</h4>
            <p>Use QIC to compare models with different correlation structures or predictor sets.
            Lower QIC indicates better fit. The working correlation structure does not affect
            coefficient estimates with robust standard errors, but correct specification improves efficiency.</p>
            </div>
            </body>
            </html>")

            self$results$interpretationNote$setContent(html)

        }
    )
)
