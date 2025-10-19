cohenskappaClass <- R6::R6Class(
    "cohenskappaClass",
    inherit = cohenskappaBase,
    private = list(
        .init = function() {
            todo <- glue::glue(
            "
            <br>Welcome to ClinicoPath
            <br><br>
            <b>Cohen's Kappa Agreement Analysis</b>
            <br><br>
            This analysis calculates Cohen's kappa and weighted kappa for assessing agreement between two raters:
            
            <br><br>
            <b>Analysis Steps:</b>
            <ul>
            <li>1. Select your <b>First Rater</b> variable (ratings from rater 1)</li>
            <li>2. Select your <b>Second Rater</b> variable (ratings from rater 2)</li>
            <li>3. Choose <b>Kappa Method</b> (unweighted, linear weights, or quadratic weights)</li>
            <li>4. Configure confidence intervals and other analysis options</li>
            <li>5. Review results and clinical interpretation</li>
            </ul>
            
            <br>
            <b>Key Features:</b>
            <ul>
            <li><b>Cohen's Kappa:</b> Standard measure of inter-rater reliability for categorical data</li>
            <li><b>Weighted Kappa:</b> Accounts for degree of disagreement (for ordinal data)</li>
            <li><b>Confidence Intervals:</b> Asymptotic and bootstrap CI methods available</li>
            <li><b>Agreement Statistics:</b> Overall and category-specific agreement rates</li>
            <li><b>Marginal Homogeneity:</b> Tests if raters have similar rating distributions</li>
            <li><b>Clinical Interpretation:</b> Guidelines for interpreting kappa values in practice</li>
            </ul>
            
            <br>
            <b>When to Use Cohen's Kappa:</b>
            <ul>
            <li>Assessing agreement between two pathologists on diagnoses</li>
            <li>Quality assurance studies in clinical pathology</li>
            <li>Validation of diagnostic criteria or grading systems</li>
            <li>Inter-observer reliability studies</li>
            <li>Comparing human vs automated diagnostic methods</li>
            </ul>
            
            <br>
            <b>Kappa Interpretation (Landis & Koch):</b>
            <ul>
            <li><b>κ < 0.00:</b> Poor agreement</li>
            <li><b>κ 0.00-0.20:</b> Slight agreement</li>
            <li><b>κ 0.21-0.40:</b> Fair agreement</li>
            <li><b>κ 0.41-0.60:</b> Moderate agreement</li>
            <li><b>κ 0.61-0.80:</b> Substantial agreement</li>
            <li><b>κ 0.81-1.00:</b> Almost perfect agreement</li>
            </ul>
            
            <br>
            <b>Required Packages:</b> psych, irr, vcd, DescTools
            <br>
            <b>Recommended minimum:</b> At least 20-30 observations for reliable estimates
            <br><br>
            "
            )
            
            self$results$todo$setContent(todo)
        },

        .run = function() {
            
            # Get options
            rater1 <- self$options$rater1
            rater2 <- self$options$rater2
            
            # Check if variables are selected
            if (is.null(rater1) || is.null(rater2)) {
                return()
            }
            
            # Check required packages
            required_packages <- c("psych", "irr", "vcd", "DescTools")
            missing_packages <- c()
            
            for (pkg in required_packages) {
                if (!requireNamespace(pkg, quietly = TRUE)) {
                    missing_packages <- c(missing_packages, pkg)
                }
            }
            
            if (length(missing_packages) > 0) {
                error_msg <- glue::glue("
                <h3>Error: Required Packages Not Found</h3>
                <p>The following packages are required but not installed:</p>
                <ul>
                {paste0('<li><b>', missing_packages, '</b></li>', collapse = '')}
                </ul>
                <p>Please install them using:</p>
                <code>install.packages(c('{paste(missing_packages, collapse = \"', '\")}'))</code>
                ")
                self$results$summary$setContent(error_msg)
                return()
            }
            
            # Get data
            data <- self$data
            
            if (nrow(data) == 0) return()
            
            # Prepare variables
            rater1_data <- data[[rater1]]
            rater2_data <- data[[rater2]]
            
            # Handle missing data
            if (self$options$missing_treatment == "listwise") {
                complete_cases <- complete.cases(rater1_data, rater2_data)
                if (sum(complete_cases) < nrow(data)) {
                    missing_count <- nrow(data) - sum(complete_cases)
                    warning_msg <- glue::glue("
                    <p><b>Note:</b> {missing_count} observations with missing values were excluded from analysis (listwise deletion).</p>
                    ")
                } else {
                    warning_msg <- ""
                }
                rater1_data <- rater1_data[complete_cases]
                rater2_data <- rater2_data[complete_cases]
            } else {
                warning_msg <- ""
            }
            
            # Convert to factors if needed
            if (is.numeric(rater1_data)) {
                rater1_data <- as.factor(rater1_data)
            }
            if (is.numeric(rater2_data)) {
                rater2_data <- as.factor(rater2_data)
            }
            
            # Check minimum sample size
            n_obs <- length(rater1_data)
            if (n_obs < 10) {
                self$results$summary$setContent("<h3>Warning: Small Sample Size</h3><p>Less than 10 observations. Results may be unreliable.</p>")
                return()
            }
            
            # Check minimum categories
            n_categories <- length(unique(c(levels(rater1_data), levels(rater2_data))))
            min_categories <- self$options$minimum_categories
            if (n_categories < min_categories) {
                self$results$summary$setContent(glue::glue("<h3>Warning: Insufficient Categories</h3><p>Only {n_categories} categories found. Minimum of {min_categories} required.</p>"))
                return()
            }
            
            tryCatch({
                
                # Create contingency table
                confusion_table <- table(rater1_data, rater2_data)
                
                # Calculate various kappa statistics
                kappa_results <- list()
                
                kappa_type <- self$options$kappa_type
                confidence_level <- self$options$confidence_level
                
                # Cohen's unweighted kappa
                if (kappa_type %in% c("cohen", "all")) {
                    cohen_result <- psych::cohen.kappa(confusion_table)
                    kappa_results$cohen <- list(
                        method = "Cohen's Kappa (Unweighted)",
                        kappa = cohen_result$kappa,
                        se = sqrt(cohen_result$var.kappa),
                        z_value = cohen_result$kappa / sqrt(cohen_result$var.kappa),
                        weights = "none"
                    )
                }
                
                # Weighted kappa (linear)
                if (kappa_type %in% c("linear", "all")) {
                    linear_result <- psych::cohen.kappa(confusion_table, w = "equal")
                    kappa_results$linear <- list(
                        method = "Weighted Kappa (Linear)",
                        kappa = linear_result$kappa,
                        se = sqrt(linear_result$var.kappa),
                        z_value = linear_result$kappa / sqrt(linear_result$var.kappa),
                        weights = "linear"
                    )
                }
                
                # Weighted kappa (quadratic)
                if (kappa_type %in% c("quadratic", "all")) {
                    quad_result <- psych::cohen.kappa(confusion_table, w = "squared")
                    kappa_results$quadratic <- list(
                        method = "Weighted Kappa (Quadratic)",
                        kappa = quad_result$kappa,
                        se = sqrt(quad_result$var.kappa),
                        z_value = quad_result$kappa / sqrt(quad_result$var.kappa),
                        weights = "quadratic"
                    )
                }
                
                # Function to interpret kappa values
                interpret_kappa <- function(kappa) {
                    if (is.na(kappa)) return("Cannot be determined")
                    if (kappa < 0.00) return("Poor agreement")
                    if (kappa <= 0.20) return("Slight agreement")
                    if (kappa <= 0.40) return("Fair agreement")
                    if (kappa <= 0.60) return("Moderate agreement")
                    if (kappa <= 0.80) return("Substantial agreement")
                    return("Almost perfect agreement")
                }
                
                # Create kappa table
                kappa_table_data <- data.frame()
                
                for (method_name in names(kappa_results)) {
                    result <- kappa_results[[method_name]]
                    
                    # Calculate confidence intervals
                    z_alpha <- qnorm((1 + confidence_level) / 2)
                    lower_ci <- result$kappa - z_alpha * result$se
                    upper_ci <- result$kappa + z_alpha * result$se
                    
                    # Calculate p-value (test H0: kappa = 0)
                    p_value <- 2 * (1 - pnorm(abs(result$z_value)))
                    
                    kappa_row <- data.frame(
                        method = result$method,
                        kappa = result$kappa,
                        se = result$se,
                        lower_ci = lower_ci,
                        upper_ci = upper_ci,
                        z_value = result$z_value,
                        p_value = p_value,
                        interpretation = interpret_kappa(result$kappa)
                    )
                    
                    kappa_table_data <- rbind(kappa_table_data, kappa_row)
                }
                
                # Populate kappa table row by row
                for (i in 1:nrow(kappa_table_data)) {
                    self$results$kappaTable$addRow(rowKey = i, values = as.list(kappa_table_data[i, ]))
                }
                
                # Agreement statistics
                if (self$options$exact_agreement) {
                    total_obs <- sum(confusion_table)
                    observed_agreement <- sum(diag(confusion_table)) / total_obs
                    
                    # Expected agreement by chance
                    marginal_rater1 <- rowSums(confusion_table) / total_obs
                    marginal_rater2 <- colSums(confusion_table) / total_obs
                    expected_agreement <- sum(marginal_rater1 * marginal_rater2)
                    
                    agreement_data <- data.frame(
                        statistic = c("Total Observations", "Observed Agreement", "Expected Agreement by Chance", "Maximum Possible Kappa"),
                        value = c(total_obs, observed_agreement, expected_agreement, 
                                 ifelse(expected_agreement < 1.0, (observed_agreement - expected_agreement)/(1 - expected_agreement), 1.0)),
                        percentage = c(NA, observed_agreement * 100, expected_agreement * 100, NA),
                        description = c("Total number of paired observations", 
                                       "Proportion of cases where raters agreed",
                                       "Expected agreement if ratings were independent",
                                       "Theoretical maximum kappa given marginal distributions")
                    )
                    
                    # Populate agreement stats table row by row
                    for (i in 1:nrow(agreement_data)) {
                        self$results$agreementStats$addRow(rowKey = i, values = as.list(agreement_data[i, ]))
                    }
                }
                
                # Confusion matrix display
                if (self$options$confusion_matrix) {
                    confusion_df <- as.data.frame.matrix(confusion_table)
                    confusion_df$rater1_category <- rownames(confusion_df)
                    confusion_df$rater2_categories <- apply(confusion_df[, -ncol(confusion_df)], 1, function(x) paste(names(x)[x > 0], collapse = ", "))
                    
                    # Populate confusion matrix table row by row
                    for (i in 1:nrow(confusion_df)) {
                        self$results$confusionMatrix$addRow(rowKey = i, values = as.list(confusion_df[i, ]))
                    }
                }
                
                # Category-specific analysis
                if (self$options$category_analysis) {
                    categories <- rownames(confusion_table)
                    category_data <- data.frame()
                    
                    for (cat in categories) {
                        n_cases <- sum(confusion_table[cat, ]) + sum(confusion_table[, cat]) - confusion_table[cat, cat]
                        agreement_count <- confusion_table[cat, cat]
                        agreement_rate <- ifelse(n_cases > 0, agreement_count / n_cases, 0)
                        
                        category_row <- data.frame(
                            category = cat,
                            n_cases = n_cases,
                            agreement_count = agreement_count,
                            agreement_rate = agreement_rate,
                            kappa_contribution = NA  # Would need more complex calculation
                        )
                        
                        category_data <- rbind(category_data, category_row)
                    }
                    
                    # Populate category stats table row by row
                    for (i in 1:nrow(category_data)) {
                        self$results$categoryStats$addRow(rowKey = i, values = as.list(category_data[i, ]))
                    }
                }
                
                # Marginal homogeneity test
                if (self$options$marginal_homogeneity && nrow(confusion_table) > 1 && ncol(confusion_table) > 1) {
                    # Stuart-Maxwell test for marginal homogeneity
                    tryCatch({
                        marginal_test <- DescTools::StuartMaxwellTest(confusion_table)
                        
                        marginal_data <- data.frame(
                            test = "Stuart-Maxwell Test",
                            statistic = marginal_test$statistic,
                            df = marginal_test$parameter,
                            p_value = marginal_test$p.value,
                            interpretation = ifelse(marginal_test$p.value < 0.05, 
                                                   "Marginal distributions differ significantly", 
                                                   "Marginal distributions are similar")
                        )
                        
                        self$results$marginalTest$addRow(rowKey = 1, values = as.list(marginal_data))
                    }, error = function(e) {
                        # Fallback: McNemar test for 2x2 tables
                        if (nrow(confusion_table) == 2 && ncol(confusion_table) == 2) {
                            mcnemar_test <- mcnemar.test(confusion_table)
                            
                            marginal_data <- data.frame(
                                test = "McNemar's Test",
                                statistic = mcnemar_test$statistic,
                                df = mcnemar_test$parameter,
                                p_value = mcnemar_test$p.value,
                                interpretation = ifelse(mcnemar_test$p.value < 0.05, 
                                                       "Marginal distributions differ significantly", 
                                                       "Marginal distributions are similar")
                            )
                            
                            self$results$marginalTest$addRow(rowKey = 1, values = as.list(marginal_data))
                        }
                    })
                }
                
                # Bootstrap confidence intervals
                if (self$options$ci_method %in% c("bootstrap", "both")) {
                    n_bootstrap <- self$options$bootstrap_samples
                    
                    # Simple bootstrap for Cohen's kappa
                    if ("cohen" %in% names(kappa_results)) {
                        bootstrap_kappas <- numeric(n_bootstrap)
                        
                        for (i in 1:n_bootstrap) {
                            boot_indices <- sample(1:length(rater1_data), replace = TRUE)
                            boot_table <- table(rater1_data[boot_indices], rater2_data[boot_indices])
                            boot_result <- psych::cohen.kappa(boot_table)
                            bootstrap_kappas[i] <- boot_result$kappa
                        }
                        
                        boot_lower <- quantile(bootstrap_kappas, (1 - confidence_level) / 2, na.rm = TRUE)
                        boot_upper <- quantile(bootstrap_kappas, (1 + confidence_level) / 2, na.rm = TRUE)
                        
                        bootstrap_data <- data.frame(
                            method = "Cohen's Kappa (Unweighted)",
                            kappa = kappa_results$cohen$kappa,
                            boot_lower = boot_lower,
                            boot_upper = boot_upper,
                            boot_samples = n_bootstrap
                        )
                        
                        self$results$bootstrapResults$addRow(rowKey = 1, values = as.list(bootstrap_data))
                    }
                }
                
                # Analysis summary
                primary_kappa <- kappa_results[[names(kappa_results)[1]]]
                summary_text <- glue::glue("
                <h3>Cohen's Kappa Analysis Results</h3>
                {warning_msg}
                <p><b>Analysis Summary:</b></p>
                <ul>
                <li>Sample size: {n_obs} paired observations</li>
                <li>Number of categories: {n_categories}</li>
                <li>Primary kappa: {round(primary_kappa$kappa, 3)} ({interpret_kappa(primary_kappa$kappa)})</li>
                <li>Confidence level: {confidence_level * 100}%</li>
                <li>Method: {primary_kappa$method}</li>
                </ul>
                ")
                
                if (n_obs < 30) {
                    summary_text <- paste0(summary_text, "<p><b>Note:</b> Sample size is relatively small (n < 30). Consider increasing sample size for more reliable estimates.</p>")
                }
                
                self$results$summary$setContent(summary_text)
                
                # Store results for plotting
                private$results <- list(
                    confusion_table = confusion_table,
                    kappa_results = kappa_results,
                    rater1_data = rater1_data,
                    rater2_data = rater2_data,
                    n_obs = n_obs,
                    categories = unique(c(levels(rater1_data), levels(rater2_data)))
                )
                
                # Clinical interpretation guide
                if (self$options$interpretation_guide) {
                    interpretation_text <- "
                    <h3>Clinical Interpretation Guidelines</h3>
                    
                    <h4>Kappa Value Interpretation (Landis & Koch, 1977)</h4>
                    <table style='border-collapse: collapse; width: 100%;'>
                    <tr style='border: 1px solid #ddd; background-color: #f2f2f2;'>
                    <th style='border: 1px solid #ddd; padding: 8px;'>Kappa Range</th>
                    <th style='border: 1px solid #ddd; padding: 8px;'>Interpretation</th>
                    <th style='border: 1px solid #ddd; padding: 8px;'>Clinical Meaning</th>
                    </tr>
                    <tr style='border: 1px solid #ddd;'>
                    <td style='border: 1px solid #ddd; padding: 8px;'>< 0.00</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Poor agreement</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Agreement worse than chance; systematic disagreement</td>
                    </tr>
                    <tr style='border: 1px solid #ddd; background-color: #f9f9f9;'>
                    <td style='border: 1px solid #ddd; padding: 8px;'>0.00 - 0.20</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Slight agreement</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Minimal agreement beyond chance; unreliable for clinical use</td>
                    </tr>
                    <tr style='border: 1px solid #ddd;'>
                    <td style='border: 1px solid #ddd; padding: 8px;'>0.21 - 0.40</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Fair agreement</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Some agreement but substantial disagreement; needs improvement</td>
                    </tr>
                    <tr style='border: 1px solid #ddd; background-color: #f9f9f9;'>
                    <td style='border: 1px solid #ddd; padding: 8px;'>0.41 - 0.60</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Moderate agreement</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Acceptable agreement for some applications</td>
                    </tr>
                    <tr style='border: 1px solid #ddd;'>
                    <td style='border: 1px solid #ddd; padding: 8px;'>0.61 - 0.80</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Substantial agreement</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Good agreement suitable for most clinical applications</td>
                    </tr>
                    <tr style='border: 1px solid #ddd; background-color: #f9f9f9;'>
                    <td style='border: 1px solid #ddd; padding: 8px;'>0.81 - 1.00</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Almost perfect agreement</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Excellent agreement suitable for critical applications</td>
                    </tr>
                    </table>
                    
                    <h4>Choosing the Right Kappa</h4>
                    <ul>
                    <li><b>Unweighted Cohen's Kappa:</b> Use for nominal (unordered) categories where all disagreements are equally important</li>
                    <li><b>Linear Weighted Kappa:</b> Use for ordinal data where disagreements increase linearly with category distance</li>
                    <li><b>Quadratic Weighted Kappa:</b> Use for ordinal data where distant disagreements are much more serious than adjacent ones</li>
                    </ul>
                    
                    <h4>Clinical Applications</h4>
                    <ul>
                    <li><b>Diagnostic Agreement:</b> κ ≥ 0.60 typically acceptable for clinical diagnosis</li>
                    <li><b>Grading Systems:</b> κ ≥ 0.80 preferred for critical grading decisions</li>
                    <li><b>Quality Assurance:</b> κ ≥ 0.70 often required for certification purposes</li>
                    <li><b>Research Studies:</b> κ ≥ 0.60 generally acceptable, κ ≥ 0.80 preferred</li>
                    </ul>
                    
                    <h4>Sample Size Considerations</h4>
                    <ul>
                    <li>Minimum 30 paired observations for stable estimates</li>
                    <li>At least 10 observations per category for reliable category-specific analysis</li>
                    <li>Bootstrap confidence intervals recommended for small samples</li>
                    </ul>
                    "
                    
                    self$results$interpretationGuide$setContent(interpretation_text)
                }
                
                # Technical notes
                technical_text <- "
                <h3>Technical Notes and Assumptions</h3>
                
                <h4>Cohen's Kappa Methodology</h4>
                <p>Cohen's kappa measures inter-rater agreement for categorical items, correcting for agreement that could occur by chance alone:</p>
                <p><b>κ = (P<sub>o</sub> - P<sub>e</sub>) / (1 - P<sub>e</sub>)</b></p>
                <p>Where P<sub>o</sub> is observed agreement and P<sub>e</sub> is expected agreement by chance.</p>
                
                <h4>Weighted Kappa</h4>
                <p>For ordinal data, weighted kappa accounts for the degree of disagreement:</p>
                <ul>
                <li><b>Linear weights:</b> w<sub>ij</sub> = 1 - |i-j|/(k-1)</li>
                <li><b>Quadratic weights:</b> w<sub>ij</sub> = 1 - (i-j)²/(k-1)²</li>
                </ul>
                
                <h4>Assumptions</h4>
                <ul>
                <li><b>Independence:</b> Each observation should be independent</li>
                <li><b>Same Categories:</b> Both raters use the same set of categories</li>
                <li><b>Fixed Marginals:</b> Raters' propensities to use categories should be relatively stable</li>
                <li><b>Random Sampling:</b> Sample should be representative of the population</li>
                </ul>
                
                <h4>Confidence Intervals</h4>
                <ul>
                <li><b>Asymptotic:</b> Based on large-sample normal approximation (Fleiss et al., 1979)</li>
                <li><b>Bootstrap:</b> Non-parametric method that doesn't assume normality</li>
                </ul>
                
                <h4>Marginal Homogeneity Testing</h4>
                <ul>
                <li><b>Stuart-Maxwell Test:</b> Tests if marginal distributions are equal (generalization of McNemar's test)</li>
                <li><b>Interpretation:</b> Significant result suggests systematic bias between raters</li>
                </ul>
                
                <h4>Limitations</h4>
                <ul>
                <li>Kappa can be paradoxically low when base rates are extreme</li>
                <li>Sensitive to the number and distribution of categories</li>
                <li>Does not indicate the direction of disagreement</li>
                <li>May be inappropriate when categories are not mutually exclusive</li>
                </ul>
                "
                
                self$results$technicalNotes$setContent(technical_text)

                # Multi-rater analysis (if 3+ raters selected)
                if (!is.null(self$options$rater3)) {
                    private$.performMultiRaterAnalysis()
                }

            }, error = function(e) {
                error_msg <- glue::glue("<h3>Error in Analysis</h3><p>An error occurred during analysis: {e$message}</p><p>Please check your data and parameter settings.</p>")
                self$results$summary$setContent(error_msg)
                return()
            })
        },

        # ===== Multi-Rater Agreement (Fleiss' Kappa) =====
        # Private helper functions for multi-rater analysis

        .performMultiRaterAnalysis = function() {
            # Get all rater variables
            raters <- c(self$options$rater1, self$options$rater2)
            if (!is.null(self$options$rater3)) raters <- c(raters, self$options$rater3)
            if (!is.null(self$options$rater4)) raters <- c(raters, self$options$rater4)
            if (!is.null(self$options$rater5)) raters <- c(raters, self$options$rater5)

            n_raters <- length(raters)

            # Only proceed if we have 3+ raters
            if (n_raters < 3) {
                return(NULL)
            }

            # Check for irr package
            if (!requireNamespace("irr", quietly = TRUE)) {
                error_msg <- glue::glue("
                <h3>Error: Package Required</h3>
                <p>The 'irr' package is required for multi-rater analysis but is not installed.</p>
                <p>Please install it using:</p>
                <code>install.packages('irr')</code>
                ")
                self$results$multiRaterSummary$setContent(error_msg)
                return(NULL)
            }

            # Get data
            data <- self$data

            # Extract rater data
            rater_data_list <- list()
            for (i in 1:n_raters) {
                rater_var <- raters[i]
                rater_col <- data[[rater_var]]

                # Convert to factor if numeric
                if (is.numeric(rater_col)) {
                    rater_col <- as.factor(rater_col)
                }

                rater_data_list[[i]] <- rater_col
            }

            # Combine into matrix (subjects as rows, raters as columns)
            rater_matrix <- do.call(cbind, rater_data_list)
            colnames(rater_matrix) <- paste0("Rater", 1:n_raters)

            # Handle missing data
            if (self$options$missing_treatment == "listwise") {
                complete_cases <- complete.cases(rater_matrix)
                rater_matrix <- rater_matrix[complete_cases, , drop = FALSE]
            }

            n_subjects <- nrow(rater_matrix)

            # Check if we have enough data
            if (n_subjects < 3) {
                error_msg <- "<h3>Error: Insufficient Data</h3><p>Multi-rater analysis requires at least 3 complete observations.</p>"
                self$results$multiRaterSummary$setContent(error_msg)
                return(NULL)
            }

            # Determine method
            method <- self$options$multi_rater_method

            # Auto-select method based on missingness
            if (method == "auto") {
                has_missing <- any(is.na(rater_matrix))
                method <- if (has_missing) "light" else "fleiss"
            }

            # Calculate multi-rater kappa
            result <- private$.calculateMultiRaterKappa(rater_matrix, method, n_raters, n_subjects)

            # Store for pairwise analysis if requested
            if (self$options$show_pairwise_kappa) {
                private$.calculatePairwiseKappa(rater_data_list, raters)
            }

            return(result)
        },

        .calculateMultiRaterKappa = function(rater_matrix, method, n_raters, n_subjects) {
            tryCatch({
                if (method == "fleiss") {
                    # Fleiss' Kappa - all raters evaluate all subjects
                    fleiss_result <- irr::kappam.fleiss(rater_matrix)

                    kappa_value <- fleiss_result$value
                    p_value <- fleiss_result$p.value

                    # Approximate SE (Fleiss method)
                    # SE calculation for Fleiss kappa is complex; using approximation
                    se_kappa <- sqrt((1 - kappa_value)^2 / (n_subjects * (n_raters - 1)))
                    z_value <- kappa_value / se_kappa

                    method_name <- "Fleiss' Kappa"

                } else if (method == "light") {
                    # Light's Kappa - average of pairwise kappas
                    light_result <- irr::kappam.light(rater_matrix)

                    kappa_value <- light_result$value
                    p_value <- light_result$p.value

                    # Approximate SE
                    se_kappa <- sqrt((1 - kappa_value)^2 / (n_subjects * (n_raters - 1)))
                    z_value <- kappa_value / se_kappa

                    method_name <- "Light's Kappa (Average Pairwise)"
                }

                # Interpret kappa
                interpretation <- private$.interpretKappa(kappa_value)

                # Populate multi-rater table
                self$results$multiRaterKappa$addRow(rowKey = 1, values = list(
                    method = method_name,
                    kappa = kappa_value,
                    se = se_kappa,
                    z_value = z_value,
                    p_value = p_value,
                    n_raters = n_raters,
                    n_subjects = n_subjects,
                    interpretation = interpretation
                ))

                # Create summary
                summary_html <- glue::glue("
                <h3>Multi-Rater Agreement Analysis</h3>
                <p><b>Method:</b> {method_name}</p>
                <p><b>Number of Raters:</b> {n_raters}</p>
                <p><b>Number of Subjects:</b> {n_subjects}</p>
                <p><b>Kappa Value:</b> {round(kappa_value, 3)} ({interpretation})</p>

                <h4>Interpretation:</h4>
                <ul>
                <li><b>Fleiss' Kappa:</b> Generalization of Cohen's kappa for multiple raters. Measures overall agreement among all raters simultaneously.</li>
                <li><b>Light's Kappa:</b> Average of all pairwise Cohen's kappas. More robust to missing data and different rating patterns.</li>
                </ul>

                <h4>When to Use:</h4>
                <ul>
                <li><b>Fleiss' Kappa:</b> When all raters rate all subjects; no missing data; raters are interchangeable</li>
                <li><b>Light's Kappa:</b> When missing data present; when raters may have different rating patterns; provides conservative estimate</li>
                </ul>

                <p><b>Statistical Significance:</b> p = {sprintf('%.4f', p_value)}</p>
                <p>{ifelse(p_value < 0.05, '✓ Agreement is significantly better than chance', '✗ Agreement not significantly different from chance')}</p>
                ")

                self$results$multiRaterSummary$setContent(summary_html)

                return(list(success = TRUE, kappa = kappa_value, method = method_name))

            }, error = function(e) {
                error_msg <- glue::glue("
                <h3>Error in Multi-Rater Analysis</h3>
                <p>An error occurred: {e$message}</p>
                <p>Please check that:</p>
                <ul>
                <li>All raters use the same categories</li>
                <li>Data format is correct (factors or numeric)</li>
                <li>Sufficient complete observations (n ≥ 3)</li>
                </ul>
                ")
                self$results$multiRaterSummary$setContent(error_msg)
                return(list(success = FALSE))
            })
        },

        .calculatePairwiseKappa = function(rater_data_list, rater_names) {
            n_raters <- length(rater_data_list)

            # Generate all pairs
            pairs <- combn(n_raters, 2)

            for (i in 1:ncol(pairs)) {
                rater_i <- pairs[1, i]
                rater_j <- pairs[2, i]

                # Get data for this pair
                rater_i_data <- rater_data_list[[rater_i]]
                rater_j_data <- rater_data_list[[rater_j]]

                # Remove missing for this pair
                complete_idx <- complete.cases(rater_i_data, rater_j_data)
                r_i <- rater_i_data[complete_idx]
                r_j <- rater_j_data[complete_idx]

                if (length(r_i) < 3) {
                    next  # Skip if insufficient data
                }

                # Calculate Cohen's kappa for this pair
                tryCatch({
                    confusion_table <- table(r_i, r_j)
                    cohen_result <- psych::cohen.kappa(confusion_table)

                    kappa_val <- cohen_result$kappa
                    se_val <- sqrt(cohen_result$var.kappa)
                    z_val <- kappa_val / se_val
                    p_val <- 2 * (1 - pnorm(abs(z_val)))

                    pair_name <- paste(rater_names[rater_i], "vs", rater_names[rater_j])

                    self$results$pairwiseKappaMatrix$addRow(rowKey = i, values = list(
                        rater_pair = pair_name,
                        kappa = kappa_val,
                        se = se_val,
                        p_value = p_val
                    ))
                }, error = function(e) {
                    # Skip pairs with errors
                })
            }
        },

        .interpretKappa = function(kappa) {
            if (is.na(kappa)) return("Cannot be determined")
            if (kappa < 0.00) return("Poor agreement")
            if (kappa <= 0.20) return("Slight agreement")
            if (kappa <= 0.40) return("Fair agreement")
            if (kappa <= 0.60) return("Moderate agreement")
            if (kappa <= 0.80) return("Substantial agreement")
            return("Almost perfect agreement")
        },

        # Plotting functions
        .plotAgreement = function(image, ...) {
            if (!exists("results", envir = private) || is.null(private$results)) return()
            
            confusion_table <- private$results$confusion_table
            categories <- private$results$categories
            
            # Create agreement plot
            total_obs <- sum(confusion_table)
            observed_props <- confusion_table / total_obs
            
            # Expected proportions under independence
            marginal_rater1 <- rowSums(confusion_table) / total_obs
            marginal_rater2 <- colSums(confusion_table) / total_obs
            expected_props <- outer(marginal_rater1, marginal_rater2)
            
            # Create data for plotting
            plot_data <- expand.grid(Rater1 = categories, Rater2 = categories)
            plot_data$Observed <- as.vector(observed_props)
            plot_data$Expected <- as.vector(expected_props)
            plot_data$Difference <- plot_data$Observed - plot_data$Expected
            plot_data$Agreement <- plot_data$Rater1 == plot_data$Rater2
            
            p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Expected, y = Observed)) +
                ggplot2::geom_point(ggplot2::aes(color = Agreement, size = abs(Difference)), alpha = 0.7) +
                ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", alpha = 0.6) +
                ggplot2::scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "darkgreen")) +
                ggplot2::scale_size_continuous(range = c(2, 8)) +
                ggplot2::labs(
                    title = "Agreement Analysis: Observed vs Expected Proportions",
                    subtitle = "Points above the diagonal line indicate better than chance agreement",
                    x = "Expected Proportion (under independence)",
                    y = "Observed Proportion",
                    color = "Category Match",
                    size = "Absolute Difference"
                ) +
                ggplot2::theme_minimal() +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
                    plot.subtitle = ggplot2::element_text(hjust = 0.5)
                )
            
            # Add category labels for diagonal points
            diagonal_data <- plot_data[plot_data$Agreement, ]
            if (nrow(diagonal_data) > 0) {
                p <- p + ggrepel::geom_text_repel(
                    data = diagonal_data,
                    ggplot2::aes(label = Rater1),
                    size = 3,
                    alpha = 0.8
                )
            }
            
            print(p)
            return(TRUE)
        },

        .plotConfusionHeatmap = function(image, ...) {
            if (!exists("results", envir = private) || is.null(private$results)) return()
            
            confusion_table <- private$results$confusion_table
            
            # Convert to data frame for ggplot
            confusion_df <- as.data.frame(confusion_table)
            names(confusion_df) <- c("Rater1", "Rater2", "Count")
            
            # Calculate proportions for better visualization
            confusion_df$Proportion <- confusion_df$Count / sum(confusion_df$Count)
            confusion_df$Percentage <- confusion_df$Proportion * 100
            
            # Create heatmap
            p <- ggplot2::ggplot(confusion_df, ggplot2::aes(x = Rater2, y = Rater1, fill = Count)) +
                ggplot2::geom_tile(color = "white", size = 0.5) +
                ggplot2::geom_text(ggplot2::aes(label = paste0(Count, "\n(", round(Percentage, 1), "%)")), 
                                  color = "white", fontface = "bold", size = 3) +
                ggplot2::scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Count") +
                ggplot2::labs(
                    title = "Confusion Matrix: Inter-Rater Agreement",
                    subtitle = "Diagonal cells represent perfect agreement",
                    x = "Rater 2",
                    y = "Rater 1"
                ) +
                ggplot2::theme_minimal() +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
                    plot.subtitle = ggplot2::element_text(hjust = 0.5),
                    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
                )
            
            # Highlight diagonal (perfect agreement) with border
            diagonal_indices <- which(confusion_df$Rater1 == confusion_df$Rater2)
            if (length(diagonal_indices) > 0) {
                diagonal_df <- confusion_df[diagonal_indices, ]
                p <- p + ggplot2::geom_tile(data = diagonal_df, 
                                          ggplot2::aes(x = Rater2, y = Rater1), 
                                          fill = NA, color = "red", size = 2)
            }
            
            print(p)
            return(TRUE)
        },

        results = NULL
    )
)