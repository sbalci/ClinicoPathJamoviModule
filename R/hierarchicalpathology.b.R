# This file is a generated template, your changes will not be overwritten

#' @import jmvcore
#' @import R6
#' @import lme4
#' @import nlme
#' @import performance
#' @importFrom glmmTMB glmmTMB nbinom2
#' @importFrom stats formula terms model.matrix model.frame na.omit
#' @importFrom utils capture.output
#' @export


hierarchicalpathologyClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "hierarchicalpathologyClass",
    inherit = hierarchicalpathologyBase,
    private = list(
        .init = function() {

            # Initialize results tables with proper structure
            private$.initDescriptiveTable()
            private$.initVarianceTable()
            private$.initModelTable()
            private$.initRandomEffectsTable()
            private$.initICCTable()

            # Set up instructions
            private$.populateInstructionsTable()

        },

        .run = function() {

            # Early exit if no data
            if (is.null(self$data) || nrow(self$data) == 0)
                return()

            # Get variables
            dependent <- self$options$dependent
            level_3 <- self$options$level_3
            level_2 <- self$options$level_2
            level_1 <- self$options$level_1
            covariates <- self$options$covariates

            # Validate inputs
            if (is.null(dependent) || length(dependent) == 0) {
                self$results$instructions$setContent("Please specify a dependent variable to begin analysis.")
                return()
            }

            if (is.null(level_3) || length(level_3) == 0) {
                self$results$instructions$setContent("Please specify at least the highest level grouping variable (e.g., Patient ID).")
                return()
            }

            # Build the analysis
            private$.buildAnalysis()

        },

        .buildAnalysis = function() {

            # Prepare data for analysis
            analysisData <- private$.prepareData()

            if (is.null(analysisData)) {
                return()
            }

            # Perform descriptive analysis
            private$.descriptiveAnalysis(analysisData)

            # Fit hierarchical models
            private$.fitHierarchicalModels(analysisData)

            # Calculate variance components and ICC
            private$.calculateVarianceComponents(analysisData)

            # Generate interpretation
            private$.generateInterpretation()

        },

        .prepareData = function() {

            # Get variables
            dependent <- self$options$dependent
            level_3 <- self$options$level_3
            level_2 <- self$options$level_2
            level_1 <- self$options$level_1
            covariates <- self$options$covariates

            # Get data
            data <- self$data

            # Remove missing values
            vars_to_check <- c(dependent, level_3, level_2, level_1, covariates)
            vars_to_check <- vars_to_check[!sapply(vars_to_check, is.null)]

            if (length(vars_to_check) == 0) {
                return(NULL)
            }

            # Create analysis dataset
            analysisData <- data[, vars_to_check, drop = FALSE]
            analysisData <- na.omit(analysisData)

            if (nrow(analysisData) < 10) {
                self$results$instructions$setContent("Insufficient data for hierarchical analysis. Need at least 10 complete observations.")
                return(NULL)
            }

            # Ensure grouping variables are factors
            if (!is.null(level_3)) {
                analysisData[[level_3]] <- as.factor(analysisData[[level_3]])
            }
            if (!is.null(level_2)) {
                analysisData[[level_2]] <- as.factor(analysisData[[level_2]])
            }
            if (!is.null(level_1)) {
                analysisData[[level_1]] <- as.factor(analysisData[[level_1]])
            }

            return(analysisData)
        },

        .descriptiveAnalysis = function(data) {

            if (!self$options$descriptives) return()

            dependent <- self$options$dependent
            level_3 <- self$options$level_3
            level_2 <- self$options$level_2
            level_1 <- self$options$level_1

            table <- self$results$descriptives

            # Overall statistics
            overall_stats <- private$.calculateDescriptiveStats(data[[dependent]])

            row <- list(
                level = "Overall",
                n_obs = nrow(data),
                n_groups = NA,
                mean = overall_stats$mean,
                sd = overall_stats$sd,
                min_val = overall_stats$min,
                max_val = overall_stats$max,
                median = overall_stats$median,
                iqr = overall_stats$iqr
            )
            table$addRow(rowKey = "overall", values = row)

            # Level 3 statistics
            if (!is.null(level_3)) {
                level3_groups <- unique(data[[level_3]])
                level3_stats <- private$.calculateGroupStats(data, dependent, level_3)

                row <- list(
                    level = paste("Level 3:", level_3),
                    n_obs = nrow(data),
                    n_groups = length(level3_groups),
                    mean = level3_stats$mean,
                    sd = level3_stats$sd,
                    min_val = level3_stats$min,
                    max_val = level3_stats$max,
                    median = level3_stats$median,
                    iqr = level3_stats$iqr
                )
                table$addRow(rowKey = "level3", values = row)
            }

            # Level 2 statistics
            if (!is.null(level_2)) {
                level2_groups <- unique(data[[level_2]])
                level2_stats <- private$.calculateGroupStats(data, dependent, level_2)

                row <- list(
                    level = paste("Level 2:", level_2),
                    n_obs = nrow(data),
                    n_groups = length(level2_groups),
                    mean = level2_stats$mean,
                    sd = level2_stats$sd,
                    min_val = level2_stats$min,
                    max_val = level2_stats$max,
                    median = level2_stats$median,
                    iqr = level2_stats$iqr
                )
                table$addRow(rowKey = "level2", values = row)
            }

            # Level 1 statistics
            if (!is.null(level_1)) {
                level1_groups <- unique(data[[level_1]])
                level1_stats <- private$.calculateGroupStats(data, dependent, level_1)

                row <- list(
                    level = paste("Level 1:", level_1),
                    n_obs = nrow(data),
                    n_groups = length(level1_groups),
                    mean = level1_stats$mean,
                    sd = level1_stats$sd,
                    min_val = level1_stats$min,
                    max_val = level1_stats$max,
                    median = level1_stats$median,
                    iqr = level1_stats$iqr
                )
                table$addRow(rowKey = "level1", values = row)
            }
        },

        .calculateDescriptiveStats = function(x) {
            x <- x[!is.na(x)]
            if (length(x) == 0) {
                return(list(mean = NA, sd = NA, min = NA, max = NA, median = NA, iqr = NA))
            }

            list(
                mean = round(mean(x), 3),
                sd = round(sd(x), 3),
                min = round(min(x), 3),
                max = round(max(x), 3),
                median = round(median(x), 3),
                iqr = round(IQR(x), 3)
            )
        },

        .calculateGroupStats = function(data, dependent, grouping_var) {
            # Calculate mean statistics across groups
            group_means <- tapply(data[[dependent]], data[[grouping_var]], function(x) {
                x <- x[!is.na(x)]
                if (length(x) == 0) return(NA)
                mean(x)
            })
            group_means <- group_means[!is.na(group_means)]

            if (length(group_means) == 0) {
                return(list(mean = NA, sd = NA, min = NA, max = NA, median = NA, iqr = NA))
            }

            list(
                mean = round(mean(group_means), 3),
                sd = round(sd(group_means), 3),
                min = round(min(group_means), 3),
                max = round(max(group_means), 3),
                median = round(median(group_means), 3),
                iqr = round(IQR(group_means), 3)
            )
        },

        .fitHierarchicalModels = function(data) {

            dependent <- self$options$dependent
            level_3 <- self$options$level_3
            level_2 <- self$options$level_2
            level_1 <- self$options$level_1
            covariates <- self$options$covariates
            model_type <- self$options$model_type

            # Build formula based on available levels
            formula_parts <- list()
            random_parts <- list()

            # Fixed effects
            if (!is.null(covariates) && length(covariates) > 0) {
                formula_parts <- c(formula_parts, paste(covariates, collapse = " + "))
            } else {
                formula_parts <- c(formula_parts, "1")
            }

            # Random effects structure
            if (!is.null(level_1) && !is.null(level_2) && !is.null(level_3)) {
                # Three-level model: (1 | level_3/level_2/level_1)
                random_parts <- c(random_parts, paste("(1 |", level_3, "/", level_2, "/", level_1, ")"))
            } else if (!is.null(level_2) && !is.null(level_3)) {
                # Two-level model: (1 | level_3/level_2)
                random_parts <- c(random_parts, paste("(1 |", level_3, "/", level_2, ")"))
            } else if (!is.null(level_3)) {
                # Simple random intercept: (1 | level_3)
                random_parts <- c(random_parts, paste("(1 |", level_3, ")"))
            }

            if (length(random_parts) == 0) {
                self$results$instructions$setContent("At least one grouping variable is required for hierarchical modeling.")
                return()
            }

            # Complete formula
            formula_str <- paste(dependent, "~", paste(formula_parts, collapse = " + "), "+",
                               paste(random_parts, collapse = " + "))

            # Fit model based on type
            tryCatch({
                if (model_type == "linear") {
                    model <- lme4::lmer(formula(formula_str), data = data)
                } else if (model_type == "logistic") {
                    model <- lme4::glmer(formula(formula_str), data = data, family = binomial)
                } else if (model_type == "poisson") {
                    model <- lme4::glmer(formula(formula_str), data = data, family = poisson)
                } else if (model_type == "negative_binomial") {
                    # Use glmmTMB for negative binomial
                    model <- glmmTMB::glmmTMB(formula(formula_str), data = data, family = glmmTMB::nbinom2)
                } else {
                    model <- lme4::lmer(formula(formula_str), data = data)
                }

                # Store model for later use
                private$.model <- model

                # Populate model results table
                private$.populateModelTable(model)

                # Populate random effects table
                private$.populateRandomEffectsTable(model)

            }, error = function(e) {
                self$results$instructions$setContent(paste("Model fitting failed:", e$message))
            })
        },

        .calculateVarianceComponents = function(data) {

            if (is.null(private$.model)) return()

            model <- private$.model
            model_type <- self$options$model_type

            tryCatch({

                if (model_type == "linear") {
                    # For linear mixed models
                    var_comp <- as.data.frame(VarCorr(model))

                    # Calculate ICCs
                    total_var <- sum(var_comp$vcov, na.rm = TRUE)

                    table <- self$results$variancecomponents

                    for (i in 1:nrow(var_comp)) {

                        component <- var_comp$grp[i]
                        if (is.na(component)) component <- "Residual"

                        variance <- var_comp$vcov[i]
                        prop_var <- variance / total_var

                        row <- list(
                            component = component,
                            variance = round(variance, 4),
                            std_dev = round(sqrt(variance), 4),
                            proportion = round(prop_var, 4),
                            percentage = round(prop_var * 100, 2)
                        )

                        table$addRow(rowKey = paste0("comp_", i), values = row)
                    }

                    # Calculate ICCs
                    private$.calculateICC(var_comp, total_var)

                } else {
                    # For GLMMs, variance components are more complex
                    self$results$variancecomponents$addRow(
                        rowKey = "note",
                        values = list(
                            component = "GLMM Variance",
                            variance = NA,
                            std_dev = NA,
                            proportion = NA,
                            percentage = "See Random Effects table for GLMM variance components"
                        )
                    )
                }

            }, error = function(e) {
                self$results$instructions$setContent(paste("Variance component calculation failed:", e$message))
            })
        },

        .calculateICC = function(var_comp, total_var) {

            level_3 <- self$options$level_3
            level_2 <- self$options$level_2
            level_1 <- self$options$level_1

            table <- self$results$icc

            # Simple ICC calculations based on available levels
            if (!is.null(level_3)) {
                # Find variance for level 3
                level3_var <- var_comp$vcov[var_comp$grp == level_3]
                if (length(level3_var) > 0) {
                    icc_level3 <- level3_var / total_var

                    table$addRow(
                        rowKey = "icc3",
                        values = list(
                            level = paste("Level 3 ICC:", level_3),
                            icc_value = round(icc_level3, 4),
                            ci_lower = NA,
                            ci_upper = NA,
                            interpretation = private$.interpretICC(icc_level3)
                        )
                    )
                }
            }

            if (!is.null(level_2)) {
                # Find variance for level 2
                level2_var <- var_comp$vcov[var_comp$grp == level_2]
                if (length(level2_var) > 0) {
                    icc_level2 <- level2_var / total_var

                    table$addRow(
                        rowKey = "icc2",
                        values = list(
                            level = paste("Level 2 ICC:", level_2),
                            icc_value = round(icc_level2, 4),
                            ci_lower = NA,
                            ci_upper = NA,
                            interpretation = private$.interpretICC(icc_level2)
                        )
                    )
                }
            }

            if (!is.null(level_1)) {
                # Find variance for level 1
                level1_var <- var_comp$vcov[var_comp$grp == level_1]
                if (length(level1_var) > 0) {
                    icc_level1 <- level1_var / total_var

                    table$addRow(
                        rowKey = "icc1",
                        values = list(
                            level = paste("Level 1 ICC:", level_1),
                            icc_value = round(icc_level1, 4),
                            ci_lower = NA,
                            ci_upper = NA,
                            interpretation = private$.interpretICC(icc_level1)
                        )
                    )
                }
            }
        },

        .interpretICC = function(icc) {
            if (is.na(icc)) return("Cannot calculate")
            if (icc < 0.05) return("Poor reliability")
            if (icc < 0.40) return("Fair reliability")
            if (icc < 0.60) return("Moderate reliability")
            if (icc < 0.75) return("Good reliability")
            return("Excellent reliability")
        },

        .populateModelTable = function(model) {

            table <- self$results$modelresults

            # Model summary statistics
            if (inherits(model, "lmerMod")) {
                # Linear mixed model
                model_summary <- summary(model)

                # Fixed effects
                fixed_effects <- model_summary$coefficients

                for (i in 1:nrow(fixed_effects)) {
                    term <- rownames(fixed_effects)[i]
                    estimate <- fixed_effects[i, "Estimate"]
                    se <- fixed_effects[i, "Std. Error"]
                    t_value <- fixed_effects[i, "t value"]

                    row <- list(
                        term = term,
                        estimate = round(estimate, 4),
                        std_error = round(se, 4),
                        statistic = round(t_value, 4),
                        p_value = NA,  # t-tests in lmer don't have p-values by default
                        ci_lower = round(estimate - 1.96 * se, 4),
                        ci_upper = round(estimate + 1.96 * se, 4)
                    )

                    table$addRow(rowKey = paste0("fixed_", i), values = row)
                }

            } else if (inherits(model, "glmerMod")) {
                # Generalized linear mixed model
                model_summary <- summary(model)
                fixed_effects <- model_summary$coefficients

                for (i in 1:nrow(fixed_effects)) {
                    term <- rownames(fixed_effects)[i]
                    estimate <- fixed_effects[i, "Estimate"]
                    se <- fixed_effects[i, "Std. Error"]
                    z_value <- fixed_effects[i, "z value"]
                    p_value <- fixed_effects[i, "Pr(>|z|)"]

                    row <- list(
                        term = term,
                        estimate = round(estimate, 4),
                        std_error = round(se, 4),
                        statistic = round(z_value, 4),
                        p_value = round(p_value, 4),
                        ci_lower = round(estimate - 1.96 * se, 4),
                        ci_upper = round(estimate + 1.96 * se, 4)
                    )

                    table$addRow(rowKey = paste0("fixed_", i), values = row)
                }
            }
        },

        .populateRandomEffectsTable = function(model) {

            table <- self$results$randomeffects

            # Random effects summary
            random_summary <- VarCorr(model)

            if (inherits(model, "lmerMod")) {
                # Linear mixed model random effects
                for (i in 1:length(random_summary)) {
                    group_name <- names(random_summary)[i]
                    variance <- attr(random_summary[[i]], "stddev")^2
                    std_dev <- attr(random_summary[[i]], "stddev")

                    row <- list(
                        group = group_name,
                        effect = "(Intercept)",
                        variance = round(variance, 4),
                        std_dev = round(std_dev, 4),
                        correlation = NA
                    )

                    table$addRow(rowKey = paste0("random_", i), values = row)
                }

                # Residual variance
                residual_var <- attr(random_summary, "sc")^2
                residual_sd <- attr(random_summary, "sc")

                table$addRow(
                    rowKey = "residual",
                    values = list(
                        group = "Residual",
                        effect = "",
                        variance = round(residual_var, 4),
                        std_dev = round(residual_sd, 4),
                        correlation = NA
                    )
                )
            }
        },

        .generateInterpretation = function() {

            # Generate clinical interpretation
            dependent <- self$options$dependent
            level_3 <- self$options$level_3
            level_2 <- self$options$level_2
            level_1 <- self$options$level_1
            model_type <- self$options$model_type

            html <- ""

            html <- paste0(html, "<h3>Hierarchical Analysis Interpretation</h3>")

            html <- paste0(html, "<h4>Model Structure</h4>")
            html <- paste0(html, "<p>This analysis examines <strong>", dependent, "</strong> using a ")

            if (model_type == "linear") {
                html <- paste0(html, "linear mixed-effects model")
            } else if (model_type == "logistic") {
                html <- paste0(html, "logistic mixed-effects model")
            } else if (model_type == "poisson") {
                html <- paste0(html, "Poisson mixed-effects model")
            } else {
                html <- paste0(html, "negative binomial mixed-effects model")
            }

            html <- paste0(html, " with hierarchical structure:</p>")
            html <- paste0(html, "<ul>")

            if (!is.null(level_3)) {
                html <- paste0(html, "<li><strong>Level 3:</strong> ", level_3, " (highest level grouping)</li>")
            }
            if (!is.null(level_2)) {
                html <- paste0(html, "<li><strong>Level 2:</strong> ", level_2, " (intermediate grouping)</li>")
            }
            if (!is.null(level_1)) {
                html <- paste0(html, "<li><strong>Level 1:</strong> ", level_1, " (lowest level grouping)</li>")
            }

            html <- paste0(html, "</ul>")

            html <- paste0(html, "<h4>Clinical Significance</h4>")
            html <- paste0(html, "<p>Hierarchical models are essential for analyzing nested pathology data because:</p>")
            html <- paste0(html, "<ul>")
            html <- paste0(html, "<li><strong>Accounts for clustering:</strong> Observations within the same group are more similar</li>")
            html <- paste0(html, "<li><strong>Prevents Type I errors:</strong> Ignoring hierarchical structure inflates false positive rates</li>")
            html <- paste0(html, "<li><strong>Estimates reliability:</strong> ICC values indicate measurement consistency across levels</li>")
            html <- paste0(html, "<li><strong>Improves generalizability:</strong> Random effects allow inference beyond the specific sample</li>")
            html <- paste0(html, "</ul>")

            html <- paste0(html, "<h4>Interpretation Guidelines</h4>")
            html <- paste0(html, "<p><strong>Variance Components:</strong> Show how much variation exists at each hierarchical level</p>")
            html <- paste0(html, "<p><strong>ICC Values:</strong></p>")
            html <- paste0(html, "<ul>")
            html <- paste0(html, "<li>< 0.05: Poor reliability</li>")
            html <- paste0(html, "<li>0.05-0.40: Fair reliability</li>")
            html <- paste0(html, "<li>0.40-0.60: Moderate reliability</li>")
            html <- paste0(html, "<li>0.60-0.75: Good reliability</li>")
            html <- paste0(html, "<li>> 0.75: Excellent reliability</li>")
            html <- paste0(html, "</ul>")

            html <- paste0(html, "<h4>Digital Pathology Applications</h4>")
            html <- paste0(html, "<p>This hierarchical approach is crucial for:</p>")
            html <- paste0(html, "<ul>")
            html <- paste0(html, "<li><strong>WSI Analysis:</strong> Patient > Slide > ROI > Cell measurements</li>")
            html <- paste0(html, "<li><strong>Multi-center Studies:</strong> Institution > Patient > Sample effects</li>")
            html <- paste0(html, "<li><strong>Reproducibility Assessment:</strong> Between and within observer variation</li>")
            html <- paste0(html, "<li><strong>Algorithm Validation:</strong> Performance across different data sources</li>")
            html <- paste0(html, "</ul>")

            self$results$interpretation$setContent(html)
        },

        .initDescriptiveTable = function() {
            table <- self$results$descriptives

            table$addColumn(name = 'level', title = 'Level', type = 'text')
            table$addColumn(name = 'n_obs', title = 'N Observations', type = 'integer')
            table$addColumn(name = 'n_groups', title = 'N Groups', type = 'integer')
            table$addColumn(name = 'mean', title = 'Mean', type = 'number')
            table$addColumn(name = 'sd', title = 'SD', type = 'number')
            table$addColumn(name = 'min_val', title = 'Min', type = 'number')
            table$addColumn(name = 'max_val', title = 'Max', type = 'number')
            table$addColumn(name = 'median', title = 'Median', type = 'number')
            table$addColumn(name = 'iqr', title = 'IQR', type = 'number')
        },

        .initVarianceTable = function() {
            table <- self$results$variancecomponents

            table$addColumn(name = 'component', title = 'Component', type = 'text')
            table$addColumn(name = 'variance', title = 'Variance', type = 'number')
            table$addColumn(name = 'std_dev', title = 'Std Dev', type = 'number')
            table$addColumn(name = 'proportion', title = 'Proportion', type = 'number')
            table$addColumn(name = 'percentage', title = 'Percentage', type = 'text')
        },

        .initModelTable = function() {
            table <- self$results$modelresults

            table$addColumn(name = 'term', title = 'Term', type = 'text')
            table$addColumn(name = 'estimate', title = 'Estimate', type = 'number')
            table$addColumn(name = 'std_error', title = 'Std Error', type = 'number')
            table$addColumn(name = 'statistic', title = 'Statistic', type = 'number')
            table$addColumn(name = 'p_value', title = 'p-value', type = 'number', format = 'zto,pvalue')
            table$addColumn(name = 'ci_lower', title = '95% CI Lower', type = 'number')
            table$addColumn(name = 'ci_upper', title = '95% CI Upper', type = 'number')
        },

        .initRandomEffectsTable = function() {
            table <- self$results$randomeffects

            table$addColumn(name = 'group', title = 'Group', type = 'text')
            table$addColumn(name = 'effect', title = 'Effect', type = 'text')
            table$addColumn(name = 'variance', title = 'Variance', type = 'number')
            table$addColumn(name = 'std_dev', title = 'Std Dev', type = 'number')
            table$addColumn(name = 'correlation', title = 'Correlation', type = 'number')
        },

        .initICCTable = function() {
            table <- self$results$icc

            table$addColumn(name = 'level', title = 'Level', type = 'text')
            table$addColumn(name = 'icc_value', title = 'ICC', type = 'number')
            table$addColumn(name = 'ci_lower', title = '95% CI Lower', type = 'number')
            table$addColumn(name = 'ci_upper', title = '95% CI Upper', type = 'number')
            table$addColumn(name = 'interpretation', title = 'Interpretation', type = 'text')
        },

        .populateInstructionsTable = function() {
            html <- paste0(
                "<h2>Hierarchical Mixed-Effects Models</h2>",
                "<p>This module performs hierarchical (multilevel) analysis for nested pathology data.</p>",
                "<h3>Getting Started</h3>",
                "<ol>",
                "<li><strong>Dependent Variable:</strong> Select your outcome measure (continuous, binary, or count)</li>",
                "<li><strong>Hierarchical Structure:</strong> Specify grouping variables from highest to lowest level:</li>",
                "<ul>",
                "<li><strong>Level 3:</strong> Highest grouping (e.g., Patient, Institution)</li>",
                "<li><strong>Level 2:</strong> Intermediate grouping (e.g., Slide, Sample)</li>",
                "<li><strong>Level 1:</strong> Lowest grouping (e.g., ROI, Field)</li>",
                "</ul>",
                "<li><strong>Covariates:</strong> Optional fixed effects (age, treatment, etc.)</li>",
                "<li><strong>Model Type:</strong> Choose based on your outcome variable type</li>",
                "</ol>",
                "<h3>When to Use</h3>",
                "<ul>",
                "<li><strong>WSI Analysis:</strong> Multiple ROIs per slide, multiple slides per patient</li>",
                "<li><strong>Multi-center Studies:</strong> Patients nested within institutions</li>",
                "<li><strong>Repeated Measurements:</strong> Multiple observations per subject</li>",
                "<li><strong>Algorithm Validation:</strong> Performance across different datasets</li>",
                "</ul>",
                "<h3>Output Interpretation</h3>",
                "<p><strong>Variance Components:</strong> Partition total variance across hierarchical levels</p>",
                "<p><strong>ICC (Intraclass Correlation):</strong> Proportion of variance due to clustering</p>",
                "<p><strong>Fixed Effects:</strong> Average effects of covariates across all groups</p>",
                "<p><strong>Random Effects:</strong> Group-specific deviations from overall mean</p>"
            )

            self$results$instructions$setContent(html)
        },

        .model = NULL
    )
)
