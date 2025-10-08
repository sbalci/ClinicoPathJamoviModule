
#' @title Variable Importance Biplot
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom FactoMineR PCA
#' @importFrom factoextra fviz_pca_biplot get_pca_var get_pca_ind
#' @importFrom stats prcomp predict
#' @importFrom cluster silhouette
#' @export


variablebiplotClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "variablebiplotClass",
    inherit = variablebiplotBase,
    private = list(

        #---------------------------------------------
        # Escape variable names with special characters
        .escapeVar = function(x) {
            gsub("[^A-Za-z0-9_]+", "_", make.names(x))
        },

        # Safe variable access
        .getVarData = function(varname) {
            # Try direct access first
            if (varname %in% names(self$data)) {
                return(self$data[[varname]])
            }
            # Try escaped name
            escaped <- private$.escapeVar(varname)
            if (escaped %in% names(self$data)) {
                return(self$data[[escaped]])
            }
            stop(paste("Variable not found:", varname))
        },

        #---------------------------------------------
        # Input validation
        .validateInputs = function() {
            # Check variable selection
            if (is.null(self$options$groupVar)) {
                self$results$todo$setContent(paste0(
                    "<div style='padding: 1em;'>",
                    "<p><b>To create a biplot, please select:</b></p>",
                    "<ol>",
                    "<li><b>Grouping Variable</b> – The groups you want to compare ",
                    "(e.g., tumor stage, treatment group, disease subtype)</li>",
                    "<li>At least 2 <b>Features</b> – Clinical/pathological measurements ",
                    "(e.g., Ki-67, tumor size, biomarker expression)</li>",
                    "</ol>",
                    "<p><i>Tip: Start with 3-5 features for clearest visualization.</i></p>",
                    "</div>"
                ))
                return(FALSE)
            }

            if (length(self$options$features) < 2) {
                self$results$todo$setContent(paste0(
                    "<div style='padding: 1em;'>",
                    "<p><b>Need more features:</b> Biplot analysis requires at least ",
                    "<b>2 features</b> (variables).</p>",
                    "<p>Please select additional features from your dataset.</p>",
                    "<p><i>Recommended: 3-10 features for optimal visualization.</i></p>",
                    "</div>"
                ))
                return(FALSE)
            }

            # Validate group structure
            # DISABLED FOR DEBUGGING - tryCatch({
                group_data <- private$.getVarData(self$options$groupVar)
                group_clean <- na.omit(group_data)
                n_groups <- length(unique(group_clean))

                if (n_groups < 2) {
                    self$results$todo$setContent(paste0(
                        "<div style='padding: 1em; background: #fff3cd; border-left: 4px solid #ffc107;'>",
                        "<p><b>⚠ Grouping Variable Issue:</b></p>",
                        "<p>The selected grouping variable has only <b>", n_groups, " unique value</b>. ",
                        "Biplot analysis requires at least <b>2 groups</b> to compare.</p>",
                        "<p>Please select a different grouping variable with 2+ categories.</p>",
                        "</div>"
                    ))
                    return(FALSE)
                }

                # Check sample size per group
                group_sizes <- table(group_clean)
                min_size <- min(group_sizes)

                if (min_size < 3) {
                    warning_msg <- paste0(
                        "<div style='padding: 1em; background: #fff3cd; border-left: 4px solid #ffc107;'>",
                        "<p><b>⚠ Small Group Size Warning:</b></p>",
                        "<p>One or more groups have very few samples (minimum: <b>", min_size, "</b>).</p>",
                        "<p><b>Recommendation:</b> At least 5 samples per group for reliable results. ",
                        "Results with n&lt;3 may be unstable.</p>",
                        "</div>"
                    )
                    self$results$assumptions$setContent(warning_msg)
                }

                # Check missing data
                missing_pct <- (length(group_data) - length(group_clean)) / length(group_data) * 100
                if (missing_pct > 10) {
                    existing_content <- self$results$assumptions$state
                    warning_msg <- paste0(
                        if (!is.null(existing_content)) existing_content else "",
                        "<div style='padding: 1em; background: #d1ecf1; border-left: 4px solid #0c5460;'>",
                        "<p><b>ℹ Missing Data Notice:</b></p>",
                        "<p><b>", round(missing_pct, 1), "%</b> of cases have missing data in the grouping variable. ",
                        "These cases will be excluded from analysis.</p>",
                        "</div>"
                    )
                    self$results$assumptions$setContent(warning_msg)
                }

                # Check feature types
                for (feat in self$options$features) {
                    feat_data <- private$.getVarData(feat)
                    message("DEBUG: Checking feature '", feat, "' - is.factor=", is.factor(feat_data),
                            " is.ordered=", is.ordered(feat_data))
                    if (is.factor(feat_data) && !is.ordered(feat_data)) {
                        message("DEBUG: Feature '", feat, "' is unordered factor - VALIDATION FAILED")
                        self$results$todo$setContent(paste0(
                            "<div style='padding: 1em; background: #f8d7da; border-left: 4px solid #dc3545;'>",
                            "<p><b>❌ Feature Type Error:</b></p>",
                            "<p>Feature <b>'", feat, "'</b> is categorical (nominal factor). ",
                            "Biplot analysis requires <b>numeric or ordinal</b> features.</p>",
                            "<p><b>Solutions:</b></p>",
                            "<ul>",
                            "<li>Remove this feature, or</li>",
                            "<li>Convert to numeric (if it represents ordered categories like grade), or</li>",
                            "<li>Use dummy coding (create separate 0/1 variables for each category)</li>",
                            "</ul>",
                            "</div>"
                        ))
                        return(FALSE)
                    }
                }

                # Clear todo if all validations pass
                self$results$todo$setContent("")
                return(TRUE)

            # DISABLED FOR DEBUGGING
            # }, error = function(e) {
            #     self$results$todo$setContent(paste0(
            #         "<div style='padding: 1em; background: #f8d7da; border-left: 4px solid #dc3545;'>",
            #         "<p><b>❌ Input Error:</b></p>",
            #         "<p>", e$message, "</p>",
            #         "</div>"
            #     ))
            #     return(FALSE)
            # }
            # )
        },

        #---------------------------------------------
        # Method-specific validation
        .validateMethodChoice = function() {
            method <- self$options$method
            n_features <- length(self$options$features)

            group_data <- private$.getVarData(self$options$groupVar)
            n_groups <- length(unique(na.omit(group_data)))
            n_samples <- sum(!is.na(group_data))

            warnings <- ""

            # Sample size warnings
            if (n_samples < 30 && method == "lda") {
                warnings <- paste0(warnings,
                    "<div style='margin: 0.5em 0; padding: 0.8em; background: #fff3cd; border-left: 4px solid #ffc107;'>",
                    "<p><b>⚠ Small Sample Size for LDA:</b></p>",
                    "<p>You have <b>", n_samples, " samples</b>. LDA typically requires n≥30 ",
                    "and assumes normally distributed data.</p>",
                    "<p><b>Recommendation:</b> Consider using <b>PLS-DA</b> instead, which is ",
                    "more robust with small samples and doesn't require normality assumptions.</p>",
                    "</div>"
                )
            }

            # Feature-to-sample ratio
            if (n_features > n_samples * 0.5) {
                warnings <- paste0(warnings,
                    "<div style='margin: 0.5em 0; padding: 0.8em; background: #fff3cd; border-left: 4px solid #ffc107;'>",
                    "<p><b>⚠ High Feature-to-Sample Ratio:</b></p>",
                    "<p>You have <b>", n_features, " features</b> but only <b>", n_samples, " samples</b>. ",
                    "This high ratio (>50%) may lead to overfitting.</p>",
                    "<p><b>Recommendations:</b></p>",
                    "<ul>",
                    "<li>Reduce number of features (keep most clinically relevant), or</li>",
                    "<li>Use PLS-DA which handles high-dimensional data better</li>",
                    "</ul>",
                    "</div>"
                )
            }

            # LDA dimensionality note
            if (method == "lda" && n_features > (n_groups - 1)) {
                warnings <- paste0(warnings,
                    "<div style='margin: 0.5em 0; padding: 0.8em; background: #d1ecf1; border-left: 4px solid #0c5460;'>",
                    "<p><b>ℹ LDA Dimensionality Note:</b></p>",
                    "<p>LDA produces at most <b>", n_groups - 1, " discriminant functions</b> ",
                    "for ", n_groups, " groups. You selected ", n_features, " features, ",
                    "but only ", n_groups - 1, " dimensions will be meaningful.</p>",
                    "<p>This is normal behavior for LDA.</p>",
                    "</div>"
                )
            }

            # PLS-DA recommendation for biomarker discovery
            if (method == "pca" && n_groups >= 2) {
                warnings <- paste0(warnings,
                    "<div style='margin: 0.5em 0; padding: 0.8em; background: #d4edda; border-left: 4px solid #28a745;'>",
                    "<p><b>💡 Tip for Biomarker Discovery:</b></p>",
                    "<p>You're using <b>PCA</b> (unsupervised), which explores overall patterns ",
                    "but doesn't focus on separating your groups.</p>",
                    "<p>For identifying features that <i>distinguish</i> your groups ",
                    "(e.g., biomarker discovery), consider using <b>PLS-DA</b> instead.</p>",
                    "</div>"
                )
            }

            # Update assumptions panel with warnings
            if (nchar(warnings) > 0) {
                existing <- self$results$assumptions$state
                combined <- paste0(if (!is.null(existing)) existing else "", warnings)
                self$results$assumptions$setContent(combined)
            }
        },

        #---------------------------------------------
        .init = function() {

            # Comprehensive About Panel
            about_html <- paste0(
                "<style>",
                ".about-panel { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif; line-height: 1.6; }",
                ".about-panel h3 { color: #2c3e50; margin-top: 1.2em; margin-bottom: 0.5em; }",
                ".about-panel h4 { color: #34495e; margin-top: 1em; margin-bottom: 0.4em; }",
                ".about-panel ul { margin-left: 1.5em; margin-bottom: 0.8em; }",
                ".about-panel table { width: 100%; border-collapse: collapse; margin: 1em 0; }",
                ".about-panel th { padding: 8px; text-align: left; background: #f8f9fa; border-bottom: 2px solid #dee2e6; }",
                ".about-panel td { padding: 8px; border-bottom: 1px solid #dee2e6; }",
                ".clinical-example { background: #f8f9fa; padding: 1em; border-left: 4px solid #3498db; margin: 1em 0; }",
                ".tip-box { background: #d4edda; padding: 0.8em; border-left: 4px solid #28a745; margin: 0.8em 0; }",
                "</style>",

                "<div class='about-panel'>",
                "<h3>🔬 Variable Importance Biplot</h3>",

                "<p><b>What it does:</b> Visualizes how well your features (biomarkers, clinical variables) ",
                "separate different groups (tumor stages, treatment responses, etc.) and identifies which ",
                "variables are most important for distinguishing groups.</p>",

                "<h4>📊 Understanding the Plot</h4>",
                "<ul>",
                "<li><b>Points (●)</b> = Individual patients/samples, colored by group</li>",
                "<li><b>Arrows (→)</b> = Features/variables (longer arrow = more important for separation)</li>",
                "<li><b>Distance between groups</b> = How well they separate</li>",
                "<li><b>Arrow direction</b> = Which group that feature associates with</li>",
                "<li><b>Angle between arrows</b> = Correlation (small angle = positive, 90° = uncorrelated, 180° = negative)</li>",
                "</ul>",

                "<h4>🎯 Which Method to Choose?</h4>",
                "<table>",
                "<tr>",
                "<th>Method</th>",
                "<th>When to Use</th>",
                "<th>Clinical Example</th>",
                "</tr>",
                "<tr>",
                "<td><b>PCA</b></td>",
                "<td>Explore patterns without predefined groups</td>",
                "<td>\"Are there natural clusters in my pathology data?\"</td>",
                "</tr>",
                "<tr>",
                "<td><b>PLS-DA</b> ⭐ Recommended</td>",
                "<td>Find biomarkers that distinguish groups</td>",
                "<td>\"Which markers separate stage I from stage III?\"</td>",
                "</tr>",
                "<tr>",
                "<td><b>LDA</b></td>",
                "<td>Well-defined groups, normally distributed data</td>",
                "<td>\"Compare 3 treatment arms in a controlled trial\"</td>",
                "</tr>",
                "</table>",

                "<div class='clinical-example'>",
                "<h4>💡 Clinical Example: Biomarker Discovery</h4>",
                "<p><b>Research Question:</b> Which pathology markers best distinguish early-stage (I/II) ",
                "from late-stage (III/IV) colorectal cancer?</p>",

                "<p><b>Setup:</b></p>",
                "<ul>",
                "<li><b>Grouping Variable:</b> tumor_stage (Early vs Late)</li>",
                "<li><b>Features:</b> Ki-67 index, p53 expression, tumor size (mm), lymphocyte count, differentiation grade</li>",
                "<li><b>Method:</b> PLS-DA (biomarker discovery)</li>",
                "<li><b>Options:</b> Check 'Show Variable Loadings' and set 'Top Contributors' to 10</li>",
                "</ul>",

                "<p><b>How to Interpret Results:</b></p>",
                "<ul>",
                "<li>Look for <b>long arrows pointing toward late-stage group</b> → features associated with advanced disease</li>",
                "<li>Check <b>contribution table</b> → Top 3 features are your best biomarkers</li>",
                "<li>Examine <b>group separation</b> → Clear gap = good discrimination</li>",
                "</ul>",
                "</div>",

                "<h4>⚙️ Quick Start Guide</h4>",
                "<ol>",
                "<li>Select <b>Grouping Variable</b> (the groups you want to compare)</li>",
                "<li>Select 3+ <b>Features</b> (biomarkers, clinical measurements)</li>",
                "<li>Choose <b>Method</b> (start with PLS-DA if unsure)</li>",
                "<li>Under 'Biplot Options', check <b>'Show Variable Loadings'</b></li>",
                "<li>Set <b>'Top Contributors to Label'</b> to 10</li>",
                "<li>Under 'Contribution Metrics', check <b>'Variable Contribution Table'</b></li>",
                "</ol>",

                "<div class='tip-box'>",
                "<p><b>💡 Pro Tip:</b> For clinical reporting, enable the 'Summary' output option ",
                "to get a copy-ready paragraph with all key statistics automatically filled in.</p>",
                "</div>",

                "<h4>⚠️ Important Notes</h4>",
                "<ul>",
                "<li>Need at least <b>5 samples per group</b> for reliable results</li>",
                "<li>Features must be <b>numeric or ordinal</b> (not categorical)</li>",
                "<li>Check 'Center and Scale Variables' if features have different units (e.g., mixing percentages with counts)</li>",
                "<li>For small samples (n&lt;30), prefer <b>PLS-DA</b> over LDA</li>",
                "<li>High number of features (p &gt; n/2) may lead to overfitting - consider feature selection first</li>",
                "</ul>",

                "<h4>📚 Further Reading</h4>",
                "<ul>",
                "<li>Inspired by Orange Data Mining's contribution analysis</li>",
                "<li>Adapted specifically for clinical and pathological research</li>",
                "<li>Includes comprehensive variable importance metrics</li>",
                "</ul>",

                "</div>"
            )

            self$results$about$setContent(about_html)

            # Initialize variance table
            if (self$options$showVarianceExplained) {
                varTable <- self$results$componentVariance
                varTable$setVisible(TRUE)
            }

            # Initialize contribution table
            if (self$options$showContribTable) {
                contribTable <- self$results$contributionTable
                contribTable$setVisible(TRUE)
            }

            # Initialize separation table
            if (self$options$showSeparation) {
                sepTable <- self$results$separationAnalysis
                sepTable$setVisible(TRUE)
            }
        },

        #---------------------------------------------
        .run = function() {

            # DEBUG: Log entry to .run()
            message("DEBUG: .run() called")

            # Validate inputs first
            valid <- private$.validateInputs()
            message("DEBUG: validateInputs returned: ", valid)
            if (!valid) {
                message("DEBUG: Exiting .run() due to validation failure")
                return()
            }

            # Add checkpoint for cancellation
            private$.checkpoint()

            # Perform method-specific validation and warnings
            private$.validateMethodChoice()

            # Get data
            groupVar <- self$options$groupVar
            features <- self$options$features
            method <- self$options$method

            message("DEBUG: groupVar=", groupVar, " features=", paste(features, collapse=","), " method=", method)

            # Progress feedback
            self$results$biplot$setState(paste0("Preparing data for ",
                                                 toupper(method), " analysis..."))
            private$.checkpoint()

            data <- self$data

            # Extract variables using safe accessor with improved error handling
            # DISABLED FOR DEBUGGING - tryCatch({
                message("DEBUG: Extracting group data...")
                group_data <- jmvcore::toNumeric(private$.getVarData(groupVar))
                group_factor <- factor(private$.getVarData(groupVar))
                message("DEBUG: Group data extracted, n_groups=", length(unique(group_factor)))

                feature_data <- sapply(features, function(f) {
                    var_data <- private$.getVarData(f)
                    if (is.factor(var_data)) {
                        if (!is.ordered(var_data)) {
                            stop(paste0("Feature '", f, "' is an unordered factor. ",
                                        "Convert to numeric or ordered factor first."))
                        }
                        as.numeric(var_data)
                    } else {
                        as.numeric(var_data)
                    }
                })
            # DISABLED FOR DEBUGGING
            # }, error = function(e) {
            #     stop(paste0("Error extracting features [", paste(features, collapse=", "),
            #                 "]: ", e$message))
            # }
            # )

            # Remove missing data
            complete_cases <- complete.cases(feature_data, group_data)
            n_missing <- sum(!complete_cases)

            feature_data <- feature_data[complete_cases, , drop = FALSE]
            group_factor <- group_factor[complete_cases]

            if (nrow(feature_data) < 10) {
                stop(paste0("Insufficient complete cases for analysis. ",
                            "Found ", nrow(feature_data), " complete cases, need at least 10. ",
                            "(", n_missing, " cases excluded due to missing data)"))
            }

            # Store feature names
            feature_names <- features
            colnames(feature_data) <- feature_names

            # Center and scale if requested
            if (self$options$centerScale) {
                feature_data <- scale(feature_data)
            }

            private$.checkpoint()

            # Perform analysis based on method
            method <- self$options$method

            message("DEBUG: About to run analysis, method=", method)

            if (method == "pca") {
                self$results$biplot$setState("Running PCA...")
                private$.checkpoint()
                message("DEBUG: Calling .runPCA()")
                private$.runPCA(feature_data, group_factor, features)
                message("DEBUG: .runPCA() completed")
            } else if (method == "plsda") {
                self$results$biplot$setState("Running PLS-DA (this may take a moment)...")
                private$.checkpoint()
                message("DEBUG: Calling .runPLSDA()")
                private$.runPLSDA(feature_data, group_factor, features)
                message("DEBUG: .runPLSDA() completed")
            } else if (method == "lda") {
                self$results$biplot$setState("Running LDA...")
                private$.checkpoint()
                message("DEBUG: Calling .runLDA()")
                private$.runLDA(feature_data, group_factor, features)
                message("DEBUG: .runLDA() completed")
            }

            # Clear state after completion
            self$results$biplot$setState(NULL)
            message("DEBUG: .run() completed successfully")
        },

        #---------------------------------------------
        .runPCA = function(feature_data, group_factor, feature_names) {

            message("DEBUG: .runPCA() entry, feature_data dim=", nrow(feature_data), "x", ncol(feature_data))

            # Perform PCA with error context
            # DISABLED FOR DEBUGGING - tryCatch({
                message("DEBUG: Running prcomp()...")
                pca_result <- prcomp(feature_data, center = FALSE, scale. = FALSE)
                message("DEBUG: prcomp() completed, sdev=", paste(head(pca_result$sdev, 3), collapse=","))
            # DISABLED FOR DEBUGGING
            # }, error = function(e) {
            #     stop(paste0("PCA failed: ", e$message,
            #                 ". Check that features are numeric and have sufficient variance."))
            # }
            # )

            # Store for plotting
            private$.pca_result <- pca_result
            private$.group_factor <- group_factor
            private$.feature_names <- feature_names

            # Variance explained
            if (self$options$showVarianceExplained) {
                message("DEBUG: Populating variance table...")
                private$.populateVarianceTable(pca_result)
                message("DEBUG: Variance table populated")
            }

            # Variable contributions
            if (self$options$showContribTable) {
                message("DEBUG: Populating contribution table...")
                private$.populateContributionTable(pca_result, feature_names)
                message("DEBUG: Contribution table populated")
            }

            # Group separation
            if (self$options$showSeparation) {
                scores <- pca_result$x
                private$.analyzeGroupSeparation(scores, group_factor)
            }

            # Summary
            private$.generateSummary(pca_result, "PCA")
        },

        #---------------------------------------------
        .runPLSDA = function(feature_data, group_factor, feature_names) {

            # For PLS-DA, we need plsda from mixOmics or similar
            # DISABLED FOR DEBUGGING - Fallback to PCA if not available
            # if (!requireNamespace("mixOmics", quietly = TRUE)) {
            #     self$results$methodSummary$setContent(
            #         "<p><b>Note:</b> PLS-DA requires the 'mixOmics' package. Falling back to PCA.</p>"
            #     )
            #     private$.runPCA(feature_data, group_factor, feature_names)
            #     return()
            # }
            # Now it will error if mixOmics is not available

            # Perform PLS-DA
            plsda_result <- mixOmics::plsda(feature_data, group_factor, ncomp = min(10, ncol(feature_data)))

            # Store for plotting
            private$.plsda_result <- plsda_result
            private$.group_factor <- group_factor
            private$.feature_names <- feature_names

            # Variance explained
            if (self$options$showVarianceExplained) {
                private$.populateVarianceTablePLSDA(plsda_result)
            }

            # Variable contributions (loadings)
            if (self$options$showContribTable) {
                private$.populateContributionTablePLSDA(plsda_result, feature_names)
            }

            # Group separation
            if (self$options$showSeparation) {
                scores <- plsda_result$variates$X
                private$.analyzeGroupSeparation(scores, group_factor)
            }

            # Summary
            private$.generateSummary(plsda_result, "PLS-DA")
        },

        #---------------------------------------------
        .runLDA = function(feature_data, group_factor, feature_names) {

            # DISABLED FOR DEBUGGING - will error if MASS not available
            # if (!requireNamespace("MASS", quietly = TRUE)) {
            #     stop("LDA requires the 'MASS' package")
            # }

            # Perform LDA
            lda_result <- MASS::lda(feature_data, grouping = group_factor)

            # Get scores
            lda_scores <- predict(lda_result, feature_data)$x

            # Store for plotting
            private$.lda_result <- lda_result
            private$.lda_scores <- lda_scores
            private$.group_factor <- group_factor
            private$.feature_names <- feature_names

            # Variance explained (proportion of trace)
            if (self$options$showVarianceExplained) {
                private$.populateVarianceTableLDA(lda_result)
            }

            # Variable contributions (scaling coefficients)
            if (self$options$showContribTable) {
                private$.populateContributionTableLDA(lda_result, feature_names)
            }

            # Group separation
            if (self$options$showSeparation) {
                private$.analyzeGroupSeparation(lda_scores, group_factor)
            }

            # Summary
            private$.generateSummary(lda_result, "LDA")
        },

        #---------------------------------------------
        .populateVarianceTable = function(pca_result) {

            varTable <- self$results$componentVariance

            # Calculate variance explained
            var_explained <- (pca_result$sdev^2) / sum(pca_result$sdev^2) * 100
            cumvar <- cumsum(var_explained)
            eigenvalues <- pca_result$sdev^2

            # Populate table for components up to minVariance threshold
            n_comp <- min(which(cumvar >= self$options$minVariance)[1], length(var_explained))
            if (is.na(n_comp)) n_comp <- length(var_explained)

            for (i in 1:n_comp) {
                varTable$addRow(rowKey = i, values = list(
                    component = paste0("PC", i),
                    eigenvalue = eigenvalues[i],
                    variance = var_explained[i],
                    cumulative = cumvar[i]
                ))
            }
        },

        #---------------------------------------------
        .populateContributionTable = function(pca_result, feature_names) {

            contribTable <- self$results$contributionTable

            pc1 <- self$options$pc1
            pc2 <- self$options$pc2

            # Get loadings
            loadings <- pca_result$rotation

            # Calculate contributions based on selected metric
            metric <- self$options$contributionMetric

            if (metric == "loading") {
                contrib_pc1 <- abs(loadings[, pc1])
                contrib_pc2 <- abs(loadings[, pc2])
            } else if (metric == "squared") {
                contrib_pc1 <- (loadings[, pc1]^2) / sum(loadings[, pc1]^2) * 100
                contrib_pc2 <- (loadings[, pc2]^2) / sum(loadings[, pc2]^2) * 100
            } else { # correlation
                # Correlation = loading * sqrt(eigenvalue)
                contrib_pc1 <- abs(loadings[, pc1] * pca_result$sdev[pc1])
                contrib_pc2 <- abs(loadings[, pc2] * pca_result$sdev[pc2])
            }

            total_contrib <- contrib_pc1 + contrib_pc2
            quality <- (loadings[, pc1]^2 + loadings[, pc2]^2)

            # Create data frame and sort by total contribution
            contrib_df <- data.frame(
                variable = feature_names,
                pc1_contrib = contrib_pc1,
                pc2_contrib = contrib_pc2,
                total_contrib = total_contrib,
                quality = quality,
                stringsAsFactors = FALSE
            )

            contrib_df <- contrib_df[order(-contrib_df$total_contrib), ]

            # Populate table
            for (i in 1:nrow(contrib_df)) {
                contribTable$addRow(rowKey = i, values = list(
                    variable = contrib_df$variable[i],
                    pc1_contrib = contrib_df$pc1_contrib[i],
                    pc2_contrib = contrib_df$pc2_contrib[i],
                    total_contrib = contrib_df$total_contrib[i],
                    quality = contrib_df$quality[i]
                ))
            }

            # Add clinical interpretation
            private$.addContributionInterpretation(contrib_df)
        },

        #---------------------------------------------
        .addContributionInterpretation = function(contrib_df) {

            # Early return if no contributions to interpret
            if (is.null(contrib_df) || nrow(contrib_df) == 0) {
                return()
            }

            # Get top 3 contributors
            top_n <- min(3, nrow(contrib_df))
            top_vars <- contrib_df$variable[1:top_n]
            top_contribs <- contrib_df$total_contrib[1:top_n]

            # Calculate contribution statistics
            total_top3 <- sum(top_contribs)
            avg_contrib <- mean(contrib_df$total_contrib)

            # Generate interpretation
            interpretation_html <- paste0(
                "<div style='padding: 1em; background: #f0f8ff; border-left: 4px solid #0066cc;'>",
                "<h4>💡 Clinical Interpretation</h4>",

                "<p><b>Top Contributing Features:</b></p>",
                "<ol>"
            )

            for (i in 1:top_n) {
                interpretation_html <- paste0(interpretation_html,
                    "<li><b>", top_vars[i], "</b> (", round(top_contribs[i], 1),
                    if (self$options$contributionMetric == "squared") "% contribution)" else ")",
                    if (i == 1) " – <i>Most important variable for group separation</i>" else "",
                    "</li>"
                )
            }

            interpretation_html <- paste0(interpretation_html,
                "</ol>",

                "<p><b>What This Means:</b></p>",
                "<ul>",
                "<li>These ",top_n, " features account for <b>",
                if (self$options$contributionMetric == "squared") {
                    paste0(round(total_top3, 1), "%")
                } else {
                    "a substantial portion"
                },
                "</b> of the information in the biplot.</li>",

                "<li>In clinical terms: <b>", top_vars[1], "</b> is the single most important ",
                "variable for distinguishing between your groups in this analysis.</li>",

                if (total_top3 > 70 && self$options$contributionMetric == "squared") {
                    paste0("<li><b>Focused pattern:</b> A few key features drive most of the separation. ",
                    "These are your best candidate biomarkers.</li>")
                } else if (total_top3 < 40 && self$options$contributionMetric == "squared") {
                    paste0("<li><b>Distributed pattern:</b> Contribution is spread across many features. ",
                    "Group differences may be subtle or multifactorial.</li>")
                } else {
                    paste0("<li><b>Moderate pattern:</b> Several features contribute to separation. ",
                    "Consider all top contributors when interpreting results.</li>")
                },

                "</ul>",

                "<p><b>For Biomarker Discovery:</b></p>",
                "<ul>",
                "<li>Prioritize features in this table (sorted by contribution) for follow-up validation</li>",
                "<li>Features with high Total Contrib are most discriminatory between groups</li>",
                "<li>Features with high Quality (Cos²) are well-represented in this 2D view</li>",
                "</ul>",

                "<p><b>Example Reporting:</b></p>",
                "<p style='background: white; padding: 0.8em; font-style: italic;'>",
                "\"The top three contributing variables were ", top_vars[1],
                if (top_n > 1) paste0(", ", top_vars[2]) else "",
                if (top_n > 2) paste0(", and ", top_vars[3]) else "",
                ", with ", top_vars[1], " showing the strongest discriminatory power between groups.\"",
                "</p>",

                "</div>"
            )

            self$results$contributionInterpretation$setContent(interpretation_html)
        },

        #---------------------------------------------
        .analyzeGroupSeparation = function(scores, group_factor) {

            sepTable <- self$results$separationAnalysis
            metric <- self$options$separationMetric

            pc1 <- self$options$pc1
            pc2 <- self$options$pc2

            # Use first two components from scores
            scores_2d <- scores[, c(pc1, pc2), drop = FALSE]

            if (metric == "silhouette") {
                # DISABLED FOR DEBUGGING - will error if cluster not available
                # if (requireNamespace("cluster", quietly = TRUE)) {
                    dist_mat <- dist(scores_2d)
                    sil <- cluster::silhouette(as.numeric(group_factor), dist_mat)
                    avg_sil <- mean(sil[, 3])

                    interpretation <- if (avg_sil > 0.7) {
                        "Excellent separation - groups are well-defined"
                    } else if (avg_sil > 0.5) {
                        "Good separation - groups are reasonably distinct"
                    } else if (avg_sil > 0.25) {
                        "Weak separation - groups overlap considerably"
                    } else {
                        "Poor separation - groups are not well-separated"
                    }

                    sepTable$addRow(rowKey = 1, values = list(
                        metric = "Silhouette Score",
                        value = avg_sil,
                        interpretation = interpretation
                    ))
                # DISABLED FOR DEBUGGING
                # }
            } else if (metric == "bw_ratio") {
                # Between-group vs within-group variance ratio
                group_means <- aggregate(scores_2d, by = list(group_factor), FUN = mean)
                overall_mean <- colMeans(scores_2d)

                # Between-group variance
                bss <- sum(sapply(1:nrow(group_means), function(i) {
                    n_i <- sum(group_factor == group_means[i, 1])
                    sum((group_means[i, -1] - overall_mean)^2) * n_i
                }))

                # Within-group variance
                wss <- sum(sapply(unique(group_factor), function(g) {
                    group_data <- scores_2d[group_factor == g, , drop = FALSE]
                    group_mean <- colMeans(group_data)
                    sum((sweep(group_data, 2, group_mean))^2)
                }))

                bw_ratio <- bss / wss

                interpretation <- if (bw_ratio > 5) {
                    "Excellent separation - between-group variance >> within-group variance"
                } else if (bw_ratio > 2) {
                    "Good separation - clear group differences"
                } else if (bw_ratio > 1) {
                    "Moderate separation - some group differences"
                } else {
                    "Poor separation - within-group variance dominates"
                }

                sepTable$addRow(rowKey = 1, values = list(
                    metric = "Between/Within Ratio",
                    value = bw_ratio,
                    interpretation = interpretation
                ))
            } else if (metric == "mahalanobis") {
                # Average Mahalanobis distance between group centroids
                group_means <- aggregate(scores_2d, by = list(group_factor), FUN = mean)[, -1]
                pooled_cov <- cov(scores_2d)

                if (nrow(group_means) == 2) {
                    # For 2 groups, calculate distance
                    diff <- as.numeric(group_means[1, ] - group_means[2, ])
                    maha_dist <- sqrt(t(diff) %*% solve(pooled_cov) %*% diff)

                    interpretation <- if (maha_dist > 3) {
                        "Excellent separation - groups are very distinct"
                    } else if (maha_dist > 2) {
                        "Good separation - groups are reasonably distinct"
                    } else if (maha_dist > 1) {
                        "Moderate separation - some overlap between groups"
                    } else {
                        "Poor separation - substantial overlap between groups"
                    }

                    sepTable$addRow(rowKey = 1, values = list(
                        metric = "Mahalanobis Distance",
                        value = as.numeric(maha_dist),
                        interpretation = interpretation
                    ))
                } else {
                    # For >2 groups, use average pairwise distance
                    n_groups <- nrow(group_means)
                    distances <- c()
                    for (i in 1:(n_groups-1)) {
                        for (j in (i+1):n_groups) {
                            diff <- as.numeric(group_means[i, ] - group_means[j, ])
                            maha_dist <- sqrt(t(diff) %*% solve(pooled_cov) %*% diff)
                            distances <- c(distances, maha_dist)
                        }
                    }
                    avg_dist <- mean(distances)

                    interpretation <- if (avg_dist > 3) {
                        "Excellent separation - groups are very distinct"
                    } else if (avg_dist > 2) {
                        "Good separation - groups are reasonably distinct"
                    } else if (avg_dist > 1) {
                        "Moderate separation - some overlap between groups"
                    } else {
                        "Poor separation - substantial overlap between groups"
                    }

                    sepTable$addRow(rowKey = 1, values = list(
                        metric = "Avg Mahalanobis Distance",
                        value = avg_dist,
                        interpretation = interpretation
                    ))
                }
            }
        },

        #---------------------------------------------
        .generateSummary = function(result, method_name) {

            n_features <- length(private$.feature_names)
            n_obs <- if (method_name == "PCA") nrow(result$x) else
                     if (method_name == "LDA") nrow(private$.lda_scores) else
                     nrow(result$variates$X)
            n_groups <- length(unique(private$.group_factor))
            pc1 <- self$options$pc1
            pc2 <- self$options$pc2

            # Calculate variance explained
            if (method_name == "PCA") {
                var1 <- (result$sdev[pc1]^2) / sum(result$sdev^2) * 100
                var2 <- (result$sdev[pc2]^2) / sum(result$sdev^2) * 100
            } else if (method_name == "PLS-DA") {
                var1 <- result$explained_variance$X[pc1]
                var2 <- result$explained_variance$X[pc2]
            } else {  # LDA
                prop_trace <- result$svd^2 / sum(result$svd^2) * 100
                var1 <- prop_trace[pc1]
                var2 <- if (length(prop_trace) >= pc2) prop_trace[pc2] else 0
            }

            total_var <- var1 + var2

            # Natural-language Summary (controlled by checkbox)
            if (self$options$showSummary) {
                summary_text <- sprintf(
                    "<div style='padding: 1em; background: #f8f9fa; border-left: 4px solid #3498db;'>",
                    "<h4>📊 Summary</h4>",
                    "<p><b>Analysis:</b> %s biplot comparing <b>%d groups</b> using <b>%d features</b> ",
                    "from %d observations.</p>",
                    "<p><b>Key Finding:</b> The first two components (PC%d and PC%d) explain ",
                    "<b>%.1f%% + %.1f%% = %.1f%%</b> of the %s in the data.</p>",
                    "<p><b>Clinical Interpretation:</b> %s</p>",
                    "</div>",
                    method_name, n_groups, n_features, n_obs,
                    pc1, pc2, var1, var2, total_var,
                    if (method_name == "PCA") "total variance" else "group separation",
                    if (total_var > 70) {
                        paste0("These two components capture most of the important information (",
                               round(total_var), "%), suggesting the biplot provides a good summary of ",
                               if (method_name == "PCA") "data structure" else "group differences",
                               ". The biplot should show clear patterns.")
                    } else if (total_var > 50) {
                        paste0("These components capture a moderate amount of information (",
                               round(total_var), "%). Additional components may be needed to fully ",
                               "understand ", if (method_name == "PCA") "the data" else "group differences",
                               ". Consider examining higher components.")
                    } else {
                        paste0("These components capture relatively little information (",
                               round(total_var), "%). This suggests either: (1) variance is spread ",
                               "across many components, or (2) groups may not separate well on these ",
                               "particular features. Consider feature selection or examining more components.")
                    }
                )
                self$results$summary$setContent(summary_text)
            }

            # Copy-Ready Report (controlled by checkbox)
            if (self$options$showReport) {
                private$.generateReport(result, method_name, n_obs, n_features, n_groups,
                                        var1, var2, total_var)
            }

            # Interpretation Guide (controlled by checkbox)
            if (self$options$showInterpretation) {
                private$.generateInterpretationGuide(method_name)
            }

            # R Code Generation (controlled by checkbox)
            if (self$options$showRCode) {
                private$.generateRCode(result, method_name, n_obs, n_features)
            }
        },

        #---------------------------------------------
        .generateReport = function(result, method_name, n_obs, n_features, n_groups,
                                    var1, var2, total_var) {

            # Get top 3 contributors if available
            top_features <- ""
            if (self$options$showContribTable) {
                # Extract top contributors from stored results
                if (method_name == "PCA") {
                    loadings <- result$rotation[, c(self$options$pc1, self$options$pc2)]
                    contrib <- sqrt(loadings[, 1]^2 + loadings[, 2]^2)
                } else {
                    contrib <- rep(1, n_features)  # Placeholder
                }

                top_idx <- order(-contrib)[1:min(3, n_features)]
                top_vars <- private$.feature_names[top_idx]
                top_features <- paste(paste0("'", top_vars, "'"), collapse=", ")
            } else {
                top_features <- "[enable Variable Contribution Table to auto-fill]"
            }

            # Group separation quality
            separation_quality <- if (total_var > 70) "clear separation between groups" else
                                  if (total_var > 50) "moderate separation between groups" else
                                  "partial overlap between groups"

            report_html <- sprintf(
                "<div style='padding: 1em; background: #e7f3ff; border: 1px solid #0066cc;'>",
                "<h4>📋 Copy-Ready Report Paragraph</h4>",
                "<div style='background: white; padding: 1em; margin: 1em 0; font-family: Georgia, serif;'>",
                "We performed %s to visualize group separation based on %d clinical/pathological features ",
                "(n=%d observations, %d groups). The first two components explained <b>%.1f%%</b> and ",
                "<b>%.1f%%</b> of the %s, respectively (total = <b>%.1f%%</b>). Biplot visualization ",
                "revealed %s. The top contributing variables were %s, suggesting these features are ",
                "most important for distinguishing between groups.",
                "</div>",
                "<p><i>Tip: Click and drag to select text, then copy (Ctrl+C / Cmd+C)</i></p>",
                "</div>",
                if (method_name == "PCA") "principal component analysis (PCA)" else
                if (method_name == "PLS-DA") "partial least squares discriminant analysis (PLS-DA)" else
                "linear discriminant analysis (LDA)",
                n_features, n_obs, n_groups,
                var1, var2,
                if (method_name == "PCA") "total variance" else "variance in group separation",
                total_var,
                separation_quality,
                top_features
            )

            self$results$report$setContent(report_html)
        },

        #---------------------------------------------
        .generateInterpretationGuide = function(method_name) {

            guide_html <- paste0(
                "<div style='padding: 1em; background: #f8f9fa;'>",
                "<h4>📖 How to Read This Biplot</h4>",

                "<p><b>Visual Elements:</b></p>",
                "<ul>",
                "<li><b>Points (●)</b> represent individual ",
                if (method_name != "PCA") "patients/samples" else "observations",
                ", colored by group.</li>",
                "<li><b>Arrows (→)</b> represent features/variables. ",
                "<i>Longer arrows = stronger contribution to the displayed components.</i></li>",
                "<li><b>Distance between point clusters</b> indicates how well groups separate.</li>",
                "<li><b>Arrow direction</b> shows which group a feature associates with ",
                "(arrows point toward observations with high values for that feature).</li>",
                "</ul>",

                "<p><b>Interpreting Arrow Patterns:</b></p>",
                "<ul>",
                "<li><b>Arrows pointing in similar directions</b> (small angle) → Features are positively correlated</li>",
                "<li><b>Arrows at ~90°</b> → Features are uncorrelated (independent information)</li>",
                "<li><b>Arrows pointing opposite directions</b> (180°) → Features are negatively correlated</li>",
                "<li><b>Long arrows toward a group</b> → Important features for that group</li>",
                "</ul>",

                "<p><b>Clinical Interpretation:</b></p>",
                if (method_name == "PLS-DA" || method_name == "LDA") {
                    paste0(
                        "<p><b>For Biomarker Discovery:</b> Features with long arrows pointing ",
                        "toward a specific group (e.g., late-stage tumors) are the best candidate ",
                        "biomarkers for distinguishing that group. Check the Contribution Table to ",
                        "rank features quantitatively.</p>"
                    )
                } else {
                    paste0(
                        "<p><b>For Pattern Exploration:</b> PCA finds overall patterns without ",
                        "considering group labels. If groups cluster separately, it suggests they ",
                        "have distinct feature profiles. If they overlap, groups may be similar ",
                        "or additional features may be needed.</p>"
                    )
                },

                "<p><b>What to Report:</b></p>",
                "<ol>",
                "<li>Variance explained by the first two components</li>",
                "<li>Visual separation quality (clear/moderate/poor)</li>",
                "<li>Top 3-5 contributing features (from Contribution Table)</li>",
                "<li>Which features distinguish which groups</li>",
                "</ol>",

                "</div>"
            )

            self$results$interpretation$setContent(guide_html)
        },

        #---------------------------------------------
        .generateRCode = function(result, method_name, n_obs, n_features) {

            groupVar <- self$options$groupVar
            features <- private$.feature_names
            pc1 <- self$options$pc1
            pc2 <- self$options$pc2
            centerScale <- self$options$centerScale
            showLoadings <- self$options$showLoadings
            topContributors <- self$options$topContributors

            # Build feature string
            feature_str <- paste0("c(\"", paste(features, collapse = "\", \""), "\")")

            # Generate R code based on method
            if (method_name == "PCA") {
                r_code <- sprintf(
'# ============================================
# Reproducible R Code for PCA Biplot
# ============================================
# This code uses base R stats package instead of jamovi

# Load required packages
library(ggplot2)

# Assuming your data is in a data frame called "mydata"
# with grouping variable "%s" and features: %s

# 1. Prepare data
features <- %s
group_var <- "%s"

# Extract complete cases
complete_idx <- complete.cases(mydata[, c(group_var, features)])
data_clean <- mydata[complete_idx, ]

# Extract features and group
X <- as.matrix(data_clean[, features])
groups <- factor(data_clean[[group_var]])

%s

# 2. Perform PCA
pca_result <- prcomp(X, center = FALSE, scale. = FALSE)

# 3. Extract variance explained
var_explained <- (pca_result$sdev^2 / sum(pca_result$sdev^2)) * 100

# Variance for PC%d and PC%d
cat(sprintf("PC%d: %%.1f%%%% variance\\n", var_explained[%d]))
cat(sprintf("PC%d: %%.1f%%%% variance\\n", var_explained[%d]))

# 4. Extract scores and loadings
scores <- as.data.frame(pca_result$x[, c(%d, %d)])
colnames(scores) <- c("PC1", "PC2")
scores$Group <- groups

loadings <- as.data.frame(pca_result$rotation[, c(%d, %d)])
colnames(loadings) <- c("PC1", "PC2")
loadings$Variable <- features

%s

# 5. Create biplot
p <- ggplot(scores, aes(x = PC1, y = PC2, color = Group)) +
    geom_point(size = 3, alpha = 0.7) +
    labs(
        title = "PCA Biplot",
        x = sprintf("PC%d (%%.1f%%%% variance)", var_explained[%d]),
        y = sprintf("PC%d (%%.1f%%%% variance)", var_explained[%d])
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    theme_bw()

# 6. Add loading vectors (if requested)
%s

# 7. Display plot
print(p)

# 8. Variable contributions (optional)
# Calculate squared loadings contribution
contrib <- (pca_result$rotation[, c(%d, %d)]^2) /
           colSums(pca_result$rotation[, c(%d, %d)]^2) * 100
contrib_total <- rowSums(contrib)
contrib_df <- data.frame(
    Variable = features,
    PC%d = contrib[, 1],
    PC%d = contrib[, 2],
    Total = contrib_total
)
contrib_df <- contrib_df[order(-contrib_df$Total), ]
print(contrib_df)

# ============================================',
                    groupVar, paste(features, collapse = ", "),
                    feature_str, groupVar,
                    if (centerScale) "# Center and scale features\nX <- scale(X)" else "# Features used as-is (already centered/scaled in jamovi)",
                    pc1, pc2, pc1, pc1, pc2, pc2,
                    pc1, pc2, pc1, pc2,
                    if (showLoadings) sprintf(
'# Scale loadings for visualization
scale_factor <- %.1f
loadings$PC1_scaled <- loadings$PC1 * scale_factor * max(abs(scores$PC1))
loadings$PC2_scaled <- loadings$PC2 * scale_factor * max(abs(scores$PC2))

# Sort by contribution and keep top %d
loadings$contrib <- sqrt(loadings$PC1^2 + loadings$PC2^2)
loadings <- loadings[order(-loadings$contrib), ]
loadings_top <- loadings[1:min(%d, nrow(loadings)), ]',
                        self$options$loadingScale, topContributors, topContributors
                    ) else "# Loading vectors not requested",
                    pc1, pc2, pc1, pc2,
                    if (showLoadings) sprintf(
'p <- p +
    geom_segment(
        data = loadings_top,
        aes(x = 0, y = 0, xend = PC1_scaled, yend = PC2_scaled),
        arrow = arrow(length = unit(0.2, "cm")),
        color = "darkred", alpha = 0.6, inherit.aes = FALSE
    ) +
    geom_text(
        data = loadings_top,
        aes(x = PC1_scaled, y = PC2_scaled, label = Variable),
        color = "darkred", size = 3, inherit.aes = FALSE
    )'
                    ) else '# No loading vectors added',
                    pc1, pc2, pc1, pc2, pc1, pc2
                )

            } else if (method_name == "PLS-DA") {
                r_code <- sprintf(
'# ============================================
# Reproducible R Code for PLS-DA Biplot
# ============================================
# This code uses mixOmics package

# Load required packages
if (!requireNamespace("mixOmics", quietly = TRUE)) {
    install.packages("BiocManager")
    BiocManager::install("mixOmics")
}
library(mixOmics)
library(ggplot2)

# Assuming your data is in a data frame called "mydata"

# 1. Prepare data
features <- %s
group_var <- "%s"

# Extract complete cases
complete_idx <- complete.cases(mydata[, c(group_var, features)])
data_clean <- mydata[complete_idx, ]

# Extract features and group
X <- as.matrix(data_clean[, features])
Y <- factor(data_clean[[group_var]])

%s

# 2. Perform PLS-DA
plsda_result <- plsda(X, Y, ncomp = min(10, ncol(X)))

# 3. Extract variance explained
expl_var <- plsda_result$explained_variance$X

cat(sprintf("Comp%d: %%.1f%%%% variance\\n", expl_var[%d]))
cat(sprintf("Comp%d: %%.1f%%%% variance\\n", expl_var[%d]))

# 4. Extract scores and loadings
scores <- as.data.frame(plsda_result$variates$X[, c(%d, %d)])
colnames(scores) <- c("Comp1", "Comp2")
scores$Group <- Y

loadings <- as.data.frame(plsda_result$loadings$X[, c(%d, %d)])
colnames(loadings) <- c("Comp1", "Comp2")
loadings$Variable <- features

# 5. Create biplot
p <- ggplot(scores, aes(x = Comp1, y = Comp2, color = Group)) +
    geom_point(size = 3, alpha = 0.7) +
    labs(
        title = "PLS-DA Biplot",
        x = sprintf("Component %d (%%.1f%%%% variance)", expl_var[%d]),
        y = sprintf("Component %d (%%.1f%%%% variance)", expl_var[%d])
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    theme_bw()

# 6. Add loading vectors (if requested)
%s

print(p)

# 7. Variable importance (VIP scores)
vip_scores <- vip(plsda_result)
print(vip_scores)

# ============================================',
                    feature_str, groupVar,
                    if (centerScale) "# Center and scale features\nX <- scale(X)" else "# Features used as-is",
                    pc1, pc1, pc2, pc2,
                    pc1, pc2, pc1, pc2,
                    pc1, pc1, pc2, pc2,
                    if (showLoadings) sprintf(
'# Scale loadings
scale_factor <- %.1f
loadings$Comp1_scaled <- loadings$Comp1 * scale_factor * max(abs(scores$Comp1))
loadings$Comp2_scaled <- loadings$Comp2 * scale_factor * max(abs(scores$Comp2))
loadings$contrib <- sqrt(loadings$Comp1^2 + loadings$Comp2^2)
loadings <- loadings[order(-loadings$contrib), ]
loadings_top <- loadings[1:min(%d, nrow(loadings)), ]

p <- p +
    geom_segment(
        data = loadings_top,
        aes(x = 0, y = 0, xend = Comp1_scaled, yend = Comp2_scaled),
        arrow = arrow(length = unit(0.2, "cm")),
        color = "darkred", alpha = 0.6, inherit.aes = FALSE
    ) +
    geom_text(
        data = loadings_top,
        aes(x = Comp1_scaled, y = Comp2_scaled, label = Variable),
        color = "darkred", size = 3, inherit.aes = FALSE
    )',
                        self$options$loadingScale, topContributors
                    ) else '# No loading vectors'
                )

            } else {  # LDA
                r_code <- sprintf(
'# ============================================
# Reproducible R Code for LDA Biplot
# ============================================
# This code uses MASS package

# Load required packages
library(MASS)
library(ggplot2)

# Assuming your data is in a data frame called "mydata"

# 1. Prepare data
features <- %s
group_var <- "%s"

# Extract complete cases
complete_idx <- complete.cases(mydata[, c(group_var, features)])
data_clean <- mydata[complete_idx, ]

# Extract features and group
X <- as.matrix(data_clean[, features])
groups <- factor(data_clean[[group_var]])

%s

# 2. Create data frame for LDA
lda_data <- as.data.frame(X)
lda_data$Group <- groups

# 3. Perform LDA
lda_result <- lda(Group ~ ., data = lda_data)

# 4. Predict scores
lda_scores <- predict(lda_result)$x

# 5. Extract proportion of trace
prop_trace <- lda_result$svd^2 / sum(lda_result$svd^2) * 100

# Handle single discriminant case
if (ncol(lda_scores) == 1) {
    scores <- data.frame(
        LD1 = lda_scores[, 1],
        LD2 = 0,
        Group = groups
    )
    cat(sprintf("LD1: %%.1f%%%% of trace\\n", prop_trace[1]))
} else {
    ld1 <- min(%d, ncol(lda_scores))
    ld2 <- min(%d, ncol(lda_scores))
    scores <- data.frame(
        LD1 = lda_scores[, ld1],
        LD2 = lda_scores[, ld2],
        Group = groups
    )
    cat(sprintf("LD%d: %%.1f%%%% of trace\\n", prop_trace[ld1]))
    cat(sprintf("LD%d: %%.1f%%%% of trace\\n", prop_trace[ld2]))
}

# 6. Create biplot
p <- ggplot(scores, aes(x = LD1, y = LD2, color = Group)) +
    geom_point(size = 3, alpha = 0.7) +
    labs(
        title = "LDA Biplot",
        x = sprintf("LD%d (%%.1f%%%% trace)", prop_trace[%d]),
        y = if (ncol(lda_scores) > 1) sprintf("LD%d (%%.1f%%%% trace)", prop_trace[%d]) else ""
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    theme_bw()

print(p)

# 7. Scaling coefficients (variable contributions)
print(lda_result$scaling)

# ============================================',
                    feature_str, groupVar,
                    if (centerScale) "# Center and scale features\nX <- scale(X)" else "# Features used as-is",
                    pc1, pc2, pc1, pc2
                )
            }

            # Format with syntax highlighting hints
            r_code_html <- paste0(
                "<div style='background: #f5f5f5; padding: 1em; border: 1px solid #ddd; border-radius: 4px;'>",
                "<h4>📝 Copy-Ready R Code</h4>",
                "<p><b>Instructions:</b></p>",
                "<ul>",
                "<li>This code uses <b>upstream packages</b> (stats, mixOmics, MASS) directly</li>",
                "<li>Replace <code>mydata</code> with your actual data frame name</li>",
                "<li>Adjust parameters as needed for your analysis</li>",
                "<li><b>Copy:</b> Click and drag to select all text, then Ctrl+C (Cmd+C on Mac)</li>",
                "</ul>",
                "<pre style='background: white; padding: 1em; overflow-x: auto; border: 1px solid #ccc;'><code>",
                htmltools::htmlEscape(r_code),
                "</code></pre>",
                "<p style='margin-top: 1em;'><i>💡 Tip: This code is fully reproducible and can be shared with colleagues who don't use jamovi.</i></p>",
                "</div>"
            )

            self$results$rCode$setContent(r_code_html)
        },

        #---------------------------------------------
        .biplot = function(image, ggtheme, theme, ...) {

            if (is.null(private$.pca_result) && is.null(private$.plsda_result) && is.null(private$.lda_result)) {
                return()
            }

            method <- self$options$method

            if (method == "pca" && !is.null(private$.pca_result)) {
                private$.biplotPCA(image, ggtheme, theme, ...)
            } else if (method == "plsda" && !is.null(private$.plsda_result)) {
                private$.biplotPLSDA(image, ggtheme, theme, ...)
            } else if (method == "lda" && !is.null(private$.lda_result)) {
                private$.biplotLDA(image, ggtheme, theme, ...)
            }
        },

        #---------------------------------------------
        .biplotPCA = function(image, ggtheme, theme, ...) {

            library(ggplot2)

            pca_result <- private$.pca_result
            group_factor <- private$.group_factor

            pc1 <- self$options$pc1
            pc2 <- self$options$pc2

            # Get scores
            scores <- as.data.frame(pca_result$x[, c(pc1, pc2)])
            colnames(scores) <- c("PC1", "PC2")
            scores$Group <- group_factor

            # Get loadings
            loadings <- as.data.frame(pca_result$rotation[, c(pc1, pc2)])
            colnames(loadings) <- c("PC1", "PC2")
            loadings$Variable <- private$.feature_names

            # Scale loadings based on biplot type
            scale_factor <- self$options$loadingScale
            if (self$options$biplotType == "correlation") {
                # Correlation biplot: scale by sqrt(eigenvalue)
                loadings$PC1 <- loadings$PC1 * pca_result$sdev[pc1] * scale_factor * max(abs(scores$PC1))
                loadings$PC2 <- loadings$PC2 * pca_result$sdev[pc2] * scale_factor * max(abs(scores$PC2))
            } else {
                # Covariance biplot: standard scaling
                loadings$PC1 <- loadings$PC1 * scale_factor * max(abs(scores$PC1))
                loadings$PC2 <- loadings$PC2 * scale_factor * max(abs(scores$PC2))
            }

            # Filter top contributors
            if (self$options$topContributors > 0 && self$options$topContributors < nrow(loadings)) {
                loadings$contrib <- sqrt(loadings$PC1^2 + loadings$PC2^2)
                loadings <- loadings[order(-loadings$contrib), ][1:self$options$topContributors, ]
            }

            # Create plot
            p <- ggplot(scores, aes(x = PC1, y = PC2, color = Group)) +
                geom_point(size = self$options$pointSize, alpha = 0.7)

            # Add point labels if requested
            if (self$options$labelPoints != "none") {
                if (self$options$labelPoints == "rownum") {
                    scores$Label <- seq_len(nrow(scores))
                } else if (self$options$labelPoints == "rowname") {
                    scores$Label <- rownames(pca_result$x[, c(pc1, pc2)])
                    if (is.null(scores$Label)) scores$Label <- seq_len(nrow(scores))
                }

                p <- p + geom_text(aes(label = Label), size = 2.5,
                                  hjust = -0.2, vjust = -0.2,
                                  check_overlap = TRUE, show.legend = FALSE)
            }

            # Add confidence ellipses
            if (self$options$showConfidenceEllipse) {
                p <- p + stat_ellipse(level = 0.95, linetype = 2)
            }

            # Add variable vectors
            if (self$options$showLoadings) {
                p <- p +
                    geom_segment(data = loadings,
                                aes(x = 0, y = 0, xend = PC1, yend = PC2),
                                arrow = arrow(length = unit(0.3, "cm")),
                                color = "darkred", size = 0.8, inherit.aes = FALSE) +
                    geom_text(data = loadings,
                             aes(x = PC1 * 1.1, y = PC2 * 1.1, label = Variable),
                             color = "darkred", size = 3, inherit.aes = FALSE)
            }

            # Labels
            var1 <- (pca_result$sdev[pc1]^2) / sum(pca_result$sdev^2) * 100
            var2 <- (pca_result$sdev[pc2]^2) / sum(pca_result$sdev^2) * 100

            biplot_type_label <- if (self$options$biplotType == "correlation") "Correlation" else "Covariance"

            p <- p +
                labs(title = paste0("PCA ", biplot_type_label, " Biplot"),
                     x = paste0("PC", pc1, " (", round(var1, 1), "%)"),
                     y = paste0("PC", pc2, " (", round(var2, 1), "%)")) +
                theme_minimal(base_size = 12) +
                theme(legend.position = "right")

            print(p)
            TRUE
        },

        #---------------------------------------------
        .screeplot = function(image, ggtheme, theme, ...) {

            if (is.null(private$.pca_result)) return()

            library(ggplot2)

            pca_result <- private$.pca_result
            var_explained <- (pca_result$sdev^2) / sum(pca_result$sdev^2) * 100

            # Create data frame
            scree_df <- data.frame(
                Component = factor(1:length(var_explained), levels = 1:length(var_explained)),
                Variance = var_explained
            )

            # Limit to components explaining meaningful variance
            scree_df <- scree_df[1:min(20, nrow(scree_df)), ]

            p <- ggplot(scree_df, aes(x = Component, y = Variance)) +
                geom_bar(stat = "identity", fill = "steelblue") +
                geom_line(aes(group = 1), color = "red", size = 1) +
                geom_point(color = "red", size = 2) +
                labs(title = "Scree Plot - Variance Explained",
                     x = "Principal Component",
                     y = "% Variance Explained") +
                theme_minimal(base_size = 12) +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))

            print(p)
            TRUE
        },

        #---------------------------------------------
        .loadingPlot = function(image, ggtheme, theme, ...) {

            if (is.null(private$.pca_result)) return()

            library(ggplot2)

            pca_result <- private$.pca_result
            pc1 <- self$options$pc1
            pc2 <- self$options$pc2

            loadings <- as.data.frame(pca_result$rotation[, c(pc1, pc2)])
            colnames(loadings) <- c("PC1", "PC2")
            loadings$Variable <- private$.feature_names
            loadings$Contribution <- sqrt(loadings$PC1^2 + loadings$PC2^2)

            # Sort by contribution
            loadings <- loadings[order(-loadings$Contribution), ]

            # Limit display
            if (nrow(loadings) > 20) loadings <- loadings[1:20, ]

            loadings$Variable <- factor(loadings$Variable,
                                        levels = loadings$Variable[order(loadings$Contribution)])

            p <- ggplot(loadings, aes(x = Variable, y = Contribution)) +
                geom_bar(stat = "identity", fill = "darkgreen") +
                coord_flip() +
                labs(title = "Variable Contribution to Selected Components",
                     x = "Variable",
                     y = "Contribution (vector length)") +
                theme_minimal(base_size = 12)

            print(p)
            TRUE
        },

        # PLS-DA variance table
        .populateVarianceTablePLSDA = function(result) {
            varTable <- self$results$componentVariance

            # PLS-DA explained variance per component
            expl_var <- result$explained_variance$X
            cumvar <- cumsum(expl_var)

            n_comp <- min(which(cumvar >= self$options$minVariance)[1], length(expl_var))
            if (is.na(n_comp)) n_comp <- length(expl_var)

            for (i in 1:n_comp) {
                varTable$addRow(rowKey = i, values = list(
                    component = paste0("Comp", i),
                    eigenvalue = NA,  # Not directly available in PLS-DA
                    variance = expl_var[i],
                    cumulative = cumvar[i]
                ))
            }
        },

        .populateContributionTablePLSDA = function(result, feature_names) {
            contribTable <- self$results$contributionTable

            pc1 <- self$options$pc1
            pc2 <- self$options$pc2

            # Get loadings from PLS-DA
            loadings <- result$loadings$X

            # Calculate contributions
            metric <- self$options$contributionMetric

            if (metric == "loading") {
                contrib_pc1 <- abs(loadings[, pc1])
                contrib_pc2 <- abs(loadings[, pc2])
            } else if (metric == "squared") {
                contrib_pc1 <- (loadings[, pc1]^2) / sum(loadings[, pc1]^2) * 100
                contrib_pc2 <- (loadings[, pc2]^2) / sum(loadings[, pc2]^2) * 100
            } else { # correlation
                contrib_pc1 <- abs(loadings[, pc1])
                contrib_pc2 <- abs(loadings[, pc2])
            }

            total_contrib <- contrib_pc1 + contrib_pc2
            quality <- (loadings[, pc1]^2 + loadings[, pc2]^2)

            # Create data frame and sort
            contrib_df <- data.frame(
                variable = feature_names,
                pc1_contrib = contrib_pc1,
                pc2_contrib = contrib_pc2,
                total_contrib = total_contrib,
                quality = quality,
                stringsAsFactors = FALSE
            )

            contrib_df <- contrib_df[order(-contrib_df$total_contrib), ]

            # Populate table
            for (i in 1:nrow(contrib_df)) {
                contribTable$addRow(rowKey = i, values = list(
                    variable = contrib_df$variable[i],
                    pc1_contrib = contrib_df$pc1_contrib[i],
                    pc2_contrib = contrib_df$pc2_contrib[i],
                    total_contrib = contrib_df$total_contrib[i],
                    quality = contrib_df$quality[i]
                ))
            }

            # Add clinical interpretation
            private$.addContributionInterpretation(contrib_df)
        },

        .populateVarianceTableLDA = function(result) {
            varTable <- self$results$componentVariance

            # LDA: proportion of trace (between-group variance)
            prop_trace <- result$svd^2 / sum(result$svd^2) * 100
            cumvar <- cumsum(prop_trace)

            n_disc <- length(prop_trace)

            for (i in 1:n_disc) {
                varTable$addRow(rowKey = i, values = list(
                    component = paste0("LD", i),
                    eigenvalue = result$svd[i]^2,
                    variance = prop_trace[i],
                    cumulative = cumvar[i]
                ))
            }
        },

        .populateContributionTableLDA = function(result, feature_names) {
            contribTable <- self$results$contributionTable

            pc1 <- self$options$pc1
            pc2 <- self$options$pc2

            # Get scaling coefficients from LDA
            scaling <- result$scaling

            # Ensure we don't exceed available dimensions
            max_dim <- ncol(scaling)
            if (pc1 > max_dim) pc1 <- 1
            if (pc2 > max_dim) pc2 <- min(2, max_dim)

            # Calculate contributions
            metric <- self$options$contributionMetric

            if (metric == "loading") {
                contrib_pc1 <- abs(scaling[, pc1])
                contrib_pc2 <- if (pc2 <= max_dim) abs(scaling[, pc2]) else rep(0, nrow(scaling))
            } else if (metric == "squared") {
                contrib_pc1 <- (scaling[, pc1]^2) / sum(scaling[, pc1]^2) * 100
                contrib_pc2 <- if (pc2 <= max_dim) (scaling[, pc2]^2) / sum(scaling[, pc2]^2) * 100 else rep(0, nrow(scaling))
            } else { # correlation
                contrib_pc1 <- abs(scaling[, pc1])
                contrib_pc2 <- if (pc2 <= max_dim) abs(scaling[, pc2]) else rep(0, nrow(scaling))
            }

            total_contrib <- contrib_pc1 + contrib_pc2
            quality <- if (pc2 <= max_dim) (scaling[, pc1]^2 + scaling[, pc2]^2) else scaling[, pc1]^2

            # Create data frame and sort
            contrib_df <- data.frame(
                variable = feature_names,
                pc1_contrib = contrib_pc1,
                pc2_contrib = contrib_pc2,
                total_contrib = total_contrib,
                quality = quality,
                stringsAsFactors = FALSE
            )

            contrib_df <- contrib_df[order(-contrib_df$total_contrib), ]

            # Populate table
            for (i in 1:nrow(contrib_df)) {
                contribTable$addRow(rowKey = i, values = list(
                    variable = contrib_df$variable[i],
                    pc1_contrib = contrib_df$pc1_contrib[i],
                    pc2_contrib = contrib_df$pc2_contrib[i],
                    total_contrib = contrib_df$total_contrib[i],
                    quality = contrib_df$quality[i]
                ))
            }

            # Add clinical interpretation
            private$.addContributionInterpretation(contrib_df)
        },

        .biplotPLSDA = function(image, ggtheme, theme, ...) {
            plsda_result <- private$.plsda_result
            if (is.null(plsda_result)) return()

            pc1 <- self$options$pc1
            pc2 <- self$options$pc2

            # Extract scores from PLS-DA result
            scores <- as.data.frame(plsda_result$variates$X[, c(pc1, pc2)])
            colnames(scores) <- c("PC1", "PC2")
            scores$Group <- private$.group_factor

            # Extract loadings
            loadings <- as.data.frame(plsda_result$loadings$X[, c(pc1, pc2)])
            colnames(loadings) <- c("PC1", "PC2")
            loadings$Variable <- private$.feature_names

            # Scale loadings based on biplot type
            scale_factor <- self$options$loadingScale
            if (self$options$biplotType == "correlation") {
                # Correlation biplot: scale by component importance
                comp_importance <- apply(plsda_result$variates$X, 2, sd)
                loadings$PC1 <- loadings$PC1 * comp_importance[pc1] * scale_factor * max(abs(scores$PC1))
                loadings$PC2 <- loadings$PC2 * comp_importance[pc2] * scale_factor * max(abs(scores$PC2))
            } else {
                # Covariance biplot
                loadings$PC1 <- loadings$PC1 * scale_factor * max(abs(scores$PC1))
                loadings$PC2 <- loadings$PC2 * scale_factor * max(abs(scores$PC2))
            }

            # Get variance explained
            expl_var <- plsda_result$explained_variance$X
            var1 <- round(expl_var[pc1], 2)
            var2 <- round(expl_var[pc2], 2)

            # Create base plot
            p <- ggplot2::ggplot(scores, ggplot2::aes(x = PC1, y = PC2, color = Group)) +
                ggplot2::geom_point(size = self$options$pointSize, alpha = 0.7) +
                ggplot2::labs(
                    title = paste0("PLS-DA ",
                                   if (self$options$biplotType == "correlation") "Correlation" else "Covariance",
                                   " Biplot"),
                    subtitle = paste("Component", pc1, "vs Component", pc2),
                    x = paste0("Component ", pc1, " (", var1, "% variance)"),
                    y = paste0("Component ", pc2, " (", var2, "% variance)")
                ) +
                ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
                ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray50")

            # Add confidence ellipses
            if (self$options$showConfidenceEllipse) {
                p <- p + ggplot2::stat_ellipse(level = 0.95, type = "norm")
            }

            # Add point labels if requested
            if (self$options$labelPoints != "none") {
                if (self$options$labelPoints == "rownum") {
                    scores$Label <- seq_len(nrow(scores))
                } else if (self$options$labelPoints == "rowname") {
                    scores$Label <- rownames(plsda_result$variates$X[, c(pc1, pc2)])
                    if (is.null(scores$Label)) scores$Label <- seq_len(nrow(scores))
                }
                p <- p + ggplot2::geom_text(ggplot2::aes(label = Label), size = 2.5,
                                            hjust = -0.2, vjust = -0.2,
                                            check_overlap = TRUE, show.legend = FALSE)
            }

            # Add loadings if requested
            if (self$options$showLoadings && self$options$topContributors > 0) {
                # Calculate contribution for sorting
                loadings$contrib <- sqrt(loadings$PC1^2 + loadings$PC2^2)
                loadings <- loadings[order(-loadings$contrib), ]

                # Keep only top contributors
                n_top <- min(self$options$topContributors, nrow(loadings))
                loadings_top <- loadings[1:n_top, ]

                # Add loading vectors
                p <- p +
                    ggplot2::geom_segment(
                        data = loadings_top,
                        ggplot2::aes(x = 0, y = 0, xend = PC1, yend = PC2),
                        arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm")),
                        color = "darkred", alpha = 0.6, inherit.aes = FALSE
                    ) +
                    ggplot2::geom_text(
                        data = loadings_top,
                        ggplot2::aes(x = PC1, y = PC2, label = Variable),
                        color = "darkred", size = 3,
                        hjust = ifelse(loadings_top$PC1 > 0, -0.1, 1.1),
                        vjust = ifelse(loadings_top$PC2 > 0, -0.3, 1.3),
                        inherit.aes = FALSE
                    )
            }

            # Apply theme
            p <- p + ggtheme

            print(p)
            TRUE
        },

        .biplotLDA = function(image, ggtheme, theme, ...) {
            lda_result <- private$.lda_result
            lda_scores <- private$.lda_scores
            if (is.null(lda_result) || is.null(lda_scores)) return()

            pc1 <- self$options$pc1
            pc2 <- self$options$pc2

            # Ensure we don't exceed available dimensions
            max_dim <- ncol(lda_scores)
            if (pc1 > max_dim) {
                pc1 <- 1
                warning("Requested pc1 exceeds available dimensions, using LD1")
            }
            if (pc2 > max_dim) {
                pc2 <- min(2, max_dim)
                warning("Requested pc2 exceeds available dimensions, using LD", pc2)
            }

            # Handle single discriminant case
            if (max_dim == 1) {
                # For single discriminant, create a dummy second dimension
                scores <- data.frame(
                    PC1 = lda_scores[, 1],
                    PC2 = rep(0, nrow(lda_scores)),
                    Group = private$.group_factor
                )
                pc2 <- 1  # Use same dimension for plotting
            } else {
                scores <- data.frame(
                    PC1 = lda_scores[, pc1],
                    PC2 = lda_scores[, pc2],
                    Group = private$.group_factor
                )
            }

            # Extract loadings (scaling matrix)
            scaling <- lda_result$scaling
            if (max_dim == 1) {
                loadings <- data.frame(
                    PC1 = scaling[, 1],
                    PC2 = rep(0, nrow(scaling)),
                    Variable = private$.feature_names
                )
            } else {
                loadings <- data.frame(
                    PC1 = scaling[, pc1],
                    PC2 = if (pc2 <= max_dim) scaling[, pc2] else rep(0, nrow(scaling)),
                    Variable = private$.feature_names
                )
            }

            # Scale loadings based on biplot type
            scale_factor <- self$options$loadingScale
            if (self$options$biplotType == "correlation") {
                # Correlation biplot: scale by discriminant importance
                disc_importance <- if (!is.null(lda_result$svd)) lda_result$svd else rep(1, max_dim)
                loadings$PC1 <- loadings$PC1 * disc_importance[pc1] * scale_factor * max(abs(scores$PC1))
                if (max_dim > 1 && pc2 <= max_dim) {
                    loadings$PC2 <- loadings$PC2 * disc_importance[pc2] * scale_factor * max(abs(scores$PC2))
                }
            } else {
                # Covariance biplot
                loadings$PC1 <- loadings$PC1 * scale_factor * max(abs(scores$PC1))
                if (max_dim > 1) {
                    loadings$PC2 <- loadings$PC2 * scale_factor * max(abs(scores$PC2))
                }
            }

            # Calculate proportion of trace for variance labels
            prop_trace <- if (!is.null(lda_result$svd)) {
                lda_result$svd^2 / sum(lda_result$svd^2) * 100
            } else {
                rep(100 / max_dim, max_dim)
            }
            var1 <- round(prop_trace[pc1], 2)
            var2 <- if (max_dim > 1 && pc2 <= max_dim) round(prop_trace[pc2], 2) else 0

            # Create base plot
            subtitle_text <- if (max_dim == 1) {
                "Single Discriminant Function"
            } else {
                paste("Discriminant", pc1, "vs Discriminant", pc2)
            }

            p <- ggplot2::ggplot(scores, ggplot2::aes(x = PC1, y = PC2, color = Group)) +
                ggplot2::geom_point(size = self$options$pointSize, alpha = 0.7) +
                ggplot2::labs(
                    title = paste0("LDA ",
                                   if (self$options$biplotType == "correlation") "Correlation" else "Covariance",
                                   " Biplot"),
                    subtitle = subtitle_text,
                    x = paste0("LD", pc1, " (", var1, "% trace)"),
                    y = if (max_dim > 1) paste0("LD", pc2, " (", var2, "% trace)") else ""
                ) +
                ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
                ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray50")

            # Add confidence ellipses
            if (self$options$showConfidenceEllipse && max_dim > 1) {
                p <- p + ggplot2::stat_ellipse(level = 0.95, type = "norm")
            }

            # Add point labels if requested
            if (self$options$labelPoints != "none") {
                if (self$options$labelPoints == "rownum") {
                    scores$Label <- seq_len(nrow(scores))
                } else if (self$options$labelPoints == "rowname") {
                    scores$Label <- rownames(lda_scores)
                    if (is.null(scores$Label)) scores$Label <- seq_len(nrow(scores))
                }
                p <- p + ggplot2::geom_text(ggplot2::aes(label = Label), size = 2.5,
                                            hjust = -0.2, vjust = -0.2,
                                            check_overlap = TRUE, show.legend = FALSE)
            }

            # Add loadings if requested
            if (self$options$showLoadings && self$options$topContributors > 0) {
                # Calculate contribution for sorting
                loadings$contrib <- sqrt(loadings$PC1^2 + loadings$PC2^2)
                loadings <- loadings[order(-loadings$contrib), ]

                # Keep only top contributors
                n_top <- min(self$options$topContributors, nrow(loadings))
                loadings_top <- loadings[1:n_top, ]

                # Add loading vectors
                p <- p +
                    ggplot2::geom_segment(
                        data = loadings_top,
                        ggplot2::aes(x = 0, y = 0, xend = PC1, yend = PC2),
                        arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm")),
                        color = "darkred", alpha = 0.6, inherit.aes = FALSE
                    ) +
                    ggplot2::geom_text(
                        data = loadings_top,
                        ggplot2::aes(x = PC1, y = PC2, label = Variable),
                        color = "darkred", size = 3,
                        hjust = ifelse(loadings_top$PC1 > 0, -0.1, 1.1),
                        vjust = ifelse(loadings_top$PC2 > 0, -0.3, 1.3),
                        inherit.aes = FALSE
                    )
            }

            # Apply theme
            p <- p + ggtheme

            print(p)
            TRUE
        },

        # Storage for results
        .pca_result = NULL,
        .plsda_result = NULL,
        .lda_result = NULL,
        .lda_scores = NULL,
        .group_factor = NULL,
        .feature_names = NULL
    )
)
