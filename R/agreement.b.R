#' @title Interrater Reliability Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import magrittr
#' @import ggplot2
#' @import dplyr
#' @importFrom irr kappa2 kappam.fleiss agree
#' @importFrom psych ICC
#' @importFrom htmlTable htmlTable
#' @importFrom glue glue
#' @importFrom reshape2 melt
#' @importFrom scales percent
#' @importFrom grid pushViewport viewport
#'
#' @description
#' Interrater reliability analysis including Cohen's kappa, Fleiss' kappa,
#' Krippendorff's alpha, and agreement visualization.

agreementClass <- if (requireNamespace("jmvcore")) R6::R6Class(
    "agreementClass",
    inherit = agreementBase,
    private = list(
        # Private data storage
        .data_matrix = NULL,
        .rater_names = NULL,
        .categories = NULL,
        .n_cases = NULL,
        .n_raters = NULL,
        .agreement_results = NULL,
        .pairwise_results = NULL,
        .category_results = NULL,
        .style_clustering_results = NULL,

        # Initialization
        .init = function() {
            if (is.null(self$data) || length(self$options$vars) == 0) {
                todo <- "
                    <br>Welcome to Interrater Reliability Analysis
                    <br><br>
                    This tool provides analysis of agreement between multiple raters/observers.
                    <br><br>
                    To begin:
                    <ul>
                        <li>Select 2 or more rater variables (each column represents one rater)</li>
                        <li>Choose appropriate analysis options based on your data type</li>
                    </ul>
                    <br>
                    The analysis will calculate:
                    <ul>
                        <li>Cohen's kappa (2 raters) or Fleiss' kappa (3+ raters)</li>
                        <li>Krippendorff's alpha (optional)</li>
                        <li>Agreement heatmap visualization</li>
                        <li>Frequency tables for each rater</li>
                    </ul>
                "
                self$results$todo$setContent(todo)
            }
        },

        # Main analysis function
        .run = function() {
            # Early return if no variables selected
            if (is.null(self$options$vars) || length(self$options$vars) < 2) {
                return()
            }

            if (nrow(self$data) == 0) {
                stop('Data contains no (complete) rows')
            }

            # Prepare data
            private$.prepareData()

            # Clear todo message
            self$results$todo$setContent("")

            # Perform analyses
            private$.performOverviewAnalysis()
            private$.performKappaAnalysis()

            # if (self$options$icc) {
            #     private$.performICCAnalysis()
            # }

            if (self$options$kripp) {
                private$.performKrippendorffAnalysis()
            }

            # if (self$options$pairwiseAnalysis) {
            #     private$.performPairwiseAnalysis()
            # }

            # if (self$options$categoryAnalysis) {
            #     private$.performCategoryAnalysis()
            # }

            # if (self$options$outlierAnalysis) {
            #     private$.performOutlierAnalysis()
            # }

            # if (self$options$pathologyContext) {
            #     private$.performPathologyAnalysis()
            # }

            # if (self$options$showInterpretation) {
            #     private$.showInterpretationGuidelines()
            # }

            # if (self$options$diagnosticStyleAnalysis) {
            #     private$.performDiagnosticStyleAnalysis()
            # }

            if (self$options$sft) {
                private$.generateFrequencyTables()
            }
        },

        # Data preparation
        .prepareData = function() {
            # Get rater variables
            rater_vars <- self$options$vars
            private$.rater_names <- rater_vars

            # Extract data matrix
            data_subset <- self$data[rater_vars]

            # Remove rows with any missing values
            complete_cases <- complete.cases(data_subset)
            data_subset <- data_subset[complete_cases, , drop = FALSE]

            if (nrow(data_subset) == 0) {
                stop('No complete cases found. Please check for missing values.')
            }

            private$.data_matrix <- data_subset
            private$.n_cases <- nrow(data_subset)
            private$.n_raters <- ncol(data_subset)

            # Get unique categories across all raters
            all_values <- unlist(data_subset)
            private$.categories <- sort(unique(all_values))
        },

        # Overview analysis
        .performOverviewAnalysis = function() {
            # Calculate overall agreement percentage
            agreement_count <- 0
            total_cases <- private$.n_cases

            for (i in 1:total_cases) {
                case_values <- as.character(private$.data_matrix[i, ])
                if (length(unique(case_values)) == 1) {
                    agreement_count <- agreement_count + 1
                }
            }

            overall_agreement <- (agreement_count / total_cases) * 100

            # Determine primary method
            primary_method <- if (private$.n_raters == 2) {
                "Cohen's Kappa"
            } else {
                "Fleiss' Kappa"
            }

            # Populate overview table
            overview <- self$results$overviewTable
            overview$setRow(rowNo = 1, values = list(
                cases = private$.n_cases,
                raters = private$.n_raters,
                categories = length(private$.categories),
                overall_agreement = overall_agreement,
                primary_method = primary_method
            ))
        },

        # Kappa analysis
        .performKappaAnalysis = function() {
            kappa_table <- self$results$kappaTable

            if (private$.n_raters == 2) {
                # Cohen's kappa for 2 raters
                private$.performCohensKappa(kappa_table)
            } else {
                # Fleiss' kappa for 3+ raters
                private$.performFleissKappa(kappa_table)
            }
        },

        # Cohen's kappa (2 raters)
        .performCohensKappa = function(table) {
            wght <- self$options$wght
            conf_level <- 0.95 # self$options$confidenceLevel

            # Check if weighting is appropriate
            if (wght %in% c("equal", "squared")) {
                # Check if variables are ordinal
                var_types <- sapply(private$.data_matrix, is.ordered)
                if (!all(var_types)) {
                    stop("Weighted kappa requires ordinal variables. Please ensure your variables are properly ordered factors.")
                }
            }

            # Calculate Cohen's kappa
            result <- irr::kappa2(private$.data_matrix, weight = wght)

            # Interpretation
            interpretation <- private$.interpretKappa(result$value)

            # Validate variance estimate and calculate standard error
            se_value <- NA
            ci_lower <- NA
            ci_upper <- NA

            # Calculate standard error from z-statistic if available
            # For Cohen's kappa: SE = kappa / z_statistic
            if (!is.null(result$statistic) && is.numeric(result$statistic) &&
                !is.na(result$statistic) && result$statistic != 0 &&
                !is.null(result$value) && is.numeric(result$value) && !is.na(result$value)) {
                se_value <- abs(result$value / result$statistic)
                ci_lower <- result$value - qnorm((1 + conf_level)/2) * se_value
                ci_upper <- result$value + qnorm((1 + conf_level)/2) * se_value
            } else {
                # Fallback: try to extract variance from different possible structures
                var_kappa <- NULL
                if (!is.null(result$var.kappa)) {
                    var_kappa <- result$var.kappa
                } else if (!is.null(result$variance)) {
                    var_kappa <- result$variance
                } else if (!is.null(result$se) && is.numeric(result$se)) {
                    var_kappa <- result$se^2
                }

                if (!is.null(var_kappa) && is.numeric(var_kappa) &&
                    length(var_kappa) > 0 && !is.na(var_kappa) && var_kappa >= 0) {
                    se_value <- sqrt(var_kappa)
                    ci_lower <- result$value - qnorm((1 + conf_level)/2) * se_value
                    ci_upper <- result$value + qnorm((1 + conf_level)/2) * se_value
                }
            }

            # Add to table
            table$addRow(rowKey = "cohens", values = list(
                method = "Cohen's Kappa",
                kappa = result$value,
                se = se_value,
                ci_lower = ci_lower,
                ci_upper = ci_upper,
                # Original code (commented out due to non-numeric error):
                # se = sqrt(result$var.kappa),
                # ci_lower = result$value - qnorm((1 + conf_level)/2) * sqrt(result$var.kappa),
                # ci_upper = result$value + qnorm((1 + conf_level)/2) * sqrt(result$var.kappa),
                z = result$statistic,
                p = result$p.value,
                interpretation = interpretation
            ))
        },

        # Fleiss' kappa (3+ raters)
        .performFleissKappa = function(table) {
            exact <- self$options$exct
            conf_level <- 0.95 # self$options$confidenceLevel

            # Calculate Fleiss' kappa
            result <- irr::kappam.fleiss(private$.data_matrix, exact = exact, detail = TRUE)

            # Interpretation
            interpretation <- private$.interpretKappa(result$value)

            # Calculate standard error and confidence interval
            se_value <- NA
            ci_lower <- NA
            ci_upper <- NA

            # Calculate standard error from z-statistic if available (same method as Cohen's kappa)
            if (!is.null(result$statistic) && is.numeric(result$statistic) &&
                !is.na(result$statistic) && result$statistic != 0 &&
                !is.null(result$value) && is.numeric(result$value) && !is.na(result$value)) {
                se_value <- abs(result$value / result$statistic)
                ci_lower <- result$value - qnorm((1 + conf_level)/2) * se_value
                ci_upper <- result$value + qnorm((1 + conf_level)/2) * se_value
            }

            # Determine method name based on exact calculation
            method_name <- if (exact) {
                "Fleiss' Kappa (Exact)"
            } else {
                "Fleiss' Kappa"
            }

            # Add to table
            table$addRow(rowKey = "fleiss", values = list(
                method = method_name,
                kappa = result$value,
                se = se_value,
                ci_lower = ci_lower,
                ci_upper = ci_upper,
                # Original code (commented out due to non-numeric error):
                # se = sqrt(result$var.kappa),
                # ci_lower = result$value - qnorm((1 + conf_level)/2) * sqrt(result$var.kappa),
                # ci_upper = result$value + qnorm((1 + conf_level)/2) * sqrt(result$var.kappa),
                z = result$statistic,
                p = result$p.value,
                interpretation = interpretation
            ))
        },

        # # ICC analysis - commented out for future release
        # .performICCAnalysis = function() {
        #     icc_table <- self$results$iccTable
        #     icc_type <- self$options$iccType
        #     conf_level <- 0.95 # self$options$confidenceLevel
        #
        #     # Convert data to numeric for ICC
        #     numeric_data <- apply(private$.data_matrix, 2, function(x) as.numeric(as.factor(x)))
        #
        #     # Calculate ICC based on selected type
        #     icc_formula <- switch(icc_type,
        #         "ICC1" = "1",
        #         "ICC2" = "2",
        #         "ICC3" = "3",
        #         "ICC1k" = "1k",
        #         "ICC2k" = "2k",
        #         "ICC3k" = "3k"
        #     )
        #
        #     tryCatch({
        #         icc_result <- psych::ICC(numeric_data, alpha = 1 - conf_level)
        #
        #         # Extract specific ICC type
        #         icc_row <- switch(icc_type,
        #             "ICC1" = 1,
        #             "ICC2" = 2,
        #             "ICC3" = 3,
        #             "ICC1k" = 4,
        #             "ICC2k" = 5,
        #             "ICC3k" = 6
        #         )
        #
        #         icc_value <- icc_result$results[icc_row, "ICC"]
        #         ci_lower <- icc_result$results[icc_row, "lower bound"]
        #         ci_upper <- icc_result$results[icc_row, "upper bound"]
        #         f_value <- icc_result$results[icc_row, "F"]
        #         p_value <- icc_result$results[icc_row, "p"]
        #
        #         # Interpretation
        #         interpretation <- private$.interpretICC(icc_value)
        #
        #         # Add to table
        #         icc_table$addRow(rowKey = icc_type, values = list(
        #             type = private$.getICCTypeDescription(icc_type),
        #             icc_value = icc_value,
        #             ci_lower = ci_lower,
        #             ci_upper = ci_upper,
        #             f_value = f_value,
        #             p = p_value,
        #             interpretation = interpretation
        #         ))
        #     }, error = function(e) {
        #         # Add error message to table
        #         icc_table$addRow(rowKey = "error", values = list(
        #             type = "Error",
        #             icc_value = NaN,
        #             ci_lower = NaN,
        #             ci_upper = NaN,
        #             f_value = NaN,
        #             p = NaN,
        #             interpretation = paste("Error calculating ICC:", e$message)
        #         ))
        #     })
        # },

        # Krippendorff's alpha analysis
        .performKrippendorffAnalysis = function() {
            kripp_table <- self$results$krippTable
            kripp_method <- self$options$krippMethod
            bootstrap <- FALSE # self$options$bootstrap

            # Try to calculate Krippendorff's alpha
            alpha_value <- NaN
            interpretation <- "Krippendorff's alpha calculation failed"

            tryCatch({
                # Convert data to format expected by Krippendorff's alpha
                # Data should be in format: rows = raters, columns = subjects
                kripp_data <- t(private$.data_matrix)

                # Try using irr package kripp.alpha function
                if (requireNamespace("irr", quietly = TRUE) && exists("kripp.alpha", where = asNamespace("irr"))) {
                    # Convert method name to irr package format
                    method_map <- list(
                        "nominal" = "nominal",
                        "ordinal" = "ordinal",
                        "interval" = "interval",
                        "ratio" = "ratio"
                    )
                    irr_method <- method_map[[kripp_method]] %||% "nominal"

                    result <- irr::kripp.alpha(kripp_data, method = irr_method)
                    if (!is.null(result) && !is.null(result$value) && is.numeric(result$value) && !is.na(result$value)) {
                        alpha_value <- result$value
                        interpretation <- private$.interpretKrippendorff(alpha_value)
                    }
                } else {
                    # Fallback: basic implementation for nominal data
                    if (kripp_method == "nominal") {
                        alpha_value <- private$.calculateBasicKrippendorffNominal(kripp_data)
                        if (!is.na(alpha_value)) {
                            interpretation <- private$.interpretKrippendorff(alpha_value)
                        }
                    } else {
                        interpretation <- "Krippendorff's alpha for non-nominal data requires full irr package implementation"
                    }
                }
            }, error = function(e) {
                alpha_value <<- NaN
                interpretation <<- paste("Error calculating Krippendorff's alpha:", e$message)
            })

            # Add result to table
            kripp_table$addRow(rowKey = "kripp", values = list(
                data_type = kripp_method,
                alpha = alpha_value,
                ci_lower = if (bootstrap) NaN else NULL,
                ci_upper = if (bootstrap) NaN else NULL,
                interpretation = interpretation
            ))
        },

        # # Pairwise analysis - commented out for future release
        # .performPairwiseAnalysis = function() {
        #     pairwise_table <- self$results$pairwiseTable
        #     conf_level <- 0.95 # self$options$confidenceLevel
        #
        #     n_raters <- private$.n_raters
        #     rater_names <- private$.rater_names
        #
        #     # Calculate pairwise agreements
        #     for (i in 1:(n_raters - 1)) {
        #         for (j in (i + 1):n_raters) {
        #             # Extract pair data
        #             pair_data <- private$.data_matrix[, c(i, j)]
        #
        #             # Calculate agreement percentage
        #             agreement_count <- sum(pair_data[, 1] == pair_data[, 2])
        #             agreement_percent <- (agreement_count / nrow(pair_data)) * 100
        #
        #             # Calculate Cohen's kappa for this pair
        #             kappa_result <- irr::kappa2(pair_data, weight = self$options$wght)
        #
        #             # Confidence interval
        #             se <- sqrt(kappa_result$var.kappa)
        #             ci_lower <- kappa_result$value - qnorm((1 + conf_level)/2) * se
        #             ci_upper <- kappa_result$value + qnorm((1 + conf_level)/2) * se
        #
        #             # Interpretation
        #             interpretation <- private$.interpretKappa(kappa_result$value)
        #
        #             # Add to table
        #             pair_name <- paste(rater_names[i], "vs", rater_names[j])
        #             pairwise_table$addRow(rowKey = pair_name, values = list(
        #                 rater_pair = pair_name,
        #                 agreement_percent = agreement_percent,
        #                 kappa = kappa_result$value,
        #                 ci_lower = ci_lower,
        #                 ci_upper = ci_upper,
        #                 p = kappa_result$p.value,
        #                 interpretation = interpretation
        #             ))
        #         }
        #     }
        # },

        # # Category-specific analysis - commented out for future release
        # .performCategoryAnalysis = function() {
        #     category_table <- self$results$categoryTable
        #
        #     categories <- private$.categories
        #     data_matrix <- private$.data_matrix
        #
        #     for (category in categories) {
        #         # Calculate frequency
        #         frequency <- sum(apply(data_matrix, 1, function(row) any(row == category)))
        #
        #         # Create binary version for this category
        #         binary_data <- apply(data_matrix, 2, function(col) ifelse(col == category, 1, 0))
        #
        #         # Calculate agreement for this category
        #         agreement_count <- sum(apply(binary_data, 1, function(row) length(unique(row)) == 1))
        #         agreement_percent <- (agreement_count / nrow(binary_data)) * 100
        #
        #         # Calculate category-specific kappa
        #         tryCatch({
        #             if (private$.n_raters == 2) {
        #                 kappa_result <- irr::kappa2(data.frame(binary_data))
        #                 category_kappa <- kappa_result$value
        #             } else {
        #                 kappa_result <- irr::kappam.fleiss(data.frame(binary_data))
        #                 category_kappa <- kappa_result$value
        #             }
        #         }, error = function(e) {
        #             category_kappa <- NaN
        #         })
        #
        #         # Add to table
        #         category_table$addRow(rowKey = category, values = list(
        #             category = as.character(category),
        #             frequency = frequency,
        #             agreement_percent = agreement_percent,
        #             kappa = category_kappa,
        #             sensitivity = if (self$options$pathologyContext) NaN else NULL,
        #             specificity = if (self$options$pathologyContext) NaN else NULL
        #         ))
        #     }
        # },

        # # Outlier analysis - commented out for future release
        # .performOutlierAnalysis = function() {
        #     outlier_table <- self$results$outlierTable
        #
        #     data_matrix <- private$.data_matrix
        #     n_cases <- private$.n_cases
        #
        #     # Calculate disagreement score for each case
        #     case_scores <- numeric(n_cases)
        #
        #     for (i in 1:n_cases) {
        #         case_values <- as.character(data_matrix[i, ])
        #         unique_values <- unique(case_values)
        #
        #         # Disagreement score: proportion of raters not agreeing with mode
        #         if (length(unique_values) == 1) {
        #             case_scores[i] <- 0  # Perfect agreement
        #         } else {
        #             # Find mode
        #             value_counts <- table(case_values)
        #             mode_count <- max(value_counts)
        #             disagreement_count <- private$.n_raters - mode_count
        #             case_scores[i] <- disagreement_count / private$.n_raters
        #         }
        #     }
        #
        #     # Identify outliers (cases with high disagreement)
        #     threshold <- quantile(case_scores, 0.8)  # Top 20% disagreement
        #     outlier_indices <- which(case_scores >= threshold & case_scores > 0)
        #
        #     # Add outlier cases to table
        #     for (idx in outlier_indices) {
        #         case_id <- if (!is.null(self$options$caseID)) {
        #             as.character(self$data[[self$options$caseID]][idx])
        #         } else {
        #             paste("Case", idx)
        #         }
        #
        #         case_values <- as.character(data_matrix[idx, ])
        #         disagreement_count <- sum(case_values != names(sort(table(case_values), decreasing = TRUE))[1])
        #
        #         outlier_table$addRow(rowKey = case_id, values = list(
        #             case_id = case_id,
        #             disagreement_count = disagreement_count,
        #             agreement_score = 1 - case_scores[idx],
        #             rater_assignments = paste(case_values, collapse = ", "),
        #             consensus_diagnosis = if (self$options$pathologyContext) "TBD" else NULL
        #         ))
        #     }
        # },

        # # Pathology-specific analysis - commented out for future release
        # .performPathologyAnalysis = function() {
        #     if (is.null(self$options$diagnosisVar)) {
        #         return()
        #     }
        #
        #     diagnostic_table <- self$results$diagnosticAccuracyTable
        #
        #     # Get gold standard diagnoses
        #     gold_standard <- self$data[[self$options$diagnosisVar]]
        #     complete_cases <- complete.cases(private$.data_matrix)
        #     gold_standard <- gold_standard[complete_cases]
        #
        #     # Calculate diagnostic accuracy for each rater
        #     rater_names <- private$.rater_names
        #
        #     for (i in 1:length(rater_names)) {
        #         rater_diagnoses <- private$.data_matrix[, i]
        #
        #         # Calculate accuracy metrics
        #         accuracy <- sum(rater_diagnoses == gold_standard) / length(gold_standard) * 100
        #
        #         # Calculate sensitivity, specificity, PPV, NPV for each category
        #         # (This is simplified - full implementation would handle multi-class metrics)
        #
        #         # Placeholder values
        #         sensitivity <- NaN
        #         specificity <- NaN
        #         ppv <- NaN
        #         npv <- NaN
        #
        #         # Kappa vs gold standard
        #         kappa_vs_gold <- tryCatch({
        #             kappa_result <- irr::kappa2(data.frame(
        #                 gold = gold_standard,
        #                 rater = rater_diagnoses
        #             ))
        #             kappa_result$value
        #         }, error = function(e) NaN)
        #
        #         diagnostic_table$addRow(rowKey = rater_names[i], values = list(
        #             rater = rater_names[i],
        #             accuracy = accuracy,
        #             sensitivity = sensitivity,
        #             specificity = specificity,
        #             ppv = ppv,
        #             npv = npv,
        #             kappa_vs_gold = kappa_vs_gold
        #         ))
        #     }
        # },

        # # Show interpretation guidelines - commented out for future release
        # .showInterpretationGuidelines = function() {
        #     interpretation_table <- self$results$interpretationTable
        #
        #     guidelines <- list(
        #         list("< 0.00", "Poor", "Less than chance agreement"),
        #         list("0.00 - 0.20", "Slight", "Minimal agreement"),
        #         list("0.21 - 0.40", "Fair", "Weak agreement"),
        #         list("0.41 - 0.60", "Moderate", "Acceptable agreement"),
        #         list("0.61 - 0.80", "Substantial", "Good agreement"),
        #         list("0.81 - 1.00", "Almost Perfect", "Excellent agreement")
        #     )
        #
        #     for (i in 1:length(guidelines)) {
        #         interpretation_table$addRow(rowKey = i, values = list(
        #             kappa_range = guidelines[[i]][[1]],
        #             agreement_level = guidelines[[i]][[2]],
        #             clinical_interpretation = guidelines[[i]][[3]]
        #         ))
        #     }
        # },

        # # Diagnostic style clustering analysis (Usubutun et al. 2012) - commented out for future release
        # .performDiagnosticStyleAnalysis = function() {
        #     style_table <- self$results$diagnosticStyleTable
        #
        #     # Create distance matrix between raters based on diagnostic patterns
        #     distance_matrix <- private$.calculateRaterDistanceMatrix()
        #
        #     # Perform hierarchical clustering
        #     cluster_method <- self$options$styleClusterMethod
        #     hc <- hclust(distance_matrix, method = cluster_method)
        #
        #     # Cut dendrogram to get specified number of groups
        #     n_groups <- self$options$numberOfStyleGroups
        #     style_groups <- cutree(hc, k = n_groups)
        #
        #     # Populate style clustering results table
        #     rater_names <- private$.rater_names
        #     for (i in 1:length(rater_names)) {
        #         # Calculate rater characteristics if available
        #         experience <- if (!is.null(self$options$experienceVar)) {
        #             as.character(self$data[[self$options$experienceVar]][i])
        #         } else { "Not specified" }
        #
        #         training <- if (!is.null(self$options$trainingVar)) {
        #             as.character(self$data[[self$options$trainingVar]][i])
        #         } else { "Not specified" }
        #
        #         institution <- if (!is.null(self$options$institutionVar)) {
        #             as.character(self$data[[self$options$institutionVar]][i])
        #         } else { "Not specified" }
        #
        #         specialty <- if (!is.null(self$options$specialtyVar)) {
        #             as.character(self$data[[self$options$specialtyVar]][i])
        #         } else { "Not specified" }
        #
        #         # Calculate agreement with other raters in same style group
        #         same_group_raters <- which(style_groups == style_groups[i] & 1:length(rater_names) != i)
        #         within_group_agreement <- if (length(same_group_raters) > 0) {
        #             private$.calculateWithinGroupAgreement(i, same_group_raters)
        #         } else { 100.0 }
        #
        #         style_table$addRow(rowKey = rater_names[i], values = list(
        #             rater = rater_names[i],
        #             style_group = paste("Style", style_groups[i]),
        #             within_group_agreement = within_group_agreement,
        #             experience = experience,
        #             training = training,
        #             institution = institution,
        #             specialty = specialty
        #         ))
        #     }
        #
        #     # Store clustering results for visualization
        #     private$.style_clustering_results <- list(
        #         hclust = hc,
        #         groups = style_groups,
        #         distance_matrix = distance_matrix
        #     )
        #
        #     # Generate style summary
        #     private$.generateStyleSummary(style_groups)
        #
        #     # Identify discordant cases if requested
        #     if (self$options$identifyDiscordantCases) {
        #         private$.identifyDiscordantCases(style_groups)
        #     }
        # },

        # Calculate distance matrix between raters
        .calculateRaterDistanceMatrix = function() {
            distance_metric <- self$options$styleDistanceMetric
            data_matrix <- private$.data_matrix
            n_raters <- private$.n_raters
            n_cases <- private$.n_cases

            # Initialize distance matrix
            dist_matrix <- matrix(0, nrow = n_raters, ncol = n_raters)
            rownames(dist_matrix) <- private$.rater_names
            colnames(dist_matrix) <- private$.rater_names

            # Calculate pairwise distances
            for (i in 1:(n_raters - 1)) {
                for (j in (i + 1):n_raters) {
                    if (distance_metric == "agreement") {
                        # Percentage disagreement (1 - percentage agreement)
                        agreement_count <- sum(data_matrix[, i] == data_matrix[, j])
                        agreement_percent <- agreement_count / n_cases
                        distance <- 1 - agreement_percent
                    } else if (distance_metric == "correlation") {
                        # Convert to numeric for correlation
                        rater_i <- as.numeric(as.factor(data_matrix[, i]))
                        rater_j <- as.numeric(as.factor(data_matrix[, j]))
                        correlation <- cor(rater_i, rater_j, use = "complete.obs")
                        distance <- 1 - abs(correlation)
                    } else {  # euclidean
                        # Euclidean distance on numeric coding
                        rater_i <- as.numeric(as.factor(data_matrix[, i]))
                        rater_j <- as.numeric(as.factor(data_matrix[, j]))
                        distance <- sqrt(sum((rater_i - rater_j)^2)) / n_cases
                    }

                    dist_matrix[i, j] <- distance
                    dist_matrix[j, i] <- distance
                }
            }

            return(as.dist(dist_matrix))
        },

        # Calculate within-group agreement for a rater
        .calculateWithinGroupAgreement = function(rater_index, same_group_indices) {
            if (length(same_group_indices) == 0) return(100.0)

            data_matrix <- private$.data_matrix
            rater_data <- data_matrix[, rater_index]

            # Calculate average agreement with other raters in same group
            total_agreement <- 0
            n_comparisons <- 0

            for (other_index in same_group_indices) {
                other_data <- data_matrix[, other_index]
                agreement_count <- sum(rater_data == other_data)
                agreement_percent <- (agreement_count / length(rater_data)) * 100
                total_agreement <- total_agreement + agreement_percent
                n_comparisons <- n_comparisons + 1
            }

            return(if (n_comparisons > 0) total_agreement / n_comparisons else 100.0)
        },

        # Generate style group summary
        .generateStyleSummary = function(style_groups) {
            style_summary_table <- self$results$styleSummaryTable

            unique_groups <- sort(unique(style_groups))

            for (group in unique_groups) {
                group_members <- which(style_groups == group)
                n_members <- length(group_members)
                member_names <- paste(private$.rater_names[group_members], collapse = ", ")

                # Calculate average within-group agreement
                if (n_members > 1) {
                    total_agreement <- 0
                    n_pairs <- 0

                    for (i in 1:(n_members - 1)) {
                        for (j in (i + 1):n_members) {
                            rater_i <- group_members[i]
                            rater_j <- group_members[j]

                            agreement_count <- sum(private$.data_matrix[, rater_i] == private$.data_matrix[, rater_j])
                            agreement_percent <- (agreement_count / private$.n_cases) * 100
                            total_agreement <- total_agreement + agreement_percent
                            n_pairs <- n_pairs + 1
                        }
                    }

                    avg_agreement <- if (n_pairs > 0) total_agreement / n_pairs else 100.0
                } else {
                    avg_agreement <- 100.0  # Single member group
                }

                # Identify predominant characteristics
                group_experience <- "Mixed"
                group_training <- "Mixed"
                group_institution <- "Mixed"

                if (self$options$raterCharacteristics) {
                    # Analyze experience distribution
                    if (!is.null(self$options$experienceVar)) {
                        exp_values <- self$data[[self$options$experienceVar]][group_members]
                        if (length(unique(exp_values)) == 1) {
                            group_experience <- as.character(unique(exp_values))
                        }
                    }

                    # Analyze training distribution
                    if (!is.null(self$options$trainingVar)) {
                        train_values <- self$data[[self$options$trainingVar]][group_members]
                        if (length(unique(train_values)) == 1) {
                            group_training <- as.character(unique(train_values))
                        }
                    }

                    # Analyze institution distribution
                    if (!is.null(self$options$institutionVar)) {
                        inst_values <- self$data[[self$options$institutionVar]][group_members]
                        if (length(unique(inst_values)) == 1) {
                            group_institution <- as.character(unique(inst_values))
                        }
                    }
                }

                style_summary_table$addRow(rowKey = paste("Style", group), values = list(
                    style_group = paste("Style", group),
                    n_members = n_members,
                    members = member_names,
                    avg_within_agreement = avg_agreement,
                    predominant_experience = group_experience,
                    predominant_training = group_training,
                    predominant_institution = group_institution
                ))
            }
        },

        # Identify discordant cases that distinguish style groups
        .identifyDiscordantCases = function(style_groups) {
            discordant_table <- self$results$discordantCasesTable

            data_matrix <- private$.data_matrix
            n_cases <- private$.n_cases
            unique_groups <- sort(unique(style_groups))

            # Calculate disagreement between style groups for each case
            case_discord_scores <- numeric(n_cases)

            for (case_idx in 1:n_cases) {
                case_diagnoses <- as.character(data_matrix[case_idx, ])

                # Calculate between-group disagreement
                group_diagnoses <- list()
                for (group in unique_groups) {
                    group_members <- which(style_groups == group)
                    group_diagnoses[[group]] <- case_diagnoses[group_members]
                }

                # Score based on how much groups disagree on this case
                between_group_disagreement <- 0
                n_group_pairs <- 0

                for (i in 1:(length(unique_groups) - 1)) {
                    for (j in (i + 1):length(unique_groups)) {
                        group_i_mode <- names(sort(table(group_diagnoses[[unique_groups[i]]]), decreasing = TRUE))[1]
                        group_j_mode <- names(sort(table(group_diagnoses[[unique_groups[j]]]), decreasing = TRUE))[1]

                        if (group_i_mode != group_j_mode) {
                            between_group_disagreement <- between_group_disagreement + 1
                        }
                        n_group_pairs <- n_group_pairs + 1
                    }
                }

                case_discord_scores[case_idx] <- between_group_disagreement / n_group_pairs
            }

            # Identify top discordant cases
            threshold <- quantile(case_discord_scores, 0.8)  # Top 20%
            discordant_indices <- which(case_discord_scores >= threshold & case_discord_scores > 0)

            for (idx in discordant_indices) {
                case_id <- if (!is.null(self$options$caseID)) {
                    as.character(self$data[[self$options$caseID]][idx])
                } else {
                    paste("Case", idx)
                }

                case_diagnoses <- as.character(data_matrix[idx, ])

                # Show diagnosis by style group
                group_diagnoses_str <- ""
                for (group in unique_groups) {
                    group_members <- which(style_groups == group)
                    group_diag <- case_diagnoses[group_members]
                    group_mode <- names(sort(table(group_diag), decreasing = TRUE))[1]
                    group_diagnoses_str <- paste0(group_diagnoses_str,
                                                "Style ", group, ": ", group_mode, "; ")
                }

                discordant_table$addRow(rowKey = case_id, values = list(
                    case_id = case_id,
                    discord_score = case_discord_scores[idx],
                    style_group_diagnoses = group_diagnoses_str,
                    case_interpretation = "High inter-style disagreement"
                ))
            }
        },

        # Generate frequency tables
        .generateFrequencyTables = function() {
            data_matrix <- private$.data_matrix
            rater_names <- private$.rater_names

            # Create HTML content for frequency tables
            html_content <- "<h3>Frequency Tables by Rater</h3>"

            for (i in 1:length(rater_names)) {
                rater_data <- data_matrix[, i]
                freq_table <- table(rater_data)

                html_content <- paste0(html_content,
                    "<h4>", rater_names[i], "</h4>",
                    "<table border='1' style='margin-bottom: 20px;'>",
                    "<tr><th>Category</th><th>Frequency</th><th>Percentage</th></tr>"
                )

                for (j in 1:length(freq_table)) {
                    category <- names(freq_table)[j]
                    frequency <- freq_table[j]
                    percentage <- round(frequency / sum(freq_table) * 100, 1)

                    html_content <- paste0(html_content,
                        "<tr><td>", category, "</td><td>", frequency, "</td><td>", percentage, "%</td></tr>"
                    )
                }

                html_content <- paste0(html_content, "</table>")
            }

            # Cross-tabulation for pairs (if 2 raters)
            if (private$.n_raters == 2) {
                cross_table <- table(data_matrix[, 1], data_matrix[, 2])
                html_content <- paste0(html_content,
                    "<h4>Cross-tabulation: ", rater_names[1], " vs ", rater_names[2], "</h4>",
                    "<div>", htmlTable::htmlTable(cross_table), "</div>"
                )
            }

            self$results$frequencyTables$setContent(html_content)
        },

        # Helper functions
        .interpretKappa = function(kappa) {
            if (is.na(kappa)) return("Cannot calculate")
            if (kappa < 0) return("Poor")
            if (kappa <= 0.20) return("Slight")
            if (kappa <= 0.40) return("Fair")
            if (kappa <= 0.60) return("Moderate")
            if (kappa <= 0.80) return("Substantial")
            return("Almost Perfect")
        },

        .interpretKrippendorff = function(alpha) {
            if (is.na(alpha) || is.nan(alpha)) return("Cannot calculate")
            if (alpha < 0) return("Poor")
            if (alpha < 0.20) return("Slight")
            if (alpha < 0.40) return("Fair")
            if (alpha < 0.60) return("Moderate")
            if (alpha < 0.80) return("Substantial")
            return("Almost Perfect")
        },

        .calculateBasicKrippendorffNominal = function(kripp_data) {
            tryCatch({
                # Remove rows with all NA values
                valid_rows <- apply(kripp_data, 1, function(x) !all(is.na(x)))
                if (sum(valid_rows) < 2) return(NaN)

                kripp_data <- kripp_data[valid_rows, , drop = FALSE]

                # Calculate observed disagreement
                total_pairs <- 0
                disagreements <- 0

                for (i in 1:nrow(kripp_data)) {
                    for (j in 1:nrow(kripp_data)) {
                        if (i != j) {
                            for (k in 1:ncol(kripp_data)) {
                                if (!is.na(kripp_data[i, k]) && !is.na(kripp_data[j, k])) {
                                    total_pairs <- total_pairs + 1
                                    if (kripp_data[i, k] != kripp_data[j, k]) {
                                        disagreements <- disagreements + 1
                                    }
                                }
                            }
                        }
                    }
                }

                if (total_pairs == 0) return(NaN)
                observed_disagreement <- disagreements / total_pairs

                # Calculate expected disagreement (marginal probabilities)
                all_values <- as.vector(kripp_data)
                all_values <- all_values[!is.na(all_values)]
                if (length(all_values) == 0) return(NaN)

                value_counts <- table(all_values)
                value_probs <- value_counts / sum(value_counts)

                expected_disagreement <- 1 - sum(value_probs^2)

                if (expected_disagreement == 0) return(NaN)

                # Krippendorff's alpha = 1 - (observed disagreement / expected disagreement)
                alpha <- 1 - (observed_disagreement / expected_disagreement)
                return(alpha)
            }, error = function(e) {
                return(NaN)
            })
        },

        .interpretICC = function(icc) {
            if (is.na(icc)) return("Cannot calculate")
            if (icc < 0.50) return("Poor")
            if (icc < 0.75) return("Moderate")
            if (icc < 0.90) return("Good")
            return("Excellent")
        },

        .getICCTypeDescription = function(icc_type) {
            switch(icc_type,
                "ICC1" = "ICC(1,1) - Single Measures, Absolute Agreement",
                "ICC2" = "ICC(2,1) - Single Measures, Consistency",
                "ICC3" = "ICC(3,1) - Single Measures, Consistency (Fixed Raters)",
                "ICC1k" = "ICC(1,k) - Average Measures, Absolute Agreement",
                "ICC2k" = "ICC(2,k) - Average Measures, Consistency",
                "ICC3k" = "ICC(3,k) - Average Measures, Consistency (Fixed Raters)"
            )
        },

        # Visualization functions
        .heatmapPlot = function(image, ggtheme, theme, ...) {
            if (private$.n_raters < 2) return()

            # Create pairwise agreement matrix
            n_raters <- private$.n_raters
            rater_names <- private$.rater_names
            agreement_matrix <- matrix(1, nrow = n_raters, ncol = n_raters)
            rownames(agreement_matrix) <- rater_names
            colnames(agreement_matrix) <- rater_names

            # Fill matrix with pairwise kappa values
            for (i in 1:(n_raters - 1)) {
                for (j in (i + 1):n_raters) {
                    pair_data <- private$.data_matrix[, c(i, j)]
                    kappa_result <- irr::kappa2(pair_data, weight = self$options$wght)
                    agreement_matrix[i, j] <- kappa_result$value
                    agreement_matrix[j, i] <- kappa_result$value
                }
            }

            # Convert to long format for ggplot
            melted_matrix <- reshape2::melt(agreement_matrix)

            # Create heatmap
            p <- ggplot(melted_matrix, aes(Var1, Var2, fill = value)) +
                geom_tile() +
                geom_text(aes(label = round(value, 3)), color = "white", size = 4) +
                scale_fill_gradient2(low = "red", mid = "yellow", high = "green",
                                   midpoint = 0.5, name = "Kappa") +
                labs(title = "Rater Agreement Heatmap",
                     x = "Rater", y = "Rater") +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))

            print(p)
            TRUE
        }

        # # Pairwise plot - commented out for future release
        # .pairwisePlot = function(image, ggtheme, theme, ...) {
        #     # Placeholder for pairwise agreement plot
        #     p <- ggplot() +
        #         geom_text(aes(x = 0.5, y = 0.5, label = "Pairwise Agreement Plot\n(Implementation pending)"),
        #                  size = 6) +
        #         xlim(0, 1) + ylim(0, 1) +
        #         theme_void()
        #     print(p)
        #     TRUE
        # },

        # # Category plot - commented out for future release
        # .categoryPlot = function(image, ggtheme, theme, ...) {
        #     # Placeholder for category agreement plot
        #     p <- ggplot() +
        #         geom_text(aes(x = 0.5, y = 0.5, label = "Category Agreement Plot\n(Implementation pending)"),
        #                  size = 6) +
        #         xlim(0, 1) + ylim(0, 1) +
        #         theme_void()
        #     print(p)
        #     TRUE
        # },

        # # Confusion matrix plot - commented out for future release
        # .confusionMatrixPlot = function(image, ggtheme, theme, ...) {
        #     # Placeholder for confusion matrix plot
        #     p <- ggplot() +
        #         geom_text(aes(x = 0.5, y = 0.5, label = "Confusion Matrix Plot\n(Implementation pending)"),
        #                  size = 6) +
        #         xlim(0, 1) + ylim(0, 1) +
        #         theme_void()
        #     print(p)
        #     TRUE
        # },

        # # Diagnostic style dendrogram - commented out for future release
        # .diagnosticStyleDendrogram = function(image, ggtheme, theme, ...) {
        #      if (!self$options$diagnosticStyleAnalysis || is.null(private$.style_clustering_results)) {
        #         p <- ggplot() +
        #             geom_text(aes(x = 0.5, y = 0.5, label = "Enable Diagnostic Style Analysis\nto view dendrogram"),
        #                      size = 6) +
        #             xlim(0, 1) + ylim(0, 1) +
        #             theme_void()
        #         print(p)
        #         return(TRUE)
        #     }
        #
        #     # Extract clustering results
        #     hc <- private$.style_clustering_results$hclust
        #     style_groups <- private$.style_clustering_results$groups
        #
        #     # Create dendrogram plot
        #     require(ggdendro)
        #     dend_data <- dendro_data(hc)
        #
        #     # Color branches by style groups
        #     n_groups <- self$options$numberOfStyleGroups
        #     group_colors <- rainbow(n_groups)
        #
        #     # Create segment colors based on groups
        #     segment_colors <- rep("black", nrow(dend_data$segments))
        #
        #     p <- ggplot() +
        #         geom_segment(data = dend_data$segments,
        #                    aes(x = x, y = y, xend = xend, yend = yend)) +
        #         geom_text(data = dend_data$labels,
        #                  aes(x = x, y = y, label = label),
        #                  hjust = 1, angle = 90, size = 3) +
        #         labs(title = "Diagnostic Style Clustering Dendrogram",
        #              subtitle = paste("Method:", self$options$styleClusterMethod,
        #                             "| Distance:", self$options$styleDistanceMetric,
        #                             "| Groups:", n_groups),
        #              x = "Rater", y = "Distance") +
        #         theme_minimal() +
        #         theme(axis.text.x = element_blank(),
        #               axis.ticks.x = element_blank(),
        #               plot.title = element_text(hjust = 0.5),
        #               plot.subtitle = element_text(hjust = 0.5))
        #
        #     print(p)
        #     TRUE
        # },
        #
        # # # Diagnostic style heatmap - commented out for future release
        # # .diagnosticStyleHeatmap = function(image, ggtheme, theme, ...) {
        #     if (!self$options$diagnosticStyleAnalysis || is.null(private$.style_clustering_results)) {
        #         p <- ggplot() +
        #             geom_text(aes(x = 0.5, y = 0.5, label = "Enable Diagnostic Style Analysis\nto view style heatmap"),
        #                      size = 6) +
        #             xlim(0, 1) + ylim(0, 1) +
        #             theme_void()
        #         print(p)
        #         return(TRUE)
        #     }
        #
        #     # Create a heatmap similar to Figure 1 in Usubutun paper
        #     # showing diagnostic patterns by rater and style group
        #
        #     data_matrix <- private$.data_matrix
        #     style_groups <- private$.style_clustering_results$groups
        #     rater_names <- private$.rater_names
        #     categories <- private$.categories
        #
        #     # Create a data frame for heatmap
        #     heatmap_data <- data.frame()
        #
        #     for (i in 1:length(rater_names)) {
        #         rater_data <- data_matrix[, i]
        #         style_group <- style_groups[i]
        #
        #         for (category in categories) {
        #             frequency <- sum(rater_data == category)
        #             percentage <- (frequency / length(rater_data)) * 100
        #
        #             heatmap_data <- rbind(heatmap_data, data.frame(
        #                 Rater = rater_names[i],
        #                 Category = as.character(category),
        #                 Percentage = percentage,
        #                 StyleGroup = paste("Style", style_group),
        #                 RaterIndex = i
        #             ))
        #         }
        #     }
        #
        #     # Order raters by style group for visualization
        #     heatmap_data$Rater <- factor(heatmap_data$Rater,
        #                                levels = rater_names[order(style_groups)])
        #
        #     # Create heatmap
        #     p <- ggplot(heatmap_data, aes(x = Category, y = Rater, fill = Percentage)) +
        #         geom_tile(color = "white", size = 0.5) +
        #         geom_text(aes(label = round(Percentage, 1)), size = 3, color = "white") +
        #         scale_fill_gradient2(low = "blue", mid = "yellow", high = "red",
        #                            midpoint = 50, name = "% Usage") +
        #         facet_grid(StyleGroup ~ ., scales = "free_y", space = "free_y") +
        #         labs(title = "Diagnostic Style Heatmap",
        #              subtitle = "Diagnostic Category Usage by Rater (Grouped by Style)",
        #              x = "Diagnostic Category", y = "Rater") +
        #         theme_minimal() +
        #         theme(axis.text.x = element_text(angle = 45, hjust = 1),
        #               plot.title = element_text(hjust = 0.5),
        #               plot.subtitle = element_text(hjust = 0.5),
        #               strip.text = element_text(face = "bold"))
        #
        #     print(p)
        #     TRUE
        # # }
    )
)
