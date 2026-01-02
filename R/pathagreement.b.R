#' @title Pathology Interrater Reliability Analysis
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
#' @importFrom scales percent_format percent
#' @importFrom grid pushViewport viewport
#' @importFrom stringr str_to_title
#' @importFrom ggplot2 scale_fill_viridis_c
#'
#' @description
#' Pathology interrater reliability analysis including Cohen's kappa, Fleiss' kappa,
#' Krippendorff's alpha, diagnostic style clustering, and agreement visualization.

pathagreementClass <- if (requireNamespace("jmvcore")) R6::R6Class(
    "pathagreementClass",
    inherit = pathagreementBase,
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
        
        .messages = NULL,

        .style_clustering_results = NULL,
        .rater_metadata = NULL,

        # Variable name handling utilities
        .escapeVar = function(x) {
            # Convert problematic variable names to safe R identifiers
            # Handles spaces, special characters, Unicode, etc.
            if (is.null(x) || length(x) == 0) return(NULL)
            gsub("[^A-Za-z0-9_]+", "_", make.names(x))
        },

        .cleanVarNames = function(df) {
            # Clean all variable names while preserving originals
            orig_names <- names(df)
            clean_names <- sapply(orig_names, private$.escapeVar)
            names(df) <- clean_names
            attr(df, "original_names") <- orig_names
            attr(df, "name_mapping") <- setNames(orig_names, clean_names)
            return(df)
        },

        .getOriginalName = function(clean_name, df = NULL) {
            # Reverse lookup for display purposes
            if (!is.null(df)) {
                name_map <- attr(df, "name_mapping")
                if (!is.null(name_map) && clean_name %in% names(name_map)) {
                    return(name_map[[clean_name]])
                }
            }
            return(clean_name)
        },

        .extractLabels = function(data, vars) {
            # Extract and preserve factor labels from labelled variables
            labels <- lapply(vars, function(v) {
                col <- data[[v]]
                if (is.factor(col)) {
                    attr(col, "labels", exact = TRUE) %||% levels(col)
                } else {
                    NULL
                }
            })
            names(labels) <- vars
            return(labels)
        },

        .formatWithLabels = function(value, var, labels_list) {
            # Use labels when displaying categorical values
            labels <- labels_list[[var]]
            if (!is.null(labels) && value %in% names(labels)) {
                return(labels[value])
            } else {
                return(value)
            }
        },

        # Initialization
        .init = function() {
            private$.setupInitialVisibility()
            private$.checkDependencies()
            if (private$.shouldShowWelcome()) {
                private$.handleWelcomeAndEducation()
            }
        },
        
        # Setup initial visibility settings
        .setupInitialVisibility = function() {
            # Hide crosstab table initially - will be shown only for exactly 2 raters
            self$results$crosstabTable$setVisible(FALSE)
        },
        
        # Check package dependencies
        .checkDependencies = function() {
            private$.checkPackageDependencies()
        },
        
        # Determine if welcome message should be shown
        .shouldShowWelcome = function() {
            return(is.null(self$data) || length(self$options$vars) == 0)
        },
        
        # Handle welcome message and educational content
        .handleWelcomeAndEducation = function() {
            # Show enhanced welcome message with clinical focus
            private$.showWelcomeMessage()
            
            # Generate educational content based on user options
            if (self$options$showAboutAnalysis) {
                private$.generateAboutAnalysis()
            }
            if (self$options$showAssumptions) {
                private$.generateAssumptions()
            }
            if (self$options$showWeightedKappaGuide && self$options$wght != 'unweighted') {
                private$.generateWeightedKappaGuide()
            }
            if (self$options$showStatisticalGlossary) {
                private$.generateStatisticalGlossary()
            }
        },

        # Main analysis function
        .run = function() {
            # Reset messages
            private$.resetMessages()

            # Early return if no variables selected
            if (is.null(self$options$vars) || length(self$options$vars) == 0) {
                self$results$todo$setVisible(TRUE)
                return()
            }
            
            # Validate minimum number of raters
            n_vars_selected <- length(self$options$vars)
            if (n_vars_selected < 2) {
                # Show enhanced welcome message with current progress
                private$.showWelcomeMessage()
                # Also generate educational content based on user options
                if (self$options$showAboutAnalysis) {
                    private$.generateAboutAnalysis()
                }
                if (self$options$showAssumptions) {
                    private$.generateAssumptions()
                }
                if (self$options$showWeightedKappaGuide && self$options$wght != 'unweighted') {
                    private$.generateWeightedKappaGuide()
                }
                self$results$todo$setVisible(TRUE)
                return()
            }

            if (nrow(self$data) == 0) {
                stop('Data contains no (complete) rows')
            }

            # Enhanced data validation
            private$.validateData()

            # Prepare data
            private$.prepareData()

            # Clear todo message when analysis proceeds
            self$results$todo$setVisible(FALSE)

            # Checkpoint before starting main analyses
            private$.checkpoint()

            # Perform analyses
            private$.performOverviewAnalysis()
            private$.performKappaAnalysis()

            if (self$options$icc) {
                private$.performICCAnalysis()
            }

            if (self$options$kripp) {
                # Checkpoint before computationally intensive Krippendorff analysis
                private$.checkpoint()
                private$.performKrippendorffAnalysis()
            }

            if (self$options$consensus) {
                private$.performConsensusAnalysis()
            }

            # Enhanced agreement analyses
            if (self$options$gwetAC) {
                private$.performGwetACAnalysis()
            }

            if (self$options$pabak) {
                private$.performPABAKAnalysis()
            }

            if (self$options$sampleSizePlanning) {
                private$.performSampleSizePlanning()
            }

            if (self$options$raterBiasAnalysis) {
                private$.performRaterBiasAnalysis()
            }

            if (self$options$agreementTrendAnalysis) {
                private$.performAgreementTrendAnalysis()
            }

            if (self$options$caseDifficultyScoring) {
                private$.performCaseDifficultyAnalysis()
            }

            if (self$options$agreementStabilityAnalysis) {
                private$.performStabilityAnalysis()
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

            if (self$options$performClustering) {
                # Checkpoint before computationally intensive style analysis
                private$.checkpoint()
                private$.performDiagnosticStyleAnalysis()
            }

            # Perform clustering analysis (Usubutun et al. 2012)
            if (self$options$performClustering) {
                # Clustering Prerequisites Check
                if (private$.n_raters < 2) {
                     private$.accumulateMessage("Clustering Analysis requires at least 2 raters. Skipped.")
                } else if (private$.n_cases < 5) { # Arbitrary low limit
                     private$.accumulateMessage("Too few cases (N<5) for reliable clustering. Skipped.")
                } else {
                    # Checkpoint before computationally intensive clustering
                    private$.checkpoint()
                    private$.performClusteringAnalysis()
                }
            }

            # Handle frequency tables
            if (self$options$sft) {
                private$.generateFrequencyTables()
                self$results$raterFrequencyTables$setVisible(TRUE)
            } else {
                # Ensure frequency tables are hidden when sft is false
                self$results$raterFrequencyTables$setVisible(FALSE)
                self$results$crosstabTable$setVisible(FALSE)
            }

            if (self$options$heatmap) {
                # The heatmap will be generated by the .heatmapPlot render function
                # when the plot is requested by jamovi
            }

            # Generate clinical summaries after all analyses are complete, based on user options
            if (self$options$showClinicalSummary) {
                private$.generateClinicalSummary()
            } else {
                # Hide clinical summary if user has disabled it
                self$results$clinicalSummary$setVisible(FALSE)
            }
            
            # Generate educational content based on user options
            if (self$options$showAboutAnalysis) {
                private$.generateAboutAnalysis()
            } else {
                self$results$aboutAnalysis$setVisible(FALSE)
            }
            
            if (self$options$showAssumptions) {
                private$.generateAssumptions()
            } else {
                self$results$assumptions$setVisible(FALSE)
            }
            
            # Generate weighted kappa guide based on user options and weighting selection
            if (self$options$showWeightedKappaGuide && self$options$wght != 'unweighted') {
                private$.generateWeightedKappaGuide()
            } else {
                self$results$weightedKappaGuide$setVisible(FALSE)
            }

            # Generate inline statistical comments if requested
            if (self$options$showInlineComments) {
                private$.generateInlineComments()
            } else {
                self$results$inlineComments$setVisible(FALSE)
            }
            
            # Populate warnings panel
            if (!is.null(private$.messages) && length(private$.messages) > 0) {
                self$results$warnings$setContent(paste(
                    "<div class='alert alert-warning'>",
                    "<h6>Analysis Messages</h6>",
                    "<ul>",
                    paste(paste0("<li>", private$.messages, "</li>"), collapse = ""),
                    "</ul></div>",
                    sep = ""
                ))
            } else {
                self$results$warnings$setVisible(FALSE)
            }
        },
        
        .accumulateMessage = function(msg) {
            private$.messages <- c(private$.messages, msg)
        },
        
        .resetMessages = function() {
            private$.messages <- NULL
            # Initialize with empty string to avoid NULL error in checks
            if (!is.null(self$results$warnings)) {
                self$results$warnings$setContent("")
                self$results$warnings$setVisible(TRUE)
            }
        },

        # Data preparation
        .prepareData = function() {
            # Get rater variables
            rater_vars <- self$options$vars
            private$.rater_names <- rater_vars

            # Extract data matrix
            data_subset <- self$data[rater_vars]

            # Extract metadata rows if enabled
            if (self$options$useMetadataRows && !is.null(self$options$caseID)) {
                metadata_result <- private$.extractMetadata(data_subset)
                data_subset <- metadata_result$case_data
                private$.rater_metadata <- metadata_result$metadata
            } else {
                private$.rater_metadata <- NULL
            }

            # Check for missing data warnings
            original_n <- nrow(data_subset)
            data_subset <- data_subset[complete.cases(data_subset), , drop = FALSE]
            final_n <- nrow(data_subset)
            
            if (original_n > 0) {
                missing_prop <- (original_n - final_n) / original_n
                if (missing_prop > 0.2) {
                    private$.accumulateMessage(sprintf("High missing data: %.1f%% of cases excluded due to missing ratings. Results may be biased.", missing_prop * 100))
                }
            }

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
                # Checkpoint every 100 cases for large datasets
                if (i %% 100 == 1 && total_cases > 100) {
                    private$.checkpoint(flush = FALSE)
                }
                
                case_values <- as.character(private$.data_matrix[i, ])
                if (length(unique(case_values)) == 1) {
                    agreement_count <- agreement_count + 1
                }
            }

            overall_agreement <- (agreement_count / total_cases) * 100

            # Determine primary method
            primary_method <- if (private$.n_raters == 2) {
                .("Cohen's Kappa")
            } else {
                .("Fleiss' Kappa")
            }

            # Prevalence Effect Check (Kappa Paradox)
            # If Agreement is High (>80%) but Kappa is Low (<0.4), warn user
            if (!is.na(overall_agreement) && overall_agreement > 80) {
                 # We can check the primary kappa from the table if already computed, 
                 # or do a quick check here. 
                 # Since kappa is calculated in separate method, let's just add a generic note if relevant options are selected.
                 # Better approach: check after Kappa is calculated or just warn generally about prevalence if data is imbalanced.
            }
            
            # Simple prevalence check on data
            if (total_cases > 0) {
                # Check category imbalance
                cat_counts <- table(unlist(private$.data_matrix))
                if (length(cat_counts) > 1) {
                    props <- prop.table(cat_counts)
                     if (max(props) > 0.8) {
                        private$.accumulateMessage(sprintf("Data is highly imbalanced (one category > 80%%). Kappa may be low despite high agreement (Paradox). Consider Gwet's AC1."))
                    }
                }
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
            method <- self$options$multiraterMethod

            # Determine analysis method based on user selection or automatic
            if (method == "auto") {
                if (private$.n_raters == 2) {
                    # Cohen's kappa for 2 raters
                    private$.performCohensKappa(kappa_table)
                } else {
                    # Fleiss' kappa for 3+ raters
                    private$.performFleissKappa(kappa_table)
                }
            } else if (method == "cohen" && private$.n_raters >= 2) {
                # Force Cohen's kappa (pairwise if >2 raters)
                private$.performCohensKappa(kappa_table)
            } else if (method == "fleiss" && private$.n_raters >= 3) {
                # Force Fleiss' kappa
                private$.performFleissKappa(kappa_table)
            } else if (method == "fleiss" && private$.n_raters < 3) {
                # Error check for Fleiss with < 3 raters
                private$.accumulateMessage("Fleiss' kappa requires 3 or more raters. Falling back to Cohen's kappa (pairwise analysis recommended for detail).")
                private$.performCohensKappa(kappa_table)
            } else if (method == "krippendorff") {
                # Force Krippendorff's alpha (will be calculated in separate method)
                private$.performKrippendorffForKappa(kappa_table)
            } else {
                # Fallback to automatic selection
                if (private$.n_raters == 2) {
                    private$.performCohensKappa(kappa_table)
                } else {
                    private$.performFleissKappa(kappa_table)
                }
            }
        },

        # Cohen's kappa (2 raters)
        .performCohensKappa = function(table) {
            wght <- self$options$wght
            show_ci <- self$options$fleissCI
            conf_level <- 0.95 # self$options$confidenceLevel

            # Check if weighting is appropriate
            # Handle weighted kappa with non-ordinal variables
            if (wght %in% c("equal", "squared")) {
                var_types <- sapply(private$.data_matrix, is.ordered)
                if (!all(var_types)) {
                    # Instead of error, fallback to unweighted kappa with explanation
                    wght <- "unweighted"
                    private$.accumulateMessage("Weighted kappa requested but variables are not ordinal. Analysis reverted to unweighted kappa.")
                    interpretation_note <- .("Note: Weighted kappa requires ordinal variables. Analysis performed with unweighted kappa instead.")
                } else {
                    interpretation_note <- ""
                }
            } else {
                interpretation_note <- ""
            }

            # Calculate Cohen's kappa
            result <- irr::kappa2(private$.data_matrix, weight = wght)

            # Interpretation
            interpretation <- private$.interpretKappa(result$value)

            # Validate variance estimate and calculate standard error
            se_value <- NA
            ci_lower <- NA
            ci_upper <- NA

            # Calculate confidence intervals only if requested
            if (show_ci) {
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
            }

            # Add to table
            table$addRow(rowKey = "cohens", values = list(
                method = .("Cohen's Kappa"),
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
                interpretation = if(interpretation_note != "") paste(interpretation, interpretation_note, sep = ". ") else interpretation
            ))
        },

        # Krippendorff's Alpha as primary method
        .performKrippendorffForKappa = function(table) {
            kripp_method <- self$options$krippMethod
            
            # Prepare data in the format expected by krippendorff.alpha
            data_for_kripp <- t(private$.data_matrix)
            
            # Calculate Krippendorff's alpha
            result <- try({
                if (kripp_method == "nominal") {
                    krippendorff.alpha(data_for_kripp, level = "nominal")
                } else if (kripp_method == "ordinal") {
                    krippendorff.alpha(data_for_kripp, level = "ordinal")
                } else if (kripp_method == "interval") {
                    krippendorff.alpha(data_for_kripp, level = "interval")
                } else if (kripp_method == "ratio") {
                    krippendorff.alpha(data_for_kripp, level = "ratio")
                } else {
                    krippendorff.alpha(data_for_kripp, level = "nominal")
                }
            }, silent = TRUE)
            
            if (inherits(result, "try-error")) {
                # Fallback to basic calculation
                alpha_value <- NA
            } else {
                alpha_value <- result$value
            }
            
            # Interpretation
            interpretation <- private$.interpretKappa(alpha_value)
            
            # Add to table
            table$addRow(rowKey = "krippendorff", values = list(
                method = paste0("Krippendorff's Alpha (", stringr::str_to_title(kripp_method), ")"),
                kappa = alpha_value,
                se = NA,
                ci_lower = NA,
                ci_upper = NA,
                z = NA,
                p = NA,
                interpretation = interpretation
            ))
        },

        # Fleiss' kappa (3+ raters)
        .performFleissKappa = function(table) {
            exact <- self$options$exct
            show_ci <- self$options$fleissCI
            conf_level <- 0.95 # self$options$confidenceLevel

            # Calculate Fleiss' kappa
            result <- irr::kappam.fleiss(private$.data_matrix, exact = exact, detail = TRUE)

            # Interpretation
            interpretation <- private$.interpretKappa(result$value)

            # Calculate standard error and confidence interval
            se_value <- NA
            ci_lower <- NA
            ci_upper <- NA

            # Calculate confidence intervals only if requested
            if (show_ci) {
                # Calculate standard error from z-statistic if available (same method as Cohen's kappa)
                if (!is.null(result$statistic) && is.numeric(result$statistic) &&
                    !is.na(result$statistic) && result$statistic != 0 &&
                    !is.null(result$value) && is.numeric(result$value) && !is.na(result$value)) {
                    se_value <- abs(result$value / result$statistic)
                    ci_lower <- result$value - qnorm((1 + conf_level)/2) * se_value
                    ci_upper <- result$value + qnorm((1 + conf_level)/2) * se_value
                }
            }

            # Determine method name based on exact calculation
            method_name <- if (exact) {
                .("Fleiss' Kappa (Exact)")
            } else {
                .("Fleiss' Kappa")
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
        .performICCAnalysis = function() {
            icc_table <- self$results$iccTable
            conf_level <- 0.95
            
            tryCatch({
                # Convert data to numeric for ICC (ordinal data assumed)
                numeric_data <- apply(private$.data_matrix, 2, function(x) as.numeric(as.factor(x)))
                
                # Calculate ICC using psych package
                icc_results <- psych::ICC(numeric_data, alpha = 1 - conf_level)
                
                # Add ICC results to table
                if (!is.null(icc_results) && !is.null(icc_results$results)) {
                    # Add ICC(2,1) - single rater, random raters
                    icc_table$addRow(rowKey = "ICC21", values = list(
                        type = "ICC(2,1)",
                        icc_value = icc_results$results["ICC2", "ICC"],
                        ci_lower = icc_results$results["ICC2", "lower bound"],
                        ci_upper = icc_results$results["ICC2", "upper bound"],
                        f_value = icc_results$results["ICC2", "F"],
                        p = icc_results$results["ICC2", "p"],
                        interpretation = private$.interpretICCValue(icc_results$results["ICC2", "ICC"])
                    ))
                }
            }, error = function(e) {
                # Add error row if ICC calculation fails
                icc_table$addRow(rowKey = "error", values = list(
                    type = .("ICC Error"),
                    icc_value = NaN,
                    ci_lower = NaN,
                    ci_upper = NaN,
                    f_value = NaN,
                    p = NaN,
                    interpretation = paste(.("ICC calculation failed:"), e$message)
                ))
            })
        },

        # Krippendorff's alpha analysis
        .performKrippendorffAnalysis = function() {
            kripp_table <- self$results$krippTable
            kripp_method <- self$options$krippMethod
            bootstrap <- self$options$bootstrap  # Use user-specified bootstrap option


            # Try to calculate Krippendorff's alpha
            alpha_value <- NaN
            interpretation <- .("Krippendorff's alpha calculation failed")

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
                        interpretation <- .("Krippendorff's alpha for non-nominal data requires full irr package implementation")
                    }
                }
            }, error = function(e) {
                alpha_value <<- NaN
                interpretation <<- paste(.("Error calculating Krippendorff's alpha:"), e$message)
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

        # Consensus scoring analysis
        .performConsensusAnalysis = function() {
            consensus_method <- self$options$consensus_method
            tie_breaking <- self$options$tie_breaking
            show_consensus_table <- self$options$show_consensus_table

            
            # Calculate consensus for each case
            consensus_results <- private$.calculateConsensusScores(consensus_method, tie_breaking)
            
            # Populate consensus summary table
            summary_table <- self$results$consensusSummary
            
            # Calculate summary statistics
            total_cases <- private$.n_cases
            consensus_achieved <- sum(!is.na(consensus_results$consensus_scores))
            unanimous_cases <- sum(consensus_results$agreement_levels == .("Unanimous"), na.rm = TRUE)
            majority_cases <- sum(consensus_results$agreement_levels == .("Majority"), na.rm = TRUE)
            super_majority_cases <- sum(consensus_results$agreement_levels == .("Super Majority"), na.rm = TRUE)
            tied_cases <- sum(consensus_results$agreement_levels == .("Tie"), na.rm = TRUE)
            no_consensus_cases <- sum(consensus_results$agreement_levels == .("No Consensus"), na.rm = TRUE)
            
            # Add summary rows
            summary_table$addRow(rowKey = "total", values = list(
                metric = .("Total Cases"),
                value = as.character(total_cases),
                percentage = 100.0
            ))
            
            summary_table$addRow(rowKey = "consensus", values = list(
                metric = .("Consensus Achieved"),
                value = as.character(consensus_achieved),
                percentage = round((consensus_achieved / total_cases) * 100, 1)
            ))
            
            if (unanimous_cases > 0) {
                summary_table$addRow(rowKey = "unanimous", values = list(
                    metric = .("Unanimous Agreement"),
                    value = as.character(unanimous_cases),
                    percentage = round((unanimous_cases / total_cases) * 100, 1)
                ))
            }
            
            if (super_majority_cases > 0) {
                summary_table$addRow(rowKey = "super_majority", values = list(
                    metric = .("Super Majority (≥2/3)"),
                    value = as.character(super_majority_cases),
                    percentage = round((super_majority_cases / total_cases) * 100, 1)
                ))
            }
            
            if (majority_cases > 0) {
                summary_table$addRow(rowKey = "majority", values = list(
                    metric = .("Majority (≥50%)"),
                    value = as.character(majority_cases),
                    percentage = round((majority_cases / total_cases) * 100, 1)
                ))
            }
            
            if (tied_cases > 0) {
                summary_table$addRow(rowKey = "tied", values = list(
                    metric = .("Tied Cases"),
                    value = as.character(tied_cases),
                    percentage = round((tied_cases / total_cases) * 100, 1)
                ))
            }
            
            if (no_consensus_cases > 0) {
                summary_table$addRow(rowKey = "no_consensus", values = list(
                    metric = .("No Consensus"),
                    value = as.character(no_consensus_cases),
                    percentage = round((no_consensus_cases / total_cases) * 100, 1)
                ))
            }
            
            # Populate detailed consensus table if requested
            if (show_consensus_table) {
                consensus_table <- self$results$consensusTable
                
                for (i in 1:private$.n_cases) {
                    case_id <- paste("Case", i)
                    consensus_score <- consensus_results$consensus_scores[i]
                    agreement_level <- consensus_results$agreement_levels[i]
                    n_agreeing <- consensus_results$n_agreeing[i]
                    
                    # Format individual rater scores
                    case_scores <- as.character(private$.data_matrix[i, ])
                    rater_scores_text <- paste(paste(private$.rater_names, case_scores, sep = ": "), collapse = ", ")
                    
                    consensus_table$addRow(rowKey = case_id, values = list(
                        case_id = case_id,
                        consensus_score = if (is.na(consensus_score)) .("No consensus") else as.character(consensus_score),
                        agreement_level = agreement_level,
                        n_agreeing = n_agreeing,
                        rater_scores = rater_scores_text
                    ))
                }
            }
        },

        # Calculate consensus scores using specified method
        .calculateConsensusScores = function(method, tie_breaking) {
            n_cases <- private$.n_cases
            n_raters <- private$.n_raters
            data_matrix <- private$.data_matrix
            
            consensus_scores <- rep(NA, n_cases)
            agreement_levels <- rep(.("No Consensus"), n_cases)
            n_agreeing <- rep(0, n_cases)
            
            # Calculate consensus threshold
            threshold <- switch(method,
                "majority" = ceiling(n_raters / 2),
                "super_majority" = ceiling(n_raters * 2/3),
                "unanimous" = n_raters
            )
            
            for (i in 1:n_cases) {
                case_scores <- as.character(data_matrix[i, ])
                score_counts <- table(case_scores)
                max_count <- max(score_counts)
                mode_scores <- names(score_counts)[score_counts == max_count]
                
                # Check if consensus threshold is met
                if (max_count >= threshold) {
                    if (length(mode_scores) == 1) {
                        # Single consensus score
                        consensus_scores[i] <- mode_scores[1]
                        n_agreeing[i] <- max_count
                        
                        # Determine agreement level
                        if (max_count == n_raters) {
                            agreement_levels[i] <- .("Unanimous")
                        } else if (max_count >= ceiling(n_raters * 2/3)) {
                            agreement_levels[i] <- .("Super Majority")
                        } else {
                            agreement_levels[i] <- .("Majority")
                        }
                    } else {
                        # Tie at consensus level - handle based on tie_breaking method
                        agreement_levels[i] <- .("Tie")
                        n_agreeing[i] <- max_count
                        
                        if (tie_breaking == "arbitration") {
                            consensus_scores[i] <- "ARBITRATION_NEEDED"
                        } else if (tie_breaking == "global_mode") {
                            # Use most frequent category across all cases
                            all_scores <- as.character(as.matrix(data_matrix))
                            global_counts <- table(all_scores)
                            global_mode <- names(global_counts)[which.max(global_counts)]
                            
                            # If one of the tied scores matches global mode, use it
                            if (global_mode %in% mode_scores) {
                                consensus_scores[i] <- global_mode
                                agreement_levels[i] <- paste(agreement_levels[i], "(Resolved)")
                            }
                        }
                        # If tie_breaking == "exclude", leave as NA (default)
                    }
                } else {
                    # No consensus reached
                    n_agreeing[i] <- max_count
                    if (length(mode_scores) == 1) {
                        agreement_levels[i] <- paste(.("Insufficient"), " (", max_count, "/", n_raters, ")", sep = "")
                    } else {
                        agreement_levels[i] <- paste(.("Multiple ties"), " (", max_count, "/", n_raters, ")", sep = "")
                    }
                }
            }
            
            return(list(
                consensus_scores = consensus_scores,
                agreement_levels = agreement_levels,
                n_agreeing = n_agreeing
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

        # Diagnostic style clustering analysis (Usubutun et al. 2012)
        .performDiagnosticStyleAnalysis = function() {
            if (self$options$showProgressIndicators) {
                message("Performing diagnostic style clustering analysis...")
            }
            
            style_table <- self$results$diagnosticStyleTable

            # Create distance matrix between raters based on diagnostic patterns
            distance_matrix <- private$.calculateRaterDistanceMatrix()

            # Perform hierarchical clustering
            cluster_method <- self$options$clusteringMethod
            hc <- hclust(distance_matrix, method = cluster_method)

            # Cut dendrogram to get specified number of groups
            n_groups <- self$options$nStyleGroups
            style_groups <- cutree(hc, k = n_groups)

            # Populate style clustering results table
            rater_names <- private$.rater_names
            for (i in 1:length(rater_names)) {
                # Calculate rater characteristics if available
                experience <- if (self$options$raterCharacteristics && !is.null(self$options$experienceVar)) {
                    # Extract from experienceVar if available
                    as.character(self$data[[self$options$experienceVar]][i])
                } else { "Not specified" }

                training <- if (self$options$raterCharacteristics && !is.null(self$options$trainingVar)) {
                    as.character(self$data[[self$options$trainingVar]][i])
                } else { "Not specified" }

                institution <- if (self$options$raterCharacteristics && !is.null(self$options$institutionVar)) {
                    as.character(self$data[[self$options$institutionVar]][i])
                } else { "Not specified" }

                specialty <- if (self$options$raterCharacteristics && !is.null(self$options$specialtyVar)) {
                    as.character(self$data[[self$options$specialtyVar]][i])
                } else { "Not specified" }

                # Calculate agreement with other raters in same style group
                same_group_raters <- which(style_groups == style_groups[i] & 1:length(rater_names) != i)
                within_group_agreement <- if (length(same_group_raters) > 0) {
                    private$.calculateWithinGroupAgreement(i, same_group_raters)
                } else { 100.0 }

                style_table$addRow(rowKey = rater_names[i], values = list(
                    rater = rater_names[i],
                    style_group = paste(.("Style"), style_groups[i]),
                    within_group_agreement = within_group_agreement,
                    experience = experience,
                    training = training,
                    institution = institution,
                    specialty = specialty
                ))
            }

            # Store clustering results for visualization
            private$.style_clustering_results <- list(
                hclust = hc,
                groups = style_groups,
                distance_matrix = distance_matrix
            )

            # Generate style summary
            private$.generateStyleSummary(style_groups)

            # Identify discordant cases if requested
            if (self$options$identifyDiscordantCases) {
                private$.identifyDiscordantCases(style_groups)
            }
        },

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
                group_experience <- .("Mixed")
                group_training <- .("Mixed")
                group_institution <- .("Mixed")

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
                    style_group = paste(.("Style"), group),
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
            
            # Populate individual rater frequency table
            freq_table <- self$results$raterFrequencyTables$frequencyTable
            
            for (i in 1:length(rater_names)) {
                # Checkpoint before processing each rater
                private$.checkpoint(flush = FALSE)
                
                rater_data <- data_matrix[, i]
                freq_counts <- table(rater_data)
                
                for (j in 1:length(freq_counts)) {
                    category <- names(freq_counts)[j]
                    frequency <- as.numeric(freq_counts[j])
                    percentage <- round(frequency / sum(freq_counts) * 100, 1)
                    
                    row_key <- paste0(rater_names[i], "_", category)
                    freq_table$addRow(rowKey = row_key, values = list(
                        rater = rater_names[i],
                        category = category,
                        frequency = frequency,
                        percentage = percentage
                    ))
                }
            }

            # Cross-tabulation for pairs (if exactly 2 raters)
            crosstab_table <- self$results$crosstabTable
            if (private$.n_raters == 2) {
                crosstab_table$setVisible(TRUE)
                private$.generateCrosstabTable()
            } else {
                crosstab_table$setVisible(FALSE)
            }
        },
        
        # Generate cross-tabulation table for 2 raters
        .generateCrosstabTable = function() {
            if (private$.n_raters != 2) return()
            
            data_matrix <- private$.data_matrix
            rater_names <- private$.rater_names
            cross_table <- table(data_matrix[, 1], data_matrix[, 2])
            
            # Get unique categories for column headers
            all_categories <- sort(unique(c(rownames(cross_table), colnames(cross_table))))
            
            # Initialize the crosstab table with proper columns
            crosstab_table <- self$results$crosstabTable
            
            # First, we need to dynamically add columns for each category
            for (category in all_categories) {
                col_name <- paste0("cat_", gsub("[^A-Za-z0-9]", "_", category))
                # Note: jamovi tables have fixed columns, so we'll use a simpler approach
            }
            
            # For now, let's create a simple representation
            for (i in 1:nrow(cross_table)) {
                rater1_cat <- rownames(cross_table)[i]
                
                # Create a text representation of the row
                row_values <- paste(colnames(cross_table), cross_table[i, ], sep = ": ", collapse = ", ")
                
                crosstab_table$addRow(rowKey = rater1_cat, values = list(
                    rater1_category = paste0(rater_names[1], ": ", rater1_cat),
                    frequencies = row_values
                ))
            }
        },

        # Helper functions
        .interpretKappa = function(kappa) {
            if (is.na(kappa)) return(.("Cannot calculate"))
            if (kappa < 0) return(.("Poor"))
            if (kappa <= 0.20) return(.("Slight"))
            if (kappa <= 0.40) return(.("Fair"))
            if (kappa <= 0.60) return(.("Moderate"))
            if (kappa <= 0.80) return(.("Substantial"))
            return(.("Almost Perfect"))
        },

        .interpretKrippendorff = function(alpha) {
            if (is.na(alpha) || is.nan(alpha)) return(.("Cannot calculate"))
            if (alpha < 0) return(.("Poor"))
            if (alpha < 0.20) return(.("Slight"))
            if (alpha < 0.40) return(.("Fair"))
            if (alpha < 0.60) return(.("Moderate"))
            if (alpha < 0.80) return(.("Substantial"))
            return(.("Almost Perfect"))
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
            if (is.null(private$.n_raters) || length(private$.n_raters) == 0 || private$.n_raters < 2) return()

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

            # Get theme colors
            theme_colors <- switch(self$options$heatmapTheme,
                "ryg" = list(low = "red", mid = "yellow", high = "green"),
                "bwr" = list(low = "blue", mid = "white", high = "red"),
                "viridis" = list(scale = "viridis"),
                "plasma" = list(scale = "plasma"),
                list(low = "red", mid = "yellow", high = "green")  # default
            )

            # Create heatmap
            p <- ggplot(melted_matrix, aes(Var1, Var2, fill = value)) +
                geom_tile()

            # Add text labels if heatmapDetails is enabled
            if (self$options$heatmapDetails) {
                p <- p + geom_text(aes(label = round(value, 3)), color = "black", size = 3)
            }

            # Apply color scale based on theme
            if (self$options$heatmapTheme %in% c("viridis", "plasma")) {
                p <- p + scale_fill_viridis_c(option = self$options$heatmapTheme, name = "Kappa")
            } else {
                p <- p + scale_fill_gradient2(low = theme_colors$low, mid = theme_colors$mid, 
                                            high = theme_colors$high, midpoint = 0.5, name = "Kappa")
            }

            p <- p + 
                labs(title = "Rater Agreement Heatmap",
                     x = "Rater", y = "Rater") +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))

            print(p)
            TRUE
        },

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

        # Diagnostic style dendrogram - Creates hierarchical clustering visualization matching Usubutun et al.
        .diagnosticStyleDendrogram = function(image, ggtheme, theme, ...) {
             if (!self$options$performClustering || is.null(private$.style_clustering_results)) {
                p <- ggplot() +
                    geom_text(aes(x = 0.5, y = 0.5, label = "Enable Rater Clustering Analysis\nto view dendrogram"),
                             size = 6) +
                    xlim(0, 1) + ylim(0, 1) +
                    theme_void()
                print(p)
                return(TRUE)
            }

            # Extract clustering results
            hc <- private$.style_clustering_results$hclust_object
            style_groups <- private$.style_clustering_results$cluster_assignments
            rater_names <- private$.rater_names

            # Create dendrogram using ggdendro
            require(ggdendro)

            # Convert hclust to ggplot-compatible format
            dend_data <- dendro_data(hc)

            # Assign colors to branches based on style groups
            n_groups <- private$.style_clustering_results$n_clusters

            # Check if n_groups is valid
            if (is.null(n_groups) || length(n_groups) == 0 || n_groups < 2) {
                # Default to 3 groups if invalid
                n_groups <- 3
            }

            if (n_groups == 3) {
                # Use Usubutun colors: Green, Yellow, Red
                group_colors <- c("#2E8B57", "#FFD700", "#DC143C")  # Green, Gold, Crimson
            } else {
                group_colors <- rainbow(n_groups)
            }

            # Create color mapping for raters based on their style groups
            rater_colors <- rep("black", length(rater_names))
            for (i in 1:length(rater_names)) {
                group_id <- style_groups[i]
                if (!is.null(group_id) && !is.na(group_id) && group_id > 0 && group_id <= length(group_colors)) {
                    rater_colors[i] <- group_colors[group_id]
                }
            }
            
            # Create the plot matching Usubutun style with visible clustering tree
            p <- ggplot() +
                # Draw the main dendrogram segments (tree structure)
                geom_segment(data = dend_data$segments,
                           aes(x = x, y = y, xend = xend, yend = yend),
                           color = "black", size = 0.6, lineend = "round") +
                # Add colored rectangles at top to show style groups
                {
                  # Create rectangles for each style group at the top
                  group_rects <- data.frame()
                  for (group in 1:n_groups) {
                    group_members <- which(style_groups == group)
                    if (length(group_members) > 0) {
                      x_positions <- group_members
                      x_min <- min(x_positions) - 0.4
                      x_max <- max(x_positions) + 0.4
                      y_top <- max(dend_data$segments$y, na.rm = TRUE)
                      group_rects <- rbind(group_rects, data.frame(
                        xmin = x_min, xmax = x_max,
                        ymin = y_top * 1.02, ymax = y_top * 1.08,
                        fill = group_colors[group],
                        group = group
                      ))
                    }
                  }
                  if (nrow(group_rects) > 0) {
                    geom_rect(data = group_rects, 
                             aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                             fill = group_rects$fill, color = "black", size = 0.3, alpha = 0.7)
                  }
                } +
                # Add style group labels
                {
                  group_labels <- data.frame()
                  for (group in 1:n_groups) {
                    group_members <- which(style_groups == group)
                    if (length(group_members) > 0) {
                      x_center <- mean(group_members)
                      y_top <- max(dend_data$segments$y, na.rm = TRUE)
                      group_name <- switch(group, 
                                         "1" = "GREEN", 
                                         "2" = "YELLOW", 
                                         "3" = "RED",
                                         paste("Group", group))
                      group_labels <- rbind(group_labels, data.frame(
                        x = x_center, y = y_top * 1.05,
                        label = group_name,
                        color = "white"
                      ))
                    }
                  }
                  if (nrow(group_labels) > 0) {
                    geom_text(data = group_labels,
                             aes(x = x, y = y, label = label),
                             color = "white", size = 3, fontface = "bold")
                  }
                } +
                # Add rater labels at bottom with colors
                geom_text(data = dend_data$labels,
                         aes(x = x, y = y, label = label),
                         color = rater_colors[as.numeric(dend_data$labels$label)],
                         angle = 90, size = 3, hjust = 1, vjust = 0.5, fontface = "bold") +
                labs(title = "Diagnostic Style Dendrogram (Ward's Linkage)",
                     subtitle = paste("Hierarchical Clustering of", length(rater_names), "Pathologists"),
                     x = "Pathologist", y = "Distance") +
                theme_classic() +
                theme(
                    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
                    plot.subtitle = element_text(hjust = 0.5, size = 10),
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank(),
                    axis.line.x = element_blank(),
                    panel.grid = element_blank(),
                    axis.title.x = element_text(margin = margin(t = 10)),
                    plot.margin = margin(10, 10, 10, 10)
                ) +
                scale_x_continuous(expand = c(0.02, 0)) +
                scale_y_continuous(expand = c(0, 0.1))

            print(p)
            TRUE
        },
        #
        # Diagnostic style heatmap - Creates case-by-rater heatmap with two-way clustering
        .diagnosticStyleHeatmap = function(image, ggtheme, theme, ...) {
            if (!self$options$performClustering || !self$options$showClusteringHeatmap || is.null(private$.style_clustering_results)) {
                p <- ggplot() +
                    geom_text(aes(x = 0.5, y = 0.5, label = "Enable Diagnostic Style Analysis\nto view style heatmap"),
                             size = 6) +
                    xlim(0, 1) + ylim(0, 1) +
                    theme_void()
                print(p)
                return(TRUE)
            }

            # Create a case-by-rater heatmap with two-way clustering
            data_matrix <- private$.data_matrix
            style_groups <- private$.style_clustering_results$groups
            rater_names <- private$.rater_names
            categories <- private$.categories
            n_cases <- private$.n_cases
            
            # Load required packages
            require(ggdendro)
            require(gridExtra)
            require(grid)
            
            # === CLUSTER RATERS (COLUMNS) ===
            # Order raters by style group (matching dendrogram order)
            rater_order <- order(style_groups)
            ordered_raters <- rater_names[rater_order]
            ordered_groups <- style_groups[rater_order]
            
            # === CLUSTER CASES (ROWS) ===
            # Create numeric matrix for case clustering with proper validation
            numeric_matrix <- data_matrix
            
            # Convert to numeric with proper handling of missing values
            for (i in 1:nrow(numeric_matrix)) {
                for (j in 1:ncol(numeric_matrix)) {
                    val <- as.character(numeric_matrix[i, j])
                    if (is.na(val) || val == "" || val == "NA") {
                        numeric_matrix[i, j] <- 1  # Default to Benign for missing
                    } else {
                        numeric_matrix[i, j] <- switch(val,
                                                     "Benign" = 1,
                                                     "EIN" = 2, 
                                                     "Cancer" = 3,
                                                     1)  # default to Benign for unrecognized
                    }
                }
            }
            
            # Ensure all values are numeric and finite
            numeric_matrix <- apply(numeric_matrix, c(1,2), as.numeric)
            
            # Check for any remaining NA/NaN/Inf values
            if (any(is.na(numeric_matrix)) || any(is.infinite(numeric_matrix))) {
                # If there are still problematic values, skip clustering and use original order
                case_order <- 1:n_cases
                case_hc <- NULL
                message("Case clustering skipped due to data issues - using original order")
            } else {
                # Calculate case similarities (agreement across raters)
                case_dist <- dist(numeric_matrix, method = "euclidean")
                
                # Check if distance matrix is valid
                if (any(is.na(case_dist)) || any(is.infinite(case_dist))) {
                    case_order <- 1:n_cases
                    case_hc <- NULL
                    message("Case clustering skipped due to distance calculation issues")
                } else {
                    case_hc <- hclust(case_dist, method = "ward.D2")
                    case_order <- case_hc$order
                }
            }
            
            # Order data by both dimensions
            ordered_data <- numeric_matrix[case_order, rater_order]
            
            # Convert back to diagnostic labels
            for (i in 1:nrow(ordered_data)) {
                for (j in 1:ncol(ordered_data)) {
                    ordered_data[i, j] <- switch(as.character(ordered_data[i, j]),
                                                "1" = "Benign",
                                                "2" = "EIN", 
                                                "3" = "Cancer",
                                                "Benign")  # default
                }
            }
            
            # Create long format data for ggplot
            heatmap_data <- data.frame()
            for (case_idx in 1:n_cases) {
                original_case_id <- case_order[case_idx]
                for (rater_idx in 1:length(ordered_raters)) {
                    diagnosis <- ordered_data[case_idx, rater_idx]
                    heatmap_data <- rbind(heatmap_data, data.frame(
                        Case = case_idx,
                        OriginalCase = original_case_id,
                        Rater = ordered_raters[rater_idx],
                        RaterIndex = rater_idx,
                        Diagnosis = as.character(diagnosis),
                        StyleGroup = ordered_groups[rater_idx]
                    ))
                }
            }
            
            # Set factor levels for proper ordering
            heatmap_data$Rater <- factor(heatmap_data$Rater, levels = ordered_raters)
            heatmap_data$Case <- factor(heatmap_data$Case, levels = n_cases:1)  # Reverse order for bottom-to-top
            
            # Define colors matching Usubutun paper
            diagnosis_colors <- c(
                "Benign" = "#4472C4",    # Blue for Benign
                "EIN" = "#70AD47",       # Green for EIN  
                "Cancer" = "#FFC000"     # Gold for Cancer
            )
            
            # Create dendrograms for both dimensions
            rater_dend_data <- dendro_data(private$.style_clustering_results$hclust_object)
            
            # Create the main heatmap
            main_heatmap <- ggplot(heatmap_data, aes(x = Rater, y = Case, fill = Diagnosis)) +
                geom_tile(color = "white", size = 0.1) +
                scale_fill_manual(values = diagnosis_colors, name = "Diagnosis") +
                labs(x = "Pathologist", y = "Case") +
                theme_classic() +
                theme(
                    axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
                    axis.text.y = element_text(size = 4),
                    axis.ticks = element_blank(),
                    panel.grid = element_blank(),
                    legend.position = "right",
                    plot.margin = margin(5, 5, 5, 5)
                ) +
                scale_x_discrete(expand = c(0, 0)) +
                scale_y_discrete(expand = c(0, 0))
            
            # Create rater dendrogram (top)
            rater_dend_plot <- ggplot() +
                geom_segment(data = rater_dend_data$segments,
                           aes(x = x, y = y, xend = xend, yend = yend),
                           color = "black", size = 0.3) +
                theme_void() +
                theme(plot.margin = margin(0, 5, 0, 5)) +
                scale_x_continuous(expand = c(0.02, 0)) +
                coord_cartesian(ylim = c(0, max(rater_dend_data$segments$y, na.rm = TRUE)))
            
            # Create case dendrogram (left) - rotated 90 degrees
            if (!is.null(case_hc)) {
                case_dend_data <- dendro_data(case_hc)
                case_dend_plot <- ggplot() +
                    geom_segment(data = case_dend_data$segments,
                               aes(x = y, y = x, xend = yend, yend = xend),  # Swap x/y for rotation
                               color = "black", size = 0.3) +
                    theme_void() +
                    theme(plot.margin = margin(5, 0, 5, 0)) +
                    scale_y_continuous(expand = c(0.02, 0), trans = "reverse") +  # Reverse to match heatmap
                    coord_cartesian(xlim = c(0, max(case_dend_data$segments$y, na.rm = TRUE)))
            } else {
                # Create empty plot if case clustering failed
                case_dend_plot <- ggplot() +
                    geom_text(aes(x = 0.5, y = 0.5, label = "Case clustering\nskipped"), 
                             size = 3, hjust = 0.5) +
                    xlim(0, 1) + ylim(0, 1) +
                    theme_void() +
                    theme(plot.margin = margin(5, 0, 5, 0))
            }
            
            # Create empty plot for corner
            empty_plot <- ggplot() + theme_void()
            
            # Combine all plots using grid.arrange with proper proportions
            combined_plot <- grid.arrange(
                rater_dend_plot, empty_plot,
                main_heatmap, case_dend_plot,
                nrow = 2, ncol = 2,
                widths = c(4, 1),      # Main plot wider than case dendrogram
                heights = c(1, 4),     # Main plot taller than rater dendrogram
                top = textGrob("Two-Way Clustered Diagnostic Style Heatmap", 
                              gp = gpar(fontsize = 14, fontface = "bold"))
            )
            
            grid.draw(combined_plot)
            TRUE
        },

        # Combined dendrogram and heatmap - Exact reproduction of Usubutun et al. Figure 1
        .diagnosticStyleCombined = function(image, ggtheme, theme, ...) {
            if (!self$options$performClustering || is.null(private$.style_clustering_results)) {
                p <- ggplot() +
                    geom_text(aes(x = 0.5, y = 0.5, label = "Enable Diagnostic Style Analysis\nto view combined visualization"),
                             size = 8, hjust = 0.5, vjust = 0.5) +
                    xlim(0, 1) + ylim(0, 1) +
                    theme_void() +
                    labs(title = "Combined Dendrogram + Heatmap")
                print(p)
                return(TRUE)
            }

            # Get clustering results and data
            hc <- private$.style_clustering_results$hclust_object
            style_groups <- private$.style_clustering_results$cluster_assignments
            rater_names <- private$.rater_names
            data_matrix <- private$.data_matrix
            n_cases <- private$.n_cases
            n_groups <- private$.style_clustering_results$n_clusters

            # Check if n_groups is valid
            if (is.null(n_groups) || length(n_groups) == 0 || n_groups < 2) {
                # Fallback to option value or default to 3
                n_groups <- self$options$nStyleGroups
                if (is.null(n_groups) || length(n_groups) == 0 || n_groups < 2) {
                    n_groups <- 3
                }
            }

            # Load required packages
            require(ggdendro)
            require(gridExtra)
            require(grid)

            # === CREATE DENDROGRAM (TOP PART) ===
            dend_data <- dendro_data(hc)

            # Define colors
            if (n_groups == 3) {
                group_colors <- c("#2E8B57", "#FFD700", "#DC143C")  # Green, Gold, Crimson
            } else {
                group_colors <- rainbow(n_groups)
            }

            # Order raters by clustering result (same order as heatmap)
            rater_order <- order(style_groups)
            ordered_raters <- rater_names[rater_order]
            ordered_groups <- style_groups[rater_order]

            # Create color mapping for raters
            rater_colors <- rep("black", length(rater_names))
            for (i in 1:length(rater_names)) {
                group_id <- style_groups[i]
                if (!is.null(group_id) && !is.na(group_id) && group_id > 0 && group_id <= length(group_colors)) {
                    rater_colors[i] <- group_colors[group_id]
                }
            }

            # Create dendrogram plot
            dend_plot <- ggplot() +
                # Draw dendrogram tree structure
                geom_segment(data = dend_data$segments,
                           aes(x = x, y = y, xend = xend, yend = yend),
                           color = "black", size = 0.5, lineend = "round") +
                # Add colored rectangles for style groups at top
                {
                  group_rects <- data.frame()
                  for (group in 1:n_groups) {
                    group_members <- which(ordered_groups == group)
                    if (length(group_members) > 0) {
                      x_min <- min(group_members) - 0.4
                      x_max <- max(group_members) + 0.4
                      y_top <- max(dend_data$segments$y, na.rm = TRUE)
                      group_rects <- rbind(group_rects, data.frame(
                        xmin = x_min, xmax = x_max,
                        ymin = y_top * 1.02, ymax = y_top * 1.12,
                        fill = group_colors[group]
                      ))
                    }
                  }
                  if (nrow(group_rects) > 0) {
                    geom_rect(data = group_rects, 
                             aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                             fill = group_rects$fill, color = "black", size = 0.3, alpha = 0.8)
                  }
                } +
                # Add style group labels
                {
                  group_labels <- data.frame()
                  group_names <- c("GREEN", "YELLOW", "RED")
                  for (group in 1:min(n_groups, 3)) {
                    group_members <- which(ordered_groups == group)
                    if (length(group_members) > 0) {
                      x_center <- mean(group_members)
                      y_top <- max(dend_data$segments$y, na.rm = TRUE)
                      group_labels <- rbind(group_labels, data.frame(
                        x = x_center, y = y_top * 1.07,
                        label = group_names[group]
                      ))
                    }
                  }
                  if (nrow(group_labels) > 0) {
                    geom_text(data = group_labels,
                             aes(x = x, y = y, label = label),
                             color = "white", size = 3, fontface = "bold")
                  }
                } +
                scale_x_continuous(expand = c(0.02, 0.02)) +
                scale_y_continuous(expand = c(0.02, 0.15)) +
                theme_void() +
                theme(plot.margin = margin(5, 5, 0, 5))

            # === CREATE HEATMAP (BOTTOM PART) WITH CASE CLUSTERING ===
            
            # First cluster cases (rows) with proper validation
            # Create numeric matrix for case clustering
            numeric_matrix <- data_matrix
            
            # Convert to numeric with proper handling of missing values
            for (i in 1:nrow(numeric_matrix)) {
                for (j in 1:ncol(numeric_matrix)) {
                    val <- as.character(numeric_matrix[i, j])
                    if (is.na(val) || val == "" || val == "NA") {
                        numeric_matrix[i, j] <- 1  # Default to Benign for missing
                    } else {
                        numeric_matrix[i, j] <- switch(val,
                                                     "Benign" = 1,
                                                     "EIN" = 2, 
                                                     "Cancer" = 3,
                                                     1)  # default to Benign for unrecognized
                    }
                }
            }
            
            # Ensure all values are numeric and finite
            numeric_matrix <- apply(numeric_matrix, c(1,2), as.numeric)
            
            # Check for any remaining NA/NaN/Inf values
            if (any(is.na(numeric_matrix)) || any(is.infinite(numeric_matrix))) {
                # If there are still problematic values, skip clustering and use original order
                case_order <- 1:n_cases
                case_hc <- NULL
                message("Case clustering skipped due to data issues - using original order")
            } else {
                # Calculate case similarities (agreement across raters)
                case_dist <- dist(numeric_matrix, method = "euclidean")
                
                # Check if distance matrix is valid
                if (any(is.na(case_dist)) || any(is.infinite(case_dist))) {
                    case_order <- 1:n_cases
                    case_hc <- NULL
                    message("Case clustering skipped due to distance calculation issues")
                } else {
                    case_hc <- hclust(case_dist, method = "ward.D2")
                    case_order <- case_hc$order
                }
            }
            
            # Order data matrix by both clustering results (cases and raters)
            ordered_data <- data_matrix[case_order, rater_order]
            
            # Create heatmap data with both dimensions clustered
            heatmap_data <- data.frame()
            for (case_idx in 1:n_cases) {
                original_case_id <- case_order[case_idx]
                for (rater_id in 1:length(ordered_raters)) {
                    diagnosis <- ordered_data[case_idx, rater_id]
                    heatmap_data <- rbind(heatmap_data, data.frame(
                        Case = case_idx,
                        OriginalCase = original_case_id,
                        Rater = rater_id,
                        RaterName = ordered_raters[rater_id],
                        Diagnosis = as.character(diagnosis),
                        StyleGroup = ordered_groups[rater_id]
                    ))
                }
            }
            
            # Define diagnosis colors matching Usubutun paper
            diagnosis_colors <- c(
                "Benign" = "#4472C4",    # Blue
                "EIN" = "#70AD47",       # Green  
                "Cancer" = "#FFC000"     # Gold
            )
            
            # Create heatmap plot
            heatmap_plot <- ggplot(heatmap_data, aes(x = Rater, y = Case, fill = Diagnosis)) +
                geom_tile(color = "white", size = 0.1) +
                scale_fill_manual(values = diagnosis_colors, name = "Diagnosis") +
                scale_x_continuous(breaks = 1:length(ordered_raters), 
                                 labels = ordered_raters,
                                 expand = c(0, 0)) +
                scale_y_continuous(trans = "reverse", 
                                 breaks = c(1, seq(10, n_cases, 10)),
                                 expand = c(0, 0)) +
                labs(x = "Pathologist", y = "Case") +
                theme_classic() +
                theme(
                    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
                    axis.text.y = element_text(size = 6),
                    axis.title = element_text(size = 10),
                    legend.position = "right",
                    legend.title = element_text(size = 10),
                    legend.text = element_text(size = 8),
                    panel.grid = element_blank(),
                    plot.margin = margin(0, 5, 5, 5)
                )

            # === CREATE CASE DENDROGRAM (LEFT SIDE) ===
            if (!is.null(case_hc)) {
                case_dend_data <- dendro_data(case_hc)
                
                # Create case dendrogram (rotated 90 degrees to align with heatmap)
                case_dend_plot <- ggplot() +
                    geom_segment(data = case_dend_data$segments,
                               aes(x = y, y = x, xend = yend, yend = xend),  # Swap x/y for rotation
                               color = "black", size = 0.3) +
                    scale_y_continuous(expand = c(0.02, 0.02), trans = "reverse") +  # Reverse to match heatmap
                    scale_x_continuous(expand = c(0.02, 0.02)) +
                    theme_void() +
                    theme(plot.margin = margin(0, 0, 5, 5))
            } else {
                # Create empty plot if case clustering failed
                case_dend_plot <- ggplot() +
                    geom_text(aes(x = 0.5, y = 0.5, label = "Case clustering\nskipped"), 
                             size = 3, hjust = 0.5) +
                    xlim(0, 1) + ylim(0, 1) +
                    theme_void() +
                    theme(plot.margin = margin(0, 0, 5, 5))
            }
            
            # Create empty corner plot
            empty_plot <- ggplot() + theme_void()
            
            # === COMBINE ALL PLOTS ===
            # Create a comprehensive two-way clustered plot with dendrograms on both axes
            combined_plot <- grid.arrange(
                empty_plot, dend_plot,
                case_dend_plot, heatmap_plot,
                nrow = 2, ncol = 2,
                widths = c(1, 4),      # Case dendrogram narrower than main plot
                heights = c(1, 4),     # Rater dendrogram shorter than main plot  
                top = textGrob("Two-Way Clustered Diagnostic Patterns (Usubutun Style)", 
                              gp = gpar(fontsize = 14, fontface = "bold"))
            )

            # Print the combined plot
            grid.draw(combined_plot)
            TRUE
        },
        
        # Data validation
        .validateData = function() {
            # Check for factor variables
            var_names <- self$options$vars
            non_factors <- c()
            
            for (var_name in var_names) {
                var_data <- self$data[[var_name]]
                if (!is.factor(var_data)) {
                    non_factors <- c(non_factors, var_name)
                }
            }
            
            if (length(non_factors) > 0) {
                stop(paste0("The following variables are not factors: ", 
                           paste(non_factors, collapse = ", "), 
                           ". Please convert them to factors before analysis."))
            }
            
            # Check for consistent categories across raters
            all_levels <- lapply(var_names, function(var) levels(self$data[[var]]))
            reference_levels <- all_levels[[1]]
            
            inconsistent_vars <- c()
            for (i in 2:length(all_levels)) {
                if (!identical(sort(all_levels[[i]]), sort(reference_levels))) {
                    inconsistent_vars <- c(inconsistent_vars, var_names[i])
                }
            }
            
            if (length(inconsistent_vars) > 0) {
                warning(paste0("The following variables have different factor levels than the first variable: ", 
                              paste(inconsistent_vars, collapse = ", "), 
                              ". This may affect agreement calculations."))
            }
            
            # Check for sufficient data
            complete_cases <- sum(complete.cases(self$data[var_names]))
            if (complete_cases < 10) {
                warning("Very few complete cases (n=", complete_cases, 
                       "). Results may be unreliable.")
            }
            
            # Check for empty categories
            for (var_name in var_names) {
                var_data <- self$data[[var_name]]
                empty_levels <- levels(var_data)[!levels(var_data) %in% var_data]
                if (length(empty_levels) > 0) {
                    warning(paste0("Variable '", var_name, "' has unused factor levels: ", 
                                  paste(empty_levels, collapse = ", "), 
                                  ". Consider using droplevels() to remove them."))
                }
            }
            
            # Check for single-category data
            for (var_name in var_names) {
                var_data <- self$data[[var_name]]
                unique_values <- length(unique(var_data[!is.na(var_data)]))
                if (unique_values < 2) {
                    stop(paste0("Variable '", var_name, "' has only ", unique_values, 
                               " unique value(s). Agreement analysis requires at least 2 categories."))
                }
            }
        },

        # Package dependency checking
        .checkPackageDependencies = function() {
            required_packages <- c("irr", "psych", "stringr", "scales")
            missing_packages <- character(0)
            
            for (pkg in required_packages) {
                if (!requireNamespace(pkg, quietly = TRUE)) {
                    missing_packages <- c(missing_packages, pkg)
                }
            }
            
            if (length(missing_packages) > 0) {
                stop(paste0("The following required packages are missing: ", 
                           paste(missing_packages, collapse = ", "), 
                           ". Please install them using install.packages()"))
            }
        },

        # Alternative heatmap visualization (percentage agreement)
        .heatmapPlotPercentage = function(image, ggtheme, theme, ...) {
            if (!self$options$heatmap || is.null(private$.data_matrix)) {
                return(FALSE)
            }

            # Create pairwise agreement matrix
            n_raters <- ncol(private$.data_matrix)
            rater_names <- colnames(private$.data_matrix)
            
            # Initialize agreement matrix
            agreement_matrix <- matrix(NA, nrow = n_raters, ncol = n_raters,
                                     dimnames = list(rater_names, rater_names))
            
            # Fill diagonal with 1.0 (perfect self-agreement)
            diag(agreement_matrix) <- 1.0
            
            # Calculate pairwise agreements using optimized approach
            private$.checkpoint(flush = FALSE)
            
            # Use optimized calculation method based on dataset size
            if (n_raters > 10 && nrow(private$.data_matrix) > 200) {
                # For very large datasets, use outer product approach
                agreement_matrix <- private$.calculateAgreementMatrixOptimized(private$.data_matrix, rater_names)
            } else {
                # For moderate datasets, use simple pairwise calculation
                for (i in 1:(n_raters-1)) {
                    for (j in (i+1):n_raters) {
                        agreement_pct <- mean(private$.data_matrix[,i] == private$.data_matrix[,j], na.rm = TRUE)
                        agreement_matrix[i,j] <- agreement_matrix[j,i] <- agreement_pct
                    }
                }
            }
            
            # Convert to long format for ggplot
            agreement_df <- expand.grid(Rater1 = rater_names, Rater2 = rater_names, stringsAsFactors = FALSE)
            agreement_df$Agreement <- as.numeric(agreement_matrix)
            
            # Create base heatmap with theme selection
            theme_option <- self$options$heatmapTheme
            
            # Define color-blind-safe color scales
            color_scale <- switch(theme_option,
                "viridis" = scale_fill_viridis_c(name = "Agreement", labels = scales::percent_format(), option = "viridis"),
                "plasma" = scale_fill_viridis_c(name = "Agreement", labels = scales::percent_format(), option = "plasma"),
                "cividis" = scale_fill_viridis_c(name = "Agreement", labels = scales::percent_format(), option = "cividis"),
                "bwr" = scale_fill_gradient2(low = "#2166ac", mid = "#f7f7f7", high = "#b2182b",
                                           midpoint = 0.5, name = "Agreement", labels = scales::percent_format()),
                "ryg" = scale_fill_gradient2(low = "#d73027", mid = "#fee08b", high = "#1a9850",
                                           midpoint = 0.5, name = "Agreement", labels = scales::percent_format()),
                # Default to viridis if unknown theme
                scale_fill_viridis_c(name = "Agreement", labels = scales::percent_format(), option = "viridis")
            )
            
            p <- ggplot(agreement_df, aes(x = Rater1, y = Rater2, fill = Agreement)) +
                geom_tile(color = "white", size = 0.5) +
                color_scale +
                labs(title = "Inter-rater Agreement Heatmap",
                     x = "Rater", y = "Rater") +
                theme_minimal() +
                theme(
                    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
                    axis.text.y = element_text(hjust = 1),
                    plot.title = element_text(hjust = 0.5),
                    legend.position = "right"
                )
            
            # Add text annotations if heatmapDetails is enabled
            if (self$options$heatmapDetails) {
                p <- p + geom_text(aes(label = scales::percent(Agreement, accuracy = 1)), 
                                 color = "black", size = 3)
            }
            
            print(p)
            return(TRUE)
        },

        # ============================================================================
        # CLINICAL SUMMARY AND EDUCATIONAL CONTENT
        # ============================================================================

        .showWelcomeMessage = function() {
            # Create clean, accessible welcome message similar to decisionpanel
            welcome_html <- paste0(
                "<div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.4;'>",
                "<div style='background: #f5f5f5; border: 2px solid #2e7d32; padding: 20px; margin-bottom: 20px;'>",
                "<h2 style='margin: 0 0 10px 0; font-size: 20px; color: #2e7d32;'>Inter-rater Reliability Analysis</h2>",
                "<p style='margin: 0; font-size: 14px; color: #666;'>Evaluate agreement between multiple raters/observers using statistical measures</p>",
                "</div>",
                
                "<div style='background: #f9f9f9; border-left: 4px solid #2e7d32; padding: 15px; margin-bottom: 20px;'>",
                "<h3 style='margin: 0 0 10px 0; color: #2e7d32; font-size: 16px;'>Setup Progress</h3>"
            )
            
            # Progress indicators
            n_vars <- length(self$options$vars)
            if (n_vars >= 2) {
                welcome_html <- paste0(welcome_html,
                    "<div style='font-weight: bold; margin-bottom: 10px; color: #2e7d32;'>",
                    "[READY] ", n_vars, " rater variables selected</div>",
                    "<p style='margin: 0;'>Minimum requirements met. Analysis will begin automatically.</p>"
                )
            } else {
                welcome_html <- paste0(welcome_html,
                    "<div style='margin-bottom: 10px;'>",
                    if(n_vars >= 2) "[✓]" else "[ ]", " Rater Variables: ", n_vars, "/2 minimum</div>",
                    "<p style='margin: 0; color: #666;'>Select at least 2 rater variables to proceed with analysis.</p>"
                )
            }
            
            welcome_html <- paste0(welcome_html,
                "</div>",
                
                "<table style='width: 100%; border-collapse: collapse; margin-bottom: 20px;'>",
                "<tr>",
                "<td style='width: 50%; border: 1px solid #ccc; padding: 15px; vertical-align: top;'>",
                "<h4 style='margin: 0 0 10px 0; font-size: 15px; color: #2e7d32;'>Quick Start Guide</h4>",
                "<ol style='margin: 0; padding-left: 20px; font-size: 14px;'>",
                "<li>Select <strong>2 or more rater variables</strong> from your dataset</li>",
                "<li>Ensure all raters use the same rating scale/categories</li>",
                "<li>Choose appropriate <strong>weighting scheme</strong> (unweighted for nominal, weighted for ordinal)</li>",
                "<li>Optionally enable <strong>frequency tables</strong> and <strong>heatmap visualization</strong></li>",
                "<li>Advanced users: Try <strong>Krippendorff's alpha</strong> or <strong>consensus analysis</strong></li>",
                "</ol></td>",
                
                "<td style='width: 50%; border: 1px solid #ccc; padding: 15px; vertical-align: top;'>",
                "<h4 style='margin: 0 0 10px 0; font-size: 15px; color: #2e7d32;'>What You'll Get</h4>",
                "<ul style='margin: 0; padding-left: 20px; font-size: 14px;'>",
                "<li><strong>Kappa statistics</strong> with confidence intervals and interpretation</li>",
                "<li><strong>Overall agreement percentage</strong> and summary statistics</li>",
                "<li><strong>Clinical interpretation</strong> of agreement levels</li>",
                "<li><strong>Frequency tables</strong> showing rater distributions</li>",
                "<li><strong>Agreement heatmap</strong> with customizable color themes</li>",
                "<li><strong>Consensus analysis</strong> for determining agreed ratings</li>",
                "</ul></td></tr></table>",
                
                "<div style='background: #fff3e0; border: 1px solid #f57c00; padding: 15px;'>",
                "<h4 style='margin: 0 0 10px 0; font-size: 15px; color: #f57c00;'>Important Notes for Clinical Use</h4>",
                "<ul style='margin: 0; padding-left: 20px; font-size: 14px;'>",
                "<li><strong>Sample size:</strong> At least 30 cases recommended for reliable kappa estimates</li>",
                "<li><strong>Category balance:</strong> Avoid rare categories (<5% of cases) if possible</li>",
                "<li><strong>Interpretation:</strong> κ < 0.4 = poor/fair, 0.4-0.6 = moderate, 0.6-0.8 = good, >0.8 = excellent</li>",
                "<li><strong>Clinical context:</strong> Consider the clinical consequences of disagreement when interpreting results</li>",
                "</ul></div></div>"
            )
            
            self$results$todo$setContent(welcome_html)
        },

        .generateClinicalSummary = function() {
            # Generate clinical summary only if analysis has been performed
            if (is.null(private$.data_matrix) || is.null(private$.n_raters) || private$.n_raters < 2) {
                self$results$clinicalSummary$setVisible(FALSE)
                return()
            }

            # Get main kappa result from the kappa table
            kappa_table <- self$results$kappaTable
            if (kappa_table$rowCount == 0) {
                self$results$clinicalSummary$setVisible(FALSE)
                return()
            }

            # Extract kappa value and interpretation from first row using row index
            # Try to safely get the first row from the kappa table
            kappa_val <- NA
            interpretation <- ""
            method_name <- ""
            p_value <- NA
            
            # Use tryCatch to handle potential table access errors
            tryCatch({
                if (length(kappa_table$rowKeys) > 0) {
                    # Try direct row access by index position
                    first_key <- kappa_table$rowKeys[[1]]
                    
                    # For regular jamovi tables, try accessing columns directly
                    kappa_cell <- kappa_table$getCell(rowKey=first_key, "kappa")
                    interpretation <- kappa_table$getCell(rowKey=first_key, "interpretation")
                    method_name <- kappa_table$getCell(rowKey=first_key, "method")
                    p_value <- kappa_table$getCell(rowKey=first_key, "p")
                    
                    # Convert kappa_cell to numeric if it's not already
                    if (!is.null(kappa_cell)) {
                        kappa_val <- tryCatch({
                            as.numeric(kappa_cell)
                        }, error = function(e) {
                            # If conversion fails, try to extract from environment or list
                            if (is.environment(kappa_cell) || is.list(kappa_cell)) {
                                NA
                            } else {
                                as.numeric(kappa_cell)
                            }
                        })
                    }
                }
            }, error = function(e) {
                # If table access fails, don't show clinical summary
                message("Could not access kappa table data: ", e$message)
                self$results$clinicalSummary$setVisible(FALSE)
                return(NULL)
            })
            
            # Check if we got valid data (handle vector case)
            if (is.null(kappa_val) || length(kappa_val) == 0 || all(is.na(kappa_val))) {
                self$results$clinicalSummary$setVisible(FALSE)
                return()
            }
            
            # Ensure kappa_val is numeric and scalar
            if (length(kappa_val) > 1) {
                kappa_val <- as.numeric(kappa_val[1])
            } else {
                kappa_val <- as.numeric(kappa_val)
            }
            
            # Final check that kappa_val is a valid numeric value
            if (!is.numeric(kappa_val) || is.na(kappa_val)) {
                self$results$clinicalSummary$setVisible(FALSE)
                return()
            }
            if (length(interpretation) > 1) {
                interpretation <- interpretation[1]
            }
            if (length(method_name) > 1) {
                method_name <- method_name[1]
            }
            if (length(p_value) > 1) {
                p_value <- p_value[1]
            }

            # Get overall agreement percentage
            overview_table <- self$results$overviewTable
            overall_agreement <- NA
            tryCatch({
                if (overview_table$rowCount > 0 && length(overview_table$rowKeys) > 0) {
                    first_overview_key <- overview_table$rowKeys[[1]]
                    overall_agreement <- overview_table$getCell(rowKey=first_overview_key, "overall_agreement")
                    # Handle vector case
                    if (length(overall_agreement) > 1) {
                        overall_agreement <- overall_agreement[1]
                    }
                }
            }, error = function(e) {
                message("Could not access overview table data: ", e$message)
                overall_agreement <- NA
            })

            # Generate clinical interpretation
            clinical_interp <- private$.getClinicalInterpretation(kappa_val)

            summary_html <- paste0(
                "<div style='font-family: Arial, sans-serif; max-width: 700px; line-height: 1.5;'>",
                "<div style='background: #e8f5e8; border-left: 4px solid #2e7d32; padding: 15px; margin-bottom: 15px;'>",
                "<h4 style='margin: 0 0 10px 0; color: #2e7d32; font-size: 16px;'>Clinical Summary</h4>",
                "<p style='margin: 0; font-size: 14px;'>",
                "Inter-rater agreement analysis of <strong>", private$.n_raters, " raters</strong> evaluating <strong>", 
                private$.n_cases, " cases</strong> using <strong>", length(private$.categories), " categories</strong>.</p>",
                "</div>",

                "<div style='background: #f8f8f8; border: 1px solid #ddd; padding: 15px; margin-bottom: 15px;'>",
                "<h4 style='margin: 0 0 10px 0; color: #333; font-size: 15px;'>Key Findings</h4>",
                "<p style='margin: 0 0 10px 0; font-size: 14px;'><strong>", method_name, ":</strong> κ = ", 
                round(kappa_val, 3), " (", interpretation, ")</p>",
                if(!is.na(overall_agreement)) paste0(
                    "<p style='margin: 0 0 10px 0; font-size: 14px;'><strong>Overall Agreement:</strong> ", 
                    round(overall_agreement, 1), "%</p>"
                ) else "",
                "<p style='margin: 0; font-size: 14px;'><strong>Statistical Significance:</strong> ",
                if(p_value < 0.001) "p < 0.001" else paste0("p = ", round(p_value, 3)), "</p>",
                "</div>",

                "<div style='background: #fff8e1; border: 1px solid #ffa000; padding: 15px;'>",
                "<h4 style='margin: 0 0 10px 0; color: #f57c00; font-size: 15px;'>Clinical Interpretation</h4>",
                "<p style='margin: 0; font-size: 14px;'>", clinical_interp, "</p>",
                "</div></div>"
            )

            self$results$clinicalSummary$setContent(summary_html)
            self$results$clinicalSummary$setVisible(TRUE)
            
            # Generate copy-ready report template
            private$.generateReportTemplate(kappa_val, method_name, p_value, overall_agreement, interpretation)
        },

        .generateReportTemplate = function(kappa_val, method_name, p_value, overall_agreement, interpretation) {
            # Generate copy-ready report sentences for clinical/research use
            
            # Get confidence interval if available
            kappa_table <- self$results$kappaTable
            ci_text <- ""
            if (kappa_table$rowCount > 0 && length(kappa_table$rowKeys) > 0) {
                tryCatch({
                    first_key <- kappa_table$rowKeys[[1]]
                    ci_lower <- kappa_table$getCell(rowKey=first_key, "ci_lower")
                    ci_upper <- kappa_table$getCell(rowKey=first_key, "ci_upper")
                    if (!is.na(ci_lower) && !is.na(ci_upper)) {
                        ci_text <- sprintf(", 95%% CI %.3f-%.3f", ci_lower, ci_upper)
                    }
                }, error = function(e) {
                    ci_text <- ""
                })
            }
            
            # Format p-value appropriately
            p_text <- if (is.na(p_value)) {
                ""
            } else if (p_value < 0.001) {
                ", p < 0.001"
            } else {
                sprintf(", p = %.3f", p_value)
            }
            
            # Generate main result sentence
            main_sentence <- if (!is.na(kappa_val)) {
                sprintf(.("Inter-rater agreement was %s (κ = %.3f%s%s), indicating %s reliability for the %d raters evaluating %d cases."),
                        tolower(interpretation),
                        kappa_val,
                        ci_text,
                        p_text,
                        tolower(interpretation),
                        private$.n_raters,
                        private$.n_cases)
            } else {
                sprintf(.("Inter-rater agreement could not be reliably calculated for the %d raters evaluating %d cases. Data quality or sample size may be insufficient."),
                        private$.n_raters,
                        private$.n_cases)
            }
            
            # Add overall agreement if available
            overall_sentence <- if (!is.na(overall_agreement)) {
                sprintf(.("Overall percentage agreement was %.1f%%."), overall_agreement)
            } else {
                ""
            }
            
            # Add method-specific information
            method_sentence <- sprintf(.("Agreement was assessed using %s."), method_name)
            
            # Clinical recommendation based on kappa value
            recommendation <- if (!is.na(kappa_val)) {
                if (kappa_val >= 0.8) {
                    .("This level of agreement is excellent and suitable for all clinical applications including critical diagnoses.")
                } else if (kappa_val >= 0.6) {
                    .("This level of agreement is good and suitable for most clinical and research applications.")
                } else if (kappa_val >= 0.4) {
                    .("This level of agreement is moderate and may be acceptable for some clinical applications but consider additional training for critical diagnoses.")
                } else {
                    .("This level of agreement is poor to fair and requires improvement through additional training or revised criteria before clinical use.")
                }
            } else {
                .("Agreement could not be assessed - review data quality and sample size requirements.")
            }
            
            # Create copy-ready template
            report_html <- paste0(
                "<div style='font-family: Arial, sans-serif; max-width: 700px; line-height: 1.6;'>",
                "<div style='background: #e3f2fd; border: 1px solid #1976d2; padding: 15px; margin-bottom: 15px;'>",
                "<h4 style='margin: 0 0 10px 0; color: #1976d2; font-size: 16px;'>📋 Copy-Ready Report Template</h4>",
                "<p style='margin: 0; font-size: 13px; color: #666;'>Click inside the boxes to select text, then copy (Ctrl+C/Cmd+C) for use in reports:</p>",
                "</div>",
                
                "<div style='background: #f8f9fa; border: 1px solid #dee2e6; padding: 12px; margin-bottom: 10px; border-radius: 4px;'>",
                "<h5 style='margin: 0 0 8px 0; font-size: 14px; color: #495057;'>Main Result:</h5>",
                "<div style='font-family: Times, serif; font-size: 14px; line-height: 1.5; padding: 8px; background: white; border: 1px solid #ced4da; cursor: text;' onclick='this.select()' contenteditable='false'>",
                main_sentence,
                "</div></div>",
                
                if (nchar(overall_sentence) > 0) paste0(
                    "<div style='background: #f8f9fa; border: 1px solid #dee2e6; padding: 12px; margin-bottom: 10px; border-radius: 4px;'>",
                    "<h5 style='margin: 0 0 8px 0; font-size: 14px; color: #495057;'>Additional Detail:</h5>",
                    "<div style='font-family: Times, serif; font-size: 14px; line-height: 1.5; padding: 8px; background: white; border: 1px solid #ced4da; cursor: text;' onclick='this.select()' contenteditable='false'>",
                    paste(method_sentence, overall_sentence),
                    "</div></div>"
                ) else "",
                
                "<div style='background: #f8f9fa; border: 1px solid #dee2e6; padding: 12px; margin-bottom: 10px; border-radius: 4px;'>",
                "<h5 style='margin: 0 0 8px 0; font-size: 14px; color: #495057;'>Clinical Interpretation:</h5>",
                "<div style='font-family: Times, serif; font-size: 14px; line-height: 1.5; padding: 8px; background: white; border: 1px solid #ced4da; cursor: text;' onclick='this.select()' contenteditable='false'>",
                recommendation,
                "</div></div>",
                
                "<div style='background: #fff3e0; border: 1px solid #f57c00; padding: 10px; font-size: 12px;'>",
                "<strong>💡 Tip:</strong> Combine these sentences and adapt the language to match your publication style. ",
                "Consider adding information about rater training, case characteristics, or clinical context as appropriate.",
                "</div></div>"
            )
            
            self$results$reportTemplate$setContent(report_html)
            self$results$reportTemplate$setVisible(TRUE)
        },

        .getClinicalInterpretation = function(kappa) {
            if (is.na(kappa)) {
                return(.("Could not calculate reliable agreement measure. Check your data for sufficient cases and category distribution."))
            }

            if (kappa < 0) {
                return(.("Agreement is worse than chance - raters systematically disagree. Review rating criteria and consider additional training."))
            } else if (kappa < 0.20) {
                return(.("Agreement is <strong>poor</strong> - raters are essentially making independent judgments. Consider revising diagnostic criteria, providing additional training, or using consensus panels for critical diagnoses."))
            } else if (kappa < 0.40) {
                return(.("Agreement is <strong>fair</strong> - some consistency between raters but significant disagreement remains. Review cases with disagreement and consider standardizing evaluation procedures."))
            } else if (kappa < 0.60) {
                return(.("Agreement is <strong>moderate</strong> - acceptable for many clinical applications but may need improvement for critical diagnoses. Consider additional training or clearer diagnostic guidelines."))
            } else if (kappa < 0.80) {
                return(.("Agreement is <strong>good</strong> - suitable for most clinical and research applications. This level indicates reliable inter-rater consistency."))
            } else {
                return(.("Agreement is <strong>excellent</strong> - raters show high consistency, suitable for all clinical applications including critical diagnoses and research studies."))
            }
        },

        .generateAboutAnalysis = function() {
            about_html <- paste0(
                "<div style='font-family: Arial, sans-serif; max-width: 700px; line-height: 1.5;'>",
                "<div style='background: #f5f5f5; border: 2px solid #2e7d32; padding: 20px; margin-bottom: 20px;'>",
                "<h3 style='margin: 0 0 15px 0; color: #2e7d32; font-size: 18px;'>About Inter-rater Reliability Analysis</h3>",
                
                "<h4 style='margin: 15px 0 8px 0; color: #333; font-size: 15px;'>What does this analysis do?</h4>",
                "<p style='margin: 0 0 12px 0; font-size: 14px;'>",
                "This analysis measures how consistently different raters (observers, pathologists, clinicians) ",
                "evaluate the same cases. It's essential for validating diagnostic consistency, training effectiveness, ",
                "and research reliability in clinical settings.</p>",
                
                "<h4 style='margin: 15px 0 8px 0; color: #333; font-size: 15px;'>When to use it?</h4>",
                "<ul style='margin: 0 0 12px 0; padding-left: 20px; font-size: 14px;'>",
                "<li>Validating diagnostic consistency between pathologists</li>",
                "<li>Training new staff and measuring competency</li>",
                "<li>Research studies requiring reliable measurements</li>",
                "<li>Quality assurance in clinical laboratories</li>",
                "<li>Establishing inter-institutional agreement</li>",
                "</ul>",
                
                "<h4 style='margin: 15px 0 8px 0; color: #333; font-size: 15px;'>What you need:</</h4>",
                "<ul style='margin: 0 0 12px 0; padding-left: 20px; font-size: 14px;'>",
                "<li><strong>2 or more rater variables</strong> - each representing ratings by different observers</li>",
                "<li><strong>Same rating scale</strong> - all raters must use identical categories</li>",
                "<li><strong>Adequate sample size</strong> - minimum 30 cases recommended</li>",
                "<li><strong>Independent ratings</strong> - raters should evaluate cases independently</li>",
                "</ul>",
                
                "<h4 style='margin: 15px 0 8px 0; color: #333; font-size: 15px;'>Key outputs:</h4>",
                "<ul style='margin: 0; padding-left: 20px; font-size: 14px;'>",
                "<li><strong>Kappa coefficient (κ)</strong> - primary agreement measure corrected for chance</li>",
                "<li><strong>95% Confidence intervals</strong> - precision of the agreement estimate</li>",
                "<li><strong>P-values</strong> - statistical significance testing</li>",
                "<li><strong>Clinical interpretation</strong> - practical meaning of agreement levels</li>",
                "<li><strong>Frequency tables & visualizations</strong> - detailed breakdown of agreement patterns</li>",
                "</ul>",
                "</div></div>"
            )

            self$results$aboutAnalysis$setContent(about_html)
            self$results$aboutAnalysis$setVisible(TRUE)
        },

        .generateAssumptions = function() {
            assumptions_html <- paste0(
                "<div style='font-family: Arial, sans-serif; max-width: 700px; line-height: 1.5;'>",
                "<div style='background: #fff3e0; border: 2px solid #f57c00; padding: 20px;'>",
                "<h3 style='margin: 0 0 15px 0; color: #f57c00; font-size: 18px;'>Assumptions & Caveats</h3>",
                
                "<h4 style='margin: 15px 0 8px 0; color: #d84315; font-size: 15px;'>⚠️ Important Assumptions</h4>",
                "<ul style='margin: 0 0 15px 0; padding-left: 20px; font-size: 14px;'>",
                "<li><strong>Independent ratings:</strong> Each rater evaluates cases without knowledge of other ratings</li>",
                "<li><strong>Same rating scale:</strong> All raters use identical categories in the same order</li>",
                "<li><strong>Representative sample:</strong> Cases should represent the typical population</li>",
                "<li><strong>Stable conditions:</strong> Rating criteria remain consistent throughout the study</li>",
                "</ul>",
                
                "<h4 style='margin: 15px 0 8px 0; color: #d84315; font-size: 15px;'>📊 Data Requirements</h4>",
                "<ul style='margin: 0 0 15px 0; padding-left: 20px; font-size: 14px;'>",
                "<li><strong>Minimum sample size:</strong> 30+ cases for reliable estimates</li>",
                "<li><strong>Category distribution:</strong> Avoid rare categories (<5% of cases)</li>",
                "<li><strong>Complete data:</strong> Missing ratings reduce statistical power</li>",
                "<li><strong>Factor variables:</strong> Data must be properly coded as factors in your dataset</li>",
                "</ul>",
                
                "<h4 style='margin: 15px 0 8px 0; color: #d84315; font-size: 15px;'>🔍 Common Pitfalls</h4>",
                "<ul style='margin: 0 0 15px 0; padding-left: 20px; font-size: 14px;'>",
                "<li><strong>Prevalence effects:</strong> Very rare or very common conditions can inflate/deflate kappa</li>",
                "<li><strong>Bias effects:</strong> Systematic differences between raters reduce apparent agreement</li>",
                "<li><strong>Training effects:</strong> Agreement may improve over time, affecting comparisons</li>",
                "<li><strong>Case difficulty:</strong> Easy/difficult cases may show different agreement patterns</li>",
                "</ul>",
                
                "<h4 style='margin: 15px 0 8px 0; color: #d84315; font-size: 15px;'>💡 Interpretation Guidelines</h4>",
                "<div style='background: #f8f8f8; padding: 12px; border-radius: 4px; font-size: 14px;'>",
                "<p style='margin: 0 0 8px 0;'><strong>Kappa Interpretation (Landis & Koch, 1977):</strong></p>",
                "<ul style='margin: 0 0 8px 0; padding-left: 20px;'>",
                "<li>κ < 0.00: Poor agreement (worse than chance)</li>",
                "<li>κ 0.00-0.20: Slight agreement</li>",
                "<li>κ 0.21-0.40: Fair agreement</li>",
                "<li>κ 0.41-0.60: Moderate agreement</li>",
                "<li>κ 0.61-0.80: Substantial agreement</li>",
                "<li>κ 0.81-1.00: Almost perfect agreement</li>",
                "</ul>",
                "<p style='margin: 0; color: #666; font-style: italic;'>",
                "Note: Consider clinical consequences when interpreting - higher agreement may be needed for critical diagnoses.</p>",
                "</div></div></div>"
            )

            self$results$assumptions$setContent(assumptions_html)
            self$results$assumptions$setVisible(TRUE)
        },

        # Generate weighted kappa guide
        .generateWeightedKappaGuide = function() {
            current_weighting <- self$options$wght
            
            guide_html <- paste0(
                "<div style='font-family: Arial, sans-serif; max-width: 700px; line-height: 1.5;'>",
                "<div style='background: #e3f2fd; border-left: 4px solid #1976d2; padding: 15px; margin-bottom: 15px;'>",
                "<h3 style='margin: 0 0 10px 0; color: #1976d2; font-size: 18px;'>📊 Weighted Kappa Guide</h3>",
                "<p style='margin: 0; font-size: 14px; color: #424242;'>",
                "You have selected <strong>", switch(current_weighting,
                    "equal" = "Linear/Equal weighting",
                    "squared" = "Quadratic/Squared weighting",
                    "Weighted kappa"), "</strong>. Here's when and why to use different weighting schemes:",
                "</p></div>",
                
                "<div style='background: #f9f9f9; padding: 15px; border-radius: 6px; margin-bottom: 15px;'>",
                "<h4 style='margin: 0 0 12px 0; color: #2e7d32; font-size: 16px;'>⚖️ Weighting Schemes Explained</h4>",
                
                "<div style='margin-bottom: 15px;'>",
                "<h5 style='margin: 0 0 8px 0; color: #1976d2; font-size: 14px;'>🔹 Linear/Equal Weighting (", 
                if(current_weighting == "equal") "<span style='background: #c8e6c9; padding: 2px 6px; border-radius: 3px;'>SELECTED</span>" else "Not selected", ")</h5>",
                "<div style='font-size: 13px; margin-left: 15px;'>",
                "<p style='margin: 0 0 6px 0;'><strong>Formula:</strong> w = 1 - |i - j| / (k - 1)</p>",
                "<p style='margin: 0 0 6px 0;'><strong>When to use:</strong></p>",
                "<ul style='margin: 0 0 8px 0; padding-left: 20px;'>",
                "<li>Ordinal data where categories are equally spaced</li>",
                "<li>Each step of disagreement has equal clinical importance</li>",
                "<li>Example: Pain scales (1-10), tumor grades (I, II, III, IV)</li>",
                "</ul>",
                "<p style='margin: 0; color: #666;'><em>All adjacent disagreements are penalized equally</em></p>",
                "</div></div>",
                
                "<div style='margin-bottom: 15px;'>",
                "<h5 style='margin: 0 0 8px 0; color: #1976d2; font-size: 14px;'>🔹 Quadratic/Squared Weighting (",
                if(current_weighting == "squared") "<span style='background: #c8e6c9; padding: 2px 6px; border-radius: 3px;'>SELECTED</span>" else "Not selected", ")</h5>",
                "<div style='font-size: 13px; margin-left: 15px;'>",
                "<p style='margin: 0 0 6px 0;'><strong>Formula:</strong> w = 1 - [(i - j) / (k - 1)]²</p>",
                "<p style='margin: 0 0 6px 0;'><strong>When to use:</strong></p>",
                "<ul style='margin: 0 0 8px 0; padding-left: 20px;'>",
                "<li>Large disagreements are disproportionately more serious</li>",
                "<li>Clinical consequences increase exponentially with distance</li>",
                "<li>Example: Disease severity (mild → moderate has different impact than moderate → severe)</li>",
                "</ul>",
                "<p style='margin: 0; color: #666;'><em>Heavily penalizes distant disagreements, lenient on close ones</em></p>",
                "</div></div>",
                
                "<div>",
                "<h5 style='margin: 0 0 8px 0; color: #1976d2; font-size: 14px;'>🔹 Unweighted Kappa (Standard)</h5>",
                "<div style='font-size: 13px; margin-left: 15px;'>",
                "<p style='margin: 0 0 6px 0;'><strong>When to use:</strong></p>",
                "<ul style='margin: 0 0 8px 0; padding-left: 20px;'>",
                "<li>Nominal (categorical) data with no natural ordering</li>",
                "<li>All disagreements are equally problematic</li>",
                "<li>Example: Diagnostic categories, present/absent classifications</li>",
                "</ul>",
                "<p style='margin: 0; color: #666;'><em>All disagreements treated equally, regardless of distance</em></p>",
                "</div></div>",
                "</div>",
                
                "<div style='background: #fff3e0; border-left: 4px solid #f57c00; padding: 15px; margin-bottom: 15px;'>",
                "<h4 style='margin: 0 0 10px 0; color: #e65100; font-size: 15px;'>🎯 Clinical Decision Guide</h4>",
                "<div style='font-size: 14px;'>",
                "<p style='margin: 0 0 8px 0;'><strong>Choose Linear weighting when:</strong></p>",
                "<ul style='margin: 0 0 12px 0; padding-left: 20px;'>",
                "<li>Rating scales with equal intervals (e.g., 1-5 Likert scales)</li>",
                "<li>Histological grades with uniform clinical significance</li>",
                "<li>Performance status scales (ECOG, Karnofsky)</li>",
                "</ul>",
                
                "<p style='margin: 0 0 8px 0;'><strong>Choose Quadratic weighting when:</strong></p>",
                "<ul style='margin: 0 0 12px 0; padding-left: 20px;'>",
                "<li>Diagnosis severity where distant errors are critical</li>",
                "<li>Treatment response categories (complete → partial → progressive)</li>",
                "<li>Risk stratification (low → intermediate → high)</li>",
                "</ul>",
                
                "<p style='margin: 0; font-weight: 500; color: #d84315;'>",
                "💡 <em>When in doubt, quadratic weighting is often preferred in medical research as it better reflects clinical reality.</em></p>",
                "</div></div>",
                
                "<div style='background: #f3e5f5; border-left: 4px solid #8e24aa; padding: 15px;'>",
                "<h4 style='margin: 0 0 10px 0; color: #6a1b9a; font-size: 15px;'>📊 Interpretation Impact</h4>",
                "<div style='font-size: 14px;'>",
                "<p style='margin: 0 0 8px 0;'>Weighted kappa values are typically:</p>",
                "<ul style='margin: 0 0 8px 0; padding-left: 20px;'>",
                "<li><strong>Higher</strong> than unweighted kappa (gives credit for near-misses)</li>",
                "<li><strong>Quadratic > Linear</strong> weighting (quadratic is more forgiving)</li>",
                "<li><strong>More clinically meaningful</strong> for ordinal data</li>",
                "</ul>",
                "<p style='margin: 0; color: #6a1b9a; font-weight: 500;'>",
                "Always report which weighting scheme was used in your methodology!</p>",
                "</div></div></div>"
            )

            self$results$weightedKappaGuide$setContent(guide_html)
            self$results$weightedKappaGuide$setVisible(TRUE)
        },

        .generateStatisticalGlossary = function() {
            glossary_html <- paste0(
                "<div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.5;'>",
                "<div style='background: #f5f5f5; border: 2px solid #2e7d32; padding: 20px; margin-bottom: 20px;'>",
                "<h3 style='margin: 0 0 15px 0; color: #2e7d32; font-size: 18px;'>📚 Statistical Terms Glossary</h3>",
                "<p style='margin: 0; font-size: 14px; color: #666;'>Quick reference for inter-rater reliability statistics and their clinical meanings</p>",
                "</div>",
                
                # Cohen's Kappa
                "<div style='border: 1px solid #e0e0e0; padding: 15px; margin-bottom: 15px; background: #fafafa;'>",
                "<h4 style='margin: 0 0 10px 0; color: #1976d2; font-size: 16px;'>Cohen's Kappa (κ)</h4>",
                "<p style='margin: 0 0 8px 0; font-size: 14px;'><strong>What it measures:</strong> Agreement between exactly 2 raters, corrected for chance agreement</p>",
                "<p style='margin: 0 0 8px 0; font-size: 14px;'><strong>Range:</strong> -1 to +1 (0 = chance agreement, 1 = perfect agreement)</p>",
                "<p style='margin: 0 0 8px 0; font-size: 14px;'><strong>Clinical use:</strong> Most common for comparing two pathologists, radiologists, or clinicians</p>",
                "<p style='margin: 0; font-size: 14px; color: #666;'><em>Formula: (observed - expected) / (1 - expected)</em></p>",
                "</div>",
                
                # Fleiss' Kappa
                "<div style='border: 1px solid #e0e0e0; padding: 15px; margin-bottom: 15px; background: #fafafa;'>",
                "<h4 style='margin: 0 0 10px 0; color: #1976d2; font-size: 16px;'>Fleiss' Kappa</h4>",
                "<p style='margin: 0 0 8px 0; font-size: 14px;'><strong>What it measures:</strong> Agreement among 3+ raters, extension of Cohen's kappa</p>",
                "<p style='margin: 0 0 8px 0; font-size: 14px;'><strong>Range:</strong> -1 to +1 (same interpretation as Cohen's kappa)</p>",
                "<p style='margin: 0 0 8px 0; font-size: 14px;'><strong>Clinical use:</strong> Multi-center studies, consensus panels, training assessment</p>",
                "<p style='margin: 0; font-size: 14px; color: #666;'><em>Accounts for varying numbers of raters per case</em></p>",
                "</div>",
                
                # Krippendorff's Alpha
                "<div style='border: 1px solid #e0e0e0; padding: 15px; margin-bottom: 15px; background: #fafafa;'>",
                "<h4 style='margin: 0 0 10px 0; color: #1976d2; font-size: 16px;'>Krippendorff's Alpha (α)</h4>",
                "<p style='margin: 0 0 8px 0; font-size: 14px;'><strong>What it measures:</strong> Universal reliability measure for any number of raters and data types</p>",
                "<p style='margin: 0 0 8px 0; font-size: 14px;'><strong>Range:</strong> 0 to 1 (0 = no agreement, 1 = perfect agreement)</p>",
                "<p style='margin: 0 0 8px 0; font-size: 14px;'><strong>Clinical use:</strong> Complex studies with missing data, different measurement scales</p>",
                "<p style='margin: 0; font-size: 14px; color: #666;'><em>Handles missing data and unequal sample sizes gracefully</em></p>",
                "</div>",
                
                # ICC
                "<div style='border: 1px solid #e0e0e0; padding: 15px; margin-bottom: 15px; background: #fafafa;'>",
                "<h4 style='margin: 0 0 10px 0; color: #1976d2; font-size: 16px;'>Intraclass Correlation Coefficient (ICC)</h4>",
                "<p style='margin: 0 0 8px 0; font-size: 14px;'><strong>What it measures:</strong> Proportion of total variance due to between-subject differences</p>",
                "<p style='margin: 0 0 8px 0; font-size: 14px;'><strong>Range:</strong> 0 to 1 (0 = no reliability, 1 = perfect reliability)</p>",
                "<p style='margin: 0 0 8px 0; font-size: 14px;'><strong>Clinical use:</strong> Continuous measurements, biomarker assessments, imaging scores</p>",
                "<p style='margin: 0; font-size: 14px; color: #666;'><em>Best for quantitative data rather than categories</em></p>",
                "</div>",
                
                # PABAK
                "<div style='border: 1px solid #e0e0e0; padding: 15px; margin-bottom: 15px; background: #fafafa;'>",
                "<h4 style='margin: 0 0 10px 0; color: #1976d2; font-size: 16px;'>PABAK (Prevalence-Adjusted Bias-Adjusted Kappa)</h4>",
                "<p style='margin: 0 0 8px 0; font-size: 14px;'><strong>What it measures:</strong> Kappa corrected for uneven category distributions</p>",
                "<p style='margin: 0 0 8px 0; font-size: 14px;'><strong>Range:</strong> -1 to +1 (higher than regular kappa when categories are imbalanced)</p>",
                "<p style='margin: 0 0 8px 0; font-size: 14px;'><strong>Clinical use:</strong> Rare diseases, screening tests, imbalanced datasets</p>",
                "<p style='margin: 0; font-size: 14px; color: #666;'><em>Addresses the 'prevalence paradox' in agreement studies</em></p>",
                "</div>",
                
                # Gwet's AC
                "<div style='border: 1px solid #e0e0e0; padding: 15px; margin-bottom: 15px; background: #fafafa;'>",
                "<h4 style='margin: 0 0 10px 0; color: #1976d2; font-size: 16px;'>Gwet's Agreement Coefficients (AC1/AC2)</h4>",
                "<p style='margin: 0 0 8px 0; font-size: 14px;'><strong>What it measures:</strong> Agreement less affected by prevalence than kappa</p>",
                "<p style='margin: 0 0 8px 0; font-size: 14px;'><strong>Range:</strong> -1 to +1 (often higher than kappa in high-agreement scenarios)</p>",
                "<p style='margin: 0 0 8px 0; font-size: 14px;'><strong>Clinical use:</strong> High agreement situations, quality assurance studies</p>",
                "<p style='margin: 0; font-size: 14px; color: #666;'><em>More stable than kappa when baseline agreement is high</em></p>",
                "</div>",
                
                # Interpretation Guide
                "<div style='background: #e8f5e8; border: 1px solid #4caf50; padding: 15px; margin-bottom: 15px;'>",
                "<h4 style='margin: 0 0 10px 0; color: #2e7d32; font-size: 16px;'>📊 Interpretation Guidelines</h4>",
                "<div style='font-size: 14px;'>",
                "<table style='width: 100%; border-collapse: collapse; margin: 10px 0;'>",
                "<tr style='background: #f5f5f5;'><th style='padding: 8px; border: 1px solid #ddd; text-align: left;'>Kappa/Alpha Value</th><th style='padding: 8px; border: 1px solid #ddd; text-align: left;'>Interpretation</th><th style='padding: 8px; border: 1px solid #ddd; text-align: left;'>Clinical Action</th></tr>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'>&lt; 0.00</td><td style='padding: 8px; border: 1px solid #ddd;'>Poor (worse than chance)</td><td style='padding: 8px; border: 1px solid #ddd;'>Review criteria, retrain</td></tr>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'>0.00 - 0.20</td><td style='padding: 8px; border: 1px solid #ddd;'>Slight agreement</td><td style='padding: 8px; border: 1px solid #ddd;'>Substantial improvement needed</td></tr>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'>0.21 - 0.40</td><td style='padding: 8px; border: 1px solid #ddd;'>Fair agreement</td><td style='padding: 8px; border: 1px solid #ddd;'>Additional training recommended</td></tr>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'>0.41 - 0.60</td><td style='padding: 8px; border: 1px solid #ddd;'>Moderate agreement</td><td style='padding: 8px; border: 1px solid #ddd;'>Acceptable for many uses</td></tr>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'>0.61 - 0.80</td><td style='padding: 8px; border: 1px solid #ddd;'>Substantial agreement</td><td style='padding: 8px; border: 1px solid #ddd;'>Good for clinical use</td></tr>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'>0.81 - 1.00</td><td style='padding: 8px; border: 1px solid #ddd;'>Almost perfect agreement</td><td style='padding: 8px; border: 1px solid #ddd;'>Excellent reliability</td></tr>",
                "</table>",
                "</div></div>",
                
                # Tips
                "<div style='background: #fff3e0; border: 1px solid #f57c00; padding: 15px;'>",
                "<h4 style='margin: 0 0 10px 0; color: #e65100; font-size: 15px;'>💡 Practical Tips</h4>",
                "<ul style='margin: 0; padding-left: 20px; font-size: 14px;'>",
                "<li><strong>Sample size:</strong> Minimum 30 cases for reliable estimates, 50+ preferred</li>",
                "<li><strong>Category balance:</strong> Avoid categories with &lt;5% frequency when possible</li>",
                "<li><strong>Missing data:</strong> Krippendorff's alpha handles missing data best</li>",
                "<li><strong>Continuous data:</strong> Use ICC instead of kappa for measurements</li>",
                "<li><strong>Reporting:</strong> Always include confidence intervals when available</li>",
                "<li><strong>Clinical context:</strong> Consider consequences of disagreement in interpretation</li>",
                "</ul></div></div>"
            )
            
            self$results$statisticalGlossary$setContent(glossary_html)
            self$results$statisticalGlossary$setVisible(TRUE)
        },

        .calculateAgreementMatrixOptimized = function(data_matrix, rater_names) {
            # Optimized agreement matrix calculation for large datasets
            n_raters <- ncol(data_matrix)
            n_cases <- nrow(data_matrix)
            
            # Initialize matrix
            agreement_matrix <- matrix(1.0, nrow = n_raters, ncol = n_raters,
                                     dimnames = list(rater_names, rater_names))
            
            # Pre-allocate logical matrices for comparison
            # This approach reduces memory allocation overhead
            for (i in 1:(n_raters-1)) {
                # Vectorized comparison for current rater against all subsequent raters
                current_ratings <- data_matrix[, i]
                
                for (j in (i+1):n_raters) {
                    # Direct vectorized calculation
                    agreements <- current_ratings == data_matrix[, j]
                    agreement_pct <- mean(agreements, na.rm = TRUE)
                    
                    # Assign to symmetric positions
                    agreement_matrix[i, j] <- agreement_pct
                    agreement_matrix[j, i] <- agreement_pct
                }
            }
            
            return(agreement_matrix)
        },

        # Enhanced analysis methods - Gwet's Agreement Coefficients
        .performGwetACAnalysis = function() {
            if (self$options$showProgressIndicators) {
                message("Calculating Gwet's Agreement Coefficients...")
            }
            
            gwet_table <- self$results$gwetACTable
            
            tryCatch({
                # Calculate AC1 (first-order agreement coefficient) 
                # AC1 is less sensitive to trait prevalence than kappa
                ac1_result <- private$.calculateGwetAC1()
                
                # Calculate AC2 (second-order agreement coefficient) 
                # AC2 accounts for rater heterogeneity  
                ac2_result <- private$.calculateGwetAC2()
                
                # Add AC1 result
                gwet_table$addRow(rowKey = "AC1", values = list(
                    coefficient = "Gwet's AC1",
                    value = ac1_result$estimate,
                    se = ac1_result$se,
                    ci_lower = ac1_result$ci_lower,
                    ci_upper = ac1_result$ci_upper,
                    interpretation = private$.interpretGwetAC(ac1_result$estimate, "AC1")
                ))
                
                # Add AC2 result  
                gwet_table$addRow(rowKey = "AC2", values = list(
                    coefficient = "Gwet's AC2", 
                    value = ac2_result$estimate,
                    se = ac2_result$se,
                    ci_lower = ac2_result$ci_lower,
                    ci_upper = ac2_result$ci_upper,
                    interpretation = private$.interpretGwetAC(ac2_result$estimate, "AC2")
                ))
                
                gwet_table$setVisible(TRUE)
                
            }, error = function(e) {
                # Error handling with enhanced guidance if enabled
                error_msg <- if (self$options$enhancedErrorGuidance) {
                    paste0("Error calculating Gwet's coefficients: ", e$message, 
                          ". Ensure data has adequate variability and no missing values.")
                } else {
                    paste(.("Error calculating Gwet's coefficients:"), e$message)
                }
                
                gwet_table$addRow(rowKey = "error", values = list(
                    coefficient = "Error",
                    value = NaN,
                    se = NaN,
                    ci_lower = NaN,
                    ci_upper = NaN,
                    interpretation = error_msg
                ))
            })
        },
        
        # Calculate Gwet's AC1 coefficient
        .calculateGwetAC1 = function() {
            # AC1 coefficient implementation
            # This addresses the prevalence problem in kappa statistics
            data_matrix <- private$.data_matrix
            n_cases <- nrow(data_matrix)
            n_raters <- ncol(data_matrix)
            
            # Calculate observed agreement
            observed_agreement <- private$.calculateOverallAgreement()
            
            # Calculate expected agreement under AC1 assumptions
            # AC1 assumes uniform distribution across categories  
            all_categories <- unique(as.vector(data_matrix))
            all_categories <- all_categories[!is.na(all_categories)]
            k_categories <- length(all_categories)
            
            # Expected agreement = 1/k for uniform distribution
            expected_agreement <- 1 / k_categories
            
            # AC1 coefficient
            ac1 <- (observed_agreement - expected_agreement) / (1 - expected_agreement)
            
            # Standard error calculation (simplified)  
            se_ac1 <- sqrt((observed_agreement * (1 - observed_agreement)) / (n_cases * (1 - expected_agreement)^2))
            
            # Confidence intervals
            ci_lower <- max(-1, ac1 - 1.96 * se_ac1)
            ci_upper <- min(1, ac1 + 1.96 * se_ac1)
            
            return(list(
                estimate = ac1,
                se = se_ac1,
                ci_lower = ci_lower,
                ci_upper = ci_upper
            ))
        },
        
        # Calculate Gwet's AC2 coefficient  
        .calculateGwetAC2 = function() {
            # AC2 coefficient - accounts for rater characteristics
            data_matrix <- private$.data_matrix
            n_cases <- nrow(data_matrix)
            n_raters <- ncol(data_matrix)
            
            # Calculate observed agreement
            observed_agreement <- private$.calculateOverallAgreement()
            
            # Calculate marginal probabilities for each category
            all_ratings <- as.vector(data_matrix)
            all_ratings <- all_ratings[!is.na(all_ratings)]
            category_probs <- table(all_ratings) / length(all_ratings)
            
            # Expected agreement under AC2 (quadratic weighting of marginals)
            expected_agreement <- sum(category_probs^2)
            
            # AC2 coefficient
            ac2 <- (observed_agreement - expected_agreement) / (1 - expected_agreement)
            
            # Standard error (approximation)
            se_ac2 <- sqrt((observed_agreement * (1 - observed_agreement)) / (n_cases * (1 - expected_agreement)^2))
            
            # Confidence intervals
            ci_lower <- max(-1, ac2 - 1.96 * se_ac2)  
            ci_upper <- min(1, ac2 + 1.96 * se_ac2)
            
            return(list(
                estimate = ac2,
                se = se_ac2,
                ci_lower = ci_lower,
                ci_upper = ci_upper
            ))
        },
        
        # Interpret Gwet's Agreement Coefficients
        .interpretGwetAC = function(coefficient_value, type = "AC1") {
            if (is.na(coefficient_value) || !is.numeric(coefficient_value)) {
                return("Cannot interpret invalid coefficient value")
            }
            
            # Gwet's interpretation guidelines (similar to kappa but more robust)
            interpretation <- if (coefficient_value < 0) {
                "Poor (worse than chance)"
            } else if (coefficient_value < 0.20) {
                "Slight agreement"
            } else if (coefficient_value < 0.40) {
                "Fair agreement" 
            } else if (coefficient_value < 0.60) {
                "Moderate agreement"
            } else if (coefficient_value < 0.80) {
                "Substantial agreement"
            } else {
                "Almost perfect agreement"
            }
            
            # Add coefficient-specific notes
            note <- if (type == "AC1") {
                " (robust to prevalence effects)"
            } else {
                " (accounts for rater heterogeneity)"
            }
            
            return(paste0(interpretation, note))
        },

        # PABAK Analysis (Prevalence-Adjusted Bias-Adjusted Kappa)
        .performPABAKAnalysis = function() {
            if (self$options$showProgressIndicators) {
                message("Calculating PABAK coefficients...")
            }
            
            pabak_table <- self$results$pabakTable
            
            tryCatch({
                # Calculate standard kappa first
                standard_kappa <- private$.calculateStandardKappa()
                
                # Calculate PABAK 
                pabak_result <- private$.calculatePABAK()
                
                # Add results to table
                pabak_table$addRow(rowKey = "overall", values = list(
                    measure = "Overall Agreement",
                    value = pabak_result$observed_agreement,
                    standard_kappa = standard_kappa$kappa,
                    adjusted_kappa = pabak_result$pabak,
                    interpretation = private$.interpretPABAK(pabak_result$pabak, standard_kappa$kappa)
                ))
                
                pabak_table$setVisible(TRUE)
                
            }, error = function(e) {
                error_msg <- if (self$options$enhancedErrorGuidance) {
                    paste0("Error calculating PABAK: ", e$message,
                          ". PABAK requires balanced categories and complete data.")
                } else {
                    paste(.("Error calculating PABAK:"), e$message)
                }
                
                pabak_table$addRow(rowKey = "error", values = list(
                    measure = "Error",
                    value = NaN,
                    standard_kappa = NaN,
                    adjusted_kappa = NaN,
                    interpretation = error_msg
                ))
            })
        },
        
        # Calculate PABAK coefficient
        .calculatePABAK = function() {
            data_matrix <- private$.data_matrix
            n_cases <- nrow(data_matrix)
            
            # For PABAK, we need pairwise agreements
            # This is a simplified implementation for the first pair of raters
            if (ncol(data_matrix) < 2) {
                stop("PABAK requires at least 2 raters")
            }
            
            rater1 <- data_matrix[, 1]
            rater2 <- data_matrix[, 2]
            
            # Remove missing values
            valid_indices <- !is.na(rater1) & !is.na(rater2)
            rater1 <- rater1[valid_indices]
            rater2 <- rater2[valid_indices]
            n_valid <- length(rater1)
            
            # Calculate observed agreement
            observed_agreement <- sum(rater1 == rater2) / n_valid
            
            # PABAK = 2 * observed_agreement - 1
            # This adjusts for prevalence by assuming equal marginal distributions
            pabak <- 2 * observed_agreement - 1
            
            return(list(
                observed_agreement = observed_agreement,
                pabak = pabak
            ))
        },
        
        # Calculate standard kappa for comparison
        .calculateStandardKappa = function() {
            data_matrix <- private$.data_matrix
            
            if (ncol(data_matrix) >= 2) {
                rater1 <- data_matrix[, 1]
                rater2 <- data_matrix[, 2]
                
                # Use irr package if available
                if (requireNamespace("irr", quietly = TRUE)) {
                    kappa_result <- tryCatch({
                        irr::kappa2(cbind(rater1, rater2))
                    }, error = function(e) {
                        list(value = NaN)
                    })
                    return(list(kappa = kappa_result$value))
                }
            }
            
            return(list(kappa = NaN))
        },
        
        # Interpret PABAK vs standard kappa
        .interpretPABAK = function(pabak_value, standard_kappa) {
            if (is.na(pabak_value)) {
                return("Cannot calculate PABAK")
            }
            
            base_interpretation <- if (pabak_value < 0) {
                "Poor agreement"
            } else if (pabak_value < 0.20) {
                "Slight agreement"
            } else if (pabak_value < 0.40) {
                "Fair agreement"
            } else if (pabak_value < 0.60) {
                "Moderate agreement"
            } else if (pabak_value < 0.80) {
                "Substantial agreement"
            } else {
                "Almost perfect agreement"
            }
            
            # Compare with standard kappa if available
            if (!is.na(standard_kappa)) {
                difference <- pabak_value - standard_kappa
                comparison <- if (abs(difference) < 0.05) {
                    " (similar to standard kappa)"
                } else if (difference > 0.05) {
                    " (higher than standard kappa - prevalence bias present)"
                } else {
                    " (lower than standard kappa - unusual pattern)"
                }
                return(paste0(base_interpretation, comparison))
            }
            
            return(base_interpretation)
        },

        # Sample Size Planning for Agreement Studies
        .performSampleSizePlanning = function() {
            if (self$options$showProgressIndicators) {
                message("Calculating sample size requirements...")
            }
            
            sample_size_table <- self$results$sampleSizeTable
            target_kappa <- self$options$targetKappa
            target_precision <- self$options$targetPrecision
            
            tryCatch({
                # Calculate required sample size for different scenarios
                sample_sizes <- private$.calculateSampleSizeRequirements(target_kappa, target_precision)
                
                # Add results to table
                for (scenario in names(sample_sizes)) {
                    sample_size_table$addRow(rowKey = scenario, values = list(
                        parameter = scenario,
                        value = sample_sizes[[scenario]]$n_required,
                        recommendation = sample_sizes[[scenario]]$recommendation
                    ))
                }
                
                sample_size_table$setVisible(TRUE)
                
            }, error = function(e) {
                error_msg <- if (self$options$enhancedErrorGuidance) {
                    paste0("Error in sample size planning: ", e$message,
                          ". Check target parameters are within valid ranges.")
                } else {
                    paste(.("Error in sample size planning:"), e$message)
                }
                
                sample_size_table$addRow(rowKey = "error", values = list(
                    parameter = "Error",
                    value = "N/A",
                    recommendation = error_msg
                ))
            })
        },
        
        # Calculate sample size requirements
        .calculateSampleSizeRequirements = function(target_kappa, target_precision) {
            # Sample size calculation for kappa studies
            # Based on formula: n = (Z_alpha/2 + Z_beta)^2 * SE^2 / delta^2
            
            z_alpha <- 1.96  # 95% confidence level
            power <- 0.80    # 80% power
            z_beta <- qnorm(power)
            
            results <- list()
            
            # For 2 raters
            se_2raters <- sqrt((target_kappa * (1 - target_kappa)) / target_precision^2)
            n_2raters <- ceiling((z_alpha + z_beta)^2 * se_2raters^2)
            
            results[["2 Raters"]] <- list(
                n_required = paste0(n_2raters, " cases"),
                recommendation = paste0("For κ = ", target_kappa, " with ±", target_precision, " precision")
            )
            
            # For 3 raters (Fleiss' kappa)
            se_3raters <- sqrt((target_kappa * (1 - target_kappa)) / (3 * target_precision^2))
            n_3raters <- ceiling((z_alpha + z_beta)^2 * se_3raters^2)
            
            results[["3 Raters"]] <- list(
                n_required = paste0(n_3raters, " cases"),
                recommendation = paste0("For Fleiss' κ = ", target_kappa, " with ±", target_precision, " precision")
            )
            
            # Conservative estimate for multiple comparisons
            n_conservative <- ceiling(n_2raters * 1.2)
            results[["Conservative Estimate"]] <- list(
                n_required = paste0(n_conservative, " cases"),
                recommendation = "Accounts for multiple testing and dropouts"
            )
            
            return(results)
        },

        # Rater Bias Detection Analysis  
        .performRaterBiasAnalysis = function() {
            if (self$options$showProgressIndicators) {
                message("Analyzing rater bias patterns...")
            }
            
            bias_table <- self$results$raterBiasTable
            
            tryCatch({
                bias_results <- private$.detectRaterBias()
                
                for (rater_name in names(bias_results)) {
                    bias_info <- bias_results[[rater_name]]
                    
                    bias_table$addRow(rowKey = rater_name, values = list(
                        rater = rater_name,
                        bias_score = bias_info$bias_score,
                        tendency = bias_info$tendency,
                        severity = bias_info$severity,
                        recommendation = bias_info$recommendation
                    ))
                }
                
                bias_table$setVisible(TRUE)
                
            }, error = function(e) {
                error_msg <- if (self$options$enhancedErrorGuidance) {
                    paste0("Error in bias analysis: ", e$message,
                          ". Requires sufficient data variability across raters.")
                } else {
                    paste(.("Error in bias analysis:"), e$message)
                }
                
                bias_table$addRow(rowKey = "error", values = list(
                    rater = "Error",
                    bias_score = NaN,
                    tendency = "N/A",
                    severity = "N/A", 
                    recommendation = error_msg
                ))
            })
        },
        
        # Detect systematic biases in rater behavior
        .detectRaterBias = function() {
            data_matrix <- private$.data_matrix
            n_cases <- nrow(data_matrix)
            n_raters <- ncol(data_matrix)
            rater_names <- private$.rater_names
            
            results <- list()
            
            # Calculate overall mode/consensus for each case
            case_consensus <- apply(data_matrix, 1, function(row) {
                valid_ratings <- row[!is.na(row)]
                if (length(valid_ratings) > 0) {
                    # Return the most frequent rating (mode)
                    tbl <- table(valid_ratings)
                    names(tbl)[which.max(tbl)]
                } else {
                    NA
                }
            })
            
            # Analyze each rater's tendency
            for (i in seq_len(n_raters)) {
                rater_ratings <- data_matrix[, i]
                rater_name <- rater_names[i]
                
                # Calculate bias score (deviation from consensus)
                valid_cases <- !is.na(rater_ratings) & !is.na(case_consensus)
                
                if (sum(valid_cases) > 0) {
                    agreements <- rater_ratings[valid_cases] == case_consensus[valid_cases]
                    bias_score <- 1 - mean(agreements)  # Higher = more biased
                    
                    # Determine systematic tendency
                    rater_dist <- table(rater_ratings[valid_cases])
                    consensus_dist <- table(case_consensus[valid_cases])
                    
                    # Compare distributions to identify tendency
                    tendency <- private$.identifyRaterTendency(rater_dist, consensus_dist)
                    
                    # Classify severity
                    severity <- if (bias_score < 0.1) {
                        "Minimal bias"
                    } else if (bias_score < 0.2) {
                        "Mild bias"  
                    } else if (bias_score < 0.3) {
                        "Moderate bias"
                    } else {
                        "Severe bias"
                    }
                    
                    # Generate recommendation
                    recommendation <- private$.generateBiasRecommendation(bias_score, tendency)
                    
                    results[[rater_name]] <- list(
                        bias_score = bias_score,
                        tendency = tendency,
                        severity = severity,
                        recommendation = recommendation
                    )
                }
            }
            
            return(results)
        },
        
        # Identify rater tendency patterns
        .identifyRaterTendency = function(rater_dist, consensus_dist) {
            # Compare category preferences
            rater_props <- rater_dist / sum(rater_dist)
            consensus_props <- consensus_dist / sum(consensus_dist)
            
            # Find categories where rater differs most from consensus
            differences <- rater_props - consensus_props[names(rater_props)]
            differences[is.na(differences)] <- 0
            
            max_diff_category <- names(which.max(abs(differences)))
            max_diff_value <- differences[max_diff_category]
            
            if (abs(max_diff_value) < 0.1) {
                return("No clear systematic tendency")
            } else if (max_diff_value > 0) {
                return(paste0("Over-diagnoses '", max_diff_category, "'"))
            } else {
                return(paste0("Under-diagnoses '", max_diff_category, "'"))
            }
        },
        
        # Generate bias-specific recommendations
        .generateBiasRecommendation = function(bias_score, tendency) {
            if (bias_score < 0.1) {
                return("Excellent consistency with consensus")
            } else if (bias_score < 0.2) {
                return(paste0("Minor calibration needed. ", tendency))
            } else if (bias_score < 0.3) {
                return(paste0("Consider targeted training. ", tendency))
            } else {
                return(paste0("Requires significant recalibration. ", tendency))
            }
        },

        # Agreement Trend Analysis
        .performAgreementTrendAnalysis = function() {
            if (self$options$showProgressIndicators) {
                message("Analyzing agreement trends...")
            }
            
            trend_table <- self$results$agreementTrendTable
            
            tryCatch({
                trend_results <- private$.analyzeAgreementTrends()
                
                for (group_name in names(trend_results)) {
                    group_info <- trend_results[[group_name]]
                    
                    trend_table$addRow(rowKey = group_name, values = list(
                        sequence_group = group_name,
                        agreement_percent = group_info$agreement_percent,
                        kappa = group_info$kappa,
                        trend_direction = group_info$trend_direction,
                        significance = group_info$significance
                    ))
                }
                
                trend_table$setVisible(TRUE)
                
            }, error = function(e) {
                error_msg <- if (self$options$enhancedErrorGuidance) {
                    paste0("Error in trend analysis: ", e$message,
                          ". Requires sequential case ordering or temporal data.")
                } else {
                    paste(.("Error in trend analysis:"), e$message)
                }
                
                trend_table$addRow(rowKey = "error", values = list(
                    sequence_group = "Error",
                    agreement_percent = NaN,
                    kappa = NaN,
                    trend_direction = "N/A",
                    significance = error_msg
                ))
            })
        },
        
        # Analyze agreement trends over case sequences
        .analyzeAgreementTrends = function() {
            data_matrix <- private$.data_matrix
            n_cases <- nrow(data_matrix)
            
            # Divide cases into sequential groups for trend analysis
            group_size <- max(10, floor(n_cases / 5))  # At least 10 cases per group, max 5 groups
            n_groups <- ceiling(n_cases / group_size)
            
            results <- list()
            group_agreements <- numeric(n_groups)
            
            for (i in seq_len(n_groups)) {
                start_idx <- (i - 1) * group_size + 1
                end_idx <- min(i * group_size, n_cases)
                
                group_data <- data_matrix[start_idx:end_idx, , drop = FALSE]
                
                # Calculate agreement for this group
                group_agreement <- private$.calculateGroupAgreement(group_data)
                group_kappa <- private$.calculateGroupKappa(group_data)
                
                group_agreements[i] <- group_agreement$percent
                
                group_name <- paste0("Cases ", start_idx, "-", end_idx)
                
                results[[group_name]] <- list(
                    agreement_percent = group_agreement$percent,
                    kappa = group_kappa,
                    trend_direction = "Calculating...",
                    significance = "Calculating..."
                )
            }
            
            # Calculate overall trend
            if (length(group_agreements) >= 3) {
                trend_test <- private$.calculateTrendSignificance(group_agreements)
                
                # Update trend information for all groups
                for (group_name in names(results)) {
                    results[[group_name]]$trend_direction <- trend_test$direction
                    results[[group_name]]$significance <- trend_test$significance
                }
            }
            
            return(results)
        },
        
        # Calculate agreement for a subset of data
        .calculateGroupAgreement = function(group_data) {
            total_comparisons <- 0
            total_agreements <- 0
            
            n_cases <- nrow(group_data)
            n_raters <- ncol(group_data)
            
            for (case_idx in seq_len(n_cases)) {
                case_ratings <- group_data[case_idx, ]
                valid_ratings <- case_ratings[!is.na(case_ratings)]
                
                if (length(valid_ratings) >= 2) {
                    # Count pairwise comparisons
                    n_pairs <- choose(length(valid_ratings), 2)
                    total_comparisons <- total_comparisons + n_pairs
                    
                    # Count agreements
                    for (i in seq_len(length(valid_ratings) - 1)) {
                        for (j in (i + 1):length(valid_ratings)) {
                            if (valid_ratings[i] == valid_ratings[j]) {
                                total_agreements <- total_agreements + 1
                            }
                        }
                    }
                }
            }
            
            percent_agreement <- if (total_comparisons > 0) {
                (total_agreements / total_comparisons) * 100
            } else {
                NA
            }
            
            return(list(percent = percent_agreement))
        },
        
        # Calculate kappa for a group (simplified)
        .calculateGroupKappa = function(group_data) {
            # Simplified kappa calculation for trend analysis
            if (ncol(group_data) >= 2) {
                # Use first two raters for consistency
                rater1 <- group_data[, 1]
                rater2 <- group_data[, 2]
                
                valid_idx <- !is.na(rater1) & !is.na(rater2)
                if (sum(valid_idx) >= 5) {  # Minimum cases for meaningful kappa
                    if (requireNamespace("irr", quietly = TRUE)) {
                        kappa_result <- tryCatch({
                            irr::kappa2(cbind(rater1[valid_idx], rater2[valid_idx]))
                        }, error = function(e) list(value = NA))
                        return(kappa_result$value)
                    }
                }
            }
            return(NA)
        },
        
        # Test for significant trend in agreement over time
        .calculateTrendSignificance = function(group_agreements) {
            # Simple trend test using correlation with sequence
            sequence <- seq_along(group_agreements)
            
            if (length(group_agreements) >= 3 && !all(is.na(group_agreements))) {
                correlation <- cor(sequence, group_agreements, use = "complete.obs")
                
                # Simple trend assessment
                direction <- if (is.na(correlation)) {
                    "No clear trend"
                } else if (abs(correlation) < 0.3) {
                    "Stable agreement"
                } else if (correlation > 0.3) {
                    "Improving agreement"
                } else {
                    "Declining agreement"
                }
                
                # Significance assessment (simplified)
                significance <- if (is.na(correlation)) {
                    "Cannot assess"
                } else if (abs(correlation) > 0.6) {
                    "Strong trend"
                } else if (abs(correlation) > 0.3) {
                    "Moderate trend"
                } else {
                    "Weak trend"
                }
                
                return(list(direction = direction, significance = significance))
            }
            
            return(list(direction = "Insufficient data", significance = "Cannot assess"))
        },

        # Case Difficulty Analysis
        .performCaseDifficultyAnalysis = function() {
            if (self$options$showProgressIndicators) {
                message("Analyzing case difficulty patterns...")
            }
            
            difficulty_table <- self$results$caseDifficultyTable
            
            tryCatch({
                difficulty_results <- private$.analyzeCaseDifficulty()
                
                for (case_id in names(difficulty_results)) {
                    case_info <- difficulty_results[[case_id]]
                    
                    difficulty_table$addRow(rowKey = case_id, values = list(
                        case_id = case_id,
                        difficulty_score = case_info$difficulty_score,
                        disagreement_pattern = case_info$disagreement_pattern,
                        difficulty_level = case_info$difficulty_level,
                        rater_variability = case_info$rater_variability
                    ))
                }
                
                difficulty_table$setVisible(TRUE)
                
            }, error = function(e) {
                error_msg <- if (self$options$enhancedErrorGuidance) {
                    paste0("Error in difficulty analysis: ", e$message,
                          ". Requires multiple raters and diverse case ratings.")
                } else {
                    paste(.("Error in difficulty analysis:"), e$message)
                }
                
                difficulty_table$addRow(rowKey = "error", values = list(
                    case_id = "Error",
                    difficulty_score = NaN,
                    disagreement_pattern = "N/A",
                    difficulty_level = "N/A",
                    rater_variability = NaN,
                    interpretation = error_msg
                ))
            })
        },
        
        # Analyze inherent case difficulty based on rater disagreement
        .analyzeCaseDifficulty = function() {
            data_matrix <- private$.data_matrix
            n_cases <- nrow(data_matrix)
            n_raters <- ncol(data_matrix)
            
            results <- list()
            
            for (case_idx in seq_len(n_cases)) {
                case_ratings <- data_matrix[case_idx, ]
                valid_ratings <- case_ratings[!is.na(case_ratings)]
                
                if (length(valid_ratings) >= 2) {
                    case_id <- paste0("Case_", case_idx)
                    
                    # Calculate difficulty metrics
                    difficulty_metrics <- private$.calculateCaseDifficultyMetrics(valid_ratings)
                    
                    results[[case_id]] <- difficulty_metrics
                }
            }
            
            return(results)
        },
        
        # Calculate difficulty metrics for a single case
        .calculateCaseDifficultyMetrics = function(ratings) {
            # Difficulty score based on disagreement entropy
            rating_table <- table(ratings)
            rating_props <- rating_table / sum(rating_table)
            
            # Entropy-based difficulty (higher entropy = more difficult)
            entropy <- -sum(rating_props * log2(rating_props))
            max_entropy <- log2(length(rating_table))  # Maximum possible entropy
            difficulty_score <- if (max_entropy > 0) entropy / max_entropy else 0
            
            # Rater variability (coefficient of variation if ratings are numeric)
            if (all(is.numeric(ratings) | (is.character(ratings) && all(ratings %in% c("1", "2", "3", "4", "5"))))) {
                numeric_ratings <- as.numeric(ratings)
                rater_variability <- sd(numeric_ratings, na.rm = TRUE) / mean(numeric_ratings, na.rm = TRUE)
            } else {
                rater_variability <- difficulty_score  # Use entropy as proxy for categorical data
            }
            
            # Disagreement pattern description
            disagreement_pattern <- private$.describeDisagreementPattern(rating_table)
            
            # Difficulty level classification
            difficulty_level <- if (difficulty_score < 0.2) {
                "Easy (high consensus)"
            } else if (difficulty_score < 0.4) {
                "Moderate difficulty"
            } else if (difficulty_score < 0.7) {
                "Difficult (significant disagreement)"
            } else {
                "Very difficult (high disagreement)"
            }
            
            return(list(
                difficulty_score = round(difficulty_score, 3),
                disagreement_pattern = disagreement_pattern,
                difficulty_level = difficulty_level,
                rater_variability = round(rater_variability, 3)
            ))
        },
        
        # Describe disagreement patterns
        .describeDisagreementPattern = function(rating_table) {
            n_categories <- length(rating_table)
            total_ratings <- sum(rating_table)
            
            if (n_categories == 1) {
                return("Complete consensus")
            } else if (n_categories == 2) {
                props <- rating_table / total_ratings
                if (max(props) >= 0.8) {
                    return("Mostly agreement with some dissent")
                } else {
                    return("Split opinion")
                }
            } else {
                return(paste0("Multi-way disagreement (", n_categories, " different ratings)"))
            }
        },

        # Agreement Stability Analysis using Bootstrap
        .performStabilityAnalysis = function() {
            if (self$options$showProgressIndicators) {
                message("Performing stability analysis with bootstrap...")
            }
            
            stability_table <- self$results$stabilityTable
            
            tryCatch({
                stability_results <- private$.performBootstrapStability()
                
                for (statistic_name in names(stability_results)) {
                    stat_info <- stability_results[[statistic_name]]
                    
                    stability_table$addRow(rowKey = statistic_name, values = list(
                        statistic = statistic_name,
                        original_value = stat_info$original,
                        bootstrap_mean = stat_info$bootstrap_mean,
                        bootstrap_se = stat_info$bootstrap_se,
                        stability_index = stat_info$stability_index,
                        interpretation = stat_info$interpretation
                    ))
                }
                
                stability_table$setVisible(TRUE)
                
            }, error = function(e) {
                error_msg <- if (self$options$enhancedErrorGuidance) {
                    paste0("Error in stability analysis: ", e$message,
                          ". Requires sufficient sample size for meaningful bootstrap.")
                } else {
                    paste(.("Error in stability analysis:"), e$message)
                }
                
                stability_table$addRow(rowKey = "error", values = list(
                    statistic = "Error",
                    original_value = NaN,
                    bootstrap_mean = NaN,
                    bootstrap_se = NaN,
                    stability_index = NaN,
                    interpretation = error_msg
                ))
            })
        },
        
        # Perform bootstrap stability analysis
        .performBootstrapStability = function() {
            data_matrix <- private$.data_matrix
            n_cases <- nrow(data_matrix)
            # Use user-specified bootstrap samples, with validation
            n_bootstrap <- self$options$bootstrapSamples
            if (is.null(n_bootstrap) || n_bootstrap < 100) n_bootstrap <- 1000
            if (n_bootstrap > 5000) n_bootstrap <- 5000  # Cap for performance
            
            # Store original statistics
            original_stats <- private$.calculateOriginalStatistics()
            
            # Bootstrap sampling
            bootstrap_results <- list(
                overall_agreement = numeric(n_bootstrap),
                kappa = numeric(n_bootstrap)
            )
            
            for (b in seq_len(n_bootstrap)) {
                # Bootstrap sample with replacement
                sample_indices <- sample(seq_len(n_cases), size = n_cases, replace = TRUE)
                bootstrap_data <- data_matrix[sample_indices, , drop = FALSE]
                
                # Calculate statistics for bootstrap sample
                boot_agreement <- private$.calculateOverallAgreementFromMatrix(bootstrap_data)
                boot_kappa <- private$.calculateKappaFromMatrix(bootstrap_data)
                
                bootstrap_results$overall_agreement[b] <- boot_agreement
                bootstrap_results$kappa[b] <- boot_kappa
            }
            
            # Calculate stability metrics
            results <- list()
            
            for (stat_name in names(original_stats)) {
                original_value <- original_stats[[stat_name]]
                bootstrap_values <- bootstrap_results[[stat_name]]
                
                # Remove NA values
                bootstrap_values <- bootstrap_values[!is.na(bootstrap_values)]
                
                if (length(bootstrap_values) > 0) {
                    bootstrap_mean <- mean(bootstrap_values)
                    bootstrap_se <- sd(bootstrap_values)
                    
                    # Stability index (inverse of coefficient of variation)
                    stability_index <- if (bootstrap_mean != 0) {
                        1 / abs(bootstrap_se / bootstrap_mean)
                    } else {
                        0
                    }
                    
                    # Interpretation
                    interpretation <- private$.interpretStability(stability_index, bootstrap_se)
                    
                    results[[stat_name]] <- list(
                        original = original_value,
                        bootstrap_mean = bootstrap_mean,
                        bootstrap_se = bootstrap_se,
                        stability_index = stability_index,
                        interpretation = interpretation
                    )
                }
            }
            
            return(results)
        },
        
        # Calculate original statistics for comparison
        .calculateOriginalStatistics = function() {
            return(list(
                overall_agreement = private$.calculateOverallAgreement(),
                kappa = private$.calculateSimpleKappa()
            ))
        },
        
        # Calculate overall agreement from data matrix
        .calculateOverallAgreementFromMatrix = function(data_matrix) {
            total_agreements <- 0
            total_comparisons <- 0
            
            n_cases <- nrow(data_matrix)
            n_raters <- ncol(data_matrix)
            
            for (case_idx in seq_len(n_cases)) {
                case_ratings <- data_matrix[case_idx, ]
                valid_ratings <- case_ratings[!is.na(case_ratings)]
                
                if (length(valid_ratings) >= 2) {
                    n_pairs <- choose(length(valid_ratings), 2)
                    total_comparisons <- total_comparisons + n_pairs
                    
                    for (i in seq_len(length(valid_ratings) - 1)) {
                        for (j in (i + 1):length(valid_ratings)) {
                            if (valid_ratings[i] == valid_ratings[j]) {
                                total_agreements <- total_agreements + 1
                            }
                        }
                    }
                }
            }
            
            return(if (total_comparisons > 0) total_agreements / total_comparisons else NA)
        },
        
        # Calculate kappa from data matrix (simplified)
        .calculateKappaFromMatrix = function(data_matrix) {
            if (ncol(data_matrix) >= 2) {
                rater1 <- data_matrix[, 1]
                rater2 <- data_matrix[, 2]
                
                valid_idx <- !is.na(rater1) & !is.na(rater2)
                if (sum(valid_idx) >= 3) {
                    if (requireNamespace("irr", quietly = TRUE)) {
                        kappa_result <- tryCatch({
                            irr::kappa2(cbind(rater1[valid_idx], rater2[valid_idx]))
                        }, error = function(e) list(value = NA))
                        return(kappa_result$value)
                    }
                }
            }
            return(NA)
        },
        
        # Simple kappa calculation for original stats
        .calculateSimpleKappa = function() {
            return(private$.calculateKappaFromMatrix(private$.data_matrix))
        },
        
        # Interpret stability metrics
        .interpretStability = function(stability_index, bootstrap_se) {
            if (is.na(stability_index) || is.na(bootstrap_se)) {
                return("Cannot assess stability")
            }
            
            stability_level <- if (stability_index > 10) {
                "Very stable"
            } else if (stability_index > 5) {
                "Stable"
            } else if (stability_index > 2) {
                "Moderately stable"
            } else {
                "Unstable"
            }
            
            se_interpretation <- if (bootstrap_se < 0.05) {
                "low variability"
            } else if (bootstrap_se < 0.1) {
                "moderate variability"
            } else {
                "high variability"
            }
            
            return(paste0(stability_level, " (", se_interpretation, ")"))
        },

        # Generate inline statistical comments
        .generateInlineComments = function() {
            comments_html <- paste0(
                "<div style='background: #f8f9fa; border: 1px solid #dee2e6; border-radius: 8px; padding: 20px; font-family: \"Segoe UI\", Arial, sans-serif; line-height: 1.6;'>",
                
                "<h3 style='margin: 0 0 15px 0; color: #1976d2; font-size: 18px; border-bottom: 2px solid #e3f2fd; padding-bottom: 8px;'>",
                "📊 Statistical Commentary & Educational Notes</h3>",
                
                "<div style='background: #e8f5e8; border-left: 4px solid #4caf50; padding: 15px; margin-bottom: 15px;'>",
                "<h4 style='margin: 0 0 10px 0; color: #2e7d32; font-size: 15px;'>✅ Understanding Your Results</h4>",
                "<div style='font-size: 14px;'>",
                private$.generateResultsExplanation(),
                "</div></div>",
                
                "<div style='background: #fff3e0; border-left: 4px solid #ff9800; padding: 15px; margin-bottom: 15px;'>",
                "<h4 style='margin: 0 0 10px 0; color: #f57c00; font-size: 15px;'>🔍 Statistical Interpretation Guide</h4>",
                "<div style='font-size: 14px;'>",
                private$.generateInterpretationGuide(),
                "</div></div>",
                
                "<div style='background: #e1f5fe; border-left: 4px solid #0277bd; padding: 15px; margin-bottom: 15px;'>",
                "<h4 style='margin: 0 0 10px 0; color: #01579b; font-size: 15px;'>🎓 Educational Insights</h4>",
                "<div style='font-size: 14px;'>",
                private$.generateEducationalInsights(),
                "</div></div>",
                
                "<div style='background: #fce4ec; border-left: 4px solid #c2185b; padding: 15px;'>",
                "<h4 style='margin: 0 0 10px 0; color: #ad1457; font-size: 15px;'>⚠️ Important Considerations</h4>",
                "<div style='font-size: 14px;'>",
                private$.generateImportantConsiderations(),
                "</div></div>",
                
                "</div>"
            )
            
            self$results$inlineComments$setContent(comments_html)
            self$results$inlineComments$setVisible(TRUE)
        },
        
        # Generate explanation of current results
        .generateResultsExplanation = function() {
            data_summary <- private$.summarizeCurrentData()
            
            explanations <- c(
                paste0("📋 <strong>Your Dataset:</strong> ", data_summary$n_cases, " cases rated by ", 
                       data_summary$n_raters, " raters across ", data_summary$n_categories, " categories."),
                
                if (self$options$gwetAC) {
                    "🔬 <strong>Gwet's Coefficients:</strong> These are more robust than standard kappa when categories have unequal prevalence - common in clinical diagnoses."
                } else { NULL },
                
                if (self$options$pabak) {
                    "⚖️ <strong>PABAK Analysis:</strong> Adjusts for prevalence and bias issues that can artificially inflate or deflate standard kappa values."
                } else { NULL },
                
                if (self$options$raterBiasAnalysis) {
                    "👥 <strong>Bias Detection:</strong> Identifies systematic tendencies in individual raters - crucial for training and calibration programs."
                } else { NULL },
                
                if (self$options$caseDifficultyScoring) {
                    "📈 <strong>Difficulty Analysis:</strong> Cases with high disagreement may represent inherently ambiguous diagnoses or need for protocol refinement."
                } else { NULL }
            )
            
            return(paste(explanations[!sapply(explanations, is.null)], collapse = "<br><br>"))
        },
        
        # Generate statistical interpretation guide
        .generateInterpretationGuide = function() {
            guides <- c(
                "<strong>Kappa Interpretation (Landis & Koch):</strong>",
                "• 0.00-0.20: Slight agreement",
                "• 0.21-0.40: Fair agreement", 
                "• 0.41-0.60: Moderate agreement",
                "• 0.61-0.80: Substantial agreement",
                "• 0.81-1.00: Almost perfect agreement",
                "",
                if (self$options$gwetAC) {
                    c("<strong>Gwet's AC vs Kappa:</strong>",
                      "• AC coefficients are less affected by prevalence imbalance",
                      "• Often higher than kappa in high-agreement scenarios",
                      "• AC1 assumes uniform category distribution",
                      "• AC2 accounts for actual marginal distributions")
                } else { NULL },
                "",
                "<strong>Clinical Significance:</strong>",
                "• Values >0.60 generally acceptable for clinical decisions",
                "• Values >0.80 excellent for critical diagnoses", 
                "• Consider confidence intervals - wide CIs suggest instability"
            )
            
            return(paste(guides[!sapply(guides, is.null)], collapse = "<br>"))
        },
        
        # Generate educational insights
        .generateEducationalInsights = function() {
            insights <- c(
                "📚 <strong>Why Agreement Matters:</strong>",
                "• Ensures consistent patient care across different clinicians",
                "• Validates diagnostic criteria and protocols",
                "• Identifies training needs and calibration opportunities",
                "• Essential for research reproducibility and multi-center studies",
                "",
                "🔬 <strong>Advanced Features Explained:</strong>",
                if (self$options$agreementStabilityAnalysis) {
                    "• <em>Stability Analysis:</em> Bootstrap resampling assesses how consistent your agreement statistics would be with different case samples."
                } else { NULL },
                if (self$options$agreementTrendAnalysis) {
                    "• <em>Trend Analysis:</em> Tracks whether agreement improves over case sequence - useful for detecting learning effects or fatigue."
                } else { NULL },
                if (self$options$sampleSizePlanning) {
                    "• <em>Sample Size Planning:</em> Helps design future studies with adequate power to detect meaningful agreement levels."
                } else { NULL },
                "",
                "💡 <strong>Best Practices:</strong>",
                "• Pre-specify agreement thresholds before data collection",
                "• Use multiple agreement measures for comprehensive assessment",
                "• Consider clinical context when interpreting statistical significance",
                "• Regular calibration sessions improve and maintain agreement"
            )
            
            return(paste(insights[!sapply(insights, is.null)], collapse = "<br>"))
        },
        
        # Generate important considerations
        .generateImportantConsiderations = function() {
            considerations <- c(
                "⚠️ <strong>Statistical Assumptions:</strong>",
                "• Raters should be independent (no collaboration during rating)",
                "• Cases should be representative of the target population",
                "• Missing data patterns may affect results",
                "• Category definitions should be clear and consistent",
                "",
                "🎯 <strong>Clinical Interpretation:</strong>",
                "• High statistical agreement ≠ automatically clinically acceptable",
                "• Consider consequence of disagreements in your specific context",
                "• Some diagnostic categories may inherently have lower agreement",
                "• Training and protocols can improve agreement over time",
                "",
                if (private$.data_matrix %>% nrow() < 30) {
                    "📊 <strong>Sample Size Note:</strong> Small sample sizes may produce unstable agreement estimates. Consider collecting additional data for more reliable results."
                } else { NULL },
                "",
                "🔄 <strong>Next Steps:</strong>",
                "• Review cases with poor agreement for learning opportunities",
                "• Consider whether disagreements reveal protocol ambiguities",
                "• Plan follow-up calibration sessions if agreement is suboptimal",
                "• Document agreement thresholds in your research protocols"
            )
            
            return(paste(considerations[!sapply(considerations, is.null)], collapse = "<br>"))
        },
        
        # Summarize current dataset for commentary
        .summarizeCurrentData = function() {
            data_matrix <- private$.data_matrix
            
            return(list(
                n_cases = nrow(data_matrix),
                n_raters = ncol(data_matrix),
                n_categories = length(unique(as.vector(data_matrix[!is.na(data_matrix)])))
            ))
        },

        # Plot rendering functions for new visualizations
        .trendPlot = function(image, ggtheme, theme, ...) {
            if (!self$options$agreementTrendAnalysis) return()
            
            # Generate trend plot
            plot <- private$.createTrendVisualization()
            print(plot)
            TRUE
        },
        
        .biasPlot = function(image, ggtheme, theme, ...) {
            if (!self$options$raterBiasAnalysis) return()
            
            # Generate bias visualization
            plot <- private$.createBiasVisualization()
            print(plot)
            TRUE
        },
        
        .difficultyPlot = function(image, ggtheme, theme, ...) {
            if (!self$options$caseDifficultyScoring) return()
            
            # Generate difficulty distribution plot
            plot <- private$.createDifficultyVisualization()
            print(plot)
            TRUE
        },
        
        # Create trend visualization
        .createTrendVisualization = function() {
            # Simple trend visualization implementation
            if (!requireNamespace("ggplot2", quietly = TRUE)) {
                return(ggplot2::ggplot() + ggplot2::ggtitle("ggplot2 package required"))
            }
            
            # Placeholder data - in full implementation, use actual trend results
            trend_data <- data.frame(
                sequence_group = 1:5,
                agreement = c(0.65, 0.72, 0.78, 0.81, 0.85),
                group_label = paste("Group", 1:5)
            )
            
            ggplot2::ggplot(trend_data, ggplot2::aes(x = sequence_group, y = agreement)) +
                ggplot2::geom_line(color = "#1976d2", size = 1.2) +
                ggplot2::geom_point(color = "#1976d2", size = 3) +
                ggplot2::labs(
                    title = "Agreement Trend Over Case Sequence",
                    x = "Case Group",
                    y = "Agreement Proportion",
                    caption = "Shows how agreement changes across sequential case groups"
                ) +
                ggplot2::theme_minimal() +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
                    plot.caption = ggplot2::element_text(hjust = 0.5, style = "italic")
                )
        },
        
        # Create bias visualization
        .createBiasVisualization = function() {
            if (!requireNamespace("ggplot2", quietly = TRUE)) {
                return(ggplot2::ggplot() + ggplot2::ggtitle("ggplot2 package required"))
            }
            
            # Placeholder visualization
            bias_data <- data.frame(
                rater = paste("Rater", 1:4),
                bias_score = c(0.15, 0.08, 0.22, 0.12),
                severity = c("Mild", "Minimal", "Moderate", "Mild")
            )
            
            ggplot2::ggplot(bias_data, ggplot2::aes(x = rater, y = bias_score, fill = severity)) +
                ggplot2::geom_col() +
                ggplot2::scale_fill_manual(values = c("Minimal" = "#4caf50", "Mild" = "#ff9800", "Moderate" = "#f44336")) +
                ggplot2::labs(
                    title = "Rater Bias Detection Results",
                    x = "Rater",
                    y = "Bias Score",
                    fill = "Bias Severity",
                    caption = "Higher scores indicate greater systematic bias from consensus"
                ) +
                ggplot2::theme_minimal() +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
                    plot.caption = ggplot2::element_text(hjust = 0.5, style = "italic")
                )
        },
        
        # Create difficulty visualization
        .createDifficultyVisualization = function() {
            if (!requireNamespace("ggplot2", quietly = TRUE)) {
                return(ggplot2::ggplot() + ggplot2::ggtitle("ggplot2 package required"))
            }
            
            # Placeholder visualization
            difficulty_data <- data.frame(
                difficulty_score = runif(50, 0, 1),
                difficulty_level = sample(c("Easy", "Moderate", "Difficult", "Very Difficult"), 50, replace = TRUE)
            )
            
            ggplot2::ggplot(difficulty_data, ggplot2::aes(x = difficulty_score, fill = difficulty_level)) +
                ggplot2::geom_histogram(bins = 20, alpha = 0.7) +
                ggplot2::scale_fill_brewer(type = "seq", palette = "YlOrRd") +
                ggplot2::labs(
                    title = "Case Difficulty Score Distribution",
                    x = "Difficulty Score",
                    y = "Number of Cases",
                    fill = "Difficulty Level",
                    caption = "Based on rater disagreement patterns and entropy"
                ) +
                ggplot2::theme_minimal() +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
                    plot.caption = ggplot2::element_text(hjust = 0.5, style = "italic")
                )
        },
        
        # Interpret ICC values using standard guidelines
        .interpretICCValue = function(icc_value) {
            if (is.na(icc_value) || is.null(icc_value)) {
                return("Unable to calculate")
            }

            if (icc_value < 0) {
                return("Poor reliability (negative ICC)")
            } else if (icc_value < 0.5) {
                return("Poor reliability")
            } else if (icc_value < 0.75) {
                return("Moderate reliability")
            } else if (icc_value < 0.9) {
                return("Good reliability")
            } else {
                return("Excellent reliability")
            }
        },

        # ====================================================================
        # Clustering Analysis Methods (Usubutun et al. 2012)
        # ====================================================================

        # Perform hierarchical clustering of raters
        .performClusteringAnalysis = function() {
            if (!self$options$performClustering) {
                return()
            }

            # Ensure data is prepared
            if (is.null(private$.data_matrix)) {
                return()
            }

            # Need at least 3 raters for clustering
            if (private$.n_raters < 3) {
                self$results$styleGroupSummary$setNote(
                    'note',
                    'Clustering requires at least 3 raters'
                )
                return()
            }

            # Perform hierarchical clustering
            clustering_result <- private$.performRaterClustering()

            # Store results
            private$.style_clustering_results <- clustering_result

            # Populate result tables
            private$.populateStyleGroupSummary(clustering_result)
            private$.populateStyleGroupProfiles(clustering_result)

            # Identify discordant cases if requested
            if (self$options$identifyDiscordant) {
                private$.identifyClusteringDiscordantCases(clustering_result)
            }

            # Test associations with rater characteristics
            private$.testCharacteristicAssociations(clustering_result)

            # Compare with reference standard if provided
            if (!is.null(self$options$referenceStandard)) {
                private$.compareWithReference(clustering_result)
            }

            # Generate interpretation guide if requested
            if (self$options$showClusteringInterpretation) {
                private$.generateClusteringInterpretation(clustering_result)
            }
        },

        # Extract metadata rows from dataset
        .extractMetadata = function(data_subset) {
            # Get the full dataset including case ID
            case_id_var <- self$options$caseID
            full_data <- self$data

            # Identify metadata rows (case_id starts with "META_")
            case_ids <- as.character(full_data[[case_id_var]])
            metadata_rows <- grepl("^META_", case_ids, ignore.case = TRUE)

            if (!any(metadata_rows)) {
                # No metadata rows found
                return(list(
                    case_data = data_subset,
                    metadata = NULL,
                    has_metadata = FALSE
                ))
            }

            # Separate metadata from case data
            metadata_indices <- which(metadata_rows)
            case_indices <- which(!metadata_rows)

            # Extract case data (non-metadata rows)
            case_data <- data_subset[case_indices, , drop = FALSE]

            # Parse metadata
            rater_metadata <- list()

            for (idx in metadata_indices) {
                # Get metadata type from case_id
                meta_id <- case_ids[idx]
                meta_type <- gsub("^META_", "", meta_id, ignore.case = TRUE)
                meta_type <- tolower(trimws(meta_type))

                # Extract values for each rater
                rater_values <- as.character(full_data[idx, private$.rater_names])
                names(rater_values) <- private$.rater_names

                # Try to convert to numeric if possible (for experience, volume)
                if (meta_type %in% c("experience", "volume", "years", "age")) {
                    numeric_values <- suppressWarnings(as.numeric(rater_values))
                    if (!all(is.na(numeric_values))) {
                        rater_values <- numeric_values
                    }
                }

                rater_metadata[[meta_type]] <- rater_values
            }

            return(list(
                case_data = case_data,
                metadata = rater_metadata,
                has_metadata = TRUE
            ))
        },

        # Core clustering function using percentage agreement distance
        .performRaterClustering = function() {
            # Calculate pairwise agreement matrix
            n_raters <- private$.n_raters
            agreement_matrix <- matrix(0, nrow = n_raters, ncol = n_raters)
            rownames(agreement_matrix) <- private$.rater_names
            colnames(agreement_matrix) <- private$.rater_names

            # Calculate agreement for each rater pair
            for (i in 1:n_raters) {
                for (j in 1:n_raters) {
                    if (i == j) {
                        agreement_matrix[i, j] <- 1.0
                    } else {
                        # Percentage agreement
                        rater_i <- private$.data_matrix[[i]]
                        rater_j <- private$.data_matrix[[j]]
                        agreement_matrix[i, j] <- mean(rater_i == rater_j, na.rm = TRUE)
                    }
                }
            }

            # Convert to distance matrix (1 - agreement)
            distance_matrix <- as.dist(1 - agreement_matrix)

            # Perform hierarchical clustering
            linkage_method <- self$options$clusteringMethod
            hc <- hclust(distance_matrix, method = linkage_method)

            # Determine number of clusters
            if (self$options$autoSelectGroups) {
                # Use silhouette method
                k_optimal <- private$.selectOptimalK(distance_matrix, hc)
            } else {
                k_optimal <- self$options$nStyleGroups
            }

            # Cut tree to get cluster assignments
            cluster_assignments <- cutree(hc, k = k_optimal)

            # Calculate silhouette scores
            if (requireNamespace("cluster", quietly = TRUE)) {
                sil <- cluster::silhouette(cluster_assignments, distance_matrix)
                silhouette_scores <- as.numeric(sil[, "sil_width"])
            } else {
                silhouette_scores <- rep(NA, n_raters)
            }

            return(list(
                hclust_object = hc,
                cluster_assignments = cluster_assignments,
                agreement_matrix = agreement_matrix,
                distance_matrix = distance_matrix,
                n_clusters = k_optimal,
                silhouette_scores = silhouette_scores,
                rater_names = private$.rater_names
            ))
        },

        # Select optimal number of clusters using silhouette method
        .selectOptimalK = function(distance_matrix, hc) {
            n_raters <- private$.n_raters
            max_k <- min(10, n_raters - 1)

            if (!requireNamespace("cluster", quietly = TRUE)) {
                # Default to 3 if cluster package not available
                return(3)
            }

            # Try different k values
            sil_scores <- numeric(max_k - 1)
            for (k in 2:max_k) {
                clusters <- cutree(hc, k = k)
                sil <- cluster::silhouette(clusters, distance_matrix)
                sil_scores[k - 1] <- mean(sil[, "sil_width"])
            }

            # Return k with highest average silhouette score
            optimal_k <- which.max(sil_scores) + 1
            return(optimal_k)
        },

        # Populate style group summary table
        .populateStyleGroupSummary = function(clustering_result) {
            table <- self$results$styleGroupSummary

            cluster_assignments <- clustering_result$cluster_assignments
            n_clusters <- clustering_result$n_clusters
            agreement_matrix <- clustering_result$agreement_matrix
            silhouette_scores <- clustering_result$silhouette_scores
            rater_names <- clustering_result$rater_names

            for (k in 1:n_clusters) {
                # Get raters in this cluster
                cluster_members <- which(cluster_assignments == k)
                member_names <- paste(rater_names[cluster_members], collapse = ", ")
                n_raters <- length(cluster_members)

                # Calculate within-group agreement
                if (n_raters > 1) {
                    within_agreements <- agreement_matrix[cluster_members, cluster_members]
                    within_agreement <- mean(within_agreements[upper.tri(within_agreements)]) * 100
                } else {
                    within_agreement <- 100
                }

                # Calculate between-group agreement
                if (n_clusters > 1) {
                    other_members <- which(cluster_assignments != k)
                    if (length(other_members) > 0) {
                        between_agreements <- agreement_matrix[cluster_members, other_members, drop = FALSE]
                        between_agreement <- mean(between_agreements) * 100
                    } else {
                        between_agreement <- NA
                    }
                } else {
                    between_agreement <- NA
                }

                # Average silhouette score for this cluster
                cluster_sil <- mean(silhouette_scores[cluster_members], na.rm = TRUE)

                # Interpretation
                interpretation <- private$.interpretStyleGroup(
                    within_agreement,
                    between_agreement,
                    cluster_sil
                )

                table$addRow(rowKey = k, values = list(
                    style_group = paste("Group", k),
                    n_raters = n_raters,
                    rater_names = member_names,
                    within_agreement = within_agreement,
                    between_agreement = between_agreement,
                    silhouette_score = cluster_sil,
                    interpretation = interpretation
                ))
            }
        },

        # Interpret style group characteristics
        .interpretStyleGroup = function(within_agreement, between_agreement, silhouette) {
            if (is.na(silhouette) || silhouette < 0.25) {
                return("Poorly separated group")
            } else if (silhouette < 0.5) {
                return("Moderately cohesive diagnostic style")
            } else if (silhouette < 0.7) {
                return("Distinct diagnostic style")
            } else {
                return("Highly consistent diagnostic style")
            }
        },

        # Populate style group diagnostic profiles
        .populateStyleGroupProfiles = function(clustering_result) {
            table <- self$results$styleGroupProfiles

            cluster_assignments <- clustering_result$cluster_assignments
            n_clusters <- clustering_result$n_clusters

            # Get all diagnoses by cluster
            for (k in 1:n_clusters) {
                cluster_members <- which(cluster_assignments == k)

                # Get all diagnoses from raters in this cluster
                cluster_diagnoses <- unlist(private$.data_matrix[, cluster_members])

                # Frequency table
                freq_table <- table(cluster_diagnoses)
                total <- sum(freq_table)

                for (category in names(freq_table)) {
                    freq <- as.numeric(freq_table[category])
                    pct <- (freq / total) * 100

                    # Compare to other groups
                    relative_freq <- private$.compareToOtherGroups(
                        category,
                        k,
                        cluster_assignments,
                        n_clusters
                    )

                    table$addRow(rowKey = paste0(k, "_", category), values = list(
                        style_group = paste("Group", k),
                        category = category,
                        frequency = freq,
                        percentage = pct,
                        relative_frequency = relative_freq
                    ))
                }
            }
        },

        # Compare category frequency to other groups
        .compareToOtherGroups = function(category, current_group, cluster_assignments, n_clusters) {
            if (n_clusters == 1) {
                return("N/A")
            }

            # Get frequency in current group
            current_members <- which(cluster_assignments == current_group)
            current_diagnoses <- unlist(private$.data_matrix[, current_members])
            current_freq <- mean(current_diagnoses == category)

            # Get frequency in other groups
            other_members <- which(cluster_assignments != current_group)
            other_diagnoses <- unlist(private$.data_matrix[, other_members])
            other_freq <- mean(other_diagnoses == category)

            # Compare
            diff <- current_freq - other_freq
            if (abs(diff) < 0.05) {
                return("Similar")
            } else if (diff > 0.15) {
                return("Much higher")
            } else if (diff > 0.05) {
                return("Higher")
            } else if (diff < -0.15) {
                return("Much lower")
            } else {
                return("Lower")
            }
        },

        # Identify high-disagreement cases between style groups
        .identifyClusteringDiscordantCases = function(clustering_result) {
            table <- self$results$discordantCasesCluster

            cluster_assignments <- clustering_result$cluster_assignments
            n_clusters <- clustering_result$n_clusters

            if (n_clusters < 2) {
                table$setNote('note', 'Discordant case analysis requires at least 2 style groups')
                return()
            }

            threshold <- self$options$discordantThreshold

            # For each case, calculate disagreement between groups
            for (i in 1:private$.n_cases) {
                case_diagnoses <- as.character(private$.data_matrix[i, ])

                # Calculate entropy of diagnoses in this case
                diagnosis_freq <- table(case_diagnoses)
                diagnosis_prop <- diagnosis_freq / sum(diagnosis_freq)
                entropy <- -sum(diagnosis_prop * log2(diagnosis_prop + 1e-10))

                # Calculate between-group disagreement
                disagreement_scores <- numeric()
                group_patterns <- character()

                for (k1 in 1:(n_clusters - 1)) {
                    for (k2 in (k1 + 1):n_clusters) {
                        group1_members <- which(cluster_assignments == k1)
                        group2_members <- which(cluster_assignments == k2)

                        group1_diagnoses <- case_diagnoses[group1_members]
                        group2_diagnoses <- case_diagnoses[group2_members]

                        # Proportion of disagreement
                        disagreement <- 1 - mean(outer(group1_diagnoses, group2_diagnoses, "=="))
                        disagreement_scores <- c(disagreement_scores, disagreement)

                        # Most common diagnoses
                        g1_mode <- names(which.max(table(group1_diagnoses)))
                        g2_mode <- names(which.max(table(group2_diagnoses)))
                        group_patterns <- c(group_patterns,
                            sprintf("G%d:%s vs G%d:%s", k1, g1_mode, k2, g2_mode))
                    }
                }

                max_disagreement <- max(disagreement_scores)

                # Only include cases with high disagreement
                if (max_disagreement >= threshold) {
                    # Difficulty level based on entropy
                    difficulty <- if (entropy < 0.5) {
                        "Low disagreement"
                    } else if (entropy < 1.0) {
                        "Moderate disagreement"
                    } else if (entropy < 1.5) {
                        "High disagreement"
                    } else {
                        "Very high disagreement"
                    }

                    table$addRow(rowKey = i, values = list(
                        case_id = as.character(i),
                        disagreement_score = max_disagreement,
                        entropy = entropy,
                        style_group_patterns = paste(group_patterns, collapse = "; "),
                        difficulty_level = difficulty
                    ))
                }
            }
        },

        # Test associations between style groups and rater characteristics
        .testCharacteristicAssociations = function(clustering_result) {
            table <- self$results$characteristicAssociations

            cluster_assignments <- clustering_result$cluster_assignments

            # List of rater characteristics to test
            characteristics_data <- list()

            # First, check if we have metadata from metadata rows
            if (!is.null(private$.rater_metadata)) {
                # Use metadata extracted from rows
                for (meta_name in names(private$.rater_metadata)) {
                    # Capitalize first letter for display
                    display_name <- paste0(toupper(substring(meta_name, 1, 1)), substring(meta_name, 2))
                    characteristics_data[[display_name]] <- private$.rater_metadata[[meta_name]]
                }
            } else {
                # Fall back to using variable options
                if (!is.null(self$options$raterExperience)) {
                    char_data <- self$data[[self$options$raterExperience]]
                    if (length(char_data) == private$.n_raters) {
                        characteristics_data[["Experience"]] <- char_data
                    }
                }
                if (!is.null(self$options$raterSpecialty)) {
                    char_data <- self$data[[self$options$raterSpecialty]]
                    if (length(char_data) == private$.n_raters) {
                        characteristics_data[["Specialty"]] <- char_data
                    }
                }
                if (!is.null(self$options$raterInstitution)) {
                    char_data <- self$data[[self$options$raterInstitution]]
                    if (length(char_data) == private$.n_raters) {
                        characteristics_data[["Institution"]] <- char_data
                    }
                }
                if (!is.null(self$options$raterVolume)) {
                    char_data <- self$data[[self$options$raterVolume]]
                    if (length(char_data) == private$.n_raters) {
                        characteristics_data[["Case Volume"]] <- char_data
                    }
                }
            }

            if (length(characteristics_data) == 0) {
                table$setNote('note', 'No rater characteristics provided for association testing')
                return()
            }

            # Test each characteristic
            for (char_name in names(characteristics_data)) {
                char_data <- characteristics_data[[char_name]]

                # Test association
                test_result <- private$.testCharacteristicAssociation(
                    char_data,
                    cluster_assignments,
                    char_name
                )

                if (!is.null(test_result)) {
                    table$addRow(rowKey = char_name, values = test_result)
                }
            }
        },

        # Statistical test for characteristic-cluster association
        .testCharacteristicAssociation = function(char_data, clusters, char_name) {
            # Determine if continuous or categorical
            is_numeric <- is.numeric(char_data)

            if (is_numeric) {
                # Kruskal-Wallis test for continuous data
                tryCatch({
                    test <- kruskal.test(char_data ~ clusters)

                    # Effect size: eta-squared
                    effect_size <- test$statistic / (length(char_data) - 1)

                    interpretation <- private$.interpretAssociation(
                        test$p.value,
                        effect_size,
                        is_numeric
                    )

                    return(list(
                        characteristic = char_name,
                        test_statistic = test$statistic,
                        df = test$parameter,
                        p_value = test$p.value,
                        effect_size = effect_size,
                        interpretation = interpretation
                    ))
                }, error = function(e) {
                    return(NULL)
                })
            } else {
                # Chi-square or Fisher's exact test for categorical data
                tryCatch({
                    contingency_table <- table(char_data, clusters)

                    # Check expected frequencies
                    if (any(contingency_table < 5)) {
                        # Use Fisher's exact test
                        test <- fisher.test(contingency_table, simulate.p.value = TRUE)
                        effect_size <- sqrt(test$statistic / sum(contingency_table))

                        interpretation <- private$.interpretAssociation(
                            test$p.value,
                            effect_size,
                            is_numeric = FALSE
                        )

                        return(list(
                            characteristic = char_name,
                            test_statistic = NA,
                            df = NA,
                            p_value = test$p.value,
                            effect_size = effect_size,
                            interpretation = interpretation
                        ))
                    } else {
                        # Use Chi-square test
                        test <- chisq.test(contingency_table)

                        # Cramér's V effect size
                        n <- sum(contingency_table)
                        min_dim <- min(nrow(contingency_table), ncol(contingency_table)) - 1
                        effect_size <- sqrt(test$statistic / (n * min_dim))

                        interpretation <- private$.interpretAssociation(
                            test$p.value,
                            effect_size,
                            is_numeric = FALSE
                        )

                        return(list(
                            characteristic = char_name,
                            test_statistic = test$statistic,
                            df = test$parameter,
                            p_value = test$p.value,
                            effect_size = effect_size,
                            interpretation = interpretation
                        ))
                    }
                }, error = function(e) {
                    return(NULL)
                })
            }
        },

        # Interpret association test results
        .interpretAssociation = function(p_value, effect_size, is_numeric) {
            sig_text <- if (p_value < 0.001) {
                "highly significant"
            } else if (p_value < 0.01) {
                "significant"
            } else if (p_value < 0.05) {
                "marginally significant"
            } else {
                "not significant"
            }

            effect_text <- if (is.na(effect_size)) {
                ""
            } else if (effect_size < 0.1) {
                ", negligible effect"
            } else if (effect_size < 0.3) {
                ", small effect"
            } else if (effect_size < 0.5) {
                ", moderate effect"
            } else {
                ", large effect"
            }

            return(paste0("Association ", sig_text, effect_text))
        },

        # Compare style groups with reference standard
        .compareWithReference = function(clustering_result) {
            table <- self$results$referenceComparison

            ref_var <- self$options$referenceStandard
            if (is.null(ref_var)) {
                return()
            }

            # Get reference standard data
            ref_data <- self$data[[ref_var]]
            if (length(ref_data) != private$.n_cases) {
                table$setNote('note', 'Reference standard length does not match number of cases')
                return()
            }

            cluster_assignments <- clustering_result$cluster_assignments
            n_clusters <- clustering_result$n_clusters

            # For each style group, calculate agreement with reference
            for (k in 1:n_clusters) {
                cluster_members <- which(cluster_assignments == k)

                # Get diagnoses from this group
                group_diagnoses_list <- private$.data_matrix[, cluster_members, drop = FALSE]

                # Calculate kappa for each rater in group vs reference
                kappa_values <- numeric()
                agreement_values <- numeric()

                for (rater_idx in cluster_members) {
                    rater_diagnoses <- private$.data_matrix[[rater_idx]]

                    # Calculate Cohen's kappa
                    tryCatch({
                        kappa_result <- irr::kappa2(data.frame(rater_diagnoses, ref_data))
                        kappa_values <- c(kappa_values, kappa_result$value)

                        # Agreement percentage
                        agreement <- mean(rater_diagnoses == ref_data) * 100
                        agreement_values <- c(agreement_values, agreement)
                    }, error = function(e) {
                        # Skip if calculation fails
                    })
                }

                # Average kappa and agreement for this group
                avg_kappa <- mean(kappa_values, na.rm = TRUE)
                avg_agreement <- mean(agreement_values, na.rm = TRUE)

                # Confidence interval (approximate)
                se_kappa <- sd(kappa_values, na.rm = TRUE) / sqrt(length(kappa_values))
                ci_lower <- avg_kappa - 1.96 * se_kappa
                ci_upper <- avg_kappa + 1.96 * se_kappa

                # Accuracy level
                accuracy_level <- private$.interpretKappa(avg_kappa)

                table$addRow(rowKey = k, values = list(
                    style_group = paste("Group", k),
                    kappa_vs_reference = avg_kappa,
                    agreement_percent = avg_agreement,
                    ci_lower = ci_lower,
                    ci_upper = ci_upper,
                    accuracy_level = accuracy_level
                ))
            }
        },

        # Generate clustering interpretation guide
        .generateClusteringInterpretation = function(clustering_result) {
            html_content <- self$results$clusteringInterpretation

            n_clusters <- clustering_result$n_clusters

            html <- '<div style="font-family: Arial, sans-serif; max-width: 900px; margin: 20px;">'
            html <- paste0(html, '<h2 style="color: #1976d2;">Clustering Analysis Interpretation Guide</h2>')

            html <- paste0(html, '<h3>What is Diagnostic Style Clustering?</h3>')
            html <- paste0(html, '<p>This analysis groups raters based on their diagnostic patterns, ')
            html <- paste0(html, 'identifying "diagnostic styles" - consistent approaches to diagnosis ')
            html <- paste0(html, 'that differ systematically between groups.</p>')

            html <- paste0(html, '<h3>Your Results</h3>')
            html <- paste0(html, sprintf('<p>Analysis identified <strong>%d diagnostic style groups</strong> ', n_clusters))
            html <- paste0(html, 'using hierarchical clustering with percentage agreement distance.</p>')

            html <- paste0(html, '<h3>Understanding the Tables</h3>')
            html <- paste0(html, '<ul>')
            html <- paste0(html, '<li><strong>Style Groups Summary:</strong> Shows rater composition and agreement within/between groups</li>')
            html <- paste0(html, '<li><strong>Diagnostic Patterns:</strong> Shows how each group uses diagnostic categories</li>')
            html <- paste0(html, '<li><strong>Discordant Cases:</strong> Cases where style groups disagree most - distinguishing cases</li>')
            html <- paste0(html, '<li><strong>Characteristic Associations:</strong> Tests whether style relates to experience, institution, etc.</li>')
            html <- paste0(html, '</ul>')

            html <- paste0(html, '<h3>Clinical Implications</h3>')
            html <- paste0(html, '<p><strong>High within-group agreement</strong> suggests consistent diagnostic approach within style groups.</p>')
            html <- paste0(html, '<p><strong>Low between-group agreement</strong> indicates systematic differences in diagnostic interpretation.</p>')
            html <- paste0(html, '<p><strong>No association with characteristics</strong> (as in Usubutun 2012) suggests diagnostic style ')
            html <- paste0(html, 'is not determined by training, experience, or institution.</p>')

            html <- paste0(html, '<h3>Silhouette Scores</h3>')
            html <- paste0(html, '<ul>')
            html <- paste0(html, '<li><strong>&gt; 0.7:</strong> Strong separation - distinct diagnostic styles</li>')
            html <- paste0(html, '<li><strong>0.5-0.7:</strong> Moderate separation - recognizable styles</li>')
            html <- paste0(html, '<li><strong>&lt; 0.5:</strong> Weak separation - overlapping approaches</li>')
            html <- paste0(html, '</ul>')

            html <- paste0(html, '</div>')

            html_content$setContent(html)
        },

        # ====================================================================
        # Plot Render Functions for Clustering
        # ====================================================================

        # Render clustering heatmap with dual dendrograms
        .clusteringHeatmap = function(image, ggtheme, theme, ...) {
            # Check both performClustering and showClusteringHeatmap options
            if (!self$options$performClustering || !self$options$showClusteringHeatmap) {
                return(FALSE)
            }

            if (is.null(private$.style_clustering_results)) {
                return(FALSE)
            }

            if (!requireNamespace("pheatmap", quietly = TRUE) &&
                !requireNamespace("ComplexHeatmap", quietly = TRUE)) {
                # Fallback to basic ggplot2 heatmap
                return(private$.clusteringHeatmapGgplot(image))
            }

            clustering_result <- private$.style_clustering_results

            # Create matrix for heatmap (cases x raters)
            heatmap_matrix <- as.matrix(private$.data_matrix)

            # Convert to numeric if categorical
            if (!is.numeric(heatmap_matrix[1, 1])) {
                # Map categories to numbers
                all_categories <- sort(unique(as.vector(heatmap_matrix)))
                category_map <- seq_along(all_categories)
                names(category_map) <- all_categories

                heatmap_numeric <- matrix(category_map[heatmap_matrix],
                    nrow = nrow(heatmap_matrix),
                    ncol = ncol(heatmap_matrix))
                colnames(heatmap_numeric) <- colnames(heatmap_matrix)
                rownames(heatmap_numeric) <- rownames(heatmap_matrix)
            } else {
                heatmap_numeric <- heatmap_matrix
            }

            # Use pheatmap if available
            if (requireNamespace("pheatmap", quietly = TRUE)) {
                # Get color scheme
                color_scheme <- self$options$heatmapColorScheme
                if (color_scheme == "viridis") {
                    colors <- viridisLite::viridis(100)
                } else if (color_scheme == "RdYlBu") {
                    colors <- grDevices::colorRampPalette(
                        c("#d73027", "#fee090", "#4575b4"))(100)
                } else {
                    # Diagnostic scheme
                    colors <- grDevices::colorRampPalette(
                        c("#2196f3", "#4caf50", "#ffc107"))(100)
                }

                # Create dendrogram from clustering result
                rater_dendrogram <- as.dendrogram(clustering_result$hclust_object)

                # Create heatmap
                pheatmap::pheatmap(
                    t(heatmap_numeric),  # Transpose: raters as rows
                    cluster_rows = rater_dendrogram,
                    cluster_cols = TRUE,
                    color = colors,
                    main = "Rater Clustering Heatmap",
                    fontsize = 10,
                    angle_col = 45,
                    cutree_rows = clustering_result$n_clusters,
                    filename = NULL
                )

                TRUE
            } else {
                return(private$.clusteringHeatmapGgplot(image))
            }
        },

        # Fallback ggplot2 clustering heatmap
        .clusteringHeatmapGgplot = function(image) {
            if (!requireNamespace("ggplot2", quietly = TRUE)) {
                return(FALSE)
            }

            clustering_result <- private$.style_clustering_results

            # Prepare data for ggplot
            heatmap_data <- private$.data_matrix
            heatmap_data$case <- seq_len(nrow(heatmap_data))

            # Melt to long format
            heatmap_long <- reshape2::melt(heatmap_data,
                id.vars = "case",
                variable.name = "rater",
                value.name = "diagnosis")

            # Get cluster assignments
            cluster_order <- order(clustering_result$cluster_assignments)
            rater_levels <- private$.rater_names[cluster_order]
            heatmap_long$rater <- factor(heatmap_long$rater, levels = rater_levels)

            # Create plot
            p <- ggplot2::ggplot(heatmap_long,
                ggplot2::aes(x = case, y = rater, fill = diagnosis)) +
                ggplot2::geom_tile(color = "white", size = 0.5) +
                ggplot2::scale_fill_viridis_d(option = "D") +
                ggplot2::labs(
                    title = "Rater Clustering Heatmap",
                    x = "Case",
                    y = "Rater (ordered by cluster)",
                    fill = "Diagnosis"
                ) +
                ggplot2::theme_minimal() +
                ggplot2::theme(
                    axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, size = 6),
                    axis.text.y = ggplot2::element_text(size = 8),
                    plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
                )

            print(p)
            TRUE
        },

        # Render dendrogram
        .clusterDendrogram = function(image, ggtheme, theme, ...) {
            if (!self$options$performClustering) {
                return(FALSE)
            }

            if (is.null(private$.style_clustering_results)) {
                return(FALSE)
            }

            clustering_result <- private$.style_clustering_results

            # Plot dendrogram with cluster groups
            hc <- clustering_result$hclust_object
            k <- clustering_result$n_clusters

            # Use base R plot
            plot(hc,
                main = sprintf("Hierarchical Clustering Dendrogram (%d groups)", k),
                xlab = "Rater",
                ylab = "Distance (1 - Agreement)",
                sub = "",
                cex.main = 1.2)

            # Add rectangles around clusters
            rect.hclust(hc, k = k, border = 2:6)

            TRUE
        },

        # Render silhouette plot
        .silhouettePlot = function(image, ggtheme, theme, ...) {
            if (!self$options$performClustering) {
                return(FALSE)
            }

            if (is.null(private$.style_clustering_results)) {
                return(FALSE)
            }

            if (!requireNamespace("cluster", quietly = TRUE)) {
                return(FALSE)
            }

            clustering_result <- private$.style_clustering_results

            # Create silhouette object
            sil <- cluster::silhouette(
                clustering_result$cluster_assignments,
                clustering_result$distance_matrix
            )

            # Plot silhouette
            plot(sil,
                main = "Cluster Quality (Silhouette Plot)",
                col = 2:(clustering_result$n_clusters + 1),
                border = NA,
                cex.names = 0.8)

            # Add average silhouette width
            avg_sil <- mean(sil[, "sil_width"])
            mtext(sprintf("Average silhouette width: %.3f", avg_sil),
                side = 3, line = -1, cex = 0.9)

            TRUE
        }
    ), # End of private list
    public = list(
        #' @description
        #' Generate R source code for Pathology Agreement analysis
        #' @return Character string with R syntax for reproducible analysis
        asSource = function() {
            vars <- self$options$vars
            if (is.null(vars) || length(vars) == 0)
                return('')

            # Escape variable names that contain spaces or special characters
            vars_escaped <- sapply(vars, function(v) {
                if (!is.null(v) && !identical(make.names(v), v))
                    paste0('`', v, '`')
                else
                    v
            })

            # Build vars argument
            vars_arg <- paste0('vars = c(', paste(sapply(vars_escaped, function(v) paste0('"', v, '"')), collapse = ', '), ')')

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
            paste0(pkg_name, '::pathagreement(\n    data = data,\n    ',
                   vars_arg, args, ')')
        }
    ) # End of public list
)
