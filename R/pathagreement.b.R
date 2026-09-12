# ggplot2 aesthetics name data-frame columns by bare symbol; declare them so
# R CMD check does not report "no visible binding for global variable".
utils::globalVariables(c(
    "Var1", "Var2", "value", "pair", "lo", "hi", "band", "category", "label",
    "reference", "observed", "on_dark", "x", "y", "xend", "yend", "xmin", "xmax",
    "ymin", "ymax", "Rater", "Case", "Diagnosis", "group", "rater", "bias",
    "severity", "score", "level", "case", "diagnosis"
))

#' @title Pathology Interrater Reliability Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom ggplot2 ggplot aes geom_tile geom_text scale_fill_viridis_c scale_fill_gradient2 labs
#' @importFrom ggplot2 theme_minimal theme element_text xlim ylim theme_void geom_segment geom_rect
#' @importFrom ggplot2 theme_classic element_blank margin scale_x_continuous scale_y_continuous
#' @importFrom ggplot2 scale_fill_manual scale_x_discrete scale_y_discrete coord_cartesian
#' @importFrom magrittr %>%
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
#' @return An \code{R6} class generator object for the \code{pathagreementClass} backend; used internally by the jamovi analysis wrapper and not called directly.

pathagreementClass <- if (requireNamespace("jmvcore")) {
    R6::R6Class(
        "pathagreementClass",
        inherit = pathagreementBase,
        private = list(
            # Private data storage
            .data_matrix = NULL,
            .available_data_matrix = NULL,
            .kripp_data_matrix = NULL,
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
            .retained_rows = NULL,
            .restored_plot = FALSE,
            .plot_reference = NULL,
            .plot_case_labels = NULL,
            .distance_metric = NULL,

            # Initialization
            .init = function() {
                private$.checkDependencies()
                private$.initializeRows()
                if (private$.shouldShowWelcome()) {
                    private$.handleWelcomeAndEducation()
                }
            },

            .initializeRows = function() {
                vars <- self$options$vars
                if (length(vars) < 2 || is.null(self$data) || !all(vars %in% names(self$data))) {
                    return()
                }
                dm <- private$.dropMetadataRows(self$data[vars])
                kept <- which(!private$.metadataRowFlags())[complete.cases(dm)]
                dm <- dm[complete.cases(dm), , drop = FALSE]
                n <- nrow(dm)
                raters <- length(vars)
                add <- function(name, keys) {
                    table <- if (name == "frequencyTable") {
                        self$results$raterFrequencyTables$frequencyTable
                    } else {
                        self$results[[name]]
                    }
                    table$deleteRows()
                    for (key in keys) table$addRow(rowKey = unname(key))
                }
                method <- self$options$multiraterMethod
                kappa_keys <- if (method == "krippendorff") {
                    "krippendorff"
                } else if (raters == 2) {
                    "cohens"
                } else if (method == "cohen") {
                    apply(utils::combn(seq_len(raters), 2), 2, function(x) {
                        paste0("cohens_", x[1], "_", x[2])
                    })
                } else {
                    "fleiss"
                }
                add("kappaTable", kappa_keys)
                add("iccTable", if (self$options$icc && private$.commonOrdinalScale(dm)) "ICC21" else character())
                add("krippTable", if (self$options$kripp) "kripp" else character())
                add("gwetACTable", if (self$options$gwetAC) c("AC1", "AC2") else character())
                add("pabakTable", if (self$options$pabak) "overall" else character())
                add("pairwiseTable", if (self$options$pairwiseAnalysis) {
                    apply(utils::combn(vars, 2), 2, paste, collapse = " vs ")
                } else character())
                cats <- private$.categoryLabels(dm)
                add("categoryTable", if (self$options$categoryAnalysis) cats else character())
                ref <- self$options$referenceStandard
                valid_ref <- !is.null(ref) && ref %in% names(self$data) &&
                    sum(!is.na(self$data[[ref]][kept])) >= 2
                add("diagnosticAccuracyTable", if (self$options$pathologyContext && valid_ref) vars else character())
                add("diagnosticStyleTable", if (self$options$performClustering && raters >= 3 && n >= 5) vars else character())
                add("consensusSummary", if (self$options$consensus) {
                    c("total", "consensus", "unanimous", "super_majority", "majority",
                      "tied", "resolved_ties", "no_consensus")
                } else character())
                add("consensusTable", if (self$options$consensus && self$options$show_consensus_table) seq_len(n) else integer())
                add("raterBiasTable", if (self$options$raterBiasAnalysis) vars else character())
                add("caseDifficultyTable", if (self$options$caseDifficultyScoring) seq_len(n) else integer())
                add("stabilityTable", if (self$options$agreementStabilityAnalysis) {
                    c("overall_agreement", "kappa")
                } else character())
                group_size <- max(10, floor(n / 5))
                trend_keys <- vapply(seq_len(ceiling(n / group_size)), function(i) {
                    jmvcore::format(
                        .("Cases {start}-{end}"),
                        start = (i - 1) * group_size + 1,
                        end = min(i * group_size, n)
                    )
                }, character(1))
                add("agreementTrendTable", if (self$options$agreementTrendAnalysis) trend_keys else character())
                sample_keys <- if (length(cats) %in% 2:5) {
                    vapply(sort(unique(c(2, 3, min(max(raters, 2), 6)))), function(x) {
                        jmvcore::format(.("{count} raters"), count = x)
                    }, character(1))
                } else .("Not available")
                add("sampleSizeTable", if (self$options$sampleSizePlanning) sample_keys else character())
                freq_keys <- unlist(lapply(seq_along(vars), function(i) {
                    paste(i, seq_along(levels(dm[[i]])), sep = "_")
                }), use.names = FALSE)
                add("frequencyTable", if (self$options$sft) freq_keys else character())
                add("crosstabTable", if (self$options$sft && raters == 2) levels(dm[[1]]) else character())
            },

            .plotNames = function() {
                c("heatmapPlot", "pairwisePlot", "categoryPlot", "confusionMatrixPlot",
                  "diagnosticStyleDendrogram", "diagnosticStyleHeatmap", "diagnosticStyleCombined",
                  "trendPlot", "biasPlot", "difficultyPlot", "clusteringHeatmap",
                  "clusterDendrogram", "silhouettePlot")
            },

            .storePlotStates = function() {
                enabled <- c(
                    self$options$heatmap, self$options$pairwiseAnalysis,
                    self$options$categoryAnalysis, self$options$pathologyContext,
                    self$options$performClustering,
                    self$options$performClustering && self$options$showClusteringHeatmap,
                    self$options$performClustering, self$options$agreementTrendAnalysis,
                    self$options$raterBiasAnalysis, self$options$caseDifficultyScoring,
                    self$options$performClustering && self$options$showClusteringHeatmap,
                    self$options$performClustering, self$options$performClustering
                )
                state <- list(
                    data = private$.data_matrix, raters = private$.rater_names,
                    available = private$.available_data_matrix,
                    categories = private$.categories, rows = private$.retained_rows,
                    reference = private$.referenceStandardValues(), case_labels = private$.caseLabels(),
                    clustering = private$.style_clustering_results
                )
                for (name in private$.plotNames()[enabled]) {
                    self$results[[name]]$setState(state)
                }
            },

            .restorePlotState = function(image) {
                state <- image$state
                if (is.null(state)) return(FALSE)
                private$.data_matrix <- state$data
                private$.available_data_matrix <- if (is.null(state$available)) state$data else state$available
                private$.rater_names <- state$raters
                private$.categories <- state$categories
                private$.retained_rows <- state$rows
                private$.n_cases <- nrow(state$data)
                private$.n_raters <- ncol(state$data)
                private$.style_clustering_results <- state$clustering
                private$.plot_reference <- state$reference
                private$.plot_case_labels <- state$case_labels
                private$.restored_plot <- TRUE
                TRUE
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
                if (self$options$showWeightedKappaGuide && self$options$wght != "unweighted") {
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
                    if (self$options$showWeightedKappaGuide && self$options$wght != "unweighted") {
                        private$.generateWeightedKappaGuide()
                    }
                    self$results$todo$setVisible(TRUE)
                    jmvcore::reject(.("Agreement analysis requires at least 2 raters."))
                }

                if (nrow(self$data) < 2) {
                    jmvcore::reject(.("Agreement analysis requires at least 2 cases (observations)."))
                }

                # Enhanced data validation
                private$.validateData()

                # Prepare data
                private$.prepareData()

                # Krippendorff's alpha and pairwise Cohen analyses can use
                # partially observed cases even when no row is complete across
                # every rater. Publish those estimable results and explain why
                # analyses requiring joint completeness are unavailable.
                if (private$.n_cases == 0) {
                    partial_n <- nrow(private$.kripp_data_matrix)
                    pairwise_requested <- self$options$pairwiseAnalysis ||
                        self$options$multiraterMethod == "cohen"
                    self$results$todo$setVisible(FALSE)
                    self$results$overviewTable$setRow(rowNo = 1, values = list(
                        cases = partial_n,
                        raters = private$.n_raters,
                        categories = length(private$.categories),
                        overall_agreement = NaN,
                        primary_method = if (self$options$multiraterMethod == "krippendorff") {
                            .("Krippendorff's Alpha")
                        } else {
                            .("Pairwise available-case analysis")
                        }
                    ))
                    private$.accumulateMessage(
                        .("No case is complete across all selected raters. Requested analyses that support partially observed ratings used the available data; analyses requiring jointly complete rows were skipped."),
                        severity = "strong_warning"
                    )
                    if (self$options$multiraterMethod %in% c("cohen", "krippendorff")) {
                        private$.performKappaAnalysis()
                    }
                    if (self$options$kripp) {
                        private$.performKrippendorffAnalysis()
                    }
                    if (self$options$pairwiseAnalysis) {
                        private$.performPairwiseAnalysis()
                    }
                    if (self$options$sft) {
                        private$.generateFrequencyTables()
                    }
                    if (!pairwise_requested &&
                        self$options$multiraterMethod != "krippendorff" &&
                        !self$options$kripp) {
                        self$results$kappaTable$setNote(
                            "missing",
                            .("The selected overall agreement method requires jointly complete rows, so it could not be calculated.")
                        )
                    }
                    private$.storePlotStates()
                    private$.renderMessages()
                    return()
                }

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

                if (self$options$pairwiseAnalysis) {
                    private$.checkpoint()
                    private$.performPairwiseAnalysis()
                }

                if (self$options$categoryAnalysis) {
                    private$.checkpoint()
                    private$.performCategoryAnalysis()
                }

                if (self$options$outlierAnalysis) {
                    private$.performOutlierAnalysis()
                }

                if (self$options$pathologyContext) {
                    private$.performPathologyAnalysis()
                }

                # Rater clustering (Usubutun et al. 2012). One prerequisite check
                # covers both clustering paths - the diagnostic-style path used to
                # run with no check at all.
                if (self$options$performClustering) {
                    if (private$.n_raters < 3) {
                        private$.accumulateMessage(.("Rater clustering needs at least 3 raters, so it was skipped."))
                    } else if (private$.n_cases < 5) {
                        private$.accumulateMessage(.("Rater clustering needs at least 5 cases, so it was skipped."))
                    } else {
                        if (!self$options$autoSelectGroups && self$options$nStyleGroups > private$.styleGroupCount()) {
                            private$.accumulateMessage(jmvcore::format(
                                .("{groups} style groups were requested for {raters} raters; {used} were used, the most possible without every rater forming its own group."),
                                groups = self$options$nStyleGroups, raters = private$.n_raters, used = private$.styleGroupCount()
                            ))
                        }
                        private$.checkpoint()
                        private$.performDiagnosticStyleAnalysis()
                        private$.checkpoint()
                        private$.performClusteringAnalysis()
                    }
                }

                # Handle frequency tables
                if (self$options$sft) {
                    private$.generateFrequencyTables()
                }

                # Generate clinical summaries after all analyses are complete, based on user options
                if (self$options$showClinicalSummary) {
                    private$.generateClinicalSummary()
                }

                # Generate educational content based on user options
                if (self$options$showAboutAnalysis) {
                    private$.generateAboutAnalysis()
                }

                if (self$options$showAssumptions) {
                    private$.generateAssumptions()
                }

                # Generate weighted kappa guide based on user options and weighting selection
                if (self$options$showWeightedKappaGuide && self$options$wght != "unweighted") {
                    private$.generateWeightedKappaGuide()
                }

                if (self$options$showStatisticalGlossary) {
                    private$.generateStatisticalGlossary()
                }

                # Generate inline statistical comments if requested
                if (self$options$showInlineComments) {
                    private$.generateInlineComments()
                }

                private$.storePlotStates()
                private$.renderMessages()
            },
            # The messages panel used to be written ONCE, at the very end of
            # .run(). Every message accumulated before one of the earlier
            # return()/jmvcore::reject() paths was therefore discarded, and the
            # user saw a blank panel instead of "reverted to unweighted kappa" or
            # "high missing data". Render on every append instead, so a message
            # survives whatever .run() does next. .resetMessages() clears the list
            # each run and the whole list is re-rendered, so this is idempotent
            # rather than accumulating duplicates across runs.
            #
            # Messages interpolate VARIABLE NAMES, which are user data; the old
            # writer pasted them into <li> unescaped.
            .accumulateMessage = function(msg, severity = "warning") {
                severity <- match.arg(severity, c("error", "strong_warning", "warning", "info"))
                private$.messages <- c(private$.messages, list(list(
                    content = as.character(msg),
                    severity = severity
                )))
                private$.renderMessages()
            },
            .renderMessages = function() {
                item <- self$results$warnings
                if (is.null(item)) {
                    return()
                }
                if (length(private$.messages) == 0) {
                    item$setContent("")
                    item$setVisible(FALSE)
                    return()
                }
                message_html <- vapply(private$.messages, function(entry) {
                    label <- switch(entry$severity,
                        error = .("Error"),
                        strong_warning = .("Strong warning"),
                        warning = .("Warning"),
                        info = .("Information")
                    )
                    paste0(
                        "<li data-severity='", entry$severity, "'><strong>",
                        label, ":</strong> ", jmvcore::htmlEscape(entry$content), "</li>"
                    )
                }, character(1))
                item$setContent(paste0(
                    "<div style='background-color: rgba(255, 169, 33, 0.14); border-left: 4px solid #f57c00; padding: 10px 14px; color: inherit;'>",
                    "<ul style='margin: 0; padding-left: 20px;'>",
                    paste0(message_html, collapse = ""),
                    "</ul></div>"
                ))
                item$setVisible(TRUE)
            },

            # Clear derived data before validation, including data used by renderers.
            .resetMessages = function() {
                for (name in private$.plotNames()) self$results[[name]]$setState(NULL)
                private$.restored_plot <- FALSE
                private$.plot_reference <- NULL
                private$.plot_case_labels <- NULL
                private$.distance_metric <- NULL
                private$.messages <- NULL
                private$.data_matrix <- NULL
                private$.available_data_matrix <- NULL
                private$.kripp_data_matrix <- NULL
                private$.categories <- NULL
                private$.n_cases <- 0
                private$.n_raters <- 0
                private$.rater_metadata <- NULL
                private$.retained_rows <- integer()
                private$.rater_names <- NULL
                private$.agreement_results <- NULL
                private$.pairwise_results <- NULL
                private$.category_results <- NULL
                private$.style_clustering_results <- NULL
                private$.renderMessages()
            },

            # Data preparation
            .prepareData = function() {
                # Get rater variables
                rater_vars <- self$options$vars
                private$.rater_names <- rater_vars

                # Extract data matrix
                data_subset <- self$data[rater_vars]
                retained_rows <- which(!private$.metadataRowFlags())

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
                private$.available_data_matrix <- data_subset
                complete_rows <- complete.cases(data_subset)
                kripp_rows <- rowSums(!is.na(data_subset)) >= 2
                private$.kripp_data_matrix <- data_subset[kripp_rows, , drop = FALSE]
                data_subset <- data_subset[complete_rows, , drop = FALSE]
                for (rater in rater_vars) {
                    attr(data_subset[[rater]], "label") <- attr(self$data[[rater]], "label")
                }
                private$.retained_rows <- retained_rows[complete_rows]
                final_n <- nrow(data_subset)

                if (original_n > 0) {
                    missing_prop <- (original_n - final_n) / original_n
                    if (missing_prop > 0.2) {
                        private$.accumulateMessage(
                            jmvcore::format(.("High missing data: {pct}% of cases were excluded from analyses that require jointly complete ratings. Pairwise Cohen analyses use cases observed for each pair, and Krippendorff's alpha uses cases with at least two observed ratings. Complete-case results may be biased."), pct = sprintf("%.1f", missing_prop * 100)),
                            severity = "strong_warning"
                        )
                    }
                }

                if (nrow(data_subset) == 0) {
                    kripp_requested <- self$options$kripp ||
                        self$options$multiraterMethod == "krippendorff"
                    pairwise_requested <- self$options$pairwiseAnalysis ||
                        self$options$multiraterMethod == "cohen"
                    pair_counts <- if (ncol(private$.available_data_matrix) >= 2) {
                        apply(utils::combn(seq_len(ncol(private$.available_data_matrix)), 2), 2, function(index) {
                            sum(stats::complete.cases(private$.available_data_matrix[, index, drop = FALSE]))
                        })
                    } else {
                        numeric()
                    }
                    kripp_available <- kripp_requested && nrow(private$.kripp_data_matrix) >= 2
                    pairwise_available <- pairwise_requested && any(pair_counts >= 2)
                    if (!kripp_available && !pairwise_available) {
                        jmvcore::reject(.("No jointly complete cases were found. Krippendorff's alpha or pairwise Cohen analysis requires at least two usable cases."))
                    }
                }

                private$.data_matrix <- data_subset
                private$.n_cases <- nrow(data_subset)
                private$.n_raters <- ncol(data_subset)

                # Keep category labels rather than the integer codes produced by
                # unlist() on factor columns. The labels are also kept in their
                # declared order for ordinal analyses.
                category_data <- if (nrow(data_subset) > 0) {
                    data_subset
                } else {
                    private$.kripp_data_matrix
                }
                private$.categories <- private$.categoryLabels(category_data)
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

                    case_values <- private$.rowLabels(private$.data_matrix, i)
                    if (length(unique(case_values)) == 1) {
                        agreement_count <- agreement_count + 1
                    }
                }

                overall_agreement <- (agreement_count / total_cases) * 100

                # Determine primary method
                primary_method <- if (self$options$multiraterMethod == "krippendorff") {
                    .("Krippendorff's Alpha")
                } else if (private$.n_raters == 2 || self$options$multiraterMethod == "cohen") {
                    .("Cohen's Kappa")
                } else if (self$options$exct) {
                    .("Conger's Kappa")
                } else {
                    .("Fleiss' Kappa")
                }

                self$results$overviewTable$setNote("complete", .("Complete agreement: cases on which every rater gave the same rating. Pairwise agreement (used by PABAK and the trend plot) is higher whenever there are more than 2 raters."))

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
                    category_values <- unlist(
                        lapply(private$.data_matrix, as.character),
                        use.names = FALSE
                    )
                    cat_counts <- table(category_values)
                    if (length(cat_counts) > 1) {
                        props <- prop.table(cat_counts)
                        if (max(props) > 0.8) {
                            private$.accumulateMessage(
                                .("Data are highly imbalanced (one category exceeds 80%). Kappa may be low despite high agreement (the kappa paradox); consider Gwet's AC1."),
                                severity = "strong_warning"
                            )
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
                kappa_table$setNote("kripp", NULL)
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
                    private$.accumulateMessage(.("Fleiss' kappa requires 3 or more raters. Falling back to Cohen's kappa (pairwise analysis recommended for detail)."))
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

            # Run Cohen's kappa without losing the declared order of an ordinal
            # factor. irr::kappa2() first converts a data frame to a character
            # matrix, then recreates factors alphabetically. Numeric codes based
            # on the shared declared level order preserve the intended weights.
            .cohenKappa = function(pair, wght = "unweighted") {
                input <- pair
                if (wght %in% c("equal", "squared")) {
                    labels <- private$.categoryLabels(pair)
                    input <- as.data.frame(lapply(pair, function(col) {
                        match(as.character(col), labels)
                    }))
                }
                irr::kappa2(input, weight = wght)
            },

            # Non-null asymptotic SE for Cohen's kappa.
            #
            # irr::kappa2()$statistic is the H0:kappa=0 test statistic, built
            # from the NULL SE, so the old se = kappa/z produced confidence
            # intervals that were too narrow (and blew up as kappa -> 0).
            # vcd::Kappa() returns the non-null ASE - the correct SE for a Wald
            # interval - and agrees with psych::cohen.kappa(). Returns NULL for
            # degenerate tables (e.g. perfect agreement), in which case no CI is
            # reported rather than a wrong one.
            #
            # Level order must come from the FACTOR LEVELS: vcd lays its
            # Equal-Spacing / Fleiss-Cohen weight matrix over the table in column
            # order, so an alphabetical order applies ordinal weights to a
            # scrambled scale (Low/Moderate/High sorts to High/Low/Moderate).
            .kappaASE = function(pair, wght) {
                a <- as.character(pair[[1]])
                b <- as.character(pair[[2]])
                lv <- private$.categoryLabels(pair)
                lv <- lv[lv %in% c(a, b)]
                if (length(lv) < 2) {
                    return(NULL)
                }
                tryCatch(
                    {
                        tab <- table(factor(a, levels = lv), factor(b, levels = lv))
                        vk <- if (identical(wght, "equal")) {
                            vcd::Kappa(tab, weights = "Equal-Spacing")
                        } else if (identical(wght, "squared")) {
                            vcd::Kappa(tab, weights = "Fleiss-Cohen")
                        } else {
                            vcd::Kappa(tab)
                        }
                        comp <- if (identical(wght, "unweighted")) vk$Unweighted else vk$Weighted
                        ase <- unname(comp[["ASE"]])
                        if (is.na(ase) || ase <= 0) NULL else ase
                    },
                    error = function(e) NULL
                )
            },

            # Cohen's kappa (2 raters)
            .performCohensKappa = function(table) {
                wght <- self$options$wght
                show_ci <- self$options$fleissCI
                conf_level <- 0.95

                # Check if weighting is appropriate
                # Handle weighted kappa with non-ordinal variables
                if (wght %in% c("equal", "squared")) {
                    if (!private$.commonOrdinalScale(private$.data_matrix)) {
                        # Instead of error, fallback to unweighted kappa with explanation
                        wght <- "unweighted"
                        private$.accumulateMessage(.("Weighted kappa requires ordered factors with the same category levels in the same order. Analysis reverted to unweighted kappa."))
                        interpretation_note <- .("Note: The ordered category scale was not consistent across raters, so unweighted kappa was used.")
                    } else {
                        interpretation_note <- ""
                    }
                } else {
                    interpretation_note <- ""
                }

                # Cohen's kappa is defined for exactly 2 raters and
                # irr::kappa2() hard-stops above that. When the user forces
                # Cohen with 3+ raters, report a row per rater pair (the old code
                # passed the whole matrix and crashed with irr's raw
                # "Number of raters exeeds 2" message).
                if (private$.n_raters > 2) {
                    return(private$.performPairwiseCohensKappa(table, wght, interpretation_note))
                }

                # Calculate Cohen's kappa
                result <- private$.cohenKappa(private$.data_matrix, wght)

                # Interpretation
                interpretation <- private$.interpretKappa(result$value)

                # Confidence interval from the NON-NULL asymptotic SE.
                se_value <- NA
                ci_lower <- NA
                ci_upper <- NA
                if (show_ci) {
                    ase <- private$.kappaASE(private$.data_matrix[, 1:2, drop = FALSE], wght)
                    if (!is.null(ase)) {
                        se_value <- ase
                        zc <- qnorm((1 + conf_level) / 2)
                        ci_lower <- max(-1, result$value - zc * se_value)
                        ci_upper <- min(1, result$value + zc * se_value)
                    }
                }

                # Add to table
                table$setRow(rowKey = "cohens", values = list(
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
                    interpretation = if (interpretation_note != "") paste(interpretation, interpretation_note, sep = ". ") else interpretation
                ))
            },

            # One Cohen's kappa row per rater pair, used when the user selects
            # Cohen with more than 2 raters.
            .performPairwiseCohensKappa = function(table, wght, interpretation_note) {
                nm <- private$.rater_names
                show_ci <- self$options$fleissCI
                conf_level <- 0.95
                zc <- qnorm((1 + conf_level) / 2)
                pair_sizes <- character()

                private$.accumulateMessage(.("Cohen's kappa is defined for 2 raters; one kappa is reported per rater pair. Use Fleiss' kappa or Krippendorff's alpha for a single overall coefficient."))

                for (i in seq_len(private$.n_raters - 1)) {
                    for (j in (i + 1):private$.n_raters) {
                        private$.checkpoint(flush = FALSE)
                        pair <- private$.available_data_matrix[, c(i, j), drop = FALSE]
                        pair <- pair[stats::complete.cases(pair), , drop = FALSE]
                        pair_label <- paste(nm[i], "vs", nm[j])
                        pair_sizes <- c(pair_sizes, paste0(pair_label, ": ", nrow(pair)))
                        if (nrow(pair) < 2) {
                            next
                        }
                        res <- tryCatch(private$.cohenKappa(pair, wght), error = function(e) NULL)
                        if (is.null(res)) {
                            next
                        }

                        se_value <- NA
                        ci_lower <- NA
                        ci_upper <- NA
                        if (show_ci) {
                            ase <- private$.kappaASE(pair, wght)
                            if (!is.null(ase)) {
                                se_value <- ase
                                ci_lower <- max(-1, res$value - zc * ase)
                                ci_upper <- min(1, res$value + zc * ase)
                            }
                        }

                        interpretation <- private$.interpretKappa(res$value)
                        table$setRow(
                            rowKey = paste0("cohens_", i, "_", j),
                            values = list(
                                method = jmvcore::format(
                                    .("Cohen's Kappa: {first} vs {second}"),
                                    first = nm[i], second = nm[j]
                                ),
                                kappa = res$value,
                                se = se_value,
                                ci_lower = ci_lower,
                                ci_upper = ci_upper,
                                z = res$statistic,
                                p = res$p.value,
                                interpretation = if (interpretation_note != "") paste(interpretation, interpretation_note, sep = ". ") else interpretation
                            )
                        )
                    }
                }
                table$setNote(
                    "pairwise_n",
                    jmvcore::format(
                        .("Pairwise available-case analysis used every case rated by both members of a pair. Pair-specific sample sizes: {counts}."),
                        counts = paste(pair_sizes, collapse = "; ")
                    )
                )
            },

            # Krippendorff's Alpha as primary method
            .performKrippendorffForKappa = function(table) {
                kripp_method <- self$options$krippMethod

                # This used to call krippendorff.alpha(), which is not exported by
                # any package the module imports (and is not in NAMESPACE), so the
                # try() ALWAYS failed and the row was silently published with
                # alpha = NA - i.e. selecting Krippendorff as the primary method
                # produced no result at all. The shared .krippendorffAlpha() helper
                # computes it with irr::kripp.alpha(); alpha has no closed-form
                # standard error, so the interval columns stay empty here.
                res <- tryCatch(private$.krippendorffAlpha(kripp_method), error = function(e) NULL)

                if (is.null(res)) {
                    private$.accumulateMessage(
                        .("Krippendorff's alpha could not be calculated for these data. Falling back to the kappa appropriate for the number of raters."),
                        severity = "strong_warning"
                    )
                    if (private$.n_raters == 2) {
                        return(private$.performCohensKappa(table))
                    }
                    return(private$.performFleissKappa(table))
                }

                private$.noteKrippDowngrade(res, kripp_method)
                alpha_value <- res$alpha
                se_value <- res$se
                zc <- qnorm(0.975)
                ci_lower <- if (is.na(se_value)) NA else max(-1, alpha_value - zc * se_value)
                ci_upper <- if (is.na(se_value)) NA else min(1, alpha_value + zc * se_value)
                z_value <- if (is.na(se_value) || se_value <= 0) NA else alpha_value / se_value

                table$setRow(rowKey = "krippendorff", values = list(
                    method = jmvcore::format(
                        .("Krippendorff's Alpha ({level})"),
                        level = stringr::str_to_title(kripp_method)
                    ),
                    kappa = alpha_value,
                    se = se_value,
                    ci_lower = ci_lower,
                    ci_upper = ci_upper,
                    z = z_value,
                    p = res$p,
                    interpretation = private$.interpretKrippendorff(alpha_value)
                ))
                table$setNote("kripp", paste(
                    private$.krippDataNote(),
                    .("Krippendorff's alpha has no closed-form standard error. For a confidence interval, enable Krippendorff's alpha with bootstrap confidence intervals."),
                    .("The 0.667 and 0.80 interpretation cutoffs are conventional reliability guidelines, not clinical acceptability thresholds. Choose the required reliability from the consequences of disagreement.")
                ))
            },

            # Fleiss' kappa (3+ raters)
            .performFleissKappa = function(table) {
                exact <- self$options$exct
                show_ci <- self$options$fleissCI
                conf_level <- 0.95

                # Fleiss' kappa has no weighted form. Selecting "Squared"/"Equal"
                # weights and 3+ raters used to silently produce an UNWEIGHTED
                # coefficient with no indication that the weight was dropped.
                if (self$options$wght != "unweighted") {
                    private$.accumulateMessage(.("Fleiss' kappa has no weighted form, so the selected kappa weights were not applied. For a weighted ordinal coefficient use Krippendorff's alpha (ordinal) or Gwet's AC2."))
                }

                # Calculate Fleiss' kappa
                result <- irr::kappam.fleiss(private$.data_matrix, exact = exact, detail = TRUE)

                # Interpretation
                interpretation <- private$.interpretKappa(result$value)

                # Calculate standard error and confidence interval
                se_value <- NA
                ci_lower <- NA
                ci_upper <- NA

                # Confidence interval from the NON-NULL asymptotic SE.
                # irr::kappam.fleiss()$statistic is the H0:kappa=0 z, so
                # se = kappa/z recovers the NULL SE and yields an interval that
                # is too narrow. irrCAC returns the published non-null SE for the
                # estimator actually reported: fleiss.kappa.raw() for Fleiss' kappa,
                # conger.kappa.raw() for Conger's kappa (exact = TRUE).
                if (show_ci) {
                    se_value <- tryCatch(
                        as.numeric((if (exact) irrCAC::conger.kappa.raw else irrCAC::fleiss.kappa.raw)(private$.data_matrix)$est$coeff.se),
                        error = function(e) NA_real_
                    )
                    if (!is.na(se_value) && se_value > 0 && !is.na(result$value)) {
                        zc <- qnorm((1 + conf_level) / 2)
                        ci_lower <- max(-1, result$value - zc * se_value)
                        ci_upper <- min(1, result$value + zc * se_value)
                    } else {
                        se_value <- NA
                    }
                }

                # Determine method name based on exact calculation
                method_name <- if (exact) {
                    # irr's exact = TRUE is Conger's (1980) kappa, a different estimator
                    .("Conger's Kappa (exact)")
                } else {
                    .("Fleiss' Kappa")
                }
                p_value <- if (length(result$p.value) == 1L) {
                    suppressWarnings(as.numeric(result$p.value))
                } else {
                    NA_real_
                }
                if (!is.finite(p_value)) p_value <- NA_real_

                # Add to table
                table$setRow(rowKey = "fleiss", values = list(
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
                    p = p_value,
                    interpretation = interpretation
                ))
            },

            # ICC analysis
            .performICCAnalysis = function() {
                icc_table <- self$results$iccTable
                dm <- private$.data_matrix

                # ICC needs an interval-like scale; it is meaningless for nominal
                # categories, so it runs only on ordered factors.
                if (!private$.commonOrdinalScale(dm)) {
                    icc_table$setNote("icc", .("ICC needs ordered factors with the same category levels in the same order, so it was not calculated. For nominal categories use Fleiss' kappa or Krippendorff's alpha."))
                    return()
                }

                # Two defects fixed here:
                # 1. apply() turned the factors into text and as.factor() re-sorted them
                #    ALPHABETICALLY, so Benign < Atypical < Malignant was scored as
                #    Atypical < Benign < Malignant (ICC 0.44 instead of 0.70 on test data).
                # 2. psych >= 2 names the result rows "Single_random_raters" etc., so
                #    results["ICC2", ] was NA and the table was always empty.
                codes <- vapply(dm, as.integer, integer(nrow(dm)))
                res <- tryCatch(
                    suppressMessages(psych::ICC(codes, alpha = 0.05, lmer = FALSE))$results,
                    error = function(e) e
                )
                row <- if (inherits(res, "error")) NULL else res[res$type == "ICC2", , drop = FALSE]
                if (is.null(row) || nrow(row) != 1) {
                    icc_table$setRow(rowKey = "ICC21", values = list(
                        type = .("ICC Error"),
                        icc_value = NaN, ci_lower = NaN, ci_upper = NaN, f_value = NaN, p = NaN,
                        interpretation = if (inherits(res, "error")) {
                            jmvcore::format(.("ICC calculation failed: {reason}"), reason = conditionMessage(res))
                        } else {
                            .("ICC calculation failed.")
                        }
                    ))
                    return()
                }

                icc_table$setRow(rowKey = "ICC21", values = list(
                    type = .("ICC(2,1): two-way random, absolute agreement, single rater"),
                    icc_value = row$ICC,
                    ci_lower = row[["lower bound"]],
                    ci_upper = row[["upper bound"]],
                    f_value = row$F,
                    p = row$p,
                    interpretation = private$.interpretICCValue(row$ICC)
                ))
                icc_table$setNote("icc", .("Ratings were coded 1, 2, 3, ... in the order of the factor levels; ICC treats adjacent categories as equally spaced."))
            },

            # Krippendorff's alpha analysis
            .performKrippendorffAnalysis = function() {
                kripp_table <- self$results$krippTable
                kripp_method <- self$options$krippMethod

                # This used irr::kripp.alpha() on category LABELS, which it coerces to
                # numbers ("NAs introduced by coercion") and orders alphabetically for
                # ordinal distances. The shared helper passes codes in factor-level order.
                res <- tryCatch(private$.krippendorffAlpha(kripp_method), error = function(e) e)
                if (inherits(res, "error") || !is.finite(res$alpha)) {
                    kripp_table$setRow(rowKey = "kripp", values = list(
                        data_type = kripp_method,
                        alpha = NaN,
                        interpretation = if (inherits(res, "error")) {
                            jmvcore::format(.("Krippendorff's alpha could not be calculated: {reason}"), reason = conditionMessage(res))
                        } else {
                            .("Krippendorff's alpha could not be calculated for these data.")
                        }
                    ))
                    return()
                }
                private$.noteKrippDowngrade(res, kripp_method)

                # "Bootstrap Confidence Intervals" used to compute nothing - it only
                # wrote NaN into the interval cells. Resample CASES (the unit of
                # analysis), recompute alpha on the full category set, and report the
                # selected interval. BCa uses leave-one-case-out estimates for its
                # acceleration correction and falls back to percentile when those
                # estimates are insufficient or degenerate.
                ci <- c(NA_real_, NA_real_)
                interval_method <- NULL
                if (self$options$bootstrap) {
                    withr::local_seed(self$options$seed) # reproducible; restores the RNG on exit
                    dm <- private$.kripp_data_matrix
                    labels <- private$.categoryLabels(dm)
                    n_boot <- self$options$bootstrapSamples
                    boots <- numeric(n_boot)
                    for (b in seq_len(n_boot)) {
                        if (b %% 50 == 0) {
                            private$.checkpoint(flush = FALSE)
                        }
                        idx <- sample.int(nrow(dm), nrow(dm), replace = TRUE)
                        boots[b] <- tryCatch(
                            private$.krippendorffAlpha(kripp_method, dm[idx, , drop = FALSE], labels)$alpha,
                            error = function(e) NA_real_
                        )
                    }
                    boots <- boots[is.finite(boots)]
                    jackknife <- if (self$options$bootstrapCIType == "bca") {
                        vapply(seq_len(nrow(dm)), function(i) {
                            if (i %% 25 == 1) private$.checkpoint(flush = FALSE)
                            tryCatch(
                                private$.krippendorffAlpha(
                                    kripp_method, dm[-i, , drop = FALSE], labels
                                )$alpha,
                                error = function(e) NA_real_
                            )
                        }, numeric(1))
                    } else {
                        numeric()
                    }
                    interval <- private$.bootstrapInterval(boots, res$alpha, jackknife)
                    ci <- interval$ci
                    interval_method <- interval$method
                    if (isTRUE(interval$fallback)) {
                        private$.accumulateMessage(
                            .("The BCa interval for Krippendorff's alpha could not be estimated from the leave-one-case-out values, so a percentile interval was reported.")
                        )
                    }
                    kripp_table$setNote("boot", jmvcore::format(
                        .("{data_note} 95% {method} bootstrap interval from {resamples} case resamples (seed {seed})."),
                        data_note = private$.krippDataNote(), method = interval_method,
                        resamples = n_boot, seed = self$options$seed
                    ))
                } else {
                    kripp_table$setNote("data", private$.krippDataNote())
                }

                kripp_table$setRow(rowKey = "kripp", values = list(
                    data_type = kripp_method,
                    alpha = res$alpha,
                    ci_lower = if (self$options$bootstrap) ci[1] else NULL,
                    ci_upper = if (self$options$bootstrap) ci[2] else NULL,
                    interpretation = private$.interpretKrippendorff(res$alpha)
                ))
                kripp_table$setNote(
                    "interpretation",
                    .("The 0.667 and 0.80 interpretation cutoffs are conventional reliability guidelines, not clinical acceptability thresholds. Choose the required reliability from the consequences of disagreement.")
                )
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
                # Only genuine threshold consensus counts; ties resolved by the
                # tie-breaking rule are reported separately.
                consensus_achieved <- sum(consensus_results$agreement_levels %in% c(.("Unanimous"), .("Super Majority"), .("Majority")))
                unanimous_cases <- sum(consensus_results$agreement_levels == .("Unanimous"), na.rm = TRUE)
                majority_cases <- sum(consensus_results$agreement_levels == .("Majority"), na.rm = TRUE)
                super_majority_cases <- sum(consensus_results$agreement_levels == .("Super Majority"), na.rm = TRUE)
                tied_cases <- sum(consensus_results$agreement_levels %in% c(.("Tie"), .("Tie: arbitration needed")))
                resolved_ties <- sum(consensus_results$agreement_levels == .("Tie resolved by overall most common rating"))
                no_consensus_cases <- total_cases - consensus_achieved - tied_cases - resolved_ties

                # Add summary rows
                summary_table$setRow(rowKey = "total", values = list(
                    metric = .("Total Cases"),
                    value = total_cases,
                    percentage = 100.0
                ))

                summary_table$setRow(rowKey = "consensus", values = list(
                    metric = .("Consensus Achieved"),
                    value = consensus_achieved,
                    percentage = round((consensus_achieved / total_cases) * 100, 1)
                ))

                summary_table$setRow(rowKey = "unanimous", values = list(
                    metric = .("Unanimous Agreement"),
                    value = unanimous_cases,
                    percentage = round((unanimous_cases / total_cases) * 100, 1)
                ))

                summary_table$setRow(rowKey = "super_majority", values = list(
                    metric = .("Super Majority (>=2/3)"),
                    value = super_majority_cases,
                    percentage = round((super_majority_cases / total_cases) * 100, 1)
                ))

                summary_table$setRow(rowKey = "majority", values = list(
                    metric = .("Majority (more than half)"),
                    value = majority_cases,
                    percentage = round((majority_cases / total_cases) * 100, 1)
                ))

                summary_table$setRow(rowKey = "tied", values = list(
                    metric = if (tie_breaking == "arbitration") .("Ties needing arbitration") else .("Tied Cases"),
                    value = tied_cases,
                    percentage = round((tied_cases / total_cases) * 100, 1)
                ))

                summary_table$setRow(rowKey = "resolved_ties", values = list(
                    metric = .("Ties resolved by overall most common rating"),
                    value = resolved_ties,
                    percentage = round((resolved_ties / total_cases) * 100, 1)
                ))

                summary_table$setRow(rowKey = "no_consensus", values = list(
                    metric = .("No Consensus"),
                    value = no_consensus_cases,
                    percentage = round((no_consensus_cases / total_cases) * 100, 1)
                ))

                # Populate detailed consensus table if requested
                if (show_consensus_table) {
                    consensus_table <- self$results$consensusTable

                    for (i in 1:private$.n_cases) {
                        case_id <- private$.caseLabels()[i]
                        consensus_score <- consensus_results$consensus_scores[i]
                        agreement_level <- consensus_results$agreement_levels[i]
                        n_agreeing <- consensus_results$n_agreeing[i]

                        # Format individual rater scores
                        case_scores <- private$.rowLabels(private$.data_matrix, i)
                        rater_scores_text <- paste(paste(private$.rater_names, case_scores, sep = ": "), collapse = ", ")

                        consensus_table$setRow(rowKey = i, values = list(
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
                    # strictly more than half: 2 of 4 raters is not a majority
                    "majority" = floor(n_raters / 2) + 1,
                    "super_majority" = ceiling(n_raters * 2 / 3),
                    "unanimous" = n_raters
                )

                all_scores <- unlist(lapply(data_matrix, as.character), use.names = FALSE)
                global_counts <- table(all_scores)
                global_mode <- names(global_counts)[which.max(global_counts)]

                for (i in 1:n_cases) {
                    case_scores <- private$.rowLabels(data_matrix, i)
                    score_counts <- table(case_scores)
                    max_count <- max(score_counts)
                    mode_scores <- names(score_counts)[score_counts == max_count]

                    # Consensus = one rating reaches the threshold. With a strict majority
                    # a tie can never reach it, so tie-breaking applies to TIED cases that
                    # miss consensus (it used to sit inside the threshold branch, where a
                    # 2-2 split among 4 raters counted as a "majority").
                    if (max_count >= threshold && length(mode_scores) == 1) {
                        consensus_scores[i] <- mode_scores[1]
                        n_agreeing[i] <- max_count
                        if (max_count == n_raters) {
                            agreement_levels[i] <- .("Unanimous")
                        } else if (max_count >= ceiling(n_raters * 2 / 3)) {
                            agreement_levels[i] <- .("Super Majority")
                        } else {
                            agreement_levels[i] <- .("Majority")
                        }
                    } else if (length(mode_scores) > 1) {
                        n_agreeing[i] <- max_count
                        if (tie_breaking == "arbitration") {
                            agreement_levels[i] <- .("Tie: arbitration needed")
                        } else if (tie_breaking == "global_mode" && global_mode %in% mode_scores) {
                            consensus_scores[i] <- global_mode
                            agreement_levels[i] <- .("Tie resolved by overall most common rating")
                        } else {
                            agreement_levels[i] <- .("Tie")
                        }
                    } else {
                        n_agreeing[i] <- max_count
                        agreement_levels[i] <- jmvcore::format(.("Insufficient ({agreeing}/{raters})"), agreeing = max_count, raters = n_raters)
                    }
                }

                return(list(
                    consensus_scores = consensus_scores,
                    agreement_levels = agreement_levels,
                    n_agreeing = n_agreeing
                ))
            },

            # Diagnostic style clustering analysis (Usubutun et al. 2012)
            .performDiagnosticStyleAnalysis = function() {
                style_table <- self$results$diagnosticStyleTable

                # Every table and plot must describe the same fitted clustering.
                clustering_result <- private$.performRaterClustering()
                private$.style_clustering_results <- clustering_result
                style_groups <- clustering_result$cluster_assignments

                # Populate style clustering results table
                rater_names <- private$.rater_names
                not_specified <- .("Not specified")
                for (i in seq_along(rater_names)) {
                    # Rater attributes come from META_ rows (private$.rater_metadata),
                    # keyed by rater name. The removed experienceVar/trainingVar/...
                    # options indexed a CASE column by RATER position, so rater 1 was
                    # given case row 1's value.
                    rater_meta <- vapply(c("experience", "training", "institution", "specialty"),
                        function(key) private$.raterMeta(key, rater_names[i]), character(1))
                    rater_meta[is.na(rater_meta)] <- not_specified
                    experience <- rater_meta[["experience"]]
                    training <- rater_meta[["training"]]
                    institution <- rater_meta[["institution"]]
                    specialty <- rater_meta[["specialty"]]

                    # Calculate agreement with other raters in same style group
                    same_group_raters <- which(style_groups == style_groups[i] & seq_along(rater_names) != i)
                    within_group_agreement <- if (length(same_group_raters) > 0) {
                        private$.calculateWithinGroupAgreement(i, same_group_raters)
                    } else {
                        100.0
                    }

                    style_table$setRow(rowKey = rater_names[i], values = list(
                        rater = rater_names[i],
                        style_group = jmvcore::format(
                            .("Style {group}"),
                            group = style_groups[i]
                        ),
                        within_group_agreement = within_group_agreement,
                        experience = experience,
                        training = training,
                        institution = institution,
                        specialty = specialty
                    ))
                }

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

                labels <- private$.categoryLabels(data_matrix)
                numeric_ratings <- lapply(data_matrix, function(x) match(as.character(x), labels))
                ordered_scale <- all(vapply(data_matrix, is.ordered, logical(1))) &&
                    all(vapply(data_matrix, function(x) identical(levels(x), levels(data_matrix[[1]])), logical(1)))
                if (distance_metric != "agreement" && !ordered_scale) {
                    private$.accumulateMessage(.("Numeric clustering distances require ordered factors with the same category order. Percentage disagreement distance was used for these ratings."))
                    distance_metric <- "agreement"
                }
                if (distance_metric == "correlation" &&
                    any(vapply(numeric_ratings, function(x) stats::sd(x) == 0, logical(1)))) {
                    private$.accumulateMessage(.("Correlation distance requires variation in every rater's ratings. Percentage disagreement distance was used because at least one rater is constant."))
                    distance_metric <- "agreement"
                }
                private$.distance_metric <- distance_metric

                # Initialize distance matrix
                dist_matrix <- matrix(0, nrow = n_raters, ncol = n_raters)
                rownames(dist_matrix) <- private$.rater_names
                colnames(dist_matrix) <- private$.rater_names

                # Calculate pairwise distances
                for (i in 1:(n_raters - 1)) {
                    for (j in (i + 1):n_raters) {
                        if (distance_metric == "agreement") {
                            # Percentage disagreement (1 - percentage agreement)
                            agreement_count <- sum(as.character(data_matrix[, i]) == as.character(data_matrix[, j]))
                            agreement_percent <- agreement_count / n_cases
                            distance <- 1 - agreement_percent
                        } else if (distance_metric == "correlation") {
                            # Convert to numeric for correlation
                            rater_i <- numeric_ratings[[i]]
                            rater_j <- numeric_ratings[[j]]
                            correlation <- stats::cor(rater_i, rater_j, method = "spearman")
                            distance <- max(0, 1 - correlation)
                        } else { # euclidean
                            # Euclidean distance on numeric coding
                            rater_i <- numeric_ratings[[i]]
                            rater_j <- numeric_ratings[[j]]
                            distance <- sqrt(sum((rater_i - rater_j)^2))
                        }

                        dist_matrix[i, j] <- distance
                        dist_matrix[j, i] <- distance
                    }
                }

                return(as.dist(dist_matrix))
            },

            # Calculate within-group agreement for a rater
            .calculateWithinGroupAgreement = function(rater_index, same_group_indices) {
                if (length(same_group_indices) == 0) {
                    return(100.0)
                }

                data_matrix <- private$.data_matrix
                rater_data <- data_matrix[, rater_index]

                # Calculate average agreement with other raters in same group
                total_agreement <- 0
                n_comparisons <- 0

                for (other_index in same_group_indices) {
                    other_data <- data_matrix[, other_index]
                    agreement_count <- sum(as.character(rater_data) == as.character(other_data))
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

                                agreement_count <- sum(as.character(private$.data_matrix[, rater_i]) == as.character(private$.data_matrix[, rater_j]))
                                agreement_percent <- (agreement_count / private$.n_cases) * 100
                                total_agreement <- total_agreement + agreement_percent
                                n_pairs <- n_pairs + 1
                            }
                        }

                        avg_agreement <- if (n_pairs > 0) total_agreement / n_pairs else 100.0
                    } else {
                        avg_agreement <- 100.0 # Single member group
                    }

                    # Identify predominant characteristics
                    group_experience <- .("Mixed")
                    group_training <- .("Mixed")
                    group_institution <- .("Mixed")

                    if (self$options$raterCharacteristics) {
                        members <- private$.rater_names[group_members]
                        shared_value <- function(key, fallback) {
                            vals <- stats::na.omit(private$.raterMeta(key, members))
                            if (length(vals) > 0 && length(unique(vals)) == 1) vals[[1]] else fallback
                        }
                        group_experience <- shared_value("experience", group_experience)
                        group_training <- shared_value("training", group_training)
                        group_institution <- shared_value("institution", group_institution)
                    }

                    style_summary_table$addRow(
                        rowKey = jmvcore::format(.("Style {group}"), group = group),
                        values = list(
                            style_group = jmvcore::format(.("Style {group}"), group = group),
                            n_members = n_members,
                            members = member_names,
                            avg_within_agreement = avg_agreement,
                            predominant_experience = group_experience,
                            predominant_training = group_training,
                            predominant_institution = group_institution
                        )
                    )
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
                    case_diagnoses <- private$.rowLabels(data_matrix, case_idx)

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
                threshold <- quantile(case_discord_scores, 0.8) # Top 20%
                discordant_indices <- which(case_discord_scores >= threshold & case_discord_scores > 0)

                for (idx in discordant_indices) {
                    case_id <- private$.caseLabels()[idx]

                    case_diagnoses <- private$.rowLabels(data_matrix, idx)

                    # Show diagnosis by style group
                    group_diagnoses_str <- character()
                    for (group in unique_groups) {
                        group_members <- which(style_groups == group)
                        group_diag <- case_diagnoses[group_members]
                        group_mode <- names(sort(table(group_diag), decreasing = TRUE))[1]
                        group_diagnoses_str <- c(
                            group_diagnoses_str,
                            jmvcore::format(
                                .("Style {group}: {rating}"),
                                group = group,
                                rating = group_mode
                            )
                        )
                    }

                    discordant_table$addRow(rowKey = idx, values = list(
                        case_id = case_id,
                        discord_score = case_discord_scores[idx],
                        style_group_diagnoses = paste(group_diagnoses_str, collapse = "; "),
                        case_interpretation = .("High inter-style disagreement")
                    ))
                }
            },

            # Generate frequency tables
            .generateFrequencyTables = function() {
                data_matrix <- private$.available_data_matrix
                rater_names <- private$.rater_names

                # Populate individual rater frequency table
                freq_table <- self$results$raterFrequencyTables$frequencyTable

                for (i in seq_along(rater_names)) {
                    # Checkpoint before processing each rater
                    private$.checkpoint(flush = FALSE)

                    rater_data <- data_matrix[, i]
                    freq_counts <- table(rater_data)

                    for (j in seq_along(freq_counts)) {
                        category <- names(freq_counts)[j]
                        frequency <- as.numeric(freq_counts[j])
                        percentage <- round(frequency / sum(freq_counts) * 100, 1)

                        row_key <- paste(i, j, sep = "_")
                        freq_table$setRow(rowKey = row_key, values = list(
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
                    private$.generateCrosstabTable()
                }
                freq_table$setNote(
                    "denominator",
                    .("Each rater's percentages use that rater's non-missing ratings as the denominator.")
                )
            },

            # Generate cross-tabulation table for 2 raters
            .generateCrosstabTable = function() {
                if (private$.n_raters != 2) {
                    return()
                }

                data_matrix <- private$.available_data_matrix
                rater_names <- private$.rater_names
                cross_table <- table(data_matrix[, 1], data_matrix[, 2])

                crosstab_table <- self$results$crosstabTable
                for (i in seq_len(nrow(cross_table))) {
                    rater1_cat <- rownames(cross_table)[i]

                    # Create a text representation of the row
                    row_values <- paste(colnames(cross_table), cross_table[i, ], sep = ": ", collapse = ", ")

                    crosstab_table$setRow(rowKey = rater1_cat, values = list(
                        rater1_category = paste0(rater_names[1], ": ", rater1_cat),
                        frequencies = row_values
                    ))
                }
            },

            # Helper functions
            .interpretKappa = function(kappa) {
                if (is.na(kappa)) {
                    return(.("Cannot calculate"))
                }
                if (kappa < 0) {
                    return(.("Poor"))
                }
                if (kappa <= 0.20) {
                    return(.("Slight"))
                }
                if (kappa <= 0.40) {
                    return(.("Fair"))
                }
                if (kappa <= 0.60) {
                    return(.("Moderate"))
                }
                if (kappa <= 0.80) {
                    return(.("Substantial"))
                }
                return(.("Almost Perfect"))
            },
            .interpretKrippendorff = function(alpha) {
                if (is.na(alpha) || is.nan(alpha)) {
                    return(.("Cannot calculate"))
                }
                if (alpha < 0.667) {
                    return(.("Below the conventional 0.667 minimum for tentative conclusions"))
                }
                if (alpha < 0.80) {
                    return(.("Tentative conclusions only (0.667 to below 0.80)"))
                }
                return(.("Meets the conventional 0.80 reliability threshold"))
            },
            # One case's ratings as LABELS. as.character(<data.frame>[i, ]) returns
            # factor level CODES, so raters coding Benign/EIN in different level
            # orders both read "1" and a disagreement was counted as agreement; it
            # also made "global_mode" tie-breaking compare codes with labels and
            # never resolve anything.
            .rowLabels = function(df, i) {
                vapply(df[i, , drop = FALSE], as.character, character(1))
            },

            # META_ rows (case ID "META_experience", ...) carry rater attributes.
            .metadataRowFlags = function() {
                id <- self$options$caseID
                if (!isTRUE(self$options$useMetadataRows) || is.null(id) || !id %in% names(self$data)) {
                    return(rep(FALSE, nrow(self$data)))
                }
                grepl("^META_", as.character(self$data[[id]]), ignore.case = TRUE)
            },

            # Drop META_ rows from a frame row-aligned with self$data. Their values
            # ("25", "GYN") became levels of every rater factor, so categories such
            # as "25" were analysed and weighted-kappa spacing changed. Only levels
            # that occur SOLELY in META_ rows are removed - a declared but unused
            # category must survive because it changes weighted kappa.
            .dropMetadataRows = function(df) {
                meta <- private$.metadataRowFlags()
                if (!any(meta)) {
                    return(df)
                }
                out <- df[!meta, , drop = FALSE]
                metadata_values <- unique(stats::na.omit(unlist(lapply(
                    df[meta, , drop = FALSE], as.character
                ), use.names = FALSE)))
                for (col in names(out)) {
                    if (!is.factor(out[[col]])) {
                        next
                    }
                    meta_only <- setdiff(metadata_values, unique(as.character(out[[col]])))
                    if (length(meta_only) > 0) {
                        out[[col]] <- factor(
                            as.character(out[[col]]),
                            levels = setdiff(levels(out[[col]]), meta_only),
                            ordered = is.ordered(out[[col]])
                        )
                    }
                }
                out
            },

            .raterMeta = function(key, raters) {
                v <- private$.rater_metadata[[key]]
                if (!isTRUE(self$options$raterCharacteristics) || is.null(v)) {
                    return(rep(NA_character_, length(raters)))
                }
                unname(as.character(v[raters]))
            },

            # cutree() needs k <= n raters, and cluster::silhouette() returns a bare
            # NA (not a matrix) when every cluster is a singleton, i.e. k == n. With
            # the default of 3 groups that crashed every 3-rater study. n - 1 is the
            # largest meaningful partition.
            .styleGroupCount = function() {
                min(self$options$nStyleGroups, private$.n_raters - 1)
            },

            # Category order for irrCAC's ordinal weights.
            #
            # irrCAC sorts categ.labels alphabetically when none are supplied, so
            # Benign < Atypical < Malignant was weighted as Atypical < Benign <
            # Malignant. On a test set ordinal Krippendorff's alpha read 0.595
            # instead of 0.042 and Gwet's AC2 0.562 instead of 0.123. The factor
            # levels carry the real order.
            .categoryLabels = function(dm = private$.data_matrix) {
                lv <- unique(unlist(lapply(dm, function(col) {
                    if (is.factor(col)) levels(col) else sort(unique(as.character(col)))
                }), use.names = FALSE))
                used <- unique(stats::na.omit(unlist(lapply(dm, as.character), use.names = FALSE)))
                lv[lv %in% used]
            },

            .commonOrdinalScale = function(dm = private$.data_matrix) {
                if (ncol(dm) == 0 || !all(vapply(dm, is.ordered, logical(1)))) {
                    return(FALSE)
                }
                private$.commonCategoryOrder(dm)
            },

            .commonCategoryOrder = function(dm = private$.data_matrix) {
                if (ncol(dm) == 0 || !all(vapply(dm, is.factor, logical(1)))) {
                    return(FALSE)
                }
                reference <- levels(dm[[1]])
                all(vapply(dm, function(col) identical(levels(col), reference), logical(1)))
            },

            # Krippendorff's alpha, shared by the primary-method row and the
            # dedicated table, via irr::kripp.alpha() on a raters x units NUMERIC
            # matrix. Checked against Krippendorff's (2011) published worked example
            # (nominal .743, ordinal .815, interval .849, ratio .797): irr matches all
            # four. irrCAC's krippen.alpha.raw() does not - its "ordinal"/"linear"
            # weights are Gwet's, giving .834 and .800 - so it is not used here.
            # Codes follow the factor-level order, which ordinal distances depend on;
            # interval and ratio use the category values and need numeric categories.
            .krippendorffAlpha = function(level, dm = private$.kripp_data_matrix, labels = private$.categoryLabels(dm)) {
                numeric_labels <- length(labels) > 0 && !anyNA(suppressWarnings(as.numeric(labels)))
                method <- if (level %in% c("nominal", "ordinal", "interval", "ratio")) level else "nominal"
                if (method == "ordinal" && !private$.commonCategoryOrder(dm)) {
                    stop(.("Ordinal Krippendorff's alpha requires factors with the same category levels in the same order."))
                }
                if (method %in% c("interval", "ratio") && !numeric_labels) {
                    method <- "ordinal"
                }
                to_number <- if (method %in% c("interval", "ratio")) {
                    function(col) as.numeric(as.character(col))
                } else {
                    function(col) as.numeric(match(as.character(col), labels))
                }
                # matrix(..., nrow) keeps the orientation when dm has a single row
                m <- t(matrix(vapply(dm, to_number, numeric(nrow(dm))), nrow = nrow(dm)))
                list(
                    alpha = as.numeric(irr::kripp.alpha(m, method = method)$value),
                    se = NA_real_,
                    p = NA_real_,
                    downgraded = level %in% c("interval", "ratio") && !numeric_labels
                )
            },

            .krippDataNote = function() {
                dm <- private$.kripp_data_matrix
                partial <- sum(!stats::complete.cases(dm))
                jmvcore::format(
                    .("Krippendorff's alpha used {cases} cases with at least two observed ratings, including {partial} partially rated cases."),
                    cases = nrow(dm), partial = partial
                )
            },

            .bootstrapInterval = function(bootstrap_values, original, jackknife_values) {
                interval <- .pathagreementBootstrapInterval(
                    bootstrap_values, original, jackknife_values,
                    method = self$options$bootstrapCIType
                )
                interval$method <- if (interval$method == "BCa") .("BCa") else .("percentile")
                interval
            },

            .noteKrippDowngrade = function(res, level) {
                if (isTRUE(res$downgraded)) {
                    private$.accumulateMessage(jmvcore::format(
                        .("{scale} Krippendorff's alpha needs numeric categories; these categories are labels, so ordinal distances were used."),
                        scale = stringr::str_to_title(level)
                    ))
                }
            },

            # Rows kept by .prepareData(), used to line a reference-standard or
            # case-ID column up with .data_matrix after complete-case filtering.
            .keptRows = function() {
                private$.retained_rows
            },

            .caseLabels = function() {
                if (private$.restored_plot) return(private$.plot_case_labels)
                rows <- private$.keptRows()
                labels <- vapply(
                    rows,
                    function(row) jmvcore::format(.("Case {row}"), row = row),
                    character(1)
                )
                id <- self$options$caseID
                if (!is.null(id) && length(id) > 0 && id %in% names(self$data)) {
                    supplied <- as.character(self$data[[id]][rows])
                    present <- !is.na(supplied) & nzchar(supplied)
                    labels[present] <- supplied[present]
                }
                labels
            },

            # Cohen's kappa for every rater pair.
            .pairwiseKappaResults = function() {
                dm <- private$.available_data_matrix
                nm <- private$.rater_names
                n <- private$.n_raters
                wght <- self$options$wght
                if (!private$.commonOrdinalScale(dm)) {
                    wght <- "unweighted"
                }
                zc <- qnorm(0.975)

                out <- list()
                for (i in seq_len(n - 1)) {
                    for (j in (i + 1):n) {
                        pair <- dm[, c(i, j), drop = FALSE]
                        pair <- pair[stats::complete.cases(pair), , drop = FALSE]
                        if (nrow(pair) < 2) {
                            next
                        }
                        agreement_percent <- mean(as.character(pair[[1]]) == as.character(pair[[2]])) * 100
                        res <- tryCatch(private$.cohenKappa(pair, wght), error = function(e) NULL)
                        if (is.null(res)) {
                            next
                        }
                        ase <- private$.kappaASE(pair, wght)
                        out[[length(out) + 1]] <- data.frame(
                            pair = paste(nm[i], "vs", nm[j]),
                            cases = nrow(pair),
                            i = i, j = j,
                            agreement_percent = agreement_percent,
                            kappa = res$value,
                            se = if (is.null(ase)) NA_real_ else ase,
                            ci_lower = if (is.null(ase)) NA_real_ else max(-1, res$value - zc * ase),
                            ci_upper = if (is.null(ase)) NA_real_ else min(1, res$value + zc * ase),
                            p = res$p.value,
                            interpretation = private$.interpretKappa(res$value),
                            stringsAsFactors = FALSE
                        )
                    }
                }
                if (length(out) == 0) {
                    return(NULL)
                }
                do.call(rbind, out)
            },

            .performPairwiseAnalysis = function() {
                tbl <- self$results$pairwiseTable
                res <- private$.pairwiseKappaResults()
                if (is.null(res)) {
                    return()
                }
                for (r in seq_len(nrow(res))) {
                    tbl$setRow(rowKey = res$pair[r], values = list(
                        rater_pair = res$pair[r],
                        agreement_percent = res$agreement_percent[r],
                        kappa = res$kappa[r],
                        ci_lower = res$ci_lower[r],
                        ci_upper = res$ci_upper[r],
                        p = res$p[r],
                        interpretation = res$interpretation[r]
                    ))
                }
                tbl$setNote("ci", .("Confidence intervals use the non-null asymptotic standard error (vcd::Kappa), which is the appropriate standard error for an interval; a blank interval means the rating table was degenerate."))
                tbl$setNote("p", .("Each p-value tests kappa = 0 for one pair and is not adjusted for multiple comparisons; judge pairs by their kappa and confidence interval."))
                tbl$setNote(
                    "pairwise_n",
                    jmvcore::format(
                        .("Pairwise available-case analysis used every case rated by both members of a pair. Pair-specific sample sizes: {counts}."),
                        counts = paste(paste0(res$pair, ": ", res$cases), collapse = "; ")
                    )
                )
            },

            # One-vs-rest agreement for each rating category.
            #
            # Frequency is the number of CASES in which at least one rater used
            # the category. Category kappa collapses the ratings to
            # "this category / not this category" and re-runs the estimator
            # appropriate for the rater count.
            .categoryAgreementResults = function() {
                dm <- private$.data_matrix
                cats <- private$.categoryLabels(dm)
                chr <- as.data.frame(lapply(dm, as.character), stringsAsFactors = FALSE)
                gold <- private$.referenceStandardValues()

                out <- list()
                for (cat in cats) {
                    hit <- as.data.frame(lapply(chr, function(col) col == cat))
                    frequency <- sum(apply(hit, 1, any))
                    agreement_percent <- mean(apply(hit, 1, function(r) length(unique(r)) == 1)) * 100

                    bin <- as.data.frame(lapply(hit, function(col) factor(ifelse(col, "yes", "no"), levels = c("no", "yes"))))
                    kappa <- tryCatch(
                        if (ncol(bin) == 2) irr::kappa2(bin)$value else irr::kappam.fleiss(bin)$value,
                        error = function(e) NA_real_
                    )

                    sens <- NA_real_
                    spec <- NA_real_
                    if (!is.null(gold)) {
                        observed <- !is.na(gold)
                        gpos <- observed & as.character(gold) == cat
                        gneg <- observed & as.character(gold) != cat
                        # Averaged over raters: each rater is scored against the
                        # reference standard for this one category.
                        sens <- mean(vapply(chr, function(col) {
                            if (sum(gpos) == 0) NA_real_ else mean(col[gpos] == cat)
                        }, numeric(1)), na.rm = TRUE)
                        spec <- mean(vapply(chr, function(col) {
                            if (sum(gneg) == 0) NA_real_ else mean(col[gneg] != cat)
                        }, numeric(1)), na.rm = TRUE)
                    }

                    out[[length(out) + 1]] <- data.frame(
                        category = cat, frequency = frequency,
                        agreement_percent = agreement_percent, kappa = kappa,
                        sensitivity = sens, specificity = spec,
                        stringsAsFactors = FALSE
                    )
                }
                if (length(out) == 0) {
                    return(NULL)
                }
                do.call(rbind, out)
            },

            .performCategoryAnalysis = function() {
                tbl <- self$results$categoryTable
                res <- private$.categoryAgreementResults()
                if (is.null(res)) {
                    return()
                }
                for (r in seq_len(nrow(res))) {
                    tbl$setRow(rowKey = res$category[r], values = list(
                        category = res$category[r],
                        frequency = res$frequency[r],
                        agreement_percent = res$agreement_percent[r],
                        kappa = res$kappa[r],
                        sensitivity = res$sensitivity[r],
                        specificity = res$specificity[r]
                    ))
                }
                note <- .("Category kappa collapses the ratings to \u{201C}this category vs. all others\u{201D}. Frequency counts cases in which at least one rater used the category.")
                if (all(is.na(res$sensitivity))) {
                    note <- paste0(note, "\n\n", .("Sensitivity and specificity require a reference/expert standard variable."))
                }
                tbl$setNote("cat", note)
            },

            # Reference standard aligned to the rows kept in .data_matrix.
            .referenceStandardValues = function() {
                if (private$.restored_plot) return(private$.plot_reference)
                var <- self$options$referenceStandard
                if (is.null(var) || length(var) == 0 || !var %in% names(self$data)) {
                    return(NULL)
                }
                rows <- private$.keptRows()
                if (anyNA(rows) || length(rows) != nrow(private$.data_matrix)) {
                    return(NULL)
                }
                self$data[[var]][rows]
            },

            # Cases the raters disagree on most.
            .outlierCaseResults = function() {
                dm <- private$.data_matrix
                n_raters <- private$.n_raters
                chr <- t(apply(dm, 1, as.character))

                disagreement <- apply(chr, 1, function(vals) {
                    counts <- table(vals)
                    n_raters - max(counts)
                })
                consensus <- apply(chr, 1, function(vals) {
                    counts <- sort(table(vals), decreasing = TRUE)
                    if (length(counts) > 1 && counts[[1]] == counts[[2]]) .("Tied") else names(counts)[1]
                })

                keep <- which(disagreement > 0)
                if (length(keep) == 0) {
                    return(NULL)
                }
                # Worst cases first, capped so a large study does not publish one
                # row per case.
                keep <- keep[order(-disagreement[keep])]
                keep <- utils::head(keep, 50L)

                ids <- private$.caseLabels()

                data.frame(
                    case_id = ids[keep],
                    disagreement_count = as.integer(disagreement[keep]),
                    agreement_score = 1 - (disagreement[keep] / n_raters),
                    rater_assignments = apply(chr[keep, , drop = FALSE], 1, paste, collapse = ", "),
                    consensus_diagnosis = consensus[keep],
                    stringsAsFactors = FALSE
                )
            },

            .performOutlierAnalysis = function() {
                tbl <- self$results$outlierTable
                tbl$deleteRows()
                res <- private$.outlierCaseResults()
                if (is.null(res)) {
                    private$.accumulateMessage(
                        .("All raters agreed on every case, so there are no discordant cases to list."),
                        severity = "info"
                    )
                    return()
                }
                for (r in seq_len(nrow(res))) {
                    tbl$addRow(rowKey = r, values = as.list(res[r, ]))
                }
                tbl$setNote("outlier", .("Cases with at least one dissenting rater, worst first (at most 50 shown). Agreement score is the proportion of raters who chose the modal rating."))
            },

            # Per-rater accuracy against the reference/expert standard.
            #
            # Sensitivity/specificity/PPV/NPV are only defined per class on a
            # multi-category scale, so they are MACRO-AVERAGED over the
            # categories present in the reference standard (each category scored
            # one-vs-rest, then averaged unweighted). The table carries a note
            # saying so - reporting a single unqualified number would imply a
            # binary test.
            .referenceAccuracyResults = function() {
                gold <- private$.referenceStandardValues()
                if (is.null(gold)) {
                    return(NULL)
                }
                dm <- private$.data_matrix
                nm <- private$.rater_names
                gold_chr <- as.character(gold)
                ok <- !is.na(gold_chr)
                if (sum(ok) < 2) {
                    return(NULL)
                }
                cats <- sort(unique(gold_chr[ok]))

                out <- list()
                for (i in seq_along(nm)) {
                    private$.checkpoint(flush = FALSE)
                    rater <- as.character(dm[[i]])[ok]
                    g <- gold_chr[ok]

                    accuracy <- mean(rater == g) * 100
                    per_cat <- vapply(cats, function(cat) {
                        tp <- sum(rater == cat & g == cat)
                        fp <- sum(rater == cat & g != cat)
                        fn <- sum(rater != cat & g == cat)
                        tn <- sum(rater != cat & g != cat)
                        c(
                            sens = if ((tp + fn) == 0) NA_real_ else tp / (tp + fn),
                            spec = if ((tn + fp) == 0) NA_real_ else tn / (tn + fp),
                            ppv  = if ((tp + fp) == 0) NA_real_ else tp / (tp + fp),
                            npv  = if ((tn + fn) == 0) NA_real_ else tn / (tn + fn)
                        )
                    }, numeric(4))

                    lv <- union(levels(as.factor(g)), levels(as.factor(rater)))
                    kappa_vs_gold <- tryCatch(
                        irr::kappa2(data.frame(
                            gold = factor(g, levels = lv),
                            rater = factor(rater, levels = lv)
                        ))$value,
                        error = function(e) NA_real_
                    )

                    out[[length(out) + 1]] <- data.frame(
                        rater = nm[i],
                        accuracy = accuracy,
                        sensitivity = mean(per_cat["sens", ], na.rm = TRUE) * 100,
                        specificity = mean(per_cat["spec", ], na.rm = TRUE) * 100,
                        ppv = mean(per_cat["ppv", ], na.rm = TRUE) * 100,
                        npv = mean(per_cat["npv", ], na.rm = TRUE) * 100,
                        kappa_vs_gold = kappa_vs_gold,
                        stringsAsFactors = FALSE
                    )
                }
                do.call(rbind, out)
            },

            .performPathologyAnalysis = function() {
                tbl <- self$results$diagnosticAccuracyTable
                res <- private$.referenceAccuracyResults()
                if (is.null(res)) {
                    private$.accumulateMessage(
                        .("Diagnostic accuracy metrics need a reference/expert standard variable. Assign one to see per-rater accuracy and the confusion matrix."),
                        severity = "info"
                    )
                    return()
                }
                for (r in seq_len(nrow(res))) {
                    tbl$setRow(rowKey = res$rater[r], values = as.list(res[r, ]))
                }
                tbl$setNote("macro", .("Sensitivity, specificity, PPV and NPV are macro-averaged over the categories of the reference standard (each scored one-vs-rest, then averaged). Accuracy is the percentage of cases the rater classified exactly as the reference standard."))
            },

            # Visualization functions
            .heatmapPlot = function(image, ggtheme, theme, ...) {
                if (!private$.restorePlotState(image)) return(FALSE)
                if (is.null(private$.n_raters) || length(private$.n_raters) == 0 || private$.n_raters < 2) {
                    return()
                }

                # Create pairwise agreement matrix
                n_raters <- private$.n_raters
                rater_names <- private$.rater_names
                agreement_matrix <- matrix(1, nrow = n_raters, ncol = n_raters)
                rownames(agreement_matrix) <- rater_names
                colnames(agreement_matrix) <- rater_names

                # Weighted kappa only for ordinal ratings, as in the kappa table; on
                # nominal categories the weights imposed an order that does not exist.
                heatmap_weight <- if (private$.commonOrdinalScale(private$.available_data_matrix)) self$options$wght else "unweighted"

                # Fill matrix with pairwise kappa values
                for (i in 1:(n_raters - 1)) {
                    for (j in (i + 1):n_raters) {
                        pair_data <- private$.available_data_matrix[, c(i, j), drop = FALSE]
                        pair_data <- pair_data[stats::complete.cases(pair_data), , drop = FALSE]
                        pair_kappa <- if (nrow(pair_data) < 2) {
                            NA_real_
                        } else {
                            result <- tryCatch(
                                private$.cohenKappa(pair_data, heatmap_weight),
                                error = function(e) NULL
                            )
                            if (is.null(result)) NA_real_ else result$value
                        }
                        agreement_matrix[i, j] <- pair_kappa
                        agreement_matrix[j, i] <- pair_kappa
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
                    list(low = "red", mid = "yellow", high = "green") # default
                )

                # Create heatmap
                p <- ggplot(melted_matrix, aes(Var1, Var2, fill = value)) +
                    geom_tile()

                # Add text labels if heatmapDetails is enabled
                if (self$options$heatmapDetails) {
                    p <- p + geom_text(aes(label = round(value, 3)), color = "black", size = 3)
                }

                # Apply color scale based on theme
                if (self$options$heatmapTheme %in% c("viridis", "plasma", "cividis")) {
                    p <- p + scale_fill_viridis_c(option = self$options$heatmapTheme, name = .("Kappa"))
                } else {
                    p <- p + scale_fill_gradient2(
                        low = theme_colors$low, mid = theme_colors$mid,
                        high = theme_colors$high, midpoint = 0.5, name = .("Kappa")
                    )
                }

                p <- p +
                    labs(
                        title = .("Rater Agreement Heatmap"),
                        x = .("Rater"), y = .("Rater")
                    ) +
                    theme_minimal() +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1))

                print(p)
                TRUE
            },

            # Landis & Koch bands, used to colour the agreement plots
            # consistently with .interpretKappa().
            .kappaBands = function() {
                list(
                    breaks = c(-1, 0.20, 0.40, 0.60, 0.80, 1),
                    labels = c(.("Slight"), .("Fair"), .("Moderate"), .("Substantial"), .("Almost perfect")),
                    colors = c("#d73027", "#fc8d59", "#fee090", "#91bfdb", "#4575b4")
                )
            },
            .kappaBandFactor = function(x) {
                b <- private$.kappaBands()
                cut(x, breaks = b$breaks, labels = b$labels, include.lowest = TRUE)
            },
            # Every renderer runs on resize and on .omv reopen, including after
            # .run() returned early - so each one re-checks its option and its
            # data instead of trusting private state to be populated.
            .plotUnavailable = function(msg) {
                p <- ggplot2::ggplot() +
                    ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5, label = msg), size = 5) +
                    ggplot2::xlim(0, 1) +
                    ggplot2::ylim(0, 1) +
                    ggplot2::theme_void()
                print(p)
                TRUE
            },

            # Forest plot of Cohen's kappa for every rater pair.
            .pairwisePlot = function(image, ggtheme, theme, ...) {
                if (!private$.restorePlotState(image)) return(FALSE)
                if (!self$options$pairwiseAnalysis || is.null(private$.data_matrix)) {
                    return(FALSE)
                }
                if (is.null(private$.n_raters) || private$.n_raters < 2) {
                    return(private$.plotUnavailable(.("Select at least 2 raters.")))
                }
                res <- private$.pairwiseKappaResults()
                if (is.null(res)) {
                    return(private$.plotUnavailable(.("Pairwise kappa could not be calculated for these raters.")))
                }

                b <- private$.kappaBands()
                res$pair <- factor(res$pair, levels = rev(res$pair[order(res$kappa)]))
                res$band <- private$.kappaBandFactor(res$kappa)
                # Fall back to the point estimate when vcd could not return a
                # finite ASE, so the bar still draws.
                res$lo <- ifelse(is.na(res$ci_lower), res$kappa, res$ci_lower)
                res$hi <- ifelse(is.na(res$ci_upper), res$kappa, res$ci_upper)

                p <- ggplot2::ggplot(res, ggplot2::aes(x = kappa, y = pair)) +
                    ggplot2::geom_vline(xintercept = c(0.20, 0.40, 0.60, 0.80),
                        linetype = "dotted", colour = "grey60") +
                    ggplot2::geom_errorbar(ggplot2::aes(xmin = lo, xmax = hi),
                        orientation = "y", width = 0.18, colour = "grey35") +
                    ggplot2::geom_point(ggplot2::aes(fill = band), shape = 21, size = 4, colour = "grey20") +
                    ggtheme +
                    ggplot2::scale_fill_manual(values = stats::setNames(b$colors, b$labels),
                        drop = TRUE, name = .("Agreement")) +
                    ggplot2::scale_x_continuous(limits = c(min(-0.05, min(res$lo, na.rm = TRUE)), 1)) +
                    ggplot2::labs(
                        title = .("Cohen's Kappa by Rater Pair"),
                        subtitle = .("95% CI; dotted lines: Landis & Koch cutoffs"),
                        x = .("Kappa"), y = NULL
                    ) +
                    # Five band labels do not fit on one row at the declared
                    # 700px width - they were clipped at the right edge.
                    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1)) +
                    ggplot2::theme(legend.position = "bottom", legend.title = ggplot2::element_blank())

                print(p)
                TRUE
            },

            # Category-specific agreement: one bar per rating category.
            .categoryPlot = function(image, ggtheme, theme, ...) {
                if (!private$.restorePlotState(image)) return(FALSE)
                if (!self$options$categoryAnalysis || is.null(private$.data_matrix)) {
                    return(FALSE)
                }
                res <- private$.categoryAgreementResults()
                if (is.null(res) || all(is.na(res$kappa))) {
                    return(private$.plotUnavailable(.("Category-specific agreement could not be calculated for these data.")))
                }

                b <- private$.kappaBands()
                res$category <- factor(res$category, levels = rev(res$category))
                res$band <- private$.kappaBandFactor(res$kappa)
                res$label <- sprintf("%.2f (n=%d)", res$kappa, res$frequency)

                p <- ggplot2::ggplot(res, ggplot2::aes(x = kappa, y = category, fill = band)) +
                    ggplot2::geom_vline(xintercept = c(0.20, 0.40, 0.60, 0.80),
                        linetype = "dotted", colour = "grey60") +
                    ggplot2::geom_col(width = 0.65, colour = "grey20") +
                    ggplot2::geom_text(ggplot2::aes(label = label),
                        hjust = -0.12, size = 3.4, colour = "grey20") +
                    ggtheme +
                    ggplot2::scale_fill_manual(values = stats::setNames(b$colors, b$labels),
                        drop = TRUE, name = .("Agreement")) +
                    ggplot2::scale_x_continuous(limits = c(min(0, min(res$kappa, na.rm = TRUE)), 1.15),
                        breaks = seq(0, 1, 0.2)) +
                    ggplot2::labs(
                        title = .("Agreement Within Each Rating Category"),
                        subtitle = .("Kappa for \u{201C}this category vs. all others\u{201D}; n = cases using it"),
                        x = .("Category kappa"), y = NULL
                    ) +
                    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1)) +
                    ggplot2::theme(legend.position = "bottom", legend.title = ggplot2::element_blank())

                print(p)
                TRUE
            },

            # Confusion matrix against the reference/expert standard. With no
            # reference standard selected, the first two raters are cross-
            # tabulated instead and the title says so.
            .confusionMatrixPlot = function(image, ggtheme, theme, ...) {
                if (!private$.restorePlotState(image)) return(FALSE)
                if (!self$options$pathologyContext || is.null(private$.data_matrix)) {
                    return(FALSE)
                }
                if (is.null(private$.n_raters) || private$.n_raters < 2) {
                    return(private$.plotUnavailable(.("Select at least 2 raters.")))
                }
                dm <- private$.data_matrix
                gold <- private$.referenceStandardValues()

                if (!is.null(gold)) {
                    x_src <- gold
                    y_src <- dm[[1]]
                    x_lab <- .("Reference standard")
                    y_lab <- private$.rater_names[1]
                    sub <- .("First rater against the reference standard")
                } else {
                    x_src <- dm[[1]]
                    y_src <- dm[[2]]
                    x_lab <- private$.rater_names[1]
                    y_lab <- private$.rater_names[2]
                    sub <- .("No reference standard selected: first two raters")
                }
                x_vals <- as.character(x_src)
                y_vals <- as.character(y_src)

                ok <- !is.na(x_vals) & !is.na(y_vals)
                if (sum(ok) < 1) {
                    return(private$.plotUnavailable(.("No complete pairs to cross-tabulate.")))
                }

                # Category order must come from the FACTOR LEVELS, never sort().
                # On an ordinal grading scale sort() gives Atypical/Benign/Malignant
                # instead of Benign/Atypical/Malignant, which scrambles the axes and
                # makes the highlighted diagonal meaningless.
                lv <- union(levels(as.factor(x_src)), levels(as.factor(y_src)))
                lv <- lv[lv %in% c(x_vals[ok], y_vals[ok])]
                if (length(lv) < 2) {
                    return(private$.plotUnavailable(.("At least 2 rating categories are needed for a confusion matrix.")))
                }
                tab <- table(
                    factor(y_vals[ok], levels = rev(lv)),
                    factor(x_vals[ok], levels = lv)
                )
                df <- as.data.frame(tab, stringsAsFactors = FALSE)
                # as.data.frame(<table>) runs make.names() over the dimnames, so
                # name the columns explicitly rather than relying on Var1/Var2.
                names(df) <- c("observed", "reference", "n")
                df$observed <- factor(df$observed, levels = rev(lv))
                df$reference <- factor(df$reference, levels = lv)
                df$pct <- df$n / sum(df$n) * 100
                df$label <- ifelse(df$n == 0, "", sprintf("%d\n(%.1f%%)", df$n, df$pct))
                df$diagonal <- as.character(df$observed) == as.character(df$reference)
                # The dark end of the viridis ramp swallows dark text, so the busiest
                # cells - the ones a reader actually looks at - get a light label.
                df$on_dark <- df$n > max(df$n) * 0.5

                # ggtheme goes BEFORE the scales. jamovi's ggtheme is a LIST that
                # carries its own default DISCRETE colour/fill scales, so appending
                # it last replaces scale_fill_viridis_c() and the render dies with
                # "Continuous value supplied to a discrete scale". Same trap as
                # R/chisqposttest.b.R:2610.
                p <- ggplot2::ggplot(df, ggplot2::aes(x = reference, y = observed, fill = n)) +
                    ggplot2::geom_tile(colour = "white", linewidth = 0.6) +
                    ggplot2::geom_tile(
                        data = df[df$diagonal, , drop = FALSE],
                        colour = "grey15", linewidth = 1.1, fill = NA
                    ) +
                    ggplot2::geom_text(ggplot2::aes(label = label, colour = on_dark), size = 3.4) +
                    ggtheme +
                    ggplot2::scale_colour_manual(
                        values = c(`FALSE` = "grey10", `TRUE` = "white"), guide = "none"
                    ) +
                    ggplot2::scale_fill_viridis_c(option = "viridis", direction = -1, name = .("Cases")) +
                    ggplot2::labs(
                        title = .("Confusion Matrix"),
                        subtitle = sub,
                        x = x_lab, y = y_lab
                    ) +
                    ggplot2::theme(
                        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                        panel.grid = ggplot2::element_blank()
                    )

                print(p)
                TRUE
            },

            # Diagnostic style dendrogram - Creates hierarchical clustering visualization matching Usubutun et al.
            .diagnosticStyleDendrogram = function(image, ggtheme, theme, ...) {
                if (!private$.restorePlotState(image)) return(FALSE)
                if (!self$options$performClustering || is.null(private$.style_clustering_results)) {
                    p <- ggplot() +
                        geom_text(aes(x = 0.5, y = 0.5, label = .("Enable rater clustering analysis to view the dendrogram.")),
                            size = 6
                        ) +
                        xlim(0, 1) +
                        ylim(0, 1) +
                        theme_void()
                    print(p)
                    return(TRUE)
                }

                # Extract clustering results
                hc <- private$.style_clustering_results$hclust_object
                style_groups <- private$.style_clustering_results$cluster_assignments
                rater_names <- private$.rater_names

                # Create dendrogram using ggdendro
                if (!requireNamespace("ggdendro", quietly = TRUE)) {
                    stop(.("Package 'ggdendro' is required for this analysis; please install it."))
                }

                # Convert hclust to ggplot-compatible format
                dend_data <- ggdendro::dendro_data(hc)

                # Assign colors to branches based on style groups
                n_groups <- private$.style_clustering_results$n_clusters

                # Check if n_groups is valid
                if (is.null(n_groups) || length(n_groups) == 0 || n_groups < 2) {
                    # Default to 3 groups if invalid
                    n_groups <- 3
                }

                if (n_groups == 3) {
                    # Use Usubutun colors: Green, Yellow, Red
                    group_colors <- c("#2E8B57", "#FFD700", "#DC143C") # Green, Gold, Crimson
                } else {
                    group_colors <- rainbow(n_groups)
                }

                # Create color mapping for raters based on their style groups
                rater_colors <- rep("black", length(rater_names))
                for (i in seq_along(rater_names)) {
                    group_id <- style_groups[i]
                    if (!is.null(group_id) && !is.na(group_id) && group_id > 0 && group_id <= length(group_colors)) {
                        rater_colors[i] <- group_colors[group_id]
                    }
                }

                # Create the plot matching Usubutun style with visible clustering tree
                p <- ggplot() +
                    # Draw the main dendrogram segments (tree structure)
                    geom_segment(
                        data = dend_data$segments,
                        aes(x = x, y = y, xend = xend, yend = yend),
                        color = "black", linewidth = 0.6, lineend = "round"
                    ) +
                    # Add colored rectangles at top to show style groups
                    {
                        # Create rectangles for each style group at the top
                        group_rects <- data.frame()
                        for (group in 1:n_groups) {
                            group_members <- which(style_groups[hc$order] == group)
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
                            geom_rect(
                                data = group_rects,
                                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                                fill = group_rects$fill, color = "black", linewidth = 0.3, alpha = 0.7
                            )
                        }
                    } +
                    # Add style group labels
                    {
                        group_labels <- data.frame()
                        for (group in 1:n_groups) {
                            group_members <- which(style_groups[hc$order] == group)
                            if (length(group_members) > 0) {
                                x_center <- mean(group_members)
                                y_top <- max(dend_data$segments$y, na.rm = TRUE)
                                group_name <- jmvcore::format(.("Group {group}"), group = group)
                                group_labels <- rbind(group_labels, data.frame(
                                    x = x_center, y = y_top * 1.05,
                                    label = group_name,
                                    color = "white"
                                ))
                            }
                        }
                        if (nrow(group_labels) > 0) {
                            geom_text(
                                data = group_labels,
                                aes(x = x, y = y, label = label),
                                color = "white", size = 3, fontface = "bold"
                            )
                        }
                    } +
                    # Add rater labels at bottom with colors
                    geom_text(
                        data = dend_data$labels,
                        aes(x = x, y = y, label = label),
                        color = rater_colors[match(dend_data$labels$label, rater_names)],
                        angle = 90, size = 3, hjust = 1, vjust = 0.5, fontface = "bold"
                    ) +
                    labs(
                        title = .("Diagnostic Style Dendrogram"),
                        subtitle = jmvcore::format(
                            .("Hierarchical clustering of {count} pathologists"),
                            count = length(rater_names)
                        ),
                        x = .("Pathologist"), y = .("Distance")
                    ) +
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
            .numericRatingMatrix = function() {
                dm <- private$.data_matrix
                labels <- private$.categoryLabels(dm)
                vapply(dm, function(x) match(as.character(x), labels), integer(nrow(dm)))
            },

            .diagnosisColors = function() {
                labels <- private$.categoryLabels()
                colors <- switch(self$options$heatmapColorScheme,
                    viridis = viridisLite::viridis(length(labels)),
                    RdYlBu = grDevices::colorRampPalette(c("#d73027", "#fee090", "#4575b4"))(length(labels)),
                    grDevices::colorRampPalette(c("#4472C4", "#70AD47", "#FFC000"))(length(labels))
                )
                stats::setNames(colors, labels)
            },

            # Diagnostic style heatmap - Creates case-by-rater heatmap with two-way clustering
            .diagnosticStyleHeatmap = function(image, ggtheme, theme, ...) {
                if (!private$.restorePlotState(image)) return(FALSE)
                if (!self$options$performClustering || !self$options$showClusteringHeatmap || is.null(private$.style_clustering_results)) {
                    p <- ggplot() +
                        geom_text(aes(x = 0.5, y = 0.5, label = .("Enable diagnostic style analysis to view the style heatmap.")),
                            size = 6
                        ) +
                        xlim(0, 1) +
                        ylim(0, 1) +
                        theme_void()
                    print(p)
                    return(TRUE)
                }

                # Create a case-by-rater heatmap with two-way clustering
                data_matrix <- private$.data_matrix
                style_groups <- private$.style_clustering_results$cluster_assignments
                rater_names <- private$.rater_names
                n_cases <- private$.n_cases

                # Load required packages
                if (!requireNamespace("ggdendro", quietly = TRUE)) {
                    stop(.("Package 'ggdendro' is required for this analysis; please install it."))
                }
                if (!requireNamespace("gridExtra", quietly = TRUE)) {
                    stop(.("Package 'gridExtra' is required for this analysis; please install it."))
                }
                if (!requireNamespace("grid", quietly = TRUE)) {
                    stop(.("Package 'grid' is required for this analysis; please install it."))
                }

                # === CLUSTER RATERS (COLUMNS) ===
                # Order raters by style group (matching dendrogram order)
                rater_order <- private$.style_clustering_results$hclust_object$order
                ordered_raters <- rater_names[rater_order]
                ordered_groups <- style_groups[rater_order]

                # === CLUSTER CASES (ROWS) ===
                # Create numeric matrix for case clustering with proper validation
                numeric_matrix <- private$.numericRatingMatrix()

                # Check for any remaining NA/NaN/Inf values
                if (any(is.na(numeric_matrix)) || any(is.infinite(numeric_matrix))) {
                    # If there are still problematic values, skip clustering and use original order
                    case_order <- 1:n_cases
                    case_hc <- NULL
                } else {
                    # Calculate case similarities (agreement across raters)
                    case_dist <- dist(numeric_matrix, method = "euclidean")

                    # Check if distance matrix is valid
                    if (any(is.na(case_dist)) || any(is.infinite(case_dist))) {
                        case_order <- 1:n_cases
                        case_hc <- NULL
                    } else {
                        case_hc <- hclust(case_dist, method = "ward.D2")
                        case_order <- case_hc$order
                    }
                }

                # Order data by both dimensions
                ordered_data <- as.matrix(data_matrix)[case_order, rater_order, drop = FALSE]

                # Create long format data for ggplot
                heatmap_data <- data.frame()
                for (case_idx in 1:n_cases) {
                    original_case_id <- case_order[case_idx]
                    for (rater_idx in seq_along(ordered_raters)) {
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
                heatmap_data$Case <- factor(heatmap_data$Case, levels = n_cases:1) # Reverse order for bottom-to-top

                # Define colors matching Usubutun paper
                diagnosis_colors <- private$.diagnosisColors()

                # Create dendrograms for both dimensions
                rater_dend_data <- ggdendro::dendro_data(private$.style_clustering_results$hclust_object)

                # Create the main heatmap
                main_heatmap <- ggplot(heatmap_data, aes(x = Rater, y = Case, fill = Diagnosis)) +
                    geom_tile(color = "white", linewidth = 0.1) +
                    scale_fill_manual(values = diagnosis_colors, name = .("Diagnosis")) +
                    labs(x = .("Pathologist"), y = .("Case")) +
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
                    geom_segment(
                        data = rater_dend_data$segments,
                        aes(x = x, y = y, xend = xend, yend = yend),
                        color = "black", linewidth = 0.3
                    ) +
                    theme_void() +
                    theme(plot.margin = margin(0, 5, 0, 5)) +
                    scale_x_continuous(expand = c(0.02, 0)) +
                    coord_cartesian(ylim = c(0, max(rater_dend_data$segments$y, na.rm = TRUE)))

                # Create case dendrogram (left) - rotated 90 degrees
                if (!is.null(case_hc)) {
                    case_dend_data <- ggdendro::dendro_data(case_hc)
                    case_dend_plot <- ggplot() +
                        geom_segment(
                            data = case_dend_data$segments,
                            aes(x = y, y = x, xend = yend, yend = xend), # Swap x/y for rotation
                            color = "black", linewidth = 0.3
                        ) +
                        theme_void() +
                        theme(plot.margin = margin(5, 0, 5, 0)) +
                        scale_y_continuous(expand = c(0.02, 0), trans = "reverse") + # Reverse to match heatmap
                        coord_cartesian(xlim = c(0, max(case_dend_data$segments$y, na.rm = TRUE)))
                } else {
                    # Create empty plot if case clustering failed
                    case_dend_plot <- ggplot() +
                        geom_text(aes(x = 0.5, y = 0.5, label = .("Case clustering was skipped.")),
                            size = 3, hjust = 0.5
                        ) +
                        xlim(0, 1) +
                        ylim(0, 1) +
                        theme_void() +
                        theme(plot.margin = margin(5, 0, 5, 0))
                }

                # Create empty plot for corner
                empty_plot <- ggplot() +
                    theme_void()

                # Combine all plots using grid.arrange with proper proportions
                combined_plot <- gridExtra::grid.arrange(
                    rater_dend_plot, empty_plot,
                    main_heatmap, case_dend_plot,
                    nrow = 2, ncol = 2,
                    widths = c(4, 1), # Main plot wider than case dendrogram
                    heights = c(1, 4), # Main plot taller than rater dendrogram
                    top = grid::textGrob(.("Two-way clustered diagnostic style heatmap"),
                        gp = grid::gpar(fontsize = 14, fontface = "bold")
                    )
                )

                grid::grid.draw(combined_plot)
                TRUE
            },

            # Combined dendrogram and heatmap - Exact reproduction of Usubutun et al. Figure 1
            .diagnosticStyleCombined = function(image, ggtheme, theme, ...) {
                if (!private$.restorePlotState(image)) return(FALSE)
                if (!self$options$performClustering || is.null(private$.style_clustering_results)) {
                    p <- ggplot() +
                        geom_text(aes(x = 0.5, y = 0.5, label = .("Enable diagnostic style analysis to view the combined visualization.")),
                            size = 8, hjust = 0.5, vjust = 0.5
                        ) +
                        xlim(0, 1) +
                        ylim(0, 1) +
                        theme_void() +
                        labs(title = .("Combined Dendrogram and Heatmap"))
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
                    n_groups <- private$.styleGroupCount()
                    if (is.null(n_groups) || length(n_groups) == 0 || n_groups < 2) {
                        n_groups <- 3
                    }
                }

                # Load required packages
                if (!requireNamespace("ggdendro", quietly = TRUE)) {
                    stop(.("Package 'ggdendro' is required for this analysis; please install it."))
                }
                if (!requireNamespace("gridExtra", quietly = TRUE)) {
                    stop(.("Package 'gridExtra' is required for this analysis; please install it."))
                }
                if (!requireNamespace("grid", quietly = TRUE)) {
                    stop(.("Package 'grid' is required for this analysis; please install it."))
                }

                # === CREATE DENDROGRAM (TOP PART) ===
                dend_data <- ggdendro::dendro_data(hc)

                # Define colors
                if (n_groups == 3) {
                    group_colors <- c("#2E8B57", "#FFD700", "#DC143C") # Green, Gold, Crimson
                } else {
                    group_colors <- rainbow(n_groups)
                }

                # Order raters by clustering result (same order as heatmap)
                rater_order <- private$.style_clustering_results$hclust_object$order
                ordered_raters <- rater_names[rater_order]
                ordered_groups <- style_groups[rater_order]

                # Create color mapping for raters
                rater_colors <- rep("black", length(rater_names))
                for (i in seq_along(rater_names)) {
                    group_id <- style_groups[i]
                    if (!is.null(group_id) && !is.na(group_id) && group_id > 0 && group_id <= length(group_colors)) {
                        rater_colors[i] <- group_colors[group_id]
                    }
                }

                # Create dendrogram plot
                dend_plot <- ggplot() +
                    # Draw dendrogram tree structure
                    geom_segment(
                        data = dend_data$segments,
                        aes(x = x, y = y, xend = xend, yend = yend),
                        color = "black", linewidth = 0.5, lineend = "round"
                    ) +
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
                            geom_rect(
                                data = group_rects,
                                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                                fill = group_rects$fill, color = "black", linewidth = 0.3, alpha = 0.8
                            )
                        }
                    } +
                    # Add style group labels
                    {
                        group_labels <- data.frame()
                        group_names <- toupper(c(.("Green"), .("Yellow"), .("Red")))
                        for (group in seq_len(min(n_groups, 3))) {
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
                            geom_text(
                                data = group_labels,
                                aes(x = x, y = y, label = label),
                                color = "white", size = 3, fontface = "bold"
                            )
                        }
                    } +
                    scale_x_continuous(expand = c(0.02, 0.02)) +
                    scale_y_continuous(expand = c(0.02, 0.15)) +
                    theme_void() +
                    theme(plot.margin = margin(5, 5, 0, 5))

                # === CREATE HEATMAP (BOTTOM PART) WITH CASE CLUSTERING ===

                # First cluster cases (rows) with proper validation
                # Create numeric matrix for case clustering
                numeric_matrix <- private$.numericRatingMatrix()

                # Check for any remaining NA/NaN/Inf values
                if (any(is.na(numeric_matrix)) || any(is.infinite(numeric_matrix))) {
                    # If there are still problematic values, skip clustering and use original order
                    case_order <- 1:n_cases
                    case_hc <- NULL
                } else {
                    # Calculate case similarities (agreement across raters)
                    case_dist <- dist(numeric_matrix, method = "euclidean")

                    # Check if distance matrix is valid
                    if (any(is.na(case_dist)) || any(is.infinite(case_dist))) {
                        case_order <- 1:n_cases
                        case_hc <- NULL
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
                    for (rater_id in seq_along(ordered_raters)) {
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
                diagnosis_colors <- private$.diagnosisColors()

                # Create heatmap plot
                heatmap_plot <- ggplot(heatmap_data, aes(x = Rater, y = Case, fill = Diagnosis)) +
                    geom_tile(color = "white", linewidth = 0.1) +
                    scale_fill_manual(values = diagnosis_colors, name = .("Diagnosis")) +
                    scale_x_continuous(
                        breaks = seq_along(ordered_raters),
                        labels = ordered_raters,
                        expand = c(0, 0)
                    ) +
                    scale_y_continuous(
                        trans = "reverse",
                        breaks = c(1, seq(10, n_cases, 10)),
                        expand = c(0, 0)
                    ) +
                    labs(x = .("Pathologist"), y = .("Case")) +
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
                    case_dend_data <- ggdendro::dendro_data(case_hc)

                    # Create case dendrogram (rotated 90 degrees to align with heatmap)
                    case_dend_plot <- ggplot() +
                        geom_segment(
                            data = case_dend_data$segments,
                            aes(x = y, y = x, xend = yend, yend = xend), # Swap x/y for rotation
                            color = "black", linewidth = 0.3
                        ) +
                        scale_y_continuous(expand = c(0.02, 0.02), trans = "reverse") + # Reverse to match heatmap
                        scale_x_continuous(expand = c(0.02, 0.02)) +
                        theme_void() +
                        theme(plot.margin = margin(0, 0, 5, 5))
                } else {
                    # Create empty plot if case clustering failed
                    case_dend_plot <- ggplot() +
                        geom_text(aes(x = 0.5, y = 0.5, label = .("Case clustering was skipped.")),
                            size = 3, hjust = 0.5
                        ) +
                        xlim(0, 1) +
                        ylim(0, 1) +
                        theme_void() +
                        theme(plot.margin = margin(0, 0, 5, 5))
                }

                # Create empty corner plot
                empty_plot <- ggplot() +
                    theme_void()

                # === COMBINE ALL PLOTS ===
                # Create a comprehensive two-way clustered plot with dendrograms on both axes
                combined_plot <- gridExtra::grid.arrange(
                    empty_plot, dend_plot,
                    case_dend_plot, heatmap_plot,
                    nrow = 2, ncol = 2,
                    widths = c(1, 4), # Case dendrogram narrower than main plot
                    heights = c(1, 4), # Rater dendrogram shorter than main plot
                    top = grid::textGrob(.("Two-way clustered diagnostic patterns (Usubutun style)"),
                        gp = grid::gpar(fontsize = 14, fontface = "bold")
                    )
                )

                # Print the combined plot
                grid::grid.draw(combined_plot)
                TRUE
            },

            # Data validation
            .validateData = function() {
                # Check for factor variables
                var_names <- self$options$vars
                if (isTRUE(self$options$sampleSizePlanning)) {
                    target <- self$options$targetKappa
                    precision <- self$options$targetPrecision
                    if (!is.finite(target) || target <= 0 || target >= 1) {
                        jmvcore::reject(.("Target kappa must be strictly between 0 and 1 for sample-size planning."))
                    }
                    if (!is.finite(precision) || precision <= 0 ||
                        precision >= min(target, 1 - target)) {
                        jmvcore::reject(.("Target precision must be positive and smaller than both the target kappa and 1 minus the target kappa, so the requested confidence interval stays within 0 to 1."))
                    }
                }
                if (isTRUE(self$options$useMetadataRows) && is.null(self$options$caseID)) {
                    jmvcore::reject(.("Metadata rows require a case ID variable. Select the variable containing META_ identifiers and run the analysis again."))
                }
                # Validate CASE rows only: META_ rows hold rater attributes (years,
                # specialty) that would otherwise read as extra categories.
                vdata <- private$.dropMetadataRows(self$data[var_names])
                non_factors <- c()

                for (var_name in var_names) {
                    var_data <- vdata[[var_name]]
                    if (!is.factor(var_data)) {
                        non_factors <- c(non_factors, var_name)
                    }
                }

                if (length(non_factors) > 0) {
                    # reject(formats, code = NULL, ...): the value used to land in
                    # `code`, so the user saw a literal "{}".
                    jmvcore::reject(
                        .("The following variables are not factors: {}. Please convert them to factors before analysis."),
                        code = NULL,
                        paste(non_factors, collapse = ", ")
                    )
                }

                # Check for consistent categories across raters
                all_levels <- lapply(var_names, function(var) levels(vdata[[var]]))
                reference_levels <- all_levels[[1]]

                inconsistent_vars <- c()
                inconsistent_order_vars <- c()
                for (i in 2:length(all_levels)) {
                    if (!identical(sort(all_levels[[i]]), sort(reference_levels))) {
                        inconsistent_vars <- c(inconsistent_vars, var_names[i])
                    } else if (!identical(all_levels[[i]], reference_levels)) {
                        inconsistent_order_vars <- c(inconsistent_order_vars, var_names[i])
                    }
                }

                if (length(inconsistent_vars) > 0) {
                    private$.accumulateMessage(jmvcore::format(
                        .("The following raters have different factor levels than the first rater: {raters}. This may affect agreement calculations."),
                        raters = paste(inconsistent_vars, collapse = ", ")
                    ), severity = "strong_warning")
                }
                if (length(inconsistent_order_vars) > 0) {
                    private$.accumulateMessage(jmvcore::format(
                        .("The following raters use a different category order than the first rater: {raters}. Methods that require an ordinal scale will not use ordinal scoring."),
                        raters = paste(inconsistent_order_vars, collapse = ", ")
                    ), severity = "strong_warning")
                }

                # Check for sufficient data
                complete_cases <- sum(complete.cases(vdata[var_names]))
                if (complete_cases < 10) {
                    private$.accumulateMessage(jmvcore::format(
                        .("Very few complete cases (n={n}). Results may be unreliable."),
                        n = complete_cases
                    ), severity = "strong_warning")
                }

                # Check for empty categories
                for (var_name in var_names) {
                    var_data <- vdata[[var_name]]
                    empty_levels <- levels(var_data)[!levels(var_data) %in% var_data]
                    if (length(empty_levels) > 0) {
                        private$.accumulateMessage(jmvcore::format(
                            .("Rater {rater} has categories that were never used: {unused}. Confirm that the declared category scale is correct before interpreting weighted analyses."),
                            rater = var_name, unused = paste(empty_levels, collapse = ", ")
                        ))
                    }
                }

                # Check for single-category data across all raters
                all_data <- unlist(lapply(var_names, function(v) as.character(vdata[[v]])))
                unique_total <- length(unique(na.omit(all_data)))
                if (unique_total < 2) {
                    jmvcore::reject(.("Agreement analysis requires at least 2 categories across all raters."))
                }

                # Warn if individual raters have single category
                for (var_name in var_names) {
                    var_data <- vdata[[var_name]]
                    unique_values <- length(unique(var_data[!is.na(var_data)]))
                    if (unique_values < 2) {
                        private$.accumulateMessage(
                            jmvcore::format(.("Rater {rater} used only 1 category. Results for metrics requiring category variation may be unreliable."), rater = var_name),
                            severity = "strong_warning"
                        )
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
                    # reject(formats, code = NULL, ...): the value used to land in
                    # `code`, so the user saw a literal "{}".
                    jmvcore::reject(
                        .("The following required packages are missing: {}. Please install them using install.packages()"),
                        code = NULL,
                        paste(missing_packages, collapse = ", ")
                    )
                }
            },

            .showWelcomeMessage = function() {
                # Create clean, accessible welcome message similar to decisionpanel
                welcome_html <- paste0(
                    "<div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.4;'>",
                    "<div style='background-color: rgba(88, 88, 88, 0.06); border: 2px solid #2e7d32; padding: 20px; margin-bottom: 20px; color: inherit;'>",
                    .("<h2 style='margin: 0 0 10px 0; font-size: 20px; color: #2e7d32;'>Inter-rater Reliability Analysis</h2>"),
                    .("<p style='margin: 0; font-size: 14px; color: inherit;'>Evaluate agreement between multiple raters/observers using statistical measures</p>"),
                    "</div>",
                    "<div style='background-color: rgba(155, 155, 155, 0.06); border-left: 4px solid #2e7d32; padding: 15px; margin-bottom: 20px; color: inherit;'>",
                    .("<h3 style='margin: 0 0 10px 0; color: #2e7d32; font-size: 16px;'>Setup status</h3>")
                )

                # Setup status shown only before the analysis can run
                n_vars <- length(self$options$vars)
                if (n_vars >= 2) {
                    welcome_html <- paste0(
                        welcome_html,
                        "<div style='font-weight: bold; margin-bottom: 10px; color: #2e7d32;'>",
                        jmvcore::format(
                            .("[READY] {count} rater variables selected"),
                            count = n_vars
                        ),
                        "</div>",
                        .("<p style='margin: 0;'>Minimum requirements met. Analysis will begin automatically.</p>")
                    )
                } else {
                    welcome_html <- paste0(
                        welcome_html,
                        "<div style='margin-bottom: 10px;'>",
                        jmvcore::format(
                            .("[ ] Rater variables: {count}/2 minimum"),
                            count = n_vars
                        ),
                        "</div>",
                        .("<p style='margin: 0; color: inherit;'>Select at least 2 rater variables to proceed with analysis.</p>")
                    )
                }

                welcome_html <- paste0(
                    welcome_html,
                    "</div>",
                    "<table style='width: 100%; border-collapse: collapse; margin-bottom: 20px;'>",
                    "<tr>",
                    "<td style='width: 50%; border: 1px solid #ccc; padding: 15px; vertical-align: top;'>",
                    .("<h4 style='margin: 0 0 10px 0; font-size: 15px; color: #2e7d32;'>Quick start guide</h4>"),
                    "<ol style='margin: 0; padding-left: 20px; font-size: 14px;'>",
                    .("<li>Select <strong>2 or more rater variables</strong> from your dataset</li>"),
                    .("<li>Ensure all raters use the same rating scale/categories</li>"),
                    .("<li>Choose an appropriate <strong>weighting scheme</strong> (unweighted for nominal, weighted for ordinal)</li>"),
                    .("<li>Optionally enable <strong>frequency tables</strong> and <strong>heatmap visualization</strong></li>"),
                    .("<li>Advanced users can try <strong>Krippendorff's alpha</strong> or <strong>consensus analysis</strong></li>"),
                    "</ol></td>",
                    "<td style='width: 50%; border: 1px solid #ccc; padding: 15px; vertical-align: top;'>",
                    .("<h4 style='margin: 0 0 10px 0; font-size: 15px; color: #2e7d32;'>What you will get</h4>"),
                    "<ul style='margin: 0; padding-left: 20px; font-size: 14px;'>",
                    .("<li><strong>Kappa statistics</strong> with confidence intervals and interpretation</li>"),
                    .("<li><strong>Overall agreement percentage</strong> and summary statistics</li>"),
                    .("<li><strong>Clinical interpretation</strong> of agreement levels</li>"),
                    .("<li><strong>Frequency tables</strong> showing rater distributions</li>"),
                    .("<li><strong>Agreement heatmap</strong> with customizable color themes</li>"),
                    .("<li><strong>Consensus analysis</strong> for determining agreed ratings</li>"),
                    "</ul></td></tr></table>",
                    "<div style='background-color: rgba(255, 169, 33, 0.14); border: 1px solid #f57c00; padding: 15px; color: inherit;'>",
                    .("<h4 style='margin: 0 0 10px 0; font-size: 15px; color: #f57c00;'>Important notes for clinical use</h4>"),
                    "<ul style='margin: 0; padding-left: 20px; font-size: 14px;'>",
                    .("<li><strong>Sample size:</strong> Required sample size depends on expected agreement, category prevalence, rater count, and target precision; use the sample-size planning output.</li>"),
                    .("<li><strong>Category prevalence:</strong> Report rare categories and consider a prevalence-robust coefficient such as Gwet's AC1.</li>"),
                    .("<li><strong>Interpretation:</strong> Landis &amp; Koch (1977) bands for \u{03BA}: 0.20 or less slight, 0.21-0.40 fair, 0.41-0.60 moderate, 0.61-0.80 substantial, above 0.80 almost perfect. These are descriptive conventions, not clinical thresholds.</li>"),
                    .("<li><strong>Clinical context:</strong> Consider the clinical consequences of disagreement when interpreting results.</li>"),
                    "</ul></div></div>"
                )

                self$results$todo$setContent(welcome_html)
            },
            # A panel the user switched on must not silently disappear; say why it is
            # empty instead of hiding it.
            .clinicalSummaryUnavailable = function() {
                self$results$clinicalSummary$setContent(paste0(
                    "<p style='color: inherit;'>",
                    .("A clinical summary could not be generated because no kappa result is available for these data."),
                    "</p>"
                ))
            },
            .generateClinicalSummary = function() {
                # Generate clinical summary only if analysis has been performed
                if (is.null(private$.data_matrix) || is.null(private$.n_raters) || private$.n_raters < 2) {
                    private$.clinicalSummaryUnavailable()
                    return()
                }

                # Get main kappa result from the kappa table
                kappa_table <- self$results$kappaTable
                if (kappa_table$rowCount == 0) {
                    private$.clinicalSummaryUnavailable()
                    return()
                }

                # Extract kappa value and interpretation from first row using row index.
                # Table$getCell() returns a jmvcore Cell object, not the value: reading
                # it without $value made as.numeric() fail, so this summary was never
                # generated (it used to be hidden silently) and the report sentence
                # never carried its confidence interval.
                # Try to safely get the first row from the kappa table
                kappa_val <- NA
                interpretation <- ""
                method_name <- ""
                p_value <- NA

                # Use tryCatch to handle potential table access errors
                tryCatch(
                    {
                        if (length(kappa_table$rowKeys) > 0) {
                            # Try direct row access by index position
                            first_key <- kappa_table$rowKeys[[1]]

                            # For regular jamovi tables, try accessing columns directly
                            kappa_cell <- kappa_table$getCell(rowKey = first_key, "kappa")$value
                            interpretation <- kappa_table$getCell(rowKey = first_key, "interpretation")$value
                            method_name <- kappa_table$getCell(rowKey = first_key, "method")$value
                            p_value <- kappa_table$getCell(rowKey = first_key, "p")$value

                            # Convert kappa_cell to numeric if it's not already
                            if (!is.null(kappa_cell)) {
                                kappa_val <- tryCatch(
                                    {
                                        as.numeric(kappa_cell)
                                    },
                                    error = function(e) {
                                        # If conversion fails, try to extract from environment or list
                                        if (is.environment(kappa_cell) || is.list(kappa_cell)) {
                                            NA
                                        } else {
                                            as.numeric(kappa_cell)
                                        }
                                    }
                                )
                            }
                        }
                    },
                    error = function(e) {
                        # If table access fails, explain the unavailable output in its panel.
                        private$.clinicalSummaryUnavailable()
                        return(NULL)
                    }
                )

                # Check if we got valid data (handle vector case)
                if (is.null(kappa_val) || length(kappa_val) == 0 || all(is.na(kappa_val))) {
                    private$.clinicalSummaryUnavailable()
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
                    private$.clinicalSummaryUnavailable()
                    return()
                }
                if (length(interpretation) > 1) {
                    interpretation <- interpretation[1]
                }
                if (length(method_name) > 1) {
                    method_name <- method_name[1]
                }
                p_value <- if (length(p_value) == 1L) {
                    suppressWarnings(as.numeric(p_value))
                } else {
                    NA_real_
                }
                if (!is.finite(p_value)) p_value <- NA_real_

                # Get overall agreement percentage
                overview_table <- self$results$overviewTable
                overall_agreement <- NA
                tryCatch(
                    {
                        if (overview_table$rowCount > 0 && length(overview_table$rowKeys) > 0) {
                            first_overview_key <- overview_table$rowKeys[[1]]
                            overall_agreement <- overview_table$getCell(rowKey = first_overview_key, "overall_agreement")$value
                            # Handle vector case
                            if (length(overall_agreement) > 1) {
                                overall_agreement <- overall_agreement[1]
                            }
                        }
                    },
                    error = function(e) {
                        # Overall agreement is supplemental; omit it if the table is unavailable.
                        overall_agreement <- NA
                    }
                )

                # Generate clinical interpretation
                clinical_interp <- private$.getClinicalInterpretation(kappa_val)
                method_name_html <- jmvcore::htmlEscape(method_name)

                summary_html <- paste0(
                    "<div style='font-family: Arial, sans-serif; max-width: 700px; line-height: 1.5;'>",
                    "<div style='background-color: rgba(33, 159, 33, 0.1); border-left: 4px solid #2e7d32; padding: 15px; margin-bottom: 15px; color: inherit;'>",
                    .("<h4 style='margin: 0 0 10px 0; color: #2e7d32; font-size: 16px;'>Clinical summary</h4>"),
                    "<p style='margin: 0; font-size: 14px;'>",
                    jmvcore::format(
                        .("Inter-rater agreement analysis of <strong>{raters} raters</strong> evaluating <strong>{cases} cases</strong> using <strong>{categories} categories</strong>."),
                        raters = private$.n_raters,
                        cases = private$.n_cases,
                        categories = length(private$.categories)
                    ),
                    "</p>",
                    "</div>",
                    "<div style='background-color: rgba(138, 138, 138, 0.06); border: 1px solid #ddd; padding: 15px; margin-bottom: 15px; color: inherit;'>",
                    .("<h4 style='margin: 0 0 10px 0; color: inherit; font-size: 15px;'>Key findings</h4>"),
                    "<p style='margin: 0 0 10px 0; font-size: 14px;'><strong>", method_name_html, ":</strong> ",
                    round(kappa_val, 3), " (", interpretation, ")</p>",
                    if (!is.na(overall_agreement)) {
                        paste0(
                            "<p style='margin: 0 0 10px 0; font-size: 14px;'>",
                            jmvcore::format(
                                .("<strong>Complete agreement (all raters identical):</strong> {pct}%"),
                                pct = round(overall_agreement, 1)
                            ),
                            "</p>"
                        )
                    } else {
                        ""
                    },
                    "<p style='margin: 0; font-size: 14px;'><strong>",
                    .("Statistical significance"),
                    ":</strong> ",
                    if (is.na(p_value)) .("not available") else if (p_value < 0.001) "p < 0.001" else paste0("p = ", round(p_value, 3)), "</p>",
                    "</div>",
                    "<div style='background-color: rgba(255, 203, 33, 0.14); border: 1px solid #ffa000; padding: 15px; color: inherit;'>",
                    .("<h4 style='margin: 0 0 10px 0; color: #f57c00; font-size: 15px;'>Clinical interpretation</h4>"),
                    "<p style='margin: 0; font-size: 14px;'>", clinical_interp, "</p>",
                    "</div></div>"
                )

                self$results$clinicalSummary$setContent(summary_html)

                # Generate copy-ready report template
                private$.generateReportTemplate(kappa_val, method_name, p_value, overall_agreement, interpretation)
            },
            .generateReportTemplate = function(kappa_val, method_name, p_value, overall_agreement, interpretation) {
                # Generate copy-ready report sentences for clinical/research use

                # Get confidence interval if available
                kappa_table <- self$results$kappaTable
                ci_text <- ""
                if (kappa_table$rowCount > 0 && length(kappa_table$rowKeys) > 0) {
                    tryCatch(
                        {
                            first_key <- kappa_table$rowKeys[[1]]
                            ci_lower <- kappa_table$getCell(rowKey = first_key, "ci_lower")$value
                            ci_upper <- kappa_table$getCell(rowKey = first_key, "ci_upper")$value
                            if (!is.na(ci_lower) && !is.na(ci_upper)) {
                                ci_text <- sprintf(", 95%% CI %.3f-%.3f", ci_lower, ci_upper)
                            }
                        },
                        error = function(e) {
                            ci_text <- ""
                        }
                    )
                }

                # Format p-value appropriately
                p_text <- if (is.na(p_value)) {
                    ""
                } else if (p_value < 0.001) {
                    ", p < 0.001"
                } else {
                    sprintf(", p = %.3f", p_value)
                }

                # Generate main result sentence.
                # Name the estimate by the method actually reported: the primary row can
                # be Krippendorff's alpha (not kappa) or, when Cohen's kappa is forced with
                # more than 2 raters, a single rater pair. The sentence used to say
                # "kappa" and "for the 5 raters" in both cases.
                pair <- if (grepl(" vs ", method_name, fixed = TRUE)) sub("^[^:]*: ", "", method_name) else NA_character_
                pair_html <- if (is.na(pair)) pair else jmvcore::htmlEscape(pair)
                method_name_html <- jmvcore::htmlEscape(method_name)
                estimate_label <- if (grepl("Krippendorff", method_name, fixed = TRUE)) "\u{03B1}" else "\u{03BA}"
                main_sentence <- if (!is.na(kappa_val) && is.na(pair)) {
                    jmvcore::format(
                        .("Inter-rater agreement among {raters} raters evaluating {cases} cases was {interpretation} ({symbol} = {estimate}{ci}{p})."),
                        raters = private$.n_raters,
                        cases = private$.n_cases,
                        interpretation = tolower(interpretation),
                        symbol = estimate_label,
                        estimate = sprintf("%.3f", kappa_val),
                        ci = ci_text,
                        p = p_text
                    )
                } else if (!is.na(kappa_val)) {
                    jmvcore::format(
                        .("Agreement between {pair} was {interpretation} ({symbol} = {estimate}{ci}{p}) across {cases} cases. This is one of several rater pairs ({raters} raters in total); report the pairwise results rather than this pair alone."),
                        pair = pair_html,
                        interpretation = tolower(interpretation),
                        symbol = estimate_label,
                        estimate = sprintf("%.3f", kappa_val),
                        ci = ci_text,
                        p = p_text,
                        cases = private$.n_cases,
                        raters = private$.n_raters
                    )
                } else {
                    jmvcore::format(
                        .("Inter-rater agreement could not be reliably calculated for the {raters} raters evaluating {cases} cases. Data quality or sample size may be insufficient."),
                        raters = private$.n_raters,
                        cases = private$.n_cases
                    )
                }

                # Add overall agreement if available
                overall_sentence <- if (!is.na(overall_agreement)) {
                    jmvcore::format(.("All raters gave the same rating in {pct}% of cases."), pct = sprintf("%.1f", overall_agreement))
                } else {
                    ""
                }

                # Add method-specific information
                method_sentence <- jmvcore::format(
                    .("Agreement was assessed using {method}."),
                    method = method_name_html
                )

                # Clinical recommendation based on kappa value
                recommendation <- if (!is.na(kappa_val)) {
                    band <- private$.interpretKappa(kappa_val)
                    jmvcore::format(
                        .("By the Landis and Koch (1977) convention this is {band} agreement; whether it is adequate depends on the confidence interval, the prevalence of each category and the clinical consequence of a disagreement."),
                        band = tolower(band)
                    )
                } else {
                    .("Agreement could not be assessed - review data quality and sample size requirements.")
                }

                # Create copy-ready template
                report_html <- paste0(
                    "<div style='font-family: Arial, sans-serif; max-width: 700px; line-height: 1.6;'>",
                    "<div style='background-color: rgba(33, 152, 239, 0.13); border: 1px solid #1976d2; padding: 15px; margin-bottom: 15px; color: inherit;'>",
                    .("<h4 style='margin: 0 0 10px 0; color: #1976d2; font-size: 16px;'>Copy-ready report template</h4>"),
                    .("<p style='margin: 0; font-size: 13px; color: inherit;'>Click inside the boxes to select text, then copy (Ctrl+C/Cmd+C) for use in reports:</p>"),
                    "</div>",
                    "<div style='background-color: rgba(138, 155, 172, 0.06); border: 1px solid #dee2e6; padding: 12px; margin-bottom: 10px; border-radius: 4px; color: inherit;'>",
                    .("<h5 style='margin: 0 0 8px 0; font-size: 14px; color: inherit;'>Main result:</h5>"),
                    "<div style='font-family: Times, serif; font-size: 14px; line-height: 1.5; padding: 8px; background: rgba(138, 155, 172, 0.06); color: inherit; border: 1px solid #ced4da; cursor: text;' onclick='this.select()' contenteditable='false'>",
                    main_sentence,
                    "</div></div>",
                    if (nchar(overall_sentence) > 0) {
                        paste0(
                            "<div style='background-color: rgba(138, 155, 172, 0.06); border: 1px solid #dee2e6; padding: 12px; margin-bottom: 10px; border-radius: 4px; color: inherit;'>",
                            .("<h5 style='margin: 0 0 8px 0; font-size: 14px; color: inherit;'>Additional detail:</h5>"),
                            "<div style='font-family: Times, serif; font-size: 14px; line-height: 1.5; padding: 8px; background: rgba(138, 155, 172, 0.06); color: inherit; border: 1px solid #ced4da; cursor: text;' onclick='this.select()' contenteditable='false'>",
                            paste(method_sentence, overall_sentence),
                            "</div></div>"
                        )
                    } else {
                        ""
                    },
                    "<div style='background-color: rgba(138, 155, 172, 0.06); border: 1px solid #dee2e6; padding: 12px; margin-bottom: 10px; border-radius: 4px; color: inherit;'>",
                    .("<h5 style='margin: 0 0 8px 0; font-size: 14px; color: inherit;'>Clinical interpretation:</h5>"),
                    "<div style='font-family: Times, serif; font-size: 14px; line-height: 1.5; padding: 8px; background: rgba(138, 155, 172, 0.06); color: inherit; border: 1px solid #ced4da; cursor: text;' onclick='this.select()' contenteditable='false'>",
                    recommendation,
                    "</div></div>",
                    "<div style='background-color: rgba(255, 169, 33, 0.14); border: 1px solid #f57c00; padding: 10px; font-size: 12px; color: inherit;'>",
                    .("<strong>Tip:</strong> Combine these sentences and adapt the language to match your publication style. Consider adding information about rater training, case characteristics, or clinical context as appropriate."),
                    "</div></div>"
                )

                self$results$reportTemplate$setContent(report_html)
            },
            # Interpretation text follows the same Landis & Koch bands as the kappa
            # table. It used a third scheme (poor/fair/moderate/good/excellent) and
            # told users that kappa >= 0.8 was "suitable for all clinical
            # applications including critical diagnoses" - a claim the point estimate
            # cannot support without its interval, the category prevalence and the
            # clinical consequence of a disagreement.
            .getClinicalInterpretation = function(kappa) {
                if (is.na(kappa)) {
                    return(.("Could not calculate reliable agreement measure. Check your data for sufficient cases and category distribution."))
                }
                caution <- .("Kappa bands are descriptive conventions (Landis and Koch 1977), not clinical thresholds; judge adequacy from the confidence interval, the prevalence of each category and the consequence of a disagreement.")
                band <- if (kappa < 0) {
                    .("Agreement is <strong>worse than chance</strong>; raters systematically disagree. Review the rating criteria.")
                } else if (kappa <= 0.20) {
                    .("Agreement is <strong>slight</strong>; raters are close to making independent judgments.")
                } else if (kappa <= 0.40) {
                    .("Agreement is <strong>fair</strong>; there is some consistency but substantial disagreement remains.")
                } else if (kappa <= 0.60) {
                    .("Agreement is <strong>moderate</strong>.")
                } else if (kappa <= 0.80) {
                    .("Agreement is <strong>substantial</strong>.")
                } else {
                    .("Agreement is <strong>almost perfect</strong>.")
                }
                paste(band, caution)
            },
            .generateAboutAnalysis = function() {
                about_html <- paste0(
                    "<div style='font-family: Arial, sans-serif; max-width: 700px; line-height: 1.5;'>",
                    "<div style='background-color: rgba(88, 88, 88, 0.06); border: 2px solid #2e7d32; padding: 20px; margin-bottom: 20px; color: inherit;'>",
                    .("<h3 style='margin: 0 0 15px 0; color: #2e7d32; font-size: 18px;'>About inter-rater reliability analysis</h3>"),
                    .("<h4 style='margin: 15px 0 8px 0; color: inherit; font-size: 15px;'>What does this analysis do?</h4>"),
                    "<p style='margin: 0 0 12px 0; font-size: 14px;'>",
                    .("This analysis measures how consistently different raters (observers, pathologists, clinicians) evaluate the same cases. It supports assessment of diagnostic consistency, training effectiveness, and research reliability in clinical settings."),
                    "</p>",
                    .("<h4 style='margin: 15px 0 8px 0; color: inherit; font-size: 15px;'>When to use it?</h4>"),
                    "<ul style='margin: 0 0 12px 0; padding-left: 20px; font-size: 14px;'>",
                    .("<li>Validating diagnostic consistency between pathologists</li>"),
                    .("<li>Training new staff and measuring competency</li>"),
                    .("<li>Research studies requiring reliable measurements</li>"),
                    .("<li>Quality assurance in clinical laboratories</li>"),
                    .("<li>Establishing inter-institutional agreement</li>"),
                    "</ul>",
                    .("<h4 style='margin: 15px 0 8px 0; color: inherit; font-size: 15px;'>What you need:</h4>"),
                    "<ul style='margin: 0 0 12px 0; padding-left: 20px; font-size: 14px;'>",
                    .("<li><strong>2 or more rater variables</strong> - each representing ratings by different observers</li>"),
                    .("<li><strong>Same rating scale</strong> - all raters must use identical categories</li>"),
                    .("<li><strong>Planned sample size</strong> - base it on expected agreement, category prevalence, rater count, and target precision</li>"),
                    .("<li><strong>Independent ratings</strong> - raters should evaluate cases independently</li>"),
                    "</ul>",
                    .("<h4 style='margin: 15px 0 8px 0; color: inherit; font-size: 15px;'>Key outputs:</h4>"),
                    "<ul style='margin: 0; padding-left: 20px; font-size: 14px;'>",
                    .("<li><strong>Kappa coefficient (\u{03BA})</strong> - primary agreement measure corrected for chance</li>"),
                    .("<li><strong>95% confidence intervals</strong> - precision of the agreement estimate</li>"),
                    .("<li><strong>P-values</strong> - statistical significance testing</li>"),
                    .("<li><strong>Clinical interpretation</strong> - practical meaning of agreement levels</li>"),
                    .("<li><strong>Frequency tables and visualizations</strong> - detailed breakdown of agreement patterns</li>"),
                    "</ul>",
                    "</div></div>"
                )

                self$results$aboutAnalysis$setContent(about_html)
            },
            .generateAssumptions = function() {
                assumptions_html <- paste0(
                    "<div style='font-family: Arial, sans-serif; max-width: 700px; line-height: 1.5;'>",
                    "<div style='background-color: rgba(255, 169, 33, 0.14); border: 2px solid #f57c00; padding: 20px; color: inherit;'>",
                    .("<h3 style='margin: 0 0 15px 0; color: #f57c00; font-size: 18px;'>Assumptions and caveats</h3>"),
                    .("<h4 style='margin: 15px 0 8px 0; color: #d84315; font-size: 15px;'>Important assumptions</h4>"),
                    "<ul style='margin: 0 0 15px 0; padding-left: 20px; font-size: 14px;'>",
                    .("<li><strong>Independent ratings:</strong> Each rater evaluates cases without knowledge of other ratings</li>"),
                    .("<li><strong>Same rating scale:</strong> All raters use identical categories in the same order</li>"),
                    .("<li><strong>Representative sample:</strong> Cases should represent the typical population</li>"),
                    .("<li><strong>Stable conditions:</strong> Rating criteria remain consistent throughout the study</li>"),
                    "</ul>",
                    .("<h4 style='margin: 15px 0 8px 0; color: #d84315; font-size: 15px;'>Data requirements</h4>"),
                    "<ul style='margin: 0 0 15px 0; padding-left: 20px; font-size: 14px;'>",
                    .("<li><strong>Sample size:</strong> Plan from expected agreement, category prevalence, rater count, and target precision</li>"),
                    .("<li><strong>Category prevalence:</strong> Report rare categories and consider how prevalence affects each coefficient</li>"),
                    .("<li><strong>Complete data:</strong> Missing ratings reduce the analyzed sample</li>"),
                    .("<li><strong>Factor variables:</strong> Data must be properly coded as factors in your dataset</li>"),
                    "</ul>",
                    .("<h4 style='margin: 15px 0 8px 0; color: #d84315; font-size: 15px;'>Common pitfalls</h4>"),
                    "<ul style='margin: 0 0 15px 0; padding-left: 20px; font-size: 14px;'>",
                    .("<li><strong>Prevalence effects:</strong> Very rare or very common conditions can inflate or deflate kappa</li>"),
                    .("<li><strong>Bias effects:</strong> Systematic differences between raters reduce apparent agreement</li>"),
                    .("<li><strong>Training effects:</strong> Agreement may improve over time, affecting comparisons</li>"),
                    .("<li><strong>Case difficulty:</strong> Easy and difficult cases may show different agreement patterns</li>"),
                    "</ul>",
                    .("<h4 style='margin: 15px 0 8px 0; color: #d84315; font-size: 15px;'>Interpretation guidelines</h4>"),
                    "<div style='background-color: rgba(138, 138, 138, 0.06); padding: 12px; border-radius: 4px; font-size: 14px; color: inherit;'>",
                    .("<p style='margin: 0 0 8px 0;'><strong>Kappa interpretation (Landis & Koch, 1977):</strong></p>"),
                    "<ul style='margin: 0 0 8px 0; padding-left: 20px;'>",
                    .("<li>\u{03BA} &lt; 0.00: Poor agreement (worse than chance)</li>"),
                    .("<li>\u{03BA} 0.00-0.20: Slight agreement</li>"),
                    .("<li>\u{03BA} 0.21-0.40: Fair agreement</li>"),
                    .("<li>\u{03BA} 0.41-0.60: Moderate agreement</li>"),
                    .("<li>\u{03BA} 0.61-0.80: Substantial agreement</li>"),
                    .("<li>\u{03BA} 0.81-1.00: Almost perfect agreement</li>"),
                    "</ul>",
                    "<p style='margin: 0; color: inherit; font-style: italic;'>",
                    .("Note: The required agreement depends on the consequences of disagreement; the Landis and Koch bands are descriptive conventions."),
                    "</p>",
                    "</div></div></div>"
                )

                self$results$assumptions$setContent(assumptions_html)
            },

            # Generate weighted kappa guide
            .generateWeightedKappaGuide = function() {
                current_weighting <- self$options$wght
                weighting_label <- switch(
                    current_weighting,
                    "equal" = .("Linear/equal weighting"),
                    "squared" = .("Quadratic/squared weighting"),
                    .("Weighted kappa")
                )
                selected_badge <- paste0(
                    "<span style='background-color: rgba(33, 154, 37, 0.25); padding: 2px 6px; border-radius: 3px; color: inherit;'>",
                    .("Selected"),
                    "</span>"
                )
                linear_status <- if (current_weighting == "equal") selected_badge else .("Not selected")
                squared_status <- if (current_weighting == "squared") selected_badge else .("Not selected")

                guide_html <- paste0(
                    "<div style='font-family: Arial, sans-serif; max-width: 700px; line-height: 1.5;'>",
                    "<div style='background-color: rgba(33, 152, 239, 0.13); border-left: 4px solid #1976d2; padding: 15px; margin-bottom: 15px; color: inherit;'>",
                    .("<h3 style='margin: 0 0 10px 0; color: #1976d2; font-size: 18px;'>Weighted kappa guide</h3>"),
                    "<p style='margin: 0; font-size: 14px; color: inherit;'>",
                    jmvcore::format(
                        .("You selected <strong>{weighting}</strong>. The sections below explain when and why to use each weighting scheme."),
                        weighting = weighting_label
                    ),
                    "</p></div>",
                    "<div style='background-color: rgba(155, 155, 155, 0.06); padding: 15px; border-radius: 6px; margin-bottom: 15px; color: inherit;'>",
                    .("<h4 style='margin: 0 0 12px 0; color: #2e7d32; font-size: 16px;'>Weighting schemes explained</h4>"),
                    "<div style='margin-bottom: 15px;'>",
                    "<h5 style='margin: 0 0 8px 0; color: #1976d2; font-size: 14px;'>",
                    jmvcore::format(
                        .("Linear/equal weighting ({status})"),
                        status = linear_status
                    ),
                    "</h5>",
                    "<div style='font-size: 13px; margin-left: 15px;'>",
                    .("<p style='margin: 0 0 6px 0;'><strong>Formula:</strong> w = 1 - |i - j| / (k - 1)</p>"),
                    .("<p style='margin: 0 0 6px 0;'><strong>When to use:</strong></p>"),
                    "<ul style='margin: 0 0 8px 0; padding-left: 20px;'>",
                    .("<li>Ordinal data where categories are equally spaced</li>"),
                    .("<li>Each step of disagreement has equal clinical importance</li>"),
                    .("<li>Example: Pain scales (1-10), tumor grades (I, II, III, IV)</li>"),
                    "</ul>",
                    .("<p style='margin: 0; color: inherit;'><em>All adjacent disagreements are penalized equally</em></p>"),
                    "</div></div>",
                    "<div style='margin-bottom: 15px;'>",
                    "<h5 style='margin: 0 0 8px 0; color: #1976d2; font-size: 14px;'>",
                    jmvcore::format(
                        .("Quadratic/squared weighting ({status})"),
                        status = squared_status
                    ),
                    "</h5>",
                    "<div style='font-size: 13px; margin-left: 15px;'>",
                    .("<p style='margin: 0 0 6px 0;'><strong>Formula:</strong> w = 1 - [(i - j) / (k - 1)]\u{00B2}</p>"),
                    .("<p style='margin: 0 0 6px 0;'><strong>When to use:</strong></p>"),
                    "<ul style='margin: 0 0 8px 0; padding-left: 20px;'>",
                    .("<li>Large disagreements are disproportionately more serious</li>"),
                    .("<li>Clinical consequences increase more rapidly with distance</li>"),
                    .("<li>Example: Disease severity, when a mild-to-severe disagreement matters more than an adjacent-category disagreement</li>"),
                    "</ul>",
                    .("<p style='margin: 0; color: inherit;'><em>Distant disagreements receive substantially less agreement credit than adjacent disagreements</em></p>"),
                    "</div></div>",
                    "<div>",
                    .("<h5 style='margin: 0 0 8px 0; color: #1976d2; font-size: 14px;'>Unweighted kappa (standard)</h5>"),
                    "<div style='font-size: 13px; margin-left: 15px;'>",
                    .("<p style='margin: 0 0 6px 0;'><strong>When to use:</strong></p>"),
                    "<ul style='margin: 0 0 8px 0; padding-left: 20px;'>",
                    .("<li>Nominal (categorical) data with no natural ordering</li>"),
                    .("<li>All disagreements are equally important</li>"),
                    .("<li>Example: Diagnostic categories, present/absent classifications</li>"),
                    "</ul>",
                    .("<p style='margin: 0; color: inherit;'><em>All disagreements are treated equally, regardless of distance</em></p>"),
                    "</div></div>",
                    "</div>",
                    "<div style='background-color: rgba(255, 169, 33, 0.14); border-left: 4px solid #f57c00; padding: 15px; margin-bottom: 15px; color: inherit;'>",
                    .("<h4 style='margin: 0 0 10px 0; color: #e65100; font-size: 15px;'>Clinical decision guide</h4>"),
                    "<div style='font-size: 14px;'>",
                    .("<p style='margin: 0 0 8px 0;'><strong>Choose linear weighting when:</strong></p>"),
                    "<ul style='margin: 0 0 12px 0; padding-left: 20px;'>",
                    .("<li>Rating scales have ordered, approximately equal steps (e.g., a 1-5 rating scale)</li>"),
                    .("<li>Each one-category error has similar clinical importance</li>"),
                    .("<li>The weighting rule was specified before examining the results</li>"),
                    "</ul>",
                    .("<p style='margin: 0 0 8px 0;'><strong>Choose quadratic weighting when:</strong></p>"),
                    "<ul style='margin: 0 0 12px 0; padding-left: 20px;'>",
                    .("<li>Distant rating errors have disproportionately larger consequences</li>"),
                    .("<li>Treatment-response categories are ordered from complete response to progression</li>"),
                    .("<li>Risk strata are ordered from low to high risk</li>"),
                    "</ul>",
                    "<p style='margin: 0; font-weight: 500; color: #d84315;'>",
                    .("<em>Choose the weighting scheme before analysis from the clinical cost of each degree of disagreement, and report it explicitly.</em>"),
                    "</p>",
                    "</div></div>",
                    "<div style='background-color: rgba(153, 33, 170, 0.12); border-left: 4px solid #8e24aa; padding: 15px; color: inherit;'>",
                    .("<h4 style='margin: 0 0 10px 0; color: #6a1b9a; font-size: 15px;'>Interpretation impact</h4>"),
                    "<div style='font-size: 14px;'>",
                    .("<p style='margin: 0 0 8px 0;'>Weighting changes how disagreements contribute to kappa:</p>"),
                    "<ul style='margin: 0 0 8px 0; padding-left: 20px;'>",
                    .("<li>Near-category disagreements receive partial agreement credit</li>"),
                    .("<li>Quadratic weights give more credit to near-category disagreements than linear weights</li>"),
                    .("<li>A larger weighted estimate does not by itself establish clinical acceptability</li>"),
                    "</ul>",
                    "<p style='margin: 0; color: #6a1b9a; font-weight: 500;'>",
                    .("Always report which weighting scheme was used."),
                    "</p>",
                    "</div></div></div>"
                )

                self$results$weightedKappaGuide$setContent(guide_html)
            },
            .generateStatisticalGlossary = function() {
                glossary_html <- paste0(
                    "<div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.5;'>",
                    "<div style='background-color: rgba(88, 88, 88, 0.06); border: 2px solid #2e7d32; padding: 20px; margin-bottom: 20px; color: inherit;'>",
                    .("<h3 style='margin: 0 0 15px 0; color: #2e7d32; font-size: 18px;'>Statistical terms glossary</h3>"),
                    .("<p style='margin: 0; font-size: 14px; color: inherit;'>Quick reference for inter-rater reliability statistics and their interpretation</p>"),
                    "</div>",

                    # Cohen's Kappa
                    "<div style='border: 1px solid #e0e0e0; padding: 15px; margin-bottom: 15px; background-color: rgba(172, 172, 172, 0.06); color: inherit;'>",
                    .("<h4 style='margin: 0 0 10px 0; color: #1976d2; font-size: 16px;'>Cohen's kappa (\u{03BA})</h4>"),
                    .("<p style='margin: 0 0 8px 0; font-size: 14px;'><strong>What it measures:</strong> Agreement between exactly 2 raters, corrected for chance agreement</p>"),
                    .("<p style='margin: 0 0 8px 0; font-size: 14px;'><strong>Range:</strong> -1 to +1 (0 = chance agreement, 1 = perfect agreement)</p>"),
                    .("<p style='margin: 0 0 8px 0; font-size: 14px;'><strong>Clinical use:</strong> Comparing two pathologists, radiologists, or clinicians</p>"),
                    .("<p style='margin: 0; font-size: 14px; color: inherit;'><em>Formula: (observed - expected) / (1 - expected)</em></p>"),
                    "</div>",

                    # Fleiss' Kappa
                    "<div style='border: 1px solid #e0e0e0; padding: 15px; margin-bottom: 15px; background-color: rgba(172, 172, 172, 0.06); color: inherit;'>",
                    .("<h4 style='margin: 0 0 10px 0; color: #1976d2; font-size: 16px;'>Fleiss' kappa</h4>"),
                    .("<p style='margin: 0 0 8px 0; font-size: 14px;'><strong>What it measures:</strong> Agreement among 3 or more raters, extending Cohen's kappa</p>"),
                    .("<p style='margin: 0 0 8px 0; font-size: 14px;'><strong>Range:</strong> -1 to +1 (same interpretation as Cohen's kappa)</p>"),
                    .("<p style='margin: 0 0 8px 0; font-size: 14px;'><strong>Clinical use:</strong> Multi-center studies, consensus panels, training assessment</p>"),
                    .("<p style='margin: 0; font-size: 14px; color: inherit;'><em>This implementation analyzes complete cases rated by the selected raters</em></p>"),
                    "</div>",

                    # Krippendorff's Alpha
                    "<div style='border: 1px solid #e0e0e0; padding: 15px; margin-bottom: 15px; background-color: rgba(172, 172, 172, 0.06); color: inherit;'>",
                    .("<h4 style='margin: 0 0 10px 0; color: #1976d2; font-size: 16px;'>Krippendorff's alpha (\u{03B1})</h4>"),
                    .("<p style='margin: 0 0 8px 0; font-size: 14px;'><strong>What it measures:</strong> Reliability using nominal, ordinal, interval, or ratio disagreement functions</p>"),
                    .("<p style='margin: 0 0 8px 0; font-size: 14px;'><strong>Range:</strong> Values can be negative; 0 indicates chance-level agreement and 1 indicates perfect agreement</p>"),
                    .("<p style='margin: 0 0 8px 0; font-size: 14px;'><strong>Data used here:</strong> Factor ratings; most analyses use complete rows, while Krippendorff's alpha also uses cases with at least two observed ratings</p>"),
                    .("<p style='margin: 0; font-size: 14px; color: inherit;'><em>The selected disagreement function must match the measurement scale</em></p>"),
                    "</div>",

                    # ICC
                    "<div style='border: 1px solid #e0e0e0; padding: 15px; margin-bottom: 15px; background-color: rgba(172, 172, 172, 0.06); color: inherit;'>",
                    .("<h4 style='margin: 0 0 10px 0; color: #1976d2; font-size: 16px;'>Intraclass correlation coefficient (ICC)</h4>"),
                    .("<p style='margin: 0 0 8px 0; font-size: 14px;'><strong>What it measures:</strong> ICC(2,1), two-way random-effects absolute agreement for a single rater</p>"),
                    .("<p style='margin: 0 0 8px 0; font-size: 14px;'><strong>Range:</strong> Estimates can be negative; larger values indicate greater reliability</p>"),
                    .("<p style='margin: 0 0 8px 0; font-size: 14px;'><strong>Data used here:</strong> Numeric level codes from rating variables declared as ordered factors</p>"),
                    .("<p style='margin: 0; font-size: 14px; color: inherit;'><em>Nominal and continuous variables are not accepted by this analysis</em></p>"),
                    "</div>",

                    # PABAK
                    "<div style='border: 1px solid #e0e0e0; padding: 15px; margin-bottom: 15px; background-color: rgba(172, 172, 172, 0.06); color: inherit;'>",
                    .("<h4 style='margin: 0 0 10px 0; color: #1976d2; font-size: 16px;'>PABAK (prevalence-adjusted bias-adjusted kappa)</h4>"),
                    .("<p style='margin: 0 0 8px 0; font-size: 14px;'><strong>What it measures:</strong> Brennan-Prediger agreement using an equal-probability chance model</p>"),
                    .("<p style='margin: 0 0 8px 0; font-size: 14px;'><strong>Range:</strong> -1 to +1; the estimate may be higher or lower than standard kappa</p>"),
                    .("<p style='margin: 0 0 8px 0; font-size: 14px;'><strong>Clinical use:</strong> A sensitivity analysis when prevalence affects standard kappa</p>"),
                    .("<p style='margin: 0; font-size: 14px; color: inherit;'><em>Report the coefficient and its chance model rather than treating it as a prevalence-free truth</em></p>"),
                    "</div>",

                    # Gwet's AC
                    "<div style='border: 1px solid #e0e0e0; padding: 15px; margin-bottom: 15px; background-color: rgba(172, 172, 172, 0.06); color: inherit;'>",
                    .("<h4 style='margin: 0 0 10px 0; color: #1976d2; font-size: 16px;'>Gwet's agreement coefficients (AC1/AC2)</h4>"),
                    .("<p style='margin: 0 0 8px 0; font-size: 14px;'><strong>What it measures:</strong> Chance-corrected agreement that is less sensitive to prevalence than kappa</p>"),
                    .("<p style='margin: 0 0 8px 0; font-size: 14px;'><strong>Range:</strong> -1 to +1; values can differ materially from kappa</p>"),
                    .("<p style='margin: 0 0 8px 0; font-size: 14px;'><strong>Use:</strong> A complementary coefficient when category prevalence is strongly imbalanced</p>"),
                    .("<p style='margin: 0; font-size: 14px; color: inherit;'><em>AC1 is nominal; AC2 applies disagreement weights for ordinal ratings</em></p>"),
                    "</div>",

                    # Interpretation Guide
                    "<div style='background-color: rgba(33, 159, 33, 0.1); border: 1px solid #4caf50; padding: 15px; margin-bottom: 15px; color: inherit;'>",
                    .("<h4 style='margin: 0 0 10px 0; color: #2e7d32; font-size: 16px;'>Interpretation guidelines</h4>"),
                    "<div style='font-size: 14px;'>",
                    "<table style='width: 100%; border-collapse: collapse; margin: 10px 0;'>",
                    .("<tr style='background-color: rgba(88, 88, 88, 0.06); color: inherit;'><th style='padding: 8px; border: 1px solid #ddd; text-align: left;'>Kappa/alpha value</th><th style='padding: 8px; border: 1px solid #ddd; text-align: left;'>Convention label</th><th style='padding: 8px; border: 1px solid #ddd; text-align: left;'>Interpretation note</th></tr>"),
                    .("<tr><td style='padding: 8px; border: 1px solid #ddd;'>&lt; 0.00</td><td style='padding: 8px; border: 1px solid #ddd;'>Poor (worse than chance)</td><td style='padding: 8px; border: 1px solid #ddd;'>Observed agreement is below the chance expectation</td></tr>"),
                    .("<tr><td style='padding: 8px; border: 1px solid #ddd;'>0.00 - 0.20</td><td style='padding: 8px; border: 1px solid #ddd;'>Slight agreement</td><td style='padding: 8px; border: 1px solid #ddd;'>Interpret with the confidence interval, prevalence, and consequences</td></tr>"),
                    .("<tr><td style='padding: 8px; border: 1px solid #ddd;'>0.21 - 0.40</td><td style='padding: 8px; border: 1px solid #ddd;'>Fair agreement</td><td style='padding: 8px; border: 1px solid #ddd;'>Interpret with the confidence interval, prevalence, and consequences</td></tr>"),
                    .("<tr><td style='padding: 8px; border: 1px solid #ddd;'>0.41 - 0.60</td><td style='padding: 8px; border: 1px solid #ddd;'>Moderate agreement</td><td style='padding: 8px; border: 1px solid #ddd;'>Interpret with the confidence interval, prevalence, and consequences</td></tr>"),
                    .("<tr><td style='padding: 8px; border: 1px solid #ddd;'>0.61 - 0.80</td><td style='padding: 8px; border: 1px solid #ddd;'>Substantial agreement</td><td style='padding: 8px; border: 1px solid #ddd;'>Interpret with the confidence interval, prevalence, and consequences</td></tr>"),
                    .("<tr><td style='padding: 8px; border: 1px solid #ddd;'>0.81 - 1.00</td><td style='padding: 8px; border: 1px solid #ddd;'>Almost perfect agreement</td><td style='padding: 8px; border: 1px solid #ddd;'>Interpret with the confidence interval, prevalence, and consequences</td></tr>"),
                    "</table>",
                    "</div></div>",

                    # Tips
                    "<div style='background-color: rgba(255, 169, 33, 0.14); border: 1px solid #f57c00; padding: 15px; color: inherit;'>",
                    .("<h4 style='margin: 0 0 10px 0; color: #e65100; font-size: 15px;'>Practical tips</h4>"),
                    "<ul style='margin: 0; padding-left: 20px; font-size: 14px;'>",
                    .("<li><strong>Sample size:</strong> Plan from expected agreement, category prevalence, rater count, and target precision</li>"),
                    .("<li><strong>Category prevalence:</strong> Report rare categories and examine prevalence-sensitive and prevalence-robust coefficients</li>"),
                    .("<li><strong>Missing data:</strong> Overall multi-rater analyses use jointly complete rows; pairwise Cohen analyses use cases observed for each pair, and Krippendorff's alpha uses cases with at least two observed ratings</li>"),
                    .("<li><strong>ICC here:</strong> Available only for ordered-factor ratings and computed from their numeric level codes</li>"),
                    .("<li><strong>Reporting:</strong> Include confidence intervals when available</li>"),
                    .("<li><strong>Clinical context:</strong> Consider consequences of disagreement in interpretation</li>"),
                    "</ul></div></div>"
                )

                self$results$statisticalGlossary$setContent(glossary_html)
            },
            .calculateAgreementMatrixOptimized = function(data_matrix, rater_names) {
                # Optimized agreement matrix calculation for large datasets
                n_raters <- ncol(data_matrix)

                # Initialize matrix
                agreement_matrix <- matrix(1.0,
                    nrow = n_raters, ncol = n_raters,
                    dimnames = list(rater_names, rater_names)
                )

                # Pre-allocate logical matrices for comparison
                # This approach reduces memory allocation overhead
                for (i in 1:(n_raters - 1)) {
                    # Vectorized comparison for current rater against all subsequent raters
                    current_ratings <- data_matrix[, i]

                    for (j in (i + 1):n_raters) {
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
                gwet_table <- self$results$gwetACTable

                tryCatch(
                    {
                        # Calculate AC1 (first-order agreement coefficient)
                        # AC1 is less sensitive to trait prevalence than kappa
                        ac1_result <- private$.calculateGwetAC1()

                        # Calculate AC2 (second-order agreement coefficient)
                        # AC2 applies ordinal weights to AC1
                        ac2_result <- private$.calculateGwetAC2()

                        # Add AC1 result
                        gwet_table$setRow(rowKey = "AC1", values = list(
                            coefficient = .("Gwet's AC1"),
                            value = ac1_result$estimate,
                            se = ac1_result$se,
                            ci_lower = ac1_result$ci_lower,
                            ci_upper = ac1_result$ci_upper,
                            interpretation = private$.interpretGwetAC(ac1_result$estimate, "AC1")
                        ))

                        # Add AC2 result
                        gwet_table$setRow(rowKey = "AC2", values = list(
                            coefficient = ac2_result$label,
                            value = ac2_result$estimate,
                            se = ac2_result$se,
                            ci_lower = ac2_result$ci_lower,
                            ci_upper = ac2_result$ci_upper,
                            interpretation = private$.interpretGwetAC(ac2_result$estimate, if (isTRUE(ac2_result$weighted)) "AC2" else "AC2-unweighted")
                        ))

                    },
                    error = function(e) {
                        error_msg <- jmvcore::format(
                            .("Error calculating Gwet's coefficients: {error}. Ensure the ratings have adequate variability and complete cases."),
                            error = e$message
                        )
                        private$.accumulateMessage(error_msg, severity = "error")
                    }
                )
            },

            # Gwet's agreement coefficients.
            #
            # The previous hand-rolled implementation was not AC1/AC2 at all:
            # it used Pe = 1/k (that is Bennett's S / Brennan-Prediger) for "AC1"
            # and Pe = sum(p_k^2) (that is Scott's pi) for "AC2", derived k from
            # as.vector(<data.frame>) - which returns the LIST OF COLUMNS, so k
            # was the rater count, not the category count - and paired both with
            # an ad-hoc SE and a hardcoded 1.96 multiplier. irrCAC (already in
            # Imports) implements Gwet (2008) exactly: AC1 is the unweighted
            # coefficient, AC2 the weighted one, and it returns the published
            # standard error.
            .gwetCoefficient = function(weights) {
                res <- irrCAC::gwet.ac1.raw(private$.data_matrix, weights = weights, categ.labels = private$.categoryLabels())$est
                est <- as.numeric(res$coeff.val)
                se <- as.numeric(res$coeff.se)
                # irrCAC estimates uncertainty from case-level influence values
                # and uses a t critical value with n - 1 degrees of freedom.
                zc <- stats::qt(0.975, df = private$.n_cases - 1)
                list(
                    estimate = est,
                    se = se,
                    ci_lower = max(-1, est - zc * se),
                    ci_upper = min(1, est + zc * se)
                )
            },
            .calculateGwetAC1 = function() {
                private$.gwetCoefficient("unweighted")
            },

            # AC2 is the WEIGHTED AC1. Ordinal weights are only meaningful when
            # the raters are ordered factors; on a nominal scale AC2 collapses to
            # AC1, so return the unweighted coefficient rather than inventing an
            # order.
            .calculateGwetAC2 = function() {
                is_ordinal <- private$.commonOrdinalScale(private$.data_matrix)
                out <- private$.gwetCoefficient(if (is_ordinal) "ordinal" else "unweighted")
                out$label <- if (is_ordinal) .("Gwet's AC2 (ordinal weights)") else .("Gwet's AC2 (unweighted; equals AC1 on a nominal scale)")
                out$weighted <- is_ordinal
                out
            },

            .interpretGwetAC = function(coefficient_value, type = "AC1") {
                if (is.na(coefficient_value) || !is.numeric(coefficient_value)) {
                    return(.("Cannot interpret an invalid coefficient value."))
                }

                # Gwet's interpretation guidelines (similar to kappa but more robust)
                interpretation <- if (coefficient_value < 0) {
                    .("Poor agreement (worse than chance)")
                } else if (coefficient_value < 0.20) {
                    .("Slight agreement")
                } else if (coefficient_value < 0.40) {
                    .("Fair agreement")
                } else if (coefficient_value < 0.60) {
                    .("Moderate agreement")
                } else if (coefficient_value < 0.80) {
                    .("Substantial agreement")
                } else {
                    .("Almost perfect agreement")
                }

                # Add coefficient-specific notes
                # AC2 is AC1 with ordinal weights; it says nothing about rater
                # heterogeneity, which the old note claimed.
                switch(type,
                    "AC1" = jmvcore::format(
                        .("{interpretation} (less affected by prevalence than kappa)"),
                        interpretation = interpretation
                    ),
                    "AC2" = jmvcore::format(
                        .("{interpretation} (ordinal weights give partial credit for near-miss ratings)"),
                        interpretation = interpretation
                    ),
                    jmvcore::format(
                        .("{interpretation} (unweighted: identical to AC1 for nominal ratings)"),
                        interpretation = interpretation
                    )
                )
            },

            # PABAK Analysis (Prevalence-Adjusted Bias-Adjusted Kappa)
            .performPABAKAnalysis = function() {
                pabak_table <- self$results$pabakTable

                tryCatch(
                    {
                        # Calculate standard kappa first
                        standard_kappa <- private$.calculateStandardKappa()

                        # Calculate PABAK
                        pabak_result <- private$.calculatePABAK()

                        # Add results to table
                        pabak_table$setRow(rowKey = "overall", values = list(
                            measure = .("Pairwise agreement (Po)"),
                            value = pabak_result$observed_agreement,
                            standard_kappa = standard_kappa$kappa,
                            adjusted_kappa = pabak_result$pabak,
                            interpretation = private$.interpretPABAK(pabak_result$pabak, standard_kappa$kappa)
                        ))

                    },
                    error = function(e) {
                        error_msg <- jmvcore::format(
                            .("Error calculating PABAK: {error}. Ensure at least two raters and complete ratings are available."),
                            error = e$message
                        )
                        private$.accumulateMessage(error_msg, severity = "error")
                    }
                )
            },

            # Calculate PABAK coefficient
            # PABAK / Brennan-Prediger.
            #
            # PABAK = 2*Po - 1 is the TWO-CATEGORY special case. The general
            # prevalence-and-bias-adjusted coefficient is (q*Po - 1)/(q - 1) for
            # q categories, so the old formula understated agreement on every
            # multi-category pathology scale (3-grade dysplasia, 4-tier Gleason,
            # ...). The old code also silently used only the FIRST TWO raters
            # while labelling the row "Overall Agreement". irrCAC::bp.coeff.raw
            # is the published Brennan-Prediger estimator and handles any number
            # of raters and categories, with a real standard error.
            .calculatePABAK = function() {
                if (ncol(private$.data_matrix) < 2) {
                    jmvcore::reject(.("PABAK requires at least 2 raters."))
                }
                res <- irrCAC::bp.coeff.raw(private$.data_matrix)$est
                list(
                    observed_agreement = as.numeric(res$pa),
                    pabak = as.numeric(res$coeff.val),
                    se = as.numeric(res$coeff.se)
                )
            },

            # Unadjusted kappa, shown next to PABAK so the user can see how much
            # of the coefficient is prevalence/bias.
            #
            # cbind(<factor>, <factor>) used to be passed to irr::kappa2; cbind
            # drops the factors to their INTEGER LEVEL CODES, so two raters with
            # different level sets were compared on codes that meant different
            # things. Keep the data frame (kappa2 then uses the factor levels)
            # and match the estimator to the rater count, since PABAK itself now
            # covers all raters.
            .calculateStandardKappa = function() {
                data_matrix <- private$.data_matrix
                if (ncol(data_matrix) < 2) {
                    return(list(kappa = NaN))
                }
                kappa_result <- tryCatch(
                    if (ncol(data_matrix) == 2) {
                        irr::kappa2(data_matrix[, 1:2, drop = FALSE])
                    } else {
                        irr::kappam.fleiss(data_matrix)
                    },
                    error = function(e) list(value = NaN)
                )
                list(kappa = kappa_result$value)
            },

            # Interpret PABAK vs standard kappa
            .interpretPABAK = function(pabak_value, standard_kappa) {
                if (is.na(pabak_value)) {
                    return(.("PABAK could not be calculated."))
                }

                base_interpretation <- if (pabak_value < 0) {
                    .("Poor agreement")
                } else if (pabak_value < 0.20) {
                    .("Slight agreement")
                } else if (pabak_value < 0.40) {
                    .("Fair agreement")
                } else if (pabak_value < 0.60) {
                    .("Moderate agreement")
                } else if (pabak_value < 0.80) {
                    .("Substantial agreement")
                } else {
                    .("Almost perfect agreement")
                }

                # Compare with standard kappa if available
                if (!is.na(standard_kappa)) {
                    difference <- pabak_value - standard_kappa
                    return(if (abs(difference) < 0.05) {
                        jmvcore::format(
                            .("{interpretation} (similar to standard kappa)"),
                            interpretation = base_interpretation
                        )
                    } else if (difference > 0.05) {
                        jmvcore::format(
                            .("{interpretation} (higher than standard kappa; prevalence bias may be present)"),
                            interpretation = base_interpretation
                        )
                    } else {
                        jmvcore::format(
                            .("{interpretation} (lower than standard kappa; review the category distribution)"),
                            interpretation = base_interpretation
                        )
                    })
                }

                return(base_interpretation)
            },

            # Sample Size Planning for Agreement Studies
            .performSampleSizePlanning = function() {
                sample_size_table <- self$results$sampleSizeTable
                target_kappa <- self$options$targetKappa
                target_precision <- self$options$targetPrecision

                tryCatch(
                    {
                        # Calculate required sample size for different scenarios
                        sample_sizes <- private$.calculateSampleSizeRequirements(target_kappa, target_precision)

                        # Add results to table
                        for (scenario in names(sample_sizes)) {
                            sample_size_table$setRow(rowKey = scenario, values = list(
                                parameter = scenario,
                                value = sample_sizes[[scenario]]$n_required,
                                recommendation = sample_sizes[[scenario]]$recommendation
                            ))
                        }

                        sample_size_table$setNote("method", .("Cases needed so the 95% confidence interval for kappa has the target half-width (Rotondi and Donner 2012, kappaSize), assuming the category proportions observed in these data. Supported for 2 to 6 raters."))
                    },
                    error = function(e) {
                        error_msg <- jmvcore::format(
                            .("Error in sample size planning: {error}. Check that the target kappa and precision are within their allowed ranges."),
                            error = e$message
                        )
                        private$.accumulateMessage(error_msg, severity = "error")
                    }
                )
            },

            # Calculate sample size requirements
            # Precision-based sample size for kappa (Rotondi & Donner 2012, kappaSize).
            #
            # The previous formula, (z_a + z_b)^2 * k(1 - k) / precision^2, treated
            # kappa as a binomial proportion, put a power term into a precision
            # problem, ignored the category proportions, divided by 3 "for 3 raters"
            # and multiplied by 1.2 "for multiple testing". For kappa 0.8 +/- 0.1 it
            # gave 126 and 42 cases where 196-534 and 120-358 are needed.
            .calculateSampleSizeRequirements = function(target_kappa, target_precision) {
                labels <- private$.categoryLabels()
                values <- unlist(lapply(private$.data_matrix, as.character), use.names = FALSE)
                props <- as.numeric(table(factor(values, levels = labels))) / length(values)
                n_categories <- length(props)
                kappa0 <- target_kappa
                lower <- kappa0 - target_precision
                upper <- kappa0 + target_precision
                prop_text <- paste(sprintf("%s %.0f%%", labels, 100 * props), collapse = ", ")

                size_fun <- switch(as.character(n_categories),
                    "2" = kappaSize::CIBinary,
                    "3" = kappaSize::CI3Cats,
                    "4" = kappaSize::CI4Cats,
                    "5" = kappaSize::CI5Cats,
                    NULL
                )
                results <- list()
                if (is.null(size_fun)) {
                    results[[.("Not available")]] <- list(
                        n_required = "-",
                        recommendation = jmvcore::format(
                            .("Sample size planning supports 2 to 5 rating categories; these data have {categories}."),
                            categories = n_categories
                        )
                    )
                    return(results)
                }

                rater_counts <- sort(unique(c(2, 3, min(max(private$.n_raters, 2), 6))))
                for (raters in rater_counts) {
                    n <- tryCatch(
                        size_fun(
                            kappa0 = kappa0, kappaL = lower, kappaU = upper,
                            props = if (n_categories == 2) props[1] else props,
                            raters = raters, alpha = 0.05
                        )$n,
                        error = function(e) NA
                    )
                    results[[jmvcore::format(.("{count} raters"), count = raters)]] <- list(
                        n_required = if (is.na(n)) "-" else jmvcore::format(.("{cases} cases"), cases = n),
                        recommendation = jmvcore::format(
                            .("95% CI for kappa {kappa}: {lower} to {upper}; category proportions {props}"),
                            kappa = kappa0, lower = round(lower, 3), upper = round(upper, 3), props = prop_text
                        )
                    )
                }
                results
            },

            # Rater Bias Detection Analysis
            .performRaterBiasAnalysis = function() {
                bias_table <- self$results$raterBiasTable

                tryCatch(
                    {
                        bias_results <- private$.detectRaterBias()

                        for (rater_name in names(bias_results)) {
                            bias_info <- bias_results[[rater_name]]

                            bias_table$setRow(rowKey = rater_name, values = list(
                                rater = rater_name,
                                bias_score = bias_info$bias_score,
                                tendency = bias_info$tendency,
                                severity = bias_info$severity,
                                recommendation = bias_info$recommendation
                            ))
                        }

                    },
                    error = function(e) {
                        error_msg <- jmvcore::format(
                            .("Error in bias analysis: {error}. Ensure multiple raters use more than one rating category."),
                            error = e$message
                        )
                        private$.accumulateMessage(error_msg, severity = "error")
                    }
                )
            },

            # Detect systematic biases in rater behavior
            # Each rater compared with the OTHER raters.
            #
            # The modal rating used to include the rater's own vote, so raters partly
            # agreed with themselves, and with 2 raters the "consensus" on a split case
            # was whichever label table() listed first - one rater of a pair always
            # looked unbiased. A rater now agrees with a case when their rating is among
            # the most common ratings of the other raters.
            .detectRaterBias = function() {
                rater_names <- private$.rater_names
                labels <- as.data.frame(lapply(private$.data_matrix, as.character), stringsAsFactors = FALSE)
                results <- list()
                for (i in seq_along(rater_names)) {
                    own <- labels[[i]]
                    others <- labels[, -i, drop = FALSE]
                    agree <- rep(NA, length(own))
                    others_mode <- rep(NA_character_, length(own))
                    for (u in seq_along(own)) {
                        v <- unlist(others[u, ], use.names = FALSE)
                        v <- v[!is.na(v)]
                        if (is.na(own[u]) || length(v) == 0) {
                            next
                        }
                        counts <- table(v)
                        modes <- names(counts)[counts == max(counts)]
                        agree[u] <- own[u] %in% modes
                        if (length(modes) == 1) {
                            others_mode[u] <- modes
                        }
                    }
                    ok <- !is.na(agree)
                    if (!any(ok)) {
                        next
                    }
                    score <- 1 - mean(agree[ok])
                    keep <- ok & !is.na(others_mode)
                    tendency <- if (any(keep)) {
                        private$.identifyRaterTendency(table(own[keep]), table(others_mode[keep]))
                    } else {
                        .("No clear systematic tendency")
                    }
                    severity <- if (score < 0.1) {
                        .("Minimal disagreement")
                    } else if (score < 0.2) {
                        .("Mild disagreement")
                    } else if (score < 0.3) {
                        .("Moderate disagreement")
                    } else {
                        .("Marked disagreement")
                    }
                    results[[rater_names[i]]] <- list(
                        bias_score = score,
                        tendency = tendency,
                        severity = severity,
                        recommendation = private$.generateBiasRecommendation(score, tendency)
                    )
                }
                results
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
                    return(.("No clear systematic tendency"))
                } else if (max_diff_value > 0) {
                    return(jmvcore::format(
                        .("Over-diagnoses '{category}'"), category = max_diff_category
                    ))
                } else {
                    return(jmvcore::format(
                        .("Under-diagnoses '{category}'"), category = max_diff_category
                    ))
                }
            },

            # Generate bias-specific recommendations
            .generateBiasRecommendation = function(bias_score, tendency) {
                if (bias_score < 0.1) {
                    return(.("Excellent consistency with consensus"))
                } else if (bias_score < 0.2) {
                    return(jmvcore::format(
                        .("Minor calibration may be needed. {tendency}"), tendency = tendency
                    ))
                } else if (bias_score < 0.3) {
                    return(jmvcore::format(
                        .("Consider targeted training. {tendency}"), tendency = tendency
                    ))
                } else {
                    return(jmvcore::format(
                        .("Substantial recalibration may be needed. {tendency}"), tendency = tendency
                    ))
                }
            },

            # Agreement Trend Analysis
            .performAgreementTrendAnalysis = function() {
                trend_table <- self$results$agreementTrendTable

                tryCatch(
                    {
                        sequence_var <- self$options$sequenceVariable
                        if (!is.null(sequence_var)) {
                            sequence_values <- self$data[[sequence_var]][private$.keptRows()]
                            if (anyNA(sequence_values)) {
                                private$.accumulateMessage(jmvcore::format(
                                    .("The trend sequence variable '{variable}' has missing values. Those cases were placed after cases with observed sequence values, while preserving their dataset order."),
                                    variable = sequence_var
                                ))
                            }
                        }
                        trend_results <- private$.analyzeAgreementTrends()

                        for (group_name in names(trend_results)) {
                            group_info <- trend_results[[group_name]]

                            trend_table$setRow(rowKey = group_name, values = list(
                                sequence_group = group_name,
                                agreement_percent = group_info$agreement_percent,
                                kappa = group_info$kappa,
                                trend_direction = group_info$trend_direction,
                                significance = group_info$significance
                            ))
                        }

                        if (is.null(sequence_var)) {
                            trend_table$setNote(
                                "order",
                                .("No trend sequence variable was selected; the current dataset row order was used.")
                            )
                        } else {
                            trend_table$setNote("order", jmvcore::format(
                                .("Cases were ordered by '{variable}'; ties preserve their dataset order."),
                                variable = sequence_var
                            ))
                        }
                        trend_table$setNote(
                            "scope",
                            .("This trend is descriptive. It cannot distinguish learning or fatigue from changes in case mix, difficulty, or other factors across the sequence.")
                        )
                    },
                    error = function(e) {
                        error_msg <- jmvcore::format(
                            .("Error in trend analysis: {error}. Ensure the rows are in the intended case order and contain enough complete ratings."),
                            error = e$message
                        )
                        private$.accumulateMessage(error_msg, severity = "error")
                    }
                )
            },

            # Analyze agreement trends over case sequences
            .analyzeAgreementTrends = function() {
                data_matrix <- private$.data_matrix[private$.trendOrder(), , drop = FALSE]
                n_cases <- nrow(data_matrix)

                # Divide cases into sequential groups for trend analysis
                group_size <- max(10, floor(n_cases / 5)) # At least 10 cases per group, max 5 groups
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

                    group_name <- jmvcore::format(
                        .("Cases {start}-{end}"), start = start_idx, end = end_idx
                    )

                    results[[group_name]] <- list(
                        agreement_percent = group_agreement$percent,
                        kappa = group_kappa,
                        trend_direction = .("Insufficient data"),
                        significance = .("Cannot assess")
                    )
                }

                # Calculate overall trend
                if (n_cases >= 10) {
                    trend_test <- private$.calculateTrendSignificance(data_matrix)

                    # Update trend information for all groups
                    for (group_name in names(results)) {
                        results[[group_name]]$trend_direction <- trend_test$direction
                        results[[group_name]]$significance <- trend_test$significance
                    }
                }

                return(results)
            },

            .trendOrder = function() {
                n <- nrow(private$.data_matrix)
                sequence_var <- self$options$sequenceVariable
                if (is.null(sequence_var) || !sequence_var %in% names(self$data)) {
                    return(seq_len(n))
                }
                values <- self$data[[sequence_var]][private$.keptRows()]
                # A second key makes ties deterministic and preserves their order
                # in the analysed dataset. Missing values are explicitly placed last.
                order(values, seq_along(values), na.last = TRUE)
            },

            # Calculate agreement for a subset of data
            .calculateGroupAgreement = function(group_data) {
                total_comparisons <- 0
                total_agreements <- 0

                n_cases <- nrow(group_data)

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
                # Kappa over ALL raters, matching the group's agreement column. This
                # used only raters 1-2 without saying so - a group could show 79%
                # all-rater agreement beside kappa = 1.00 - and passed
                # cbind(<factor>, <factor>), i.e. integer level codes, to kappa2.
                ok <- stats::complete.cases(group_data)
                if (ncol(group_data) < 2 || sum(ok) < 5) {
                    return(NA_real_)
                }
                gd <- group_data[ok, , drop = FALSE]
                tryCatch(
                    if (ncol(gd) == 2) irr::kappa2(gd)$value else irr::kappam.fleiss(gd)$value,
                    error = function(e) NA_real_
                )
            },

            # Test for significant trend in agreement over time
            .calculateTrendSignificance = function(data_matrix) {
                # A real test on CASE-level agreement. This labelled the correlation
                # of 3-5 group means "Improving agreement" / "Moderate trend" by |r|
                # alone - no test; with 5 points r = 0.4 has p ~ 0.5.
                per_case <- vapply(seq_len(nrow(data_matrix)), function(i) {
                    v <- private$.rowLabels(data_matrix, i)
                    v <- v[!is.na(v)]
                    if (length(v) < 2) {
                        return(NA_real_)
                    }
                    counts <- table(v)
                    sum(counts * (counts - 1)) / (length(v) * (length(v) - 1))
                }, numeric(1))
                ok <- !is.na(per_case)
                if (sum(ok) < 10 || stats::sd(per_case[ok]) == 0) {
                    return(list(direction = .("Insufficient data"), significance = .("Cannot assess")))
                }
                ct <- suppressWarnings(stats::cor.test(which(ok), per_case[ok], method = "spearman", exact = FALSE))
                rho <- unname(ct$estimate)
                direction <- if (ct$p.value >= 0.05) {
                    .("No significant trend")
                } else if (rho > 0) {
                    .("Improving agreement")
                } else {
                    .("Declining agreement")
                }
                list(
                    direction = direction,
                    significance = jmvcore::format(
                        .("Spearman rho = {rho}, p = {pvalue}, across the ordered case sequence"),
                        rho = sprintf("%.2f", rho), pvalue = format.pval(ct$p.value, digits = 2)
                    )
                )
            },

            # Case Difficulty Analysis
            .performCaseDifficultyAnalysis = function() {
                difficulty_table <- self$results$caseDifficultyTable

                tryCatch(
                    {
                        difficulty_results <- private$.analyzeCaseDifficulty()

                        for (i in seq_along(difficulty_results)) {
                            case_id <- names(difficulty_results)[i]
                            case_info <- difficulty_results[[i]]

                            difficulty_table$setRow(rowKey = i, values = list(
                                case_id = case_id,
                                difficulty_score = case_info$difficulty_score,
                                disagreement_pattern = case_info$disagreement_pattern,
                                difficulty_level = case_info$difficulty_level,
                                rater_variability = case_info$rater_variability
                            ))
                        }

                    },
                    error = function(e) {
                        error_msg <- jmvcore::format(
                            .("Error in difficulty analysis: {error}. Ensure multiple raters and complete case ratings are available."),
                            error = e$message
                        )
                        private$.accumulateMessage(error_msg, severity = "error")
                    }
                )
            },

            # Analyze inherent case difficulty based on rater disagreement
            .analyzeCaseDifficulty = function() {
                data_matrix <- private$.data_matrix
                n_cases <- nrow(data_matrix)

                results <- list()

                k_scale <- length(private$.categoryLabels())
                for (case_idx in seq_len(n_cases)) {
                    case_ratings <- data_matrix[case_idx, ]
                    valid_ratings <- case_ratings[!is.na(case_ratings)]

                    if (length(valid_ratings) >= 2) {
                        case_id <- private$.caseLabels()[case_idx]

                        # Calculate difficulty metrics
                        difficulty_metrics <- private$.calculateCaseDifficultyMetrics(valid_ratings, k_scale)

                        results[[length(results) + 1L]] <- difficulty_metrics
                        names(results)[length(results)] <- case_id
                    }
                }

                return(results)
            },

            # Calculate difficulty metrics for a single case
            # The level used to come from rating entropy normalised by the categories
            # USED IN THAT CASE, cut at 0.2/0.4/0.7. Entropy reacts strongly to a single
            # dissent: 9 of 10 raters agreeing scored 0.47 and was labelled "Difficult
            # (significant disagreement)" beside the pattern "Mostly agreement with
            # some dissent". Score and level now come from the share of raters choosing
            # the modal rating; entropy, normalised by the whole scale, is kept as the
            # variability column.
            .calculateCaseDifficultyMetrics = function(ratings, k_scale) {
                rating_table <- table(ratings)
                modal_share <- max(rating_table) / sum(rating_table)
                props <- rating_table / sum(rating_table)
                entropy <- -sum(props * log2(props))

                difficulty_level <- if (modal_share >= 0.9) {
                    .("Easy (high consensus)")
                } else if (modal_share >= 0.7) {
                    .("Moderate difficulty")
                } else if (modal_share > 0.5) {
                    .("Difficult (significant disagreement)")
                } else {
                    .("Very difficult (high disagreement)")
                }

                list(
                    difficulty_score = round(1 - modal_share, 3),
                    disagreement_pattern = private$.describeDisagreementPattern(rating_table),
                    difficulty_level = difficulty_level,
                    rater_variability = round(entropy / log2(max(k_scale, 2)), 3)
                )
            },

            # Describe disagreement patterns
            .describeDisagreementPattern = function(rating_table) {
                n_categories <- length(rating_table)
                total_ratings <- sum(rating_table)

                if (n_categories == 1) {
                    return(.("Complete consensus"))
                } else if (n_categories == 2) {
                    props <- rating_table / total_ratings
                    if (max(props) >= 0.8) {
                        return(.("Mostly agreement with some dissent"))
                    } else {
                        return(.("Split opinion"))
                    }
                } else {
                    return(jmvcore::format(
                        .("Multi-way disagreement ({count} different ratings)"),
                        count = n_categories
                    ))
                }
            },

            # Agreement Stability Analysis using Bootstrap
            .performStabilityAnalysis = function() {
                stability_table <- self$results$stabilityTable

                tryCatch(
                    {
                        stability_results <- private$.performBootstrapStability()

                        for (statistic_name in names(stability_results)) {
                            stat_info <- stability_results[[statistic_name]]

                            stability_table$setRow(rowKey = statistic_name, values = list(
                                statistic = switch(statistic_name,
                                    overall_agreement = .("Pairwise agreement"),
                                    kappa = if (private$.n_raters == 2) .("Cohen's kappa") else .("Fleiss' kappa"),
                                    statistic_name
                                ),
                                original_value = stat_info$original,
                                bootstrap_mean = stat_info$bootstrap_mean,
                                bootstrap_se = stat_info$bootstrap_se,
                                stability_index = stat_info$stability_index,
                                interpretation = stat_info$interpretation
                            ))
                        }

                        stability_table$setNote("index", .("Stability index = |bootstrap mean / bootstrap SE|. It is small whenever the statistic is close to zero, even when estimated precisely, so read the bootstrap SE and interval rather than the index alone."))
                        stability_table$setNote("interval", jmvcore::format(
                            .("Confidence intervals use the selected {method} case-bootstrap method; BCa uses leave-one-case-out estimates to adjust for bias and skewness."),
                            method = if (self$options$bootstrapCIType == "bca") .("BCa") else .("percentile")
                        ))
                    },
                    error = function(e) {
                        error_msg <- jmvcore::format(
                            .("Error in stability analysis: {error}. Ensure enough complete cases are available for bootstrap resampling."),
                            error = e$message
                        )
                        private$.accumulateMessage(error_msg, severity = "error")
                    }
                )
            },

            # Perform bootstrap stability analysis
            .performBootstrapStability = function() {
                withr::local_seed(self$options$seed) # reproducible; restores the RNG on exit
                data_matrix <- private$.data_matrix
                n_cases <- nrow(data_matrix)
                # Use user-specified bootstrap samples, with validation
                n_bootstrap <- self$options$bootstrapSamples
                if (is.null(n_bootstrap) || n_bootstrap < 100) n_bootstrap <- 1000
                if (n_bootstrap > 5000) n_bootstrap <- 5000 # Cap for performance

                # Store original statistics
                original_stats <- private$.calculateOriginalStatistics()

                # Bootstrap sampling
                bootstrap_results <- list(
                    overall_agreement = numeric(n_bootstrap),
                    kappa = numeric(n_bootstrap)
                )

                for (b in seq_len(n_bootstrap)) {
                    if (b %% 50 == 1) private$.checkpoint(flush = FALSE)
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
                fallback_used <- FALSE

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

                        # Report the bootstrap interval. Labels from the index ("Very
                        # stable" > 10 ... "Unstable" < 2) conflated size with precision:
                        # a kappa of -0.002 with SE 0.04 was called "Unstable".
                        jackknife_values <- if (self$options$bootstrapCIType == "bca") {
                            vapply(seq_len(n_cases), function(i) {
                                reduced <- data_matrix[-i, , drop = FALSE]
                                switch(stat_name,
                                    overall_agreement = private$.calculateOverallAgreementFromMatrix(reduced),
                                    kappa = private$.calculateKappaFromMatrix(reduced)
                                )
                            }, numeric(1))
                        } else {
                            numeric()
                        }
                        interval <- private$.bootstrapInterval(
                            bootstrap_values, original_value, jackknife_values
                        )
                        ci <- interval$ci
                        fallback_used <- fallback_used || isTRUE(interval$fallback)
                        interpretation <- jmvcore::format(
                            .("95% {method} bootstrap interval {lower} to {upper}"),
                            method = interval$method,
                            lower = sprintf("%.3f", ci[1]), upper = sprintf("%.3f", ci[2])
                        )

                        results[[stat_name]] <- list(
                            original = original_value,
                            bootstrap_mean = bootstrap_mean,
                            bootstrap_se = bootstrap_se,
                            stability_index = stability_index,
                            interpretation = interpretation
                        )
                    }
                }

                if (fallback_used) {
                    private$.accumulateMessage(
                        .("At least one BCa stability interval could not be estimated from the leave-one-case-out values, so a percentile interval was reported for that statistic.")
                    )
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

            # Overall (pairwise) agreement on the analysis data matrix.
            # Previously called but never defined - every caller
            # (.calculateGwetAC1/.calculateGwetAC2/.calculateOriginalStatistics)
            # died with "attempt to apply non-function".
            .calculateOverallAgreement = function() {
                private$.calculateOverallAgreementFromMatrix(private$.data_matrix)
            },

            # Calculate overall agreement from data matrix
            .calculateOverallAgreementFromMatrix = function(data_matrix) {
                total_agreements <- 0
                total_comparisons <- 0

                n_cases <- nrow(data_matrix)

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
            # Kappa over ALL raters (Cohen for 2, Fleiss for 3 or more). This used
            # raters 1 and 2 only, via cbind(<factor>, <factor>) - integer level
            # codes - so a 4-rater study's stability row ignored two raters.
            .calculateKappaFromMatrix = function(data_matrix) {
                dm <- data_matrix[stats::complete.cases(data_matrix), , drop = FALSE]
                if (ncol(dm) < 2 || nrow(dm) < 3) {
                    return(NA_real_)
                }
                tryCatch(
                    if (ncol(dm) == 2) irr::kappa2(dm)$value else irr::kappam.fleiss(dm)$value,
                    error = function(e) NA_real_
                )
            },

            # Simple kappa calculation for original stats
            .calculateSimpleKappa = function() {
                return(private$.calculateKappaFromMatrix(private$.data_matrix))
            },

            # Generate inline statistical comments
            .generateInlineComments = function() {
                comments_html <- paste0(
                    "<div style='background: rgba(138, 155, 172, 0.06); color: inherit; border: 1px solid #dee2e6; border-radius: 8px; padding: 20px; font-family: \"Segoe UI\", Arial, sans-serif; line-height: 1.6;'>",
                    "<h3 style='margin: 0 0 15px 0; color: #1976d2; font-size: 18px; border-bottom: 2px solid #e3f2fd; padding-bottom: 8px;'>",
                    .("Statistical commentary and educational notes"),
                    "</h3>",
                    "<div style='background-color: rgba(33, 159, 33, 0.1); border-left: 4px solid #4caf50; padding: 15px; margin-bottom: 15px; color: inherit;'>",
                    .("<h4 style='margin: 0 0 10px 0; color: #2e7d32; font-size: 15px;'>Understanding your results</h4>"),
                    "<div style='font-size: 14px;'>",
                    private$.generateResultsExplanation(),
                    "</div></div>",
                    "<div style='background-color: rgba(255, 169, 33, 0.14); border-left: 4px solid #ff9800; padding: 15px; margin-bottom: 15px; color: inherit;'>",
                    .("<h4 style='margin: 0 0 10px 0; color: #f57c00; font-size: 15px;'>Statistical interpretation guide</h4>"),
                    "<div style='font-size: 14px;'>",
                    private$.generateInterpretationGuide(),
                    "</div></div>",
                    "<div style='background-color: rgba(33, 181, 248, 0.14); border-left: 4px solid #0277bd; padding: 15px; margin-bottom: 15px; color: inherit;'>",
                    .("<h4 style='margin: 0 0 10px 0; color: #01579b; font-size: 15px;'>Educational insights</h4>"),
                    "<div style='font-size: 14px;'>",
                    private$.generateEducationalInsights(),
                    "</div></div>",
                    "<div style='background-color: rgba(230, 33, 99, 0.12); border-left: 4px solid #c2185b; padding: 15px; color: inherit;'>",
                    .("<h4 style='margin: 0 0 10px 0; color: #ad1457; font-size: 15px;'>Important considerations</h4>"),
                    "<div style='font-size: 14px;'>",
                    private$.generateImportantConsiderations(),
                    "</div></div>",
                    "</div>"
                )

                self$results$inlineComments$setContent(comments_html)
            },

            # Generate explanation of current results
            .generateResultsExplanation = function() {
                data_summary <- private$.summarizeCurrentData()

                explanations <- c(
                    jmvcore::format(
                        .("<strong>Your dataset:</strong> {cases} cases rated by {raters} raters across {categories} categories."),
                        cases = data_summary$n_cases,
                        raters = data_summary$n_raters,
                        categories = data_summary$n_categories
                    ),
                    if (self$options$gwetAC) {
                        .("<strong>Gwet's coefficients:</strong> These coefficients are less sensitive than standard kappa to unequal category prevalence.")
                    } else {
                        NULL
                    },
                    if (self$options$pabak) {
                        .("<strong>PABAK analysis:</strong> This reports Brennan-Prediger agreement under an equal-probability chance model.")
                    } else {
                        NULL
                    },
                    if (self$options$raterBiasAnalysis) {
                        .("<strong>Bias detection:</strong> This identifies systematic tendencies in individual raters for review in training or calibration.")
                    } else {
                        NULL
                    },
                    if (self$options$caseDifficultyScoring) {
                        .("<strong>Difficulty analysis:</strong> Cases with high disagreement may be ambiguous or may reveal a need to refine the rating protocol.")
                    } else {
                        NULL
                    }
                )

                return(paste(explanations[!sapply(explanations, is.null)], collapse = "<br><br>"))
            },

            # Generate statistical interpretation guide
            .generateInterpretationGuide = function() {
                guides <- c(
                    .("<strong>Kappa interpretation (Landis & Koch):</strong>"),
                    .("\u{2022} 0.00-0.20: Slight agreement"),
                    .("\u{2022} 0.21-0.40: Fair agreement"),
                    .("\u{2022} 0.41-0.60: Moderate agreement"),
                    .("\u{2022} 0.61-0.80: Substantial agreement"),
                    .("\u{2022} 0.81-1.00: Almost perfect agreement"),
                    "",
                    if (self$options$gwetAC) {
                        c(
                            .("<strong>Gwet's AC versus kappa:</strong>"),
                            .("\u{2022} AC coefficients are less affected by prevalence imbalance"),
                            .("\u{2022} AC values can differ materially from kappa"),
                            .("\u{2022} AC1 is the nominal-data coefficient"),
                            .("\u{2022} AC2 applies disagreement weights for ordinal data")
                        )
                    } else {
                        NULL
                    },
                    "",
                    .("<strong>Clinical significance:</strong>"),
                    .("\u{2022} Landis &amp; Koch bands are descriptive conventions, not clinical acceptability thresholds"),
                    .("\u{2022} The agreement required depends on the clinical consequence of a disagreement"),
                    .("\u{2022} Consider confidence intervals; wide intervals indicate imprecision")
                )

                return(paste(guides[!sapply(guides, is.null)], collapse = "<br>"))
            },

            # Generate educational insights
            .generateEducationalInsights = function() {
                insights <- c(
                    .("<strong>Why agreement matters:</strong>"),
                    .("\u{2022} Assesses consistency across clinicians"),
                    .("\u{2022} Evaluates diagnostic criteria and protocols"),
                    .("\u{2022} Identifies training needs and calibration opportunities"),
                    .("\u{2022} Supports research reproducibility and multi-center studies"),
                    "",
                    .("<strong>Advanced features explained:</strong>"),
                    if (self$options$agreementStabilityAnalysis) {
                        .("\u{2022} <em>Stability analysis:</em> Bootstrap resampling assesses how the agreement statistics vary across resampled cases.")
                    } else {
                        NULL
                    },
                    if (self$options$agreementTrendAnalysis) {
                        .("\u{2022} <em>Trend analysis:</em> Tracks agreement over the case sequence to explore learning effects or fatigue.")
                    } else {
                        NULL
                    },
                    if (self$options$sampleSizePlanning) {
                        .("\u{2022} <em>Sample-size planning:</em> Helps design future studies for a target confidence-interval precision.")
                    } else {
                        NULL
                    },
                    "",
                    .("<strong>Best practices:</strong>"),
                    .("\u{2022} Pre-specify agreement thresholds before data collection"),
                    .("\u{2022} Use complementary agreement measures when their assumptions match the data"),
                    .("\u{2022} Consider clinical context when interpreting statistical results"),
                    .("\u{2022} Use calibration sessions when the study protocol calls for them")
                )

                return(paste(insights[!sapply(insights, is.null)], collapse = "<br>"))
            },

            # Generate important considerations
            .generateImportantConsiderations = function() {
                considerations <- c(
                    .("<strong>Statistical assumptions:</strong>"),
                    .("\u{2022} Raters should be independent (no collaboration during rating)"),
                    .("\u{2022} Cases should be representative of the target population"),
                    .("\u{2022} Missing-data patterns may affect results"),
                    .("\u{2022} Category definitions should be clear and consistent"),
                    "",
                    .("<strong>Clinical interpretation:</strong>"),
                    .("\u{2022} High statistical agreement does not automatically establish clinical acceptability"),
                    .("\u{2022} Consider the consequences of disagreement in the specific context"),
                    .("\u{2022} Some diagnostic categories may inherently have lower agreement"),
                    .("\u{2022} Training and protocol changes may alter agreement over time"),
                    "",
                    .("<strong>Precision note:</strong> Judge estimate stability from its confidence interval and use the planning output when designing a future study."),
                    "",
                    .("<strong>Next steps:</strong>"),
                    .("\u{2022} Review cases with poor agreement for learning opportunities"),
                    .("\u{2022} Consider whether disagreements reveal protocol ambiguities"),
                    .("\u{2022} Plan follow-up calibration sessions if agreement is below the prespecified target"),
                    .("\u{2022} Document agreement thresholds in the research protocol")
                )

                return(paste(considerations[!sapply(considerations, is.null)], collapse = "<br>"))
            },

            # Summarize current dataset for commentary
            .summarizeCurrentData = function() {
                data_matrix <- private$.data_matrix

                return(list(
                    n_cases = nrow(data_matrix),
                    n_raters = ncol(data_matrix),
                    # as.vector(<data.frame>) returns the LIST OF COLUMNS, so this
                    # counted raters, not categories.
                    n_categories = length(private$.categoryLabels(data_matrix))
                ))
            },

            # Plot rendering functions for new visualizations
            .trendPlot = function(image, ggtheme, theme, ...) {
                if (!private$.restorePlotState(image)) return(FALSE)
                if (!self$options$agreementTrendAnalysis || is.null(private$.data_matrix)) {
                    return(FALSE)
                }
                p <- private$.createTrendVisualization(ggtheme)
                if (is.null(p)) {
                    return(private$.plotUnavailable(.("Too few cases to split into more than one case group.")))
                }
                print(p)
                TRUE
            },
            .biasPlot = function(image, ggtheme, theme, ...) {
                if (!private$.restorePlotState(image)) return(FALSE)
                if (!self$options$raterBiasAnalysis || is.null(private$.data_matrix)) {
                    return(FALSE)
                }
                p <- private$.createBiasVisualization(ggtheme)
                if (is.null(p)) {
                    return(private$.plotUnavailable(.("Rater bias could not be calculated for these data.")))
                }
                print(p)
                TRUE
            },
            .difficultyPlot = function(image, ggtheme, theme, ...) {
                if (!private$.restorePlotState(image)) return(FALSE)
                if (!self$options$caseDifficultyScoring || is.null(private$.data_matrix)) {
                    return(FALSE)
                }
                p <- private$.createDifficultyVisualization(ggtheme)
                if (is.null(p)) {
                    return(private$.plotUnavailable(.("Case difficulty could not be calculated for these data.")))
                }
                print(p)
                TRUE
            },

            # ---- Trend / bias / difficulty plots ------------------------------
            #
            # These three plotted HARDCODED numbers - c(0.65, ..., 0.85), four
            # invented "Rater 1-4" bias scores, and runif(50) difficulty scores -
            # under authoritative titles, next to tables computed from the real
            # data. They now plot the producer each table uses. ggtheme goes before
            # the scales: it carries its own discrete fill scale.
            .createTrendVisualization = function(ggtheme) {
                res <- private$.analyzeAgreementTrends()
                if (length(res) < 2) {
                    return(NULL)
                }
                df <- data.frame(
                    group = factor(names(res), levels = names(res)),
                    agreement = vapply(res, function(x) as.numeric(x$agreement_percent), numeric(1)),
                    kappa = vapply(res, function(x) as.numeric(x$kappa), numeric(1))
                )
                df$label <- ifelse(is.na(df$kappa), "", sprintf("\u{03BA} = %.2f", df$kappa))
                trend <- res[[1]]$trend_direction
                subtitle <- if (is.null(trend) || identical(trend, "Calculating...")) {
                    .("Pairwise agreement in consecutive case groups")
                } else {
                    jmvcore::format(.("Trend: {direction}"), direction = trend)
                }
                ggplot2::ggplot(df, ggplot2::aes(x = group, y = agreement, group = 1)) +
                    ggplot2::geom_line(linewidth = 0.9, colour = "#2166ac") +
                    ggplot2::geom_point(size = 3, colour = "#2166ac") +
                    ggplot2::geom_text(ggplot2::aes(label = label), vjust = -1.1, size = 3.3) +
                    ggtheme +
                    ggplot2::coord_cartesian(ylim = c(0, 105)) +
                    ggplot2::labs(
                        title = .("Agreement Across Sequential Case Groups"),
                        subtitle = subtitle,
                        x = .("Case group (data order)"),
                        y = .("Pairwise agreement (%)")
                    )
            },

            .createBiasVisualization = function(ggtheme) {
                res <- private$.detectRaterBias()
                if (length(res) == 0) {
                    return(NULL)
                }
                severity_levels <- c(
                    .("Minimal disagreement"), .("Mild disagreement"),
                    .("Moderate disagreement"), .("Marked disagreement")
                )
                # Short legend labels: the full wording is in the table, and the long
                # labels were clipped at the declared 700px width.
                short_names <- c(.("Minimal"), .("Mild"), .("Moderate"), .("Marked"))
                full <- vapply(res, function(x) as.character(x$severity), character(1))
                df <- data.frame(
                    rater = factor(names(res), levels = names(res)),
                    bias = vapply(res, function(x) as.numeric(x$bias_score), numeric(1)),
                    severity = factor(short_names[match(full, severity_levels)], levels = short_names)
                )
                ggplot2::ggplot(df, ggplot2::aes(x = rater, y = bias, fill = severity)) +
                    ggplot2::geom_hline(yintercept = c(0.1, 0.2, 0.3), linetype = "dotted", colour = "grey60") +
                    ggplot2::geom_col(width = 0.7, colour = "grey20") +
                    ggtheme +
                    ggplot2::scale_fill_manual(
                        values = stats::setNames(c("#4575b4", "#91bfdb", "#fc8d59", "#d73027"), short_names),
                        drop = TRUE, name = NULL
                    ) +
                    ggplot2::coord_cartesian(ylim = c(0, max(0.35, max(df$bias, na.rm = TRUE) * 1.1))) +
                    ggplot2::labs(
                        title = .("Disagreement with the Other Raters"),
                        subtitle = .("Share of cases differing from the other raters"),
                        x = NULL, y = .("Disagreement")
                    ) +
                    ggplot2::theme(
                        legend.position = "bottom",
                        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
                    )
            },

            .createDifficultyVisualization = function(ggtheme) {
                res <- private$.analyzeCaseDifficulty()
                if (length(res) == 0) {
                    return(NULL)
                }
                level_names <- c(
                    "Easy (high consensus)", "Moderate difficulty",
                    "Difficult (significant disagreement)", "Very difficult (high disagreement)"
                )
                # Short legend labels: the full wording is in the table, and it was
                # clipped at the declared 700px width.
                short_names <- c(.("Easy"), .("Moderate"), .("Difficult"), .("Very difficult"))
                full <- vapply(res, function(x) as.character(x$difficulty_level), character(1))
                df <- data.frame(
                    score = vapply(res, function(x) as.numeric(x$difficulty_score), numeric(1)),
                    level = factor(short_names[match(full, level_names)], levels = short_names)
                )
                ggplot2::ggplot(df, ggplot2::aes(x = score, fill = level)) +
                    ggplot2::geom_histogram(breaks = seq(0, 1, by = 0.05), colour = "grey20", linewidth = 0.2) +
                    ggtheme +
                    ggplot2::scale_fill_manual(
                        values = stats::setNames(c("#4575b4", "#91bfdb", "#fc8d59", "#d73027"), short_names),
                        drop = TRUE, name = NULL
                    ) +
                    ggplot2::coord_cartesian(xlim = c(0, 1)) +
                    ggplot2::labs(
                        title = .("Case Difficulty Distribution"),
                        subtitle = jmvcore::format(.("Share of raters differing from the modal rating, {cases} cases"), cases = nrow(df)),
                        x = .("Difficulty score (0 = full consensus)"),
                        y = .("Cases")
                    ) +
                    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1)) +
                    ggplot2::theme(legend.position = "bottom")
            },

            # Interpret ICC values using standard guidelines
            .interpretICCValue = function(icc_value) {
                if (is.na(icc_value) || is.null(icc_value)) {
                    return(.("Unable to calculate"))
                }

                if (icc_value < 0) {
                    return(.("Poor reliability (negative ICC)"))
                } else if (icc_value < 0.5) {
                    return(.("Poor reliability"))
                } else if (icc_value < 0.75) {
                    return(.("Moderate reliability"))
                } else if (icc_value < 0.9) {
                    return(.("Good reliability"))
                } else {
                    return(.("Excellent reliability"))
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
                        "note",
                        .("Clustering requires at least 3 raters.")
                    )
                    return()
                }

                # Perform hierarchical clustering
                clustering_result <- private$.style_clustering_results
                if (is.null(clustering_result)) {
                    clustering_result <- private$.performRaterClustering()
                }

                # Store results
                private$.style_clustering_results <- clustering_result

                # Populate result tables
                private$.populateStyleGroupSummary(clustering_result)
                private$.populateStyleGroupProfiles(clustering_result)

                # Identify discordant cases if requested
                if (self$options$identifyDiscordant) {
                    private$.identifyClusteringDiscordantCases(clustering_result)
                }

                # Test associations with rater characteristics when requested
                if (self$options$raterCharacteristics) {
                    private$.testCharacteristicAssociations(clustering_result)
                }

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

                # Extract case data (non-metadata rows)
                case_data <- private$.dropMetadataRows(data_subset)

                # Parse metadata
                rater_metadata <- list()

                for (idx in metadata_indices) {
                    # Get metadata type from case_id
                    meta_id <- case_ids[idx]
                    meta_type <- gsub("^META_", "", meta_id, ignore.case = TRUE)
                    meta_type <- tolower(trimws(meta_type))

                    # Extract values for each rater
                    # as.character(<data.frame>[idx, cols]) returns factor level CODES,
                    # so META_experience used to read as level indices, not years.
                    rater_values <- vapply(full_data[idx, private$.rater_names, drop = FALSE], as.character, character(1))
                    names(rater_values) <- private$.rater_names

                    # Try to convert to numeric if possible (for experience, volume)
                    if (meta_type %in% c("experience", "volume", "years", "age")) {
                        numeric_values <- suppressWarnings(as.numeric(rater_values))
                        if (!all(is.na(numeric_values))) {
                            # as.numeric() drops names; .raterMeta() looks values up by rater name
                            rater_values <- stats::setNames(numeric_values, names(rater_values))
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
                            agreement_matrix[i, j] <- mean(as.character(rater_i) == as.character(rater_j), na.rm = TRUE)
                        }
                    }
                }

                distance_matrix <- private$.calculateRaterDistanceMatrix()

                # Perform hierarchical clustering
                linkage_method <- self$options$clusteringMethod
                hc_method <- if (identical(linkage_method, "ward")) "ward.D2" else linkage_method
                # Disagreement and 1 - correlation are squared Euclidean
                # dissimilarities; Ward.D2 takes their square roots.
                clustering_distance <- if (linkage_method == "ward" && private$.distance_metric != "euclidean") {
                    sqrt(2 * distance_matrix)
                } else distance_matrix
                hc <- hclust(clustering_distance, method = hc_method)

                # Determine number of clusters
                if (self$options$autoSelectGroups) {
                    # Use silhouette method
                    k_optimal <- private$.selectOptimalK(distance_matrix, hc)
                } else {
                    k_optimal <- private$.styleGroupCount()
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
                        style_group = jmvcore::format(.("Group {group}"), group = k),
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
                    return(.("Poorly separated group"))
                } else if (silhouette < 0.5) {
                    return(.("Moderately cohesive diagnostic style"))
                } else if (silhouette < 0.7) {
                    return(.("Distinct diagnostic style"))
                } else {
                    return(.("Highly consistent diagnostic style"))
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
                    cluster_diagnoses <- unlist(
                        lapply(private$.data_matrix[, cluster_members, drop = FALSE], as.character),
                        use.names = FALSE
                    )

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
                            style_group = jmvcore::format(.("Group {group}"), group = k),
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
                    return(.("Not available"))
                }

                # Get frequency in current group
                current_members <- which(cluster_assignments == current_group)
                current_diagnoses <- unlist(
                    lapply(private$.data_matrix[, current_members, drop = FALSE], as.character),
                    use.names = FALSE
                )
                current_freq <- mean(current_diagnoses == category)

                # Get frequency in other groups
                other_members <- which(cluster_assignments != current_group)
                other_diagnoses <- unlist(
                    lapply(private$.data_matrix[, other_members, drop = FALSE], as.character),
                    use.names = FALSE
                )
                other_freq <- mean(other_diagnoses == category)

                # Compare
                diff <- current_freq - other_freq
                if (abs(diff) < 0.05) {
                    return(.("Similar"))
                } else if (diff > 0.15) {
                    return(.("Much higher"))
                } else if (diff > 0.05) {
                    return(.("Higher"))
                } else if (diff < -0.15) {
                    return(.("Much lower"))
                } else {
                    return(.("Lower"))
                }
            },

            # Identify high-disagreement cases between style groups
            .identifyClusteringDiscordantCases = function(clustering_result) {
                table <- self$results$discordantCasesCluster

                cluster_assignments <- clustering_result$cluster_assignments
                n_clusters <- clustering_result$n_clusters

                if (n_clusters < 2) {
                    table$setNote("note", .("Discordant case analysis requires at least 2 style groups."))
                    return()
                }

                threshold <- self$options$discordantThreshold

                # For each case, calculate disagreement between groups
                for (i in 1:private$.n_cases) {
                    case_diagnoses <- private$.rowLabels(private$.data_matrix, i)

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
                            group_patterns <- c(
                                group_patterns,
                                jmvcore::format(
                                    .("Group {group1}: {rating1} vs group {group2}: {rating2}"),
                                    group1 = k1,
                                    rating1 = g1_mode,
                                    group2 = k2,
                                    rating2 = g2_mode
                                )
                            )
                        }
                    }

                    max_disagreement <- max(disagreement_scores)

                    # Only include cases with high disagreement
                    if (max_disagreement >= threshold) {
                        # Difficulty level based on entropy
                        difficulty <- if (entropy < 0.5) {
                            .("Low disagreement")
                        } else if (entropy < 1.0) {
                            .("Moderate disagreement")
                        } else if (entropy < 1.5) {
                            .("High disagreement")
                        } else {
                            .("Very high disagreement")
                        }

                        table$addRow(rowKey = i, values = list(
                            case_id = private$.caseLabels()[i],
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
                }

                if (length(characteristics_data) == 0) {
                    table$setNote("note", .("No rater characteristics found. Add META_ rows (for example META_experience) to the dataset, select the case ID variable, and enable metadata rows."))
                    return()
                }

                table$setNote("design", .("Each test uses one value per rater, so the sample size is the number of raters; p-values are not adjusted across characteristics. Categorical characteristics use Fisher's exact test (10,000 simulations, seeded) when any expected count is below 5; effect size is Cramer's V, or epsilon-squared for numeric characteristics."))

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
            # Association between a rater characteristic and style-group membership.
            #
            # The categorical branch used fisher.test()$statistic, which does not exist
            # for an r x c table; the effect size became numeric(0), interpretation
            # threw "argument is of length zero", the tryCatch returned NULL and the
            # row disappeared (institution never appeared on the shipped META_ data).
            # Fisher was also chosen by OBSERVED counts < 5 and its simulated p-value
            # was unseeded, so it changed on every run.
            .testCharacteristicAssociation = function(char_data, clusters, char_name) {
                if (is.numeric(char_data)) {
                    test <- tryCatch(stats::kruskal.test(char_data ~ factor(clusters)), error = function(e) NULL)
                    if (is.null(test)) {
                        return(NULL)
                    }
                    # epsilon-squared for Kruskal-Wallis: H / (n - 1)
                    effect_size <- unname(test$statistic) / (length(char_data) - 1)
                    return(list(
                        characteristic = char_name,
                        test_statistic = unname(test$statistic),
                        df = unname(test$parameter),
                        p_value = test$p.value,
                        effect_size = effect_size,
                        interpretation = private$.interpretAssociation(test$p.value, effect_size, TRUE)
                    ))
                }

                tab <- table(as.character(char_data), clusters)
                if (nrow(tab) < 2 || ncol(tab) < 2) {
                    return(NULL)
                }
                chi <- suppressWarnings(stats::chisq.test(tab, correct = FALSE))
                cramers_v <- sqrt(unname(chi$statistic) / (sum(tab) * (min(dim(tab)) - 1)))
                if (any(chi$expected < 5)) {
                    test <- withr::with_seed(self$options$seed, stats::fisher.test(tab, simulate.p.value = TRUE, B = 10000))
                    statistic <- NA_real_
                    df <- NA_real_
                } else {
                    test <- chi
                    statistic <- unname(chi$statistic)
                    df <- unname(chi$parameter)
                }
                list(
                    characteristic = char_name,
                    test_statistic = statistic,
                    df = df,
                    p_value = test$p.value,
                    effect_size = cramers_v,
                    interpretation = private$.interpretAssociation(test$p.value, cramers_v, FALSE)
                )
            },

            # Interpret association test results
            .interpretAssociation = function(p_value, effect_size, is_numeric) {
                if (length(p_value) != 1 || is.na(p_value)) {
                    return(.("Association could not be tested"))
                }
                sig_text <- if (p_value < 0.05) {
                    jmvcore::format(.("p = {p}"), p = format.pval(p_value, digits = 2, eps = 0.001))
                } else {
                    jmvcore::format(.("not significant, p = {p}"), p = format.pval(p_value, digits = 2))
                }
                effect_text <- if (length(effect_size) != 1 || is.na(effect_size)) {
                    ""
                } else if (effect_size < 0.1) {
                    .("negligible effect")
                } else if (effect_size < 0.3) {
                    .("small effect")
                } else if (effect_size < 0.5) {
                    .("moderate effect")
                } else {
                    .("large effect")
                }
                if (nzchar(effect_text)) paste0(sig_text, "; ", effect_text) else sig_text
            },

            # Compare style groups with reference standard
            .referenceGroupKappa = function(dm, reference, groups) {
                raters <- vapply(dm, function(x) {
                    tryCatch(irr::kappa2(data.frame(
                        rating = as.character(x), reference = as.character(reference)
                    ))$value, error = function(e) NA_real_)
                }, numeric(1))
                vapply(sort(unique(groups)), function(k) {
                    values <- raters[groups == k]
                    if (any(!is.finite(values))) NA_real_ else mean(values)
                }, numeric(1))
            },

            .referenceBootstrap = function(dm, reference, groups) {
                withr::local_seed(self$options$seed)
                n <- nrow(dm)
                reps <- as.integer(self$options$bootstrapSamples)
                group_ids <- sort(unique(groups))
                estimates <- matrix(NA_real_, reps, length(group_ids))
                for (b in seq_len(reps)) {
                    if (b %% 25 == 1) private$.checkpoint(flush = FALSE)
                    rows <- sample.int(n, n, replace = TRUE)
                    estimates[b, ] <- private$.referenceGroupKappa(
                        dm[rows, , drop = FALSE], reference[rows], groups
                    )
                }
                original <- private$.referenceGroupKappa(dm, reference, groups)
                jackknife <- if (self$options$bootstrapCIType == "bca") {
                    matrix(vapply(seq_len(n), function(i) {
                        private$.referenceGroupKappa(
                            dm[-i, , drop = FALSE], reference[-i], groups
                        )
                    }, numeric(length(group_ids))), nrow = length(group_ids))
                } else {
                    matrix(numeric(), nrow = length(group_ids), ncol = 0)
                }
                fallbacks <- logical(length(group_ids))
                methods <- rep(NA_character_, length(group_ids))
                intervals <- matrix(NA_real_, nrow = 2, ncol = ncol(estimates))
                for (k in seq_len(ncol(estimates))) {
                    valid <- estimates[, k][is.finite(estimates[, k])]
                    if (length(valid) < max(50, ceiling(0.8 * reps))) next
                    interval <- private$.bootstrapInterval(valid, original[k], jackknife[k, ])
                    fallbacks[k] <- interval$fallback
                    methods[k] <- interval$method
                    intervals[, k] <- interval$ci
                }
                list(
                    intervals = intervals, valid = colSums(is.finite(estimates)), reps = reps,
                    methods = methods, fallbacks = fallbacks
                )
            },

            .compareWithReference = function(clustering_result) {
                table <- self$results$referenceComparison
                reference <- private$.referenceStandardValues()
                valid <- !is.na(reference)
                if (is.null(reference) || sum(valid) < 2) {
                    table$setNote("note", .("Reference comparison requires at least two retained cases with reference ratings."))
                    return()
                }
                dm <- private$.data_matrix[valid, , drop = FALSE]
                reference <- as.character(reference[valid])
                groups <- clustering_result$cluster_assignments
                kappas <- private$.referenceGroupKappa(dm, reference, groups)
                intervals <- matrix(NA_real_, 2, length(kappas))
                note <- .("The estimate is the mean of individual rater kappas against the reference standard, conditional on the observed style groups. Enable bootstrap confidence intervals to resample whole cases, keeping all raters and the reference together.")
                if (self$options$bootstrap) {
                    boot <- private$.referenceBootstrap(dm, reference, groups)
                    intervals <- boot$intervals
                    note <- jmvcore::format(
                        .("95% {method} conditional intervals use {resamples} case bootstrap resamples (seed {seed}). All ratings for a case are sampled together. The observed raters and style groups are held fixed; these intervals do not include uncertainty from selecting the groups or sampling new raters."),
                        method = if (self$options$bootstrapCIType == "bca") .("BCa") else .("percentile"),
                        resamples = boot$reps, seed = self$options$seed
                    )
                    if (any(boot$fallbacks)) {
                        note <- paste(
                            note,
                            .("A percentile interval is shown where the BCa acceleration could not be estimated.")
                        )
                    }
                    if (anyNA(intervals)) {
                        private$.accumulateMessage(.("Some reference-comparison bootstrap intervals could not be estimated because too many resamples had undefined kappas. The corresponding interval cells are left empty."))
                    }
                }
                for (k in seq_along(kappas)) {
                    agreement <- mean(vapply(dm[groups == k], function(x) {
                        mean(as.character(x) == reference)
                    }, numeric(1))) * 100
                    table$addRow(rowKey = k, values = list(
                        style_group = jmvcore::format(.("Group {group}"), group = k),
                        kappa_vs_reference = kappas[k],
                        agreement_percent = agreement, ci_lower = intervals[1, k],
                        ci_upper = intervals[2, k], accuracy_level = private$.interpretKappa(kappas[k])
                    ))
                }
                table$setNote("note", note)
            },

            # Generate clustering interpretation guide
            .generateClusteringInterpretation = function(clustering_result) {
                html_content <- self$results$clusteringInterpretation

                n_clusters <- clustering_result$n_clusters

                html <- '<div style="font-family: Arial, sans-serif; max-width: 900px; margin: 20px;">'
                html <- paste0(
                    html,
                    .('<h2 style="color: #1976d2;">Clustering analysis interpretation guide</h2>')
                )

                html <- paste0(html, .("<h3>What is diagnostic style clustering?</h3>"))
                html <- paste0(
                    html,
                    .('<p>This analysis groups raters by their diagnostic patterns. The resulting "diagnostic styles" describe approaches that differ systematically between groups.</p>')
                )

                html <- paste0(html, .("<h3>Your results</h3>"))
                distance_label <- switch(private$.distance_metric,
                    agreement = .("percentage disagreement distance"),
                    correlation = .("one minus signed Spearman correlation distance"),
                    euclidean = .("Euclidean distance"),
                    .("the selected distance")
                )
                html <- paste0(
                    html,
                    "<p>",
                    jmvcore::format(
                        .("Analysis identified <strong>{count} diagnostic style groups</strong> using hierarchical clustering with {distance}."),
                        count = n_clusters,
                        distance = distance_label
                    ),
                    "</p>"
                )

                html <- paste0(html, .("<h3>Understanding the tables</h3>"))
                html <- paste0(html, "<ul>")
                html <- paste0(html, .("<li><strong>Style-groups summary:</strong> Shows rater composition and agreement within and between groups</li>"))
                html <- paste0(html, .("<li><strong>Diagnostic patterns:</strong> Shows how each group uses diagnostic categories</li>"))
                html <- paste0(html, .("<li><strong>Discordant cases:</strong> Shows cases where style groups disagree most</li>"))
                html <- paste0(html, .("<li><strong>Characteristic associations:</strong> Tests whether style is associated with experience, institution, or other supplied characteristics</li>"))
                html <- paste0(html, "</ul>")

                html <- paste0(html, .("<h3>Clinical implications</h3>"))
                html <- paste0(html, .("<p><strong>High within-group agreement</strong> suggests a consistent diagnostic approach within a style group.</p>"))
                html <- paste0(html, .("<p><strong>Low between-group agreement</strong> indicates systematic differences in diagnostic interpretation.</p>"))
                html <- paste0(html, .("<p><strong>No detected association with a characteristic</strong> means these data do not provide evidence of an association; it does not establish that the characteristic has no effect.</p>"))

                html <- paste0(html, .("<h3>Silhouette scores</h3>"))
                html <- paste0(html, "<ul>")
                html <- paste0(html, .("<li><strong>&gt; 0.7:</strong> Strong separation between the observed groups</li>"))
                html <- paste0(html, .("<li><strong>0.5-0.7:</strong> Moderate separation between the observed groups</li>"))
                html <- paste0(html, .("<li><strong>&lt; 0.5:</strong> Weak separation with overlapping groups</li>"))
                html <- paste0(html, "</ul>")

                html <- paste0(html, "</div>")

                html_content$setContent(html)
            },

            # ====================================================================
            # Plot Render Functions for Clustering
            # ====================================================================

            # Render clustering heatmap with dual dendrograms
            .clusteringHeatmap = function(image, ggtheme, theme, ...) {
                if (!private$.restorePlotState(image)) return(FALSE)
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
                        ncol = ncol(heatmap_matrix)
                    )
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
                            c("#d73027", "#fee090", "#4575b4")
                        )(100)
                    } else {
                        # Diagnostic scheme
                        colors <- grDevices::colorRampPalette(
                            c("#2196f3", "#4caf50", "#ffc107")
                        )(100)
                    }

                    # Create heatmap
                    pheatmap::pheatmap(
                        t(heatmap_numeric), # Transpose: raters as rows
                        cluster_rows = clustering_result$hclust_object,
                        cluster_cols = TRUE,
                        color = colors,
                        main = .("Rater Clustering Heatmap"),
                        fontsize = 10,
                        angle_col = 45,
                        cutree_rows = clustering_result$n_clusters
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
                    value.name = "diagnosis"
                )

                # Get cluster assignments
                cluster_order <- order(clustering_result$cluster_assignments)
                rater_levels <- private$.rater_names[cluster_order]
                heatmap_long$rater <- factor(heatmap_long$rater, levels = rater_levels)

                # Create plot
                p <- ggplot2::ggplot(
                    heatmap_long,
                    ggplot2::aes(x = case, y = rater, fill = diagnosis)
                ) +
                    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
                    ggplot2::scale_fill_viridis_d(option = "D") +
                    ggplot2::labs(
                        title = .("Rater Clustering Heatmap"),
                        x = .("Case"),
                        y = .("Rater (ordered by cluster)"),
                        fill = .("Diagnosis")
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
                if (!private$.restorePlotState(image)) return(FALSE)
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
                    main = jmvcore::format(
                        .("Hierarchical Clustering Dendrogram ({groups} groups)"),
                        groups = k
                    ),
                    xlab = .("Rater"),
                    ylab = .("Rater distance"),
                    sub = "",
                    cex.main = 1.2
                )

                # Add rectangles around clusters
                rect.hclust(hc, k = k, border = 2:6)

                TRUE
            },

            # Render silhouette plot
            .silhouettePlot = function(image, ggtheme, theme, ...) {
                if (!private$.restorePlotState(image)) return(FALSE)
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
                    main = .("Cluster Quality (Silhouette Plot)"),
                    col = 2:(clustering_result$n_clusters + 1),
                    border = NA,
                    cex.names = 0.8
                )

                # Add average silhouette width
                avg_sil <- mean(sil[, "sil_width"])
                mtext(jmvcore::format(
                    .("Average silhouette width: {width}"),
                    width = sprintf("%.3f", avg_sil)
                ),
                    side = 3, line = -1, cex = 0.9
                )

                TRUE
            }
        ) # End of private list
    )
}
